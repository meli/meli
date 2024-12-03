//
// meli
//
// Copyright 2024, 2026 Manos Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{convert::TryInto, sync::Arc};

use http::{header, status::StatusCode, Request};
use isahc::AsyncReadResponseExt;

use crate::{
    backends::prelude::ResultFuture,
    contacts::{
        backend::{ContactBackend, ContactBackendCapabilities},
        vcard, AddressBookName, Card,
    },
    error::{Error, ErrorKind, Result, ResultIntoError},
    utils::webdav::*,
};

#[derive(Debug)]
pub struct CardDAVConnection {
    pub webdav_connection: WebDAVConnection,
    pub can_put: bool,
    pub addressbook_home_sets: Vec<Arc<str>>,
}

impl CardDAVConnection {
    pub async fn new(webdav_connection: WebDAVConnection) -> Result<Self> {
        let can_put;
        let addressbook_home_sets = {
            let session_guard = webdav_connection.online_status.session_guard().await?;
            if !session_guard.supports_carddav() {
                return Err(Error::new(format!(
                    "CardDAV connection from existing WebDAV connection failed: the server does \
                     not support the `addressbook` capability. The server replied with the \
                     following capabilities in the `DAV` http header: {:?}",
                    session_guard.dav_capabilities
                ))
                .set_kind(ErrorKind::ProtocolNotSupported));
            }
            can_put = session_guard.allow_methods.contains("PUT");
            for method in ["REPORT", "PROPFIND"] {
                if !session_guard.allow_methods.contains(method) {
                    return Err(Error::new(format!(
                        "CardDAV connection from existing WebDAV connection failed: the server \
                         does not support the `{method}` HTTP method. The server replied with the \
                         following allowed methods in the `allow` http header: {:?}",
                        session_guard.allow_methods
                    ))
                    .set_kind(ErrorKind::ProtocolNotSupported));
                }
            }
            let mut uri = webdav_connection.server_conf.url.clone();
            uri.set_path(&session_guard.principal);
            drop(session_guard);
            let request = Request::builder()
                .method("PROPFIND")
                .uri(uri.as_str())
                .header(header::CONTENT_TYPE, "application/xml; content-type=utf-8")
                .header("Depth", "0")
                .body(
                    r#"<d:propfind xmlns:d="DAV:" xmlns:card="urn:ietf:params:xml:ns:carddav">
   <d:prop>
      <card:addressbook-home-set />
   </d:prop>
</d:propfind>"#,
                )
                .unwrap();
            let mut resp = webdav_connection.send_async(request).await?;
            if !resp.status().is_success() {
                let kind: crate::error::NetworkErrorKind = resp.status().into();
                let res_text = resp.text().await.unwrap_or_default();
                return Err(Error::new(format!(
                    "Could not connect to WebDAV server endpoint for {}. Reply from server: {}",
                    webdav_connection.server_conf.url, res_text
                ))
                .set_kind(kind.into()));
            }
            let text = resp.text().await?;
            let multistatus: Multistatus<AddressBookHomeSetProp> =
                quick_xml::de::from_str(&text)
                    .chain_err_summary(|| "Could not deserialize PROPFIND response from XML")?;
            let mut addressbook_home_sets = vec![];
            for response in multistatus.response {
                for propstat in response.propstat {
                    if let (data, Some(StatusCode::OK)) = (
                        propstat.prop.addressbook_home_set.href,
                        StatusCode::from_status_line(&propstat.status),
                    ) {
                        if !data.is_empty() {
                            addressbook_home_sets.push(data.into());
                        }
                    }
                }
            }
            addressbook_home_sets
        };

        Ok(Self {
            webdav_connection,
            can_put,
            addressbook_home_sets,
        })
    }

    pub async fn create(
        &self,
        addressbook_home_set: Arc<str>,
        filename: &str,
        vcard: &[u8],
    ) -> Result<Option<String>> {
        let mut uri = self.webdav_connection.server_conf.url.clone();
        uri.set_path(&format!(
            "{addressbook_home_set}/{filename}{vcf}",
            addressbook_home_set = addressbook_home_set.as_ref(),
            vcf = if std::path::Path::new(filename)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("vcf"))
            {
                ""
            } else {
                ".vcf"
            }
        ));
        let request = Request::builder()
            .method("PUT")
            .uri(uri.as_str())
            .body(vcard)
            .unwrap();

        let resp = self.webdav_connection.send_async(request).await?;

        if resp.status() == 201 || resp.status() == 204 {
            Ok(resp
                .headers()
                .get("ETag")
                .and_then(|s| s.to_str().ok())
                .map(Into::into))
        } else {
            Ok(None)
        }
    }

    pub async fn all(&self, addressbook_home_set: Arc<str>) -> Result<Vec<Card>> {
        let mut uri = self.webdav_connection.server_conf.url.clone();
        uri.set_path(&addressbook_home_set);

        let request = Request::builder()
            .method("REPORT")
            .uri(uri.as_str())
                .header(
                    header::CONTENT_TYPE,
                    "application/xml; content-type=utf-8",
                )
                .header(
                    "Depth",
                    "1",
                )
            .body(
r#"<card:addressbook-query xmlns:d="DAV:" xmlns:card="urn:ietf:params:xml:ns:carddav">
    <d:prop>
        <d:getetag />
        <card:address-data />
    </d:prop>
</card:addressbook-query>"#
)
            .unwrap();

        let mut resp = self.webdav_connection.send_async(request).await?;
        let text = resp.text().await?;
        let multistatus: Multistatus<AddressDataProp> = quick_xml::de::from_str(&text)?;
        let mut cards = vec![];
        for response in multistatus.response {
            for propstat in response.propstat {
                if let (Some(data), Some(StatusCode::OK)) = (
                    propstat.prop.address_data.as_deref(),
                    StatusCode::from_status_line(&propstat.status),
                ) {
                    if !data.is_empty() {
                        let card: Card = vcard::CardDeserializer::try_from_str(data)
                            .and_then(TryInto::try_into)?;
                        cards.push(card);
                    }
                }
            }
        }

        Ok(cards)
    }

    pub async fn filter(&self, addressbook_home_set: Arc<str>, term: &str) -> Result<Vec<Card>> {
        let mut uri = self.webdav_connection.server_conf.url.clone();
        uri.set_path(&addressbook_home_set);

        let req_body = format!(
            r#"<card:addressbook-query xmlns:d="DAV:" xmlns:card="urn:ietf:params:xml:ns:carddav">
    <d:prop>
        <d:getetag />
        <card:address-data />
    </d:prop>
    <C:filter test="anyof">
        <C:prop-filter name="FN">
         <C:text-match collation="i;unicode-casemap" match-type="contains">{term}</C:text-match>
        </C:prop-filter>
        <C:prop-filter name="EMAIL">
         <C:text-match collation="i;unicode-casemap" match-type="contains">{term}</C:text-match>
        </C:prop-filter>
    </C:filter>
</card:addressbook-query>"#
        );
        let request = Request::builder()
            .method("REPORT")
            .uri(uri.as_str())
            .header(header::CONTENT_TYPE, "application/xml; content-type=utf-8")
            .header("Depth", "1")
            .body(req_body.as_bytes())
            .unwrap();

        let mut resp = self.webdav_connection.send_async(request).await?;
        let text = resp.text().await?;
        let multistatus: Multistatus<AddressDataProp> = quick_xml::de::from_str(&text)
            .chain_err_summary(|| "Could not deserialize REPORT response from XML")?;
        let mut cards = vec![];
        for response in multistatus.response {
            for propstat in response.propstat {
                if let (Some(data), Some(StatusCode::OK)) = (
                    propstat.prop.address_data.as_deref(),
                    StatusCode::from_status_line(&propstat.status),
                ) {
                    if !data.is_empty() {
                        let card: Card = vcard::CardDeserializer::try_from_str(data)
                            .and_then(TryInto::try_into)?;
                        cards.push(card);
                    }
                }
            }
        }

        Ok(cards)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AddressDataProp {
    #[serde(rename = "address-data")]
    pub address_data: Option<String>,
    #[serde(rename = "getetag")]
    pub getetag: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AddressBookHomeSetProp {
    #[serde(rename = "addressbook-home-set")]
    pub addressbook_home_set: HrefValue,
}

#[derive(Debug)]
pub struct CardDAVContacts {
    pub connection: Arc<CardDAVConnection>,
}

impl ContactBackend for CardDAVContacts {
    fn capabilities(&mut self) -> ContactBackendCapabilities {
        ContactBackendCapabilities {
            is_async: true,
            is_remote: true,
            supports_search: true,
            metadata: Some(serde_json::json! {{
                "url": self.connection.webdav_connection.server_conf.url.to_string(),
            }}),
            ..ContactBackendCapabilities::default()
        }
    }

    fn address_books(&mut self) -> ResultFuture<Vec<AddressBookName>> {
        let names = self
            .connection
            .addressbook_home_sets
            .iter()
            .map(|n| n.clone().into())
            .collect();
        Ok(Box::pin(async { Ok(names) }))
    }

    fn fetch_book(&mut self, address_book: &AddressBookName) -> ResultFuture<Vec<Card>> {
        let conn = self.connection.clone();
        let address_book = address_book.clone();
        Ok(Box::pin(async move { conn.all(address_book.0).await }))
    }

    fn search(
        &self,
        term: &str,
        address_book: Option<&AddressBookName>,
    ) -> ResultFuture<Vec<Card>> {
        let conn = self.connection.clone();
        let address_books = address_book
            .cloned()
            .map(|a| vec![a.0])
            .unwrap_or_else(|| self.connection.addressbook_home_sets.clone());
        let term = term.to_string();
        Ok(Box::pin(async move {
            let mut ret = vec![];
            for book in address_books {
                ret.extend(conn.filter(book, &term).await?);
            }
            Ok(ret)
        }))
    }
}
