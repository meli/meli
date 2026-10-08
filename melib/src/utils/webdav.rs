//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

//! Connect to a `WebDAV` server.
//!
//! For supported authentication schemes see the [`WebDAVAuthentication`]
//! `enum`.
//!
//! A `WebDAV` connection is used to create a `CardDAV` one, see the
//! [`carddav`](crate::contacts::carddav) module for more
//! details.
//!
//! # Example
//!
//! See [`WebDAVConnection`] and its [`WebDAVConnection::connect`] method.
//!
//! ```rust,no_run
//! use melib::{conf::Secret, utils::webdav::*};
//! # async fn _connect() {
//! let server_conf = WebDAVServerConf {
//!     url: "http://localhost:8081".try_into().unwrap(),
//!     username: Secret::Value(String::new()),
//!     password: Secret::Value(String::new()),
//!     auth: WebDAVAuthentication::None,
//!     danger_accept_invalid_certs: false,
//!     timeout: None,
//! };
//! let mut connection = WebDAVConnection::new(&server_conf).await.unwrap();
//! connection.connect().await.unwrap();
//! # }
//! ```

use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use futures::lock::{
    MappedMutexGuard as FutureMappedMutexGuard, Mutex as FutureMutex,
    MutexGuard as FutureMutexGuard,
};
use http::{status::StatusCode, Request};
use indexmap::IndexSet;
use isahc::{
    config::{Configurable as _, DnsCache, RedirectPolicy, SslOption},
    http, AsyncBody, AsyncReadResponseExt as _, HttpClient,
};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::{
    conf::Secret,
    email::parser::BytesExt as _,
    error::{Error, ErrorKind, NetworkErrorKind, Result, ResultIntoError as _},
};

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum WebDAVAuthentication {
    Basic,
    BearerToken,
    None,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WebDAVServerConf {
    pub url: Url,
    pub username: Secret,
    pub password: Secret,
    pub auth: WebDAVAuthentication,
    #[serde(default)]
    pub danger_accept_invalid_certs: bool,
    #[serde(default)]
    pub timeout: Option<Duration>,
}

#[derive(Debug)]
pub struct WebDAVConnection {
    client: Arc<HttpClient>,
    pub online_status: OnlineStatus,
    pub server_conf: WebDAVServerConf,
}

/// Metadata of a live `WebDAV` server connection session.
#[derive(Debug)]
pub struct WebDAVSession {
    /// The set of capabilities this `WebDAV` server supports.
    ///
    /// Example value:
    ///
    /// ```text
    /// { "1", "2", "3", "calendar-access", "calendar-auto-scheduling", "addressbook", "extended-mkcol", "add-member", "sync-collection", "quota" }
    /// ```
    pub dav_capabilities: IndexSet<Box<str>>,
    /// HTTP methods this `WebDAV` server supports.
    ///
    /// Example value:
    ///
    /// ```text
    /// { "DELETE", "GET", "HEAD", "MKCALENDAR", "MKCOL", "OPTIONS", "POST", "PROPFIND", "PROPPATCH", "PUT", "REPORT" }
    /// ```
    pub allow_methods: IndexSet<Box<str>>,
    /// The date of connection as provided by the server.
    pub date: Option<Box<str>>,
    /// An identifier provided by the server.
    pub server_id: Option<Box<str>>,
    /// URL path of the authenticated user.
    pub principal: Box<str>,
}

impl WebDAVSession {
    /// Whether the `WebDAV` server replied with the `addressbook` capability in
    /// its `DAV` HTTP headers on response to `OPTIONS` request.
    #[inline]
    pub fn supports_carddav(&self) -> bool {
        self.dav_capabilities.contains("addressbook")
    }
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct OnlineStatus(pub Arc<FutureMutex<(Instant, Result<WebDAVSession>)>>);

impl OnlineStatus {
    /// Returns if session value is `Ok(_)`.
    pub async fn is_ok(&self) -> bool {
        self.0.lock().await.1.is_ok()
    }

    /// Get timestamp of last update.
    pub async fn timestamp(&self) -> Instant {
        self.0.lock().await.0
    }

    /// Get timestamp of last update.
    pub async fn update_timestamp(&self, value: Option<Instant>) {
        self.0.lock().await.0 = value.unwrap_or_else(Instant::now);
    }

    /// Set inner value.
    pub async fn set(
        &self,
        t: Option<Instant>,
        value: Result<WebDAVSession>,
    ) -> Result<WebDAVSession> {
        std::mem::replace(
            &mut (*self.0.lock().await),
            (t.unwrap_or_else(Instant::now), value),
        )
        .1
    }

    pub async fn session_guard(
        &'_ self,
    ) -> Result<FutureMappedMutexGuard<'_, (Instant, Result<WebDAVSession>), WebDAVSession>> {
        let guard = self.0.lock().await;
        if let Err(ref err) = guard.1 {
            return Err(err.clone());
        }
        Ok(FutureMutexGuard::map(guard, |status| {
            // SAFETY: we checked if it's an Err() in the previous line, but we cannot do it
            // in here since it's a closure. So unwrap unchecked for API
            // convenience.
            unsafe { status.1.as_mut().unwrap_unchecked() }
        }))
    }
}

impl WebDAVConnection {
    pub async fn new(server_conf: &WebDAVServerConf) -> Result<Self> {
        let client = HttpClient::builder()
            .dns_cache(DnsCache::Forever)
            .connection_cache_size(8)
            .connection_cache_ttl(Duration::from_secs(30 * 60))
            // .default_header(http::header::CONTENT_TYPE, "application/json")
            .ssl_options(if server_conf.danger_accept_invalid_certs {
                SslOption::DANGER_ACCEPT_INVALID_CERTS
                    | SslOption::DANGER_ACCEPT_INVALID_HOSTS
                    | SslOption::DANGER_ACCEPT_REVOKED_CERTS
            } else {
                SslOption::NONE
            })
            .tcp_nodelay()
            .tcp_keepalive(Duration::new(60 * 9, 0))
            .redirect_policy(RedirectPolicy::None);
        let client = if let Some(dur) = server_conf.timeout.filter(|dur| *dur != Duration::ZERO) {
            client
                .timeout(dur)
                .connect_timeout(dur + Duration::from_secs(300))
        } else {
            client
        };
        let username = server_conf
            .username
            .value_with_timeout(Duration::from_millis(100))
            .await?;
        let password = server_conf
            .password
            .value_with_timeout(Duration::from_millis(100))
            .await?;
        let client = match server_conf.auth {
            WebDAVAuthentication::BearerToken => client
                .authentication(isahc::auth::Authentication::none())
                .default_header(http::header::AUTHORIZATION, format!("Bearer {password}")),
            WebDAVAuthentication::Basic => client
                .authentication(isahc::auth::Authentication::basic())
                .credentials(isahc::auth::Credentials::new(&username, &password)),
            WebDAVAuthentication::None => client,
        };
        let client = client.build()?;
        let server_conf = server_conf.clone();
        let online_status = OnlineStatus(Arc::new(FutureMutex::new((
            std::time::Instant::now(),
            Err(Error::new("Account is uninitialised.")),
        ))));
        Ok(Self {
            client: Arc::new(client),
            server_conf,
            online_status,
        })
    }

    pub async fn connect(&mut self) -> Result<()> {
        if self.online_status.is_ok().await {
            return Ok(());
        }
        let request = Request::builder()
            .method("OPTIONS")
            .uri(self.server_conf.url.as_str())
            .body(())
            .unwrap();

        let mut resp = match self.client.send_async(request).await.map_err(Error::from) {
            Err(err) => 'block: {
                if matches!(err.kind, ErrorKind::Network(NetworkErrorKind::ProtocolViolation) if self.server_conf.url.scheme() == "http")
                {
                    // attempt recovery by trying https://
                    self.server_conf.url.set_scheme("https").expect(
                        "set_scheme to https must succeed here because we checked earlier that \
                         current scheme is http",
                    );
                    let request = Request::builder()
                        .method("OPTIONS")
                        .uri(self.server_conf.url.as_str())
                        .body(())
                        .unwrap();
                    if let Ok(s) = self.client.send_async(request).await {
                        log::error!(
                            "WebDAV server URL should start with `https`. Please correct your \
                             configuration value. Its current value is `{}`.",
                            self.server_conf.url
                        );
                        break 'block s;
                    }
                }

                let err = Error::new(format!(
                    "Could not connect to WebDAV server endpoint for {}.\nError connecting to \
                     server: {}",
                    self.server_conf.url, err
                ))
                .set_source(Some(Box::new(err)));
                _ = self.online_status.set(None, Err(err.clone())).await;
                return Err(err);
            }
            Ok(s) => s,
        };
        let req_instant = Instant::now();

        if !resp.status().is_success() {
            let kind: crate::error::NetworkErrorKind = resp.status().into();
            let res_text = resp.text().await.unwrap_or_default();
            let mut err = Error::new(format!(
                "Could not connect to WebDAV server endpoint for {}. Reply from server: {}",
                self.server_conf.url, res_text
            ))
            .set_kind(kind.into());

            let mut retry = false;
            if resp.status() == 404 && self.server_conf.url.path() == "/" {
                // attempt recovery by trying "/.well-known/carddav"
                self.server_conf.url.set_path("/.well-known/carddav");
                let request = Request::builder()
                    .method("PROPFIND")
                    .uri(self.server_conf.url.as_str())
                    .body(())
                    .unwrap();
                if let Ok(s) = self.client.send_async(request).await {
                    if let Some(redirect) = s
                        .headers()
                        .get("location")
                        .and_then(|r| r.to_str().ok()?.try_into().ok())
                    {
                        self.server_conf.url = redirect;
                        let request = Request::builder()
                            .method("OPTIONS")
                            .uri(self.server_conf.url.as_str())
                            .body(())
                            .unwrap();
                        if let Ok(s) = self.client.send_async(request).await {
                            resp = s;
                            retry = true;
                        }
                    }
                }
            } else if resp.status() == 401 {
                let mut supports_bearer = false;
                let mut supports_basic = false;
                for val in resp
                    .headers()
                    .get_all(http::header::WWW_AUTHENTICATE)
                    .iter()
                {
                    supports_bearer |= val.as_bytes().contains_subsequence(b"Bearer".as_slice());
                    supports_basic |= val.as_bytes().contains_subsequence(b"Basic".as_slice());
                }
                match (self.server_conf.auth, supports_bearer, supports_basic) {
                    (WebDAVAuthentication::Basic, true, _) => {
                        err = err
                            .set_details(
                                "The server rejected your authentication credentials because it \
                                 expects authentication with a Bearer token instead of a \
                                 password. Check your provider's client connection documentation. \
                                 Note that to use Bearer token authentication, you must \
                                 explicitly set `use_token=true` in the account's configuration.",
                            )
                            .set_kind(ErrorKind::Authentication);
                    }
                    (WebDAVAuthentication::BearerToken, false, true) => {
                        err = err
                            .set_details(
                                "The server rejected your authentication credentials because it \
                                 expects authentication with a username and password but \
                                 `use_token` is set to `true`. Try setting it to `false`.",
                            )
                            .set_kind(ErrorKind::Authentication);
                    }
                    (auth_method, false, false) => {
                        let schemes = resp
                            .headers()
                            .get_all(http::header::WWW_AUTHENTICATE)
                            .iter()
                            .map(|val| String::from_utf8_lossy(val.as_bytes()))
                            .collect::<Vec<_>>();
                        if matches!(auth_method, WebDAVAuthentication::None) {
                            err = err
                                .set_details("The server requires authentication.")
                                .set_kind(ErrorKind::Authentication);
                        }
                        if schemes.is_empty() {
                            err = err
                                .set_details(
                                    "The server fails to report what authentication schemes it \
                                     supports. Please report this to your e-mail provider! (The \
                                     server is expected to provide the supported schemes with the \
                                     WWW-Authenticate HTTP header)",
                                )
                                .set_kind(ErrorKind::ProtocolError);
                        } else {
                            err = err
                                .set_details(format!(
                                    "The server does not support any of the implemented \
                                     authentication mechanisms (Basic or Bearer token). Here are \
                                     the authentication schemes it reports to support: {}",
                                    schemes.join(", ")
                                ))
                                .set_kind(ErrorKind::Authentication);
                        }
                    }
                    (WebDAVAuthentication::BearerToken, true, _)
                    | (WebDAVAuthentication::Basic, _, true) => {
                        err = err
                            .set_details(
                                "The server rejected your authentication credentials. Confirm you \
                                 are not using an invalid password or token value.",
                            )
                            .set_kind(ErrorKind::Authentication);
                    }
                    (WebDAVAuthentication::None, supports_basic, supports_bearer) => {
                        err = err
                            .set_details("The server requires authentication.")
                            .set_kind(ErrorKind::Authentication);
                        // Either basic or bearer token is support here, because the `false, false`
                        // pattern was caught in another match guard.
                        if supports_basic {
                            err = err.set_details(
                                "It supports HTTP Basic authentication (username and password).",
                            );
                        }
                        if supports_bearer {
                            err = err.set_details(
                                "It also supports authentication using a Bearer Token.",
                            );
                        }
                    }
                }
            }
            if !retry {
                _ = self
                    .online_status
                    .set(Some(req_instant), Err(err.clone()))
                    .await;
                return Err(err);
            }
        }

        let _res_text = match resp.text().await {
            Err(err) => {
                let err = Error::new(format!(
                    "Could not connect to WebDAV server endpoint for {}\nReply from server: {}",
                    self.server_conf.url, err
                ))
                .set_source(Some(Box::new(err)));
                _ = self
                    .online_status
                    .set(Some(req_instant), Err(err.clone()))
                    .await;
                return Err(err);
            }
            Ok(s) => s,
        };
        if resp.headers().get("dav").is_none() {
            let err = Error::new(format!(
                "WebDAV server {} did not reply to OPTIONS request with a `DAV` capability \
                 header.\nHeaders in server reply:\n{:#?}",
                self.server_conf.url,
                resp.headers()
            ))
            .set_kind(ErrorKind::ProtocolNotSupported);
            _ = self
                .online_status
                .set(Some(req_instant), Err(err.clone()))
                .await;
            return Err(err);
        };
        let allow_methods: IndexSet<Box<str>> = resp
            .headers()
            .get_all("allow")
            .iter()
            .map(|s| s.to_str().unwrap())
            .flat_map(|s| s.split(", "))
            .map(|s| s.into())
            .collect();
        let dav_capabilities = resp
            .headers()
            .get_all("dav")
            .iter()
            .map(|s| s.to_str().unwrap())
            .flat_map(|s| s.split(", "))
            .map(|s| s.into())
            .collect();
        if !allow_methods.contains("PROPFIND") {
            return Err(Error::new(format!(
                "WebDAV connection failed: the server does not support the `PROPFIND` HTTP \
                 method. The server replied with the following allowed methods in the `allow` \
                 http header: {:?}",
                allow_methods
            ))
            .set_kind(ErrorKind::ProtocolNotSupported));
        }
        let request = Request::builder()
            .method("PROPFIND")
            .uri(self.server_conf.url.as_str())
            .header(
                http::header::CONTENT_TYPE,
                "application/xml; content-type=utf-8",
            )
            .header("Depth", "0")
            .body(
                r#"<d:propfind xmlns:d="DAV:">
  <d:prop>
     <d:current-user-principal />
  </d:prop>
</d:propfind>"#,
            )
            .unwrap();
        let mut resp = self.client.send_async(request).await?;
        if !resp.status().is_success() {
            let kind: crate::error::NetworkErrorKind = resp.status().into();
            let res_text = resp.text().await.unwrap_or_default();
            let err = Error::new(format!(
                "Could not connect to WebDAV server endpoint for {}. Reply from server: {}",
                self.server_conf.url, res_text
            ))
            .set_kind(kind.into());

            _ = self
                .online_status
                .set(Some(req_instant), Err(err.clone()))
                .await;
            return Err(err);
        }

        log::trace!("propfind current user principal {:?}", resp);
        let text = resp.text().await?;
        let mut principals = vec![];
        {
            let multistatus: Multistatus<CurrentUserPrincipalProp> = quick_xml::de::from_str(&text)
                .chain_err_summary(|| "Could not deserialize PROPFIND response from XML")?;
            for response in multistatus.response {
                for propstat in response.propstat {
                    if let (data, Some(StatusCode::OK)) = (
                        propstat.prop.current_user_principal.href,
                        StatusCode::from_status_line(&propstat.status),
                    ) {
                        if !data.is_empty() {
                            principals.push(data);
                        }
                    }
                }
            }
        }
        if principals.is_empty() {
            return Err(Error::new(format!(
                "Could not connect to WebDAV server endpoint for {}",
                self.server_conf.url
            ))
            .set_details(format!(
                "Server returned no principals in PROPFIND response: {text:?}"
            ))
            .set_kind(ErrorKind::ProtocolError));
        }
        let principal = principals.remove(0);

        let session = WebDAVSession {
            allow_methods,
            dav_capabilities,
            date: resp
                .headers()
                .get("date")
                .and_then(|d| d.to_str().ok())
                .map(Into::into),
            server_id: resp
                .headers()
                .get("server")
                .and_then(|s| s.to_str().ok())
                .map(Into::into),
            principal,
        };
        _ = self
            .online_status
            .set(Some(Instant::now()), Ok(session))
            .await;

        Ok(())
    }

    pub async fn send_async<B>(&self, request: Request<B>) -> Result<http::Response<AsyncBody>>
    where
        B: Into<AsyncBody>,
    {
        let req_instant = Instant::now();
        let mut resp = self.client.send_async(request).await?;
        if !resp.status().is_success() {
            let kind: crate::error::NetworkErrorKind = resp.status().into();
            let res_text = resp.text().await.unwrap_or_default();
            let err = Error::new(format!(
                "Could not connect to WebDAV server endpoint for {}. Reply from server: {}",
                self.server_conf.url, res_text
            ))
            .set_kind(kind.into());

            _ = self
                .online_status
                .set(Some(req_instant), Err(err.clone()))
                .await;
            return Err(err);
        }
        self.online_status.update_timestamp(Some(req_instant)).await;
        Ok(resp)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Multistatus<Prop: std::fmt::Debug> {
    #[serde(rename = "response")]
    pub response: Vec<Response<Prop>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Response<Prop: std::fmt::Debug> {
    #[serde(rename = "href")]
    pub href: Box<str>,
    #[serde(rename = "propstat")]
    pub propstat: Vec<Propstat<Prop>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Propstat<Prop: std::fmt::Debug> {
    #[serde(rename = "status")]
    pub status: Box<str>,
    #[serde(rename = "prop")]
    pub prop: Prop,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CurrentUserPrincipalProp {
    #[serde(rename = "current-user-principal")]
    pub current_user_principal: HrefValue,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct HrefValue {
    #[serde(rename = "href")]
    pub href: Box<str>,
}

mod private {
    pub trait Sealed {}
}

/// Parse an HTTP protocol `Status-Line` grammar item into
/// `http::status::StatusCode`.
///
/// ```rust
/// use http::status::StatusCode;
/// use melib::utils::webdav::FromStatusLine;
///
/// assert_eq!(
///     StatusCode::from_status_line("HTTP/1.1 200 OK").unwrap(),
///     StatusCode::OK
/// );
/// assert_eq!(
///     StatusCode::from_status_line("HTTP/1.1 404 Not Found").unwrap(),
///     StatusCode::NOT_FOUND
/// );
/// ```
pub trait FromStatusLine: private::Sealed + Sized {
    /// Parse a `Status-Line` into `http::status::StatusCode`.
    ///
    /// `Status-Line` is defined in [RFC2068] [Section 6.1 "Status-Line"] as:
    ///
    /// ```text
    /// Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
    /// ```
    ///
    /// Reason phrase is ignored, because "The client is not required to examine
    /// or display the Reason-Phrase".
    ///
    /// [RFC2068]: https://www.rfc-editor.org/rfc/rfc2068
    /// [Section 6.1 "Status-Line"]: https://www.rfc-editor.org/rfc/rfc2068#section-6.1
    fn from_status_line(_: &str) -> Option<Self>;
}

impl private::Sealed for http::status::StatusCode {}

impl FromStatusLine for http::status::StatusCode {
    fn from_status_line(mut val: &str) -> Option<Self> {
        // Example value: "HTTP/1.1 200 OK"
        if !val.is_ascii() {
            return None;
        }
        if val.ends_with("\r\n") {
            val = val.trim_end_matches("\r\n");
        }
        let mut split_iter = val.split(' ');
        _ = split_iter.next()?;
        let status_code_s = split_iter.next()?;
        // Ignore Reason-Phrase
        // _ = split_iter.next()?;
        Self::from_bytes(status_code_s.as_bytes()).ok()
    }
}
