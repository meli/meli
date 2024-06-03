//
// meli
//
// Copyright 2026 - Manos Pitsidianakis
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

//! URI Template (level 1) format `RFC6570` values

use std::sync::Arc;

use serde::ser::{Serialize, Serializer};
use url::Url;

use crate::{
    email::parser::BytesExt,
    error::{Error, ErrorKind, Result},
    jmap::objects::{Account, BlobObject, Id},
};

#[derive(Clone, Debug)]
pub struct RequestUrlTemplate {
    pub text: String,
    pub url: Url,
}

impl std::fmt::Display for RequestUrlTemplate {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.text, fmt)
    }
}

impl Serialize for RequestUrlTemplate {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.text)
    }
}

impl<'de> ::serde::de::Deserialize<'de> for RequestUrlTemplate {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: ::serde::de::Deserializer<'de>,
    {
        use serde::de::{Error, Unexpected, Visitor};

        struct _Visitor;

        impl Visitor<'_> for _Visitor {
            type Value = RequestUrlTemplate;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string representing an URL")
            }

            fn visit_str<E>(self, s: &str) -> std::result::Result<Self::Value, E>
            where
                E: Error,
            {
                let url = Url::parse(s).map_err(|err| {
                    let err_s = format!("{err}");
                    Error::invalid_value(Unexpected::Str(s), &err_s.as_str())
                })?;
                let text = s.to_string();
                Ok(RequestUrlTemplate { text, url })
            }
        }

        deserializer.deserialize_str(_Visitor)
    }
}

macro_rules! format_url {
    ($self:expr,
     [$(($field:ident: $lit:literal)),*$(,)?],
     opt [$(($opt_field:ident: $opt_lit:literal)),*$(,)?],
     $details_fn:expr) => {{
        let mut ret = String::new();
        let mut prev_pos = 0;

        while let Some(pos) = $self.text.as_bytes()[prev_pos..].find(b"{") {
            ret.push_str(&$self.text[prev_pos..prev_pos + pos]);
            prev_pos += pos;
            $({
                if $self.text[prev_pos..].starts_with($lit) {
                    ret.push_str($field.into());
                    prev_pos += $lit.len();
                    continue;
                }
            })*
            $({
                if $self.text[prev_pos..].starts_with($opt_lit) {
                    ret.push_str($opt_field.as_deref().unwrap_or(""));
                    prev_pos += $opt_lit.len();
                    continue;
                }
            })*
            log::error!(
                "BUG: unknown parameter in {}: {}",
                stringify!($self), &$self.text[prev_pos..]
            );
            return Err(
                Error::new("Could not instantiate URL from JMAP server's URL template value")
                .set_details($details_fn(&$self.text[prev_pos..]))
                .set_kind(ErrorKind::ProtocolError)
            );
        }

        if prev_pos != $self.text.len() {
            ret.push_str(&$self.text[prev_pos..]);
        }
        ret
    }}
}

pub fn download_request_format(
    download_url: &RequestUrlTemplate,
    account_id: &Id<Account>,
    blob_id: &Id<BlobObject>,
    name: Option<String>,
) -> Result<Url> {
    let r#type = "application/octet-stream";
    #[expect(clippy::literal_string_with_formatting_args)]
    let ret = format_url!(download_url,
        [
        (account_id: "{accountId}"),
        (blob_id: "{blobId}"),
        (r#type: "{type}"),
        ],
        opt [
        (name: "{name}"),
        ],
        |rest| {
            format!(
                "`download_url` template returned by server in session object could not be \
                     instantiated with `accountId`:\ndownload_url: {}\naccountId: {}\nblobId: \
                     {}\nUnknown parameter found {}\n\nIf you believe these values are correct and \
                     should have been accepted, please report it as a bug! Otherwise inform the \
                     server administrator for this protocol violation.",
                     download_url.text,
                     account_id,
                     blob_id,
                     rest
            )
        }
    );
    Url::parse(&ret).map_err(|err| {
        Error::new("Could not instantiate URL from JMAP server's URL template value")
            .set_details(format!(
                "`download_url` template returned by server in session object could not be \
                 instantiated with `accountId`:\ndownload_url: {}\naccountId: {}\nblobId: \
                 {}\nResult was {ret}\n\nIf you believe these values are correct and should have \
                 been accepted, please report it as a bug! Otherwise inform the server \
                 administrator for this protocol violation.",
                download_url.text, account_id, blob_id,
            ))
            .set_kind(ErrorKind::ProtocolError)
            .set_source(Some(Arc::new(err)))
    })
}

pub fn upload_request_format(
    upload_url: &RequestUrlTemplate,
    account_id: &Id<Account>,
) -> Result<Url> {
    let ret = format_url!(upload_url,
            [
                (account_id: "{accountId}"),
            ],
            opt [],
            |rest| {
    format!(
                    "`upload_url` template returned by server in session object could not be \
                     instantiated with `accountId`:\nupload_url: {}\naccountId: {}\nUnknown parameter: \
                     {rest}\n\nIf you believe these values are correct and should have been accepted, \
                     please report it as a bug! Otherwise inform the server administrator for this \
                     protocol violation.",
                    upload_url.text, account_id
                )
            }
        );

    Url::parse(&ret).map_err(|err| {
        Error::new("Could not instantiate URL from JMAP server's URL template value")
            .set_details(format!(
                "`upload_url` template returned by server in session object could not be \
                 instantiated with `accountId`:\nupload_url: {}\naccountId: {}\nresult: \
                 {ret}\n\nIf you believe these values are correct and should have been accepted, \
                 please report it as a bug! Otherwise inform the server administrator for this \
                 protocol violation.",
                upload_url.text, account_id
            ))
            .set_kind(ErrorKind::ProtocolError)
            .set_source(Some(Arc::new(err)))
    })
}

pub fn event_source_request_format(
    event_source_url: &RequestUrlTemplate,
    types: &str,
    closeafter: &str,
    ping: &str,
) -> Result<Url> {
    #[expect(clippy::literal_string_with_formatting_args)]
    let ret = format_url!(event_source_url,
            [
                (types: "{types}"),
                (closeafter: "{closeafter}"),
                (ping: "{ping}"),
            ],
            opt [],
            |rest| {
    format!(
                    "`event_source_url` template returned by server in session object could not be \
                     instantiated:\nupload_url: {event_source_url}\ntypes: {types}\ncloseafter: {closeafter}\nping: {ping}\nUnknown parameter: \
                     {rest}\n\nIf you believe these values are correct and should have been accepted, \
                     please report it as a bug! Otherwise inform the server administrator for this \
                     protocol violation.",
                )
            }
        );

    Url::parse(&ret).map_err(|err| {
        Error::new("Could not instantiate URL from JMAP server's URL template value")
            .set_details(format!(
                "`event_source_url` template returned by server in session object could not be \
                 instantiated:\nupload_url: {event_source_url}\ntypes: {types}\ncloseafter: \
                 {closeafter}\nping: {ping}\nResult: {ret}\n\nIf you believe these values are \
                 correct and should have been accepted, please report it as a bug! Otherwise \
                 inform the server administrator for this protocol violation.",
            ))
            .set_kind(ErrorKind::ProtocolError)
            .set_source(Some(Arc::new(err)))
    })
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use url::Url;

    use crate::jmap::{
        objects::{Account, BlobObject, Id},
        url_template::{
            download_request_format, event_source_request_format, upload_request_format,
            RequestUrlTemplate,
        },
    };

    #[test]
    fn test_jmap_url_template() {
        assert_eq!(
            event_source_request_format(
                &serde_json::from_value::<RequestUrlTemplate>(json!(
                    "https://example.com/jmap/event/"
                ))
                .unwrap(),
                "Email,CalendarEvent",
                "state",
                "300"
            )
            .unwrap(),
            serde_json::from_str::<Url>(&json!("https://example.com/jmap/event/").to_string())
                .unwrap()
        );

        assert_eq!(
            event_source_request_format(
                &serde_json::from_value::<RequestUrlTemplate>(json!(
                        "https://jmap.example.com/eventsource/?types={types}&closeafter={closeafter}&ping={ping}"
                ))
                .unwrap(),
                "Email,CalendarEvent",
                "state",
                "300"
            )
            .unwrap(),
            serde_json::from_str::<Url>(&json!("https://jmap.example.com/eventsource/?types=Email,CalendarEvent&closeafter=state&ping=300").to_string())
            .unwrap()
        );
    }

    #[test]
    fn test_jmap_url_template_upload() {
        let account_id: Id<Account> = "blahblah".into();
        assert_eq!(
            upload_request_format(
                &serde_json::from_value(json!(r"https://jmap.example.com/upload/{accountId}/"))
                    .unwrap(),
                &account_id
            )
            .unwrap(),
            serde_json::from_str::<Url>(
                &json!("https://jmap.example.com/upload/blahblah/").to_string()
            )
            .unwrap()
        );
    }

    #[test]
    fn test_jmap_url_template_download() {
        let download_url_text =
            r"http://localhost/download/{accountId}/{blobId}/{name}?accept={type}";
        let download_url: RequestUrlTemplate =
            serde_json::from_value(json!(download_url_text)).unwrap();
        assert_eq!(download_url.text, download_url_text);

        let account_id: Id<Account> = "blahblah".into();
        let blob_id: Id<BlobObject> = Id::from("683f9246-56d4-4d7d-bd0c-3d4de6db7cbf");
        assert_eq!(
            download_request_format(&download_url, &account_id, &blob_id, None),
            Ok(
                Url::parse(r"http://localhost/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/?accept=application/octet-stream")
                    .unwrap()
            )
        );

        let download_template_url: RequestUrlTemplate = serde_json::from_value(json!(
            r"https://jmap.example.com/download/{accountId}/{blobId}/{name}"
        ))
        .unwrap();
        assert_eq!(
            download_request_format(
                &download_template_url,
                &account_id,
                &blob_id,
                Some("attachment.txt".into())
            )
            .unwrap(),
            serde_json::from_str::<Url>(
                &json!("https://jmap.example.com/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/attachment.txt").to_string()
            )
            .unwrap()
        );
        assert_eq!(
            download_request_format(
                &download_template_url,
                &account_id,
                &blob_id,
                Some("attachment filename.txt".into()),
            )
            .unwrap(),
            serde_json::from_str::<Url>(
                &json!("https://jmap.example.com/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/attachment%20filename.txt").to_string()
            )
            .unwrap()
        );

        let download_template_url: RequestUrlTemplate = serde_json::from_value(json!(
            r"https://www.example.com/jmap/download/{accountId}/{blobId}/{name}?type={type}"
        ))
        .unwrap();
        assert_eq!(
            download_request_format(
                &download_template_url,
                &account_id,
                &blob_id,
                Some("attachment.txt".into())
            )
            .unwrap(),
            serde_json::from_str::<Url>(
                &json!("https://www.example.com/jmap/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/attachment.txt?type=application/octet-stream").to_string()
            )
            .unwrap()
        );
        assert_eq!(
            download_request_format(
                &download_template_url,
                &account_id,
                &blob_id,
                None,
            )
            .unwrap(),
            serde_json::from_str::<Url>(
                &json!("https://www.example.com/jmap/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/?type=application/octet-stream").to_string()
            )
            .unwrap()
        );
    }
}
