/*
 * meli - melib crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

/*! Compose a `Draft`, with MIME and attachment support */
use std::{
    convert::TryFrom,
    ffi::OsStr,
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

use data_encoding::BASE64_MIME;
use xdg_utils::query_mime_info;

use super::*;
use crate::{
    email::{
        attachment_types::{Charset, ContentTransferEncoding, ContentType, MultipartType},
        attachments::AttachmentBuilder,
    },
    utils::{datetime, shellexpand::ShellExpandTrait},
};

pub mod mime;
pub mod random;

//use self::mime::*;

use super::parser;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Draft {
    pub headers: HeaderMap,
    pub body: String,
    pub wrap_header_preamble: Option<(String, String)>,

    pub attachments: Vec<AttachmentBuilder>,
}

impl Default for Draft {
    fn default() -> Self {
        let mut headers = HeaderMap::default();
        headers.insert(
            HeaderName::DATE,
            datetime::timestamp_to_string(
                datetime::now(),
                Some(datetime::formats::RFC822_DATE),
                true,
            ),
        );
        headers.insert(HeaderName::FROM, "".into());
        headers.insert(HeaderName::TO, "".into());
        headers.insert(HeaderName::CC, "".into());
        headers.insert(HeaderName::BCC, "".into());
        headers.insert(HeaderName::SUBJECT, "".into());

        Draft {
            headers,
            body: String::new(),
            wrap_header_preamble: None,

            attachments: Vec::new(),
        }
    }
}

impl FromStr for Draft {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        if s.is_empty() {
            return Err(Error::new("Empty input in Draft::from_str"));
        }

        let (headers, _) = parser::mail(s.as_bytes())?;
        let mut ret = Draft::default();

        for (k, v) in headers {
            ret.headers
                .insert(k.try_into()?, String::from_utf8(v.to_vec())?);
        }
        let body = Envelope::new(EnvelopeHash::default()).body_bytes(s.as_bytes());

        ret.body = String::from_utf8(body.decode(Default::default()))?;

        Ok(ret)
    }
}

impl Draft {
    pub fn edit(envelope: &Envelope, bytes: &[u8]) -> Result<Self> {
        let mut ret = Draft::default();
        for (k, v) in envelope.headers(bytes).unwrap_or_else(|_| Vec::new()) {
            ret.headers.insert(k.try_into()?, v.into());
        }

        let body = envelope.body_bytes(bytes);
        ret.body = body.text();
        ret.attachments = body.attachments().into_iter().map(Into::into).collect();

        Ok(ret)
    }

    pub fn set_header(&mut self, header: HeaderName, value: String) -> &mut Self {
        self.headers.insert(header, value);
        self
    }

    pub fn try_set_header(
        &mut self,
        header: &str,
        value: String,
    ) -> std::result::Result<&mut Self, InvalidHeaderName> {
        self.headers.insert(HeaderName::try_from(header)?, value);
        Ok(self)
    }

    pub fn set_wrap_header_preamble(&mut self, value: Option<(String, String)>) -> &mut Self {
        self.wrap_header_preamble = value;
        self
    }

    pub fn update(&mut self, value: &str) -> Result<bool> {
        let mut value: std::borrow::Cow<'_, str> = value.into();
        if let Some((pre, post)) = self.wrap_header_preamble.as_ref() {
            let mut s = value.as_ref();
            s = s.strip_prefix(pre).unwrap_or(s);
            s = s.strip_prefix('\n').unwrap_or(s);

            if let Some(pos) = s.find(post) {
                let mut headers = &s[..pos];
                headers = headers.strip_suffix(post).unwrap_or(headers);
                if headers.ends_with('\n') {
                    headers = &headers[..headers.len() - 1];
                }
                value = format!(
                    "{headers}{body}",
                    headers = headers,
                    body = &s[pos + post.len()..]
                )
                .into();
            }
        }
        let new = Draft::from_str(value.as_ref())?;
        let changes: bool = self.headers != new.headers || self.body != new.body;
        self.headers = new.headers;
        self.body = new.body;
        Ok(changes)
    }

    pub fn new_reply(envelope: &Envelope, bytes: &[u8], reply_to_all: bool) -> Self {
        let mut ret = Draft::default();
        ret.headers_mut().insert(
            HeaderName::REFERENCES,
            format!(
                "{} {}",
                envelope
                    .references()
                    .iter()
                    .fold(String::new(), |mut acc, x| {
                        if !acc.is_empty() {
                            acc.push(' ');
                        }
                        acc.push_str(&x.to_string());
                        acc
                    }),
                envelope.message_id_display()
            ),
        );
        ret.headers_mut().insert(
            HeaderName::IN_REPLY_TO,
            envelope.message_id_display().into(),
        );
        // "Mail-Followup-To/(To+Cc+(Mail-Reply-To/Reply-To/From)) for follow-up,
        // Mail-Reply-To/Reply-To/From for reply-to-author."
        // source: https://cr.yp.to/proto/replyto.html
        if reply_to_all {
            if let Some(reply_to) = envelope.other_headers().get("Mail-Followup-To") {
                ret.headers_mut()
                    .insert(HeaderName::TO, reply_to.to_string());
            } else if let Some(reply_to) = envelope.other_headers().get("Reply-To") {
                ret.headers_mut()
                    .insert(HeaderName::TO, reply_to.to_string());
            } else {
                ret.headers_mut()
                    .insert(HeaderName::TO, envelope.field_from_to_string());
            }
            // FIXME: add To/Cc
        } else if let Some(reply_to) = envelope.other_headers().get("Mail-Reply-To") {
            ret.headers_mut()
                .insert(HeaderName::TO, reply_to.to_string());
        } else if let Some(reply_to) = envelope.other_headers().get("Reply-To") {
            ret.headers_mut()
                .insert(HeaderName::TO, reply_to.to_string());
        } else {
            ret.headers_mut()
                .insert(HeaderName::TO, envelope.field_from_to_string());
        }
        ret.headers_mut()
            .insert(HeaderName::CC, envelope.field_cc_to_string());
        let body = envelope.body_bytes(bytes);
        ret.body = {
            let reply_body_bytes = body.decode_rec(Default::default());
            let reply_body = String::from_utf8_lossy(&reply_body_bytes);
            let lines: Vec<&str> = reply_body.lines().collect();
            let mut ret = format!(
                "On {} {} wrote:\n",
                envelope.date_as_str(),
                &ret.headers()[HeaderName::TO]
            );
            for l in lines {
                ret.push('>');
                ret.push_str(l);
                ret.push('\n');
            }
            ret.pop();
            ret
        };

        ret
    }

    pub fn headers_mut(&mut self) -> &mut HeaderMap {
        &mut self.headers
    }

    pub fn headers(&self) -> &HeaderMap {
        &self.headers
    }

    pub fn attachments(&self) -> &Vec<AttachmentBuilder> {
        &self.attachments
    }

    pub fn attachments_mut(&mut self) -> &mut Vec<AttachmentBuilder> {
        &mut self.attachments
    }

    pub fn body(&self) -> &str {
        &self.body
    }

    pub fn set_body(&mut self, s: String) -> &mut Self {
        self.body = s;
        self
    }

    pub fn to_edit_string(&self) -> String {
        let mut ret = String::new();

        if let Some((pre, _)) = self.wrap_header_preamble.as_ref() {
            if !pre.is_empty() {
                ret.push_str(pre);
                if !pre.ends_with('\n') {
                    ret.push('\n');
                }
            }
        }

        for (k, v) in self.headers.deref() {
            ret.push_str(&format!("{}: {}\n", k, v));
        }

        if let Some((_, post)) = self.wrap_header_preamble.as_ref() {
            if !post.is_empty() {
                if !post.starts_with('\n') && !ret.ends_with('\n') {
                    ret.push('\n');
                }
                ret.push_str(post);
                ret.push('\n');
            }
        }

        ret.push('\n');
        ret.push_str(&self.body);

        ret
    }

    pub fn finalise(mut self) -> Result<String> {
        let mut ret = String::new();
        let has_from: bool = self.headers.contains_key("From");
        let has_msg_id: bool = self.headers.contains_key("Message-ID");
        let has_mime: bool = self.headers.contains_key("MIME-Version");
        let has_ctype: bool = self.headers.contains_key("Content-Type");
        let has_cte: bool = self.headers.contains_key("Content-Transfer-Encoding");

        if has_from && !has_msg_id {
            if let Ok((_, addr)) = super::parser::address::mailbox(self.headers["From"].as_bytes())
            {
                if let Some(fqdn) = addr.get_fqdn() {
                    self.headers
                        .insert(HeaderName::MESSAGE_ID, random::gen_message_id(&fqdn));
                }
            }
        }
        for (k, v) in self.headers.deref() {
            if v.is_ascii() {
                ret.push_str(&format!("{}: {}\r\n", k, v));
            } else {
                ret.push_str(&format!("{}: {}\r\n", k, mime::encode_header(v)));
            }
        }
        if !has_mime {
            ret.push_str("MIME-Version: 1.0\r\n");
        }

        if self.attachments.is_empty() {
            if !has_ctype {
                let content_type: ContentType = Default::default();
                let content_transfer_encoding: ContentTransferEncoding =
                    ContentTransferEncoding::_8Bit;
                ret.push_str(&format!(
                    "Content-Type: {}; charset=\"utf-8\"\r\n",
                    content_type
                ));
                if !has_cte {
                    ret.push_str(&format!(
                        "Content-Transfer-Encoding: {}\r\n",
                        content_transfer_encoding
                    ));
                }
            }
            ret.push_str("\r\n");
            for line in self.body.lines() {
                ret.push_str(line);
                ret.push_str("\r\n");
            }
        } else if self.body.is_empty() && self.attachments.len() == 1 {
            let attachment = std::mem::take(&mut self.attachments).remove(0);
            print_attachment(&mut ret, attachment);
        } else {
            let mut parts = Vec::with_capacity(self.attachments.len() + 1);
            let attachments = std::mem::take(&mut self.attachments);
            if !self.body.is_empty() {
                let mut body_attachment = AttachmentBuilder::default();
                body_attachment.set_raw(self.body.as_bytes().to_vec());
                parts.push(body_attachment);
            }
            parts.extend(attachments.into_iter());
            build_multipart(&mut ret, MultipartType::Mixed, &[], parts);
        }

        Ok(ret)
    }
}

fn build_multipart(
    ret: &mut String,
    kind: MultipartType,
    parameters: &[(Vec<u8>, Vec<u8>)],
    parts: Vec<AttachmentBuilder>,
) {
    let boundary = ContentType::make_boundary(&parts);
    ret.push_str(&format!(
        r#"Content-Type: {}; charset="utf-8"; boundary="{}""#,
        kind, boundary
    ));
    if kind == MultipartType::Encrypted {
        ret.push_str(r#"; protocol="application/pgp-encrypted""#);
    }
    for (n, v) in parameters {
        ret.push_str("; ");
        ret.push_str(&String::from_utf8_lossy(n));
        ret.push('=');
        if v.contains(&b' ') {
            ret.push('"');
        }
        ret.push_str(&String::from_utf8_lossy(v));
        if v.contains(&b' ') {
            ret.push('"');
        }
    }
    ret.push_str("\r\n\r\n");
    /* rfc1341 */
    ret.push_str(
        "This is a MIME formatted message with attachments. Use a MIME-compliant client to view \
         it properly.\r\n",
    );
    for sub in parts {
        ret.push_str("--");
        ret.push_str(&boundary);
        ret.push_str("\r\n");
        print_attachment(ret, sub);
    }
    ret.push_str("--");
    ret.push_str(&boundary);
    ret.push_str("--\r\n");
}

fn print_attachment(ret: &mut String, a: AttachmentBuilder) {
    use ContentType::*;
    match a.content_type {
        ContentType::Text {
            kind: crate::email::attachment_types::Text::Plain,
            charset: Charset::UTF8,
            parameters: ref v,
        } if v.is_empty() => {
            ret.push_str("\r\n");
            for line in String::from_utf8_lossy(a.raw()).lines() {
                ret.push_str(line);
                ret.push_str("\r\n");
            }
        }
        Text { .. } => {
            for line in a.build().into_raw().lines() {
                ret.push_str(line);
                ret.push_str("\r\n");
            }
        }
        Multipart {
            boundary: _,
            kind,
            parts,
            parameters,
        } => {
            build_multipart(
                ret,
                kind,
                &parameters,
                parts
                    .into_iter()
                    .map(|s| s.into())
                    .collect::<Vec<AttachmentBuilder>>(),
            );
        }
        MessageRfc822 => {
            ret.push_str(&format!(
                "Content-Type: {}; charset=\"utf-8\"\r\n",
                a.content_type
            ));
            ret.push_str("Content-Disposition: attachment\r\n");
            ret.push_str("\r\n");
            for line in String::from_utf8_lossy(a.raw()).lines() {
                ret.push_str(line);
                ret.push_str("\r\n");
            }
        }
        PGPSignature => {
            ret.push_str(&format!(
                "Content-Type: {}; charset=\"utf-8\"; name=\"signature.asc\"\r\n",
                a.content_type
            ));
            ret.push_str("Content-Description: Digital signature\r\n");
            ret.push_str("Content-Disposition: inline\r\n");
            ret.push_str("\r\n");
            for line in String::from_utf8_lossy(a.raw()).lines() {
                ret.push_str(line);
                ret.push_str("\r\n");
            }
        }
        _ => {
            let content_transfer_encoding: ContentTransferEncoding = if a.raw().is_ascii() {
                ContentTransferEncoding::_8Bit
            } else {
                ContentTransferEncoding::Base64
            };
            if let Some(name) = a.content_type().name() {
                ret.push_str(&format!(
                    "Content-Type: {}; name=\"{}\"; charset=\"utf-8\"\r\n",
                    a.content_type(),
                    name
                ));
            } else {
                ret.push_str(&format!(
                    "Content-Type: {}; charset=\"utf-8\"\r\n",
                    a.content_type()
                ));
            }
            ret.push_str("Content-Disposition: attachment\r\n");
            ret.push_str(&format!(
                "Content-Transfer-Encoding: {}\r\n",
                content_transfer_encoding
            ));
            ret.push_str("\r\n");
            if content_transfer_encoding == ContentTransferEncoding::Base64 {
                for line in BASE64_MIME.encode(a.raw()).trim().lines() {
                    ret.push_str(line);
                    ret.push_str("\r\n");
                }
            } else {
                for line in String::from_utf8_lossy(a.raw()).lines() {
                    ret.push_str(line);
                    ret.push_str("\r\n");
                }
            }
        }
    }
}

/// Reads file from given path, and returns an 'application/octet-stream'
/// AttachmentBuilder object
pub fn attachment_from_file<I>(path: &I) -> Result<AttachmentBuilder>
where
    I: AsRef<OsStr>,
{
    let path: PathBuf = Path::new(path).expand();
    if !path.is_file() {
        return Err(Error::new(format!("{} is not a file", path.display())));
    }

    let mut file = std::fs::File::open(&path)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    let mut attachment = AttachmentBuilder::default();

    attachment
        .set_raw(contents)
        .set_body_to_raw()
        .set_content_type(ContentType::Other {
            name: path.file_name().map(|s| s.to_string_lossy().into()),
            tag: if let Ok(mime_type) = query_mime_info(&path) {
                mime_type
            } else {
                b"application/octet-stream".to_vec()
            },
            parameters: vec![],
        });

    Ok(attachment)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_new_draft() {
        let mut default = Draft::default();
        assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
        default.set_body("αδφαφσαφασ".to_string());
        assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
        default.set_body("ascii only".to_string());
        assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
    }

    #[test]
    fn test_draft_update() {
        let mut default = Draft::default();
        default
            .set_wrap_header_preamble(Some(("<!--".to_string(), "-->".to_string())))
            .set_body("αδφαφσαφασ".to_string())
            .set_header(HeaderName::SUBJECT, "test_update()".into())
            .set_header(HeaderName::DATE, "Sun, 16 Jun 2013 17:56:45 +0200".into());

        let original = default.clone();
        let s = default.to_edit_string();
        assert_eq!(
            s,
            "<!--\nDate: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
             test_update()\n-->\n\nαδφαφσαφασ"
        );
        assert!(!default.update(&s).unwrap());
        assert_eq!(&original, &default);

        default.set_wrap_header_preamble(Some(("".to_string(), "".to_string())));
        let original = default.clone();
        let s = default.to_edit_string();
        assert_eq!(
            s,
            "Date: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
             test_update()\n\nαδφαφσαφασ"
        );
        assert!(!default.update(&s).unwrap());
        assert_eq!(&original, &default);

        default.set_wrap_header_preamble(None);
        let original = default.clone();
        let s = default.to_edit_string();
        assert_eq!(
            s,
            "Date: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
             test_update()\n\nαδφαφσαφασ"
        );
        assert!(!default.update(&s).unwrap());
        assert_eq!(&original, &default);

        default.set_wrap_header_preamble(Some((
            "{-\n\n\n===========".to_string(),
            "</mixed>".to_string(),
        )));
        let original = default.clone();
        let s = default.to_edit_string();
        assert_eq!(
            s,
            "{-\n\n\n===========\nDate: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \
             \nSubject: test_update()\n</mixed>\n\nαδφαφσαφασ"
        );
        assert!(!default.update(&s).unwrap());
        assert_eq!(&original, &default);

        default
            .set_body(
                "hellohello<!--\n<!--\n<--hellohello\nhellohello-->\n-->\n-->hello\n".to_string(),
            )
            .set_wrap_header_preamble(Some(("<!--".to_string(), "-->".to_string())));
        let original = default.clone();
        let s = default.to_edit_string();
        #[rustfmt::skip]
        assert_eq!(
            s,
            "<!--\nDate: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
             test_update()\n-->\n\nhellohello<!--\n<!--\n<--hellohello\nhellohello-->\n-->\n-->hello\n"
        );
        assert!(!default.update(&s).unwrap());
        assert_eq!(&original, &default);
    }

    /*
    #[test]
    fn test_attachments() {
        let mut default = Draft::default();
        default.set_body("αδφαφσαφασ".to_string());

        let mut file = std::fs::File::open("file path").unwrap();
        let mut contents = Vec::new();
        file.read_to_end(&mut contents).unwrap();

        let mut attachment = AttachmentBuilder::new(b"");
        attachment
            .set_raw(contents)
            .set_content_type(ContentType::Other {
                name: Some("images.jpeg".to_string()),
                tag: b"image/jpeg".to_vec(),
            })
            .set_content_transfer_encoding(ContentTransferEncoding::Base64);
        default.attachments_mut().push(attachment);
        println!("{}", default.finalise().unwrap());
    }
        */
}
