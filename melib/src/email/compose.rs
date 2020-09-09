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

use super::*;
use crate::email::attachment_types::{
    Charset, ContentTransferEncoding, ContentType, MultipartType,
};
use crate::email::attachments::{decode, decode_rec, AttachmentBuilder};
use crate::shellexpand::ShellExpandTrait;
use data_encoding::BASE64_MIME;
use std::ffi::OsStr;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str;

pub mod mime;
pub mod random;

//use self::mime::*;

use super::parser;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Draft {
    pub headers: HeaderMap,
    pub body: String,

    pub attachments: Vec<AttachmentBuilder>,
}

impl Default for Draft {
    fn default() -> Self {
        let mut headers = HeaderMap::default();
        headers.insert(
            HeaderName::new_unchecked("Date"),
            crate::datetime::timestamp_to_string(crate::datetime::now(), None),
        );
        headers.insert(HeaderName::new_unchecked("From"), "".into());
        headers.insert(HeaderName::new_unchecked("To"), "".into());
        headers.insert(HeaderName::new_unchecked("Cc"), "".into());
        headers.insert(HeaderName::new_unchecked("Bcc"), "".into());
        headers.insert(HeaderName::new_unchecked("Subject"), "".into());

        Draft {
            headers,
            body: String::new(),

            attachments: Vec::new(),
        }
    }
}

impl str::FromStr for Draft {
    type Err = MeliError;
    fn from_str(s: &str) -> Result<Self> {
        if s.is_empty() {
            return Err(MeliError::new("Empty input in Draft::from_str"));
        }

        let (headers, _) = parser::mail(s.as_bytes())?;
        let mut ret = Draft::default();

        for (k, v) in headers {
            ret.headers
                .insert(k.try_into()?, String::from_utf8(v.to_vec())?);
        }
        let body = Envelope::new(0).body_bytes(s.as_bytes());

        ret.body = String::from_utf8(decode(&body, None))?;

        Ok(ret)
    }
}

impl Draft {
    pub fn edit(envelope: &Envelope, bytes: &[u8]) -> Result<Self> {
        let mut ret = Draft::default();
        for (k, v) in envelope.headers(&bytes).unwrap_or_else(|_| Vec::new()) {
            ret.headers.insert(k.try_into()?, v.into());
        }

        ret.body = envelope.body_bytes(bytes).text();

        Ok(ret)
    }

    pub fn set_header(&mut self, header: &str, value: String) -> &mut Self {
        self.headers
            .insert(HeaderName::new_unchecked(header), value);
        self
    }

    pub fn new_reply(envelope: &Envelope, bytes: &[u8], reply_to_all: bool) -> Self {
        let mut ret = Draft::default();
        ret.headers_mut().insert(
            HeaderName::new_unchecked("References"),
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
            HeaderName::new_unchecked("In-Reply-To"),
            envelope.message_id_display().into(),
        );
        // "Mail-Followup-To/(To+Cc+(Mail-Reply-To/Reply-To/From)) for follow-up,
        // Mail-Reply-To/Reply-To/From for reply-to-author."
        // source: https://cr.yp.to/proto/replyto.html
        if reply_to_all {
            if let Some(reply_to) = envelope.other_headers().get("Mail-Followup-To") {
                ret.headers_mut()
                    .insert(HeaderName::new_unchecked("To"), reply_to.to_string());
            } else {
                if let Some(reply_to) = envelope.other_headers().get("Reply-To") {
                    ret.headers_mut()
                        .insert(HeaderName::new_unchecked("To"), reply_to.to_string());
                } else {
                    ret.headers_mut().insert(
                        HeaderName::new_unchecked("To"),
                        envelope.field_from_to_string(),
                    );
                }
                // FIXME: add To/Cc
            }
        } else {
            if let Some(reply_to) = envelope.other_headers().get("Mail-Reply-To") {
                ret.headers_mut()
                    .insert(HeaderName::new_unchecked("To"), reply_to.to_string());
            } else if let Some(reply_to) = envelope.other_headers().get("Reply-To") {
                ret.headers_mut()
                    .insert(HeaderName::new_unchecked("To"), reply_to.to_string());
            } else {
                ret.headers_mut().insert(
                    HeaderName::new_unchecked("To"),
                    envelope.field_from_to_string(),
                );
            }
        }
        ret.headers_mut().insert(
            HeaderName::new_unchecked("Cc"),
            envelope.field_cc_to_string(),
        );
        let body = envelope.body_bytes(bytes);
        ret.body = {
            let reply_body_bytes = decode_rec(&body, None);
            let reply_body = String::from_utf8_lossy(&reply_body_bytes);
            let lines: Vec<&str> = reply_body.lines().collect();
            let mut ret = format!(
                "On {} {} wrote:\n",
                envelope.date_as_str(),
                &ret.headers()["To"]
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

    pub fn to_string(&self) -> Result<String> {
        let mut ret = String::new();

        for (k, v) in self.headers.deref() {
            ret.extend(format!("{}: {}\n", k, v).chars());
        }

        ret.push('\n');
        ret.push_str(&self.body);

        Ok(ret)
    }

    pub fn finalise(mut self) -> Result<String> {
        let mut ret = String::new();

        if self.headers.contains_key("From") && !self.headers.contains_key("Message-ID") {
            if let Ok((_, addr)) = super::parser::address::mailbox(self.headers["From"].as_bytes())
            {
                if let Some(fqdn) = addr.get_fqdn() {
                    self.headers.insert(
                        HeaderName::new_unchecked("Message-ID"),
                        random::gen_message_id(&fqdn),
                    );
                }
            }
        }
        for (k, v) in self.headers.deref() {
            if v.is_ascii() {
                ret.extend(format!("{}: {}\n", k, v).chars());
            } else {
                ret.extend(format!("{}: {}\n", k, mime::encode_header(v)).chars());
            }
        }
        ret.push_str("MIME-Version: 1.0\n");

        if self.attachments.is_empty() {
            let content_type: ContentType = Default::default();
            let content_transfer_encoding: ContentTransferEncoding = ContentTransferEncoding::_8Bit;
            ret.extend(format!("Content-Type: {}; charset=\"utf-8\"\n", content_type).chars());
            ret.extend(
                format!("Content-Transfer-Encoding: {}\n", content_transfer_encoding).chars(),
            );
            ret.push('\n');
            ret.push_str(&self.body);
        } else if self.body.is_empty() && self.attachments.len() == 1 {
            let attachment = std::mem::replace(&mut self.attachments, Vec::new()).remove(0);
            print_attachment(&mut ret, &Default::default(), attachment);
        } else {
            let mut parts = Vec::with_capacity(self.attachments.len() + 1);
            let attachments = std::mem::replace(&mut self.attachments, Vec::new());
            if !self.body.is_empty() {
                let mut body_attachment = AttachmentBuilder::default();
                body_attachment.set_raw(self.body.as_bytes().to_vec());
                parts.push(body_attachment);
            }
            parts.extend(attachments.into_iter());
            build_multipart(&mut ret, MultipartType::Mixed, parts);
        }

        Ok(ret)
    }
}

fn build_multipart(ret: &mut String, kind: MultipartType, parts: Vec<AttachmentBuilder>) {
    let boundary = ContentType::make_boundary(&parts);
    ret.extend(
        format!(
            "Content-Type: {}; charset=\"utf-8\"; boundary=\"{}\"\n",
            kind, boundary
        )
        .chars(),
    );
    ret.push('\n');
    /* rfc1341 */
    ret.push_str("This is a MIME formatted message with attachments. Use a MIME-compliant client to view it properly.\n");
    for sub in parts {
        ret.push_str("--");
        ret.push_str(&boundary);
        ret.push('\n');
        print_attachment(ret, &kind, sub);
    }
    ret.push_str("--");
    ret.push_str(&boundary);
    ret.push_str("--\n");
}

fn print_attachment(ret: &mut String, kind: &MultipartType, a: AttachmentBuilder) {
    use ContentType::*;
    match a.content_type {
        ContentType::Text {
            kind: crate::email::attachment_types::Text::Plain,
            charset: Charset::UTF8,
            parameters: ref v,
        } if v.is_empty() => {
            ret.push('\n');
            ret.push_str(&String::from_utf8_lossy(a.raw()));
            ret.push('\n');
        }
        Text { .. } => {
            ret.push_str(&a.build().into_raw());
            ret.push('\n');
        }
        Multipart {
            boundary: _boundary,
            kind,
            parts: subparts,
        } => {
            build_multipart(
                ret,
                kind,
                subparts
                    .into_iter()
                    .map(|s| s.into())
                    .collect::<Vec<AttachmentBuilder>>(),
            );
            ret.push('\n');
        }
        MessageRfc822 | PGPSignature => {
            ret.push_str(&format!("Content-Type: {}; charset=\"utf-8\"\n", kind));
            ret.push('\n');
            ret.push_str(&String::from_utf8_lossy(a.raw()));
            ret.push('\n');
        }
        _ => {
            let content_transfer_encoding: ContentTransferEncoding =
                ContentTransferEncoding::Base64;
            if let Some(name) = a.content_type().name() {
                ret.extend(
                    format!(
                        "Content-Type: {}; name=\"{}\"; charset=\"utf-8\"\n",
                        a.content_type(),
                        name
                    )
                    .chars(),
                );
            } else {
                ret.extend(
                    format!("Content-Type: {}; charset=\"utf-8\"\n", a.content_type()).chars(),
                );
            }
            ret.extend(
                format!("Content-Transfer-Encoding: {}\n", content_transfer_encoding).chars(),
            );
            ret.push('\n');
            ret.push_str(&BASE64_MIME.encode(a.raw()).trim());
            ret.push('\n');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_new() {
        let mut default = Draft::default();
        assert_eq!(
            Draft::from_str(&default.to_string().unwrap()).unwrap(),
            default
        );
        default.set_body("αδφαφσαφασ".to_string());
        assert_eq!(
            Draft::from_str(&default.to_string().unwrap()).unwrap(),
            default
        );
        default.set_body("ascii only".to_string());
        assert_eq!(
            Draft::from_str(&default.to_string().unwrap()).unwrap(),
            default
        );
    }

    #[test]
    fn test_attachments() {
        /*
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
        */
    }
}

/// Reads file from given path, and returns an 'application/octet-stream' AttachmentBuilder object
pub fn attachment_from_file<I>(path: &I) -> Result<AttachmentBuilder>
where
    I: AsRef<OsStr>,
{
    let path: PathBuf = Path::new(path).expand();
    if !path.is_file() {
        return Err(MeliError::new(format!("{} is not a file", path.display())));
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
            tag: b"application/octet-stream".to_vec(),
        });

    Ok(attachment)
}
