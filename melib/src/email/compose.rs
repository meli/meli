use super::*;
use crate::backends::BackendOp;
use chrono::{DateTime, Local};
use data_encoding::BASE64_MIME;
use std::str;

mod mime;
mod random;

//use self::mime::*;

use super::parser;
use fnv::FnvHashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Draft {
    headers: FnvHashMap<String, String>,
    header_order: Vec<String>,
    body: String,

    attachments: Vec<Attachment>,
}

impl Default for Draft {
    fn default() -> Self {
        let mut headers = FnvHashMap::with_capacity_and_hasher(8, Default::default());
        let mut header_order = Vec::with_capacity(8);
        headers.insert("From".into(), "".into());
        headers.insert("To".into(), "".into());
        headers.insert("Cc".into(), "".into());
        headers.insert("Bcc".into(), "".into());

        let now: DateTime<Local> = Local::now();
        headers.insert("Date".into(), now.to_rfc2822());
        headers.insert("Subject".into(), "".into());
        headers.insert("User-Agent".into(), "meli 0.0".into());
        header_order.push("Date".into());
        header_order.push("From".into());
        header_order.push("To".into());
        header_order.push("Cc".into());
        header_order.push("Bcc".into());
        header_order.push("Subject".into());
        header_order.push("User-Agent".into());
        Draft {
            headers,
            header_order,
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

        let (headers, _) = parser::mail(s.as_bytes()).to_full_result()?;
        let mut ret = Draft::default();

        for (k, v) in headers {
            if ignore_header(k) {
                continue;
            }
            if ret
                .headers
                .insert(
                    String::from_utf8(k.to_vec())?,
                    String::from_utf8(v.to_vec())?,
                )
                .is_none()
            {
                ret.header_order.push(String::from_utf8(k.to_vec())?);
            }
        }
        if ret.headers.contains_key("From") && !ret.headers.contains_key("Message-ID") {
            if let super::parser::IResult::Done(_, addr) =
                super::parser::mailbox(ret.headers["From"].as_bytes())
            {
                if let Some(fqdn) = addr.get_fqdn() {
                    if ret
                        .headers
                        .insert("Message-ID".into(), random::gen_message_id(&fqdn))
                        .is_none()
                    {
                        let pos = ret
                            .header_order
                            .iter()
                            .position(|h| h == "Subject")
                            .unwrap();
                        ret.header_order.insert(pos, "Message-ID".into());
                    }
                }
            }
        }

        let body = Envelope::new(0).body_bytes(s.as_bytes());

        ret.body = String::from_utf8(decode(&body, None))?;

        //ret.attachments = body.attachments();

        Ok(ret)
    }
}

impl Draft {
    pub fn edit(envelope: &Envelope, mut op: Box<BackendOp>) -> Self {
        let mut ret = Draft::default();
        //TODO: Inform user if error
        {
            let bytes = op.as_bytes().unwrap_or(&[]);
            for (k, v) in envelope.headers(bytes).unwrap_or_else(|_| Vec::new()) {
                if ignore_header(k.as_bytes()) {
                    continue;
                }
                if ret.headers.insert(k.into(), v.into()).is_none() {
                    ret.header_order.push(k.into());
                }
            }
        }

        ret.body = envelope.body(op).text();

        ret
    }
    pub fn set_header(&mut self, header: &str, value: String) {
        if self.headers.insert(header.to_string(), value).is_none() {
            self.header_order.push(header.to_string());
        }
    }
    pub fn new_reply(envelope: &Envelope, bytes: &[u8]) -> Self {
        let mut ret = Draft::default();
        ret.headers_mut().insert(
            "References".into(),
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
        ret.header_order.push("References".into());
        ret.headers_mut()
            .insert("In-Reply-To".into(), envelope.message_id_display().into());
        ret.header_order.push("In-Reply-To".into());
        if let Some(reply_to) = envelope.other_headers().get("Reply-To") {
            ret.headers_mut().insert("To".into(), reply_to.to_string());
        } else {
            ret.headers_mut()
                .insert("To".into(), envelope.field_from_to_string());
        }
        ret.headers_mut()
            .insert("Cc".into(), envelope.field_cc_to_string());
        let body = envelope.body_bytes(bytes);
        ret.body = {
            let reply_body_bytes = decode_rec(&body, None);
            let reply_body = String::from_utf8_lossy(&reply_body_bytes);
            let lines: Vec<&str> = reply_body.lines().collect();
            let mut ret = String::with_capacity(reply_body.len() + lines.len());
            for l in lines {
                ret.push('>');
                ret.push(' ');
                ret.push_str(l.trim());
                ret.push('\n');
            }
            ret.pop();
            ret
        };

        ret
    }

    pub fn headers_mut(&mut self) -> &mut FnvHashMap<String, String> {
        &mut self.headers
    }

    pub fn headers(&self) -> &FnvHashMap<String, String> {
        &self.headers
    }

    pub fn body(&self) -> &str {
        &self.body
    }

    pub fn set_body(&mut self, s: String) {
        self.body = s;
    }

    pub fn to_string(&self) -> Result<String> {
        let mut ret = String::new();

        for k in &self.header_order {
            let v = &self.headers[k];
            ret.extend(format!("{}: {}\n", k, v).chars());
        }

        ret.push('\n');
        ret.push_str(&self.body);

        Ok(ret)
    }

    pub fn finalise(mut self) -> Result<String> {
        let mut ret = String::new();

        if self.headers.contains_key("From") && !self.headers.contains_key("Message-ID") {
            if let super::parser::IResult::Done(_, addr) =
                super::parser::mailbox(self.headers["From"].as_bytes())
            {
                if let Some(fqdn) = addr.get_fqdn() {
                    if self
                        .headers
                        .insert("Message-ID".into(), random::gen_message_id(&fqdn))
                        .is_none()
                    {
                        let pos = self
                            .header_order
                            .iter()
                            .position(|h| h == "Subject")
                            .unwrap();
                        self.header_order.insert(pos, "Message-ID".into());
                    }
                }
            }
        }
        for k in &self.header_order {
            let v = &self.headers[k];
            if v.is_ascii() {
                ret.extend(format!("{}: {}\n", k, v).chars());
            } else {
                ret.extend(format!("{}: {}\n", k, mime::encode_header(v)).chars());
            }
        }
        ret.push_str("MIME-Version: 1.0\n");

        if self.body.is_ascii() {
            ret.push('\n');
            ret.push_str(&self.body);
        } else {
            let content_type: ContentType = Default::default();
            let content_transfer_encoding: ContentTransferEncoding =
                ContentTransferEncoding::Base64;

            ret.extend(format!("Content-Type: {}; charset=\"utf-8\"\n", content_type).chars());
            ret.extend(
                format!("Content-Transfer-Encoding: {}\n", content_transfer_encoding).chars(),
            );
            ret.push('\n');

            ret.push_str(&BASE64_MIME.encode(&self.body.as_bytes()).trim());
            ret.push('\n');
        }

        Ok(ret)
    }
}

fn ignore_header(header: &[u8]) -> bool {
    match header {
        b"From" => false,
        b"To" => false,
        b"Date" => false,
        b"Message-ID" => false,
        b"User-Agent" => false,
        b"Subject" => false,
        b"Reply-to" => false,
        b"Cc" => false,
        b"Bcc" => false,
        b"In-Reply-To" => false,
        b"References" => false,
        b"MIME-Version" => true,
        h if h.starts_with(b"X-") => false,
        _ => true,
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
}
