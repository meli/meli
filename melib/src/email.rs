/*
 * meli - email module
 *
 * Copyright 2017 Manos Pitsidianakis
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

/*!
 * Email parsing and composing.
 *
 * # Parsing bytes into an `Envelope`
 *
 * An [`Envelope`](Envelope) represents the information you can get from an
 * email's headers and body structure. Addresses in `To`, `From` fields etc
 * are parsed into [`Address`](crate::email::Address) types.
 *
 * ```
 * use melib::{Attachment, Envelope};
 *
 * let raw_mail = r#"From: "some name" <some@example.com>
 * To: "me" <myself@example.com>
 * Cc:
 * Subject: =?utf-8?Q?gratuitously_encoded_subject?=
 * Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
 * MIME-Version: 1.0
 * Content-Type: multipart/mixed; charset="utf-8";
 *  boundary="bzz_bzz__bzz__"
 *
 * This is a MIME formatted message with attachments. Use a MIME-compliant client to view it properly.
 * --bzz_bzz__bzz__
 *
 * hello world.
 * --bzz_bzz__bzz__
 * Content-Type: image/gif; name="test_image.gif"; charset="utf-8"
 * Content-Disposition: attachment
 * Content-Transfer-Encoding: base64
 *
 * R0lGODdhKAAXAOfZAAABzAADzQAEzgQFtBEAxAAGxBcAxwALvRcFwAAPwBcLugATuQEUuxoNuxYQ
 * sxwOvAYVvBsStSAVtx8YsRUcuhwhth4iuCQsyDAwuDc1vTc3uDg4uT85rkc9ukJBvENCvURGukdF
 * wUVKt0hLuUxPvVZSvFlYu1hbt2BZuFxdul5joGhqlnNuf3FvlnBvwXJyt3Jxw3N0oXx1gH12gV99
 * z317f3N7spFxwHp5wH99gYB+goF/g25+26tziIOBhWqD3oiBjICAuudkjIN+zHeC2n6Bzc1vh4eF
 * iYaBw8F0kImHi4KFxYyHmIWIvI2Lj4uIvYaJyY+IuJGMi5iJl4qKxZSMmIuLxpONnpGPk42NvI2M
 * 1LKGl46OvZePm5ORlZiQnJqSnpaUmLyJnJuTn5iVmZyUoJGVyZ2VoZSVw5iXoZmWrO18rJiUyp6W
 * opuYnKaVnZ+Xo5yZncaMoaCYpJiaqo+Z2Z2annuf5qGZpa2WoJybpZmayZ2Z0KCZypydrZ6dp6Cd
 * oZ6a0aGay5ucy5+eqKGeouWMgp+b0qKbzKCfqdqPnp2ezaGgqqOgpKafqrScpp+gz6ajqKujr62j
 * qayksKmmq62lsaiosqqorOyWnaqqtKeqzLGptaurta2rr7Kqtq+ssLOrt6+uuLGusuqhfbWtubCv
 * ubKvs7GwurOwtPSazbevu+ali7SxtbiwvOykjLOyvLWytuCmqOankrSzvbazuLmyvrW0vre0uba1
 * wLi1ury0wLm2u721wbe3wbq3vMC2vLi4wr+3w7m5w8C4xLi6yry6vsG5xbu7xcC6zMK6xry8xry+
 * u8O7x729x8C9wb++yMG+wsO+vMK/w8a+y8e/zMnBzcXH18nL2///////////////////////////
 * ////////////////////////////////////////////////////////////////////////////
 * /////////////////////////////////////////////////////ywAAAAAKAAXAAAI/gBP4Cjh
 * IYMLEh0w4EgBgsMLEyFGFBEB5cOFABgzatS4AVssZAOsLOHCxooVMzCyoNmzaBOkJlS0VEDyZMjG
 * mxk3XOMF60CDBgsoPABK9KcDCRImPCiQYAECAgQCRMU4VSrGCjFarBgUSJCgQ10FBTrkNRCfPnz4
 * dA3UNa1btnDZqgU7Ntqzu3ej2X2mFy9eaHuhNRtMGJrhwYYN930G2K7eaNIY34U2mfJkwpgzI9Yr
 * GBqwR2KSvAlMOXHnw5pTNzPdLNoWIWtU9XjGjDEYS8LAlFm1SrVvzIKj5TH0KpORSZOryPgCZgqL
 * Ob+jG0YVRBErUrOiiGJ8KxgtYsh27xWL/tswnTtEbsiRVYdJNMHk4yOGhswGjR88UKjQ9Ey+/8TL
 * XKKGGn7Akph/8XX2WDTTcAYfguVt9hhrEPqmzIOJ3VUheb48WJiHG6amC4i+WVJKKCimqGIoYxyj
 * WWK8kKjaJ9bA18sxvXjYhourmbbMMrjI+OIn1QymDCVXANGFK4S1gQw0PxozzC+33FLLKUJq9gk1
 * gyWDhyNwrMLkYGUEM4wvuLRiCiieXIJJJVlmJskcZ9TZRht1lnFGGmTMkMoonVQSSSOFAGJHHI0w
 * ouiijDaaCCGQRgrpH3q4QYYXWDihxBE+7KCDDjnUIEVAADs=
 * --bzz_bzz__bzz__--"#;
 *
 * let envelope = Envelope::from_bytes(raw_mail.as_bytes(), None).expect("Could not parse mail");
 * assert_eq!(envelope.subject().as_ref(), "gratuitously encoded subject");
 * assert_eq!(
 *     envelope.message_id_display().as_ref(),
 *     "<h2g7f.z0gy2pgaen5m@example.com>"
 * );
 *
 * let body = envelope.body_bytes(raw_mail.as_bytes());
 * assert_eq!(body.content_type().to_string().as_str(), "multipart/mixed");
 *
 * let body_text = body.text();
 * assert_eq!(body_text.as_str(), "hello world.");
 *
 * let subattachments: Vec<Attachment> = body.attachments();
 * assert_eq!(subattachments.len(), 3);
 * assert_eq!(
 *     subattachments[2].content_type().name().unwrap(),
 *     "test_image.gif"
 * );
 * ```
 */

pub mod address;
pub mod attachment_types;
pub mod attachments;
pub mod compose;
pub mod headers;
pub mod list_management;
pub mod mailto;
pub mod parser;
pub mod pgp;

use std::{borrow::Cow, convert::TryInto, ops::Deref};

pub use address::{Address, MessageID, References, StrBuild, StrBuilder};
pub use attachments::{Attachment, AttachmentBuilder};
pub use compose::{attachment_from_file, Draft};
pub use headers::*;
use smallvec::SmallVec;

use crate::{
    datetime::UnixTimestamp,
    error::{Error, Result},
    parser::BytesExt,
    thread::ThreadNodeHash,
    TagHash,
};

bitflags! {
    #[derive(Default, Serialize, Deserialize)]
    pub struct Flag: u8 {
        const PASSED  =  0b0000_0001;
        const REPLIED =  0b0000_0010;
        const SEEN    =  0b0000_0100;
        const TRASHED =  0b0000_1000;
        const DRAFT   =  0b0001_0000;
        const FLAGGED =  0b0010_0000;
    }
}

impl PartialEq<&str> for Flag {
    fn eq(&self, other: &&str) -> bool {
        (other.eq_ignore_ascii_case("passed") && self.contains(Flag::PASSED))
            || (other.eq_ignore_ascii_case("replied") && self.contains(Flag::REPLIED))
            || (other.eq_ignore_ascii_case("seen") && self.contains(Flag::SEEN))
            || (other.eq_ignore_ascii_case("read") && self.contains(Flag::SEEN))
            || (other.eq_ignore_ascii_case("junk") && self.contains(Flag::TRASHED))
            || (other.eq_ignore_ascii_case("trash") && self.contains(Flag::TRASHED))
            || (other.eq_ignore_ascii_case("trashed") && self.contains(Flag::TRASHED))
            || (other.eq_ignore_ascii_case("draft") && self.contains(Flag::DRAFT))
            || (other.eq_ignore_ascii_case("flagged") && self.contains(Flag::FLAGGED))
    }
}

macro_rules! flag_impl {
    (fn $name:ident, $val:expr) => {
        pub const fn $name(&self) -> bool {
            self.contains($val)
        }
    };
}

impl Flag {
    flag_impl!(fn is_seen, Flag::SEEN);
    flag_impl!(fn is_draft, Flag::DRAFT);
    flag_impl!(fn is_trashed, Flag::TRASHED);
    flag_impl!(fn is_passed, Flag::PASSED);
    flag_impl!(fn is_replied, Flag::REPLIED);
    flag_impl!(fn is_flagged, Flag::FLAGGED);
}

///`Mail` holds both the envelope info of an email in its `envelope` field and
/// the raw bytes that describe the email in `bytes`. Its body as an
/// `melib::email::Attachment` can be parsed on demand
/// with the `melib::email::Mail::body` method.
#[derive(Debug, Clone, Default)]
pub struct Mail {
    pub envelope: Envelope,
    pub bytes: Vec<u8>,
}

impl Deref for Mail {
    type Target = Envelope;

    fn deref(&self) -> &Envelope {
        &self.envelope
    }
}

impl Mail {
    pub fn new(bytes: Vec<u8>, flags: Option<Flag>) -> Result<Self> {
        Ok(Mail {
            envelope: Envelope::from_bytes(&bytes, flags)?,
            bytes,
        })
    }

    pub fn envelope(&self) -> &Envelope {
        &self.envelope
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn body(&self) -> Attachment {
        self.envelope.body_bytes(&self.bytes)
    }
}

crate::declare_u64_hash!(EnvelopeHash);

/// `Envelope` represents all the header and structure data of an email we need
/// to know.
///
///  Attachments (the email's body) is parsed on demand with `body` method.
///
///To access the email attachments, you need to parse them from the raw email
/// bytes into an `Attachment` object.
#[derive(Clone, Serialize, Deserialize)]
pub struct Envelope {
    pub hash: EnvelopeHash,
    pub date: String,
    pub timestamp: UnixTimestamp,
    pub from: SmallVec<[Address; 1]>,
    pub to: SmallVec<[Address; 1]>,
    pub cc: SmallVec<[Address; 1]>,
    pub bcc: Vec<Address>,
    pub subject: Option<String>,
    pub message_id: MessageID,
    pub in_reply_to: Option<MessageID>,
    pub references: Option<References>,
    pub other_headers: HeaderMap,
    pub thread: ThreadNodeHash,
    pub flags: Flag,
    pub has_attachments: bool,
    pub tags: SmallVec<[TagHash; 8]>,
}

impl core::fmt::Debug for Envelope {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.debug_struct("Envelope")
            .field("Subject", &self.subject())
            .field("Date", &self.date)
            .field("From", &self.from)
            .field("To", &self.to)
            .field("Message-ID", &self.message_id_display())
            .field("In-Reply-To", &self.in_reply_to_display())
            .field("References", &self.references)
            .field("Flags", &self.flags)
            .field("Hash", &self.hash)
            .finish()
    }
}

impl Default for Envelope {
    fn default() -> Self {
        Envelope::new(EnvelopeHash::default())
    }
}

impl Envelope {
    pub fn new(hash: EnvelopeHash) -> Self {
        Envelope {
            hash,
            date: String::new(),
            timestamp: 0,
            from: SmallVec::new(),
            to: SmallVec::new(),
            cc: SmallVec::new(),
            bcc: Vec::new(),
            subject: None,
            message_id: MessageID::default(),
            in_reply_to: None,
            references: None,
            other_headers: Default::default(),
            thread: ThreadNodeHash::null(),
            has_attachments: false,
            flags: Flag::default(),
            tags: SmallVec::new(),
        }
    }

    pub fn set_hash(&mut self, new_hash: EnvelopeHash) -> &mut Self {
        self.hash = new_hash;
        self
    }

    pub fn from_bytes(bytes: &[u8], flags: Option<Flag>) -> Result<Envelope> {
        let mut e = Envelope::new(EnvelopeHash::from_bytes(bytes));
        let res = e.populate_headers(bytes).ok();
        if res.is_some() {
            if let Some(f) = flags {
                e.flags = f;
            }
            return Ok(e);
        }
        Err(Error::new("Couldn't parse mail."))
    }

    pub fn hash(&self) -> EnvelopeHash {
        self.hash
    }

    pub fn populate_headers(&mut self, mut bytes: &[u8]) -> Result<()> {
        if bytes.starts_with(b"From ") {
            /* Attempt to recover if message includes the mbox From label as first line */
            if let Some(offset) = bytes.find(b"\n") {
                bytes = &bytes[offset + 1..];
            }
        }
        let (headers, body) = match parser::mail(bytes) {
            Ok(v) => v,
            Err(e) => {
                debug!("error in parsing mail\n{:?}\n", e);
                let error_msg = String::from("Mail cannot be shown because of errors.");
                return Err(Error::new(error_msg));
            }
        };
        for (name, value) in headers {
            let name: HeaderName = name.try_into()?;
            if name == "to" {
                let parse_result = parser::address::rfc2822address_list(value);
                if let Ok((_, value)) = parse_result {
                    self.set_to(value);
                };
            } else if name == "cc" {
                let parse_result = parser::address::rfc2822address_list(value);
                if let Ok((_, value)) = parse_result {
                    self.set_cc(value);
                };
            } else if name == "bcc" {
                let parse_result = parser::address::rfc2822address_list(value);
                if let Ok((_, value)) = parse_result {
                    self.set_bcc(value.to_vec());
                };
            } else if name == "from" {
                let parse_result = parser::address::rfc2822address_list(value);
                if let Ok((_, value)) = parse_result {
                    self.set_from(value);
                }
            } else if name == "subject" {
                let parse_result = parser::encodings::phrase(value.trim(), false);
                if let Ok((_, value)) = parse_result {
                    self.set_subject(value);
                };
            } else if name == "message-id" {
                self.set_message_id(value);
            } else if name == "references" {
                self.set_references(value);
            } else if name == "in-reply-to" {
                self.set_in_reply_to(value);
            } else if name == "date" {
                let parse_result = parser::encodings::phrase(value, false);
                if let Ok((_, value)) = parse_result {
                    self.set_date(value.as_slice());
                } else {
                    self.set_date(value);
                }
            } else if name == "content-type" {
                match parser::attachments::content_type(value) {
                    Ok((_, (ct, cst, ref params)))
                        if ct.eq_ignore_ascii_case(b"multipart")
                            && cst.eq_ignore_ascii_case(b"mixed") =>
                    {
                        let mut builder = AttachmentBuilder::default();
                        builder.set_content_type_from_bytes(value);
                        let mut boundary = None;
                        for (n, v) in params {
                            if n.eq_ignore_ascii_case(b"boundary") {
                                boundary = Some(v);
                                break;
                            }
                        }
                        if let Some(boundary) = boundary {
                            self.has_attachments =
                                Attachment::check_if_has_attachments_quick(body, boundary);
                        } else {
                            debug!(
                                "{:?} has no boundary field set in multipart/mixed content-type \
                                 field.",
                                &self
                            );
                        }
                    }
                    _ => {}
                }
            }
            self.other_headers.insert(
                name,
                parser::encodings::phrase(value, false)
                    .map(|(_, value)| {
                        String::from_utf8(value)
                            .unwrap_or_else(|err| String::from_utf8_lossy(&err.into_bytes()).into())
                    })
                    .unwrap_or_else(|_| String::from_utf8_lossy(value).into()),
            );
        }
        /*
         * https://tools.ietf.org/html/rfc5322#section-3.6.4
         *
         * if self.message_id.is_none()  ...
         */
        if let Some(x) = self.in_reply_to.clone() {
            self.push_references(x);
        }
        if let Ok(d) = parser::dates::rfc5322_date(self.date.as_bytes()) {
            self.set_datetime(d);
        }
        if self.message_id.raw().is_empty() {
            let hash = self.hash;
            self.set_message_id(format!("<{:x}>", hash.0).as_bytes());
        }
        if self.references.is_some() {
            if let Some(pos) = self
                .references
                .as_ref()
                .map(|r| &r.refs)
                .unwrap()
                .iter()
                .position(|r| r == &self.message_id)
            {
                self.references.as_mut().unwrap().refs.remove(pos);
            }
        }

        Ok(())
    }

    pub fn date(&self) -> UnixTimestamp {
        self.timestamp
    }

    pub fn datetime(&self) -> UnixTimestamp {
        self.timestamp
    }

    pub fn date_as_str(&self) -> &str {
        &self.date
    }

    pub fn from(&self) -> &[Address] {
        self.from.as_slice()
    }

    pub fn field_bcc_to_string(&self) -> String {
        if self.bcc.is_empty() {
            self.other_headers
                .get("Bcc")
                .unwrap_or_default()
                .to_string()
        } else {
            self.bcc.iter().fold(String::new(), |mut acc, x| {
                if !acc.is_empty() {
                    acc.push_str(", ");
                }
                acc.push_str(&x.to_string());
                acc
            })
        }
    }

    pub fn field_cc_to_string(&self) -> String {
        if self.cc.is_empty() {
            self.other_headers.get("Cc").unwrap_or_default().to_string()
        } else {
            self.cc.iter().fold(String::new(), |mut acc, x| {
                if !acc.is_empty() {
                    acc.push_str(", ");
                }
                acc.push_str(&x.to_string());
                acc
            })
        }
    }

    pub fn field_from_to_string(&self) -> String {
        if self.from.is_empty() {
            self.other_headers
                .get("From")
                .unwrap_or_default()
                .to_string()
        } else {
            self.from.iter().fold(String::new(), |mut acc, x| {
                if !acc.is_empty() {
                    acc.push_str(", ");
                }
                acc.push_str(&x.to_string());
                acc
            })
        }
    }

    pub fn to(&self) -> &[Address] {
        self.to.as_slice()
    }

    pub fn cc(&self) -> &[Address] {
        self.cc.as_slice()
    }

    pub fn bcc(&self) -> &[Address] {
        self.bcc.as_slice()
    }

    pub fn field_to_to_string(&self) -> String {
        if self.to.is_empty() {
            self.other_headers.get("To").unwrap_or_default().to_string()
        } else {
            self.to
                .iter()
                .map(|a| format!("{}", a))
                .fold(String::new(), |mut acc, x| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.push_str(&x);
                    acc
                })
        }
    }

    pub fn field_references_to_string(&self) -> String {
        let refs = self.references();
        if refs.is_empty() {
            self.other_headers
                .get("References")
                .unwrap_or_default()
                .to_string()
        } else {
            refs.iter()
                .map(|a| a.to_string())
                .fold(String::new(), |mut acc, x| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.push_str(&x);
                    acc
                })
        }
    }

    pub fn body_bytes(&self, bytes: &[u8]) -> Attachment {
        let builder = AttachmentBuilder::new(bytes);
        builder.build()
    }

    pub fn headers<'a>(&self, bytes: &'a [u8]) -> Result<Vec<(&'a str, &'a str)>> {
        let ret = parser::headers::headers(bytes)?.1;
        let len = ret.len();
        ret.into_iter()
            .try_fold(Vec::with_capacity(len), |mut acc, (a, b)| {
                Ok({
                    acc.push((std::str::from_utf8(a)?, std::str::from_utf8(b)?));
                    acc
                })
            })
    }

    pub fn subject(&self) -> Cow<str> {
        match self.subject {
            Some(ref s) => Cow::from(s),
            _ => Cow::from(String::new()),
        }
    }

    pub fn in_reply_to(&self) -> Option<&MessageID> {
        self.in_reply_to
            .as_ref()
            .or_else(|| self.references.as_ref().and_then(|r| r.refs.last()))
    }

    pub fn in_reply_to_display(&self) -> Option<Cow<str>> {
        self.in_reply_to
            .as_ref()
            .map(|m| String::from_utf8_lossy(m.val()))
    }

    pub fn in_reply_to_raw(&self) -> Option<Cow<str>> {
        self.in_reply_to
            .as_ref()
            .map(|m| String::from_utf8_lossy(m.raw()))
    }

    pub fn message_id(&self) -> &MessageID {
        &self.message_id
    }

    pub fn message_id_display(&self) -> Cow<str> {
        String::from_utf8_lossy(self.message_id.val())
    }

    pub fn message_id_raw(&self) -> Cow<str> {
        String::from_utf8_lossy(self.message_id.raw())
    }

    pub fn set_date(&mut self, new_val: &[u8]) -> &mut Self {
        let new_val = new_val.trim();
        self.date = String::from_utf8_lossy(new_val).into_owned();
        self
    }

    pub fn set_bcc(&mut self, new_val: Vec<Address>) -> &mut Self {
        self.bcc = new_val;
        self
    }

    pub fn set_cc(&mut self, new_val: SmallVec<[Address; 1]>) -> &mut Self {
        self.cc = new_val;
        self
    }

    pub fn set_from(&mut self, new_val: SmallVec<[Address; 1]>) -> &mut Self {
        self.from = new_val;
        self
    }

    pub fn set_to(&mut self, new_val: SmallVec<[Address; 1]>) -> &mut Self {
        self.to = new_val;
        self
    }

    pub fn set_in_reply_to(&mut self, new_val: &[u8]) -> &mut Self {
        // FIXME msg_id_list
        let new_val = new_val.trim();
        if !new_val.is_empty() {
            let val = match parser::address::msg_id(new_val) {
                Ok(v) => v.1,
                Err(_) => {
                    self.in_reply_to = Some(MessageID::new(new_val, new_val));
                    return self;
                }
            };
            self.in_reply_to = Some(val);
        } else {
            self.in_reply_to = None;
        }
        self
    }

    pub fn set_subject(&mut self, new_val: Vec<u8>) -> &mut Self {
        let mut new_val = String::from_utf8(new_val)
            .unwrap_or_else(|err| String::from_utf8_lossy(&err.into_bytes()).into());
        while new_val
            .chars()
            .last()
            .map(char::is_control)
            .unwrap_or(false)
        {
            new_val.pop();
        }

        self.subject = Some(new_val);
        self
    }

    pub fn set_message_id(&mut self, new_val: &[u8]) -> &mut Self {
        let new_val = new_val.trim();
        match parser::address::msg_id(new_val) {
            Ok((_, val)) => {
                self.message_id = val;
            }
            Err(_) => {
                self.message_id = MessageID::new(new_val, new_val);
            }
        }
        self
    }

    pub fn push_references(&mut self, new_ref: MessageID) {
        match self.references {
            Some(ref mut s) => {
                if s.refs.contains(&new_ref) {
                    if s.refs[s.refs.len() - 1] != new_ref {
                        if let Some(index) = s.refs.iter().position(|x| *x == new_ref) {
                            s.refs.remove(index);
                        } else {
                            panic!();
                        }
                    } else {
                        return;
                    }
                }
                s.refs.push(new_ref);
            }
            None => {
                let v = vec![new_ref];
                self.references = Some(References {
                    raw: "".into(),
                    refs: v,
                });
            }
        }
    }

    pub fn set_references(&mut self, new_val: &[u8]) -> &mut Self {
        let new_val = new_val.trim();
        if !new_val.is_empty() {
            self.references = None;
            {
                let parse_result = parser::address::msg_id_list(new_val);
                if let Ok((_, value)) = parse_result {
                    for v in value {
                        self.push_references(v);
                    }
                }
            }
            match self.references {
                Some(ref mut s) => {
                    s.raw = new_val.into();
                }
                None => {
                    self.references = Some(References {
                        raw: new_val.into(),
                        refs: Vec::new(),
                    });
                }
            }
        }
        self
    }

    pub fn references(&self) -> SmallVec<[&MessageID; 8]> {
        match self.references {
            Some(ref s) => s.refs.iter().fold(SmallVec::new(), |mut acc, x| {
                acc.push(x);
                acc
            }),
            None => SmallVec::new(),
        }
    }

    pub fn other_headers(&self) -> &HeaderMap {
        &self.other_headers
    }

    pub fn other_headers_mut(&mut self) -> &mut HeaderMap {
        &mut self.other_headers
    }

    pub fn thread(&self) -> ThreadNodeHash {
        self.thread
    }

    pub fn set_thread(&mut self, new_val: ThreadNodeHash) -> &mut Self {
        self.thread = new_val;
        self
    }

    pub fn set_datetime(&mut self, new_val: UnixTimestamp) -> &mut Self {
        self.timestamp = new_val;
        self
    }

    pub fn set_flag(&mut self, f: Flag, value: bool) -> &mut Self {
        self.flags.set(f, value);
        self
    }

    pub fn set_flags(&mut self, f: Flag) -> &mut Self {
        self.flags = f;
        self
    }

    pub fn flags(&self) -> Flag {
        self.flags
    }

    pub fn set_seen(&mut self) -> &mut Self {
        self.set_flag(Flag::SEEN, true);
        self
    }

    pub fn set_unseen(&mut self) -> &mut Self {
        self.set_flag(Flag::SEEN, false);
        self
    }

    pub fn is_seen(&self) -> bool {
        self.flags.contains(Flag::SEEN)
    }

    pub fn set_has_attachments(&mut self, new_val: bool) -> &mut Self {
        self.has_attachments = new_val;
        self
    }

    pub fn has_attachments(&self) -> bool {
        self.has_attachments
    }

    pub fn tags(&self) -> &SmallVec<[TagHash; 8]> {
        &self.tags
    }

    pub fn tags_mut(&mut self) -> &mut SmallVec<[TagHash; 8]> {
        &mut self.tags
    }
}

impl Eq for Envelope {}

impl Ord for Envelope {
    fn cmp(&self, other: &Envelope) -> std::cmp::Ordering {
        self.datetime().cmp(&other.datetime())
    }
}

impl PartialOrd for Envelope {
    fn partial_cmp(&self, other: &Envelope) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Envelope {
    fn eq(&self, other: &Envelope) -> bool {
        self.hash == other.hash
    }
}
