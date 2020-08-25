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
 * Email parsing, handling, sending etc.
 */
use std::convert::TryInto;
mod compose;
pub use self::compose::*;

pub mod list_management;
mod mailto;
pub use mailto::*;
mod attachment_types;
pub mod attachments;
pub use crate::attachments::*;
mod address;
//pub mod parser;
pub mod parser;
use crate::parser::BytesExt;
pub use address::*;
mod headers;
pub mod signatures;
pub use headers::*;

use crate::datetime::UnixTimestamp;
use crate::error::{MeliError, Result};
use crate::thread::ThreadNodeHash;

use smallvec::SmallVec;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::Hasher;
use std::option::Option;
use std::str;
use std::string::String;

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

#[derive(Debug, Clone, Default)]
pub struct EnvelopeWrapper {
    envelope: Envelope,
    buffer: Vec<u8>,
}

use std::ops::Deref;

impl Deref for EnvelopeWrapper {
    type Target = Envelope;

    fn deref(&self) -> &Envelope {
        &self.envelope
    }
}

impl EnvelopeWrapper {
    pub fn new(buffer: Vec<u8>) -> Result<Self> {
        Ok(EnvelopeWrapper {
            envelope: Envelope::from_bytes(&buffer, None)?,
            buffer,
        })
    }

    pub fn update(&mut self, new_buffer: Vec<u8>) {
        // TODO: Propagate error.
        if let Ok(e) = EnvelopeWrapper::new(new_buffer) {
            *self = e;
        }
    }

    pub fn envelope(&self) -> &Envelope {
        &self.envelope
    }
    pub fn buffer(&self) -> &[u8] {
        &self.buffer
    }
}

pub type EnvelopeHash = u64;

/// `Envelope` represents all the data of an email we need to know.
///
///  Attachments (the email's body) is parsed on demand with `body`.
///
///  Access to the underlying email object in the account's backend (for example the file or the
///  entry in an IMAP server) is given through `operation_token`. For more information see
///  `BackendOp`.
#[derive(Clone, Serialize, Deserialize)]
pub struct Envelope {
    date: String,
    from: SmallVec<[Address; 1]>,
    to: SmallVec<[Address; 1]>,
    cc: SmallVec<[Address; 1]>,
    bcc: Vec<Address>,
    subject: Option<String>,
    message_id: MessageID,
    in_reply_to: Option<MessageID>,
    pub references: Option<References>,
    other_headers: HeaderMap,

    timestamp: UnixTimestamp,
    thread: ThreadNodeHash,

    hash: EnvelopeHash,

    flags: Flag,
    has_attachments: bool,
    labels: SmallVec<[u64; 8]>,
}

impl fmt::Debug for Envelope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Envelope")
            .field("Subject", &self.subject())
            .field("Date", &self.date)
            .field("From", &self.from)
            .field("To", &self.to)
            .field("Message-ID", &self.message_id_display())
            .field("In-Reply-To", &self.in_reply_to_display())
            .field("References", &self.references)
            .field("Hash", &self.hash)
            .finish()
    }
}

impl Default for Envelope {
    fn default() -> Self {
        Envelope::new(0)
    }
}

impl Envelope {
    pub fn new(hash: EnvelopeHash) -> Self {
        Envelope {
            date: String::new(),
            from: SmallVec::new(),
            to: SmallVec::new(),
            cc: SmallVec::new(),
            bcc: Vec::new(),
            subject: None,
            message_id: MessageID::default(),
            in_reply_to: None,
            references: None,
            other_headers: Default::default(),

            timestamp: 0,

            thread: ThreadNodeHash::null(),

            hash,
            has_attachments: false,
            flags: Flag::default(),
            labels: SmallVec::new(),
        }
    }

    pub fn set_hash(&mut self, new_hash: EnvelopeHash) {
        self.hash = new_hash;
    }

    pub fn from_bytes(bytes: &[u8], flags: Option<Flag>) -> Result<Envelope> {
        let mut h = DefaultHasher::new();
        h.write(bytes);
        let mut e = Envelope::new(h.finish());
        let res = e.populate_headers(bytes).ok();
        if res.is_some() {
            if let Some(f) = flags {
                e.flags = f;
            }
            return Ok(e);
        }
        Err(MeliError::new("Couldn't parse mail."))
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
                return Err(MeliError::new(error_msg));
            }
        };
        let mut in_reply_to = None;

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
                {
                    let parse_result = parser::address::references(value);
                    if let Ok((_, value)) = parse_result {
                        for v in value {
                            self.push_references(v);
                        }
                    }
                }
                self.set_references(value);
            } else if name == "in-reply-to" {
                self.set_in_reply_to(value);
                in_reply_to = Some(value);
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
                            if n == b"boundary" {
                                boundary = Some(v);
                                break;
                            }
                        }
                        if let Some(boundary) = boundary {
                            self.has_attachments =
                                Attachment::check_if_has_attachments_quick(body, boundary);
                        } else {
                            debug!("{:?} has no boundary field set in multipart/mixed content-type field.", &self);
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
        if let Some(ref mut x) = in_reply_to {
            self.push_references(x);
        }
        if let Ok(d) = parser::generic::date(&self.date.as_bytes()) {
            self.set_datetime(d);
        }
        if self.message_id.raw().is_empty() {
            let hash = self.hash;
            self.set_message_id(format!("<{:x}>", hash).as_bytes());
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
                .map(|s| s.as_str())
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
            self.other_headers
                .get("Cc")
                .map(|s| s.as_str())
                .unwrap_or_default()
                .to_string()
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
                .map(|s| s.as_str())
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
    pub fn field_to_to_string(&self) -> String {
        if self.to.is_empty() {
            self.other_headers
                .get("To")
                .map(|s| s.as_str())
                .unwrap_or_default()
                .to_string()
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
                .map(|s| s.as_str())
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

    /// Requests bytes from backend and thus can fail
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
    pub fn set_date(&mut self, new_val: &[u8]) {
        let new_val = new_val.trim();
        self.date = String::from_utf8_lossy(new_val).into_owned();
    }
    pub fn set_bcc(&mut self, new_val: Vec<Address>) {
        self.bcc = new_val;
    }
    pub fn set_cc(&mut self, new_val: SmallVec<[Address; 1]>) {
        self.cc = new_val;
    }
    pub fn set_from(&mut self, new_val: SmallVec<[Address; 1]>) {
        self.from = new_val;
    }
    pub fn set_to(&mut self, new_val: SmallVec<[Address; 1]>) {
        self.to = new_val;
    }
    pub fn set_in_reply_to(&mut self, new_val: &[u8]) {
        let new_val = new_val.trim();
        let slice = match parser::address::message_id(new_val) {
            Ok(v) => v.1,
            Err(_) => {
                self.in_reply_to = None;
                return;
            }
        };
        self.in_reply_to = Some(MessageID::new(new_val, slice));
    }
    pub fn set_subject(&mut self, new_val: Vec<u8>) {
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
    }
    pub fn set_message_id(&mut self, new_val: &[u8]) {
        let new_val = new_val.trim();
        match parser::address::message_id(new_val) {
            Ok((_, slice)) => {
                self.message_id = MessageID::new(new_val, slice);
            }
            Err(_) => {
                self.message_id = MessageID::new(new_val, new_val);
            }
        }
    }
    pub fn push_references(&mut self, new_val: &[u8]) {
        let new_val = new_val.trim();
        let slice = match parser::address::message_id(new_val) {
            Ok(v) => v.1,
            Err(e) => {
                debug!(e);
                return;
            }
        };
        let new_ref = MessageID::new(new_val, slice);
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
    pub fn set_references(&mut self, new_val: &[u8]) {
        let new_val = new_val.trim();
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
    pub fn set_thread(&mut self, new_val: ThreadNodeHash) {
        self.thread = new_val;
    }
    pub fn set_datetime(&mut self, new_val: UnixTimestamp) {
        self.timestamp = new_val;
    }
    pub fn set_flag(&mut self, f: Flag, value: bool) {
        self.flags.set(f, value);
    }
    pub fn set_flags(&mut self, f: Flag) {
        self.flags = f;
    }
    pub fn flags(&self) -> Flag {
        self.flags
    }
    pub fn set_seen(&mut self) {
        self.set_flag(Flag::SEEN, true)
    }
    pub fn set_unseen(&mut self) {
        self.set_flag(Flag::SEEN, false)
    }
    pub fn is_seen(&self) -> bool {
        self.flags.contains(Flag::SEEN)
    }

    pub fn set_has_attachments(&mut self, new_val: bool) {
        self.has_attachments = new_val;
    }

    pub fn has_attachments(&self) -> bool {
        self.has_attachments
    }

    pub fn labels(&self) -> &SmallVec<[u64; 8]> {
        &self.labels
    }

    pub fn labels_mut(&mut self) -> &mut SmallVec<[u64; 8]> {
        &mut self.labels
    }
}

impl Eq for Envelope {}
impl Ord for Envelope {
    fn cmp(&self, other: &Envelope) -> Ordering {
        self.datetime().cmp(&other.datetime())
    }
}

impl PartialOrd for Envelope {
    fn partial_cmp(&self, other: &Envelope) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Envelope {
    fn eq(&self, other: &Envelope) -> bool {
        self.hash == other.hash
    }
}
