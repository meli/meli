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
use fnv::FnvHashMap;
mod compose;
pub use self::compose::*;

mod mailto;
pub use mailto::*;
mod attachment_types;
pub mod attachments;
pub use crate::attachments::*;
pub mod parser;
use crate::parser::BytesExt;

use crate::backends::BackendOp;
use crate::error::{MeliError, Result};
use crate::thread::ThreadHash;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::Hasher;
use std::option::Option;
use std::str;
use std::string::String;

use chrono;
use chrono::TimeZone;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GroupAddress {
    pub raw: Vec<u8>,
    pub display_name: StrBuilder,
    pub mailbox_list: Vec<Address>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MailboxAddress {
    pub raw: Vec<u8>,
    pub display_name: StrBuilder,
    pub address_spec: StrBuilder,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum Address {
    Mailbox(MailboxAddress),
    Group(GroupAddress),
}

impl Address {
    pub fn get_display_name(&self) -> String {
        match self {
            Address::Mailbox(m) => m.display_name.display(&m.raw),
            Address::Group(g) => g.display_name.display(&g.raw),
        }
    }

    pub fn get_email(&self) -> String {
        match self {
            Address::Mailbox(m) => m.address_spec.display(&m.raw),
            Address::Group(_) => String::new(),
        }
    }
    pub fn get_fqdn(&self) -> Option<String> {
        match self {
            Address::Mailbox(m) => {
                let raw_address = m.address_spec.display_bytes(&m.raw);
                let fqdn_pos = raw_address.iter().position(|&b| b == b'@')? + 1;
                Some(String::from_utf8_lossy(&raw_address[fqdn_pos..]).into())
            }
            Address::Group(_) => None,
        }
    }
}

impl Eq for Address {}
impl PartialEq for Address {
    fn eq(&self, other: &Address) -> bool {
        match (self, other) {
            (Address::Mailbox(_), Address::Group(_)) | (Address::Group(_), Address::Mailbox(_)) => {
                false
            }
            (Address::Mailbox(s), Address::Mailbox(o)) => {
                s.address_spec.display_bytes(&s.raw) == o.address_spec.display_bytes(&o.raw)
            }
            (Address::Group(s), Address::Group(o)) => {
                s.display_name.display_bytes(&s.raw) == o.display_name.display_bytes(&o.raw)
                    && s.mailbox_list
                        .iter()
                        .zip(o.mailbox_list.iter())
                        .fold(true, |b, (s, o)| b && (s == o))
            }
        }
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Address::Mailbox(m) if m.display_name.length > 0 => write!(
                f,
                "{} <{}>",
                m.display_name.display(&m.raw),
                m.address_spec.display(&m.raw)
            ),
            Address::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| format!("{}", a)).collect();
                write!(
                    f,
                    "{}: {}",
                    g.display_name.display(&g.raw),
                    attachment_strings.join(", ")
                )
            }
            Address::Mailbox(m) => write!(f, "{}", m.address_spec.display(&m.raw)),
        }
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Helper struct to return slices from a struct field on demand.
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct StrBuilder {
    pub offset: usize,
    pub length: usize,
}

/// Structs implementing this trait must contain a `StrBuilder` field.
pub trait StrBuild {
    /// Create a new `Self` out of a string and a slice
    fn new(string: &[u8], slice: &[u8]) -> Self;
    /// Get the slice part of the string
    fn raw(&self) -> &[u8];
    /// Get the entire string as a slice
    fn val(&self) -> &[u8];
}

impl StrBuilder {
    fn display<'a>(&self, s: &'a [u8]) -> String {
        let offset = self.offset;
        let length = self.length;
        String::from_utf8(s[offset..offset + length].to_vec()).unwrap()
    }
    fn display_bytes<'a>(&self, b: &'a [u8]) -> &'a [u8] {
        &b[self.offset..(self.offset + self.length)]
    }
}

/// `MessageID` is accessed through the `StrBuild` trait.
#[derive(Clone, Serialize, Deserialize, Default)]
pub struct MessageID(pub Vec<u8>, pub StrBuilder);

impl StrBuild for MessageID {
    fn new(string: &[u8], slice: &[u8]) -> Self {
        let offset = string.find(slice).unwrap();
        MessageID(
            string.to_owned(),
            StrBuilder {
                offset,
                length: slice.len() + 1,
            },
        )
    }
    fn raw(&self) -> &[u8] {
        let offset = self.1.offset;
        let length = self.1.length;
        &self.0[offset..offset + length.saturating_sub(1)]
    }
    fn val(&self) -> &[u8] {
        &self.0
    }
}

#[test]
fn test_strbuilder() {
    let m_id = b"<20170825132332.6734-1@el13635@mail.ntua.gr>";
    let (_, slice) = parser::message_id(m_id).unwrap();
    assert_eq!(
        MessageID::new(m_id, slice),
        MessageID(
            m_id.to_vec(),
            StrBuilder {
                offset: 1,
                length: 43,
            }
        )
    );
}

impl fmt::Display for MessageID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.val().is_ascii() {
            write!(f, "{}", unsafe { str::from_utf8_unchecked(self.val()) })
        } else {
            write!(f, "{}", String::from_utf8_lossy(self.val()))
        }
    }
}

impl PartialEq for MessageID {
    fn eq(&self, other: &MessageID) -> bool {
        self.raw() == other.raw()
    }
}
impl fmt::Debug for MessageID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", String::from_utf8(self.raw().to_vec()).unwrap())
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct References {
    raw: Vec<u8>,
    refs: Vec<MessageID>,
}

impl fmt::Debug for References {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self.refs)
    }
}

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

pub type UnixTimestamp = u64;
pub type EnvelopeHash = u64;

/// `Envelope` represents all the data of an email we need to know.
///
///  Attachments (the email's body) is parsed on demand with `body`.
///
///  Access to the underlying email object in the account's backend (for example the file or the
///  entry in an IMAP server) is given through `operation_token`. For more information see
///  `BackendOp`.
#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Envelope {
    date: String,
    from: Vec<Address>,
    to: Vec<Address>,
    cc: Vec<Address>,
    bcc: Vec<Address>,
    subject: Option<Vec<u8>>,
    message_id: MessageID,
    in_reply_to: Option<MessageID>,
    references: Option<References>,
    other_headers: FnvHashMap<String, String>,

    timestamp: UnixTimestamp,
    thread: ThreadHash,

    hash: EnvelopeHash,

    flags: Flag,
    has_attachments: bool,
}

impl fmt::Debug for Envelope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Envelope {{\n\tsubject: {}\n\tdate: {},\n\tfrom:{:#?},\n\tto {:#?},\n\tmessage_id: {},\n\tin_reply_to: {:?}\n\treferences: {:#?},\n\thash: {}\n}}",
               self.subject(),
               self.date,
               self.from,
               self.to,
               self.message_id_display(),
               self.in_reply_to_display(),
               self.references,
               self.hash)
    }
}

impl Envelope {
    pub fn new(hash: EnvelopeHash) -> Self {
        Envelope {
            date: String::new(),
            from: Vec::new(),
            to: Vec::new(),
            cc: Vec::new(),
            bcc: Vec::new(),
            subject: None,
            message_id: MessageID::default(),
            in_reply_to: None,
            references: None,
            other_headers: FnvHashMap::default(),

            timestamp: 0,

            thread: ThreadHash::null(),

            hash,
            has_attachments: false,
            flags: Flag::default(),
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
    pub fn from_token(mut operation: Box<dyn BackendOp>, hash: EnvelopeHash) -> Option<Envelope> {
        let mut e = Envelope::new(hash);
        e.flags = operation.fetch_flags();
        if let Ok(bytes) = operation.as_bytes() {
            let res = e.populate_headers(bytes).ok();
            if res.is_some() {
                return Some(e);
            }
        }
        None
    }
    pub fn hash(&self) -> EnvelopeHash {
        self.hash
    }
    pub fn populate_headers(&mut self, bytes: &[u8]) -> Result<()> {
        let (headers, body) = match parser::mail(bytes).to_full_result() {
            Ok(v) => v,
            Err(e) => {
                debug!("error in parsing mail\n{:?}\n", e);
                let error_msg = String::from("Mail cannot be shown because of errors.");
                return Err(MeliError::new(error_msg));
            }
        };
        let mut in_reply_to = None;

        for (name, value) in headers {
            if value.len() == 1 && value.is_empty() {
                continue;
            }
            if name.eq_ignore_ascii_case(b"to") {
                let parse_result = parser::rfc2822address_list(value);
                if parse_result.is_done() {
                    let value = parse_result.to_full_result().unwrap();
                    self.set_to(value);
                };
            } else if name.eq_ignore_ascii_case(b"cc") {
                let parse_result = parser::rfc2822address_list(value);
                if parse_result.is_done() {
                    let value = parse_result.to_full_result().unwrap();
                    self.set_cc(value);
                };
            } else if name.eq_ignore_ascii_case(b"bcc") {
                let parse_result = parser::rfc2822address_list(value);
                if parse_result.is_done() {
                    let value = parse_result.to_full_result().unwrap();
                    self.set_bcc(value);
                };
            } else if name.eq_ignore_ascii_case(b"from") {
                let parse_result = parser::rfc2822address_list(value);
                if parse_result.is_done() {
                    let value = parse_result.to_full_result().unwrap();
                    self.set_from(value);
                }
            } else if name.eq_ignore_ascii_case(b"subject") {
                let parse_result = parser::phrase(value.trim());
                if parse_result.is_done() {
                    let value = parse_result.to_full_result().unwrap();
                    self.set_subject(value);
                };
            } else if name.eq_ignore_ascii_case(b"message-id") {
                self.set_message_id(value);
            } else if name.eq_ignore_ascii_case(b"references") {
                {
                    let parse_result = parser::references(value);
                    if parse_result.is_done() {
                        for v in parse_result.to_full_result().unwrap() {
                            self.push_references(v);
                        }
                    }
                }
                self.set_references(value);
            } else if name.eq_ignore_ascii_case(b"in-reply-to") {
                self.set_in_reply_to(value);
                in_reply_to = Some(value);
            } else if name.eq_ignore_ascii_case(b"date") {
                let parse_result = parser::phrase(value);
                if parse_result.is_done() {
                    let value = parse_result.to_full_result().unwrap();
                    self.set_date(value.as_slice());
                } else {
                    self.set_date(value);
                }
            } else if name.eq_ignore_ascii_case(b"content-type") {
                match parser::content_type(value).to_full_result() {
                    Ok((ct, cst, _))
                        if ct.eq_ignore_ascii_case(b"multipart")
                            && cst.eq_ignore_ascii_case(b"mixed") =>
                    {
                        let mut builder = AttachmentBuilder::new(body);
                        builder.set_content_type_from_bytes(value);
                        let b = builder.build();
                        let subs = b.attachments();

                        self.has_attachments = subs.iter().any(|sub| !sub.is_text());
                    }
                    _ => {}
                }
            } else {
                self.other_headers.insert(
                    String::from_utf8_lossy(name).into(),
                    String::from_utf8_lossy(value).into(),
                );
            }
        }
        /*
         * https://tools.ietf.org/html/rfc5322#section-3.6.4
         *
         * if self.message_id.is_none()  ...
         */
        if let Some(ref mut x) = in_reply_to {
            self.push_references(x);
        }
        if let Some(d) = parser::date(&self.date.as_bytes()) {
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

    pub fn populate_headers_from_token(&mut self, mut operation: Box<dyn BackendOp>) -> Result<()> {
        let headers = operation.fetch_headers()?;
        self.populate_headers(headers)
    }
    pub fn date(&self) -> UnixTimestamp {
        self.timestamp
    }

    pub fn datetime(&self) -> chrono::DateTime<chrono::FixedOffset> {
        if let Some(d) = parser::date(&self.date.as_bytes()) {
            return d;
        }
        chrono::FixedOffset::west(0)
            .ymd(1970, 1, 1)
            .and_hms(0, 0, 0)
    }
    pub fn date_as_str(&self) -> &str {
        &self.date
    }
    pub fn from(&self) -> &Vec<Address> {
        &self.from
    }
    pub fn field_bcc_to_string(&self) -> String {
        let _strings: Vec<String> = self.bcc.iter().map(|a| format!("{}", a)).collect();
        _strings.join(", ")
    }
    pub fn field_cc_to_string(&self) -> String {
        let _strings: Vec<String> = self.cc.iter().map(|a| format!("{}", a)).collect();
        _strings.join(", ")
    }
    pub fn field_from_to_string(&self) -> String {
        let _strings: Vec<String> = self.from().iter().map(|a| format!("{}", a)).collect();
        _strings.join(", ")
    }
    pub fn to(&self) -> &Vec<Address> {
        &self.to
    }
    pub fn field_to_to_string(&self) -> String {
        let _strings: Vec<String> = self.to.iter().map(|a| format!("{}", a)).collect();
        _strings.join(", ")
    }
    pub fn bytes(&self, mut operation: Box<dyn BackendOp>) -> Vec<u8> {
        operation
            .as_bytes()
            .map(|v| v.into())
            .unwrap_or_else(|_| Vec::new())
    }
    pub fn body_bytes(&self, bytes: &[u8]) -> Attachment {
        if bytes.is_empty() {
            let builder = AttachmentBuilder::new(bytes);
            return builder.build();
        }

        let (headers, body) = match parser::mail(bytes).to_full_result() {
            Ok(v) => v,
            Err(_) => {
                debug!("error in parsing mail\n");
                let error_msg = b"Mail cannot be shown because of errors.";
                let builder = AttachmentBuilder::new(error_msg);
                return builder.build();
            }
        };
        let mut builder = AttachmentBuilder::new(body);
        for (name, value) in headers {
            if value.len() == 1 && value.is_empty() {
                continue;
            }
            if name.eq_ignore_ascii_case(b"content-transfer-encoding") {
                builder.set_content_transfer_encoding(ContentTransferEncoding::from(value));
            } else if name.eq_ignore_ascii_case(b"content-type") {
                builder.set_content_type_from_bytes(value);
            }
        }
        builder.build()
    }
    pub fn headers<'a>(&self, bytes: &'a [u8]) -> Result<Vec<(&'a str, &'a str)>> {
        let ret = parser::headers(bytes).to_full_result()?;
        let len = ret.len();
        ret.into_iter()
            .try_fold(Vec::with_capacity(len), |mut acc, (a, b)| {
                Ok({
                    acc.push((std::str::from_utf8(a)?, std::str::from_utf8(b)?));
                    acc
                })
            })
    }
    pub fn body(&self, mut operation: Box<dyn BackendOp>) -> Attachment {
        debug!("searching body for {:?}", self.message_id_display());
        let file = operation.as_bytes();
        self.body_bytes(file.unwrap())
        /*
        let (headers, body) = match parser::mail(file.unwrap()).to_full_result() {
            Ok(v) => v,
            Err(_) => {
            debug!("2error in parsing mail\n");
                let error_msg = b"Mail cannot be shown because of errors.";
                let mut builder = AttachmentBuilder::new(error_msg);
                return builder.build();
            }
        };
        let mut builder = AttachmentBuilder::new(body);
        for (name, value) in headers {
            if value.len() == 1 && value.is_empty() {
                continue;
            }
            if name.eq_ignore_ascii_case(b"content-transfer-encoding") {
                builder.content_transfer_encoding(value);
            } else if name.eq_ignore_ascii_case(b"content-type") {
                builder.content_type(value);
            }
        }
        builder.build()
        */
    }
    pub fn subject(&self) -> Cow<str> {
        match self.subject {
            Some(ref s) => String::from_utf8_lossy(s),
            _ => Cow::from(String::new()),
        }
    }
    pub fn in_reply_to(&self) -> Option<&MessageID> {
        self.in_reply_to.as_ref()
    }
    pub fn in_reply_to_display(&self) -> Option<Cow<str>> {
        if let Some(ref m) = self.in_reply_to {
            Some(String::from_utf8_lossy(m.val()))
        } else {
            None
        }
    }
    pub fn in_reply_to_raw(&self) -> Option<Cow<str>> {
        if let Some(ref m) = self.in_reply_to {
            Some(String::from_utf8_lossy(m.raw()))
        } else {
            None
        }
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
        self.date = String::from_utf8_lossy(new_val).into_owned();
    }
    pub fn set_bcc(&mut self, new_val: Vec<Address>) {
        self.bcc = new_val;
    }
    pub fn set_cc(&mut self, new_val: Vec<Address>) {
        self.cc = new_val;
    }
    pub fn set_from(&mut self, new_val: Vec<Address>) {
        self.from = new_val;
    }
    pub fn set_to(&mut self, new_val: Vec<Address>) {
        self.to = new_val;
    }
    pub fn set_in_reply_to(&mut self, new_val: &[u8]) {
        let slice = match parser::message_id(new_val).to_full_result() {
            Ok(v) => v,
            Err(_) => {
                self.in_reply_to = None;
                return;
            }
        };
        self.in_reply_to = Some(MessageID::new(new_val, slice));
    }
    pub fn set_subject(&mut self, new_val: Vec<u8>) {
        self.subject = Some(new_val);
    }
    pub fn set_message_id(&mut self, new_val: &[u8]) {
        let slice = match parser::message_id(new_val).to_full_result() {
            Ok(v) => v,
            Err(e) => {
                debug!(e);
                return;
            }
        };
        self.message_id = MessageID::new(new_val, slice);
    }
    pub fn push_references(&mut self, new_val: &[u8]) {
        let slice = match parser::message_id(new_val).to_full_result() {
            Ok(v) => v,
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
    pub fn references(&self) -> Vec<&MessageID> {
        match self.references {
            Some(ref s) => s
                .refs
                .iter()
                .fold(Vec::with_capacity(s.refs.len()), |mut acc, x| {
                    acc.push(x);
                    acc
                }),
            None => Vec::new(),
        }
    }
    pub fn other_headers(&self) -> &FnvHashMap<String, String> {
        &self.other_headers
    }

    pub fn other_headers_mut(&mut self) -> &mut FnvHashMap<String, String> {
        &mut self.other_headers
    }

    pub fn thread(&self) -> ThreadHash {
        self.thread
    }
    pub fn set_thread(&mut self, new_val: ThreadHash) {
        self.thread = new_val;
    }
    pub fn set_datetime(&mut self, new_val: chrono::DateTime<chrono::FixedOffset>) {
        self.timestamp = new_val.timestamp() as UnixTimestamp;
    }
    pub fn set_flag(&mut self, f: Flag, mut operation: Box<dyn BackendOp>) -> Result<()> {
        self.flags.toggle(f);
        operation.set_flag(self, f)?;
        Ok(())
    }
    pub fn set_flags(&mut self, f: Flag) {
        self.flags = f;
    }
    pub fn flags(&self) -> Flag {
        self.flags
    }
    pub fn set_seen(&mut self, operation: Box<dyn BackendOp>) -> Result<()> {
        if !self.flags.contains(Flag::SEEN) {
            self.set_flag(Flag::SEEN, operation)
        } else {
            Ok(())
        }
    }
    pub fn set_unseen(&mut self, operation: Box<dyn BackendOp>) -> Result<()> {
        if self.flags.contains(Flag::SEEN) {
            self.set_flag(Flag::SEEN, operation)
        } else {
            Ok(())
        }
    }
    pub fn is_seen(&self) -> bool {
        self.flags.contains(Flag::SEEN)
    }
    pub fn has_attachments(&self) -> bool {
        self.has_attachments
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
