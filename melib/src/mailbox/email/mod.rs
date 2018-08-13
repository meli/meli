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
mod attachment_types;
pub mod attachments;
pub use self::attachments::*;
pub mod parser;
use parser::BytesExt;

use error::{MeliError, Result};
use mailbox::backends::BackendOp;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::Hasher;
use std::option::Option;
use std::string::String;

use chrono;
use chrono::TimeZone;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GroupAddress {
    raw: Vec<u8>,
    display_name: StrBuilder,
    mailbox_list: Vec<Address>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MailboxAddress {
    raw: Vec<u8>,
    display_name: StrBuilder,
    address_spec: StrBuilder,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Address {
    Mailbox(MailboxAddress),
    Group(GroupAddress),
}

impl Eq for Address {}
impl PartialEq for Address {
    fn eq(&self, other: &Address) -> bool {
        match (self, other) {
            (Address::Mailbox(_), Address::Group(_)) | (Address::Group(_), Address::Mailbox(_)) => {
                false
            }
            (Address::Mailbox(s), Address::Mailbox(o)) => {
                s.address_spec.display(&s.raw) == o.address_spec.display(&o.raw)
            }
            (Address::Group(s), Address::Group(o)) => {
                s.display_name.display(&s.raw) == o.display_name.display(&o.raw)
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

/// Helper struct to return slices from a struct field on demand.
#[derive(Clone, Debug, Serialize, Deserialize)]
struct StrBuilder {
    offset: usize,
    length: usize,
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
}

/// `MessageID` is accessed through the `StrBuild` trait.
#[derive(Clone, Serialize, Deserialize)]
pub struct MessageID(Vec<u8>, StrBuilder);

impl StrBuild for MessageID {
    fn new(string: &[u8], slice: &[u8]) -> Self {
        let offset = string.find(slice).unwrap();
        MessageID(
            string.to_owned(),
            StrBuilder {
                offset: offset,
                length: slice.len() + 1,
            },
        )
    }
    fn raw(&self) -> &[u8] {
        let offset = self.1.offset;
        let length = self.1.length;
        &self.0[offset..offset + length - 1]
    }
    fn val(&self) -> &[u8] {
        &self.0
    }
}

#[test]
fn test_strbuilder() {
    let m_id = "<20170825132332.6734-1-el13635@mail.ntua.gr>";
    let (_, slice) = parser::message_id(m_id.as_bytes()).unwrap();
    assert_eq!(
        MessageID::new(m_id, slice),
        MessageID(
            m_id.to_string(),
            StrBuilder {
                offset: 1,
                length: 43,
            }
        )
    );
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

#[derive(Clone, Debug, Serialize, Deserialize)]
struct References {
    raw: Vec<u8>,
    refs: Vec<MessageID>,
}

bitflags! {
    #[derive(Default, Serialize, Deserialize)]
    pub struct Flag: u8 {
        const PASSED  =  0b00000001;
        const REPLIED =  0b00000010;
        const SEEN    =  0b00000100;
        const TRASHED =  0b00001000;
        const DRAFT   =  0b00010000;
        const FLAGGED =  0b00100000;
    }
}

#[derive(Debug, Clone, Default)]
pub struct EnvelopeBuilder {
    from: Option<Vec<Address>>,
    to: Vec<Address>,
    body: Option<Attachment>,
    in_reply_to: Option<MessageID>,
    flags: Flag,
}

impl EnvelopeBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn build(self) -> Envelope {
        unimplemented!();

        /*
         * 1. Check for date. Default is now
         * 2.
        Envelope {


        */
    }
}

/// `Envelope` represents all the data of an email we need to know.
///
///  Attachments (the email's body) is parsed on demand with `body`.
///
///  Access to the underlying email object in the account's backend (for example the file or the
///  entry in an IMAP server) is given through `operation_token`. For more information see
///  `BackendOp`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Envelope {
    date: String,
    from: Vec<Address>,
    to: Vec<Address>,
    body: Option<Attachment>,
    subject: Option<Vec<u8>>,
    message_id: Option<MessageID>,
    in_reply_to: Option<MessageID>,
    references: Option<References>,

    timestamp: u64,
    thread: usize,

    hash: u64,

    flags: Flag,
}

impl Envelope {
    pub fn new(hash: u64) -> Self {
        Envelope {
            date: String::new(),
            from: Vec::new(),
            to: Vec::new(),
            body: None,
            subject: None,
            message_id: None,
            in_reply_to: None,
            references: None,

            timestamp: 0,

            thread: 0,

            hash,
            flags: Flag::default(),
        }
    }
    pub fn from_token(operation: Box<BackendOp>, hash: u64) -> Option<Envelope> {
        let mut e = Envelope::new(hash);
        e.flags = operation.fetch_flags();
        let res = e.populate_headers(operation).ok();
        if res.is_some() {
            Some(e)
        } else {
            None
        }
    }
    pub fn hash(&self) -> u64 {
        self.hash
    }
    pub fn populate_headers(&mut self, mut operation: Box<BackendOp>) -> Result<()> {
        {
            let headers = match parser::headers(operation.fetch_headers()?).to_full_result() {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("error in parsing mail\n");
                    return Err(MeliError::from(e));
                }
            };

            let mut in_reply_to = None;
            let mut datetime = None;

            for (name, value) in headers {
                if value.len() == 1 && value.is_empty() {
                    continue;
                }
                if name.eq_ignore_ascii_case(b"to") {
                    let parse_result = parser::rfc2822address_list(value);
                    let value = if parse_result.is_done() {
                        parse_result.to_full_result().unwrap()
                    } else {
                        Vec::new()
                    };
                    self.set_to(value);
                } else if name.eq_ignore_ascii_case(b"from") {
                    let parse_result = parser::rfc2822address_list(value);
                    let value = if parse_result.is_done() {
                        parse_result.to_full_result().unwrap()
                    } else {
                        Vec::new()
                    };
                    self.set_from(value);
                } else if name.eq_ignore_ascii_case(b"subject") {
                    let parse_result = parser::phrase(value.trim());
                    let value = if parse_result.is_done() {
                        parse_result.to_full_result().unwrap()
                    } else {
                        "".into()
                    };
                    self.set_subject(value);
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
                    self.set_date(value);
                    datetime = Some(value);
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
            if let Some(ref mut d) = datetime {
                if let Some(d) = parser::date(d) {
                    self.set_datetime(d);
                }
            }
    }
        if self.message_id.is_none() {
            let mut h = DefaultHasher::new();
            h.write(&self.bytes(operation));
            self.set_message_id(format!("<{:x}>", h.finish()).as_bytes());
        }
        Ok(())
    }
    pub fn date(&self) -> u64 {
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

    pub fn from_to_string(&self) -> String {
        let _strings: Vec<String> = self.from.iter().map(|a| format!("{}", a)).collect();
        _strings.join(", ")
    }
    pub fn to(&self) -> &Vec<Address> {
        &self.to
    }
    pub fn to_to_string(&self) -> String {
        let _strings: Vec<String> = self.to.iter().map(|a| format!("{}", a)).collect();
        _strings.join(", ")
    }
    pub fn bytes(&self, mut operation: Box<BackendOp>) -> Vec<u8> {
        operation
            .as_bytes()
            .map(|v| v.into())
            .unwrap_or_else(|_| Vec::new())
    }
    pub fn body(&self, mut operation: Box<BackendOp>) -> Attachment {
        let file = operation.as_bytes();
        let (headers, body) = match parser::mail(file.unwrap()).to_full_result() {
            Ok(v) => v,
            Err(_) => {
                eprintln!("error in parsing mail\n");
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
    }
    pub fn subject(&self) -> Cow<str> {
        match self.subject {
            Some(ref s) => String::from_utf8_lossy(s),
            _ => Cow::from(String::new()),
        }
    }
    pub fn in_reply_to(&self) -> Cow<str> {
        match self.in_reply_to {
            Some(ref s) => String::from_utf8_lossy(s.val()),
            _ => Cow::from(String::new()),
        }
    }
    pub fn in_reply_to_raw(&self) -> Cow<str> {
        match self.in_reply_to {
            Some(ref s) => String::from_utf8_lossy(s.raw()).into(),
            _ => Cow::from(String::new()),
        }
    }
    pub fn message_id(&self) -> Cow<str> {
        match self.message_id {
            Some(ref s) => String::from_utf8_lossy(s.val()),
            _ => Cow::from(String::new()),
        }
    }
    pub fn message_id_raw(&self) -> Cow<str> {
        match self.message_id {
            Some(ref s) => String::from_utf8_lossy(s.raw()),
            _ => Cow::from(String::new()),
        }
    }
    fn set_date(&mut self, new_val: &[u8]) -> () {
        self.date = String::from_utf8_lossy(new_val).into_owned();
    }
    fn set_from(&mut self, new_val: Vec<Address>) -> () {
        self.from = new_val;
    }
    fn set_to(&mut self, new_val: Vec<Address>) -> () {
        self.to = new_val;
    }
    fn set_in_reply_to(&mut self, new_val: &[u8]) -> () {
        let slice = match parser::message_id(new_val).to_full_result() {
            Ok(v) => v,
            Err(_) => {
                self.in_reply_to = None;
                return;
            }
        };
        self.in_reply_to = Some(MessageID::new(new_val, slice));
    }
    fn set_subject(&mut self, new_val: Vec<u8>) -> () {
        self.subject = Some(new_val);
    }
    fn set_message_id(&mut self, new_val: &[u8]) -> () {
        let slice = match parser::message_id(new_val).to_full_result() {
            Ok(v) => v,
            Err(_) => {
                self.message_id = None;
                return;
            }
        };
        self.message_id = Some(MessageID::new(new_val, slice));
    }
    fn push_references(&mut self, new_val: &[u8]) -> () {
        let slice = match parser::message_id(new_val).to_full_result() {
            Ok(v) => v,
            Err(_) => {
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
                let mut v = Vec::new();
                v.push(new_ref);
                self.references = Some(References {
                    raw: "".into(),
                    refs: v,
                });
            }
        }
    }
    // TODO: Check what references should be like again.
    fn set_references(&mut self, new_val: &[u8]) -> () {
        match self.references {
            Some(ref mut s) => {
                s.raw = new_val.into();
            }
            None => {
                let v = Vec::new();
                self.references = Some(References {
                    raw: new_val.into(),
                    refs: v,
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
    pub fn set_body(&mut self, new_val: Attachment) -> () {
        self.body = Some(new_val);
    }
    pub fn thread(&self) -> usize {
        self.thread
    }
    pub fn set_thread(&mut self, new_val: usize) -> () {
        self.thread = new_val;
    }
    pub fn set_datetime(&mut self, new_val: chrono::DateTime<chrono::FixedOffset>) -> () {
        self.timestamp = new_val.timestamp() as u64;
    }
    pub fn set_flag(&mut self, f: Flag, mut operation: Box<BackendOp>) -> Result<()> {
        operation.set_flag(self, &f)?;
        self.flags |= f;
        Ok(())
    }
    pub fn flags(&self) -> Flag {
        self.flags
    }
    pub fn set_seen(&mut self, operation: Box<BackendOp>) -> Result<()> {
        self.set_flag(Flag::SEEN, operation)
    }
    pub fn is_seen(&self) -> bool {
        self.flags.contains(Flag::SEEN)
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
        self.message_id_raw() == other.message_id_raw()
    }
}
