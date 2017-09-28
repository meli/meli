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

pub mod parser;
mod attachments;
use mailbox::backends::BackendOpGenerator;
use self::attachments::*;

use std::string::String;
use std::sync::Arc;
//use std;
use std::cmp::Ordering;
use std::fmt;
use std::option::Option;
//use std::io::prelude::*;
use std::ascii::AsciiExt;

use chrono;
use chrono::TimeZone;

/* Helper struct to return slices from a struct on demand */
#[derive(Clone, Debug)]
struct StrBuilder {
    offset: usize,
    length: usize,
}

pub trait StrBuild {
    fn new(&str, &str) -> Self;
    fn get_raw(&self) -> &str;
    fn get_val(&self) -> &str;
}

#[derive(Clone)]
pub struct MessageID(String, StrBuilder);

impl StrBuild for MessageID {
    fn new(string: &str, slice: &str) -> Self {
        let offset = string.find(slice).unwrap();
        MessageID(
            string.to_string(),
            StrBuilder {
                offset: offset,
                length: slice.len() + 1,
            },
        )
    }
    fn get_raw(&self) -> &str {
        let offset = self.1.offset;
        let length = self.1.length;
        &self.0[offset..length]
    }
    fn get_val(&self) -> &str {
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
        self.get_raw() == other.get_raw()
    }
}
impl fmt::Debug for MessageID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_raw())
    }
}

#[derive(Clone, Debug)]
struct References {
    raw: String,
    refs: Vec<MessageID>,
}


bitflags! {
    #[derive(Default)]
    pub struct Flag: u8 {
        const PASSED  =  0b00000001;
        const REPLIED =  0b00000010;
        const SEEN    =  0b00000100;
        const TRASHED =  0b00001000;
        const DRAFT   =  0b00010000;
        const FLAGGED =  0b00100000;
    }
}

/* A very primitive mail object */
#[derive(Debug, Clone)]
pub struct Envelope {
    date: String,
    from: Option<String>,
    to: Option<String>,
    body: Option<Attachment>,
    subject: Option<String>,
    message_id: Option<MessageID>,
    in_reply_to: Option<MessageID>,
    references: Option<References>,

    datetime: Option<chrono::DateTime<chrono::FixedOffset>>,
    timestamp: i64,
    thread: usize,

    operation_token: Arc<Box<BackendOpGenerator>>,

    flags: Flag,
}


impl Envelope {
    pub fn get_date(&self) -> i64 {
        self.timestamp
    }
    pub fn get_datetime(&self) -> chrono::DateTime<chrono::FixedOffset> {
        self.datetime.unwrap_or_else(|| {
            chrono::FixedOffset::west(0)
                .ymd(1970, 1, 1)
                .and_hms(0, 0, 0)
        })
    }
    pub fn get_date_as_str(&self) -> &str {
        &self.date
    }
    pub fn get_from(&self) -> &str {
        match self.from {
            Some(ref s) => s,
            None => "",
        }
    }
    pub fn get_to(&self) -> &str {
        match self.to {
            Some(ref s) => s,
            None => "",
        }
    }
    pub fn get_body(&self) -> Attachment {
        let mut operation = self.operation_token.generate();
        let file = operation.as_bytes();
        let (headers, body) = match parser::mail(file.unwrap()).to_full_result() {
            Ok(v) => v,
            Err(_) => {
                let operation = self.operation_token.generate();
                eprintln!("error in parsing mail\n{}", operation.description());
                panic!()
            }
        };
        let mut builder = AttachmentBuilder::new(body);
        for (name, value) in headers {
            if value.len() == 1 && value.is_empty() {
                continue;
            }
            if name.eq_ignore_ascii_case("content-transfer-encoding") {
                builder.content_transfer_encoding(value);
            } else if name.eq_ignore_ascii_case("content-type") {
                builder.content_type(value);
            }
        }
        builder.build()
    }
    pub fn get_subject(&self) -> &str {
        match self.subject {
            Some(ref s) => s,
            _ => "",
        }
    }
    pub fn get_in_reply_to(&self) -> &str {
        match self.in_reply_to {
            Some(ref s) => s.get_val(),
            _ => "",
        }
    }
    pub fn get_in_reply_to_raw(&self) -> &str {
        match self.in_reply_to {
            Some(ref s) => s.get_raw(),
            _ => "",
        }
    }
    pub fn get_message_id(&self) -> &str {
        match self.message_id {
            Some(ref s) => s.get_val(),
            _ => "",
        }
    }
    pub fn get_message_id_raw(&self) -> &str {
        match self.message_id {
            Some(ref s) => s.get_raw(),
            _ => "",
        }
    }
    fn set_date(&mut self, new_val: String) -> () {
        self.date = new_val;
    }
    fn set_from(&mut self, new_val: String) -> () {
        self.from = Some(new_val);
    }
    fn set_to(&mut self, new_val: String) -> () {
        self.to = Some(new_val);
    }
    fn set_in_reply_to(&mut self, new_val: &str) -> () {
        let slice = match parser::message_id(new_val.as_bytes()).to_full_result() {
            Ok(v) => v,
            Err(v) => {
                eprintln!("{} {:?}", new_val, v);
                self.in_reply_to = None;
                return;
            }
        };
        self.in_reply_to = Some(MessageID::new(new_val, slice));
    }
    fn set_subject(&mut self, new_val: String) -> () {
        self.subject = Some(new_val);
    }
    fn set_message_id(&mut self, new_val: &str) -> () {
        let slice = match parser::message_id(new_val.as_bytes()).to_full_result() {
            Ok(v) => v,
            Err(v) => {
                eprintln!("{} {:?}", new_val, v);
                self.message_id = None;
                return;
            }
        };
        self.message_id = Some(MessageID::new(new_val, slice));
    }
    fn push_references(&mut self, new_val: &str) -> () {
        let slice = match parser::message_id(new_val.as_bytes()).to_full_result() {
            Ok(v) => v,
            Err(v) => {
                eprintln!("{} {:?}", new_val, v);
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
                    raw: "".to_string(),
                    refs: v,
                });
            }
        }
    }
    fn set_references(&mut self, new_val: String) -> () {
        match self.references {
            Some(ref mut s) => {
                s.raw = new_val;
            }
            None => {
                let v = Vec::new();
                self.references = Some(References {
                    raw: new_val,
                    refs: v,
                });
            }
        }
    }
    pub fn get_references(&self) -> Vec<&MessageID> {
        match self.references {
            Some(ref s) => s.refs
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
    pub fn get_thread(&self) -> usize {
        self.thread
    }
    pub fn set_thread(&mut self, new_val: usize) -> () {
        self.thread = new_val;
    }
    pub fn set_datetime(&mut self, new_val: Option<chrono::DateTime<chrono::FixedOffset>>) -> () {
        self.datetime = new_val;
        if let Some(v) = self.datetime {
            self.timestamp = v.timestamp();
        }
    }
    pub fn get_flags(&self) -> Flag {
        self.flags
    }
    pub fn is_seen(&self) -> bool {
        !(self.flags & Flag::SEEN).is_empty()
    }
    pub fn new(token: Box<BackendOpGenerator>) -> Self {
        Envelope {
            date: "".to_string(),
            from: None,
            to: None,
            body: None,
            subject: None,
            message_id: None,
            in_reply_to: None,
            references: None,

            datetime: None,
            timestamp: 0,

            thread: 0,

            operation_token: Arc::new(token),
            flags: Flag::default(),
        }
    }
    pub fn from(operation_token: Box<BackendOpGenerator>) -> Option<Envelope> {
        let operation = operation_token.generate();
        let mut e = Envelope::new(operation_token);
        e.flags = operation.fetch_flags();
        Some(e)
    }

    pub fn populate_headers(&mut self) -> () {
        let mut operation = self.operation_token.generate();
        let headers = match parser::headers(operation.fetch_headers().unwrap()).to_full_result() {
            Ok(v) => v,
            _ => {
                let operation = self.operation_token.generate();
                eprintln!("error in parsing mail\n{}", operation.description());
                return;
            }
        };

        let mut in_reply_to = None;
        let mut datetime = None;

        for (name, value) in headers {
            if value.len() == 1 && value.is_empty() {
                continue;
            }
            if name.eq_ignore_ascii_case("to") {
                let parse_result = parser::subject(value.as_bytes());
                let value = if parse_result.is_done() {
                    parse_result.to_full_result().unwrap()
                } else {
                    "".to_string()
                };
                self.set_to(value);
            } else if name.eq_ignore_ascii_case("from") {
                let parse_result = parser::subject(value.as_bytes());
                let value = if parse_result.is_done() {
                    parse_result.to_full_result().unwrap()
                } else {
                    "".to_string()
                };
                self.set_from(value);
            } else if name.eq_ignore_ascii_case("subject") {
                let parse_result = parser::subject(value.trim().as_bytes());
                let value = if parse_result.is_done() {
                    parse_result.to_full_result().unwrap()
                } else {
                    "".to_string()
                };
                self.set_subject(value);
            } else if name.eq_ignore_ascii_case("message-id") {
                self.set_message_id(value);
            } else if name.eq_ignore_ascii_case("references") {
                {
                    let parse_result = parser::references(value.as_bytes());
                    if parse_result.is_done() {
                        for v in parse_result.to_full_result().unwrap() {
                            self.push_references(v);
                        }
                    }
                }
                self.set_references(value.to_string());
            } else if name.eq_ignore_ascii_case("in-reply-to") {
                self.set_in_reply_to(value);
                in_reply_to = Some(value);
            } else if name.eq_ignore_ascii_case("date") {
                self.set_date(value.to_string());
                datetime = Some(value.to_string());
            }
        }
        /*
         * https://tools.ietf.org/html/rfc5322#section-3.6.4
         *
         * if self.message_id.is_none() { ...
         */
        if let Some(ref mut x) = in_reply_to {
            self.push_references(x);
        }
        if let Some(ref mut d) = datetime {
            self.set_datetime(parser::date(d));
        }
    }
}

impl Eq for Envelope {}
impl Ord for Envelope {
    fn cmp(&self, other: &Envelope) -> Ordering {
        self.get_datetime().cmp(&other.get_datetime())
    }
}
impl PartialOrd for Envelope {
    fn partial_cmp(&self, other: &Envelope) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Envelope {
    fn eq(&self, other: &Envelope) -> bool {
        self.get_message_id_raw() == other.get_message_id_raw()
    }
}
