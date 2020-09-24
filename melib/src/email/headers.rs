/*
 * meli - headers
 *
 * Copyright 2020 Manos Pitsidianakis
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

/*! Wrapper type `HeaderName` for case-insensitive comparisons */
use crate::error::MeliError;
use indexmap::IndexMap;
use smallvec::SmallVec;
use std::borrow::Borrow;
use std::cmp::{Eq, PartialEq};
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct HeaderNameType<S>(S);

///Case insensitive wrapper for a header name. As of `RFC5322` it's guaranteened to be ASCII.
pub type HeaderName = HeaderNameType<SmallVec<[u8; 32]>>;

impl HeaderName {
    pub fn new_unchecked(from: &str) -> Self {
        HeaderNameType(from.as_bytes().into())
    }
}

impl<S: AsRef<[u8]>> fmt::Display for HeaderNameType<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.normalize())
    }
}

impl<S: AsRef<[u8]>> fmt::Debug for HeaderNameType<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<S: AsRef<[u8]>> PartialEq<[u8]> for HeaderNameType<S> {
    fn eq(&self, other: &[u8]) -> bool {
        self.0.as_ref().eq_ignore_ascii_case(other)
    }
}

impl<S: AsRef<[u8]>> PartialEq<&str> for HeaderNameType<S> {
    fn eq(&self, other: &&str) -> bool {
        self.0.as_ref().eq_ignore_ascii_case(other.as_bytes())
    }
}

impl<S1: AsRef<[u8]>, S2: AsRef<[u8]>> PartialEq<HeaderNameType<S2>> for HeaderNameType<S1> {
    fn eq(&self, other: &HeaderNameType<S2>) -> bool {
        self.0.as_ref().eq_ignore_ascii_case(other.0.as_ref())
    }
}

impl<S: AsRef<[u8]>> Eq for HeaderNameType<S> {}

impl<S: AsRef<[u8]>> Hash for HeaderNameType<S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for b in self.0.as_ref().iter() {
            b.to_ascii_lowercase().hash(state);
        }
    }
}

impl TryFrom<&[u8]> for HeaderName {
    type Error = MeliError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.is_ascii() {
            Ok(HeaderNameType(value.into()))
        } else {
            Err(MeliError::new(format!(
                "Header value is not ascii: {:?}",
                value
            )))
        }
    }
}

impl TryFrom<&str> for HeaderName {
    type Error = MeliError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.is_ascii() {
            Ok(HeaderNameType(value.as_bytes().into()))
        } else {
            Err(MeliError::new(format!(
                "Header value is not ascii: {:?}",
                value
            )))
        }
    }
}

trait HeaderKey {
    fn to_key(&self) -> &[u8];
}

impl Hash for dyn HeaderKey + '_ {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for b in self.to_key().iter() {
            b.to_ascii_lowercase().hash(state);
        }
    }
}

impl PartialEq for dyn HeaderKey + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.to_key().eq_ignore_ascii_case(other.to_key())
    }
}

impl Eq for dyn HeaderKey + '_ {}

impl<S: AsRef<[u8]>> HeaderKey for HeaderNameType<S> {
    fn to_key(&self) -> &[u8] {
        self.0.as_ref()
    }
}

//Implement Borrow for all the lookup types as returning our trait object:

impl<'a> Borrow<dyn HeaderKey + 'a> for HeaderName {
    fn borrow(&self) -> &(dyn HeaderKey + 'a) {
        self
    }
}

impl<S: AsRef<[u8]>> HeaderNameType<S> {
    pub fn as_str(&self) -> &str {
        //HeadersType are ascii so valid utf8
        unsafe { std::str::from_utf8_unchecked(self.0.as_ref()) }
    }

    pub fn normalize(&self) -> &str {
        if self == &b"subject"[..] {
            "Subject"
        } else if self == &b"from"[..] {
            "From"
        } else if self == &b"to"[..] {
            "To"
        } else if self == &b"cc"[..] {
            "Cc"
        } else if self == &b"bcc"[..] {
            "Bcc"
        } else if self == &b"reply-to"[..] {
            "Reply-To"
        } else if self == &b"in-reply-to"[..] {
            "In-Reply-To"
        } else if self == &b"references"[..] {
            "References"
        } else if self == &b"sender"[..] {
            "Sender"
        } else if self == &b"mail-reply-to"[..] {
            "Mail-Reply-To"
        } else if self == &b"mail-followup-to"[..] {
            "Mail-Followup-To"
        } else if self == &b"mime-version"[..] {
            "MIME-Version"
        } else if self == &b"content-disposition"[..] {
            "Content-Disposition"
        } else if self == &b"content-transfer-encoding"[..] {
            "Content-Transfer-Encoding"
        } else if self == &b"content-type"[..] {
            "Content-Type"
        } else if self == &b"content-id"[..] {
            "Content-ID"
        } else if self == &b"content-description"[..] {
            "Content-Description"
        } else if self == &b"authentication-results"[..] {
            "Authentication-Results"
        } else if self == &b"dkim-signature"[..] {
            "DKIM-Signature"
        } else if self == &b"delivered-to"[..] {
            "Delivered-To"
        } else if self == &b"message-id"[..] {
            "Message-ID"
        } else if self == &b"comments"[..] {
            "Comments"
        } else if self == &b"keywords"[..] {
            "Keywords"
        } else if self == &b"resent-from"[..] {
            "Resent-From"
        } else if self == &b"resent-sender"[..] {
            "Resent-Sender"
        } else if self == &b"resent-to"[..] {
            "Resent-To"
        } else if self == &b"resent-cc"[..] {
            "Resent-Cc"
        } else if self == &b"resent-bcc"[..] {
            "Resent-Bcc"
        } else if self == &b"resent-date"[..] {
            "Resent-Date"
        } else if self == &b"resent-message-id"[..] {
            "Resent-Message-ID"
        } else if self == &b"resent-reply-to"[..] {
            "Resent-Reply-To"
        } else if self == &b"return-path"[..] {
            "Return-Path"
        } else if self == &b"received"[..] {
            "Received"
        } else {
            self.as_str()
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct HeaderMap(indexmap::IndexMap<HeaderName, String>);

impl std::ops::Index<&[u8]> for HeaderMap {
    type Output = str;
    fn index(&self, k: &[u8]) -> &Self::Output {
        (self.0)[HeaderNameType(k).borrow() as &dyn HeaderKey].as_str()
    }
}

impl std::ops::Index<&str> for HeaderMap {
    type Output = str;
    fn index(&self, k: &str) -> &Self::Output {
        (self.0)[HeaderNameType(k).borrow() as &dyn HeaderKey].as_str()
    }
}

impl HeaderMap {
    pub fn get_mut(&mut self, key: &str) -> Option<&mut String> {
        (self.0).get_mut(HeaderNameType(key).borrow() as &dyn HeaderKey)
    }

    pub fn get(&self, key: &str) -> Option<&String> {
        (self.0).get(HeaderNameType(key).borrow() as &dyn HeaderKey)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        (self.0).contains_key(HeaderNameType(key).borrow() as &dyn HeaderKey)
    }

    pub fn remove(&mut self, key: &str) -> Option<String> {
        (self.0).remove(HeaderNameType(key).borrow() as &dyn HeaderKey)
    }
}

impl Deref for HeaderMap {
    type Target = IndexMap<HeaderName, String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for HeaderMap {
    fn deref_mut(&mut self) -> &mut IndexMap<HeaderName, String> {
        &mut self.0
    }
}

#[test]
fn test_headers_case_sensitivity() {
    use std::convert::TryInto;
    let mut headers = HeaderMap::default();
    headers.insert("from".try_into().unwrap(), "Myself <a@b.c>".into());
    assert_eq!(&headers["From"], "Myself <a@b.c>");
    assert_eq!(&headers["From"], &headers["from"]);
    assert_eq!(&headers["fROm"], &headers["from"]);
    headers.get_mut("from").unwrap().pop();
    assert_eq!(&headers["From"], "Myself <a@b.c");
    headers.insert("frOM".try_into().unwrap(), "nada".into());
    assert_eq!(&headers["fROm"], "nada");
}
