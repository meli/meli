/*
 * meli - melib crate.
 *
 * Copyright 2023 Manos Pitsidianakis
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

//! E-mail header names. Also referred to as `Fields` in `RFC5322`.
//!
//! See [`HeaderName`] for more information.
#![allow(non_upper_case_globals)]

use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
    convert::TryFrom,
    error::Error,
    hash::{Hash, Hasher},
    str::FromStr,
};

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use super::standards::StandardHeader;
use crate::email::parser::BytesExt;

/// Case insensitive owned wrapper for a header name.
///
/// Because it is implementing [RFC5322], it's guaranteed to be ASCII.
/// It also guarantees it does not follow any bytes not allowed in header names
/// by [RFC5322]. See [`HEADER_CHARS`] for more information.
///
/// Internally, it only allocates if the header name value is not one statically
/// encoded in the [`StandardHeader`] struct. See its definition for possible
/// standard header name values.
///
/// [RFC5322]: https://datatracker.ietf.org/doc/html/rfc5322
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HeaderName {
    pub(super) inner: Repr<Custom>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(super) enum Repr<T> {
    Standard(StandardHeader),
    Custom(T),
}

impl<T: std::fmt::Display> std::fmt::Display for Repr<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Standard(inner) => write!(fmt, "{}", inner.as_str()),
            Self::Custom(inner) => inner.fmt(fmt),
        }
    }
}

/// Wrapper type used to hijack the Hash impl
#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct Custom(SmallVec<[u8; 32]>);

impl Custom {
    #[inline]
    fn as_str(&self) -> &str {
        // SAFETY: it's always a valid ASCII string when created.
        unsafe { std::str::from_utf8_unchecked(&self.0) }
    }
}

/// A possible error when converting into a [`HeaderName`] from another type.
pub struct InvalidHeaderName;

impl Error for InvalidHeaderName {}

impl std::fmt::Debug for InvalidHeaderName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "Invalid header name.")
    }
}

impl std::fmt::Display for InvalidHeaderName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", crate::identify!(InvalidHeaderName))
    }
}

/// Valid header name ASCII bytes
///
/// The index of an ASCII byte corresponds to the byte value itself, or the
/// `NUL` byte -zero- if it's not a valid header name character.
///
/// Source: [RFC5322 3.6.8.](https://datatracker.ietf.org/doc/html/rfc5322#autoid-35)
///
/// ```text
/// field-name      =   1*ftext
///
/// ftext           =   %d33-57 /          ; Printable US-ASCII
///                     %d59-126           ;  characters not including
///                                        ;  ":".
/// ```
pub const HEADER_CHARS: [u8; 128] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //   x
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //  1x
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //  2x
    0, 0, 0, b'!', b'"', b'#', b'$', b'%', b'&', b'\'', //  3x
    0, 0, b'*', b'+', 0, b'-', b'.', 0, b'0', b'1', //  4x
    b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', 0, 0, //  5x
    0, 0, 0, 0, 0, b'a', b'b', b'c', b'd', b'e', //  6x
    b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', //  7x
    b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', //  8x
    b'z', 0, 0, 0, b'^', b'_', b'`', b'a', b'b', b'c', //  9x
    b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', // 10x
    b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', // 11x
    b'x', b'y', b'z', 0, b'|', 0, b'~', 0, // 128
];

impl HeaderName {
    /// Returns a `str` representation of the header.
    ///
    /// The returned string will always be lower case. Use `Display` for a
    /// properly formatted representation.
    #[inline]
    pub fn as_str(&self) -> &str {
        match self.inner {
            Repr::Standard(v) => v.as_str(),
            Repr::Custom(ref v) => v.as_str(),
        }
    }

    /// Returns a `&[u8]` representation of the header.
    ///
    /// The returned string will always be lower case. Use `Display` for a
    /// properly formatted representation.
    #[inline]
    pub fn as_lowercase_bytes(&self) -> &[u8] {
        match self.inner {
            Repr::Standard(v) => v.as_str().as_bytes(),
            Repr::Custom(ref v) => v.0.as_ref(),
        }
    }

    /// Converts a header into a `Vec<u8>`.
    ///
    /// The returned string will always be formatted.
    #[inline]
    pub fn into_bytes(self) -> Vec<u8> {
        self.to_string().into_bytes()
    }

    /// Checks `src` byte slice for invalid header bytes, according to
    /// [RFC5322].
    ///
    /// As an optimization, if `src` corresponds to a *standard* e-mail header
    /// we know about, as encoded in the [`StandardHeader`] type, the parsed
    /// return value does not allocate.
    ///
    /// For information over which header bytes are invalid, see documentation
    /// module constant [`HEADER_CHARS`] and its source code.
    ///
    /// [RFC5322]: https://datatracker.ietf.org/doc/html/rfc5322#autoid-35
    pub fn from_bytes(src: &[u8]) -> Result<Self, InvalidHeaderName> {
        if let Some(std) = StandardHeader::from_bytes(src.trim()) {
            Ok(Self {
                inner: Repr::Standard(std),
            })
        } else {
            let mut buf = SmallVec::<[u8; 32]>::new();
            for b in src {
                let Some(b) = HEADER_CHARS.get(*b as usize).filter(|b| **b != 0) else {
                    return Err(InvalidHeaderName::new());
                };
                buf.push(*b);
            }

            Ok(Self {
                inner: Repr::Custom(Custom(buf)),
            })
        }
    }

    #[inline]
    pub const fn is_standard(&self) -> bool {
        matches!(
            self,
            Self {
                inner: Repr::Standard(_)
            }
        )
    }
}

/// Turn a [`&str`] to a [`HeaderName`], if it does not contain any invalid
/// bytes.
///
/// It uses [`HeaderName::from_bytes`] internally.
impl FromStr for HeaderName {
    type Err = InvalidHeaderName;

    #[inline]
    fn from_str(s: &str) -> Result<Self, InvalidHeaderName> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

impl AsRef<str> for HeaderName {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<[u8]> for HeaderName {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl Borrow<str> for HeaderName {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl std::fmt::Display for HeaderName {
    #[inline]
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", &self.inner)
    }
}

/// Implement [`Deserialize`] from either a string value or a byte sequence.
impl<'de> Deserialize<'de> for HeaderName {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize, Serialize)]
        #[serde(untagged)]
        enum Helper {
            S(String),
            B(Vec<u8>),
        }
        <Helper>::deserialize(deserializer)
            .map_err(|_| de::Error::custom("invalid header name value"))
            .and_then(|s| {
                Self::from_bytes(match &s {
                    Helper::S(v) => v.as_bytes(),
                    Helper::B(v) => v.as_slice(),
                })
                .map_err(|_| de::Error::custom("invalid header name value"))
            })
    }
}

/// Serialize [`HeaderName`] to a string value.
impl Serialize for HeaderName {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl InvalidHeaderName {
    const fn new() -> Self {
        Self
    }
}

/// Convert a [`HeaderName`] reference to an owned one by cloning.
impl<'a> From<&'a Self> for HeaderName {
    fn from(src: &'a Self) -> Self {
        src.clone()
    }
}

/// Convert a [`HeaderName`] reference to a `'static` [`std::borrow::Cow`]
/// string slice.
impl From<&HeaderName> for Cow<'static, str> {
    fn from(src: &HeaderName) -> Self {
        match src.inner {
            Repr::Standard(s) => Cow::Borrowed(s.as_str()),
            Repr::Custom(_) => Cow::Owned(src.to_string()),
        }
    }
}

/// Turn a [`&str`] to a [`HeaderName`], if it does not contain any invalid
/// bytes.
///
/// It uses [`HeaderName::from_bytes`] internally.
impl<'a> TryFrom<&'a str> for HeaderName {
    type Error = InvalidHeaderName;
    #[inline]
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

/// Turn a &String to a [`HeaderName`], if it does not contain any invalid
/// bytes.
///
/// It uses [`HeaderName::from_bytes`] internally.
impl<'a> TryFrom<&'a String> for HeaderName {
    type Error = InvalidHeaderName;
    #[inline]
    fn try_from(s: &'a String) -> Result<Self, Self::Error> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

/// Turn a [`&[u8]`] to a [`HeaderName`], if it does not contain any invalid
/// bytes.
///
/// It uses [`HeaderName::from_bytes`] internally.
impl<'a> TryFrom<&'a [u8]> for HeaderName {
    type Error = InvalidHeaderName;
    #[inline]
    fn try_from(s: &'a [u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(s).map_err(|_| InvalidHeaderName::new())
    }
}

/// Turn a [`String`] to a [`HeaderName`], if it does not contain any invalid
/// bytes.
///
/// It uses [`HeaderName::from_bytes`] internally.
impl TryFrom<String> for HeaderName {
    type Error = InvalidHeaderName;

    #[inline]
    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

/// Turn a [`Vec<u8>`] to a [`HeaderName`], if it does not contain any invalid
/// bytes.
///
/// It uses [`HeaderName::from_bytes`] internally.
impl TryFrom<Vec<u8>> for HeaderName {
    type Error = InvalidHeaderName;

    #[inline]
    fn try_from(vec: Vec<u8>) -> Result<Self, Self::Error> {
        Self::from_bytes(&vec).map_err(|_| InvalidHeaderName::new())
    }
}

#[doc(hidden)]
impl From<StandardHeader> for HeaderName {
    fn from(src: StandardHeader) -> Self {
        Self {
            inner: Repr::Standard(src),
        }
    }
}

#[doc(hidden)]
impl From<Custom> for HeaderName {
    fn from(src: Custom) -> Self {
        Self {
            inner: Repr::Custom(src),
        }
    }
}

/// Performs a case-insensitive comparison.
impl<'a> PartialEq<&'a Self> for HeaderName {
    #[inline]
    fn eq(&self, other: &&'a Self) -> bool {
        *self == **other
    }
}

/// Performs a case-insensitive comparison.
impl PartialEq<HeaderName> for &HeaderName {
    #[inline]
    fn eq(&self, other: &HeaderName) -> bool {
        *other == *self
    }
}

/// Performs a case-insensitive comparison.
impl PartialEq<str> for HeaderName {
    /// Performs a case-insensitive comparison of the string against the header
    /// name
    ///
    /// # Examples
    ///
    /// ```
    /// use melib::email::headers::HeaderName;
    ///
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "content-length");
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "Content-Length");
    /// assert_ne!(HeaderName::CONTENT_LENGTH, "content length");
    /// ```
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

/// Performs a case-insensitive comparison.
impl PartialEq<HeaderName> for str {
    /// Performs a case-insensitive comparison of the string against the header
    /// name
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use melib::email::headers::HeaderName;
    ///
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "content-length");
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "Content-Length");
    /// assert_ne!(HeaderName::CONTENT_LENGTH, "content length");
    /// assert_eq!(
    ///     HeaderName::CONTENT_LENGTH,
    ///     HeaderName::try_from("content-length").unwrap()
    /// );
    /// ```
    #[inline]
    fn eq(&self, other: &HeaderName) -> bool {
        *other == *self
    }
}

/// Performs a case-insensitive comparison.
impl<'a> PartialEq<&'a str> for HeaderName {
    #[inline]
    fn eq(&self, other: &&'a str) -> bool {
        *self == **other
    }
}

/// Performs a case-insensitive comparison.
impl PartialEq<HeaderName> for &str {
    #[inline]
    fn eq(&self, other: &HeaderName) -> bool {
        *other == *self
    }
}

impl Hash for Custom {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for b in self.0.as_slice() {
            hasher.write_u8(b.to_ascii_lowercase())
        }
    }
}

const UPPERCASE_TOKENS: &[&str] = &[
    "ARC", "DKIM", "DL", "EDIINT", "ID", "IPMS", "MD5", "MIME", "MT", "MTS", "NNTP", "PICS", "RSS",
    "SIO", "SPF", "TLS", "VBR",
];

struct HeaderNameFmt<'a>(&'a str);

impl std::fmt::Display for HeaderNameFmt<'_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let as_str = self.0;
        let len = as_str.len();
        let mut bytes_count = 0;
        for chunk in as_str.split('-') {
            if let Ok(tok) = UPPERCASE_TOKENS.binary_search_by(|probe| {
                if probe.eq_ignore_ascii_case(chunk) {
                    Ordering::Equal
                } else {
                    let mut iter = AsciiIgnoreCaseCmp {
                        ord: Ordering::Equal,
                        a: probe.as_bytes(),
                        b: chunk.as_bytes(),
                    };
                    let _cnt: usize = iter.by_ref().fuse().count();
                    debug_assert!(
                        _cnt <= probe.len(),
                        "_cnt {} should be lte probe.len() {}, for probe {} and chunk {}",
                        _cnt,
                        probe.len(),
                        probe,
                        chunk
                    );
                    debug_assert!(
                        _cnt <= chunk.len(),
                        "_cnt {} should be lte chunk.len() {}, for probe {} and chunk {}",
                        _cnt,
                        chunk.len(),
                        probe,
                        chunk
                    );
                    iter.ord
                }
            }) {
                write!(fmt, "{}", UPPERCASE_TOKENS[tok])?;
            } else {
                if let Some(first) = chunk.chars().next() {
                    write!(fmt, "{}", first.to_ascii_uppercase())?;
                }
                for ch in chunk.chars().skip(1) {
                    write!(fmt, "{}", ch.to_ascii_lowercase())?
                }
            }
            bytes_count += chunk.len();
            if bytes_count != len {
                bytes_count += 1;
                write!(fmt, "-")?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Custom {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", HeaderNameFmt(self.as_str()))
    }
}

// an iterator which alternates between Some and None
struct AsciiIgnoreCaseCmp<'a, 'b> {
    ord: Ordering,
    a: &'a [u8],
    b: &'b [u8],
}

impl Iterator for AsciiIgnoreCaseCmp<'_, '_> {
    type Item = ();

    fn next(&mut self) -> Option<()> {
        match (self.a.first(), self.b.first()) {
            (Some(a_char), Some(b_char)) => {
                self.ord = a_char
                    .to_ascii_lowercase()
                    .cmp(&b_char.to_ascii_lowercase());
                self.a = &self.a[1..];
                self.b = &self.b[1..];
                if self.ord == Ordering::Equal {
                    Some(())
                } else {
                    None
                }
            }
            (Some(_), None) => {
                self.ord = Ordering::Greater;
                None
            }
            (None, Some(_)) => {
                self.ord = Ordering::Less;
                None
            }
            (None, None) => None,
        }
    }
}
