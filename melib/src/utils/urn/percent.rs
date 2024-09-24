// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: Copyright 2021, 2022, 2023 chayleaf
// <chayleaf-cratesio@pavluk.org>

//! This module contains functions for percent-encoding and decoding various
//! components of a URN.

use super::{Error, Result, TriCow};

/// Different components are percent-encoded differently...
#[derive(Clone, Copy)]
enum PctEncoded {
    Nss,
    RComponent,
    QComponent,
    FComponent,
}

/// Parse and normalize percent-encoded string. Returns the end.
fn parse(s: &mut TriCow, start: usize, kind: PctEncoded) -> Result<usize> {
    let mut it = s.bytes().enumerate().skip(start).peekable();
    while let Some((i, ch)) = it.next() {
        #[allow(clippy::match_same_arms)]
        match (kind, ch) {
            /* question mark handling */
            // ? is always allowed in f-components
            (PctEncoded::FComponent, b'?') => {}
            // ? is a valid part of q-component if not at the start
            (PctEncoded::QComponent, b'?') if i != start => {}
            // ? is a valid part of r-component if not at the start, but ?= indicates the q-component start, so only allow the ? if it isn't followed by =
            (PctEncoded::RComponent, b'?') if i != start && it.peek().map(|x| x.1) != Some(b'=') => {}
            /* slash handling */
            // slash is uniquely allowed at the start of f-component...
            (PctEncoded::FComponent, b'/') => {}
            // ...but it's allowed everywhere if it isn't at the start
            (_, b'/') if i != start => {}
            /* the rest is handled the same everywhere */
            // various symbols that are allowed as pchar
            (
                _,
                // unreserved = ALPHA / DIGIT / <the following symbols>
                b'-' | b'.' | b'_' | b'~'
                // sub-delims = <the following symbols>
                | b'!' | b'$' | b'&' | b'\'' | b'(' | b')' | b'*' | b'+' | b',' | b';' | b'='
                // pchar = unreserved / pct-encoded / sub-delims / <the following symbols>
                | b':' | b'@',
            ) => {}
            // pct-encoded = "%" HEXDIG HEXDIG
            // HEXDIG =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
            // (ABNF strings are case insensitive)
            (_, b'%') => {
                let mut pct_chars = it.take(2);
                if pct_chars.len() == 2 && pct_chars.all(|x| x.1.is_ascii_hexdigit()) {
                    // percent encoding must be normalized by uppercasing it
                    s.make_uppercase(i + 1..i + 3)?;
                    it = s.bytes().enumerate().skip(i + 3).peekable();
                } else {
                    return Ok(i);
                }
            }
            // ALPHA / DIGIT
            (_, c) if c.is_ascii_alphanumeric() => {}
            // other characters can't be part of this component, so this is the end
            _ => return Ok(i),
        }
    }
    // this was the last component!
    Ok(s.len())
}

/// Returns the NSS end
pub(super) fn parse_nss(s: &mut TriCow, start: usize) -> Result<usize> {
    parse(s, start, PctEncoded::Nss)
}
/// Returns the r-component end
pub(super) fn parse_r_component(s: &mut TriCow, start: usize) -> Result<usize> {
    parse(s, start, PctEncoded::RComponent)
}
/// Returns the q-component end
pub(super) fn parse_q_component(s: &mut TriCow, start: usize) -> Result<usize> {
    parse(s, start, PctEncoded::QComponent)
}
/// Returns the f-component end
pub(super) fn parse_f_component(s: &mut TriCow, start: usize) -> Result<usize> {
    parse(s, start, PctEncoded::FComponent)
}

/// must be a hex digit
const fn parse_hex_char(ch: u8) -> u8 {
    if ch.is_ascii_digit() {
        ch - b'0'
    } else if ch.is_ascii_lowercase() {
        ch - b'a' + 0xA
    } else {
        ch - b'A' + 0xA
    }
}

fn decode(s: &str, kind: PctEncoded) -> Option<String> {
    #[allow(clippy::iter_skip_zero)]
    let mut it = s.bytes().enumerate().skip(0).peekable();
    let mut ret = Vec::new();

    while let Some((i, ch)) = it.next() {
        #[allow(clippy::match_same_arms)]
        match (kind, ch) {
            (PctEncoded::FComponent, b'?') => {}
            (PctEncoded::QComponent, b'?') if i != 0 => {}
            (PctEncoded::RComponent, b'?') if i != 0 && it.peek().map(|x| x.1) != Some(b'=') => {}
            (PctEncoded::FComponent, b'/') => {}
            (_, b'/') if i != 0 => {}
            (
                _,
                b'-' | b'.' | b'_' | b'~' | b'!' | b'$' | b'&' | b'\'' | b'(' | b')' | b'*' | b'+'
                | b',' | b';' | b'=' | b':' | b'@',
            ) => {}
            (_, b'%') => {
                let mut pct_chars = it.take(2);
                if pct_chars.len() == 2 && pct_chars.all(|x| x.1.is_ascii_hexdigit()) {
                    ret.push(
                        parse_hex_char(s.as_bytes()[i + 1]) * 0x10
                            + parse_hex_char(s.as_bytes()[i + 2]),
                    );
                    it = s.bytes().enumerate().skip(i + 3).peekable();
                    continue;
                }
                return None;
            }
            (_, c) if c.is_ascii_alphanumeric() => {}
            _ => return None,
        }
        ret.push(ch);
    }
    String::from_utf8(ret).ok()
}

/// Percent-decode a NSS according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, Urn};
/// # fn test_main() -> Result<(), urn::Error> {
/// let urn = Urn::try_from("urn:example:string%20with%20spaces")?;
///
/// assert_eq!(urn::percent::decode_nss(urn.nss())?, "string with spaces");
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidNss`] in case of a validation failure.
pub fn decode_nss(s: &str) -> Result<String> {
    decode(s, PctEncoded::Nss).ok_or(Error::InvalidNss)
}
/// Percent-decode an r-component according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, Urn};
/// # fn test_main() -> Result<(), urn::Error> {
/// let urn = Urn::try_from("urn:example:nss?+this%20is%20the%20r-component!")?;
///
/// assert_eq!(
///     urn::percent::decode_r_component(urn.r_component().unwrap())?,
///     "this is the r-component!"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidRComponent`] in case of a validation failure.
pub fn decode_r_component(s: &str) -> Result<String> {
    decode(s, PctEncoded::RComponent).ok_or(Error::InvalidRComponent)
}
/// Percent-decode a q-component according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, Urn};
/// # fn test_main() -> Result<(), urn::Error> {
/// let urn = Urn::try_from("urn:example:nss?=this%20is%20the%20q-component!")?;
///
/// assert_eq!(
///     urn::percent::decode_q_component(urn.q_component().unwrap())?,
///     "this is the q-component!"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidQComponent`] in case of a validation failure.
pub fn decode_q_component(s: &str) -> Result<String> {
    decode(s, PctEncoded::QComponent).ok_or(Error::InvalidQComponent)
}
/// Percent-decode an f-component according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, Urn};
/// # fn test_main() -> Result<(), urn::Error> {
/// let urn = Urn::try_from("urn:example:nss#f-component%20test")?;
///
/// assert_eq!(
///     urn::percent::decode_f_component(urn.f_component().unwrap())?,
///     "f-component test"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidFComponent`] in case of a validation failure.
pub fn decode_f_component(s: &str) -> Result<String> {
    decode(s, PctEncoded::FComponent).ok_or(Error::InvalidFComponent)
}

const fn to_hex(n: u8) -> [u8; 2] {
    let a = (n & 0xF0) >> 4;
    let b = n & 0xF;
    let a = if a < 10 { b'0' + a } else { b'A' + (a - 10) };
    let b = if b < 10 { b'0' + b } else { b'A' + (b - 10) };
    [a, b]
}

fn encode(s: &str, kind: PctEncoded) -> String {
    let mut ret = String::with_capacity(s.len());
    let mut buf = [0u8; 8];
    for (i, ch) in s.chars().enumerate() {
        #[allow(clippy::match_same_arms)]
        match (kind, ch) {
            // ? and / are reserved chars in RFC2141, so they can be included
            (PctEncoded::FComponent, '?') => {}
            (PctEncoded::QComponent, '?') if i != 0 => {}
            (PctEncoded::RComponent, '?')
                if i != 0 && !matches!(s.chars().nth(i + 1), Some('=')) => {}
            (PctEncoded::FComponent, '/') => {}
            // For RFC2141 compatibility, omit / in NSS
            (PctEncoded::RComponent | PctEncoded::QComponent, '/') if i != 0 => {}
            // & is reserved in RFC2141, but ~ isn't, omit it
            (
                PctEncoded::RComponent | PctEncoded::QComponent | PctEncoded::FComponent,
                '-' | '.' | '_' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '='
                | ':' | '@',
            ) => {}
            // In NSS, omit both ~ and &
            (
                PctEncoded::Nss,
                '-' | '.' | '_' | '!' | '$' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | ':'
                | '@',
            ) => {}
            (_, ch) if ch.is_ascii_alphanumeric() => {}
            (_, ch) => {
                for byte in ch.encode_utf8(&mut buf).as_bytes() {
                    ret.push('%');
                    for digit in to_hex(*byte) {
                        ret.push(digit as char);
                    }
                }
                continue;
            }
        }
        ret.push(ch);
    }
    ret
}

/// Percent-decode a NSS according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, UrnBuilder};
/// # fn test_main() -> Result<(), urn::Error> {
/// assert_eq!(
///     UrnBuilder::new("example", &urn::percent::encode_nss("test nss")?)
///         .build()?
///         .as_str(),
///     "urn:example:test%20nss"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidNss`] when attempting to encode an empty string.
pub fn encode_nss(s: &str) -> Result<String> {
    if s.is_empty() {
        return Err(Error::InvalidNss);
    }
    Ok(encode(s, PctEncoded::Nss))
}
/// Percent-decode an r-component according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, UrnBuilder};
/// # fn test_main() -> Result<(), urn::Error> {
/// assert_eq!(
///     UrnBuilder::new("example", "nss")
///         .r_component(Some(&urn::percent::encode_r_component("😂😂💯")?))
///         .build()?
///         .as_str(),
///     "urn:example:nss?+%F0%9F%98%82%F0%9F%98%82%F0%9F%92%AF"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidRComponent`] when attempting to encode an empty
/// string.
pub fn encode_r_component(s: &str) -> Result<String> {
    if s.is_empty() {
        return Err(Error::InvalidRComponent);
    }
    Ok(encode(s, PctEncoded::RComponent))
}
/// Percent-decode a q-component according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, UrnBuilder};
/// # fn test_main() -> Result<(), urn::Error> {
/// assert_eq!(
///     UrnBuilder::new("example", "nss")
///         .q_component(Some(&urn::percent::encode_q_component("~q component~")?))
///         .build()?
///         .as_str(),
///     "urn:example:nss?=%7Eq%20component%7E"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// Returns [`Error::InvalidQComponent`] when attempting to encode an empty
/// string.
pub fn encode_q_component(s: &str) -> Result<String> {
    if s.is_empty() {
        return Err(Error::InvalidQComponent);
    }
    Ok(encode(s, PctEncoded::QComponent))
}
/// Percent-decode an f-component according to the RFC
///
/// ```
/// # use melib::utils::urn::{self, UrnBuilder};
/// # fn test_main() -> Result<(), urn::Error> {
/// assert_eq!(
///     UrnBuilder::new("example", "nss")
///         .f_component(Some(&urn::percent::encode_f_component(
///             "f-component (pretty much a fragment)"
///         )?))
///         .build()?
///         .as_str(),
///     "urn:example:nss#f-component%20(pretty%20much%20a%20fragment)"
/// );
/// # Ok(()) } test_main().unwrap();
/// ```
///
/// # Errors
/// None, this function returns a `Result` for API consistency. If the URN
/// standard gets extended in the future, this may return
/// `Error::InvalidFComponent`.
pub fn encode_f_component(s: &str) -> Result<String> {
    // fragment is allowed to be empty
    Ok(encode(s, PctEncoded::FComponent))
}
