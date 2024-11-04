/*
 * MIT License
 *
 * Copyright (c) 2021 Ilya Medvedev
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/* Code from <https://github.com/iam-medvedev/rust-utf7-imap> */

//! A Rust library for encoding and decoding [UTF-7] strings.
//!
//! A Rust library for encoding and decoding [UTF-7] strings. as defined by the
//! [IMAP] standard in [RFC 3501 (#5.1.3)].
//!
//! Idea is based on Python [mutf7] library.
//!
//! [UTF-7]: <https://datatracker.ietf.org/doc/html/rfc2152>
//! [IMAP]: <https://datatracker.ietf.org/doc/html/rfc3501>
//! [RFC 3501 (#5.1.3)]: <https://datatracker.ietf.org/doc/html/rfc3501#section-5.1.3>
//! [mutf7]: <https://github.com/cheshire-mouse/mutf7>

use encoding_rs::UTF_16BE;
use regex::{Captures, Regex};

/// Encode UTF-7 IMAP mailbox name
///
/// <https://datatracker.ietf.org/doc/html/rfc3501#section-5.1.3>
pub fn encode_utf7_imap(text: &str) -> String {
    let mut result = "".to_string();
    let text = text.replace('&', "&-");
    let mut text = text.as_str();
    while !text.is_empty() {
        result = format!("{}{}", result, get_ascii(text));
        text = remove_ascii(text);
        if !text.is_empty() {
            let tmp = get_nonascii(text);
            result = format!("{}{}", result, encode_modified_utf7(tmp));
            text = remove_nonascii(text);
        }
    }
    result
}
fn is_ascii_custom(c: u8) -> bool {
    (0x20..=0x7f).contains(&c)
}

fn get_ascii(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if !is_ascii_custom(item) {
            return &s[0..i];
        }
    }
    s
}

fn get_nonascii(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if is_ascii_custom(item) {
            return &s[0..i];
        }
    }
    s
}

fn remove_ascii(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if !is_ascii_custom(item) {
            return &s[i..];
        }
    }
    ""
}

fn remove_nonascii(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if is_ascii_custom(item) {
            return &s[i..];
        }
    }
    ""
}

fn encode_modified_utf7(text: &str) -> String {
    let capacity = 2 * text.len();
    let mut input = Vec::with_capacity(capacity);
    let text_u16 = text.encode_utf16();
    for value in text_u16 {
        input.extend_from_slice(&value.to_be_bytes());
    }
    #[allow(deprecated)]
    let text_u16 = base64::encode(input);
    let text_u16 = text_u16.trim_end_matches('=');
    let result = text_u16.replace('/', ",");
    format!("&{}-", result)
}

/// Decode UTF-7 IMAP mailbox name
///
/// <https://datatracker.ietf.org/doc/html/rfc3501#section-5.1.3>
pub fn decode_utf7_imap(text: &str) -> String {
    let pattern = Regex::new(r"&([^-]*)-").unwrap();
    pattern.replace_all(text, expand).to_string()
}

fn expand(cap: &Captures) -> String {
    if cap.get(1).unwrap().as_str() == "" {
        "&".to_string()
    } else {
        decode_utf7_part(cap.get(0).unwrap().as_str())
    }
}

fn decode_utf7_part(text: &str) -> String {
    if text == "&-" {
        return String::from("&");
    }

    let text_mb64 = &text[1..text.len() - 1];
    let mut text_b64 = text_mb64.replace(',', "/");

    while (text_b64.len() % 4) != 0 {
        text_b64 += "=";
    }

    #[allow(deprecated)]
    let text_u16 = base64::decode(text_b64).unwrap();
    let (cow, _encoding_used, _had_errors) = UTF_16BE.decode(&text_u16);
    let result = cow.as_ref();

    String::from(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn encode_test() {
        assert_eq!(
            encode_utf7_imap("Отправленные"),
            "&BB4EQgQ,BEAEMAQyBDsENQQ9BD0ESwQ1-"
        );
    }
    #[test]
    fn encode_test_split() {
        assert_eq!(
            encode_utf7_imap("Šiukšliadėžė"),
            "&AWA-iuk&AWE-liad&ARcBfgEX-"
        )
    }

    #[test]
    fn encode_consecutive_accents() {
        assert_eq!(encode_utf7_imap("théâtre"), "th&AOkA4g-tre")
    }

    #[test]
    fn decode_test() {
        assert_eq!(
            decode_utf7_imap("&BB4EQgQ,BEAEMAQyBDsENQQ9BD0ESwQ1-"),
            "Отправленные"
        );
    }
    #[test]
    fn decode_test_split() {
        // input string with utf7 encoded bits being separated by ascii
        assert_eq!(
            decode_utf7_imap("&AWA-iuk&AWE-liad&ARcBfgEX-"),
            "Šiukšliadėžė"
        )
    }

    #[test]
    fn decode_consecutive_accents() {
        assert_eq!(decode_utf7_imap("th&AOkA4g-tre"), "théâtre")
    }
}
