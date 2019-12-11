/*
 * meli - parser module
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
use super::*;
use chrono;
use data_encoding::BASE64_MIME;
use encoding::{DecoderTrap, Encoding};
use nom::{is_hex_digit, le_u8};
pub(super) use nom::{ErrorKind, IResult, Needed};

use encoding::all::*;
use std;

macro_rules! is_ctl_or_space {
    ($var:ident) => {
        /* <any ASCII control character and DEL> */
        $var < 33 || $var == 127
    };
    ($var:expr) => {
        /* <any ASCII control character and DEL> */
        $var < 33 || $var == 127
    };
}

macro_rules! is_whitespace {
    ($var:ident) => {
        $var == b' ' || $var == b'\t' || $var == b'\n' || $var == b'\r'
    };
    ($var:expr) => {
        $var == b' ' || $var == b'\t' || $var == b'\n' || $var == b'\r'
    };
}

pub trait BytesExt {
    fn rtrim(&self) -> &Self;
    fn ltrim(&self) -> &Self;
    fn trim(&self) -> &Self;
    fn find(&self, needle: &[u8]) -> Option<usize>;
    fn rfind(&self, needle: &[u8]) -> Option<usize>;
    fn replace(&self, from: &[u8], to: &[u8]) -> Vec<u8>;
    fn is_quoted(&self) -> bool;
}

impl BytesExt for [u8] {
    fn rtrim(&self) -> &Self {
        if let Some(last) = self.iter().rposition(|b| !is_whitespace!(*b)) {
            &self[..=last]
        } else {
            &[]
        }
    }
    fn ltrim(&self) -> &Self {
        if let Some(first) = self.iter().position(|b| !is_whitespace!(*b)) {
            &self[first..]
        } else {
            &[]
        }
    }
    fn trim(&self) -> &[u8] {
        self.rtrim().ltrim()
    }
    // https://stackoverflow.com/a/35907071
    fn find(&self, needle: &[u8]) -> Option<usize> {
        self.windows(needle.len())
            .position(|window| window == needle)
    }

    fn rfind(&self, needle: &[u8]) -> Option<usize> {
        self.windows(needle.len())
            .rposition(|window| window == needle)
    }

    fn replace(&self, from: &[u8], to: &[u8]) -> Vec<u8> {
        let mut ret = self.to_vec();
        if let Some(idx) = self.find(from) {
            ret.splice(idx..(idx + from.len()), to.iter().cloned());
        }
        ret
    }

    fn is_quoted(&self) -> bool {
        self.starts_with(b"\"") && self.ends_with(b"\"") && self.len() > 1
    }
}

fn quoted_printable_byte(input: &[u8]) -> IResult<&[u8], u8> {
    if input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if input[0] == b'=' && is_hex_digit(input[1]) && is_hex_digit(input[2]) {
        let a = if input[1] < b':' {
            input[1] - 48
        } else if input[1] < b'[' {
            input[1] - 55
        } else {
            input[1] - 87
        };
        let b = if input[2] < b':' {
            input[2] - 48
        } else if input[2] < b'[' {
            input[2] - 55
        } else {
            input[2] - 87
        };
        IResult::Done(&input[3..], a * 16 + b)
    } else if input.starts_with(b"\r\n") {
        IResult::Done(&input[2..], b'\n')
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

// Parser definition

/* A header can span multiple lines, eg:
 *
 * Received: from -------------------- (-------------------------)
 * 	by --------------------- (--------------------- [------------------]) (-----------------------)
 * 	with ESMTP id ------------ for <------------------->;
 * 	Tue,  5 Jan 2016 21:30:44 +0100 (CET)
 */

fn header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let input_len = input.len();
    for (i, x) in input.iter().enumerate() {
        if *x == b'\n'
            && (((i + 1) < input_len && input[i + 1] != b' ' && input[i + 1] != b'\t')
                || i + 1 == input_len)
        {
            return IResult::Done(&input[(i + 1)..], &input[0..i]);
        } else if input[i..].starts_with(b"\r\n")
            && (((i + 2) < input_len && input[i + 2] != b' ' && input[i + 2] != b'\t')
                || i + 2 == input_len)
        {
            return IResult::Done(&input[(i + 2)..], &input[0..i]);
        }
    }
    IResult::Incomplete(Needed::Unknown)
}

/* Parse a single header as a tuple */
fn header_with_val(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
    if input.is_empty() {
        return IResult::Incomplete(Needed::Unknown);
    } else if input.starts_with(b"\n") || input.starts_with(b"\r\n") {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    let mut ptr = 0;
    let mut name: &[u8] = &input[0..0];
    /* field-name  =  1*<any CHAR, excluding CTLs, SPACE, and ":"> */
    for (i, x) in input.iter().enumerate() {
        if *x == b':' {
            name = &input[0..i];
            ptr = i + 1;
            break;
        } else if is_ctl_or_space!(*x) {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    }
    if name.is_empty() {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    if ptr >= input.len() {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }

    if input[ptr] == b'\n' {
        ptr += 1;
        if ptr >= input.len() {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    } else if input[ptr..].starts_with(b"\r\n") {
        ptr += 2;
        if ptr > input.len() {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    }
    while input[ptr] == b' ' || input[ptr] == b'\t' {
        ptr += 1;
        if ptr >= input.len() {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    }
    match header_value(&input[ptr..]) {
        IResult::Done(rest, value) => IResult::Done(rest, (name, value)),
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(code) => IResult::Error(code),
    }
}

fn header_without_val(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
    if input.is_empty() {
        return IResult::Incomplete(Needed::Unknown);
    } else if input.starts_with(b"\n") || input.starts_with(b"\r\n") {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    let mut ptr = 0;
    let mut name: &[u8] = &input[0..0];
    let mut has_colon = false;
    /* field-name  =  1*<any CHAR, excluding CTLs, SPACE, and ":"> */
    for (i, x) in input.iter().enumerate() {
        if input[i..].starts_with(b"\r\n") {
            name = &input[0..i];
            ptr = i + 2;
            break;
        } else if *x == b':' || *x == b'\n' {
            name = &input[0..i];
            has_colon = true;
            ptr = i;
            break;
        } else if is_ctl_or_space!(*x) {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    }
    if name.is_empty() {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    if input[ptr] == b':' {
        ptr += 1;
        has_colon = true;
        if ptr >= input.len() {
            return IResult::Incomplete(Needed::Unknown);
        }
    }

    if !has_colon {
        return IResult::Incomplete(Needed::Unknown);
    }

    while input[ptr] == b' ' {
        ptr += 1;
        if ptr >= input.len() {
            return IResult::Incomplete(Needed::Unknown);
        }
    }
    if input[ptr..].starts_with(b"\n") {
        ptr += 1;
        if ptr >= input.len() {
            return IResult::Incomplete(Needed::Unknown);
        }
        if input.len() > ptr && input[ptr] != b' ' && input[ptr] != b'\t' {
            IResult::Done(&input[ptr..], (name, b""))
        } else {
            IResult::Error(error_code!(ErrorKind::Custom(43)))
        }
    } else if input[ptr..].starts_with(b"\r\n") {
        ptr += 2;
        if ptr > input.len() {
            return IResult::Incomplete(Needed::Unknown);
        }
        if input.len() > ptr && input[ptr] != b' ' && input[ptr] != b'\t' {
            IResult::Done(&input[ptr..], (name, b""))
        } else {
            IResult::Error(error_code!(ErrorKind::Custom(43)))
        }
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

named!(
    header<(&[u8], &[u8])>,
    alt_complete!(call!(header_without_val) | call!(header_with_val))
);
/* Parse all headers -> Vec<(&str, Vec<&str>)> */
named!(pub headers<std::vec::Vec<(&[u8], &[u8])>>,
       many1!(complete!(header)));

pub fn headers_raw(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() {
        return IResult::Incomplete(Needed::Unknown);
    }
    for i in 0..input.len() {
        if input[i..].starts_with(b"\n\n") {
            return IResult::Done(&input[(i + 1)..], &input[0..=i]);
        } else if input[i..].starts_with(b"\r\n\r\n") {
            return IResult::Done(&input[(i + 2)..], &input[0..=i]);
        }
    }
    IResult::Error(error_code!(ErrorKind::Custom(43)))
}

named!(pub body_raw<&[u8]>,
       do_parse!(
           alt_complete!(take_until1!("\n\n") | take_until1!("\r\n\r\n")) >>
           body: take_while!(call!(|_| true)) >>
           ( { body } )));

named!(pub mail<(std::vec::Vec<(&[u8], &[u8])>, &[u8])>,
       separated_pair!(headers, alt_complete!(tag!(b"\n") | tag!(b"\r\n")), take_while!(call!(|_| true))));

named!(pub attachment<(std::vec::Vec<(&[u8], &[u8])>, &[u8])>,
       do_parse!(
       pair: separated_pair!(many0!(complete!(header)), alt_complete!(tag!(b"\n") | tag!(b"\r\n")), take_while!(call!(|_| true))) >>
       ( { pair } )));

/* Header parsers */

/* Encoded words
 *"=?charset?encoding?encoded text?=".
 */
fn encoded_word(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if input.is_empty() {
        return IResult::Done(&[], Vec::with_capacity(0));
    }
    if input.len() < 5 {
        return IResult::Incomplete(Needed::Unknown);
    } else if input[0] != b'=' || input[1] != b'?' {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    /* find end of Charset tag:
     * =?charset?encoding?encoded text?=
     * ---------^
     */
    let mut tag_end_idx = None;
    for (idx, b) in input[2..].iter().enumerate() {
        if *b == b'?' {
            tag_end_idx = Some(idx + 2);
            break;
        }
    }
    if tag_end_idx.is_none() {
        return IResult::Error(error_code!(ErrorKind::Custom(42)));
    }
    let tag_end_idx = tag_end_idx.unwrap();

    if input[2 + tag_end_idx] != b'?' {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    /* See if input ends with "?=" and get ending index
     * =?charset?encoding?encoded text?=
     * -------------------------------^
     */
    let mut encoded_end_idx = None;
    for i in (3 + tag_end_idx)..input.len() {
        if input[i] == b'?' && i < input.len() && input[i + 1] == b'=' {
            encoded_end_idx = Some(i);
            break;
        }
    }
    if encoded_end_idx.is_none() {
        return IResult::Error(error_code!(ErrorKind::Custom(44)));
    }
    let encoded_end_idx = encoded_end_idx.unwrap();
    let encoded_text = &input[3 + tag_end_idx..encoded_end_idx];

    let s: Vec<u8> = match input[tag_end_idx + 1] {
        b'b' | b'B' => match BASE64_MIME.decode(encoded_text) {
            Ok(v) => v,
            Err(_) => encoded_text.to_vec(),
        },
        b'q' | b'Q' => match quoted_printable_bytes_header(encoded_text) {
            IResult::Done(b"", s) => s,
            _ => return IResult::Error(error_code!(ErrorKind::Custom(45))),
        },
        _ => return IResult::Error(error_code!(ErrorKind::Custom(46))),
    };

    let charset = Charset::from(&input[2..tag_end_idx]);

    if let Charset::UTF8 = charset {
        IResult::Done(&input[encoded_end_idx + 2..], s)
    } else {
        match decode_charset(&s, charset) {
            Ok(v) => IResult::Done(&input[encoded_end_idx + 2..], v.into_bytes()),
            _ => IResult::Error(error_code!(ErrorKind::Custom(43))),
        }
    }
}

pub fn decode_charset(s: &[u8], charset: Charset) -> Result<String> {
    match charset {
        Charset::UTF8 | Charset::Ascii => Ok(String::from_utf8_lossy(s).to_string()),
        Charset::ISO8859_1 => Ok(ISO_8859_1.decode(s, DecoderTrap::Strict)?),
        Charset::ISO8859_2 => Ok(ISO_8859_2.decode(s, DecoderTrap::Strict)?),
        Charset::ISO8859_7 => Ok(ISO_8859_7.decode(s, DecoderTrap::Strict)?),
        Charset::ISO8859_15 => Ok(ISO_8859_15.decode(s, DecoderTrap::Strict)?),
        Charset::GBK => Ok(GBK.decode(s, DecoderTrap::Strict)?),
        Charset::Windows1251 => Ok(WINDOWS_1251.decode(s, DecoderTrap::Strict)?),
        Charset::Windows1252 => Ok(WINDOWS_1252.decode(s, DecoderTrap::Strict)?),
        Charset::Windows1253 => Ok(WINDOWS_1253.decode(s, DecoderTrap::Strict)?),
        // Unimplemented:
        Charset::GB2312 => Ok(String::from_utf8_lossy(s).to_string()),
        Charset::UTF16 => Ok(String::from_utf8_lossy(s).to_string()),
        Charset::BIG5 => Ok(String::from_utf8_lossy(s).to_string()),
        Charset::ISO2022JP => Ok(String::from_utf8_lossy(s).to_string()),
    }
}

fn quoted_printable_soft_break(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.len() < 2 {
        IResult::Incomplete(Needed::Size(1))
    } else if input[0] == b'=' && input[1] == b'\n' {
        IResult::Done(&input[2..], &input[0..2]) // `=\n` is an escaped space character.
    } else if input.len() > 3 && input.starts_with(b"=\r\n") {
        IResult::Done(&input[3..], &input[0..3]) // `=\r\n` is an escaped space character.
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

named!(
    qp_underscore_header<u8>,
    do_parse!(tag!(b"_") >> ({ 0x20 }))
);

// With MIME, headers in quoted printable format can contain underscores that represent spaces.
// In non-header context, an underscore is just a plain underscore.
named!(
    pub quoted_printable_bytes_header<Vec<u8>>,
    many0!(alt_complete!(
        quoted_printable_byte | qp_underscore_header | le_u8
    ))
);

// For atoms in Header values.
named!(
    pub quoted_printable_bytes<Vec<u8>>,
    many0!(alt_complete!(
        preceded!(quoted_printable_soft_break, quoted_printable_byte) |
        preceded!(quoted_printable_soft_break, le_u8) | quoted_printable_byte | le_u8
    ))
);

fn display_addr(input: &[u8]) -> IResult<&[u8], Address> {
    if input.is_empty() || input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if !is_whitespace!(input[0]) {
        let mut display_name = StrBuilder {
            offset: 0,
            length: 0,
        };
        let mut flag = false;
        for (i, b) in input[0..].iter().enumerate() {
            if *b == b'<' {
                display_name.length = i.saturating_sub(1); // if i != 0 { i - 1 } else { 0 };
                flag = true;
                break;
            }
        }
        if !flag {
            let (rest, output) = match phrase(input) {
                IResult::Done(rest, raw) => (rest, raw),
                _ => return IResult::Error(error_code!(ErrorKind::Custom(43))),
            };
            if output.contains(&b'<') {
                match display_addr(&output) {
                    IResult::Done(_, address) => return IResult::Done(rest, address),
                    _ => return IResult::Error(error_code!(ErrorKind::Custom(43))),
                }
            }
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
        let mut end = input.len();
        let mut flag = false;
        for (i, b) in input[display_name.length + 2..].iter().enumerate() {
            match *b {
                b'@' => flag = true,
                b'>' => {
                    end = i;
                    break;
                }
                _ => {}
            }
        }
        if flag {
            match phrase(&input[0..end + display_name.length + 3]) {
                IResult::Error(e) => IResult::Error(e),
                IResult::Incomplete(i) => IResult::Incomplete(i),
                IResult::Done(_, raw) => {
                    let display_name_end = raw.find(b"<").unwrap();
                    display_name.length = { raw[0..display_name_end].trim().len() };
                    let address_spec = if display_name_end == 0 {
                        StrBuilder {
                            offset: 1,
                            length: end + 1,
                        }
                    } else {
                        StrBuilder {
                            offset: display_name_end + 1,
                            length: end,
                        }
                    };

                    if display_name.display(&raw).as_bytes().is_quoted() {
                        display_name.offset += 1;
                        display_name.length -= 2;
                    }

                    let rest_start = if input.len() > end + display_name.length + 2 {
                        end + display_name.length + 3
                    } else {
                        end + display_name.length + 2
                    };

                    IResult::Done(
                        &input[rest_start..],
                        Address::Mailbox(MailboxAddress {
                            raw,
                            display_name,
                            address_spec,
                        }),
                    )
                }
            }
        } else {
            IResult::Error(error_code!(ErrorKind::Custom(43)))
        }
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

fn addr_spec(input: &[u8]) -> IResult<&[u8], Address> {
    if input.is_empty() || input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if !is_whitespace!(input[0]) {
        let mut end = input[1..].len();
        let mut flag = false;
        for (i, b) in input[1..].iter().enumerate() {
            if *b == b'@' {
                flag = true;
            }
            if is_whitespace!(*b) {
                end = i;
                break;
            }
        }
        if flag {
            IResult::Done(
                &input[end..],
                Address::Mailbox(MailboxAddress {
                    raw: input[0..=end].into(),
                    display_name: StrBuilder {
                        offset: 0,
                        length: 0,
                    },
                    address_spec: StrBuilder {
                        offset: 0,
                        length: input[0..=end].len(),
                    },
                }),
            )
        } else {
            IResult::Error(error_code!(ErrorKind::Custom(43)))
        }
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(42)))
    }
}

named!(
    pub mailbox<Address>,
    ws!(alt_complete!(display_addr | addr_spec))
);
named!(mailbox_list<Vec<Address>>, many0!(mailbox));

/*
 * group of recipients eg. undisclosed-recipients;
 */
fn group(input: &[u8]) -> IResult<&[u8], Address> {
    let mut flag = false;
    let mut dlength = 0;
    for (i, b) in input.iter().enumerate() {
        if *b == b':' {
            flag = true;
            dlength = i;
            break;
        }
    }
    if !flag {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }

    match mailbox_list(&input[dlength..]) {
        IResult::Error(e) => IResult::Error(e),
        IResult::Done(rest, vec) => {
            let size: usize =
                (rest.as_ptr() as usize).wrapping_sub((&input[0..] as &[u8]).as_ptr() as usize);
            IResult::Done(
                rest,
                Address::Group(GroupAddress {
                    raw: input[0..size].into(),
                    display_name: StrBuilder {
                        offset: 0,
                        length: dlength,
                    },
                    mailbox_list: vec,
                }),
            )
        }
        IResult::Incomplete(i) => IResult::Incomplete(i),
    }
}

named!(pub address<Address>, ws!(alt_complete!(mailbox | group)));

named!(pub rfc2822address_list<Vec<Address>>, ws!( separated_list!(is_a!(","), address)));

named!(pub address_list<String>, ws!(do_parse!(
        list: alt_complete!( encoded_word_list | ascii_token) >>
        ( {
            let list: Vec<&[u8]> = list.split(|c| *c == b',').collect();
            let string_len = list.iter().fold(0, |mut acc, x| { acc+=x.trim().len(); acc }) + list.len() - 1;
            let list_len = list.len();
            let mut i = 0;
            list.iter().fold(String::with_capacity(string_len),
            |acc, x| {
                let mut acc = acc + &String::from_utf8_lossy(x.replace(b"\n", b"").replace(b"\r", b"").replace(b"\t", b" ").trim());
                if i != list_len - 1 {
                    acc.push_str(" ");
                    i+=1;
                }
                acc
            })
        } )

       )));

fn eat_comments(input: &[u8]) -> Vec<u8> {
    let mut in_comment = false;
    input
        .iter()
        .fold(Vec::with_capacity(input.len()), |mut acc, x| {
            if *x == b'(' && !in_comment {
                in_comment = true;
                acc
            } else if *x == b')' && in_comment {
                in_comment = false;
                acc
            } else if in_comment {
                acc
            } else {
                acc.push(*x);
                acc
            }
        })
}

/*
 * Date should tokenize input and convert the tokens,
 * right now we expect input will have no extra spaces in between tokens
 *
 * We should use a custom parser here*/
pub fn date(input: &[u8]) -> Option<chrono::DateTime<chrono::FixedOffset>> {
    let parsed_result = phrase(&eat_comments(input))
        .to_full_result()
        .unwrap()
        .replace(b"-", b"+");
    chrono::DateTime::parse_from_rfc2822(String::from_utf8_lossy(parsed_result.trim()).as_ref())
        .ok()
}

named!(pub message_id<&[u8]>,
        complete!(delimited!(tag!("<"), take_until1!(">"), tag!(">")))
 );

fn message_id_peek(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let input_length = input.len();
    if input.is_empty() {
        IResult::Incomplete(Needed::Size(1))
    } else if input_length == 2 || input[0] != b'<' {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    } else {
        for (i, &x) in input.iter().take(input_length).enumerate().skip(1) {
            if x == b'>' {
                return IResult::Done(&input[i + 1..], &input[0..=i]);
            }
        }
        IResult::Incomplete(Needed::Unknown)
    }
}

named!(pub references<Vec<&[u8]>>, separated_list!(complete!(is_a!(" \n\t\r")), message_id_peek));

pub fn multipart_parts<'a>(input: &'a [u8], boundary: &[u8]) -> IResult<&'a [u8], Vec<StrBuilder>> {
    let mut ret: Vec<_> = Vec::new();
    let mut input = input;
    let mut offset = 0;
    loop {
        let b_start = if let Some(v) = input.find(boundary) {
            v
        } else {
            return IResult::Error(error_code!(ErrorKind::Custom(39)));
        };

        if b_start < 2 {
            return IResult::Error(error_code!(ErrorKind::Custom(40)));
        }
        offset += b_start - 2;
        input = &input[b_start - 2..];
        if &input[0..2] == b"--" {
            offset += 2 + boundary.len();
            input = &input[2 + boundary.len()..];
            if input[0] == b'\n' {
                offset += 1;
                input = &input[1..];
            } else if input[0..].starts_with(b"\r\n") {
                offset += 2;
                input = &input[2..];
            } else {
                continue;
            }
            break;
        }
    }

    loop {
        if input.len() < boundary.len() + 4 {
            return IResult::Error(error_code!(ErrorKind::Custom(41)));
        }
        if let Some(end) = input.find(boundary) {
            if &input[end - 2..end] != b"--" {
                return IResult::Error(error_code!(ErrorKind::Custom(42)));
            }
            ret.push(StrBuilder {
                offset,
                length: end - 2,
            });
            offset += end + boundary.len();
            input = &input[end + boundary.len()..];
            if input.len() < 2 || input[0] != b'\n' || &input[0..2] == b"--" {
                break;
            }
            if input[0] == b'\n' {
                offset += 1;
                input = &input[1..];
            } else if input[0..].starts_with(b"\r\n") {
                offset += 2;
                input = &input[2..];
            }
            continue;
        } else {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    }
    IResult::Done(input, ret)
}

fn parts_f<'a>(input: &'a [u8], boundary: &[u8]) -> IResult<&'a [u8], Vec<&'a [u8]>> {
    let mut ret: Vec<&[u8]> = Vec::new();
    let mut input = input;
    loop {
        let b_start = if let Some(v) = input.find(boundary) {
            v
        } else {
            return IResult::Error(error_code!(ErrorKind::Custom(39)));
        };

        if b_start < 2 {
            return IResult::Error(error_code!(ErrorKind::Custom(40)));
        }
        input = &input[b_start - 2..];
        if &input[0..2] == b"--" {
            input = &input[2 + boundary.len()..];
            if input[0] == b'\n' {
                input = &input[1..];
            } else if input[0..].starts_with(b"\r\n") {
                input = &input[2..];
            } else {
                continue;
            }
            break;
        }
    }
    loop {
        if input.len() < boundary.len() + 4 {
            return IResult::Error(error_code!(ErrorKind::Custom(41)));
        }
        if let Some(end) = input.find(boundary) {
            if &input[end - 2..end] != b"--" {
                return IResult::Error(error_code!(ErrorKind::Custom(42)));
            }
            ret.push(&input[0..end - 2]);
            input = &input[end + boundary.len()..];
            if input.len() < 2
                || (input[0] != b'\n' && &input[0..2] != b"\r\n")
                || &input[0..2] == b"--"
            {
                break;
            }
            if input[0] == b'\n' {
                input = &input[1..];
            } else if input[0..].starts_with(b"\r\n") {
                input = &input[2..];
            }
            continue;
        } else {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
    }
    IResult::Done(input, ret)
}

named_args!(pub parts<'a>(boundary: &'a [u8]) < Vec<&'this_is_probably_unique_i_hope_please [u8]> >,
            alt_complete!(call!(parts_f, boundary) | do_parse!(
                        take_until_and_consume!(&b"--"[..]) >>
                        take_until_and_consume!(boundary) >>
                        ( { Vec::<&[u8]>::new() } ))
                    ));

/* Caution: values should be passed through phrase() */
named!(
    content_type_parameter<(&[u8], &[u8])>,
    do_parse!(
        tag!(";")
            >> name: terminated!(ws!(take_until!("=")), tag!("="))
            >> value:
                ws!(alt_complete!(
                    delimited!(tag!("\""), take_until!("\""), tag!("\"")) | is_not!(";")
                ))
            >> ({ (name, value) })
    )
);

named!(pub content_type< (&[u8], &[u8], Vec<(&[u8], &[u8])>) >,
       do_parse!(
           _type: take_until!("/") >>
           tag!("/") >>
           _subtype: is_not!(";") >>
           parameters: many0!(complete!(content_type_parameter)) >>
           ( {
               (_type, _subtype, parameters)
           } )
           ));

named!(pub space, eat_separator!(&b" \t\r\n"[..]));
named!(
    encoded_word_list<Vec<u8>>,
    ws!(do_parse!(
        list: separated_nonempty_list!(call!(space), encoded_word)
            >> ({
                let list_len = list.iter().fold(0, |mut acc, x| {
                    acc += x.len();
                    acc
                });
                list.iter()
                    .fold(Vec::with_capacity(list_len), |mut acc, x| {
                        acc.append(&mut x.clone());
                        acc
                    })
            })
    ))
);
named!(
    ascii_token<Vec<u8>>,
    do_parse!(
        word: alt_complete!(
            terminated!(
                take_until1!(" =?"),
                peek!(preceded!(tag!(b" "), call!(encoded_word)))
            ) | take_while!(call!(|_| true))
        ) >> ({ word.into() })
    )
);

pub fn phrase(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if input.is_empty() {
        return IResult::Done(&[], Vec::with_capacity(0));
    }

    let mut input = input.ltrim();
    let mut acc: Vec<u8> = Vec::new();
    let mut ptr = 0;

    while ptr < input.len() {
        let mut flag = false;
        // Check if word is encoded.
        while let IResult::Done(rest, v) = encoded_word(&input[ptr..]) {
            flag = true;
            input = rest;
            ptr = 0;
            acc.extend(v);

            // consume whitespace
            while ptr < input.len() && (is_whitespace!(input[ptr])) {
                ptr += 1;
            }

            if ptr >= input.len() {
                break;
            }
        }
        if flag && ptr < input.len() && ptr != 0 {
            acc.push(b' ');
        }
        let end = input[ptr..].find(b"=?");

        let end = end.unwrap_or_else(|| input.len() - ptr) + ptr;
        let ascii_s = ptr;
        let mut ascii_e;

        while ptr < end && !(is_whitespace!(input[ptr])) {
            ptr += 1;
        }

        ascii_e = ptr;
        while ptr < input.len() && (is_whitespace!(input[ptr])) {
            ptr += 1;
        }
        if ptr >= input.len() {
            acc.extend(
                ascii_token(&input[ascii_s..ascii_e])
                    .to_full_result()
                    .unwrap(),
            );
            break;
        }
        if ascii_s >= ascii_e {
            /* We have the start of an encoded word but not the end, so parse it as ascii */
            ascii_e = input[ascii_s..]
                .find(b" ")
                .unwrap_or_else(|| input[ascii_s..].len());
            ptr = ascii_e;
        }

        acc.extend(
            ascii_token(&input[ascii_s..ascii_e])
                .to_full_result()
                .unwrap(),
        );
        if ptr != ascii_e {
            acc.push(b' ');
        }
    }
    IResult::Done(&input[ptr..], acc)
}

named!(pub angle_bracket_delimeted_list<Vec<&[u8]>>, separated_nonempty_list!(complete!(is_a!(",")), ws!(complete!(complete!(delimited!(tag!("<"), take_until1!(">"), tag!(">")))))));

pub fn mailto(mut input: &[u8]) -> IResult<&[u8], Mailto> {
    if !input.starts_with(b"mailto:") {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }

    input = &input[b"mailto:".len()..];

    let end = input.iter().position(|e| *e == b'?').unwrap_or(input.len());
    let address: Address;

    if let IResult::Done(_, addr) = crate::email::parser::address(&input[..end]) {
        address = addr;
        input = if input[end..].is_empty() {
            &input[end..]
        } else {
            &input[end + 1..]
        };
    } else {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }

    let mut subject = None;
    let mut cc = None;
    let mut bcc = None;
    let mut body = None;
    while !input.is_empty() {
        let tag = if let Some(tag_pos) = input.iter().position(|e| *e == b'=') {
            let ret = &input[0..tag_pos];
            input = &input[tag_pos + 1..];
            ret
        } else {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        };

        let value_end = input.iter().position(|e| *e == b'&').unwrap_or(input.len());

        let value = String::from_utf8_lossy(&input[..value_end]).to_string();
        match tag {
            b"subject" if subject.is_none() => {
                subject = Some(value);
            }
            b"cc" if cc.is_none() => {
                cc = Some(value);
            }
            b"bcc" if bcc.is_none() => {
                bcc = Some(value);
            }
            b"body" if body.is_none() => {
                /* FIXME:
                 * Parse escaped characters properly.
                 */
                body = Some(value.replace("%20", " ").replace("%0A", "\n"));
            }
            _ => {
                return IResult::Error(error_code!(ErrorKind::Custom(43)));
            }
        }
        if input[value_end..].is_empty() {
            break;
        }
        input = &input[value_end + 1..];
    }
    IResult::Done(
        input,
        Mailto {
            address,
            subject,
            cc,
            bcc,
            body,
        },
    )
}

//alt_complete!(call!(header_without_val) | call!(header_with_val))

pub struct HeaderIterator<'a>(pub &'a [u8]);

impl<'a> Iterator for HeaderIterator<'a> {
    type Item = (&'a [u8], &'a [u8]);
    fn next(&mut self) -> Option<(&'a [u8], &'a [u8])> {
        if self.0.is_empty() {
            return None;
        }

        match header(self.0) {
            IResult::Done(rest, value) => {
                self.0 = rest;
                Some(value)
            }
            _ => {
                self.0 = &[];
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_subject() {
        let words = b"=?iso-8859-7?B?W215Y291cnNlcy5udHVhLmdyIC0gyvXs4fTp6t4g6uHpIMri4e306ere?=
     =?iso-8859-7?B?INb18+nq3l0gzd3hIMHt4erv3+358+c6IMzF0c/TIMHQz9TFy8XTzMHU?=
      =?iso-8859-7?B?2c0gwiDUzC4gysHNLiDFzsXUwdPH0yAyMDE3LTE4OiDTx8zFydnTxw==?=";
        assert!("[mycourses.ntua.gr - Κυματική και Κβαντική Φυσική] Νέα Ανακοίνωση: ΜΕΡΟΣ ΑΠΟΤΕΛΕΣΜΑΤΩΝ Β ΤΜ. ΚΑΝ. ΕΞΕΤΑΣΗΣ 2017-18: ΣΗΜΕΙΩΣΗ" == std::str::from_utf8(&phrase(words.trim()).to_full_result().unwrap()).unwrap());
        let words = b"=?UTF-8?Q?=CE=A0=CF=81=CF=8C=CF=83=CE=B8=CE=B5?= =?UTF-8?Q?=CF=84=CE=B7_=CE=B5=CE=BE=CE=B5=CF=84?= =?UTF-8?Q?=CE=B1=CF=83=CF=84=CE=B9=CE=BA=CE=AE?=";
        assert!(
            "Πρόσθετη εξεταστική"
                == std::str::from_utf8(&phrase(words.trim()).to_full_result().unwrap()).unwrap()
        );
        let words = b"[Advcomparch] =?utf-8?b?zqPPhc68z4DOtc+BzrnPhs6/z4HOrCDPg861IGZs?=\n\t=?utf-8?b?dXNoIM67z4zOs8+JIG1pc3ByZWRpY3Rpb24gzrrOsc+Ezqwgz4TOt869?=\n\t=?utf-8?b?IM61zrrPhM6tzrvOtc+Dzrcgc3RvcmU=?=";
        assert!(
            "[Advcomparch] Συμπεριφορά σε flush λόγω misprediction κατά την εκτέλεση store"
                == std::str::from_utf8(&phrase(words.trim()).to_full_result().unwrap()).unwrap()
        );
        let words = b"Re: [Advcomparch] =?utf-8?b?zqPPhc68z4DOtc+BzrnPhs6/z4HOrCDPg861IGZs?=
	=?utf-8?b?dXNoIM67z4zOs8+JIG1pc3ByZWRpY3Rpb24gzrrOsc+Ezqwgz4TOt869?=
	=?utf-8?b?IM61zrrPhM6tzrvOtc+Dzrcgc3RvcmU=?=";
        assert!(
            "Re: [Advcomparch] Συμπεριφορά σε flush λόγω misprediction κατά την εκτέλεση store"
                == std::str::from_utf8(&phrase(words.trim()).to_full_result().unwrap()).unwrap()
        );
        let words = b"sdf";
        assert!("sdf" == std::str::from_utf8(&phrase(words).to_full_result().unwrap()).unwrap());
        let words = b"=?iso-8859-7?b?U2VnIGZhdWx0IPP05+0g5er03evl8+cg9O/1?= =?iso-8859-7?q?_example_ru_n_=5Fsniper?=";
        assert!(
            "Seg fault στην εκτέλεση του example ru n _sniper"
                == std::str::from_utf8(&phrase(words).to_full_result().unwrap()).unwrap()
        );
        let words = b"Re: [Advcomparch]
 =?iso-8859-7?b?U2VnIGZhdWx0IPP05+0g5er03evl8+cg9O/1?=
 =?iso-8859-7?q?_example_ru_n_=5Fsniper?=";

        assert!(
            "Re: [Advcomparch] Seg fault στην εκτέλεση του example ru n _sniper"
                == std::str::from_utf8(&phrase(words).to_full_result().unwrap()).unwrap()
        );
    }

    macro_rules! make_address {
        ($d:literal, $a:literal) => {
            Address::Mailbox(if $d.is_empty() {
                MailboxAddress {
                    raw: format!("<{}>", $a).into_bytes(),
                    display_name: StrBuilder {
                        offset: 0,
                        length: 0,
                    },
                    address_spec: StrBuilder {
                        offset: 1,
                        length: $a.len(),
                    },
                }
            } else {
                MailboxAddress {
                    raw: format!("{} <{}>", $d, $a).into_bytes(),
                    display_name: StrBuilder {
                        offset: 0,
                        length: $d.len(),
                    },
                    address_spec: StrBuilder {
                        offset: $d.len() + 2,
                        length: $a.len(),
                    },
                }
            })
        };
    }

    #[test]
    fn test_address_list() {
        let s = b"Obit Oppidum <user@domain>,
            list <list@domain.tld>, list2 <list2@domain.tld>,
            Bobit Boppidum <user@otherdomain.com>, Cobit Coppidum <user2@otherdomain.com>, <user@domain.tld>";
        assert_eq!(
            (
                &s[0..0],
                vec![
                    make_address!("Obit Oppidum", "user@domain"),
                    make_address!("list", "list@domain.tld"),
                    make_address!("list2", "list2@domain.tld"),
                    make_address!("Bobit Boppidum", "user@otherdomain.com"),
                    make_address!("Cobit Coppidum", "user2@otherdomain.com"),
                    make_address!("", "user@domain.tld")
                ]
            ),
            rfc2822address_list(s).unwrap()
        );
    }

    #[test]
    fn test_date() {
        let s = b"Thu, 31 Aug 2017 13:43:37 +0000 (UTC)";
        let _s = b"Thu, 31 Aug 2017 13:43:37 +0000";
        let __s = b"=?utf-8?q?Thu=2C_31_Aug_2017_13=3A43=3A37_-0000?=";
        debug!("{:?}, {:?}", date(s), date(_s));
        debug!("{:?}", date(__s));
        assert_eq!(date(s).unwrap(), date(_s).unwrap());
        assert_eq!(date(_s).unwrap(), date(__s).unwrap());
    }
    #[test]
    fn test_attachments() {
        //FIXME: add file
        return;
        /*
        use std::io::Read;
        let mut buffer: Vec<u8> = Vec::new();
        let _ = std::fs::File::open("").unwrap().read_to_end(&mut buffer);
        let boundary = b"b1_4382d284f0c601a737bb32aaeda53160";
        let (_, body) = match mail(&buffer).to_full_result() {
            Ok(v) => v,
            Err(_) => panic!(),
        };
        let attachments = parts(body, boundary).to_full_result().unwrap();
        assert_eq!(attachments.len(), 4);
        let v: Vec<&str> = attachments
            .iter()
            .map(|v| std::str::from_utf8(v).unwrap())
            .collect();
        println!("attachments {:?}", v);
        */
    }
    #[test]
    fn test_addresses() {
        {
            let s = b"=?iso-8859-7?B?0/Th/fHv8iDM4ev03ebv8g==?= <maltezos@central.ntua.gr>";
            let r = mailbox(s).unwrap().1;
            match r {
                Address::Mailbox(ref m) => assert!(
                    "Σταύρος Μαλτέζος"
                        == std::str::from_utf8(&m.display_name.display_bytes(&m.raw)).unwrap()
                        && std::str::from_utf8(&m.address_spec.display_bytes(&m.raw)).unwrap()
                            == "maltezos@central.ntua.gr"
                ),
                _ => assert!(false),
            }
        }
        {
            let s = b"user@domain";
            let r = mailbox(s).unwrap().1;
            match r {
                Address::Mailbox(ref m) => assert!(
                    m.display_name.display_bytes(&m.raw) == b""
                        && m.address_spec.display_bytes(&m.raw) == b"user@domain"
                ),
                _ => assert!(false),
            }
        }
        {
            let s = b"Name <user@domain>";
            let r = display_addr(s).unwrap().1;
            match r {
                Address::Mailbox(ref m) => assert!(
                    b"Name" == m.display_name.display_bytes(&m.raw)
                        && b"user@domain" == m.address_spec.display_bytes(&m.raw)
                ),
                _ => {}
            }
        }
        {
            let s = b"user@domain";
            let r = mailbox(s).unwrap().1;
            match r {
                Address::Mailbox(ref m) => assert!(
                    b"" == m.display_name.display_bytes(&m.raw)
                        && b"user@domain" == m.address_spec.display_bytes(&m.raw)
                ),
                _ => {}
            }
        }
    }

    #[test]
    fn test_quoted_printable() {
        let input = r#"<=21-- SEPARATOR  -->
   <tr>
    <td style=3D=22padding-left: 10px;padding-right: 10px;background-color:=
 =23f3f5fa;=22>
     <table width=3D=22100%=22 cellspacing=3D=220=22 cellpadding=3D=220=22 =
border=3D=220=22>
      <tr>
       <td style=3D=22height:5px;background-color: =23f3f5fa;=22>&nbsp;</td>
      </tr>
     </table>
    </td>
   </tr>"#;
        assert_eq!(
            quoted_printable_bytes(input.as_bytes())
                .to_full_result()
                .as_ref()
                .map(|b| unsafe { std::str::from_utf8_unchecked(b) }),
            Ok(r#"<!-- SEPARATOR  -->
   <tr>
    <td style="padding-left: 10px;padding-right: 10px;background-color: #f3f5fa;">
     <table width="100%" cellspacing="0" cellpadding="0" border="0">
      <tr>
       <td style="height:5px;background-color: #f3f5fa;">&nbsp;</td>
      </tr>
     </table>
    </td>
   </tr>"#)
        );
    }
}
