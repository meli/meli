/*
 * meli - parser module
 *
 * Copyright 2017 - 2020 Manos Pitsidianakis
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

use crate::error::{MeliError, Result, ResultIntoMeliError};
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_until, take_while},
    character::is_hex_digit,
    combinator::peek,
    error::ErrorKind,
    multi::{many0, many1, separated_list, separated_nonempty_list},
    number::complete::le_u8,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
};
use smallvec::SmallVec;
use std::borrow::Cow;

#[derive(Debug, Eq, PartialEq)]
pub struct ParsingError<I> {
    input: I,
    error: Cow<'static, str>,
}

pub type IResult<I, O, E = ParsingError<I>> = std::result::Result<(I, O), nom::Err<E>>;

impl<'i> ParsingError<&'i str> {
    pub fn as_bytes(self) -> ParsingError<&'i [u8]> {
        ParsingError {
            input: self.input.as_bytes(),
            error: self.error,
        }
    }
}

impl<'i> From<(&'i [u8], &'static str)> for ParsingError<&'i [u8]> {
    fn from((input, error): (&'i [u8], &'static str)) -> Self {
        Self {
            input,
            error: error.into(),
        }
    }
}

impl<'i> From<(&'i [u8], String)> for ParsingError<&'i [u8]> {
    fn from((input, error): (&'i [u8], String)) -> Self {
        Self {
            input,
            error: error.into(),
        }
    }
}

impl<I> nom::error::ParseError<I> for ParsingError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            input,
            error: kind.description().to_string().into(),
        }
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        Self {
            input,
            error: format!("{}, {}", kind.description(), other.error).into(),
        }
    }
}

impl<'i> From<ParsingError<&'i [u8]>> for MeliError {
    fn from(val: ParsingError<&'i [u8]>) -> MeliError {
        MeliError::new("Parsing error").set_summary(format!(
            r#"In input: "{}...",
Error: {}"#,
            String::from_utf8_lossy(val.input)
                .chars()
                .take(30)
                .collect::<String>(),
            val.error
        ))
    }
}

impl<'i> From<ParsingError<&'i str>> for MeliError {
    fn from(val: ParsingError<&'i str>) -> MeliError {
        MeliError::new("Parsing error").set_summary(format!(
            r#"In input: "{}...",
Error: {}"#,
            val.input.chars().take(30).collect::<String>(),
            val.error
        ))
    }
}

impl<'i> From<nom::Err<ParsingError<&'i [u8]>>> for MeliError {
    fn from(val: nom::Err<ParsingError<&'i [u8]>>) -> MeliError {
        match val {
            nom::Err::Incomplete(_) => MeliError::new("Parsing Error: Incomplete"),
            nom::Err::Error(err) | nom::Err::Failure(err) => err.into(),
        }
    }
}

impl<'i> From<nom::Err<ParsingError<&'i str>>> for MeliError {
    fn from(val: nom::Err<ParsingError<&'i str>>) -> MeliError {
        match val {
            nom::Err::Incomplete(_) => MeliError::new("Parsing Error: Incomplete"),
            nom::Err::Error(err) | nom::Err::Failure(err) => err.into(),
        }
    }
}

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
        if needle.is_empty() {
            return None;
        }
        self.windows(needle.len())
            .position(|window| window == needle)
    }

    fn rfind(&self, needle: &[u8]) -> Option<usize> {
        if needle.is_empty() {
            return None;
        }
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

pub trait BytesIterExt {
    fn join(&mut self, sep: u8) -> Vec<u8>;
}

impl<'a, P: for<'r> FnMut(&'r u8) -> bool> BytesIterExt for std::slice::Split<'a, u8, P> {
    fn join(&mut self, sep: u8) -> Vec<u8> {
        self.fold(vec![], |mut acc, el| {
            if !acc.is_empty() {
                acc.push(sep);
            }
            acc.extend(el.iter());
            acc
        })
    }
}

//fn parser(input: I) -> IResult<I, O, E>;
pub fn mail(input: &[u8]) -> Result<(Vec<(&[u8], &[u8])>, &[u8])> {
    let (rest, result) = alt((
        separated_pair(
            headers::headers,
            alt((tag(b"\n"), tag(b"\r\n"))),
            take_while(|_| true),
        ),
        pair(headers::headers, generic::eof),
    ))(input)
    .chain_err_summary(|| "Could not parse mail")?;

    if !rest.is_empty() {
        return Err(MeliError::new("Got leftover bytes after parsing mail"));
    }

    Ok(result)
}

pub mod generic {
    use super::*;
    pub fn angle_bracket_delimeted_list(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
        separated_nonempty_list(is_a(","), delimited(tag("<"), take_until(">"), tag(">")))(
            input.rtrim(),
        )
        //    separated_nonempty_list!(complete!(is_a!(",")), ws!(complete!(complete!(delimited!(tag!("<"), take_until1!(">"), tag!(">")))))));
    }

    pub fn date(input: &[u8]) -> Result<crate::datetime::UnixTimestamp> {
        let (_, mut parsed_result) = encodings::phrase(&eat_comments(input), false)?;
        if let Some(pos) = parsed_result.find(b"-0000") {
            parsed_result[pos] = b'+';
        }

        crate::datetime::rfc822_to_timestamp(parsed_result.trim())
    }

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
    use crate::email::address::Address;
    use crate::email::mailto::Mailto;
    pub fn mailto(mut input: &[u8]) -> IResult<&[u8], Mailto> {
        if !input.starts_with(b"mailto:") {
            return Err(nom::Err::Error(
                (input, "mailto(): input doesn't start with `mailto:`").into(),
            ));
        }

        input = &input[b"mailto:".len()..];

        let end = input.iter().position(|e| *e == b'?').unwrap_or(input.len());
        let address: Address;

        if let Ok((_, addr)) = crate::email::parser::address::address(&input[..end]) {
            address = addr;
            input = if input[end..].is_empty() {
                &input[end..]
            } else {
                &input[end + 1..]
            };
        } else {
            return Err(nom::Err::Error(
                (input, "mailto(): address not found in input").into(),
            ));
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
                return Err(nom::Err::Error(
                    (input, "mailto(): extra characters found in input").into(),
                ));
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
                    return Err(nom::Err::Error(
                        (input, "mailto(): unknown tag in input").into(),
                    ));
                }
            }
            if input[value_end..].is_empty() {
                break;
            }
            input = &input[value_end + 1..];
        }
        Ok((
            input,
            Mailto {
                address,
                subject,
                cc,
                bcc,
                body,
            },
        ))
    }

    pub struct HeaderIterator<'a>(pub &'a [u8]);

    impl<'a> Iterator for HeaderIterator<'a> {
        type Item = (&'a [u8], &'a [u8]);
        fn next(&mut self) -> Option<(&'a [u8], &'a [u8])> {
            if self.0.is_empty() {
                return None;
            }

            match super::headers::header(self.0) {
                Ok((rest, value)) => {
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

    pub fn eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
        if input.is_empty() {
            Ok((input, input))
        } else {
            Err(nom::Err::Error((input, "expected EOF").into()))
        }
    }
}

pub mod headers {
    use super::*;

    pub fn headers(input: &[u8]) -> IResult<&[u8], Vec<(&[u8], &[u8])>> {
        many1(header)(input)
    }

    pub fn header(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        alt((header_without_val, header_with_val))(input)
    }

    pub fn header_without_val(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        if input.is_empty() {
            return Err(nom::Err::Error(
                (input, "header_without_val(): input is empty").into(),
            ));
        } else if input.starts_with(b"\n") || input.starts_with(b"\r\n") {
            return Err(nom::Err::Error(
                (
                    input,
                    "header_without_val(): input starts with folding whitespace",
                )
                    .into(),
            ));
        }
        let mut ptr = 0;
        let mut name: &[u8] = &[];
        let mut has_colon = false;
        /* field-name  =  1*<any CHAR, excluding CTLs, SPACE, and ":"> */
        for (i, x) in input.iter().enumerate() {
            if input[i..].starts_with(b"\r\n") {
                name = &input[0..i];
                ptr = i + 2;
                break;
            } else if *x == b':' {
                name = &input[0..i];
                has_colon = true;
                ptr = i;
                break;
            } else if *x == b'\n' {
                name = &input[0..i];
                ptr = i;
                break;
            } else if is_ctl_or_space!(*x) {
                return Err(nom::Err::Error((
                    input,
                    r#"header_without_val(): field-name should contain "any CHAR, excluding CTLs, SPACE, and ":""#,
                ).into()));
            }
        }
        if name.is_empty() || input.len() <= ptr {
            return Err(nom::Err::Error(
                (input, "header_without_val(): not enough input").into(),
            ));
        }
        if input[ptr] == b':' {
            ptr += 1;
            has_colon = true;
            if ptr >= input.len() {
                return Err(nom::Err::Error(
                    (input, "header_without_val(): EOF after colon").into(),
                ));
            }
        }

        if !has_colon {
            return Err(nom::Err::Error(
                (input, "header_without_val(): no colon found").into(),
            ));
        }

        while input[ptr] == b' ' {
            ptr += 1;
            if ptr >= input.len() {
                return Err(nom::Err::Error(
                    (
                        input,
                        "header_without_val(): expected start of next field, found EOF",
                    )
                        .into(),
                ));
            }
        }
        if input[ptr..].starts_with(b"\n") {
            ptr += 1;
            if ptr >= input.len() {
                return Err(nom::Err::Error(
                    (
                        input,
                        "header_without_val(): expected folding whitespace, found EOF",
                    )
                        .into(),
                ));
            }
            if input.len() > ptr && input[ptr] != b' ' && input[ptr] != b'\t' {
                Ok((&input[ptr..], (name, b"")))
            } else {
                Err(nom::Err::Error(
                    (
                        input,
                        "header_without_val(): expected folding whitespace, found EOF",
                    )
                        .into(),
                ))
            }
        } else if input[ptr..].starts_with(b"\r\n") {
            ptr += 2;
            if ptr > input.len() {
                return Err(nom::Err::Error(
                    (
                        input,
                        "header_without_val(): expected folding whitespace, found EOF",
                    )
                        .into(),
                ));
            }
            if input.len() > ptr && input[ptr] != b' ' && input[ptr] != b'\t' {
                Ok((&input[ptr..], (name, b"")))
            } else {
                Err(nom::Err::Error(
                    (
                        &input[ptr..],
                        "header_without_val(): expected folding whitespace, found EOF",
                    )
                        .into(),
                ))
            }
        } else {
            Err(nom::Err::Error(
                (
                    &input[ptr..],
                    "header_without_val(): expected folding whitespace (newline)",
                )
                    .into(),
            ))
        }
    }

    /* A header can span multiple lines, eg:
     *
     * Received: from -------------------- (-------------------------)
     * 	by --------------------- (--------------------- [------------------]) (-----------------------)
     * 	with ESMTP id ------------ for <------------------->;
     * 	Tue,  5 Jan 2016 21:30:44 +0100 (CET)
     */

    pub fn header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
        let input_len = input.len();
        for (i, x) in input.iter().enumerate() {
            if *x == b'\n'
                && (((i + 1) < input_len && input[i + 1] != b' ' && input[i + 1] != b'\t')
                    || i + 1 == input_len)
            {
                return Ok((&input[(i + 1)..], &input[0..i]));
            } else if input[i..].starts_with(b"\r\n")
                && (((i + 2) < input_len && input[i + 2] != b' ' && input[i + 2] != b'\t')
                    || i + 2 == input_len)
            {
                return Ok((&input[(i + 2)..], &input[0..i]));
            }
        }
        Err(nom::Err::Error(
            (
                input,
                "header_value(): expected new line after header value",
            )
                .into(),
        ))
    }

    /* Parse a single header as a tuple */
    pub fn header_with_val(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        if input.is_empty() {
            return Err(nom::Err::Error(
                (input, "header_with_val(): empty input").into(),
            ));
        } else if input.starts_with(b"\n") || input.starts_with(b"\r\n") {
            return Err(nom::Err::Error(
                (input, "header_with_val(): field name starts with new line").into(),
            ));
        }
        let mut ptr = 0;
        let mut name: &[u8] = &[];
        /* field-name  =  1*<any CHAR, excluding CTLs, SPACE, and ":"> */
        for (i, x) in input.iter().enumerate() {
            if *x == b':' {
                name = &input[0..i];
                ptr = i + 1;
                break;
            } else if is_ctl_or_space!(*x) {
                return Err(nom::Err::Error(
                    (
                        &input[i..],
                        format!("header_with_val(): invalid character: {:?}", *x as char),
                    )
                        .into(),
                ));
            }
        }
        if name.is_empty() {
            return Err(nom::Err::Error(
                (input, "header_with_val(): found empty header name ").into(),
            ));
        }
        if ptr >= input.len() {
            return Err(nom::Err::Error(
                (input, "header_with_val(): found EOF").into(),
            ));
        }

        if input[ptr] == b'\n' {
            ptr += 1;
            if ptr >= input.len() {
                return Err(nom::Err::Error(
                    (input, "header_with_val(): found EOF").into(),
                ));
            }
        } else if input[ptr..].starts_with(b"\r\n") {
            ptr += 2;
            if ptr > input.len() {
                return Err(nom::Err::Error(
                    (input, "header_with_val(): found EOF").into(),
                ));
            }
        }
        if ptr >= input.len() {
            return Err(nom::Err::Error(
                (input, "header_with_val(): found EOF").into(),
            ));
        }
        while input[ptr] == b' ' || input[ptr] == b'\t' {
            ptr += 1;
            if ptr >= input.len() {
                return Err(nom::Err::Error(
                    (input, "header_with_val(): found EOF").into(),
                ));
            }
        }
        header_value(&input[ptr..]).map(|(rest, value)| (rest, (name, value)))
    }

    pub fn headers_raw(input: &[u8]) -> IResult<&[u8], &[u8]> {
        if input.is_empty() {
            return Err(nom::Err::Error(
                (input, "headers_raw(): input is empty").into(),
            ));
        }
        for i in 0..input.len() {
            if input[i..].starts_with(b"\n\n") {
                return Ok((&input[(i + 1)..], &input[0..=i]));
            } else if input[i..].starts_with(b"\r\n\r\n") {
                return Ok((&input[(i + 2)..], &input[0..=i]));
            }
        }
        Err(nom::Err::Error(
            (input, "headers_raw(): got EOF while looking for new line").into(),
        ))
    }
}

pub mod attachments {
    use super::*;
    use crate::email::address::*;
    use crate::email::attachment_types::{ContentDisposition, ContentDispositionKind};
    pub fn attachment(input: &[u8]) -> IResult<&[u8], (std::vec::Vec<(&[u8], &[u8])>, &[u8])> {
        alt((
            separated_pair(
                many0(headers::header),
                alt((tag(b"\n"), tag(b"\r\n"))),
                take_while(|_| true),
            ),
            pair(headers::headers, generic::eof),
        ))(input)
    }

    pub fn multipart_parts<'a>(
        input: &'a [u8],
        boundary: &[u8],
    ) -> IResult<&'a [u8], Vec<StrBuilder>> {
        let mut ret: Vec<_> = Vec::new();
        let mut input = input;
        let mut offset = 0;
        loop {
            let b_start = if let Some(v) = input.find(boundary) {
                v
            } else {
                return Err(nom::Err::Error(
                    (input, "multipart_parts(): could not find starting boundary").into(),
                ));
            };

            if b_start < 2 {
                return Err(nom::Err::Error(
                    (input, "multipart_parts(): malformed boundary").into(),
                ));
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
                return Err(nom::Err::Error(
                    (input, "multipart_parts(): found EOF").into(),
                ));
            }
            if let Some(end) = input.find(boundary) {
                if &input[end - 2..end] != b"--" {
                    return Err(nom::Err::Error(
                        (input, "multipart_parts(): malformed boundary").into(),
                    ));
                }
                if input[..end - 2].ends_with(b"\r\n") {
                    ret.push(StrBuilder {
                        offset,
                        length: end - 4,
                    });
                } else {
                    ret.push(StrBuilder {
                        offset,
                        length: end - 3,
                    });
                }
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
            } else {
                ret.push(StrBuilder {
                    offset,
                    length: input.len(),
                });
                break;
            }
        }
        Ok((input, ret))
    }

    fn parts_f(boundary: &[u8]) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<&[u8]>> + '_ {
        move |input: &[u8]| -> IResult<&[u8], Vec<&[u8]>> {
            let mut ret: Vec<&[u8]> = Vec::new();
            let mut input = input;
            loop {
                let b_start = if let Some(v) = input.find(boundary) {
                    v
                } else {
                    return Err(nom::Err::Error(
                        (input, "parts_f(): could not find starting boundary").into(),
                    ));
                };

                if b_start < 2 {
                    return Err(nom::Err::Error(
                        (input, "parts_f(): malformed boundary").into(),
                    ));
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
                    return Err(nom::Err::Error((input, "parts_f(): found EOF").into()));
                }
                if let Some(end) = input.find(boundary) {
                    if &input[end - 2..end] != b"--" {
                        return Err(nom::Err::Error((input, "parts_f(): found EOF").into()));
                    }
                    if input[..end - 2].ends_with(b"\r\n") {
                        ret.push(&input[..end - 4]);
                    } else {
                        ret.push(&input[..end - 3]);
                    }
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
                } else {
                    ret.push(input);
                    break;
                }
            }
            Ok((input, ret))
        }
    }

    pub fn parts<'a>(input: &'a [u8], boundary: &[u8]) -> IResult<&'a [u8], Vec<&'a [u8]>> {
        alt((
            parts_f(boundary),
            |input: &'a [u8]| -> IResult<&'a [u8], Vec<&'a [u8]>> {
                let (input, _) = take_until(&b"--"[..])(input)?;
                let (input, _) = take_until(boundary)(input)?;
                Ok((input, Vec::<&[u8]>::new()))
            },
        ))(input)
        /*
            alt_complete!(call!(parts_f, boundary) | do_parse!(
                        take_until_and_consume!(&b"--"[..]) >>
                        take_until_and_consume!(boundary) >>
                        ( { Vec::<&[u8]>::new() } ))
                    ));
        */
    }

    /* Caution: values should be passed through phrase() */
    pub fn content_type_parameter(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        let (input, _) = tag(";")(input.ltrim())?;
        let (input, name) = terminated(take_until("="), tag("="))(input.ltrim())?;
        let (input, value) = alt((
            delimited(tag("\""), take_until("\""), tag("\"")),
            is_not(";"),
        ))(input.ltrim())?;

        Ok((input, (name, value)))
    }

    pub fn content_type(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], Vec<(&[u8], &[u8])>)> {
        let (input, _type) = take_until("/")(input.ltrim())?;
        let (input, _) = tag("/")(input)?;
        let (input, _subtype) = is_not(";")(input)?;
        let (input, parameters) = many0(content_type_parameter)(input)?;
        Ok((input, (_type, _subtype, parameters)))
        /*
           do_parse!(
               _type: take_until!("/") >>
               tag!("/") >>
               _subtype: is_not!(";") >>
               parameters: many0!(complete!(content_type_parameter)) >>
               ( {
                   (_type, _subtype, parameters)
               } )
               ));
        */
    }

    /* Caution: values should be passed through phrase() */
    pub fn content_disposition_parameter(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        let (input, _) = tag(";")(input.ltrim())?;
        let (input, name) = terminated(take_until("="), tag("="))(input.ltrim())?;
        let (input, value) = alt((
            delimited(tag("\""), take_until("\""), tag("\"")),
            is_not(";"),
        ))(input.ltrim())?;

        Ok((input, (name, value)))
    }

    pub fn content_disposition(input: &[u8]) -> IResult<&[u8], ContentDisposition> {
        let (input, kind) = alt((take_until(";"), take_while(|_| true)))(input.trim())?;
        let mut ret = ContentDisposition {
            kind: if kind.trim().eq_ignore_ascii_case(b"attachment") {
                ContentDispositionKind::Attachment
            } else {
                ContentDispositionKind::Inline
            },
            ..ContentDisposition::default()
        };
        if input.is_empty() {
            return Ok((input, ret));
        }
        let (input, parameters) = many0(content_disposition_parameter)(input.ltrim())?;
        for (k, v) in parameters {
            if k.eq_ignore_ascii_case(b"filename") {
                ret.filename =
                    Some(String::from_utf8_lossy(&super::encodings::phrase(v, false)?.1).into());
            } else if k.eq_ignore_ascii_case(b"size") {
                ret.size =
                    Some(String::from_utf8_lossy(&super::encodings::phrase(v, false)?.1).into());
            } else if k.eq_ignore_ascii_case(b"creation-date") {
                ret.creation_date =
                    Some(String::from_utf8_lossy(&super::encodings::phrase(v, false)?.1).into());
            } else if k.eq_ignore_ascii_case(b"modification-date") {
                ret.modification_date =
                    Some(String::from_utf8_lossy(&super::encodings::phrase(v, false)?.1).into());
            } else if k.eq_ignore_ascii_case(b"read-date") {
                ret.read_date =
                    Some(String::from_utf8_lossy(&super::encodings::phrase(v, false)?.1).into());
            } else {
                ret.parameter
                    .push(String::from_utf8_lossy(&super::encodings::phrase(v, false)?.1).into());
            }
        }
        Ok((input, ret))
    }
}

pub mod encodings {
    use super::*;
    use crate::email::attachment_types::Charset;
    use data_encoding::BASE64_MIME;
    use encoding::all::*;
    use encoding::{DecoderTrap, Encoding};
    pub fn quoted_printable_byte(input: &[u8]) -> IResult<&[u8], u8> {
        if input.len() < 3 {
            Err(nom::Err::Error(
                (
                    input,
                    "quoted_printable_byte(): input too short to be quoted_printable",
                )
                    .into(),
            ))
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
            Ok((&input[3..], a * 16 + b))
        } else if input.starts_with(b"\r\n") {
            Ok((&input[2..], b'\n'))
        } else {
            Err(nom::Err::Error(
                (input, "quoted_printable_byte(): invalid input").into(),
            ))
        }
    }

    /* Encoded words
     *"=?charset?encoding?encoded text?=".
     */
    fn encoded_word(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        if input.is_empty() {
            return Ok((&[], Vec::with_capacity(0)));
        }
        if input.len() < 5 {
            return Err(nom::Err::Error(
                (input, "encoded_word(): input too short to be encoded_word").into(),
            ));
        } else if input[0] != b'=' || input[1] != b'?' {
            return Err(nom::Err::Error(
                (input, "encoded_word(): invalid input").into(),
            ));
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
            return Err(nom::Err::Error(
                (input, "encoded_word(): expected end tag").into(),
            ));
        }
        let tag_end_idx = tag_end_idx.unwrap();

        if tag_end_idx + 2 >= input.len() || input[2 + tag_end_idx] != b'?' {
            return Err(nom::Err::Error(
                (input, "encoded_word(): expected valid end tag").into(),
            ));
        }
        /* See if input ends with "?=" and get ending index
         * =?charset?encoding?encoded text?=
         * -------------------------------^
         */
        let mut encoded_end_idx = None;
        for i in (3 + tag_end_idx)..input.len() {
            if input[i] == b'?' && i + 1 < input.len() && input[i + 1] == b'=' {
                encoded_end_idx = Some(i);
                break;
            }
        }
        if encoded_end_idx.is_none() {
            return Err(nom::Err::Error(
                (input, "encoded_word(): expected input after end tag").into(),
            ));
        }
        let encoded_end_idx = encoded_end_idx.unwrap();
        let encoded_text = &input[3 + tag_end_idx..encoded_end_idx];

        let s: Vec<u8> = match input[tag_end_idx + 1] {
            b'b' | b'B' => match BASE64_MIME.decode(encoded_text) {
                Ok(v) => v,
                Err(_) => encoded_text.to_vec(),
            },
            b'q' | b'Q' => match quoted_printable_bytes_header(encoded_text) {
                Ok((b"", s)) => s,
                _ => {
                    return Err(nom::Err::Error(
                        (input, "encoded_word(): invalid quoted_printable").into(),
                    ))
                }
            },
            _ => {
                return Err(nom::Err::Error(
                    (input, "encoded_word(): expected 'b|q'").into(),
                ))
            }
        };

        let charset = Charset::from(&input[2..tag_end_idx]);

        if let Charset::UTF8 = charset {
            Ok((&input[encoded_end_idx + 2..], s))
        } else {
            match decode_charset(&s, charset) {
                Ok(v) => Ok((&input[encoded_end_idx + 2..], v.into_bytes())),
                _ => Err(nom::Err::Error(
                    (
                        input,
                        format!("encoded_word(): unknown charset {:?}", charset),
                    )
                        .into(),
                )),
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
            Charset::Windows1250 => Ok(WINDOWS_1250.decode(s, DecoderTrap::Strict)?),
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
            Err(nom::Err::Error(
                (input, "quoted_printable_soft_break(): found EOF").into(),
            ))
        } else if input[0] == b'=' && input[1] == b'\n' {
            Ok((&input[2..], &input[0..2])) // `=\n` is an escaped space character.
        } else if input.len() > 3 && input.starts_with(b"=\r\n") {
            Ok((&input[3..], &input[0..3])) // `=\r\n` is an escaped space character.
        } else {
            Err(nom::Err::Error(
                (input, "quoted_printable_soft_break(): invalid input").into(),
            ))
        }
    }

    pub fn qp_underscore_header(input: &[u8]) -> IResult<&[u8], u8> {
        let (rest, _) = tag(b"_")(input)?;
        Ok((rest, 0x20))
    }

    // With MIME, headers in quoted printable format can contain underscores that represent spaces.
    // In non-header context, an underscore is just a plain underscore.
    pub fn quoted_printable_bytes_header(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        many0(alt((quoted_printable_byte, qp_underscore_header, le_u8)))(input)
    }

    // For atoms in Header values.
    pub fn quoted_printable_bytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        many0(alt((
            preceded(quoted_printable_soft_break, quoted_printable_byte),
            preceded(quoted_printable_soft_break, le_u8),
            quoted_printable_byte,
            le_u8,
        )))(input)
    }

    pub fn space(input: &[u8]) -> IResult<&[u8], ()> {
        let (rest, _) =
            take_while(|c: u8| c == b' ' || c == b'\t' || c == b'\r' || c == b'\n')(input)?;
        Ok((rest, ()))
        //eat_separator!());
    }

    pub fn encoded_word_list(input: &[u8]) -> IResult<&[u8], SmallVec<[u8; 64]>> {
        let (input, list) = separated_nonempty_list(space, encoded_word)(input)?;
        let list_len = list.iter().fold(0, |mut acc, x| {
            acc += x.len();
            acc
        });
        Ok((
            input,
            list.iter()
                .fold(SmallVec::with_capacity(list_len), |mut acc, x| {
                    acc.extend(x.into_iter().cloned());
                    acc
                }),
        ))
    }

    pub fn ascii_token(input: &[u8]) -> IResult<&[u8], SmallVec<[u8; 64]>> {
        let (input, word) = alt((
            terminated(take_until(" =?"), peek(preceded(tag(b" "), encoded_word))),
            take_while(|_| true),
        ))(input)?;
        Ok((input, SmallVec::from(word)))
    }

    pub fn phrase(
        input: &[u8],
        multiline: /* preserve newlines */ bool,
    ) -> IResult<&[u8], Vec<u8>> {
        if input.is_empty() {
            return Ok((&[], Vec::with_capacity(0)));
        }

        let mut input = input.ltrim();
        let mut acc: Vec<u8> = Vec::new();
        let mut ptr = 0;

        while ptr < input.len() {
            let mut flag = false;
            // Check if word is encoded.
            while let Ok((rest, v)) = encoded_word(&input[ptr..]) {
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
            let mut ascii_e = 0;

            while ptr < end && !(is_whitespace!(input[ptr])) {
                ptr += 1;
            }
            if !multiline {
                ascii_e = ptr;
            }

            while ptr < input.len() && (is_whitespace!(input[ptr])) {
                ptr += 1;
            }
            if multiline {
                ascii_e = ptr;
            }
            if ptr >= input.len() {
                acc.extend(ascii_token(&input[ascii_s..ascii_e])?.1);
                break;
            }
            if ascii_s >= ascii_e {
                /* We have the start of an encoded word but not the end, so parse it as ascii */
                ascii_e = input[ascii_s..]
                    .find(b" ")
                    .unwrap_or_else(|| ascii_s + input[ascii_s..].len());
                ptr = ascii_e;
            }
            if ascii_s >= ascii_e {
                return Err(nom::Err::Error(
                    (input, "phrase(): start of an encoded word but no end").into(),
                ));
            }

            acc.extend(ascii_token(&input[ascii_s..ascii_e])?.1);
            if ptr != ascii_e {
                acc.push(b' ');
            }
        }
        Ok((&input[ptr..], acc))
    }
}

pub mod address {
    use super::*;
    use crate::email::address::*;
    pub fn display_addr(input: &[u8]) -> IResult<&[u8], Address> {
        if input.is_empty() || input.len() < 3 {
            Err(nom::Err::Error((input, "display_addr(): EOF").into()))
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
                let (rest, output) = match super::encodings::phrase(input, false) {
                    Ok(v) => v,
                    _ => {
                        return Err(nom::Err::Error(
                            (input, "display_addr(): no '<' found").into(),
                        ))
                    }
                };
                if output.contains(&b'<') {
                    let (_, address) = match display_addr(&output) {
                        Ok(v) => v,
                        _ => {
                            return Err(nom::Err::Error(
                                (input, "display_addr(): invalid input").into(),
                            ))
                        }
                    };
                    return Ok((rest, address));
                }
                return Err(nom::Err::Error(
                    (input, "display_addr(): invalid input").into(),
                ));
            }
            let mut end = input.len();
            let mut at_flag = false;
            let mut flag = false;
            for (i, b) in input[display_name.length + 2..].iter().enumerate() {
                match *b {
                    b'@' => at_flag = true,
                    b'>' => {
                        end = i;
                        flag = true;
                        break;
                    }
                    _ => {}
                }
            }
            if at_flag && flag {
                let (_, raw) =
                    super::encodings::phrase(&input[0..end + display_name.length + 3], false)?;
                let display_name_end = raw.find(b"<").unwrap();
                display_name.length = raw[0..display_name_end].trim().len();
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

                Ok((
                    input.get(rest_start..).unwrap_or_default(),
                    Address::Mailbox(MailboxAddress {
                        raw,
                        display_name,
                        address_spec,
                    }),
                ))
            } else {
                Err(nom::Err::Error(
                    (input, "display_addr(): did not find both '@' and '>'").into(),
                ))
            }
        } else {
            Err(nom::Err::Error(
                (input, "display_addr(): unexpected whitespace").into(),
            ))
        }
    }

    fn addr_spec(input: &[u8]) -> IResult<&[u8], Address> {
        if input.is_empty() || input.len() < 3 {
            Err(nom::Err::Error((input, "addr_spec(): found EOF").into()))
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
                Ok((
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
                ))
            } else {
                Err(nom::Err::Error((input, "addr_spec(): expected '@'").into()))
            }
        } else {
            Err(nom::Err::Error(
                (input, "addr_spec(): unexpected whitespace").into(),
            ))
        }
    }

    pub fn mailbox(input: &[u8]) -> IResult<&[u8], Address> {
        alt((display_addr, addr_spec))(input)
        //ws!(alt_complete!(display_addr | addr_spec))
    }

    pub fn mailbox_list(input: &[u8]) -> IResult<&[u8], Vec<Address>> {
        many0(mailbox)(input)
        // many0!(mailbox));
    }

    /*
     * group of recipients eg. undisclosed-recipients;
     */
    fn group(input: &[u8]) -> IResult<&[u8], Address> {
        let mut flag = false;
        let mut dlength = 0;
        for (i, b) in input.iter().enumerate() {
            if *b == b';' {
                flag = true;
                dlength = i;
                break;
            }
        }
        if !flag {
            return Err(nom::Err::Error(
                (input, "group(): expected to find ';'").into(),
            ));
        }

        let (rest, vec) = mailbox_list(&input[dlength..])?;
        let size: usize =
            (rest.as_ptr() as usize).wrapping_sub((&input[0..] as &[u8]).as_ptr() as usize);
        Ok((
            rest,
            Address::Group(GroupAddress {
                raw: input[0..size].into(),
                display_name: StrBuilder {
                    offset: 0,
                    length: dlength,
                },
                mailbox_list: vec,
            }),
        ))
    }

    pub fn address(input: &[u8]) -> IResult<&[u8], Address> {
        alt((mailbox, group))(input.ltrim())
        // ws!(alt_complete!(mailbox | group))
    }

    pub fn rfc2822address_list(input: &[u8]) -> IResult<&[u8], SmallVec<[Address; 1]>> {
        separated_list_smallvec(is_a(","), address)(input.ltrim())
        // ws!( separated_list!(is_a!(","), address))
    }

    pub fn address_list(input: &[u8]) -> IResult<&[u8], String> {
        let (input, list) = alt((
            super::encodings::encoded_word_list,
            super::encodings::ascii_token,
        ))(input)?;
        let list: Vec<&[u8]> = list.split(|c| *c == b',').collect();
        let string_len = list.iter().fold(0, |mut acc, x| {
            acc += x.trim().len();
            acc
        }) + list.len()
            - 1;
        let list_len = list.len();
        let mut i = 0;
        Ok((
            input,
            list.iter()
                .fold(String::with_capacity(string_len), |acc, x| {
                    let mut acc = acc
                        + &String::from_utf8_lossy(
                            x.replace(b"\n", b"")
                                .replace(b"\r", b"")
                                .replace(b"\t", b" ")
                                .trim(),
                        );
                    if i != list_len - 1 {
                        acc.push_str(" ");
                        i += 1;
                    }
                    acc
                }),
        ))
    }

    pub fn message_id(input: &[u8]) -> IResult<&[u8], &[u8]> {
        delimited(tag("<"), take_until(">"), tag(">"))(input.ltrim())
        //complete!(delimited!(ws!(tag!("<")), take_until1!(">"), tag!(">")))
    }

    fn message_id_peek(input: &[u8]) -> IResult<&[u8], &[u8]> {
        let input_length = input.len();
        if input.is_empty() {
            Err(nom::Err::Error(
                (input, "message_id_peek(): found EOF").into(),
            ))
        } else if input_length == 2 || input[0] != b'<' {
            Err(nom::Err::Error(
                (input, "message_id_peek(): expected '<'").into(),
            ))
        } else {
            for (i, &x) in input.iter().take(input_length).enumerate().skip(1) {
                if x == b'>' {
                    return Ok((&input[i + 1..], &input[0..=i]));
                }
            }
            Err(nom::Err::Error(
                (input, "message_id_peek(): expected closing '>'").into(),
            ))
        }
    }

    pub fn references(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
        separated_list(is_a(" \n\t\r"), message_id_peek)(input)
        // separated_list!(complete!(is_a!(" \n\t\r")), message_id_peek));
    }

    use smallvec::SmallVec;
    pub fn separated_list_smallvec<I, O, Sep, E, F, G>(
        sep: G,
        f: F,
    ) -> impl FnMut(I) -> IResult<I, SmallVec<[O; 1]>, E>
    where
        I: Clone + PartialEq,
        F: Fn(I) -> IResult<I, O, E>,
        G: Fn(I) -> IResult<I, Sep, E>,
        E: nom::error::ParseError<I>,
    {
        move |i: I| {
            let mut res = SmallVec::new();
            let mut i = i;

            // Parse the first element
            match f(i.clone()) {
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    if i1 == i {
                        return Err(nom::Err::Error(E::from_error_kind(
                            i1,
                            ErrorKind::SeparatedList,
                        )));
                    }

                    res.push(o);
                    i = i1;
                }
            }

            loop {
                match sep(i.clone()) {
                    Err(nom::Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i1, _)) => {
                        if i1 == i {
                            return Err(nom::Err::Error(E::from_error_kind(
                                i1,
                                ErrorKind::SeparatedList,
                            )));
                        }

                        match f(i1.clone()) {
                            Err(nom::Err::Error(_)) => return Ok((i, res)),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                                if i2 == i {
                                    return Err(nom::Err::Error(E::from_error_kind(
                                        i2,
                                        ErrorKind::SeparatedList,
                                    )));
                                }

                                res.push(o);
                                i = i2;
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{address::*, encodings::*, generic::*, *};
    use crate::email::address::*;
    use crate::make_address;

    #[test]
    fn test_phrase() {
        let words = b"=?iso-8859-7?B?W215Y291cnNlcy5udHVhLmdyIC0gyvXs4fTp6t4g6uHpIMri4e306ere?=
     =?iso-8859-7?B?INb18+nq3l0gzd3hIMHt4erv3+358+c6IMzF0c/TIMHQz9TFy8XTzMHU?=
      =?iso-8859-7?B?2c0gwiDUzC4gysHNLiDFzsXUwdPH0yAyMDE3LTE4OiDTx8zFydnTxw==?=";
        assert_eq!("[mycourses.ntua.gr - Κυματική και Κβαντική Φυσική] Νέα Ανακοίνωση: ΜΕΡΟΣ ΑΠΟΤΕΛΕΣΜΑΤΩΝ Β ΤΜ. ΚΑΝ. ΕΞΕΤΑΣΗΣ 2017-18: ΣΗΜΕΙΩΣΗ" , std::str::from_utf8(&phrase(words.trim(), false).unwrap().1).unwrap());
        let words = b"=?UTF-8?Q?=CE=A0=CF=81=CF=8C=CF=83=CE=B8=CE=B5?= =?UTF-8?Q?=CF=84=CE=B7_=CE=B5=CE=BE=CE=B5=CF=84?= =?UTF-8?Q?=CE=B1=CF=83=CF=84=CE=B9=CE=BA=CE=AE?=";
        assert_eq!(
            "Πρόσθετη εξεταστική",
            std::str::from_utf8(&phrase(words.trim(), false).unwrap().1).unwrap()
        );
        let words = b"[Advcomparch] =?utf-8?b?zqPPhc68z4DOtc+BzrnPhs6/z4HOrCDPg861IGZs?=\n\t=?utf-8?b?dXNoIM67z4zOs8+JIG1pc3ByZWRpY3Rpb24gzrrOsc+Ezqwgz4TOt869?=\n\t=?utf-8?b?IM61zrrPhM6tzrvOtc+Dzrcgc3RvcmU=?=";
        assert_eq!(
            "[Advcomparch] Συμπεριφορά σε flush λόγω misprediction κατά την εκτέλεση store",
            std::str::from_utf8(&phrase(words.trim(), false).unwrap().1).unwrap()
        );
        let words = b"Re: [Advcomparch] =?utf-8?b?zqPPhc68z4DOtc+BzrnPhs6/z4HOrCDPg861IGZs?=
	=?utf-8?b?dXNoIM67z4zOs8+JIG1pc3ByZWRpY3Rpb24gzrrOsc+Ezqwgz4TOt869?=
	=?utf-8?b?IM61zrrPhM6tzrvOtc+Dzrcgc3RvcmU=?=";
        assert_eq!(
            "Re: [Advcomparch] Συμπεριφορά σε flush λόγω misprediction κατά την εκτέλεση store",
            std::str::from_utf8(&phrase(words.trim(), false).unwrap().1).unwrap()
        );
        let words = b"sdf";
        assert_eq!(
            "sdf",
            std::str::from_utf8(&phrase(words, false).unwrap().1).unwrap()
        );
        let words = b"=?iso-8859-7?b?U2VnIGZhdWx0IPP05+0g5er03evl8+cg9O/1?= =?iso-8859-7?q?_example_ru_n_=5Fsniper?=";
        assert_eq!(
            "Seg fault στην εκτέλεση του example ru n _sniper",
            std::str::from_utf8(&phrase(words, false).unwrap().1).unwrap()
        );
        let words = b"Re: [Advcomparch]
 =?iso-8859-7?b?U2VnIGZhdWx0IPP05+0g5er03evl8+cg9O/1?=
 =?iso-8859-7?q?_example_ru_n_=5Fsniper?=";

        assert_eq!(
            "Re: [Advcomparch] Seg fault στην εκτέλεση του example ru n _sniper",
            std::str::from_utf8(&phrase(words, false).unwrap().1).unwrap()
        );

        let words = r#"[internal] =?UTF-8?B?zp3Orc6/z4Igzp/OtM63zrPPjM+CIM6jz4XOs86zz4E=?=
 =?UTF-8?B?zrHPhs6uz4I=?="#;
        assert_eq!(
            "[internal] Νέος Οδηγός Συγγραφής",
            std::str::from_utf8(&phrase(words.as_bytes(), false).unwrap().1).unwrap()
        );

        let words = r#"=?UTF-8?Q?Re=3a_Climate_crisis_reality_check_=e2=80=93=c2=a0EcoHust?=
 =?UTF-8?Q?ler?="#;
        assert_eq!(
            "Re: Climate crisis reality check –\u{a0}EcoHustler",
            std::str::from_utf8(&phrase(words.as_bytes(), false).unwrap().1).unwrap()
        );

        let words = r#"Re: Climate crisis reality check =?windows-1250?B?lqBFY29IdXN0?=
 =?windows-1250?B?bGVy?="#;
        assert_eq!(
            "Re: Climate crisis reality check –\u{a0}EcoHustler",
            std::str::from_utf8(&phrase(words.as_bytes(), false).unwrap().1).unwrap()
        );
    }

    #[test]
    fn test_address_list() {
        let s = b"Obit Oppidum <user@domain>,
            list <list@domain.tld>, list2 <list2@domain.tld>,
            Bobit Boppidum <user@otherdomain.com>, Cobit Coppidum <user2@otherdomain.com>, <user@domain.tld>";
        assert_eq!(
            (
                &s[0..0],
                smallvec::smallvec![
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
        let val = b"Fri, 23 Dec 0001 21:20:36 -0800 (PST)";
        assert_eq!(date(val).unwrap(), 0);
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
        let (_, body) = match mail(&buffer) {
            Ok(v) => v,
            Err(_) => panic!(),
        };
        let attachments = parts(body, boundary).unwrap().1;
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
                .as_ref()
                .map(|(_, b)| unsafe { std::str::from_utf8_unchecked(b) }),
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
