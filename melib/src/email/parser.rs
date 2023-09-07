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

//! Parsers for email. See submodules.
#![allow(clippy::type_complexity)]

use std::{borrow::Cow, convert::TryFrom, fmt::Write};

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take, take_until, take_while, take_while1},
    character::{is_alphabetic, is_digit, is_hex_digit},
    combinator::{map, opt, peek},
    error::{context, ErrorKind},
    multi::{many0, many1, separated_list1},
    number::complete::le_u8,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
};
use smallvec::SmallVec;

use crate::{
    email::{
        address::Address,
        headers::{HeaderMap, HeaderName},
        mailto::Mailto,
    },
    error::{Error, Result, ResultIntoError},
    utils::{html_escape::HtmlEntity, percent_encoding::percent_decode},
};

macro_rules! to_str {
    ($l:expr) => {{
        unsafe { std::str::from_utf8_unchecked($l) }
    }};
}
#[derive(Eq, PartialEq)]
pub struct ParsingError<I> {
    pub input: I,
    pub error: Cow<'static, str>,
}

impl std::fmt::Debug for ParsingError<&'_ [u8]> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("ParsingError")
            .field("input", &to_str!(self.input))
            .field("error", &self.error)
            .finish()
    }
}

impl std::fmt::Debug for ParsingError<&'_ str> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("ParsingError")
            .field("input", &self.input)
            .field("error", &self.error)
            .finish()
    }
}

struct DebugOkWrapper<'r, I, R: AsRef<[u8]>>(&'r IResult<I, R>);

impl<R: AsRef<[u8]> + std::fmt::Debug> std::fmt::Debug for DebugOkWrapper<'_, &'_ [u8], R> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Ok((a, b)) = self.0 {
            write!(fmt, "Ok({}, {})", &to_str!(a), &to_str!(b.as_ref()))
        } else {
            write!(fmt, "{:?}", self.0)
        }
    }
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

impl<I> From<(I, &'static str)> for ParsingError<I> {
    fn from((input, error): (I, &'static str)) -> Self {
        Self {
            input,
            error: error.into(),
        }
    }
}

impl<I> From<(I, String)> for ParsingError<I> {
    fn from((input, error): (I, String)) -> Self {
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

impl<I, E> nom::error::FromExternalError<I, E> for ParsingError<I> {
    fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
        Self {
            input,
            error: kind.description().to_string().into(),
        }
    }
}

impl<I> nom::error::ContextError<I> for ParsingError<I> {}

impl<'i> From<ParsingError<&'i [u8]>> for Error {
    fn from(val: ParsingError<&'i [u8]>) -> Self {
        Self::new("Parsing error").set_summary(format!(
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

impl<'i> From<ParsingError<&'i str>> for Error {
    fn from(val: ParsingError<&'i str>) -> Self {
        Self::new("Parsing error").set_summary(format!(
            r#"In input: "{}...",
Error: {}"#,
            val.input.chars().take(30).collect::<String>(),
            val.error
        ))
    }
}

impl<'i> From<nom::Err<ParsingError<&'i [u8]>>> for Error {
    fn from(val: nom::Err<ParsingError<&'i [u8]>>) -> Self {
        match val {
            nom::Err::Incomplete(_) => Self::new("Parsing Error: Incomplete"),
            nom::Err::Error(err) | nom::Err::Failure(err) => err.into(),
        }
    }
}

impl<'i> From<nom::Err<ParsingError<&'i str>>> for Error {
    fn from(val: nom::Err<ParsingError<&'i str>>) -> Self {
        match val {
            nom::Err::Incomplete(_) => Self::new("Parsing Error: Incomplete"),
            nom::Err::Error(err) | nom::Err::Failure(err) => err.into(),
        }
    }
}

impl From<nom::Err<nom::error::Error<&[u8]>>> for Error {
    fn from(val: nom::Err<nom::error::Error<&[u8]>>) -> Self {
        match val {
            nom::Err::Incomplete(_) => Self::new("Parsing Error: Incomplete"),
            nom::Err::Error(_) | nom::Err::Failure(_) => Self::new("Parsing Error"),
        }
    }
}

impl<'i> From<ParsingError<&'i [u8]>> for nom::error::Error<&'i [u8]> {
    fn from(val: ParsingError<&'i [u8]>) -> Self {
        nom::error::Error::new(val.input, ErrorKind::Satisfy)
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
    fn trim_start(&self) -> &Self {
        self.ltrim()
    }
    fn trim_end(&self) -> &Self {
        self.rtrim()
    }
    fn trim(&self) -> &Self;
    fn find<T: AsRef<[u8]>>(&self, needle: T) -> Option<usize>;
    fn contains_subsequence<T: AsRef<[u8]>>(&self, needle: T) -> bool {
        self.find(needle.as_ref()).is_some()
    }

    fn rfind<T: AsRef<[u8]>>(&self, needle: T) -> Option<usize>;
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
    fn find<T: AsRef<[u8]>>(&self, needle: T) -> Option<usize> {
        let needle = needle.as_ref();
        if needle.is_empty() {
            return None;
        }
        self.windows(needle.len())
            .position(|window| window == needle)
    }

    fn rfind<T: AsRef<[u8]>>(&self, needle: T) -> Option<usize> {
        let needle = needle.as_ref();
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
pub fn mail(input: &[u8]) -> Result<(Vec<(HeaderName, &[u8])>, &[u8])> {
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
        return Err(Error::new("Got leftover bytes after parsing mail"));
    }

    Ok(result)
}

pub mod dates {
    //! Date values in headers.
    use super::{generic::*, *};
    use crate::utils::datetime::UnixTimestamp;

    fn take_n_digits(n: usize) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
        move |input: &[u8]| {
            let (input, ret) = take(n)(input)?;
            if !ret.iter().all(|c| is_digit(*c)) {
                return Err(nom::Err::Error(
                    (input, "take_n_digits(): not digits").into(),
                ));
            }
            Ok((input, ret))
        }
    }

    /// In the obsolete time zone, "UT" and "GMT" are indications of
    /// "Universal Time" and "Greenwich Mean Time", respectively, and are
    /// both semantically identical to "+0000".

    /// The remaining three character zones are the US time zones.  The first
    /// letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
    /// "Mountain", and "Pacific".  The second letter is either "S" for
    /// "Standard" time, or "D" for "Daylight Savings" (or summer) time.
    /// Their interpretations are as follows:

    ///   EDT is semantically equivalent to -0400
    ///   EST is semantically equivalent to -0500
    ///   CDT is semantically equivalent to -0500
    ///   CST is semantically equivalent to -0600
    ///   MDT is semantically equivalent to -0600
    ///   MST is semantically equivalent to -0700
    ///   PDT is semantically equivalent to -0700
    ///   PST is semantically equivalent to -0800

    /// The 1 character military time zones were defined in a non-standard
    /// way in RFC0822 and are therefore unpredictable in their meaning.
    /// The original definitions of the military zones "A" through "I" are
    /// equivalent to "+0100" through "+0900", respectively; "K", "L", and
    /// "M" are equivalent to "+1000", "+1100", and "+1200", respectively;
    /// "N" through "Y" are equivalent to "-0100" through "-1200".
    /// respectively; and "Z" is equivalent to "+0000".  However, because of
    /// the error in RFC0822, they SHOULD all be considered equivalent to
    /// "-0000" unless there is out-of-band information confirming their
    /// meaning.

    /// Other multi-character (usually between 3 and 5) alphabetic time zones
    /// have been used in Internet messages.  Any such time zone whose
    /// meaning is not known SHOULD be considered equivalent to "-0000"
    /// unless there is out-of-band information confirming their meaning.
    fn obs_zone(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        alt((
            map(tag("UT"), |_| (&b"+"[..], &b"0000"[..])),
            map(tag("GMT"), |_| (&b"+"[..], &b"0000"[..])),
            map(tag("EDT"), |_| (&b"-"[..], &b"0400"[..])),
            map(tag("EST"), |_| (&b"-"[..], &b"0500"[..])),
            map(tag("CDT"), |_| (&b"-"[..], &b"0500"[..])),
            map(tag("CST"), |_| (&b"-"[..], &b"0600"[..])),
            map(tag("MDT"), |_| (&b"-"[..], &b"0600"[..])),
            map(tag("MST"), |_| (&b"-"[..], &b"0700"[..])),
            map(tag("PDT"), |_| (&b"-"[..], &b"0700"[..])),
            map(tag("PST"), |_| (&b"-"[..], &b"0800"[..])),
            map(take_while1(is_alphabetic), |_| (&b"-"[..], &b"0000"[..])),
        ))(input)
    }

    /// ```text
    /// zone            =   (FWS ( "+" / "-" ) 4DIGIT) / obs-zone
    /// ```
    fn zone(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        alt((
            |input| {
                let (input, sign) = alt((tag("+"), tag("-")))(input)?;
                let (input, zone) = take_n_digits(4)(input)?;
                Ok((input, (sign, zone)))
            },
            obs_zone,
        ))(input)
    }

    /// ```text
    /// date-time       =   [ day-of-week "," ] date time [CFWS]
    /// date            =   day month year
    /// time            =   time-of-day zone
    /// time-of-day     =   hour ":" minute [ ":" second ]
    /// hour            =   2DIGIT / obs-hour
    /// minute          =   2DIGIT / obs-minute
    /// second          =   2DIGIT / obs-second
    /// ```
    fn date_time(input: &[u8]) -> IResult<&[u8], UnixTimestamp> {
        let orig_input = input;
        let mut accum: SmallVec<[u8; 32]> = SmallVec::new();
        let (input, day_of_week) = opt(terminated(day_of_week, tag(",")))(input)?;
        let (input, day) = day(input)?;
        let (input, month) = month(input)?;
        let (input, year) = year(input)?;
        let (input, hour) = take_n_digits(2)(input)?;
        let (input, _) = tag(":")(input)?;
        let (input, minute) = take_n_digits(2)(input)?;
        let (input, second) = opt(preceded(tag(":"), take_n_digits(2)))(input)?;
        let (input, _) = fws(input)?;
        let (input, (sign, zone)) = zone(input)?;
        let (input, _) = opt(cfws)(input)?;
        if let Some(day_of_week) = day_of_week {
            accum.extend_from_slice(&day_of_week);
            accum.extend_from_slice(b", ");
        }
        accum.extend_from_slice(day);
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(month);
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(year);
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(hour);
        accum.extend_from_slice(b":");
        accum.extend_from_slice(minute);
        if let Some(second) = second {
            accum.extend_from_slice(b":");
            accum.extend_from_slice(second);
        }
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(sign);
        accum.extend_from_slice(zone);
        match crate::utils::datetime::rfc822_to_timestamp(accum.to_vec()) {
            Ok(t) => Ok((input, t)),
            Err(_err) => Err(nom::Err::Error(
                (
                    orig_input,
                    "date_time(): could not convert date from rfc822",
                )
                    .into(),
            )),
        }
    }

    /// e.g Wed Sep  9 00:27:54 2020
    ///
    /// ```text
    /// day-of-week month day time year
    /// date-time       =   [ day-of-week "," ] date time [CFWS]
    /// date            =   day month year
    /// time            =   time-of-day zone
    /// time-of-day     =   hour ":" minute [ ":" second ]
    /// hour            =   2DIGIT / obs-hour
    /// minute          =   2DIGIT / obs-minute
    /// second          =   2DIGIT / obs-second
    /// ```
    pub fn mbox_date_time(input: &[u8]) -> IResult<&[u8], UnixTimestamp> {
        let orig_input = input;
        let mut accum: SmallVec<[u8; 32]> = SmallVec::new();
        let (input, _) = opt(cfws)(input)?;
        let (input, day_of_week) = day_of_week(input)?;
        let (input, _) = opt(cfws)(input)?;
        let (input, month) = month(input)?;
        let (input, _) = opt(cfws)(input)?;
        let (input, day) = day(input)?;
        let (input, _) = opt(cfws)(input)?;
        let (input, hour) = take_n_digits(2)(input)?;
        let (input, _) = tag(":")(input)?;
        let (input, minute) = take_n_digits(2)(input)?;
        let (input, second) = opt(preceded(tag(":"), take_n_digits(2)))(input)?;
        let (input, _) = fws(input)?;
        let (input, zone) = opt(zone)(input)?;
        let (input, _) = opt(cfws)(input)?;
        let (input, year) = year(input)?;
        accum.extend_from_slice(&day_of_week);
        accum.extend_from_slice(b", ");
        accum.extend_from_slice(day);
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(month);
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(year);
        accum.extend_from_slice(b" ");
        accum.extend_from_slice(hour);
        accum.extend_from_slice(b":");
        accum.extend_from_slice(minute);
        if let Some(second) = second {
            accum.extend_from_slice(b":");
            accum.extend_from_slice(second);
        }
        if let Some((sign, zone)) = zone {
            accum.extend_from_slice(b" ");
            accum.extend_from_slice(sign);
            accum.extend_from_slice(zone);
        }
        match crate::utils::datetime::rfc822_to_timestamp(accum.to_vec()) {
            Ok(t) => Ok((input, t)),
            Err(_err) => Err(nom::Err::Error(
                (
                    orig_input,
                    "mbox_date_time(): could not convert date from rfc822",
                )
                    .into(),
            )),
        }
    }

    /// ```text
    /// day-of-week     =   ([FWS] day-name) / obs-day-of-week
    /// day-name        =   "Mon" / "Tue" / "Wed" / "Thu" /
    ///                    "Fri" / "Sat" / "Sun"
    /// ```
    fn day_of_week(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let (input, day_name) = alt((
            tag("Mon"),
            tag("Tue"),
            tag("Wed"),
            tag("Thu"),
            tag("Fri"),
            tag("Sat"),
            tag("Sun"),
        ))(input)?;
        Ok((input, day_name.into()))
    }

    /// `day             =   ([FWS] 1*2DIGIT FWS) / obs-day`
    fn day(input: &[u8]) -> IResult<&[u8], &[u8]> {
        let (input, _) = opt(fws)(input)?;
        let (input, ret) = alt((take_n_digits(2), take_n_digits(1)))(input)?;
        let (input, _) = fws(input)?;

        Ok((input, ret))
    }

    /// ```text
    /// month           =   "Jan" / "Feb" / "Mar" / "Apr" /
    ///                    "May" / "Jun" / "Jul" / "Aug" /
    ///                    "Sep" / "Oct" / "Nov" / "Dec"
    /// ```
    fn month(input: &[u8]) -> IResult<&[u8], &[u8]> {
        alt((
            tag("Jan"),
            tag("Feb"),
            tag("Mar"),
            tag("Apr"),
            tag("May"),
            tag("Jun"),
            tag("Jul"),
            tag("Aug"),
            tag("Sep"),
            tag("Oct"),
            tag("Nov"),
            tag("Dec"),
        ))(input)
    }

    ///year            =   (FWS 4*DIGIT FWS) / obs-year
    fn year(input: &[u8]) -> IResult<&[u8], &[u8]> {
        let (input, _) = opt(fws)(input)?;
        let (input, ret) = take_n_digits(4)(input)?;
        let (input, _) = opt(fws)(input)?;
        Ok((input, ret))
    }

    pub fn rfc5322_date(input: &[u8]) -> Result<crate::UnixTimestamp> {
        date_time(input)
            .or_else(|_| {
                //let (_, mut parsed_result) = encodings::phrase(&eat_comments(input), false)?;
                let (rest, parsed_result) = encodings::phrase(input, false)?;
                let (_, ret) = match date_time(&parsed_result) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(nom::Err::Error(
                            (rest, "rfc5322_date(): invalid input").into(),
                        ));
                    }
                };
                Ok((rest, ret))
            })
            .or_else(|_| {
                let (rest, ret) = match mbox_date_time(input) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(nom::Err::Error(
                            (input, "rfc5322_date(): invalid input").into(),
                        ));
                    }
                };
                Ok((rest, ret))
            })
            .map(|(_, r)| r)
            .map_err(|err: nom::Err<ParsingError<_>>| err.into())
        /*
        }
        if let Some(pos) = parsed_result.find(b"-0000") {
            parsed_result[pos] = b'+';
        }

        crate::utils::datetime::rfc822_to_timestamp(parsed_result.trim())
            */
    }

    #[test]
    fn test_date_new() {
        let s = b"Thu, 31 Aug 2017 13:43:37 +0000 (UTC)";
        let _s = b"Thu, 31 Aug 2017 13:43:37 +0000";
        let __s = b"=?utf-8?q?Thu=2C_31_Aug_2017_13=3A43=3A37_-0000?=";
        assert_eq!(rfc5322_date(s).unwrap(), rfc5322_date(_s).unwrap());
        assert_eq!(rfc5322_date(_s).unwrap(), rfc5322_date(__s).unwrap());
        let val = b"Fri, 23 Dec 0001 21:20:36 -0800 (PST)";
        assert_eq!(rfc5322_date(val).unwrap(), 0);
        let val = b"Wed Sep  9 00:27:54 2020";
        assert_eq!(rfc5322_date(val).unwrap(), 1599611274);
    }
}

pub mod generic {
    //! Generally useful parser combinators.
    use super::*;
    #[inline(always)]
    pub fn byte_in_slice<'a>(slice: &'static [u8]) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], u8> {
        move |input| {
            if input.is_empty() {
                return Err(nom::Err::Error((input, "empty input").into()));
            }
            if slice.contains(&input[0]) {
                Ok((&input[1..], input[0]))
            } else {
                Err(nom::Err::Error((input, "out of range").into()))
            }
        }
    }

    #[inline(always)]
    pub fn byte_in_range<'a>(a: u8, b: u8) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], u8> {
        move |input| {
            if input.is_empty() {
                return Err(nom::Err::Error((input, "empty input").into()));
            }
            if input[0] >= a && input[0] <= b {
                Ok((&input[1..], input[0]))
            } else {
                Err(nom::Err::Error((input, "out of range").into()))
            }
        }
    }

    /// UTF-8 characters can be defined in terms of octets using the
    /// following ABNF `[RFC5234]`, taken from `[RFC3629]`:
    /// UTF8-non-ascii  =   UTF8-2 / UTF8-3 / UTF8-4
    fn utf8_non_ascii(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        /// UTF8-2      = %xC2-DF UTF8-tail
        fn utf8_2(input: &[u8]) -> IResult<&[u8], &[u8]> {
            let (rest, _) = byte_in_range(0xc2, 0xdf)(input)?;
            let (rest, _) = utf8_tail(rest)?;
            Ok((rest, &input[0..2]))
        }
        /// UTF8-3      = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) / %xED
        /// %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
        fn utf8_3<'a>(input: &'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
            alt((
                |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
                    let (rest, _) = byte_in_range(0xe0, 0xe0)(input)?;
                    let (rest, _) = byte_in_range(0xa0, 0xbf)(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..3]))
                },
                |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
                    let (rest, _) = byte_in_range(0xe1, 0xec)(input)?;
                    let (rest, _) = utf8_tail(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..3]))
                },
                |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
                    let (rest, _) = byte_in_range(0xed, 0xed)(input)?;
                    let (rest, _) = byte_in_range(0x80, 0x9f)(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..3]))
                },
                |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
                    let (rest, _) = byte_in_range(0xee, 0xef)(input)?;
                    let (rest, _) = utf8_tail(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..3]))
                },
            ))(input)
        }
        /// UTF8-4      = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
        /// %xF4 %x80-8F 2( UTF8-tail )
        fn utf8_4<'a>(input: &'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
            alt((
                |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
                    let (rest, _) = byte_in_range(0xf0, 0xf0)(input)?;
                    let (rest, _) = byte_in_range(0x90, 0xbf)(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..4]))
                },
                |input: &'a [u8]| {
                    let (rest, _) = byte_in_range(0xf1, 0xf3)(input)?;
                    let (rest, _) = utf8_tail(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..4]))
                },
                |input: &'a [u8]| {
                    let (rest, _) = byte_in_range(0xf4, 0xf4)(input)?;
                    let (rest, _) = byte_in_range(0x80, 0x8f)(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    let (rest, _) = utf8_tail(rest)?;
                    Ok((rest, &input[0..4]))
                },
            ))(input)
        }
        ///  UTF8-tail   = %x80-BF
        fn utf8_tail(input: &[u8]) -> IResult<&[u8], &[u8]> {
            let (rest, _) = byte_in_range(0x80, 0xbf)(input)?;
            Ok((rest, &input[0..1]))
        }

        let (rest, ret) = alt((utf8_2, utf8_3, utf8_4))(input)?;

        Ok((rest, ret.into()))
    }

    ///`%x21-7E`
    /// RFC6532 adds: `VCHAR   =/  UTF8-non-ascii`
    fn vchar(input: &[u8]) -> IResult<&[u8], u8> {
        byte_in_range(0x21, 0x7e)(input)
    }

    ///`quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp`
    fn quoted_pair(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        preceded(
            tag("\\"),
            alt((
                utf8_non_ascii,
                map(vchar, |byte| vec![byte].into()),
                map(wsp, |byte| vec![byte].into()),
            )),
        )(input)
    }

    ///```text
    /// ctext           =   %d33-39 /          ; Printable US-ASCII
    ///                     %d42-91 /          ;  characters not including
    ///                     %d93-126 /         ;  "(", ")", or "\"
    ///                     obs-ctext
    /// ```
    fn ctext(input: &[u8]) -> IResult<&[u8], ()> {
        alt((
            map(
                alt((
                    byte_in_range(33, 39),
                    byte_in_range(42, 91),
                    byte_in_range(93, 126),
                )),
                |_| (),
            ),
            map(utf8_non_ascii, |_| ()),
        ))(input)
    }

    /// Invalid version of [`ctext`] that accepts non-ascii characters.
    fn ctext_invalid(input: &[u8]) -> IResult<&[u8], ()> {
        map(is_not("()\\"), |_| ())(input)
    }

    ///```text
    /// ctext           =   %d33-39 /          ; Printable US-ASCII
    ///                    %d42-91 /          ;  characters not including
    ///                    %d93-126 /         ;  "(", ")", or "\"
    ///                    obs-ctext
    /// ccontent        =   ctext / quoted-pair / comment
    /// comment         =   "(" *([FWS] ccontent) [FWS] ")"
    /// ```
    pub fn comment(input: &[u8]) -> IResult<&[u8], ()> {
        if !input.starts_with(b"(") {
            return Err(nom::Err::Error(
                (input, "comment(): not starting with '('").into(),
            ));
        }
        let mut input = &input[1..];
        let mut comment_level = 1;
        while comment_level > 0 {
            if input.is_empty() {
                return Err(nom::Err::Error(
                    (input, "comment(): unclosed comment").into(),
                ));
            }
            input = context("comment()", opt(fws))(input)?.0;
            while let Ok((_input, _)) = context(
                "comment()",
                alt((ctext, ctext_invalid, map(quoted_pair, |_| ()))),
            )(input)
            {
                input = _input;
            }

            if input.starts_with(b")") {
                comment_level -= 1;
                input = &input[1..];
            } else if input.starts_with(b"(") {
                comment_level += 1;
                input = &input[1..];
            } else {
                input = context("comment()", opt(fws))(input)?.0;
            }
        }
        Ok((input, ()))
    }

    #[test]
    fn test_parser_comment() {
        let s = b"(recursive (comment) block)";
        assert_eq!(comment(s), Ok((&b""[..], ())));
    }

    ///`FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS`
    pub fn fws(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        if let Ok((rest, ws)) = terminated(many0(wsp), crlf)(input) {
            let mut v: Vec<u8> = ws.into_iter().fold(vec![], |mut acc, x| {
                acc.push(x);
                acc
            });

            let mut width = 0;
            let mut input = rest;
            while let Ok((input_, w)) = wsp(input) {
                v.push(w);
                width += 1;
                input = input_;
            }
            if width == 0 {
                Err(nom::Err::Error((input, "fws(): no WSP").into()))
            } else {
                Ok((input, Cow::Owned(v)))
            }
        } else {
            let orig_input = input;
            let mut input = input;
            let mut width = 0;
            while let Ok((input_, _)) = wsp(input) {
                width += 1;
                input = input_;
            }
            if width == 0 {
                Err(nom::Err::Error((input, "fws(): no WSP").into()))
            } else {
                Ok((input, Cow::Borrowed(&orig_input[..width])))
            }
        }
    }

    ///`WSP            =  SP / HTAB ; white space`
    pub fn wsp(input: &[u8]) -> IResult<&[u8], u8> {
        if input.starts_with(b" ") || input.starts_with(b"\t") {
            Ok((&input[1..], input[0]))
        } else {
            Err(nom::Err::Error((input, "wsp(): not whitespace").into()))
        }
    }

    pub fn crlf(input: &[u8]) -> IResult<&[u8], ()> {
        if input.starts_with(b"\n") {
            Ok((&input[1..], ()))
        } else if input.starts_with(b"\r\n") {
            Ok((&input[2..], ()))
        } else {
            Err(nom::Err::Error((input, "crlf(): not whitespace").into()))
        }
    }

    ///`CFWS            =   (1*([FWS] comment) [FWS]) / FWS`
    pub fn cfws(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        alt((
            |input| {
                let (input, pr) = many1(terminated(opt(fws), comment))(input)?;
                let (input, end) = opt(fws)(input)?;
                let mut pr = pr.into_iter().flatten().fold(vec![], |mut acc, x| {
                    acc.extend_from_slice(&x);
                    acc
                });
                if pr.is_empty() {
                    Ok((input, end.unwrap_or_else(|| (&b""[..]).into())))
                } else {
                    if let Some(end) = end {
                        pr.extend_from_slice(&end);
                    }
                    Ok((input, pr.into()))
                }
            },
            fws,
        ))(input)
    }

    ///`unstructured    =   (*([FWS] VCHAR) *WSP) / obs-unstruct`
    pub fn unstructured(input: &[u8]) -> Result<String> {
        let (input, r): (_, Vec<(Option<Cow<'_, [u8]>>, u8)>) =
            many0(pair(opt(fws), vchar))(input)?;
        let (input, rest_wsp): (_, Vec<u8>) = many0(wsp)(input)?;
        let mut ret_s = Vec::new();
        for (opt_slice, b) in r {
            if let Some(slice) = opt_slice {
                ret_s.extend_from_slice(&slice);
            }
            ret_s.push(b);
        }
        ret_s.extend_from_slice(&rest_wsp);
        let ret_s = String::from_utf8_lossy(&ret_s).into_owned();
        if !input.is_empty() {
            Err(Error::from(format!(
                "unstructured(): unmatched input: {} while result is {}",
                to_str!(input),
                ret_s
            )))
        } else {
            Ok(ret_s)
        }
    }

    pub fn mailto(mut input: &[u8]) -> IResult<&[u8], Mailto> {
        let orig_input = input;
        if !input.starts_with(b"mailto:") {
            return Err(nom::Err::Error(
                (input, "mailto(): input doesn't start with `mailto:`").into(),
            ));
        }

        let mut body = None;
        let mut headers = HeaderMap::empty();
        let mut address: Vec<Address>;

        if String::from_utf8_lossy(input).matches('?').count() > 1 {
            return Err(nom::Err::Error(
                (input, "mailto(): Using '?' twice is invalid.").into(),
            ));
        }
        input = &input[b"mailto:".len()..];
        let mut decoded_owned = percent_decode(input).decode_utf8().unwrap().to_string();

        let mut substitutions = vec![];
        for (i, _) in decoded_owned.match_indices('&') {
            if let Some(j) = HtmlEntity::ALL
                .iter()
                .position(|e| decoded_owned[i..].starts_with(e))
            {
                substitutions.push((i, HtmlEntity::ALL[j].len(), HtmlEntity::GLYPHS[j]));
            }
        }

        for (i, len, g) in substitutions.into_iter().rev() {
            decoded_owned.replace_range(i..(i + len), g);
        }

        let mut decoded = decoded_owned.as_str();

        let end = decoded.as_bytes().iter().position(|e| *e == b'?');
        let end_or_len = end.unwrap_or(decoded.len());

        if let Ok(addr) = Address::list_try_from(&decoded[..end_or_len]) {
            address = addr;
            decoded = if decoded[end_or_len..].is_empty() {
                &decoded[end_or_len..]
            } else {
                &decoded[end_or_len + 1..]
            };
        } else if end.is_some() {
            decoded = &decoded[1..];
            address = vec![];
        } else {
            return Err(nom::Err::Error(
                (
                    input,
                    format!("input {:?}", String::from_utf8_lossy(orig_input)),
                )
                    .into(),
            ));
        }

        if !address.is_empty() {
            let mut full_address = String::new();
            for address in &address {
                write!(&mut full_address, "{}, ", address)
                    .expect("Could not write into a String, are you out of memory?");
            }
            if full_address.ends_with(", ") {
                let len = full_address.len();
                full_address.truncate(len - ", ".len());
            }
            headers.insert(HeaderName::TO, full_address);
        }

        while !decoded.is_empty() {
            if decoded.starts_with("&amp;") {
                decoded = &decoded["&amp;".len()..];
                continue;
            }

            let tag = if let Some(tag_pos) = decoded.as_bytes().iter().position(|e| *e == b'=') {
                let ret = &decoded[0..tag_pos];
                decoded = &decoded[tag_pos + 1..];
                ret
            } else if decoded.as_bytes().starts_with(b"body") {
                let ret = &decoded[0.."body".len()];
                decoded = &decoded[("body".len() + 1)..];
                ret
            } else {
                return Err(nom::Err::Error(
                    (
                        input,
                        format!("mailto(): extra characters found in input: {}", decoded),
                    )
                        .into(),
                ));
            };

            let value_end = if tag == "body" {
                decoded.len()
            } else {
                decoded
                    .as_bytes()
                    .iter()
                    .position(|e| *e == b'&')
                    .unwrap_or(decoded.len())
            };

            let value = decoded[..value_end].to_string();
            match tag {
                "body" if body.is_none() => {
                    body = Some(value);
                }
                other => match HeaderName::try_from(other) {
                    Ok(hdr) if hdr == HeaderName::TO => {
                        if !headers.contains_key(&hdr) {
                            if let Ok(address_val) = Address::list_try_from(value.as_str()) {
                                address.extend(address_val.into_iter());
                            }
                            headers.insert(HeaderName::TO, value);
                        }
                    }
                    Ok(hdr) if hdr.is_standard() => {
                        if Mailto::IGNORE_HEADERS.contains(&hdr) {
                            log::warn!(
                                "parsing mailto(): header {} is not allowed in mailto URIs for \
                                 safety and will be ignored. Value was {:?}",
                                hdr,
                                value
                            );
                        }
                        if !headers.contains_key(&hdr) {
                            headers.insert(hdr, value);
                        }
                    }
                    Ok(hdr) => {
                        log::warn!(
                            "parsing mailto(): header {} is not a known header and it will be \
                             ignored.Value was {:?}",
                            hdr,
                            value
                        );
                    }
                    _ => {
                        return Err(nom::Err::Error(
                            (input, "mailto(): unknown tag in input").into(),
                        ));
                    }
                },
            }
            if decoded[value_end..].is_empty() {
                break;
            }
            decoded = &decoded[value_end + 1..];
        }
        Ok((
            input,
            Mailto {
                address,
                body,
                headers,
            },
        ))
    }

    pub struct HeaderIterator<'a>(pub &'a [u8]);

    impl<'a> Iterator for HeaderIterator<'a> {
        type Item = (HeaderName, &'a [u8]);

        fn next(&mut self) -> Option<(HeaderName, &'a [u8])> {
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

    #[test]
    fn test_parser_cfws() {
        let s = r#"This
 is a test"#;
        assert_eq!(&unstructured(s.as_bytes()).unwrap(), "This is a test",);

        assert_eq!(&unstructured(s.as_bytes()).unwrap(), "This is a test",);
        let s = "this is\n\ta folded name";
        assert_eq!(
            &unstructured(s.as_bytes()).unwrap(),
            "this is\ta folded name",
        );
    }

    ///`atom            =   [CFWS] 1*atext [CFWS]`
    pub fn atom(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let (input, opt_space) = opt(cfws)(input)?;
        let mut i = 0;
        while i < input.len() {
            //&& !input[i].is_ascii_whitespace() {
            match input[i] {
                b'(' | b')' | b'<' | b'>' | b'[' | b']' | b':' | b';' | b'@' | b'\\' | b','
                | b'.' | b'\r' | b'\n' | b'"' => break,
                _ => {}
            }
            i += 1;
        }
        if i == 0 {
            return Err(nom::Err::Error(
                (input, "atom(): starts with whitespace or empty").into(),
            ));
        }
        while i + 1 > 0 {
            if input[i - 1] == b' ' || input[i - 1] == b'\t' {
                i -= 1;
            } else {
                break;
            }
        }
        let (rest, opt_space2) = opt(cfws)(&input[i..])?;
        let ret = if opt_space.is_some() || opt_space2.is_some() {
            let mut ret = Vec::with_capacity(i + 2);
            if let Some(opt_space) = opt_space {
                ret.extend_from_slice(&opt_space);
            }
            ret.extend_from_slice(&input[..i]);
            if let Some(opt_space) = opt_space2 {
                ret.extend_from_slice(&opt_space);
            }
            Cow::Owned(ret)
        } else {
            Cow::Borrowed(&input[..i])
        };
        Ok((rest, ret))
    }

    ///`quoted-string   =   [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE
    /// [CFWS]`
    pub fn quoted_string(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let (input, opt_space) = opt(cfws)(input)?;
        if !input.starts_with(b"\"") {
            return Err(nom::Err::Error(
                (input, "quoted_string(): doesn't start with DQUOTE").into(),
            ));
        }
        let input = &input[1..];
        let mut i = 0;
        while i < input.len() && input[i] != b'"' {
            if opt_space.is_some() || (input[i..].starts_with(b"\\") && i + 1 < input.len()) {
                let mut ret = if let Some(opt_space) = opt_space {
                    let mut r = Vec::with_capacity(2 * i);
                    r.extend_from_slice(&opt_space);
                    r
                } else {
                    Vec::with_capacity(2 * i)
                };
                ret.extend_from_slice(&input[..i]);
                i += 1;
                ret.push(input[i]);
                i += 1;
                while i < input.len() && input[i] != b'"' {
                    if input[i..].starts_with(b"\\") && i + 1 < input.len() {
                        i += 1;
                    }
                    ret.push(input[i]);
                    i += 1;
                }
                if i < input.len() {
                    // skip DQUOTE
                    i += 1;
                } else {
                    return Err(nom::Err::Error(
                        (input, "quoted_string(): unclosed DQUOTE").into(),
                    ));
                }

                let (rest, opt_sp) = opt(cfws)(&input[i..])?;
                if let Some(opt_sp) = opt_sp {
                    ret.extend_from_slice(&opt_sp);
                }
                let ret = Cow::Owned(ret);
                return Ok((rest, ret));
            }
            i += 1;
        }
        let ret = Cow::Borrowed(&input[..i]);
        if i < input.len() {
            // skip DQUOTE
            i += 1;
        } else {
            return Err(nom::Err::Error(
                (input, "quoted_string(): unclosed DQUOTE").into(),
            ));
        }

        let (rest, opt_sp) = opt(cfws)(&input[i..])?;
        if let Some(opt_sp) = opt_sp {
            let mut ret = ret.to_vec();
            ret.extend_from_slice(&opt_sp);
            Ok((rest, Cow::Owned(ret)))
        } else {
            Ok((rest, ret))
        }
    }

    ///`word            =   atom / quoted-string`
    pub fn word(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        alt((quoted_string, atom))(input)
    }

    ///`phrase          =   1*word / obs-phrase`
    pub fn phrase2(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        let (rest, words) = many1(word)(input)?;
        let len = words.iter().map(|v| v.len()).sum::<usize>();
        let mut ret = words
            .into_iter()
            .fold(Vec::with_capacity(len), |mut acc, el| {
                acc.extend_from_slice(&el);
                acc
            });
        let right_wsp_padding = ret.len() - ret.rtrim().len();
        for _ in 0..right_wsp_padding {
            ret.pop();
        }
        Ok((rest, ret))
    }

    #[test]
    fn test_phrase() {
        let s = b"\"Jeffrey \\\"fejj\\\" Stedfast\""; // <fejj@helixcode.com>"
        assert_eq!(to_str!(&phrase2(s).unwrap().1), "Jeffrey \"fejj\" Stedfast");
    }

    ///dot-atom-text   =   1*atext *("." 1*atext)
    pub fn dot_atom_text(mut input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let mut ret = vec![];
        let mut at_least_one = false;
        while let Ok((_input, atext_r)) = atext(input) {
            at_least_one = true;
            ret.extend_from_slice(&atext_r);
            input = _input;
        }
        if !at_least_one {
            return Err(nom::Err::Error(
                (input, "dot_atom(): starts with at least one atext").into(),
            ));
        }

        loop {
            if !input.starts_with(b".") {
                break;
            }
            ret.push(b'.');
            input = &input[1..];
            let mut at_least_one = false;
            while let Ok((_input, atext_r)) = atext(input) {
                at_least_one = true;
                ret.extend_from_slice(&atext_r);
                input = _input;
            }
            if !at_least_one {
                return Err(nom::Err::Error(
                    (input, "dot_atom(): DOT followed with at least one atext").into(),
                ));
            }
        }
        Ok((input, ret.into()))
    }

    ///`atext           =   ALPHA / DIGIT /    ; Printable US-ASCII "!" / "#" /
    /// ;  characters not including "$" / "%" /        ;  specials.  Used for
    /// atoms.  "&" / "'" / "*" / "+" / "-" / "/" / "=" / "?" / "^" / "_" / "`"
    /// / "{" / "|" / "}" / "~"`
    pub fn atext_ascii(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        if input.is_empty() {
            return Err(nom::Err::Error((input, "atext(): empty input").into()));
        }
        if input[0].is_ascii_alphanumeric()
            || [
                b'!', b'#', b'$', b'%', b'&', b'\'', b'*', b'+', b'-', b'/', b'=', b'?', b'^',
                b'_', b'`', b'{', b'|', b'}', b'~',
            ]
            .contains(&input[0])
        {
            Ok((&input[1..], input[0..1].into()))
        } else {
            Err(nom::Err::Error((input, "atext(): invalid byte").into()))
        }
    }

    pub fn atext(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        alt((atext_ascii, utf8_non_ascii))(input)
    }

    ///`dot-atom        =   [CFWS] dot-atom-text [CFWS]`
    pub fn dot_atom(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let (input, _) = opt(cfws)(input)?;
        let (input, ret) = dot_atom_text(input)?;
        let (input, _) = opt(cfws)(input)?;
        Ok((input, ret))
    }

    ///```text
    /// dtext           =   %d33-90 /          ; Printable US-ASCII
    ///                    %d94-126 /         ;  characters not including
    ///                    obs-dtext          ;  "[", "]", or "\"
    /// ```
    pub fn dtext(input: &[u8]) -> IResult<&[u8], u8> {
        alt((byte_in_range(33, 90), byte_in_range(94, 125)))(input)
    }
}

pub mod mailing_lists {
    //! Mailing lists headers.
    //!
    //! Implemented RFCs:
    //!
    //! - [RFC2369 "The Use of URLs as Meta-Syntax for Core Mail List Commands and their Transport through Message Header Fields"](https://tools.ietf.org/html/rfc2369)
    use generic::cfws;

    use super::*;

    ///Parse the value of headers defined in RFC2369 "The Use of URLs as
    /// Meta-Syntax for Core Mail List Commands and their Transport through
    /// Message Header Fields"
    pub fn rfc_2369_list_headers_action_list(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
        let (input, _) = opt(cfws)(input)?;
        let (input, ret) = alt((
            separated_list1(
                delimited(
                    map(opt(cfws), |_| ()),
                    map(is_a(", "), |_| ()),
                    map(opt(cfws), |_| ()),
                ),
                delimited(tag("<"), take_until(">"), tag(">")),
            ),
            map(delimited(tag("<"), take_until(">"), tag(">")), |el| {
                vec![el]
            }),
            map(
                delimited(
                    map(opt(cfws), |_| ()),
                    map(tag("NO"), |_| ()),
                    map(opt(cfws), |_| ()),
                ),
                |_| vec![&b"NO"[..]],
            ),
        ))(input)?;
        let (input, _) = opt(cfws)(input)?;
        Ok((input, ret))
    }

    #[test]
    fn test_parser_rfc_2369_list() {
        let s = r#"List-Help: <mailto:list@host.com?subject=help> (List Instructions)
List-Help: <mailto:list-manager@host.com?body=info>
List-Help: <mailto:list-info@host.com> (Info about the list)
List-Help: <http://www.host.com/list/>, <mailto:list-info@host.com>
List-Help: <ftp://ftp.host.com/list.txt> (FTP),
    <mailto:list@host.com?subject=help>
List-Post: <mailto:list@host.com>
List-Post: <mailto:moderator@host.com> (Postings are Moderated)
List-Post: <mailto:moderator@host.com?subject=list%20posting>
List-Post: NO (posting not allowed on this list)
List-Archive: <mailto:archive@host.com?subject=index%20list>
List-Archive: <ftp://ftp.host.com/pub/list/archive/>
List-Archive: <http://www.host.com/list/archive/> (Web Archive)
"#;
        let (rest, headers) = headers::headers(s.as_bytes()).unwrap();
        assert!(rest.is_empty());
        for (_h, v) in headers {
            let (rest, _action_list) = rfc_2369_list_headers_action_list(v).unwrap();
            assert!(rest.is_empty());
        }
    }
}

pub mod headers {
    //! Email headers.
    use super::*;

    pub fn headers(input: &[u8]) -> IResult<&[u8], Vec<(HeaderName, &[u8])>> {
        many1(header)(input)
    }

    pub fn header(input: &[u8]) -> IResult<&[u8], (HeaderName, &[u8])> {
        alt((header_without_val, header_with_val))(input)
    }

    pub fn header_without_val(input: &[u8]) -> IResult<&[u8], (HeaderName, &[u8])> {
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
        let Ok(name) = HeaderName::try_from(name) else {
            return Err(nom::Err::Error(
                (input, "header_without_val(): invalid header name").into(),
            ));
        };
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
     * 	by --------------------- (--------------------- [------------------])
     * (-----------------------) 	with ESMTP id ------------ for
     * <------------------->; 	Tue,  5 Jan 2016 21:30:44 +0100 (CET)
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

    /// Parse a single header as a ([`HeaderName`](crate::email::HeaderName),
    /// [`&[u8]`]) tuple.
    pub fn header_with_val(input: &[u8]) -> IResult<&[u8], (HeaderName, &[u8])> {
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
        let Ok(name) = HeaderName::try_from(name) else {
            return Err(nom::Err::Error(
                (input, "header_with_val(): invalid header name").into(),
            ));
        };
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
    //! Email attachments.
    use super::*;
    use crate::email::{
        address::*,
        attachment_types::{ContentDisposition, ContentDispositionKind},
    };

    pub fn attachment(input: &[u8]) -> IResult<&[u8], (std::vec::Vec<(HeaderName, &[u8])>, &[u8])> {
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
            /* RFC2183 Content-Disposition: "Unrecognized disposition types should be treated as
             * `attachment'." */
            kind: if kind.trim().eq_ignore_ascii_case(b"inline") {
                ContentDispositionKind::Inline
            } else {
                ContentDispositionKind::Attachment
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
    //! Email encodings (quoted printable, `MIME`).
    use data_encoding::BASE64_MIME;
    use encoding::{all::*, DecoderTrap, Encoding};

    use super::*;
    use crate::email::attachment_types::Charset;
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
            b'b' | b'B' => BASE64_MIME
                .decode(encoded_text)
                .map_or_else(|_| encoded_text.to_vec(), |v| v),
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

        if Charset::UTF8 == charset {
            Ok((&input[encoded_end_idx + 2..], s))
        } else {
            decode_charset(&s, charset).map_or_else(
                |_| {
                    Err(nom::Err::Error(
                        (
                            input,
                            format!("encoded_word(): unknown charset {:?}", charset),
                        )
                            .into(),
                    ))
                },
                |v| Ok((&input[encoded_end_idx + 2..], v.into_bytes())),
            )
        }
    }

    pub fn decode_charset(s: &[u8], charset: Charset) -> Result<String> {
        match charset {
            Charset::UTF8 | Charset::Ascii => Ok(String::from_utf8_lossy(s).to_string()),
            Charset::ISO8859_1 => Ok(ISO_8859_1.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_2 => Ok(ISO_8859_2.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_3 => Ok(ISO_8859_3.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_4 => Ok(ISO_8859_4.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_5 => Ok(ISO_8859_5.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_6 => Ok(ISO_8859_6.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_7 => Ok(ISO_8859_7.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_8 => Ok(ISO_8859_8.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_10 => Ok(ISO_8859_10.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_13 => Ok(ISO_8859_13.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_14 => Ok(ISO_8859_14.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_15 => Ok(ISO_8859_15.decode(s, DecoderTrap::Strict)?),
            Charset::ISO8859_16 => Ok(ISO_8859_16.decode(s, DecoderTrap::Strict)?),
            Charset::GBK => Ok(GBK.decode(s, DecoderTrap::Strict)?),
            Charset::Windows1250 => Ok(WINDOWS_1250.decode(s, DecoderTrap::Strict)?),
            Charset::Windows1251 => Ok(WINDOWS_1251.decode(s, DecoderTrap::Strict)?),
            Charset::Windows1252 => Ok(WINDOWS_1252.decode(s, DecoderTrap::Strict)?),
            Charset::Windows1253 => Ok(WINDOWS_1253.decode(s, DecoderTrap::Strict)?),
            Charset::KOI8R => Ok(KOI8_R.decode(s, DecoderTrap::Strict)?),
            Charset::KOI8U => Ok(KOI8_U.decode(s, DecoderTrap::Strict)?),
            Charset::BIG5 => Ok(BIG5_2003.decode(s, DecoderTrap::Strict)?),
            Charset::GB2312 => {
                Ok(encoding::codec::simpchinese::GBK_ENCODING.decode(s, DecoderTrap::Strict)?)
            }
            Charset::GB18030 => Ok(
                encoding::codec::simpchinese::GB18030_ENCODING.decode(s, DecoderTrap::Strict)?
            ),
            Charset::UTF16 => {
                Ok(encoding::codec::utf_16::UTF_16LE_ENCODING.decode(s, DecoderTrap::Strict)?)
            }
            Charset::ISO2022JP => Ok(ISO_2022_JP.decode(s, DecoderTrap::Strict)?),
            Charset::EUCJP => Ok(EUC_JP.decode(s, DecoderTrap::Strict)?),
        }
    }

    fn quoted_printable_soft_break(input: &[u8]) -> IResult<&[u8], u8> {
        if input.starts_with(b"=\n") {
            Ok((&input[2..], input[1])) // `=\n` is an escaped space character.
        } else if input.starts_with(b"=\r\n") {
            Ok((&input[3..], input[2])) // `=\r\n` is an escaped space
                                        // character.
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

    // With MIME, headers in quoted printable format can contain underscores that
    // represent spaces. In non-header context, an underscore is just a plain
    // underscore.
    pub fn quoted_printable_bytes_header(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        many0(alt((quoted_printable_byte, qp_underscore_header, le_u8)))(input)
    }

    // For atoms in Header values.
    pub fn quoted_printable_bytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        many0(alt((
            terminated(quoted_printable_soft_break, tag("\n")),
            terminated(quoted_printable_soft_break, tag("\r\n")),
            terminated(quoted_printable_soft_break, generic::eof),
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
        let (input, list) = separated_list1(space, encoded_word)(input)?;
        let list_len = list.iter().fold(0, |mut acc, x| {
            acc += x.len();
            acc
        });
        Ok((
            input,
            list.iter()
                .fold(SmallVec::with_capacity(list_len), |mut acc, x| {
                    acc.extend(x.iter().cloned());
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

            let end = end.unwrap_or(input.len() - ptr) + ptr;
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
    //! Parsing of address values and address-related headers.
    //!
    //! Implemented RFCs:
    //!
    //! - [RFC5322 "Internet Message Format"](https://tools.ietf.org/html/rfc5322)
    //! - [RFC6532 "Internationalized Email Headers"](https://tools.ietf.org/html/rfc6532)
    //! - [RFC2047 "MIME Part Three: Message Header Extensions for Non-ASCII Text"](https://tools.ietf.org/html/rfc2047)
    use super::*;
    use crate::email::{
        address::*,
        parser::generic::{atom, cfws, dot_atom, dot_atom_text, dtext, phrase2, quoted_string},
    };
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

    ///`angle-addr      =   [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr`
    pub fn angle_addr(input: &[u8]) -> IResult<&[u8], Address> {
        let (input, _) = opt(cfws)(input)?;
        let (input, _) = tag("<")(input)?;
        let (input, addr_spec) = addr_spec(input)?;
        let (input, _) = tag(">")(input)?;
        let (input, _) = opt(cfws)(input)?;
        Ok((input, addr_spec))
    }

    ///`obs-domain      =   atom *("." atom)`
    pub fn obs_domain(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let (mut input, atom_) = context("obs_domain", atom)(input)?;
        let mut ret: Vec<u8> = atom_.into();
        loop {
            if !input.starts_with(b".") {
                break;
            }
            ret.push(b'.');
            input = &input[1..];
            if let Ok((_input, atom_)) = context("obs_domain", atom)(input) {
                ret.extend_from_slice(&atom_);
                input = _input;
            } else {
                return Err(nom::Err::Error(
                    (input, "obs_domain(): expected <atom> after DOT").into(),
                ));
            }
        }
        Ok((input, ret.into()))
    }

    ///`local-part      =   dot-atom / quoted-string / obs-local-part`
    pub fn local_part(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        alt((dot_atom, quoted_string))(input)
    }

    ///`domain          =   dot-atom / domain-literal / obs-domain`
    pub fn domain(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        alt((dot_atom, domain_literal, obs_domain))(input)
    }

    ///`domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]`
    pub fn domain_literal(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        use crate::email::parser::generic::fws;
        let (input, first_opt_space) = context("domain_literal()", opt(cfws))(input)?;
        let (input, _) = context("domain_literal()", tag("["))(input)?;
        let (input, dtexts) = many0(pair(opt(fws), dtext))(input)?;
        let (input, end_fws): (_, Option<_>) = context("domain_literal()", opt(fws))(input)?;
        let (input, _) = context("domain_literal()", tag("]"))(input)?;
        let (input, _) = context("domain_literal()", opt(cfws))(input)?;
        let mut ret_s = vec![b'['];
        if let Some(first_opt_space) = first_opt_space {
            ret_s.extend_from_slice(&first_opt_space);
        }
        for (fws_opt, dtext) in dtexts {
            if let Some(fws_opt) = fws_opt {
                ret_s.extend_from_slice(&fws_opt);
            }
            ret_s.push(dtext);
        }
        if let Some(end_fws) = end_fws {
            ret_s.extend_from_slice(&end_fws);
        }
        ret_s.push(b']');
        Ok((input, ret_s.into()))
    }

    ///`addr-spec       =   local-part "@" domain`
    pub fn addr_spec(input: &[u8]) -> IResult<&[u8], Address> {
        let (input, local_part) = context("addr_spec()", local_part)(input)?;
        let (input, _) = context("addr_spec()", tag("@"))(input)?;
        let (input, domain) = context("addr_spec()", domain)(input)?;

        Ok((
            input,
            Address::new(
                None,
                format!("{}@{}", to_str!(&local_part), to_str!(&domain)),
            ),
        ))
    }

    ///Returns the raw `local_part` and `domain` parts.
    ///
    ///`addr-spec       =   local-part "@" domain`
    pub fn addr_spec_raw(input: &[u8]) -> IResult<&[u8], (Cow<'_, [u8]>, Cow<'_, [u8]>)> {
        let (input, local_part) = context("addr_spec()", local_part)(input)?;
        let (input, _) = context("addr_spec()", tag("@"))(input)?;
        let (input, domain) = context("addr_spec()", domain)(input)?;

        Ok((input, (local_part, domain)))
    }

    ///`display-name    =   phrase`
    pub fn display_name(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        let (rest, ret) = phrase2(input)?;
        if let Ok((_, ret)) = crate::email::parser::encodings::phrase(&ret, true) {
            Ok((rest, ret))
        } else {
            Ok((rest, ret))
        }
    }

    ///`name-addr       =   [display-name] angle-addr`
    pub fn name_addr(input: &[u8]) -> IResult<&[u8], Address> {
        let (input, (display_name, angle_addr)) = alt((
            pair(map(display_name, Some), angle_addr),
            map(angle_addr, |r| (None, r)),
        ))(input.ltrim())?;
        Ok((
            input,
            Address::new(
                display_name.map(|v| to_str!(&v).to_string()),
                angle_addr.get_email(),
            ),
        ))
    }

    ///`mailbox         =   name-addr / addr-spec`
    pub fn mailbox(input: &[u8]) -> IResult<&[u8], Address> {
        alt((addr_spec, name_addr))(input.ltrim())
    }

    ///`group-list      =   mailbox-list / CFWS / obs-group-list`
    pub fn group_list(input: &[u8]) -> IResult<&[u8], Vec<Address>> {
        ///`mailbox-list    =   (mailbox *("," mailbox)) / obs-mbox-list`
        fn mailbox_list(input: &[u8]) -> IResult<&[u8], Vec<Address>> {
            let (mut input, first_m) = mailbox(input.ltrim())?;
            let mut ret = vec![first_m];
            loop {
                if !input.starts_with(b",") {
                    break;
                }
                input = &input[1..];
                let (input_, next_m) = mailbox(input)?;
                ret.push(next_m);
                input = input_;
            }
            Ok((input, ret))
        }

        if let Ok((input, mailboxes)) = mailbox_list(input.ltrim()) {
            Ok((input, mailboxes))
        } else {
            let (input, _) = cfws(input)?;
            Ok((input, vec![]))
        }
    }

    ///`group           =   display-name ":" [group-list] ";" [CFWS]`
    fn group(input: &[u8]) -> IResult<&[u8], Address> {
        let (input, display_name) = context("group()", display_name)(input)?;
        let (input, _) = context("group()", tag(":"))(input)?;
        let (input, group_list): (_, Option<Vec<Address>>) =
            context("group()", opt(group_list))(input)?;
        let (input, _) = context("group()", tag(";"))(input)?;
        let (input, _) = context("group()", opt(cfws))(input)?;
        Ok((
            input,
            Address::new_group(
                to_str!(&display_name).to_string(),
                group_list.unwrap_or_default(),
            ),
        ))
    }

    /// `address         =   mailbox / group`
    pub fn address(input: &[u8]) -> IResult<&[u8], Address> {
        alt((mailbox, group))(input.ltrim())
    }

    pub fn rfc2822address_list(input: &[u8]) -> IResult<&[u8], SmallVec<[Address; 1]>> {
        separated_list_smallvec(is_a(", "), address)(input.ltrim())
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
                        acc.push(' ');
                        i += 1;
                    }
                    acc
                }),
        ))
    }

    ///`no-fold-literal =   "[" *dtext "]"`
    pub fn no_fold_literal(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        let orig_input = input;
        let (input, _) = tag("[")(input)?;
        let (input, ret) = many0(dtext)(input)?;
        let (input, _) = tag("]")(input)?;
        Ok((input, Cow::Borrowed(&orig_input[0..ret.len() + 1])))
    }

    ///`id-left         =   dot-atom-text / obs-id-left`
    pub fn id_left(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        dot_atom_text(input)
    }
    ///`id-right        =   dot-atom-text / no-fold-literal / obs-id-right`
    pub fn id_right(input: &[u8]) -> IResult<&[u8], Cow<'_, [u8]>> {
        alt((dot_atom_text, no_fold_literal))(input)
    }

    ///`msg-id =   [CFWS] "<" id-left "@" id-right ">" [CFWS]`
    pub fn msg_id(input: &[u8]) -> IResult<&[u8], MessageID> {
        let (input, _) = opt(cfws)(input)?;
        let orig_input = input;
        let (input, _) = tag("<")(input)?;
        let (input, id_left_) = id_left(input)?;
        let (input, _) = tag("@")(input)?;
        let (input, id_right_) = id_right(input)?;
        let (input, _) = tag(">")(input)?;
        let (input, _) = opt(cfws)(input)?;
        Ok((
            input,
            MessageID::new(
                &orig_input[..3 + id_left_.len() + id_right_.len()],
                &orig_input[1..2 + id_left_.len() + id_right_.len()],
            ),
        ))
    }

    pub fn msg_id_list(input: &[u8]) -> IResult<&[u8], Vec<MessageID>> {
        many0(msg_id)(input)
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
    use super::{address::*, encodings::*, *};
    use crate::{email::address::*, make_address};

    #[test]
    fn test_phrase() {
        let words = b"=?iso-8859-7?B?W215Y291cnNlcy5udHVhLmdyIC0gyvXs4fTp6t4g6uHpIMri4e306ere?=
     =?iso-8859-7?B?INb18+nq3l0gzd3hIMHt4erv3+358+c6IMzF0c/TIMHQz9TFy8XTzMHU?=
      =?iso-8859-7?B?2c0gwiDUzC4gysHNLiDFzsXUwdPH0yAyMDE3LTE4OiDTx8zFydnTxw==?=";
        assert_eq!(
            "[mycourses.ntua.gr - Κυματική και Κβαντική Φυσική] Νέα Ανακοίνωση: ΜΕΡΟΣ \
             ΑΠΟΤΕΛΕΣΜΑΤΩΝ Β ΤΜ. ΚΑΝ. ΕΞΕΤΑΣΗΣ 2017-18: ΣΗΜΕΙΩΣΗ",
            std::str::from_utf8(&phrase(words.trim(), false).unwrap().1).unwrap()
        );
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

        let words = r#"=?gb18030?B?zNrRtsbz0rXTys/k19S2r9eqt6LR6dak08q8/g==?="#;
        assert_eq!(
            "腾讯企业邮箱自动转发验证邮件",
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

    //    // [ref:FIXME]: add file
    //#[test]
    //fn test_attachments() {
    //            use std::io::Read;
    //            let mut buffer: Vec<u8> = Vec::new();
    //            let _ = std::fs::File::open("").unwrap().read_to_end(&mut buffer);
    //            let boundary = b"b1_4382d284f0c601a737bb32aaeda53160";
    //            let (_, body) = match mail(&buffer) {
    //                Ok(v) => v,
    //                Err(_) => panic!(),
    //            };
    //            let attachments = parts(body, boundary).unwrap().1;
    //            assert_eq!(attachments.len(), 4);
    //            let v: Vec<&str> = attachments
    //                .iter()
    //                .map(|v| std::str::from_utf8(v).unwrap())
    //                .collect();
    //    //println!("attachments {:?}", v);
    //}

    #[test]
    fn test_addresses() {
        macro_rules! assert_parse {
            ($name:literal, $addr:literal, $raw:literal) => {{
                #[allow(clippy::string_lit_as_bytes)]
                let s = $raw.as_bytes();
                let r = address(s).unwrap().1;
                match r {
                    Address::Mailbox(ref m) => {
                        assert_eq!(to_str!(m.display_name.display_bytes(&m.raw)), $name);
                        assert_eq!(to_str!(m.address_spec.display_bytes(&m.raw)), $addr);
                    }
                    _ => assert!(false),
                }
            }};
        }

        assert_parse!(
            "Σταύρος Μαλτέζος",
            "maltezos@central.ntua.gr",
            "=?iso-8859-7?B?0/Th/fHv8iDM4ev03ebv8g==?= <maltezos@central.ntua.gr>"
        );
        assert_parse!("", "user@domain", "user@domain");
        assert_parse!("", "user@domain", "<user@domain>");
        assert_parse!("", "user@domain", "  <user@domain>");
        assert_parse!("Name", "user@domain", "Name <user@domain>");
        assert_parse!(
            "",
            "julia@ficdep.minitrue",
            "julia(outer party)@ficdep.minitrue"
        );
        assert_parse!(
            "Winston Smith",
            "winston.smith@recdep.minitrue",
            "\"Winston Smith\" <winston.smith@recdep.minitrue> (Records Department)"
        );
        assert_parse!(
            "John Q. Public",
            "JQB@bar.com",
            "\"John Q. Public\" <JQB@bar.com>"
        );
        assert_parse!(
            "John Q. Public",
            "JQB@bar.com",
            "John \"Q.\" Public <JQB@bar.com>"
        );
        assert_parse!(
            "John Q. Public",
            "JQB@bar.com",
            "\"John Q.\" Public <JQB@bar.com>"
        );
        assert_parse!(
            "John Q. Public",
            "JQB@bar.com",
            "John \"Q. Public\" <JQB@bar.com>"
        );
        assert_parse!(
            "Jeffrey Stedfast",
            "fejj@helixcode.com",
            "Jeffrey Stedfast <fejj@helixcode.com>"
        );
        assert_parse!(
            "this is\ta folded name",
            "folded@name.com",
            "this is\n\ta folded name <folded@name.com>"
        );
        assert_parse!(
            "Jeffrey fejj Stedfast",
            "fejj@helixcode.com",
            "Jeffrey fejj Stedfast <fejj@helixcode.com>"
        );
        assert_parse!(
            "Jeffrey fejj Stedfast",
            "fejj@helixcode.com",
            "Jeffrey \"fejj\" Stedfast <fejj@helixcode.com>"
        );
        assert_parse!(
            "Jeffrey \"fejj\" Stedfast",
            "fejj@helixcode.com",
            "\"Jeffrey \\\"fejj\\\" Stedfast\" <fejj@helixcode.com>"
        );
        assert_parse!(
            "Stedfast, Jeffrey",
            "fejj@helixcode.com",
            "\"Stedfast, Jeffrey\" <fejj@helixcode.com>"
        );
        assert_parse!(
            "",
            "fejj@helixcode.com",
            "fejj@helixcode.com (Jeffrey Stedfast)"
        );
        assert_parse!(
            "Jeffrey Stedfast",
            "fejj@helixcode.com",
            "Jeffrey Stedfast <fejj(nonrecursive block)@helixcode.(and a comment here)com>"
        );
        assert_parse!(
            "Jeffrey Stedfast",
            "fejj@helixcode.com",
            "Jeffrey Stedfast <fejj(recursive (comment) block)@helixcode.(and a comment here)com>"
        );
        assert_parse!(
            "Joe Q. Public",
            "john.q.public@example.com",
            "\"Joe Q. Public\" <john.q.public@example.com>"
        );
        assert_parse!("Mary Smith", "mary@x.test", "Mary Smith <mary@x.test>");
        assert_parse!("Mary Smith", "mary@x.test", "Mary Smith <mary@x.test>");
        assert_parse!("", "jdoe@example.org", "jdoe@example.org");
        assert_parse!("Who?", "one@y.test", "Who? <one@y.test>");
        assert_parse!("", "boss@nil.test", "<boss@nil.test>");
        assert_parse!(
            "Giant; \"Big\" Box",
            "sysservices@example.net",
            r#""Giant; \"Big\" Box" <sysservices@example.net>"#
        );
        //assert_eq!(
        //    make_address!("Jeffrey Stedfast", "fejj@helixcode.com"),
        //    address(b"Jeffrey Stedfast <fejj@helixcode.com.>")
        //        .unwrap()
        //        .1
        //);
        assert_parse!(
            "John <middle> Doe",
            "jdoe@machine.example",
            "\"John <middle> Doe\" <jdoe@machine.example>"
        );
        // RFC 2047 "Q"-encoded ISO-8859-1 address.
        assert_parse!(
            "Jörg Doe",
            "joerg@example.com",
            "=?iso-8859-1?q?J=F6rg_Doe?= <joerg@example.com>"
        );

        // RFC 2047 "Q"-encoded US-ASCII address. Dumb but legal.
        assert_parse!(
            "Jorg Doe",
            "joerg@example.com",
            "=?us-ascii?q?J=6Frg_Doe?= <joerg@example.com>"
        );
        // RFC 2047 "Q"-encoded UTF-8 address.
        assert_parse!(
            "Jörg Doe",
            "joerg@example.com",
            "=?utf-8?q?J=C3=B6rg_Doe?= <joerg@example.com>"
        );
        // RFC 2047 "Q"-encoded UTF-8 address with multiple encoded-words.
        assert_parse!(
            "JörgDoe",
            "joerg@example.com",
            "=?utf-8?q?J=C3=B6rg?=  =?utf-8?q?Doe?= <joerg@example.com>"
        );
        assert_parse!(
            "André Pirard",
            "PIRARD@vm1.ulg.ac.be",
            "=?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>"
        );
        // Custom example of RFC 2047 "B"-encoded ISO-8859-1 address.
        assert_parse!(
            "Jörg",
            "joerg@example.com",
            "=?ISO-8859-1?B?SvZyZw==?= <joerg@example.com>"
        );
        // Custom example of RFC 2047 "B"-encoded UTF-8 address.
        assert_parse!(
            "Jörg",
            "joerg@example.com",
            "=?UTF-8?B?SsO2cmc=?= <joerg@example.com>"
        );
        // Custom example with "." in name. For issue 4938
        //assert_parse!(
        //    "Asem H.",
        //    "noreply@example.com",
        //    "Asem H. <noreply@example.com>"
        //);
        assert_parse!(
            // RFC 6532 3.2.3, qtext /= UTF8-non-ascii
            "Gø Pher",
            "gopher@example.com",
            "\"Gø Pher\" <gopher@example.com>"
        );
        // RFC 6532 3.2, atext /= UTF8-non-ascii
        assert_parse!("µ", "micro@example.com", "µ <micro@example.com>");
        // RFC 6532 3.2.2, local address parts allow UTF-8
        //assert_parse!("Micro", "µ@example.com", "Micro <µ@example.com>");
        // RFC 6532 3.2.4, domains parts allow UTF-8
        //assert_parse!(
        //    "Micro",
        //    "micro@µ.example.com",
        //    "Micro <micro@µ.example.com>"
        //);
        // Issue 14866
        assert_parse!(
            "",
            "emptystring@example.com",
            "\"\" <emptystring@example.com>"
        );
        // CFWS
        assert_parse!(
            "",
            "cfws@example.com",
            "<cfws@example.com> (CFWS (cfws))  (another comment)"
        );
        //"<cfws@example.com> ()  (another comment), <cfws2@example.com> (another)"
        assert_parse!(
            "Kristoffer Brånemyr",
            "ztion@swipenet.se",
            "=?iso-8859-1?q?Kristoffer_Br=E5nemyr?= <ztion@swipenet.se>"
        );
        assert_parse!(
            "François Pons",
            "fpons@mandrakesoft.com",
            "=?iso-8859-1?q?Fran=E7ois?= Pons <fpons@mandrakesoft.com>"
        );
        assert_parse!(
            "هل تتكلم اللغة الإنجليزية /العربية؟",
            "do.you.speak@arabic.com",
            "=?utf-8?b?2YfZhCDYqtiq2YPZhNmFINin2YTZhNi62Kkg2KfZhNil2YbYrNmE2YrYstmK2Kk=?=\n \
             =?utf-8?b?IC/Yp9mE2LnYsdio2YrYqdif?= <do.you.speak@arabic.com>"
        );
        assert_parse!(
            "狂ったこの世で狂うなら気は確かだ。",
            "famous@quotes.ja",
            "=?utf-8?b?54uC44Gj44Gf44GT44Gu5LiW44Gn54uC44GG44Gq44KJ5rCX44Gv56K644GL44Gg?=\n \
             =?utf-8?b?44CC?= <famous@quotes.ja>"
        );
        assert_eq!(
            Address::new_group(
                "A Group".to_string(),
                vec![
                    make_address!("Ed Jones", "c@a.test"),
                    make_address!("", "joe@where.test"),
                    make_address!("John", "jdoe@one.test")
                ]
            ),
            address(b"A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;")
                .unwrap()
                .1
        );
        assert_eq!(
            Address::new_group("Undisclosed recipients".to_string(), vec![]),
            address(b"Undisclosed recipients:;").unwrap().1
        );
        assert_parse!(
            "狂ったこの世で狂うなら気は確かだ。",
            "famous@quotes.ja",
            "狂ったこの世で狂うなら気は確かだ。 <famous@quotes.ja>"
        );
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

    #[test]
    fn test_msg_id() {
        let s = "Message-ID: <1234@local.machine.example>\r\n";
        let (rest, (_header_name, value)) = headers::header(s.as_bytes()).unwrap();
        assert!(rest.is_empty());
        let a = msg_id(value).unwrap().1;
        assert_eq!(a.val(), b"<1234@local.machine.example>");
        let s = "Message-ID:              <testabcd.1234@silly.test>\r\n";
        let (rest, (_header_name, value)) = headers::header(s.as_bytes()).unwrap();
        assert!(rest.is_empty());
        let b = msg_id(value).unwrap().1;
        assert_eq!(b.val(), b"<testabcd.1234@silly.test>");
        let s = "References: <1234@local.machine.example>\r\n";
        let (rest, (_header_name, value)) = headers::header(s.as_bytes()).unwrap();
        assert!(rest.is_empty());
        assert_eq!(&msg_id_list(value).unwrap().1, &[a.clone()]);
        let s = "References: <1234@local.machine.example> <3456@example.net>\r\n";
        let (rest, (_header_name, value)) = headers::header(s.as_bytes()).unwrap();
        assert!(rest.is_empty());
        let s = b"<3456@example.net>";
        let c = msg_id(s).unwrap().1;
        assert_eq!(&msg_id_list(value).unwrap().1, &[a, c]);
    }

    #[rustfmt::skip]
    #[test]
    fn test_mailto_parse() {
        let value = "mailto:1001084@bugs.debian.org?In-Reply-To=%3C168435582154.540248.\
             1403466294276093439%40xxxxxxxxxxxxxx%3E&References=%3C163857393129.1083042.\
             11053317018847169002.reportbug%40xxxxxxxxxxxxxx%3E%0A%20%3C168435582154.540248.\
             1403466294276093439%40xxxxxxxxxxxxxx%3E&subject=Re%3A%20ITP%3A%20meli%20--%\
             20terminal%20mail%20client&body=On%20Wed%2C%2017%20May%202023%2022%3A37%3A01%20%\
             2B0200%20xxxxx%20zzzzzzzzzz%20%3Cdr%40xxxxxxxx%3E%20wrote%3A%0A%3E%200.7.2%\
             2B20230517%20draft%201%20needs%20embedding%208%20crates%20%286%20missing%2C%201%\
             20unwanted%2C%201%20ahead%29%3B%0A%3E%20runs%20and%20seems%20to%20work%20from%20a%\
             20brief%20test%20use.%0A%3E%20%0A%3E%20Main%20tasks%20are%20still%20to%20keep%\
             20package%20up-to-date%20with%20upstream%20releases%2C%20and%0A%3E%20to%20package%\
             20more%20of%20the%20crates%20currently%20embedded.%0A%3E%20%0A%3E%20Here%27s%20how%\
             20you%20can%20help%3A%0A%3E%20%0A%3E%20As%20user%20running%20Debian%2C%20you%20can%\
             20test%20this%20draft%20package%3A%20Either%20build%20it%0A%3E%20yourself%20from%\
             20source%20or%20tell%20%28by%20posting%20to%20this%20bugreport%29%20if%20you%0A%3E%\
             20prefer%20testing%20the%20binary%20packages%20I%20built%20-%20then%20I%20will%\
             20share%20those.%0A%3E%20%0A%3E%20As%20developer%20%28but%20no%20need%20to%20be%\
             20official%20member%20of%20Debian%21%29%2C%20you%20can%0A%3E%20join%20the%20Debian%\
             20Rust%20team%20and%20help%20package%20these%20missing%20crates%3A%0A%3E%20https%3A%\
             2F%2Fsalsa.debian.org%2Fdebian%2Fmeli%2F-%2Fblob%2Fdebian%2Flatest%2Fdebian%2FTODO%\
             0A%3E%20%0A%3E%20%0A%3E%20%20-%20JJJJJ%0A%3E%20%0A%3E%20--%20%0A%3E%20%20%2A%20JJJJJ%\
             20xxxxxxxxxx%20-%20idealist%20%26%20Internet-arkitekt%0A%3E%20%20%2A%20Tlf.%3A%20%\
             2B45%20%20%20Website%3A%20http%3A%2F%2Fxxxxxxxxxxx%2F%0A%3E%20%0A%3E%20%20%5Bx%5D%\
             20quote%20me%20freely%20%20%5B%20%5D%20ask%20before%20reusing%20%20%5B%20%5D%20keep%\
             20private";
        let (rest, mailto) = crate::parser::generic::mailto(value.as_bytes())
            .map_err(|err| match err {
                nom::Err::Failure(err) | nom::Err::Error(err) => {
                    format!(
                        "kind {:?} at: {:?}",
                        err,
                        String::from_utf8_lossy(err.input)
                    )
                }
                nom::Err::Incomplete(_) => "incomplete input".to_string(),
            })
            .unwrap();
        println!(
            "rest = {}, mailto = {:#?}",
            String::from_utf8_lossy(rest),
            mailto
        );

        assert_eq!(
            mailto,
            Mailto {
                address: vec![
                    Address::new(None, "1001084@bugs.debian.org".to_string())

                ],
                body: Some(
                    "On Wed, 17 May 2023 22:37:01 +0200 xxxxx zzzzzzzzzz <dr@xxxxxxxx> wrote:\n> 0.7.2+20230517 draft 1 needs embedding 8 crates (6 missing, 1 unwanted, 1 ahead);\n> runs and seems to work from a brief test use.\n> \n> Main tasks are still to keep package up-to-date with upstream releases, and\n> to package more of the crates currently embedded.\n> \n> Here's how you can help:\n> \n> As user running Debian, you can test this draft package: Either build it\n> yourself from source or tell (by posting to this bugreport) if you\n> prefer testing the binary packages I built - then I will share those.\n> \n> As developer (but no need to be official member of Debian!), you can\n> join the Debian Rust team and help package these missing crates:\n> https://salsa.debian.org/debian/meli/-/blob/debian/latest/debian/TODO\n> \n> \n>  - JJJJJ\n> \n> -- \n>  * JJJJJ xxxxxxxxxx - idealist & Internet-arkitekt\n>  * Tlf.: +45   Website: http://xxxxxxxxxxx/\n> \n>  [x] quote me freely  [ ] ask before reusing  [ ] keep private".to_string(),
                ),
                headers: {
                    let mut map = HeaderMap::new();
                    map.insert(HeaderName::TO, "1001084@bugs.debian.org".into());
                    map.insert(HeaderName::IN_REPLY_TO, "<168435582154.540248.1403466294276093439@xxxxxxxxxxxxxx>".into());
                    map.insert(HeaderName::REFERENCES, "<163857393129.1083042.11053317018847169002.reportbug@xxxxxxxxxxxxxx>\n <168435582154.540248.1403466294276093439@xxxxxxxxxxxxxx>".into());
                    map.insert(HeaderName::SUBJECT, "Re: ITP: meli -- terminal mail client".into());
                    map
                },

            });
    }
}
