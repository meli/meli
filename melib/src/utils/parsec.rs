/*
 * meli - melib crate.
 *
 * Copyright 2019 Manos Pitsidianakis
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

//! Parser combinators.

use std::borrow::Cow;

use crate::utils::datetime::{parse_timestamp_from_string, UnixTimestamp};

pub const CRLF: &[u8] = b"\r\n";
pub const LF: &[u8] = b"\n";
pub const NULL: &[u8] = b"\0";
pub const EMPTY: &[u8] = b"";

pub type Result<'a, Output> = std::result::Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> Result<'a, Output>;

    fn parse_complete(&self, input: &'a str) -> Result<'a, Output> {
        match self.parse(input) {
            r @ Ok(("", _)) => r,
            r @ Err(_) => r,
            Ok(_) => Err(input),
        }
    }

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> Result<Output>,
{
    fn parse(&self, input: &'a str) -> Result<'a, Output> {
        self(input)
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

pub fn map_res<'a, P, F, E, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> std::result::Result<B, E>,
{
    move |input| {
        parser.parse(input).and_then(|(next_input, result)| {
            map_fn(result).map_or_else(|_| Err(next_input), |res| Ok((next_input, res)))
        })
    }
}

pub fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn match_literal_anycase<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next.eq_ignore_ascii_case(expected) => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

pub fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

pub fn quoted_string<'a>() -> impl Parser<'a, String> {
    map(
        right(
            match_literal("\""),
            left(
                zero_or_more(pred(any_char, |c| *c != '"')),
                match_literal("\""),
            ),
        ),
        |chars| chars.into_iter().collect(),
    )
}

pub fn quoted_slice<'a>() -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        if input.is_empty() || !input.starts_with('"') {
            return Err(input);
        }

        let mut i = 1;
        while i < input.len() {
            if input[i..].starts_with('\"') && !input[i - 1..].starts_with('\\') {
                return Ok((&input[i + 1..], &input[1..i]));
            }
            i += 1;
        }

        Err(input)
    }
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> Result<'a, Output> {
        self.parser.parse(input)
    }
}

pub fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

pub fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

pub use whitespace_wrap as ws_eat;

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

pub fn pairmutation<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        if let ok @ Ok(_) = parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        }) {
            return ok;
        }
        parser2.parse(input).and_then(|(next_input, result1)| {
            parser1
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result2, result1)))
        })
    }
}

#[macro_export]
macro_rules! permutation {
    ($input:expr, $($field:tt, $t:ty, $parser:expr),*) => {{
        'perm: {
            struct PermStruct {
                $($field: Option<$t>),*
            }
            let mut results = PermStruct {
                $($field: None),*
            };
            let mut input = $input;
            let mut left = 0;
            $(_ = &$parser; left += 1;)*
            let mut count = 1;
            let mut finished = 0;
            loop {
                let mut any_success = false;
                $(if results.$field.is_none() {
                    if let Ok((rest, res)) = $parser.parse(input) {
                        if res.is_some() || count > left {
                            results.$field = Some(res);
                            finished += 1;
                            count = 1;
                            input = rest;
                        }
                        any_success = true;
                    }
                })*
                count += 1;

                if !any_success {
                    break 'perm Err(input);
                }
                if finished == left || count >= 2*left {
                    break;
                }

            }
            if finished != left {
                break 'perm Err(input);
            }
            let PermStruct {
                $($field),*
            } = results;
            Ok((input, ($($field.unwrap()),*)))
        }
    }}
}

pub fn prefix<'a, PN, P, R, RN>(pre: PN, parser: P) -> impl Parser<'a, R>
where
    PN: Parser<'a, RN>,
    P: Parser<'a, R>,
{
    move |input| {
        pre.parse(input)
            .and_then(|(last_input, _)| parser.parse(last_input))
    }
}

pub fn suffix<'a, PN, P, R, RN>(parser: P, suf: PN) -> impl Parser<'a, R>
where
    PN: Parser<'a, RN>,
    P: Parser<'a, R>,
{
    move |input| {
        parser
            .parse(input)
            .and_then(|(last_input, result)| suf.parse(last_input).map(|(rest, _)| (rest, result)))
    }
}

pub fn delimited<'a, PNL, PNR, LN, RN, P, R>(
    lparser: PNL,
    mid: P,
    rparser: PNR,
) -> impl Parser<'a, R>
where
    PNL: Parser<'a, LN>,
    PNR: Parser<'a, RN>,
    P: Parser<'a, R>,
{
    move |input| {
        lparser.parse(input).and_then(|(next_input, _)| {
            mid.parse(next_input).and_then(|(last_input, result)| {
                rparser.parse(last_input).map(|(rest, _)| (rest, result))
            })
        })
    }
}

pub fn any_char(input: &str) -> Result<char> {
    input
        .chars()
        .next()
        .map_or_else(|| Err(input), |next| Ok((&input[next.len_utf8()..], next)))
}

pub fn string<'a>() -> impl Parser<'a, String> {
    one_or_more(pred(any_char, |c| {
        c.is_alphanumeric() || (c.is_ascii_graphic() && !['"', '(', ')', ' '].contains(c))
    }))
    .map(|r| r.into_iter().collect())
}

pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

pub fn is_a<'a>(slice: &'static [u8]) -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut i = 0;
        for byte in input.as_bytes().iter() {
            if !slice.contains(byte) {
                break;
            }
            i += 1;
        }
        if i == 0 {
            return Err("");
        }
        let (b, a) = input.split_at(i);
        Ok((a, b))
    }
}

pub fn is_not<'a>(slice: &'static [u8]) -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut i = 0;
        for byte in input.as_bytes().iter() {
            if slice.contains(byte) {
                break;
            }
            i += 1;
        }
        if i == 0 {
            return Err("");
        }
        let (b, a) = input.split_at(i);
        Ok((a, b))
    }
}

/// Try alternative parsers in order until one succeeds.
///
/// ```rust
/// # use melib::utils::parsec::{Parser, quoted_slice, match_literal, alt, delimited, prefix};
/// #
/// let parser = |input| {
///     alt([
///         delimited(match_literal("{"), quoted_slice(), match_literal("}")),
///         delimited(match_literal("["), quoted_slice(), match_literal("]")),
///     ])
///     .parse(input)
/// };
///
/// let input1: &str = "{\"quoted\"}";
/// assert_eq!(Ok(("", "quoted")), parser.parse(input1));
/// let input2: &str = "[\"quoted\"]";
/// assert_eq!(Ok(("", "quoted")), parser.parse(input2));
/// ```
pub fn alt<'a, P, A, const N: usize>(parsers: [P; N]) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    move |input| {
        for parser in parsers.iter() {
            if let Ok(res) = parser.parse(input) {
                return Ok(res);
            }
        }
        Err(input)
    }
}

pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

pub fn opt<'a, P, A>(opt_parser: P) -> impl Parser<'a, Option<A>>
where
    P: Parser<'a, A>,
{
    move |input| match opt_parser.parse(input) {
        Ok((next_input, result)) => Ok((next_input, Some(result))),
        Err(_) => Ok((input, None)),
    }
}

pub fn peek<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    move |input| match parser.parse(input) {
        Ok((_, result)) => Ok((input, result)),
        e @ Err(_) => e,
    }
}

pub fn take_until<'a, A, P>(end: P) -> impl Parser<'a, &'a str>
where
    P: Parser<'a, A>,
{
    move |input: &'a str| {
        let mut offset = 0;
        while !input[offset..].is_empty() {
            if let Ok((rest, _)) = end.parse(&input[offset..]) {
                return Ok((
                    rest,
                    &input[..(offset + input[offset..].len() - rest.len())],
                ));
            }
            while offset != input.len() {
                offset += 1;
                if input.is_char_boundary(offset) {
                    break;
                }
            }
        }
        Ok((&input[offset..], input))
    }
}

pub fn separated_list0<'a, P, A, S, Sep>(
    parser: P,
    separator: S,
    terminated: bool,
) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
    S: Parser<'a, Sep>,
{
    move |mut input| {
        let mut result = Vec::new();
        let mut prev_sep_result = Ok(());
        let mut last_item_input = input;

        while let Ok((next_input, next_item)) = parser.parse(input) {
            prev_sep_result?;
            input = next_input;
            last_item_input = next_input;
            result.push(next_item);
            match separator.parse(input) {
                Ok((next_input, _)) => {
                    input = next_input;
                }
                Err(err) => {
                    prev_sep_result = Err(err);
                }
            }
        }

        if !terminated {
            input = last_item_input;
        }

        Ok((input, result))
    }
}

/// Take `count` bytes
pub fn take<'a>(count: usize) -> impl Parser<'a, &'a str> {
    move |i: &'a str| {
        if i.len() < count || !i.is_char_boundary(count) {
            Err("")
        } else {
            let (b, a) = i.split_at(count);
            Ok((a, b))
        }
    }
}

/// Take a literal
///
///```rust
///  # use std::str::FromStr;
///  # use melib::utils::parsec::{Parser, delimited, match_literal, map_res, is_a, take_literal};
/// let lit: &str = "{31}\r\nThere is no script by that name\r\n";
/// assert_eq!(
///     take_literal(delimited(
///         match_literal("{"),
///         map_res(is_a(b"0123456789"), |s| usize::from_str(s)),
///         match_literal("}\r\n"),
///     ))
///     .parse(lit),
///     Ok(("\r\n", "There is no script by that name",))
/// );
/// ```
pub fn take_literal<'a, P>(parser: P) -> impl Parser<'a, &'a str>
where
    P: Parser<'a, usize>,
{
    move |input: &'a str| {
        let (rest, length) = parser.parse(input)?;
        take(length).parse(rest)
    }
}

pub fn date<'a, T: Into<Cow<'static, str>>>(fmt: T) -> impl Parser<'a, UnixTimestamp> {
    let fmt = fmt.into();
    move |input: &'a str| match parse_timestamp_from_string(input, &fmt) {
        Ok((idx, t)) => Ok((&input[idx..], t)),
        Err(_) => Err(input),
    }
}

pub fn integer<'a>() -> impl Parser<'a, usize> {
    use std::str::FromStr;
    map_res(is_a(b"0123456789"), usize::from_str)
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn test_parsec() {
        #[derive(Debug, PartialEq)]
        enum JsonValue {
            String(String),
            Number(f64),
            Bool(bool),
            Null,
            Object(HashMap<String, JsonValue>),
            Array(Vec<JsonValue>),
        }

        fn parse_value<'a>() -> impl Parser<'a, JsonValue> {
            move |input| {
                either(
                    either(
                        either(
                            either(
                                either(
                                    map(parse_bool(), JsonValue::Bool),
                                    map(parse_null(), |()| JsonValue::Null),
                                ),
                                map(parse_array(), JsonValue::Array),
                            ),
                            map(parse_object(), JsonValue::Object),
                        ),
                        map(parse_number(), JsonValue::Number),
                    ),
                    map(quoted_string(), JsonValue::String),
                )
                .parse(input)
            }
        }

        fn parse_number<'a>() -> impl Parser<'a, f64> {
            move |input| {
                either(
                    map(match_literal("TRUE"), |()| 1.0),
                    map(match_literal("FALSe"), |()| 1.0),
                )
                .parse(input)
            }
        }

        fn parse_bool<'a>() -> impl Parser<'a, bool> {
            move |input| {
                ws_eat(either(
                    map(match_literal("true"), |()| true),
                    map(match_literal("false"), |()| false),
                ))
                .parse(input)
            }
        }

        fn parse_null<'a>() -> impl Parser<'a, ()> {
            move |input| ws_eat(match_literal("null")).parse(input)
        }

        fn parse_array<'a>() -> impl Parser<'a, Vec<JsonValue>> {
            move |input| {
                delimited(
                    ws_eat(match_literal("[")),
                    separated_list0(parse_value(), ws_eat(match_literal(",")), false),
                    ws_eat(match_literal("]")),
                )
                .parse(input)
            }
        }

        fn parse_object<'a>() -> impl Parser<'a, HashMap<String, JsonValue>> {
            move |input| {
                map(
                    delimited(
                        ws_eat(match_literal("{")),
                        separated_list0(
                            pair(
                                suffix(quoted_string(), ws_eat(match_literal(":"))),
                                parse_value(),
                            ),
                            ws_eat(match_literal(",")),
                            false,
                        ),
                        ws_eat(match_literal("}")),
                    ),
                    |vec: Vec<(String, JsonValue)>| vec.into_iter().collect(),
                )
                .parse(input)
            }
        }
        assert_eq!(
            Ok(("", JsonValue::String("a".to_string()))),
            parse_value().parse(r#""a""#)
        );
        assert_eq!(
            Ok(("", JsonValue::Bool(true))),
            parse_value().parse(r#"true"#)
        );
        assert_eq!(
            Ok(("", JsonValue::Object(HashMap::default()))),
            parse_value().parse(r#"{}"#)
        );
        println!("{:?}", parse_value().parse(r#"{"a":true}"#));
        println!("{:?}", parse_value().parse(r#"{"a":true,"b":false}"#));
        println!("{:?}", parse_value().parse(r#"{ "a" : true,"b":  false }"#));
        println!("{:?}", parse_value().parse(r#"{ "a" : true,"b":  false,}"#));
        println!("{:?}", parse_value().parse(r#"{"a":false,"b":false,}"#));
        // Line:0 Col:18 Error parsing object
        // { "a":1, "b"  :  2, }
        //                   ^Unexpected ','
    }
}
