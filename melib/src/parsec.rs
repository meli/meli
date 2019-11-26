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

pub fn prefix<'a, PN, P, R, RN>(pre: PN, parser: P) -> impl Parser<'a, R>
where
    PN: Parser<'a, RN>,
    P: Parser<'a, R>,
{
    move |input| {
        pre.parse(input).and_then(|(last_input, _)| {
            parser
                .parse(last_input)
                .map(|(rest, result)| (rest, result))
        })
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

pub fn delimited<'a, PN, RN, P, R>(lparser: PN, mid: P, rparser: PN) -> impl Parser<'a, R>
where
    PN: Parser<'a, RN>,
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
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

pub fn string<'a>() -> impl Parser<'a, String> {
    one_or_more(pred(any_char, |c| c.is_alphanumeric())).map(|r| r.into_iter().collect())
}

pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
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
