/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

use melib::email::{Flag, UnixTimestamp};
use melib::parsec::*;
use melib::{
    backends::{FolderHash, MailBackend},
    email::EnvelopeHash,
    thread::{SortField, SortOrder},
    Result, StackVec,
};
use std::sync::{Arc, RwLock};

pub use query_parser::query;
use Query::*;

#[derive(Debug, PartialEq)]
pub enum Query {
    Before(UnixTimestamp),
    After(UnixTimestamp),
    Between(UnixTimestamp, UnixTimestamp),
    On(UnixTimestamp),
    /* * * * */
    From(String),
    To(String),
    Cc(String),
    Bcc(String),
    InReplyTo(String),
    References(String),
    AllAddresses(String),
    /* * * * */
    Body(String),
    Subject(String),
    AllText(String),
    /* * * * */
    Flag(Flag),
    And(Box<Query>, Box<Query>),
    Or(Box<Query>, Box<Query>),
    Not(Box<Query>),
}

pub mod query_parser {
    use super::Query::{self, *};
    use melib::parsec::*;

    fn subject<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("subject:")),
            whitespace_wrap(literal()),
        )
        .map(|term| Query::Subject(term))
    }

    fn from<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("from:")),
            whitespace_wrap(literal()),
        )
        .map(|term| Query::From(term))
    }

    fn or<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(match_literal_anycase("or"))
                .parse(input)
                .and_then(|(last_input, _)| query().parse(debug!(last_input)))
        }
    }

    fn not<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(either(
                match_literal_anycase("not"),
                match_literal_anycase("!"),
            ))
            .parse(input)
            .and_then(|(last_input, _)| query().parse(debug!(last_input)))
        }
    }

    fn and<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(match_literal_anycase("and"))
                .parse(input)
                .and_then(|(last_input, _)| query().parse(debug!(last_input)))
        }
    }

    fn literal<'a>() -> impl Parser<'a, String> {
        move |input| either(quoted_string(), string()).parse(input)
    }

    fn parentheses_query<'a>() -> impl Parser<'a, Query> {
        move |input| {
            delimited(
                whitespace_wrap(match_literal("(")),
                whitespace_wrap(query()),
                whitespace_wrap(match_literal(")")),
            )
            .parse(input)
        }
    }

    /// Parser from `String` to `Query`.
    ///
    /// # Invocation
    /// ```
    /// use ui::cache::query;
    /// use ui::cache::Query;
    /// use melib::parsec::Parser;
    ///
    /// let input = "test";
    /// let query = query().parse(input);
    /// assert_eq!(Ok(("", Query::AllText("test".to_string()))), query);
    /// ```
    pub fn query<'a>() -> impl Parser<'a, Query> {
        move |input| {
            let (rest, query_a): (&'a str, Query) = if let Ok(q) = parentheses_query().parse(input)
            {
                Ok(q)
            } else if let Ok(q) = subject().parse(input) {
                Ok(q)
            } else if let Ok(q) = from().parse(input) {
                Ok(q)
            } else if let Ok((rest, query_a)) = not().parse(input) {
                Ok((rest, Not(Box::new(query_a))))
            } else if let Ok((rest, query_a)) = {
                let result = literal().parse(input);
                if result.is_ok()
                    && result
                        .as_ref()
                        .map(|(_, s)| s != "and" && s != "or" && s != "not")
                        .unwrap_or(false)
                {
                    result.map(|(r, s)| (r, AllText(s)))
                } else {
                    Err("")
                }
            } {
                Ok((rest, query_a))
            } else {
                Err("")
            }?;
            if rest.is_empty() {
                return Ok((rest, query_a));
            }

            if let Ok((rest, query_b)) = and().parse(rest) {
                Ok((rest, And(Box::new(query_a), Box::new(query_b))))
            } else if let Ok((rest, query_b)) = or().parse(rest) {
                Ok((rest, Or(Box::new(query_a), Box::new(query_b))))
            } else if let Ok((rest, query_b)) = query().parse(rest) {
                Ok((rest, Or(Box::new(query_a), Box::new(query_b))))
            } else {
                Ok((rest, query_a))
            }
        }
    }

    #[test]
    fn test_query_parsing() {
        assert_eq!(
            Err("subject: test and"),
            query().parse_complete("subject: test and")
        );
        assert_eq!(
            Ok((
                "",
                And(
                    Box::new(Subject("test".to_string())),
                    Box::new(AllText("i".to_string()))
                )
            )),
            query().parse_complete("subject: test and i")
        );
        assert_eq!(
            Ok(("", AllText("test".to_string()))),
            query().parse_complete("test")
        );
        assert_eq!(
            Ok(("", Subject("test".to_string()))),
            query().parse_complete("subject: test")
        );
        assert_eq!(
            Ok((
                "",
                Or(
                    Box::new(Subject("wah ah ah".to_string())),
                    Box::new(And(
                        Box::new(From("Manos".to_string())),
                        Box::new(From("Sia".to_string()))
                    ))
                )
            )),
            query().parse_complete("subject: \"wah ah ah\" or (from: Manos and from: Sia)")
        );
        assert_eq!(
            Ok((
                "",
                Or(
                    Box::new(Subject("wah".to_string())),
                    Box::new(And(
                        Box::new(From("Manos".to_string())),
                        Box::new(Or(
                            Box::new(Subject("foo".to_string())),
                            Box::new(Subject("bar".to_string())),
                        ))
                    ))
                )
            )),
            query()
                .parse_complete("subject: wah or (from: Manos and (subject:foo or subject: bar))")
        );
        assert_eq!(
            Ok((
                "",
                And(
                    Box::new(From("Manos".to_string())),
                    Box::new(And(
                        Box::new(Or(
                            Box::new(Subject("foo".to_string())),
                            Box::new(Subject("bar".to_string()))
                        )),
                        Box::new(Or(
                            Box::new(From("woo".to_string())),
                            Box::new(From("my".to_string()))
                        ))
                    ))
                )
            )),
            query().parse_complete(
                "(from: Manos and (subject:foo or subject: bar) and (from:woo or from:my))"
            )
        );
    }
}
