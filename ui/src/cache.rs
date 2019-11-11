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

use melib::email::UnixTimestamp;
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
    Flags(Vec<String>),
    And(Box<Query>, Box<Query>),
    Or(Box<Query>, Box<Query>),
    Not(Box<Query>),
}

pub mod query_parser {
    use super::*;

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

    fn to<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("to:")),
            whitespace_wrap(literal()),
        )
        .map(|term| Query::To(term))
    }

    fn cc<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("cc:")),
            whitespace_wrap(literal()),
        )
        .map(|term| Query::Cc(term))
    }

    fn bcc<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("bcc:")),
            whitespace_wrap(literal()),
        )
        .map(|term| Query::Bcc(term))
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

    fn flags<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(either(
                match_literal_anycase("flags:"),
                match_literal_anycase("is:"),
            ))
            .parse(input)
            .and_then(|(rest, _)| {
                map(one_or_more(pred(any_char, |c| *c != ' ')), |chars| {
                    chars.into_iter().collect::<String>()
                })
                .parse(rest)
            })
            .and_then(|(rest, flags_list)| {
                if let Ok(r) = flags_list
                    .split(",")
                    .map(|t| {
                        either(quoted_string(), string())
                            .parse_complete(t)
                            .map(|(_, r)| r)
                    })
                    .collect::<std::result::Result<Vec<String>, &str>>()
                    .map(|v| Flags(v))
                {
                    Ok((rest, r))
                } else {
                    Err(rest)
                }
            })
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
            let (rest, query_a): (&'a str, Query) = if let Ok(q) = parentheses_query()
                .parse(input)
                .or(from().parse(input))
                .or(to().parse(input))
                .or(cc().parse(input))
                .or(bcc().parse(input))
                .or(subject().parse(input))
                .or(flags().parse(input))
            {
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
        assert_eq!(
            Ok(("", Flags(vec!["test".to_string(), "testtest".to_string()]))),
            query().parse_complete("flags:test,testtest")
        );
    }
}

pub fn query_to_imap(q: &Query) -> String {
    fn rec(q: &Query, s: &mut String) {
        use crate::sqlite3::escape_double_quote;
        match q {
            Subject(t) => {
                s.push_str(" SUBJECT \"");
                s.extend(escape_double_quote(t).chars());
                s.push_str("\"");
            }
            From(t) => {
                s.push_str(" FROM \"");
                s.extend(escape_double_quote(t).chars());
                s.push_str("\"");
            }
            To(t) => {
                s.push_str(" TO \"");
                s.extend(escape_double_quote(t).chars());
                s.push_str("\"");
            }
            Cc(t) => {
                s.push_str(" CC \"");
                s.extend(escape_double_quote(t).chars());
                s.push_str("\"");
            }
            Bcc(t) => {
                s.push_str(" BCC \"");
                s.extend(escape_double_quote(t).chars());
                s.push_str("\"");
            }
            AllText(t) => {
                s.push_str(" TEXT \"");
                s.extend(escape_double_quote(t).chars());
                s.push_str("\"");
            }
            Flags(v) => {
                for f in v {
                    match f.as_str() {
                        "draft" => {
                            s.push_str(" DRAFT ");
                        }
                        "deleted" => {
                            s.push_str(" DELETED ");
                        }
                        "flagged" => {
                            s.push_str(" FLAGGED ");
                        }
                        "recent" => {
                            s.push_str(" RECENT ");
                        }
                        "seen" | "read" => {
                            s.push_str(" SEEN ");
                        }
                        "unseen" | "unread" => {
                            s.push_str(" UNSEEN ");
                        }
                        "answered" => {
                            s.push_str(" ANSWERED ");
                        }
                        "unanswered" => {
                            s.push_str(" UNANSWERED ");
                        }
                        keyword => {
                            s.push_str(" ");
                            s.extend(keyword.chars());
                            s.push_str(" ");
                        }
                    }
                }
            }
            And(q1, q2) => {
                rec(q1, s);
                s.push_str(" ");
                rec(q2, s);
            }
            Or(q1, q2) => {
                s.push_str(" OR ");
                rec(q1, s);
                s.push_str(" ");
                rec(q2, s);
            }
            Not(q) => {
                s.push_str(" NOT ");
                rec(q, s);
            }
            _ => {}
        }
    }
    let mut ret = String::new();
    rec(q, &mut ret);
    ret
}

pub fn imap_search(
    term: &str,
    (sort_field, sort_order): (SortField, SortOrder),
    folder_hash: FolderHash,
    backend: &Arc<RwLock<Box<dyn MailBackend>>>,
) -> Result<StackVec<EnvelopeHash>> {
    let query = query().parse(term)?.1;
    let backend_lck = backend.read().unwrap();

    let b = (*backend_lck).as_any();
    if let Some(imap_backend) = b.downcast_ref::<melib::backends::ImapType>() {
        imap_backend.search(query_to_imap(&query), folder_hash)
    } else {
        panic!("Could not downcast ImapType backend. BUG");
    }
}
