/*
 * meli - melib
 *
 * Copyright 2019-2020 Manos Pitsidianakis
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

use std::{borrow::Cow, convert::TryFrom};

pub use query_parser::query;
use Query::*;

use crate::{
    email::headers::HeaderName,
    utils::{
        datetime::{formats, UnixTimestamp},
        parsec::*,
    },
};

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Query {
    Before(UnixTimestamp),
    After(UnixTimestamp),
    Between(UnixTimestamp, UnixTimestamp),
    On(UnixTimestamp),
    Header(HeaderName, String),
    From(String),
    To(String),
    Cc(String),
    Bcc(String),
    InReplyTo(String),
    References(String),
    AllAddresses(String),
    Body(String),
    Subject(String),
    AllText(String),
    Flags(Vec<String>),
    HasAttachment,
    And(Box<Query>, Box<Query>),
    Or(Box<Query>, Box<Query>),
    Not(Box<Query>),
    /// By us.
    Answered,
    /// By an address/name.
    AnsweredBy {
        by: String,
    },
    Larger {
        than: usize,
    },
    Smaller {
        than: usize,
    },
}

pub trait QueryTrait {
    fn is_match(&self, query: &Query) -> bool;
}

impl QueryTrait for crate::Envelope {
    fn is_match(&self, query: &Query) -> bool {
        use Query::*;
        match query {
            Before(timestamp) => self.date() < *timestamp,
            After(timestamp) => self.date() > *timestamp,
            Between(timestamp_a, timestamp_b) => {
                self.date() > *timestamp_a && self.date() < *timestamp_b
            }
            On(timestamp) => {
                self.date() > timestamp.saturating_sub(60 * 60 * 24)
                    && self.date() < *timestamp + 60 * 60 * 24
            }
            Header(name, needle) => self
                .other_headers()
                .get(name)
                .map(|h| h.contains(needle.as_str()))
                .unwrap_or(false),
            From(s) => self.other_headers()[HeaderName::FROM].contains(s),
            To(s) => self.other_headers()[HeaderName::TO].contains(s),
            Cc(s) => self.other_headers()[HeaderName::CC].contains(s),
            Bcc(s) => self.other_headers()[HeaderName::BCC].contains(s),
            AllAddresses(s) => {
                self.is_match(&From(s.clone()))
                    || self.is_match(&To(s.clone()))
                    || self.is_match(&Cc(s.clone()))
                    || self.is_match(&Bcc(s.clone()))
            }
            Flags(v) => v.iter().any(|s| self.flags() == s.as_str()),
            Subject(s) => self.other_headers()[HeaderName::SUBJECT].contains(s),
            HasAttachment => self.has_attachments(),
            And(q_a, q_b) => self.is_match(q_a) && self.is_match(q_b),
            Or(q_a, q_b) => self.is_match(q_a) || self.is_match(q_b),
            Not(q) => !self.is_match(q),
            InReplyTo(s) => <Self as QueryTrait>::is_match(
                self,
                &Query::Header(HeaderName::IN_REPLY_TO, s.to_string()),
            ),
            References(s) => <Self as QueryTrait>::is_match(
                self,
                &Query::Header(HeaderName::REFERENCES, s.to_string()),
            ),
            AllText(_) => {
                log::warn!("Filtering with AllText is unimplemented.");
                false
            }
            Body(_) => {
                log::warn!("Filtering with Body is unimplemented.");
                false
            }
            Answered => {
                log::warn!("Filtering with Answered is unimplemented.");
                false
            }
            AnsweredBy { .. } => {
                log::warn!("Filtering with AnsweredBy is unimplemented.");
                false
            }
            Larger { .. } => {
                log::warn!("Filtering with Larger is unimplemented.");
                false
            }
            Smaller { .. } => {
                log::warn!("Filtering with Smaller is unimplemented.");
                false
            }
        }
    }
}

impl TryFrom<&str> for Query {
    type Error = crate::error::Error;
    fn try_from(t: &str) -> crate::error::Result<Self> {
        query()
            .parse_complete(t)
            .map(|(_, q)| q)
            .map_err(|err| err.into())
    }
}

pub mod query_parser {
    use super::*;

    fn date<'a>() -> impl Parser<'a, UnixTimestamp> {
        move |input| {
            literal().parse(input).and_then(|(next_input, result)| {
                if let Ok((_, t)) = crate::utils::datetime::parse_timestamp_from_string(
                    result,
                    formats::RFC3339_DATE,
                ) {
                    Ok((next_input, t))
                } else {
                    Err(next_input)
                }
            })
        }
    }

    fn before<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("before:")),
            whitespace_wrap(date()),
        )
        .map(Query::Before)
    }

    fn after<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("after:")),
            whitespace_wrap(date()),
        )
        .map(Query::After)
    }

    fn between<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("between:")),
            pair(
                suffix(whitespace_wrap(date()), whitespace_wrap(match_literal(","))),
                whitespace_wrap(date()),
            ),
        )
        .map(|(t1, t2)| Query::Between(t1, t2))
    }

    fn on<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("on:")),
            whitespace_wrap(date()),
        )
        .map(Query::After)
    }

    fn smaller<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("smaller:")),
            whitespace_wrap(integer()),
        )
        .map(|than| Query::Smaller { than })
    }

    fn larger<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("larger:")),
            whitespace_wrap(integer()),
        )
        .map(|than| Query::Larger { than })
    }

    fn answered_by<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("answered-by:")),
            whitespace_wrap(literal()),
        )
        .map(|by| Query::AnsweredBy { by })
    }

    fn answered<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(match_literal_anycase("answered"))
                .map(|()| Query::Answered)
                .parse(input)
        }
    }

    fn subject<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("subject:")),
            whitespace_wrap(literal()),
        )
        .map(Query::Subject)
    }

    fn from<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("from:")),
            whitespace_wrap(literal()),
        )
        .map(Query::From)
    }

    fn to<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("to:")),
            whitespace_wrap(literal()),
        )
        .map(Query::To)
    }

    fn cc<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("cc:")),
            whitespace_wrap(literal()),
        )
        .map(Query::Cc)
    }

    fn bcc<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("bcc:")),
            whitespace_wrap(literal()),
        )
        .map(Query::Bcc)
    }

    fn message_id<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(either(
                match_literal("message-id:"),
                match_literal("msg-id:"),
            )),
            whitespace_wrap(literal()),
        )
        .map(|s| Query::Header(HeaderName::MESSAGE_ID, s))
    }

    fn in_reply_to<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("in-reply-to:")),
            whitespace_wrap(literal()),
        )
        .map(|s| Query::Header(HeaderName::IN_REPLY_TO, s))
    }

    fn references<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("references:")),
            whitespace_wrap(literal()),
        )
        .map(|s| Query::Header(HeaderName::REFERENCES, s))
    }

    fn header<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("header:")),
            pair(
                suffix(
                    whitespace_wrap(move |input| {
                        is_not(b",").parse(input).and_then(|(last_input, _)| {
                            {
                                |s| {
                                    <HeaderName as std::str::FromStr>::from_str(s)
                                        .map(|res| ("", res))
                                        .map_err(|_| s)
                                }
                            }
                            .parse(last_input)
                        })
                    }),
                    whitespace_wrap(match_literal(",")),
                ),
                whitespace_wrap(literal()),
            ),
        )
        .map(|(t1, t2)| Query::Header(t1, t2))
    }

    fn all_addresses<'a>() -> impl Parser<'a, Query> {
        prefix(
            whitespace_wrap(match_literal("all-addresses:")),
            whitespace_wrap(literal()),
        )
        .map(Query::AllAddresses)
    }

    fn or<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(match_literal_anycase("or"))
                .parse(input)
                .and_then(|(last_input, _)| query().parse(last_input))
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

    fn has_attachment<'a>() -> impl Parser<'a, Query> {
        move |input| {
            whitespace_wrap(either(
                match_literal_anycase("has:attachment"),
                match_literal_anycase("has:attachments"),
            ))
            .map(|()| Query::HasAttachment)
            .parse(input)
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
                match_literal_anycase("is:"),
                either(
                    either(
                        match_literal_anycase("flag:"),
                        match_literal_anycase("flags:"),
                    ),
                    either(
                        match_literal_anycase("tag:"),
                        match_literal_anycase("tags:"),
                    ),
                ),
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
                    .split(',')
                    .map(|t| {
                        either(quoted_string(), string())
                            .parse_complete(t)
                            .map(|(_, r)| r)
                    })
                    .collect::<std::result::Result<Vec<String>, &str>>()
                    .map(Flags)
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
    /// use melib::{
    ///     search::{query, Query},
    ///     utils::parsec::Parser,
    /// };
    ///
    /// let input = "test";
    /// let query = query().parse(input);
    /// assert_eq!(Ok(("", Query::Body("test".to_string()))), query);
    /// ```
    pub fn query<'a>() -> impl Parser<'a, Query> {
        move |input| {
            let (rest, query_a): (&'a str, Query) = if let Ok(q) = parentheses_query()
                .parse(input)
                .or_else(|_| from().parse(input))
                .or_else(|_| to().parse(input))
                .or_else(|_| cc().parse(input))
                .or_else(|_| bcc().parse(input))
                .or_else(|_| message_id().parse(input))
                .or_else(|_| in_reply_to().parse(input))
                .or_else(|_| references().parse(input))
                .or_else(|_| header().parse(input))
                .or_else(|_| all_addresses().parse(input))
                .or_else(|_| subject().parse(input))
                .or_else(|_| before().parse(input))
                .or_else(|_| after().parse(input))
                .or_else(|_| on().parse(input))
                .or_else(|_| between().parse(input))
                .or_else(|_| flags().parse(input))
                .or_else(|_| answered().parse(input))
                .or_else(|_| answered_by().parse(input))
                .or_else(|_| larger().parse(input))
                .or_else(|_| smaller().parse(input))
                .or_else(|_| has_attachment().parse(input))
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
                    result.map(|(r, s)| (r, Body(s)))
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
                Ok((rest, And(Box::new(query_a), Box::new(query_b))))
            } else {
                Ok((rest, query_a))
            }
        }
    }
}

#[inline(always)]
pub fn escape_double_quote(w: &str) -> Cow<str> {
    if w.contains('"') {
        Cow::from(w.replace('"', "\"\""))
    } else {
        Cow::from(w)
    }
}

use serde::{de, Deserialize, Deserializer};
impl<'de> Deserialize<'de> for Query {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        let ret = query()
            .parse(&s)
            .map(|(_, q)| q)
            .map_err(|err| de::Error::custom(format!("invalid query value: {err}")));
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_parsing() {
        assert_eq!(
            Err("subject:test and"),
            query().parse_complete("subject:test and")
        );
        assert_eq!(
            Ok((
                "",
                And(
                    Box::new(Subject("test".to_string())),
                    Box::new(Body("i".to_string()))
                )
            )),
            query().parse_complete("subject:test and i")
        );
        assert_eq!(
            Ok(("", Body("test".to_string()))),
            query().parse_complete("test")
        );
        assert_eq!(
            Ok(("", Subject("test".to_string()))),
            query().parse_complete("subject:test")
        );
        assert_eq!(
            Ok((
                "",
                And(
                    Box::new(From("Manos".to_string())),
                    Box::new(From("Sia".to_string()))
                )
            )),
            query().parse_complete("from:Manos and from:Sia")
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
            query().parse_complete("subject:\"wah ah ah\" or (from:Manos and from:Sia)")
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
            query().parse_complete("subject:wah or (from:Manos and (subject:foo or subject:bar))")
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
                "(from:Manos and (subject:foo or subject:bar) and (from:woo or from:my))"
            )
        );
        assert_eq!(
            Ok((
                "",
                Or(
                    Box::new(Header(
                        HeaderName::MESSAGE_ID,
                        "123_user@example.org".to_string()
                    )),
                    Box::new(And(
                        Box::new(Header(
                            HeaderName::MESSAGE_ID,
                            "1234_user@example.org".to_string()
                        )),
                        Box::new(Body("header:List-ID,meli-devel".to_string()))
                    ))
                )
            )),
            query().parse_complete(
                "(message-id:123_user@example.org or (msg-id:1234_user@example.org) and \
                 (header:List-ID,meli-devel))"
            )
        );
        assert_eq!(
            Ok(("", Flags(vec!["test".to_string(), "testtest".to_string()]))),
            query().parse_complete("flags:test,testtest")
        );
        assert_eq!(
            query().parse_complete("flags:test,testtest"),
            query().parse_complete("tags:test,testtest")
        );
        assert_eq!(
            query().parse_complete("flags:seen"),
            query().parse_complete("tags:seen")
        );
        assert_eq!(
            query().parse_complete("is:unseen"),
            query().parse_complete("tags:unseen")
        );
        assert_eq!(
            Ok(("", Flags(vec!["f".to_string()]))),
            query().parse_complete("tags:f")
        );
    }
}
