/*
 * meli - imap module.
 *
 * Copyright 2017 - 2023 Manos Pitsidianakis
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

//! Convert [`crate::search::Query`] into IMAP search criteria.

use std::collections::VecDeque;

use crate::{
    datetime::{formats::IMAP_DATE, timestamp_to_string},
    search::*,
};

mod private {
    pub trait Sealed {}
}

pub trait ToImapSearch: private::Sealed {
    /// Convert [`crate::search::Query`] into IMAP search criteria.
    fn to_imap_search(&self) -> String;
}

impl private::Sealed for Query {}

macro_rules! space_pad {
    ($s:ident) => {{
        if !$s.is_empty() && !$s.ends_with('(') && !$s.ends_with(' ') {
            $s.push(' ');
            false
        } else {
            true
        }
    }};
}

impl ToImapSearch for Query {
    fn to_imap_search(&self) -> String {
        enum Step<'a> {
            Q(&'a Query),
            Lit(char),
        }
        use Step::*;

        let mut stack = VecDeque::new();
        stack.push_front(Q(self));
        let mut s = String::new();
        while let Some(q) = stack.pop_front() {
            use Query::*;
            match q {
                Lit(lit) => {
                    s.push(lit);
                }
                Q(Subject(t)) => {
                    space_pad!(s);
                    s.push_str("SUBJECT \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(From(t)) => {
                    space_pad!(s);
                    s.push_str("FROM \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(To(t)) => {
                    space_pad!(s);
                    s.push_str("TO \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(Cc(t)) => {
                    space_pad!(s);
                    s.push_str("CC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(Bcc(t)) => {
                    space_pad!(s);
                    s.push_str("BCC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(AllText(t)) => {
                    space_pad!(s);
                    s.push_str("TEXT \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(Flags(v)) => {
                    space_pad!(s);
                    for f in v {
                        match f.as_str() {
                            "draft" => {
                                s.push_str("DRAFT ");
                            }
                            "deleted" => {
                                s.push_str("DELETED ");
                            }
                            "flagged" => {
                                s.push_str("FLAGGED ");
                            }
                            "recent" => {
                                s.push_str("RECENT ");
                            }
                            "seen" | "read" => {
                                s.push_str("SEEN ");
                            }
                            "unseen" | "unread" => {
                                s.push_str("UNSEEN ");
                            }
                            "answered" => {
                                s.push_str("ANSWERED ");
                            }
                            "unanswered" => {
                                s.push_str("UNANSWERED ");
                            }
                            keyword => {
                                s.push_str("KEYWORD ");
                                s.push_str(keyword);
                                s.push(' ');
                            }
                        }
                    }
                }
                Q(And(q1, q2)) => {
                    let is_empty = space_pad!(s);
                    if !is_empty {
                        stack.push_front(Lit(')'));
                    }
                    stack.push_front(Q(q2));
                    stack.push_front(Q(q1));
                    if !is_empty {
                        stack.push_front(Lit('('));
                    }
                }
                Q(Or(q1, q2)) => {
                    space_pad!(s);
                    s.push_str("OR");
                    stack.push_front(Q(q2));
                    stack.push_front(Q(q1));
                }
                Q(Not(q)) => {
                    space_pad!(s);
                    s.push_str("NOT (");
                    stack.push_front(Lit(')'));
                    stack.push_front(Q(q));
                }
                Q(Before(t)) => {
                    space_pad!(s);
                    s.push_str("BEFORE ");
                    s.push_str(&timestamp_to_string(*t, Some(IMAP_DATE), true));
                }
                Q(After(t)) => {
                    space_pad!(s);
                    s.push_str("SINCE ");
                    s.push_str(&timestamp_to_string(*t, Some(IMAP_DATE), true));
                }
                Q(Between(t1, t2)) => {
                    space_pad!(s);
                    s.push_str("(SINCE ");
                    s.push_str(&timestamp_to_string(*t1, Some(IMAP_DATE), true));
                    s.push_str(" BEFORE ");
                    s.push_str(&timestamp_to_string(*t2, Some(IMAP_DATE), true));
                    s.push(')');
                }
                Q(On(t)) => {
                    space_pad!(s);
                    s.push_str("ON ");
                    s.push_str(&timestamp_to_string(*t, Some(IMAP_DATE), true));
                }
                Q(InReplyTo(t)) => {
                    space_pad!(s);
                    s.push_str(r#"HEADER "In-Reply-To" ""#);
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(References(t)) => {
                    space_pad!(s);
                    s.push_str(r#"HEADER "References" ""#);
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(AllAddresses(t)) => {
                    let is_empty = space_pad!(s);
                    if !is_empty {
                        s.push('(');
                    }
                    s.push_str("OR FROM \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\" (OR TO \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\" (OR CC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\" BCC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str(r#""))"#);
                    if !is_empty {
                        s.push(')');
                    }
                }
                Q(Body(t)) => {
                    space_pad!(s);
                    s.push_str(r#"BODY ""#);
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(HasAttachment) => {
                    log::warn!("HasAttachment in IMAP is unimplemented.");
                }
                Q(Answered) => {
                    space_pad!(s);
                    s.push_str(r#"ANSWERED ""#);
                }
                Q(AnsweredBy { by }) => {
                    space_pad!(s);
                    s.push_str(r#"HEADER "From" ""#);
                    s.extend(escape_double_quote(by).chars());
                    s.push('"');
                }
                Q(Larger { than }) => {
                    space_pad!(s);
                    s.push_str("LARGER ");
                    s.push_str(&than.to_string());
                }
                Q(Smaller { than }) => {
                    space_pad!(s);
                    s.push_str("SMALLER ");
                    s.push_str(&than.to_string());
                }
            }
        }
        while s.ends_with(' ') {
            s.pop();
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsec::Parser;

    #[test]
    fn test_imap_query_search() {
        let (_, q) = query().parse_complete("subject: test and i").unwrap();
        assert_eq!(&q.to_imap_search(), r#"SUBJECT "test" TEXT "i""#);

        let (_, q) = query().parse_complete("is:unseen").unwrap();
        assert_eq!(&q.to_imap_search(), r#"UNSEEN"#);

        let (_, q) = query().parse_complete("from:user@example.org").unwrap();
        assert_eq!(&q.to_imap_search(), r#"FROM "user@example.org""#);

        let (_, q) = query()
            .parse_complete(
                "from:user@example.org and subject:
            \"foobar space\"",
            )
            .unwrap();
        assert_eq!(
            &q.to_imap_search(),
            r#"FROM "user@example.org" SUBJECT "foobar space""#
        );

        assert_eq!(
            &timestamp_to_string(1685739600, Some(IMAP_DATE), true),
            "03-Jun-2023"
        );

        let (_, q) = query()
            .parse_complete("before:2023-06-04 from:user@example.org")
            .unwrap();
        assert_eq!(
            &q.to_imap_search(),
            r#"BEFORE 04-Jun-2023 FROM "user@example.org""#
        );
        let (_, q) = query()
            .parse_complete(r#"subject:"wah ah ah" or (from:Manos and from:Sia)"#)
            .unwrap();
        assert_eq!(
            &q.to_imap_search(),
            r#"OR SUBJECT "wah ah ah" (FROM "Manos" FROM "Sia")"#
        );

        let (_, q) = query()
            .parse_complete(r#"subject:wo or (all-addresses:Manos)"#)
            .unwrap();
        assert_eq!(
            &q.to_imap_search(),
            r#"OR SUBJECT "wo" (OR FROM "Manos" (OR TO "Manos" (OR CC "Manos" BCC "Manos")))"#
        );
    }
}
