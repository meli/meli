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
    datetime::{timestamp_to_string, IMAP_DATE},
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
        if !$s.is_empty() {
            $s.push(' ');
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
                    s.push_str("SUBJECT \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(From(t)) => {
                    s.push_str("FROM \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(To(t)) => {
                    s.push_str("TO \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(Cc(t)) => {
                    s.push_str("CC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(Bcc(t)) => {
                    s.push_str("BCC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(AllText(t)) => {
                    s.push_str("TEXT \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(Flags(v)) => {
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
                    space_pad!(s);
                    stack.push_front(Q(q2));
                    stack.push_front(Lit(' '));
                    stack.push_front(Q(q1));
                }
                Q(Or(q1, q2)) => {
                    space_pad!(s);
                    s.push_str("OR ");
                    stack.push_front(Q(q2));
                    stack.push_front(Lit(' '));
                    stack.push_front(Q(q1));
                }
                Q(Not(q)) => {
                    space_pad!(s);
                    s.push_str("NOT ");
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
                Q(AllAddresses(_)) => {
                    // From OR To OR Cc OR Bcc
                }
                Q(Body(t)) => {
                    space_pad!(s);
                    s.push_str(r#"BODY ""#);
                    s.extend(escape_double_quote(t).chars());
                    s.push('"');
                }
                Q(HasAttachment) => {
                    // ???
                }
            }
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
    }
}
