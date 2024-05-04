/*
 * meli - addressbook module
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

//! # Mutt contact formats

use std::collections::VecDeque;

use super::*;
use crate::utils::parsec::{is_not, map_res, match_literal_anycase, prefix, Parser};

//alias <nickname> [ <long name> ] <address>
// From mutt doc:
//
// ```text
// Since the name can consist of several whitespace-separated words, the
// last word is considered the address, and it can be optionally enclosed
// between angle brackets.
// For example: alias mumon My dear pupil Mumon foobar@example.com
// will be parsed in this way:
//
// alias mumon      My dear pupil Mumon foobar@example.com
//       ^          ^                   ^
//       nickname   long name           email address
// The nickname (or alias) will be used to select a corresponding long name
// and email address when specifying the To field of an outgoing message,
// e.g. when using the  function in the browser or index context.
// The long name is optional, so you can specify an alias command in this
// way:
//
// alias mumon      foobar@example.com
//       ^          ^
//       nickname   email address
// ```
pub fn parse_mutt_contact<'a>() -> impl Parser<'a, Card> {
    move |input| {
        map_res(
            prefix(match_literal_anycase("alias "), is_not(b"\r\n")),
            |l| {
                let mut tokens = l.split_whitespace().collect::<VecDeque<&str>>();

                let mut ret = Card::new();
                let title = tokens.pop_front().ok_or(l)?.to_string();
                let mut email = tokens.pop_back().ok_or(l)?.to_string();
                if email.starts_with('<') && email.ends_with('>') {
                    email.pop();
                    email.remove(0);
                }
                let mut name = tokens.into_iter().fold(String::new(), |mut acc, el| {
                    acc.push_str(el);
                    acc.push(' ');
                    acc
                });
                name.pop();
                if name.trim().is_empty() {
                    name.clone_from(&title);
                }
                ret.set_title(title).set_email(email).set_name(name);
                Ok::<Card, &'a str>(ret)
            },
        )
        .parse(input)
    }
}

#[test]
fn test_mutt_contacts() {
    let a = "alias mumon      My dear pupil Mumon foobar@example.com";
    let b = "alias mumon      foobar@example.com";
    let c = "alias <nickname> <long name> <address>";

    let (other, a_card) = parse_mutt_contact().parse(a).unwrap();
    assert!(other.is_empty());
    assert_eq!(a_card.name(), "My dear pupil Mumon");
    assert_eq!(a_card.title(), "mumon");
    assert_eq!(a_card.email(), "foobar@example.com");

    let (other, b_card) = parse_mutt_contact().parse(b).unwrap();
    assert!(other.is_empty());
    assert_eq!(b_card.name(), "mumon");
    assert_eq!(b_card.title(), "mumon");
    assert_eq!(b_card.email(), "foobar@example.com");

    let (other, c_card) = parse_mutt_contact().parse(c).unwrap();
    assert!(other.is_empty());
    assert_eq!(c_card.name(), "<long name>");
    assert_eq!(c_card.title(), "<nickname>");
    assert_eq!(c_card.email(), "address");
}
