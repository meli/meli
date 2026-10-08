/*
 * meli - contacts module
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

use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
};

use crate::{
    backends::prelude::ResultFuture,
    contacts::{
        backend::{ContactBackend, ContactBackendCapabilities},
        AddressBookName, Card,
    },
    error::{Error, ErrorKind},
    utils::parsec::{is_not, map_res, match_literal_anycase, prefix, Parser},
    ShellExpandTrait as _,
};

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

#[derive(Debug)]
pub struct MuttContacts {
    pub path: PathBuf,
}

impl ContactBackend for MuttContacts {
    fn capabilities(&mut self) -> ContactBackendCapabilities {
        ContactBackendCapabilities::default()
    }

    fn address_books(&mut self) -> ResultFuture<Vec<AddressBookName>> {
        Ok(Box::pin(async {
            Ok(vec![AddressBookName("mutt_alias_file".into())])
        }))
    }

    fn fetch_book(&mut self, address_book: &AddressBookName) -> ResultFuture<Vec<Card>> {
        if address_book.0.as_ref() != "mutt_alias_file" {
            return Err(Error::new("").set_kind(ErrorKind::ValueError));
        }
        let mutt_alias_file = &self.path;
        let cards = match std::fs::read_to_string(Path::new(mutt_alias_file).expand())
            .map_err(|err| Error::from(err).set_related_path(Some(mutt_alias_file)))
            .and_then(|contents| {
                Ok(contents
                    .lines()
                    .map(|line| parse_mutt_contact().parse(line).map(|(_, c)| c))
                    .collect::<std::result::Result<Vec<Card>, &str>>()
                    .map_err(|err| format!("Could not parse file: {err}"))?)
            }) {
            Ok(cards) => cards,
            Err(err) => {
                return Err(Error::new(format!(
                    "Could not load mutt alias file {mutt_alias_file:?}"
                ))
                .set_source(Some(Box::new(err))));
            }
        };

        Ok(Box::pin(async { Ok(cards) }))
    }

    fn search(
        &self,
        _term: &str,
        _address_book: Option<&AddressBookName>,
    ) -> ResultFuture<Vec<Card>> {
        Err(Error::new("").set_kind(ErrorKind::NotSupported))
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
