//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use crate::{
    backends::prelude::ResultFuture,
    contacts::{
        backend::{ContactBackend, ContactBackendCapabilities},
        AddressBookName, Card,
    },
    error::{Error, ErrorKind, Result},
    text::Truncate as _,
};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct NotmuchContact {
    pub name: String,
    pub address: String,
    #[serde(rename = "name-addr")]
    pub name_addr: String,
}

pub fn parse_notmuch_contacts(input: &str) -> Result<Vec<Card>> {
    let mut cards = Vec::new();
    let abook = serde_json::from_str::<Vec<NotmuchContact>>(input)?;

    for c in abook.iter() {
        cards.push(
            Card::new()
                .set_title(c.name_addr.clone())
                .set_email(c.address.clone())
                .set_name(c.name.clone())
                .set_external_resource(true)
                .clone(),
        );
    }

    Ok(cards)
}

#[derive(Debug)]
pub struct NotmuchContacts {
    pub query: String,
}

impl ContactBackend for NotmuchContacts {
    fn capabilities(&mut self) -> ContactBackendCapabilities {
        ContactBackendCapabilities::default()
    }

    fn address_books(&mut self) -> ResultFuture<Vec<AddressBookName>> {
        let name = self.query.clone();
        Ok(Box::pin(async { Ok(vec![AddressBookName(name.into())]) }))
    }

    fn fetch_book(&mut self, address_book: &AddressBookName) -> ResultFuture<Vec<Card>> {
        if address_book.0.as_ref() != self.query {
            return Err(Error::new("").set_kind(ErrorKind::ValueError));
        }
        let query = &self.query;
        let cards = match std::process::Command::new("sh")
            .args([
                "-c",
                &format!("notmuch address --format=json --output=recipients {query}",),
            ])
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .output()
        {
            Ok(notmuch_addresses) => {
                if notmuch_addresses.status.success() {
                    match std::str::from_utf8(&notmuch_addresses.stdout) {
                        Ok(notmuch_address_out) => {
                            match parse_notmuch_contacts(notmuch_address_out) {
                                Ok(contacts) => contacts,
                                Err(err) => {
                                    return Err(Error::new(format!(
                                        "Unable to parse notmuch contact result into cards: {}",
                                        notmuch_address_out.trim_at_boundary(100),
                                    ))
                                    .set_source(Some(Box::new(err))));
                                }
                            }
                        }
                        Err(err) => {
                            return Err(Error::new(format!(
                                "Unable to read from notmuch address query: {query}",
                            ))
                            .set_source(Some(Box::new(err))));
                        }
                    }
                } else {
                    return Err(Error::new(format!(
                        "Error running notmuch address: {} stdout: {} stderr: {}",
                        notmuch_addresses.status,
                        String::from_utf8_lossy(&notmuch_addresses.stdout),
                        String::from_utf8_lossy(&notmuch_addresses.stderr)
                    ))
                    .set_kind(ErrorKind::External));
                }
            }
            Err(err) => {
                return Err(Error::new("Unable to run notmuch address command")
                    .set_kind(ErrorKind::External)
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
fn test_addressbook_notmuchcontact() {
    let cards = parse_notmuch_contacts(
            r#"[{"name": "Full Name", "address": "user@example.com", "name-addr": "Full Name <user@example.com>"},
            {"name": "Full2 Name", "address": "user2@example.com", "name-addr": "Full2 Name <user2@example.com>"}]"#
        ).unwrap();
    assert_eq!(cards[0].name(), "Full Name");
    assert_eq!(cards[0].title(), "Full Name <user@example.com>");
    assert_eq!(cards[0].email(), "user@example.com");
    assert_eq!(cards[1].name(), "Full2 Name");
    assert_eq!(cards[1].title(), "Full2 Name <user2@example.com>");
    assert_eq!(cards[1].email(), "user2@example.com");
}
