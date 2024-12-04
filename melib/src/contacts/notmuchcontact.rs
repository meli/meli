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

use crate::{contacts::Card, error::Result};

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct NotMuchContact {
    pub name: String,
    pub address: String,
    #[serde(rename = "name-addr")]
    pub name_addr: String,
}

pub fn parse_notmuch_contacts(input: &str) -> Result<Vec<Card>> {
    let mut cards = Vec::new();
    let abook = serde_json::from_str::<Vec<NotMuchContact>>(input)?;

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
