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

pub mod jscontact;
pub mod mutt;
pub mod notmuchcontact;
pub mod vcard;

mod card;
use std::{
    hash::{Hash, Hasher},
    ops::Deref,
    path::Path,
};

pub use card::*;
use indexmap::IndexMap;
use uuid::Uuid;

use crate::utils::{
    datetime::{now, UnixTimestamp},
    parsec::Parser,
    shellexpand::ShellExpandTrait,
};

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(from = "String")]
#[serde(into = "String")]
pub enum CardId {
    Uuid(Uuid),
    Hash(u64),
}

impl std::fmt::Display for CardId {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Uuid(u) => u.as_hyphenated().fmt(fmt),
            Self::Hash(u) => u.fmt(fmt),
        }
    }
}

impl From<CardId> for String {
    fn from(val: CardId) -> Self {
        val.to_string()
    }
}

impl From<String> for CardId {
    fn from(s: String) -> Self {
        use std::{collections::hash_map::DefaultHasher, str::FromStr};

        if let Ok(u) = Uuid::try_parse(s.as_str()) {
            Self::Uuid(u)
        } else if let Ok(num) = u64::from_str(s.trim()) {
            Self::Hash(num)
        } else {
            let mut hasher = DefaultHasher::default();
            s.hash(&mut hasher);
            Self::Hash(hasher.finish())
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Contacts {
    display_name: String,
    created: UnixTimestamp,
    last_edited: UnixTimestamp,
    pub cards: IndexMap<CardId, Card>,
}

impl Contacts {
    pub fn new(display_name: String) -> Self {
        Self {
            display_name,
            created: now(),
            last_edited: now(),
            cards: IndexMap::default(),
        }
    }

    pub fn with_account(s: &crate::conf::AccountSettings) -> Self {
        let mut ret = Self::new(s.name.clone());
        if let Some(mutt_alias_file) = s.extra.get("mutt_alias_file") {
            match std::fs::read_to_string(Path::new(mutt_alias_file).expand())
                .map_err(|err| err.to_string())
                .and_then(|contents| {
                    contents
                        .lines()
                        .map(|line| mutt::parse_mutt_contact().parse(line).map(|(_, c)| c))
                        .collect::<Result<Vec<Card>, &str>>()
                        .map_err(|err| err.to_string())
                }) {
                Ok(cards) => {
                    for c in cards {
                        ret.add_card(c);
                    }
                }
                Err(err) => {
                    log::warn!(
                        "Could not load mutt alias file {:?}: {}",
                        mutt_alias_file,
                        err
                    );
                }
            }
        }
        if let Some(vcard_path) = s.vcard_folder() {
            let expanded_path = Path::new(vcard_path).expand();
            match vcard::load_cards(&expanded_path) {
                Ok(cards) => {
                    for c in cards {
                        ret.add_card(c);
                    }
                }
                Err(err) => {
                    log::warn!("Could not load vcards from {:?}: {}", vcard_path, err);
                    if expanded_path.display().to_string() != vcard_path {
                        log::warn!(
                            "Note: vcard_folder was expanded from {} to {}",
                            vcard_path,
                            expanded_path.display()
                        );
                    }
                }
            }
        }
        use std::process::Command;
        if let Some(notmuch_addressbook_query) = s.notmuch_address_book_query() {
            let notmuch_addresses = Command::new("notmuch")
                .arg("address")
                .args(notmuch_addressbook_query.split_whitespace())
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .output()
                .unwrap();

            if notmuch_addresses.status.success() {
                for card in notmuchcontact::parse_notmuch_contacts(
                    std::str::from_utf8(&notmuch_addresses.stdout)
                        .expect("utf8 conversion failure"),
                )
                .expect("notmuch addr book parsing failed")
                .iter()
                {
                    ret.add_card(card.clone());
                }
            }
        }
        ret
    }

    pub fn add_card(&mut self, card: Card) {
        self.cards.insert(card.id, card);
    }

    pub fn remove_card(&mut self, card_id: CardId) {
        self.cards.shift_remove(&card_id);
    }

    pub fn card_exists(&self, card_id: CardId) -> bool {
        self.cards.contains_key(&card_id)
    }

    pub fn search(&self, term: &str) -> Vec<String> {
        self.cards
            .values()
            .filter(|c| c.email.contains(term) || c.name.contains(term))
            .map(|c| {
                crate::email::Address::new(
                    if c.name.is_empty() {
                        None
                    } else {
                        Some(c.name.clone())
                    },
                    c.email.clone(),
                )
                .to_string()
            })
            .collect()
    }
}

impl Deref for Contacts {
    type Target = IndexMap<CardId, Card>;

    fn deref(&self) -> &IndexMap<CardId, Card> {
        &self.cards
    }
}
