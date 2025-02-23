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

use std::{
    hash::{Hash, Hasher},
    ops::Deref,
    path::Path,
    process::Command,
    sync::Arc,
};

use indexmap::IndexMap;
use uuid::Uuid;

use crate::{
    text::Truncate,
    utils::{parsec::Parser, shellexpand::ShellExpandTrait},
};

mod card;
pub mod jscontact;
pub mod mutt;
pub mod notmuchcontact;
pub mod vcard;

pub use card::*;

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

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct AddressBookName(Arc<str>);

impl Deref for AddressBookName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for AddressBookName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(fmt)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AddressBook {
    pub name: AddressBookName,
    pub format: Arc<str>,
    pub read_only: bool,
    pub cards: IndexMap<CardId, Card>,
}

impl AddressBook {
    pub fn new(name: Arc<str>, format: Arc<str>, read_only: bool) -> Self {
        Self {
            name: AddressBookName(name),
            format,
            read_only,
            cards: IndexMap::default(),
        }
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

    pub fn search(&self, term: &str) -> Vec<Card> {
        self.cards
            .values()
            .filter(|c| c.email.contains(term) || c.name.contains(term))
            .cloned()
            .collect()
    }
}

impl Deref for AddressBook {
    type Target = IndexMap<CardId, Card>;

    fn deref(&self) -> &IndexMap<CardId, Card> {
        &self.cards
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Contacts {
    name: String,
    pub books: IndexMap<AddressBookName, AddressBook>,
}

impl Contacts {
    pub fn new(name: String) -> Self {
        Self {
            name,
            books: IndexMap::default(),
        }
    }

    pub fn with_account(s: &crate::conf::AccountSettings) -> Self {
        let mut ret = Self::new(s.name.clone());
        match s.mutt_alias_file() {
            Ok(None) => {}
            Ok(Some(mutt_alias_file)) => {
                match std::fs::read_to_string(Path::new(mutt_alias_file.as_ref()).expand())
                    .map_err(|err| err.to_string())
                    .and_then(|contents| {
                        contents
                            .lines()
                            .map(|line| mutt::parse_mutt_contact().parse(line).map(|(_, c)| c))
                            .collect::<Result<Vec<Card>, &str>>()
                            .map_err(|err| err.to_string())
                    }) {
                    Ok(cards) => {
                        let mut book = AddressBook::new(
                            mutt_alias_file.into(),
                            "mutt_alias_file".into(),
                            true,
                        );
                        for c in cards {
                            book.add_card(c);
                        }
                        ret.books.insert(book.name.clone(), book);
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
            Err(err) => {
                log::error!("Could not read mutt alias configuration value: {err}",);
            }
        }
        match s.vcard_folder() {
            Ok(None) => {}
            Ok(Some(vcard_path)) => {
                let expanded_path = Path::new(vcard_path.as_ref()).expand();
                match vcard::load_cards(&expanded_path) {
                    Ok(cards) => {
                        let mut book = AddressBook::new(vcard_path.into(), "vcard".into(), true);
                        for c in cards {
                            book.add_card(c);
                        }
                        ret.books.insert(book.name.clone(), book);
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
            Err(err) => {
                log::error!("Could not read vcard_folder value: {err}",);
            }
        }
        match s.notmuch_address_book_query() {
            Ok(None) => {}
            Ok(Some(notmuch_address_book_query)) => {
                match Command::new("sh")
                    .args([
                        "-c",
                        &format!(
                            "notmuch address --format=json --output=recipients \
                             {notmuch_address_book_query}",
                        ),
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
                                    match notmuchcontact::parse_notmuch_contacts(
                                        notmuch_address_out,
                                    ) {
                                        Ok(contacts) => {
                                            let mut book = AddressBook::new(
                                                notmuch_address_book_query.into(),
                                                "notmuch_address_book_query".into(),
                                                true,
                                            );
                                            for c in contacts {
                                                book.add_card(c);
                                            }
                                            ret.books.insert(book.name.clone(), book);
                                        }
                                        Err(err) => {
                                            log::warn!(
                                                "Unable to parse notmuch contact result into \
                                                 cards: {} {err}",
                                                notmuch_address_out.trim_at_boundary(100),
                                            );
                                        }
                                    }
                                }
                                Err(err) => {
                                    log::warn!(
                                        "Unable to read from notmuch address query: \
                                         {notmuch_address_book_query}: {err}",
                                    );
                                }
                            }
                        } else {
                            log::warn!(
                                "Error ({}) running notmuch address: {} {}",
                                notmuch_addresses.status,
                                String::from_utf8_lossy(&notmuch_addresses.stdout),
                                String::from_utf8_lossy(&notmuch_addresses.stderr)
                            );
                        }
                    }
                    Err(err) => log::warn!("Unable to run notmuch address command: {err}"),
                }
            }
            Err(err) => {
                log::error!("Could not read notmuch_address_book_query configuration value: {err}",);
            }
        }
        ret
    }
}
