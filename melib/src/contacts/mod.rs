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

use std::{borrow::Borrow, hash::Hash, ops::Deref, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use uuid::Uuid;

pub mod backend;
#[cfg(feature = "webdav")]
pub mod carddav;
pub mod jscontact;
pub mod mutt;
pub mod notmuchcontact;
pub mod vcard;

pub use jscontact::JSContactV1;
pub type Card = JSContactV1;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CardId(Uuid);

impl serde::Serialize for CardId {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for CardId {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = <Uuid>::deserialize(deserializer)?;
        Ok(Self(s))
    }
}

impl CardId {
    pub const fn as_uuid(&self) -> Uuid {
        self.0
    }
}

impl std::fmt::Display for CardId {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.as_hyphenated().fmt(fmt)
    }
}

impl From<CardId> for String {
    fn from(val: CardId) -> Self {
        val.to_string()
    }
}

impl From<Uuid> for CardId {
    fn from(u: Uuid) -> Self {
        Self(u)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct AddressBookName(pub Arc<str>);

impl Deref for AddressBookName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<&str> for AddressBookName {
    fn from(s: &str) -> Self {
        Self(s.to_string().into_boxed_str().into())
    }
}

impl From<Arc<str>> for AddressBookName {
    fn from(inner: Arc<str>) -> Self {
        Self(inner)
    }
}

impl std::fmt::Display for AddressBookName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(fmt)
    }
}

impl Borrow<str> for AddressBookName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AddressBook {
    pub name: AddressBookName,
    pub read_only: bool,
    pub cards: IndexMap<CardId, Card>,
}

impl AddressBook {
    pub fn new(name: AddressBookName, read_only: bool) -> Self {
        Self {
            name,
            read_only,
            cards: IndexMap::default(),
        }
    }

    pub fn add_card(&mut self, card: Card) {
        self.cards.insert(card.id(), card);
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
            .filter(|c| {
                c.email().unwrap_or_default().contains(term)
                    || c.name().unwrap_or_default().contains(term)
                    || c.nicknames.values().any(|nick| nick.name.contains(term))
                    || c.organizations.values().any(|org| org.name.contains(term))
                    || c.keywords.keys().any(|keyword| keyword.contains(term))
            })
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

#[derive(Clone, Hash, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ContactBackendID {
    pub name: Box<str>,
    pub format: Box<str>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Contacts {
    name: String,
    pub books: IndexMap<(Arc<ContactBackendID>, AddressBookName), AddressBook>,
    pub backends: IndexSet<Arc<ContactBackendID>>,
}

impl Contacts {
    pub fn new(name: String) -> Self {
        Self {
            name,
            books: IndexMap::default(),
            backends: IndexSet::default(),
        }
    }

    pub fn add_book(&mut self, backend_id: Arc<ContactBackendID>, book: AddressBook) {
        let id = if let Some(id) = self.backends.iter().find(|i| {
            i.name.as_ref() == backend_id.name.as_ref()
                && i.format.as_ref() == backend_id.format.as_ref()
        }) {
            Arc::clone(id)
        } else {
            let id = backend_id;
            self.backends.insert(Arc::clone(&id));
            id
        };
        self.books.insert((id, book.name.clone()), book);
    }

    pub fn get_book(&self, backend_id: &ContactBackendID, name: &str) -> Option<&AddressBook> {
        let id = self.backends.iter().find(|i| {
            i.name.as_ref() == backend_id.name.as_ref()
                && i.format.as_ref() == backend_id.format.as_ref()
        })?;
        self.books.iter().find_map(|(k, v)| {
            if k.0 == *id && (k.1).0.as_ref() == name {
                Some(v)
            } else {
                None
            }
        })
    }
}
