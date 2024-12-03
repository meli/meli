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
    borrow::Borrow,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::Arc,
};

use indexmap::{IndexMap, IndexSet};
use uuid::Uuid;

pub mod backend;
mod card;
#[cfg(feature = "webdav")]
pub mod carddav;
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

    pub fn add_book(&mut self, backend_name: &str, backend_format: &str, book: AddressBook) {
        let id = if let Some(id) = self
            .backends
            .iter()
            .find(|i| i.name.as_ref() == backend_name && i.format.as_ref() == backend_format)
        {
            Arc::clone(id)
        } else {
            let id = Arc::new(ContactBackendID {
                name: backend_name.to_string().into_boxed_str(),
                format: backend_format.to_string().into_boxed_str(),
            });
            self.backends.insert(Arc::clone(&id));
            id
        };
        self.books.insert((id, book.name.clone()), book);
    }

    pub fn get_book(
        &self,
        backend_name: &str,
        backend_format: &str,
        name: &str,
    ) -> Option<&AddressBook> {
        let id = self
            .backends
            .iter()
            .find(|i| i.name.as_ref() == backend_name && i.format.as_ref() == backend_format)?;
        self.books.iter().find_map(|(k, v)| {
            if k.0 == *id && (k.1).0.as_ref() == name {
                Some(v)
            } else {
                None
            }
        })
    }
}
