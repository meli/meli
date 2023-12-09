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

pub mod mutt;
#[cfg(feature = "vcard")]
pub mod vcard;

use std::{collections::HashMap, ops::Deref};

use uuid::Uuid;

use crate::utils::{
    datetime::{now, timestamp_to_string, UnixTimestamp},
    parsec::Parser,
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
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash, Hasher},
            str::FromStr,
        };

        if let Ok(u) = Uuid::parse_str(s.as_str()) {
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
pub struct AddressBook {
    display_name: String,
    created: UnixTimestamp,
    last_edited: UnixTimestamp,
    pub cards: HashMap<CardId, Card>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Card {
    id: CardId,
    title: String,
    name: String,
    additionalname: String,
    name_prefix: String,
    name_suffix: String,
    birthday: Option<UnixTimestamp>,
    email: String,
    url: String,
    key: String,
    color: u8,
    last_edited: UnixTimestamp,
    extra_properties: HashMap<String, String>,
    /// If `true`, we can't make any changes because we do not manage this
    /// resource.
    external_resource: bool,
}

impl std::fmt::Display for Card {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if !self.name.is_empty() {
            self.name.fmt(fmt)
        } else if !self.email.is_empty() {
            self.email.fmt(fmt)
        } else {
            "empty contact".fmt(fmt)
        }
    }
}

impl AddressBook {
    pub fn new(display_name: String) -> Self {
        Self {
            display_name,
            created: now(),
            last_edited: now(),
            cards: HashMap::default(),
        }
    }

    pub fn with_account(s: &crate::conf::AccountSettings) -> Self {
        let mut ret = Self::new(s.name.clone());
        if let Some(mutt_alias_file) = s.extra.get("mutt_alias_file").map(String::as_str) {
            match std::fs::read_to_string(std::path::Path::new(mutt_alias_file))
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
        #[cfg(feature = "vcard")]
        if let Some(vcard_path) = s.vcard_folder() {
            match vcard::load_cards(std::path::Path::new(vcard_path)) {
                Ok(cards) => {
                    for c in cards {
                        ret.add_card(c);
                    }
                }
                Err(err) => {
                    log::warn!("Could not load vcards from {:?}: {}", vcard_path, err);
                }
            }
        }
        ret
    }

    pub fn add_card(&mut self, card: Card) {
        self.cards.insert(card.id, card);
    }

    pub fn remove_card(&mut self, card_id: CardId) {
        self.cards.remove(&card_id);
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

impl Deref for AddressBook {
    type Target = HashMap<CardId, Card>;

    fn deref(&self) -> &HashMap<CardId, Card> {
        &self.cards
    }
}

macro_rules! get_fn {
    ($fn:tt str) => {
        pub fn $fn(&self) -> &str {
            self.$fn.as_str()
        }
    };
}

macro_rules! set_fn {
    ($n:ident, $fn:tt) => {
        pub fn $n(&mut self, val: String) -> &mut Self {
            self.$fn = val;
            self
        }
    };
}

impl Card {
    pub fn new() -> Self {
        Self {
            id: CardId::Uuid(Uuid::new_v4()),
            title: String::new(),
            name: String::new(),
            additionalname: String::new(),
            name_prefix: String::new(),
            name_suffix: String::new(),
            //address
            birthday: None,
            email: String::new(),
            url: String::new(),
            key: String::new(),

            last_edited: now(),
            external_resource: false,
            extra_properties: HashMap::default(),
            color: 0,
        }
    }

    pub fn id(&self) -> &CardId {
        &self.id
    }

    get_fn! { title str }
    get_fn! { name str }
    get_fn! { additionalname str }
    get_fn! { name_prefix str }
    get_fn! { name_suffix str }
    get_fn! { email str }
    get_fn! { url str }
    get_fn! { key str }

    pub fn last_edited(&self) -> String {
        timestamp_to_string(self.last_edited, None, false)
    }

    pub fn extra_property(&self, key: &str) -> Option<&str> {
        self.extra_properties.get(key).map(String::as_str)
    }

    pub fn extra_properties(&self) -> &HashMap<String, String> {
        &self.extra_properties
    }

    pub fn external_resource(&self) -> bool {
        self.external_resource
    }

    pub fn set_id(&mut self, new_val: CardId) -> &mut Self {
        self.id = new_val;
        self
    }

    set_fn! { set_title, title }
    set_fn! { set_name, name }
    set_fn! { set_additionalname, additionalname }
    set_fn! { set_name_prefix, name_prefix }
    set_fn! { set_name_suffix, name_suffix }
    set_fn! { set_email, email }
    set_fn! { set_url, url }
    set_fn! { set_key, key }

    pub fn set_extra_property(&mut self, key: &str, value: String) -> &mut Self {
        self.extra_properties.insert(key.to_string(), value);
        self
    }

    pub fn set_external_resource(&mut self, new_val: bool) -> &mut Self {
        self.external_resource = new_val;
        self
    }
}

impl From<HashMap<String, String>> for Card {
    fn from(mut map: HashMap<String, String>) -> Self {
        let mut card = Self::new();
        macro_rules! get {
            ($key:literal, $field:tt) => {
                if let Some(val) = map.remove($key) {
                    card.$field = val;
                }
            };
        }
        get! { "TITLE", title };
        get! { "NAME", name };
        get! { "ADDITIONAL NAME", additionalname };
        get! { "NAME PREFIX", name_prefix };
        get! { "NAME SUFFIX", name_suffix };
        get! { "E-MAIL", email };
        get! { "URL", url };
        get! { "KEY", key };
        card.extra_properties = map;
        card
    }
}

impl Default for Card {
    fn default() -> Self {
        Self::new()
    }
}
