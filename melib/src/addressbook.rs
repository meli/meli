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

#[cfg(feature = "vcard")]
pub mod vcard;

use crate::datetime::{self, UnixTimestamp};
use std::collections::HashMap;
use uuid::Uuid;

use std::ops::Deref;

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy, Deserialize, Serialize)]
#[serde(from = "String")]
#[serde(into = "String")]
pub enum CardId {
    Uuid(Uuid),
    Hash(u64),
}

impl Into<String> for CardId {
    fn into(self) -> String {
        match self {
            CardId::Uuid(u) => u.to_string(),
            CardId::Hash(u) => u.to_string(),
        }
    }
}

impl From<String> for CardId {
    fn from(s: String) -> CardId {
        if let Ok(u) = uuid::Uuid::parse_str(s.as_str()) {
            CardId::Uuid(u)
        } else {
            use std::str::FromStr;
            CardId::Hash(u64::from_str(&s).unwrap())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct AddressBook {
    display_name: String,
    created: UnixTimestamp,
    last_edited: UnixTimestamp,
    pub cards: HashMap<CardId, Card>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Card {
    id: CardId,
    title: String,
    name: String,
    additionalname: String,
    name_prefix: String,
    name_suffix: String,
    //address
    birthday: Option<UnixTimestamp>,
    email: String,
    url: String,
    key: String,

    color: u8,
    last_edited: UnixTimestamp,
    extra_properties: HashMap<String, String>,

    /// If true, we can't make any changes because we do not manage this resource.
    external_resource: bool,
}

impl AddressBook {
    pub fn new(display_name: String) -> AddressBook {
        AddressBook {
            display_name,
            created: datetime::now(),
            last_edited: datetime::now(),
            cards: HashMap::default(),
        }
    }

    pub fn with_account(s: &crate::conf::AccountSettings) -> AddressBook {
        #[cfg(not(feature = "vcard"))]
        {
            AddressBook::new(s.name.clone())
        }
        #[cfg(feature = "vcard")]
        {
            let mut ret = AddressBook::new(s.name.clone());
            if let Some(vcard_path) = s.vcard_folder() {
                if let Ok(cards) = vcard::load_cards(std::path::Path::new(vcard_path)) {
                    for c in cards {
                        ret.add_card(c);
                    }
                }
            }
            ret
        }
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
            .filter(|c| c.email.contains(term))
            .map(|c| format!("{} <{}>", &c.name, &c.email))
            .collect()
    }
}

impl Deref for AddressBook {
    type Target = HashMap<CardId, Card>;

    fn deref(&self) -> &HashMap<CardId, Card> {
        &self.cards
    }
}

impl Card {
    pub fn new() -> Card {
        Card {
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

            last_edited: datetime::now(),
            external_resource: false,
            extra_properties: HashMap::default(),
            color: 0,
        }
    }

    pub fn id(&self) -> &CardId {
        &self.id
    }

    pub fn title(&self) -> &str {
        self.title.as_str()
    }
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
    pub fn additionalname(&self) -> &str {
        self.additionalname.as_str()
    }
    pub fn name_prefix(&self) -> &str {
        self.name_prefix.as_str()
    }
    pub fn name_suffix(&self) -> &str {
        self.name_suffix.as_str()
    }
    pub fn email(&self) -> &str {
        self.email.as_str()
    }
    pub fn url(&self) -> &str {
        self.url.as_str()
    }
    pub fn key(&self) -> &str {
        self.key.as_str()
    }
    pub fn last_edited(&self) -> String {
        datetime::timestamp_to_string(self.last_edited, None, false)
    }

    pub fn set_id(&mut self, new_val: CardId) {
        self.id = new_val;
    }
    pub fn set_title(&mut self, new: String) {
        self.title = new;
    }
    pub fn set_name(&mut self, new: String) {
        self.name = new;
    }
    pub fn set_additionalname(&mut self, new: String) {
        self.additionalname = new;
    }
    pub fn set_name_prefix(&mut self, new: String) {
        self.name_prefix = new;
    }
    pub fn set_name_suffix(&mut self, new: String) {
        self.name_suffix = new;
    }
    pub fn set_email(&mut self, new: String) {
        self.email = new;
    }
    pub fn set_url(&mut self, new: String) {
        self.url = new;
    }
    pub fn set_key(&mut self, new: String) {
        self.key = new;
    }

    pub fn set_extra_property(&mut self, key: &str, value: String) {
        self.extra_properties.insert(key.to_string(), value);
    }

    pub fn extra_property(&self, key: &str) -> Option<&str> {
        self.extra_properties.get(key).map(String::as_str)
    }

    pub fn extra_properties(&self) -> &HashMap<String, String> {
        &self.extra_properties
    }

    pub fn set_external_resource(&mut self, new_val: bool) {
        self.external_resource = new_val;
    }

    pub fn external_resource(&self) -> bool {
        self.external_resource
    }
}

impl From<HashMap<String, String>> for Card {
    fn from(mut map: HashMap<String, String>) -> Card {
        let mut card = Card::new();
        if let Some(val) = map.remove("TITLE") {
            card.title = val;
        }
        if let Some(val) = map.remove("NAME") {
            card.name = val;
        }
        if let Some(val) = map.remove("ADDITIONAL NAME") {
            card.additionalname = val;
        }
        if let Some(val) = map.remove("NAME PREFIX") {
            card.name_prefix = val;
        }
        if let Some(val) = map.remove("NAME SUFFIX") {
            card.name_suffix = val;
        }

        if let Some(val) = map.remove("E-MAIL") {
            card.email = val;
        }
        if let Some(val) = map.remove("URL") {
            card.url = val;
        }
        if let Some(val) = map.remove("KEY") {
            card.key = val;
        }
        card.extra_properties = map;
        card
    }
}

impl Default for Card {
    fn default() -> Self {
        Self::new()
    }
}
