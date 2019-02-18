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
use chrono::{DateTime, Local};
use uuid::Uuid;
use fnv::FnvHashMap;

use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct AddressBook {
    display_name: String,
    created: DateTime<Local>,
    last_edited: DateTime<Local>,
    cards: FnvHashMap<Uuid, Card>
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Card {
    uuid: Uuid,
    title: String,
    firstname: String,
    lastname: String,
    additionalname: String,
    name_prefix: String,
    name_suffix: String,
    //address

    birthday: Option<DateTime<Local>>,
    email: String,
    url: String,
    key: String,

    last_edited: DateTime<Local>,
    extra_properties: FnvHashMap<String, String>
}

impl AddressBook {
    pub fn new(display_name: String) -> AddressBook {
        AddressBook {
            display_name,
            created: Local::now(),
            last_edited: Local::now(),
            cards: FnvHashMap::default(),
        }
    }
    pub fn add_card(&mut self, card: Card) {
        self.cards.insert(card.uuid, card);
    }
    pub fn remove_card(&mut self, card_uuid: Uuid) {
        self.cards.remove(&card_uuid);
    }
    pub fn card_exists(&self, card_uuid: Uuid) -> bool {
        self.cards.contains_key(&card_uuid)
    }
}

impl Deref for AddressBook {
    type Target = FnvHashMap<Uuid, Card>;

    fn deref(&self) -> &FnvHashMap<Uuid, Card> {
        &self.cards
    }
}


impl Card {
    pub fn new() -> Card {
        Card {
            uuid: Uuid::new_v4(),
            title: String::new(),
            firstname: String::new(),
            lastname: String::new(),
            additionalname: String::new(),
            name_prefix: String::new(),
            name_suffix: String::new(),
            //address

            birthday: None,
            email: String::new(),
            url: String::new(),
            key: String::new(),

            last_edited: Local::now(),
            extra_properties: FnvHashMap::default(),
        }
    }

    pub fn uuid(&self) -> &Uuid {
        &self.uuid
    }

    pub fn title(&self) -> &str {
        self.title.as_str()
    }
    pub fn firstname(&self) -> &str {
        self.firstname.as_str()
    }
    pub fn lastname(&self) -> &str {
        self.lastname.as_str()
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
        self.last_edited.to_rfc2822()
    }

    pub fn set_title(&mut self, new: &str) {
        self.title = new.to_string();()
    }
    pub fn set_firstname(&mut self, new: &str) {
        self.firstname = new.to_string();
    }
    pub fn set_lastname(&mut self, new: &str) {
        self.lastname = new.to_string();
    }
    pub fn set_additionalname(&mut self, new: &str) {
        self.additionalname = new.to_string();
    }
    pub fn set_name_prefix(&mut self, new: &str) {
        self.name_prefix = new.to_string();
    }
    pub fn set_name_suffix(&mut self, new: &str) {
        self.name_suffix = new.to_string();
    }
    pub fn set_email(&mut self, new: &str) {
        self.email = new.to_string();
    }
    pub fn set_url(&mut self, new: &str) {
        self.url = new.to_string();
    }
    pub fn set_key(&mut self, new: &str) {
        self.key = new.to_string();
    }

    pub fn set_extra_property(&mut self, key: &str, value: String) {
        self.extra_properties.insert(key.to_string(), value);
    }
    pub fn extra_property(&self, key: &str) -> Option<&str> {
        self.extra_properties.get(key).map(|v| v.as_str())
    }

}
