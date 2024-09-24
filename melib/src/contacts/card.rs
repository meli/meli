//
// meli
//
// Copyright 2019, 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

//! Type for representing contacts.

use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use indexmap::IndexMap;
use uuid::Uuid;

use crate::{
    contacts::CardId,
    utils::datetime::{now, timestamp_to_string, UnixTimestamp},
};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Card {
    pub id: CardId,
    pub title: String,
    pub name: String,
    pub additionalname: String,
    pub name_prefix: String,
    pub name_suffix: String,
    pub birthday: Option<UnixTimestamp>,
    pub email: String,
    pub url: String,
    pub key: String,
    pub color: u8,
    pub last_edited: UnixTimestamp,
    pub extra_properties: IndexMap<String, String>,
    pub external_resource: bool,
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

impl Hash for Card {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut serialized = serde_json::json! { self };
        // The following anonymous let bind is just to make sure at compile-time that
        // `id` field is present in Self and serialized["id"] will be a valid access.
        let _: &CardId = &self.id;
        serialized["id"] = serde_json::json! { CardId::Hash(0) };
        serialized.to_string().hash(state);
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

impl Default for Card {
    fn default() -> Self {
        Self::new()
    }
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
            extra_properties: IndexMap::default(),
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

    pub fn extra_properties(&self) -> &IndexMap<String, String> {
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

    pub fn to_vcard_string(&self) -> String {
        use crate::utils::vobject::{vcard::VcardBuilder, *};
        write_component(
            &VcardBuilder::new()
                .with_email(self.email.clone())
                .with_fullname(self.name.clone())
                .with_title(self.title.clone())
                .with_url(self.url.clone())
                .with_uid(if let CardId::Uuid(v) = self.id {
                    v.as_urn().to_string()
                } else {
                    String::new()
                })
                .with_nickname(Default::default(), self.additionalname.clone())
                .build()
                .unwrap(),
        )
    }
}

impl From<IndexMap<String, String>> for Card {
    fn from(mut map: IndexMap<String, String>) -> Self {
        let mut card = Self::new();
        macro_rules! get {
            ($key:literal, $field:tt) => {
                if let Some(val) = map.swap_remove($key) {
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

impl From<HashMap<String, String>> for Card {
    fn from(map: HashMap<String, String>) -> Self {
        let map: IndexMap<String, String> = map.into_iter().collect();
        Self::from(map)
    }
}
