/*
 * meli - configuration module.
 *
 * Copyright 2017 Manos Pitsidianakis
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
use crate::backends::SpecialUsageMailbox;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;

#[derive(Debug, Serialize, Default, Clone)]
pub struct AccountSettings {
    pub name: String,
    pub root_mailbox: String,
    pub format: String,
    pub identity: String,
    pub read_only: bool,
    pub display_name: Option<String>,
    pub subscribed_mailboxes: Vec<String>,
    #[serde(default)]
    pub mailboxes: HashMap<String, MailboxConf>,
    #[serde(default)]
    pub manual_refresh: bool,
    #[serde(flatten)]
    pub extra: HashMap<String, String>,
}

impl AccountSettings {
    pub fn format(&self) -> &str {
        &self.format
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn set_name(&mut self, s: String) {
        self.name = s;
    }
    pub fn root_mailbox(&self) -> &str {
        &self.root_mailbox
    }
    pub fn identity(&self) -> &str {
        &self.identity
    }
    pub fn read_only(&self) -> bool {
        self.read_only
    }
    pub fn display_name(&self) -> Option<&String> {
        self.display_name.as_ref()
    }

    pub fn subscribed_mailboxes(&self) -> &Vec<String> {
        &self.subscribed_mailboxes
    }

    #[cfg(feature = "vcard")]
    pub fn vcard_folder(&self) -> Option<&str> {
        self.extra.get("vcard_folder").map(String::as_str)
    }
}

#[serde(default)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MailboxConf {
    pub alias: Option<String>,
    #[serde(default = "false_val")]
    pub autoload: bool,
    #[serde(default)]
    pub subscribe: ToggleFlag,
    #[serde(default)]
    pub ignore: ToggleFlag,
    #[serde(default = "none")]
    pub usage: Option<SpecialUsageMailbox>,
    #[serde(flatten)]
    pub extra: HashMap<String, String>,
}

impl Default for MailboxConf {
    fn default() -> Self {
        MailboxConf {
            alias: None,
            autoload: false,
            subscribe: ToggleFlag::Unset,
            ignore: ToggleFlag::Unset,
            usage: None,
            extra: HashMap::default(),
        }
    }
}

impl MailboxConf {
    pub fn alias(&self) -> Option<&str> {
        self.alias.as_ref().map(String::as_str)
    }
}

pub fn true_val() -> bool {
    true
}

pub fn false_val() -> bool {
    false
}

pub fn none<T>() -> Option<T> {
    None
}

#[derive(Copy, Debug, Clone, PartialEq)]
pub enum ToggleFlag {
    Unset,
    InternalVal(bool),
    False,
    True,
}

impl From<bool> for ToggleFlag {
    fn from(val: bool) -> Self {
        if val {
            ToggleFlag::True
        } else {
            ToggleFlag::False
        }
    }
}

impl Default for ToggleFlag {
    fn default() -> Self {
        ToggleFlag::Unset
    }
}

impl ToggleFlag {
    pub fn is_unset(&self) -> bool {
        ToggleFlag::Unset == *self
    }
    pub fn is_internal(&self) -> bool {
        if let ToggleFlag::InternalVal(_) = *self {
            true
        } else {
            false
        }
    }
    pub fn is_false(&self) -> bool {
        ToggleFlag::False == *self || ToggleFlag::InternalVal(false) == *self
    }
    pub fn is_true(&self) -> bool {
        ToggleFlag::True == *self || ToggleFlag::InternalVal(true) == *self
    }
}

impl Serialize for ToggleFlag {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            ToggleFlag::Unset | ToggleFlag::InternalVal(_) => serializer.serialize_none(),
            ToggleFlag::False => serializer.serialize_bool(false),
            ToggleFlag::True => serializer.serialize_bool(true),
        }
    }
}

impl<'de> Deserialize<'de> for ToggleFlag {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <bool>::deserialize(deserializer);
        Ok(match s? {
            true => ToggleFlag::True,
            false => ToggleFlag::False,
        })
    }
}
