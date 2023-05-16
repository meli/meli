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

//! Basic mail account configuration to use with
//! [`backends`](./backends/index.html)
use std::collections::HashMap;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{
    backends::SpecialUsageMailbox,
    error::{Error, Result},
};
pub use crate::{SortField, SortOrder};

#[derive(Debug, Serialize, Default, Clone)]
pub struct AccountSettings {
    pub name: String,
    pub root_mailbox: String,
    pub format: String,
    pub identity: String,
    pub extra_identities: Vec<String>,
    pub read_only: bool,
    pub display_name: Option<String>,
    #[serde(default)]
    pub order: (SortField, SortOrder),
    pub subscribed_mailboxes: Vec<String>,
    #[serde(default)]
    pub mailboxes: HashMap<String, MailboxConf>,
    #[serde(default)]
    pub manual_refresh: bool,
    #[serde(flatten)]
    pub extra: HashMap<String, String>,
}

impl AccountSettings {
    /// Create the account's display name from fields
    /// [`AccountSettings::identity`] and [`AccountSettings::display_name`].
    pub fn make_display_name(&self) -> String {
        if let Some(d) = self.display_name.as_ref() {
            format!("{} <{}>", d, self.identity)
        } else {
            self.identity.to_string()
        }
    }

    pub fn order(&self) -> Option<(SortField, SortOrder)> {
        Some(self.order)
    }

    #[cfg(feature = "vcard")]
    pub fn vcard_folder(&self) -> Option<&str> {
        self.extra.get("vcard_folder").map(String::as_str)
    }

    /// Get the server password, either directly from the `server_password`
    /// settings value, or by running the `server_password_command` and reading
    /// the output.
    pub fn server_password(&self) -> Result<String> {
        if let Some(cmd) = self.extra.get("server_password_command") {
            let output = std::process::Command::new("sh")
                .args(["-c", cmd])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .output()?;

            if output.status.success() {
                Ok(std::str::from_utf8(&output.stdout)?.trim_end().to_string())
            } else {
                Err(Error::new(format!(
                    "({}) server_password_command `{}` returned {}: {}",
                    self.name,
                    cmd,
                    output.status,
                    String::from_utf8_lossy(&output.stderr)
                )))
            }
        } else if let Some(pass) = self.extra.get("server_password") {
            Ok(pass.to_owned())
        } else {
            Err(Error::new(format!(
                "Configuration error: connection requires either server_password or \
                 server_password_command"
            )))
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct MailboxConf {
    #[serde(alias = "rename")]
    pub alias: Option<String>,
    #[serde(default = "false_val")]
    pub autoload: bool,
    #[serde(default)]
    pub subscribe: ToggleFlag,
    #[serde(default)]
    pub ignore: ToggleFlag,
    #[serde(default = "none")]
    pub usage: Option<SpecialUsageMailbox>,
    #[serde(default = "none")]
    pub sort_order: Option<usize>,
    #[serde(default = "none")]
    pub encoding: Option<String>,
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
            sort_order: None,
            encoding: None,
            extra: HashMap::default(),
        }
    }
}

impl MailboxConf {
    pub fn alias(&self) -> Option<&str> {
        self.alias.as_deref()
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

macro_rules! named_unit_variant {
    ($variant:ident) => {
        pub mod $variant {
            pub fn deserialize<'de, D>(deserializer: D) -> Result<(), D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct V;
                impl<'de> serde::de::Visitor<'de> for V {
                    type Value = ();
                    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        f.write_str(concat!("\"", stringify!($variant), "\""))
                    }
                    fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<Self::Value, E> {
                        if value == stringify!($variant) {
                            Ok(())
                        } else {
                            Err(E::invalid_value(serde::de::Unexpected::Str(value), &self))
                        }
                    }
                }
                deserializer.deserialize_str(V)
            }
        }
    };
}

mod strings {
    named_unit_variant!(ask);
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum ToggleFlag {
    Unset,
    InternalVal(bool),
    False,
    True,
    Ask,
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
        matches!(self, ToggleFlag::InternalVal(_))
    }

    pub fn is_ask(&self) -> bool {
        matches!(self, ToggleFlag::Ask)
    }

    pub fn is_false(&self) -> bool {
        matches!(self, ToggleFlag::False | ToggleFlag::InternalVal(false))
    }

    pub fn is_true(&self) -> bool {
        matches!(self, ToggleFlag::True | ToggleFlag::InternalVal(true))
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
            ToggleFlag::Ask => serializer.serialize_str("ask"),
        }
    }
}

impl<'de> Deserialize<'de> for ToggleFlag {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        pub enum InnerToggleFlag {
            Bool(bool),
            #[serde(with = "strings::ask")]
            Ask,
        }
        let s = <InnerToggleFlag>::deserialize(deserializer);
        Ok(
            match s.map_err(|err| {
                serde::de::Error::custom(format!(
                    r#"expected one of "true", "false", "ask", found `{}`"#,
                    err
                ))
            })? {
                InnerToggleFlag::Bool(true) => ToggleFlag::True,
                InnerToggleFlag::Bool(false) => ToggleFlag::False,
                InnerToggleFlag::Ask => ToggleFlag::Ask,
            },
        )
    }
}
