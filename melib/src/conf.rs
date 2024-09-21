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

use std::path::Path;

use indexmap::IndexMap;

use crate::{
    backends::SpecialUsageMailbox,
    email::Address,
    error::{Error, ErrorKind, Result},
    ShellExpandTrait,
};
pub use crate::{SortField, SortOrder};

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AccountSettings {
    pub name: String,
    /// Name of mailbox that is the root of the mailbox hierarchy.
    ///
    /// Note that this may have special or no meaning depending on the e-mail
    /// backend.
    pub root_mailbox: String,
    pub format: String,
    pub identity: String,
    #[serde(default)]
    pub extra_identities: Vec<String>,
    #[serde(default = "false_val")]
    pub read_only: bool,
    #[serde(default)]
    pub display_name: Option<String>,
    #[serde(default)]
    pub order: (SortField, SortOrder),
    #[serde(default)]
    pub subscribed_mailboxes: Vec<String>,
    #[serde(default)]
    pub mailboxes: IndexMap<String, MailboxConf>,
    #[serde(default)]
    pub manual_refresh: bool,
    #[serde(flatten)]
    pub extra: IndexMap<String, String>,
}

impl AccountSettings {
    /// Create the account's display name from fields
    /// [`AccountSettings::identity`] and [`AccountSettings::display_name`].
    #[deprecated(
        since = "0.8.5",
        note = "Use AcountSettings::main_identity_address instead."
    )]
    pub fn make_display_name(&self) -> Address {
        Address::new(self.display_name.clone(), self.identity.clone())
    }

    /// Return address associated with this account.
    /// It combines the values from [`AccountSettings::identity`] and
    /// [`AccountSettings::display_name`].
    pub fn main_identity_address(&self) -> Address {
        Address::new(self.display_name.clone(), self.identity.clone())
    }

    /// Return addresses of extra identites associated with this account,
    /// if any.
    pub fn extra_identity_addresses(&self) -> Vec<Address> {
        self.extra_identities
            .iter()
            .map(|i| Address::new(None, i.clone()))
            .collect()
    }

    pub fn order(&self) -> Option<(SortField, SortOrder)> {
        Some(self.order)
    }

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
            Err(Error::new(
                "Configuration error: connection requires either server_password or \
                 server_password_command",
            ))
        }
    }

    pub fn validate_config(&mut self) -> Result<()> {
        {
            if let Some(folder) = self.extra.swap_remove("vcard_folder") {
                let path = Path::new(&folder).expand();

                if !matches!(path.try_exists(), Ok(true)) {
                    return Err(Error::new(format!(
                        "`vcard_folder` path {} does not exist",
                        path.display()
                    ))
                    .set_details("`vcard_folder` must be a path of a folder containing .vcf files")
                    .set_kind(ErrorKind::Configuration));
                }
                if !path.is_dir() {
                    return Err(Error::new(format!(
                        "`vcard_folder` path {} is not a directory",
                        path.display()
                    ))
                    .set_details("`vcard_folder` must be a path of a folder containing .vcf files")
                    .set_kind(ErrorKind::Configuration));
                }
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
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
    pub extra: IndexMap<String, String>,
}

impl Default for MailboxConf {
    fn default() -> Self {
        Self {
            alias: None,
            autoload: false,
            subscribe: ToggleFlag::Unset,
            ignore: ToggleFlag::Unset,
            usage: None,
            sort_order: None,
            encoding: None,
            extra: IndexMap::default(),
        }
    }
}

impl MailboxConf {
    pub fn alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }
}

pub const fn true_val() -> bool {
    true
}

pub const fn false_val() -> bool {
    false
}

pub const fn none<T>() -> Option<T> {
    None
}

pub use config_field_types::*;

pub mod config_field_types {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
                        fn visit_str<E: serde::de::Error>(
                            self,
                            value: &str,
                        ) -> Result<Self::Value, E> {
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

    pub mod strings {
        named_unit_variant!(ask);
    }

    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    pub enum ToggleFlag {
        #[default]
        Unset,
        InternalVal(bool),
        False,
        True,
    }

    impl From<bool> for ToggleFlag {
        fn from(val: bool) -> Self {
            if val {
                Self::True
            } else {
                Self::False
            }
        }
    }

    impl ToggleFlag {
        pub fn is_unset(&self) -> bool {
            Self::Unset == *self
        }

        pub fn is_internal(&self) -> bool {
            matches!(self, Self::InternalVal(_))
        }

        pub fn is_false(&self) -> bool {
            matches!(self, Self::False | Self::InternalVal(false))
        }

        pub fn is_true(&self) -> bool {
            matches!(self, Self::True | Self::InternalVal(true))
        }
    }

    impl Serialize for ToggleFlag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                Self::Unset | Self::InternalVal(_) => serializer.serialize_none(),
                Self::False => serializer.serialize_bool(false),
                Self::True => serializer.serialize_bool(true),
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
            enum InnerToggleFlag {
                Bool(bool),
            }
            let s = <InnerToggleFlag>::deserialize(deserializer);
            Ok(
                match s.map_err(|err| {
                    serde::de::Error::custom(format!(
                        r#"expected one of "true", "false", found `{}`"#,
                        err
                    ))
                })? {
                    InnerToggleFlag::Bool(true) => Self::True,
                    InnerToggleFlag::Bool(false) => Self::False,
                },
            )
        }
    }

    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    pub enum ActionFlag {
        InternalVal(bool),
        False,
        True,
        #[default]
        Ask,
    }

    impl From<bool> for ActionFlag {
        fn from(val: bool) -> Self {
            if val {
                Self::True
            } else {
                Self::False
            }
        }
    }

    impl ActionFlag {
        pub fn is_internal(&self) -> bool {
            matches!(self, Self::InternalVal(_))
        }

        pub fn is_ask(&self) -> bool {
            matches!(self, Self::Ask)
        }

        pub fn is_false(&self) -> bool {
            matches!(self, Self::False | Self::InternalVal(false))
        }

        pub fn is_true(&self) -> bool {
            matches!(self, Self::True | Self::InternalVal(true))
        }
    }

    impl Serialize for ActionFlag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                Self::InternalVal(_) => serializer.serialize_none(),
                Self::False => serializer.serialize_bool(false),
                Self::True => serializer.serialize_bool(true),
                Self::Ask => serializer.serialize_str("ask"),
            }
        }
    }

    impl<'de> Deserialize<'de> for ActionFlag {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            #[derive(Deserialize)]
            #[serde(untagged)]
            enum InnerActionFlag {
                Bool(bool),
                #[serde(with = "strings::ask")]
                Ask,
            }
            let s = <InnerActionFlag>::deserialize(deserializer);
            Ok(
                match s.map_err(|err| {
                    serde::de::Error::custom(format!(
                        r#"expected one of "true", "false", "ask", found `{}`"#,
                        err
                    ))
                })? {
                    InnerActionFlag::Bool(true) => Self::True,
                    InnerActionFlag::Bool(false) => Self::False,
                    InnerActionFlag::Ask => Self::Ask,
                },
            )
        }
    }
}
