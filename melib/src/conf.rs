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

mod field_types;
#[cfg(test)]
mod tests;

pub use field_types::*;

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
        note = "Use AccountSettings::main_identity_address instead."
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

    /// Return addresses of extra identities associated with this account,
    /// if any.
    pub fn extra_identity_addresses(&self) -> Vec<Address> {
        self.extra_identities
            .iter()
            .map(|i| Address::new(None::<&str>, i.clone()))
            .collect()
    }

    pub fn vcard_folder(&self) -> Option<&str> {
        self.extra.get("vcard_folder").map(String::as_str)
    }

    pub fn notmuch_address_book_query(&self) -> Option<&str> {
        self.extra
            .get("notmuch_address_book_query")
            .map(String::as_str)
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
            _ = self.extra.swap_remove("notmuch_address_book_query");
        }
        {
            if let Some(mutt_alias_file) = self.extra.swap_remove("mutt_alias_file") {
                let path = Path::new(&mutt_alias_file).expand();

                if !matches!(path.try_exists(), Ok(true)) {
                    return Err(Error::new(format!(
                        "`mutt_alias_file` path {} does not exist",
                        path.display()
                    ))
                    .set_details("`mutt_alias_file` must be an existing path of a mutt alias file")
                    .set_kind(ErrorKind::Configuration));
                }
                if !path.is_file() {
                    return Err(Error::new(format!(
                        "`mutt_alias_file` path {} is not a file",
                        path.display()
                    ))
                    .set_details("`mutt_alias_file` must be a path of a mutt alias file")
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
