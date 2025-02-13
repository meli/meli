//
// meli
//
// Copyright 2017- Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use crate::conf::*;

pub trait DotAddressable: serde::Serialize {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        if !path.is_empty() {
            Err(Error::new(format!(
                "{} has no fields, it is of type {}",
                parent_field,
                std::any::type_name::<Self>()
            )))
        } else {
            Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string())
        }
    }
}

impl DotAddressable for bool {}

impl DotAddressable for String {}
impl DotAddressable for (String, String) {}
impl DotAddressable for char {}
impl DotAddressable for data_types::IndexStyle {}
impl DotAddressable for data_types::SearchBackend {}
impl DotAddressable for data_types::ThreadLayout {}
impl DotAddressable for data_types::UINotifications {}
impl DotAddressable for u64 {}
impl DotAddressable for TagHash {}
impl DotAddressable for crate::terminal::Color {}
impl DotAddressable for crate::terminal::Attr {}
impl DotAddressable for crate::terminal::Key {}
impl DotAddressable for usize {}
impl DotAddressable for Query {}
impl DotAddressable for melib::LogLevel {}
impl DotAddressable for PathBuf {}
impl DotAddressable for ToggleFlag {}
impl DotAddressable for ActionFlag {}
impl DotAddressable for melib::SpecialUsageMailbox {}
impl DotAddressable for melib::email::HeaderName {}
impl<T: DotAddressable> DotAddressable for Option<T> {}
impl<T: DotAddressable> DotAddressable for Vec<T> {}
// impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash, V: DotAddressable>
// DotAddressable     for HashMap<K, V>
// {
// }
// impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash> DotAddressable for
// HashSet<K> {}
impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash, V: DotAddressable> DotAddressable
    for indexmap::IndexMap<K, V>
{
}
impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash> DotAddressable for indexmap::IndexSet<K> {}
impl DotAddressable for (SortField, SortOrder) {}

impl DotAddressable for LogSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "log_file" => self.log_file.lookup(field, tail),
                    "maximum_level" => self.maximum_level.lookup(field, tail),

                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for Settings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "accounts" => self.accounts.lookup(field, tail),
                    "pager" => self.pager.lookup(field, tail),
                    "listing" => self.listing.lookup(field, tail),
                    "notifications" => self.notifications.lookup(field, tail),
                    "shortcuts" => self.shortcuts.lookup(field, tail),
                    "tags" => Err(Error::new("unimplemented")),
                    "composing" => Err(Error::new("unimplemented")),
                    "pgp" => Err(Error::new("unimplemented")),
                    "terminal" => self.terminal.lookup(field, tail),
                    "log" => self.log.lookup(field, tail),

                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for AccountConf {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "account" => self.account.lookup(field, tail),
                    "conf" => self.conf.lookup(field, tail),
                    "conf_override" => self.conf_override.lookup(field, tail),
                    "mailbox_confs" => self.mailbox_confs.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for MailUIConf {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let _tail = &path[1..];
                match *field {
                    "pager" => Err(Error::new("unimplemented")), /* self.pager.lookup(field, */
                    // tail),
                    "listing" => Err(Error::new("unimplemented")), /* self.listing.lookup(field, */
                    // tail),
                    "notifications" => Err(Error::new("unimplemented")), /* self.notifications.lookup(field, tail), */
                    "shortcuts" => Err(Error::new("unimplemented")),     /* self.shortcuts. */
                    // lookup(field,
                    // tail),
                    "composing" => Err(Error::new("unimplemented")), /* self.composing. */
                    // lookup(field, tail),
                    "identity" => Err(Error::new("unimplemented")), /* self.identity. */
                    // lookup(field,
                    // tail)<String>,
                    "tags" => Err(Error::new("unimplemented")), /* self.tags.lookup(field, */
                    // tail),
                    "themes" => Err(Error::new("unimplemented")), /* self.themes. */
                    // lookup(field,
                    // tail)<Themes>,
                    "pgp" => Err(Error::new("unimplemented")), //self.pgp.lookup(field, tail),

                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for FileMailboxConf {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "conf_override" => self.conf_override.lookup(field, tail),
                    "mailbox_conf" => self.mailbox_conf.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for FileAccount {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "root_mailbox" => self.root_mailbox.lookup(field, tail),
                    "format" => self.format.lookup(field, tail),
                    "identity" => self.identity.lookup(field, tail),
                    "display_name" => self.display_name.lookup(field, tail),
                    "read_only" => self.read_only.lookup(field, tail),
                    "subscribed_mailboxes" => self.subscribed_mailboxes.lookup(field, tail),
                    "mailboxes" => self.mailboxes.lookup(field, tail),
                    "search_backend" => self.search_backend.lookup(field, tail),
                    "manual_refresh" => self.manual_refresh.lookup(field, tail),
                    "refresh_command" => self.refresh_command.lookup(field, tail),
                    "conf_override" => self.conf_override.lookup(field, tail),
                    "extra" => self.extra.lookup(field, tail),
                    "order" => self.order.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for melib::AccountSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "name" => self.name.lookup(field, tail),
                    "root_mailbox" => self.root_mailbox.lookup(field, tail),
                    "format" => self.format.lookup(field, tail),
                    "identity" => self.identity.lookup(field, tail),
                    "read_only" => self.read_only.lookup(field, tail),
                    "display_name" => self.display_name.lookup(field, tail),
                    "subscribed_mailboxes" => self.subscribed_mailboxes.lookup(field, tail),
                    "mailboxes" => self.mailboxes.lookup(field, tail),
                    "manual_refresh" => self.manual_refresh.lookup(field, tail),
                    "extra" => self.extra.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for melib::MailboxConf {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "alias" => self.alias.lookup(field, tail),
                    "autoload" => self.autoload.lookup(field, tail),
                    "subscribe" => self.subscribe.lookup(field, tail),
                    "ignore" => self.ignore.lookup(field, tail),
                    "usage" => self.usage.lookup(field, tail),
                    "extra" => self.extra.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}
