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

//! Configuration logic and `config.toml` interfaces.

extern crate serde;
extern crate toml;
extern crate xdg;

use std::{
    env,
    fs::OpenOptions,
    io::Write,
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
    sync::Arc,
};

use indexmap::IndexMap;
use melib::{
    backends::{MailboxHash, TagHash},
    conf::{ActionFlag, MailboxConf, ToggleFlag},
    error::*,
    search::Query,
    ShellExpandTrait, SortField, SortOrder, StderrLogger,
};
use serde::{Deserialize, Serialize};

use crate::{conf::deserializers::non_empty_opt_string, terminal::Color};

pub mod default_values;
pub mod preprocessing;
use preprocessing as pp;

pub mod data_types;
#[cfg(test)]
pub mod tests;
#[rustfmt::skip]
mod overrides;
pub use overrides::*;
pub mod composing;
pub mod notifications;
pub mod pager;
pub mod pgp;
pub mod tags;
#[macro_use]
pub mod shortcuts;
mod listing;
pub mod terminal;
mod themes;
use default_values::*;
pub use themes::*;

pub use self::{composing::*, pgp::*, shortcuts::*, tags::*};

/// Utility macro to access an [`AccountConf`] setting field from
/// [`Context`](crate::Context) indexed by `$account_hash`
///
/// The value returned is the optionally overriden one in the
/// [`AccountConf::conf_override`] field, otherwise the global one.
///
/// See also the [`mailbox_settings`](crate::mailbox_settings) macro.
#[macro_export]
macro_rules! account_settings {
    ($context:ident[$account_hash:expr].$setting:ident.$field:ident) => {{
        $context.accounts[&$account_hash]
            .settings
            .conf_override
            .$setting
            .$field
            .as_ref()
            .unwrap_or(&$context.settings.$setting.$field)
    }};
    ($context:ident[$account_hash:expr].$field:ident) => {{
        &$context.accounts[&$account_hash].settings.$field
    }};
}

/// Utility macro to access an [`AccountConf`] setting field from
/// [`Context`](crate::Context) indexed by `$account_hash` and a mailbox.
///
/// The value returned is the optionally overriden one in the
/// [`FileMailboxConf::conf_override`] field, otherwise the
/// [`AccountConf::conf_override`] field, otherwise the global one.
///
/// See also the [`account_settings`] macro.
#[macro_export]
macro_rules! mailbox_settings {
    ($context:ident[$account_hash:expr][$mailbox_path:expr].$setting:ident.$field:ident) => {{
        $context.accounts[&$account_hash][$mailbox_path]
            .conf
            .conf_override
            .$setting
            .$field
            .as_ref()
            .or($context.accounts[&$account_hash]
                .settings
                .conf_override
                .$setting
                .$field
                .as_ref())
            .unwrap_or(&$context.settings.$setting.$field)
    }};
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct MailUIConf {
    pub send_mail: Option<SendMail>,
    #[serde(default)]
    pub pager: PagerSettingsOverride,
    #[serde(default)]
    pub listing: ListingSettingsOverride,
    #[serde(default)]
    pub notifications: NotificationsSettingsOverride,
    #[serde(default)]
    pub shortcuts: ShortcutsOverride,
    #[serde(default)]
    pub composing: ComposingSettingsOverride,
    #[serde(default)]
    pub identity: Option<String>,
    #[serde(default)]
    pub tags: TagsSettingsOverride,
    #[serde(default)]
    pub themes: Option<Themes>,
    #[serde(default)]
    pub pgp: PGPSettingsOverride,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct FileMailboxConf {
    #[serde(flatten)]
    pub conf_override: MailUIConf,
    #[serde(default = "false_val")]
    pub collapsed: bool,
    #[serde(flatten)]
    pub mailbox_conf: MailboxConf,
}

impl FileMailboxConf {
    pub fn conf_override(&self) -> &MailUIConf {
        &self.conf_override
    }

    pub fn mailbox_conf(&self) -> &MailboxConf {
        &self.mailbox_conf
    }
}

use crate::conf::deserializers::extra_settings;
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct FileAccount {
    pub root_mailbox: String,
    /// The mailbox that is the default to open / view for this account. Must be
    /// a valid mailbox path.
    ///
    /// If not specified, the default is [`Self::root_mailbox`].
    #[serde(default = "none", skip_serializing_if = "Option::is_none")]
    pub default_mailbox: Option<String>,
    pub format: String,
    pub send_mail: SendMail,
    pub identity: String,
    #[serde(default)]
    pub extra_identities: Vec<String>,
    #[serde(default = "none", skip_serializing_if = "Option::is_none")]
    pub display_name: Option<String>,
    #[serde(default = "false_val")]
    pub read_only: bool,
    #[serde(default)]
    pub subscribed_mailboxes: Vec<String>,
    #[serde(default)]
    pub mailboxes: IndexMap<String, FileMailboxConf>,
    #[serde(default)]
    pub search_backend: data_types::SearchBackend,
    #[serde(default)]
    pub order: (SortField, SortOrder),
    #[serde(default = "false_val")]
    pub manual_refresh: bool,
    #[serde(default = "none", skip_serializing_if = "Option::is_none")]
    pub refresh_command: Option<String>,
    #[serde(flatten)]
    pub conf_override: MailUIConf,
    #[serde(flatten)]
    #[serde(
        deserialize_with = "extra_settings",
        skip_serializing_if = "IndexMap::is_empty"
    )]
    /// Use custom deserializer to convert any given value (eg `bool`, number,
    /// etc) to `String`.
    pub extra: IndexMap<String, String>,
}

impl FileAccount {
    pub fn mailboxes(&self) -> &IndexMap<String, FileMailboxConf> {
        &self.mailboxes
    }

    pub fn search_backend(&self) -> &data_types::SearchBackend {
        &self.search_backend
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct FileSettings {
    pub accounts: IndexMap<String, FileAccount>,
    #[serde(default)]
    pub pager: pager::PagerSettings,
    #[serde(default)]
    pub listing: listing::ListingSettings,
    #[serde(default)]
    pub notifications: notifications::NotificationsSettings,
    #[serde(default)]
    pub shortcuts: shortcuts::Shortcuts,
    #[serde(default)]
    pub composing: composing::ComposingSettings,
    #[serde(default)]
    pub tags: tags::TagsSettings,
    #[serde(default)]
    pub pgp: pgp::PGPSettings,
    #[serde(default)]
    pub terminal: terminal::TerminalSettings,
    #[serde(default)]
    pub log: LogSettings,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AccountConf {
    pub account: melib::AccountSettings,
    /// How to send e-mail for this account.
    /// Required
    pub send_mail: SendMail,
    pub default_mailbox: Option<MailboxHash>,
    pub sent_mailbox: Option<MailboxHash>,
    pub conf: FileAccount,
    pub conf_override: MailUIConf,
    pub mailbox_confs: IndexMap<String, FileMailboxConf>,
}

impl AccountConf {
    pub fn account(&self) -> &melib::AccountSettings {
        &self.account
    }
    pub fn account_mut(&mut self) -> &mut melib::AccountSettings {
        &mut self.account
    }
    pub fn conf(&self) -> &FileAccount {
        &self.conf
    }
    pub fn conf_mut(&mut self) -> &mut FileAccount {
        &mut self.conf
    }
}

impl From<melib::AccountSettings> for AccountConf {
    fn from(account: melib::AccountSettings) -> Self {
        Self {
            account,
            ..Self::default()
        }
    }
}
impl From<FileAccount> for AccountConf {
    fn from(x: FileAccount) -> Self {
        let format = x.format.to_lowercase();
        let root_mailbox = x.root_mailbox.clone();
        let identity = x.identity.clone();
        let display_name = x.display_name.clone();
        let order = x.order;
        let mailboxes = x
            .mailboxes
            .iter()
            .map(|(k, v)| (k.clone(), v.mailbox_conf.clone()))
            .collect();

        let account = melib::AccountSettings {
            name: String::new(),
            root_mailbox,
            format,
            identity,
            extra_identities: x.extra_identities.clone(),
            read_only: x.read_only,
            display_name,
            order,
            subscribed_mailboxes: x.subscribed_mailboxes.clone(),
            mailboxes,
            manual_refresh: x.manual_refresh,
            extra: x.extra.clone().into_iter().collect(),
        };

        let mailbox_confs = x.mailboxes.clone();
        Self {
            send_mail: x.send_mail.clone(),
            default_mailbox: None,
            sent_mailbox: None,
            conf_override: x.conf_override.clone(),
            conf: x,
            mailbox_confs,
            ..Self::from(account)
        }
    }
}

pub fn get_config_file() -> Result<PathBuf> {
    if let Ok(path) = env::var("MELI_CONFIG") {
        return Ok(PathBuf::from(path).expand());
    }
    let xdg_dirs = xdg::BaseDirectories::with_prefix("meli")?;
    xdg_dirs
        .place_config_file("config.toml")
        .chain_err_summary(|| {
            format!(
                "Cannot create configuration directory in {}",
                xdg_dirs.get_config_home().display()
            )
        })
        .chain_err_kind(ErrorKind::Platform)
}

impl FileSettings {
    pub const EXAMPLE_CONFIG: &'static str = include_str!("../docs/samples/sample-config.toml");

    pub fn new() -> Result<Self> {
        let config_path = get_config_file()?;
        if !config_path.exists() {
            let path_string = config_path.display().to_string();
            if path_string.is_empty() {
                return Err(Error::new("Given configuration path is empty.")
                    .set_kind(ErrorKind::Configuration));
            }
            #[cfg(not(test))]
            let ask = crate::terminal::Ask::new(format!(
                "No configuration found. Would you like to generate one in {}?",
                path_string
            ));
            #[cfg(not(test))]
            let mut stdout = std::io::stdout();
            #[cfg(not(test))]
            let stdin = std::io::stdin();
            #[cfg(not(test))]
            if ask.run(&mut stdout, &mut stdin.lock()) {
                create_config_file(&config_path)?;
                return Err(
                    Error::new("Edit the sample configuration and relaunch meli.")
                        .set_kind(ErrorKind::Configuration),
                );
            }
            #[cfg(test)]
            return Ok(Self::default());
            #[cfg(not(test))]
            return Err(
                Error::new("No configuration file found.").set_kind(ErrorKind::Configuration)
            );
        }

        let mut stdout = std::io::stdout();
        let stdin = std::io::stdin();
        crate::version_migrations::version_setup(&config_path, &mut stdout, &mut stdin.lock())?;
        Self::validate(config_path, false)
    }

    /// Validate configuration from `input` string.
    pub fn validate_string(s: String, clear_extras: bool) -> Result<Self> {
        let _: toml::value::Table = melib::serde_path_to_error::deserialize(
            toml::Deserializer::new(&s),
        )
        .map_err(|err| {
            Error::new("Config file is invalid TOML")
                .set_source(Some(Arc::new(err)))
                .set_kind(ErrorKind::ValueError)
        })?;

        let mut s: Self = melib::serde_path_to_error::deserialize(toml::Deserializer::new(&s))
            .map_err(|err| {
                Error::new("Input contains errors")
                    .set_source(Some(Arc::new(err)))
                    .set_kind(ErrorKind::Configuration)
            })?;
        let backends = melib::backends::Backends::new();
        let Themes {
            light: default_light,
            dark: default_dark,
            ..
        } = Themes::default();
        for (k, v) in default_light.keys.into_iter() {
            if !s.terminal.themes.light.contains_key(&k) {
                s.terminal.themes.light.insert(k, v);
            }
        }
        for theme in s.terminal.themes.other_themes.values_mut() {
            for (k, v) in default_dark.keys.clone().into_iter() {
                if !theme.contains_key(&k) {
                    theme.insert(k, v);
                }
            }
        }
        for (k, v) in default_dark.keys.into_iter() {
            if !s.terminal.themes.dark.contains_key(&k) {
                s.terminal.themes.dark.insert(k, v);
            }
        }
        match s.terminal.theme.as_str() {
            themes::DARK | themes::LIGHT => {}
            t if s.terminal.themes.other_themes.contains_key(t) => {}
            t => {
                return Err(Error::new(format!("Theme `{}` was not found.", t))
                    .set_kind(ErrorKind::Configuration));
            }
        }

        s.terminal.themes.validate()?;
        for (name, acc) in s.accounts.iter_mut() {
            let FileAccount {
                root_mailbox,
                format,
                send_mail: _,
                identity,
                extra_identities,
                read_only,
                display_name,
                order,
                subscribed_mailboxes,
                mailboxes,
                extra,
                manual_refresh,
                default_mailbox: _,
                refresh_command: _,
                search_backend: _,
                conf_override: _,
            } = acc.clone();

            let lowercase_format = format.to_lowercase();
            let mut s = melib::AccountSettings {
                name: name.to_string(),
                root_mailbox,
                format: format.clone(),
                identity,
                extra_identities,
                read_only,
                display_name,
                order,
                subscribed_mailboxes,
                manual_refresh,
                mailboxes: mailboxes
                    .into_iter()
                    .map(|(k, v)| (k, v.mailbox_conf))
                    .collect(),
                extra: extra.into_iter().collect(),
            };
            s.validate_config()?;
            backends.validate_config(&lowercase_format, &mut s)?;
            if !s.extra.is_empty() {
                return Err(Error::new(format!(
                    "Unrecognised configuration values: {:?}",
                    s.extra
                ))
                .set_kind(ErrorKind::Configuration));
            }
            if clear_extras {
                acc.extra.clear();
            }
        }

        Ok(s)
    }

    /// Validate `path` and print errors.
    pub fn validate(path: PathBuf, clear_extras: bool) -> Result<Self> {
        let s = pp::pp(&path)?;
        let _: toml::value::Table = toml::from_str(&s).map_err(|err| {
            Error::new(format!(
                "{}: Config file is invalid TOML; {}",
                path.display(),
                err
            ))
        })?;

        let mut s: Self = toml::from_str(&s).map_err(|err| {
            Error::new(format!("{}: Config file contains errors", path.display()))
                .set_source(Some(Arc::new(err)))
                .set_kind(ErrorKind::Configuration)
        })?;
        let backends = melib::backends::Backends::new();
        let Themes {
            light: default_light,
            dark: default_dark,
            ..
        } = Themes::default();
        for (k, v) in default_light.keys.into_iter() {
            if !s.terminal.themes.light.contains_key(&k) {
                s.terminal.themes.light.insert(k, v);
            }
        }
        for theme in s.terminal.themes.other_themes.values_mut() {
            for (k, v) in default_dark.keys.clone().into_iter() {
                if !theme.contains_key(&k) {
                    theme.insert(k, v);
                }
            }
        }
        for (k, v) in default_dark.keys.into_iter() {
            if !s.terminal.themes.dark.contains_key(&k) {
                s.terminal.themes.dark.insert(k, v);
            }
        }
        match s.terminal.theme.as_str() {
            themes::DARK | themes::LIGHT => {}
            t if s.terminal.themes.other_themes.contains_key(t) => {}
            t => {
                return Err(Error::new(format!("Theme `{}` was not found.", t))
                    .set_kind(ErrorKind::Configuration));
            }
        }

        s.terminal.themes.validate()?;
        for (name, acc) in s.accounts.iter_mut() {
            let FileAccount {
                root_mailbox,
                format,
                send_mail: _,
                identity,
                extra_identities,
                read_only,
                display_name,
                order,
                subscribed_mailboxes,
                mailboxes,
                extra,
                manual_refresh,
                default_mailbox: _,
                refresh_command: _,
                search_backend: _,
                conf_override: _,
            } = acc.clone();

            let lowercase_format = format.to_lowercase();
            let mut s = melib::AccountSettings {
                name: name.to_string(),
                root_mailbox,
                format: format.clone(),
                identity,
                extra_identities,
                read_only,
                display_name,
                order,
                subscribed_mailboxes,
                manual_refresh,
                mailboxes: mailboxes
                    .into_iter()
                    .map(|(k, v)| (k, v.mailbox_conf))
                    .collect(),
                extra: extra.into_iter().collect(),
            };
            s.validate_config()?;
            backends.validate_config(&lowercase_format, &mut s)?;
            if !s.extra.is_empty() {
                return Err(Error::new(format!(
                    "Unrecognised configuration values: {:?}",
                    s.extra
                ))
                .set_kind(ErrorKind::Configuration));
            }
            if clear_extras {
                acc.extra.clear();
            }
        }

        Ok(s)
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Settings {
    pub accounts: IndexMap<String, AccountConf>,
    pub pager: pager::PagerSettings,
    pub listing: listing::ListingSettings,
    pub notifications: notifications::NotificationsSettings,
    pub shortcuts: shortcuts::Shortcuts,
    pub tags: tags::TagsSettings,
    pub composing: composing::ComposingSettings,
    pub pgp: pgp::PGPSettings,
    pub terminal: terminal::TerminalSettings,
    pub log: LogSettings,
    #[serde(skip)]
    pub _logger: StderrLogger,
}

impl Settings {
    pub fn new() -> Result<Self> {
        let fs = FileSettings::new()?;
        let mut s: IndexMap<String, AccountConf> = IndexMap::new();

        for (id, x) in fs.accounts {
            let mut ac = AccountConf::from(x);
            ac.account.name.clone_from(&id);

            s.insert(id, ac);
        }

        let mut _logger = StderrLogger::new(fs.log.maximum_level);

        if let Some(ref log_path) = fs.log.log_file {
            _logger.change_log_dest(log_path.into());
        }

        Ok(Self {
            accounts: s,
            pager: fs.pager,
            listing: fs.listing,
            notifications: fs.notifications,
            shortcuts: fs.shortcuts,
            tags: fs.tags,
            composing: fs.composing,
            pgp: fs.pgp,
            terminal: fs.terminal,
            log: fs.log,
            _logger,
        })
    }

    pub fn without_accounts() -> Result<Self> {
        let fs = FileSettings::new()?;
        let mut _logger = StderrLogger::new(fs.log.maximum_level);

        if let Some(ref log_path) = fs.log.log_file {
            _logger.change_log_dest(log_path.into());
        }

        Ok(Self {
            accounts: IndexMap::new(),
            pager: fs.pager,
            listing: fs.listing,
            notifications: fs.notifications,
            shortcuts: fs.shortcuts,
            tags: fs.tags,
            composing: fs.composing,
            pgp: fs.pgp,
            terminal: fs.terminal,
            log: fs.log,
            _logger,
        })
    }
}

mod deserializers {
    use serde::{de, Deserialize, Deserializer};

    pub(in crate::conf) fn non_empty_opt_string<'de, D, T: std::convert::From<Option<String>>>(
        deserializer: D,
    ) -> std::result::Result<T, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        if s.is_empty() {
            Ok(None.into())
        } else {
            Ok(Some(s).into())
        }
    }

    pub(in crate::conf) fn non_empty_string<'de, D, T: std::convert::From<String>>(
        deserializer: D,
    ) -> std::result::Result<T, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        if s.is_empty() {
            Err(de::Error::custom("This field value cannot be empty."))
        } else {
            Ok(s.into())
        }
    }

    use toml::Value;
    fn any_of<'de, D>(deserializer: D) -> std::result::Result<String, D::Error>
    where
        D: Deserializer<'de>,
    {
        let v: Value = Deserialize::deserialize(deserializer)?;
        if let Some(s) = v.as_str() {
            return Ok(s.to_string());
        }
        let mut ret = v.to_string();
        if (ret.starts_with('"') && ret.ends_with('"'))
            || (ret.starts_with('\"') && ret.ends_with('\''))
        {
            ret.drain(0..1).count();
            ret.drain(ret.len() - 1..).count();
        }
        Ok(ret)
    }

    use indexmap::IndexMap;
    pub(in crate::conf) fn extra_settings<'de, D>(
        deserializer: D,
    ) -> std::result::Result<IndexMap<String, String>, D::Error>
    where
        D: Deserializer<'de>,
    {
        /* Why is this needed? If the user gives a configuration value such as key =
         * true, the parsing will fail since it expects string values. We
         * want to accept key = true as well as key = "true". */
        #[derive(Deserialize)]
        struct Wrapper(#[serde(deserialize_with = "any_of")] String);

        let v = <IndexMap<String, Wrapper>>::deserialize(deserializer)?;
        Ok(v.into_iter().map(|(k, Wrapper(v))| (k, v)).collect())
    }
}

pub fn create_config_file(p: &Path) -> Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(p)
        .chain_err_summary(|| format!("Cannot create configuration file in {}", p.display()))?;
    file.write_all(FileSettings::EXAMPLE_CONFIG.as_bytes())
        .and_then(|()| file.flush())
        .chain_err_summary(|| format!("Could not write to configuration file  {}", p.display()))?;
    println!("Written example configuration to {}", p.display());
    let set_permissions = |file: std::fs::File| -> Result<()> {
        let metadata = file.metadata()?;
        let mut permissions = metadata.permissions();

        permissions.set_mode(0o600); // Read/write for owner only.
        file.set_permissions(permissions)?;
        Ok(())
    };
    if let Err(err) = set_permissions(file) {
        println!(
            "Warning: Could not set permissions of {} to 0o600: {}",
            p.display(),
            err
        );
    }
    Ok(())
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct LogSettings {
    #[serde(default)]
    pub log_file: Option<PathBuf>,
    #[serde(default)]
    pub maximum_level: melib::LogLevel,
}

pub use data_types::dotaddressable::*;
