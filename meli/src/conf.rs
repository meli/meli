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
    collections::HashSet,
    io::Read,
    process::{Command, Stdio},
    sync::Arc,
};

use melib::{
    backends::{MailboxHash, TagHash},
    search::Query,
    ShellExpandTrait, SortField, SortOrder, StderrLogger,
};

use crate::{conf::deserializers::non_empty_opt_string, terminal::Color};

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
use std::{
    collections::HashMap,
    env,
    fs::OpenOptions,
    io::{self, BufRead, Write},
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
};

use indexmap::IndexMap;
use melib::{
    conf::{ActionFlag, MailboxConf, ToggleFlag},
    error::*,
};
use pager::PagerSettings;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
pub use themes::*;

pub use self::{composing::*, pgp::*, shortcuts::*, tags::*};
use self::{
    default_vals::*, listing::ListingSettings, notifications::NotificationsSettings,
    terminal::TerminalSettings,
};

#[macro_export]
macro_rules! split_command {
    ($cmd:expr) => {{
        $cmd.split_whitespace().collect::<Vec<&str>>()
    }};
}

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
    pub search_backend: SearchBackend,
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

    pub fn search_backend(&self) -> &SearchBackend {
        &self.search_backend
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct FileSettings {
    pub accounts: IndexMap<String, FileAccount>,
    #[serde(default)]
    pub pager: PagerSettings,
    #[serde(default)]
    pub listing: ListingSettings,
    #[serde(default)]
    pub notifications: NotificationsSettings,
    #[serde(default)]
    pub shortcuts: Shortcuts,
    #[serde(default)]
    pub composing: ComposingSettings,
    #[serde(default)]
    pub tags: TagsSettings,
    #[serde(default)]
    pub pgp: PGPSettings,
    #[serde(default)]
    pub terminal: TerminalSettings,
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

        let acc = melib::AccountSettings {
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
            account: acc,
            send_mail: x.send_mail.clone(),
            default_mailbox: None,
            sent_mailbox: None,
            conf_override: x.conf_override.clone(),
            conf: x,
            mailbox_confs,
        }
    }
}

pub fn get_config_file() -> Result<PathBuf> {
    if let Ok(path) = env::var("MELI_CONFIG") {
        return Ok(PathBuf::from(path).expand());
    }
    let xdg_dirs = xdg::BaseDirectories::with_prefix("meli").map_err(|err| {
        Error::new("Could not detect XDG directories for user")
            .set_source(Some(std::sync::Arc::new(Box::new(err))))
            .set_kind(ErrorKind::NotSupported)
    })?;
    xdg_dirs
        .place_config_file("config.toml")
        .chain_err_summary(|| {
            format!(
                "Cannot create configuration directory in {}",
                xdg_dirs.get_config_home().display()
            )
        })
        .chain_err_kind(ErrorKind::OSError)
}

pub fn get_included_configs(conf_path: PathBuf) -> Result<Vec<PathBuf>> {
    const M4_PREAMBLE: &str = r#"divert(-1)dnl
define(`include', `divert(0)$1
divert(-1)
')dnl
changequote(`"', `"')dnl
"#;
    let mut ret = vec![];
    let prefix = conf_path.parent().unwrap().to_path_buf();
    let mut stack = vec![(None::<PathBuf>, conf_path)];
    let mut contents = String::new();
    while let Some((parent, p)) = stack.pop() {
        if !p.exists() || p.is_dir() {
            return Err(Error::new(format!(
                "Path {}{included}{in_parent} {msg}.",
                p.display(),
                included = if parent.is_some() {
                    " which is included in "
                } else {
                    ""
                },
                in_parent = if let Some(parent) = parent {
                    std::borrow::Cow::Owned(parent.display().to_string())
                } else {
                    std::borrow::Cow::Borrowed("")
                },
                msg = if !p.exists() {
                    "does not exist"
                } else {
                    "is a directory, not a text file"
                }
            ))
            .set_kind(ErrorKind::Configuration));
        }
        contents.clear();
        let mut file = std::fs::File::open(&p)?;
        file.read_to_string(&mut contents)?;

        let mut handle = match Command::new("m4")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(handle) => handle,
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => {
                    return Err(Error::new(
                        "`m4` executable not found in PATH. Please provide an m4 binary.",
                    )
                    .set_kind(ErrorKind::Platform))
                }
                _ => {
                    return Err(Error::new("Could not process configuration with `m4`")
                        .set_source(Some(Arc::new(err)))
                        .set_kind(ErrorKind::OSError))
                }
            },
        };

        let mut stdin = handle.stdin.take().unwrap();
        stdin.write_all(M4_PREAMBLE.as_bytes())?;
        stdin.write_all(contents.as_bytes())?;
        drop(stdin);
        let stdout = handle.wait_with_output()?.stdout.clone();
        for subpath in stdout.lines() {
            let subpath = subpath?;
            let path = &Path::new(&subpath);
            if path.is_absolute() {
                stack.push((Some(p.clone()), path.to_path_buf()));
            } else {
                stack.push((Some(p.clone()), prefix.join(path)));
            }
        }
        ret.push(p);
    }

    Ok(ret)
}

pub fn expand_config(conf_path: PathBuf) -> Result<String> {
    let _paths = get_included_configs(conf_path.clone())?;
    const M4_PREAMBLE: &str = r#"define(`builtin_include', defn(`include'))dnl
define(`include', `builtin_include(substr($1,1,decr(decr(len($1)))))dnl')dnl
"#;
    let mut contents = String::new();
    contents.clear();
    let mut file = std::fs::File::open(&conf_path)?;
    file.read_to_string(&mut contents)?;

    let mut handle = Command::new("m4")
        .current_dir(conf_path.parent().unwrap_or_else(|| Path::new("/")))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let mut stdin = handle.stdin.take().unwrap();
    stdin.write_all(M4_PREAMBLE.as_bytes())?;
    stdin.write_all(contents.as_bytes())?;
    drop(stdin);
    let stdout = handle.wait_with_output()?.stdout;
    Ok(String::from_utf8_lossy(&stdout).to_string())
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
            let ask = crate::terminal::Ask {
                message: format!(
                    "No configuration found. Would you like to generate one in {}?",
                    path_string
                ),
            };
            #[cfg(not(test))]
            if ask.run() {
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
    pub pager: PagerSettings,
    pub listing: ListingSettings,
    pub notifications: NotificationsSettings,
    pub shortcuts: Shortcuts,
    pub tags: TagsSettings,
    pub composing: ComposingSettings,
    pub pgp: PGPSettings,
    pub terminal: TerminalSettings,
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

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum IndexStyle {
    Plain,
    Threaded,
    #[default]
    Compact,
    Conversations,
}

/*
 * Deserialize default functions
 */

mod default_vals {
    pub(in crate::conf) fn false_val<T: std::convert::From<bool>>() -> T {
        false.into()
    }

    pub(in crate::conf) fn true_val<T: std::convert::From<bool>>() -> T {
        true.into()
    }

    pub(in crate::conf) fn zero_val<T: std::convert::From<usize>>() -> T {
        0.into()
    }

    pub(in crate::conf) fn eighty_val<T: std::convert::From<usize>>() -> T {
        80.into()
    }

    pub(in crate::conf) fn none<T>() -> Option<T> {
        None
    }

    pub(in crate::conf) fn internal_value_false<T: std::convert::From<melib::conf::ToggleFlag>>(
    ) -> T {
        melib::conf::ToggleFlag::InternalVal(false).into()
    }

    pub(in crate::conf) fn internal_value_true<T: std::convert::From<melib::conf::ToggleFlag>>() -> T
    {
        melib::conf::ToggleFlag::InternalVal(true).into()
    }

    pub(in crate::conf) fn action_internal_value_false<T: std::convert::From<melib::ActionFlag>>(
    ) -> T {
        melib::conf::ActionFlag::InternalVal(false).into()
    }

    //pub(in crate::conf) fn action_internal_value_true<
    //    T: std::convert::From<melib::conf::ActionFlag>,
    //>() -> T {
    //    melib::conf::ActionFlag::InternalVal(true).into()
    //}

    pub(in crate::conf) fn ask<T: std::convert::From<melib::conf::ActionFlag>>() -> T {
        melib::conf::ActionFlag::Ask.into()
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
        let mut ret = v.to_string();
        if ret.starts_with('"') && ret.ends_with('"') {
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

impl<'de> Deserialize<'de> for IndexStyle {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            "Plain" | "plain" => Ok(Self::Plain),
            "Threaded" | "threaded" => Ok(Self::Threaded),
            "Compact" | "compact" => Ok(Self::Compact),
            "Conversations" | "conversations" => Ok(Self::Conversations),
            _ => Err(de::Error::custom("invalid `index_style` value")),
        }
    }
}

impl Serialize for IndexStyle {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Plain => serializer.serialize_str("plain"),
            Self::Threaded => serializer.serialize_str("threaded"),
            Self::Compact => serializer.serialize_str("compact"),
            Self::Conversations => serializer.serialize_str("conversations"),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum SearchBackend {
    None,
    #[default]
    Auto,
    #[cfg(feature = "sqlite3")]
    Sqlite3,
}

impl<'de> Deserialize<'de> for SearchBackend {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            #[cfg(feature = "sqlite3")]
            sqlite3
                if sqlite3.eq_ignore_ascii_case("sqlite3")
                    || sqlite3.eq_ignore_ascii_case("sqlite") =>
            {
                Ok(Self::Sqlite3)
            }
            none if none.eq_ignore_ascii_case("none")
                || none.eq_ignore_ascii_case("nothing")
                || none.is_empty() =>
            {
                Ok(Self::None)
            }
            auto if auto.eq_ignore_ascii_case("auto") => Ok(Self::Auto),
            _ => Err(de::Error::custom("invalid `search_backend` value")),
        }
    }
}

impl Serialize for SearchBackend {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            #[cfg(feature = "sqlite3")]
            Self::Sqlite3 => serializer.serialize_str("sqlite3"),
            Self::None => serializer.serialize_str("none"),
            Self::Auto => serializer.serialize_str("auto"),
        }
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

mod pp {
    //! Preprocess configuration files by unfolding `include` macros.
    use std::{
        io::Read,
        path::{Path, PathBuf},
    };

    use melib::{
        error::{Error, Result},
        utils::parsec::*,
        ShellExpandTrait,
    };

    /// Try to parse line into a path to be included.
    fn include_directive<'a>() -> impl Parser<'a, Option<&'a str>> {
        move |input: &'a str| {
            enum State {
                Start,
                Path,
            }
            use State::*;
            let mut state = State::Start;

            let mut i = 0;
            while i < input.len() {
                match (&state, input.as_bytes()[i]) {
                    (Start, b'#') => {
                        return Ok(("", None));
                    }
                    (Start, b) if (b as char).is_whitespace() => { /* consume */ }
                    (Start, _) if input.as_bytes()[i..].starts_with(b"include(") => {
                        i += "include(".len();
                        state = Path;
                        continue;
                    }
                    (Start, _) => {
                        return Ok(("", None));
                    }
                    (Path, b'"') | (Path, b'\'') | (Path, b'`') => {
                        let mut end = i + 1;
                        while end < input.len() && input.as_bytes()[end] != input.as_bytes()[i] {
                            end += 1;
                        }
                        if end == input.len() {
                            return Err(input);
                        }
                        let ret = &input[i + 1..end];
                        end += 1;
                        if end < input.len() && input.as_bytes()[end] != b')' {
                            /* Nothing else allowed in line */
                            return Err(input);
                        }
                        end += 1;
                        while end < input.len() {
                            if !(input.as_bytes()[end] as char).is_whitespace() {
                                /* Nothing else allowed in line */
                                return Err(input);
                            }
                            end += 1;
                        }
                        return Ok(("", Some(ret)));
                    }
                    (Path, _) => return Err(input),
                }
                i += 1;
            }
            Ok(("", None))
        }
    }

    /// Expands `include` macros in path.
    fn pp_helper(path: &Path, level: u8) -> Result<String> {
        if level > 7 {
            return Err(Error::new(format!(
                "Maximum recursion limit reached while unfolding include directives in {}. Have \
                 you included a config file within itself?",
                path.display()
            )));
        }
        let mut contents = String::new();
        let mut file = std::fs::File::open(path)?;
        file.read_to_string(&mut contents)?;
        let mut ret = String::with_capacity(contents.len());

        for (i, l) in contents.lines().enumerate() {
            if let (_, Some(sub_path)) = include_directive().parse(l).map_err(|l| {
                Error::new(format!(
                    "Malformed include directive in line {} of file {}: {}\nConfiguration uses \
                     the standard m4 macro include(\"filename\").",
                    i,
                    path.display(),
                    l
                ))
            })? {
                let mut p = Path::new(sub_path).expand();
                if p.is_relative() {
                    /* We checked that path is ok above so we can do unwrap here */
                    let prefix = path.parent().unwrap();
                    p = prefix.join(p)
                }

                ret.push_str(&pp_helper(&p, level + 1)?);
            } else {
                ret.push_str(l);
                ret.push('\n');
            }
        }

        Ok(ret)
    }

    /// Expands `include` macros in configuration file and other configuration
    /// files (eg. themes) in the filesystem.
    pub(super) fn pp<P: AsRef<Path>>(path: P) -> Result<String> {
        let p_buf: PathBuf = if path.as_ref().is_relative() {
            path.as_ref().expand().canonicalize()?
        } else {
            path.as_ref().expand()
        };

        let mut ret = super::expand_config(p_buf)?;
        if let Ok(xdg_dirs) = xdg::BaseDirectories::with_prefix("meli") {
            for theme_mailbox in xdg_dirs.find_config_files("themes") {
                let read_dir = std::fs::read_dir(theme_mailbox)?;
                for theme in read_dir {
                    let file = theme?.path();
                    if let Some(extension) = file.extension() {
                        if extension == "toml" {
                            ret.push_str(&pp_helper(&file, 0)?);
                        }
                    }
                }
            }
        }
        Ok(ret)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct LogSettings {
    #[serde(default)]
    pub log_file: Option<PathBuf>,
    #[serde(default)]
    pub maximum_level: melib::LogLevel,
}

pub use dotaddressable::*;
mod dotaddressable {
    use super::*;
    pub trait DotAddressable: Serialize {
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
    impl DotAddressable for char {}
    impl DotAddressable for IndexStyle {}
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
    impl DotAddressable for SearchBackend {}
    impl DotAddressable for melib::SpecialUsageMailbox {}
    impl<T: DotAddressable> DotAddressable for Option<T> {}
    impl<T: DotAddressable> DotAddressable for Vec<T> {}
    impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash, V: DotAddressable> DotAddressable
        for HashMap<K, V>
    {
    }
    impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash, V: DotAddressable> DotAddressable
        for IndexMap<K, V>
    {
    }
    impl<K: DotAddressable + std::cmp::Eq + std::hash::Hash> DotAddressable for HashSet<K> {}
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
                        "listing" => Err(Error::new("unimplemented")), /* self.listing.lookup(field, tail), */
                        "notifications" => Err(Error::new("unimplemented")), /* self.notifications.lookup(field, tail), */
                        "shortcuts" => Err(Error::new("unimplemented")),     /* self.shortcuts. */
                        // lookup(field,
                        // tail),
                        "composing" => Err(Error::new("unimplemented")), /* self.composing.lookup(field, tail), */
                        "identity" => Err(Error::new("unimplemented")),  /* self.identity. */
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
}

#[cfg(test)]
pub mod tests {
    use std::{
        fmt::Write as FmtWrite,
        fs::{self, OpenOptions},
        io::Write,
        path::PathBuf,
    };

    use crate::conf::FileSettings;

    pub struct ConfigFile {
        pub path: PathBuf,
        pub file: fs::File,
    }

    impl ConfigFile {
        pub fn new(
            content: &str,
            dir: &tempfile::TempDir,
        ) -> std::result::Result<Self, std::io::Error> {
            let mut filename = String::with_capacity(2 * 16);
            for byte in melib::utils::random::random_u64().to_be_bytes() {
                write!(&mut filename, "{:02X}", byte).unwrap();
            }
            let mut path = dir.path().to_path_buf();
            path.push(&*filename);
            let mut file = OpenOptions::new()
                .create_new(true)
                .append(true)
                .open(&path)?;
            file.write_all(content.as_bytes())?;
            Ok(Self { path, file })
        }
    }

    impl Drop for ConfigFile {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.path);
        }
    }

    pub const TEST_CONFIG: &str = r#"
[accounts.account-name]
root_mailbox = "/path/to/root/mailbox"
format = "Maildir"
send_mail = 'false'
listing.index_style = "Conversations" # or [plain, threaded, compact]
identity="email@example.com"
display_name = "Name"
subscribed_mailboxes = ["INBOX", "INBOX/Sent", "INBOX/Drafts", "INBOX/Junk"]

# Set mailbox-specific settings
  [accounts.account-name.mailboxes]
  "INBOX" = { rename="Inbox" }
  "drafts" = { rename="Drafts" }
  "foobar-devel" = { ignore = true } # don't show notifications for this mailbox

# Setting up an mbox account
[accounts.mbox]
root_mailbox = "/var/mail/username"
format = "mbox"
send_mail = 'false'
listing.index_style = "Compact"
identity="username@hostname.local"
"#;

    pub const EXTRA_CONFIG: &str = r#"
[accounts.mbox]
root_mailbox = "/"
format = "mbox"
send_mail = 'false'
index_style = "Compact"
identity="username@hostname.local"
    "#;
    pub const IMAP_CONFIG: &str = r#"
[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password_command = "false"
    "#;

    #[test]
    fn test_config_parse() {
        let tempdir = tempfile::tempdir().unwrap();
        let new_file = ConfigFile::new(TEST_CONFIG, &tempdir).unwrap();
        let err = FileSettings::validate(new_file.path.clone(), true).unwrap_err();
        assert_eq!(
            err.summary.as_ref(),
            "Configuration error (account-name): root_mailbox `/path/to/root/mailbox` is not a \
             valid directory."
        );

        /* Test unrecognised configuration entries error */

        let new_file = ConfigFile::new(EXTRA_CONFIG, &tempdir).unwrap();
        let err = FileSettings::validate(new_file.path.clone(), true).unwrap_err();
        assert_eq!(
            err.summary.as_ref(),
            "Unrecognised configuration values: {\"index_style\": \"Compact\"}"
        );

        /* Test IMAP config */

        let new_file = ConfigFile::new(IMAP_CONFIG, &tempdir).unwrap();
        FileSettings::validate(new_file.path.clone(), true).expect("could not parse IMAP config");

        /* Test sample config */

        let example_config = FileSettings::EXAMPLE_CONFIG.replace("\n#", "\n");
        let re = regex::Regex::new(r#"root_mailbox\s*=\s*"[^"]*""#).unwrap();
        let example_config = re.replace_all(
            &example_config,
            &format!(r#"root_mailbox = "{}""#, tempdir.path().to_str().unwrap()),
        );

        let new_file = ConfigFile::new(&example_config, &tempdir).unwrap();
        let config = FileSettings::validate(new_file.path.clone(), true)
            .expect("Could not parse example config!");
        for (accname, acc) in config.accounts.iter() {
            if !acc.extra.is_empty() {
                panic!(
                    "In example config, account `{}` has unrecognised configuration entries: {:?}",
                    accname, acc.extra
                );
            }
        }
        if let Err(err) = tempdir.close() {
            eprintln!("Could not cleanup tempdir: {}", err);
        }
    }
}
