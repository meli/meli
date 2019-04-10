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

extern crate bincode;
extern crate config;
extern crate serde;
extern crate xdg;

pub mod mailer;
pub mod notifications;
pub mod pager;
pub mod shortcuts;

pub mod accounts;
pub use self::accounts::Account;
use self::config::{Config, File, FileFormat};
pub use self::mailer::*;
pub use self::shortcuts::*;

use self::default_vals::*;
use self::notifications::NotificationsSettings;
use melib::conf::AccountSettings;
use melib::error::*;
use pager::PagerSettings;

use self::serde::{de, Deserialize, Deserializer};
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;

#[macro_export]
macro_rules! split_command {
    ($cmd:expr) => {{
        $cmd.split_whitespace().collect::<Vec<&str>>()
    }};
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct FolderConf {
    rename: Option<String>,
    #[serde(default = "true_val")]
    autoload: bool,
    ignore: bool,
}

impl FolderConf {
    pub fn rename(&self) -> Option<&str> {
        self.rename.as_ref().map(|v| v.as_str())
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct FileAccount {
    root_folder: String,
    format: String,
    sent_folder: String,
    draft_folder: String,
    identity: String,

    #[serde(default = "none")]
    display_name: Option<String>,
    #[serde(deserialize_with = "index_from_str")]
    index: IndexStyle,

    /// A command to pipe html output before displaying it in a pager
    /// Default: None
    #[serde(default = "none", deserialize_with = "non_empty_string")]
    html_filter: Option<String>,

    folders: Option<HashMap<String, FolderConf>>,
}

impl From<FileAccount> for AccountConf {
    fn from(x: FileAccount) -> Self {
        let format = x.format.to_lowercase();
        let sent_folder = x.sent_folder.clone();
        let root_folder = x.root_folder.clone();
        let identity = x.identity.clone();
        let display_name = x.display_name.clone();

        let acc = AccountSettings {
            name: String::new(),
            root_folder,
            format,
            sent_folder,
            identity,
            display_name,
        };

        AccountConf {
            account: acc,
            conf: x,
        }
    }
}

impl FileAccount {
    pub fn folders(&self) -> Option<&HashMap<String, FolderConf>> {
        self.folders.as_ref()
    }
    pub fn folder(&self) -> &str {
        &self.root_folder
    }
    pub fn index(&self) -> IndexStyle {
        self.index
    }
    pub fn sent_folder(&self) -> &str {
        self.sent_folder.as_str()
    }
    pub fn html_filter(&self) -> Option<&str> {
        self.html_filter.as_ref().map(|f| f.as_str())
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
struct FileSettings {
    accounts: HashMap<String, FileAccount>,
    pager: PagerSettings,
    notifications: NotificationsSettings,
    shortcuts: Shortcuts,
    mailer: MailerSettings,
}

#[derive(Debug, Clone, Default)]
pub struct AccountConf {
    account: AccountSettings,
    conf: FileAccount,
}

impl AccountConf {
    pub fn account(&self) -> &AccountSettings {
        &self.account
    }
    pub fn conf(&self) -> &FileAccount {
        &self.conf
    }
    pub fn conf_mut(&mut self) -> &mut FileAccount {
        &mut self.conf
    }
}

#[derive(Debug, Clone, Default)]
pub struct Settings {
    pub accounts: HashMap<String, AccountConf>,
    pub pager: PagerSettings,
    pub notifications: NotificationsSettings,
    pub shortcuts: Shortcuts,
    pub mailer: MailerSettings,
}

impl FileSettings {
    pub fn new() -> Result<FileSettings> {
        let config_path = match env::var("MELI_CONFIG") {
            Ok(path) => PathBuf::from(path),
            Err(_) => {
                let xdg_dirs = xdg::BaseDirectories::with_prefix("meli").unwrap();
                xdg_dirs
                    .place_config_file("config")
                    .expect("cannot create configuration directory")
            }
        };
        if !config_path.exists() {
            panic!(
                "Config file path `{}` doesn't exist or can't be created.",
                config_path.display()
            );
        }
        let mut s = Config::new();
        let s = s.merge(File::new(config_path.to_str().unwrap(), FileFormat::Toml));

        /* No point in returning without a config file. */
        match s.unwrap().deserialize() {
            Ok(v) => Ok(v),
            Err(e) => Err(MeliError::new(e.to_string())),
        }
    }
}

impl Settings {
    pub fn new() -> Settings {
        let fs = FileSettings::new().unwrap_or_else(|e| panic!(format!("{}", e)));
        let mut s: HashMap<String, AccountConf> = HashMap::new();

        for (id, x) in fs.accounts {
            let mut ac = AccountConf::from(x);
            ac.account.set_name(id.clone());

            s.insert(id, ac);
        }

        Settings {
            accounts: s,
            pager: fs.pager,
            notifications: fs.notifications,
            shortcuts: fs.shortcuts,
            mailer: fs.mailer,
        }
    }
}

#[derive(Copy, Debug, Clone, Deserialize)]
pub enum IndexStyle {
    Plain,
    Threaded,
    Compact,
}

impl Default for IndexStyle {
    fn default() -> Self {
        IndexStyle::Compact
    }
}

fn index_from_str<'de, D>(deserializer: D) -> std::result::Result<IndexStyle, D::Error>
where
    D: Deserializer<'de>,
{
    let s = <String>::deserialize(deserializer)?;
    match s.as_str() {
        "Plain" | "plain" => Ok(IndexStyle::Plain),
        "Threaded" | "threaded" => Ok(IndexStyle::Threaded),
        "Compact" | "compact" => Ok(IndexStyle::Compact),
        _ => Err(de::Error::custom("invalid `index` value")),
    }
}

fn non_empty_string<'de, D>(deserializer: D) -> std::result::Result<Option<String>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = <String>::deserialize(deserializer)?;
    if s.is_empty() {
        Ok(None)
    } else {
        Ok(Some(s))
    }
}

/*
 * Deserialize default functions
 */

mod default_vals {
    pub(in conf) fn false_val() -> bool {
        true
    }

    pub(in conf) fn true_val() -> bool {
        true
    }

    pub(in conf) fn zero_val() -> usize {
        0
    }

    pub(in conf) fn eighty_percent() -> usize {
        80
    }

    pub(in conf) fn none() -> Option<String> {
        None
    }
}
