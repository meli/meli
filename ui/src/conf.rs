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
extern crate serde;
extern crate toml;
extern crate xdg;

pub mod mailer;
pub mod notifications;
pub mod pager;
pub mod shortcuts;

pub mod accounts;
pub use self::accounts::Account;
pub use self::mailer::*;
pub use self::shortcuts::*;

use self::default_vals::*;
use self::notifications::NotificationsSettings;
use crate::pager::PagerSettings;
use melib::backends::SpecialUseMailbox;
use melib::conf::AccountSettings;
use melib::error::*;

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use std::collections::HashMap;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, Read, Write};
use std::path::PathBuf;

#[macro_export]
macro_rules! split_command {
    ($cmd:expr) => {{
        $cmd.split_whitespace().collect::<Vec<&str>>()
    }};
}

#[derive(Debug, Clone, PartialEq)]
pub enum ToggleFlag {
    Unset,
    InternalVal(bool),
    False,
    True,
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

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct MailUIConf {
    pub pager: Option<PagerSettings>,
    pub notifications: Option<NotificationsSettings>,
    pub shortcuts: Option<Shortcuts>,
    pub mailer: Option<MailerSettings>,
    pub identity: Option<String>,
    pub index: Option<IndexStyle>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FolderConf {
    pub rename: Option<String>,
    #[serde(default = "true_val")]
    pub autoload: bool,
    #[serde(deserialize_with = "toggleflag_de", default)]
    pub subscribe: ToggleFlag,
    #[serde(deserialize_with = "toggleflag_de", default)]
    pub ignore: ToggleFlag,
    #[serde(default = "none")]
    pub usage: Option<SpecialUseMailbox>,
    #[serde(flatten)]
    pub conf_override: MailUIConf,
}

impl Default for FolderConf {
    fn default() -> Self {
        FolderConf {
            rename: None,
            autoload: true,
            subscribe: ToggleFlag::Unset,
            ignore: ToggleFlag::Unset,
            usage: None,
            conf_override: MailUIConf::default(),
        }
    }
}

impl FolderConf {
    pub fn rename(&self) -> Option<&str> {
        self.rename.as_ref().map(String::as_str)
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FileAccount {
    root_folder: String,
    format: String,
    sent_folder: String,
    draft_folder: String,
    identity: String,
    #[serde(flatten)]
    pub extra: HashMap<String, String>,

    #[serde(default = "none")]
    display_name: Option<String>,
    index: IndexStyle,

    /// A command to pipe html output before displaying it in a pager
    /// Default: None
    #[serde(default = "none", deserialize_with = "non_empty_string")]
    html_filter: Option<String>,

    #[serde(default = "false_val")]
    read_only: bool,
    subscribed_folders: Vec<String>,
    folders: Option<HashMap<String, FolderConf>>,
}

impl From<FileAccount> for AccountConf {
    fn from(x: FileAccount) -> Self {
        let format = x.format.to_lowercase();
        let sent_folder = x.sent_folder.clone();
        let root_folder = x.root_folder.clone();
        let identity = x.identity.clone();
        let display_name = x.display_name.clone();

        let mut acc = AccountSettings {
            name: String::new(),
            root_folder,
            format,
            sent_folder,
            identity,
            read_only: x.read_only,
            display_name,
            subscribed_folders: x.subscribed_folders.clone(),
            extra: x.extra.clone(),
        };

        let root_path = PathBuf::from(acc.root_folder.as_str());
        let root_tmp = root_path
            .components()
            .last()
            .unwrap()
            .as_os_str()
            .to_str()
            .unwrap()
            .to_string();
        if !acc.subscribed_folders.contains(&root_tmp) {
            acc.subscribed_folders.push(root_tmp);
        }
        let mut folder_confs = x.folders.clone().unwrap_or_else(Default::default);
        for s in &x.subscribed_folders {
            if !folder_confs.contains_key(s) {
                folder_confs.insert(
                    s.to_string(),
                    FolderConf {
                        subscribe: ToggleFlag::True,
                        ..FolderConf::default()
                    },
                );
            } else {
                if !folder_confs[s].subscribe.is_unset() {
                    eprintln!("Configuration error: folder `{}` cannot both have `subscribe` flag set and be in the `subscribed_folders` array", s);
                    std::process::exit(1);
                }
                folder_confs.get_mut(s).unwrap().subscribe = ToggleFlag::True;
            }

            if folder_confs[s].usage.is_none() {
                let name = s
                    .split(if s.contains('/') { '/' } else { '.' })
                    .last()
                    .unwrap_or("");
                folder_confs.get_mut(s).unwrap().usage = if name.eq_ignore_ascii_case("inbox") {
                    Some(SpecialUseMailbox::Inbox)
                } else if name.eq_ignore_ascii_case("archive") {
                    Some(SpecialUseMailbox::Archive)
                } else if name.eq_ignore_ascii_case("drafts") {
                    Some(SpecialUseMailbox::Drafts)
                } else if name.eq_ignore_ascii_case("junk") {
                    Some(SpecialUseMailbox::Junk)
                } else if name.eq_ignore_ascii_case("spam") {
                    Some(SpecialUseMailbox::Junk)
                } else if name.eq_ignore_ascii_case("sent") {
                    Some(SpecialUseMailbox::Sent)
                } else if name.eq_ignore_ascii_case("trash") {
                    Some(SpecialUseMailbox::Trash)
                } else {
                    Some(SpecialUseMailbox::Normal)
                };
            }

            if folder_confs[s].ignore.is_unset() {
                use SpecialUseMailbox::*;
                if [Junk, Sent, Trash].contains(&folder_confs[s].usage.as_ref().unwrap()) {
                    folder_confs.get_mut(s).unwrap().ignore = ToggleFlag::InternalVal(true);
                }
            }
        }

        AccountConf {
            account: acc,
            conf: x,
            folder_confs,
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
        self.html_filter.as_ref().map(String::as_str)
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct FileSettings {
    accounts: HashMap<String, FileAccount>,
    #[serde(default)]
    pager: PagerSettings,
    #[serde(default)]
    notifications: NotificationsSettings,
    #[serde(default)]
    shortcuts: Shortcuts,
    mailer: MailerSettings,
}

#[derive(Debug, Clone, Default)]
pub struct AccountConf {
    pub(crate) account: AccountSettings,
    pub(crate) conf: FileAccount,
    pub(crate) folder_confs: HashMap<String, FolderConf>,
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
            println!(
                "No configuration found. Would you like to generate one in {}? [Y/n]",
                config_path.display()
            );
            let mut buffer = String::new();
            let stdin = io::stdin();
            let mut handle = stdin.lock();

            loop {
                buffer.clear();
                handle
                    .read_line(&mut buffer)
                    .expect("Could not read from stdin.");

                match buffer.trim() {
                    "Y" | "y" | "yes" | "YES" | "Yes" => {
                        let mut file = OpenOptions::new()
                            .write(true)
                            .create_new(true)
                            .open(config_path.as_path())
                            .expect("Could not create config file.");
                        file.write_all(include_bytes!("../../sample-config"))
                            .expect("Could not write to config file.");
                        println!("Written config to {}", config_path.display());
                        std::process::exit(1);
                    }
                    "n" | "N" | "no" | "No" | "NO" => {
                        std::process::exit(1);
                    }
                    _ => {
                        println!(
                            "No configuration found. Would you like to generate one in {}? [Y/n]",
                            config_path.display()
                        );
                    }
                }
            }
        }

        let mut file = File::open(config_path.to_str().unwrap())?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let s = toml::from_str(&contents);
        if let Err(e) = s {
            eprintln!("Config file contains errors: {}", e.to_string());
            std::process::exit(1);
        }

        Ok(s.unwrap())
    }
}

impl Settings {
    pub fn new() -> Settings {
        let fs = FileSettings::new().unwrap_or_else(|e| {
            eprintln!("Configuration error: {}", e);
            std::process::exit(1);
        });
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

#[derive(Copy, Debug, Clone, Hash, PartialEq)]
pub enum IndexStyle {
    Plain,
    Threaded,
    Compact,
    Conversations,
}

impl Default for IndexStyle {
    fn default() -> Self {
        IndexStyle::Compact
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

fn toggleflag_de<'de, D>(deserializer: D) -> std::result::Result<ToggleFlag, D::Error>
where
    D: Deserializer<'de>,
{
    let s = <bool>::deserialize(deserializer);
    Ok(match s {
        Err(_) => ToggleFlag::Unset,
        Ok(true) => ToggleFlag::True,
        Ok(false) => ToggleFlag::False,
    })
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

/*
 * Deserialize default functions
 */

mod default_vals {
    pub(in crate::conf) fn false_val() -> bool {
        false
    }

    pub(in crate::conf) fn true_val() -> bool {
        true
    }

    pub(in crate::conf) fn zero_val() -> usize {
        0
    }

    pub(in crate::conf) fn eighty_percent() -> usize {
        80
    }

    pub(in crate::conf) fn none<T>() -> Option<T> {
        None
    }

    pub(in crate::conf) fn internal_value_false() -> super::ToggleFlag {
        super::ToggleFlag::InternalVal(false)
    }
}

impl<'de> Deserialize<'de> for IndexStyle {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            "Plain" | "plain" => Ok(IndexStyle::Plain),
            "Threaded" | "threaded" => Ok(IndexStyle::Threaded),
            "Compact" | "compact" => Ok(IndexStyle::Compact),
            "Conversations" | "conversations" => Ok(IndexStyle::Conversations),
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
            IndexStyle::Plain => serializer.serialize_str("plain"),
            IndexStyle::Threaded => serializer.serialize_str("threaded"),
            IndexStyle::Compact => serializer.serialize_str("compact"),
            IndexStyle::Conversations => serializer.serialize_str("conversations"),
        }
    }
}
