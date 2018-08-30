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

extern crate config;
extern crate serde;
extern crate xdg;
pub mod pager;

use melib::conf::AccountSettings;
use melib::error::*;
use pager::PagerSettings;

use std::collections::HashMap;

#[derive(Debug, Clone, Default, Deserialize)]
pub struct FileAccount {
    root_folder: String,
    format: String,
    sent_folder: String,
    identity: String,
    display_name: Option<String>,
    threaded: bool,
    folders: Option<HashMap<String, String>>,
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
    pub fn folders(&self) -> Option<&HashMap<String, String>> {
        self.folders.as_ref()
    }
    pub fn folder(&self) -> &str {
        &self.root_folder
    }
    pub fn threaded(&self) -> bool {
        self.threaded
    }
    pub fn toggle_threaded(&mut self) {
        self.threaded = !self.threaded;
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
struct FileSettings {
    accounts: HashMap<String, FileAccount>,
    pager: PagerSettings,
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
}

use self::config::{Config, File, FileFormat};
impl FileSettings {
    pub fn new() -> Result<FileSettings> {
        let xdg_dirs = xdg::BaseDirectories::with_prefix("meli").unwrap();
        let config_path = xdg_dirs
            .place_config_file("config")
            .expect("cannot create configuration directory");
        let mut s = Config::new();
        let s = s.merge(File::new(config_path.to_str().unwrap(), FileFormat::Toml));

        /* No point in returning without a config file.
           TODO: Error and exit instead of panic. */
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
        }
    }
}
