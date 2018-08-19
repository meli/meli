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
use pager::PagerSettings;

use std::collections::HashMap;

#[derive(Debug, Deserialize)]
pub struct FileAccount {
    root_folder: String,
    format: String,
    sent_folder: String,
    threaded: bool,
    folders: Option<HashMap<String, String>>,
}

impl FileAccount {
    pub fn folder(&self) -> &str {
        &self.root_folder
    }
}

#[derive(Debug, Deserialize)]
struct FileSettings {
    accounts: HashMap<String, FileAccount>,
    pager: PagerSettings,
}

#[derive(Debug, Clone, Default)]
pub struct Settings {
    pub accounts: HashMap<String, AccountSettings>,
    pub pager: PagerSettings,
}

use self::config::{Config, File, FileFormat};
impl FileSettings {
    pub fn new() -> FileSettings {
        let xdg_dirs = xdg::BaseDirectories::with_prefix("meli").unwrap();
        let config_path = xdg_dirs
            .place_config_file("config")
            .expect("cannot create configuration directory");
        //let setts = Config::default().merge(File::new(config_path.to_str().unwrap_or_default(), config::FileFormat::Toml)).unwrap();
        let mut s = Config::new();
        let s = s.merge(File::new(config_path.to_str().unwrap(), FileFormat::Toml));

        // No point in returning without a config file.
        // TODO: Error and exit instead of panic.
        s.unwrap().deserialize().unwrap()
    }
}

impl Settings {
    pub fn new() -> Settings {
        let fs = FileSettings::new();
        let mut s: HashMap<String, AccountSettings> = HashMap::new();

        for (id, x) in fs.accounts {
            let format = x.format.to_lowercase();
            let sent_folder = x.sent_folder;
            let threaded = x.threaded;
            let root_folder = x.root_folder;

            let acc = AccountSettings {
                name: id.clone(),
                root_folder,
                format,
                sent_folder,
                threaded,
            };

            s.insert(id, acc);
        }

        Settings {
            accounts: s,
            pager: fs.pager,
        }
    }
}
