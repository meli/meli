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
extern crate xdg;
extern crate serde;

use std::collections::HashMap;
use std::io;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize)]
struct FileAccount {
    folders: String,
    format: String,
    sent_folder: String,
    threaded: bool,
}

#[derive(Debug, Deserialize, Default)]
struct FileSettings {
    accounts: HashMap<String, FileAccount>,
}

#[derive(Debug, Clone)]
pub struct AccountSettings {
    pub folders: Vec<String>,
    format: String,
    pub sent_folder: String,
    threaded: bool,
}

impl AccountSettings {
    pub fn get_format(&self) -> &str {
        &self.format
    }
}

#[derive(Debug)]
pub struct Settings {
    pub accounts: HashMap<String, AccountSettings>,
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

        if s.is_ok() {
            s.unwrap().deserialize().unwrap()
        } else {
            eprintln!("{:?}", s.err().unwrap());
            let mut buf = String::new();
            io::stdin()
                .read_line(&mut buf)
                .expect("Failed to read line");
            FileSettings {
                ..Default::default()
            }
        }
    }
}

impl Settings {
    pub fn new() -> Settings {
        let fs = FileSettings::new();
        let mut s: HashMap<String, AccountSettings> = HashMap::new();

        for (id, x) in fs.accounts {
            let mut folders = Vec::new();
            fn recurse_folders<P: AsRef<Path>>(folders: &mut Vec<String>, p: P) {
                for mut f in fs::read_dir(p).unwrap() {
                    for f in f.iter_mut() {
                        {
                            let path = f.path();
                            if path.ends_with("cur") || path.ends_with("new") ||
                                path.ends_with("tmp")
                            {
                                continue;
                            }
                            if path.is_dir() {
                                folders.push(path.to_str().unwrap().to_string());
                                recurse_folders(folders, path);
                            }
                        }
                    }
                }
            };
            let path = PathBuf::from(&x.folders);
            if path.is_dir() {
                folders.push(path.to_str().unwrap().to_string());
            }
            recurse_folders(&mut folders, &x.folders);
            s.insert(
                id.clone(),
                AccountSettings {
                    folders: folders,
                    format: x.format.to_lowercase(),
                    sent_folder: x.sent_folder.clone(),
                    threaded: x.threaded,
                },
            );
        }

        Settings { accounts: s }
    }
}
