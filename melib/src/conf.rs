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
use std::collections::hash_map::HashMap;

#[derive(Debug, Serialize, Default, Clone)]
pub struct AccountSettings {
    pub name: String,
    pub root_folder: String,
    pub format: String,
    pub identity: String,
    pub read_only: bool,
    pub display_name: Option<String>,
    pub subscribed_folders: Vec<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, String>,
}

impl AccountSettings {
    pub fn format(&self) -> &str {
        &self.format
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn set_name(&mut self, s: String) {
        self.name = s;
    }
    pub fn root_folder(&self) -> &str {
        &self.root_folder
    }
    pub fn identity(&self) -> &str {
        &self.identity
    }
    pub fn read_only(&self) -> bool {
        self.read_only
    }
    pub fn display_name(&self) -> Option<&String> {
        self.display_name.as_ref()
    }

    pub fn subscribed_folders(&self) -> &Vec<String> {
        &self.subscribed_folders
    }
}
