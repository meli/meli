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

#[derive(Debug, Clone)]
pub struct AccountSettings {
    pub name: String,
    pub root_folder: String,
    pub format: String,
    pub sent_folder: String,
    pub threaded: bool,
}

impl AccountSettings {
    pub fn format(&self) -> &str {
        &self.format
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn root_folder(&self) -> &str {
        &self.root_folder
    }
}
