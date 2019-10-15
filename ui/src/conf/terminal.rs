/*
 * meli - configuration module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use super::deserializers::non_empty_string;

/// Settings for terminal display
#[derive(Debug, Deserialize, Clone, Serialize)]
#[serde(default)]
pub struct TerminalSettings {
    /// light, dark
    pub theme: String,
    pub ascii_drawing: bool,
    #[serde(deserialize_with = "non_empty_string")]
    pub window_title: Option<String>,
}

impl Default for TerminalSettings {
    fn default() -> Self {
        TerminalSettings {
            theme: "dark".to_string(),
            ascii_drawing: false,
            window_title: Some("meli".to_string()),
        }
    }
}
