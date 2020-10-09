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

//! Settings for terminal display

use super::deserializers::non_empty_string;
use super::DotAddressable;
use super::Themes;
use melib::{MeliError, Result, ToggleFlag};

/// Settings for terminal display
#[derive(Debug, Deserialize, Clone, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct TerminalSettings {
    /// light, dark
    pub theme: String,
    pub themes: Themes,
    pub ascii_drawing: bool,
    pub use_color: ToggleFlag,
    #[serde(deserialize_with = "non_empty_string")]
    pub window_title: Option<String>,
    #[serde(deserialize_with = "non_empty_string")]
    pub file_picker_command: Option<String>,
}

impl Default for TerminalSettings {
    fn default() -> Self {
        TerminalSettings {
            theme: "dark".to_string(),
            themes: Themes::default(),
            ascii_drawing: false,
            use_color: ToggleFlag::InternalVal(true),
            window_title: Some("meli".to_string()),
            file_picker_command: None,
        }
    }
}

impl TerminalSettings {
    pub fn use_color(&self) -> bool {
        /* Don't use color if
         * - Either NO_COLOR is set and user hasn't explicitly set use_colors or
         * - User has explicitly set use_colors to false
         */
        !((std::env::var("NO_COLOR").is_ok()
            && (self.use_color.is_false() || self.use_color.is_internal()))
            || (self.use_color.is_false() && !self.use_color.is_internal()))
    }
}

impl DotAddressable for TerminalSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "theme" => self.theme.lookup(field, tail),
                    "themes" => Err(MeliError::new("unimplemented")),
                    "ascii_drawing" => self.ascii_drawing.lookup(field, tail),
                    "use_color" => self.use_color.lookup(field, tail),
                    "window_title" => self.window_title.lookup(field, tail),
                    "file_picker_command" => self.file_picker_command.lookup(field, tail),
                    other => Err(MeliError::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::to_string(self).map_err(|err| err.to_string())?),
        }
    }
}
