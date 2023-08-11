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

use melib::{Error, Result, ToggleFlag};

use super::{deserializers::non_empty_opt_string, DotAddressable, Themes};

/// Settings for terminal display
#[derive(Debug, Deserialize, Clone, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct TerminalSettings {
    /// light, dark
    pub theme: String,
    pub themes: Themes,
    pub ascii_drawing: bool,
    pub use_color: ToggleFlag,
    /// Use mouse events. This will disable text selection, but you will be able
    /// to resize some widgets.
    /// Default: False
    pub use_mouse: ToggleFlag,
    /// String to show in status bar if mouse is active.
    /// Default: "🖱️ "
    #[serde(deserialize_with = "non_empty_opt_string")]
    pub mouse_flag: Option<String>,
    #[serde(deserialize_with = "non_empty_opt_string")]
    pub window_title: Option<String>,
    #[serde(deserialize_with = "non_empty_opt_string")]
    pub file_picker_command: Option<String>,
    /// Choose between 30-something built in sequences (integers between 0-30)
    /// or define your own list of strings for the progress spinner
    /// animation. Default: 0
    #[serde(default)]
    pub progress_spinner_sequence: Option<ProgressSpinnerSequence>,
}

impl Default for TerminalSettings {
    fn default() -> Self {
        TerminalSettings {
            theme: "dark".to_string(),
            themes: Themes::default(),
            ascii_drawing: false,
            use_color: ToggleFlag::InternalVal(true),
            use_mouse: ToggleFlag::InternalVal(false),
            mouse_flag: Some("🖱️ ".to_string()),
            window_title: Some("meli".to_string()),
            file_picker_command: None,
            progress_spinner_sequence: None,
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
                    "themes" => Err(Error::new("unimplemented")),
                    "ascii_drawing" => self.ascii_drawing.lookup(field, tail),
                    "use_color" => self.use_color.lookup(field, tail),
                    "use_mouse" => self.use_mouse.lookup(field, tail),
                    "mouse_flag" => self.mouse_flag.lookup(field, tail),
                    "window_title" => self.window_title.lookup(field, tail),
                    "file_picker_command" => self.file_picker_command.lookup(field, tail),
                    "progress_spinner_sequence" => {
                        self.progress_spinner_sequence.lookup(field, tail)
                    }
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::to_string(self).map_err(|err| err.to_string())?),
        }
    }
}

#[derive(Debug, Deserialize, Clone, Serialize)]
#[serde(untagged)]
pub enum ProgressSpinnerSequence {
    Integer(usize),
    Custom {
        frames: Vec<String>,
        #[serde(default = "interval_ms_val")]
        interval_ms: u64,
    },
}

const fn interval_ms_val() -> u64 {
    crate::utilities::ProgressSpinner::INTERVAL_MS
}

impl DotAddressable for ProgressSpinnerSequence {}
