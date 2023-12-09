/*
 * meli - notifications conf module
 *
 * Copyright 2018 Manos Pitsidianakis
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

use melib::{Error, Result, ToggleFlag};

use super::{
    default_vals::{internal_value_false, none, true_val},
    DotAddressable,
};

/// Settings for the notifications function.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct NotificationsSettings {
    /// Enable notifications.
    /// Default: True
    #[serde(default = "true_val")]
    pub enable: bool,

    /// A command to pipe notifications through.
    /// Default: None
    #[serde(default = "none")]
    pub script: Option<String>,

    /// A command to pipe new mail notifications through (preferred over
    /// `script`). Default: None
    #[serde(default = "none")]
    pub new_mail_script: Option<String>,

    /// A file location which has its size changed when new mail arrives (max
    /// 128 bytes). Can be used to trigger new mail notifications eg with
    /// `xbiff(1)`. Default: None
    #[serde(default = "none", alias = "xbiff-file-path")]
    pub xbiff_file_path: Option<String>,

    #[serde(default = "internal_value_false", alias = "play-sound")]
    pub play_sound: ToggleFlag,

    #[serde(default = "none", alias = "sound-file")]
    pub sound_file: Option<String>,
}

impl Default for NotificationsSettings {
    fn default() -> Self {
        Self {
            enable: true,
            script: None,
            new_mail_script: None,
            xbiff_file_path: None,
            play_sound: ToggleFlag::InternalVal(false),
            sound_file: None,
        }
    }
}
impl DotAddressable for NotificationsSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "enable" => self.enable.lookup(field, tail),
                    "script" => self.script.lookup(field, tail),
                    "new_mail_script" => self.new_mail_script.lookup(field, tail),
                    "xbiff_file_path" => self.xbiff_file_path.lookup(field, tail),
                    "play_sound" => self.play_sound.lookup(field, tail),
                    "sound_file" => self.sound_file.lookup(field, tail),
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
