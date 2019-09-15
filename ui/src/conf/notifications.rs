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

use super::default_vals::internal_value_false;
use super::toggleflag_de;

fn none() -> Option<String> {
    None
}

/// Settings for the notifications function.
#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct NotificationsSettings {
    /// A command to pipe notifications through
    /// Default: None
    #[serde(default = "none")]
    pub script: Option<String>,
    /// A file location which has its size changed when new mail arrives (max 128 bytes). Can be
    /// used to trigger new mail notifications eg with `xbiff(1)`
    /// Default: None
    #[serde(default = "none")]
    pub xbiff_file_path: Option<String>,
    #[serde(deserialize_with = "toggleflag_de", default = "internal_value_false")]
    pub play_sound: super::ToggleFlag,
    #[serde(default = "none")]
    pub sound_file: Option<String>,
}
