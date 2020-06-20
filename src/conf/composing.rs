/*
 * meli - conf module
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

//! Configuration for composing email.
use super::default_vals::{false_val, none, true_val};
use std::collections::HashMap;

/// Settings for writing and sending new e-mail
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ComposingSettings {
    /// A command to pipe new emails to
    /// Required
    #[serde(alias = "mailer-cmd")]
    pub mailer_cmd: String,
    /// Command to launch editor. Can have arguments. Draft filename is given as the last argument. If it's missing, the environment variable $EDITOR is looked up.
    #[serde(default = "none", alias = "editor-cmd")]
    pub editor_cmd: Option<String>,
    /// Embed editor (for terminal interfaces) instead of forking and waiting.
    #[serde(default = "false_val")]
    pub embed: bool,
    /// Set "format=flowed" in plain text attachments.
    /// Default: true
    #[serde(default = "true_val", alias = "format-flowed")]
    pub format_flowed: bool,
    /// Set default header values for new drafts
    /// Default: empty
    #[serde(default, alias = "default-header-values")]
    pub default_header_values: HashMap<String, String>,
}

impl Default for ComposingSettings {
    fn default() -> Self {
        ComposingSettings {
            mailer_cmd: String::new(),
            editor_cmd: None,
            embed: false,
            format_flowed: true,
            default_header_values: HashMap::default(),
        }
    }
}
