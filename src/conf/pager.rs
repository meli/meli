/*
 * meli - pager conf module
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

//! Settings for the pager function.

use super::default_vals::*;
use super::deserializers::*;
use melib::ToggleFlag;

/// Settings for the pager function.
#[derive(Debug, Deserialize, Clone, Serialize)]
pub struct PagerSettings {
    /// Number of context lines when going to next page.
    /// Default: 0
    #[serde(default = "zero_val", alias = "pager-context")]
    pub pager_context: usize,

    /// Stop at the end instead of displaying next mail.
    /// Default: false
    #[serde(default = "false_val", alias = "pager-stop")]
    pub pager_stop: bool,

    /// Always show headers when scrolling.
    /// Default: true
    #[serde(default = "true_val", alias = "headers-sticky")]
    pub headers_sticky: bool,

    /// The height of the pager in mail view, in percent.
    /// Default: 80
    #[serde(default = "eighty_val", alias = "pager-ratio")]
    pub pager_ratio: usize,

    /// A command to pipe mail output through for viewing in pager.
    /// Default: None
    #[serde(default = "none", deserialize_with = "non_empty_string")]
    pub filter: Option<String>,

    /// A command to pipe html output before displaying it in a pager
    /// Default: None
    #[serde(
        default = "none",
        deserialize_with = "non_empty_string",
        alias = "html-filter"
    )]
    pub html_filter: Option<String>,

    /// Respect "format=flowed"
    /// Default: true
    #[serde(default = "true_val", alias = "format-flowed")]
    pub format_flowed: bool,

    /// Split long lines that would overflow on the x axis.
    /// Default: true
    #[serde(default = "true_val", alias = "split-long-lines")]
    pub split_long_lines: bool,

    /// Minimum text width in columns.
    /// Default: 80
    #[serde(default = "eighty_val", alias = "minimum-width")]
    pub minimum_width: usize,

    /// Choose `text/html` alternative if `text/plain` is empty in `multipart/alternative`
    /// attachments.
    /// Default: true
    #[serde(
        default = "internal_value_true",
        alias = "auto-choose-multipart-alternative"
    )]
    pub auto_choose_multipart_alternative: ToggleFlag,
}

impl Default for PagerSettings {
    fn default() -> Self {
        Self {
            pager_context: 0,
            pager_stop: false,
            headers_sticky: true,
            pager_ratio: 80,
            filter: None,
            html_filter: None,
            format_flowed: true,
            split_long_lines: true,
            minimum_width: 80,
            auto_choose_multipart_alternative: ToggleFlag::InternalVal(true),
        }
    }
}
