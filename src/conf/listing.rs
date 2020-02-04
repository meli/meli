/*
 * meli - listing conf module
 *
 * Copyright 2020 Manos Pitsidianakis
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

use super::default_vals::*;

/// Settings for mail listings
#[derive(Debug, Deserialize, Clone, Default, Serialize)]
pub struct ListingSettings {
    /// Number of context lines when going to next page.
    /// Default: 0
    #[serde(default = "zero_val")]
    pub context_lines: usize,

    /// Datetime formatting passed verbatim to strftime(3).
    /// Default: %Y-%m-%d %T
    #[serde(default = "none")]
    pub datetime_fmt: Option<String>,

    /// Show recent dates as `X {minutes,hours,days} ago`, up to 7 days.
    /// Default: true
    #[serde(default = "true_val")]
    pub recent_dates: bool,
}
