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

use super::{default_vals::*, DotAddressable, IndexStyle};
use melib::search::Query;
use melib::{MeliError, Result};

/// Settings for mail listings
#[derive(Debug, Deserialize, Clone, Serialize)]
pub struct ListingSettings {
    /// Number of context lines when going to next page.
    /// Default: 0
    #[serde(default = "zero_val", alias = "context-lines")]
    pub context_lines: usize,

    /// Datetime formatting passed verbatim to strftime(3).
    /// Default: %Y-%m-%d %T
    #[serde(default = "none", alias = "datetime-fmt")]
    pub datetime_fmt: Option<String>,

    /// Show recent dates as `X {minutes,hours,days} ago`, up to 7 days.
    /// Default: true
    #[serde(default = "true_val", alias = "recent-dates")]
    pub recent_dates: bool,

    /// Show only envelopes that match this query
    /// Default: None
    #[serde(default = "none")]
    pub filter: Option<Query>,

    #[serde(default, alias = "index-style")]
    pub index_style: IndexStyle,
}

impl Default for ListingSettings {
    fn default() -> Self {
        Self {
            context_lines: 0,
            datetime_fmt: None,
            recent_dates: true,
            filter: None,
            index_style: IndexStyle::default(),
        }
    }
}

impl DotAddressable for ListingSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "context_lines" => self.context_lines.lookup(field, tail),
                    "datetime_fmt" => self.datetime_fmt.lookup(field, tail),
                    "recent_dates" => self.recent_dates.lookup(field, tail),
                    "filter" => self.filter.lookup(field, tail),
                    "index_style" => self.index_style.lookup(field, tail),
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
