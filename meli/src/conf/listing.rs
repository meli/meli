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

use melib::{search::Query, Error, Result};

use super::{default_vals::*, DotAddressable, IndexStyle};

/// Settings for mail listings
///
///
/// Tree decoration examples:
///
///```no_run
/// const HAS_SIBLING: &str = " ┃";
/// const NO_SIBLING: &str = "  ";
/// const HAS_SIBLING_LEAF: &str = " ┣━";
/// const NO_SIBLING_LEAF: &str = " ┗━";
/// ```
///
///```no_run
/// const HAS_SIBLING: &str = " |";
/// const NO_SIBLING: &str = "  ";
/// const HAS_SIBLING_LEAF: &str = " |\\_";
/// const NO_SIBLING_LEAF: &str = " \\_";
/// ```
///
///```no_run
/// const HAS_SIBLING: &str = " ";
/// const NO_SIBLING: &str = " ";
/// const HAS_SIBLING_LEAF: &str = " ";
/// const NO_SIBLING_LEAF: &str = " ";
/// ```
///
///```no_run
/// const HAS_SIBLING: &str = " │";
/// const NO_SIBLING: &str = "  ";
/// const HAS_SIBLING_LEAF: &str = " ├─";
/// const NO_SIBLING_LEAF: &str = " ╰─";
/// ```
#[derive(Debug, Deserialize, Clone, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ListingSettings {
    /// Number of context lines when going to next page.
    /// Default: 0
    #[serde(default = "zero_val", alias = "context-lines")]
    pub context_lines: usize,

    /// Show auto-hiding scrollbar in accounts sidebar menu.
    /// Default: True
    #[serde(default = "true_val")]
    pub show_menu_scrollbar: bool,

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

    /// Default: " "
    #[serde(default = "none")]
    pub sidebar_mailbox_tree_has_sibling: Option<String>,

    /// Default: " "
    #[serde(default)]
    pub sidebar_mailbox_tree_no_sibling: Option<String>,

    /// Default: " "
    #[serde(default)]
    pub sidebar_mailbox_tree_has_sibling_leaf: Option<String>,

    /// Default: " "
    #[serde(default)]
    pub sidebar_mailbox_tree_no_sibling_leaf: Option<String>,

    /// Default: ' '
    #[serde(default = "default_divider")]
    pub sidebar_divider: char,

    /// Default: 90
    #[serde(default = "default_ratio")]
    pub sidebar_ratio: usize,

    /// Flag to show if thread entry contains unseen mail.
    /// Default: "●"
    #[serde(default)]
    pub unseen_flag: Option<String>,

    /// Flag to show if thread has been snoozed.
    /// Default: "💤"
    #[serde(default)]
    pub thread_snoozed_flag: Option<String>,

    /// Flag to show if thread entry has been selected.
    /// Default: "☑️"
    #[serde(default)]
    pub selected_flag: Option<String>,

    /// Flag to show if thread entry contains attachments.
    /// Default: "📎"
    #[serde(default)]
    pub attachment_flag: Option<String>,

    /// Should threads with different Subjects show a list of those
    /// subjects on the entry title?
    /// Default: "true"
    #[serde(default = "true_val")]
    pub thread_subject_pack: bool,

    /// In threaded listing style, repeat identical From column values within a
    /// thread. Not repeating adds empty space in the From column which
    /// might result in less visual clutter.
    /// Default: "false"
    #[serde(default = "false_val")]
    pub threaded_repeat_identical_from_values: bool,

    /// Show relative indices in menu mailboxes to quickly help with jumping to
    /// them. Default: "true"
    #[serde(default = "true_val", alias = "relative-menu-indices")]
    pub relative_menu_indices: bool,
}

const fn default_divider() -> char {
    ' '
}

const fn default_ratio() -> usize {
    90
}

impl Default for ListingSettings {
    fn default() -> Self {
        Self {
            context_lines: 0,
            show_menu_scrollbar: true,
            datetime_fmt: None,
            recent_dates: true,
            filter: None,
            index_style: IndexStyle::default(),
            sidebar_mailbox_tree_has_sibling: None,
            sidebar_mailbox_tree_no_sibling: None,
            sidebar_mailbox_tree_has_sibling_leaf: None,
            sidebar_mailbox_tree_no_sibling_leaf: None,
            sidebar_divider: default_divider(),
            sidebar_ratio: 90,
            unseen_flag: None,
            thread_snoozed_flag: None,
            selected_flag: None,
            attachment_flag: None,
            thread_subject_pack: true,
            threaded_repeat_identical_from_values: false,
            relative_menu_indices: true,
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
                    "show_menu_scrollbar" => self.show_menu_scrollbar.lookup(field, tail),
                    "datetime_fmt" => self.datetime_fmt.lookup(field, tail),
                    "recent_dates" => self.recent_dates.lookup(field, tail),
                    "filter" => self.filter.lookup(field, tail),
                    "index_style" => self.index_style.lookup(field, tail),
                    "sidebar_mailbox_tree_has_sibling" => {
                        self.sidebar_mailbox_tree_has_sibling.lookup(field, tail)
                    }
                    "sidebar_mailbox_tree_no_sibling" => {
                        self.sidebar_mailbox_tree_no_sibling.lookup(field, tail)
                    }
                    "sidebar_mailbox_tree_has_sibling_leaf" => self
                        .sidebar_mailbox_tree_has_sibling_leaf
                        .lookup(field, tail),
                    "sidebar_mailbox_tree_no_sibling_leaf" => self
                        .sidebar_mailbox_tree_no_sibling_leaf
                        .lookup(field, tail),
                    "sidebar_divider" => self.sidebar_divider.lookup(field, tail),
                    "sidebar_ratio" => self.sidebar_ratio.lookup(field, tail),
                    "unseen_flag" => self.unseen_flag.lookup(field, tail),
                    "thread_snoozed_flag" => self.thread_snoozed_flag.lookup(field, tail),
                    "selected_flag" => self.selected_flag.lookup(field, tail),
                    "attachment_flag" => self.attachment_flag.lookup(field, tail),
                    "thread_subject_pack" => self.thread_subject_pack.lookup(field, tail),
                    "threaded_repeat_identical_from_values" => self
                        .threaded_repeat_identical_from_values
                        .lookup(field, tail),
                    "relative_menu_indices" => self.relative_menu_indices.lookup(field, tail),
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
