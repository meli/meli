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

use indexmap::IndexMap;
use melib::{Error, Result};

use super::DotAddressable;
use crate::terminal::Key;

#[macro_export]
macro_rules! shortcut {
    ($key:ident == $shortcuts:ident[$section:expr][$val:literal]) => {
        $shortcuts
            .get($section)
            .and_then(|s| s.get($val).map(|v| v == $key))
            .unwrap_or(false)
    };
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Shortcuts {
    #[serde(default)]
    pub general: GeneralShortcuts,
    #[serde(default)]
    pub listing: ListingShortcuts,
    #[serde(default)]
    pub composing: ComposingShortcuts,
    #[serde(default, alias = "contact-list")]
    pub contact_list: ContactListShortcuts,
    #[serde(default, alias = "envelope-view")]
    pub envelope_view: EnvelopeViewShortcuts,
    #[serde(default, alias = "thread-view")]
    pub thread_view: ThreadViewShortcuts,
    #[serde(default)]
    pub pager: PagerShortcuts,
}

impl Shortcuts {
    pub const GENERAL: &'static str = "general";
    pub const LISTING: &'static str = "listing";
    pub const COMPOSING: &'static str = "composing";
    pub const CONTACT_LIST: &'static str = "contact_list";
    pub const ENVELOPE_VIEW: &'static str = "envelope_view";
    pub const THREAD_VIEW: &'static str = "thread_view";
    pub const PAGER: &'static str = "pager";
}

impl DotAddressable for Shortcuts {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "general" => self.general.lookup(field, tail),
                    "listing" => self.listing.lookup(field, tail),
                    "composing" => self.composing.lookup(field, tail),
                    "contact_list" | "contact-list" => self.contact_list.lookup(field, tail),
                    "envelope_view" | "envelope-view" => self.envelope_view.lookup(field, tail),
                    "thread_view" | "thread-view" => self.thread_view.lookup(field, tail),
                    "pager" => self.pager.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CommandShortcut {
    pub shortcut: Key,
    pub command: Vec<String>,
}

/// Create a struct holding all of a Component's shortcuts.
#[macro_export]
macro_rules! shortcut_key_values {
    (
        $cname:expr,
        $(#[$outer:meta])*
        pub struct $name:ident { $($fname:ident |> $fdesc:literal |> $default:expr),* }) => {
        $(#[$outer])*
        #[derive(Debug, Clone, Serialize, Deserialize)]
        #[serde(default)]
        #[serde(rename = $cname)]
        pub struct $name {
            pub commands: Vec<CommandShortcut>,
            $(pub $fname : Key),*
        }

        impl $name {
            /// Returns a shortcut's description
            pub fn key_desc(&self, key: &str) -> &'static str {
                match key {
                    $(stringify!($fname) => $fdesc),*,
                        _ => unreachable!()
                }
            }
            /// Returns a hashmap of all shortcuts and their values
            pub fn key_values(&self) -> IndexMap<&'static str, Key> {
                [
                $((stringify!($fname),(self.$fname).clone()),)*
                ].iter().cloned().collect()
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self {
                    commands : vec![],
                    $($fname: $default),*
                }
            }
        }

        impl DotAddressable for $name {
            fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
                match path.first() {
                    Some(field) => {
                        let tail = &path[1..];
                        match *field {
                            $(stringify!($fname) => self.$fname.lookup(field, tail),)*
                            other => Err(Error::new(format!(
                                        "{} has no field named {}",
                                        parent_field, other
                            ))),
                        }
                    }
                    None => Ok(toml::Value::try_from(self).map_err(|err| err.to_string())?.to_string()),
                }
            }
        }
    }
}

shortcut_key_values! { "listing",
    /// Shortcut listing for a mail listing.
    pub struct ListingShortcuts {
        scroll_up |> "Scroll up list." |> Key::Char('k'),
        scroll_down |> "Scroll down list." |> Key::Char('j'),
        new_mail |> "Start new mail draft in new tab." |>  Key::Char('m'),
        next_account |> "Go to next account." |> Key::Char('H'),
        next_mailbox |> "Go to next mailbox." |> Key::Char('J'),
        next_page |> "Go to next page." |> Key::PageDown,
        prev_account |> "Go to previous account." |> Key::Char('L'),
        prev_mailbox |> "Go to previous mailbox." |> Key::Char('K'),
        open_mailbox |> "Open selected mailbox." |> Key::Char('\n'),
        toggle_mailbox_collapse |> "Toggle mailbox collapse in menu." |> Key::Char(' '),
        prev_page |> "Go to previous page." |> Key::PageUp,
        search |> "Search within list of e-mails." |> Key::Char('/'),
        refresh |> "Manually request a mailbox refresh." |> Key::F(5),
        set_seen |> "Set thread as seen." |> Key::Char('n'),
        union_modifier |> "Union modifier." |> Key::Ctrl('u'),
        diff_modifier |> "Difference modifier." |> Key::Ctrl('d'),
        intersection_modifier |> "Intersection modifier." |> Key::Ctrl('i'),
        select_entry |> "Select thread entry." |> Key::Char('v'),
        increase_sidebar |> "Increase sidebar width." |> Key::Ctrl('f'),
        decrease_sidebar |> "Decrease sidebar width." |> Key::Ctrl('d'),
        next_entry |> "Focus on next entry." |> Key::Ctrl('n'),
        previous_entry |> "Focus on previous entry." |> Key::Ctrl('p'),
        toggle_menu_visibility |> "Toggle visibility of side menu in mail list." |> Key::Char('`'),
        focus_left |> "Switch focus on the left." |> Key::Left,
        focus_right |> "Switch focus on the right." |> Key::Right,
        exit_entry |> "Exit e-mail entry." |> Key::Char('i'),
        open_entry |> "Open e-mail entry." |> Key::Char('\n')
    }
}

shortcut_key_values! { "contact-list",
    /// Shortcut listing for the contact list view
    pub struct ContactListShortcuts {
        scroll_up |> "Scroll up list." |> Key::Char('k'),
        scroll_down |> "Scroll down list." |> Key::Char('j'),
        create_contact |> "Create new contact." |> Key::Char('c'),
        edit_contact |> "Edit contact under cursor." |> Key::Char('e'),
        delete_contact |> "Delete contact under cursor." |> Key::Char('d'),
        mail_contact |> "Mail contact under cursor." |> Key::Char('m'),
        next_account |> "Go to next account." |> Key::Char('H'),
        prev_account |> "Go to previous account." |> Key::Char('L'),
        toggle_menu_visibility |> "Toggle visibility of side menu in mail list." |> Key::Char('`')
    }
}

shortcut_key_values! { "pager",
    /// Shortcut listing for the text pager
    pub struct PagerShortcuts {
        page_down |> "Go to next pager page." |>  Key::PageDown,
        page_up |> "Go to previous pager page." |>  Key::PageUp,
        scroll_down |> "Scroll down pager." |> Key::Char('j'),
        scroll_up |> "Scroll up pager." |> Key::Char('k'),
        select_filter |> "Select content filter." |> Key::Char('f')
    }
}

shortcut_key_values! { "general",
    pub struct GeneralShortcuts {
        toggle_help |> "Toggle help and shortcuts view." |> Key::Char('?'),
        enter_command_mode |> "Enter COMMAND mode." |> Key::Char(':'),
        quit |> "Quit meli." |> Key::Char('q'),
        go_to_tab |> "Go to the nth tab." |> Key::Alt('n'),
        next_tab |> "Go to the next tab." |> Key::Char('T'),
        scroll_right |> "Generic scroll right (catch-all setting)" |> Key::Char('l'),
        scroll_left |> "Generic scroll left (catch-all setting)" |>Key::Char('h'),
        scroll_up |> "Generic scroll up (catch-all setting)" |> Key::Char('k'),
        scroll_down |> "Generic scroll down (catch-all setting)" |> Key::Char('j'),
        next_page |> "Go to next page. (catch-all setting)" |> Key::PageDown,
        prev_page |> "Go to previous page. (catch-all setting)" |> Key::PageUp,
        home_page |> "Go to first page. (catch-all setting)" |> Key::Home,
        end_page |> "Go to last page. (catch-all setting)" |> Key::End,
        open_entry |> "Open list entry. (catch-all setting)" |> Key::Char('\n'),
        info_message_next |> "Show next info message, if any." |> Key::Char('>'),
        info_message_previous |> "Show previous info message, if any." |> Key::Char('<'),
        focus_in_text_field |> "Focus on a text field." |> Key::Char('\n'),
        next_search_result |> "Scroll to next search result." |> Key::Char('n'),
        previous_search_result |> "Scroll to previous search result." |> Key::Char('N')
    }
}

shortcut_key_values! { "composing",
    pub struct ComposingShortcuts {
        edit |> "Edit." |> Key::Char('e'),
        send_mail |> "Deliver draft to mailer." |> Key::Char('s'),
        scroll_up |> "Change field focus." |> Key::Char('k'),
        scroll_down |> "Change field focus." |> Key::Char('j')
    }
}

shortcut_key_values! { "envelope-view",
    pub struct EnvelopeViewShortcuts {
        add_addresses_to_contacts |> "Select addresses from envelope to add to contacts." |> Key::Char('c'),
        edit |> "Open envelope in composer." |> Key::Char('e'),
        go_to_url |> "Go to url of given index." |> Key::Char('g'),
        open_attachment |> "Opens selected attachment with xdg-open." |> Key::Char('a'),
        open_mailcap |> "Opens selected attachment according to its mailcap entry." |> Key::Char('m'),
        open_html |> "Opens html attachment in the default browser." |> Key::Char('v'),
        reply |> "Reply to envelope." |> Key::Char('R'),
        reply_to_author |> "Reply to author." |> Key::Ctrl('r'),
        reply_to_all |> "Reply to all/Reply to list/Follow up." |> Key::Ctrl('g'),
        forward |> "Forward email." |> Key::Ctrl('f'),
        return_to_normal_view |> "Return to envelope if viewing raw source or attachment." |> Key::Char('r'),
        toggle_expand_headers |> "Expand extra headers (References and others)." |> Key::Char('h'),
        toggle_url_mode |> "Toggles url open mode." |> Key::Char('u'),
        view_raw_source |> "View envelope source in a pager. (toggles between raw and decoded source)" |> Key::Alt('r'),
        change_charset |> "Force attachment charset for decoding." |> Key::Char('d')
    }
}

shortcut_key_values! { "thread-view",
    pub struct ThreadViewShortcuts {
        scroll_up |> "Scroll up list." |> Key::Char('k'),
        scroll_down |> "Scroll down list." |> Key::Char('j'),
        collapse_subtree |> "collapse thread branches." |> Key::Char('h'),
        next_page |> "Go to next page." |> Key::PageDown,
        prev_page |> "Go to previous page." |> Key::PageUp,
        reverse_thread_order |> "reverse thread order." |> Key::Ctrl('r'),
        toggle_mailview |> "toggle mail view visibility." |> Key::Char('p'),
        toggle_threadview |> "toggle thread view visibility." |> Key::Char('t'),
        toggle_layout |> "Toggle between horizontal and vertical layout." |> Key::Char(' ')
    }
}
