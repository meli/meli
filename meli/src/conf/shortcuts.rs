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
use melib::{
    backends::{AccountHash, MailboxHash},
    Error, Result,
};

use super::DotAddressable;
use crate::{state::Context, terminal::Key};

#[macro_export]
macro_rules! shortcut {
    ($key:ident == $shortcuts:ident[$section:expr][$val:expr]) => {
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

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ShortcutsOverride {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub general: Option<GeneralShortcutsOverride>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub listing: Option<ListingShortcutsOverride>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub composing: Option<ComposingShortcutsOverride>,
    #[serde(
        default,
        alias = "contact-list",
        skip_serializing_if = "Option::is_none"
    )]
    pub contact_list: Option<ContactListShortcutsOverride>,
    #[serde(
        default,
        alias = "envelope-view",
        skip_serializing_if = "Option::is_none"
    )]
    pub envelope_view: Option<EnvelopeViewShortcutsOverride>,
    #[serde(
        default,
        alias = "thread-view",
        skip_serializing_if = "Option::is_none"
    )]
    pub thread_view: Option<ThreadViewShortcutsOverride>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pager: Option<PagerShortcutsOverride>,
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
                        "{parent_field} has no field named {other}"
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
        pub struct $name:ident ($overridename:ident) { $($fname:ident |> $fdesc:literal |> $default:expr),*$(,)? }) => {
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

        #[derive(Debug, Default, Serialize, Deserialize, Clone)]
        #[serde(deny_unknown_fields)]
        pub struct $overridename {
            #[serde(default)]
            pub commands: Vec<CommandShortcut>,
            $(
                #[serde(default, skip_serializing_if = "Option::is_none")]
                pub $fname : Option<Key>
            ),*
        }
    }
}

shortcut_key_values! { "listing",
    /// Shortcut listing for a mail listing.
    pub struct ListingShortcuts (ListingShortcutsOverride) {
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
        send_to_trash |> "Send entry to trash folder." |> Key::Char('D'),
        union_modifier |> "Union modifier." |> Key::Ctrl('u'),
        diff_modifier |> "Difference modifier." |> Key::Ctrl('d'),
        intersection_modifier |> "Intersection modifier." |> Key::Ctrl('i'),
        select_entry |> "Select thread entry." |> Key::Char('V'),
        select_motion |> "Perform select motion with a movement." |> Key::Char('v'),
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
    pub struct ContactListShortcuts (ContactListShortcutsOverride) {
        create_contact |> "Create new contact." |> Key::Char('c'),
        edit_contact |> "Edit contact under cursor." |> Key::Char('e'),
        export_contact |> "Export contact under cursor to .vcf." |> Key::Char('E'),
        import_contact |> "Import contact from .vcf file." |> Key::Char('I'),
        delete_contact |> "Delete contact under cursor." |> Key::Char('d'),
        mail_contact |> "Mail contact under cursor." |> Key::Char('m'),
    }
}

shortcut_key_values! { "pager",
    /// Shortcut listing for the text pager
    pub struct PagerShortcuts (PagerShortcutsOverride) {
        page_down |> "Go to next pager page." |>  Key::PageDown,
        page_up |> "Go to previous pager page." |>  Key::PageUp,
        scroll_down |> "Scroll down pager." |> Key::Char('j'),
        scroll_up |> "Scroll up pager." |> Key::Char('k'),
        select_filter |> "Select content filter." |> Key::Char('f')
    }
}

shortcut_key_values! { "general",
    pub struct GeneralShortcuts (GeneralShortcutsOverride) {
        toggle_help |> "Toggle help and shortcuts view." |> Key::Char('?'),
        enter_command_mode |> "Enter COMMAND mode." |> Key::Char(':'),
        open_terminal |> "Opens $SHELL in current directory." |> Key::Ctrl('t'),
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
        open_entry |> "Open list entry, press button, toggle entry. (catch-all setting)" |> Key::Char('\n'),
        info_message_next |> "Show next info message, if any." |> Key::Char('>'),
        info_message_previous |> "Show previous info message, if any." |> Key::Char('<'),
        focus_in_text_field |> "Focus on a text field." |> Key::Char('\n'),
        search |> "Search within pager." |> Key::Char('/'),
        next_search_result |> "Scroll to next search result." |> Key::Char('n'),
        previous_search_result |> "Scroll to previous search result." |> Key::Char('N')
    }
}

shortcut_key_values! { "composing",
    pub struct ComposingShortcuts (ComposingShortcutsOverride) {
        edit |> "Edit." |> Key::Char('e'),
        send_mail |> "Deliver draft to mailer." |> Key::Char('s'),
        scroll_up |> "Change field focus." |> Key::Char('k'),
        scroll_down |> "Change field focus." |> Key::Char('j'),
        reset_date |> "Reset date to current time." |> Key::F(1),
        reset_body |> "Reset body to default template." |> Key::F(2),
    }
}

shortcut_key_values! { "envelope-view",
    pub struct EnvelopeViewShortcuts (EnvelopeViewShortcutsOverride) {
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
    pub struct ThreadViewShortcuts (ThreadViewShortcutsOverride) {
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

macro_rules! get_commands {
    ($(pub fn $fn:ident: $field:ident -> Vec<CommandShortcut>;)*) => {
        $(pub fn $fn(
            &self,
            account_hash: Option<(AccountHash, Option<MailboxHash>)>,
        ) -> Vec<CommandShortcut> {
            let (account_override, mailbox_override): (
                Option<&[CommandShortcut]>,
                Option<&[CommandShortcut]>,
            ) = account_hash
                .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
                .map(|(a, m)| {
                    let mailbox_override = m
                        .and_then(|h| a.mailbox_entries.get(&h))
                        .and_then(|m| m.conf.conf_override.shortcuts.$field.as_ref())
                        .map(|m| m.commands.as_slice());
                    (
                        a.settings
                            .conf_override
                            .shortcuts
                            .$field
                            .as_ref()
                            .map(|s| s.commands.as_slice()),
                        mailbox_override,
                    )
                })
                .unwrap_or((None, None));

            let mut commands = self.settings.shortcuts.$field.commands.clone();

            if let Some(account_override) = account_override {
                for cmd in account_override {
                    if let Some(pos) = commands.iter().position(|c| c.shortcut == cmd.shortcut) {
                        commands[pos] = cmd.clone();
                    }
                }
            }
            if let Some(mailbox_override) = mailbox_override {
                for cmd in mailbox_override {
                    if let Some(pos) = commands.iter().position(|c| c.shortcut == cmd.shortcut) {
                        commands[pos] = cmd.clone();
                    }
                }
            }
            commands
        })*
    };
}

impl Context {
    pub fn general_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: GeneralShortcuts = GeneralShortcuts {
            commands: Vec::new(),
            toggle_help: Key::Null,
            enter_command_mode: Key::Null,
            open_terminal: Key::Null,
            quit: Key::Null,
            go_to_tab: Key::Null,
            next_tab: Key::Null,
            scroll_right: Key::Null,
            scroll_left: Key::Null,
            scroll_up: Key::Null,
            scroll_down: Key::Null,
            next_page: Key::Null,
            prev_page: Key::Null,
            home_page: Key::Null,
            end_page: Key::Null,
            open_entry: Key::Null,
            info_message_next: Key::Null,
            info_message_previous: Key::Null,
            focus_in_text_field: Key::Null,
            search: Key::Null,
            next_search_result: Key::Null,
            previous_search_result: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&GeneralShortcutsOverride>,
            Option<&GeneralShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.general.as_ref());
                (
                    a.settings.conf_override.shortcuts.general.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut general_shortcuts = self.settings.shortcuts.general.key_values();

        macro_rules! check_override {
            ($fname:ident) => {{
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != general_shortcuts[stringify! { $fname }] {
                                general_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != general_shortcuts[stringify! { $fname }] {
                                general_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
            }};
            ($($fname:ident),*$(,)?) => {{
                $(
                    check_override!($fname);
                )*
            }};
        }

        check_override! {
            toggle_help,
            enter_command_mode,
            open_terminal,
            quit,
            go_to_tab,
            next_tab,
            scroll_right,
            scroll_left,
            scroll_up,
            scroll_down,
            next_page,
            prev_page,
            home_page,
            end_page,
            open_entry,
            info_message_next,
            info_message_previous,
            focus_in_text_field,
            search,
            next_search_result,
            previous_search_result,
        };

        general_shortcuts
    }

    pub fn listing_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: ListingShortcuts = ListingShortcuts {
            commands: Vec::new(),
            scroll_up: Key::Null,
            scroll_down: Key::Null,
            new_mail: Key::Null,
            next_account: Key::Null,
            next_mailbox: Key::Null,
            next_page: Key::Null,
            prev_account: Key::Null,
            prev_mailbox: Key::Null,
            open_mailbox: Key::Null,
            toggle_mailbox_collapse: Key::Null,
            prev_page: Key::Null,
            search: Key::Null,
            refresh: Key::Null,
            set_seen: Key::Null,
            send_to_trash: Key::Null,
            union_modifier: Key::Null,
            diff_modifier: Key::Null,
            intersection_modifier: Key::Null,
            select_entry: Key::Null,
            select_motion: Key::Null,
            increase_sidebar: Key::Null,
            decrease_sidebar: Key::Null,
            next_entry: Key::Null,
            previous_entry: Key::Null,
            toggle_menu_visibility: Key::Null,
            focus_left: Key::Null,
            focus_right: Key::Null,
            exit_entry: Key::Null,
            open_entry: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&ListingShortcutsOverride>,
            Option<&ListingShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.listing.as_ref());
                (
                    a.settings.conf_override.shortcuts.listing.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut listing_shortcuts = self.settings.shortcuts.listing.key_values();

        macro_rules! check_override {
            ($($fname:ident),*$(,)?) => {{
                $(
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != listing_shortcuts[stringify! { $fname }] {
                                listing_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != listing_shortcuts[stringify! { $fname }] {
                                listing_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                )*
            }};
        }

        check_override! {
            scroll_up,
            scroll_down,
            new_mail,
            next_account,
            next_mailbox,
            next_page,
            prev_account,
            prev_mailbox,
            open_mailbox,
            toggle_mailbox_collapse,
            prev_page,
            search,
            refresh,
            set_seen,
            send_to_trash,
            union_modifier,
            diff_modifier,
            intersection_modifier,
            select_entry,
            select_motion,
            increase_sidebar,
            decrease_sidebar,
            next_entry,
            previous_entry,
            toggle_menu_visibility,
            focus_left,
            focus_right,
            exit_entry,
            open_entry,
        };

        listing_shortcuts
    }

    pub fn composing_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: ComposingShortcuts = ComposingShortcuts {
            commands: Vec::new(),
            edit: Key::Null,
            send_mail: Key::Null,
            scroll_up: Key::Null,
            scroll_down: Key::Null,
            reset_date: Key::Null,
            reset_body: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&ComposingShortcutsOverride>,
            Option<&ComposingShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.composing.as_ref());
                (
                    a.settings.conf_override.shortcuts.composing.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut composing_shortcuts = self.settings.shortcuts.composing.key_values();

        macro_rules! check_override {
            ($($fname:ident),*$(,)?) => {{
                $(
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != composing_shortcuts[stringify! { $fname }] {
                                composing_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != composing_shortcuts[stringify! { $fname }] {
                                composing_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                )*
            }};
        }

        check_override! {
            edit,
            send_mail,
            scroll_up,
            scroll_down,
            reset_date,
            reset_body,
        };

        composing_shortcuts
    }

    pub fn contact_list_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: ContactListShortcuts = ContactListShortcuts {
            commands: Vec::new(),
            create_contact: Key::Null,
            edit_contact: Key::Null,
            export_contact: Key::Null,
            import_contact: Key::Null,
            delete_contact: Key::Null,
            mail_contact: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&ContactListShortcutsOverride>,
            Option<&ContactListShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.contact_list.as_ref());
                (
                    a.settings.conf_override.shortcuts.contact_list.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut contact_list_shortcuts = self.settings.shortcuts.contact_list.key_values();

        macro_rules! check_override {
            ($($fname:ident),*$(,)?) => {{
                $(
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != contact_list_shortcuts[stringify! { $fname }] {
                                contact_list_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != contact_list_shortcuts[stringify! { $fname }] {
                                contact_list_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                )*
            }};
        }

        check_override! {
            create_contact,
            edit_contact,
            export_contact,
            import_contact,
            delete_contact,
            mail_contact,
        };

        contact_list_shortcuts
    }

    pub fn envelope_view_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: EnvelopeViewShortcuts = EnvelopeViewShortcuts {
            commands: Vec::new(),
            add_addresses_to_contacts: Key::Null,
            edit: Key::Null,
            go_to_url: Key::Null,
            open_attachment: Key::Null,
            open_mailcap: Key::Null,
            open_html: Key::Null,
            reply: Key::Null,
            reply_to_author: Key::Null,
            reply_to_all: Key::Null,
            forward: Key::Null,
            return_to_normal_view: Key::Null,
            toggle_expand_headers: Key::Null,
            toggle_url_mode: Key::Null,
            view_raw_source: Key::Null,
            change_charset: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&EnvelopeViewShortcutsOverride>,
            Option<&EnvelopeViewShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.envelope_view.as_ref());
                (
                    a.settings.conf_override.shortcuts.envelope_view.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut envelope_view_shortcuts = self.settings.shortcuts.envelope_view.key_values();

        macro_rules! check_override {
            ($($fname:ident),*$(,)?) => {{
                $(
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != envelope_view_shortcuts[stringify! { $fname }] {
                                envelope_view_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != envelope_view_shortcuts[stringify! { $fname }] {
                                envelope_view_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                )*
            }};
        }

        check_override! {
            add_addresses_to_contacts,
            edit,
            go_to_url,
            open_attachment,
            open_mailcap,
            open_html,
            reply,
            reply_to_author,
            reply_to_all,
            forward,
            return_to_normal_view,
            toggle_expand_headers,
            toggle_url_mode,
            view_raw_source,
            change_charset,
        };

        envelope_view_shortcuts
    }

    pub fn thread_view_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: ThreadViewShortcuts = ThreadViewShortcuts {
            commands: Vec::new(),
            scroll_up: Key::Null,
            scroll_down: Key::Null,
            collapse_subtree: Key::Null,
            next_page: Key::Null,
            prev_page: Key::Null,
            reverse_thread_order: Key::Null,
            toggle_mailview: Key::Null,
            toggle_threadview: Key::Null,
            toggle_layout: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&ThreadViewShortcutsOverride>,
            Option<&ThreadViewShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.thread_view.as_ref());
                (
                    a.settings.conf_override.shortcuts.thread_view.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut thread_view_shortcuts = self.settings.shortcuts.thread_view.key_values();

        macro_rules! check_override {
            ($($fname:ident),*$(,)?) => {{
                $(
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != thread_view_shortcuts[stringify! { $fname }] {
                                thread_view_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != thread_view_shortcuts[stringify! { $fname }] {
                                thread_view_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                )*
            }};
        }

        check_override! {
            scroll_up,
            scroll_down,
            collapse_subtree,
            next_page,
            prev_page,
            reverse_thread_order,
            toggle_mailview,
            toggle_threadview,
            toggle_layout,
        };

        thread_view_shortcuts
    }

    pub fn pager_shortcuts(
        &self,
        account_hash: Option<(AccountHash, Option<MailboxHash>)>,
    ) -> IndexMap<&'static str, Key> {
        // compile-time assert we don't forget to change any field here
        const _: PagerShortcuts = PagerShortcuts {
            commands: Vec::new(),
            page_down: Key::Null,
            page_up: Key::Null,
            scroll_down: Key::Null,
            scroll_up: Key::Null,
            select_filter: Key::Null,
        };
        let (account_override, mailbox_override): (
            Option<&PagerShortcutsOverride>,
            Option<&PagerShortcutsOverride>,
        ) = account_hash
            .and_then(|(a, m)| Some((self.accounts.get(&a)?, m)))
            .map(|(a, m)| {
                let mailbox_override = m
                    .and_then(|h| a.mailbox_entries.get(&h))
                    .and_then(|m| m.conf.conf_override.shortcuts.pager.as_ref());
                (
                    a.settings.conf_override.shortcuts.pager.as_ref(),
                    mailbox_override,
                )
            })
            .unwrap_or((None, None));

        let mut pager_shortcuts = self.settings.shortcuts.pager.key_values();

        macro_rules! check_override {
            ($($fname:ident),*$(,)?) => {{
                $(
                    if let Some(account_override) = account_override {
                        if let Some(key) = &account_override.$fname {
                            if key != pager_shortcuts[stringify! { $fname }] {
                                pager_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                    if let Some(mailbox_override) = mailbox_override {
                        if let Some(key) = &mailbox_override.$fname {
                            if key != pager_shortcuts[stringify! { $fname }] {
                                pager_shortcuts[stringify! { $fname }] = key.clone();
                            }
                        }
                    }
                )*
            }};
        }

        check_override! {
        page_down,
        page_up,
        scroll_down,
        scroll_up,
        select_filter,
        };

        pager_shortcuts
    }

    get_commands! {
        pub fn general_commands: general -> Vec<CommandShortcut>;
        pub fn listing_commands: listing -> Vec<CommandShortcut>;
        pub fn composing_commands: composing -> Vec<CommandShortcut>;
        pub fn envelope_view_commands: envelope_view -> Vec<CommandShortcut>;
        pub fn contact_list_commands: contact_list -> Vec<CommandShortcut>;
        pub fn thread_view_commands: thread_view -> Vec<CommandShortcut>;
        pub fn pager_commands: pager -> Vec<CommandShortcut>;
    }
}
