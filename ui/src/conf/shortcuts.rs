use crate::terminal::Key;
//use std::any::TypeId;
use fnv::FnvHashMap;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Shortcuts {
    #[serde(default)]
    pub general: GeneralShortcuts,
    #[serde(default)]
    pub listing: ListingShortcuts,
    #[serde(default)]
    pub composing: ComposingShortcuts,
    #[serde(default)]
    pub compact_listing: CompactListingShortcuts,
    #[serde(default)]
    pub contact_list: ContactListShortcuts,
    #[serde(default)]
    pub envelope_view: EnvelopeViewShortcuts,
    #[serde(default)]
    pub pager: PagerShortcuts,
}

/// Create a struct holding all of a Component's shortcuts.
#[macro_export]
macro_rules! shortcut_key_values {
    (
        $cname:expr,
        $(#[$outer:meta])*
        pub struct $name:ident { $($fname:ident : Key |> $fdesc:literal |> $default:expr),* }) => {
        $(#[$outer])*
        #[derive(Debug, Clone, Serialize, Deserialize)]
        #[serde(default)]
        #[serde(rename = $cname)]
        pub struct $name {
            $($fname : Key),*
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
            pub fn key_values(&self) -> FnvHashMap<&'static str, Key> {
                [
                $((stringify!($fname),(self.$fname).clone()),)*
                ].iter().cloned().collect()
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self {
                    $($fname: $default),*
                }
            }
        }
    }
}

shortcut_key_values! { "compact-listing",
    /// Shortcut listing for a mail listing in compact mode.
    pub struct CompactListingShortcuts {
            open_thread: Key |> "Open thread." |> Key::Char('\n'),
            exit_thread: Key |> "Exit thread view." |> Key::Char('i'),
            select_entry: Key |> "Select thread entry." |> Key::Char('v')
    }
}

shortcut_key_values! { "listing",
    /// Shortcut listing for a mail listing.
    pub struct ListingShortcuts {
            prev_page: Key |> "Go to previous page." |> Key::PageUp,
            next_page: Key |> "Go to next page." |> Key::PageDown,
            prev_folder: Key |> "Go to previous folder." |> Key::Char('K'),
            next_folder: Key |> "Go to next folder." |> Key::Char('J'),
            prev_account: Key |> "Go to previous account." |> Key::Char('l'),
            next_account: Key |> "Go to next account." |> Key::Char('h'),
            new_mail: Key |> "Start new mail draft in new tab." |>  Key::Char('m'),
            set_seen: Key |> "Set thread as seen." |> Key::Char('n'),
            search: Key |> "Search within list of e-mails." |> Key::Char('/'),
            toggle_menu_visibility: Key |> "Toggle visibility of side menu in mail list." |> Key::Char('`')
    }
}

shortcut_key_values! { "contact-list",
    /// Shortcut listing for the contact list view
    pub struct ContactListShortcuts {
        create_contact: Key |> "Create new contact." |> Key::Char('c'),
        edit_contact: Key |> "Edit contact under cursor." |> Key::Char('e'),
        toggle_menu_visibility: Key |> "Toggle visibility of side menu in mail list." |> Key::Char('`'),
        prev_account: Key |> "Go to previous account." |> Key::Char('l'),
        next_account: Key |> "Go to next account." |> Key::Char('h')
    }
}

shortcut_key_values! { "pager",
    /// Shortcut listing for the text pager
    pub struct PagerShortcuts {
        scroll_up: Key |> "Scroll up pager." |> Key::Char('k'),
        scroll_down: Key |> "Scroll down pager." |> Key::Char('j'),
        page_up: Key |> "Go to previous pager page" |>  Key::PageUp,
        page_down: Key |> "Go to next pager page" |>  Key::PageDown
    }
}

shortcut_key_values! { "general",
    pub struct GeneralShortcuts {
            next_tab: Key |> "Next tab." |> Key::Char('T'),
            go_to_tab: Key |> "Go to the nth tab" |> Key::Alt('n')
    }
}

shortcut_key_values! { "composing",
    pub struct ComposingShortcuts {
            send_mail: Key |> "Deliver draft to mailer" |> Key::Char('s'),
            edit_mail: Key |> "Edit mail." |> Key::Char('e')
    }
}

shortcut_key_values! { "envelope-view",
    pub struct EnvelopeViewShortcuts {
            reverse_thread_order: Key |> "reverse thread order" |> Key::Ctrl('r'),
            toggle_mailview: Key |> "toggle mail view visibility" |> Key::Char('p'),
            toggle_threadview: Key |> "toggle thread view visibility" |> Key::Char('t'),
            collapse_subtree: Key |> "collapse thread branches" |> Key::Char('h'),
            exit_thread: Key |> "exit thread" |> Key::Char('i'),
            prev_page: Key |> "Go to previous page." |> Key::PageUp,
            next_page: Key |> "Go to next page." |> Key::PageDown
    }
}
