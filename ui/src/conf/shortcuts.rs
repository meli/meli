use crate::terminal::Key;
//use std::any::TypeId;
use fnv::FnvHashMap;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Shortcuts {
    #[serde(flatten)]
    pub listing: ListingShortcuts,
    #[serde(flatten)]
    pub compact_listing: CompactListingShortcuts,
    #[serde(flatten)]
    pub contact_list: ContactListShortcuts,
    #[serde(flatten)]
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
            pub fn key_values(&self) -> FnvHashMap<&'static str, &Key> {
                [
                $((stringify!($fname),&(self.$fname)),)*
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

shortcut_key_values! { "compact_listing",
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
