use terminal::Key;
//use std::any::TypeId;
use fnv::FnvHashMap;

#[derive(Debug, Clone, Default, Deserialize)]
pub struct Shortcuts {
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
        pub struct $name:ident { $($fname:ident : Key |> $fdesc:expr),* }) => {
        $(#[$outer])*
        #[derive(Debug, Clone, Deserialize)]
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
                let mut map: FnvHashMap<&'static str, &Key> = Default::default();
                $(map.insert(stringify!($fname),&(self.$fname));)*
                  map
            }
        }
    }
}

shortcut_key_values! { "compact-listing",
/// Shortcut listing for a mail listing in compact mode.
pub struct CompactListingShortcuts {
        open_thread: Key |> "Open thread.",
        exit_thread: Key |> "Exit thread view.",
        prev_page: Key |> "Go to previous page.",
        next_page: Key |> "Go to next page.",
        prev_folder: Key |> "Go to previous folder.",
        next_folder: Key |> "Go to next folder.",
        prev_account: Key |> "Go to previous account.",
        next_account: Key |> "Go to next account.",
        new_mail: Key |> "Start new mail draft in new tab."
}
}

impl Default for CompactListingShortcuts {
    fn default() -> Self {
        CompactListingShortcuts {
            open_thread: Key::Char('\n'),
            exit_thread: Key::Char('i'),
            prev_page: Key::PageUp,
            next_page: Key::PageDown,
            prev_folder: Key::Char('J'),
            next_folder: Key::Char('K'),
            prev_account: Key::Char('h'),
            next_account: Key::Char('l'),
            new_mail: Key::Char('m'),
        }
    }
}

shortcut_key_values! { "contact-list",
/// Shortcut listing for the contact list view
pub struct ContactListShortcuts {
    create_contact: Key |> "Create new contact.",
    edit_contact: Key |> "Edit contact under cursor."
}
}

impl Default for ContactListShortcuts {
    fn default() -> Self {
        ContactListShortcuts {
            create_contact: Key::Char('c'),
            edit_contact: Key::Char('e'),
        }
    }
}

shortcut_key_values! { "pager",
/// Shortcut listing for the text pager
pub struct PagerShortcuts {
    scroll_up: Key |> "Scroll up pager.",
    scroll_down: Key |> "Scroll down pager.",
    page_up: Key |> "Go to previous pager page",
    page_down: Key |> "Go to next pager page"
}
}

impl Default for PagerShortcuts {
    fn default() -> Self {
        PagerShortcuts {
            scroll_up: Key::Char('k'),
            scroll_down: Key::Char('j'),
            page_up: Key::PageUp,
            page_down: Key::PageDown,
        }
    }
}
