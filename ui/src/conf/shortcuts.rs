use crate::terminal::Key;
//use std::any::TypeId;
use fnv::FnvHashMap;

#[macro_export]
macro_rules! shortcut {
    ($key:ident == $shortcuts:ident[$section:expr][$val:literal]) => {
        $shortcuts[$section]
            .get($val)
            .map(|v| v == $key)
            .unwrap_or(false)
    };
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Shortcuts {
    #[serde(default)]
    pub general: GeneralShortcuts,
    #[serde(default)]
    pub listing: ListingShortcuts,
    #[serde(default)]
    pub composing: ComposingShortcuts,
    #[serde(default, alias = "compact-listing")]
    pub compact_listing: CompactListingShortcuts,
    #[serde(default, alias = "contact-list")]
    pub contact_list: ContactListShortcuts,
    #[serde(default, alias = "envelope-view")]
    pub envelope_view: EnvelopeViewShortcuts,
    #[serde(default, alias = "thread-view")]
    pub thread_view: ThreadViewShortcuts,
    #[serde(default)]
    pub pager: PagerShortcuts,
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
        exit_thread |> "Exit thread view." |> Key::Char('i'),
        open_thread |> "Open thread." |> Key::Char('\n'),
        select_entry |> "Select thread entry." |> Key::Char('v')
    }
}

shortcut_key_values! { "listing",
    /// Shortcut listing for a mail listing.
    pub struct ListingShortcuts {
        new_mail |> "Start new mail draft in new tab." |>  Key::Char('m'),
        next_account |> "Go to next account." |> Key::Char('h'),
        next_folder |> "Go to next folder." |> Key::Char('J'),
        next_page |> "Go to next page." |> Key::PageDown,
        prev_account |> "Go to previous account." |> Key::Char('l'),
        prev_folder |> "Go to previous folder." |> Key::Char('K'),
        prev_page |> "Go to previous page." |> Key::PageUp,
        search |> "Search within list of e-mails." |> Key::Char('/'),
        set_seen |> "Set thread as seen." |> Key::Char('n'),
        toggle_menu_visibility |> "Toggle visibility of side menu in mail list." |> Key::Char('`')
    }
}

shortcut_key_values! { "contact-list",
    /// Shortcut listing for the contact list view
    pub struct ContactListShortcuts {
        create_contact |> "Create new contact." |> Key::Char('c'),
        edit_contact |> "Edit contact under cursor." |> Key::Char('e'),
        mail_contact |> "Mail contact under cursor." |> Key::Char('m'),
        next_account |> "Go to next account." |> Key::Char('h'),
        prev_account |> "Go to previous account." |> Key::Char('l'),
        toggle_menu_visibility |> "Toggle visibility of side menu in mail list." |> Key::Char('`')
    }
}

shortcut_key_values! { "pager",
    /// Shortcut listing for the text pager
    pub struct PagerShortcuts {
        page_down |> "Go to next pager page" |>  Key::PageDown,
        page_up |> "Go to previous pager page" |>  Key::PageUp,
        scroll_down |> "Scroll down pager." |> Key::Char('j'),
        scroll_up |> "Scroll up pager." |> Key::Char('k')
    }
}

shortcut_key_values! { "general",
    pub struct GeneralShortcuts {
        go_to_tab |> "Go to the nth tab" |> Key::Alt('n'),
        next_tab |> "Next tab." |> Key::Char('T')
    }
}

shortcut_key_values! { "composing",
    pub struct ComposingShortcuts {
        edit_mail |> "Edit mail." |> Key::Char('e'),
        send_mail |> "Deliver draft to mailer" |> Key::Char('s')
    }
}

shortcut_key_values! { "envelope-view",
    pub struct EnvelopeViewShortcuts {
        add_addresses_to_contacts |> "Select addresses from envelope to add to contacts." |> Key::Char('c'),
        edit |> "Open envelope in composer." |> Key::Char('e'),
        go_to_url |> "Go to url of given index" |> Key::Char('g'),
        open_attachment |> "Opens selected attachment with xdg-open." |> Key::Char('a'),
        open_mailcap |> "Opens selected attachment according to its mailcap entry." |> Key::Char('m'),
        reply |> "Reply to envelope." |> Key::Char('R'),
        return_to_normal_view |> "Return to envelope if viewing raw source or attachment." |> Key::Char('r'),
        toggle_expand_headers |> "Expand extra headers (References and others)." |> Key::Char('h'),
        toggle_url_mode |> "Toggles url open mode." |> Key::Char('u'),
        view_raw_source |> "View raw envelope source in a pager." |> Key::Alt('r')
    }
}

shortcut_key_values! { "thread-view",
    pub struct ThreadViewShortcuts {
        collapse_subtree |> "collapse thread branches" |> Key::Char('h'),
        next_page |> "Go to next page." |> Key::PageDown,
        prev_page |> "Go to previous page." |> Key::PageUp,
        reverse_thread_order |> "reverse thread order" |> Key::Ctrl('r'),
        toggle_mailview |> "toggle mail view visibility" |> Key::Char('p'),
        toggle_threadview |> "toggle thread view visibility" |> Key::Char('t')
    }
}
