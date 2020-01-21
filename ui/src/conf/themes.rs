/*
 * meli - themes conf module
 *
 * Copyright 2019  Manos Pitsidianakis
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

use crate::terminal::Color;
use crate::Context;
use melib::Result;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

#[inline(always)]
pub fn color(context: &Context, key: &'static str) -> Color {
    let theme = match context.settings.terminal.theme.as_str() {
        "light" => &context.settings.terminal.themes.light,
        "dark" | _ => &context.settings.terminal.themes.dark,
    };
    unlink(theme, &Cow::from(key))
}

#[inline(always)]
fn unlink<'k, 't: 'k>(
    theme: &'t HashMap<Cow<'static, str>, ThemeValue>,
    mut key: &'k Cow<'static, str>,
) -> Color {
    loop {
        match &theme[key] {
            ThemeValue::Link(ref new_key) => key = new_key,
            ThemeValue::Value(val) => return *val,
        }
    }
}

const DEFAULT_KEYS: &'static [&'static str] = &[
    "general.background",
    "general.foreground",
    "general.status_bar_fg",
    "general.status_bar_bg",
    "general.tab_focused_fg",
    "general.tab_focused_bg",
    "general.tab_unfocused_fg",
    "general.tab_unfocused_bg",
    "general.tab_bar_bg",
    "mail.sidebar_fg",
    "mail.sidebar_bg",
    "mail.sidebar_unread_count_fg",
    "mail.sidebar_unread_count_bg",
    "mail.sidebar_index_fg",
    "mail.sidebar_index_bg",
    "mail.sidebar_highlighted_fg",
    "mail.sidebar_highlighted_bg",
    "mail.sidebar_highlighted_unread_count_fg",
    "mail.sidebar_highlighted_unread_count_bg",
    "mail.sidebar_highlighted_index_fg",
    "mail.sidebar_highlighted_index_bg",
    "mail.sidebar_highlighted_account_fg",
    "mail.sidebar_highlighted_account_bg",
    "mail.sidebar_highlighted_account_unread_count_fg",
    "mail.sidebar_highlighted_account_unread_count_bg",
    "mail.sidebar_highlighted_account_index_fg",
    "mail.sidebar_highlighted_account_index_bg",
    "mail.listing.compact.even_fg",
    "mail.listing.compact.even_bg",
    "mail.listing.compact.odd_fg",
    "mail.listing.compact.odd_bg",
    "mail.listing.compact.unseen_fg",
    "mail.listing.compact.unseen_bg",
    "mail.listing.compact.selected_fg",
    "mail.listing.compact.selected_bg",
    "mail.listing.compact.highlighted_fg",
    "mail.listing.compact.highlighted_bg",
    "mail.listing.plain.even_fg",
    "mail.listing.plain.even_bg",
    "mail.listing.plain.odd_fg",
    "mail.listing.plain.odd_bg",
    "mail.listing.plain.unseen_fg",
    "mail.listing.plain.unseen_bg",
    "mail.listing.conversations.fg",
    "mail.listing.conversations.bg",
    "mail.listing.conversations.subject_fg",
    "mail.listing.conversations.subject_bg",
    "mail.listing.conversations.from_fg",
    "mail.listing.conversations.from_bg",
    "mail.listing.conversations.date_fg",
    "mail.listing.conversations.date_bg",
    "mail.listing.conversations.padding",
    "mail.listing.conversations.unseen_fg",
    "mail.listing.conversations.unseen_bg",
    "mail.listing.conversations.unseen_padding",
    "mail.listing.conversations.highlighted_fg",
    "mail.listing.conversations.highlighted_bg",
    "mail.listing.conversations.selected_fg",
    "mail.listing.conversations.selected_bg",
    "mail.view.headers_fg",
    "mail.view.headers_bg",
    "mail.view.body_fg",
    "mail.view.body_bg",
    "mail.listing.attachment_flag_fg",
    "mail.listing.attachment_flag_bg",
    "mail.listing.thread_snooze_flag_fg",
    "mail.listing.thread_snooze_flag_bg",
];

#[derive(Debug, Clone)]
pub enum ThemeValue {
    Value(Color),
    Link(Cow<'static, str>),
}

impl From<Color> for ThemeValue {
    fn from(from: Color) -> Self {
        ThemeValue::Value(from)
    }
}

impl Default for ThemeValue {
    fn default() -> Self {
        ThemeValue::Value(Color::Default)
    }
}

impl<'de> Deserialize<'de> for ThemeValue {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            if let Ok(c) = Color::from_string_de::<'de, D>(s.clone()) {
                Ok(ThemeValue::Value(c))
            } else {
                Ok(ThemeValue::Link(s.into()))
            }
        } else {
            Err(de::Error::custom("invalid theme color value"))
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Theme {
    #[serde(default)]
    pub light: HashMap<Cow<'static, str>, ThemeValue>,
    #[serde(default)]
    pub dark: HashMap<Cow<'static, str>, ThemeValue>,
}

impl Theme {
    pub fn validate(&self) -> Result<()> {
        let hash_set: HashSet<&'static str> = DEFAULT_KEYS.into_iter().map(|k| *k).collect();
        let keys: Vec<&'_ str> = self
            .light
            .keys()
            .chain(self.dark.keys())
            .filter_map(|k| {
                if !hash_set.contains(&k.as_ref()) {
                    Some(k.as_ref())
                } else {
                    None
                }
            })
            .collect();
        if keys.is_empty() {
            Ok(())
        } else {
            Err(format!("Unrecognized theme keywords: {}", keys.join(", ")).into())
        }
    }
}

impl Default for Theme {
    fn default() -> Theme {
        let mut light = HashMap::default();
        let mut dark = HashMap::default();

        macro_rules! add {
            ($key:literal, light=$light:literal, dark=$dark:literal) => {
                light.insert($key.into(), ThemeValue::Link($light.into()));
                dark.insert($key.into(), ThemeValue::Link($dark.into()));
            };
            ($key:literal, dark=$dark:literal, light=$light:literal) => {
                light.insert($key.into(), ThemeValue::Link($light.into()));
                dark.insert($key.into(), ThemeValue::Link($dark.into()));
            };
            ($key:literal, light=$light:literal) => {
                light.insert($key.into(), $ThemeValue::Link(light);)
                dark.insert($key.into(), ThemeValue::Value(Color::Default));
            };
            ($key:literal, dark=$dark:literal) => {
                light.insert($key.into(),ThemeValue::Value(Color::Default));
                dark.insert($key.into(), ThemeValue::Link($dark.into()));
            };
            ($key:literal, light=$light:expr, dark=$dark:expr) => {
                light.insert($key.into(), ThemeValue::Value($light));
                dark.insert($key.into(), ThemeValue::Value($dark));
            };
            ($key:literal, dark=$dark:expr, light=$light:expr) => {
                light.insert($key.into(), ThemeValue::Value($light));
                dark.insert($key.into(), ThemeValue::Value($dark));
            };
            ($key:literal, light=$light:expr) => {
                light.insert($key.into(), $ThemeValue::Value(light);)
                dark.insert($key.into(), ThemeValue::Value(Color::Default));
            };
            ($key:literal, dark=$dark:expr) => {
                light.insert($key.into(),ThemeValue::Value(Color::Default));
                dark.insert($key.into(), ThemeValue::Value($dark));
            };
            ($key:literal) => {
                light.insert($key.into(), ThemeValue::Value(Color::Default));
                dark.insert($key.into(), ThemeValue::Value(Color::Default));
            };
        }

        add!("general.background");
        add!("general.foreground");
        /*
        "general.status_bar_fg",
        "general.status_bar_bg",
        "general.tab_focused_fg",
        "general.tab_focused_bg",
        "general.tab_unfocused_fg",
        "general.tab_unfocused_bg",
        "general.tab_bar_bg",
        */

        /* Mail Sidebar */

        add!("mail.sidebar_fg");
        add!("mail.sidebar_bg");
        add!("mail.sidebar_unread_count_fg", dark = Color::Byte(243));
        add!("mail.sidebar_unread_count_bg");
        add!("mail.sidebar_index_fg", dark = Color::Byte(243));
        add!("mail.sidebar_index_bg");
        add!("mail.sidebar_highlighted_fg", dark = Color::Byte(233));
        add!("mail.sidebar_highlighted_bg", dark = Color::Byte(15));
        add!(
            "mail.sidebar_highlighted_unread_count_fg",
            light = "mail.sidebar_highlighted_fg",
            dark = "mail.sidebar_highlighted_fg"
        );
        add!(
            "mail.sidebar_highlighted_unread_count_bg",
            light = "mail.sidebar_highlighted_bg",
            dark = "mail.sidebar_highlighted_bg"
        );
        add!(
            "mail.sidebar_highlighted_index_fg",
            light = "mail.sidebar_index_fg",
            dark = "mail.sidebar_index_fg"
        );
        add!(
            "mail.sidebar_highlighted_index_bg",
            light = "mail.sidebar_highlighted_bg",
            dark = "mail.sidebar_highlighted_bg"
        );
        add!(
            "mail.sidebar_highlighted_account_fg",
            dark = Color::Byte(15)
        );
        add!(
            "mail.sidebar_highlighted_account_bg",
            dark = Color::Byte(233)
        );
        add!(
            "mail.sidebar_highlighted_account_unread_count_fg",
            light = "mail.sidebar_unread_count_fg",
            dark = "mail.sidebar_unread_count_fg"
        );
        add!(
            "mail.sidebar_highlighted_account_unread_count_bg",
            light = "mail.sidebar_highlighted_account_bg",
            dark = "mail.sidebar_highlighted_account_bg"
        );
        add!(
            "mail.sidebar_highlighted_account_index_fg",
            light = "mail.sidebar_index_fg",
            dark = "mail.sidebar_index_fg"
        );
        add!(
            "mail.sidebar_highlighted_account_index_bg",
            light = "mail.sidebar_highlighted_account_bg",
            dark = "mail.sidebar_highlighted_account_bg"
        );

        /* CompactListing */
        add!("mail.listing.compact.even_fg");
        add!(
            "mail.listing.compact.even_bg",
            dark = Color::Byte(236),
            light = Color::Byte(252)
        );
        add!("mail.listing.compact.odd_fg");
        add!("mail.listing.compact.odd_bg");
        add!(
            "mail.listing.compact.unseen_fg",
            dark = Color::Byte(0),
            light = Color::Byte(0)
        );
        add!(
            "mail.listing.compact.unseen_bg",
            dark = Color::Byte(251),
            light = Color::Byte(251)
        );
        add!("mail.listing.compact.selected_fg");
        add!(
            "mail.listing.compact.selected_bg",
            dark = Color::Byte(210),
            light = Color::Byte(210)
        );
        add!("mail.listing.compact.highlighted_fg");
        add!(
            "mail.listing.compact.highlighted_bg",
            dark = Color::Byte(246),
            light = Color::Byte(244)
        );

        /* ConversationsListing */

        add!("mail.listing.conversations.fg");
        add!("mail.listing.conversations.bg");
        add!("mail.listing.conversations.subject_fg");
        add!("mail.listing.conversations.subject_bg");
        add!("mail.listing.conversations.from_fg");
        add!("mail.listing.conversations.from_bg");
        add!("mail.listing.conversations.date_fg");
        add!("mail.listing.conversations.date_bg");
        add!(
            "mail.listing.conversations.padding",
            dark = Color::Byte(235),
            light = Color::Byte(254)
        );
        add!(
            "mail.listing.conversations.unseen_padding",
            dark = Color::Byte(235),
            light = Color::Byte(254)
        );
        add!(
            "mail.listing.conversations.unseen_fg",
            dark = Color::Byte(0),
            light = Color::Byte(0)
        );
        add!(
            "mail.listing.conversations.unseen_bg",
            dark = Color::Byte(251),
            light = Color::Byte(251)
        );
        add!("mail.listing.conversations.highlighted_fg");
        add!(
            "mail.listing.conversations.highlighted_bg",
            dark = Color::Byte(246),
            light = Color::Byte(246)
        );
        add!("mail.listing.conversations.selected_fg");
        add!(
            "mail.listing.conversations.selected_bg",
            dark = Color::Byte(210),
            light = Color::Byte(210)
        );

        /*
        "mail.listing.plain.even_fg",
        "mail.listing.plain.even_bg",
        "mail.listing.plain.odd_fg",
        "mail.listing.plain.odd_bg",
        "mail.listing.plain.unseen_fg",
        "mail.listing.plain.unseen_bg",
        "mail.listing.conversations.subject_fg",
        "mail.listing.conversations.subject_bg",
        "mail.listing.conversations.from_fg",
        "mail.listing.conversations.from_bg",
        "mail.listing.conversations.date_fg",
        "mail.listing.conversations.date_bg",
        "mail.listing.conversations.unseen_padding",
        */
        add!(
            "mail.view.headers_fg",
            dark = Color::Byte(33),
            light = Color::Black
        );
        add!("mail.view.headers_bg");
        add!("mail.view.body_fg");
        add!("mail.view.body_bg");

        add!(
            "mail.listing.attachment_flag_fg",
            light = Color::Byte(103),
            dark = Color::Byte(103)
        );

        add!(
            "mail.listing.attachment_flag_bg",
            light = Color::Default,
            dark = Color::Default
        );

        add!(
            "mail.listing.thread_snooze_flag_fg",
            light = Color::Red,
            dark = Color::Red
        );

        add!(
            "mail.listing.thread_snooze_flag_bg",
            light = Color::Default,
            dark = Color::Default
        );

        Theme { light, dark }
    }
}

impl Serialize for Theme {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut dark: HashMap<Cow<'static, str>, Color> = Default::default();
        let mut light: HashMap<Cow<'static, str>, Color> = Default::default();

        for k in self.dark.keys() {
            dark.insert(k.clone(), unlink(&self.dark, k));
        }

        for k in self.light.keys() {
            light.insert(k.clone(), unlink(&self.light, k));
        }

        #[derive(Serialize)]
        struct ThemeSer {
            light: HashMap<Cow<'static, str>, Color>,
            dark: HashMap<Cow<'static, str>, Color>,
        }
        use serde::ser::SerializeStruct;
        let mut s = serializer.serialize_struct("ThemeSer", 2)?;
        s.serialize_field("light", &light)?;
        s.serialize_field("dark", &dark)?;
        s.end()
    }
}
