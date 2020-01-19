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
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

#[inline(always)]
pub fn color(context: &Context, key: &'static str) -> Color {
    (match context.settings.terminal.theme.as_str() {
        "light" => &context.settings.terminal.themes.light,
        "dark" | _ => &context.settings.terminal.themes.dark,
    })[key]
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
    "mail.listing.compact.even_fg",
    "mail.listing.compact.even_bg",
    "mail.listing.compact.odd_fg",
    "mail.listing.compact.odd_bg",
    "mail.listing.compact.unseen_fg",
    "mail.listing.compact.unseen_fg",
    "mail.listing.compact.selected_fg",
    "mail.listing.compact.selected_bg",
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
    "mail.listing.conversations.padding",
    "mail.listing.conversations.unseen_fg",
    "mail.listing.conversations.unseen_bg",
    "mail.listing.conversations.unseen_padding",
    "mail.view.headers_fg",
    "mail.view.headers_bg",
    "mail.view.body_fg",
    "mail.view.body_bg",
    "mail.listing.attachment_flag_fg",
    "mail.listing.attachment_flag_bg",
    "mail.listing.thread_snooze_flag_fg",
    "mail.listing.thread_snooze_flag_bg",
];
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Theme {
    #[serde(default)]
    pub light: HashMap<Cow<'static, str>, Color>,
    #[serde(default)]
    pub dark: HashMap<Cow<'static, str>, Color>,
}

impl Theme {
    pub fn validate(&self) -> Option<String> {
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
            None
        } else {
            Some(format!("Unrecognized theme keywords: {}", keys.join(", ")))
        }
    }
}

impl Default for Theme {
    fn default() -> Theme {
        let mut light = HashMap::default();
        let mut dark = HashMap::default();

        light.insert("general.background".into(), Color::Default);
        light.insert("general.foreground".into(), Color::Default);

        dark.insert("general.background".into(), Color::Default);
        dark.insert("general.foreground".into(), Color::Default);
        /*
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
            */
        light.insert("mail.listing.compact.even_fg".into(), Color::Default);
        light.insert("mail.listing.compact.even_bg".into(), Color::Byte(252));
        light.insert("mail.listing.compact.odd_fg".into(), Color::Default);
        light.insert("mail.listing.compact.odd_bg".into(), Color::Default);
        light.insert("mail.listing.compact.unseen_fg".into(), Color::Byte(0));
        light.insert("mail.listing.compact.unseen_bg".into(), Color::Byte(251));
        light.insert("mail.listing.compact.selected_fg".into(), Color::Default);
        light.insert("mail.listing.compact.selected_bg".into(), Color::Byte(210));
        light.insert("mail.listing.compact.highlighted_fg".into(), Color::Default);
        light.insert(
            "mail.listing.compact.highlighted_bg".into(),
            Color::Byte(244),
        );

        dark.insert("mail.listing.compact.even_fg".into(), Color::Default);
        dark.insert("mail.listing.compact.even_bg".into(), Color::Byte(236));
        dark.insert("mail.listing.compact.odd_fg".into(), Color::Default);
        dark.insert("mail.listing.compact.odd_bg".into(), Color::Default);
        dark.insert("mail.listing.compact.unseen_fg".into(), Color::Byte(0));
        dark.insert("mail.listing.compact.unseen_bg".into(), Color::Byte(251));
        dark.insert("mail.listing.compact.selected_fg".into(), Color::Default);
        dark.insert("mail.listing.compact.selected_bg".into(), Color::Byte(210));
        dark.insert("mail.listing.compact.highlighted_fg".into(), Color::Default);
        dark.insert(
            "mail.listing.compact.highlighted_bg".into(),
            Color::Byte(246),
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
        "mail.listing.conversations.padding",
        "mail.listing.conversations.unseen_fg",
        "mail.listing.conversations.unseen_bg",
        "mail.listing.conversations.unseen_padding",
        "mail.view.headers_fg",
        "mail.view.headers_bg",
        "mail.view.body_fg",
        "mail.view.body_bg",
        */
        light.insert("mail.listing.attachment_flag_fg".into(), Color::Byte(103));
        light.insert("mail.listing.attachment_flag_bg".into(), Color::Default);
        light.insert("mail.listing.thread_snooze_flag_fg".into(), Color::Red);
        light.insert("mail.listing.thread_snooze_flag_bg".into(), Color::Default);
        dark.insert("mail.listing.attachment_flag_fg".into(), Color::Byte(103));
        dark.insert("mail.listing.attachment_flag_bg".into(), Color::Default);
        dark.insert("mail.listing.thread_snooze_flag_fg".into(), Color::Red);
        dark.insert("mail.listing.thread_snooze_flag_bg".into(), Color::Default);

        Theme { light, dark }
    }
}
