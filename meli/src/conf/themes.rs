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

//! Application themes.
//!
//! * An attribute is a triple of foreground color, background color and
//!   terminal attribute `ThemeValue`s.
//! * A `ThemeValue<T>` is either an actual value or the key name of another
//!   value to which it depends. The value is either `Color` or `Attr`.
//! * `ThemeAttributeInner` is an attribute triplet.
//! * `ThemeAttribute` is an attribute triplet with the links resolved.
//!
//! On startup a [DFS](https://en.wikipedia.org/wiki/Depth-first_search) is performed to see if there are any cycles in the link graph.

use std::{
    borrow::Cow,
    collections::HashSet,
    fmt::Write,
    ops::{Deref, DerefMut},
};

use indexmap::IndexMap;
use melib::{Error, Result};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use crate::{
    terminal::{Attr, Color},
    Context,
};

pub const LIGHT: &str = "light";
pub const DARK: &str = "dark";

#[inline(always)]
pub fn value(context: &Context, key: &'static str) -> ThemeAttribute {
    let theme = match context.settings.terminal.theme.as_str() {
        self::LIGHT => &context.settings.terminal.themes.light,
        self::DARK => &context.settings.terminal.themes.dark,
        t => context
            .settings
            .terminal
            .themes
            .other_themes
            .get(t)
            .unwrap_or(&context.settings.terminal.themes.dark),
    };
    unlink(theme, &Cow::from(key))
}

#[inline(always)]
pub fn fg_color(context: &Context, key: &'static str) -> Color {
    let theme = match context.settings.terminal.theme.as_str() {
        self::LIGHT => &context.settings.terminal.themes.light,
        self::DARK => &context.settings.terminal.themes.dark,
        t => context
            .settings
            .terminal
            .themes
            .other_themes
            .get(t)
            .unwrap_or(&context.settings.terminal.themes.dark),
    };
    unlink_fg(theme, &ColorField::Fg, &Cow::from(key))
}

#[inline(always)]
pub fn bg_color(context: &Context, key: &'static str) -> Color {
    let theme = match context.settings.terminal.theme.as_str() {
        self::LIGHT => &context.settings.terminal.themes.light,
        self::DARK => &context.settings.terminal.themes.dark,
        t => context
            .settings
            .terminal
            .themes
            .other_themes
            .get(t)
            .unwrap_or(&context.settings.terminal.themes.dark),
    };
    unlink_bg(theme, &ColorField::Bg, &Cow::from(key))
}

#[inline(always)]
pub fn attrs(context: &Context, key: &'static str) -> Attr {
    let theme = match context.settings.terminal.theme.as_str() {
        self::LIGHT => &context.settings.terminal.themes.light,
        self::DARK => &context.settings.terminal.themes.dark,
        t => context
            .settings
            .terminal
            .themes
            .other_themes
            .get(t)
            .unwrap_or(&context.settings.terminal.themes.dark),
    };
    unlink_attrs(theme, &Cow::from(key))
}

#[inline(always)]
fn unlink<'k, 't: 'k>(theme: &'t Theme, key: &'k str) -> ThemeAttribute {
    ThemeAttribute {
        fg: unlink_fg(theme, &ColorField::Fg, key),
        bg: unlink_bg(theme, &ColorField::Bg, key),
        attrs: unlink_attrs(theme, key),
    }
}

#[inline(always)]
fn unlink_fg<'k, 't: 'k>(theme: &'t Theme, mut field: &'k ColorField, mut key: &'k str) -> Color {
    loop {
        match field {
            ColorField::LikeSelf | ColorField::Fg => match &theme[key].fg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Alias(ref alias_ident) => {
                    let mut alias_ident = alias_ident;
                    'self_alias_loop: loop {
                        match &theme.color_aliases[alias_ident.as_ref()] {
                            ThemeValue::Link(ref new_key, ref new_field) => {
                                key = new_key;
                                field = new_field;
                                break 'self_alias_loop;
                            }
                            ThemeValue::Alias(ref new_alias_ident) => alias_ident = new_alias_ident,
                            ThemeValue::Value(val) => return *val,
                        }
                    }
                }

                ThemeValue::Value(val) => return *val,
            },
            ColorField::Bg => match &theme[key].bg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Alias(ref alias_ident) => {
                    let mut alias_ident = alias_ident;
                    'other_alias_loop: loop {
                        match &theme.color_aliases[alias_ident.as_ref()] {
                            ThemeValue::Link(ref new_key, ref new_field) => {
                                key = new_key;
                                field = new_field;
                                break 'other_alias_loop;
                            }
                            ThemeValue::Alias(ref new_alias_ident) => alias_ident = new_alias_ident,
                            ThemeValue::Value(val) => return *val,
                        }
                    }
                }
                ThemeValue::Value(val) => return *val,
            },
        }
    }
}

#[inline(always)]
fn unlink_bg<'k, 't: 'k>(theme: &'t Theme, mut field: &'k ColorField, mut key: &'k str) -> Color {
    loop {
        match field {
            ColorField::LikeSelf | ColorField::Bg => match &theme[key].bg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Alias(ref alias_ident) => {
                    let mut alias_ident = alias_ident;
                    'self_alias_loop: loop {
                        match &theme.color_aliases[alias_ident.as_ref()] {
                            ThemeValue::Link(ref new_key, ref new_field) => {
                                key = new_key;
                                field = new_field;
                                break 'self_alias_loop;
                            }
                            ThemeValue::Alias(ref new_alias_ident) => alias_ident = new_alias_ident,
                            ThemeValue::Value(val) => return *val,
                        }
                    }
                }
                ThemeValue::Value(val) => return *val,
            },
            ColorField::Fg => match &theme[key].fg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Alias(ref alias_ident) => {
                    let mut alias_ident = alias_ident;
                    'other_alias_loop: loop {
                        match &theme.color_aliases[alias_ident.as_ref()] {
                            ThemeValue::Link(ref new_key, ref new_field) => {
                                key = new_key;
                                field = new_field;
                                break 'other_alias_loop;
                            }
                            ThemeValue::Alias(ref new_alias_ident) => alias_ident = new_alias_ident,
                            ThemeValue::Value(val) => return *val,
                        }
                    }
                }
                ThemeValue::Value(val) => return *val,
            },
        }
    }
}

#[inline(always)]
fn unlink_attrs<'k, 't: 'k>(theme: &'t Theme, mut key: &'k str) -> Attr {
    loop {
        match &theme[key].attrs {
            ThemeValue::Link(ref new_key, ()) => key = new_key,
            ThemeValue::Alias(ref alias_ident) => {
                let mut alias_ident = alias_ident;
                'alias_loop: loop {
                    match &theme.attr_aliases[alias_ident.as_ref()] {
                        ThemeValue::Link(ref new_key, ()) => {
                            key = new_key;
                            break 'alias_loop;
                        }
                        ThemeValue::Alias(ref new_alias_ident) => alias_ident = new_alias_ident,
                        ThemeValue::Value(val) => return *val,
                    }
                }
            }
            ThemeValue::Value(val) => return *val,
        }
    }
}

const DEFAULT_KEYS: &[&str] = &[
    "theme_default",
    "text.normal",
    "text.unfocused",
    "text.error",
    "text.highlight",
    "error_message",
    "highlight",
    "status.bar",
    "status.command_bar",
    "status.history",
    "status.history.hints",
    "status.notification",
    "tab.focused",
    "tab.unfocused",
    "tab.bar",
    "widgets.list.header",
    "widgets.form.label",
    "widgets.form.field",
    "widgets.form.highlighted",
    "widgets.options.highlighted",
    "mail.sidebar",
    "mail.sidebar_divider",
    "mail.sidebar_account_name",
    "mail.sidebar_unread_count",
    "mail.sidebar_index",
    "mail.sidebar_highlighted",
    "mail.sidebar_highlighted_account_name",
    "mail.sidebar_highlighted_unread_count",
    "mail.sidebar_highlighted_index",
    "mail.sidebar_highlighted_account",
    "mail.sidebar_highlighted_account_unread_count",
    "mail.sidebar_highlighted_account_index",
    "mail.listing.compact.even",
    "mail.listing.compact.odd",
    "mail.listing.compact.even_unseen",
    "mail.listing.compact.odd_unseen",
    "mail.listing.compact.even_selected",
    "mail.listing.compact.odd_selected",
    "mail.listing.compact.even_highlighted",
    "mail.listing.compact.odd_highlighted",
    "mail.listing.compact.even_highlighted_selected",
    "mail.listing.compact.odd_highlighted_selected",
    "mail.listing.plain.even",
    "mail.listing.plain.odd",
    "mail.listing.plain.even_unseen",
    "mail.listing.plain.odd_unseen",
    "mail.listing.plain.even_selected",
    "mail.listing.plain.odd_selected",
    "mail.listing.plain.even_highlighted",
    "mail.listing.plain.odd_highlighted",
    "mail.listing.plain.even_highlighted_selected",
    "mail.listing.plain.odd_highlighted_selected",
    "mail.listing.conversations",
    "mail.listing.conversations.subject",
    "mail.listing.conversations.from",
    "mail.listing.conversations.date",
    "mail.listing.conversations.unseen",
    "mail.listing.conversations.highlighted",
    "mail.listing.conversations.selected",
    "mail.listing.conversations.highlighted_selected",
    "mail.view.headers",
    "mail.view.headers_names",
    "mail.view.headers_area",
    "mail.view.body",
    "mail.view.thread.indentation.a",
    "mail.view.thread.indentation.b",
    "mail.view.thread.indentation.c",
    "mail.view.thread.indentation.d",
    "mail.view.thread.indentation.e",
    "mail.view.thread.indentation.f",
    "mail.listing.attachment_flag",
    "mail.listing.thread_snooze_flag",
    "mail.listing.tag_default",
    "mail.listing.highlight_self",
    "pager.highlight_search",
    "pager.highlight_search_current",
];

/// `ThemeAttributeInner` but with the links resolved.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ThemeAttribute {
    pub fg: Color,
    pub bg: Color,
    pub attrs: Attr,
}

/// Holds {fore,back}ground color and terminal attribute values.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ThemeAttributeInner {
    #[serde(default)]
    fg: ThemeValue<Color>,
    #[serde(default)]
    bg: ThemeValue<Color>,
    #[serde(default)]
    attrs: ThemeValue<Attr>,
}

impl Default for ThemeAttributeInner {
    fn default() -> Self {
        Self {
            fg: "theme_default".into(),
            bg: "theme_default".into(),
            attrs: "theme_default".into(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ColorField {
    // Like self, i.e. either Fg or Bg
    LikeSelf,
    Fg,
    Bg,
}

/// The field a `ThemeValue::Link` refers to.
trait ThemeLink {
    type LinkType;
}

/// A color value that's a link can either refer to `.fg` or `.bg` field
impl ThemeLink for Color {
    type LinkType = ColorField;
}

/// An `attr` value that's a link can only refer to an `.attr` field
impl ThemeLink for Attr {
    type LinkType = ();
}

#[derive(Clone, Debug)]
/// Holds either an actual value or refers to the key name of the attribute that
/// holds the value.
enum ThemeValue<T: ThemeLink> {
    Value(T),
    Alias(Cow<'static, str>),
    Link(Cow<'static, str>, T::LinkType),
}

impl From<&'static str> for ThemeValue<Color> {
    fn from(from: &'static str) -> Self {
        Self::Link(from.into(), ColorField::LikeSelf)
    }
}

impl From<&'static str> for ThemeValue<Attr> {
    fn from(from: &'static str) -> Self {
        Self::Link(from.into(), ())
    }
}

impl From<Color> for ThemeValue<Color> {
    fn from(from: Color) -> Self {
        Self::Value(from)
    }
}

impl From<Attr> for ThemeValue<Attr> {
    fn from(from: Attr) -> Self {
        Self::Value(from)
    }
}

impl Default for ThemeValue<Color> {
    fn default() -> Self {
        Self::Value(Color::Default)
    }
}

impl Default for ThemeValue<Attr> {
    fn default() -> Self {
        Self::Value(Attr::DEFAULT)
    }
}

impl<'de> Deserialize<'de> for ThemeValue<Attr> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            if let Some(stripped) = s.strip_prefix('$') {
                Ok(Self::Alias(stripped.to_string().into()))
            } else if let Ok(c) = Attr::from_string_de::<'de, D, String>(s.clone()) {
                Ok(Self::Value(c))
            } else {
                Ok(Self::Link(s.into(), ()))
            }
        } else {
            Err(de::Error::custom("invalid theme attribute value"))
        }
    }
}

impl Serialize for ThemeValue<Attr> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Value(s) => s.serialize(serializer),
            Self::Alias(s) => format!("${}", s).serialize(serializer),
            Self::Link(s, ()) => serializer.serialize_str(s.as_ref()),
        }
    }
}

impl Serialize for ThemeValue<Color> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Value(s) => s.serialize(serializer),
            Self::Alias(s) => format!("${}", s).serialize(serializer),
            Self::Link(s, ColorField::LikeSelf) => serializer.serialize_str(s.as_ref()),
            Self::Link(s, ColorField::Fg) => serializer.serialize_str(format!("{}.fg", s).as_ref()),
            Self::Link(s, ColorField::Bg) => serializer.serialize_str(format!("{}.bg", s).as_ref()),
        }
    }
}

impl<'de> Deserialize<'de> for ThemeValue<Color> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            if let Some(stripped) = s.strip_prefix('$') {
                Ok(Self::Alias(stripped.to_string().into()))
            } else if let Ok(c) = Color::from_string_de::<'de, D>(s.clone()) {
                Ok(Self::Value(c))
            } else if s.ends_with(".fg") {
                Ok(Self::Link(
                    s[..s.len() - 3].to_string().into(),
                    ColorField::Fg,
                ))
            } else if s.ends_with(".bg") {
                Ok(Self::Link(
                    s[..s.len() - 3].to_string().into(),
                    ColorField::Bg,
                ))
            } else {
                Ok(Self::Link(s.into(), ColorField::LikeSelf))
            }
        } else {
            Err(de::Error::custom("invalid theme color value"))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Themes {
    pub light: Theme,
    pub dark: Theme,
    pub other_themes: IndexMap<String, Theme>,
}

#[derive(Clone, Debug)]
pub struct Theme {
    color_aliases: IndexMap<Cow<'static, str>, ThemeValue<Color>>,
    attr_aliases: IndexMap<Cow<'static, str>, ThemeValue<Attr>>,
    #[cfg(feature = "regexp")]
    text_format_regexps: IndexMap<Cow<'static, str>, SmallVec<[TextFormatterSetting; 32]>>,
    pub keys: IndexMap<Cow<'static, str>, ThemeAttributeInner>,
}

#[cfg(feature = "regexp")]
pub use regexp::text_format_regexps;
#[cfg(feature = "regexp")]
use regexp::*;

#[cfg(feature = "regexp")]
mod regexp {
    use super::*;
    use crate::terminal::FormatTag;

    pub(super) const DEFAULT_TEXT_FORMATTER_KEYS: &[&str] =
        &["pager.envelope.body", "listing.from", "listing.subject"];

    #[derive(Clone)]
    pub struct RegexpWrapper(pub pcre2::bytes::Regex);

    #[derive(Clone, Debug)]
    pub(super) struct TextFormatterSetting {
        pub(super) regexp: RegexpWrapper,
        pub(super) fg: Option<ThemeValue<Color>>,
        pub(super) bg: Option<ThemeValue<Color>>,
        pub(super) attrs: Option<ThemeValue<Attr>>,
        pub(super) priority: u8,
    }

    #[derive(Clone, Debug)]
    pub struct TextFormatter<'r> {
        pub regexp: &'r RegexpWrapper,
        pub tag: FormatTag,
    }

    impl Default for RegexpWrapper {
        fn default() -> Self {
            Self(pcre2::bytes::Regex::new("").unwrap())
        }
    }

    impl std::fmt::Debug for RegexpWrapper {
        fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
            std::fmt::Debug::fmt(self.0.as_str(), fmt)
        }
    }

    impl std::hash::Hash for RegexpWrapper {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.0.as_str().hash(state)
        }
    }

    impl Eq for RegexpWrapper {}

    impl PartialEq for RegexpWrapper {
        fn eq(&self, other: &Self) -> bool {
            self.0.as_str().eq(other.0.as_str())
        }
    }

    impl RegexpWrapper {
        pub(super) fn new(
            pattern: &str,
            caseless: bool,
            dotall: bool,
            extended: bool,
            multi_line: bool,
            ucp: bool,
            jit_if_available: bool,
        ) -> std::result::Result<Self, pcre2::Error> {
            Ok(Self(
                pcre2::bytes::RegexBuilder::new()
                    .caseless(caseless)
                    .dotall(dotall)
                    .extended(extended)
                    .multi_line(multi_line)
                    .ucp(ucp)
                    .jit_if_available(jit_if_available)
                    .build(pattern)?,
            ))
        }

        pub fn find_iter<'w, 's>(&'w self, s: &'s str) -> FindIter<'w, 's> {
            FindIter {
                pcre_iter: self.0.find_iter(s.as_bytes()),
                char_indices: s.char_indices(),
                char_offset: 0,
            }
        }
    }

    pub struct FindIter<'r, 's> {
        pcre_iter: pcre2::bytes::Matches<'r, 's>,
        char_indices: std::str::CharIndices<'s>,
        char_offset: usize,
    }

    impl<'r, 's> Iterator for FindIter<'r, 's> {
        type Item = (usize, usize);

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                let next_byte_offset = self.pcre_iter.next()?;
                if next_byte_offset.is_err() {
                    continue;
                }
                let next_byte_offset = next_byte_offset.unwrap();

                let mut next_char_index = self.char_indices.next()?;

                while next_byte_offset.start() < next_char_index.0 {
                    self.char_offset += 1;
                    next_char_index = self.char_indices.next()?;
                }
                let start = self.char_offset;

                while next_byte_offset.end()
                    > self
                        .char_indices
                        .next()
                        .map(|(v, _)| v)
                        .unwrap_or_else(|| next_byte_offset.end())
                {
                    self.char_offset += 1;
                }
                let end = self.char_offset + 1;

                return Some((start, end));
            }
        }
    }

    #[inline(always)]
    pub fn text_format_regexps<'ctx>(
        context: &'ctx Context,
        key: &'static str,
    ) -> SmallVec<[TextFormatter<'ctx>; 64]> {
        let theme = match context.settings.terminal.theme.as_str() {
            self::LIGHT => &context.settings.terminal.themes.light,
            self::DARK => &context.settings.terminal.themes.dark,
            t => context
                .settings
                .terminal
                .themes
                .other_themes
                .get(t)
                .unwrap_or(&context.settings.terminal.themes.dark),
        };
        theme.text_format_regexps[&Cow::from(key)]
            .iter()
            .map(|v| TextFormatter {
                regexp: &v.regexp,
                tag: FormatTag {
                    fg: v.fg.as_ref().map(|v| match v {
                        ThemeValue::Link(ref key, ref field) => unlink_fg(theme, field, key),
                        ThemeValue::Alias(ref alias_ident) => {
                            let mut alias_ident = alias_ident;
                            let ret;
                            'fg_alias_loop: loop {
                                match &theme.color_aliases[alias_ident.as_ref()] {
                                    ThemeValue::Link(ref new_key, ref new_field) => {
                                        ret = unlink_fg(theme, new_field, new_key);
                                        break 'fg_alias_loop;
                                    }

                                    ThemeValue::Alias(ref new_alias_ident) => {
                                        alias_ident = new_alias_ident
                                    }
                                    ThemeValue::Value(val) => {
                                        ret = *val;
                                        break 'fg_alias_loop;
                                    }
                                }
                            }
                            ret
                        }
                        ThemeValue::Value(val) => *val,
                    }),
                    bg: v.bg.as_ref().map(|v| match v {
                        ThemeValue::Link(ref key, ref field) => unlink_bg(theme, field, key),
                        ThemeValue::Alias(ref alias_ident) => {
                            let mut alias_ident = alias_ident;
                            let ret;
                            'bg_alias_loop: loop {
                                match &theme.color_aliases[alias_ident.as_ref()] {
                                    ThemeValue::Link(ref new_key, ref new_field) => {
                                        ret = unlink_bg(theme, new_field, new_key);
                                        break 'bg_alias_loop;
                                    }

                                    ThemeValue::Alias(ref new_alias_ident) => {
                                        alias_ident = new_alias_ident
                                    }
                                    ThemeValue::Value(val) => {
                                        ret = *val;
                                        break 'bg_alias_loop;
                                    }
                                }
                            }
                            ret
                        }
                        ThemeValue::Value(val) => *val,
                    }),
                    attrs: v.attrs.as_ref().map(|v| match v {
                        ThemeValue::Link(ref key, ()) => unlink_attrs(theme, key),
                        ThemeValue::Alias(ref alias_ident) => {
                            let mut alias_ident = alias_ident;
                            let ret;
                            'attrs_alias_loop: loop {
                                match &theme.attr_aliases[alias_ident.as_ref()] {
                                    ThemeValue::Link(ref new_key, ()) => {
                                        ret = unlink_attrs(theme, new_key);
                                        break 'attrs_alias_loop;
                                    }
                                    ThemeValue::Alias(ref new_alias_ident) => {
                                        alias_ident = new_alias_ident
                                    }
                                    ThemeValue::Value(val) => {
                                        ret = *val;
                                        break 'attrs_alias_loop;
                                    }
                                }
                            }
                            ret
                        }
                        ThemeValue::Value(val) => *val,
                    }),
                    priority: v.priority,
                },
            })
            .collect()
    }
}

impl Deref for Theme {
    type Target = IndexMap<Cow<'static, str>, ThemeAttributeInner>;

    fn deref(&self) -> &Self::Target {
        &self.keys
    }
}

impl DerefMut for Theme {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.keys
    }
}

impl<'de> Deserialize<'de> for Themes {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[cfg(feature = "regexp")]
        const fn false_val() -> bool {
            false
        }
        #[cfg(feature = "regexp")]
        const fn true_val() -> bool {
            true
        }
        #[derive(Deserialize)]
        struct ThemesOptions {
            #[serde(default)]
            light: ThemeOptions,
            #[serde(default)]
            dark: ThemeOptions,
            #[serde(flatten, default)]
            other_themes: IndexMap<String, ThemeOptions>,
        }
        #[derive(Default, Deserialize)]
        struct ThemeOptions {
            #[serde(default)]
            color_aliases: IndexMap<Cow<'static, str>, ThemeValue<Color>>,
            #[serde(default)]
            attr_aliases: IndexMap<Cow<'static, str>, ThemeValue<Attr>>,
            #[cfg(feature = "regexp")]
            #[serde(default)]
            text_format_regexps: IndexMap<Cow<'static, str>, IndexMap<String, RegexpOptions>>,
            #[serde(flatten, default)]
            keys: IndexMap<Cow<'static, str>, ThemeAttributeInnerOptions>,
        }
        #[cfg(feature = "regexp")]
        #[derive(Default, Deserialize)]
        struct RegexpOptions {
            #[serde(default = "false_val")]
            caseless: bool,
            #[serde(default = "false_val")]
            dotall: bool,
            #[serde(default = "false_val")]
            extended: bool,
            #[serde(default = "false_val")]
            multi_line: bool,
            #[serde(default = "true_val")]
            ucp: bool,
            #[serde(default = "false_val")]
            jit_if_available: bool,
            #[serde(default)]
            priority: u8,
            #[serde(flatten)]
            rest: ThemeAttributeInnerOptions,
        }
        #[derive(Default, Deserialize)]
        #[serde(deny_unknown_fields)]
        struct ThemeAttributeInnerOptions {
            #[serde(default)]
            fg: Option<ThemeValue<Color>>,
            #[serde(default)]
            bg: Option<ThemeValue<Color>>,
            #[serde(default)]
            attrs: Option<ThemeValue<Attr>>,
        }

        let mut ret = Self::default();
        let mut s = <ThemesOptions>::deserialize(deserializer)?;
        for tk in s.other_themes.keys() {
            ret.other_themes.insert(tk.clone(), ret.dark.clone());
        }

        for (k, v) in ret.light.iter_mut() {
            if let Some(mut att) = s.light.keys.remove(k) {
                if let Some(att) = att.fg.take() {
                    v.fg = att;
                }
                if let Some(att) = att.bg.take() {
                    v.bg = att;
                }
                if let Some(att) = att.attrs.take() {
                    v.attrs = att;
                }
            }
        }
        if !s.light.keys.is_empty() {
            return Err(de::Error::custom(format!(
                "light theme contains unrecognized theme keywords: {}",
                s.light
                    .keys
                    .keys()
                    .map(|k| k.as_ref())
                    .collect::<SmallVec<[_; 128]>>()
                    .join(", ")
            )));
        }
        ret.light.color_aliases = s.light.color_aliases;
        ret.light.attr_aliases = s.light.attr_aliases;
        #[cfg(feature = "regexp")]
        for (k, v) in s.light.text_format_regexps {
            let mut acc = SmallVec::new();
            for (rs, v) in v {
                match RegexpWrapper::new(
                    &rs,
                    v.caseless,
                    v.dotall,
                    v.extended,
                    v.multi_line,
                    v.ucp,
                    v.jit_if_available,
                ) {
                    Ok(regexp) => {
                        acc.push(TextFormatterSetting {
                            regexp,
                            fg: v.rest.fg,
                            bg: v.rest.bg,
                            attrs: v.rest.attrs,
                            priority: v.priority,
                        });
                    }
                    Err(err) => {
                        return Err(de::Error::custom(err.to_string()));
                    }
                }
            }
            ret.light.text_format_regexps.insert(k, acc);
        }
        for (k, v) in ret.dark.iter_mut() {
            if let Some(mut att) = s.dark.keys.remove(k) {
                if let Some(att) = att.fg.take() {
                    v.fg = att;
                }
                if let Some(att) = att.bg.take() {
                    v.bg = att;
                }
                if let Some(att) = att.attrs.take() {
                    v.attrs = att;
                }
            }
        }
        if !s.dark.keys.is_empty() {
            return Err(de::Error::custom(format!(
                "dark theme contains unrecognized theme keywords: {}",
                s.dark
                    .keys
                    .keys()
                    .map(|k| k.as_ref())
                    .collect::<SmallVec<[_; 128]>>()
                    .join(", ")
            )));
        }
        ret.dark.color_aliases = s.dark.color_aliases;
        ret.dark.attr_aliases = s.dark.attr_aliases;
        #[cfg(feature = "regexp")]
        for (k, v) in s.dark.text_format_regexps {
            let mut acc = SmallVec::new();
            for (rs, v) in v {
                match RegexpWrapper::new(
                    &rs,
                    v.caseless,
                    v.dotall,
                    v.extended,
                    v.multi_line,
                    v.ucp,
                    v.jit_if_available,
                ) {
                    Ok(regexp) => {
                        acc.push(TextFormatterSetting {
                            regexp,
                            fg: v.rest.fg,
                            bg: v.rest.bg,
                            attrs: v.rest.attrs,
                            priority: v.priority,
                        });
                    }
                    Err(err) => {
                        return Err(de::Error::custom(err.to_string()));
                    }
                }
            }
            ret.dark.text_format_regexps.insert(k, acc);
        }
        for (tk, t) in ret.other_themes.iter_mut() {
            let mut theme = s.other_themes.remove(tk).unwrap();
            for (k, v) in t.iter_mut() {
                if let Some(mut att) = theme.keys.remove(k) {
                    if let Some(att) = att.fg.take() {
                        v.fg = att;
                    }
                    if let Some(att) = att.bg.take() {
                        v.bg = att;
                    }
                    if let Some(att) = att.attrs.take() {
                        v.attrs = att;
                    }
                }
            }
            if !theme.keys.is_empty() {
                return Err(de::Error::custom(format!(
                    "{} theme contains unrecognized theme keywords: {}",
                    tk,
                    theme
                        .keys
                        .keys()
                        .map(|k| k.as_ref())
                        .collect::<SmallVec<[_; 128]>>()
                        .join(", ")
                )));
            }
            t.color_aliases = theme.color_aliases;
            t.attr_aliases = theme.attr_aliases;
            #[cfg(feature = "regexp")]
            for (k, v) in theme.text_format_regexps {
                let mut acc = SmallVec::new();
                for (rs, v) in v {
                    match RegexpWrapper::new(
                        &rs,
                        v.caseless,
                        v.dotall,
                        v.extended,
                        v.multi_line,
                        v.ucp,
                        v.jit_if_available,
                    ) {
                        Ok(regexp) => {
                            acc.push(TextFormatterSetting {
                                regexp,
                                fg: v.rest.fg,
                                bg: v.rest.bg,
                                attrs: v.rest.attrs,
                                priority: v.priority,
                            });
                        }
                        Err(err) => {
                            return Err(de::Error::custom(err.to_string()));
                        }
                    }
                }
                t.text_format_regexps.insert(k, acc);
            }
        }
        Ok(ret)
    }
}

impl Themes {
    fn validate_keys(name: &str, theme: &Theme, hash_set: &HashSet<&'static str>) -> Result<()> {
        #[allow(unused_mut)]
        let mut keys = theme
            .keys()
            .filter_map(|k| {
                if !hash_set.contains(&k.as_ref()) {
                    Some((None, "key", "invalid key", k.as_ref()))
                } else {
                    None
                }
            })
            .chain(theme.color_aliases.iter().filter_map(|(key, a)| match a {
                ThemeValue::Link(ref r, ref field) => {
                    if !hash_set.contains(&r.as_ref()) {
                        Some((
                            Some(key),
                            match field {
                                ColorField::LikeSelf => "Color alias link",
                                ColorField::Fg => "Color alias fg link",
                                ColorField::Bg => "Color alias bg link",
                            },
                            "invalid key",
                            r.as_ref(),
                        ))
                    } else {
                        None
                    }
                }
                ThemeValue::Alias(ref ident) => {
                    if !theme.color_aliases.contains_key(ident.as_ref()) {
                        Some((Some(key), "alias", "nonexistent color alias", ident))
                    } else {
                        None
                    }
                }
                _ => None,
            }))
            .chain(theme.attr_aliases.iter().filter_map(|(key, a)| match a {
                ThemeValue::Link(ref r, ()) => {
                    if !hash_set.contains(&r.as_ref()) {
                        Some((Some(key), "Attr alias link", "invalid key", r.as_ref()))
                    } else {
                        None
                    }
                }
                ThemeValue::Alias(ref ident) => {
                    if !theme.attr_aliases.contains_key(ident.as_ref()) {
                        Some((Some(key), "alias", "nonexistent color alias", ident))
                    } else {
                        None
                    }
                }
                _ => None,
            }))
            .chain(theme.iter().filter_map(|(key, a)| {
                if let ThemeValue::Link(ref r, _) = a.fg {
                    if !hash_set.contains(&r.as_ref()) {
                        Some((Some(key), "fg link", "invalid key", r.as_ref()))
                    } else {
                        None
                    }
                } else if let ThemeValue::Alias(ref ident) = a.fg {
                    if !theme.color_aliases.contains_key(ident.as_ref()) {
                        Some((Some(key), "fg alias", "nonexistent color alias", ident))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }))
            .chain(theme.iter().filter_map(|(key, a)| {
                if let ThemeValue::Link(ref r, _) = a.bg {
                    if !hash_set.contains(&r.as_ref()) {
                        Some((Some(key), "bg link", "invalid key", r.as_ref()))
                    } else {
                        None
                    }
                } else if let ThemeValue::Alias(ref ident) = a.bg {
                    if !theme.color_aliases.contains_key(ident.as_ref()) {
                        Some((Some(key), "bg alias", "nonexistent color alias", ident))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }))
            .chain(theme.iter().filter_map(|(key, a)| {
                if let ThemeValue::Link(ref r, _) = a.attrs {
                    if !hash_set.contains(&r.as_ref()) {
                        Some((Some(key), "attrs link", "invalid key", r.as_ref()))
                    } else {
                        None
                    }
                } else if let ThemeValue::Alias(ref ident) = a.attrs {
                    if !theme.attr_aliases.contains_key(ident.as_ref()) {
                        Some((
                            Some(key),
                            "attrs alias",
                            "nonexistent text attribute alias",
                            ident,
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }))
            .collect::<SmallVec<[(Option<_>, &'_ str, &'_ str, &'_ str); 128]>>();
        #[cfg(feature = "regexp")]
        {
            for (key, v) in &theme.text_format_regexps {
                if !regexp::DEFAULT_TEXT_FORMATTER_KEYS.contains(&key.as_ref()) {
                    keys.push((
                        None,
                        "key",
                        "invalid key in `text_format_regexps`",
                        key.as_ref(),
                    ));
                } else {
                    for tfs in v {
                        if let Some(fg) = &tfs.fg {
                            if let ThemeValue::Link(ref r, _) = fg {
                                if !hash_set.contains(&r.as_ref()) {
                                    keys.push((
                                        Some(key),
                                        "fg link",
                                        "invalid key in `text_format_regexps`",
                                        r.as_ref(),
                                    ));
                                }
                            } else if let ThemeValue::Alias(ref ident) = fg {
                                if !theme.color_aliases.contains_key(ident.as_ref()) {
                                    keys.push((
                                        Some(key),
                                        "fg alias",
                                        "nonexistent color alias in `text_format_regexps`",
                                        ident,
                                    ));
                                }
                            }
                        }
                        if let Some(bg) = &tfs.bg {
                            if let ThemeValue::Link(ref r, _) = bg {
                                if !hash_set.contains(&r.as_ref()) {
                                    keys.push((
                                        Some(key),
                                        "bg link",
                                        "invalid key in `text_format_regexps`",
                                        r.as_ref(),
                                    ));
                                }
                            } else if let ThemeValue::Alias(ref ident) = bg {
                                if !theme.color_aliases.contains_key(ident.as_ref()) {
                                    keys.push((
                                        Some(key),
                                        "bg alias",
                                        "nonexistent color alias in `text_format_regexps`",
                                        ident,
                                    ));
                                }
                            }
                        }
                        if let Some(attrs) = &tfs.attrs {
                            if let ThemeValue::Link(ref r, _) = attrs {
                                if !hash_set.contains(&r.as_ref()) {
                                    keys.push((
                                        Some(key),
                                        "attrs link",
                                        "invalid key in `text_format_regexps`",
                                        r.as_ref(),
                                    ));
                                }
                            } else if let ThemeValue::Alias(ref ident) = attrs {
                                if !theme.attr_aliases.contains_key(ident.as_ref()) {
                                    keys.push((
                                        Some(key),
                                        "attrs alias",
                                        "nonexistent text attribute alias in `text_format_regexps`",
                                        ident,
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }

        if !keys.is_empty() {
            return Err(format!(
                "{} theme contains invalid data: {}",
                name,
                keys.into_iter()
                    .map(|(key_opt, desc, kind, link)| if let Some(key) = key_opt {
                        format!("{} {}: {} \"{}\"", key, desc, kind, link)
                    } else {
                        format!("{}: {} \"{}\"", desc, kind, link)
                    })
                    .collect::<SmallVec<[String; 128]>>()
                    .join(", ")
            )
            .into());
        }
        Ok(())
    }
    pub fn validate(&self) -> Result<()> {
        let hash_set: HashSet<&'static str> = DEFAULT_KEYS.iter().copied().collect();
        Self::validate_keys(self::LIGHT, &self.light, &hash_set)?;
        Self::validate_keys(self::DARK, &self.dark, &hash_set)?;
        for (name, t) in self.other_themes.iter() {
            Self::validate_keys(name, t, &hash_set)?;
        }
        if let Err(err) = is_cyclic(&self.light) {
            return Err(Error::new(format!("light theme contains a cycle: {}", err)));
        }
        if let Err(err) = is_cyclic(&self.dark) {
            return Err(Error::new(format!("dark theme contains a cycle: {}", err)));
        }
        for (k, t) in self.other_themes.iter() {
            if let Err(err) = is_cyclic(t) {
                return Err(Error::new(format!("{} theme contains a cycle: {}", k, err)));
            }
        }
        Ok(())
    }

    pub fn key_to_string(&self, key: &str, unlink: bool) -> String {
        let theme = match key {
            self::LIGHT => &self.light,
            self::DARK => &self.dark,
            t => self.other_themes.get(t).unwrap_or(&self.dark),
        };
        let mut ret = String::new();
        let _ = writeln!(ret, "[terminal.themes.{}]", key);
        if unlink {
            for k in theme.keys() {
                let _ = writeln!(
                    ret,
                    "\"{}\" = {{ fg = {}, bg = {}, attrs = {} }}",
                    k,
                    toml::to_string(&unlink_fg(theme, &ColorField::Fg, k)).unwrap(),
                    toml::to_string(&unlink_bg(theme, &ColorField::Bg, k)).unwrap(),
                    toml::to_string(&unlink_attrs(theme, k)).unwrap(),
                );
            }
        } else {
            for k in theme.keys() {
                let _ = writeln!(
                    ret,
                    "\"{}\" = {{ fg = {}, bg = {}, attrs = {} }}",
                    k,
                    toml::to_string(&theme[k].fg).unwrap(),
                    toml::to_string(&theme[k].bg).unwrap(),
                    toml::to_string(&theme[k].attrs).unwrap(),
                );
            }
        }
        ret
    }
}

impl std::fmt::Display for Themes {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut ret = String::new();
        ret.push_str(&self.key_to_string(self::DARK, true));

        ret.push_str("\n\n");
        ret.push_str(&self.key_to_string(self::LIGHT, true));
        for name in self.other_themes.keys() {
            ret.push_str("\n\n");
            ret.push_str(&self.key_to_string(name, true));
        }
        write!(fmt, "{}", ret)
    }
}

impl Default for Themes {
    #[allow(clippy::needless_update)]
    fn default() -> Self {
        let mut light = IndexMap::default();
        let mut dark = IndexMap::default();
        let other_themes = IndexMap::default();

        macro_rules! add {
            ($key:literal, $($theme:ident={ $($name:ident : $val:expr),*$(,)? }),*$(,)?) => {
                add!($key);
                $($theme.insert($key.into(), ThemeAttributeInner {
                    $($name: $val.into()),*
                        ,..ThemeAttributeInner::default() }));*
            };
            ($key:literal) => {
                light.insert($key.into(), ThemeAttributeInner::default());
                dark.insert($key.into(), ThemeAttributeInner::default());
            };
            ($key:literal, $copy_from:literal) => {
                light.insert($key.into(), light[$copy_from].clone());
                dark.insert($key.into(), dark[$copy_from].clone());
            };
        }
        add!("theme_default", dark = { fg: Color::Default, bg: Color::Default, attrs: Attr::DEFAULT }, light = { fg: Color::Default, bg: Color::Default, attrs: Attr::DEFAULT });

        add!("error_message", dark = { fg: Color::Red, bg: "theme_default", attrs: "theme_default" }, light = { fg: Color::Red, bg: "theme_default", attrs: "theme_default" });

        /* text palettes */
        add!("text.normal", "theme_default");
        add!("text.unfocused", dark = { fg: Color::GREY, bg: "theme_default", attrs: Attr::DIM }, light = { fg: Color::GREY, bg: "theme_default", attrs: Attr::DIM });
        add!("text.error", "error_message");
        add!("text.highlight", dark = { fg: Color::Blue, bg: "theme_default", attrs: Attr::REVERSE }, light = { fg: Color::Blue, bg: "theme_default", attrs: Attr::REVERSE });

        /* rest */
        add!("highlight", dark = { fg: Color::Byte(240), bg: Color::Byte(237), attrs: Attr::BOLD }, light = { fg: Color::Byte(240), bg: Color::Byte(237), attrs: Attr::BOLD });

        add!("status.bar", dark = { fg: Color::Byte(123), bg: Color::Byte(26) }, light = { fg: Color::Byte(123), bg: Color::Byte(26) });
        add!("status.command_bar", dark = { fg: Color::Byte(219), bg: Color::Byte(88) }, light = { fg: Color::Byte(219), bg: Color::Byte(88) });
        add!("status.history", dark = { fg: Color::Byte(197), bg: Color::Byte(174) }, light = { fg: Color::Byte(197), bg: Color::Byte(174) });
        add!("status.history.hints", dark = { fg: Color::Black, bg: "status.command_bar" }, light = { fg: Color::Black, bg: "status.command_bar" });
        add!("status.notification", dark = { fg: Color::Byte(219), bg: Color::Default }, light = { fg: Color::Byte(219), bg: Color::Default });

        add!("tab.focused");
        add!("tab.unfocused", dark = { fg: Color::Byte(15), bg: Color::Byte(8), }, light = { fg: Color::Byte(15), bg: Color::Byte(8), });
        add!("tab.bar");
        add!(
            "widgets.list.header",
            dark = { fg: Color::Black, bg: Color::White, attrs: Attr::BOLD },
            light = {fg: Color::White, bg: Color::Black, attrs: Attr::BOLD }
        );
        add!(
            "widgets.form.label",
            dark = { attrs: Attr::BOLD },
            light = { attrs: Attr::BOLD }
        );
        add!("widgets.form.field");
        add!("widgets.form.highlighted", light = { bg: Color::Byte(246) }, dark = { bg: Color::Byte(246) });
        add!("widgets.options.highlighted", light = { bg: Color::Byte(8) }, dark = { bg: Color::Byte(8) });

        /* Mail Sidebar */

        add!("mail.sidebar");
        add!("mail.sidebar_divider");
        add!(
            "mail.sidebar_account_name",
            dark = {
                fg: "mail.sidebar",
                bg: "mail.sidebar",
                attrs: Attr::BOLD,
            },
            light = {
                fg: "mail.sidebar",
                bg: "mail.sidebar",
                attrs: Attr::BOLD,
            }
        );
        add!("mail.sidebar_unread_count", dark = { fg: Color::Byte(243) });
        add!("mail.sidebar_index", dark = { fg: Color::Byte(243) });
        add!("mail.sidebar_highlighted", dark = { fg: Color::Byte(233), bg: Color::Byte(15) });
        add!(
            "mail.sidebar_highlighted_unread_count",
            light = {
                fg: "mail.sidebar_highlighted",
                bg: "mail.sidebar_highlighted"
            },
            dark = {
                fg: "mail.sidebar_highlighted",
                bg: "mail.sidebar_highlighted"
            }
        );
        add!(
            "mail.sidebar_highlighted_index",
            light = {
                fg: "mail.sidebar_index",
                bg: "mail.sidebar_highlighted",
            },
            dark = {
                fg: "mail.sidebar_index",
                bg: "mail.sidebar_highlighted",
            },
        );
        add!(
            "mail.sidebar_highlighted_account",
            dark = {
                fg: Color::Byte(15),
                bg: Color::Byte(233),
            }
        );
        add!(
            "mail.sidebar_highlighted_account_name",
            dark = {
                fg: "mail.sidebar_highlighted_account",
                bg: "mail.sidebar_highlighted_account",
                attrs: Attr::BOLD,
            },
            light = {
                fg: "mail.sidebar_highlighted_account",
                bg: "mail.sidebar_highlighted_account",
                attrs: Attr::BOLD,
            }
        );
        add!(
            "mail.sidebar_highlighted_account_unread_count",
            light = {
                fg: "mail.sidebar_unread_count",
                bg: "mail.sidebar_highlighted_account",
            },
            dark = {
                fg: "mail.sidebar_unread_count",
                bg: "mail.sidebar_highlighted_account"
            }
        );
        add!(
            "mail.sidebar_highlighted_account_index",
            light = {
                fg: "mail.sidebar_index",
                bg: "mail.sidebar_highlighted_account"
            },
            dark = {
                fg: "mail.sidebar_index",
                bg: "mail.sidebar_highlighted_account"
            }
        );

        /* CompactListing */
        add!("mail.listing.compact.even",
            dark = {
                bg: Color::Byte(236)
            },
            light = {
                bg: Color::Byte(252)
            }
        );
        add!("mail.listing.compact.odd");
        add!(
            "mail.listing.compact.even_unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!(
            "mail.listing.compact.odd_unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!("mail.listing.compact.even_selected",
            dark = {
                bg: Color::Byte(210)
            },
            light = {
                bg: Color::Byte(210)
            }
        );
        add!("mail.listing.compact.odd_selected",
            dark = {
                bg: Color::Byte(210)
            },
            light = {
                bg: Color::Byte(210)
            }
        );
        add!(
            "mail.listing.compact.even_highlighted",
            dark = {
                bg: Color::Byte(246)
            },
            light = {
                bg: Color::Byte(244)
            }
        );
        add!(
            "mail.listing.compact.odd_highlighted",
            dark = {
                bg: Color::Byte(246)
            },
            light = {
                bg: Color::Byte(244)
            }
        );
        add!("mail.listing.compact.even_highlighted_selected",
            dark = {
                bg: Color::Byte(210),
                attrs: Attr::REVERSE,
            },
            light = {
                bg: Color::Byte(210),
                attrs: Attr::REVERSE,
            }
        );
        add!(
            "mail.listing.compact.odd_highlighted_selected",
            "mail.listing.compact.even_highlighted_selected"
        );

        /* ConversationsListing */

        add!("mail.listing.conversations",
            dark = {
                /* Grey */
                fg: Color::Byte(8),
            },
            light = {
                /* Grey */
                fg: Color::Byte(8),
            }
        );
        add!("mail.listing.conversations.subject");
        add!("mail.listing.conversations.from",
            dark = {
                /* Grey */
                fg: Color::Byte(8),
            },
            light = {
                /* Grey */
                fg: Color::Byte(8),
            }
        );
        add!("mail.listing.conversations.date",
            dark = {
                fg: Color::Magenta,
            },
            light = {
                fg: Color::Magenta,
            }
        );
        add!(
            "mail.listing.conversations.unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!(
            "mail.listing.conversations.highlighted",
            dark = {
                bg: Color::Byte(246),
                attrs: Attr::BOLD,
            },
            light = {
                bg: Color::Byte(246),
                attrs: Attr::BOLD,
            }
        );
        add!("mail.listing.conversations.selected",
            dark = {
                bg: Color::Byte(210),
            },
            light = {
                bg: Color::Byte(210)
            }
        );

        add!("mail.listing.conversations.highlighted_selected",
            dark = {
                bg: Color::Byte(210),
                attrs: Attr::REVERSE,
            },
            light = {
                bg: Color::Byte(210),
                attrs: Attr::REVERSE,
            }
        );

        /* PlainListing */
        add!("mail.listing.plain.even",
            dark = {
                bg: Color::Byte(236)
            },
            light = {
                bg: Color::Byte(252)
            }
        );
        add!("mail.listing.plain.odd");
        add!(
            "mail.listing.plain.even_unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)

            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!(
            "mail.listing.plain.odd_unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)

            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!("mail.listing.plain.even_selected",
            dark = {
                bg: Color::Byte(210)
            },
            light = {
                bg: Color::Byte(210)
            }
        );
        add!("mail.listing.plain.odd_selected",
            dark = {
                bg: Color::Byte(210)
            },
            light = {
                bg: Color::Byte(210)
            }
        );
        add!(
            "mail.listing.plain.even_highlighted",
            dark = {
                bg: Color::Byte(246)
            },
            light = {
                bg: Color::Byte(244)
            }
        );
        add!(
            "mail.listing.plain.odd_highlighted",
            dark = {
                bg: Color::Byte(246)
            },
            light = {
                bg: Color::Byte(244)
            }
        );
        add!("mail.listing.plain.even_highlighted_selected",
            dark = {
                bg: Color::Byte(210),
                attrs: Attr::REVERSE,
            },
            light = {
                bg: Color::Byte(210),
                attrs: Attr::REVERSE,
            }
        );
        add!(
            "mail.listing.plain.odd_highlighted_selected",
            "mail.listing.plain.even_highlighted_selected"
        );

        add!(
            "mail.view.headers",
            dark = {
                fg: Color::Byte(33),
            },
            light = {
                fg: Color::Black,
            }
        );
        add!(
            "mail.view.headers_names",
            light = {
                fg: "mail.view.headers",
                bg: "mail.view.headers",
                attrs: "mail.view.headers",
            },
            dark = {
                fg: "mail.view.headers",
                bg: "mail.view.headers",
                attrs: "mail.view.headers",
            }
        );
        add!("mail.view.headers_area");
        add!("mail.view.body");
        add!("mail.view.thread.indentation.a", light = { bg: Color::Byte(69) }, dark = { bg: Color::Byte(69) }); // CornflowerBlue
        add!("mail.view.thread.indentation.b", light = { bg: Color::Byte(196) }, dark = { bg: Color::Byte(196) }); // Red1
        add!("mail.view.thread.indentation.c", light = { bg: Color::Byte(175) }, dark = { bg: Color::Byte(175) }); // Pink3
        add!("mail.view.thread.indentation.d", light = { bg: Color::Byte(220) }, dark = { bg: Color::Byte(220) }); // Gold1
        add!("mail.view.thread.indentation.e", light = { bg: Color::Byte(172) }, dark = { bg: Color::Byte(172) }); // Orange3
        add!("mail.view.thread.indentation.f", light = { bg: Color::Byte(72) }, dark = { bg: Color::Byte(72) }); // CadetBlue

        add!(
            "mail.listing.attachment_flag",
            light = {
                fg: Color::Byte(103),
            },
            dark = {
                fg: Color::Byte(103)
            }
        );

        add!(
            "mail.listing.thread_snooze_flag",
            light = {
                fg: Color::Red,
            },
            dark = {
                fg: Color::Red,
            }
        );

        add!(
            "mail.listing.tag_default",
            light = {
                fg: Color::White,
                bg: Color::Byte(250),
                attrs: Attr::BOLD
            },
            dark = {
                fg: Color::White,
                bg: Color::Byte(8),
                attrs: Attr::BOLD
            }
        );
        add!(
            "mail.listing.highlight_self",
            light = {
                fg: Color::BLUE,
            },
            dark = {
                fg: Color::BLUE,
            }
        );

        add!("pager.highlight_search", light = { fg: Color::White, bg: Color::Byte(6) /* Teal */, attrs: Attr::BOLD }, dark = { fg: Color::White, bg: Color::Byte(6) /* Teal */, attrs: Attr::BOLD });
        add!("pager.highlight_search_current", light = { fg: Color::White, bg: Color::Byte(17) /* NavyBlue */, attrs: Attr::BOLD }, dark = { fg: Color::White, bg: Color::Byte(17) /* NavyBlue */, attrs: Attr::BOLD });
        Self {
            light: Theme {
                keys: light,
                attr_aliases: Default::default(),
                color_aliases: Default::default(),
                #[cfg(feature = "regexp")]
                text_format_regexps: DEFAULT_TEXT_FORMATTER_KEYS
                    .iter()
                    .map(|&k| (k.into(), SmallVec::new()))
                    .collect(),
            },
            dark: Theme {
                keys: dark,
                attr_aliases: Default::default(),
                color_aliases: Default::default(),
                #[cfg(feature = "regexp")]
                text_format_regexps: DEFAULT_TEXT_FORMATTER_KEYS
                    .iter()
                    .map(|&k| (k.into(), SmallVec::new()))
                    .collect(),
            },
            other_themes,
        }
    }
}

impl Serialize for Themes {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut dark: IndexMap<Cow<'static, str>, ThemeAttribute> = Default::default();
        let mut light: IndexMap<Cow<'static, str>, ThemeAttribute> = Default::default();
        let mut other_themes: IndexMap<String, _> = Default::default();

        for k in self.dark.keys() {
            dark.insert(
                k.clone(),
                ThemeAttribute {
                    fg: unlink_fg(&self.dark, &ColorField::Fg, k),
                    bg: unlink_bg(&self.dark, &ColorField::Bg, k),
                    attrs: unlink_attrs(&self.dark, k),
                },
            );
        }

        for k in self.light.keys() {
            light.insert(
                k.clone(),
                ThemeAttribute {
                    fg: unlink_fg(&self.light, &ColorField::Fg, k),
                    bg: unlink_bg(&self.light, &ColorField::Bg, k),
                    attrs: unlink_attrs(&self.light, k),
                },
            );
        }

        for (name, t) in self.other_themes.iter() {
            let mut new_map: IndexMap<Cow<'static, str>, ThemeAttribute> = Default::default();

            for k in t.keys() {
                new_map.insert(
                    k.clone(),
                    ThemeAttribute {
                        fg: unlink_fg(t, &ColorField::Fg, k),
                        bg: unlink_bg(t, &ColorField::Bg, k),
                        attrs: unlink_attrs(t, k),
                    },
                );
            }
            other_themes.insert(name.to_string(), new_map);
        }

        other_themes.insert(self::LIGHT.to_string(), light);
        other_themes.insert(self::DARK.to_string(), dark);
        other_themes.serialize(serializer)
    }
}

/* Check Theme linked values for cycles */
fn is_cyclic(theme: &Theme) -> std::result::Result<(), String> {
    #[derive(Clone, Copy, Eq, Hash, PartialEq)]
    enum Course {
        Fg,
        Bg,
        Attrs,
        ColorAliasFg,
        ColorAliasBg,
        AttrAlias,
    }
    fn is_cyclic_util<'a>(
        course: Course,
        k: &'a str,
        visited: &mut IndexMap<(&'a str, Course), bool>,
        stack: &mut IndexMap<(&'a str, Course), bool>,
        path: &mut SmallVec<[(&'a str, Course); 16]>,
        theme: &'a Theme,
    ) -> bool {
        if !visited[&(k, course)] {
            visited.entry((k, course)).and_modify(|e| *e = true);
            stack.entry((k, course)).and_modify(|e| *e = true);

            match course {
                Course::Fg => match theme[k].fg {
                    ThemeValue::Link(ref l, ColorField::LikeSelf)
                    | ThemeValue::Link(ref l, ColorField::Fg) => {
                        let l = l.as_ref();
                        path.push((l, Course::Fg));
                        if (!visited[&(l, Course::Fg)]
                            && is_cyclic_util(course, l, visited, stack, path, theme))
                            || stack[&(l, Course::Fg)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Link(ref l, ColorField::Bg) => {
                        let l = l.as_ref();
                        path.push((l, Course::Bg));
                        if (!visited[&(l, Course::Bg)]
                            && is_cyclic_util(Course::Bg, l, visited, stack, path, theme))
                            || stack[&(l, Course::Bg)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Alias(ref ident) => {
                        let ident = ident.as_ref();
                        path.push((ident, Course::ColorAliasFg));
                        if (!visited[&(ident, Course::ColorAliasFg)]
                            && is_cyclic_util(
                                Course::ColorAliasFg,
                                ident,
                                visited,
                                stack,
                                path,
                                theme,
                            ))
                            || stack[&(ident, Course::ColorAliasFg)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::Bg => match theme[k].bg {
                    ThemeValue::Link(ref l, ColorField::LikeSelf)
                    | ThemeValue::Link(ref l, ColorField::Bg) => {
                        let l = l.as_ref();
                        path.push((l, Course::Bg));
                        if (!visited[&(l, Course::Bg)]
                            && is_cyclic_util(Course::Bg, l, visited, stack, path, theme))
                            || stack[&(l, Course::Bg)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Link(ref l, ColorField::Fg) => {
                        let l = l.as_ref();
                        path.push((l, Course::Fg));
                        if (!visited[&(l, Course::Fg)]
                            && is_cyclic_util(Course::Fg, l, visited, stack, path, theme))
                            || stack[&(l, Course::Fg)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Alias(ref ident) => {
                        let ident = ident.as_ref();
                        path.push((ident, Course::ColorAliasBg));
                        if (!visited[&(ident, Course::ColorAliasBg)]
                            && is_cyclic_util(
                                Course::ColorAliasBg,
                                ident,
                                visited,
                                stack,
                                path,
                                theme,
                            ))
                            || stack[&(ident, Course::ColorAliasBg)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::Attrs => match theme[k].attrs {
                    ThemeValue::Link(ref l, _) => {
                        let l = l.as_ref();
                        path.push((l, course));
                        if (!visited[&(l, course)]
                            && is_cyclic_util(course, l, visited, stack, path, theme))
                            || stack[&(l, course)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Alias(ref ident) => {
                        let ident = ident.as_ref();
                        path.push((ident, Course::AttrAlias));
                        if (!visited[&(ident, Course::AttrAlias)]
                            && is_cyclic_util(
                                Course::AttrAlias,
                                ident,
                                visited,
                                stack,
                                path,
                                theme,
                            ))
                            || stack[&(ident, Course::AttrAlias)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::ColorAliasFg | Course::ColorAliasBg => match &theme.color_aliases[k] {
                    ThemeValue::Link(ref l, ref field) => {
                        let l = l.as_ref();
                        let course = match (course, field) {
                            (Course::ColorAliasFg, ColorField::LikeSelf) => Course::Fg,
                            (Course::ColorAliasBg, ColorField::LikeSelf) => Course::Bg,
                            (_, ColorField::LikeSelf) => unsafe {
                                std::hint::unreachable_unchecked()
                            },
                            (_, ColorField::Fg) => Course::Fg,
                            (_, ColorField::Bg) => Course::Bg,
                        };
                        path.push((l, course));
                        if (!visited[&(l, course)]
                            && is_cyclic_util(course, l, visited, stack, path, theme))
                            || stack[&(l, course)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Alias(ref ident) => {
                        let ident = ident.as_ref();
                        path.push((ident, course));
                        if (!visited[&(ident, course)]
                            && is_cyclic_util(course, ident, visited, stack, path, theme))
                            || stack[&(ident, course)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::AttrAlias => match &theme.attr_aliases[k] {
                    ThemeValue::Link(ref l, ()) => {
                        let l = l.as_ref();
                        path.push((l, Course::Attrs));
                        if (!visited[&(l, Course::Attrs)]
                            && is_cyclic_util(Course::Attrs, l, visited, stack, path, theme))
                            || stack[&(l, Course::Attrs)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Alias(ref ident) => {
                        let ident = ident.as_ref();
                        path.push((ident, course));
                        if (!visited[&(ident, course)]
                            && is_cyclic_util(course, ident, visited, stack, path, theme))
                            || stack[&(ident, course)]
                        {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
            }
        }
        stack.entry((k, course)).and_modify(|e| *e = false);
        false
    }

    let mut path = SmallVec::new();
    let mut visited = theme
        .keys()
        .flat_map(|k| {
            std::iter::once(((k.as_ref(), Course::Fg), false))
                .chain(std::iter::once(((k.as_ref(), Course::Bg), false)))
                .chain(std::iter::once(((k.as_ref(), Course::Attrs), false)))
        })
        .chain(theme.color_aliases.keys().flat_map(|k| {
            std::iter::once(((k.as_ref(), Course::ColorAliasFg), false))
                .chain(std::iter::once(((k.as_ref(), Course::ColorAliasBg), false)))
        }))
        .chain(
            theme
                .attr_aliases
                .keys()
                .map(|k| ((k.as_ref(), Course::AttrAlias), false)),
        )
        .collect::<IndexMap<(&str, Course), bool>>();

    let mut stack = visited.clone();
    for k in theme.keys() {
        for &course in [Course::Fg, Course::Bg, Course::Attrs].iter() {
            path.push((k.as_ref(), course));
            if is_cyclic_util(course, k, &mut visited, &mut stack, &mut path, theme) {
                let path = path
                    .into_iter()
                    .map(|(k, c)| match c {
                        Course::Fg => format!("{}.fg", k,),
                        Course::Bg => format!("{}.fg", k,),
                        Course::Attrs => format!("{}.attrs", k,),
                        Course::ColorAliasFg => format!("(Color fg) ${}", k),
                        Course::ColorAliasBg => format!("(Color bg) ${}", k),
                        Course::AttrAlias => format!("(Attr) ${}", k),
                    })
                    .collect::<Vec<String>>();
                return Err(format!(
                    "{} {}",
                    match course {
                        Course::Fg => "fg: ",
                        Course::Bg => "bg: ",
                        Course::Attrs => "attrs: ",
                        Course::ColorAliasFg => "color alias fg: ",
                        Course::ColorAliasBg => "color alias bg: ",
                        Course::AttrAlias => "attribute alias: ",
                    },
                    path.join(" -> ")
                ));
            }
            for v in visited.values_mut() {
                *v = false;
            }
            path.pop();
        }
    }

    Ok(())
}

#[test]
fn test_theme_parsing() {
    /* MUST SUCCEED: default themes should be valid */
    let def = Themes::default();
    def.validate().unwrap();
    /* MUST SUCCEED: new user theme `hunter2`, theme `dark` has user
     * redefinitions */
    const TEST_STR: &str = r#"[dark]
"mail.listing.tag_default" = { fg = "White", bg = "HotPink3" }
"mail.listing.attachment_flag" = { fg = "mail.listing.tag_default.bg" }
"mail.view.headers" = { bg = "mail.listing.tag_default.fg" }

["hunter2"]
"mail.view.body" = { fg = "Black", bg = "White"}"#;
    let parsed: Themes = toml::from_str(TEST_STR).unwrap();
    assert!(parsed.other_themes.contains_key("hunter2"));
    assert_eq!(
        unlink_bg(
            &parsed.dark,
            &ColorField::Bg,
            &Cow::from("mail.listing.tag_default")
        ),
        Color::Byte(132)
    );
    assert_eq!(
        unlink_fg(
            &parsed.dark,
            &ColorField::Fg,
            &Cow::from("mail.listing.attachment_flag")
        ),
        Color::Byte(132)
    );
    assert_eq!(
        unlink_bg(
            &parsed.dark,
            &ColorField::Bg,
            &Cow::from("mail.view.headers")
        ),
        Color::Byte(15), // White
    );
    parsed.validate().unwrap();
    /* MUST FAIL: theme `dark` contains a cycle */
    const HAS_CYCLE: &str = r#"[dark]
"mail.listing.compact.even" = { fg = "mail.listing.compact.odd" }
"mail.listing.compact.odd" = { fg = "mail.listing.compact.even" }
"#;
    let parsed: Themes = toml::from_str(HAS_CYCLE).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: theme `dark` contains an invalid key */
    const HAS_INVALID_KEYS: &str = r#"[dark]
"asdfsafsa" = { fg = "Black" }
"#;
    let parsed: std::result::Result<Themes, _> = toml::from_str(HAS_INVALID_KEYS);
    parsed.unwrap_err();
    /* MUST SUCCEED: alias $Jebediah resolves to a valid color */
    const TEST_ALIAS_STR: &str = r##"[dark]
color_aliases= { "Jebediah" = "#b4da55" }
"mail.listing.tag_default" = { fg = "$Jebediah" }
"##;
    let parsed: Themes = toml::from_str(TEST_ALIAS_STR).unwrap();
    parsed.validate().unwrap();
    assert_eq!(
        unlink_fg(
            &parsed.dark,
            &ColorField::Fg,
            &Cow::from("mail.listing.tag_default")
        ),
        Color::Rgb(180, 218, 85)
    );
    /* MUST FAIL: Misspell color alias $Jebediah as $Jebedia */
    const TEST_INVALID_ALIAS_STR: &str = r##"[dark]
color_aliases= { "Jebediah" = "#b4da55" }
"mail.listing.tag_default" = { fg = "$Jebedia" }
"##;
    let parsed: Themes = toml::from_str(TEST_INVALID_ALIAS_STR).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: Color alias $Jebediah is defined as itself */
    const TEST_CYCLIC_ALIAS_STR: &str = r#"[dark]
color_aliases= { "Jebediah" = "$Jebediah" }
"mail.listing.tag_default" = { fg = "$Jebediah" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_STR).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: Attr alias $Jebediah is defined as itself */
    const TEST_CYCLIC_ALIAS_ATTR_STR: &str = r#"[dark]
attr_aliases= { "Jebediah" = "$Jebediah" }
"mail.listing.tag_default" = { attrs = "$Jebediah" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_ATTR_STR).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: alias $Jebediah resolves to a cycle */
    const TEST_CYCLIC_ALIAS_STR_2: &str = r#"[dark]
color_aliases= { "Jebediah" = "$JebediahJr", "JebediahJr" = "mail.listing.tag_default" }
"mail.listing.tag_default" = { fg = "$Jebediah" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_STR_2).unwrap();
    parsed.validate().unwrap_err();
    /* MUST SUCCEED: alias $Jebediah resolves to a key's field */
    const TEST_CYCLIC_ALIAS_STR_3: &str = r#"[dark]
color_aliases= { "Jebediah" = "$JebediahJr", "JebediahJr" = "mail.listing.tag_default.bg" }
"mail.listing.tag_default" = { fg = "$Jebediah", bg = "Black" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_STR_3).unwrap();
    parsed.validate().unwrap();
    /* MUST FAIL: alias $Jebediah resolves to an invalid key */
    const TEST_INVALID_LINK_KEY_FIELD_STR: &str = r#"[dark]
color_aliases= { "Jebediah" = "$JebediahJr", "JebediahJr" = "mail.listing.tag_default.attrs" }
"mail.listing.tag_default" = { fg = "$Jebediah", bg = "Black" }
"#;
    let parsed: Themes = toml::from_str(TEST_INVALID_LINK_KEY_FIELD_STR).unwrap();
    parsed.validate().unwrap_err();
}

#[test]
fn test_theme_key_values() {
    use std::{collections::VecDeque, fs::File, io::Read, path::PathBuf};
    let mut rust_files: VecDeque<PathBuf> = VecDeque::new();
    let mut dirs_queue: VecDeque<PathBuf> = VecDeque::new();
    dirs_queue.push_back("src/".into());
    let re_whitespace = regex::Regex::new(r"\s*").unwrap();
    let re_conf = regex::Regex::new(r#"value\([&]?context,"([^"]*)""#).unwrap();

    while let Some(dir) = dirs_queue.pop_front() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                dirs_queue.push_back(path);
            } else if path.extension().map(|os_s| os_s == "rs").unwrap_or(false) {
                rust_files.push_back(path);
            }
        }
    }
    for file_path in rust_files {
        let mut file = File::open(&file_path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        let content = re_whitespace.replace_all(&content, "");
        for mat in re_conf.captures_iter(&content) {
            let theme_key = &mat[1];
            if !DEFAULT_KEYS.contains(&theme_key) {
                panic!(
                    "Source file {} contains a hardcoded theme key str, {:?}, that is not \
                     included in the DEFAULT_KEYS table.",
                    file_path.display(),
                    theme_key
                );
            }
        }
    }
}
