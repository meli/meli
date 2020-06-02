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
//! * An attribute is a triple of foreground color, background color and terminal attribute `ThemeValue`s.
//! * A `ThemeValue<T>` is either an actual value or the key name of another value to which it depends. The value is either `Color` or `Attr`.
//! * `ThemeAttributeInner` is an attribute triplet.
//! * `ThemeAttribute` is an attribute triplet with the links resolved.
//!
//! On startup a [DFS](https://en.wikipedia.org/wiki/Depth-first_search) is performed to see if there are any cycles in the link graph.

use crate::terminal::{Attr, Color};
use crate::Context;
use melib::{MeliError, Result};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

#[inline(always)]
pub fn value(context: &Context, key: &'static str) -> ThemeAttribute {
    let theme = match context.settings.terminal.theme.as_str() {
        "light" => &context.settings.terminal.themes.light,
        "dark" => &context.settings.terminal.themes.dark,
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
        "light" => &context.settings.terminal.themes.light,
        "dark" => &context.settings.terminal.themes.dark,
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
        "light" => &context.settings.terminal.themes.light,
        "dark" => &context.settings.terminal.themes.dark,
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
        "light" => &context.settings.terminal.themes.light,
        "dark" => &context.settings.terminal.themes.dark,
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
fn unlink<'k, 't: 'k>(theme: &'t Theme, key: &'k Cow<'static, str>) -> ThemeAttribute {
    ThemeAttribute {
        fg: unlink_fg(theme, &ColorField::Fg, key),
        bg: unlink_bg(theme, &ColorField::Bg, key),
        attrs: unlink_attrs(theme, key),
    }
}

#[inline(always)]
fn unlink_fg<'k, 't: 'k>(
    theme: &'t Theme,
    mut field: &'k ColorField,
    mut key: &'k Cow<'static, str>,
) -> Color {
    loop {
        match field {
            ColorField::LikeSelf | ColorField::Fg => match &theme[key].fg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Value(val) => return *val,
            },
            ColorField::Bg => match &theme[key].bg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Value(val) => return *val,
            },
        }
    }
}

#[inline(always)]
fn unlink_bg<'k, 't: 'k>(
    theme: &'t Theme,
    mut field: &'k ColorField,
    mut key: &'k Cow<'static, str>,
) -> Color {
    loop {
        match field {
            ColorField::LikeSelf | ColorField::Bg => match &theme[key].bg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Value(val) => return *val,
            },
            ColorField::Fg => match &theme[key].fg {
                ThemeValue::Link(ref new_key, ref new_field) => {
                    key = new_key;
                    field = new_field
                }
                ThemeValue::Value(val) => return *val,
            },
        }
    }
}

#[inline(always)]
fn unlink_attrs<'k, 't: 'k>(theme: &'t Theme, mut key: &'k Cow<'static, str>) -> Attr {
    loop {
        match &theme[key].attrs {
            ThemeValue::Link(ref new_key, ()) => key = new_key,
            ThemeValue::Value(val) => return *val,
        }
    }
}

const DEFAULT_KEYS: &'static [&'static str] = &[
    "theme_default",
    "status.bar",
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
    "mail.sidebar_unread_count",
    "mail.sidebar_index",
    "mail.sidebar_highlighted",
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
    "mail.listing.plain.even",
    "mail.listing.plain.odd",
    "mail.listing.plain.even_unseen",
    "mail.listing.plain.odd_unseen",
    "mail.listing.plain.even_selected",
    "mail.listing.plain.odd_selected",
    "mail.listing.plain.even_highlighted",
    "mail.listing.plain.odd_highlighted",
    "mail.listing.conversations",
    "mail.listing.conversations.subject",
    "mail.listing.conversations.from",
    "mail.listing.conversations.date",
    "mail.listing.conversations.padding",
    "mail.listing.conversations.unseen",
    "mail.listing.conversations.unseen_padding",
    "mail.listing.conversations.highlighted",
    "mail.listing.conversations.selected",
    "mail.view.headers",
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
    "pager.highlight_search",
    "pager.highlight_search_current",
];

/// `ThemeAttributeInner` but with the links resolved.
#[derive(Debug, PartialEq, Eq, Clone, Default, Copy, Serialize, Deserialize)]
pub struct ThemeAttribute {
    pub fg: Color,
    pub bg: Color,
    pub attrs: Attr,
}

/// Holds {fore,back}ground color and terminal attribute values.
#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone)]
pub enum ColorField {
    // Like self, i.e. either Fg or Bg
    LikeSelf,
    Fg,
    Bg,
}

/// The field a ThemeValue::Link refers to.
trait ThemeLink {
    type LinkType;
}

/// A color value that's a link can either refer to .fg or .bg field
impl ThemeLink for Color {
    type LinkType = ColorField;
}

/// An attr value that's a link can only refer to an .attr field
impl ThemeLink for Attr {
    type LinkType = ();
}

#[derive(Debug, Clone)]
/// Holds either an actual value or refers to the key name of the attribute that holds the value.
enum ThemeValue<T: ThemeLink> {
    Value(T),
    Link(Cow<'static, str>, T::LinkType),
}

impl From<&'static str> for ThemeValue<Color> {
    fn from(from: &'static str) -> Self {
        ThemeValue::Link(from.into(), ColorField::LikeSelf)
    }
}

impl From<&'static str> for ThemeValue<Attr> {
    fn from(from: &'static str) -> Self {
        ThemeValue::Link(from.into(), ())
    }
}

impl From<Color> for ThemeValue<Color> {
    fn from(from: Color) -> Self {
        ThemeValue::Value(from)
    }
}

impl From<Attr> for ThemeValue<Attr> {
    fn from(from: Attr) -> Self {
        ThemeValue::Value(from)
    }
}

impl Default for ThemeValue<Color> {
    fn default() -> Self {
        ThemeValue::Value(Color::Default)
    }
}

impl Default for ThemeValue<Attr> {
    fn default() -> Self {
        ThemeValue::Value(Attr::DEFAULT)
    }
}

impl<'de> Deserialize<'de> for ThemeValue<Attr> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            if let Ok(c) = Attr::from_string_de::<'de, D, String>(s.clone()) {
                Ok(ThemeValue::Value(c))
            } else {
                Ok(ThemeValue::Link(s.into(), ()))
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
            ThemeValue::Value(s) => s.serialize(serializer),
            ThemeValue::Link(s, ()) => serializer.serialize_str(s.as_ref()),
        }
    }
}

impl Serialize for ThemeValue<Color> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            ThemeValue::Value(s) => s.serialize(serializer),
            ThemeValue::Link(s, ColorField::LikeSelf) => serializer.serialize_str(s.as_ref()),
            ThemeValue::Link(s, ColorField::Fg) => {
                serializer.serialize_str(format!("{}.fg", s).as_ref())
            }
            ThemeValue::Link(s, ColorField::Bg) => {
                serializer.serialize_str(format!("{}.bg", s).as_ref())
            }
        }
    }
}

impl<'de> Deserialize<'de> for ThemeValue<Color> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            if let Ok(c) = Color::from_string_de::<'de, D>(s.clone()) {
                Ok(ThemeValue::Value(c))
            } else if s.ends_with(".fg") {
                Ok(ThemeValue::Link(
                    s[..s.len() - 3].to_string().into(),
                    ColorField::Fg,
                ))
            } else if s.ends_with(".bg") {
                Ok(ThemeValue::Link(
                    s[..s.len() - 3].to_string().into(),
                    ColorField::Bg,
                ))
            } else {
                Ok(ThemeValue::Link(s.into(), ColorField::LikeSelf))
            }
        } else {
            Err(de::Error::custom("invalid theme color value"))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Themes {
    pub light: Theme,
    pub dark: Theme,
    pub other_themes: HashMap<String, Theme>,
}

#[derive(Debug, Clone)]
pub struct Theme {
    pub keys: HashMap<Cow<'static, str>, ThemeAttributeInner>,
}

use std::ops::{Deref, DerefMut};
impl Deref for Theme {
    type Target = HashMap<Cow<'static, str>, ThemeAttributeInner>;
    fn deref(&self) -> &Self::Target {
        &self.keys
    }
}

impl DerefMut for Theme {
    fn deref_mut(&mut self) -> &mut HashMap<Cow<'static, str>, ThemeAttributeInner> {
        &mut self.keys
    }
}

impl<'de> Deserialize<'de> for Themes {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct ThemesOptions {
            #[serde(default)]
            light: ThemeOptions,
            #[serde(default)]
            dark: ThemeOptions,
            #[serde(flatten, default)]
            other_themes: HashMap<String, ThemeOptions>,
        }
        #[derive(Deserialize, Default)]
        struct ThemeOptions {
            #[serde(flatten, default)]
            keys: HashMap<Cow<'static, str>, ThemeAttributeInnerOptions>,
        }
        #[derive(Deserialize, Default)]
        struct ThemeAttributeInnerOptions {
            #[serde(default)]
            fg: Option<ThemeValue<Color>>,
            #[serde(default)]
            bg: Option<ThemeValue<Color>>,
            #[serde(default)]
            attrs: Option<ThemeValue<Attr>>,
        }

        let mut ret = Themes::default();
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
                    .into_iter()
                    .map(|k| k.as_ref())
                    .collect::<SmallVec<[_; 128]>>()
                    .join(", ")
            )));
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
                    .into_iter()
                    .map(|k| k.as_ref())
                    .collect::<SmallVec<[_; 128]>>()
                    .join(", ")
            )));
        }
        for (tk, t) in ret.other_themes.iter_mut() {
            for (k, v) in t.iter_mut() {
                if let Some(mut att) = s
                    .other_themes
                    .get_mut(tk)
                    .and_then(|theme| theme.keys.remove(k))
                {
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
            if !s.other_themes[tk].keys.is_empty() {
                return Err(de::Error::custom(format!(
                    "{} theme contains unrecognized theme keywords: {}",
                    tk,
                    s.other_themes[tk]
                        .keys
                        .keys()
                        .into_iter()
                        .map(|k| k.as_ref())
                        .collect::<SmallVec<[_; 128]>>()
                        .join(", ")
                )));
            }
        }
        Ok(ret)
    }
}

impl Themes {
    fn validate_keys(name: &str, theme: &Theme, hash_set: &HashSet<&'static str>) -> Result<()> {
        let keys = theme
            .keys()
            .filter_map(|k| {
                if !hash_set.contains(&k.as_ref()) {
                    Some((None, "key", k.as_ref()))
                } else {
                    None
                }
            })
            .chain(theme.iter().filter_map(|(key, a)| {
                if let ThemeValue::Link(ref r, _) = a.fg {
                    if !hash_set.contains(&r.as_ref()) {
                        Some((Some(key), "fg link", r.as_ref()))
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
                        Some((Some(key), "bg link", r.as_ref()))
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
                        Some((Some(key), "attrs link", r.as_ref()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }))
            .collect::<SmallVec<[(Option<_>, &'_ str, &'_ str); 128]>>();

        if !keys.is_empty() {
            return Err(format!(
                "{} theme contains unrecognized theme keywords: {}",
                name,
                keys.into_iter()
                    .map(|(key_opt, desc, link)| if let Some(key) = key_opt {
                        format!("{} {}: \"{}\"", key, desc, link)
                    } else {
                        format!("{}: \"{}\"", desc, link)
                    })
                    .collect::<SmallVec<[String; 128]>>()
                    .join(", ")
            )
            .into());
        }
        Ok(())
    }
    pub fn validate(&self) -> Result<()> {
        let hash_set: HashSet<&'static str> = DEFAULT_KEYS.into_iter().map(|k| *k).collect();
        Themes::validate_keys("light", &self.light, &hash_set)?;
        Themes::validate_keys("dark", &self.dark, &hash_set)?;
        for (name, t) in self.other_themes.iter() {
            Themes::validate_keys(name, t, &hash_set)?;
        }
        if let Err(err) = is_cyclic(&self.light) {
            return Err(MeliError::new(format!(
                "light theme contains a cycle: {}",
                err
            )));
        }
        if let Err(err) = is_cyclic(&self.dark) {
            return Err(MeliError::new(format!(
                "dark theme contains a cycle: {}",
                err
            )));
        }
        for (k, t) in self.other_themes.iter() {
            if let Err(err) = is_cyclic(t) {
                return Err(MeliError::new(format!(
                    "{} theme contains a cycle: {}",
                    k, err
                )));
            }
        }
        Ok(())
    }

    pub fn key_to_string(&self, key: &str, unlink: bool) -> String {
        let theme = match key {
            "light" => &self.light,
            "dark" => &self.dark,
            t => self.other_themes.get(t).unwrap_or(&self.dark),
        };
        let mut ret = String::new();
        ret.extend(format!("[terminal.themes.{}]\n", key).chars());
        if unlink {
            for k in theme.keys() {
                ret.extend(
                    format!(
                        "\"{}\" = {{ fg = {}, bg = {}, attrs = {} }}\n",
                        k,
                        toml::to_string(&unlink_fg(&theme, &ColorField::Fg, k)).unwrap(),
                        toml::to_string(&unlink_bg(&theme, &ColorField::Bg, k)).unwrap(),
                        toml::to_string(&unlink_attrs(&theme, k)).unwrap(),
                    )
                    .chars(),
                );
            }
        } else {
            for k in theme.keys() {
                ret.extend(
                    format!(
                        "\"{}\" = {{ fg = {}, bg = {}, attrs = {} }}\n",
                        k,
                        toml::to_string(&theme[k].fg).unwrap(),
                        toml::to_string(&theme[k].bg).unwrap(),
                        toml::to_string(&theme[k].attrs).unwrap(),
                    )
                    .chars(),
                );
            }
        }
        ret
    }
    pub fn to_string(&self) -> String {
        let mut ret = String::new();
        ret.extend(self.key_to_string("dark", true).chars());

        ret.push_str("\n\n");
        ret.extend(self.key_to_string("light", true).chars());
        for name in self.other_themes.keys() {
            ret.push_str("\n\n");
            ret.extend(self.key_to_string(name, true).chars());
        }
        ret
    }
}

impl Default for Themes {
    fn default() -> Themes {
        let mut light = HashMap::default();
        let mut dark = HashMap::default();
        let other_themes = HashMap::default();

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
        }
        add!("theme_default", dark = { fg: Color::Default, bg: Color::Default, attrs: Attr::DEFAULT }, light = { fg: Color::Default, bg: Color::Default, attrs: Attr::DEFAULT });
        add!("status.bar", dark = { fg: Color::Byte(123), bg: Color::Byte(26) }, light = { fg: Color::Byte(123), bg: Color::Byte(26) });
        add!("status.notification", dark = { fg: Color::Byte(219), bg: Color::Byte(88) }, light = { fg: Color::Byte(219), bg: Color::Byte(88) });

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

        /* ConversationsListing */

        add!("mail.listing.conversations");
        add!("mail.listing.conversations.subject");
        add!("mail.listing.conversations.from");
        add!("mail.listing.conversations.date");
        add!(
            "mail.listing.conversations.padding",
            dark = {
                fg: Color::Byte(235),
                bg: Color::Byte(235),
            },
            light = {
                fg: Color::Byte(254),
                bg: Color::Byte(254),
            }
        );
        add!(
            "mail.listing.conversations.unseen_padding",
            dark = {
                fg: Color::Byte(235),
                bg: Color::Byte(235),
            },
            light = {
                fg: Color::Byte(254),
                bg: Color::Byte(254),
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
            },
            light = {
                bg: Color::Byte(246)
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

        add!(
            "mail.view.headers",
            dark = {
                fg: Color::Byte(33),
            },
            light = {
                fg: Color::Black,
            }
        );
        add!("mail.view.body");
        add!("mail.view.thread.indentation.a", light = { bg: Color::Byte(69) }, dark = { bg: Color::Byte(69) }); // CornflowerBlue
        add!("mail.view.thread.indentation.b", light = { bg: Color::Byte(196) }, dark = { bg: Color::Byte(196) }); // Red1
        add!("mail.view.thread.indentation.c", light = { bg: Color::Byte(175) }, dark = { bg: Color::Byte(175) }); // Pink3
        add!("mail.view.thread.indentation.d", light = { bg: Color::Byte(220) }, dark = { bg: Color::Byte(220) }); // Gold1
        add!("mail.view.thread.indentation.e", light = { bg: Color::Byte(172) }, dark = { bg: Color::Byte(172) }); // Orange3
        add!("mail.view.thread.indentation.f", light = { bg: Color::Byte(072) }, dark = { bg: Color::Byte(072) }); // CadetBlue

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
                bg: Color::Byte(8),
                attrs: Attr::BOLD
            },
            dark = {
                fg: Color::White,
                bg: Color::Byte(8),
                attrs: Attr::BOLD
            }
        );

        add!("pager.highlight_search", light = { fg: Color::White, bg: Color::Byte(6) /* Teal */, attrs: Attr::BOLD }, dark = { fg: Color::White, bg: Color::Byte(6) /* Teal */, attrs: Attr::BOLD });
        add!("pager.highlight_search_current", light = { fg: Color::White, bg: Color::Byte(17) /* NavyBlue */, attrs: Attr::BOLD }, dark = { fg: Color::White, bg: Color::Byte(17) /* NavyBlue */, attrs: Attr::BOLD });
        Themes {
            light: Theme { keys: light },
            dark: Theme { keys: dark },
            other_themes,
        }
    }
}

impl Serialize for Themes {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut dark: HashMap<Cow<'static, str>, ThemeAttribute> = Default::default();
        let mut light: HashMap<Cow<'static, str>, ThemeAttribute> = Default::default();
        let mut other_themes: HashMap<String, _> = Default::default();

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
            let mut new_map: HashMap<Cow<'static, str>, ThemeAttribute> = Default::default();

            for k in t.keys() {
                new_map.insert(
                    k.clone(),
                    ThemeAttribute {
                        fg: unlink_fg(&t, &ColorField::Fg, k),
                        bg: unlink_bg(&t, &ColorField::Bg, k),
                        attrs: unlink_attrs(&t, k),
                    },
                );
            }
            other_themes.insert(name.to_string(), new_map);
        }

        other_themes.insert("light".to_string(), light);
        other_themes.insert("dark".to_string(), dark);
        other_themes.serialize(serializer)
    }
}

/* Check Theme linked values for cycles */
fn is_cyclic(theme: &Theme) -> std::result::Result<(), String> {
    #[derive(Hash, Copy, Clone, PartialEq, Eq)]
    enum Course {
        Fg,
        Bg,
        Attrs,
    }
    fn is_cyclic_util<'a>(
        course: Course,
        k: &'a Cow<'static, str>,
        visited: &mut HashMap<(&'a Cow<'static, str>, Course), bool>,
        stack: &mut HashMap<(&'a Cow<'static, str>, Course), bool>,
        path: &mut SmallVec<[(&'a Cow<'static, str>, Course); 16]>,
        theme: &'a Theme,
    ) -> bool {
        if !visited[&(k, course)] {
            visited.entry((k, course)).and_modify(|e| *e = true);
            stack.entry((k, course)).and_modify(|e| *e = true);

            match course {
                Course::Fg => match theme[k].fg {
                    ThemeValue::Link(ref l, ColorField::LikeSelf)
                    | ThemeValue::Link(ref l, ColorField::Fg) => {
                        path.push((l, Course::Fg));
                        if !visited[&(l, Course::Fg)]
                            && is_cyclic_util(course, l, visited, stack, path, theme)
                        {
                            return true;
                        } else if stack[&(l, Course::Fg)] {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Link(ref l, ColorField::Bg) => {
                        path.push((l, Course::Bg));
                        if !visited[&(l, Course::Bg)]
                            && is_cyclic_util(Course::Bg, l, visited, stack, path, theme)
                        {
                            return true;
                        } else if stack[&(l, Course::Bg)] {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::Bg => match theme[k].bg {
                    ThemeValue::Link(ref l, ColorField::LikeSelf)
                    | ThemeValue::Link(ref l, ColorField::Bg) => {
                        path.push((l, Course::Bg));
                        if !visited[&(l, Course::Bg)]
                            && is_cyclic_util(Course::Bg, l, visited, stack, path, theme)
                        {
                            return true;
                        } else if stack[&(l, Course::Bg)] {
                            return true;
                        }
                        path.pop();
                    }
                    ThemeValue::Link(ref l, ColorField::Fg) => {
                        path.push((l, Course::Fg));
                        if !visited[&(l, Course::Fg)]
                            && is_cyclic_util(Course::Fg, l, visited, stack, path, theme)
                        {
                            return true;
                        } else if stack[&(l, Course::Fg)] {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::Attrs => match theme[k].attrs {
                    ThemeValue::Link(ref l, _) => {
                        path.push((l, course));
                        if !visited[&(l, course)]
                            && is_cyclic_util(course, l, visited, stack, path, theme)
                        {
                            return true;
                        } else if stack[&(l, course)] {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
            }
        }
        stack.entry((k, course)).and_modify(|e| *e = false);
        return false;
    }

    let mut path = SmallVec::new();
    let mut visited = theme
        .keys()
        .map(|k| {
            std::iter::once(((k, Course::Fg), false))
                .chain(std::iter::once(((k, Course::Bg), false)))
                .chain(std::iter::once(((k, Course::Attrs), false)))
        })
        .flatten()
        .collect::<HashMap<(&Cow<'static, str>, Course), bool>>();

    let mut stack = visited.clone();
    for k in theme.keys() {
        for &course in [Course::Fg, Course::Bg, Course::Attrs].iter() {
            path.push((k, course));
            if is_cyclic_util(course, k, &mut visited, &mut stack, &mut path, &theme) {
                let path = path
                    .into_iter()
                    .map(|(k, c)| {
                        format!(
                            "{}.{}",
                            k,
                            match c {
                                Course::Fg => "fg",
                                Course::Bg => "bg",
                                Course::Attrs => "attrs",
                            }
                        )
                    })
                    .collect::<Vec<String>>();
                return Err(format!(
                    "{} {}",
                    match course {
                        Course::Fg => "fg: ",
                        Course::Bg => "bg: ",
                        Course::Attrs => "attrs: ",
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

    return Ok(());
}

#[test]
fn test_theme_parsing() {
    let def = Themes::default();
    assert!(def.validate().is_ok());
    const TEST_STR: &'static str = r##"[dark]
"mail.listing.tag_default" = { fg = "White", bg = "HotPink3" }
"mail.listing.attachment_flag" = { fg = "mail.listing.tag_default.bg" }
"mail.view.headers" = { bg = "mail.listing.tag_default.fg" }

["hunter2"]
"mail.view.body" = { fg = "Black", bg = "White"}"##;
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
    assert!(parsed.validate().is_ok());
    const HAS_CYCLE: &'static str = r##"[dark]
"mail.listing.compact.even" = { fg = "mail.listing.compact.odd" }
"mail.listing.compact.odd" = { fg = "mail.listing.compact.even" }
"##;
    let parsed: Themes = toml::from_str(HAS_CYCLE).unwrap();
    assert!(parsed.validate().is_err());
    const HAS_INVALID_KEYS: &'static str = r##"[dark]
"asdfsafsa" = { fg = "Black" }
"##;
    let parsed: std::result::Result<Themes, _> = toml::from_str(HAS_INVALID_KEYS);
    assert!(parsed.is_err());
}
