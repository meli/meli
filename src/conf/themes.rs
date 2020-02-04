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
    unlink_fg(theme, &Cow::from(key))
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
    unlink_bg(theme, &Cow::from(key))
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
fn unlink<'k, 't: 'k>(
    theme: &'t HashMap<Cow<'static, str>, ThemeAttributeInner>,
    key: &'k Cow<'static, str>,
) -> ThemeAttribute {
    ThemeAttribute {
        fg: unlink_fg(theme, key),
        bg: unlink_bg(theme, key),
        attrs: unlink_attrs(theme, key),
    }
}

#[inline(always)]
fn unlink_fg<'k, 't: 'k>(
    theme: &'t HashMap<Cow<'static, str>, ThemeAttributeInner>,
    mut key: &'k Cow<'static, str>,
) -> Color {
    loop {
        match &theme[key].fg {
            ThemeValue::Link(ref new_key) => key = new_key,
            ThemeValue::Value(val) => return *val,
        }
    }
}

#[inline(always)]
fn unlink_bg<'k, 't: 'k>(
    theme: &'t HashMap<Cow<'static, str>, ThemeAttributeInner>,
    mut key: &'k Cow<'static, str>,
) -> Color {
    loop {
        match &theme[key].bg {
            ThemeValue::Link(ref new_key) => key = new_key,
            ThemeValue::Value(val) => return *val,
        }
    }
}

#[inline(always)]
fn unlink_attrs<'k, 't: 'k>(
    theme: &'t HashMap<Cow<'static, str>, ThemeAttributeInner>,
    mut key: &'k Cow<'static, str>,
) -> Attr {
    loop {
        match &theme[key].attrs {
            ThemeValue::Link(ref new_key) => key = new_key,
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
    "mail.listing.compact.unseen",
    "mail.listing.compact.selected",
    "mail.listing.compact.highlighted",
    "mail.listing.plain.even",
    "mail.listing.plain.odd",
    "mail.listing.plain.unseen",
    "mail.listing.plain.selected",
    "mail.listing.plain.highlighted",
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
    "mail.listing.attachment_flag",
    "mail.listing.thread_snooze_flag",
];

/// `ThemeAttributeInner` but with the links resolved.
#[derive(Debug, PartialEq, Eq, Clone, Default, Copy, Serialize, Deserialize)]
pub struct ThemeAttribute {
    pub fg: Color,
    pub bg: Color,
    pub attrs: Attr,
}

/// Holds {fore,back}ground color and terminal attribute values.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ThemeAttributeInner {
    #[serde(default)]
    fg: ThemeValue<Color>,
    #[serde(default)]
    bg: ThemeValue<Color>,
    #[serde(default)]
    attrs: ThemeValue<Attr>,
}

#[derive(Debug, Clone)]
/// Holds either an actual value or refers to the key name of the attribute that holds the value.
pub enum ThemeValue<T> {
    Value(T),
    Link(Cow<'static, str>),
}

impl<T> From<&'static str> for ThemeValue<T> {
    fn from(from: &'static str) -> Self {
        ThemeValue::Link(from.into())
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
        ThemeValue::Value(Attr::Default)
    }
}

impl<'de> Deserialize<'de> for ThemeValue<Attr> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            if let Ok(c) = Attr::from_string_de::<'de, D>(s.clone()) {
                Ok(ThemeValue::Value(c))
            } else {
                Ok(ThemeValue::Link(s.into()))
            }
        } else {
            Err(de::Error::custom("invalid theme attribute value"))
        }
    }
}

impl<T: Serialize> Serialize for ThemeValue<T> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            ThemeValue::Value(s) => s.serialize(serializer),
            ThemeValue::Link(s) => serializer.serialize_str(s.as_ref()),
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
    pub light: HashMap<Cow<'static, str>, ThemeAttributeInner>,
    #[serde(default)]
    pub dark: HashMap<Cow<'static, str>, ThemeAttributeInner>,
    #[serde(flatten, default)]
    pub other_themes: HashMap<String, HashMap<Cow<'static, str>, ThemeAttributeInner>>,
}

impl Theme {
    pub fn validate(&self) -> Result<()> {
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
        let hash_set: HashSet<&'static str> = DEFAULT_KEYS.into_iter().map(|k| *k).collect();
        let keys = self
            .light
            .keys()
            .filter_map(|k| {
                if !hash_set.contains(&k.as_ref()) {
                    Some(k.as_ref())
                } else {
                    None
                }
            })
            .collect::<SmallVec<[&'_ str; 128]>>();
        if !keys.is_empty() {
            return Err(format!(
                "light theme contains unrecognized theme keywords: {}",
                keys.join(", ")
            )
            .into());
        }
        let keys = self
            .dark
            .keys()
            .filter_map(|k| {
                if !hash_set.contains(&k.as_ref()) {
                    Some(k.as_ref())
                } else {
                    None
                }
            })
            .collect::<SmallVec<[&'_ str; 128]>>();
        if !keys.is_empty() {
            return Err(format!(
                "light theme contains unrecognized theme keywords: {}",
                keys.join(", ")
            )
            .into());
        }
        for (name, t) in self.other_themes.iter() {
            let keys = t
                .keys()
                .filter_map(|k| {
                    if !hash_set.contains(&k.as_ref()) {
                        Some(k.as_ref())
                    } else {
                        None
                    }
                })
                .collect::<SmallVec<[&'_ str; 128]>>();
            if !keys.is_empty() {
                return Err(format!(
                    "`{}` theme contains unrecognized theme keywords: {}",
                    name,
                    keys.join(", ")
                )
                .into());
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
                        toml::to_string(&unlink_fg(&theme, k)).unwrap(),
                        toml::to_string(&unlink_bg(&theme, k)).unwrap(),
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

impl Default for Theme {
    fn default() -> Theme {
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
        add!("theme_default");
        add!("status.bar", dark = { fg: Color::Byte(123), bg: Color::Byte(26) }, light = { fg: Color::Byte(123), bg: Color::Byte(26) });
        add!("status.notification", dark = { fg: Color::Byte(219), bg: Color::Byte(88) }, light = { fg: Color::Byte(219), bg: Color::Byte(88) });

        add!("tab.focused");
        add!("tab.unfocused", dark = { fg: Color::Byte(15), bg: Color::Byte(8), }, light = { fg: Color::Byte(15), bg: Color::Byte(8), });
        add!("tab.bar");
        add!(
            "widgets.form.label",
            dark = { attrs: Attr::Bold },
            light = { attrs: Attr::Bold }
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
            "mail.listing.compact.unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)

            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!("mail.listing.compact.selected",
            dark = {
                bg: Color::Byte(210)
            },
            light = {
                bg: Color::Byte(210)
            }
        );
        add!(
            "mail.listing.compact.highlighted",
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
            "mail.listing.plain.unseen",
            dark = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)

            },
            light = {
                fg: Color::Byte(0),
                bg: Color::Byte(251)
            }
        );
        add!("mail.listing.plain.selected",
            dark = {
                bg: Color::Byte(210)
            },
            light = {
                bg: Color::Byte(210)
            }
        );
        add!(
            "mail.listing.plain.highlighted",
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

        Theme {
            light,
            dark,
            other_themes,
        }
    }
}

impl Serialize for Theme {
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
                    fg: unlink_fg(&self.dark, k),
                    bg: unlink_bg(&self.dark, k),
                    attrs: unlink_attrs(&self.dark, k),
                },
            );
        }

        for k in self.light.keys() {
            light.insert(
                k.clone(),
                ThemeAttribute {
                    fg: unlink_fg(&self.light, k),
                    bg: unlink_bg(&self.light, k),
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
                        fg: unlink_fg(&t, k),
                        bg: unlink_bg(&t, k),
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
fn is_cyclic(
    theme: &HashMap<Cow<'static, str>, ThemeAttributeInner>,
) -> std::result::Result<(), String> {
    enum Course {
        Fg,
        Bg,
        Attrs,
    }
    fn is_cyclic_util<'a>(
        course: &Course,
        k: &'a Cow<'static, str>,
        visited: &mut HashMap<&'a Cow<'static, str>, bool>,
        stack: &mut HashMap<&'a Cow<'static, str>, bool>,
        path: &mut SmallVec<[&'a Cow<'static, str>; 16]>,
        theme: &'a HashMap<Cow<'static, str>, ThemeAttributeInner>,
    ) -> bool {
        if !visited[k] {
            visited.entry(k).and_modify(|e| *e = true);
            stack.entry(k).and_modify(|e| *e = true);

            match course {
                Course::Fg => match theme[k].fg {
                    ThemeValue::Link(ref l) => {
                        path.push(l);
                        if !visited[l] && is_cyclic_util(course, l, visited, stack, path, theme) {
                            return true;
                        } else if stack[l] {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::Bg => match theme[k].bg {
                    ThemeValue::Link(ref l) => {
                        path.push(l);
                        if !visited[l] && is_cyclic_util(course, l, visited, stack, path, theme) {
                            return true;
                        } else if stack[l] {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
                Course::Attrs => match theme[k].attrs {
                    ThemeValue::Link(ref l) => {
                        path.push(l);
                        if !visited[l] && is_cyclic_util(course, l, visited, stack, path, theme) {
                            return true;
                        } else if stack[l] {
                            return true;
                        }
                        path.pop();
                    }
                    _ => {}
                },
            }
        }
        stack.entry(k).and_modify(|e| *e = false);
        return false;
    }

    let mut path = SmallVec::new();
    let mut visited = theme
        .keys()
        .map(|k| (k, false))
        .collect::<HashMap<&Cow<'static, str>, bool>>();

    let mut stack = theme
        .keys()
        .map(|k| (k, false))
        .collect::<HashMap<&Cow<'static, str>, bool>>();
    for k in theme.keys() {
        for course in [Course::Fg, Course::Bg, Course::Attrs].into_iter() {
            path.push(k);
            if is_cyclic_util(course, k, &mut visited, &mut stack, &mut path, &theme) {
                let path = path
                    .into_iter()
                    .map(|k| k.to_string())
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
