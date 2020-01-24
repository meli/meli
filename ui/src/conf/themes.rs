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
    "general",
    "general.status_bar",
    "general.tab_focused",
    "general.tab_unfocused",
    "general.tab_bar",
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

#[derive(Debug, Clone, Default, Copy, Serialize, Deserialize)]
pub struct ThemeAttribute {
    pub fg: Color,
    pub bg: Color,
    pub attrs: Attr,
}

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
        if let Ok(s) = <Attr>::deserialize(deserializer) {
            Ok(ThemeValue::Value(s))
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
            _ => unreachable!(),
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
        let keys: Vec<&'_ str> = self
            .light
            .keys()
            .chain(self.dark.keys())
            .chain(self.other_themes.values().flat_map(|v| v.keys()))
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
        let other_themes = HashMap::default();

        macro_rules! add {
            ($key:literal, $($theme:ident={ $($name:ident : $val:expr),*$(,)? }),*$(,)?) => {
                $($theme.insert($key.into(), ThemeAttributeInner {
                    $($name: $val.into()),*
                        ,..ThemeAttributeInner::default() }));*
            };
            ($key:literal) => {
                light.insert($key.into(), ThemeAttributeInner::default());
                dark.insert($key.into(), ThemeAttributeInner::default());
            };
        }
        add!("general");
        /*
        "general.status_bar",
        "general.tab_focused",
        "general.tab_unfocused",
        "general.tab_bar",
        */

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

        /*
        "mail.listing.plain.even",
        "mail.listing.plain.odd",
        "mail.listing.plain.unseen",
        "mail.listing.conversations.subject",
        "mail.listing.conversations.from",
        "mail.listing.conversations.date",
        "mail.listing.conversations.unseen_padding",
        */
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

        for k in self.dark.keys() {
            dark.insert(
                k.clone(),
                ThemeAttribute {
                    fg: unlink_fg(&self.light, k),
                    bg: unlink_bg(&self.light, k),
                    attrs: unlink_attrs(&self.light, k),
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

        #[derive(Serialize)]
        struct ThemeSer {
            light: HashMap<Cow<'static, str>, ThemeAttribute>,
            dark: HashMap<Cow<'static, str>, ThemeAttribute>,
        }
        use serde::ser::SerializeStruct;
        let mut s = serializer.serialize_struct("ThemeSer", 2)?;
        s.serialize_field("light", &light)?;
        s.serialize_field("dark", &dark)?;
        s.end()
    }
}

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
