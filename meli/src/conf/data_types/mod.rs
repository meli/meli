//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

pub mod dotaddressable;
pub mod regex_pattern;

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum IndexStyle {
    Plain,
    Threaded,
    #[default]
    Compact,
    Conversations,
}

impl<'de> Deserialize<'de> for IndexStyle {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            plain if plain.eq_ignore_ascii_case("plain") => Ok(Self::Plain),
            threaded if threaded.eq_ignore_ascii_case("threaded") => Ok(Self::Threaded),
            compact if compact.eq_ignore_ascii_case("compact") => Ok(Self::Compact),
            conversations if conversations.eq_ignore_ascii_case("conversations") => {
                Ok(Self::Conversations)
            }
            _ => Err(de::Error::custom(
                "invalid `index_style` value, expected one of: \"plain\", \"threaded\", \
                 \"compact\" or \"conversations\".",
            )),
        }
    }
}

impl Serialize for IndexStyle {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Plain => serializer.serialize_str("plain"),
            Self::Threaded => serializer.serialize_str("threaded"),
            Self::Compact => serializer.serialize_str("compact"),
            Self::Conversations => serializer.serialize_str("conversations"),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum SearchBackend {
    None,
    #[default]
    Auto,
    #[cfg(feature = "sqlite3")]
    Sqlite3,
}

impl<'de> Deserialize<'de> for SearchBackend {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            #[cfg(feature = "sqlite3")]
            sqlite3
                if sqlite3.eq_ignore_ascii_case("sqlite3")
                    || sqlite3.eq_ignore_ascii_case("sqlite") =>
            {
                Ok(Self::Sqlite3)
            }
            none if none.eq_ignore_ascii_case("none")
                || none.eq_ignore_ascii_case("nothing")
                || none.is_empty() =>
            {
                Ok(Self::None)
            }
            auto if auto.eq_ignore_ascii_case("auto") => Ok(Self::Auto),
            _ => Err(de::Error::custom(if cfg!(feature = "sqlite3") {
                "invalid `search_backend` value, expected one of: \"sqlite3\", \"sqlite\", \
                 \"none\" or \"auto\"."
            } else {
                "invalid `search_backend` value, expected one of: \"none\" or \"auto\"."
            })),
        }
    }
}

impl Serialize for SearchBackend {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            #[cfg(feature = "sqlite3")]
            Self::Sqlite3 => serializer.serialize_str("sqlite3"),
            Self::None => serializer.serialize_str("none"),
            Self::Auto => serializer.serialize_str("auto"),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum ThreadLayout {
    Vertical,
    Horizontal,
    #[default]
    Auto,
}

impl<'de> Deserialize<'de> for ThreadLayout {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            vertical if vertical.eq_ignore_ascii_case("vertical") => Ok(Self::Vertical),
            horizontal if horizontal.eq_ignore_ascii_case("horizontal") => Ok(Self::Horizontal),
            auto if auto.eq_ignore_ascii_case("auto") => Ok(Self::Auto),
            _ => Err(de::Error::custom(
                "invalid `thread_layout` value, expected one of: \"vertical\", \"horizontal\" or \
                 \"auto\".",
            )),
        }
    }
}

impl Serialize for ThreadLayout {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Vertical => serializer.serialize_str("vertical"),
            Self::Horizontal => serializer.serialize_str("horizontal"),
            Self::Auto => serializer.serialize_str("auto"),
        }
    }
}

/// How to handle UI notifications.
#[derive(Clone, Copy, Debug, Default)]
pub enum NotificationEnable {
    /// Always show as both system and in-UI(default).
    #[default]
    SystemAndUI,
    /// Do not show any notifications.
    None,
    /// Show them only as system notifications.
    SystemOnly,
    /// Show them only as UI notifications.
    UIOnly,
}

impl NotificationEnable {
    #[inline(always)]
    pub const fn enabled(&self) -> bool {
        !matches!(self, Self::None)
    }

    #[inline(always)]
    pub const fn ui_enabled(&self) -> bool {
        matches!(self, Self::SystemAndUI | Self::UIOnly)
    }

    #[inline(always)]
    pub const fn system_enabled(&self) -> bool {
        matches!(self, Self::SystemAndUI | Self::SystemOnly)
    }
}

impl<'de> Deserialize<'de> for NotificationEnable {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        match s.as_str() {
            v if v.eq_ignore_ascii_case("system_and_ui") => Ok(Self::SystemAndUI),
            v if v.eq_ignore_ascii_case("system_only") => Ok(Self::SystemOnly),
            v if v.eq_ignore_ascii_case("ui_only") => Ok(Self::UIOnly),
            v if v.eq_ignore_ascii_case("none") => Ok(Self::None),
            _ => Err(de::Error::custom(
                "invalid `ui_notifications` value, expected one of: \"system_and_ui\", \"none\", \
                 \"system_only\" or \"ui_only\".",
            )),
        }
    }
}

impl Serialize for NotificationEnable {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::SystemAndUI => serializer.serialize_str("system_and_ui"),
            Self::None => serializer.serialize_str("none"),
            Self::SystemOnly => serializer.serialize_str("system_only"),
            Self::UIOnly => serializer.serialize_str("ui_only"),
        }
    }
}
