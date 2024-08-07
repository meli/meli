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
            "Plain" | "plain" => Ok(Self::Plain),
            "Threaded" | "threaded" => Ok(Self::Threaded),
            "Compact" | "compact" => Ok(Self::Compact),
            "Conversations" | "conversations" => Ok(Self::Conversations),
            _ => Err(de::Error::custom("invalid `index_style` value")),
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
            _ => Err(de::Error::custom("invalid `search_backend` value")),
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
