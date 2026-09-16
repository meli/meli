/*
 * meli - configuration module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

//! E-mail tag configuration and {de,}serializing.

use std::hash::{Hash, Hasher};

use indexmap::{IndexMap, IndexSet};
use melib::{Error, Result, TagHash};
use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, Serializer},
};

use crate::{conf::DotAddressable, terminal::Color};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TagName {
    pub name: String,
    pub hash: TagHash,
}

impl std::borrow::Borrow<TagHash> for TagName {
    #[inline]
    fn borrow(&self) -> &TagHash {
        &self.hash
    }
}

impl Serialize for TagName {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.name)
    }
}

impl<'de> Deserialize<'de> for TagName {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let name = <String>::deserialize(deserializer)?;
        let hash = TagHash::from_bytes(name.as_bytes());
        Ok(Self { name, hash })
    }
}

impl Hash for TagName {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash.hash(hasher)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TagsSettings {
    #[serde(default, deserialize_with = "tag_color_de")]
    pub colors: IndexMap<TagName, Color>,
    #[serde(default, alias = "ignore-tags")]
    pub ignore_tags: IndexSet<TagName>,
    #[serde(default)]
    pub rename: IndexMap<TagName, String>,
}

pub fn tag_color_de<'de, D, T: std::convert::From<IndexMap<TagName, Color>>>(
    deserializer: D,
) -> std::result::Result<T, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum _Color {
        B(u8),
        C(Color),
    }

    Ok(<IndexMap<TagName, _Color>>::deserialize(deserializer)?
        .into_iter()
        .map(|(tag, color)| {
            (
                tag,
                match color {
                    _Color::B(b) => Color::Byte(b),
                    _Color::C(c) => c,
                },
            )
        })
        .collect::<IndexMap<TagName, Color>>()
        .into())
}

impl DotAddressable for TagsSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "colors" => self.colors.lookup(field, tail),
                    "ignore_tags" => self.ignore_tags.lookup(field, tail),
                    "rename" => self.rename.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{parent_field} has no field named {other}"
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

impl DotAddressable for TagName {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(other) => Err(Error::new(format!(
                "{parent_field} has no field named {other}"
            ))),
            None => Ok(toml::Value::try_from(&self.name)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}
