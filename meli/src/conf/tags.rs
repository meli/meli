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

use std::collections::{HashMap, HashSet};

use melib::{Error, Result, TagHash};
use serde::{Deserialize, Deserializer};

use super::DotAddressable;
use crate::terminal::Color;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TagsSettings {
    #[serde(default, deserialize_with = "tag_color_de")]
    pub colors: HashMap<TagHash, Color>,
    #[serde(default, deserialize_with = "tag_set_de", alias = "ignore-tags")]
    pub ignore_tags: HashSet<TagHash>,
}

pub fn tag_set_de<'de, D, T: std::convert::From<HashSet<TagHash>>>(
    deserializer: D,
) -> std::result::Result<T, D::Error>
where
    D: Deserializer<'de>,
{
    Ok(<Vec<String>>::deserialize(deserializer)?
        .into_iter()
        .map(|tag| TagHash::from_bytes(tag.as_bytes()))
        .collect::<HashSet<TagHash>>()
        .into())
}

pub fn tag_color_de<'de, D, T: std::convert::From<HashMap<TagHash, Color>>>(
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

    Ok(<HashMap<String, _Color>>::deserialize(deserializer)?
        .into_iter()
        .map(|(tag, color)| {
            (
                TagHash::from_bytes(tag.as_bytes()),
                match color {
                    _Color::B(b) => Color::Byte(b),
                    _Color::C(c) => c,
                },
            )
        })
        .collect::<HashMap<TagHash, Color>>()
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
                    other => Err(Error::new(format!(
                        "{} has no field named {}",
                        parent_field, other
                    ))),
                }
            }
            None => Ok(toml::to_string(self).map_err(|err| err.to_string())?),
        }
    }
}
