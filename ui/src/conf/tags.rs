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

use serde::{Deserialize, Deserializer};
use std::collections::{hash_map::DefaultHasher, HashMap, HashSet};
use std::hash::Hasher;

#[derive(Debug, Deserialize, Clone, Serialize)]
pub struct TagsSettings {
    #[serde(default, deserialize_with = "tag_color_de")]
    pub colors: HashMap<u64, u8>,
    #[serde(default, deserialize_with = "tag_set_de")]
    pub ignore_tags: HashSet<u64>,
}

impl Default for TagsSettings {
    fn default() -> Self {
        TagsSettings {
            colors: Default::default(),
            ignore_tags: Default::default(),
        }
    }
}

pub fn tag_set_de<'de, D>(deserializer: D) -> std::result::Result<HashSet<u64>, D::Error>
where
    D: Deserializer<'de>,
{
    Ok(<Vec<String>>::deserialize(deserializer)?
        .into_iter()
        .map(|tag| {
            let mut hasher = DefaultHasher::new();
            hasher.write(tag.as_bytes());
            hasher.finish()
        })
        .collect())
}

pub fn tag_color_de<'de, D>(deserializer: D) -> std::result::Result<HashMap<u64, u8>, D::Error>
where
    D: Deserializer<'de>,
{
    Ok(<HashMap<String, u8>>::deserialize(deserializer)?
        .into_iter()
        .map(|(tag, color)| {
            let mut hasher = DefaultHasher::new();
            hasher.write(tag.as_bytes());
            (hasher.finish(), color)
        })
        .collect())
}
