/*
 * meli - headers
 *
 * Copyright 2020 Manos Pitsidianakis
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

//! Wrapper type [`HeaderName`] for case-insensitive comparisons.

pub mod names;
pub mod standards;
use std::{
    borrow::Borrow,
    cmp::{Eq, PartialEq},
    convert::{TryFrom, TryInto},
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
};

use indexmap::IndexMap;
pub use names::{HeaderName, InvalidHeaderName, Protocol};
pub use standards::{Standard, StandardHeader, Status};

trait HeaderKey {
    fn to_key(&self) -> &[u8];
}

impl Hash for dyn HeaderKey + '_ {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for b in self.to_key().iter() {
            b.to_ascii_lowercase().hash(state);
        }
    }
}

impl PartialEq for dyn HeaderKey + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.to_key().eq_ignore_ascii_case(other.to_key())
    }
}

impl Eq for dyn HeaderKey + '_ {}

impl HeaderKey for HeaderName {
    fn to_key(&self) -> &[u8] {
        self.as_lowercase_bytes()
    }
}

//Implement Borrow for all the lookup types as returning our trait object:

impl<'a> Borrow<dyn HeaderKey + 'a> for HeaderName {
    fn borrow(&self) -> &(dyn HeaderKey + 'a) {
        self
    }
}

/// Map of mail headers and values.
///
/// Can be indexed by:
///
/// - `usize`
/// - `&[u8]`, which panics if it's not a valid header value.
/// - `&str`, which also panics if it's not a valid header value.
/// - [`HeaderName`], which is guaranteed to be valid.
///
/// # Panics
///
/// Except for the above, indexing will also panic if index is out of range or
/// header key is not present in the map.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct HeaderMap(indexmap::IndexMap<HeaderName, String>);

impl std::ops::Index<usize> for HeaderMap {
    type Output = str;
    fn index(&self, k: usize) -> &Self::Output {
        (self.0)[k].as_str()
    }
}

impl std::ops::Index<&[u8]> for HeaderMap {
    type Output = str;
    fn index(&self, k: &[u8]) -> &Self::Output {
        (self.0)[&HeaderName::try_from(k).expect("Invalid bytes in header name.")].as_str()
    }
}

impl std::ops::Index<&str> for HeaderMap {
    type Output = str;
    fn index(&self, k: &str) -> &Self::Output {
        (self.0)[&HeaderName::try_from(k).expect("Invalid bytes in header name.")].as_str()
    }
}

impl std::ops::Index<&HeaderName> for HeaderMap {
    type Output = str;
    fn index(&self, k: &HeaderName) -> &Self::Output {
        (self.0)[k].as_str()
    }
}

impl std::ops::Index<HeaderName> for HeaderMap {
    type Output = str;
    fn index(&self, k: HeaderName) -> &Self::Output {
        (self.0)[&k].as_str()
    }
}

impl HeaderMap {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_mut<T: TryInto<HeaderName> + std::fmt::Debug>(
        &mut self,
        key: T,
    ) -> Option<&mut String>
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        let k = key.try_into().ok()?;
        (self.0).get_mut(&k)
    }

    pub fn get<T: TryInto<HeaderName> + std::fmt::Debug>(&self, key: T) -> Option<&str>
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        let k = key.try_into().ok()?;
        (self.0).get(&k).map(|x| x.as_str())
    }

    pub fn contains_key<T: TryInto<HeaderName> + std::fmt::Debug>(&self, key: T) -> bool
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        key.try_into()
            .ok()
            .map(|k| (self.0).contains_key(&k))
            .unwrap_or(false)
    }

    pub fn remove<T: TryInto<HeaderName> + std::fmt::Debug>(&mut self, key: T) -> Option<String>
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        key.try_into().ok().and_then(|k| (self.0).remove(&k))
    }

    pub fn into_inner(self) -> indexmap::IndexMap<HeaderName, String> {
        self.0
    }
}

impl Deref for HeaderMap {
    type Target = IndexMap<HeaderName, String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for HeaderMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_headers_case_sensitivity() {
        let mut headers = HeaderMap::default();
        headers.insert("from".try_into().unwrap(), "Myself <a@b.c>".into());
        assert_eq!(&headers["From"], "Myself <a@b.c>");
        assert_eq!(&headers["From"], &headers["from"]);
        assert_eq!(&headers["fROm"], &headers["from"]);
        headers.get_mut("from").unwrap().pop();
        assert_eq!(&headers["From"], "Myself <a@b.c");
        headers.insert("frOM".try_into().unwrap(), "nada".into());
        assert_eq!(&headers["fROm"], "nada");
    }

    #[test]
    fn test_headers_map_index() {
        let mut headers = HeaderMap::default();
        headers.insert(HeaderName::SUBJECT, "foobar".into());
        headers.insert(HeaderName::MESSAGE_ID, "foobar@examplecom".into());
        assert_eq!(&headers[0], "foobar");
        assert_eq!(&headers[HeaderName::SUBJECT], "foobar");
        assert_eq!(&headers[&HeaderName::SUBJECT], "foobar");
        assert_eq!(&headers["subject"], "foobar");
        assert_eq!(&headers["Subject"], "foobar");
        assert_eq!(&headers[b"Subject".as_slice()], "foobar");
        assert!(&headers[HeaderName::MESSAGE_ID] != "foobar");
    }
}
