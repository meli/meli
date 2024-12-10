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

//! Wrapper type [`HeaderName`] and [`HeaderMap`] for case-insensitive
//! comparisons.
//!
//! # Synopsis
//!
//! This module provides a container type for headers and their values
//! ([`HeaderMap`]) and a type to represent header names [`HeaderName`].
//!
//! [`HeaderMap`] is a wrapper over [`IndexMap`] makes sure that all key lookups
//! are case-insensitive by using the [`HeaderName`] type as hash key.
//!
//! [`HeaderName`] is a container of bytes that guarantees its contents are a
//! valid header name as specified by the relevant standards. See its
//! documentation for more information.

pub mod names;
pub mod standards;
#[cfg(test)]
mod tests;

use std::{
    cmp::{Eq, PartialEq},
    convert::{TryFrom, TryInto},
    ops::{Deref, DerefMut},
};

use indexmap::IndexMap;
pub use names::{HeaderName, InvalidHeaderName};
pub use standards::{Protocol, Standard, StandardHeader, Status};

/// Map of mail headers and values.
///
/// Can be indexed by:
///
/// - `usize` which is the order of insertion.
/// - `&[u8]`, which panics if it's not a valid header value.
/// - `&str`, which also panics if it's not a valid header value.
/// - [`HeaderName`], which is guaranteed to be valid.
///
/// # Panics
///
/// Except for the above, indexing will also panic if index is out of range or
/// header key is not present in the map.
///
/// For a non-panicking version see [`HeaderMap::get`] and
/// [`HeaderMap::get_mut`] methods.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct HeaderMap(indexmap::IndexMap<HeaderName, String>);

impl std::ops::Index<usize> for HeaderMap {
    type Output = str;

    #[inline]
    fn index(&self, k: usize) -> &Self::Output {
        (self.0)[k].as_str()
    }
}

impl std::ops::Index<&[u8]> for HeaderMap {
    type Output = str;

    #[inline]
    fn index(&self, k: &[u8]) -> &Self::Output {
        (self.0)[&HeaderName::try_from(k).expect("Invalid bytes in header name.")].as_str()
    }
}

impl std::ops::Index<&str> for HeaderMap {
    type Output = str;

    #[inline]
    fn index(&self, k: &str) -> &Self::Output {
        (self.0)[&HeaderName::try_from(k).expect("Invalid bytes in header name.")].as_str()
    }
}

impl std::ops::Index<&HeaderName> for HeaderMap {
    type Output = str;

    #[inline]
    fn index(&self, k: &HeaderName) -> &Self::Output {
        (self.0)[k].as_str()
    }
}

impl std::ops::Index<HeaderName> for HeaderMap {
    type Output = str;

    #[inline]
    fn index(&self, k: HeaderName) -> &Self::Output {
        (self.0)[&k].as_str()
    }
}

impl HeaderMap {
    #[inline]
    pub fn empty() -> Self {
        Self::default()
    }

    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    fn get_mut_inner(&mut self, key: &HeaderName) -> Option<&mut String> {
        (self.0).get_mut(key)
    }

    #[inline]
    pub fn get_mut<T: TryInto<HeaderName> + std::fmt::Debug>(
        &mut self,
        key: T,
    ) -> Option<&mut String>
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        let k = key.try_into().ok()?;
        self.get_mut_inner(&k)
    }

    #[inline]
    fn get_inner(&self, key: &HeaderName) -> Option<&str> {
        (self.0).get(key).map(|x| x.as_str())
    }

    #[inline]
    pub fn get<T: TryInto<HeaderName> + std::fmt::Debug>(&self, key: T) -> Option<&str>
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        let k = key.try_into().ok()?;
        self.get_inner(&k)
    }

    #[inline]
    fn contains_key_inner(&self, key: &HeaderName) -> bool {
        (self.0).contains_key(key)
    }

    #[inline]
    pub fn contains_key<T: TryInto<HeaderName> + std::fmt::Debug>(&self, key: T) -> bool
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        key.try_into()
            .ok()
            .map(|k| self.contains_key_inner(&k))
            .unwrap_or(false)
    }

    #[inline]
    fn remove_inner(&mut self, key: &HeaderName) -> Option<String> {
        (self.0).shift_remove(key)
    }

    #[inline]
    pub fn remove<T: TryInto<HeaderName> + std::fmt::Debug>(&mut self, key: T) -> Option<String>
    where
        <T as TryInto<HeaderName>>::Error: std::fmt::Debug,
    {
        key.try_into().ok().and_then(|k| self.remove_inner(&k))
    }

    #[inline]
    pub fn into_inner(self) -> indexmap::IndexMap<HeaderName, String> {
        self.0
    }
}

impl Deref for HeaderMap {
    type Target = IndexMap<HeaderName, String>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for HeaderMap {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
