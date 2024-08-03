/*
 * meli - jmap module.
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

use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use indexmap::IndexMap;
use serde_json::Value;

use crate::jmap::protocol::Method;

pub type PatchObject = Value;

impl Object for PatchObject {
    const NAME: &'static str = "PatchObject";
}

pub trait Object: Send + Sync {
    const NAME: &'static str;
    const SERVER_SET_FIELDS: &'static [&'static str] = &["id"];
}

#[derive(Deserialize, Serialize)]
#[serde(transparent)]
pub struct Id<OBJ> {
    pub inner: String,
    #[serde(skip)]
    pub _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ: Object> std::fmt::Debug for Id<OBJ> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple(&format!("Id<{}>", OBJ::NAME))
            .field(&self.inner)
            .finish()
    }
}

impl std::fmt::Debug for Id<String> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("Id<Any>").field(&self.inner).finish()
    }
}

//, Hash, Eq, PartialEq, Default)]
impl<OBJ> Clone for Id<OBJ> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _ph: PhantomData,
        }
    }
}

impl<OBJ> std::cmp::Eq for Id<OBJ> {}

impl<OBJ> std::cmp::PartialEq for Id<OBJ> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<OBJ> Hash for Id<OBJ> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<OBJ> Default for Id<OBJ> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<OBJ> From<String> for Id<OBJ> {
    fn from(inner: String) -> Self {
        Self {
            inner,
            _ph: PhantomData,
        }
    }
}

impl<OBJ> From<&str> for Id<OBJ> {
    fn from(inner: &str) -> Self {
        Self {
            inner: inner.to_string(),
            _ph: PhantomData,
        }
    }
}

impl<OBJ> std::fmt::Display for Id<OBJ> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.inner, fmt)
    }
}

impl<OBJ> Id<OBJ> {
    pub fn empty() -> Self {
        Self {
            inner: String::new(),
            _ph: PhantomData,
        }
    }

    pub fn new_uuid_v4() -> Self {
        Self {
            inner: uuid::Uuid::new_v4().hyphenated().to_string(),
            _ph: PhantomData,
        }
    }

    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(transparent)]
pub struct State<OBJ> {
    pub inner: String,
    #[serde(skip)]
    pub _ph: PhantomData<fn() -> OBJ>,
}

//, Hash, Eq, PartialEq, Default)]
impl<OBJ> Clone for State<OBJ> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _ph: PhantomData,
        }
    }
}

impl<OBJ> std::cmp::Eq for State<OBJ> {}

impl<OBJ> std::cmp::PartialEq for State<OBJ> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<OBJ> Hash for State<OBJ> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<OBJ> Default for State<OBJ> {
    fn default() -> Self {
        Self::new()
    }
}

impl<OBJ> From<String> for State<OBJ> {
    fn from(inner: String) -> Self {
        Self {
            inner,
            _ph: PhantomData,
        }
    }
}

impl<OBJ> std::fmt::Display for State<OBJ> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.inner, fmt)
    }
}

impl<OBJ> State<OBJ> {
    pub fn new() -> Self {
        Self {
            inner: String::new(),
            _ph: PhantomData,
        }
    }

    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Account {
    pub name: String,
    pub is_personal: bool,
    pub is_read_only: bool,
    pub account_capabilities: IndexMap<String, Value>,
    #[serde(flatten)]
    pub extra_properties: IndexMap<String, Value>,
}

impl Object for Account {
    const NAME: &'static str = stringify!(Account);
}

#[derive(Clone, Copy, Debug)]
pub struct BlobObject;

impl Object for BlobObject {
    const NAME: &'static str = "Blob";
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BlobGet;

impl Method<BlobObject> for BlobGet {
    const NAME: &'static str = "Blob/get";
}
