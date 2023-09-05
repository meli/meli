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

use std::sync::Arc;

use indexmap::IndexMap;
use serde_json::Value;

use crate::jmap::{
    rfc8620::{Account, Id, Object, State},
    IdentityObject,
};

#[derive(Deserialize, Serialize, Debug, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct Session {
    pub capabilities: IndexMap<String, CapabilitiesObject>,
    pub accounts: IndexMap<Id<Account>, Account>,
    pub primary_accounts: IndexMap<String, Id<Account>>,
    #[serde(skip)]
    pub identities: IndexMap<Id<IdentityObject>, IdentityObject>,
    pub username: String,
    pub api_url: Arc<String>,
    pub download_url: Arc<String>,

    pub upload_url: Arc<String>,
    pub event_source_url: Arc<String>,
    pub state: State<Session>,
    #[serde(flatten)]
    pub extra_properties: IndexMap<String, Value>,
}

impl Object for Session {
    const NAME: &'static str = stringify!(Session);
}

#[derive(Deserialize, Serialize, Clone, Default, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CapabilitiesObject {
    #[serde(default)]
    pub max_size_upload: u64,
    #[serde(default)]
    pub max_concurrent_upload: u64,
    #[serde(default)]
    pub max_size_request: u64,
    #[serde(default)]
    pub max_concurrent_requests: u64,
    #[serde(default)]
    pub max_calls_in_request: u64,
    #[serde(default)]
    pub max_objects_in_get: u64,
    #[serde(default)]
    pub max_objects_in_set: u64,
    #[serde(default)]
    pub collation_algorithms: Vec<String>,
}