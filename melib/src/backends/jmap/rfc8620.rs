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

use super::Id;
use crate::email::parser::BytesExt;
use core::marker::PhantomData;
use serde::de::DeserializeOwned;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use serde_json::{value::RawValue, Value};

mod filters;
pub use filters::*;
mod comparator;
pub use comparator::*;
mod argument;
pub use argument::*;

use super::protocol::Method;
use std::collections::HashMap;
pub trait Object {
    const NAME: &'static str;
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct JmapSession {
    pub capabilities: HashMap<String, CapabilitiesObject>,
    pub accounts: HashMap<Id, Account>,
    pub primary_accounts: HashMap<String, Id>,
    pub username: String,
    pub api_url: String,
    pub download_url: String,

    pub upload_url: String,
    pub event_source_url: String,
    pub state: String,
    #[serde(flatten)]
    pub extra_properties: HashMap<String, Value>,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CapabilitiesObject {
    #[serde(default)]
    max_size_upload: u64,
    #[serde(default)]
    max_concurrent_upload: u64,
    #[serde(default)]
    max_size_request: u64,
    #[serde(default)]
    max_concurrent_requests: u64,
    #[serde(default)]
    max_calls_in_request: u64,
    #[serde(default)]
    max_objects_in_get: u64,
    #[serde(default)]
    max_objects_in_set: u64,
    #[serde(default)]
    collation_algorithms: Vec<String>,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Account {
    name: String,
    is_personal: bool,
    is_read_only: bool,
    account_capabilities: HashMap<String, Value>,
    #[serde(flatten)]
    extra_properties: HashMap<String, Value>,
}

/// #`get`
///
///    Objects of type `Foo` are fetched via a call to `Foo/get`.
///
///    It takes the following arguments:
///
///    - `account_id`: "Id"
///
///       The id of the account to use.
///
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Get<OBJ: Object>
where
    OBJ: std::fmt::Debug + Serialize,
{
    #[serde(skip_serializing_if = "String::is_empty")]
    pub account_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(flatten)]
    pub ids: Option<JmapArgument<Vec<String>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<String>>,
    #[serde(skip)]
    _ph: PhantomData<*const OBJ>,
}

impl<OBJ: Object> Get<OBJ>
where
    OBJ: std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: String::new(),
            ids: None,
            properties: None,
            _ph: PhantomData,
        }
    }
    _impl!(
        ///   -  accountId: "Id"
        ///
        ///      The id of the account to use.
        ///
        account_id: String
    );
    _impl!(
        ///   -  ids: `Option<JmapArgument<Vec<String>>>`
        ///
        ///      The ids of the Foo objects to return.  If `None`, then *all* records
        ///      of the data type are returned, if this is supported for that data
        ///      type and the number of records does not exceed the
        ///      "max_objects_in_get" limit.
        ///
        ids: Option<JmapArgument<Vec<String>>>
    );
    _impl!(
        ///   -  properties: Option<Vec<String>>
        ///
        ///      If supplied, only the properties listed in the array are returned
        ///      for each `Foo` object.  If `None`, all properties of the object are
        ///      returned.  The `id` property of the object is *always* returned,
        ///      even if not explicitly requested.  If an invalid property is
        ///      requested, the call WILL be rejected with an "invalid_arguments"
        ///      error.
        properties: Option<Vec<String>>
    );
}

impl<OBJ: Object + Serialize + std::fmt::Debug> Serialize for Get<OBJ> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut fields_no = 0;
        if !self.account_id.is_empty() {
            fields_no += 1;
        }
        if self.ids.is_some() {
            fields_no += 1;
        }
        if self.properties.is_some() {
            fields_no += 1;
        }

        let mut state = serializer.serialize_struct("Get", fields_no)?;
        if !self.account_id.is_empty() {
            state.serialize_field("accountId", &self.account_id)?;
        }
        match self.ids.as_ref() {
            None => {}
            Some(JmapArgument::Value(ref v)) => state.serialize_field("ids", v)?,
            Some(JmapArgument::ResultReference {
                ref result_of,
                ref name,
                ref path,
            }) => {
                #[derive(Serialize)]
                #[serde(rename_all = "camelCase")]
                struct A<'a> {
                    result_of: &'a str,
                    name: &'a str,
                    path: &'a str,
                }

                state.serialize_field(
                    "#ids",
                    &A {
                        result_of,
                        name,
                        path,
                    },
                )?;
            }
        }

        if self.properties.is_some() {
            state.serialize_field("properties", self.properties.as_ref().unwrap())?;
        }

        state.end()
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MethodResponse<'a> {
    #[serde(borrow)]
    pub method_responses: Vec<&'a RawValue>,
    #[serde(default)]
    pub created_ids: HashMap<Id, Id>,
    #[serde(default)]
    pub session_state: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct GetResponse<OBJ: Object> {
    #[serde(skip_serializing_if = "String::is_empty")]
    pub account_id: String,
    #[serde(default)]
    pub state: String,
    pub list: Vec<OBJ>,
    pub not_found: Vec<String>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for GetResponse<OBJ> {
    type Error = crate::error::MeliError;
    fn try_from(t: &RawValue) -> Result<GetResponse<OBJ>, crate::error::MeliError> {
        let res: (String, GetResponse<OBJ>, String) = serde_json::from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/get", OBJ::NAME));
        Ok(res.1)
    }
}

impl<OBJ: Object> GetResponse<OBJ> {
    _impl!(get_mut  account_id_mut, account_id: String);
    _impl!(get_mut  state_mut, state: String);
    _impl!(get_mut  list_mut, list: Vec<OBJ>);
    _impl!(get_mut  not_found_mut, not_found: Vec<String>);
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum JmapError {
    RequestTooLarge,
    InvalidArguments,
    InvalidResultReference,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Query<F: FilterTrait<OBJ>, OBJ: Object>
where
    OBJ: std::fmt::Debug + Serialize,
{
    account_id: String,
    filter: Option<F>,
    sort: Option<Comparator<OBJ>>,
    #[serde(default)]
    position: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    anchor: Option<String>,
    #[serde(default)]
    #[serde(skip_serializing_if = "u64_zero")]
    anchor_offset: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    limit: Option<u64>,
    #[serde(default = "bool_false")]
    calculate_total: bool,
    #[serde(skip)]
    _ph: PhantomData<*const OBJ>,
}

impl<F: FilterTrait<OBJ>, OBJ: Object> Query<F, OBJ>
where
    OBJ: std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: String::new(),
            filter: None,
            sort: None,
            position: 0,
            anchor: None,
            anchor_offset: 0,
            limit: None,
            calculate_total: false,
            _ph: PhantomData,
        }
    }

    _impl!(account_id: String);
    _impl!(filter: Option<F>);
    _impl!(sort: Option<Comparator<OBJ>>);
    _impl!(position: u64);
    _impl!(anchor: Option<String>);
    _impl!(anchor_offset: u64);
    _impl!(limit: Option<u64>);
    _impl!(calculate_total: bool);
}

pub fn u64_zero(num: &u64) -> bool {
    *num == 0
}

pub fn bool_false() -> bool {
    false
}

pub fn bool_true() -> bool {
    true
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct QueryResponse<OBJ: Object> {
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub account_id: String,
    pub query_state: String,
    pub can_calculate_changes: bool,
    pub position: u64,
    pub ids: Vec<Id>,
    #[serde(default)]
    pub total: u64,
    #[serde(default)]
    pub limit: u64,
    #[serde(skip)]
    _ph: PhantomData<*const OBJ>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for QueryResponse<OBJ> {
    type Error = crate::error::MeliError;
    fn try_from(t: &RawValue) -> Result<QueryResponse<OBJ>, crate::error::MeliError> {
        let res: (String, QueryResponse<OBJ>, String) = serde_json::from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/query", OBJ::NAME));
        Ok(res.1)
    }
}

impl<OBJ: Object> QueryResponse<OBJ> {
    _impl!(get_mut  ids_mut, ids: Vec<Id>);
}

pub struct ResultField<M: Method<OBJ>, OBJ: Object> {
    pub field: &'static str,
    pub _ph: PhantomData<*const (OBJ, M)>,
}

// error[E0723]: trait bounds other than `Sized` on const fn parameters are unstable
//    --> melib/src/backends/jmap/rfc8620.rs:626:6
//     |
// 626 | impl<M: Method<OBJ>, OBJ: Object> ResultField<M, OBJ> {
//     |      ^
//     |
//     = note: for more information, see issue https://github.com/rust-lang/rust/issues/57563
//     = help: add `#![feature(const_fn)]` to the crate attributes to enable
// impl<M: Method<OBJ>, OBJ: Object> ResultField<M, OBJ> {
//     pub const fn new(field: &'static str) -> Self {
//         Self {
//             field,
//             _ph: PhantomData,
//         }
//     }
// }

/// #`changes`
///
///    The "Foo/changes" method allows a client to efficiently update the state of its Foo cache
///    to match the new state on the server. It takes the following arguments:
///
///    - accountId: "Id" The id of the account to use.
///    - sinceState: "String"
///     The current state of the client. This is the string that was
///     returned as the "state" argument in the "Foo/get" response. The
///     server will return the changes that have occurred since this
///     state.
///
///    - maxChanges: "UnsignedInt|null"
///     The maximum number of ids to return in the response. The server
///     MAY choose to return fewer than this value but MUST NOT return
///     more. If not given by the client, the server may choose how many
///     to return. If supplied by the client, the value MUST be a
///     positive integer greater than 0. If a value outside of this range
///     is given, the server MUST re
///
#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
/* ch-ch-ch-ch-ch-Changes */
pub struct Changes<OBJ: Object>
where
    OBJ: std::fmt::Debug + Serialize,
{
    #[serde(skip_serializing_if = "String::is_empty")]
    pub account_id: String,
    pub since_state: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_changes: Option<u64>,
    #[serde(skip)]
    _ph: PhantomData<*const OBJ>,
}

impl<OBJ: Object> Changes<OBJ>
where
    OBJ: std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: String::new(),
            since_state: String::new(),
            max_changes: None,
            _ph: PhantomData,
        }
    }
    _impl!(
        ///   -  accountId: "Id"
        ///
        ///      The id of the account to use.
        ///
        account_id: String
    );
    _impl!(
        ///    - since_state: "String"
        ///     The current state of the client. This is the string that was
        ///     returned as the "state" argument in the "Foo/get" response. The
        ///     server will return the changes that have occurred since this
        ///     state.
        ///
        ///
        since_state: String
    );
    _impl!(
        ///    - max_changes: "UnsignedInt|null"
        ///     The maximum number of ids to return in the response. The server
        ///     MAY choose to return fewer than this value but MUST NOT return
        ///     more. If not given by the client, the server may choose how many
        ///     to return. If supplied by the client, the value MUST be a
        ///     positive integer greater than 0. If a value outside of this range
        ///     is given, the server MUST re
        max_changes: Option<u64>
    );
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ChangesResponse<OBJ: Object> {
    #[serde(skip_serializing_if = "String::is_empty")]
    pub account_id: String,
    pub old_state: String,
    pub new_state: String,
    pub has_more_changes: bool,
    pub created: Vec<Id>,
    pub updated: Vec<Id>,
    pub destroyed: Vec<Id>,
    #[serde(skip)]
    _ph: PhantomData<*const OBJ>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for ChangesResponse<OBJ> {
    type Error = crate::error::MeliError;
    fn try_from(t: &RawValue) -> Result<ChangesResponse<OBJ>, crate::error::MeliError> {
        let res: (String, ChangesResponse<OBJ>, String) = serde_json::from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/changes", OBJ::NAME));
        Ok(res.1)
    }
}

impl<OBJ: Object> ChangesResponse<OBJ> {
    _impl!(get_mut  account_id_mut, account_id: String);
    _impl!(get_mut  old_state_mut, old_state: String);
    _impl!(get_mut  new_state_mut, new_state: String);
    _impl!(get has_more_changes, has_more_changes: bool);
    _impl!(get_mut  created_mut, created: Vec<String>);
    _impl!(get_mut  updated_mut, updated: Vec<String>);
    _impl!(get_mut  destroyed_mut, destroyed: Vec<String>);
}

pub fn download_request_format(
    session: &JmapSession,
    account_id: &Id,
    blob_id: &Id,
    name: Option<String>,
) -> String {
    // https://jmap.fastmail.com/download/{accountId}/{blobId}/{name}
    let mut ret = String::with_capacity(
        session.download_url.len()
            + blob_id.len()
            + name.as_ref().map(|n| n.len()).unwrap_or(0)
            + account_id.len(),
    );
    let mut prev_pos = 0;

    while let Some(pos) = session.download_url.as_bytes()[prev_pos..].find(b"{") {
        ret.push_str(&session.download_url[prev_pos..prev_pos + pos]);
        prev_pos += pos;
        if session.download_url[prev_pos..].starts_with("{accountId}") {
            ret.push_str(account_id);
            prev_pos += "{accountId}".len();
        } else if session.download_url[prev_pos..].starts_with("{blobId}") {
            ret.push_str(blob_id);
            prev_pos += "{blobId}".len();
        } else if session.download_url[prev_pos..].starts_with("{name}") {
            ret.push_str(name.as_ref().map(String::as_str).unwrap_or(""));
            prev_pos += "{name}".len();
        }
    }
    if prev_pos != session.download_url.len() {
        ret.push_str(&session.download_url[prev_pos..]);
    }
    ret
}

pub fn upload_request_format(session: &JmapSession, account_id: &Id) -> String {
    //"uploadUrl": "https://jmap.fastmail.com/upload/{accountId}/",
    let mut ret = String::with_capacity(session.upload_url.len() + account_id.len());
    let mut prev_pos = 0;

    while let Some(pos) = session.upload_url.as_bytes()[prev_pos..].find(b"{") {
        ret.push_str(&session.upload_url[prev_pos..prev_pos + pos]);
        prev_pos += pos;
        if session.upload_url[prev_pos..].starts_with("{accountId}") {
            ret.push_str(account_id);
            prev_pos += "{accountId}".len();
            break;
        } else {
            ret.push('{');
            prev_pos += 1;
        }
    }
    if prev_pos != session.upload_url.len() {
        ret.push_str(&session.upload_url[prev_pos..]);
    }
    ret
}
