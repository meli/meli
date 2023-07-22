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

use core::marker::PhantomData;
use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use serde::{
    de::DeserializeOwned,
    ser::{Serialize, SerializeStruct, Serializer},
};
use serde_json::{value::RawValue, Value};

use crate::email::parser::BytesExt;

mod filters;
pub use filters::*;
mod comparator;
pub use comparator::*;
mod argument;
use std::collections::HashMap;

pub use argument::*;

use super::{deserialize_from_str, protocol::Method};
pub trait Object {
    const NAME: &'static str;
}

#[derive(Deserialize, Serialize)]
#[serde(transparent)]
pub struct Id<OBJ> {
    pub inner: String,
    #[serde(skip)]
    pub _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ: Object> core::fmt::Debug for Id<OBJ> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple(&format!("Id<{}>", OBJ::NAME))
            .field(&self.inner)
            .finish()
    }
}

impl core::fmt::Debug for Id<String> {
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
        Self::new()
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

impl<OBJ> core::fmt::Display for Id<OBJ> {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(&self.inner, fmt)
    }
}

impl<OBJ> Id<OBJ> {
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

#[derive(Deserialize, Serialize, Debug)]
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

impl<OBJ> core::fmt::Display for State<OBJ> {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(&self.inner, fmt)
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

#[derive(Deserialize, Serialize, Debug, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct JmapSession {
    pub capabilities: HashMap<String, CapabilitiesObject>,
    pub accounts: HashMap<Id<Account>, Account>,
    pub primary_accounts: HashMap<String, Id<Account>>,
    pub username: String,
    pub api_url: Arc<String>,
    pub download_url: Arc<String>,

    pub upload_url: Arc<String>,
    pub event_source_url: Arc<String>,
    pub state: State<JmapSession>,
    #[serde(flatten)]
    pub extra_properties: HashMap<String, Value>,
}

impl Object for JmapSession {
    const NAME: &'static str = "Session";
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

#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Account {
    pub name: String,
    pub is_personal: bool,
    pub is_read_only: bool,
    pub account_capabilities: HashMap<String, Value>,
    #[serde(flatten)]
    pub extra_properties: HashMap<String, Value>,
}

impl Object for Account {
    const NAME: &'static str = "Account";
}

#[derive(Copy, Clone, Debug)]
pub struct BlobObject;

impl Object for BlobObject {
    const NAME: &'static str = "Blob";
}

/// #`get`
///
///    Objects of type `Foo` are fetched via a call to `Foo/get`.
///
///    It takes the following arguments:
///
///    - `account_id`: `Id`
///
///       The id of the account to use.
#[derive(Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Get<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub account_id: Id<Account>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(flatten)]
    pub ids: Option<JmapArgument<Vec<Id<OBJ>>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<String>>,
    #[serde(skip)]
    _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ> Get<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: Id::new(),
            ids: None,
            properties: None,
            _ph: PhantomData,
        }
    }
    _impl!(
        ///   - accountId: `Id`
        ///
        ///      The id of the account to use.
        account_id: Id<Account>
    );
    _impl!(
        ///   - ids: `Option<JmapArgument<Vec<String>>>`
        ///
        ///      The ids of the Foo objects to return.  If `None`, then *all*
        /// records      of the data type are returned, if this is
        /// supported for that data      type and the number of records
        /// does not exceed the      `max_objects_in_get` limit.
        ids: Option<JmapArgument<Vec<Id<OBJ>>>>
    );
    _impl!(
        ///   - properties: `Option<Vec<String>>`
        ///
        ///      If supplied, only the properties listed in the array are
        /// returned      for each `Foo` object.  If `None`, all
        /// properties of the object are      returned.  The `id`
        /// property of the object is *always* returned,      even if
        /// not explicitly requested.  If an invalid property is
        ///      requested, the call WILL be rejected with an
        /// `invalid_arguments`      error.
        properties: Option<Vec<String>>
    );
}

impl<OBJ> Default for Get<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    fn default() -> Self {
        Self::new()
    }
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

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MethodResponse<'a> {
    #[serde(borrow)]
    pub method_responses: Vec<&'a RawValue>,
    #[serde(default)]
    pub created_ids: HashMap<Id<String>, Id<String>>,
    #[serde(default)]
    pub session_state: State<JmapSession>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct GetResponse<OBJ: Object> {
    pub account_id: Id<Account>,
    #[serde(default = "State::default")]
    pub state: State<OBJ>,
    pub list: Vec<OBJ>,
    pub not_found: Vec<Id<OBJ>>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for GetResponse<OBJ> {
    type Error = crate::error::Error;

    fn try_from(t: &RawValue) -> Result<Self, crate::error::Error> {
        let res: (String, Self, String) = deserialize_from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/get", OBJ::NAME));
        Ok(res.1)
    }
}

impl<OBJ: Object> GetResponse<OBJ> {
    _impl!(get_mut  account_id_mut, account_id: Id<Account>);
    _impl!(get_mut  state_mut, state: State<OBJ>);
    _impl!(get_mut  list_mut, list: Vec<OBJ>);
    _impl!(get_mut  not_found_mut, not_found: Vec<Id<OBJ>>);
}

#[derive(Deserialize, Clone, Copy, Debug)]
#[serde(rename_all = "camelCase")]
enum JmapError {
    RequestTooLarge,
    InvalidArguments,
    InvalidResultReference,
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Query<F: FilterTrait<OBJ>, OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub account_id: Id<Account>,
    pub filter: Option<F>,
    pub sort: Option<Comparator<OBJ>>,
    #[serde(default)]
    pub position: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anchor: Option<String>,
    #[serde(default)]
    #[serde(skip_serializing_if = "u64_zero")]
    pub anchor_offset: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limit: Option<u64>,
    #[serde(default = "bool_false")]
    pub calculate_total: bool,
    #[serde(skip)]
    _ph: PhantomData<fn() -> OBJ>,
}

impl<F: FilterTrait<OBJ>, OBJ> Query<F, OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: Id::new(),
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

    _impl!(account_id: Id<Account>);
    _impl!(filter: Option<F>);
    _impl!(sort: Option<Comparator<OBJ>>);
    _impl!(position: u64);
    _impl!(anchor: Option<String>);
    _impl!(anchor_offset: u64);
    _impl!(limit: Option<u64>);
    _impl!(calculate_total: bool);
}

impl<F: FilterTrait<OBJ>, OBJ> Default for Query<F, OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    fn default() -> Self {
        Self::new()
    }
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

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct QueryResponse<OBJ: Object> {
    pub account_id: Id<Account>,
    pub query_state: String,
    pub can_calculate_changes: bool,
    pub position: u64,
    pub ids: Vec<Id<OBJ>>,
    #[serde(default)]
    pub total: u64,
    #[serde(default)]
    pub limit: u64,
    #[serde(skip)]
    _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for QueryResponse<OBJ> {
    type Error = crate::error::Error;

    fn try_from(t: &RawValue) -> std::result::Result<Self, Self::Error> {
        let res: (String, Self, String) = deserialize_from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/query", OBJ::NAME));
        Ok(res.1)
    }
}

impl<OBJ: Object> QueryResponse<OBJ> {
    _impl!(get_mut  ids_mut, ids: Vec<Id<OBJ>>);
}

pub struct ResultField<M: Method<OBJ>, OBJ: Object> {
    pub field: &'static str,
    pub _ph: PhantomData<fn() -> (OBJ, M)>,
}

impl<M: Method<OBJ>, OBJ: Object> ResultField<M, OBJ> {
    pub fn new(field: &'static str) -> Self {
        Self {
            field,
            _ph: PhantomData,
        }
    }
}

// error[E0723]: trait bounds other than `Sized` on const fn parameters are
// unstable    --> melib/src/backends/jmap/rfc8620.rs:626:6
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
///    The `Foo/changes` method allows a client to efficiently update the state
/// of its Foo cache    to match the new state on the server. It takes the
/// following arguments:
///
///    - accountId: `Id` The id of the account to use.
///    - sinceState: `String`
///     The current state of the client. This is the string that was
///     returned as the `state` argument in the `Foo/get` response. The
///     server will return the changes that have occurred since this
///     state.
///
///    - maxChanges: `UnsignedInt|null`
///     The maximum number of ids to return in the response. The server
///     MAY choose to return fewer than this value but MUST NOT return
///     more. If not given by the client, the server may choose how many
///     to return. If supplied by the client, the value MUST be a
///     positive integer greater than 0. If a value outside of this range
///     is given, the server MUST re
#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
/* ch-ch-ch-ch-ch-Changes */
pub struct Changes<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub account_id: Id<Account>,
    pub since_state: State<OBJ>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_changes: Option<u64>,
    #[serde(skip)]
    _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ> Changes<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: Id::new(),
            since_state: State::new(),
            max_changes: None,
            _ph: PhantomData,
        }
    }
    _impl!(
        ///   - accountId: `Id`
        ///
        ///      The id of the account to use.
        account_id: Id<Account>
    );
    _impl!(
        ///    - since_state: `String`
        ///     The current state of the client. This is the string that was
        ///     returned as the `state` argument in the `Foo/get` response. The
        ///     server will return the changes that have occurred since this
        ///     state.
        since_state: State<OBJ>
    );
    _impl!(
        ///    - max_changes: `UnsignedInt|null`
        ///     The maximum number of ids to return in the response. The server
        ///     MAY choose to return fewer than this value but MUST NOT return
        ///     more. If not given by the client, the server may choose how many
        ///     to return. If supplied by the client, the value MUST be a
        ///     positive integer greater than 0. If a value outside of this
        /// range     is given, the server MUST re
        max_changes: Option<u64>
    );
}

impl<OBJ> Default for Changes<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ChangesResponse<OBJ: Object> {
    pub account_id: Id<Account>,
    pub old_state: State<OBJ>,
    pub new_state: State<OBJ>,
    pub has_more_changes: bool,
    pub created: Vec<Id<OBJ>>,
    pub updated: Vec<Id<OBJ>>,
    pub destroyed: Vec<Id<OBJ>>,
    #[serde(skip)]
    pub _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for ChangesResponse<OBJ> {
    type Error = crate::error::Error;

    fn try_from(t: &RawValue) -> std::result::Result<Self, Self::Error> {
        let res: (String, Self, String) = deserialize_from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/changes", OBJ::NAME));
        Ok(res.1)
    }
}

impl<OBJ: Object> ChangesResponse<OBJ> {
    _impl!(get_mut  account_id_mut, account_id: Id<Account>);
    _impl!(get_mut  old_state_mut, old_state: State<OBJ>);
    _impl!(get_mut  new_state_mut, new_state: State<OBJ>);
    _impl!(get has_more_changes, has_more_changes: bool);
    _impl!(get_mut  created_mut, created: Vec<Id<OBJ>>);
    _impl!(get_mut  updated_mut, updated: Vec<Id<OBJ>>);
    _impl!(get_mut  destroyed_mut, destroyed: Vec<Id<OBJ>>);
}

/// #`set`
///
/// Modifying the state of Foo objects on the server is done via the
/// `Foo/set` method.  This encompasses creating, updating, and
/// destroying Foo records.  This allows the server to sort out ordering
/// and dependencies that may exist if doing multiple operations at once
/// (for example, to ensure there is always a minimum number of a certain
/// record type).
#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Set<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    /// o  accountId: `Id`
    ///
    ///   The id of the account to use.
    pub account_id: Id<Account>,
    /// o  ifInState: `String|null`
    ///
    ///   This is a state string as returned by the `Foo/get` method
    ///   (representing the state of all objects of this type in the
    ///   account).  If supplied, the string must match the current state;
    ///   otherwise, the method will be aborted and a `stateMismatch` error
    ///   returned.  If null, any changes will be applied to the current
    ///   state.
    pub if_in_state: Option<State<OBJ>>,
    /// o  create: `Id[Foo]|null`
    ///
    ///   A map of a *creation id* (a temporary id set by the client) to Foo
    ///   objects, or null if no objects are to be created.
    ///
    ///   The Foo object type definition may define default values for
    ///   properties.  Any such property may be omitted by the client.
    ///
    ///   The client MUST omit any properties that may only be set by the
    ///   server (for example, the `id` property on most object types).
    pub create: Option<HashMap<Id<OBJ>, OBJ>>,
    /// o  update: `Id[PatchObject]|null`
    ///
    ///   A map of an id to a Patch object to apply to the current Foo
    ///   object with that id, or null if no objects are to be updated.
    ///
    ///   A *PatchObject* is of type `String[*]` and represents an unordered
    ///   set of patches.  The keys are a path in JSON Pointer format
    ///   `RFC6901`, with an implicit leading `/` (i.e., prefix each key
    ///   with `/` before applying the JSON Pointer evaluation algorithm).
    ///
    ///   All paths MUST also conform to the following restrictions; if
    ///   there is any violation, the update MUST be rejected with an
    ///   `invalidPatch` error:
    ///   * The pointer MUST NOT reference inside an array (i.e., you MUST NOT
    ///     insert/delete from an array; the array MUST be replaced in its
    ///     entirety instead).
    ///
    ///   * All parts prior to the last (i.e., the value after the final slash)
    ///     MUST already exist on the object being patched.
    ///
    ///   * There MUST NOT be two patches in the PatchObject where the pointer
    ///     of one is the prefix of the pointer of the other, e.g.,
    ///     `alerts/1/offset` and `alerts`.
    ///
    ///   The value associated with each pointer determines how to apply
    ///   that patch:
    ///
    ///   * If null, set to the default value if specified for this property;
    ///     otherwise, remove the property from the patched object.  If the key
    ///     is not present in the parent, this a no-op.
    ///
    ///   * Anything else: The value to set for this property (this may be a
    ///     replacement or addition to the object being patched).
    ///
    ///   Any server-set properties MAY be included in the patch if their
    ///   value is identical to the current server value (before applying
    ///   the patches to the object).  Otherwise, the update MUST be
    ///   rejected with an `invalidProperties` SetError.
    ///
    ///   This patch definition is designed such that an entire Foo object
    ///   is also a valid PatchObject.  The client may choose to optimise
    ///   network usage by just sending the diff or may send the whole
    ///   object; the server processes it the same either way.
    pub update: Option<HashMap<Id<OBJ>, Value>>,
    /// o  destroy: `Id[]|null`
    ///
    ///   A list of ids for Foo objects to permanently delete, or null if no
    ///   objects are to be destroyed.
    pub destroy: Option<Vec<Id<OBJ>>>,
}

impl<OBJ> Set<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub fn new() -> Self {
        Self {
            account_id: Id::new(),
            if_in_state: None,
            create: None,
            update: None,
            destroy: None,
        }
    }
    _impl!(account_id: Id<Account>);
    _impl!(
        /// o  ifInState: `String|null`
        ///
        ///   This is a state string as returned by the `Foo/get` method
        ///   (representing the state of all objects of this type in the
        ///   account).  If supplied, the string must match the current state;
        ///   otherwise, the method will be aborted and a `stateMismatch` error
        ///   returned.  If null, any changes will be applied to the current
        ///   state.
        if_in_state: Option<State<OBJ>>
    );
    _impl!(update: Option<HashMap<Id<OBJ>, Value>>);
}

impl<OBJ> Default for Set<OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SetResponse<OBJ: Object> {
    /// o  accountId: `Id`
    ///
    ///   The id of the account used for the call.
    pub account_id: Id<Account>,
    /// o  oldState: `String|null`
    ///
    ///   The state string that would have been returned by `Foo/get` before
    ///   making the requested changes, or null if the server doesn't know
    ///   what the previous state string was.
    pub old_state: State<OBJ>,
    /// o  newState: `String`
    ///
    ///   The state string that will now be returned by `Foo/get`.
    pub new_state: State<OBJ>,
    /// o  created: `Id[Foo]|null`
    ///
    ///   A map of the creation id to an object containing any properties of
    ///   the created Foo object that were not sent by the client.  This
    ///   includes all server-set properties (such as the `id` in most
    ///   object types) and any properties that were omitted by the client
    ///   and thus set to a default by the server.
    ///
    ///   This argument is null if no Foo objects were successfully created.
    pub created: Option<HashMap<Id<OBJ>, OBJ>>,
    /// o  updated: `Id[Foo|null]|null`
    ///
    ///   The keys in this map are the ids of all Foos that were
    ///   successfully updated.
    ///
    ///   The value for each id is a Foo object containing any property that
    ///   changed in a way *not* explicitly requested by the PatchObject
    ///   sent to the server, or null if none.  This lets the client know of
    ///   any changes to server-set or computed properties.
    ///
    ///   This argument is null if no Foo objects were successfully updated.
    pub updated: Option<HashMap<Id<OBJ>, Option<OBJ>>>,
    /// o  destroyed: `Id[]|null`
    ///
    ///   A list of Foo ids for records that were successfully destroyed, or
    ///   null if none.
    pub destroyed: Option<Vec<Id<OBJ>>>,
    /// o  notCreated: `Id[SetError]|null`
    ///
    ///   A map of the creation id to a SetError object for each record that
    ///   failed to be created, or null if all successful.
    pub not_created: Option<Vec<SetError>>,
    /// o  notUpdated: `Id[SetError]|null`
    ///
    ///   A map of the Foo id to a SetError object for each record that
    ///   failed to be updated, or null if all successful.
    pub not_updated: Option<Vec<SetError>>,
    /// o  notDestroyed: `Id[SetError]|null`
    ///
    ///   A map of the Foo id to a SetError object for each record that
    ///   failed to be destroyed, or null if all successful.//
    pub not_destroyed: Option<Vec<SetError>>,
}

impl<OBJ: Object + DeserializeOwned> std::convert::TryFrom<&RawValue> for SetResponse<OBJ> {
    type Error = crate::error::Error;

    fn try_from(t: &RawValue) -> Result<Self, crate::error::Error> {
        let res: (String, Self, String) = deserialize_from_str(t.get())?;
        assert_eq!(&res.0, &format!("{}/set", OBJ::NAME));
        Ok(res.1)
    }
}

#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "description")]
pub enum SetError {
    /// (create; update; destroy).  The create/update/destroy would violate an
    /// ACL or other permissions policy.
    Forbidden(Option<String>),
    /// (create; update).  The create would exceed a server- defined limit on
    /// the number or total size of objects of this type.
    OverQuota(Option<String>),

    /// (create; update).  The create/update would result in an object that
    /// exceeds a server-defined limit for the maximum size of a single object
    /// of this type.
    TooLarge(Option<String>),

    /// (create).  Too many objects of this type have been created recently, and
    /// a server-defined rate limit has been reached.  It may work if tried
    /// again later.
    RateLimit(Option<String>),

    /// (update; destroy).  The id given to update/destroy cannot be found.
    NotFound(Option<String>),

    /// (update).  The PatchObject given to update the record was not a valid
    /// patch (see the patch description).
    InvalidPatch(Option<String>),

    /// (update).  The client requested that an object be both updated and
    /// destroyed in the same /set request, and the server has decided to
    /// therefore ignore the update.
    WillDestroy(Option<String>),
    /// (create; update).  The record given is invalid in some way.
    InvalidProperties {
        description: Option<String>,
        properties: Vec<String>,
    },
    /// (create; destroy).  This is a singleton type, so you cannot create
    /// another one or destroy the existing one.
    Singleton(Option<String>),
    RequestTooLarge(Option<String>),
    StateMismatch(Option<String>),
}

impl core::fmt::Display for SetError {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        use SetError::*;
        match self {
            Forbidden(Some(description)) => write!(fmt, "Forbidden: {}", description),
            Forbidden(None) => write!(fmt, "Forbidden"),
            OverQuota(Some(description)) => write!(fmt, "OverQuota: {}", description),
            OverQuota(None) => write!(fmt, "OverQuota"),
            TooLarge(Some(description)) => write!(fmt, "TooLarge: {}", description),
            TooLarge(None) => write!(fmt, "TooLarge"),
            RateLimit(Some(description)) => write!(fmt, "RateLimit: {}", description),
            RateLimit(None) => write!(fmt, "RateLimit"),
            NotFound(Some(description)) => write!(fmt, "NotFound: {}", description),
            NotFound(None) => write!(fmt, "NotFound"),
            InvalidPatch(Some(description)) => write!(fmt, "InvalidPatch: {}", description),
            InvalidPatch(None) => write!(fmt, "InvalidPatch"),
            WillDestroy(Some(description)) => write!(fmt, "WillDestroy: {}", description),
            WillDestroy(None) => write!(fmt, "WillDestroy"),
            InvalidProperties {
                description: Some(description),
                properties,
            } => write!(
                fmt,
                "InvalidProperties: {}, {}",
                description,
                properties.join(",")
            ),
            InvalidProperties {
                description: None,
                properties,
            } => write!(fmt, "InvalidProperties: {}", properties.join(",")),
            Singleton(Some(description)) => write!(fmt, "Singleton: {}", description),
            Singleton(None) => write!(fmt, "Singleton"),
            RequestTooLarge(Some(description)) => write!(fmt, "RequestTooLarge: {}", description),
            RequestTooLarge(None) => write!(fmt, "RequestTooLarge"),
            StateMismatch(Some(description)) => write!(fmt, "StateMismatch: {}", description),
            StateMismatch(None) => write!(fmt, "StateMismatch"),
        }
    }
}

pub fn download_request_format(
    download_url: &str,
    account_id: &Id<Account>,
    blob_id: &Id<BlobObject>,
    name: Option<String>,
) -> String {
    // https://jmap.fastmail.com/download/{accountId}/{blobId}/{name}
    let mut ret = String::with_capacity(
        download_url.len()
            + blob_id.len()
            + name.as_ref().map(|n| n.len()).unwrap_or(0)
            + account_id.len(),
    );
    let mut prev_pos = 0;

    while let Some(pos) = download_url.as_bytes()[prev_pos..].find(b"{") {
        ret.push_str(&download_url[prev_pos..prev_pos + pos]);
        prev_pos += pos;
        if download_url[prev_pos..].starts_with("{accountId}") {
            ret.push_str(account_id.as_str());
            prev_pos += "{accountId}".len();
        } else if download_url[prev_pos..].starts_with("{blobId}") {
            ret.push_str(blob_id.as_str());
            prev_pos += "{blobId}".len();
        } else if download_url[prev_pos..].starts_with("{name}") {
            ret.push_str(name.as_deref().unwrap_or(""));
            prev_pos += "{name}".len();
        } else if download_url[prev_pos..].starts_with("{type}") {
            ret.push_str("application/octet-stream");
            prev_pos += "{name}".len();
        } else {
            // [ref:FIXME]: return protocol error here
            log::error!(
                "BUG: unknown parameter in download url: {}",
                &download_url[prev_pos..]
            );
            break;
        }
    }
    if prev_pos != download_url.len() {
        ret.push_str(&download_url[prev_pos..]);
    }
    ret
}

pub fn upload_request_format(upload_url: &str, account_id: &Id<Account>) -> String {
    //"uploadUrl": "https://jmap.fastmail.com/upload/{accountId}/",
    let mut ret = String::with_capacity(upload_url.len() + account_id.len());
    let mut prev_pos = 0;

    while let Some(pos) = upload_url.as_bytes()[prev_pos..].find(b"{") {
        ret.push_str(&upload_url[prev_pos..prev_pos + pos]);
        prev_pos += pos;
        if upload_url[prev_pos..].starts_with("{accountId}") {
            ret.push_str(account_id.as_str());
            prev_pos += "{accountId}".len();
            break;
        } else {
            ret.push('{');
            prev_pos += 1;
        }
    }
    if prev_pos != upload_url.len() {
        ret.push_str(&upload_url[prev_pos..]);
    }
    ret
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct UploadResponse {
    /// o  accountId: `Id`
    ///
    ///   The id of the account used for the call.
    pub account_id: Id<Account>,
    /// o  blobId: `Id`
    ///
    /// The id representing the binary data uploaded.  The data for this id is
    /// immutable. The id *only* refers to the binary data, not any
    /// metadata.
    pub blob_id: Id<BlobObject>,
    /// o  type: `String`
    ///
    /// The media type of the file (as specified in `RFC6838`,
    /// Section 4.2) as set in the Content-Type header of the upload HTTP
    /// request.

    #[serde(rename = "type")]
    pub _type: String,

    /// o  size: `UnsignedInt`
    ///
    /// The size of the file in octets.
    pub size: usize,
}

/// #`queryChanges`
///
///   The `Foo/queryChanges` method allows a client to efficiently update
///   the state of a cached query to match the new state on the server.  It
///   takes the following arguments:
#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct QueryChanges<F: FilterTrait<OBJ>, OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub account_id: Id<Account>,
    pub filter: Option<F>,
    pub sort: Option<Comparator<OBJ>>,
    /// sinceQueryState: `String`
    ///
    /// The current state of the query in the client.  This is the string
    /// that was returned as the `queryState` argument in the `Foo/query`
    /// response with the same sort/filter.  The server will return the
    /// changes made to the query since this state.
    pub since_query_state: String,
    /// o  maxChanges: `UnsignedInt|null`
    ///
    /// The maximum number of changes to return in the response.  See
    /// error descriptions below for more details.
    pub max_changes: Option<usize>,
    /// o  upToId: `Id|null`
    ///
    /// The last (highest-index) id the client currently has cached from
    /// the query results.  When there are a large number of results, in a
    /// common case, the client may have only downloaded and cached a
    /// small subset from the beginning of the results.  If the sort and
    /// filter are both only on immutable properties, this allows the
    /// server to omit changes after this point in the results, which can
    /// significantly increase efficiency.  If they are not immutable,
    /// this argument is ignored.
    pub up_to_id: Option<Id<OBJ>>,

    /// o  calculateTotal: `Boolean` (default: false)
    ///
    /// Does the client wish to know the total number of results now in
    /// the query?  This may be slow and expensive for servers to
    /// calculate, particularly with complex filters, so clients should
    /// take care to only request the total when needed.
    #[serde(default = "bool_false")]
    pub calculate_total: bool,

    #[serde(skip)]
    _ph: PhantomData<fn() -> OBJ>,
}

impl<F: FilterTrait<OBJ>, OBJ> QueryChanges<F, OBJ>
where
    OBJ: Object + std::fmt::Debug + Serialize,
{
    pub fn new(account_id: Id<Account>, since_query_state: String) -> Self {
        Self {
            account_id,
            filter: None,
            sort: None,
            since_query_state,
            max_changes: None,
            up_to_id: None,
            calculate_total: false,
            _ph: PhantomData,
        }
    }
    _impl!(filter: Option<F>);
    _impl!(sort: Option<Comparator<OBJ>>);
    _impl!(max_changes: Option<usize>);
    _impl!(up_to_id: Option<Id<OBJ>>);
    _impl!(calculate_total: bool);
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct QueryChangesResponse<OBJ: Object> {
    /// The id of the account used for the call.
    pub account_id: Id<Account>,
    /// This is the `sinceQueryState` argument echoed back; that is, the state
    /// from which the server is returning changes.
    pub old_query_state: String,
    /// This is the state the query will be in after applying the set of changes
    /// to the old state.
    pub new_query_state: String,
    /// The total number of Foos in the results (given the `filter`).  This
    /// argument MUST be omitted if the `calculateTotal` request argument is not
    /// true.
    #[serde(default)]
    pub total: Option<usize>,
    /// The `id` for every Foo that was in the query results in the old
    /// state and that is not in the results in the new state.

    /// If the server cannot calculate this exactly, the server MAY return
    /// the ids of extra Foos in addition that may have been in the old
    /// results but are not in the new results.

    /// If the sort and filter are both only on immutable properties and
    /// an `upToId` is supplied and exists in the results, any ids that
    /// were removed but have a higher index than `upToId` SHOULD be
    /// omitted.

    /// If the `filter` or `sort` includes a mutable property, the server
    /// MUST include all Foos in the current results for which this
    /// property may have changed.  The position of these may have moved
    /// in the results, so they must be reinserted by the client to ensure
    /// its query cache is correct.
    pub removed: Vec<Id<OBJ>>,
    /// The id and index in the query results (in the new state) for every
    /// Foo that has been added to the results since the old state AND
    /// every Foo in the current results that was included in the
    /// `removed` array (due to a filter or sort based upon a mutable
    /// property).

    /// If the sort and filter are both only on immutable properties and
    /// an `upToId` is supplied and exists in the results, any ids that
    /// were added but have a higher index than `upToId` SHOULD be
    /// omitted.

    /// The array MUST be sorted in order of index, with the lowest index
    /// first.

    /// An *AddedItem* object has the following properties:

    /// * id: `Id`

    /// * index: `UnsignedInt`

    /// The result of this is that if the client has a cached sparse array of
    /// Foo ids corresponding to the results in the old state, then:

    /// fooIds = [ `id1`, `id2`, null, null, `id3`, `id4`, null, null, null ]

    /// If it *splices out* all ids in the removed array that it has in its
    /// cached results, then:

    ///   removed = [ `id2`, `id31`, ... ];
    ///   fooIds => [ `id1`, null, null, `id3`, `id4`, null, null, null ]

    /// and *splices in* (one by one in order, starting with the lowest
    /// index) all of the ids in the added array:

    /// added = [{ id: `id5`, index: 0, ... }];
    /// fooIds => [ `id5`, `id1`, null, null, `id3`, `id4`, null, null, null ]

    /// and *truncates* or *extends* to the new total length, then the
    /// results will now be in the new state.

    /// Note: splicing in adds the item at the given index, incrementing the
    /// index of all items previously at that or a higher index.  Splicing
    /// out is the inverse, removing the item and decrementing the index of
    /// every item after it in the array.
    pub added: Vec<AddedItem<OBJ>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AddedItem<OBJ: Object> {
    pub id: Id<OBJ>,
    pub index: usize,
}
