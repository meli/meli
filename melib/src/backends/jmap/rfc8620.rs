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
use core::marker::PhantomData;
use serde::{de::DeserializeOwned, Serialize};
use serde_json::Value;

mod filters;
pub use filters::*;
mod comparator;
pub use comparator::*;

use super::protocol::Method;
use std::collections::HashMap;
pub trait Object {}

// 5.1.  /get
//
//    Objects of type Foo are fetched via a call to "Foo/get".
//
//    It takes the following arguments:
//
//    o  accountId: "Id"
//
//       The id of the account to use.
//
//    o  ids: "Id[]|null"
//
//       The ids of the Foo objects to return.  If null, then *all* records
//       of the data type are returned, if this is supported for that data
//       type and the number of records does not exceed the
//       "maxObjectsInGet" limit.
//
//    o  properties: "String[]|null"
//
//       If supplied, only the properties listed in the array are returned
//       for each Foo object.  If null, all properties of the object are
//       returned.  The id property of the object is *always* returned,
//       even if not explicitly requested.  If an invalid property is
//       requested, the call MUST be rejected with an "invalidArguments"
//       error.

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct JmapSession {
    capabilities: HashMap<String, CapabilitiesObject>,
    accounts: HashMap<Id, Account>,
    primary_accounts: Vec<Id>,
    username: String,
    api_url: String,
    download_url: String,

    upload_url: String,
    event_source_url: String,
    state: String,
    #[serde(flatten)]
    extra_properties: HashMap<String, Value>,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CapabilitiesObject {
    max_size_upload: u64,
    max_concurrent_upload: u64,
    max_size_request: u64,
    max_concurrent_requests: u64,
    max_calls_in_request: u64,
    max_objects_in_get: u64,
    max_objects_in_set: u64,
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

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct GetCall<OBJ: Object>
where
    OBJ: std::fmt::Debug + Serialize,
{
    #[serde(skip_serializing_if = "String::is_empty")]
    pub account_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<String>>,
    _ph: PhantomData<*const OBJ>,
}

impl<OBJ: Object> GetCall<OBJ>
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
    _impl!(account_id: String);
    _impl!(ids: Option<Vec<String>>);
    _impl!(properties: Option<Vec<String>>);
}
//   The response has the following arguments:
//
//   o  accountId: "Id"
//
//      The id of the account used for the call.
//
//   o  state: "String"
//
//      A (preferably short) string representing the state on the server
//      for *all* the data of this type in the account (not just the
//      objects returned in this call).  If the data changes, this string
//      MUST change.  If the Foo data is unchanged, servers SHOULD return
//      the same state string on subsequent requests for this data type.
//      When a client receives a response with a different state string to
//      a previous call, it MUST either throw away all currently cached
//      objects for the type or call "Foo/changes" to get the exact
//      changes.
//
//   o  list: "Foo[]"
//
//      An array of the Foo objects requested.  This is the *empty array*
//      if no objects were found or if the "ids" argument passed in was
//      also an empty array.  The results MAY be in a different order to
//      the "ids" in the request arguments.  If an identical id is
//      included more than once in the request, the server MUST only
//      include it once in either the "list" or the "notFound" argument of
//      the response.
//
//   o  notFound: "Id[]"
//
//      This array contains the ids passed to the method for records that
//      do not exist.  The array is empty if all requested ids were found
//      or if the "ids" argument passed in was either null or an empty
//      array.
//
//   The following additional error may be returned instead of the "Foo/
//   get" response:
//
//   "requestTooLarge": The number of ids requested by the client exceeds
//   the maximum number the server is willing to process in a single
//   method call.

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct GetResponse<T> {
    #[serde(skip_serializing_if = "String::is_empty")]
    account_id: String,
    state: String,
    list: Vec<T>,
    not_found: Vec<String>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum JmapError {
    RequestTooLarge,
    InvalidArguments,
    InvalidResultReference,
}

// 5.5.  /query
//
//    For data sets where the total amount of data is expected to be very
//    small, clients can just fetch the complete set of data and then do
//    any sorting/filtering locally.  However, for large data sets (e.g.,
//    multi-gigabyte mailboxes), the client needs to be able to
//    search/sort/window the data type on the server.
//
//    A query on the set of Foos in an account is made by calling "Foo/
//    query".  This takes a number of arguments to determine which records
//    to include, how they should be sorted, and which part of the result
//    should be returned (the full list may be *very* long).  The result is
//    returned as a list of Foo ids.
//
//    A call to "Foo/query" takes the following arguments:
//
//    o  accountId: "Id"
//
//       The id of the account to use.
//
//    o  filter: "FilterOperator|FilterCondition|null"
//
//       Determines the set of Foos returned in the results.  If null, all
//       objects in the account of this type are included in the results.
//       A *FilterOperator* object has the following properties:
//
//       *  operator: "String"
//
//          This MUST be one of the following strings:
//
//          +  "AND": All of the conditions must match for the filter to
//             match.
//
//          +  "OR": At least one of the conditions must match for the
//             filter to match.
//
//          +  "NOT": None of the conditions must match for the filter to
//             match.
//
//       *  conditions: "(FilterOperator|FilterCondition)[]"
//
//          The conditions to evaluate against each record.
//
//       A *FilterCondition* is an "object" whose allowed properties and
//       semantics depend on the data type and is defined in the /query
//       method specification for that type.  It MUST NOT have an
//       "operator" property.
//
//    o  sort: "Comparator[]|null"
//
//       Lists the names of properties to compare between two Foo records,
//       and how to compare them, to determine which comes first in the
//       sort.  If two Foo records have an identical value for the first
//       comparator, the next comparator will be considered, and so on.  If
//       all comparators are the same (this includes the case where an
//       empty array or null is given as the "sort" argument), the sort
//       order is server dependent, but it MUST be stable between calls to
//       "Foo/query".  A *Comparator* has the following properties:
//
//       *  property: "String"
//
//          The name of the property on the Foo objects to compare.
//
//       *  isAscending: "Boolean" (optional; default: true)
//
//          If true, sort in ascending order.  If false, reverse the
//          comparator's results to sort in descending order.
//
//       *  collation: "String" (optional; default is server-dependent)
//
//          The identifier, as registered in the collation registry defined
//          in [RFC4790], for the algorithm to use when comparing the order
//          of strings.  The algorithms the server supports are advertised
//          in the capabilities object returned with the Session object
//          (see Section 2).
//
//          If omitted, the default algorithm is server dependent, but:
//
//          1.  It MUST be unicode-aware.
//
//          2.  It MAY be selected based on an Accept-Language header in
//              the request (as defined in [RFC7231], Section 5.3.5) or
//              out-of-band information about the user's language/locale.
//
//          3.  It SHOULD be case insensitive where such a concept makes
//              sense for a language/locale.  Where the user's language is
//              unknown, it is RECOMMENDED to follow the advice in
//              Section 5.2.3 of [RFC8264].
//
//          The "i;unicode-casemap" collation [RFC5051] and the Unicode
//          Collation Algorithm (<http://www.unicode.org/reports/tr10/>)
//          are two examples that fulfil these criterion and provide
//          reasonable behaviour for a large number of languages.
//
//          When the property being compared is not a string, the
//          "collation" property is ignored, and the following comparison
//          rules apply based on the type.  In ascending order:
//
//          +  "Boolean": false comes before true.
//
//          +  "Number": A lower number comes before a higher number.
//
//          +  "Date"/"UTCDate": The earlier date comes first.
//
//       The Comparator object may also have additional properties as
//       required for specific sort operations defined in a type's /query
//       method.
//
//    o  position: "Int" (default: 0)
//
//       The zero-based index of the first id in the full list of results
//       to return.
//
//       If a negative value is given, it is an offset from the end of the
//       list.  Specifically, the negative value MUST be added to the total
//       number of results given the filter, and if still negative, it's
//       clamped to "0".  This is now the zero-based index of the first id
//       to return.
//
//       If the index is greater than or equal to the total number of
//       objects in the results list, then the "ids" array in the response
//       will be empty, but this is not an error.
//
//    o  anchor: "Id|null"
//
//       A Foo id.  If supplied, the "position" argument is ignored.  The
//       index of this id in the results will be used in combination with
//       the "anchorOffset" argument to determine the index of the first
//       result to return (see below for more details).
//
//    o  anchorOffset: "Int" (default: 0)
//
//       The index of the first result to return relative to the index of
//       the anchor, if an anchor is given.  This MAY be negative.  For
//       example, "-1" means the Foo immediately preceding the anchor is
//       the first result in the list returned (see below for more
//       details).
//
//    o  limit: "UnsignedInt|null"
//
//       The maximum number of results to return.  If null, no limit
//       presumed.  The server MAY choose to enforce a maximum "limit"
//       argument.  In this case, if a greater value is given (or if it is
//       null), the limit is clamped to the maximum; the new limit is
//       returned with the response so the client is aware.  If a negative
//       value is given, the call MUST be rejected with an
//       "invalidArguments" error.
//
//    o  calculateTotal: "Boolean" (default: false)
//
//       Does the client wish to know the total number of results in the
//       query?  This may be slow and expensive for servers to calculate,
//       particularly with complex filters, so clients should take care to
//       only request the total when needed.
//
//    If an "anchor" argument is given, the anchor is looked for in the
//    results after filtering and sorting.  If found, the "anchorOffset" is
//    then added to its index.  If the resulting index is now negative, it
//    is clamped to 0.  This index is now used exactly as though it were
//    supplied as the "position" argument.  If the anchor is not found, the
//    call is rejected with an "anchorNotFound" error.
//
//    If an "anchor" is specified, any position argument supplied by the
//    client MUST be ignored.  If no "anchor" is supplied, any
//    "anchorOffset" argument MUST be ignored.
//
//    A client can use "anchor" instead of "position" to find the index of
//    an id within a large set of results.

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct QueryCall<F: FilterTrait<OBJ>, OBJ: Object>
where
    OBJ: std::fmt::Debug + Serialize,
{
    account_id: String,
    filter: Option<Filter<F, OBJ>>,
    sort: Option<Comparator<OBJ>>,
    #[serde(default)]
    position: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    anchor: Option<String>,
    #[serde(default)]
    anchor_offset: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    limit: Option<u64>,
    #[serde(default = "bool_false")]
    calculate_total: bool,
    _ph: PhantomData<*const OBJ>,
}

impl<F: FilterTrait<OBJ>, OBJ: Object> QueryCall<F, OBJ>
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
    _impl!(filter: Option<Filter<F, OBJ>>);
    _impl!(sort: Option<Comparator<OBJ>>);
    _impl!(position: u64);
    _impl!(anchor: Option<String>);
    _impl!(anchor_offset: u64);
    _impl!(limit: Option<u64>);
    _impl!(calculate_total: bool);
}

pub fn bool_false() -> bool {
    false
}

pub fn bool_true() -> bool {
    true
}

//    The response has the following arguments:
//
//    o  accountId: "Id"
//
//       The id of the account used for the call.
//
//    o  queryState: "String"
//
//       A string encoding the current state of the query on the server.
//       This string MUST change if the results of the query (i.e., the
//       matching ids and their sort order) have changed.  The queryState
//       string MAY change if something has changed on the server, which
//       means the results may have changed but the server doesn't know for
//       sure.
//
//       The queryState string only represents the ordered list of ids that
//       match the particular query (including its sort/filter).  There is
//       no requirement for it to change if a property on an object
//       matching the query changes but the query results are unaffected
//       (indeed, it is more efficient if the queryState string does not
//       change in this case).  The queryState string only has meaning when
//       compared to future responses to a query with the same type/sort/
//       filter or when used with /queryChanges to fetch changes.
//
//       Should a client receive back a response with a different
//       queryState string to a previous call, it MUST either throw away
//       the currently cached query and fetch it again (note, this does not
//       require fetching the records again, just the list of ids) or call
//       "Foo/queryChanges" to get the difference.
//
//    o  canCalculateChanges: "Boolean"
//
//       This is true if the server supports calling "Foo/queryChanges"
//       with these "filter"/"sort" parameters.  Note, this does not
//       guarantee that the "Foo/queryChanges" call will succeed, as it may
//       only be possible for a limited time afterwards due to server
//       internal implementation details.
//
//    o  position: "UnsignedInt"
//
//       The zero-based index of the first result in the "ids" array within
//       the complete list of query results.
//
//    o  ids: "Id[]"
//
//       The list of ids for each Foo in the query results, starting at the
//       index given by the "position" argument of this response and
//       continuing until it hits the end of the results or reaches the
//       "limit" number of ids.  If "position" is >= "total", this MUST be
//       the empty list.
//
//    o  total: "UnsignedInt" (only if requested)
//
//       The total number of Foos in the results (given the "filter").
//       This argument MUST be omitted if the "calculateTotal" request
//       argument is not true.
//
//    o  limit: "UnsignedInt" (if set by the server)
//
//       The limit enforced by the server on the maximum number of results
//       to return.  This is only returned if the server set a limit or
//       used a different limit than that given in the request.
//
//    The following additional errors may be returned instead of the "Foo/
//    query" response:
//
//    "anchorNotFound": An anchor argument was supplied, but it cannot be
//    found in the results of the query.
//
//    "unsupportedSort": The "sort" is syntactically valid, but it includes
//    a property the server does not support sorting on or a collation
//    method it does not recognise.
//
//    "unsupportedFilter": The "filter" is syntactically valid, but the
//    server cannot process it.  If the filter was the result of a user's
//    search input, the client SHOULD suggest that the user simplify their
//    search.
