//
// meli
//
// Copyright 2026  Manos Pitsidianakis
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::sync::Arc;

use futures::lock::Mutex as FutureMutex;
use isahc::AsyncReadResponseExt as _;

use crate::{
    backends::prelude::*,
    contacts::{
        backend::{ContactBackend, ContactBackendCapabilities},
        jscontact::{JSContact, JSContactVersion1},
        AddressBook, AddressBookName, Card,
    },
    jmap::{
        argument::Argument,
        deserialize_from_str,
        filters::{Filter, FilterTrait},
        methods::{
            Get, GetResponse, MethodResponse, Query, QueryResponse, ResultField, Set, SetResponse,
        },
        objects::{Account, Id, Object},
        protocol::{Method, Request, UtcDate, USING_CONTACTS as USING},
        JmapConnection, JmapServerConf, Store,
    },
    utils::futures::timeout,
};

#[derive(Debug)]
pub struct JmapContacts {
    pub capabilities: ContactsCapabilitiesObject,
    pub account_id: Id<Account>,
    pub server_conf: JmapServerConf,
    pub connection: Arc<FutureMutex<JmapConnection>>,
    pub store: Arc<Store>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContactsCapabilitiesObject {
    #[serde(default)]
    pub may_create_address_book: bool,
    #[serde(default)]
    pub max_address_books_per_card: Option<u64>,
}

impl ContactBackend for JmapContacts {
    fn capabilities(&mut self) -> ContactBackendCapabilities {
        let can_create_address_book = self.capabilities.may_create_address_book;
        let max_address_books_per_card = self.capabilities.max_address_books_per_card;
        ContactBackendCapabilities {
            is_async: true,
            is_remote: true,
            supports_search: true,
            can_create_address_book,
            max_address_books_per_card,
            ..Default::default()
        }
    }

    fn is_online(&mut self) -> ResultFuture<()> {
        let online = self.store.online_status.clone();
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout;
        Ok(Box::pin(async move {
            let _conn = timeout(timeout_dur, connection.lock()).await?;
            let _session = timeout(timeout_dur, online.session_guard()).await??;
            Ok(())
        }))
    }

    fn address_books(&mut self) -> crate::prelude::ResultFuture<Vec<AddressBookName>> {
        let store = self.store.clone();
        let account_id = self.account_id.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            let client = conn.client().await?;
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            let get_call = AddressBookGet::new(Get::new().account_id(account_id));

            req.add_call(&get_call);

            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse { list, .. } =
                GetResponse::<AddressBookObject>::try_from(v.method_responses.remove(0))?;
            store.online_status.update_timestamp(None).await;
            Ok(list
                .into_iter()
                .map(|b| AddressBookName(b.name.into()))
                .collect())
        }))
    }

    fn fetch_book(
        &mut self,
        address_book: &AddressBookName,
    ) -> crate::prelude::ResultFuture<AddressBook> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        let account_id = self.account_id.clone();
        let address_book = address_book.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            let client = conn.client().await?;
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            let get_call = AddressBookGet::new(Get::new().account_id(account_id.clone()));

            req.add_call(&get_call);

            // [ref:FIXME]: make one roundtrip with a Query request instead of two
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse { list, .. } =
                GetResponse::<AddressBookObject>::try_from(v.method_responses.remove(0))?;
            store.online_status.update_timestamp(None).await;
            let Some(book) = list.iter().find(|b| b.name == address_book.deref()) else {
                return Err(Error::new(format!(
                    "Address book {} not found. Available address books: {:?}",
                    address_book.deref(),
                    list.into_iter().map(|b| b.name).collect::<Vec<_>>()
                ))
                .set_kind(ErrorKind::ValueError));
            };
            let get_call: ContactCardGet = ContactCardGet::new(Get::new().account_id(account_id));
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            req.add_call(&get_call);
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse { list, .. } =
                GetResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;
            let cards = list
                .into_iter()
                .map(|obj| {
                    let card: Card = obj.jscontact;
                    (card.id(), card)
                })
                .collect::<IndexMap<_, _>>();
            Ok(AddressBook {
                name: address_book,
                read_only: !book.my_rights.may_write,
                cards,
            })
        }))
    }

    fn search(
        &self,
        _term: &str,
        _address_book: Option<&AddressBookName>,
    ) -> ResultFuture<Vec<Card>> {
        Err(Error::new("").set_kind(ErrorKind::NotImplemented))
    }

    fn add(&mut self, address_book: &AddressBookName, new_card: Card) -> ResultFuture<Card> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        let account_id = self.account_id.clone();
        let address_book = address_book.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            let client = conn.client().await?;
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            let get_call = AddressBookGet::new(Get::new().account_id(account_id.clone()));

            req.add_call(&get_call);

            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse { list, .. } =
                GetResponse::<AddressBookObject>::try_from(v.method_responses.remove(0))?;
            store.online_status.update_timestamp(None).await;
            let Some(book) = list.iter().find(|b| b.name == address_book.deref()) else {
                return Err(Error::new(format!(
                    "Address book {} not found. Available address books: {:?}",
                    address_book.deref(),
                    list.into_iter().map(|b| b.name).collect::<Vec<_>>()
                ))
                .set_kind(ErrorKind::ValueError));
            };
            let mut new_obj: ContactCardObject = new_card.into();
            new_obj.address_book_ids.insert(book.id.clone(), true);
            let create_id = new_obj.id.clone();
            let set_call: ContactCardSet = ContactCardSet::new(
                Set::new(/* TODO */ None)
                    .account_id(account_id.clone())
                    .create(Some(
                        indexmap::indexmap! { Argument::Value(create_id.clone()) => new_obj.clone() },
                    )),
            );
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            req.add_call(&set_call);
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let set_resp =
                SetResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;
            let SetResponse {
                not_created,
                created,
                ..
            } = set_resp;
            let not_created = not_created.unwrap_or_default();
            if !not_created.is_empty() {
                return Err(Error::new(format!("Could not create: {not_created:?}")));
            }
            let created_card = created
                .unwrap_or_default()
                .swap_remove(&create_id)
                .ok_or_else(|| {
                    Error::new("Could not create contact card")
                        .set_details("ContactCard/Set server response had an empty `created` field")
                        .set_kind(ErrorKind::Bug)
                })?;
            new_obj.id = serde_json::from_value(created_card["id"].clone()).unwrap();
            if let Some(created) = created_card.get("created").cloned() {
                new_obj.jscontact.0.created = serde_json::from_value(created).unwrap();
            }
            let card: Card = new_obj.jscontact;
            Ok(card)
        }))
    }

    fn edit(&mut self, address_book: &AddressBookName, card: Card) -> ResultFuture<Card> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        let account_id = self.account_id.clone();
        let address_book = address_book.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            let client = conn.client().await?;
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            let get_call = AddressBookGet::new(Get::new().account_id(account_id.clone()));

            req.add_call(&get_call);

            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse { list, .. } =
                GetResponse::<AddressBookObject>::try_from(v.method_responses.remove(0))?;
            store.online_status.update_timestamp(None).await;
            let Some(book) = list.iter().find(|b| b.name == address_book.deref()) else {
                return Err(Error::new(format!(
                    "Address book {} not found. Available address books: {:?}",
                    address_book.deref(),
                    list.into_iter().map(|b| b.name).collect::<Vec<_>>()
                ))
                .set_kind(ErrorKind::ValueError));
            };
            let uid = card.0.uid;
            let mut obj: ContactCardObject = card.into();
            obj.address_book_ids.insert(book.id.clone(), true);
            let mut req = Request::new_with_using(client.request_no.clone(), USING);

            req.add_call(&ContactCardQuery::new(
                Query::new()
                    .account_id(account_id.clone())
                    .filter(Some(Filter::Condition(
                        ContactCardFilterCondition::new().uid(uid.hyphenated().to_string()),
                    )))
                    .position(0)
                    .limit(None),
            ));
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let QueryResponse { ids, .. } =
                QueryResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;
            let Some(card_id) = ids.first().cloned() else {
                return Err(
                    Error::new(format!("Contact card {uid} not found on server"))
                        .set_kind(ErrorKind::ValueError),
                );
            };
            let mut req = Request::new_with_using(client.request_no.clone(), USING);

            let mut updated_properties = serde_json::json!(obj.clone());
            if let Some(map) = updated_properties.as_object_mut() {
                map.remove("uid");
                map.remove("addressBookIds");
                map.remove("id");
            }
            req.add_call(&ContactCardSet::new(
                Set::new(/* TODO */ None)
                    .account_id(account_id.clone())
                    .update(Some(
                        indexmap::indexmap! { Argument::value(card_id.clone()) => updated_properties },
                    )),
            ));
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let set_resp =
                SetResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;
            let SetResponse {
                not_updated,
                updated,
                ..
            } = set_resp;
            let not_updated = not_updated.unwrap_or_default();
            if !not_updated.is_empty() {
                return Err(Error::new(format!(
                    "Could not update contact card: {not_updated:?}",
                )));
            }
            if let Some(mut updated_card) = updated
                .unwrap_or_default()
                .swap_remove(&card_id)
                .ok_or_else(|| {
                    Error::new("Could not update contact card")
                        .set_details("ContactCard/Set server response had an empty `updated` field")
                        .set_kind(ErrorKind::Bug)
                })?
            {
                let updated_card = updated_card.as_object_mut().expect("map");
                if updated_card.contains_key("id") {
                    obj.id = serde_json::from_value(updated_card["id"].clone()).unwrap();
                }
                if let Some(uid) = updated_card.get("uid").cloned() {
                    obj.jscontact.0.uid = serde_json::from_value(uid).unwrap();
                }
                if let Some(updated) = updated_card.get("updated").cloned() {
                    obj.jscontact.0.updated = serde_json::from_value(updated).unwrap();
                }
            }
            let card = obj.jscontact;
            Ok(card)
        }))
    }

    fn delete(&mut self, address_book: &AddressBookName, card: Card) -> ResultFuture<()> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        let account_id = self.account_id.clone();
        let address_book = address_book.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            let client = conn.client().await?;
            let mut req = Request::new_with_using(client.request_no.clone(), USING);
            let get_call = AddressBookGet::new(Get::new().account_id(account_id.clone()));

            req.add_call(&get_call);

            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse { list, .. } =
                GetResponse::<AddressBookObject>::try_from(v.method_responses.remove(0))?;
            store.online_status.update_timestamp(None).await;
            let Some(book) = list.iter().find(|b| b.name == address_book.deref()) else {
                return Err(Error::new(format!(
                    "Address book {} not found. Available address books: {:?}",
                    address_book.deref(),
                    list.into_iter().map(|b| b.name).collect::<Vec<_>>()
                ))
                .set_kind(ErrorKind::ValueError));
            };
            let uid = card.0.uid;
            let mut req = Request::new_with_using(client.request_no.clone(), USING);

            let query_call = req.add_call(&ContactCardQuery::new(
                Query::new()
                    .account_id(account_id.clone())
                    .filter(Some(Filter::Condition(
                        ContactCardFilterCondition::new().uid(uid.hyphenated().to_string()),
                    )))
                    .position(0)
                    .limit(None),
            ));
            req.add_call(&ContactCardGet::new(
                Get::new()
                    .account_id(account_id.clone())
                    .ids(Some(Argument::reference::<
                        ContactCardQuery,
                        ContactCardObject,
                        ContactCardObject,
                    >(
                        query_call,
                        ResultField::<ContactCardQuery, ContactCardObject>::new("/ids"),
                    ))),
            ));
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let QueryResponse { ids, .. } =
                QueryResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;
            if ids.is_empty() {
                return Err(
                    Error::new(format!("Contact card {uid} not found on server"))
                        .set_kind(ErrorKind::ValueError),
                );
            }
            let GetResponse { list, .. } =
                GetResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;

            let Some(mut obj) = list.first().cloned() else {
                return Err(
                    Error::new(format!("Contact card {uid} not found on server"))
                        .set_kind(ErrorKind::ValueError),
                );
            };
            let mut req = Request::new_with_using(client.request_no.clone(), USING);

            let card_id = obj.id.clone();
            obj.address_book_ids.shift_remove(&book.id);
            let destroy = obj.address_book_ids.is_empty();
            if destroy {
                req.add_call(&ContactCardSet::new(
                    Set::new(/* TODO */ None)
                        .account_id(account_id.clone())
                        .destroy(Some(vec![Argument::value(card_id.clone())])),
                ));
            } else {
                let mut updated_properties = serde_json::json!(obj.clone());
                if let Some(map) = updated_properties.as_object_mut() {
                    map.remove("uid");
                    map.remove("id");
                }
                req.add_call(&ContactCardSet::new(
                        Set::new(/* TODO */ None)
                        .account_id(account_id.clone())
                        .update(Some(
                                indexmap::indexmap! { Argument::value(card_id.clone()) => updated_properties },
                        )),
                ));
            }
            let res_text = client
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = client.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let set_resp =
                SetResponse::<ContactCardObject>::try_from(v.method_responses.remove(0))?;
            let SetResponse {
                not_updated,
                not_destroyed,
                ..
            } = set_resp;
            if destroy {
                let not_destroyed = not_destroyed.unwrap_or_default();
                if !not_destroyed.is_empty() {
                    return Err(Error::new(format!(
                        "Could not delete contact card: {not_destroyed:?}",
                    )));
                }
            } else {
                let not_updated = not_updated.unwrap_or_default();
                if !not_updated.is_empty() {
                    return Err(Error::new(format!(
                        "Could not delete contact card: {not_updated:?}",
                    )));
                }
            }
            Ok(())
        }))
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AddressBookGet {
    #[serde(flatten)]
    pub get_call: Get<AddressBookObject>,
}

impl Method<AddressBookObject> for AddressBookGet {
    const NAME: &'static str = "AddressBook/get";
}

impl AddressBookGet {
    pub fn new(get_call: Get<AddressBookObject>) -> Self {
        Self { get_call }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AddressBookObject {
    #[serde(default)]
    pub id: Id<Self>,
    pub name: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub sort_order: u64,
    pub is_default: bool,
    pub is_subscribed: bool,
    #[serde(skip)]
    pub share_with: (),
    pub my_rights: AddressBookRights,
}

impl Object for AddressBookObject {
    const NAME: &'static str = "AddressBook";
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContactCardObject {
    #[serde(default)]
    pub id: Id<Self>,
    #[serde(default)]
    pub address_book_ids: IndexMap<Id<AddressBookObject>, bool>,
    #[serde(flatten)]
    pub jscontact: JSContact<JSContactVersion1>,
}

impl Object for ContactCardObject {
    const NAME: &'static str = "ContactCard";
}

impl From<Card> for ContactCardObject {
    fn from(jscontact: Card) -> Self {
        Self {
            id: Id::new_random(),
            address_book_ids: IndexMap::default(),
            jscontact,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContactCardGet {
    #[serde(flatten)]
    pub get_call: Get<ContactCardObject>,
}

impl Method<ContactCardObject> for ContactCardGet {
    const NAME: &'static str = "ContactCard/get";
}

impl ContactCardGet {
    pub fn new(get_call: Get<ContactCardObject>) -> Self {
        Self { get_call }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContactCardSet {
    #[serde(flatten)]
    pub set_call: Set<ContactCardObject>,
}

impl Method<ContactCardObject> for ContactCardSet {
    const NAME: &'static str = "ContactCard/set";
}

impl ContactCardSet {
    pub fn new(set_call: Set<ContactCardObject>) -> Self {
        Self { set_call }
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContactCardFilterCondition {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub in_address_book: Option<Id<AddressBookObject>>,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub uid: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub has_member: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub kind: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub created_before: UtcDate,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub created_after: UtcDate,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub updated_before: UtcDate,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub updated_after: UtcDate,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub text: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub name: String,
    #[serde(
        rename = "name/given",
        skip_serializing_if = "String::is_empty",
        default
    )]
    pub name_given: String,
    #[serde(
        rename = "name/surname",
        skip_serializing_if = "String::is_empty",
        default
    )]
    pub name_surname: String,
    #[serde(
        rename = "name/surname2",
        skip_serializing_if = "String::is_empty",
        default
    )]
    pub name_surname2: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub nickname: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub organization: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub email: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub phone: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub online_service: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub address: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub note: String,
}

impl ContactCardFilterCondition {
    pub fn new() -> Self {
        Self::default()
    }

    _impl!(in_address_book: Option<Id<AddressBookObject>>);
    _impl!(uid: String);
    _impl!(has_member: String);
    _impl!(kind: String);
    _impl!(created_before: UtcDate);
    _impl!(created_after: UtcDate);
    _impl!(updated_before: UtcDate);
    _impl!(updated_after: UtcDate);
    _impl!(text: String);
    _impl!(name: String);
    _impl!(name_given: String);
    _impl!(name_surname: String);
    _impl!(name_surname2: String);
    _impl!(nickname: String);
    _impl!(organization: String);
    _impl!(email: String);
    _impl!(phone: String);
    _impl!(online_service: String);
    _impl!(address: String);
    _impl!(note: String);
}

impl FilterTrait<ContactCardObject> for ContactCardFilterCondition {}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContactCardQuery {
    #[serde(flatten)]
    pub query_call: Query<Filter<ContactCardFilterCondition, ContactCardObject>, ContactCardObject>,
}

impl Method<ContactCardObject> for ContactCardQuery {
    const NAME: &'static str = "ContactCard/query";
}

impl ContactCardQuery {
    pub fn new(
        query_call: Query<Filter<ContactCardFilterCondition, ContactCardObject>, ContactCardObject>,
    ) -> Self {
        Self { query_call }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AddressBookRights {
    pub may_read: bool,
    pub may_write: bool,
    pub may_share: bool,
    pub may_delete: bool,
}
