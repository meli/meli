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
    collections::HashMap,
    convert::TryFrom,
    sync::{Arc, Mutex},
};

use isahc::AsyncReadResponseExt;
use serde_json::value::RawValue;
use smallvec::SmallVec;

use crate::{
    error::{Error, Result},
    jmap::{
        argument::Argument,
        deserialize_from_str,
        methods::{Changes, ChangesResponse, Get, GetResponse, MethodResponse, ResultField, Set},
        objects::{Id, Object, State},
        protocol::{Method, Request},
        JmapClient, JmapMailbox,
    },
    BackendEvent, LazyCountSet, MailboxHash, RefreshEvent, RefreshEventKind,
};

impl Id<MailboxObject> {
    pub fn into_hash(&self) -> MailboxHash {
        MailboxHash::from_bytes(self.inner.as_bytes())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MailboxObject {
    pub id: Id<Self>,
    pub is_subscribed: bool,
    pub my_rights: JmapRights,
    pub name: String,
    pub parent_id: Option<Id<Self>>,
    pub role: Option<String>,
    pub sort_order: u64,
    pub total_emails: u64,
    pub total_threads: u64,
    pub unread_emails: u64,
    pub unread_threads: u64,
}

impl Object for MailboxObject {
    const NAME: &'static str = "Mailbox";
    const SERVER_SET_FIELDS: &'static [&'static str] = &[
        "id",
        "totalEmails",
        "unreadEmails",
        "unreadThreads",
        "totalThreads",
        "myRights",
    ];
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct JmapRights {
    pub may_add_items: bool,
    pub may_create_child: bool,
    pub may_delete: bool,
    pub may_read_items: bool,
    pub may_remove_items: bool,
    pub may_rename: bool,
    pub may_set_keywords: bool,
    pub may_set_seen: bool,
    pub may_submit: bool,
}

impl Default for JmapRights {
    fn default() -> Self {
        Self {
            may_add_items: true,
            may_create_child: true,
            may_delete: true,
            may_read_items: true,
            may_remove_items: true,
            may_rename: true,
            may_set_keywords: true,
            may_set_seen: true,
            may_submit: true,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MailboxGet {
    #[serde(flatten)]
    pub get_call: Get<MailboxObject>,
}

impl MailboxGet {
    pub fn new(get_call: Get<MailboxObject>) -> Self {
        Self { get_call }
    }
}

impl Method<MailboxObject> for MailboxGet {
    const NAME: &'static str = "Mailbox/get";
}

/// 2.5.  Mailbox/set
///
/// This is a standard `/set` method as described in `[RFC8620]`,
/// Section 5.3 but with the following additional request argument:
///
///
/// The following extra [`crate::jmap::methods::SetError`] types are defined:
///
/// For `destroy`:
///
/// - `mailboxHasChild`: The [`Mailbox`](crate::jmap::mailbox::MailboxObject)
///   still has at least one child
///   [`Mailbox`](crate::jmap::mailbox::MailboxObject).  The client MUST remove
///   these before it can delete the parent
///   [`Mailbox`](crate::jmap::mailbox::MailboxObject).
///
/// - `mailboxHasEmail`: The [`Mailbox`](crate::jmap::mailbox::MailboxObject)
///   has at least one [`Email`](crate::jmap::email::EmailObject) assigned to
///   it, and the `onDestroyRemoveEmails` argument was false.
#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MailboxSet {
    #[serde(flatten)]
    pub set_call: Set<MailboxObject>,
    /// onDestroyRemoveEmails: `Boolean` (default: false)
    ///
    /// If false, any attempt to destroy a
    /// [`Mailbox`](crate::jmap::mailbox::MailboxObject) that still
    /// has [`Email`s](crate::jmap::email::EmailObject) in it will be rejected
    /// with a `mailboxHasEmail` [`crate::jmap::methods::SetError`].  If
    /// true, any [`Email`s](crate::jmap::email::EmailObject) that were in the
    /// [`Mailbox`](crate::jmap::mailbox::MailboxObject) will be removed from
    /// it, and if in no
    /// other [`Mailbox`es](crate::jmap::mailbox::MailboxObject), they will be
    /// destroyed when the [`Mailbox`](crate::jmap::mailbox::MailboxObject)
    /// is destroyed.
    #[serde(default, skip_serializing_if = "is_false")]
    pub on_destroy_remove_emails: bool,
}

const fn is_false(v: &bool) -> bool {
    !*v
}

impl MailboxSet {
    pub fn new(set_call: Set<MailboxObject>) -> Self {
        Self {
            set_call,
            on_destroy_remove_emails: false,
        }
    }

    _impl!(on_destroy_remove_emails: bool);
}

impl Method<MailboxObject> for MailboxSet {
    const NAME: &'static str = "Mailbox/set";
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MailboxChangesResponse {
    #[serde(flatten)]
    pub changes_response: ChangesResponse<MailboxObject>,
    #[serde(default)]
    pub updated_properties: Option<Vec<String>>,
}

impl std::convert::TryFrom<&RawValue> for MailboxChangesResponse {
    type Error = Error;

    fn try_from(t: &RawValue) -> Result<Self> {
        let res: (String, Self, String) = deserialize_from_str(t.get())?;
        assert_eq!(&res.0, "Mailbox/changes");
        Ok(res.1)
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MailboxChanges {
    #[serde(flatten)]
    pub changes_call: Changes<MailboxObject>,
}

impl MailboxChanges {
    pub fn new(changes_call: Changes<MailboxObject>) -> Self {
        Self { changes_call }
    }
}

impl Method<MailboxObject> for MailboxChanges {
    const NAME: &'static str = "Mailbox/changes";
}

impl JmapClient {
    pub async fn mailbox_changed(
        &self,
        new_state: State<MailboxObject>,
    ) -> Result<Option<BackendEvent>> {
        let mut cached_state: State<MailboxObject> =
            if let Some(s) = self.store.mailbox_state.lock().await.as_ref() {
                if s == &new_state {
                    return Ok(None);
                }
                s.clone()
            } else {
                return Ok(None);
            };
        let (mail_account_id, is_personal) = {
            let session = self.session_guard().await?;
            let mail_account_id = session.mail_account_id();
            let is_personal = session
                .accounts
                .get(&mail_account_id)
                .map(|acc| acc.is_personal)
                .unwrap_or(false);
            (mail_account_id, is_personal)
        };

        let mut events = vec![];
        loop {
            let mut req = Request::new(self.request_no.clone());
            let prev_seq = req.add_call(&MailboxChanges::new(
                Changes::new()
                    .account_id(mail_account_id.clone())
                    .since_state(cached_state.clone()),
            ));
            req.add_call(&MailboxGet::new(
                Get::new()
                    .ids(Some(Argument::reference::<
                        MailboxChanges,
                        MailboxObject,
                        MailboxObject,
                    >(
                        prev_seq,
                        ResultField::<MailboxChanges, MailboxObject>::new("/created"),
                    )))
                    .account_id(mail_account_id.clone()),
            ));
            req.add_call(&MailboxGet::new(
                Get::new()
                    .ids(Some(Argument::reference::<
                        MailboxChanges,
                        MailboxObject,
                        MailboxObject,
                    >(
                        prev_seq,
                        ResultField::<MailboxChanges, MailboxObject>::new("/updated"),
                    )))
                    .account_id(mail_account_id.clone()),
            ));

            let res_text = self
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;
            if cfg!(feature = "jmap-trace") {
                log::trace!("mailbox_since_state(): response {res_text:?}");
            }
            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = self.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let mut changes_response =
                MailboxChangesResponse::try_from(v.method_responses.remove(0))?;
            if changes_response.changes_response.new_state == cached_state {
                return Ok(None);
            }
            for destroyed_id in std::mem::take(&mut changes_response.changes_response.destroyed) {
                let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                if let Some(mailbox_hash) = mailboxes_lck.iter().find_map(|(h, m)| {
                    if m.id == destroyed_id {
                        Some(*h)
                    } else {
                        None
                    }
                }) {
                    mailboxes_lck.remove(&mailbox_hash);
                    events.push(RefreshEvent {
                        account_hash: self.store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::MailboxDelete(mailbox_hash),
                    });
                }
            }
            let get_response =
                GetResponse::<MailboxObject>::try_from(v.method_responses.remove(0))?;

            {
                // Created
                let GetResponse::<MailboxObject> { list, .. } = get_response;

                let mut created: HashMap<MailboxHash, JmapMailbox> = list
                    .into_iter()
                    .map(|r| mailbox_object_into_backend_mailbox(r, is_personal))
                    .collect();
                let cloned_keys = created
                    .keys()
                    .cloned()
                    .collect::<SmallVec<[MailboxHash; 24]>>();
                for key in cloned_keys {
                    if let Some(parent_hash) = created[&key].parent_hash {
                        if created.contains_key(&parent_hash) {
                            created
                                .entry(parent_hash)
                                .and_modify(|e| e.children.push(key));
                        } else {
                            let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                            mailboxes_lck
                                .entry(parent_hash)
                                .and_modify(|e| e.children.push(key));
                        }
                    }
                }
                for (h, m) in &created {
                    events.push(RefreshEvent {
                        account_hash: self.store.account_hash,
                        mailbox_hash: *h,
                        kind: RefreshEventKind::MailboxCreate(
                            crate::backends::BackendMailbox::clone(m),
                        ),
                    });
                }

                let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                mailboxes_lck.extend(created);
            }
            let get_response =
                GetResponse::<MailboxObject>::try_from(v.method_responses.remove(0))?;

            {
                // Updated
                let GetResponse::<MailboxObject> { list, .. } = get_response;

                let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                for v in list {
                    let hash = v.id.into_hash();
                    let parent_hash = v.parent_id.clone().map(|id| id.into_hash());
                    let prev_parent_hash = if let Some(ref mut m) = mailboxes_lck.get_mut(&hash) {
                        let is_subscribed = v.is_subscribed || is_personal;
                        if is_subscribed != m.is_subscribed {
                            events.push(RefreshEvent {
                                account_hash: self.store.account_hash,
                                mailbox_hash: hash,
                                kind: if is_subscribed {
                                    RefreshEventKind::MailboxSubscribe(hash)
                                } else {
                                    {
                                        RefreshEventKind::MailboxUnsubscribe(hash)
                                    }
                                },
                            });
                            m.is_subscribed = is_subscribed;
                        }
                        if m.name != v.name {
                            m.name = v.name.clone();
                            m.path = v.name;
                            events.push(RefreshEvent {
                                account_hash: self.store.account_hash,
                                mailbox_hash: hash,
                                kind: RefreshEventKind::MailboxRename {
                                    old_mailbox_hash: hash,
                                    new_mailbox:
                                        <JmapMailbox as crate::backends::BackendMailbox>::clone(m),
                                },
                            });
                        }
                        let prev_parent_hash = m.parent_hash;
                        m.parent_hash = parent_hash;
                        prev_parent_hash
                    } else {
                        None
                    };
                    if parent_hash != prev_parent_hash {
                        if let Some(p) = prev_parent_hash {
                            mailboxes_lck
                                .entry(p)
                                .and_modify(|e| e.children.retain(|c| *c != hash));
                        }
                        if let Some(p) = parent_hash {
                            mailboxes_lck.entry(p).and_modify(|e| e.children.push(hash));
                        }
                    }
                }
            }
            if !v.method_responses.is_empty() {
                panic!("{:?}", v.method_responses);
            }
            if changes_response.changes_response.has_more_changes {
                cached_state = changes_response.changes_response.new_state;
            } else {
                *self.store.mailbox_state.lock().await =
                    Some(changes_response.changes_response.new_state);

                break;
            }
        }

        Ok(events.try_into().ok())
    }

    pub async fn get_mailboxes(
        &mut self,
        request: Option<Request>,
    ) -> Result<HashMap<MailboxHash, JmapMailbox>> {
        let mut req = request.unwrap_or_else(|| Request::new(self.request_no.clone()));
        let mail_account_id = self.session_guard().await?.mail_account_id();
        let mailbox_get: MailboxGet =
            MailboxGet::new(Get::<MailboxObject>::new().account_id(mail_account_id));
        req.add_call(&mailbox_get);
        let res_text = self.send_request(serde_json::to_string(&req)?).await?;

        let v: MethodResponse = deserialize_from_str(&res_text)?;
        self.store.online_status.update_timestamp(None).await;
        let m = GetResponse::<MailboxObject>::try_from(*v.method_responses.last().unwrap())?;
        let GetResponse::<MailboxObject> {
            list,
            account_id,
            state,
            ..
        } = m;
        *self.store.mailbox_state.lock().await = Some(state);
        // Is account set as `personal`? (`isPersonal` property). Then, even if
        // `isSubscribed` is false on a mailbox, it should be regarded as
        // subscribed.
        let is_personal: bool = {
            let session = self.session_guard().await?;
            session
                .accounts
                .get(&account_id)
                .map(|acc| acc.is_personal)
                .unwrap_or(false)
        };
        let mut ret: HashMap<MailboxHash, JmapMailbox> = list
            .into_iter()
            .map(|r| mailbox_object_into_backend_mailbox(r, is_personal))
            .collect();
        let cloned_keys = ret.keys().cloned().collect::<SmallVec<[MailboxHash; 24]>>();
        for key in cloned_keys {
            if let Some(parent_hash) = ret[&key].parent_hash {
                ret.entry(parent_hash).and_modify(|e| e.children.push(key));
            }
        }
        Ok(ret)
    }
}

fn mailbox_object_into_backend_mailbox(
    m: MailboxObject,
    is_personal: bool,
) -> (MailboxHash, JmapMailbox) {
    let MailboxObject {
        id,
        is_subscribed,
        my_rights,
        name,
        parent_id,
        role,
        sort_order,
        total_emails,
        total_threads,
        unread_emails,
        unread_threads,
    } = m;
    let mut total_emails_set = LazyCountSet::default();
    total_emails_set.set_not_yet_seen(total_emails.try_into().unwrap_or(0));
    let total_emails = total_emails_set;
    let mut unread_emails_set = LazyCountSet::default();
    unread_emails_set.set_not_yet_seen(unread_emails.try_into().unwrap_or(0));
    let unread_emails = unread_emails_set;
    let hash = id.into_hash();
    let parent_hash = parent_id.clone().map(|id| id.into_hash());
    (
        hash,
        JmapMailbox {
            name: name.clone(),
            hash,
            path: name,
            children: Vec::new(),
            id,
            is_subscribed: is_subscribed || is_personal,
            my_rights,
            parent_id,
            parent_hash,
            role,
            usage: Default::default(),
            sort_order,
            total_emails: Arc::new(Mutex::new(total_emails)),
            total_threads,
            unread_emails: Arc::new(Mutex::new(unread_emails)),
            unread_threads,
        },
    )
}
