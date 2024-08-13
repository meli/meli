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
    collections::{BTreeSet, HashMap},
    convert::{TryFrom, TryInto},
    sync::{Arc, Mutex},
};

use futures::lock::Mutex as FutureMutex;
use isahc::AsyncReadResponseExt;
use serde::Serialize;
use serde_json::Value;
use smallvec::SmallVec;

use crate::{
    email::Envelope,
    error::Result,
    jmap::{
        argument::Argument,
        backend_mailbox::JmapMailbox,
        capabilities::*,
        deserialize_from_str,
        email::{EmailFilterCondition, EmailGet, EmailObject, EmailQuery},
        filters::Filter,
        mailbox::{MailboxGet, MailboxObject},
        methods::{Get, GetResponse, MethodResponse, Query, QueryResponse},
        objects::{Id, Object, State},
        JmapConnection, Store,
    },
    Flag, LazyCountSet, MailboxHash,
};

pub type UtcDate = String;

crate::_impl_jmap_capability! { JmapMailCapability: "urn:ietf:params:jmap:mail", name: "Mail" }
crate::_impl_jmap_capability! { JmapSubmissionCapability: "urn:ietf:params:jmap:submission", name: "Submission" }

pub trait Response<OBJ: Object>: Send + Sync {
    const NAME: &'static str;
}

pub trait Method<OBJ: Object>: Serialize + Send + Sync {
    const NAME: &'static str;
}

static USING: &[&str] = &[
    JmapCoreCapability::uri(),
    JmapMailCapability::uri(),
    JmapSubmissionCapability::uri(),
];

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    using: &'static [&'static str],

    /// This field is `Value` instead of `Box<dyn Method<_>>` because the
    /// `Method` trait cannot be made into a trait object; that requires its
    /// `serialize()` implementation to be generic.
    method_calls: Vec<Value>,

    #[serde(skip)]
    request_no: Arc<FutureMutex<usize>>,
}

macro_rules! get_request_no {
    ($lock:expr) => {{
        let mut lck = $lock.lock().await;
        let ret = *lck;
        *lck += 1;
        drop(lck);
        ret
    }};
}

impl Request {
    pub fn new(request_no: Arc<FutureMutex<usize>>) -> Self {
        Self {
            using: USING,
            method_calls: Vec::new(),
            request_no,
        }
    }

    pub async fn add_call<M: Method<O>, O: Object>(&mut self, call: &M) -> usize {
        let seq = get_request_no!(self.request_no);
        self.method_calls
            .push(serde_json::to_value((M::NAME, call, &format!("m{}", seq))).unwrap());
        seq
    }

    pub fn request_no(&self) -> Arc<FutureMutex<usize>> {
        self.request_no.clone()
    }

    pub async fn request_no_value(&self) -> usize {
        get_request_no!(self.request_no)
    }
}

pub async fn get_mailboxes(
    conn: &mut JmapConnection,
    request: Option<Request>,
) -> Result<HashMap<MailboxHash, JmapMailbox>> {
    let mut req = request.unwrap_or_else(|| Request::new(conn.request_no.clone()));
    let mail_account_id = conn.session_guard().await?.mail_account_id();
    let mailbox_get: MailboxGet =
        MailboxGet::new(Get::<MailboxObject>::new().account_id(mail_account_id));
    req.add_call(&mailbox_get).await;
    let res_text = conn.send_request(serde_json::to_string(&req)?).await?;

    let v: MethodResponse = deserialize_from_str(&res_text)?;
    conn.store.online_status.update_timestamp(None).await;
    let m = GetResponse::<MailboxObject>::try_from(*v.method_responses.last().unwrap())?;
    let GetResponse::<MailboxObject> {
        list,
        account_id,
        state,
        ..
    } = m;
    *conn.store.mailbox_state.lock().await = state;
    conn.last_method_response = Some(res_text);
    // Is account set as `personal`? (`isPersonal` property). Then, even if
    // `isSubscribed` is false on a mailbox, it should be regarded as
    // subscribed.
    let is_personal: bool = {
        let session = conn.session_guard().await?;
        session
            .accounts
            .get(&account_id)
            .map(|acc| acc.is_personal)
            .unwrap_or(false)
    };
    let mut ret: HashMap<MailboxHash, JmapMailbox> = list
        .into_iter()
        .map(|r| {
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
            } = r;
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
                    email_state: Arc::new(Mutex::new(None)),
                    email_query_state: Arc::new(Mutex::new(None)),
                },
            )
        })
        .collect();
    let cloned_keys = ret.keys().cloned().collect::<SmallVec<[MailboxHash; 24]>>();
    for key in cloned_keys {
        if let Some(parent_hash) = ret[&key].parent_hash {
            ret.entry(parent_hash).and_modify(|e| e.children.push(key));
        }
    }
    Ok(ret)
}

pub async fn get_message_list(
    conn: &mut JmapConnection,
    mailbox: &JmapMailbox,
) -> Result<Vec<Id<EmailObject>>> {
    let mail_account_id = conn.session_guard().await?.mail_account_id();
    let email_call: EmailQuery = EmailQuery::new(
        Query::new()
            .account_id(mail_account_id)
            .filter(Some(Filter::Condition(
                EmailFilterCondition::new()
                    .in_mailbox(Some(mailbox.id.clone()))
                    .into(),
            )))
            .position(0),
    )
    .collapse_threads(false);

    let mut req = Request::new(conn.request_no.clone());
    req.add_call(&email_call).await;

    let res_text = conn
        .post_async(None, serde_json::to_string(&req)?)
        .await?
        .text()
        .await?;

    let mut v: MethodResponse = match deserialize_from_str(&res_text) {
        Err(err) => {
            _ = conn.store.online_status.set(None, Err(err.clone())).await;
            return Err(err);
        }
        Ok(s) => s,
    };
    conn.store.online_status.update_timestamp(None).await;
    let m = QueryResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let QueryResponse::<EmailObject> { ids, .. } = m;
    conn.last_method_response = Some(res_text);
    Ok(ids)
}

pub struct EmailFetcher {
    pub connection: Arc<FutureMutex<JmapConnection>>,
    pub store: Arc<Store>,
    pub batch_size: u64,
    pub state: EmailFetchState,
}

pub enum EmailFetchState {
    Start,
    Ongoing { position: u64 },
}

impl EmailFetcher {
    pub async fn must_update_state(
        conn: &JmapConnection,
        mailbox_hash: MailboxHash,
        state: State<EmailObject>,
    ) -> Result<bool> {
        {
            let (is_empty, is_equal) = {
                let mailboxes_lck = conn.store.mailboxes.read().unwrap();
                mailboxes_lck
                    .get(&mailbox_hash)
                    .map(|mbox| {
                        let current_state_lck = mbox.email_state.lock().unwrap();
                        (
                            current_state_lck.is_none(),
                            current_state_lck.as_ref() == Some(&state),
                        )
                    })
                    .unwrap_or((true, true))
            };
            if is_empty {
                let mut mailboxes_lck = conn.store.mailboxes.write().unwrap();
                debug!("{:?}: inserting state {}", EmailObject::NAME, &state);
                mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                    *mbox.email_state.lock().unwrap() = Some(state);
                });
            } else if !is_equal {
                conn.email_changes(mailbox_hash).await?;
            }
            Ok(is_empty || !is_equal)
        }
    }

    pub async fn fetch(&mut self, mailbox_hash: MailboxHash) -> Result<Vec<Envelope>> {
        loop {
            match self.state {
                EmailFetchState::Start => {
                    self.state = EmailFetchState::Ongoing { position: 0 };
                    continue;
                }
                EmailFetchState::Ongoing { mut position } => {
                    let mut conn = self.connection.lock().await;
                    conn.connect().await?;
                    let mail_account_id = conn.session_guard().await?.mail_account_id();
                    let mailbox_id = self.store.mailboxes.read().unwrap()[&mailbox_hash]
                        .id
                        .clone();
                    let email_query_call: EmailQuery = EmailQuery::new(
                        Query::new()
                            .account_id(mail_account_id.clone())
                            .filter(Some(Filter::Condition(
                                EmailFilterCondition::new()
                                    .in_mailbox(Some(mailbox_id))
                                    .into(),
                            )))
                            .position(position)
                            .limit(Some(self.batch_size)),
                    )
                    .collapse_threads(false);

                    let mut req = Request::new(conn.request_no.clone());
                    let prev_seq = req.add_call(&email_query_call).await;

                    let email_call: EmailGet = EmailGet::new(
                        Get::new()
                            .ids(Some(Argument::reference::<
                                EmailQuery,
                                EmailObject,
                                EmailObject,
                            >(
                                prev_seq, EmailQuery::RESULT_FIELD_IDS
                            )))
                            .account_id(mail_account_id),
                    );

                    let _prev_seq = req.add_call(&email_call).await;
                    let res_text = conn.send_request(serde_json::to_string(&req)?).await?;
                    let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                        Err(err) => {
                            _ = conn.store.online_status.set(None, Err(err.clone())).await;
                            return Err(err);
                        }
                        Ok(v) => v,
                    };

                    let e =
                        GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
                    let GetResponse::<EmailObject> { list, state, .. } = e;
                    conn.last_method_response = Some(res_text);

                    if Self::must_update_state(&conn, mailbox_hash, state).await? {
                        self.state = EmailFetchState::Start;
                        continue;
                    }
                    drop(conn);
                    let mut total = BTreeSet::default();
                    let mut unread = BTreeSet::default();
                    let mut ret = Vec::with_capacity(list.len());
                    for obj in list {
                        let env = self.store.add_envelope(obj).await;
                        total.insert(env.hash());
                        if !env.is_seen() {
                            unread.insert(env.hash());
                        }
                        ret.push(env);
                    }
                    let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                    mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                        mbox.total_emails.lock().unwrap().insert_existing_set(total);
                        mbox.unread_emails
                            .lock()
                            .unwrap()
                            .insert_existing_set(unread);
                    });
                    position += self.batch_size;
                    self.state = EmailFetchState::Ongoing { position };
                    return Ok(ret);
                }
            }
        }
    }
}

pub fn keywords_to_flags(keywords: Vec<String>) -> (Flag, Vec<String>) {
    let mut f = Flag::default();
    let mut tags = vec![];
    for k in keywords {
        match k.as_str() {
            "$draft" => {
                f |= Flag::DRAFT;
            }
            "$seen" => {
                f |= Flag::SEEN;
            }
            "$flagged" => {
                f |= Flag::FLAGGED;
            }
            "$answered" => {
                f |= Flag::REPLIED;
            }
            "$junk" | "$notjunk" => { /* ignore */ }
            _ => tags.push(k),
        }
    }
    (f, tags)
}
