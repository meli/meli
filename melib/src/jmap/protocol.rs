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
    collections::BTreeSet,
    convert::TryFrom as _,
    sync::{atomic::AtomicUsize, Arc},
};

use futures::lock::Mutex as FutureMutex;
use serde::Serialize;
use serde_json::Value;

use crate::{
    email::Envelope,
    error::Result,
    jmap::{
        argument::Argument,
        capabilities::*,
        deserialize_from_str,
        email::{EmailFilterCondition, EmailGet, EmailObject, EmailQuery},
        filters::Filter,
        methods::{Get, GetResponse, MethodResponse, Query},
        objects::{Account, Id, Object, State},
        JmapClient, JmapConnection, Store,
    },
    Flag, MailboxHash,
};

pub type UtcDate = String;

pub trait Response<OBJ: Object>: Send + Sync {
    const NAME: &'static str;
}

pub trait Method<OBJ: Object>: Serialize + Send + Sync {
    const NAME: &'static str;
}

const USING: &[&str] = &[
    JmapCoreCapability::uri(),
    JmapMailCapability::uri(),
    JmapSubmissionCapability::uri(),
];

pub const USING_CONTACTS: &[&str] = &[JmapCoreCapability::uri(), JmapContactsCapability::uri()];

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    using: &'static [&'static str],

    /// This field is `Value` instead of `Box<dyn Method<_>>` because the
    /// `Method` trait cannot be made into a trait object; that requires its
    /// `serialize()` implementation to be generic.
    method_calls: Vec<Value>,

    #[serde(skip)]
    #[allow(clippy::struct_field_names)]
    request_no: Arc<AtomicUsize>,
}

macro_rules! get_request_no {
    ($lock:expr) => {{
        $lock.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    }};
}

impl Request {
    pub fn new(request_no: Arc<AtomicUsize>) -> Self {
        Self {
            using: USING,
            method_calls: Vec::new(),
            request_no,
        }
    }

    pub fn new_with_using(request_no: Arc<AtomicUsize>, using: &'static [&'static str]) -> Self {
        Self {
            using,
            method_calls: Vec::new(),
            request_no,
        }
    }

    pub fn add_call<M: Method<O>, O: Object>(&mut self, call: &M) -> usize {
        let seq = get_request_no!(self.request_no);
        self.method_calls
            .push(serde_json::to_value((M::NAME, call, &format!("m{seq}"))).unwrap());
        seq
    }
}

pub struct EmailFetcher {
    pub connection: Arc<FutureMutex<JmapConnection>>,
    pub mail_account_id: Id<Account>,
    pub store: Arc<Store>,
    pub batch_size: u64,
    pub state: EmailFetchState,
}

pub enum EmailFetchState {
    Start,
    Ongoing { position: u64 },
}

impl EmailFetcher {
    pub async fn must_update_state(client: &JmapClient, state: State<EmailObject>) -> Result<bool> {
        {
            let (is_empty, is_equal) = {
                let current_state_lck = client.store.email_state.lock().await;
                (
                    current_state_lck.is_none(),
                    current_state_lck.as_ref() == Some(&state),
                )
            };
            if is_empty {
                log::debug!("{:?}: inserting state {state}", EmailObject::NAME);
                *client.store.email_state.lock().await = Some(state);
            } else if !is_equal {
                if let Some(ev) = client.email_changed(Some(state)).await? {
                    client.add_backend_event(ev);
                }
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
                    let client = conn.client().await?;
                    client.connect().await?;
                    let mailbox_id = self.store.mailboxes.read().unwrap()[&mailbox_hash]
                        .id
                        .clone();
                    let email_query_call: EmailQuery = EmailQuery::new(
                        Query::new()
                            .account_id(self.mail_account_id.clone())
                            .filter(Some(Filter::Condition(
                                EmailFilterCondition::new().in_mailbox(Some(mailbox_id)),
                            )))
                            .position(position)
                            .limit(Some(self.batch_size)),
                    )
                    .collapse_threads(false);

                    let mut req = Request::new(client.request_no.clone());
                    let prev_seq = req.add_call(&email_query_call);

                    let email_call: EmailGet = EmailGet::new(
                        Get::new()
                            .ids(Some(Argument::reference::<
                                EmailQuery,
                                EmailObject,
                                EmailObject,
                            >(
                                prev_seq, EmailQuery::RESULT_FIELD_IDS
                            )))
                            .account_id(self.mail_account_id.clone()),
                    );

                    let _prev_seq = req.add_call(&email_call);
                    let res_text = client.send_request(serde_json::to_string(&req)?).await?;
                    let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                        Err(err) => {
                            _ = client.store.online_status.set(None, Err(err.clone())).await;
                            return Err(err);
                        }
                        Ok(v) => v,
                    };

                    let e =
                        GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
                    let GetResponse::<EmailObject> { list, state, .. } = e;

                    if Self::must_update_state(client, state).await? {
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
                        let mut counters = mbox.counters.lock().unwrap();
                        counters.total.insert_existing_set(total);
                        counters.unseen.insert_existing_set(unread);
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
