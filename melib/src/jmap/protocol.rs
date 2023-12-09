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

use std::convert::{TryFrom, TryInto};

use serde::Serialize;
use serde_json::Value;

use super::{mailbox::JmapMailbox, *};

pub type UtcDate = String;

use super::rfc8620::{Object, State};

macro_rules! get_request_no {
    ($lock:expr) => {{
        let mut lck = $lock.lock().unwrap();
        let ret = *lck;
        *lck += 1;
        ret
    }};
}

pub trait Response<OBJ: Object> {
    const NAME: &'static str;
}

pub trait Method<OBJ: Object>: Serialize {
    const NAME: &'static str;
}

static USING: &[&str] = &[JMAP_CORE_CAPABILITY, JMAP_MAIL_CAPABILITY];

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    using: &'static [&'static str],
    /* Why is this Value instead of Box<dyn Method<_>>? The Method trait cannot be made into a
     * Trait object because its serialize() will be generic. */
    method_calls: Vec<Value>,

    #[serde(skip)]
    request_no: Arc<Mutex<usize>>,
}

impl Request {
    pub fn new(request_no: Arc<Mutex<usize>>) -> Self {
        Self {
            using: USING,
            method_calls: Vec::new(),
            request_no,
        }
    }

    pub fn add_call<M: Method<O>, O: Object>(&mut self, call: &M) -> usize {
        let seq = get_request_no!(self.request_no);
        self.method_calls
            .push(serde_json::to_value((M::NAME, call, &format!("m{}", seq))).unwrap());
        seq
    }
}

pub async fn get_mailboxes(
    conn: &mut JmapConnection,
    request: Option<Request>,
) -> Result<HashMap<MailboxHash, JmapMailbox>> {
    let mut req = request.unwrap_or_else(|| Request::new(conn.request_no.clone()));
    let mailbox_get: MailboxGet =
        MailboxGet::new(Get::<MailboxObject>::new().account_id(conn.mail_account_id()));
    req.add_call(&mailbox_get);
    let res_text = conn.send_request(serde_json::to_string(&req)?).await?;

    let mut v: MethodResponse = deserialize_from_str(&res_text)?;
    *conn.store.online_status.lock().await = (std::time::Instant::now(), Ok(()));
    let m = GetResponse::<MailboxObject>::try_from(v.method_responses.remove(0))?;
    let GetResponse::<MailboxObject> {
        list, account_id, ..
    } = m;
    conn.last_method_response = Some(res_text);
    // Is account set as `personal`? (`isPersonal` property). Then, even if
    // `isSubscribed` is false on a mailbox, it should be regarded as
    // subscribed.
    let is_personal: bool = {
        let session = conn.session_guard();
        session
            .accounts
            .get(&account_id)
            .map(|acc| acc.is_personal)
            .unwrap_or(false)
    };
    *conn.store.account_id.lock().unwrap() = account_id;
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
    let email_call: EmailQuery = EmailQuery::new(
        Query::new()
            .account_id(conn.mail_account_id())
            .filter(Some(Filter::Condition(
                EmailFilterCondition::new()
                    .in_mailbox(Some(mailbox.id.clone()))
                    .into(),
            )))
            .position(0),
    )
    .collapse_threads(false);

    let mut req = Request::new(conn.request_no.clone());
    req.add_call(&email_call);

    let mut res = conn.post_async(None, serde_json::to_string(&req)?).await?;

    let res_text = res.text().await?;
    let mut v: MethodResponse = match deserialize_from_str(&res_text) {
        Err(err) => {
            *conn.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }
        Ok(s) => s,
    };
    *conn.store.online_status.lock().await = (std::time::Instant::now(), Ok(()));
    let m = QueryResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let QueryResponse::<EmailObject> { ids, .. } = m;
    conn.last_method_response = Some(res_text);
    Ok(ids)
}

/*
pub async fn get_message(conn: &JmapConnection, ids: &[String]) -> Result<Vec<Envelope>> {
    let email_call: EmailGet = EmailGet::new(
        Get::new()
            .ids(Some(Argument::value(ids.to_vec())))
            .account_id(conn.mail_account_id().to_string()),
    );

    let mut req = Request::new(conn.request_no.clone());
    req.add_call(&email_call);
    let mut res = conn
        .post_async(None, serde_json::to_string(&req)?)
        .await?;

    let res_text = res.text().await?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    let e = GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let GetResponse::<EmailObject> { list, .. } = e;
    Ok(list
        .into_iter()
        .map(std::convert::Into::into)
        .collect::<Vec<Envelope>>())
}
*/

#[derive(Clone, Copy)]
pub enum EmailFetchState {
    Start { batch_size: u64 },
    Ongoing { position: u64, batch_size: u64 },
}

impl EmailFetchState {
    pub async fn must_update_state(
        &mut self,
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

    pub async fn fetch(
        &mut self,
        conn: &mut JmapConnection,
        store: &Store,
        mailbox_hash: MailboxHash,
    ) -> Result<Vec<Envelope>> {
        loop {
            match *self {
                Self::Start { batch_size } => {
                    *self = Self::Ongoing {
                        position: 0,
                        batch_size,
                    };
                    continue;
                }
                Self::Ongoing {
                    mut position,
                    batch_size,
                } => {
                    let mailbox_id = store.mailboxes.read().unwrap()[&mailbox_hash].id.clone();
                    let email_query_call: EmailQuery = EmailQuery::new(
                        Query::new()
                            .account_id(conn.mail_account_id().clone())
                            .filter(Some(Filter::Condition(
                                EmailFilterCondition::new()
                                    .in_mailbox(Some(mailbox_id))
                                    .into(),
                            )))
                            .position(position)
                            .limit(Some(batch_size)),
                    )
                    .collapse_threads(false);

                    let mut req = Request::new(conn.request_no.clone());
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
                            .account_id(conn.mail_account_id().clone()),
                    );

                    let _prev_seq = req.add_call(&email_call);
                    let res_text = conn.send_request(serde_json::to_string(&req)?).await?;
                    let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                        Err(err) => {
                            *conn.store.online_status.lock().await =
                                (Instant::now(), Err(err.clone()));
                            return Err(err);
                        }
                        Ok(v) => v,
                    };

                    let e =
                        GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
                    let GetResponse::<EmailObject> { list, state, .. } = e;
                    conn.last_method_response = Some(res_text);

                    if self.must_update_state(conn, mailbox_hash, state).await? {
                        *self = Self::Start { batch_size };
                        continue;
                    }
                    let mut total = BTreeSet::default();
                    let mut unread = BTreeSet::default();
                    let mut ret = Vec::with_capacity(list.len());
                    for obj in list {
                        let env = store.add_envelope(obj);
                        total.insert(env.hash());
                        if !env.is_seen() {
                            unread.insert(env.hash());
                        }
                        ret.push(env);
                    }
                    let mut mailboxes_lck = store.mailboxes.write().unwrap();
                    mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                        mbox.total_emails.lock().unwrap().insert_existing_set(total);
                        mbox.unread_emails
                            .lock()
                            .unwrap()
                            .insert_existing_set(unread);
                    });
                    position += batch_size;
                    *self = Self::Ongoing {
                        position,
                        batch_size,
                    };
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
