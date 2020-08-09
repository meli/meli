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

use super::mailbox::JmapMailbox;
use super::*;
use serde::Serialize;
use serde_json::{json, Value};
use smallvec::SmallVec;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

pub type Id = String;
pub type UtcDate = String;

use super::rfc8620::Object;

macro_rules! get_request_no {
    ($lock:expr) => {{
        let mut lck = $lock.lock().unwrap();
        let ret = *lck;
        *lck += 1;
        ret
    }};
}

macro_rules! tag_hash {
    ($t:ident) => {{
        let mut hasher = DefaultHasher::default();
        $t.hash(&mut hasher);
        hasher.finish()
    }};
    ($t:literal) => {{
        let mut hasher = DefaultHasher::default();
        $t.hash(&mut hasher);
        hasher.finish()
    }};
}

pub trait Response<OBJ: Object> {
    const NAME: &'static str;
}

pub trait Method<OBJ: Object>: Serialize {
    const NAME: &'static str;
}

static USING: &[&str] = &["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"];

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
        Request {
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

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct JsonResponse<'a> {
    #[serde(borrow)]
    method_responses: Vec<MethodResponse<'a>>,
}

pub async fn get_mailboxes(conn: &JmapConnection) -> Result<HashMap<MailboxHash, JmapMailbox>> {
    let seq = get_request_no!(conn.request_no);
    let mut res = conn
        .client
        .post_async(
            &conn.session.api_url,
            serde_json::to_string(&json!({
                "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"],
                "methodCalls": [["Mailbox/get", {
                "accountId": conn.mail_account_id()
                },
                 format!("#m{}",seq).as_str()]],
            }))?,
        )
        .await?;

    let res_text = res.text_async().await?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    *conn.online_status.lock().await = (std::time::Instant::now(), Ok(()));
    let m = GetResponse::<MailboxObject>::try_from(v.method_responses.remove(0))?;
    let GetResponse::<MailboxObject> {
        list, account_id, ..
    } = m;
    *conn.account_id.lock().unwrap() = account_id;
    Ok(list
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
            let hash = crate::get_path_hash!(&name);
            (
                hash,
                JmapMailbox {
                    name: name.clone(),
                    hash,
                    path: name,
                    v: Vec::new(),
                    id,
                    is_subscribed,
                    my_rights,
                    parent_id,
                    role,
                    usage: Default::default(),
                    sort_order,
                    total_emails: Arc::new(Mutex::new(total_emails)),
                    total_threads,
                    unread_emails: Arc::new(Mutex::new(unread_emails)),
                    unread_threads,
                },
            )
        })
        .collect())
}

pub async fn get_message_list(conn: &JmapConnection, mailbox: &JmapMailbox) -> Result<Vec<String>> {
    let email_call: EmailQuery = EmailQuery::new(
        Query::new()
            .account_id(conn.mail_account_id().to_string())
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

    let mut res = conn
        .client
        .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
        .await?;

    let res_text = res.text_async().await?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    *conn.online_status.lock().await = (std::time::Instant::now(), Ok(()));
    let m = QueryResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let QueryResponse::<EmailObject> { ids, .. } = m;
    Ok(ids)
}

pub async fn get_message(conn: &JmapConnection, ids: &[String]) -> Result<Vec<Envelope>> {
    let email_call: EmailGet = EmailGet::new(
        Get::new()
            .ids(Some(JmapArgument::value(ids.to_vec())))
            .account_id(conn.mail_account_id().to_string()),
    );

    let mut req = Request::new(conn.request_no.clone());
    req.add_call(&email_call);
    let mut res = conn
        .client
        .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
        .await?;

    let res_text = res.text_async().await?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    let e = GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let GetResponse::<EmailObject> { list, .. } = e;
    Ok(list
        .into_iter()
        .map(std::convert::Into::into)
        .collect::<Vec<Envelope>>())
}

pub async fn fetch(
    conn: &JmapConnection,
    store: &Arc<RwLock<Store>>,
    tag_index: &Arc<RwLock<BTreeMap<u64, String>>>,
    mailboxes: &Arc<RwLock<HashMap<MailboxHash, JmapMailbox>>>,
    mailbox_hash: MailboxHash,
) -> Result<Vec<Envelope>> {
    let mailbox_id = mailboxes.read().unwrap()[&mailbox_hash].id.clone();
    let email_query_call: EmailQuery = EmailQuery::new(
        Query::new()
            .account_id(conn.mail_account_id().to_string())
            .filter(Some(Filter::Condition(
                EmailFilterCondition::new()
                    .in_mailbox(Some(mailbox_id))
                    .into(),
            )))
            .position(0),
    )
    .collapse_threads(false);

    let mut req = Request::new(conn.request_no.clone());
    let prev_seq = req.add_call(&email_query_call);

    let email_call: EmailGet = EmailGet::new(
        Get::new()
            .ids(Some(JmapArgument::reference(
                prev_seq,
                EmailQuery::RESULT_FIELD_IDS,
            )))
            .account_id(conn.mail_account_id().to_string()),
    );

    req.add_call(&email_call);

    let mut res = conn
        .client
        .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
        .await?;

    let res_text = res.text_async().await?;

    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    let e = GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
    let GetResponse::<EmailObject> { list, state, .. } = e;
    {
        let mut states_lck = conn.method_call_states.lock().unwrap();

        if let Some(prev_state) = states_lck.get_mut(&EmailGet::NAME) {
            debug!("{:?}: prev_state was {}", EmailGet::NAME, prev_state);

            if *prev_state != state { /* FIXME Query Changes. */ }

            *prev_state = state;
            debug!("{:?}: curr state is {}", EmailGet::NAME, prev_state);
        } else {
            debug!("{:?}: inserting state {}", EmailGet::NAME, &state);
            states_lck.insert(EmailGet::NAME, state);
        }
    }
    let mut tag_lck = tag_index.write().unwrap();
    let ids = list
        .iter()
        .map(|obj| {
            let tags = obj
                .keywords()
                .keys()
                .map(|tag| {
                    let tag_hash = {
                        let mut hasher = DefaultHasher::default();
                        tag.hash(&mut hasher);
                        hasher.finish()
                    };
                    if !tag_lck.contains_key(&tag_hash) {
                        tag_lck.insert(tag_hash, tag.to_string());
                    }
                    tag_hash
                })
                .collect::<SmallVec<[u64; 1024]>>();
            (tags, obj.id.clone(), obj.blob_id.clone())
        })
        .collect::<Vec<(SmallVec<[u64; 1024]>, Id, Id)>>();
    drop(tag_lck);
    let mut ret = list
        .into_iter()
        .map(std::convert::Into::into)
        .collect::<Vec<Envelope>>();

    let mut store_lck = store.write().unwrap();
    debug_assert_eq!(tag_hash!("$draft"), 6613915297903591176);
    debug_assert_eq!(tag_hash!("$seen"), 1683863812294339685);
    debug_assert_eq!(tag_hash!("$flagged"), 2714010747478170100);
    debug_assert_eq!(tag_hash!("$answered"), 8940855303929342213);
    debug_assert_eq!(tag_hash!("$junk"), 2656839745430720464);
    debug_assert_eq!(tag_hash!("$notjunk"), 4091323799684325059);
    for (env, (tags, id, blob_id)) in ret.iter_mut().zip(ids.into_iter()) {
        store_lck.id_store.insert(env.hash(), id);
        store_lck.blob_id_store.insert(env.hash(), blob_id);
        for t in tags {
            match t {
                6613915297903591176 => {
                    env.set_flags(env.flags() | Flag::DRAFT);
                }
                1683863812294339685 => {
                    env.set_flags(env.flags() | Flag::SEEN);
                }
                2714010747478170100 => {
                    env.set_flags(env.flags() | Flag::FLAGGED);
                }
                8940855303929342213 => {
                    env.set_flags(env.flags() | Flag::REPLIED);
                }
                2656839745430720464 | 4091323799684325059 => { /* ignore */ }
                _ => env.labels_mut().push(t),
            }
        }
    }
    Ok(ret)
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
