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

use super::folder::JmapFolder;
use super::*;
use serde::{de::DeserializeOwned, Serialize};
use serde_json::{json, Value};
use std::convert::TryFrom;

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

pub trait Response<OBJ: Object> {
    const NAME: &'static str;
}

pub trait Method<OBJ: Object>: Serialize {
    const NAME: &'static str;
}

macro_rules! get_path_hash {
    ($path:expr) => {{
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        $path.hash(&mut hasher);
        hasher.finish()
    }};
}

static USING: &'static [&'static str] = &["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"];

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

    pub fn add_call<M: Method<O>, O: Object>(&mut self, call: M) -> usize {
        let seq = get_request_no!(self.request_no);
        self.method_calls
            .push(serde_json::to_value((M::NAME, call, &format!("m{}", seq))).unwrap());
        seq
    }
}

#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum MethodCall {
    #[serde(rename_all = "camelCase")]
    EmailQuery {
        filter: Vec<String>, /* "inMailboxes": [ folder.id ] },*/
        collapse_threads: bool,
        position: u64,
        fetch_threads: bool,
        fetch_messages: bool,
        fetch_message_properties: Vec<MessageProperty>,
    },
    MailboxGet {},
    Empty {},
}

pub fn get_mailboxes(conn: &JmapConnection) -> Result<FnvHashMap<FolderHash, JmapFolder>> {
    let seq = get_request_no!(conn.request_no);
    let res = conn
        .client
        .lock()
        .unwrap()
        .post("https://jmap-proxy.local/jmap/fc32dffe-14e7-11ea-a277-2477037a1804/")
        .json(&json!({
            "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"],
            "methodCalls": [["Mailbox/get", {},
             format!("#m{}",seq).as_str()]],
        }))
        .send();

    let res_text = res?.text()?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    *conn.online_status.lock().unwrap() = true;
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
            let hash = get_path_hash!(&name);
            (
                hash,
                JmapFolder {
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
                    total_emails,
                    total_threads,
                    unread_emails,
                    unread_threads,
                },
            )
        })
        .collect())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct JsonResponse<'a> {
    #[serde(borrow)]
    method_responses: Vec<MethodResponse<'a>>,
}

pub fn get_message_list(conn: &JmapConnection, folder: &JmapFolder) -> Result<Vec<String>> {
    let seq = get_request_no!(conn.request_no);
    let email_call: EmailQueryCall = EmailQueryCall {
        filter: EmailFilterCondition {
            in_mailboxes: vec![folder.id.clone()],
            ..Default::default()
        },
        collapse_threads: false,
        position: 0,
        fetch_threads: true,
        fetch_messages: true,
        fetch_message_properties: vec![
            MessageProperty::ThreadId,
            MessageProperty::MailboxId,
            MessageProperty::IsUnread,
            MessageProperty::IsFlagged,
            MessageProperty::IsAnswered,
            MessageProperty::IsDraft,
            MessageProperty::HasAttachment,
            MessageProperty::From,
            MessageProperty::To,
            MessageProperty::Subject,
            MessageProperty::Date,
            MessageProperty::Preview,
        ],
    };

    let mut req = Request::new(conn.request_no.clone());
    req.add_call(email_call);

    /*
    {
        "using": [
            "urn:ietf:params:jmap:core",
            "urn:ietf:params:jmap:mail"
        ],
        "methodCalls": [[
                "Email/query",
                {
                    "collapseThreads": false,
                    "fetchMessageProperties": [
                        "threadId",
                        "mailboxId",
                        "isUnread",
                        "isFlagged",
                        "isAnswered",
                        "isDraft",
                        "hasAttachment",
                        "from",
                        "to",
                        "subject",
                        "date",
                        "preview"
                    ],
                    "fetchMessages": true,
                    "fetchThreads": true,
                    "filter": [
                        {
                            "inMailboxes": [
                                "fde49e47-14e7-11ea-a277-2477037a1804"
                            ]
                        }
                    ],
                    "position": 0
                },
                "f"
            ]]
    }
            */
    /*
        r#"
        "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"],
        "methodCalls": [["Email/query", { "filter": {
            "inMailboxes": [ folder.id ]
        },
       "collapseThreads": false,
       "position": 0,
       "fetchThreads": true,
       "fetchMessages": true,
       "fetchMessageProperties": [
           "threadId",
           "mailboxId",
           "isUnread",
           "isFlagged",
           "isAnswered",
           "isDraft",
           "hasAttachment",
           "from",
           "to",
           "subject",
           "date",
           "preview"
       ],
        }, format!("m{}", seq).as_str()]],
    });"
    );*/

    let res = conn
        .client
        .lock()
        .unwrap()
        .post("https://jmap-proxy.local/jmap/fc32dffe-14e7-11ea-a277-2477037a1804/")
        .json(&req)
        .send();

    let res_text = res?.text()?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    *conn.online_status.lock().unwrap() = true;
    let m = QueryResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let QueryResponse::<EmailObject> { ids, .. } = m;
    Ok(ids)
}

pub fn get_message(conn: &JmapConnection, ids: &[String]) -> Result<Vec<Envelope>> {
    let seq = get_request_no!(conn.request_no);
    let email_call: EmailGet = EmailGet::new(
        Get::new()
            .ids(Some(ids.iter().cloned().collect::<Vec<String>>()))
            .account_id(conn.account_id.lock().unwrap().clone()),
    );

    let mut req = Request::new(conn.request_no.clone());
    req.add_call(email_call);
    let res = conn
        .client
        .lock()
        .unwrap()
        .post("https://jmap-proxy.local/jmap/fc32dffe-14e7-11ea-a277-2477037a1804/")
        .json(&req)
        .send();

    let res_text = res?.text()?;
    let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
    let e = GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
    let GetResponse::<EmailObject> { list, .. } = e;
    Ok(list
        .into_iter()
        .map(std::convert::Into::into)
        .collect::<Vec<Envelope>>())
}

/*
 *
 *json!({
            "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"],
            "methodCalls": [["Email/get", {
                "ids": ids,
                "properties": [ "threadId", "mailboxIds", "from", "subject",
                "receivedAt",
                "htmlBody", "bodyValues" ],
                "bodyProperties": [ "partId", "blobId", "size", "type" ],
                "fetchHTMLBodyValues": true,
                "maxBodyValueBytes": 256
            }, format!("m{}", seq).as_str()]],
        }))

*/
