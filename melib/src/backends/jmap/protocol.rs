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

pub type Id = String;
pub type UtcDate = String;

use super::rfc8620::Object;

pub trait Method<OBJ: Object>: Serialize {
    const NAME: &'static str;
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub enum MessageProperty {
    ThreadId,
    MailboxId,
    IsUnread,
    IsFlagged,
    IsAnswered,
    IsDraft,
    HasAttachment,
    From,
    To,
    Subject,
    Date,
    Preview,
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
}

impl Request {
    pub fn new() -> Self {
        Request {
            using: USING,
            method_calls: Vec::new(),
        }
    }

    pub fn add_call<M: Method<O>, O: Object>(&mut self, call: M) {
        self.method_calls
            .push(serde_json::to_value((M::NAME, call, "f")).unwrap());
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

pub fn get_mailboxes(conn: &mut JmapConnection) -> Result<FnvHashMap<FolderHash, JmapFolder>> {
    let res = conn
        .client
        .post("https://jmap-proxy.local/jmap/fc32dffe-14e7-11ea-a277-2477037a1804/")
        .json(&json!({
            "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"],
            "methodCalls": [["Mailbox/get", {},
             format!("#m{}", conn.request_no + 1).as_str()]],
        }))
        .send();
    conn.request_no += 1;

    let mut v: JsonResponse =
        serde_json::from_str(&std::dbg!(res.unwrap().text().unwrap())).unwrap();
    *conn.online_status.lock().unwrap() = true;
    std::dbg!(&v);
    assert_eq!("Mailbox/get", v.method_responses[0].0);
    Ok(
        if let Response::MailboxGet { list, .. } = v.method_responses.remove(0).1 {
            list.into_iter().map(|r| {
                if let MailboxResponse {
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
                } = r
                {
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
                } else {
                    panic!()
                }
            })
        } else {
            panic!()
        }
        .collect(),
    )
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct JsonResponse {
    method_responses: Vec<MethodResponse>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MethodResponse(String, Response, String);

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum Response {
    #[serde(rename_all = "camelCase")]
    MailboxGet {
        account_id: String,
        list: Vec<MailboxResponse>,
        not_found: Vec<String>,
        state: String,
    },
    #[serde(rename_all = "camelCase")]
    EmailQuery {
        account_id: String,
        can_calculate_changes: bool,
        collapse_threads: bool,
        filter: Value,
        ids: Vec<String>,
        position: u64,
        query_state: String,
        sort: Option<String>,
        total: usize,
    },
    Empty {},
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MailboxResponse {
    id: String,
    is_subscribed: bool,
    my_rights: JmapRights,
    name: String,
    parent_id: Option<String>,
    role: Option<String>,
    sort_order: u64,
    total_emails: u64,
    total_threads: u64,
    unread_emails: u64,
    unread_threads: u64,
}
#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct JmapRights {
    may_add_items: bool,
    may_create_child: bool,
    may_delete: bool,
    may_read_items: bool,
    may_remove_items: bool,
    may_rename: bool,
    may_set_keywords: bool,
    may_set_seen: bool,
    may_submit: bool,
}

// [
//     [ "getMessageList", {
//         filter: {
//             inMailboxes: [ "mailbox1" ]
//         },
//         sort: [ "date desc", "id desc" ]
//         collapseThreads: true,
//         position: 0,
//         limit: 10,
//         fetchThreads: true,
//         fetchMessages: true,
//         fetchMessageProperties: [
//             "threadId",
//             "mailboxId",
//             "isUnread",
//             "isFlagged",
//             "isAnswered",
//             "isDraft",
//             "hasAttachment",
//             "from",
//             "to",
//             "subject",
//             "date",
//             "preview"
//         ],
//         fetchSearchSnippets: false
//     }, "call1"]
// ]
pub fn get_message_list(conn: &mut JmapConnection, folder: &JmapFolder) -> Result<Vec<String>> {
    let email_call: EmailQueryCall = EmailQueryCall {
        filter: vec![EmailFilterCondition {
            in_mailboxes: vec![folder.id.clone()],
            ..Default::default()
        }],
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

    let mut req = Request::new();
    req.add_call(email_call);
    std::dbg!(serde_json::to_string(&req));

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
    let res = conn
        .client
        .post("https://jmap-proxy.local/jmap/fc32dffe-14e7-11ea-a277-2477037a1804/")
        .json(&json!({
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
            }, format!("#m{}", conn.request_no + 1).as_str()]],
        }))
        .send();

    conn.request_no += 1;
    let mut v: JsonResponse = serde_json::from_str(&std::dbg!(res.unwrap().text().unwrap()))?;

    let result: Response = v.method_responses.remove(0).1;
    if let Response::EmailQuery { ids, .. } = result {
        Ok(ids)
    } else {
        Err(MeliError::new(format!("response was {:#?}", &result)))
    }
}

pub fn get_message(conn: &mut JmapConnection, ids: &[String]) -> Result<Vec<Envelope>> {
    let res = conn
        .client
        .post("https://jmap-proxy.local/jmap/fc32dffe-14e7-11ea-a277-2477037a1804/")
        .json(&json!({
            "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"],
            "methodCalls": [["Email/get", {
                "ids": ids,
                "properties": [ "threadId", "mailboxIds", "from", "subject",
                "receivedAt",
                "htmlBody", "bodyValues" ],
                "bodyProperties": [ "partId", "blobId", "size", "type" ],
                "fetchHTMLBodyValues": true,
                "maxBodyValueBytes": 256
            }, format!("#m{}", conn.request_no + 1).as_str()]],
        }))
        .send();
    conn.request_no += 1;

    let v: JsonResponse = serde_json::from_str(&std::dbg!(res.unwrap().text().unwrap()))?;
    std::dbg!(&v);
    Ok(vec![])
}
