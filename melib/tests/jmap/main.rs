//
// meli
//
// Copyright 2025 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

#[cfg(feature = "jmap")]
use rusty_fork::rusty_fork_test;

#[cfg(feature = "jmap")]
rusty_fork_test! {
    #[test]
    fn test_jmap_refresh() {
        tests::run_jmap_refresh();
    }
}

#[cfg(feature = "jmap")]
pub mod server {
    use std::{
        net::{SocketAddr, TcpListener},
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::UnboundedReceiver,
        future::{self, Either},
        io::{AsyncReadExt, AsyncWriteExt},
        StreamExt,
    };
    use melib::{
        backends::prelude::*,
        jmap::{argument::*, identity, methods::*, objects::*, session::Session, *},
        smol::Async,
        Mail,
    };
    use serde::{Deserialize, Serialize};
    use serde_json::Value;

    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct ClientRequest {
        pub using: Vec<String>,
        pub method_calls: Vec<[Value; 3]>,
    }

    #[derive(Clone, Debug, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct ClientMethodResponse {
        #[serde(serialize_with = "method_response_ser")]
        pub method_responses: IndexMap<String, Value>,
        #[serde(default)]
        pub created_ids: IndexMap<Id<String>, Id<String>>,
        pub session_state: State<Session>,
    }

    fn method_response_ser<S>(
        value: &IndexMap<String, Value>,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        let values = value.values().cloned().collect::<Vec<Value>>();
        values.serialize(serializer)
    }

    #[derive(Debug)]
    pub enum StateChange<OBJ: objects::Object> {
        Created(Id<OBJ>),
        Updated(Id<OBJ>),
        Destroyed(Id<OBJ>),
    }

    /// Server state with only one mailbox (INBOX).
    pub struct ServerState {
        pub envelopes: IndexMap<Id<email::EmailObject>, Mail>,
        pub identities: IndexMap<Id<identity::Identity>, identity::Identity>,
        pub mailboxes: IndexMap<Id<mailbox::MailboxObject>, mailbox::MailboxObject>,
        pub identity_state: State<identity::Identity>,
        pub email_state: State<email::EmailObject>,
        pub mailbox_state: State<mailbox::MailboxObject>,
        pub email_state_changes:
            IndexMap<State<email::EmailObject>, StateChange<email::EmailObject>>,
        pub session: Session,
    }

    impl ServerState {
        pub fn new(addr: &SocketAddr) -> Self {
            let mut session = serde_json::from_str::<Session>(&format!(
                r#"{{
     "capabilities": {{
       "urn:ietf:params:jmap:core": {{
         "maxSizeUpload": 50000000,
         "maxConcurrentUpload": 8,
         "maxSizeRequest": 10000000,
         "maxConcurrentRequests": 8,
         "maxCallsInRequest": 32,
         "maxObjectsInGet": 256,
         "maxObjectsInSet": 128,
         "collationAlgorithms": [
           "i;ascii-numeric",
           "i;ascii-casemap",
           "i;unicode-casemap"
         ]
       }},
       "urn:ietf:params:jmap:mail": {{}},
       "urn:ietf:params:jmap:contacts": {{}}
     }},
     "accounts": {{
       "A13824": {{
         "name": "user@example.com",
         "isPersonal": true,
         "isReadOnly": false,
         "accountCapabilities": {{
           "urn:ietf:params:jmap:mail": {{
             "maxMailboxesPerEmail": null,
             "maxMailboxDepth": 10
           }}
         }}
       }}
     }},
     "primaryAccounts": {{
       "urn:ietf:params:jmap:mail": "A13824",
       "urn:ietf:params:jmap:contacts": "A13824"
     }},
     "username": "john@example.com",
     "apiUrl": "http://{host}:{port}/api/",
     "downloadUrl": "http://{host}:{port}/download/{{accountId}}/{{blobId}}/{{name}}?accept={{type}}",
     "uploadUrl": "http://{host}:{port}/upload/{{accountId}}/",
     "eventSourceUrl": "http://{host}:{port}/eventsource/?types={{types}}&closeafter={{closeafter}}&ping={{ping}}",
     "state": "75128aab4b1b"
}}"#,
                host = addr.ip(),
                port = addr.port()
            ))
            .unwrap();
            session.state = State::new_random();

            Self {
                envelopes: indexmap::indexmap! {},
                identities: indexmap::indexmap! {},
                mailboxes: {
                    let inbox_id = Id::new_random();
                    let inbox = mailbox::MailboxObject {
                        id: inbox_id.clone(),
                        is_subscribed: true,
                        my_rights: Default::default(),
                        name: "Inbox".to_string(),
                        parent_id: None,
                        role: Some("inbox".to_string()),
                        sort_order: 0,
                        total_emails: 0,
                        total_threads: 0,
                        unread_emails: 0,
                        unread_threads: 0,
                    };
                    indexmap::indexmap! {
                        inbox_id => inbox
                    }
                },
                identity_state: State::new_random(),
                email_state: State::new_random(),
                mailbox_state: State::new_random(),
                email_state_changes: indexmap::indexmap! {},
                session,
            }
        }

        pub fn api(&mut self, request: &str) -> ClientMethodResponse {
            let request: ClientRequest = serde_json::from_str(request).unwrap();

            let mut responses = ClientMethodResponse {
                method_responses: indexmap::indexmap! {},
                created_ids: indexmap::indexmap! {},
                session_state: self.session.state.clone(),
            };

            #[derive(Clone, Copy, Debug)]
            enum MethodCallType {
                Get,
                Changes,
                Set,
                Copy,
                Query,
                QueryChanges,
            }

            fn parse_type(value: &str) -> (String, MethodCallType) {
                let mut iter = value.split('/');
                let name = iter.next().unwrap().to_string();
                let r#type = match iter.next().unwrap() {
                    "get" => MethodCallType::Get,
                    "changes" => MethodCallType::Changes,
                    "set" => MethodCallType::Set,
                    "copy" => MethodCallType::Copy,
                    "query" => MethodCallType::Query,
                    "queryChanges" => MethodCallType::QueryChanges,
                    other => panic!("Unexpected method call type: {}", other),
                };
                (name, r#type)
            }

            eprintln!("Received API call request {:?}", request);
            for [r#type, body, id] in request.method_calls {
                let r#type: String = serde_json::value::from_value(r#type).unwrap();
                let id: String = serde_json::value::from_value(id).unwrap();
                let (object_type_name, method_type) = parse_type(&r#type);
                eprintln!(
                    "Processing method call {}: object_type_name = {:?}, method_type ={:?}, id = \
                     {:?}",
                    r#type, object_type_name, method_type, id
                );
                match method_type {
                    MethodCallType::Get => match object_type_name.as_str() {
                        "Identity" => {
                            let get: Get<identity::Identity> =
                                serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Identity/get object {:?}", get);
                            let response: GetResponse<identity::Identity> = GetResponse {
                                account_id: get.account_id,
                                state: self.identity_state.clone(),
                                list: self.identities.values().cloned().collect(),
                                not_found: vec![],
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        "Mailbox" => {
                            let get: Get<mailbox::MailboxObject> =
                                serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Mailbox/get object {:?}", get);
                            let response: GetResponse<mailbox::MailboxObject> = GetResponse {
                                account_id: get.account_id,
                                state: self.mailbox_state.clone(),
                                list: self.mailboxes.values().cloned().collect(),
                                not_found: vec![],
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        "Email" => {
                            let get: Get<email::EmailObject> =
                                serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Email/get object {:?}", get);
                            let mut not_found = vec![];
                            eprintln!("get.ids = {:?}", &get.ids);
                            let list = match get.ids {
                                None => {
                                    // Return all.
                                    self.envelopes
                                        .values()
                                        .map(Into::into)
                                        .zip(self.envelopes.keys())
                                        .map(|(mut email, id): (email::EmailObject, &Id<_>)| {
                                            email.id = id.clone();
                                            email.mailbox_ids.insert(
                                                self.mailboxes.keys().next().unwrap().clone(),
                                                true,
                                            );

                                            email
                                        })
                                        .collect()
                                }
                                Some(Argument::Value(email_ids)) => {
                                    let mut list = Vec::with_capacity(email_ids.len());
                                    for email_id in email_ids {
                                        if let Some(mail) = self.envelopes.get(&email_id) {
                                            let mut email_obj: email::EmailObject = mail.into();
                                            email_obj.id = email_id.clone();
                                            email_obj.mailbox_ids.insert(
                                                self.mailboxes.keys().next().unwrap().clone(),
                                                true,
                                            );
                                            list.push(email_obj);
                                        } else {
                                            not_found.push(email_id);
                                        }
                                    }
                                    list
                                }
                                Some(Argument::ResultReference {
                                    result_of,
                                    name,
                                    path,
                                }) => {
                                    // [ref:TODO]: `path` is a JSON pointer, so we should parse it
                                    // as such. Example values: "/created", "/list/*/threadId"
                                    //
                                    // [ref:TODO]: if result reference evaluation fails, respond
                                    // with `invalidResultReference`:
                                    //
                                    // > If any result reference fails to resolve, the whole method
                                    // > MUST be rejected with an "invalidResultReference" error.
                                    // > If an arguments object contains the same argument name in
                                    // > normal and referenced form (e.g., "foo" and "#foo"), the
                                    // > method MUST return an "invalidArguments" error.
                                    let prev_response =
                                        responses.method_responses.get(&result_of).unwrap();
                                    assert_eq!(prev_response[0].as_str().unwrap(), &name);
                                    prev_response[1][path.trim_matches('/')]
                                        .as_array()
                                        .unwrap()
                                        .iter()
                                        .filter_map(|id| {
                                            let email_id =
                                                serde_json::from_value::<Id<email::EmailObject>>(
                                                    id.clone(),
                                                )
                                                .unwrap();
                                            if let Some(mail) = self.envelopes.get(&email_id) {
                                                let mut email_obj: email::EmailObject = mail.into();
                                                email_obj.id = email_id.clone();
                                                email_obj.mailbox_ids.insert(
                                                    self.mailboxes.keys().next().unwrap().clone(),
                                                    true,
                                                );
                                                Some(email_obj)
                                            } else {
                                                not_found.push(email_id);
                                                None
                                            }
                                        })
                                        .collect::<Vec<email::EmailObject>>()
                                }
                            };
                            let response: GetResponse<email::EmailObject> = GetResponse {
                                account_id: get.account_id,
                                state: self.email_state.clone(),
                                list,
                                not_found,
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        other => panic!("other type name {}", other),
                    },
                    MethodCallType::Changes => match object_type_name.as_str() {
                        "Email" => {
                            let changes: Changes<email::EmailObject> =
                                serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Email/changes object {:?}", changes);
                            let Some(pos) = self
                                .email_state_changes
                                .keys()
                                .position(|k| *k == changes.since_state)
                            else {
                                responses.method_responses.insert(
                                    id.clone(),
                                    serde_json::json! {{ "type": "cannotCalculateChanges" }},
                                );
                                continue;
                            };
                            let mut created = IndexSet::new();
                            let mut updated = IndexSet::new();
                            let mut destroyed = IndexSet::new();
                            for (_, change) in &self.email_state_changes[pos..] {
                                match change {
                                    StateChange::Created(id) => {
                                        _ = created.insert(id.clone());
                                    }
                                    StateChange::Updated(id) => {
                                        if !created.contains(id) {
                                            _ = updated.insert(id.clone());
                                        }
                                    }
                                    StateChange::Destroyed(id) => {
                                        created.shift_remove(id);
                                        updated.shift_remove(id);
                                        destroyed.insert(id.clone());
                                    }
                                }
                            }
                            let response: ChangesResponse<email::EmailObject> = ChangesResponse {
                                account_id: changes.account_id,
                                old_state: changes.since_state.clone(),
                                new_state: self.email_state.clone(),
                                has_more_changes: false,
                                created: created.into_iter().collect(),
                                updated: updated.into_iter().collect(),
                                destroyed: destroyed.into_iter().collect(),
                                _ph: std::marker::PhantomData,
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        other => panic!("other type name {}", other),
                    },
                    MethodCallType::Set => match object_type_name.as_str() {
                        "Identity" => {
                            let set = serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Identity/set object {:?}", set);
                            let Set {
                                account_id,
                                if_in_state,
                                create,
                                update,
                                destroy,
                            }: Set<identity::Identity> = set;
                            let mut created = indexmap::indexmap! {};
                            let updated = indexmap::indexmap! {};
                            let destroyed = vec![];
                            let mut update_state = false;
                            if let Some(ref if_in_state) = if_in_state {
                                if *if_in_state != self.identity_state {
                                    unimplemented!();
                                }
                            }
                            for (id, mut obj) in create.unwrap_or_default() {
                                update_state = true;
                                let argument::Argument::Value(id) = id else {
                                    unreachable!();
                                };
                                obj.id = id.clone();
                                self.identities.insert(id.clone(), obj.clone());
                                created.insert(id, obj);
                            }
                            assert_eq!(update.unwrap_or_default().len(), 0);
                            assert_eq!(destroy.unwrap_or_default().len(), 0);
                            // for _ in update.unwrap_or_default() {
                            //     unimplemented!();
                            // }
                            // for _ in destroy.unwrap_or_default() {
                            //     unimplemented!();
                            // }
                            let old_state = if update_state {
                                Some(std::mem::replace(
                                    &mut self.identity_state,
                                    State::new_random(),
                                ))
                            } else {
                                None
                            };
                            let new_state = self.identity_state.clone();
                            let response: SetResponse<identity::Identity> = SetResponse {
                                account_id,
                                old_state,
                                new_state,
                                created: if created.is_empty() {
                                    None
                                } else {
                                    Some(created)
                                },
                                updated: if updated.is_empty() {
                                    None
                                } else {
                                    Some(updated)
                                },
                                destroyed: if destroyed.is_empty() {
                                    None
                                } else {
                                    Some(destroyed)
                                },
                                not_created: None,
                                not_updated: None,
                                not_destroyed: None,
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        "Mailbox" => {
                            let set = serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Mailbox/set object {:?}", set);
                            let Set {
                                account_id,
                                if_in_state,
                                create,
                                update,
                                destroy,
                            }: Set<mailbox::MailboxObject> = set;
                            let mut created = indexmap::indexmap! {};
                            let updated = indexmap::indexmap! {};
                            let destroyed = vec![];
                            let mut update_state = false;
                            if let Some(ref if_in_state) = if_in_state {
                                if *if_in_state != self.mailbox_state {
                                    unimplemented!();
                                }
                            }
                            for (id, mut obj) in create.unwrap_or_default() {
                                update_state = true;
                                let argument::Argument::Value(id) = id else {
                                    unreachable!();
                                };
                                obj.id = id.clone();
                                self.mailboxes.insert(id.clone(), obj.clone());
                                created.insert(id, obj);
                            }
                            assert_eq!(update.unwrap_or_default().len(), 0);
                            assert_eq!(destroy.unwrap_or_default().len(), 0);
                            // for _ in update.unwrap_or_default() {
                            //     unimplemented!();
                            // }
                            // for _ in destroy.unwrap_or_default() {
                            //     unimplemented!();
                            // }
                            let old_state = if update_state {
                                Some(std::mem::replace(
                                    &mut self.mailbox_state,
                                    State::new_random(),
                                ))
                            } else {
                                None
                            };
                            let new_state = self.mailbox_state.clone();
                            let response: SetResponse<mailbox::MailboxObject> = SetResponse {
                                account_id,
                                old_state,
                                new_state,
                                created: if created.is_empty() {
                                    None
                                } else {
                                    Some(created)
                                },
                                updated: if updated.is_empty() {
                                    None
                                } else {
                                    Some(updated)
                                },
                                destroyed: if destroyed.is_empty() {
                                    None
                                } else {
                                    Some(destroyed)
                                },
                                not_created: None,
                                not_updated: None,
                                not_destroyed: None,
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        other => panic!("other type name {}", other),
                    },
                    MethodCallType::Copy => unimplemented!(),
                    MethodCallType::Query => match object_type_name.as_str() {
                        "Email" => {
                            let query: email::EmailQuery =
                                serde_json::value::from_value(body).unwrap();
                            eprintln!("Parsed Email/query object {:?}", query);
                            let response: QueryResponse<email::EmailObject> = QueryResponse {
                                account_id: query.query_call.account_id,
                                query_state: String::new(),
                                can_calculate_changes: false,
                                position: 0,
                                ids: vec![],
                                total: if query.query_call.calculate_total {
                                    Some(self.envelopes.len().try_into().unwrap_or_default())
                                } else {
                                    None
                                },
                                limit: None,
                                _ph: std::marker::PhantomData,
                            };
                            eprintln!("Sending {id} {} response: {:?}", r#type, response);
                            responses
                                .method_responses
                                .insert(id.clone(), serde_json::json! {[r#type, response, id]});
                        }
                        other => panic!("other type name {}", other),
                    },
                    MethodCallType::QueryChanges => unimplemented!(),
                }
            }
            eprintln!("Sending API call responses: {:?}", responses);
            responses
        }

        pub fn insert_email(&mut self, new: Mail) {
            let id: Id<email::EmailObject> = Id::new_random();
            let old_state = std::mem::replace(&mut self.email_state, State::new_random());
            self.envelopes.insert(id.clone(), new);
            self.email_state_changes
                .insert(old_state, StateChange::Created(id));
        }

        pub fn destroy_email(&mut self, id: Id<email::EmailObject>) {
            let old_state = std::mem::replace(&mut self.email_state, State::new_random());
            self.envelopes.swap_remove(&id);
            self.email_state_changes
                .insert(old_state, StateChange::Destroyed(id));
        }
    }

    #[derive(Debug)]
    pub enum ServerEvent {
        New(Mail),
        Destroy(Id<email::EmailObject>),
        Quit,
    }

    pub struct JmapServerStream {
        pub tcp_listener: Async<TcpListener>,
        pub event_receiver: UnboundedReceiver<ServerEvent>,
        pub state: Arc<Mutex<ServerState>>,
        pub addr: SocketAddr,
    }

    impl JmapServerStream {
        pub fn new(
            tcp_listener: Async<TcpListener>,
            event_receiver: UnboundedReceiver<ServerEvent>,
            addr: SocketAddr,
        ) -> Self {
            let state = Arc::new(Mutex::new(ServerState::new(&addr)));
            Self {
                tcp_listener,
                event_receiver,
                state,
                addr,
            }
        }

        pub async fn loop_handler(self) -> Result<()> {
            let Self {
                tcp_listener,
                event_receiver,
                state,
                addr: _,
            } = self;

            let mut event_receiver = Box::pin(event_receiver.into_future());
            let mut accept = Box::pin(tcp_listener.accept());
            let mut buf = vec![0; 64 * 1024];
            loop {
                match future::select(accept.as_mut(), event_receiver.as_mut()).await {
                    Either::Left((request, _)) => {
                        accept = Box::pin(tcp_listener.accept());
                        let (mut tcp_stream, _socket_addr) = request?;
                        let bytes = tcp_stream.read(&mut buf).await?;
                        let lossy = String::from_utf8_lossy(&buf[..bytes]);
                        eprintln!("loop_handler read:\n{}", lossy);
                        tcp_stream.write_all(b"HTTP/1.1 200 OK\r\n").await?;
                        let response = if lossy.starts_with("GET /.well-known/jmap") {
                            // Respond with Session resource.
                            serde_json::to_string(&state.lock().unwrap().session)
                                .unwrap()
                                .into_bytes()
                        } else if lossy.starts_with("POST /api/") {
                            serde_json::to_string(
                                &state
                                    .lock()
                                    .unwrap()
                                    .api(lossy.lines().rev().nth(0).unwrap()),
                            )
                            .unwrap()
                            .into_bytes()
                        } else {
                            unimplemented!()
                        };
                        tcp_stream
                            .write_all(
                                format!("Content-Length: {}\r\n\r\n", response.len()).as_bytes(),
                            )
                            .await?;
                        tcp_stream.write_all(&response).await?;
                        tcp_stream.flush().await?;
                    }
                    Either::Right(((event, stream), _)) => {
                        *event_receiver = stream.into_future();
                        let Some(event) = event else {
                            continue;
                        };
                        match event {
                            ServerEvent::Quit => {
                                return Ok(());
                            }
                            ServerEvent::New(mail) => {
                                let mut state = state.lock().unwrap();
                                state.insert_email(mail);
                            }
                            ServerEvent::Destroy(mail_id) => {
                                let mut state = state.lock().unwrap();
                                state.destroy_email(mail_id);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(feature = "jmap")]
mod tests {
    use std::{
        net::TcpListener,
        sync::{Arc, Mutex},
    };

    use futures::{channel::mpsc::unbounded, executor::block_on, StreamExt};
    use melib::{
        backends::prelude::*,
        jmap::*,
        utils::logging::{LogLevel, StderrLogger},
        Mail,
    };
    use tempfile::TempDir;

    use super::server::*;

    /// Test that `JmapType::refresh` returns the expected `Refresh` events when
    /// altering the mail store in the jmap server.
    pub(crate) fn run_jmap_refresh() {
        let mut _logger = StderrLogger::new(LogLevel::TRACE);
        let temp_dir = TempDir::new().unwrap();
        let backend_event_queue =
            Arc::new(Mutex::new(std::collections::VecDeque::with_capacity(16)));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                eprintln!("BackendEventConsumer: ah {:?} be {:?}", ah, be);
                backend_event_queue.lock().unwrap().push_back((ah, be));
            }))
        };

        for var in [
            "HOME",
            "XDG_CACHE_HOME",
            "XDG_STATE_HOME",
            "XDG_CONFIG_DIRS",
            "XDG_CONFIG_HOME",
            "XDG_DATA_DIRS",
            "XDG_DATA_HOME",
        ] {
            std::env::remove_var(var);
        }
        for (var, dir) in [
            ("HOME", temp_dir.path().to_path_buf()),
            ("XDG_CACHE_HOME", temp_dir.path().join(".cache")),
            ("XDG_STATE_HOME", temp_dir.path().join(".local/state")),
            ("XDG_CONFIG_HOME", temp_dir.path().join(".config")),
            ("XDG_DATA_HOME", temp_dir.path().join(".local/share")),
        ] {
            std::fs::create_dir_all(&dir).unwrap_or_else(|err| {
                panic!("Could not create {} path, {}: {}", var, dir.display(), err);
            });
            std::env::set_var(var, &dir);
        }

        let listener = TcpListener::bind(("0.0.0.0", 0)).unwrap();
        let local_addr = listener.local_addr().unwrap();
        let account_conf = AccountSettings {
            name: "test".to_string(),
            root_mailbox: "INBOX".to_string(),
            format: "jmap".to_string(),
            identity: "user@example.com".to_string(),
            extra_identities: vec![],
            read_only: false,
            display_name: None,
            order: Default::default(),
            subscribed_mailboxes: vec![],
            mailboxes: indexmap::indexmap! {},
            manual_refresh: false,
            extra: indexmap::indexmap! {
                "server_url".to_string() => format!("http://{}:{}", local_addr.ip(), local_addr.port()),
                "server_username".to_string() => "user".to_string(),
                "server_password".to_string() => "password".to_string(),
                "use_token".to_string() => "true".to_string(),
            },
        };

        let mut jmap =
            JmapType::new(&account_conf, Default::default(), backend_event_consumer).unwrap();
        let (server_event_sender, server_event_receiver) = unbounded();
        let server = JmapServerStream::new(
            smol::Async::new(listener).unwrap(),
            server_event_receiver,
            local_addr,
        );
        let server_state = server.state.clone();
        std::thread::spawn(move || {
            block_on(server.loop_handler()).unwrap();
        });
        eprintln!(
            "Assert that account is not online because we have not performed any operations yet."
        );
        assert_eq!(
            &block_on(jmap.is_online().unwrap()).unwrap_err().to_string(),
            "Account is uninitialised."
        );
        let mailboxes = block_on(jmap.mailboxes().unwrap()).unwrap();
        assert_eq!(
            mailboxes.len(),
            1,
            "Expected one mailbox, Inbox. Got: {:?}",
            mailboxes
        );
        let inbox_hash = *mailboxes.keys().next().unwrap();
        eprintln!("Inbox hash: {:?}", inbox_hash);
        {
            let events = backend_event_queue
                .lock()
                .unwrap()
                .drain(..)
                .collect::<Vec<_>>();
            assert_eq!(
                events.len(),
                0,
                "Unexpected events without having performed any changes: {:?}",
                events
            );
        }
        eprintln!("Refresh account and assert that no events arrive.");
        block_on(jmap.refresh(inbox_hash).unwrap()).unwrap();
        {
            let events = backend_event_queue
                .lock()
                .unwrap()
                .drain(..)
                .collect::<Vec<_>>();
            assert_eq!(
                events.len(),
                0,
                "Unexpected events after refreshing without having performed any changes: {:?}",
                events
            );
        }

        eprintln!(
            "Add new e-mail on server store and assert that a refresh results in a Refresh Create \
             event."
        );
        let new_mail = Mail::new(
            br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#
            .to_vec(),
            None,
        )
        .unwrap();
        let fetch_result: Vec<_> = block_on(jmap.fetch(inbox_hash).unwrap().collect::<Vec<_>>());
        for res in fetch_result {
            let res = res.unwrap();
            assert_eq!(res, vec![]);
        }
        server_event_sender
            .unbounded_send(ServerEvent::New(new_mail))
            .unwrap();
        block_on(jmap.refresh(inbox_hash).unwrap()).unwrap();
        let env_1 = {
            let events = backend_event_queue
                .lock()
                .unwrap()
                .drain(..)
                .collect::<Vec<_>>();
            assert_eq!(
                events.len(),
                1,
                "Expected one Refresh Create event: {:?}",
                events
            );
            let backend_event = events.into_iter().next().unwrap().1;
            let BackendEvent::Refresh(refresh_event) = backend_event else {
                panic!("Expected Refresh event, got: {:?}", backend_event);
            };
            let RefreshEventKind::Create(env) = refresh_event.kind else {
                panic!("Expected Create event, got: {:?}", refresh_event);
            };
            env
        };
        let fetch_result: Vec<_> = block_on(jmap.fetch(inbox_hash).unwrap().collect::<Vec<_>>());
        for res in fetch_result {
            let res = res.unwrap();
            assert_eq!(res.len(), 1);
            assert_eq!(res[0].hash(), env_1.hash());
        }
        eprintln!(
            "Destroy e-mail on server store and assert that a refresh results in a Refresh Remove \
             event."
        );
        let id_1 = server_state
            .lock()
            .unwrap()
            .envelopes
            .keys()
            .next()
            .unwrap()
            .clone();
        server_event_sender
            .unbounded_send(ServerEvent::Destroy(id_1))
            .unwrap();
        block_on(jmap.refresh(inbox_hash).unwrap()).unwrap();
        {
            let events = backend_event_queue
                .lock()
                .unwrap()
                .drain(..)
                .collect::<Vec<_>>();
            assert_eq!(
                events.len(),
                1,
                "Expected one Refresh Remove event: {:?}",
                events
            );
            let backend_event = events.into_iter().next().unwrap().1;
            let BackendEvent::Refresh(refresh_event) = backend_event else {
                panic!("Expected Refresh event, got: {:?}", backend_event);
            };
            let RefreshEventKind::Remove(env_hash) = refresh_event.kind else {
                panic!("Expected Remove event, got: {:?}", refresh_event);
            };
            assert_eq!(env_hash, env_1.hash());
        }
        server_event_sender
            .unbounded_send(ServerEvent::Quit)
            .unwrap();
    }
}
