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

use crate::backends::*;
use crate::conf::AccountSettings;
use crate::email::*;
use crate::error::{MeliError, Result};
use futures::lock::Mutex as FutureMutex;
use isahc::prelude::HttpClient;
use isahc::ResponseExt;
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;
use std::str::FromStr;
use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;

#[macro_export]
macro_rules! _impl {
        ($(#[$outer:meta])*$field:ident : $t:ty) => {
            $(#[$outer])*
            pub fn $field(mut self, new_val: $t) -> Self {
                self.$field = new_val;
                self
            }
        };
        (get_mut $(#[$outer:meta])*$method:ident, $field:ident : $t:ty) => {
            $(#[$outer])*
            pub fn $method(&mut self) -> &mut $t {
                &mut self.$field
            }
        };
        (get $(#[$outer:meta])*$method:ident, $field:ident : $t:ty) => {
            $(#[$outer])*
            pub fn $method(&self) -> &$t {
                &self.$field
            }
        }
    }

pub mod operations;
use operations::*;

pub mod connection;
use connection::*;

pub mod protocol;
use protocol::*;

pub mod rfc8620;
use rfc8620::*;

pub mod objects;
use objects::*;

pub mod mailbox;
use mailbox::*;

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<String>,
    headers: Option<String>,
    body: Option<String>,
    flags: Option<Flag>,
}

#[derive(Debug, Clone)]
pub struct JmapServerConf {
    pub server_hostname: String,
    pub server_username: String,
    pub server_password: String,
    pub server_port: u16,
    pub danger_accept_invalid_certs: bool,
}

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
                "Configuration error ({}): JMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    MeliError::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

impl JmapServerConf {
    pub fn new(s: &AccountSettings) -> Result<Self> {
        Ok(JmapServerConf {
            server_hostname: get_conf_val!(s["server_hostname"])?.to_string(),
            server_username: get_conf_val!(s["server_username"])?.to_string(),
            server_password: get_conf_val!(s["server_password"])?.to_string(),
            server_port: get_conf_val!(s["server_port"], 443)?,
            danger_accept_invalid_certs: get_conf_val!(s["danger_accept_invalid_certs"], false)?,
        })
    }
}

struct IsSubscribedFn(Box<dyn Fn(&str) -> bool + Send + Sync>);

impl std::fmt::Debug for IsSubscribedFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "IsSubscribedFn Box")
    }
}

impl std::ops::Deref for IsSubscribedFn {
    type Target = Box<dyn Fn(&str) -> bool + Send + Sync>;
    fn deref(&self) -> &Box<dyn Fn(&str) -> bool + Send + Sync> {
        &self.0
    }
}
macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
                "Configuration error ({}): JMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    MeliError::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

#[derive(Debug, Default)]
pub struct Store {
    byte_cache: HashMap<EnvelopeHash, EnvelopeCache>,
    id_store: HashMap<EnvelopeHash, Id>,
    blob_id_store: HashMap<EnvelopeHash, Id>,
}

#[derive(Debug)]
pub struct JmapType {
    account_name: String,
    account_hash: AccountHash,
    online: Arc<FutureMutex<(Instant, Result<()>)>>,
    is_subscribed: Arc<IsSubscribedFn>,
    server_conf: JmapServerConf,
    connection: Arc<FutureMutex<JmapConnection>>,
    store: Arc<RwLock<Store>>,
    tag_index: Arc<RwLock<BTreeMap<u64, String>>>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, JmapMailbox>>>,
}

impl MailBackend for JmapType {
    fn capabilities(&self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            is_async: true,
            is_remote: true,
            supports_search: true,
            extensions: None,
            supports_tags: true,
            supports_submission: false,
        };
        CAPABILITIES
    }

    fn is_online(&self) -> ResultFuture<()> {
        let online = self.online.clone();
        Ok(Box::pin(async move {
            //match timeout(std::time::Duration::from_secs(3), connection.lock()).await {
            let online_lck = online.lock().await;
            if online_lck.1.is_err()
                && Instant::now().duration_since(online_lck.0) >= std::time::Duration::new(2, 0)
            {
                //let _ = self.mailboxes();
            }
            online_lck.1.clone()
        }))
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let mailboxes = self.mailboxes.clone();
        let store = self.store.clone();
        let tag_index = self.tag_index.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async_stream::try_stream! {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let res = protocol::fetch(
                &conn,
                &store,
                &tag_index,
                &mailboxes,
                mailbox_hash,
            ).await?;
            yield res;
        }))
    }

    fn refresh(&mut self, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn watch(&self) -> ResultFuture<()> {
        Err(MeliError::from("JMAP watch for updates is unimplemented"))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let mailboxes = self.mailboxes.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            if mailboxes.read().unwrap().is_empty() {
                let new_mailboxes = debug!(protocol::get_mailboxes(&conn).await)?;
                *mailboxes.write().unwrap() = new_mailboxes;
            }

            let ret = mailboxes
                .read()
                .unwrap()
                .iter()
                .filter(|(_, f)| f.is_subscribed)
                .map(|(&h, f)| (h, BackendMailbox::clone(f) as Mailbox))
                .collect();

            Ok(ret)
        }))
    }

    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        Ok(Box::new(JmapOp::new(
            hash,
            self.connection.clone(),
            self.store.clone(),
        )))
    }

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let mailboxes = self.mailboxes.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            /*
             * 1. upload binary blob, get blobId
             * 2. Email/import
             */
            let mut res = conn
                .client
                .post_async(
                    &upload_request_format(&conn.session, conn.mail_account_id()),
                    bytes,
                )
                .await?;

            let mailbox_id: String = {
                let mailboxes_lck = mailboxes.read().unwrap();
                if let Some(mailbox) = mailboxes_lck.get(&mailbox_hash) {
                    mailbox.id.clone()
                } else {
                    return Err(MeliError::new(format!(
                        "Mailbox with hash {} not found",
                        mailbox_hash
                    )));
                }
            };
            let res_text = res.text_async().await?;

            let upload_response: UploadResponse = serde_json::from_str(&res_text)?;
            let mut req = Request::new(conn.request_no.clone());
            let creation_id = "1".to_string();
            let mut email_imports = HashMap::default();
            let mut mailbox_ids = HashMap::default();
            mailbox_ids.insert(mailbox_id, true);
            email_imports.insert(
                creation_id.clone(),
                EmailImport::new()
                    .blob_id(upload_response.blob_id)
                    .mailbox_ids(mailbox_ids),
            );

            let import_call: ImportCall = ImportCall::new()
                .account_id(conn.mail_account_id().to_string())
                .emails(email_imports);

            req.add_call(&import_call);
            let mut res = conn
                .client
                .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
                .await?;
            let res_text = res.text_async().await?;

            let mut v: MethodResponse = serde_json::from_str(&res_text)?;
            let m = ImportResponse::try_from(v.method_responses.remove(0)).or_else(|err| {
                let ierr: Result<ImportError> =
                    serde_json::from_str(&res_text).map_err(|err| err.into());
                if let Ok(err) = ierr {
                    Err(MeliError::new(format!("Could not save message: {:?}", err)))
                } else {
                    Err(err.into())
                }
            })?;

            if let Some(err) = m.not_created.get(&creation_id) {
                return Err(MeliError::new(format!("Could not save message: {:?}", err)));
            }
            Ok(())
        }))
    }

    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        Some(self.tag_index.clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn search(
        &self,
        q: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        let connection = self.connection.clone();
        let filter = if let Some(mailbox_hash) = mailbox_hash {
            let mailbox_id = self.mailboxes.read().unwrap()[&mailbox_hash].id.clone();

            let mut f = Filter::Condition(
                EmailFilterCondition::new()
                    .in_mailbox(Some(mailbox_id))
                    .into(),
            );
            f &= Filter::<EmailFilterCondition, EmailObject>::from(q);
            f
        } else {
            Filter::<EmailFilterCondition, EmailObject>::from(q)
        };

        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let email_call: EmailQuery = EmailQuery::new(
                Query::new()
                    .account_id(conn.mail_account_id().to_string())
                    .filter(Some(filter))
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
            let ret = ids
                .into_iter()
                .map(|id| {
                    use std::hash::Hasher;
                    let mut h = std::collections::hash_map::DefaultHasher::new();
                    h.write(id.as_bytes());
                    h.finish()
                })
                .collect();
            Ok(ret)
        }))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(MeliError::new("Unimplemented."))
    }

    fn create_mailbox(
        &mut self,
        _path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(MeliError::new("Unimplemented."))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()> {
        let mailboxes = self.mailboxes.clone();
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let (source_mailbox_id, destination_mailbox_id) = {
                let mailboxes_lck = mailboxes.read().unwrap();
                if !mailboxes_lck.contains_key(&source_mailbox_hash) {
                    return Err(MeliError::new(format!(
                        "Could not find source mailbox with hash {}",
                        source_mailbox_hash
                    )));
                }
                if !mailboxes_lck.contains_key(&destination_mailbox_hash) {
                    return Err(MeliError::new(format!(
                        "Could not find destination mailbox with hash {}",
                        destination_mailbox_hash
                    )));
                }

                (
                    mailboxes_lck[&source_mailbox_hash].id.clone(),
                    mailboxes_lck[&destination_mailbox_hash].id.clone(),
                )
            };
            let mut update_map: HashMap<String, Value> = HashMap::default();
            let mut ids: Vec<Id> = Vec::with_capacity(env_hashes.rest.len() + 1);
            let mut id_map: HashMap<Id, EnvelopeHash> = HashMap::default();
            let mut update_keywords: HashMap<String, Value> = HashMap::default();
            update_keywords.insert(
                format!("mailboxIds/{}", &destination_mailbox_id),
                serde_json::json!(true),
            );
            if move_ {
                update_keywords.insert(
                    format!("mailboxIds/{}", &source_mailbox_hash),
                    serde_json::json!(null),
                );
            }
            {
                let store_lck = store.read().unwrap();
                for env_hash in env_hashes.iter() {
                    if let Some(id) = store_lck.id_store.get(&env_hash) {
                        ids.push(id.clone());
                        id_map.insert(id.clone(), env_hash);
                        update_map.insert(id.clone(), serde_json::json!(update_keywords.clone()));
                    }
                }
            }
            let conn = connection.lock().await;

            let email_set_call: EmailSet = EmailSet::new(
                Set::<EmailObject>::new()
                    .account_id(conn.mail_account_id().to_string())
                    .update(Some(update_map)),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&email_set_call);

            let mut res = conn
                .client
                .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
                .await?;

            let res_text = res.text_async().await?;

            let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
            *conn.online_status.lock().await = (std::time::Instant::now(), Ok(()));
            let m = SetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            if let Some(ids) = m.not_updated {
                if !ids.is_empty() {
                    return Err(MeliError::new(format!(
                        "Could not update ids: {}",
                        ids.into_iter()
                            .map(|err| err.to_string())
                            .collect::<Vec<String>>()
                            .join(",")
                    )));
                }
            }
            Ok(())
        }))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()> {
        let mailboxes = self.mailboxes.clone();
        let store = self.store.clone();
        let account_hash = self.account_hash;
        let tag_index = self.tag_index.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mailbox_id = mailboxes.read().unwrap()[&mailbox_hash].id.clone();
            let mut update_map: HashMap<String, Value> = HashMap::default();
            let mut ids: Vec<Id> = Vec::with_capacity(env_hashes.rest.len() + 1);
            let mut id_map: HashMap<Id, EnvelopeHash> = HashMap::default();
            let mut update_keywords: HashMap<String, Value> = HashMap::default();
            for (flag, value) in flags.iter() {
                match flag {
                    Ok(f) => {
                        update_keywords.insert(
                            format!(
                                "keywords/{}",
                                match *f {
                                    Flag::DRAFT => "$draft",
                                    Flag::FLAGGED => "$flagged",
                                    Flag::SEEN => "$seen",
                                    Flag::REPLIED => "$answered",
                                    Flag::TRASHED => "$junk",
                                    Flag::PASSED => "$passed",
                                    _ => continue, //FIXME
                                }
                            ),
                            if *value {
                                serde_json::json!(true)
                            } else {
                                serde_json::json!(null)
                            },
                        );
                    }
                    Err(t) => {
                        update_keywords.insert(
                            format!("keywords/{}", t),
                            if *value {
                                serde_json::json!(true)
                            } else {
                                serde_json::json!(null)
                            },
                        );
                    }
                }
            }
            {
                let store_lck = store.read().unwrap();
                for hash in env_hashes.iter() {
                    if let Some(id) = store_lck.id_store.get(&hash) {
                        ids.push(id.clone());
                        id_map.insert(id.clone(), hash);
                        update_map.insert(id.clone(), serde_json::json!(update_keywords.clone()));
                    }
                }
            }
            let conn = connection.lock().await;

            let email_set_call: EmailSet = EmailSet::new(
                Set::<EmailObject>::new()
                    .account_id(conn.mail_account_id().to_string())
                    .update(Some(update_map)),
            );

            let mut req = Request::new(conn.request_no.clone());
            let prev_seq = req.add_call(&email_set_call);
            let email_call: EmailGet = EmailGet::new(
                Get::new()
                    .ids(Some(JmapArgument::Value(ids)))
                    .account_id(conn.mail_account_id().to_string())
                    .properties(Some(vec!["keywords".to_string()])),
            );

            req.add_call(&email_call);
            //debug!(serde_json::to_string(&req)?);
            let mut res = conn
                .client
                .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
                .await?;

            let res_text = res.text_async().await?;
            /*
             *{"methodResponses":[["Email/set",{"notUpdated":null,"notDestroyed":null,"oldState":"86","newState":"87","accountId":"u148940c7","updated":{"M045926eed54b11423918f392":{"id":"M045926eed54b11423918f392"}},"created":null,"destroyed":null,"notCreated":null},"m3"]],"sessionState":"cyrus-0;p-5;vfs-0"}
             */
            //debug!("res_text = {}", &res_text);
            let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
            *conn.online_status.lock().await = (std::time::Instant::now(), Ok(()));
            let m = SetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            if let Some(ids) = m.not_updated {
                return Err(MeliError::new(
                    ids.into_iter()
                        .map(|err| err.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                ));
            }

            let mut tag_index_lck = tag_index.write().unwrap();
            for (flag, value) in flags.iter() {
                match flag {
                    Ok(f) => {}
                    Err(t) => {
                        if *value {
                            tag_index_lck.insert(tag_hash!(t), t.clone());
                        }
                    }
                }
            }
            let e = GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
            let GetResponse::<EmailObject> { list, state, .. } = e;
            //debug!(&list);
            for envobj in list {
                let env_hash = id_map[&envobj.id];
                conn.add_refresh_event(RefreshEvent {
                    account_hash,
                    mailbox_hash,
                    kind: RefreshEventKind::NewFlags(
                        env_hash,
                        protocol::keywords_to_flags(envobj.keywords().keys().cloned().collect()),
                    ),
                });
            }
            Ok(())
        }))
    }
}

impl JmapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        let online = Arc::new(FutureMutex::new((
            std::time::Instant::now(),
            Err(MeliError::new("Account is uninitialised.")),
        )));
        let server_conf = JmapServerConf::new(s)?;

        let account_hash = {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::Hasher;
            let mut hasher = DefaultHasher::new();
            hasher.write(s.name.as_bytes());
            hasher.finish()
        };

        Ok(Box::new(JmapType {
            connection: Arc::new(FutureMutex::new(JmapConnection::new(
                &server_conf,
                account_hash,
                event_consumer,
                online.clone(),
            )?)),
            store: Arc::new(RwLock::new(Store::default())),
            tag_index: Arc::new(RwLock::new(Default::default())),
            mailboxes: Arc::new(RwLock::new(HashMap::default())),
            account_name: s.name.clone(),
            account_hash,
            online,
            is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            server_conf,
        }))
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"])?;
        get_conf_val!(s["server_password"])?;
        get_conf_val!(s["server_port"], 443)?;
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        Ok(())
    }
}
