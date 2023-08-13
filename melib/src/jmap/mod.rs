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
    collections::{BTreeSet, HashMap, HashSet},
    convert::TryFrom,
    pin::Pin,
    str::FromStr,
    sync::{Arc, Mutex, RwLock},
    time::{Duration, Instant},
};

use futures::{lock::Mutex as FutureMutex, Stream};
use isahc::{config::RedirectPolicy, AsyncReadResponseExt, HttpClient};
use serde_json::Value;
use smallvec::SmallVec;

use crate::{
    backends::*,
    conf::AccountSettings,
    email::*,
    error::{Error, ErrorKind, Result},
    utils::futures::{sleep, timeout},
    Collection,
};

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

pub const JMAP_CORE_CAPABILITY: &str = "urn:ietf:params:jmap:core";
pub const JMAP_MAIL_CAPABILITY: &str = "urn:ietf:params:jmap:mail";

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

pub fn deserialize_from_str<'de, T: serde::de::Deserialize<'de>>(s: &'de str) -> Result<T> {
    let jd = &mut serde_json::Deserializer::from_str(s);
    match serde_path_to_error::deserialize(jd) {
        Ok(v) => Ok(v),
        Err(err) => Err(Error::new(format!(
            "BUG: Could not deserialize server JSON response properly, please report this!\nError \
             {} at {}. Reply from server: {}",
            err,
            err.path(),
            &s
        ))
        .set_source(Some(Arc::new(err)))
        .set_kind(ErrorKind::Bug)),
    }
}

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<String>,
    // headers: Option<String>,
    // body: Option<String>,
    // flags: Option<Flag>,
}

#[derive(Debug, Clone)]
pub struct JmapServerConf {
    pub server_url: String,
    pub server_username: String,
    pub server_password: String,
    pub use_token: bool,
    pub danger_accept_invalid_certs: bool,
    pub timeout: Option<Duration>,
}

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            Error::new(format!(
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
                    Error::new(format!(
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
        let use_token: bool = get_conf_val!(s["use_token"], false)?;

        if use_token
            && !(s.extra.contains_key("server_password_command")
                ^ s.extra.contains_key("server_password"))
        {
            return Err(Error::new(format!(
                "({}) `use_token` use requires either the `server_password_command` set with a \
                 command that returns an Bearer token of your account, or `server_password` with \
                 the API Bearer token as a string. Consult documentation for guidance.",
                s.name,
            )));
        }
        Ok(Self {
            server_url: get_conf_val!(s["server_url"])?.to_string(),
            server_username: get_conf_val!(s["server_username"])?.to_string(),
            server_password: s.server_password()?,
            use_token,
            danger_accept_invalid_certs: get_conf_val!(s["danger_accept_invalid_certs"], false)?,
            timeout: get_conf_val!(s["timeout"], 16_u64).map(|t| {
                if t == 0 {
                    None
                } else {
                    Some(Duration::from_secs(t))
                }
            })?,
        })
    }
}

#[derive(Debug)]
pub struct Store {
    pub account_name: Arc<String>,
    pub account_hash: AccountHash,
    pub account_id: Arc<Mutex<Id<Account>>>,
    pub byte_cache: Arc<Mutex<HashMap<EnvelopeHash, EnvelopeCache>>>,
    pub id_store: Arc<Mutex<HashMap<EnvelopeHash, Id<EmailObject>>>>,
    pub reverse_id_store: Arc<Mutex<HashMap<Id<EmailObject>, EnvelopeHash>>>,
    pub blob_id_store: Arc<Mutex<HashMap<EnvelopeHash, Id<BlobObject>>>>,
    pub collection: Collection,
    pub mailboxes: Arc<RwLock<HashMap<MailboxHash, JmapMailbox>>>,
    pub mailboxes_index: Arc<RwLock<HashMap<MailboxHash, HashSet<EnvelopeHash>>>>,
    pub mailbox_state: Arc<Mutex<State<MailboxObject>>>,
    pub online_status: Arc<FutureMutex<(Instant, Result<()>)>>,
    pub is_subscribed: Arc<IsSubscribedFn>,
    pub core_capabilities: Arc<Mutex<rfc8620::CapabilitiesObject>>,
    pub event_consumer: BackendEventConsumer,
}

impl Store {
    pub fn add_envelope(&self, obj: EmailObject) -> Envelope {
        let mut flags = Flag::default();
        let mut labels: SmallVec<[TagHash; 8]> = SmallVec::new();
        let mut tag_lck = self.collection.tag_index.write().unwrap();
        for t in obj.keywords().keys() {
            match t.as_str() {
                "$draft" => {
                    flags |= Flag::DRAFT;
                }
                "$seen" => {
                    flags |= Flag::SEEN;
                }
                "$flagged" => {
                    flags |= Flag::FLAGGED;
                }
                "$answered" => {
                    flags |= Flag::REPLIED;
                }
                "$junk" | "$notjunk" => { /* ignore */ }
                _ => {
                    let tag_hash = TagHash::from_bytes(t.as_bytes());
                    tag_lck.entry(tag_hash).or_insert_with(|| t.to_string());
                    labels.push(tag_hash);
                }
            }
        }

        let id = obj.id.clone();
        let mailbox_ids = obj.mailbox_ids.clone();
        let blob_id = obj.blob_id.clone();
        drop(tag_lck);
        let mut ret: Envelope = obj.into();
        ret.set_flags(flags);
        ret.tags_mut().append(&mut labels);

        let mut id_store_lck = self.id_store.lock().unwrap();
        let mut reverse_id_store_lck = self.reverse_id_store.lock().unwrap();
        let mut blob_id_store_lck = self.blob_id_store.lock().unwrap();
        let mailboxes_lck = self.mailboxes.read().unwrap();
        let mut mailboxes_index_lck = self.mailboxes_index.write().unwrap();
        for (mailbox_id, _) in mailbox_ids {
            if let Some((mailbox_hash, _)) = mailboxes_lck.iter().find(|(_, m)| m.id == mailbox_id)
            {
                mailboxes_index_lck
                    .entry(*mailbox_hash)
                    .or_default()
                    .insert(ret.hash());
            }
        }
        reverse_id_store_lck.insert(id.clone(), ret.hash());
        id_store_lck.insert(ret.hash(), id);
        blob_id_store_lck.insert(ret.hash(), blob_id);
        ret
    }

    pub fn remove_envelope(
        &self,
        obj_id: Id<EmailObject>,
    ) -> Option<(EnvelopeHash, SmallVec<[MailboxHash; 8]>)> {
        let env_hash = self.reverse_id_store.lock().unwrap().remove(&obj_id)?;
        self.id_store.lock().unwrap().remove(&env_hash);
        self.blob_id_store.lock().unwrap().remove(&env_hash);
        self.byte_cache.lock().unwrap().remove(&env_hash);
        let mut mailbox_hashes = SmallVec::new();
        {
            let mut mailboxes_lck = self.mailboxes_index.write().unwrap();
            for (k, set) in mailboxes_lck.iter_mut() {
                if set.remove(&env_hash) {
                    mailbox_hashes.push(*k);
                }
            }
        }
        Some((env_hash, mailbox_hashes))
    }
}

#[derive(Debug)]
pub struct JmapType {
    server_conf: JmapServerConf,
    connection: Arc<FutureMutex<JmapConnection>>,
    store: Arc<Store>,
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
        let online = self.store.online_status.clone();
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout;
        Ok(Box::pin(async move {
            match timeout(timeout_dur, connection.lock()).await {
                Ok(_conn) => match timeout(timeout_dur, online.lock()).await {
                    Err(err) => Err(err),
                    Ok(lck) if lck.1.is_err() => lck.1.clone(),
                    _ => Ok(()),
                },
                Err(err) => Err(err),
            }
        }))
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async_stream::try_stream! {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let batch_size: u64 = conn.store.core_capabilities.lock().unwrap().max_objects_in_get;
            let mut fetch_state = protocol::EmailFetchState::Start { batch_size };
            loop {
                let res = fetch_state.fetch(
                    &conn,
                    &store,
                    mailbox_hash,
                ).await?;
                if res.is_empty() {
                    return;
                }
                yield res;
            }
        }))
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            conn.email_changes(mailbox_hash).await?;
            Ok(())
        }))
    }

    fn watch(&self) -> ResultFuture<()> {
        let connection = self.connection.clone();
        let store = self.store.clone();
        Ok(Box::pin(async move {
            {
                let mut conn = connection.lock().await;
                conn.connect().await?;
            }
            loop {
                {
                    let mailbox_hashes = {
                        store
                            .mailboxes
                            .read()
                            .unwrap()
                            .keys()
                            .cloned()
                            .collect::<SmallVec<[MailboxHash; 16]>>()
                    };
                    let conn = connection.lock().await;
                    for mailbox_hash in mailbox_hashes {
                        conn.email_changes(mailbox_hash).await?;
                    }
                }
                sleep(Duration::from_secs(60)).await;
            }
        }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            if store.mailboxes.read().unwrap().is_empty() {
                let new_mailboxes = debug!(protocol::get_mailboxes(&conn).await)?;
                *store.mailboxes.write().unwrap() = new_mailboxes;
            }

            let ret = store
                .mailboxes
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
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            /*
             * 1. upload binary blob, get blobId
             * 2. Email/import
             */
            let upload_url = { conn.session.lock().unwrap().upload_url.clone() };
            let mut res = conn
                .post_async(
                    Some(&upload_request_format(
                        upload_url.as_str(),
                        &conn.mail_account_id(),
                    )),
                    bytes,
                )
                .await?;

            let mailbox_id: Id<MailboxObject> = {
                let mailboxes_lck = store.mailboxes.read().unwrap();
                if let Some(mailbox) = mailboxes_lck.get(&mailbox_hash) {
                    mailbox.id.clone()
                } else {
                    return Err(Error::new(format!(
                        "Mailbox with hash {} not found",
                        mailbox_hash
                    )));
                }
            };
            let res_text = res.text().await?;

            let upload_response: UploadResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    *conn.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                    return Err(err);
                }
                Ok(s) => s,
            };
            let mut req = Request::new(conn.request_no.clone());
            let creation_id: Id<EmailObject> = "1".to_string().into();
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
                .account_id(conn.mail_account_id())
                .emails(email_imports);

            req.add_call(&import_call);
            let mut res = conn.post_async(None, serde_json::to_string(&req)?).await?;
            let res_text = res.text().await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    *conn.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                    return Err(err);
                }
                Ok(s) => s,
            };
            let m = ImportResponse::try_from(v.method_responses.remove(0)).map_err(|err| {
                let ierr: Result<ImportError> = deserialize_from_str(&res_text);
                if let Ok(err) = ierr {
                    Error::new(format!("Could not save message: {:?}", err))
                } else {
                    err
                }
            })?;

            if let Some(err) = m.not_created.get(&creation_id) {
                return Err(Error::new(format!("Could not save message: {:?}", err)));
            }
            Ok(())
        }))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn collection(&self) -> Collection {
        self.store.collection.clone()
    }

    fn search(
        &self,
        q: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        let filter = if let Some(mailbox_hash) = mailbox_hash {
            let mailbox_id = self.store.mailboxes.read().unwrap()[&mailbox_hash]
                .id
                .clone();

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
                    .account_id(conn.mail_account_id())
                    .filter(Some(filter))
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
            *store.online_status.lock().await = (std::time::Instant::now(), Ok(()));
            let m = QueryResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            let QueryResponse::<EmailObject> { ids, .. } = m;
            let ret = ids.into_iter().map(|id| id.into_hash()).collect();
            Ok(ret)
        }))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(Error::new(
            "Renaming mailbox is currently unimplemented for the JMAP backend.",
        ))
    }

    fn create_mailbox(
        &mut self,
        _path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(Error::new(
            "Creating mailbox is currently unimplemented for the JMAP backend.",
        ))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(Error::new(
            "Deleting a mailbox is currently unimplemented for the JMAP backend.",
        ))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox subscription is currently unimplemented for the JMAP backend.",
        ))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox permissions is currently unimplemented for the JMAP backend.",
        ))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let (source_mailbox_id, destination_mailbox_id) = {
                let mailboxes_lck = store.mailboxes.read().unwrap();
                if !mailboxes_lck.contains_key(&source_mailbox_hash) {
                    return Err(Error::new(format!(
                        "Could not find source mailbox with hash {}",
                        source_mailbox_hash
                    )));
                }
                if !mailboxes_lck.contains_key(&destination_mailbox_hash) {
                    return Err(Error::new(format!(
                        "Could not find destination mailbox with hash {}",
                        destination_mailbox_hash
                    )));
                }

                (
                    mailboxes_lck[&source_mailbox_hash].id.clone(),
                    mailboxes_lck[&destination_mailbox_hash].id.clone(),
                )
            };
            let mut update_map: HashMap<Id<EmailObject>, Value> = HashMap::default();
            let mut update_keywords: HashMap<String, Value> = HashMap::default();
            update_keywords.insert(
                format!("mailboxIds/{}", &destination_mailbox_id),
                serde_json::json!(true),
            );
            if move_ {
                update_keywords.insert(
                    format!("mailboxIds/{}", &source_mailbox_id),
                    serde_json::json!(null),
                );
            }
            {
                for env_hash in env_hashes.iter() {
                    if let Some(id) = store.id_store.lock().unwrap().get(&env_hash) {
                        // ids.push(id.clone());
                        // id_map.insert(id.clone(), env_hash);
                        update_map.insert(id.clone(), serde_json::json!(update_keywords.clone()));
                    }
                }
            }
            let conn = connection.lock().await;

            let email_set_call: EmailSet = EmailSet::new(
                Set::<EmailObject>::new()
                    .account_id(conn.mail_account_id())
                    .update(Some(update_map)),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&email_set_call);

            let mut res = conn.post_async(None, serde_json::to_string(&req)?).await?;

            let res_text = res.text().await?;

            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    *conn.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                    return Err(err);
                }
                Ok(s) => s,
            };
            *store.online_status.lock().await = (std::time::Instant::now(), Ok(()));
            let m = SetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            if let Some(ids) = m.not_updated {
                if !ids.is_empty() {
                    return Err(Error::new(format!(
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
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut update_map: HashMap<Id<EmailObject>, Value> = HashMap::default();
            let mut ids: Vec<Id<EmailObject>> = Vec::with_capacity(env_hashes.rest.len() + 1);
            let mut id_map: HashMap<Id<EmailObject>, EnvelopeHash> = HashMap::default();
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
                                    _ => continue, // [ref:VERIFY]
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
                for hash in env_hashes.iter() {
                    if let Some(id) = store.id_store.lock().unwrap().get(&hash) {
                        ids.push(id.clone());
                        id_map.insert(id.clone(), hash);
                        update_map.insert(id.clone(), serde_json::json!(update_keywords.clone()));
                    }
                }
            }
            let conn = connection.lock().await;

            let email_set_call: EmailSet = EmailSet::new(
                Set::<EmailObject>::new()
                    .account_id(conn.mail_account_id())
                    .update(Some(update_map)),
            );

            let mut req = Request::new(conn.request_no.clone());
            req.add_call(&email_set_call);
            let email_call: EmailGet = EmailGet::new(
                Get::new()
                    .ids(Some(JmapArgument::Value(ids)))
                    .account_id(conn.mail_account_id())
                    .properties(Some(vec!["keywords".to_string()])),
            );

            req.add_call(&email_call);

            let mut res = conn.post_async(None, serde_json::to_string(&req)?).await?;

            let res_text = res.text().await?;
            /*
             *{"methodResponses":[["Email/set",{"notUpdated":null,"notDestroyed":null,"
             * oldState":"86","newState":"87","accountId":"u148940c7","updated":{"
             * M045926eed54b11423918f392":{"id":"M045926eed54b11423918f392"}},"created":
             * null,"destroyed":null,"notCreated":null},"m3"]],"sessionState":"cyrus-0;
             * p-5;vfs-0"}
             */
            //debug!("res_text = {}", &res_text);
            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    *conn.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                    return Err(err);
                }
                Ok(s) => s,
            };
            *store.online_status.lock().await = (std::time::Instant::now(), Ok(()));
            let m = SetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            if let Some(ids) = m.not_updated {
                return Err(Error::new(
                    ids.into_iter()
                        .map(|err| err.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                ));
            }

            {
                let mut tag_index_lck = store.collection.tag_index.write().unwrap();
                for (flag, value) in flags.iter() {
                    match flag {
                        Ok(_) => {}
                        Err(t) => {
                            if *value {
                                tag_index_lck.insert(TagHash::from_bytes(t.as_bytes()), t.clone());
                            }
                        }
                    }
                }
                drop(tag_index_lck);
            }
            let e = GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
            let GetResponse::<EmailObject> { list, state, .. } = e;
            {
                let (is_empty, is_equal) = {
                    let mailboxes_lck = conn.store.mailboxes.read().unwrap();
                    mailboxes_lck
                        .get(&mailbox_hash)
                        .map(|mbox| {
                            let current_state_lck = mbox.email_state.lock().unwrap();
                            (
                                current_state_lck.is_some(),
                                current_state_lck.as_ref() != Some(&state),
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
            }
            debug!(&list);
            for envobj in list {
                let env_hash = id_map[&envobj.id];
                conn.add_refresh_event(RefreshEvent {
                    account_hash: store.account_hash,
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

    fn delete_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Deleting messages is currently unimplemented for the JMAP backend.",
        ))
    }
}

impl JmapType {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        let online_status = Arc::new(FutureMutex::new((
            std::time::Instant::now(),
            Err(Error::new("Account is uninitialised.")),
        )));
        let server_conf = JmapServerConf::new(s)?;

        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let store = Arc::new(Store {
            account_name: Arc::new(s.name.clone()),
            account_hash,
            account_id: Arc::new(Mutex::new(Id::new())),
            online_status,
            event_consumer,
            is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            collection: Collection::default(),
            core_capabilities: Arc::new(Mutex::new(rfc8620::CapabilitiesObject::default())),
            byte_cache: Default::default(),
            id_store: Default::default(),
            reverse_id_store: Default::default(),
            blob_id_store: Default::default(),
            mailboxes: Default::default(),
            mailboxes_index: Default::default(),
            mailbox_state: Default::default(),
        });

        Ok(Box::new(Self {
            connection: Arc::new(FutureMutex::new(JmapConnection::new(
                &server_conf,
                store.clone(),
            )?)),
            store,
            server_conf,
        }))
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        macro_rules! get_conf_val {
            ($s:ident[$var:literal]) => {
                $s.extra.remove($var).ok_or_else(|| {
                    Error::new(format!(
                        "Configuration error ({}): JMAP connection requires the field `{}` set",
                        $s.name.as_str(),
                        $var
                    ))
                })
            };
            ($s:ident[$var:literal], $default:expr) => {
                $s.extra
                    .remove($var)
                    .map(|v| {
                        <_>::from_str(&v).map_err(|e| {
                            Error::new(format!(
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
        get_conf_val!(s["server_url"])?;
        get_conf_val!(s["server_username"])?;

        get_conf_val!(s["use_token"], false)?;
        // either of these two needed
        get_conf_val!(s["server_password"])
            .or_else(|_| get_conf_val!(s["server_password_command"]))?;

        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        Ok(())
    }
}
