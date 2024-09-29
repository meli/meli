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

// In case we forget to wait some future.
#![deny(unused_must_use)]

use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    str::FromStr,
    sync::{Arc, Mutex, RwLock},
    time::{Duration, Instant},
};

use futures::lock::{
    MappedMutexGuard as FutureMappedMutexGuard, Mutex as FutureMutex,
    MutexGuard as FutureMutexGuard,
};
use indexmap::{IndexMap, IndexSet};
use isahc::AsyncReadResponseExt;
use serde_json::{json, Value};
use url::Url;

use crate::{
    backends::prelude::*,
    email::*,
    error::{Error, ErrorKind, Result},
    utils::futures::{sleep, timeout},
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

pub mod operations;
use operations::*;

pub mod connection;
use connection::*;

pub mod protocol;
use protocol::*;

pub mod session;
use session::*;

pub mod objects;
use objects::{BlobObject, Id, State};

pub mod methods;
use methods::{
    upload_request_format, Get, GetResponse, MethodResponse, Query, QueryResponse, Set,
    SetResponse, UploadResponse,
};

pub mod backend_mailbox;
use backend_mailbox::JmapMailbox;

pub mod argument;
pub mod capabilities;
pub mod comparator;
pub mod email;
pub mod filters;
pub mod identity;
pub mod mailbox;
pub mod submission;
pub mod thread;

use argument::Argument;
use capabilities::JmapCoreCapability;
use filters::Filter;

#[cfg(test)]
mod tests;

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

#[derive(Clone, Debug)]
pub struct JmapServerConf {
    pub server_url: Url,
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
            .set_kind(ErrorKind::Configuration)
        })
    };
    ($s:ident[$var:literal], $t:ty, $hd: literal) => {
        get_conf_val!($s[$var]).and_then(|v| {
            <$t>::from_str(&v).map_err(|e| {
                Error::new(format!(
                    "Configuration error ({}): Invalid value for field `{}` which accepts \
                     {human_desc}: {}\n{}",
                    $s.name.as_str(),
                    $var,
                    v,
                    e,
                    human_desc = $hd,
                ))
                .set_kind(ErrorKind::ValueError)
            })
        })
    };
    ($s:ident[$var:literal], $default:expr, $hd: literal) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    Error::new(format!(
                        "Configuration error ({}): Invalid value for field `{}` which accepts \
                         {human_desc}: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e,
                        human_desc = $hd,
                    ))
                    .set_kind(ErrorKind::ValueError)
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

impl JmapServerConf {
    pub fn new(s: &AccountSettings) -> Result<Self> {
        let use_token: bool = get_conf_val!(s["use_token"], false, "true or false")?;

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
            server_url: get_conf_val!(s["server_url"], Url, "a string containing a URL")?,
            server_username: get_conf_val!(s["server_username"], String, "a string")?,
            server_password: s.server_password()?,
            use_token,
            danger_accept_invalid_certs: get_conf_val!(
                s["danger_accept_invalid_certs"],
                false,
                "true or false"
            )?,
            timeout: get_conf_val!(
                s["timeout"],
                16_u64,
                "integers setting an amount of seconds (a value of zero disables the timeout)"
            )
            .map(|t| {
                if t == 0 {
                    None
                } else {
                    Some(Duration::from_secs(t))
                }
            })?,
        })
    }
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct OnlineStatus(pub Arc<FutureMutex<(Instant, Result<Session>)>>);

impl OnlineStatus {
    /// Returns if session value is `Ok(_)`.
    pub async fn is_ok(&self) -> bool {
        self.0.lock().await.1.is_ok()
    }

    /// Get timestamp of last update.
    pub async fn timestamp(&self) -> Instant {
        self.0.lock().await.0
    }

    /// Get timestamp of last update.
    pub async fn update_timestamp(&self, value: Option<Instant>) {
        self.0.lock().await.0 = value.unwrap_or_else(Instant::now);
    }

    /// Set inner value.
    pub async fn set(&self, t: Option<Instant>, value: Result<Session>) -> Result<Session> {
        std::mem::replace(
            &mut (*self.0.lock().await),
            (t.unwrap_or_else(Instant::now), value),
        )
        .1
    }

    pub async fn session_guard(
        &'_ self,
    ) -> Result<FutureMappedMutexGuard<'_, (Instant, Result<Session>), Session>> {
        let guard = self.0.lock().await;
        if let Err(ref err) = guard.1 {
            return Err(err.clone());
        }
        Ok(FutureMutexGuard::map(guard, |status| {
            // SAFETY: we checked if it's an Err() in the previous line, but we cannot do it
            // in here since it's a closure. So unwrap unchecked for API
            // convenience.
            unsafe { status.1.as_mut().unwrap_unchecked() }
        }))
    }
}

#[derive(Debug)]
pub struct Store {
    pub account_name: Arc<String>,
    pub account_hash: AccountHash,
    pub main_identity: String,
    pub extra_identities: Vec<String>,
    pub byte_cache: Arc<FutureMutex<HashMap<EnvelopeHash, EnvelopeCache>>>,
    pub id_store: Arc<FutureMutex<HashMap<EnvelopeHash, Id<email::EmailObject>>>>,
    pub reverse_id_store: Arc<FutureMutex<HashMap<Id<email::EmailObject>, EnvelopeHash>>>,
    pub blob_id_store: Arc<FutureMutex<HashMap<EnvelopeHash, Id<BlobObject>>>>,
    pub collection: Collection,
    pub mailboxes: Arc<RwLock<HashMap<MailboxHash, JmapMailbox>>>,
    pub mailboxes_index: Arc<RwLock<HashMap<MailboxHash, HashSet<EnvelopeHash>>>>,
    pub mailbox_state: Arc<FutureMutex<Option<State<mailbox::MailboxObject>>>>,
    pub email_state: Arc<FutureMutex<Option<State<email::EmailObject>>>>,
    pub online_status: OnlineStatus,
    pub is_subscribed: IsSubscribedFn,
    pub core_capabilities: Arc<Mutex<IndexMap<String, CapabilitiesObject>>>,
    pub event_consumer: BackendEventConsumer,
}

impl Store {
    pub async fn add_envelope(&self, obj: email::EmailObject) -> Envelope {
        let mut flags = Flag::default();
        let mut labels: IndexSet<TagHash> = IndexSet::new();
        let id;
        let mailbox_ids;
        let blob_id;
        {
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
                        labels.insert(tag_hash);
                    }
                }
            }

            id = obj.id.clone();
            mailbox_ids = obj.mailbox_ids.clone();
            blob_id = obj.blob_id.clone();
        }
        let mut ret: Envelope = obj.into();
        ret.set_flags(flags);
        ret.tags_mut().extend(labels);

        let mut id_store_lck = self.id_store.lock().await;
        let mut reverse_id_store_lck = self.reverse_id_store.lock().await;
        let mut blob_id_store_lck = self.blob_id_store.lock().await;
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

    pub async fn remove_envelope(
        &self,
        obj_id: Id<email::EmailObject>,
    ) -> Option<(EnvelopeHash, SmallVec<[MailboxHash; 8]>)> {
        let env_hash = self.reverse_id_store.lock().await.remove(&obj_id)?;
        self.id_store.lock().await.remove(&env_hash);
        self.blob_id_store.lock().await.remove(&env_hash);
        self.byte_cache.lock().await.remove(&env_hash);
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
            supports_submission: true,
            extra_submission_headers: &[],
            metadata: None,
        };
        let supports_submission: bool = self
            .store
            .core_capabilities
            .lock()
            .map(|c| c.contains_key(JmapSubmissionCapability::uri()))
            .unwrap_or(false);
        MailBackendCapabilities {
            supports_submission: CAPABILITIES.supports_submission || supports_submission,
            ..CAPABILITIES
        }
    }

    fn is_online(&self) -> ResultFuture<()> {
        let online = self.store.online_status.clone();
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout;
        Ok(Box::pin(async move {
            let _conn = timeout(timeout_dur, connection.lock()).await?;
            let _session = timeout(timeout_dur, online.session_guard()).await??;
            Ok(())
        }))
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            // Suggested minimum from RFC8620 Section 2 "The JMAP Session Resource" is 500.
            let batch_size: u64 = store.core_capabilities.lock().unwrap()
                [JmapCoreCapability::uri()]
            .max_objects_in_get
            .min(500);
            let mut fetch_state = EmailFetcher {
                connection,
                store,
                batch_size,
                state: EmailFetchState::Start,
            };
            loop {
                let res = fetch_state.fetch(mailbox_hash).await?;
                if res.is_empty() {
                    return Ok(());
                }
                emitter.emit(res).await;
            }
        })))
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
                let new_mailboxes = debug!(protocol::get_mailboxes(&mut conn, None).await)?;
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
            let (upload_url, mail_account_id) = {
                let g = conn.session_guard().await?;
                (g.upload_url.clone(), g.mail_account_id())
            };
            let res_text = conn
                .post_async(
                    Some(&upload_request_format(&upload_url, &mail_account_id)?),
                    bytes,
                )
                .await?
                .text()
                .await?;

            let mailbox_id: Id<mailbox::MailboxObject> = {
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

            let upload_response: UploadResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = conn.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let mut req = Request::new(conn.request_no.clone());
            let creation_id: Id<email::EmailObject> = "1".to_string().into();

            let import_call: email::EmailImport = email::EmailImport::new()
                .account_id(mail_account_id)
                .emails(indexmap! {
                    creation_id.clone() => email::EmailImportObject::new()
                    .blob_id(upload_response.blob_id)
                    .mailbox_ids(indexmap! {
                        mailbox_id => true
                    })
                });

            req.add_call(&import_call).await;

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
            let m = email::EmailImportResponse::try_from(v.method_responses.remove(0)).map_err(
                |err| {
                    let ierr: Result<email::EmailImportError> = deserialize_from_str(&res_text);
                    if let Ok(err) = ierr {
                        Error::new(format!("Could not save message: {:?}", err))
                    } else {
                        err
                    }
                },
            )?;

            if let Some(err) = m.not_created.and_then(|m| m.get(&creation_id).cloned()) {
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
                email::EmailFilterCondition::new()
                    .in_mailbox(Some(mailbox_id))
                    .into(),
            );
            f &= Filter::<email::EmailFilterCondition, email::EmailObject>::from(q);
            f
        } else {
            Filter::<email::EmailFilterCondition, email::EmailObject>::from(q)
        };

        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let email_call = email::EmailQuery::new(
                Query::new()
                    .account_id(mail_account_id)
                    .filter(Some(filter))
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
            store.online_status.update_timestamp(None).await;
            let m = QueryResponse::<email::EmailObject>::try_from(v.method_responses.remove(0))?;
            let QueryResponse::<email::EmailObject> { ids, .. } = m;
            let ret = ids.into_iter().map(|id| id.into_hash()).collect();
            Ok(ret)
        }))
    }

    fn rename_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        new_path: String,
    ) -> ResultFuture<Mailbox> {
        let store = self.store.clone();
        let mailbox_id: Id<mailbox::MailboxObject> = {
            let mailboxes_lck = store.mailboxes.read().unwrap();
            let Some(id) = mailboxes_lck.get(&mailbox_hash).map(|m| m.id.clone()) else {
                return Err(
                    Error::new(format!("Mailbox with hash {} not found", mailbox_hash))
                        .set_kind(ErrorKind::NotFound),
                );
            };
            id
        };
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mailbox_state = store.mailbox_state.lock().await.clone();
            let mut conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let mailbox_set_call = mailbox::MailboxSet::new(
                Set::<mailbox::MailboxObject>::new(mailbox_state)
                    .account_id(mail_account_id)
                    .update(Some({
                        indexmap! {
                            mailbox_id.clone().into() => serde_json::json!{indexmap! {
                                "name" => new_path.clone()
                            }}
                        }
                    })),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&mailbox_set_call).await;
            let new_mailboxes = protocol::get_mailboxes(&mut conn, Some(req)).await?;

            let new_mailbox: Mailbox = new_mailboxes
                .iter()
                .find_map(|(_, m)| {
                    if m.path() == new_path {
                        Some(BackendMailbox::clone(m) as Mailbox)
                    } else {
                        None
                    }
                })
                .unwrap();
            *store.mailboxes.write().unwrap() = new_mailboxes;
            Ok(new_mailbox)
        }))
    }

    fn create_mailbox(
        &mut self,
        path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mailbox_state = store.mailbox_state.lock().await.clone();
            let mut conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let mailbox_set_call = mailbox::MailboxSet::new(
                Set::<mailbox::MailboxObject>::new(mailbox_state)
                    .account_id(mail_account_id)
                    .create(Some({
                        let id: Id<mailbox::MailboxObject> = path.as_str().into();
                        indexmap! {
                            id.clone().into() => mailbox::MailboxObject {
                                id,
                                name: path.clone(),
                                ..mailbox::MailboxObject::default()
                            }
                        }
                    })),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&mailbox_set_call).await;
            let new_mailboxes = protocol::get_mailboxes(&mut conn, Some(req)).await?;
            *store.mailboxes.write().unwrap() = new_mailboxes;

            let new_mailboxes: HashMap<MailboxHash, Mailbox> = store
                .mailboxes
                .read()
                .unwrap()
                .iter()
                .filter(|(_, f)| f.is_subscribed)
                .map(|(&h, f)| (h, BackendMailbox::clone(f) as Mailbox))
                .collect();
            let id = new_mailboxes
                .values()
                .find(|m| m.path() == path)
                .map(|m| m.hash())
                .unwrap();

            Ok((id, new_mailboxes))
        }))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(
            Error::new("Deleting a mailbox is currently unimplemented for the JMAP backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_subscription(
        &mut self,
        mailbox_hash: MailboxHash,
        val: bool,
    ) -> ResultFuture<()> {
        let store = self.store.clone();
        let mailbox_id: Id<mailbox::MailboxObject> = {
            let mailboxes_lck = store.mailboxes.read().unwrap();
            let Some(id) = mailboxes_lck.get(&mailbox_hash).map(|m| m.id.clone()) else {
                return Err(
                    Error::new(format!("Mailbox with hash {} not found", mailbox_hash))
                        .set_kind(ErrorKind::NotFound),
                );
            };
            id
        };
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mailbox_state = store.mailbox_state.lock().await.clone();
            let mut conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let mailbox_set_call = mailbox::MailboxSet::new(
                Set::<mailbox::MailboxObject>::new(mailbox_state)
                    .account_id(mail_account_id)
                    .update(Some({
                        indexmap! {
                            mailbox_id.clone().into() => serde_json::json!{indexmap! {
                                "isSubscribed" => val
                            }}
                        }
                    })),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&mailbox_set_call).await;
            let res_text = conn.send_request(serde_json::to_string(&req)?).await?;
            let v: MethodResponse = deserialize_from_str(&res_text)?;
            conn.store.online_status.update_timestamp(None).await;
            let SetResponse::<mailbox::MailboxObject> { new_state, .. } =
                SetResponse::<mailbox::MailboxObject>::try_from(
                    *v.method_responses.last().unwrap(),
                )?;
            *conn.store.mailbox_state.lock().await = Some(new_state);
            conn.last_method_response = Some(res_text);

            Ok(())
        }))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox permissions is currently unimplemented for the JMAP backend.",
        )
        .set_kind(ErrorKind::NotImplemented))
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
            let mut update_map: IndexMap<Argument<Id<email::EmailObject>>, Value> =
                IndexMap::default();
            let mut update_keywords: IndexMap<String, Value> = IndexMap::default();
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
                    if let Some(id) = store.id_store.lock().await.get(&env_hash) {
                        // ids.push(id.clone());
                        // id_map.insert(id.clone(), env_hash);
                        update_map.insert(
                            Argument::from(id.clone()),
                            serde_json::json!(update_keywords.clone()),
                        );
                    }
                }
            }
            let conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let state = conn.store.email_state.lock().await.clone();

            let email_set_call = email::EmailSet::new(
                Set::<email::EmailObject>::new(state)
                    .account_id(mail_account_id)
                    .update(Some(update_map)),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&email_set_call).await;

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
            store.online_status.update_timestamp(None).await;
            let SetResponse {
                not_updated,
                new_state,
                ..
            } = SetResponse::<email::EmailObject>::try_from(v.method_responses.remove(0))?;
            *conn.store.email_state.lock().await = Some(new_state);
            if let Some(ids) = not_updated {
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
        flags: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut update_map: IndexMap<Argument<Id<email::EmailObject>>, Value> =
                IndexMap::default();
            let mut ids: Vec<Id<email::EmailObject>> =
                Vec::with_capacity(env_hashes.rest.len() + 1);
            let mut id_map: IndexMap<Id<email::EmailObject>, EnvelopeHash> = IndexMap::default();
            let mut update_keywords: IndexMap<String, Value> = IndexMap::default();
            for op in flags.iter() {
                match op {
                    FlagOp::Set(f) => {
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
                            serde_json::json!(true),
                        );
                    }
                    FlagOp::UnSet(f) => {
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
                            serde_json::json!(null),
                        );
                    }
                    FlagOp::SetTag(t) => {
                        update_keywords.insert(format!("keywords/{}", t), serde_json::json!(true));
                    }
                    FlagOp::UnSetTag(t) => {
                        update_keywords.insert(format!("keywords/{}", t), serde_json::json!(null));
                    }
                }
            }
            {
                for hash in env_hashes.iter() {
                    if let Some(id) = store.id_store.lock().await.get(&hash) {
                        ids.push(id.clone());
                        id_map.insert(id.clone(), hash);
                        update_map.insert(
                            Argument::from(id.clone()),
                            serde_json::json!(update_keywords.clone()),
                        );
                    }
                }
            }
            let conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let state = conn.store.email_state.lock().await.clone();

            let email_set_call = email::EmailSet::new(
                Set::<email::EmailObject>::new(state)
                    .account_id(mail_account_id.clone())
                    .update(Some(update_map)),
            );

            let mut req = Request::new(conn.request_no.clone());
            req.add_call(&email_set_call).await;
            let email_call = email::EmailGet::new(
                Get::new()
                    .ids(Some(Argument::Value(ids)))
                    .account_id(mail_account_id)
                    .properties(Some(vec!["keywords".to_string()])),
            );

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
            store.online_status.update_timestamp(None).await;
            let SetResponse {
                not_updated,
                new_state,
                ..
            } = SetResponse::<email::EmailObject>::try_from(v.method_responses.remove(0))?;
            *conn.store.email_state.lock().await = Some(new_state);
            if let Some(ids) = not_updated {
                return Err(Error::new(
                    ids.into_iter()
                        .map(|err| err.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                ));
            }

            {
                let mut tag_index_lck = store.collection.tag_index.write().unwrap();
                for op in flags.iter() {
                    if let FlagOp::SetTag(t) = op {
                        tag_index_lck.insert(TagHash::from_bytes(t.as_bytes()), t.clone());
                    }
                }
                drop(tag_index_lck);
            }
            let e = GetResponse::<email::EmailObject>::try_from(v.method_responses.pop().unwrap())?;
            let GetResponse::<email::EmailObject> { list, .. } = e;
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
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        {
            let mailboxes_lck = self.store.mailboxes.read().unwrap();
            if !mailboxes_lck.contains_key(&mailbox_hash) {
                return Err(Error::new(format!(
                    "Could not find source mailbox with hash {}",
                    mailbox_hash
                )));
            };
        }
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut destroy_vec: Vec<Argument<Id<email::EmailObject>>> =
                Vec::with_capacity(env_hashes.len());
            for env_hash in env_hashes.iter() {
                if let Some(id) = store.id_store.lock().await.get(&env_hash) {
                    destroy_vec.push(Argument::Value(id.clone()));
                }
            }
            let batch_len = destroy_vec.len();
            let conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();
            let state = conn.store.email_state.lock().await.clone();

            let email_set_call = email::EmailSet::new(
                Set::<email::EmailObject>::new(state)
                    .account_id(mail_account_id)
                    .destroy(Some(destroy_vec)),
            );

            let mut req = Request::new(conn.request_no.clone());
            let _prev_seq = req.add_call(&email_set_call).await;

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
            store.online_status.update_timestamp(None).await;
            let SetResponse {
                not_destroyed,
                new_state,
                ..
            } = SetResponse::<email::EmailObject>::try_from(v.method_responses.remove(0))?;
            *conn.store.email_state.lock().await = Some(new_state);
            conn.add_backend_event(BackendEvent::RefreshBatch(
                env_hashes
                    .iter()
                    .map(|h| RefreshEvent {
                        account_hash: store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Remove(h),
                    })
                    .collect(),
            ));
            if let Some(ids) = not_destroyed {
                if !ids.is_empty() {
                    return Err(Error::new(format!(
                        "Could not destroy {}ids: {}",
                        if ids.len() == batch_len { "" } else { "some " },
                        ids.iter()
                            .map(|err| err.to_string())
                            .collect::<Vec<String>>()
                            .join(",")
                    )));
                }
            }
            Ok(())
        }))
    }

    // [ref:TODO] add support for BLOB extension
    fn submit(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: Option<MailboxHash>,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let store = self.store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            // Steps:
            //
            // 1. upload blob/save to Draft as EmailObject
            // 2. get id and make an EmailSubmissionObject
            // 3. This call then sends the Email immediately, and if successful, removes the
            //    "$draft" flag and moves it from the Drafts folder to the Sent folder.
            let (draft_mailbox_id, sent_mailbox_id) = {
                let mailboxes_lck = store.mailboxes.read().unwrap();
                let find_fn = |usage: SpecialUsageMailbox| -> Result<Id<mailbox::MailboxObject>> {
                    if let Some(sent_folder) =
                        mailboxes_lck.values().find(|m| m.special_usage() == usage)
                    {
                        Ok(sent_folder.id.clone())
                    } else if let Some(sent_folder) = mailboxes_lck
                        .values()
                        .find(|m| m.special_usage() == SpecialUsageMailbox::Inbox)
                    {
                        Ok(sent_folder.id.clone())
                    } else {
                        Ok(mailboxes_lck
                            .values()
                            .next()
                            .ok_or_else(|| {
                                Error::new(format!(
                                    "Account `{}` has no mailboxes.",
                                    store.account_name
                                ))
                            })?
                            .id
                            .clone())
                    }
                };

                (find_fn(SpecialUsageMailbox::Drafts)?, {
                    if let Some(h) = mailbox_hash {
                        if let Some(m) = mailboxes_lck.get(&h) {
                            m.id.clone()
                        } else {
                            return Err(Error::new(format!(
                                "Could not find mailbox with hash {h}",
                            )));
                        }
                    } else {
                        find_fn(SpecialUsageMailbox::Sent)?
                    }
                })
            };
            let conn = connection.lock().await;
            let mail_account_id = conn.session_guard().await?.mail_account_id();

            // [ref:TODO] smarter identity detection based on From: ?
            let Some(identity_id) = conn.session_guard().await?.mail_identity_id() else {
                return Err(Error::new(
                    "You need to setup an Identity in the JMAP server.",
                ));
            };
            let upload_url = { conn.session_guard().await?.upload_url.clone() };
            let res_text = conn
                .post_async(
                    Some(&upload_request_format(&upload_url, &mail_account_id)?),
                    bytes,
                )
                .await?
                .text()
                .await?;

            let upload_response: UploadResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = conn.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            {
                let mut req = Request::new(conn.request_no.clone());
                let creation_id: Id<email::EmailObject> = "newid".into();
                let import_call: email::EmailImport = email::EmailImport::new()
                    .account_id(mail_account_id.clone())
                    .emails(indexmap! {
                        creation_id => email::EmailImportObject::new()
                            .blob_id(upload_response.blob_id)
                            .keywords(indexmap! {
                                "$draft".to_string() => true,
                                "$seen".to_string() => true,
                            })
                            .mailbox_ids(indexmap! {
                                draft_mailbox_id.clone() => true,
                            }),
                    });

                req.add_call(&import_call).await;

                let res_text = conn
                    .post_async(None, serde_json::to_string(&req)?)
                    .await?
                    .text()
                    .await?;
                let v: MethodResponse = match deserialize_from_str(&res_text) {
                    Err(err) => {
                        _ = conn.store.online_status.set(None, Err(err.clone())).await;
                        return Err(err);
                    }
                    Ok(s) => s,
                };

                // [ref:TODO] handle this better?
                let res: Value = serde_json::from_str(v.method_responses[0].get())?;

                let _new_state = res[1]["newState"].as_str().unwrap().to_string();
                let _old_state = res[1]["oldState"].as_str().unwrap().to_string();
                let email_id = Id::from(
                    res[1]["created"]["newid"]["id"]
                        .as_str()
                        .unwrap()
                        .to_string(),
                );

                let mut req = Request::new(conn.request_no.clone());
                let subm_set_call = submission::EmailSubmissionSet::new(
                    Set::<submission::EmailSubmissionObject>::new(None)
                        .account_id(mail_account_id.clone())
                        .create(Some(indexmap! {
                            Argument::from(Id::from("k1490")) => submission::EmailSubmissionObject::new(
                                /* account_id: */ mail_account_id,
                                /* identity_id: */ identity_id,
                                /* email_id: */ email_id,
                                /* envelope: */ None,
                                /* undo_status: */ None
                                )
                        })),
                )
                .on_success_update_email(Some(indexmap! {
                    "#k1490".into() => json!({
                        format!("mailboxIds/{draft_mailbox_id}"): null,
                        format!("mailboxIds/{sent_mailbox_id}"): true,
                        "keywords/$draft": null
                    })
                }));

                req.add_call(&subm_set_call).await;

                let res_text = conn
                    .post_async(None, serde_json::to_string(&req)?)
                    .await?
                    .text()
                    .await?;

                // [ref:TODO] parse/return any error.
                let _: MethodResponse = match deserialize_from_str(&res_text) {
                    Err(err) => {
                        _ = conn.store.online_status.set(None, Err(err.clone())).await;
                        return Err(err);
                    }
                    Ok(s) => s,
                };
            }
            Ok(())
        }))
    }
}

impl JmapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: IsSubscribedFn,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<Self>> {
        let online_status = OnlineStatus(Arc::new(FutureMutex::new((
            std::time::Instant::now(),
            Err(Error::new("Account is uninitialised.")),
        ))));
        let server_conf = JmapServerConf::new(s)?;

        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let store = Arc::new(Store {
            account_name: Arc::new(s.name.clone()),
            account_hash,
            main_identity: s.main_identity_address().to_string(),
            extra_identities: s.extra_identities.clone(),
            online_status,
            event_consumer,
            is_subscribed,
            collection: Collection::default(),
            core_capabilities: Arc::new(Mutex::new(IndexMap::default())),
            byte_cache: Default::default(),
            id_store: Default::default(),
            reverse_id_store: Default::default(),
            blob_id_store: Default::default(),
            mailboxes: Default::default(),
            mailboxes_index: Default::default(),
            mailbox_state: Default::default(),
            email_state: Default::default(),
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
                $s.extra.swap_remove($var).ok_or_else(|| {
                    Error::new(format!(
                        "Configuration error ({}): JMAP connection requires the field `{}` set",
                        $s.name.as_str(),
                        $var
                    ))
                    .set_kind(ErrorKind::Configuration)
                })
            };
            ($s:ident[$var:literal], $t:ty, $hd: literal) => {
                get_conf_val!($s[$var]).and_then(|v| {
                    <$t>::from_str(&v).map_err(|e| {
                        Error::new(format!(
                            "Configuration error ({}): Invalid value for field `{}` which accepts \
                             {human_desc}: {}\n{}",
                            $s.name.as_str(),
                            $var,
                            v,
                            e,
                            human_desc = $hd,
                        ))
                        .set_kind(ErrorKind::ValueError)
                    })
                })
            };
            ($s:ident[$var:literal], $default:expr, $hd: literal) => {
                $s.extra
                    .swap_remove($var)
                    .map(|v| {
                        <_>::from_str(&v).map_err(|e| {
                            Error::new(format!(
                                "Configuration error ({}): Invalid value for field `{}` which \
                                 accepts {human_desc}: {}\n{}",
                                $s.name.as_str(),
                                $var,
                                v,
                                e,
                                human_desc = $hd,
                            ))
                            .set_kind(ErrorKind::ValueError)
                        })
                    })
                    .unwrap_or_else(|| Ok($default))
            };
        }
        get_conf_val!(s["server_url"], Url, "a string containing a URL")?;
        get_conf_val!(s["server_username"])?;

        get_conf_val!(s["use_token"], false, "true or false")?;
        // either of these two needed
        get_conf_val!(s["server_password"])
            .or_else(|_| get_conf_val!(s["server_password_command"]))?;

        get_conf_val!(s["danger_accept_invalid_certs"], false, "true or false")?;
        get_conf_val!(
            s["timeout"],
            16_u64,
            "integers setting an amount of seconds (a value of zero disables the timeout)"
        )?;
        Ok(())
    }
}
