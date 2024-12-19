/*
 * meli - imap module.
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

use smallvec::SmallVec;
#[macro_use]
mod protocol_parser;
pub use protocol_parser::{UntaggedResponse::*, *};
mod mailbox;
pub use mailbox::*;
mod operations;
pub use operations::*;
#[macro_use]
mod connection;
pub use connection::*;
mod watch;
pub use watch::*;
mod search;
pub use search::*;
pub mod email;
pub mod error;
pub mod fetch;
pub mod managesieve;
pub mod sync;
pub mod untagged;
use std::{
    collections::{hash_map::DefaultHasher, BTreeMap, BTreeSet, HashMap, HashSet},
    convert::TryFrom,
    hash::Hasher,
    num::NonZeroU32,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    time::{Duration, SystemTime},
};

use fetch::{FetchStage, FetchState};

pub extern crate imap_codec;
use imap_codec::imap_types::{
    command::CommandBody,
    core::Atom,
    flag::{Flag as ImapCodecFlag, StoreResponse, StoreType},
    mailbox::Mailbox as ImapTypesMailbox,
    sequence::{SequenceSet, ONE},
};
pub use sync::cache::ModSequence;

use crate::{
    backends::{prelude::*, RefreshEventKind::*},
    collection::Collection,
    conf::AccountSettings,
    email::*,
    error::{Error, ErrorKind, Result, ResultIntoError},
    imap::{protocol_parser::id_ext::IDResponse, sync::cache::ImapCache},
    text::Truncate,
    utils::futures::timeout,
};

pub type ImapNum = usize;
pub type UID = ImapNum;
pub type UIDVALIDITY = UID;
pub type MessageSequenceNumber = ImapNum;

pub static SUPPORTED_CAPABILITIES: &[&str] = &[
    "AUTH=ANONYMOUS",
    "AUTH=PLAIN",
    "AUTH=XOAUTH2",
    "COMPRESS=DEFLATE",
    "CONDSTORE",
    "ENABLE",
    "ID",
    "IDLE",
    "IMAP4REV1",
    "LIST-EXTENDED",
    "LIST-STATUS",
    "LITERAL+",
    "LOGIN",
    "LOGINDISABLED",
    "MOVE",
    "SPECIAL-USE",
    "UIDPLUS",
    "UNSELECT",
];

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<Vec<u8>>,
    flags: Option<Flag>,
}

#[derive(Clone, Debug)]
pub struct ImapServerConf {
    pub server_hostname: String,
    pub server_username: String,
    pub server_password: String,
    pub server_port: u16,
    pub use_starttls: bool,
    pub use_tls: bool,
    pub danger_accept_invalid_certs: bool,
    pub protocol: ImapProtocol,
    pub timeout: Option<Duration>,
}

type Capabilities = indexmap::IndexSet<Box<[u8]>>;

#[macro_export]
macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            Error::new(format!(
                "Configuration error ({}): IMAP connection requires the field `{}` set",
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

#[derive(Debug)]
pub struct UIDStore {
    pub account_hash: AccountHash,
    pub account_name: Arc<str>,
    pub server_id: Arc<Mutex<Option<IDResponse>>>,
    pub keep_offline_cache: AtomicBool,
    pub offline_cache: Arc<Mutex<Option<Box<dyn ImapCache>>>>,
    pub capabilities: Arc<Mutex<Capabilities>>,
    pub hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    pub uid_index: Arc<Mutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,
    pub msn_index: Arc<Mutex<HashMap<MailboxHash, BTreeMap<MessageSequenceNumber, UID>>>>,

    pub byte_cache: Arc<Mutex<HashMap<UID, EnvelopeCache>>>,
    pub collection: Collection,

    // Offline caching
    pub uidvalidity: Arc<Mutex<HashMap<MailboxHash, UID>>>,
    pub envelopes: Arc<Mutex<HashMap<EnvelopeHash, sync::cache::CachedEnvelope>>>,
    pub max_uids: Arc<Mutex<HashMap<MailboxHash, UID>>>,
    pub modseq: Arc<Mutex<HashMap<EnvelopeHash, ModSequence>>>,
    pub highestmodseqs: Arc<Mutex<HashMap<MailboxHash, std::result::Result<ModSequence, ()>>>>,
    pub mailboxes: Arc<FutureMutex<HashMap<MailboxHash, ImapMailbox>>>,
    pub is_online: Arc<Mutex<(SystemTime, Result<()>)>>,
    pub event_consumer: BackendEventConsumer,
    pub timeout: Option<Duration>,
}

impl UIDStore {
    pub fn new(
        account_hash: AccountHash,
        account_name: Arc<str>,
        event_consumer: BackendEventConsumer,
        timeout: Option<Duration>,
        keep_offline_cache: bool,
    ) -> Self {
        Self {
            account_hash,
            account_name,
            server_id: Default::default(),
            keep_offline_cache: AtomicBool::new(keep_offline_cache),
            offline_cache: Arc::new(Mutex::new(None)),
            capabilities: Default::default(),
            uidvalidity: Default::default(),
            envelopes: Default::default(),
            max_uids: Default::default(),
            modseq: Default::default(),
            highestmodseqs: Default::default(),
            hash_index: Default::default(),
            uid_index: Default::default(),
            msn_index: Default::default(),
            byte_cache: Default::default(),
            mailboxes: Arc::new(FutureMutex::new(Default::default())),
            collection: Default::default(),
            is_online: Arc::new(Mutex::new((
                SystemTime::now(),
                Err(Error::new("Account is uninitialised.")),
            ))),
            event_consumer,
            timeout,
        }
    }

    fn init_cache(
        self: &Arc<Self>,
        mutex: &mut std::sync::MutexGuard<'_, Option<Box<dyn ImapCache>>>,
    ) -> Result<()> {
        if mutex.is_none() {
            #[cfg(feature = "sqlite3")]
            {
                let value = sync::sqlite3_cache::Sqlite3Cache::get(Arc::clone(self), None);
                match value {
                    Err(err) => {
                        self.keep_offline_cache.store(false, Ordering::SeqCst);
                        return Err(err);
                    }
                    Ok(val) => {
                        **mutex = Some(val);
                    }
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ImapType {
    pub _is_subscribed: IsSubscribedFn,
    pub connection: Arc<ConnectionMutex>,
    pub server_conf: ImapServerConf,
    pub uid_store: Arc<UIDStore>,
}

impl MailBackend for ImapType {
    fn capabilities(&self) -> MailBackendCapabilities {
        let mut extensions = self
            .uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .map(|c| {
                (
                    String::from_utf8_lossy(c).into(),
                    MailBackendExtensionStatus::Unsupported { comment: None },
                )
            })
            .collect::<Vec<(String, MailBackendExtensionStatus)>>();
        if let ImapProtocol::IMAP {
            extension_use:
                ImapExtensionUse {
                    idle,
                    deflate,
                    condstore,
                    oauth2,
                    auth_anonymous,
                    id,
                },
        } = self.server_conf.protocol
        {
            for (name, status) in extensions.iter_mut() {
                match name.as_str() {
                    "IDLE" => {
                        if idle {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    "COMPRESS=DEFLATE" => {
                        if deflate {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    "CONDSTORE" => {
                        if condstore {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    "AUTH=OAUTH2" => {
                        if oauth2 {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    "AUTH=ANONYMOUS" => {
                        if auth_anonymous {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    "LOGINDISABLED" => {
                        if !(oauth2 || auth_anonymous) {
                            *status = MailBackendExtensionStatus::Enabled {
                                comment: Some(
                                    "Use of LOGIN command is the default for given user \
                                     configuration, but server rejects its use",
                                ),
                            };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some(
                                    "Current user authentication is not performed with the LOGIN \
                                     command",
                                ),
                            };
                        }
                    }
                    "ID" => {
                        if id {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    _ => {
                        if SUPPORTED_CAPABILITIES
                            .iter()
                            .any(|c| c.eq_ignore_ascii_case(name.as_str()))
                        {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        }
                    }
                }
            }
        }
        extensions.sort_by(|a, b| a.0.cmp(&b.0));
        let metadata = self
            .uid_store
            .server_id
            .lock()
            .unwrap()
            .as_ref()
            .and_then(|id| serde_json::to_value(id).ok());
        MailBackendCapabilities {
            is_async: true,
            is_remote: true,
            supports_search: true,
            extensions: Some(extensions),
            supports_tags: true,
            supports_submission: false,
            extra_submission_headers: &[],
            metadata,
        }
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        let mut state = FetchState {
            stage: FetchStage::ResyncCache,
            connection: self.connection.clone(),
            mailbox_hash,
            uid_store: self.uid_store.clone(),
            batch_size: 1500,
            cache_batch_size: 25000,
        };

        Ok(Box::pin(try_fn_stream(|emitter| async move {
            let id = state.connection.lock().await?.id.clone();
            {
                let f = &state.uid_store.mailboxes.lock().await[&mailbox_hash];
                f.set_warm(true);
                if let Ok(mut exists) = f.exists.lock() {
                    let total = exists.len();
                    exists.clear();
                    exists.set_not_yet_seen(total);
                }
                if let Ok(mut unseen) = f.unseen.lock() {
                    let total = unseen.len();
                    unseen.clear();
                    unseen.set_not_yet_seen(total);
                }
                if f.no_select {
                    emitter.emit(vec![]).await;
                    return Ok(());
                }
            };
            loop {
                let res = state.chunk().await.map_err(|err| {
                    log::trace!(
                        "{} fetch chunk at stage {:?} err {:?}",
                        id,
                        state.stage,
                        &err
                    );
                    err
                })?;
                emitter.emit(res).await;
                if state.stage == FetchStage::Finished {
                    return Ok(());
                }
            }
        })))
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let main_conn = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = main_conn.lock().await?;
            conn.refresh(mailbox_hash).await
        }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            {
                let mailboxes = uid_store.mailboxes.lock().await;
                if !mailboxes.is_empty() {
                    return Ok(mailboxes
                        .iter()
                        .map(|(h, f)| (*h, Box::new(Clone::clone(f)) as Mailbox))
                        .collect());
                }
            }
            let new_mailboxes = Self::imap_mailboxes(&connection).await?;
            let mut mailboxes = uid_store.mailboxes.lock().await;
            *mailboxes = new_mailboxes;
            /*
            let mut invalid_configs = vec![];
            for m in mailboxes.values() {
                if m.is_subscribed() != (self.is_subscribed)(m.path()) {
                    invalid_configs.push((m.path(), m.is_subscribed()));
                }
            }
            if !invalid_configs.is_empty() {
                let mut err_string = format!("{}: ", self.account_name);
                for (m, server_value) in invalid_configs.iter() {
                    err_string.extend(format!(
                            "Mailbox `{}` is {}subscribed on server but {}subscribed in your configuration. These settings have to match.\n",
                            if *server_value { "" } else { "not " },
                            if *server_value { "not " } else { "" },
                            m
                ).chars());
                }
                return Err(Error::new(err_string));
            }
            mailboxes.retain(|_, f| (self.is_subscribed)(f.path()));
            */
            let keys = mailboxes.keys().cloned().collect::<HashSet<MailboxHash>>();
            for f in mailboxes.values_mut() {
                f.children.retain(|c| keys.contains(c));
            }
            Ok(mailboxes
                .iter()
                .map(|(h, f)| (*h, Box::new(Clone::clone(f)) as Mailbox))
                .collect())
        }))
    }

    fn is_online(&self) -> ResultFuture<()> {
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await?;
            conn.is_online().await
        }))
    }

    fn watch(&self) -> ResultFuture<()> {
        let server_conf = self.server_conf.clone();
        let main_conn = self.connection.clone();
        let uid_store = self.uid_store.clone();
        Ok(Box::pin(async move {
            let has_idle: bool = match server_conf.protocol {
                ImapProtocol::IMAP {
                    extension_use: ImapExtensionUse { idle, .. },
                } => {
                    idle && uid_store
                        .capabilities
                        .lock()
                        .unwrap()
                        .iter()
                        .any(|cap| cap.eq_ignore_ascii_case(b"IDLE"))
                }
                _ => false,
            };
            while let Err(err) = if has_idle {
                idle(ImapWatchKit {
                    conn: ImapConnection::new_connection(
                        &server_conf,
                        format!(
                            "{}-watch-IDLE",
                            uid_store.account_name.as_ref().trim_at_boundary(25)
                        )
                        .into(),
                        uid_store.clone(),
                        false,
                    ),
                    main_conn: main_conn.clone(),
                    uid_store: uid_store.clone(),
                })
                .await
            } else {
                poll_with_examine(ImapWatchKit {
                    conn: ImapConnection::new_connection(
                        &server_conf,
                        format!(
                            "{}-watch-poll_with_EXAMINE",
                            uid_store.account_name.as_ref().trim_at_boundary(25)
                        )
                        .into(),
                        uid_store.clone(),
                        false,
                    ),
                    main_conn: main_conn.clone(),
                    uid_store: uid_store.clone(),
                })
                .await
            } {
                let mut main_conn_lck = main_conn.lock().await?;
                if err.kind.is_network() {
                    uid_store.is_online.lock().unwrap().1 = Err(err.clone());
                } else {
                    return Err(err);
                }
                log::trace!(
                    "{} Watch failure: {}",
                    uid_store.account_name,
                    err.to_string()
                );
                match timeout(uid_store.timeout, main_conn_lck.connect())
                    .await
                    .and_then(|res| res)
                {
                    Err(err2) => {
                        log::trace!(
                            "{} Watch reconnect attempt failed: {}",
                            uid_store.account_name,
                            err2.to_string()
                        );
                    }
                    Ok(()) => {
                        log::trace!(
                            "{} Watch reconnect attempt successful",
                            uid_store.account_name
                        );
                        continue;
                    }
                }
                let account_hash = uid_store.account_hash;
                main_conn_lck.add_refresh_event(RefreshEvent {
                    account_hash,
                    mailbox_hash: MailboxHash::default(),
                    kind: RefreshEventKind::Failure(err.clone()),
                });
                return Err(err);
            }
            log::trace!("{} watch future returning", uid_store.account_name);
            Ok(())
        }))
    }

    fn envelope_bytes_by_hash(&self, hash: EnvelopeHash) -> ResultFuture<Vec<u8>> {
        let (uid, mailbox_hash) =
            if let Some(v) = self.uid_store.hash_index.lock().unwrap().get(&hash) {
                *v
            } else {
                return Err(Error::new(
                    "Message not found in local cache, it might have been deleted before you \
                     requested it.",
                ));
            };
        let op = ImapOp::new(
            uid,
            mailbox_hash,
            self.connection.clone(),
            self.uid_store.clone(),
        );
        Ok(Box::pin(async move { op.as_bytes().await }))
    }

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await?;
            conn.save(bytes, mailbox_hash, flags).await
        }))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()> {
        let connection = self.connection.clone();
        let has_move: bool = move_
            && self
                .uid_store
                .capabilities
                .lock()
                .unwrap()
                .iter()
                .any(|cap| cap.eq_ignore_ascii_case(b"MOVE"));
        let uids: SmallVec<[UID; 64]> = {
            let hash_index_lck = self.uid_store.hash_index.lock().unwrap();
            env_hashes
                .iter()
                .filter_map(|env_hash| hash_index_lck.get(&env_hash).cloned().map(|(uid, _)| uid))
                .collect()
        };

        if uids.is_empty() {
            return Ok(Box::pin(async move { Ok(()) }));
        }
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await?;
            let dest_path = {
                let mailboxes = conn.uid_store.mailboxes.lock().await;
                let mailbox = mailboxes
                    .get(&destination_mailbox_hash)
                    .ok_or_else(|| Error::new("Destination mailbox not found"))?;
                mailbox.imap_path().to_string()
            };
            let mut response = Vec::with_capacity(8 * 1024);
            conn.select_mailbox(source_mailbox_hash, &mut response, false)
                .await?;
            if has_move {
                conn.send_command(CommandBody::r#move(uids.as_slice(), dest_path, true)?)
                    .await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
            } else {
                conn.send_command(CommandBody::copy(uids.as_slice(), dest_path, true)?)
                    .await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
                // [ref:TODO]: check for COPYUID [RFC4315 - UIDPLUS]
                if move_ {
                    conn.send_command(CommandBody::store(
                        uids.as_slice(),
                        StoreType::Add,
                        StoreResponse::Answer,
                        vec![ImapCodecFlag::Deleted],
                        true,
                    )?)
                    .await?;
                    conn.read_response(&mut response, RequiredResponses::empty())
                        .await?;
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
        let connection = self.connection.clone();
        let uids: SmallVec<[UID; 64]> = {
            let hash_index_lck = self.uid_store.hash_index.lock().unwrap();
            env_hashes
                .iter()
                .filter_map(|env_hash| hash_index_lck.get(&env_hash).cloned().map(|(uid, _)| uid))
                .collect()
        };

        if uids.is_empty() {
            return Ok(Box::pin(async move { Ok(()) }));
        }

        // Conversion from UID (usize) to UID (NonZeroU32).
        // TODO: Directly use NonZeroU32 for UID everywhere?
        let sequence_set = {
            let mut tmp = Vec::new();

            for uid in uids.iter() {
                if let Ok(uid) = u32::try_from(*uid) {
                    if let Ok(uid) = NonZeroU32::try_from(uid) {
                        tmp.push(uid);
                    } else {
                        return Err(Error::new("Application error: UID was 0"));
                    }
                } else {
                    return Err(Error::new("Application error: UID exceeded u32"));
                }
            }

            // Safety: We know this can't fail due to the `is_empty` check above.
            // TODO: This could be too easy to overlook.
            SequenceSet::try_from(tmp).unwrap()
        };

        Ok(Box::pin(async move {
            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await?;
            conn.select_mailbox(mailbox_hash, &mut response, false)
                .await?;
            if flags.iter().any(<bool>::from) {
                /* Set flags/tags to true */
                let mut set_seen = false;
                let command = {
                    let mut tag_lck = conn.uid_store.collection.tag_index.write().unwrap();
                    let flags = {
                        let mut tmp = Vec::new();

                        for op in flags.iter().filter(|op| <bool>::from(*op)) {
                            match op {
                                FlagOp::Set(flag) if *flag == Flag::REPLIED => {
                                    tmp.push(ImapCodecFlag::Answered)
                                }
                                FlagOp::Set(flag) if *flag == Flag::FLAGGED => {
                                    tmp.push(ImapCodecFlag::Flagged);
                                }
                                FlagOp::Set(flag) if *flag == Flag::TRASHED => {
                                    tmp.push(ImapCodecFlag::Deleted);
                                }
                                FlagOp::Set(flag) if *flag == Flag::SEEN => {
                                    tmp.push(ImapCodecFlag::Seen);
                                    set_seen = true;
                                }
                                FlagOp::Set(flag) if *flag == Flag::DRAFT => {
                                    tmp.push(ImapCodecFlag::Draft);
                                }
                                FlagOp::Set(_) => {
                                    log::error!(
                                        "Application error: more than one flag bit set in \
                                         set_flags: {:?}",
                                        flags
                                    );
                                    return Err(Error::new(format!(
                                        "Application error: more than one flag bit set in \
                                         set_flags: {:?}",
                                        flags
                                    ))
                                    .set_kind(crate::ErrorKind::Bug));
                                }
                                FlagOp::SetTag(tag) => {
                                    let hash = TagHash::from_bytes(tag.as_bytes());
                                    tag_lck.entry(hash).or_insert_with(|| tag.to_string());
                                    tmp.push(ImapCodecFlag::Keyword(Atom::try_from(tag.as_str())?));
                                }
                                _ => {}
                            }
                        }

                        tmp
                    };

                    CommandBody::Store {
                        sequence_set: sequence_set.clone(),
                        kind: StoreType::Add,
                        response: StoreResponse::Answer,
                        flags,
                        uid: true,
                    }
                };
                conn.send_command(command).await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
                if set_seen {
                    for f in conn.uid_store.mailboxes.lock().await.values() {
                        if let Ok(mut unseen) = f.unseen.lock() {
                            for env_hash in env_hashes.iter() {
                                unseen.remove(env_hash);
                            }
                        };
                    }
                }
            }
            if flags.iter().any(|b| !<bool>::from(b)) {
                let mut set_unseen = false;
                /* Set flags/tags to false */
                let command = {
                    let flags = {
                        let mut tmp = Vec::new();

                        for op in flags.iter().filter(|op| !<bool>::from(*op)) {
                            match op {
                                FlagOp::UnSet(flag) if *flag == Flag::REPLIED => {
                                    tmp.push(ImapCodecFlag::Answered);
                                }
                                FlagOp::UnSet(flag) if *flag == Flag::FLAGGED => {
                                    tmp.push(ImapCodecFlag::Flagged);
                                }
                                FlagOp::UnSet(flag) if *flag == Flag::TRASHED => {
                                    tmp.push(ImapCodecFlag::Deleted);
                                }
                                FlagOp::UnSet(flag) if *flag == Flag::SEEN => {
                                    tmp.push(ImapCodecFlag::Seen);
                                    set_unseen = true;
                                }
                                FlagOp::UnSet(flag) if *flag == Flag::DRAFT => {
                                    tmp.push(ImapCodecFlag::Draft);
                                }
                                FlagOp::UnSet(_) => {
                                    log::error!(
                                        "Application error: more than one flag bit set in \
                                         set_flags: {:?}",
                                        flags
                                    );
                                    return Err(Error::new(format!(
                                        "Application error: more than one flag bit set in \
                                         set_flags: {:?}",
                                        flags
                                    )));
                                }
                                FlagOp::UnSetTag(tag) => {
                                    tmp.push(ImapCodecFlag::Keyword(Atom::try_from(tag.as_str())?));
                                }
                                _ => {}
                            }
                        }

                        tmp
                    };

                    CommandBody::Store {
                        sequence_set,
                        kind: StoreType::Remove,
                        response: StoreResponse::Answer,
                        flags,
                        uid: true,
                    }
                };
                conn.send_command(command).await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
                if set_unseen {
                    for f in conn.uid_store.mailboxes.lock().await.values() {
                        if let (Ok(mut unseen), Ok(exists)) = (f.unseen.lock(), f.exists.lock()) {
                            for env_hash in env_hashes.iter().filter(|h| exists.contains(h)) {
                                unseen.insert_new(env_hash);
                            }
                        };
                    }
                }
            }
            if let Err(err) = conn
                .uid_store
                .update_flags(env_hashes, mailbox_hash, flags)
                .or_else(sync::cache::ignore_not_found)
            {
                imap_log!(error, conn, "Failed to update cache: {}", err);
            }
            Ok(())
        }))
    }

    fn delete_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        let flag_future = self.set_flags(
            env_hashes.clone(),
            mailbox_hash,
            smallvec::smallvec![FlagOp::Set(Flag::TRASHED)],
        )?;
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            flag_future.await?;
            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await?;
            let has_uid_plus = conn
                .uid_store
                .capabilities
                .lock()
                .unwrap()
                .iter()
                .any(|cap| cap.eq_ignore_ascii_case(b"UIDPLUS"));
            conn.select_mailbox(mailbox_hash, &mut response, false)
                .await?;
            if has_uid_plus {
                let uids: Vec<UID> = {
                    let hash_index_lck = uid_store.hash_index.lock().unwrap();
                    env_hashes
                        .iter()
                        .filter_map(|env_hash| {
                            hash_index_lck.get(&env_hash).cloned().map(|(uid, _)| uid)
                        })
                        .collect()
                };

                if uids.is_empty() {
                    return Ok(());
                }

                // Conversion from UID (usize) to UID (NonZeroU32).
                // TODO: Directly use NonZeroU32 for UID everywhere?
                let sequence_set = {
                    let mut tmp = Vec::new();

                    for uid in uids.iter() {
                        if let Ok(uid) = u32::try_from(*uid) {
                            if let Ok(uid) = NonZeroU32::try_from(uid) {
                                tmp.push(uid);
                            } else {
                                return Err(Error::new("Application error: UID was 0"));
                            }
                        } else {
                            return Err(Error::new("Application error: UID exceeded u32"));
                        }
                    }

                    // Safety: We know this can't fail due to the `is_empty` check above.
                    // TODO: This could be too easy to overlook.
                    SequenceSet::try_from(tmp).unwrap()
                };

                conn.send_command(CommandBody::ExpungeUid { sequence_set })
                    .await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
            } else {
                conn.send_command(CommandBody::Expunge).await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
            }
            imap_log!(
                trace,
                conn,
                "EXPUNGE response: {}",
                &String::from_utf8_lossy(&response)
            );
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
        self.uid_store.collection.clone()
    }

    fn create_mailbox(
        &mut self,
        mut path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        let new_mailbox_fut = self.mailboxes();
        Ok(Box::pin(async move {
            /* Must transform path to something the IMAP server will accept
             *
             * Each root mailbox has a hierarchy delimiter reported by the LIST entry.
             * All paths must use this delimiter to indicate children of this
             * mailbox.
             *
             * A new root mailbox should have the default delimiter, which can be found
             * out by issuing an empty LIST command as described in RFC3501:
             * C: A101 LIST "" ""
             * S: * LIST (\Noselect) "/" ""
             *
             * The default delimiter for us is '/' just like UNIX paths. I apologise if
             * this decision is unpleasant for you.
             */

            {
                let mailboxes = uid_store.mailboxes.lock().await;

                if mailboxes.values().any(|f| f.path == path) {
                    return Err(Error::new(format!(
                        "Mailbox named `{}` already exists.",
                        path,
                    )));
                }
                for root_mailbox in mailboxes.values().filter(|f| f.parent.is_none()) {
                    if path.starts_with(&root_mailbox.name) {
                        log::trace!(
                            "{} path starts with {:?}",
                            uid_store.account_name,
                            &root_mailbox
                        );
                        path = path.replace(
                            '/',
                            (root_mailbox.separator as char).encode_utf8(&mut [0; 4]),
                        );
                        break;
                    }
                }

                /* [ref:FIXME]  Do not try to CREATE a sub-mailbox in a
                 * mailbox that has the \Noinferiors flag
                 * set. */
            }

            let mut response = Vec::with_capacity(8 * 1024);
            {
                let mut conn_lck = connection.lock().await?;
                conn_lck.unselect().await?;

                conn_lck
                    .send_command(CommandBody::create(path.as_str())?)
                    .await?;
                conn_lck
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
                conn_lck
                    .send_command(CommandBody::subscribe(path.as_str())?)
                    .await?;
                conn_lck
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
            }
            let ret: Result<()> = ImapResponse::try_from(response.as_slice())?.into();
            ret?;
            let new_hash = MailboxHash::from_bytes(path.as_bytes());
            uid_store.mailboxes.lock().await.clear();
            Ok((
                new_hash,
                new_mailbox_fut?.await.map_err(|err| {
                    Error::new(format!(
                        "Mailbox create was successful (returned `{}`) but listing mailboxes \
                         afterwards returned `{}`",
                        String::from_utf8_lossy(&response),
                        err
                    ))
                })?,
            ))
        }))
    }

    fn delete_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        let new_mailbox_fut = self.mailboxes();
        Ok(Box::pin(async move {
            let imap_path: String;
            let is_subscribed: bool;
            {
                let mailboxes = uid_store.mailboxes.lock().await;
                is_subscribed = mailboxes[&mailbox_hash].is_subscribed();
                imap_path = mailboxes[&mailbox_hash].imap_path().to_string();
                let permissions = mailboxes[&mailbox_hash].permissions();
                if !permissions.delete_mailbox {
                    return Err(Error::new(format!(
                        "You do not have permission to delete `{}`. Set permissions for this \
                         mailbox are {}",
                        mailboxes[&mailbox_hash].name(),
                        permissions
                    )));
                }
            }
            let mut response = Vec::with_capacity(8 * 1024);
            {
                let mut conn_lck = connection.lock().await?;
                /* make sure mailbox is not selected before it gets deleted, otherwise
                 * connection gets dropped by server */
                conn_lck.unselect().await?;
                if is_subscribed {
                    conn_lck
                        .send_command(CommandBody::unsubscribe(imap_path.as_str())?)
                        .await?;
                    conn_lck
                        .read_response(&mut response, RequiredResponses::empty())
                        .await?;
                }

                conn_lck
                    .send_command(CommandBody::delete(imap_path.as_str())?)
                    .await?;
                conn_lck
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
            }
            let ret: Result<()> = ImapResponse::try_from(response.as_slice())?.into();
            ret?;
            uid_store.mailboxes.lock().await.clear();
            new_mailbox_fut?.await.map_err(|err| {
                format!(
                    "Mailbox delete was successful (returned `{}`) but listing mailboxes \
                     afterwards returned `{}`",
                    String::from_utf8_lossy(&response),
                    err
                )
                .into()
            })
        }))
    }

    fn set_mailbox_subscription(
        &mut self,
        mailbox_hash: MailboxHash,
        new_val: bool,
    ) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let imap_path = {
                let mailboxes = uid_store.mailboxes.lock().await;
                if mailboxes[&mailbox_hash].is_subscribed() == new_val {
                    return Ok(());
                }
                mailboxes[&mailbox_hash].imap_path().to_string()
            };

            let mut response = Vec::with_capacity(8 * 1024);
            {
                let mut conn_lck = connection.lock().await?;
                if new_val {
                    conn_lck
                        .send_command(CommandBody::subscribe(imap_path.as_str())?)
                        .await?;
                } else {
                    conn_lck
                        .send_command(CommandBody::unsubscribe(imap_path.as_str())?)
                        .await?;
                }
                conn_lck
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
            }

            let ret: Result<()> = ImapResponse::try_from(response.as_slice())?.into();
            if ret.is_ok() {
                uid_store
                    .mailboxes
                    .lock()
                    .await
                    .entry(mailbox_hash)
                    .and_modify(|entry| {
                        let _ = entry.set_is_subscribed(new_val);
                    });
            }
            ret
        }))
    }

    fn rename_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        mut new_path: String,
    ) -> ResultFuture<Mailbox> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        let new_mailbox_fut = self.mailboxes();
        Ok(Box::pin(async move {
            let command: CommandBody;
            let mut response = Vec::with_capacity(8 * 1024);
            {
                let mailboxes = uid_store.mailboxes.lock().await;
                let permissions = mailboxes[&mailbox_hash].permissions();
                if !permissions.delete_mailbox {
                    return Err(Error::new(format!(
                        "You do not have permission to rename mailbox `{}` (rename is equivalent \
                         to delete + create). Set permissions for this mailbox are {}",
                        mailboxes[&mailbox_hash].name(),
                        permissions
                    )));
                }
                if mailboxes[&mailbox_hash].separator != b'/' {
                    new_path = new_path.replace(
                        '/',
                        (mailboxes[&mailbox_hash].separator as char).encode_utf8(&mut [0; 4]),
                    );
                }
                let from =
                    ImapTypesMailbox::try_from(mailboxes[&mailbox_hash].imap_path().to_string())?;
                let to = ImapTypesMailbox::try_from(new_path.to_string())?;
                command = CommandBody::Rename { from, to };
            }
            {
                let mut conn_lck = connection.lock().await?;
                conn_lck.send_command(command).await?;
                conn_lck
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
            }
            let new_hash = MailboxHash::from_bytes(new_path.as_bytes());
            let ret: Result<()> = ImapResponse::try_from(response.as_slice())?.into();
            ret?;
            uid_store.mailboxes.lock().await.clear();
            new_mailbox_fut?.await.map_err(|err| {
                format!(
                    "Mailbox rename was successful (returned `{}`) but listing mailboxes \
                     afterwards returned `{}`",
                    String::from_utf8_lossy(&response),
                    err
                )
            })?;
            Ok(BackendMailbox::clone(
                &uid_store.mailboxes.lock().await[&new_hash],
            ))
        }))
    }

    fn set_mailbox_permissions(
        &mut self,
        mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        //let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mailboxes = uid_store.mailboxes.lock().await;
            let permissions = mailboxes[&mailbox_hash].permissions();
            if !permissions.change_permissions {
                return Err(Error::new(format!(
                    "You do not have permission to change permissions for mailbox `{}`. Set \
                     permissions for this mailbox are {}",
                    mailboxes[&mailbox_hash].name(),
                    permissions
                )));
            }

            Err(Error::new("Unimplemented."))
        }))
    }

    fn search(
        &self,
        query: Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<Vec<EnvelopeHash>> {
        if mailbox_hash.is_none() {
            return Err(Error::new(
                "Cannot search without specifying mailbox on IMAP",
            ));
        }
        let connection = self.connection.clone();

        Ok(Box::pin(async move {
            let mut conn = connection.lock().await?;
            conn.search(query, mailbox_hash).await
        }))
    }
}

impl ImapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: IsSubscribedFn,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<Self>> {
        let server_hostname = get_conf_val!(s["server_hostname"])?;
        let server_username = get_conf_val!(s["server_username"])?;
        let use_oauth2: bool = get_conf_val!(s["use_oauth2"], false)?;

        if use_oauth2
            && !s.extra.contains_key("server_password_command")
            && !s.extra.contains_key("server_password")
        {
            return Err(Error::new(format!(
                "({}) `use_oauth2` use requires either `server_password` set or \
                 `server_password_command` set with a command that returns an OAUTH2 token. \
                 Consult documentation for guidance.",
                s.name,
            ))
            .set_kind(ErrorKind::Configuration));
        }

        let server_password = s.server_password()?;
        let server_port = get_conf_val!(s["server_port"], 143)?;
        let use_tls = get_conf_val!(s["use_tls"], true)?;
        let use_starttls = use_tls && get_conf_val!(s["use_starttls"], server_port != 993)?;
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        #[cfg(feature = "sqlite3")]
        let keep_offline_cache = get_conf_val!(s["offline_cache"], true)?;
        #[cfg(not(feature = "sqlite3"))]
        let keep_offline_cache = get_conf_val!(s["offline_cache"], false)?;
        #[cfg(not(feature = "sqlite3"))]
        if keep_offline_cache {
            return Err(Error::new(format!(
                "({}) offline_cache is true but melib is not compiled with sqlite3",
                s.name,
            )));
        }
        let timeout = get_conf_val!(s["timeout"], 16_u64)?;
        let timeout = if timeout == 0 {
            None
        } else {
            Some(Duration::from_secs(timeout))
        };
        let server_conf = ImapServerConf {
            server_hostname: server_hostname.to_string(),
            server_username: server_username.to_string(),
            server_password,
            server_port,
            use_tls,
            use_starttls,
            danger_accept_invalid_certs,
            protocol: ImapProtocol::IMAP {
                extension_use: ImapExtensionUse {
                    idle: get_conf_val!(s["use_idle"], true)?,
                    condstore: get_conf_val!(s["use_condstore"], true)?,
                    deflate: get_conf_val!(s["use_deflate"], true)?,
                    oauth2: use_oauth2,
                    auth_anonymous: get_conf_val!(s["use_auth_anonymous"], false)?,
                    id: get_conf_val!(s["use_id"], false)?,
                },
            },
            timeout,
        };
        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let account_name = s.name.to_string().into();
        let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
            offline_cache: Arc::new(Mutex::new(None)),
            ..UIDStore::new(
                account_hash,
                account_name,
                event_consumer,
                server_conf.timeout,
                keep_offline_cache,
            )
        });
        let connection = ImapConnection::new_connection(
            &server_conf,
            format!(
                "{}-main-conn",
                uid_store.account_name.as_ref().trim_at_boundary(25),
            )
            .into(),
            uid_store.clone(),
            true,
        );

        Ok(Box::new(Self {
            _is_subscribed: is_subscribed,
            connection: Arc::new(ConnectionMutex::new(
                connection,
                server_conf.clone(),
                uid_store.clone(),
            )),
            server_conf,
            uid_store,
        }))
    }

    pub fn shell(&self) {
        let mut conn = ImapConnection::new_connection(
            &self.server_conf,
            format!(
                "{}-shell-conn",
                self.uid_store.account_name.as_ref().trim_at_boundary(25),
            )
            .into(),
            self.uid_store.clone(),
            true,
        );

        futures::executor::block_on(timeout(self.server_conf.timeout, conn.connect()))
            .unwrap()
            .unwrap();
        let mut res = Vec::with_capacity(8 * 1024);
        futures::executor::block_on(timeout(
            self.server_conf.timeout,
            conn.send_command(CommandBody::Noop),
        ))
        .unwrap()
        .unwrap();
        futures::executor::block_on(timeout(
            self.server_conf.timeout,
            conn.read_response(&mut res, RequiredResponses::empty()),
        ))
        .unwrap()
        .unwrap();

        let mut input = String::new();
        loop {
            use std::io;
            input.clear();

            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    futures::executor::block_on(timeout(
                        self.server_conf.timeout,
                        conn.send_command_raw(input.as_bytes()),
                    ))
                    .unwrap()
                    .unwrap();
                    futures::executor::block_on(timeout(
                        self.server_conf.timeout,
                        conn.read_lines(&mut res, None),
                    ))
                    .unwrap()
                    .unwrap();
                    if input.trim().eq_ignore_ascii_case("logout") {
                        break;
                    }
                    /*
                    if input.trim() == "IDLE" {
                        let mut iter = ImapBlockingConnection::from(conn);
                        while let Some(line) = iter.next() {
                            imap_log!(trace, "out: {}", unsafe { std::str::from_utf8_unchecked(&line) });
                        }
                        conn = iter.into_conn();
                    }
                    */
                    println!("S: {}", String::from_utf8_lossy(&res));
                }
                Err(error) => println!("error: {}", error),
            }
        }
    }

    pub async fn imap_mailboxes(
        connection: &Arc<ConnectionMutex>,
    ) -> Result<HashMap<MailboxHash, ImapMailbox>> {
        let mut mailboxes: HashMap<MailboxHash, ImapMailbox> = Default::default();
        let mut res = Vec::with_capacity(8 * 1024);
        let mut conn = connection.lock().await?;
        let has_list_status: bool = conn
            .uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case(b"LIST-STATUS"));
        if has_list_status {
            // [ref:TODO]: (#222) imap-codec does not support "LIST Command Extensions" currently.
            conn.send_command_raw(b"LIST \"\" \"*\" RETURN (STATUS (MESSAGES UNSEEN))")
                .await?;
            conn.read_response(
                &mut res,
                RequiredResponses::LIST_REQUIRED | RequiredResponses::STATUS,
            )
            .await?;
        } else {
            conn.send_command(CommandBody::list("", "*")?).await?;
            conn.read_response(&mut res, RequiredResponses::LIST_REQUIRED)
                .await?;
        }
        imap_log!(trace, conn, "LIST reply: {}", String::from_utf8_lossy(&res));
        for l in res.split_rn() {
            if !l.starts_with(b"*") {
                continue;
            }
            if let Ok(mut mailbox) = protocol_parser::list_mailbox_result(l).map(|(_, v)| v) {
                if let Some(parent) = mailbox.parent {
                    if let std::collections::hash_map::Entry::Vacant(e) = mailboxes.entry(parent) {
                        /* Insert dummy parent entry, populating only the children field. Later
                         * when we encounter the parent entry we will swap its children with
                         * dummy's */
                        e.insert(ImapMailbox {
                            children: vec![mailbox.hash],
                            ..ImapMailbox::default()
                        });
                    } else {
                        mailboxes
                            .entry(parent)
                            .and_modify(|e| e.children.push(mailbox.hash));
                    }
                }
                if let std::collections::hash_map::Entry::Vacant(e) = mailboxes.entry(mailbox.hash)
                {
                    e.insert(mailbox);
                } else {
                    let entry = mailboxes.entry(mailbox.hash).or_default();
                    std::mem::swap(&mut entry.children, &mut mailbox.children);
                    *entry = mailbox;
                }
            } else if let Ok(status) = protocol_parser::status_response(l).map(|(_, v)| v) {
                if let Some(mailbox_hash) = status.mailbox {
                    if mailboxes.contains_key(&mailbox_hash) {
                        let entry = mailboxes.entry(mailbox_hash).or_default();
                        if let Some(total) = status.messages {
                            entry.exists.lock().unwrap().set_not_yet_seen(total);
                        }
                        if let Some(total) = status.unseen {
                            entry.unseen.lock().unwrap().set_not_yet_seen(total);
                        }
                    }
                }
            } else {
                imap_log!(trace, conn, "parse error for {:?}", l);
            }
        }
        mailboxes.retain(|_, v| !v.hash.is_null());
        conn.send_command(CommandBody::lsub("", "*")?).await?;
        conn.read_response(&mut res, RequiredResponses::LSUB_REQUIRED)
            .await?;
        imap_log!(trace, conn, "LSUB reply: {}", String::from_utf8_lossy(&res));
        for l in res.split_rn() {
            if !l.starts_with(b"*") {
                continue;
            }
            if let Ok(subscription) = protocol_parser::list_mailbox_result(l).map(|(_, v)| v) {
                if let Some(f) = mailboxes.get_mut(&subscription.hash()) {
                    if f.special_usage() == SpecialUsageMailbox::Normal
                        && subscription.special_usage() != SpecialUsageMailbox::Normal
                    {
                        f.set_special_usage(subscription.special_usage())?;
                    }
                    f.is_subscribed = true;
                }
            } else {
                imap_log!(trace, conn, "parse error for {:?}", l);
            }
        }
        Ok(mailboxes)
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut keys: HashSet<&'static str> = Default::default();
        macro_rules! get_conf_val {
            ($s:ident[$var:literal]) => {{
                keys.insert($var);
                $s.extra.swap_remove($var).ok_or_else(|| {
                    Error::new(format!(
                        "Configuration error ({}): IMAP connection requires the field `{}` set",
                        $s.name.as_str(),
                        $var
                    ))
                })
            }};
            ($s:ident[$var:literal], $default:expr) => {{
                keys.insert($var);
                $s.extra
                    .swap_remove($var)
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
            }};
        }
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"])?;
        let use_oauth2: bool = get_conf_val!(s["use_oauth2"], false)?;
        keys.insert("server_password_command");
        if use_oauth2
            && !s.extra.contains_key("server_password_command")
            && !s.extra.contains_key("server_password")
        {
            return Err(Error::new(format!(
                "({}) `use_oauth2` use requires either `server_password` set or \
                 `server_password_command` set with a command that returns an OAUTH2 token. \
                 Consult documentation for guidance.",
                s.name,
            ))
            .set_kind(ErrorKind::Configuration));
        }
        if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"])?;
        } else if s.extra.contains_key("server_password") {
            return Err(Error::new(format!(
                "Configuration error ({}): both server_password and server_password_command are \
                 set, cannot choose",
                s.name.as_str(),
            ))
            .set_kind(ErrorKind::Configuration));
        }
        let _ = get_conf_val!(s["server_password_command"]);
        get_conf_val!(s["server_port"], 143)?;
        let use_tls = get_conf_val!(s["use_tls"], true)?;
        let use_starttls = get_conf_val!(s["use_starttls"], false)?;
        if !use_tls && use_starttls {
            return Err(Error::new(format!(
                "Configuration error ({}): incompatible use_tls and use_starttls values: use_tls \
                 = false, use_starttls = true",
                s.name.as_str(),
            )));
        }
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        #[cfg(feature = "sqlite3")]
        get_conf_val!(s["offline_cache"], true)?;
        #[cfg(not(feature = "sqlite3"))]
        {
            let keep_offline_cache = get_conf_val!(s["offline_cache"], false)?;
            if keep_offline_cache {
                return Err(Error::new(format!(
                    "({}) offline_cache is true but melib is not compiled with sqlite3",
                    s.name,
                )));
            }
        }
        get_conf_val!(s["use_idle"], true)?;
        get_conf_val!(s["use_condstore"], true)?;
        get_conf_val!(s["use_deflate"], true)?;
        get_conf_val!(s["use_auth_anonymous"], false)?;
        get_conf_val!(s["use_id"], false)?;
        let _timeout = get_conf_val!(s["timeout"], 16_u64)?;
        let extra_keys = s
            .extra
            .keys()
            .map(String::as_str)
            .collect::<HashSet<&str>>();
        let diff = extra_keys.difference(&keys).collect::<Vec<&&str>>();
        if !diff.is_empty() {
            return Err(Error::new(format!(
                "Configuration error ({}): the following flags are set but are not recognized: \
                 {:?}.",
                s.name.as_str(),
                diff
            )));
        }
        Ok(())
    }

    pub fn capabilities(&self) -> Vec<String> {
        self.uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .map(|c| String::from_utf8_lossy(c).into())
            .collect::<Vec<String>>()
    }
}
