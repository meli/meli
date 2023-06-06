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

use smallvec::SmallVec;
#[macro_use]
mod protocol_parser;
pub use protocol_parser::{UntaggedResponse::*, *};
mod mailbox;
pub use mailbox::*;
mod operations;
pub use operations::*;
mod connection;
pub use connection::*;
mod watch;
pub use watch::*;
mod search;
pub use search::*;
mod cache;
use cache::{ImapCacheReset, ModSequence};
pub mod error;
pub mod managesieve;
mod untagged;

use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet},
    convert::TryFrom,
    hash::Hasher,
    pin::Pin,
    str::FromStr,
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};

use futures::{lock::Mutex as FutureMutex, stream::Stream};
use imap_codec::{
    command::CommandBody,
    core::Literal,
    flag::{Flag as ImapCodecFlag, StoreResponse, StoreType},
    sequence::{SequenceSet, ONE},
};

use crate::{
    backends::{
        RefreshEventKind::{self, *},
        *,
    },
    collection::Collection,
    conf::AccountSettings,
    connections::timeout,
    email::{parser::BytesExt, *},
    error::{Error, Result, ResultIntoError},
};

pub type ImapNum = usize;
pub type UID = ImapNum;
pub type UIDVALIDITY = UID;
pub type MessageSequenceNumber = ImapNum;

pub static SUPPORTED_CAPABILITIES: &[&str] = &[
    "AUTH=OAUTH2",
    #[cfg(feature = "deflate_compression")]
    "COMPRESS=DEFLATE",
    "CONDSTORE",
    "ENABLE",
    "IDLE",
    "IMAP4REV1",
    "LIST-EXTENDED",
    "LIST-STATUS",
    "LITERAL+",
    "LOGIN",
    "LOGINDISABLED",
    "MOVE",
    "SPECIAL-USE",
    "UNSELECT",
];

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<Vec<u8>>,
    flags: Option<Flag>,
}

#[derive(Debug, Clone)]
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

type Capabilities = HashSet<Vec<u8>>;

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
    account_hash: AccountHash,
    account_name: Arc<String>,
    keep_offline_cache: bool,
    capabilities: Arc<Mutex<Capabilities>>,
    hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    uid_index: Arc<Mutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,
    msn_index: Arc<Mutex<HashMap<MailboxHash, Vec<UID>>>>,

    byte_cache: Arc<Mutex<HashMap<UID, EnvelopeCache>>>,
    collection: Collection,

    /* Offline caching */
    uidvalidity: Arc<Mutex<HashMap<MailboxHash, UID>>>,
    envelopes: Arc<Mutex<HashMap<EnvelopeHash, cache::CachedEnvelope>>>,
    max_uids: Arc<Mutex<HashMap<MailboxHash, UID>>>,
    modseq: Arc<Mutex<HashMap<EnvelopeHash, ModSequence>>>,
    highestmodseqs: Arc<Mutex<HashMap<MailboxHash, std::result::Result<ModSequence, ()>>>>,
    mailboxes: Arc<FutureMutex<HashMap<MailboxHash, ImapMailbox>>>,
    is_online: Arc<Mutex<(SystemTime, Result<()>)>>,
    event_consumer: BackendEventConsumer,
    timeout: Option<Duration>,
}

impl UIDStore {
    fn new(
        account_hash: AccountHash,
        account_name: Arc<String>,
        event_consumer: BackendEventConsumer,
        timeout: Option<Duration>,
    ) -> Self {
        UIDStore {
            account_hash,
            account_name,
            keep_offline_cache: false,
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
}

#[derive(Debug)]
pub struct ImapType {
    _is_subscribed: Arc<IsSubscribedFn>,
    connection: Arc<FutureMutex<ImapConnection>>,
    server_conf: ImapServerConf,
    uid_store: Arc<UIDStore>,
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
                    #[cfg(feature = "deflate_compression")]
                    deflate,
                    condstore,
                    oauth2,
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
                        #[cfg(feature = "deflate_compression")]
                        {
                            if deflate {
                                *status = MailBackendExtensionStatus::Enabled { comment: None };
                            } else {
                                *status = MailBackendExtensionStatus::Supported {
                                    comment: Some("Disabled by user configuration"),
                                };
                            }
                        }
                        #[cfg(not(feature = "deflate_compression"))]
                        {
                            *status = MailBackendExtensionStatus::Unsupported {
                                comment: Some("melib not compiled with DEFLATE."),
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
        MailBackendCapabilities {
            is_async: true,
            is_remote: true,
            supports_search: true,
            extensions: Some(extensions),
            supports_tags: true,
            supports_submission: false,
        }
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let cache_handle = {
            #[cfg(feature = "sqlite3")]
            if self.uid_store.keep_offline_cache {
                match cache::Sqlite3Cache::get(self.uid_store.clone()).chain_err_summary(|| {
                    format!(
                        "Could not initialize cache for IMAP account {}. Resetting database.",
                        self.uid_store.account_name
                    )
                }) {
                    Ok(v) => Some(v),
                    Err(err) => {
                        (self.uid_store.event_consumer)(self.uid_store.account_hash, err.into());
                        match cache::Sqlite3Cache::reset_db(&self.uid_store)
                            .and_then(|()| cache::Sqlite3Cache::get(self.uid_store.clone()))
                            .chain_err_summary(|| "Could not reset IMAP cache database.")
                        {
                            Ok(v) => Some(v),
                            Err(err) => {
                                (self.uid_store.event_consumer)(
                                    self.uid_store.account_hash,
                                    err.into(),
                                );
                                None
                            }
                        }
                    }
                }
            } else {
                None
            }
            #[cfg(not(feature = "sqlite3"))]
            None
        };
        let mut state = FetchState {
            stage: if self.uid_store.keep_offline_cache && cache_handle.is_some() {
                FetchStage::InitialCache
            } else {
                FetchStage::InitialFresh
            },
            connection: self.connection.clone(),
            mailbox_hash,
            uid_store: self.uid_store.clone(),
            cache_handle,
        };

        /* do this in a closure to prevent recursion limit error in async_stream
         * macro */
        let prepare_cl = |f: &ImapMailbox| {
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
        };
        Ok(Box::pin(async_stream::try_stream! {
            {
                let f = &state.uid_store.mailboxes.lock().await[&mailbox_hash];
                prepare_cl(f);
                if f.no_select {
                    yield vec![];
                    return;
                }
            };
            loop {
                let res = fetch_hlpr(&mut state).await.map_err(|err| {
                    debug!("fetch_hlpr err {:?}", &err);
                    err})?;
                yield res;
                if state.stage == FetchStage::Finished {
                    return;
                }

            }
        }))
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let main_conn = self.connection.clone();
        let uid_store = self.uid_store.clone();
        Ok(Box::pin(async move {
            let inbox = timeout(uid_store.timeout, uid_store.mailboxes.lock())
                .await?
                .get(&mailbox_hash)
                .map(std::clone::Clone::clone)
                .unwrap();
            let mut conn = timeout(uid_store.timeout, main_conn.lock()).await?;
            watch::examine_updates(inbox, &mut conn, &uid_store).await?;
            Ok(())
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
            let new_mailboxes = ImapType::imap_mailboxes(&connection).await?;
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
                .filter(|(_, f)| f.is_subscribed)
                .map(|(h, f)| (*h, Box::new(Clone::clone(f)) as Mailbox))
                .collect())
        }))
    }

    fn is_online(&self) -> ResultFuture<()> {
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout;
        Ok(Box::pin(async move {
            match timeout(timeout_dur, connection.lock()).await {
                Ok(mut conn) => {
                    debug!("is_online");
                    match timeout(timeout_dur, conn.connect()).await {
                        Ok(Ok(())) => Ok(()),
                        Err(err) | Ok(Err(err)) => {
                            conn.stream = Err(err.clone());
                            conn.connect().await
                        }
                    }
                }
                Err(err) => Err(err),
            }
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
                    conn: ImapConnection::new_connection(&server_conf, uid_store.clone()),
                    main_conn: main_conn.clone(),
                    uid_store: uid_store.clone(),
                })
                .await
            } else {
                poll_with_examine(ImapWatchKit {
                    conn: ImapConnection::new_connection(&server_conf, uid_store.clone()),
                    main_conn: main_conn.clone(),
                    uid_store: uid_store.clone(),
                })
                .await
            } {
                let mut main_conn_lck = timeout(uid_store.timeout, main_conn.lock()).await?;
                if err.kind.is_network() {
                    uid_store.is_online.lock().unwrap().1 = Err(err.clone());
                } else {
                    return Err(err);
                }
                debug!("Watch failure: {}", err.to_string());
                match timeout(uid_store.timeout, main_conn_lck.connect())
                    .await
                    .and_then(|res| res)
                {
                    Err(err2) => {
                        debug!("Watch reconnect attempt failed: {}", err2.to_string());
                    }
                    Ok(()) => {
                        debug!("Watch reconnect attempt succesful");
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
            debug!("watch future returning");
            Ok(())
        }))
    }

    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        let (uid, mailbox_hash) =
            if let Some(v) = self.uid_store.hash_index.lock().unwrap().get(&hash) {
                *v
            } else {
                return Err(Error::new(
                    "Message not found in local cache, it might have been deleted before you \
                     requested it.",
                ));
            };
        Ok(Box::new(ImapOp::new(
            uid,
            mailbox_hash,
            self.connection.clone(),
            self.uid_store.clone(),
        )))
    }

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await;
            conn.select_mailbox(mailbox_hash, &mut response, true)
                .await?;
            let path = {
                let mailboxes = uid_store.mailboxes.lock().await;

                let mailbox = mailboxes.get(&mailbox_hash).ok_or_else(|| {
                    Error::new(format!("Mailbox with hash {} not found.", mailbox_hash))
                })?;
                if !mailbox.permissions.lock().unwrap().create_messages {
                    return Err(Error::new(format!(
                        "You are not allowed to create messages in mailbox {}",
                        mailbox.path()
                    )));
                }

                mailbox.imap_path().to_string()
            };
            let flags = flags.unwrap_or_else(Flag::empty);
            let has_literal_plus: bool = uid_store
                .capabilities
                .lock()
                .unwrap()
                .iter()
                .any(|cap| cap.eq_ignore_ascii_case(b"LITERAL+"));
            let data = if has_literal_plus {
                Literal::try_from(bytes)?.into_non_sync()
            } else {
                Literal::try_from(bytes)?
            };
            conn.send_command(CommandBody::append(
                path,
                flags.derive_imap_codec_flags(),
                None,
                data,
            )?)
            .await?;
            conn.read_response(&mut response, RequiredResponses::empty())
                .await?;
            Ok(())
        }))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        let has_move: bool = move_
            && uid_store
                .capabilities
                .lock()
                .unwrap()
                .iter()
                .any(|cap| cap.eq_ignore_ascii_case(b"MOVE"));
        Ok(Box::pin(async move {
            let uids: SmallVec<[UID; 64]> = {
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
            let dest_path = {
                let mailboxes = uid_store.mailboxes.lock().await;
                let mailbox = mailboxes
                    .get(&destination_mailbox_hash)
                    .ok_or_else(|| Error::new("Destination mailbox not found"))?;
                mailbox.imap_path().to_string()
            };
            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await;
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
        flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()> {
        let connection = self.connection.clone();
        let uid_store = self.uid_store.clone();
        Ok(Box::pin(async move {
            let uids: SmallVec<[UID; 64]> = {
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

            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await;
            conn.select_mailbox(mailbox_hash, &mut response, false)
                .await?;
            if flags.iter().any(|(_, b)| *b) {
                /* Set flags/tags to true */
                let mut set_seen = false;
                let command = {
                    let mut tag_lck = uid_store.collection.tag_index.write().unwrap();
                    let mut cmd = format!("UID STORE {}", uids[0]);
                    for uid in uids.iter().skip(1) {
                        cmd = format!("{},{}", cmd, uid);
                    }
                    cmd = format!("{} +FLAGS (", cmd);
                    for (f, v) in flags.iter() {
                        if !*v {
                            continue;
                        }
                        match f {
                            Ok(flag) if *flag == Flag::REPLIED => {
                                cmd.push_str("\\Answered ");
                            }
                            Ok(flag) if *flag == Flag::FLAGGED => {
                                cmd.push_str("\\Flagged ");
                            }
                            Ok(flag) if *flag == Flag::TRASHED => {
                                cmd.push_str("\\Deleted ");
                            }
                            Ok(flag) if *flag == Flag::SEEN => {
                                cmd.push_str("\\Seen ");
                                set_seen = true;
                            }
                            Ok(flag) if *flag == Flag::DRAFT => {
                                cmd.push_str("\\Draft ");
                            }
                            Ok(_) => {
                                log::error!(
                                    "Application error: more than one flag bit set in set_flags: \
                                     {:?}",
                                    flags
                                );
                                return Err(Error::new(format!(
                                    "Application error: more than one flag bit set in set_flags: \
                                     {:?}",
                                    flags
                                ))
                                .set_kind(crate::ErrorKind::Bug));
                            }
                            Err(tag) => {
                                let hash = TagHash::from_bytes(tag.as_bytes());
                                if !tag_lck.contains_key(&hash) {
                                    tag_lck.insert(hash, tag.to_string());
                                }
                                cmd.push_str(tag);
                                cmd.push(' ');
                            }
                        }
                    }
                    // pop last space
                    cmd.pop();
                    cmd.push(')');
                    cmd
                };
                conn.send_command_raw(command.as_bytes()).await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
                if set_seen {
                    let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                    if let Ok(mut unseen) = f.unseen.lock() {
                        for env_hash in env_hashes.iter() {
                            unseen.remove(env_hash);
                        }
                    };
                }
            }
            if flags.iter().any(|(_, b)| !*b) {
                let mut set_unseen = false;
                /* Set flags/tags to false */
                let command = {
                    let mut cmd = format!("UID STORE {}", uids[0]);
                    for uid in uids.iter().skip(1) {
                        cmd = format!("{},{}", cmd, uid);
                    }
                    cmd = format!("{} -FLAGS (", cmd);
                    for (f, v) in flags.iter() {
                        if *v {
                            continue;
                        }
                        match f {
                            Ok(flag) if *flag == Flag::REPLIED => {
                                cmd.push_str("\\Answered ");
                            }
                            Ok(flag) if *flag == Flag::FLAGGED => {
                                cmd.push_str("\\Flagged ");
                            }
                            Ok(flag) if *flag == Flag::TRASHED => {
                                cmd.push_str("\\Deleted ");
                            }
                            Ok(flag) if *flag == Flag::SEEN => {
                                cmd.push_str("\\Seen ");
                                set_unseen = true;
                            }
                            Ok(flag) if *flag == Flag::DRAFT => {
                                cmd.push_str("\\Draft ");
                            }
                            Ok(_) => {
                                log::error!(
                                    "Application error: more than one flag bit set in set_flags: \
                                     {:?}",
                                    flags
                                );
                                return Err(Error::new(format!(
                                    "Application error: more than one flag bit set in set_flags: \
                                     {:?}",
                                    flags
                                )));
                            }
                            Err(tag) => {
                                cmd.push_str(tag);
                                cmd.push(' ');
                            }
                        }
                    }
                    // pop last space
                    cmd.pop();
                    cmd.push(')');
                    cmd
                };
                conn.send_command_raw(command.as_bytes()).await?;
                conn.read_response(&mut response, RequiredResponses::empty())
                    .await?;
                if set_unseen {
                    let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                    if let Ok(mut unseen) = f.unseen.lock() {
                        for env_hash in env_hashes.iter() {
                            unseen.insert_new(env_hash);
                        }
                    };
                }
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
            env_hashes,
            mailbox_hash,
            smallvec::smallvec![(Ok(Flag::TRASHED), true)],
        )?;
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            flag_future.await?;
            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await;
            conn.send_command(CommandBody::Expunge).await?;
            conn.read_response(&mut response, RequiredResponses::empty())
                .await?;
            debug!("EXPUNGE response: {}", &String::from_utf8_lossy(&response));
            Ok(())
        }))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
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
             * Each root mailbox has a hierarchy delimeter reported by the LIST entry.
             * All paths must use this delimeter to indicate children of this
             * mailbox.
             *
             * A new root mailbox should have the default delimeter, which can be found
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
                        debug!("path starts with {:?}", &root_mailbox);
                        path = path.replace(
                            '/',
                            (root_mailbox.separator as char).encode_utf8(&mut [0; 4]),
                        );
                        break;
                    }
                }

                /* FIXME  Do not try to CREATE a sub-mailbox in a mailbox
                 * that has the \Noinferiors flag set. */
            }

            let mut response = Vec::with_capacity(8 * 1024);
            {
                let mut conn_lck = connection.lock().await;
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
            let new_hash = MailboxHash::from_bytes(path.as_str().as_bytes());
            uid_store.mailboxes.lock().await.clear();
            Ok((
                new_hash,
                new_mailbox_fut?.await.map_err(|err| {
                    Error::new(format!(
                        "Mailbox create was succesful (returned `{}`) but listing mailboxes \
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
                let mut conn_lck = connection.lock().await;
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
                    .send_command(debug!(CommandBody::delete(imap_path.as_str())?))
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
                    "Mailbox delete was succesful (returned `{}`) but listing mailboxes \
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
                let mut conn_lck = connection.lock().await;
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
            let command: String;
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
                command = format!(
                    "RENAME \"{}\" \"{}\"",
                    mailboxes[&mailbox_hash].imap_path(),
                    new_path
                );
            }
            {
                let mut conn_lck = connection.lock().await;
                conn_lck
                    .send_command_raw(debug!(command).as_bytes())
                    .await?;
                conn_lck
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
            }
            let new_hash = MailboxHash::from_bytes(new_path.as_str().as_bytes());
            let ret: Result<()> = ImapResponse::try_from(response.as_slice())?.into();
            ret?;
            uid_store.mailboxes.lock().await.clear();
            new_mailbox_fut?.await.map_err(|err| {
                format!(
                    "Mailbox rename was succesful (returned `{}`) but listing mailboxes \
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
        query: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        if mailbox_hash.is_none() {
            return Err(Error::new(
                "Cannot search without specifying mailbox on IMAP",
            ));
        }
        let mailbox_hash = mailbox_hash.unwrap();
        let query_str = query.to_imap_search();
        let connection = self.connection.clone();
        let uid_store = self.uid_store.clone();

        Ok(Box::pin(async move {
            let mut response = Vec::with_capacity(8 * 1024);
            let mut conn = connection.lock().await;
            conn.examine_mailbox(mailbox_hash, &mut response, false)
                .await?;
            conn.send_command_raw(
                format!("UID SEARCH CHARSET UTF-8 {}", query_str.trim()).as_bytes(),
            )
            .await?;
            conn.read_response(&mut response, RequiredResponses::SEARCH)
                .await?;
            debug!(
                "searching for {} returned: {}",
                query_str,
                String::from_utf8_lossy(&response)
            );

            for l in response.split_rn() {
                if l.starts_with(b"* SEARCH") {
                    use std::iter::FromIterator;
                    let uid_index = uid_store.uid_index.lock()?;
                    return Ok(SmallVec::from_iter(
                        String::from_utf8_lossy(l[b"* SEARCH".len()..].trim())
                            .split_whitespace()
                            .map(UID::from_str)
                            .filter_map(std::result::Result::ok)
                            .filter_map(|uid| uid_index.get(&(mailbox_hash, uid)))
                            .copied(),
                    ));
                }
            }
            Err(Error::new(String::from_utf8_lossy(&response).to_string()))
        }))
    }
}

impl ImapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        let server_hostname = get_conf_val!(s["server_hostname"])?;
        let server_username = get_conf_val!(s["server_username"])?;
        let use_oauth2: bool = get_conf_val!(s["use_oauth2"], false)?;

        if use_oauth2 && !s.extra.contains_key("server_password_command") {
            return Err(Error::new(format!(
                "({}) `use_oauth2` use requires `server_password_command` set with a command that \
                 returns an OAUTH2 token. Consult documentation for guidance.",
                s.name,
            )));
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
                "({}) keep_offline_cache is true but melib is not compiled with sqlite3",
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
                    #[cfg(feature = "deflate_compression")]
                    deflate: get_conf_val!(s["use_deflate"], true)?,
                    oauth2: use_oauth2,
                },
            },
            timeout,
        };
        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let account_name = Arc::new(s.name.to_string());
        let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
            keep_offline_cache,
            ..UIDStore::new(
                account_hash,
                account_name,
                event_consumer,
                server_conf.timeout,
            )
        });
        let connection = ImapConnection::new_connection(&server_conf, uid_store.clone());

        Ok(Box::new(ImapType {
            server_conf,
            _is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            connection: Arc::new(FutureMutex::new(connection)),
            uid_store,
        }))
    }

    pub fn shell(&mut self) {
        let mut conn = ImapConnection::new_connection(&self.server_conf, self.uid_store.clone());

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
                        conn.read_lines(&mut res, Vec::new()),
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
                            debug!("out: {}", unsafe { std::str::from_utf8_unchecked(&line) });
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
        connection: &Arc<FutureMutex<ImapConnection>>,
    ) -> Result<HashMap<MailboxHash, ImapMailbox>> {
        let mut mailboxes: HashMap<MailboxHash, ImapMailbox> = Default::default();
        let mut res = Vec::with_capacity(8 * 1024);
        let mut conn = connection.lock().await;
        let has_list_status: bool = conn
            .uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case(b"LIST-STATUS"));
        if has_list_status {
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
        debug!("LIST reply: {}", String::from_utf8_lossy(&res));
        for l in res.split_rn() {
            if !l.starts_with(b"*") {
                continue;
            }
            if let Ok(mut mailbox) = protocol_parser::list_mailbox_result(l).map(|(_, v)| v) {
                if let Some(parent) = mailbox.parent {
                    if mailboxes.contains_key(&parent) {
                        mailboxes
                            .entry(parent)
                            .and_modify(|e| e.children.push(mailbox.hash));
                    } else {
                        /* Insert dummy parent entry, populating only the children field. Later
                         * when we encounter the parent entry we will swap its children with
                         * dummy's */
                        mailboxes.insert(
                            parent,
                            ImapMailbox {
                                children: vec![mailbox.hash],
                                ..ImapMailbox::default()
                            },
                        );
                    }
                }
                if mailboxes.contains_key(&mailbox.hash) {
                    let entry = mailboxes.entry(mailbox.hash).or_default();
                    std::mem::swap(&mut entry.children, &mut mailbox.children);
                    *entry = mailbox;
                } else {
                    mailboxes.insert(mailbox.hash, mailbox);
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
                debug!("parse error for {:?}", l);
            }
        }
        mailboxes.retain(|_, v| !v.hash.is_null());
        conn.send_command(CommandBody::lsub("", "*")?).await?;
        conn.read_response(&mut res, RequiredResponses::LSUB_REQUIRED)
            .await?;
        debug!("LSUB reply: {}", String::from_utf8_lossy(&res));
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
                debug!("parse error for {:?}", l);
            }
        }
        Ok(mailboxes)
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut keys: HashSet<&'static str> = Default::default();
        macro_rules! get_conf_val {
            ($s:ident[$var:literal]) => {{
                keys.insert($var);
                $s.extra.remove($var).ok_or_else(|| {
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
            }};
        }
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"])?;
        let use_oauth2: bool = get_conf_val!(s["use_oauth2"], false)?;
        keys.insert("server_password_command");
        if !s.extra.contains_key("server_password_command") {
            if use_oauth2 {
                return Err(Error::new(format!(
                    "({}) `use_oauth2` use requires `server_password_command` set with a command \
                     that returns an OAUTH2 token. Consult documentation for guidance.",
                    s.name,
                )));
            }
            get_conf_val!(s["server_password"])?;
        } else if s.extra.contains_key("server_password") {
            return Err(Error::new(format!(
                "Configuration error ({}): both server_password and server_password_command are \
                 set, cannot choose",
                s.name.as_str(),
            )));
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
                    "({}) keep_offline_cache is true but melib is not compiled with sqlite3",
                    s.name,
                )));
            }
        }
        get_conf_val!(s["use_idle"], true)?;
        get_conf_val!(s["use_condstore"], true)?;
        #[cfg(feature = "deflate_compression")]
        get_conf_val!(s["use_deflate"], true)?;
        #[cfg(not(feature = "deflate_compression"))]
        if s.extra.contains_key("use_deflate") {
            return Err(Error::new(format!(
                "Configuration error ({}): setting `use_deflate` is set but this version of meli \
                 isn't compiled with DEFLATE support.",
                s.name.as_str(),
            )));
        }
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

#[derive(Debug, PartialEq, Copy, Clone)]
enum FetchStage {
    InitialFresh,
    InitialCache,
    ResyncCache,
    FreshFetch { max_uid: UID },
    Finished,
}

#[derive(Debug)]
struct FetchState {
    stage: FetchStage,
    connection: Arc<FutureMutex<ImapConnection>>,
    mailbox_hash: MailboxHash,
    uid_store: Arc<UIDStore>,
    cache_handle: Option<Box<dyn cache::ImapCache>>,
}

async fn fetch_hlpr(state: &mut FetchState) -> Result<Vec<Envelope>> {
    debug!((state.mailbox_hash, &state.stage));
    loop {
        match state.stage {
            FetchStage::InitialFresh => {
                let select_response = state
                    .connection
                    .lock()
                    .await
                    .init_mailbox(state.mailbox_hash)
                    .await?;
                if let Some(ref mut cache_handle) = state.cache_handle {
                    if let Err(err) = cache_handle
                        .update_mailbox(state.mailbox_hash, &select_response)
                        .chain_err_summary(|| {
                            format!("Could not update cache for mailbox {}.", state.mailbox_hash)
                        })
                    {
                        (state.uid_store.event_consumer)(state.uid_store.account_hash, err.into());
                    }
                }

                if select_response.exists == 0 {
                    state.stage = FetchStage::Finished;
                    return Ok(Vec::new());
                }
                state.stage = FetchStage::FreshFetch {
                    max_uid: select_response.uidnext - 1,
                };
                continue;
            }
            FetchStage::InitialCache => {
                match cache::fetch_cached_envs(state).await {
                    Err(err) => {
                        log::error!(
                            "IMAP cache error: could not fetch cache for {}. Reason: {}",
                            state.uid_store.account_name,
                            err
                        );
                        /* Try resetting the database */
                        if let Some(ref mut cache_handle) = state.cache_handle {
                            if let Err(err) = cache_handle.reset() {
                                log::error!(
                                    "IMAP cache error: could not reset cache for {}. Reason: {}",
                                    state.uid_store.account_name,
                                    err
                                );
                            }
                        }
                        state.stage = FetchStage::InitialFresh;
                        continue;
                    }
                    Ok(None) => {
                        state.stage = FetchStage::InitialFresh;
                        continue;
                    }
                    Ok(Some(cached_payload)) => {
                        state.stage = FetchStage::ResyncCache;
                        debug!(
                            "fetch_hlpr fetch_cached_envs payload {} len for mailbox_hash {}",
                            cached_payload.len(),
                            state.mailbox_hash
                        );
                        let (mailbox_exists, unseen) = {
                            let f = &state.uid_store.mailboxes.lock().await[&state.mailbox_hash];
                            (f.exists.clone(), f.unseen.clone())
                        };
                        unseen.lock().unwrap().insert_existing_set(
                            cached_payload
                                .iter()
                                .filter_map(|env| {
                                    if !env.is_seen() {
                                        Some(env.hash())
                                    } else {
                                        None
                                    }
                                })
                                .collect(),
                        );
                        mailbox_exists.lock().unwrap().insert_existing_set(
                            cached_payload.iter().map(|env| env.hash()).collect::<_>(),
                        );
                        return Ok(cached_payload);
                    }
                }
            }
            FetchStage::ResyncCache => {
                let mailbox_hash = state.mailbox_hash;
                let mut conn = state.connection.lock().await;
                let res = conn.resync(mailbox_hash).await;
                if let Ok(Some(payload)) = res {
                    state.stage = FetchStage::Finished;
                    return Ok(payload);
                }
                state.stage = FetchStage::InitialFresh;
                continue;
            }
            FetchStage::FreshFetch { max_uid } => {
                let FetchState {
                    ref mut stage,
                    ref connection,
                    mailbox_hash,
                    ref uid_store,
                    ref mut cache_handle,
                } = state;
                let mailbox_hash = *mailbox_hash;
                let mut our_unseen: BTreeSet<EnvelopeHash> = BTreeSet::default();
                let (mailbox_path, mailbox_exists, no_select, unseen) = {
                    let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                    (
                        f.imap_path().to_string(),
                        f.exists.clone(),
                        f.no_select,
                        f.unseen.clone(),
                    )
                };
                if no_select {
                    state.stage = FetchStage::Finished;
                    return Ok(Vec::new());
                }
                let mut conn = connection.lock().await;
                let mut response = Vec::with_capacity(8 * 1024);
                let max_uid_left = max_uid;
                let chunk_size = 250;

                let mut envelopes = Vec::with_capacity(chunk_size);
                conn.examine_mailbox(mailbox_hash, &mut response, false)
                    .await?;
                if max_uid_left > 0 {
                    debug!("{} max_uid_left= {}", mailbox_hash, max_uid_left);
                    let sequence_set = if max_uid_left == 1 {
                        SequenceSet::from(ONE)
                    } else {
                        let min = std::cmp::max(max_uid_left.saturating_sub(chunk_size), 1);
                        let max = max_uid_left;

                        SequenceSet::try_from(min..=max)?
                    };
                    conn.send_command(CommandBody::Fetch {
                        sequence_set,
                        attributes: common_attributes(),
                        uid: true,
                    })
                    .await?;
                    conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                        .await
                        .chain_err_summary(|| {
                            format!(
                                "Could not parse fetch response for mailbox {}",
                                mailbox_path
                            )
                        })?;
                    let (_, mut v, _) = protocol_parser::fetch_responses(&response)?;
                    debug!(
                        "fetch response is {} bytes and {} lines and has {} parsed Envelopes",
                        response.len(),
                        String::from_utf8_lossy(&response).lines().count(),
                        v.len()
                    );
                    for FetchResponse {
                        ref uid,
                        ref mut envelope,
                        ref mut flags,
                        raw_fetch_value,
                        ref references,
                        ..
                    } in v.iter_mut()
                    {
                        if uid.is_none() || envelope.is_none() || flags.is_none() {
                            debug!("BUG? in fetch is none");
                            debug!(uid);
                            debug!(envelope);
                            debug!(flags);
                            debug!("response was: {}", String::from_utf8_lossy(&response));
                            debug!(conn.process_untagged(raw_fetch_value).await)?;
                            continue;
                        }
                        let uid = uid.unwrap();
                        let env = envelope.as_mut().unwrap();
                        env.set_hash(generate_envelope_hash(&mailbox_path, &uid));
                        if let Some(value) = references {
                            env.set_references(value);
                        }
                        let mut tag_lck = uid_store.collection.tag_index.write().unwrap();
                        if let Some((flags, keywords)) = flags {
                            env.set_flags(*flags);
                            if !env.is_seen() {
                                our_unseen.insert(env.hash());
                            }
                            for f in keywords {
                                let hash = TagHash::from_bytes(f.as_bytes());
                                if !tag_lck.contains_key(&hash) {
                                    tag_lck.insert(hash, f.to_string());
                                }
                                env.tags_mut().push(hash);
                            }
                        }
                    }
                    if let Some(ref mut cache_handle) = cache_handle {
                        if let Err(err) = cache_handle
                            .insert_envelopes(mailbox_hash, &v)
                            .chain_err_summary(|| {
                                format!(
                                    "Could not save envelopes in cache for mailbox {}",
                                    mailbox_path
                                )
                            })
                        {
                            (state.uid_store.event_consumer)(
                                state.uid_store.account_hash,
                                err.into(),
                            );
                        }
                    }

                    for FetchResponse {
                        uid,
                        message_sequence_number,
                        envelope,
                        ..
                    } in v
                    {
                        let uid = uid.unwrap();
                        let env = envelope.unwrap();
                        /*
                        debug!(
                            "env hash {} {} UID = {} MSN = {}",
                            env.hash(),
                            env.subject(),
                            uid,
                            message_sequence_number
                        );
                        */
                        uid_store
                            .msn_index
                            .lock()
                            .unwrap()
                            .entry(mailbox_hash)
                            .or_default()
                            .insert(message_sequence_number - 1, uid);
                        uid_store
                            .hash_index
                            .lock()
                            .unwrap()
                            .insert(env.hash(), (uid, mailbox_hash));
                        uid_store
                            .uid_index
                            .lock()
                            .unwrap()
                            .insert((mailbox_hash, uid), env.hash());
                        envelopes.push(env);
                    }
                    unseen.lock().unwrap().insert_existing_set(our_unseen);
                    mailbox_exists
                        .lock()
                        .unwrap()
                        .insert_existing_set(envelopes.iter().map(|env| env.hash()).collect::<_>());
                    drop(conn);
                }
                if max_uid_left <= 1 {
                    unseen.lock().unwrap().set_not_yet_seen(0);
                    mailbox_exists.lock().unwrap().set_not_yet_seen(0);
                    *stage = FetchStage::Finished;
                } else {
                    *stage = FetchStage::FreshFetch {
                        max_uid: std::cmp::max(max_uid_left.saturating_sub(chunk_size + 1), 1),
                    };
                }
                return Ok(envelopes);
            }
            FetchStage::Finished => {
                return Ok(vec![]);
            }
        }
    }
}
