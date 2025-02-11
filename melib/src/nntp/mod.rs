/*
 * meli - nntp module.
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

//! # NNTP backend / client
//!
//! Implements an NNTP client as specified by [RFC 3977: Network News Transfer
//! Protocol (NNTP)](https://datatracker.ietf.org/doc/html/rfc3977). Also implements [RFC 6048: Network News
//! Transfer Protocol (NNTP) Additions to LIST
//! Command](https://datatracker.ietf.org/doc/html/rfc6048).

use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet},
    hash::Hasher,
    str::FromStr,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

mod store;
pub use store::*;
#[macro_use]
mod protocol_parser;
pub use protocol_parser::*;
mod mailbox;
pub use mailbox::*;
mod operations;
pub use operations::*;
mod connection;
pub use connection::*;

use crate::{
    backends::prelude::*,
    email::{
        address::{MessageID, StrBuild},
        Mail,
    },
    error::{Error, ErrorKind, Result, ResultIntoError},
    parser::BytesExt,
    utils::futures::timeout,
};
pub type UID = usize;

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            Error::new(format!(
                "{}: NNTP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
            .set_kind(ErrorKind::Configuration)
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    Error::new(format!(
                        "{}: NNTP connection has invalid configuration",
                        $s.name.as_str(),
                    ))
                    .set_kind(ErrorKind::Configuration)
                    .set_source(Some(Arc::new(
                        Error::new(format!("Invalid value for field `{}`: {}\n{}", $var, v, e))
                            .set_kind(ErrorKind::ValueError),
                    )))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

pub static SUPPORTED_CAPABILITIES: &[&str] = &[
    "COMPRESS DEFLATE",
    "VERSION 2",
    "NEWNEWS",
    "POST",
    "OVER",
    "OVER MSGID",
    "READER",
    "STARTTLS",
    "HDR",
    "AUTHINFO USER",
];

#[derive(Clone, Debug)]
pub struct NntpServerConf {
    pub server_hostname: String,
    pub server_username: String,
    pub server_password: String,
    pub server_port: u16,
    pub use_starttls: bool,
    pub use_tls: bool,
    pub require_auth: bool,
    pub danger_accept_invalid_certs: bool,
    pub extension_use: NntpExtensionUse,
    pub timeout_dur: Option<Duration>,
}

type Capabilities = HashSet<String>;

#[derive(Debug)]
pub struct UIDStore {
    pub account_hash: AccountHash,
    pub account_name: Arc<str>,
    pub capabilities: Arc<Mutex<Capabilities>>,
    pub message_id_index: Arc<FutureMutex<HashMap<MessageID, EnvelopeHash>>>,
    pub hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    pub uid_index: Arc<FutureMutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,

    pub collection: Collection,
    pub store: Arc<FutureMutex<Option<store::Store>>>,
    pub mailboxes: Arc<FutureMutex<IndexMap<MailboxHash, NntpMailbox>>>,
    pub is_online: Arc<FutureMutex<(Instant, Result<()>)>>,
    pub is_subscribed: IsSubscribedFn,
    pub event_consumer: BackendEventConsumer,
}

impl UIDStore {
    fn new(
        account_hash: AccountHash,
        account_name: Arc<str>,
        event_consumer: BackendEventConsumer,
    ) -> Self {
        Self {
            account_hash,
            account_name,
            event_consumer,
            store: Default::default(),
            capabilities: Default::default(),
            message_id_index: Default::default(),
            hash_index: Default::default(),
            uid_index: Default::default(),
            mailboxes: Arc::new(FutureMutex::new(Default::default())),
            collection: Collection::new(),
            is_online: Arc::new(FutureMutex::new((
                Instant::now(),
                Err(Error::new("Account is uninitialised.")),
            ))),
            is_subscribed: IsSubscribedFn::default(),
        }
    }
}

#[derive(Debug)]
pub struct NntpType {
    pub connection: Arc<FutureMutex<NntpConnection>>,
    pub server_conf: NntpServerConf,
    pub uid_store: Arc<UIDStore>,
}

impl MailBackend for NntpType {
    fn capabilities(&self) -> MailBackendCapabilities {
        let mut metadata = None;
        let mut extensions = self
            .uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .filter_map(|c| {
                if let Some(stripped) = c.strip_prefix("IMPLEMENTATION ") {
                    metadata = Some(serde_json::json!( { "IMPLEMENTATION": stripped } ));
                    return None;
                }
                Some((
                    c.to_string(),
                    MailBackendExtensionStatus::Unsupported { comment: None },
                ))
            })
            .collect::<Vec<(String, MailBackendExtensionStatus)>>();
        let mut supports_submission = false;
        let NntpExtensionUse { deflate } = self.server_conf.extension_use;
        {
            for (name, status) in extensions.iter_mut() {
                match name.as_str() {
                    s if s.eq_ignore_ascii_case("POST") => {
                        supports_submission = true;
                        *status = MailBackendExtensionStatus::Enabled { comment: None };
                    }
                    "COMPRESS DEFLATE" => {
                        if deflate {
                            *status = MailBackendExtensionStatus::Enabled { comment: None };
                        } else {
                            *status = MailBackendExtensionStatus::Supported {
                                comment: Some("Disabled by user configuration"),
                            };
                        }
                    }
                    _ => {
                        if SUPPORTED_CAPABILITIES.contains(&name.as_str()) {
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
            supports_search: false,
            extensions: Some(extensions),
            supports_tags: false,
            supports_submission,
            extra_submission_headers: &[HeaderName::NEWSGROUPS],
            metadata,
        }
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        let mut state = FetchState {
            mailbox_hash,
            uid_store: self.uid_store.clone(),
            connection: self.connection.clone(),
            total_low_high: None,
        };
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            {
                let f = &state.uid_store.mailboxes.lock().await[&state.mailbox_hash];
                f.exists.lock().unwrap().clear();
                f.unseen.lock().unwrap().clear();
            };
            loop {
                match state.fetch_envs().await {
                    Err(err) if err.kind.is_disconnected() => continue,
                    Err(err) => return Err(err),
                    Ok(Some(ret)) => {
                        emitter.emit(ret).await;
                        continue;
                    }
                    Ok(None) => {}
                }
                break;
            }
            Ok(())
        })))
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout_dur;
        Ok(Box::pin(async move {
            // To get updates, either issue NEWNEWS if it's supported by the server, and fallback
            // to OVER otherwise
            let mbox: NntpMailbox = uid_store
                .mailboxes
                .lock()
                .await
                .get(&mailbox_hash)
                .cloned()
                .ok_or_else(|| {
                    Error::new(format!(
                        "Mailbox with hash {} not found in NNTP connection, this could possibly \
                         be a bug or it was deleted.",
                        mailbox_hash
                    ))
                })?;
            let latest_article: Option<crate::UnixTimestamp> = *mbox.latest_article.lock().unwrap();
            let (over_msgid_support, newnews_support): (bool, bool) = {
                let caps = uid_store.capabilities.lock().unwrap();

                (
                    caps.iter().any(|c| c.eq_ignore_ascii_case("OVER MSGID")),
                    caps.iter().any(|c| c.eq_ignore_ascii_case("NEWNEWS")),
                )
            };
            let mut res = String::with_capacity(8 * 1024);
            let mut conn = timeout(timeout_dur, connection.lock()).await?;
            if let Some(mut latest_article) = latest_article {
                let mut unseen = LazyCountSet::new();
                let timestamp = latest_article - 10 * 60;
                let datetime_str = crate::utils::datetime::timestamp_to_string_utc(
                    timestamp,
                    Some("%Y%m%d %H%M%S"),
                    true,
                );

                if newnews_support {
                    conn.send_command(
                        format!("NEWNEWS {} {}", &mbox.nntp_path, datetime_str).as_bytes(),
                    )
                    .await?;
                    conn.read_response(&mut res, true, &["230 "]).await?;
                    let message_ids = {
                        let message_id_lck = uid_store.message_id_index.lock().await;
                        res.split_rn()
                            .skip(1)
                            .map(|s| s.trim())
                            .filter(|msg_id| {
                                !message_id_lck.contains_key(&MessageID::new(
                                    msg_id.as_bytes(),
                                    msg_id.as_bytes(),
                                ))
                            })
                            .map(str::to_string)
                            .collect::<Vec<String>>()
                    };
                    if message_ids.is_empty() || !over_msgid_support {
                        return Ok(());
                    }
                    let mut env_hash_set: BTreeSet<EnvelopeHash> = Default::default();
                    for msg_id in message_ids {
                        conn.send_command(format!("OVER {}", msg_id).as_bytes())
                            .await?;
                        conn.read_response(&mut res, true, &["224 "]).await?;
                        let mut message_id_lck = uid_store.message_id_index.lock().await;
                        let mut uid_index_lck = uid_store.uid_index.lock().await;
                        let store_lck = uid_store.store.lock().await;
                        let mut hash_index_lck = uid_store.hash_index.lock().unwrap();
                        for l in res.split_rn().skip(1) {
                            let (_, (num, mut env)) = protocol_parser::over_article(l)?;

                            if let Some(s) = store_lck.as_ref() {
                                env.set_flags(s.flags(env.hash(), mailbox_hash, num)?);
                                if !env.is_seen() {
                                    unseen.insert_new(env.hash());
                                }
                            } else {
                                unseen.insert_new(env.hash());
                            }
                            env_hash_set.insert(env.hash());
                            message_id_lck.insert(env.message_id().clone(), env.hash());
                            hash_index_lck.insert(env.hash(), (num, mailbox_hash));
                            uid_index_lck.insert((mailbox_hash, num), env.hash());
                            latest_article = std::cmp::max(latest_article, env.timestamp);
                            (uid_store.event_consumer)(
                                uid_store.account_hash,
                                crate::backends::BackendEvent::Refresh(RefreshEvent {
                                    mailbox_hash,
                                    account_hash: uid_store.account_hash,
                                    kind: RefreshEventKind::Create(Box::new(env)),
                                }),
                            );
                        }
                    }
                    {
                        let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                        *f.latest_article.lock().unwrap() = Some(latest_article);
                        f.exists
                            .lock()
                            .unwrap()
                            .insert_existing_set(env_hash_set.clone());
                        f.unseen.lock().unwrap().insert_set(unseen.set);
                    }
                    return Ok(());
                }
            }
            Ok(())
        }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout_dur;
        Ok(Box::pin(async move {
            Self::nntp_mailboxes(&connection, timeout_dur).await?;
            let mailboxes_lck = uid_store.mailboxes.lock().await;
            let ret = mailboxes_lck
                .iter()
                .map(|(h, f)| (*h, Box::new(Clone::clone(f)) as Mailbox))
                .collect();
            Ok(ret)
        }))
    }

    fn is_online(&self) -> ResultFuture<()> {
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout_dur;
        Ok(Box::pin(async move {
            let mut conn = timeout(timeout_dur, connection.lock()).await?;
            log::trace!("is_online");
            match timeout(timeout_dur, conn.connect()).await {
                Ok(Ok(())) => Ok(()),
                Err(err) | Ok(Err(err)) => {
                    conn.stream = Err(err.clone());
                    conn.connect().await
                }
            }
        }))
    }

    fn watch(&self) -> ResultStream<BackendEvent> {
        Err(
            Error::new("Watching is currently unimplemented for nntp backend")
                .set_kind(ErrorKind::NotImplemented),
        )
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
        let op = NntpOp::new(
            uid,
            mailbox_hash,
            self.connection.clone(),
            self.uid_store.clone(),
        );

        Ok(Box::pin(async move { op.as_bytes().await }))
    }

    fn save(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support saving.").set_kind(ErrorKind::NotSupported))
    }

    fn copy_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _source_mailbox_hash: MailboxHash,
        _destination_mailbox_hash: MailboxHash,
        _move_: bool,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support copying/moving.").set_kind(ErrorKind::NotSupported))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        Ok(Box::pin(async move {
            let uids: SmallVec<[(EnvelopeHash, UID); 64]> = {
                let hash_index_lck = uid_store.hash_index.lock().unwrap();
                env_hashes
                    .iter()
                    .filter_map(|env_hash| {
                        hash_index_lck
                            .get(&env_hash)
                            .cloned()
                            .map(|(uid, _)| (env_hash, uid))
                    })
                    .collect()
            };

            if uids.is_empty() {
                return Ok(());
            }
            let fsets = &uid_store.mailboxes.lock().await[&mailbox_hash];
            let store_lck = uid_store.store.lock().await;
            if let Some(s) = store_lck.as_ref() {
                for op in flags {
                    if let FlagOp::Set(f) | FlagOp::UnSet(f) = op {
                        for (env_hash, uid) in &uids {
                            let mut current_val = s.flags(*env_hash, mailbox_hash, *uid)?;
                            current_val.set(f, <bool>::from(&op));
                            if !current_val.intersects(Flag::SEEN) {
                                fsets.unseen.lock().unwrap().insert_new(*env_hash);
                            } else {
                                fsets.unseen.lock().unwrap().remove(*env_hash);
                            }
                            s.set_flags(*env_hash, mailbox_hash, *uid, current_val)?;
                            (uid_store.event_consumer)(
                                uid_store.account_hash,
                                BackendEvent::Refresh(RefreshEvent {
                                    account_hash: uid_store.account_hash,
                                    mailbox_hash,
                                    kind: RefreshEventKind::NewFlags(
                                        *env_hash,
                                        (current_val, vec![]),
                                    ),
                                }),
                            );
                        }
                    }
                }
            }
            Ok(())
        }))
    }

    fn delete_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support deletion.").set_kind(ErrorKind::NotSupported))
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
        _path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(
            Error::new("Creating mailbox is not supported for nntp backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(
            Error::new("Deleting a mailbox is not supported for nntp backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_val: bool,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Setting mailbox subscription is not supported for nntp backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(
            Error::new("Renaming a mailbox is not supported for nntp backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Setting mailbox permissions is not supported for nntp backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<Vec<EnvelopeHash>> {
        Err(Error::new("Searching is not supported for nntp backend.")
            .set_kind(ErrorKind::NotSupported))
    }

    fn submit(
        &self,
        mut bytes: Vec<u8>,
        mailbox_hash: Option<MailboxHash>,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout_dur;
        Ok(Box::pin(async move {
            let mut conn = timeout(timeout_dur, connection.lock()).await?;
            let stream = conn.stream.as_ref()?;
            if !stream.supports_submission {
                return Err(
                    Error::new("Server prohibits posting.").set_kind(ErrorKind::NotSupported)
                );
            }
            // [ref:TODO] normalize CRLF in `bytes`
            let mut res = String::with_capacity(8 * 1024);
            if let Some((mailbox_hash, path)) = mailbox_hash
                .map_or_else(
                    || {
                        Box::pin(async { Ok(None) })
                            as BoxFuture<Result<Option<(MailboxHash, String)>>>
                    },
                    |m| {
                        let uid_store = conn.uid_store.clone();
                        Box::pin(async move {
                            Ok(Some((
                                m,
                                uid_store
                                    .mailboxes
                                    .lock()
                                    .await
                                    .get(&m)
                                    .ok_or_else(|| {
                                        Error::new("No such mailbox").set_kind(ErrorKind::NotFound)
                                    })?
                                    .name()
                                    .to_string(),
                            )))
                        })
                            as BoxFuture<Result<Option<(MailboxHash, String)>>>
                    },
                )
                .await?
            {
                if bytes
                    .find(b"\r\n\r\n")
                    .map(|pos| bytes.as_slice().split_at(pos).0)
                    .filter(|hdrs| {
                        !hdrs.starts_with(b"Newsgroups: ")
                            && !hdrs.contains_subsequence(b"\nNewsgroups: ")
                    })
                    .is_some()
                {
                    let newsgroups_hdr = format!("Newsgroups: {}\r\n", path);
                    let len = newsgroups_hdr.len();
                    bytes.extend(newsgroups_hdr.into_bytes());
                    bytes.rotate_right(len);
                }
                conn.select_group(mailbox_hash, false, &mut res).await?;
            }
            conn.send_command(b"POST").await?;
            conn.read_response(&mut res, false, &["340 "]).await?;
            conn.send_multiline_data_block(&bytes).await?;
            conn.read_response(&mut res, false, &["240 "]).await?;
            Ok(())
        }))
    }
}

impl NntpType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: IsSubscribedFn,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<Self>> {
        let server_hostname = get_conf_val!(s["server_hostname"])?;
        let server_port = get_conf_val!(s["server_port"], 119)?;
        let use_tls = get_conf_val!(s["use_tls"], server_port == 563)?;
        let use_starttls = use_tls && get_conf_val!(s["use_starttls"], false)?;
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        let require_auth = get_conf_val!(s["require_auth"], false)?;
        let store_flags_locally = get_conf_val!(s["store_flags_locally"], true)?;
        #[cfg(not(feature = "sqlite3"))]
        if store_flags_locally {
            return Err(Error::new(format!(
                "{}: store_flags_locally is on but this copy of melib isn't built with sqlite3 \
                 support.",
                &s.name
            )));
        }

        let timeout_dur = {
            let timeout = get_conf_val!(s["timeout"], 16_u64)?;
            if timeout == 0 {
                None
            } else {
                Some(Duration::from_secs(timeout))
            }
        };
        let server_conf = NntpServerConf {
            server_hostname: server_hostname.to_string(),
            server_username: if require_auth {
                get_conf_val!(s["server_username"])?.to_string()
            } else {
                get_conf_val!(s["server_username"], String::new())?
            },
            server_password: if require_auth
                || s.extra.contains_key("server_password")
                || s.extra.contains_key("server_password_command")
            {
                s.server_password()?
            } else {
                get_conf_val!(s["server_password"], String::new())?
            },
            require_auth,
            server_port,
            use_tls,
            use_starttls,
            danger_accept_invalid_certs,
            extension_use: NntpExtensionUse {
                deflate: get_conf_val!(s["use_deflate"], true)?,
            },
            timeout_dur,
        };
        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let account_name = s.name.to_string().into();
        let mut mailboxes = IndexMap::default();
        for (k, _f) in s.mailboxes.iter() {
            let mailbox_hash = MailboxHash::from_bytes(k.as_bytes());
            mailboxes.insert(
                mailbox_hash,
                NntpMailbox {
                    hash: mailbox_hash,
                    nntp_path: k.to_string(),
                    high_watermark: Arc::new(Mutex::new(0)),
                    low_watermark: Arc::new(Mutex::new(0)),
                    latest_article: Arc::new(Mutex::new(None)),
                    exists: Default::default(),
                    unseen: Default::default(),
                },
            );
        }
        if mailboxes.is_empty() {
            return Err(
                Error::new(format!("{} has no newsgroups configured.", account_name))
                    .set_kind(ErrorKind::Configuration),
            );
        }
        let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
            mailboxes: Arc::new(FutureMutex::new(mailboxes)),
            store: if store_flags_locally {
                Arc::new(FutureMutex::new(Some(store::Store::new(&s.name)?)))
            } else {
                Default::default()
            },
            is_subscribed,
            ..UIDStore::new(account_hash, account_name, event_consumer)
        });
        let connection = NntpConnection::new_connection(&server_conf, uid_store.clone());

        Ok(Box::new(Self {
            server_conf,
            connection: Arc::new(FutureMutex::new(connection)),
            uid_store,
        }))
    }

    pub async fn nntp_mailboxes(
        connection: &Arc<FutureMutex<NntpConnection>>,
        timeout_dur: Option<Duration>,
    ) -> Result<()> {
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = timeout(timeout_dur, connection.lock()).await?;
        let mut mailboxes = {
            let mailboxes_lck = conn.uid_store.mailboxes.lock().await;
            mailboxes_lck
                .values()
                .map(|m| m.name().to_string())
                .collect::<SmallVec<[String; 16]>>()
        };
        mailboxes.reverse();
        while !mailboxes.is_empty() {
            let mut command = "LIST ACTIVE ".to_string();
            'batch: while let Some(m) = mailboxes.pop() {
                // first check if the group name itself is too big for `LIST ACTIVE`.
                if "LIST ACTIVE ".len() + m.len() + "\r\n".len() >= 512 {
                    log::warn!(
                        "{}: Newsgroup named {} has a name that exceeds RFC 3977 limits of \
                         maximum command lines (512 octets) with LIST ACTIVE. Skipping it.",
                        &conn.uid_store.account_name,
                        m
                    );
                    continue 'batch;
                }
                if command.len() != "LIST ACTIVE ".len() {
                    command.push(',');
                }
                // RFC 3977
                // 3. Basic Concepts
                // 3.1.  Commands and Responses
                // Command lines MUST NOT exceed 512 octets, which includes the terminating CRLF
                // pair.
                if command.len() + m.len() + "\r\n".len() >= 512 {
                    mailboxes.push(m);
                    if command.ends_with(',') {
                        command.pop();
                    }
                    break 'batch;
                }
                command.push_str(&m);
            }
            conn.send_command(command.as_bytes()).await?;
            conn.read_response(&mut res, true, &["215 "])
                .await
                .chain_err_summary(|| {
                    format!(
                        "Could not get newsgroups {}: expected LIST ACTIVE response but got: {}",
                        &conn.uid_store.account_name, res
                    )
                })
                .chain_err_kind(ErrorKind::ProtocolError)?;
            let mut mailboxes_lck = conn.uid_store.mailboxes.lock().await;
            for l in res.split_rn().skip(1) {
                let s = l.split_whitespace().collect::<SmallVec<[&str; 4]>>();
                if s.len() != 4 {
                    continue;
                }
                let mailbox_hash = MailboxHash::from_bytes(s[0].as_bytes());
                mailboxes_lck.entry(mailbox_hash).and_modify(|m| {
                    *m.high_watermark.lock().unwrap() = usize::from_str(s[1]).unwrap_or(0);
                    *m.low_watermark.lock().unwrap() = usize::from_str(s[2]).unwrap_or(0);
                });
            }
        }
        // Ok, done collecting information about groups specified in configuration. Now
        // retrieve all other groups:
        conn.send_command(b"LIST ACTIVE").await?;
        conn.read_response(&mut res, true, &["215 "])
            .await
            .chain_err_summary(|| {
                format!(
                    "Could not get newsgroups {}: expected LIST ACTIVE response but got: {}",
                    &conn.uid_store.account_name, res
                )
            })
            .chain_err_kind(ErrorKind::ProtocolError)?;
        let mut mailboxes_lck = conn.uid_store.mailboxes.lock().await;
        for l in res.split_rn().skip(1) {
            let s = l.split_whitespace().collect::<SmallVec<[&str; 4]>>();
            if s.is_empty() || s.len() != 4 || !(conn.uid_store.is_subscribed)(s[0]) {
                continue;
            }
            let mailbox_hash = MailboxHash::from_bytes(s[0].as_bytes());
            mailboxes_lck
                .entry(mailbox_hash)
                .or_insert_with(|| NntpMailbox {
                    hash: mailbox_hash,
                    nntp_path: s[0].to_string(),
                    high_watermark: Arc::new(Mutex::new(usize::from_str(s[1]).unwrap_or(0))),
                    low_watermark: Arc::new(Mutex::new(usize::from_str(s[2]).unwrap_or(0))),
                    latest_article: Arc::new(Mutex::new(None)),
                    exists: Default::default(),
                    unseen: Default::default(),
                });
        }
        Ok(())
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut keys: HashSet<&'static str> = Default::default();
        macro_rules! get_conf_val {
            ($s:ident[$var:literal]) => {{
                keys.insert($var);
                $s.extra.swap_remove($var).ok_or_else(|| {
                    Error::new(format!(
                        "{}: NNTP connection requires the field `{}` set",
                        $s.name.as_str(),
                        $var
                    ))
                    .set_kind(ErrorKind::Configuration)
                })
            }};
            ($s:ident[$var:literal], $default:expr) => {{
                keys.insert($var);
                $s.extra
                    .swap_remove($var)
                    .map(|v| {
                        <_>::from_str(&v).map_err(|e| {
                            Error::new(format!(
                                "{}: NNTP connection has invalid configuration",
                                $s.name.as_str(),
                            ))
                            .set_kind(ErrorKind::Configuration)
                            .set_source(Some(Arc::new(
                                Error::new(format!(
                                    "Invalid value for field `{}`: {}\n{}",
                                    $var, v, e
                                ))
                                .set_kind(ErrorKind::ValueError),
                            )))
                        })
                    })
                    .unwrap_or_else(|| Ok($default))
            }};
        }
        #[cfg(feature = "sqlite3")]
        get_conf_val!(s["store_flags_locally"], true)?;
        #[cfg(not(feature = "sqlite3"))]
        if get_conf_val!(s["store_flags_locally"], false)? {
            return Err(Error::new(format!(
                "{}: store_flags_locally is on but this copy of melib isn't built with sqlite3 \
                 support.",
                &s.name
            ))
            .set_kind(ErrorKind::Configuration));
        }
        get_conf_val!(s["require_auth"], false)?;
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"], String::new())?;
        if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"], String::new())?;
        } else if s.extra.contains_key("server_password") {
            return Err(Error::new(format!(
                "{}: both server_password and server_password_command are set, cannot choose",
                s.name.as_str(),
            ))
            .set_kind(ErrorKind::Configuration));
        }
        let _ = get_conf_val!(s["server_password_command"]);
        let server_port = get_conf_val!(s["server_port"], 119)?;
        let use_tls = get_conf_val!(s["use_tls"], server_port == 563)?;
        let use_starttls = get_conf_val!(s["use_starttls"], server_port != 563)?;
        if !use_tls && use_starttls {
            return Err(Error::new(format!(
                "{}: incompatible use_tls and use_starttls values: use_tls = false, use_starttls \
                 = true",
                s.name.as_str(),
            ))
            .set_kind(ErrorKind::Configuration));
        }
        get_conf_val!(s["use_deflate"], false)?;
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        get_conf_val!(s["timeout"], 16_u64)?;
        let extra_keys = s
            .extra
            .keys()
            .map(String::as_str)
            .collect::<HashSet<&str>>();
        let diff = extra_keys.difference(&keys).collect::<Vec<&&str>>();
        if !diff.is_empty() {
            return Err(Error::new(format!(
                "{} the following flags are set but are not recognized: {:?}.",
                s.name.as_str(),
                diff
            ))
            .set_kind(ErrorKind::Configuration));
        }
        Ok(())
    }

    pub fn capabilities(&self) -> Vec<String> {
        self.uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .cloned()
            .collect::<Vec<String>>()
    }

    pub fn article_message_id(
        &self,
        mailbox_hash: MailboxHash,
        msg_id: MessageID,
    ) -> ResultFuture<Mail> {
        let connection = self.connection.clone();
        let timeout_dur = self.server_conf.timeout_dur;
        Ok(Box::pin(async move {
            let mut conn = timeout(timeout_dur, connection.lock()).await?;
            timeout(timeout_dur, conn.connect()).await??;
            let mut res = String::with_capacity(8 * 1024);
            conn.select_group(mailbox_hash, false, &mut res).await?;
            conn.send_command(format!("ARTICLE {}", msg_id.display_brackets()).as_bytes())
                .await?;
            let reply_code = conn
                .read_response(&mut res, true, command_to_replycodes("ARTICLE"))
                .await
                .chain_err_summary(|| {
                    format!(
                        "{} Could not select newsgroup: expected ARTICLE response but got: {}",
                        &conn.uid_store.account_name, res
                    )
                })
                .chain_err_kind(ErrorKind::ProtocolError)?;
            if reply_code == 430 {
                return Err(
                    Error::new("No article with that message-id").set_kind(ErrorKind::NotFound)
                );
            }
            let pos = res.find("\r\n").unwrap_or(0) + 2;

            Mail::new(res.as_bytes()[pos..].to_vec(), None)
        }))
    }
}

struct FetchState {
    mailbox_hash: MailboxHash,
    connection: Arc<FutureMutex<NntpConnection>>,
    uid_store: Arc<UIDStore>,
    total_low_high: Option<(usize, usize, usize)>,
}

impl FetchState {
    async fn fetch_envs(&mut self) -> Result<Option<Vec<Envelope>>> {
        let Self {
            mailbox_hash,
            ref connection,
            ref uid_store,
            ref mut total_low_high,
        } = self;
        let mailbox_hash = *mailbox_hash;
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = connection.lock().await;
        let mut unseen = LazyCountSet::new();

        if total_low_high.is_none() {
            conn.select_group(mailbox_hash, true, &mut res).await?;
            // Parameters
            //     group     Name of newsgroup
            //     number    Estimated number of articles in the group
            //     low       Reported low water mark
            //     high      Reported high water mark
            let s = res.split_whitespace().collect::<SmallVec<[&str; 6]>>();
            let path = conn.uid_store.mailboxes.lock().await[&mailbox_hash]
                .name()
                .to_string();
            if s.len() != 5 {
                let err = Error::new(format!(
                    "{} Could not select newsgroup {}: expected GROUP response but got: {}",
                    &uid_store.account_name, path, res
                ))
                .set_kind(ErrorKind::ProtocolError);
                conn.stream = Err(err.clone());
                conn.connect().await?;
                // If .connect() didn't error out, this means our error was a connection
                // failure.
                return Err(err.set_kind(ErrorKind::Network(NetworkErrorKind::ConnectionFailed)));
            }
            let total = usize::from_str(s[1]).unwrap_or(0);
            let low = usize::from_str(s[2]).unwrap_or(0);
            let high = usize::from_str(s[3]).unwrap_or(0);
            *total_low_high = Some((total, low, high));
            {
                let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                f.exists.lock().unwrap().set_not_yet_seen(total);
            };
        }

        let (low, new_low) = loop {
            let (_, low, high) = total_low_high.unwrap();
            if high <= low {
                return Ok(None);
            }
            const CHUNK_SIZE: usize = 50000;
            let new_low = std::cmp::max(low, std::cmp::min(high, low.saturating_add(CHUNK_SIZE)));
            total_low_high.as_mut().unwrap().1 = new_low;

            // [ref:FIXME]: server might not implement OVER capability
            conn.send_command(format!("OVER {}-{}", low, new_low).as_bytes())
                .await?;
            let reply_code = conn
                .read_response(&mut res, true, command_to_replycodes("OVER"))
                .await
                .chain_err_summary(|| {
                    format!(
                        "{} Could not select newsgroup: expected OVER response but got: {}",
                        &uid_store.account_name, res
                    )
                })
                .chain_err_kind(ErrorKind::ProtocolError)?;
            if reply_code == 423 {
                // No articles in this range, so move on to next chunk.
                continue;
            }
            break (low, new_low);
        };

        let mut ret = Vec::with_capacity(new_low - low);
        let mut latest_article: Option<crate::UnixTimestamp> = None;

        {
            let mut message_id_lck = uid_store.message_id_index.lock().await;
            let mut uid_index_lck = uid_store.uid_index.lock().await;
            let store_lck = uid_store.store.lock().await;
            let mut hash_index_lck = uid_store.hash_index.lock().unwrap();
            for l in res.split_rn().skip(1) {
                let (_, (num, mut env)) = protocol_parser::over_article(l)?;
                if let Some(s) = store_lck.as_ref() {
                    env.set_flags(s.flags(env.hash(), mailbox_hash, num)?);
                    if !env.is_seen() {
                        unseen.insert_new(env.hash());
                    }
                } else {
                    unseen.insert_new(env.hash());
                }
                message_id_lck.insert(env.message_id().clone(), env.hash());
                hash_index_lck.insert(env.hash(), (num, mailbox_hash));
                uid_index_lck.insert((mailbox_hash, num), env.hash());
                if let Some(ref mut v) = latest_article {
                    *v = std::cmp::max(*v, env.timestamp);
                } else {
                    latest_article = Some(env.timestamp);
                }
                ret.push(env);
            }
        }
        {
            let hash_set: BTreeSet<EnvelopeHash> = ret.iter().map(|env| env.hash()).collect();
            let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
            *f.latest_article.lock().unwrap() = latest_article;
            f.exists
                .lock()
                .unwrap()
                .insert_existing_set(hash_set.clone());
            *f.unseen.lock().unwrap() = unseen;
        };
        Ok(Some(ret))
    }
}
