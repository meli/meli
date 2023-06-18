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

use smallvec::SmallVec;

use crate::{get_conf_val, get_path_hash};
#[macro_use]
mod protocol_parser;
pub use protocol_parser::*;
mod mailbox;
pub use mailbox::*;
mod operations;
pub use operations::*;
mod connection;
use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet},
    hash::Hasher,
    pin::Pin,
    str::FromStr,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

pub use connection::*;
use futures::{lock::Mutex as FutureMutex, stream::Stream};

use crate::{
    backends::*,
    conf::AccountSettings,
    email::*,
    error::{Error, Result, ResultIntoError},
    utils::futures::timeout,
    Collection,
};
pub type UID = usize;

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            Error::new(format!(
                "Configuration error ({}): NNTP connection requires the field `{}` set",
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
                        "Configuration error ({}) NNTP: Invalid value for field `{}`: {}\n{}",
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

pub static SUPPORTED_CAPABILITIES: &[&str] = &[
    #[cfg(feature = "deflate_compression")]
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

#[derive(Debug, Clone)]
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
}

type Capabilities = HashSet<String>;

#[derive(Debug)]
pub struct UIDStore {
    account_hash: AccountHash,
    account_name: Arc<String>,
    capabilities: Arc<Mutex<Capabilities>>,
    message_id_index: Arc<Mutex<HashMap<String, EnvelopeHash>>>,
    hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    uid_index: Arc<Mutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,

    collection: Collection,
    mailboxes: Arc<FutureMutex<HashMap<MailboxHash, NntpMailbox>>>,
    is_online: Arc<Mutex<(Instant, Result<()>)>>,
    event_consumer: BackendEventConsumer,
}

impl UIDStore {
    fn new(
        account_hash: AccountHash,
        account_name: Arc<String>,
        event_consumer: BackendEventConsumer,
    ) -> Self {
        UIDStore {
            account_hash,
            account_name,
            event_consumer,
            capabilities: Default::default(),
            message_id_index: Default::default(),
            hash_index: Default::default(),
            uid_index: Default::default(),
            mailboxes: Arc::new(FutureMutex::new(Default::default())),
            collection: Collection::new(),
            is_online: Arc::new(Mutex::new((
                Instant::now(),
                Err(Error::new("Account is uninitialised.")),
            ))),
        }
    }
}

#[derive(Debug)]
pub struct NntpType {
    _is_subscribed: Arc<IsSubscribedFn>,
    connection: Arc<FutureMutex<NntpConnection>>,
    server_conf: NntpServerConf,
    uid_store: Arc<UIDStore>,
    _can_create_flags: Arc<Mutex<bool>>,
}

impl MailBackend for NntpType {
    fn capabilities(&self) -> MailBackendCapabilities {
        let mut extensions = self
            .uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .map(|c| {
                (
                    c.to_string(),
                    MailBackendExtensionStatus::Unsupported { comment: None },
                )
            })
            .collect::<Vec<(String, MailBackendExtensionStatus)>>();
        let mut supports_submission = false;
        let NntpExtensionUse {
            #[cfg(feature = "deflate_compression")]
            deflate,
        } = self.server_conf.extension_use;
        {
            for (name, status) in extensions.iter_mut() {
                match name.as_str() {
                    s if s.eq_ignore_ascii_case("POST") => {
                        supports_submission = true;
                        *status = MailBackendExtensionStatus::Enabled { comment: None };
                    }
                    "COMPRESS DEFLATE" => {
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
        }
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let mut state = FetchState {
            mailbox_hash,
            uid_store: self.uid_store.clone(),
            connection: self.connection.clone(),
            high_low_total: None,
        };
        Ok(Box::pin(async_stream::try_stream! {
            {
                let f = &state.uid_store.mailboxes.lock().await[&state.mailbox_hash];
                f.exists.lock().unwrap().clear();
                f.unseen.lock().unwrap().clear();
            };
            loop {
                if let Some(ret) = state.fetch_envs().await? {
                    yield ret;
                    continue;
                }
                break;
            }
        }))
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            /* To get updates, either issue NEWNEWS if it's supported by the server, and
             * fallback to OVER otherwise */
            let mbox: NntpMailbox = uid_store
                .mailboxes
                .lock()
                .await
                .get(&mailbox_hash)
                .map(std::clone::Clone::clone)
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
            let mut conn = timeout(Some(Duration::from_secs(60 * 16)), connection.lock()).await?;
            if let Some(mut latest_article) = latest_article {
                let timestamp = latest_article - 10 * 60;
                let datetime_str = crate::utils::datetime::timestamp_to_string(
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
                        let message_id_lck = uid_store.message_id_index.lock().unwrap();
                        res.split_rn()
                            .skip(1)
                            .map(|s| s.trim())
                            .filter(|msg_id| !message_id_lck.contains_key(*msg_id))
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
                        let mut message_id_lck = uid_store.message_id_index.lock().unwrap();
                        let mut hash_index_lck = uid_store.hash_index.lock().unwrap();
                        let mut uid_index_lck = uid_store.uid_index.lock().unwrap();
                        for l in res.split_rn().skip(1) {
                            let (_, (num, env)) = protocol_parser::over_article(l)?;
                            env_hash_set.insert(env.hash());
                            message_id_lck.insert(env.message_id_display().to_string(), env.hash());
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
                        f.unseen.lock().unwrap().insert_existing_set(env_hash_set);
                    }
                    return Ok(());
                }
            }
            //conn.select_group(mailbox_hash, false, &mut res).await?;
            Ok(())
        }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            NntpType::nntp_mailboxes(&connection).await?;
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
        Ok(Box::pin(async move {
            match timeout(Some(Duration::from_secs(60 * 16)), connection.lock()).await {
                Ok(mut conn) => {
                    debug!("is_online");
                    match debug!(timeout(Some(Duration::from_secs(60 * 16)), conn.connect()).await)
                    {
                        Ok(Ok(())) => Ok(()),
                        Err(err) | Ok(Err(err)) => {
                            conn.stream = Err(err.clone());
                            debug!(conn.connect().await)
                        }
                    }
                }
                Err(err) => Err(err),
            }
        }))
    }

    fn watch(&self) -> ResultFuture<()> {
        Err(
            Error::new("Watching is currently uniplemented for nntp backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn operation(&self, env_hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        let (uid, mailbox_hash) =
            if let Some(v) = self.uid_store.hash_index.lock().unwrap().get(&env_hash) {
                *v
            } else {
                return Err(Error::new(
                    "Message not found in local cache, it might have been deleted before you \
                     requested it.",
                ));
            };
        Ok(Box::new(NntpOp::new(
            uid,
            mailbox_hash,
            self.connection.clone(),
            self.uid_store.clone(),
        )))
    }

    fn save(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support saving."))
    }

    fn copy_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _source_mailbox_hash: MailboxHash,
        _destination_mailbox_hash: MailboxHash,
        _move_: bool,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support copying/moving."))
    }

    fn set_flags(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
        _flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support flags."))
    }

    fn delete_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        Err(Error::new("NNTP doesn't support deletion."))
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
        _path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(Error::new(
            "Creating mailbox is currently unimplemented for nntp backend.",
        ))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(Error::new(
            "Deleting a mailbox is currently unimplemented for nntp backend.",
        ))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_val: bool,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox description is currently unimplemented for nntp backend.",
        ))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(Error::new(
            "Renaming mailbox is currently unimplemented for nntp backend.",
        ))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox permissions is currently unimplemented for nntp backend.",
        ))
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        Err(Error::new(
            "Searching is currently unimplemented for nntp backend.",
        ))
    }

    fn submit(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: Option<MailboxHash>,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            match timeout(Some(Duration::from_secs(60 * 16)), connection.lock()).await {
                Ok(mut conn) => {
                    match &conn.stream {
                        Ok(stream) => {
                            if !stream.supports_submission {
                                return Err(Error::new("Server prohibits posting."));
                            }
                        }
                        Err(err) => return Err(err.clone()),
                    }
                    let mut res = String::with_capacity(8 * 1024);
                    if let Some(mailbox_hash) = mailbox_hash {
                        conn.select_group(mailbox_hash, false, &mut res).await?;
                    }
                    conn.send_command(b"POST").await?;
                    conn.read_response(&mut res, false, &["340 "]).await?;
                    conn.send_multiline_data_block(&bytes).await?;
                    conn.read_response(&mut res, false, &["240 "]).await?;
                    Ok(())
                }
                Err(err) => Err(err),
            }
        }))
    }
}

impl NntpType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        let server_hostname = get_conf_val!(s["server_hostname"])?;
        /*let server_username = get_conf_val!(s["server_username"], "")?;
        let server_password = if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"], "")?.to_string()
        } else {
            let invocation = get_conf_val!(s["server_password_command"])?;
            let output = std::process::Command::new("sh")
                .args(&["-c", invocation])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .output()?;
            if !output.status.success() {
                return Err(Error::new(format!(
                    "({}) server_password_command `{}` returned {}: {}",
                    s.name,
                    get_conf_val!(s["server_password_command"])?,
                    output.status,
                    String::from_utf8_lossy(&output.stderr)
                )));
            }
            std::str::from_utf8(&output.stdout)?.trim_end().to_string()
        };
        */
        let server_port = get_conf_val!(s["server_port"], 119)?;
        let use_tls = get_conf_val!(s["use_tls"], server_port == 563)?;
        let use_starttls = use_tls && get_conf_val!(s["use_starttls"], false)?;
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        let require_auth = get_conf_val!(s["require_auth"], false)?;
        let server_conf = NntpServerConf {
            server_hostname: server_hostname.to_string(),
            server_username: if require_auth {
                get_conf_val!(s["server_username"])?.to_string()
            } else {
                get_conf_val!(s["server_username"], String::new())?
            },
            server_password: if require_auth {
                get_conf_val!(s["server_password"])?.to_string()
            } else {
                get_conf_val!(s["server_password"], String::new())?
            },
            require_auth,
            server_port,
            use_tls,
            use_starttls,
            danger_accept_invalid_certs,
            extension_use: NntpExtensionUse {
                #[cfg(feature = "deflate_compression")]
                deflate: get_conf_val!(s["use_deflate"], false)?,
            },
        };
        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let account_name = Arc::new(s.name.to_string());
        let mut mailboxes = HashMap::default();
        for (k, _f) in s.mailboxes.iter() {
            let mailbox_hash = MailboxHash(get_path_hash!(&k));
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
            return Err(Error::new(format!(
                "{} has no newsgroups configured.",
                account_name
            )));
        }
        let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
            mailboxes: Arc::new(FutureMutex::new(mailboxes)),
            ..UIDStore::new(account_hash, account_name, event_consumer)
        });
        let connection = NntpConnection::new_connection(&server_conf, uid_store.clone());

        Ok(Box::new(NntpType {
            server_conf,
            _is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            _can_create_flags: Arc::new(Mutex::new(false)),
            connection: Arc::new(FutureMutex::new(connection)),
            uid_store,
        }))
    }

    pub async fn nntp_mailboxes(connection: &Arc<FutureMutex<NntpConnection>>) -> Result<()> {
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = connection.lock().await;
        let command = {
            let mailboxes_lck = conn.uid_store.mailboxes.lock().await;
            mailboxes_lck
                .values()
                .fold("LIST ACTIVE ".to_string(), |mut acc, x| {
                    if acc.len() != "LIST ACTIVE ".len() {
                        acc.push(',');
                    }
                    acc.push_str(x.name());
                    acc
                })
        };
        conn.send_command(command.as_bytes()).await?;
        conn.read_response(&mut res, true, &["215 "])
            .await
            .chain_err_summary(|| {
                format!(
                    "Could not get newsgroups {}: expected LIST ACTIVE response but got: {}",
                    &conn.uid_store.account_name, res
                )
            })?;
        debug!(&res);
        let mut mailboxes_lck = conn.uid_store.mailboxes.lock().await;
        for l in res.split_rn().skip(1) {
            let s = l.split_whitespace().collect::<SmallVec<[&str; 4]>>();
            if s.len() != 3 {
                continue;
            }
            let mailbox_hash = MailboxHash(get_path_hash!(&s[0]));
            mailboxes_lck.entry(mailbox_hash).and_modify(|m| {
                *m.high_watermark.lock().unwrap() = usize::from_str(s[1]).unwrap_or(0);
                *m.low_watermark.lock().unwrap() = usize::from_str(s[2]).unwrap_or(0);
            });
        }
        Ok(())
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut keys: HashSet<&'static str> = Default::default();
        macro_rules! get_conf_val {
            ($s:ident[$var:literal]) => {{
                keys.insert($var);
                $s.extra.remove($var).ok_or_else(|| {
                    Error::new(format!(
                        "Configuration error ({}): NNTP connection requires the field `{}` set",
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
                                "Configuration error ({}) NNTP: Invalid value for field `{}`: \
                                 {}\n{}",
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
        get_conf_val!(s["require_auth"], false)?;
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"], String::new())?;
        if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"], String::new())?;
        } else if s.extra.contains_key("server_password") {
            return Err(Error::new(format!(
                "Configuration error ({}): both server_password and server_password_command are \
                 set, cannot choose",
                s.name.as_str(),
            )));
        }
        let _ = get_conf_val!(s["server_password_command"]);
        let server_port = get_conf_val!(s["server_port"], 119)?;
        let use_tls = get_conf_val!(s["use_tls"], server_port == 563)?;
        let use_starttls = get_conf_val!(s["use_starttls"], server_port != 563)?;
        if !use_tls && use_starttls {
            return Err(Error::new(format!(
                "Configuration error ({}): incompatible use_tls and use_starttls values: use_tls \
                 = false, use_starttls = true",
                s.name.as_str(),
            )));
        }
        #[cfg(feature = "deflate_compression")]
        get_conf_val!(s["use_deflate"], false)?;
        #[cfg(not(feature = "deflate_compression"))]
        if s.extra.contains_key("use_deflate") {
            return Err(Error::new(format!(
                "Configuration error ({}): setting `use_deflate` is set but this version of meli \
                 isn't compiled with DEFLATE support.",
                s.name.as_str(),
            )));
        }
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        let extra_keys = s
            .extra
            .keys()
            .map(String::as_str)
            .collect::<HashSet<&str>>();
        let diff = extra_keys.difference(&keys).collect::<Vec<&&str>>();
        if !diff.is_empty() {
            return Err(Error::new(format!(
                "Configuration error ({}) NNTP: the following flags are set but are not \
                 recognized: {:?}.",
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
            .cloned()
            .collect::<Vec<String>>()
    }
}

struct FetchState {
    mailbox_hash: MailboxHash,
    connection: Arc<FutureMutex<NntpConnection>>,
    uid_store: Arc<UIDStore>,
    high_low_total: Option<(usize, usize, usize)>,
}

impl FetchState {
    async fn fetch_envs(&mut self) -> Result<Option<Vec<Envelope>>> {
        let FetchState {
            mailbox_hash,
            ref connection,
            ref uid_store,
            ref mut high_low_total,
        } = self;
        let mailbox_hash = *mailbox_hash;
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = connection.lock().await;
        if high_low_total.is_none() {
            conn.select_group(mailbox_hash, true, &mut res).await?;
            /*
             *   Parameters
                     group     Name of newsgroup
                     number    Estimated number of articles in the group
                     low       Reported low water mark
                     high      Reported high water mark
            */
            let s = res.split_whitespace().collect::<SmallVec<[&str; 6]>>();
            let path = conn.uid_store.mailboxes.lock().await[&mailbox_hash]
                .name()
                .to_string();
            if s.len() != 5 {
                return Err(Error::new(format!(
                    "{} Could not select newsgroup {}: expected GROUP response but got: {}",
                    &uid_store.account_name, path, res
                )));
            }
            let total = usize::from_str(s[1]).unwrap_or(0);
            let _low = usize::from_str(s[2]).unwrap_or(0);
            let high = usize::from_str(s[3]).unwrap_or(0);
            *high_low_total = Some((high, _low, total));
            {
                let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                f.exists.lock().unwrap().set_not_yet_seen(total);
                f.unseen.lock().unwrap().set_not_yet_seen(total);
            };
        }
        let (high, low, _) = high_low_total.unwrap();
        if high <= low {
            return Ok(None);
        }
        const CHUNK_SIZE: usize = 50000;
        let new_low = std::cmp::max(low, high.saturating_sub(CHUNK_SIZE));
        high_low_total.as_mut().unwrap().0 = new_low;

        // FIXME: server might not implement OVER capability
        conn.send_command(format!("OVER {}-{}", new_low, high).as_bytes())
            .await?;
        conn.read_response(&mut res, true, command_to_replycodes("OVER"))
            .await
            .chain_err_summary(|| {
                format!(
                    "{} Could not select newsgroup: expected OVER response but got: {}",
                    &uid_store.account_name, res
                )
            })?;
        let mut ret = Vec::with_capacity(high - new_low);
        //hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
        //uid_index: Arc<Mutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,
        let mut latest_article: Option<crate::UnixTimestamp> = None;
        {
            let mut message_id_lck = uid_store.message_id_index.lock().unwrap();
            let mut hash_index_lck = uid_store.hash_index.lock().unwrap();
            let mut uid_index_lck = uid_store.uid_index.lock().unwrap();
            for l in res.split_rn().skip(1) {
                let (_, (num, env)) = protocol_parser::over_article(l)?;
                message_id_lck.insert(env.message_id_display().to_string(), env.hash());
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
            f.unseen.lock().unwrap().insert_existing_set(hash_set);
        };
        Ok(Some(ret))
    }
}
