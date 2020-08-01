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

use crate::get_conf_val;
use crate::get_path_hash;
use smallvec::SmallVec;
#[macro_use]
mod protocol_parser;
pub use protocol_parser::*;
mod mailbox;
pub use mailbox::*;
mod operations;
pub use operations::*;
mod connection;
pub use connection::*;

use crate::async_workers::{Async, WorkContext};
use crate::backends::*;
use crate::conf::AccountSettings;
use crate::email::*;
use crate::error::{MeliError, Result, ResultIntoMeliError};
use futures::lock::Mutex as FutureMutex;
use futures::stream::Stream;
use std::collections::{hash_map::DefaultHasher, BTreeMap};
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::hash::Hasher;
use std::pin::Pin;
use std::str::FromStr;
use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;
pub type UID = usize;

pub static SUPPORTED_CAPABILITIES: &[&str] = &[
    #[cfg(feature = "deflate_compression")]
    "COMPRESS DEFLATE",
    "VERSION 2",
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

pub struct IsSubscribedFn(Box<dyn Fn(&str) -> bool + Send + Sync>);

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
type Capabilities = HashSet<String>;

#[derive(Debug)]
pub struct UIDStore {
    account_hash: AccountHash,
    account_name: Arc<String>,
    offline_cache: bool,
    capabilities: Arc<Mutex<Capabilities>>,
    hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    uid_index: Arc<Mutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,

    mailboxes: Arc<FutureMutex<HashMap<MailboxHash, NntpMailbox>>>,
    is_online: Arc<Mutex<(Instant, Result<()>)>>,
    refresh_events: Arc<Mutex<Vec<RefreshEvent>>>,
    sender: Arc<RwLock<Option<RefreshEventConsumer>>>,
}

impl Default for UIDStore {
    fn default() -> Self {
        UIDStore {
            account_hash: 0,
            account_name: Arc::new(String::new()),
            offline_cache: false,
            capabilities: Default::default(),
            hash_index: Default::default(),
            uid_index: Default::default(),
            mailboxes: Arc::new(FutureMutex::new(Default::default())),
            is_online: Arc::new(Mutex::new((
                Instant::now(),
                Err(MeliError::new("Account is uninitialised.")),
            ))),
            refresh_events: Default::default(),
            sender: Arc::new(RwLock::new(None)),
        }
    }
}

#[derive(Debug)]
pub struct NntpType {
    is_subscribed: Arc<IsSubscribedFn>,
    connection: Arc<FutureMutex<NntpConnection>>,
    server_conf: NntpServerConf,
    uid_store: Arc<UIDStore>,
    can_create_flags: Arc<Mutex<bool>>,
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
        let NntpExtensionUse {
            #[cfg(feature = "deflate_compression")]
            deflate,
        } = self.server_conf.extension_use;
        {
            for (name, status) in extensions.iter_mut() {
                match name.as_str() {
                    "COMPRESS DEFLATE" => {
                        if cfg!(feature = "deflate_compression") {
                            if deflate {
                                *status = MailBackendExtensionStatus::Enabled { comment: None };
                            } else {
                                *status = MailBackendExtensionStatus::Supported {
                                    comment: Some("Disabled by user configuration"),
                                };
                            }
                        } else {
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
        }
    }

    fn fetch_async(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let uid_store = self.uid_store.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async_stream::try_stream! {
            {
                let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                f.exists.lock().unwrap().clear();
                f.unseen.lock().unwrap().clear();
            };
            let ret = fetch_envs(mailbox_hash, connection, &uid_store).await?;
            yield ret;
        }))
    }

    fn refresh_async(
        &mut self,
        _mailbox_hash: MailboxHash,
        _sender: RefreshEventConsumer,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn mailboxes_async(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
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

    fn is_online_async(&self) -> ResultFuture<()> {
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            match timeout(std::time::Duration::from_secs(3), connection.lock()).await {
                Ok(mut conn) => {
                    debug!("is_online_async");
                    match debug!(timeout(std::time::Duration::from_secs(3), conn.connect()).await) {
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

    fn fetch(&mut self, _mailbox_hash: MailboxHash) -> Result<Async<Result<Vec<Envelope>>>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn refresh(
        &mut self,
        _mailbox_hash: MailboxHash,
        _sender: RefreshEventConsumer,
    ) -> Result<Async<()>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn watch(
        &self,
        _sender: RefreshEventConsumer,
        _work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        Err(MeliError::new("Unimplemented."))
    }

    fn watch_async(&self, _sender: RefreshEventConsumer) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn operation(&self, env_hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        let (uid, mailbox_hash) = if let Some(v) =
            self.uid_store.hash_index.lock().unwrap().get(&env_hash)
        {
            *v
        } else {
            return Err(MeliError::new(
                    "Message not found in local cache, it might have been deleted before you requested it."
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
        Err(MeliError::new("Unimplemented."))
    }

    fn copy_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _source_mailbox_hash: MailboxHash,
        _destination_mailbox_hash: MailboxHash,
        _move_: bool,
        _destination_flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_flags(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
        _flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn as_any(&self) -> &dyn ::std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn ::std::any::Any {
        self
    }

    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        None
    }

    fn create_mailbox(
        &mut self,
        _path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(MeliError::new("Unimplemented."))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_val: bool,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        Err(MeliError::new("Unimplemented."))
    }
}

impl NntpType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
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
                return Err(MeliError::new(format!(
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
        let use_starttls = use_tls && get_conf_val!(s["use_starttls"], !(server_port == 563))?;
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        let require_auth = get_conf_val!(s["require_auth"], true)?;
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
                deflate: get_conf_val!(s["use_deflate"], true)?,
            },
        };
        let account_hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(s.name.as_bytes());
            hasher.finish()
        };
        let account_name = Arc::new(s.name().to_string());
        let mut mailboxes = HashMap::default();
        for (k, _f) in s.mailboxes.iter() {
            let mailbox_hash = get_path_hash!(&k);
            mailboxes.insert(
                mailbox_hash,
                NntpMailbox {
                    hash: mailbox_hash,
                    nntp_path: k.to_string(),
                    high_watermark: Arc::new(Mutex::new(0)),
                    low_watermark: Arc::new(Mutex::new(0)),
                    exists: Default::default(),
                    unseen: Default::default(),
                },
            );
        }
        if mailboxes.is_empty() {
            return Err(MeliError::new(format!(
                "{} has no newsgroups configured.",
                account_name
            )));
        }
        let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
            account_hash,
            account_name,
            offline_cache: false, //get_conf_val!(s["X_header_caching"], false)?,
            mailboxes: Arc::new(FutureMutex::new(mailboxes)),
            ..UIDStore::default()
        });
        let connection = NntpConnection::new_connection(&server_conf, uid_store.clone());

        Ok(Box::new(NntpType {
            server_conf,
            is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            can_create_flags: Arc::new(Mutex::new(false)),
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
            let mailbox_hash = get_path_hash!(&s[0]);
            mailboxes_lck.entry(mailbox_hash).and_modify(|m| {
                *m.high_watermark.lock().unwrap() = usize::from_str(s[1]).unwrap_or(0);
                *m.low_watermark.lock().unwrap() = usize::from_str(s[2]).unwrap_or(0);
            });
        }
        Ok(())
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"], String::new())?;
        if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"], String::new())?;
        } else if s.extra.contains_key("server_password") {
            return Err(MeliError::new(format!(
                "Configuration error ({}): both server_password and server_password_command are set, cannot choose",
                s.name.as_str(),
            )));
        }
        let server_port = get_conf_val!(s["server_port"], 119)?;
        let use_tls = get_conf_val!(s["use_tls"], server_port == 563)?;
        let use_starttls = get_conf_val!(s["use_starttls"], !(server_port == 563))?;
        if !use_tls && use_starttls {
            return Err(MeliError::new(format!(
                "Configuration error ({}): incompatible use_tls and use_starttls values: use_tls = false, use_starttls = true",
                s.name.as_str(),
            )));
        }
        #[cfg(feature = "deflate_compression")]
        get_conf_val!(s["use_deflate"], true)?;
        #[cfg(not(feature = "deflate_compression"))]
        if s.extra.contains_key("use_deflate") {
            return Err(MeliError::new(format!(
                "Configuration error ({}): setting `use_deflate` is set but this version of meli isn't compiled with DEFLATE support.",
                s.name.as_str(),
            )));
        }
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        Ok(())
    }

    pub fn capabilities(&self) -> Vec<String> {
        self.uid_store
            .capabilities
            .lock()
            .unwrap()
            .iter()
            .map(|c| c.clone())
            .collect::<Vec<String>>()
    }
}

async fn fetch_envs(
    mailbox_hash: MailboxHash,
    connection: Arc<FutureMutex<NntpConnection>>,
    uid_store: &UIDStore,
) -> Result<Vec<Envelope>> {
    let mut res = String::with_capacity(8 * 1024);
    let mut conn = connection.lock().await;
    let path = uid_store.mailboxes.lock().await[&mailbox_hash]
        .name()
        .to_string();
    conn.send_command(format!("GROUP {}", path).as_bytes())
        .await?;
    conn.read_response(&mut res, false, &["211 "])
        .await
        .chain_err_summary(|| {
            format!(
                "{} Could not select newsgroup {}: expected GROUP response but got: {}",
                &uid_store.account_name, path, res
            )
        })?;
    /*
     *   Parameters
             group     Name of newsgroup
             number    Estimated number of articles in the group
             low       Reported low water mark
             high      Reported high water mark
    */
    let s = res.split_whitespace().collect::<SmallVec<[&str; 6]>>();
    if s.len() != 5 {
        return Err(MeliError::new(format!(
            "{} Could not select newsgroup {}: expected GROUP response but got: {}",
            &uid_store.account_name, path, res
        )));
    }
    let total = usize::from_str(&s[1]).unwrap_or(0);
    let _low = usize::from_str(&s[2]).unwrap_or(0);
    let high = usize::from_str(&s[3]).unwrap_or(0);
    drop(s);

    conn.send_command(format!("OVER {}-{}", high.saturating_sub(100), high).as_bytes())
        .await?;
    conn.read_response(&mut res, true, &["224 "])
        .await
        .chain_err_summary(|| {
            format!(
                "{} Could not select newsgroup {}: expected OVER response but got: {}",
                &uid_store.account_name, path, res
            )
        })?;
    let mut ret = Vec::with_capacity(total);
    //hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    //uid_index: Arc<Mutex<HashMap<(MailboxHash, UID), EnvelopeHash>>>,
    let mut hash_index_lck = uid_store.hash_index.lock().unwrap();
    let mut uid_index_lck = uid_store.uid_index.lock().unwrap();
    for l in res.split_rn().skip(1) {
        let (_, (num, env)) = debug!(protocol_parser::over_article(&l))?;
        hash_index_lck.insert(env.hash(), (num, mailbox_hash));
        uid_index_lck.insert((mailbox_hash, num), env.hash());
        ret.push(env);
    }
    Ok(ret)
}

use futures::future::{self, Either};

async fn timeout<O>(dur: std::time::Duration, f: impl Future<Output = O>) -> Result<O> {
    futures::pin_mut!(f);
    match future::select(f, smol::Timer::after(dur)).await {
        Either::Left((out, _)) => Ok(out),
        Either::Right(_) => {
            Err(MeliError::new("Timedout").set_kind(crate::error::ErrorKind::Network))
        }
    }
}
