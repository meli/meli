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

use crate::get_path_hash;
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
mod cache;
pub mod managesieve;
mod untagged;

use crate::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use crate::backends::BackendOp;
use crate::backends::RefreshEvent;
use crate::backends::RefreshEventKind::{self, *};
use crate::backends::{AccountHash, MailboxHash};
use crate::backends::{BackendMailbox, MailBackend, Mailbox, RefreshEventConsumer};
use crate::conf::AccountSettings;
use crate::email::*;
use crate::error::{MeliError, Result};
use std::collections::{hash_map::DefaultHasher, BTreeMap};
use std::collections::{HashMap, HashSet};
use std::hash::Hasher;
use std::str::FromStr;
use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;
pub type UID = usize;

pub static SUPPORTED_CAPABILITIES: &'static [&'static str] =
    &["IDLE", "LOGIN", "LOGINDISABLED", "ENABLE", "IMAP4REV1"];

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<String>,
    headers: Option<String>,
    body: Option<String>,
    flags: Option<Flag>,
}

#[derive(Debug, Clone)]
pub struct ImapServerConf {
    pub server_hostname: String,
    pub server_username: String,
    pub server_password: String,
    pub server_port: u16,
    pub use_starttls: bool,
    pub danger_accept_invalid_certs: bool,
    pub protocol: ImapProtocol,
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
type Capabilities = HashSet<Vec<u8>>;

#[macro_export]
macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
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

#[derive(Debug)]
pub struct UIDStore {
    account_hash: AccountHash,
    cache_headers: bool,
    uidvalidity: Arc<Mutex<HashMap<MailboxHash, UID>>>,
    hash_index: Arc<Mutex<HashMap<EnvelopeHash, (UID, MailboxHash)>>>,
    uid_index: Arc<Mutex<HashMap<UID, EnvelopeHash>>>,

    byte_cache: Arc<Mutex<HashMap<UID, EnvelopeCache>>>,
    tag_index: Arc<RwLock<BTreeMap<u64, String>>>,

    mailboxes: Arc<RwLock<HashMap<MailboxHash, ImapMailbox>>>,
    is_online: Arc<Mutex<(Instant, Result<()>)>>,
    refresh_events: Arc<Mutex<Vec<RefreshEvent>>>,
    sender: Arc<RwLock<Option<RefreshEventConsumer>>>,
}

impl Default for UIDStore {
    fn default() -> Self {
        UIDStore {
            account_hash: 0,
            cache_headers: false,
            uidvalidity: Default::default(),
            hash_index: Default::default(),
            uid_index: Default::default(),
            byte_cache: Default::default(),
            mailboxes: Arc::new(RwLock::new(Default::default())),
            tag_index: Arc::new(RwLock::new(Default::default())),
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
pub struct ImapType {
    account_name: String,
    is_subscribed: Arc<IsSubscribedFn>,
    connection: Arc<Mutex<ImapConnection>>,
    server_conf: ImapServerConf,
    uid_store: Arc<UIDStore>,
    can_create_flags: Arc<Mutex<bool>>,
}

#[inline(always)]
pub(self) fn try_lock<T>(
    connection: &Arc<Mutex<T>>,
    dur: Option<std::time::Duration>,
) -> Result<std::sync::MutexGuard<T>> {
    let now = Instant::now();
    while Instant::now().duration_since(now) <= dur.unwrap_or(std::time::Duration::from_millis(150))
    {
        if let Ok(guard) = connection.try_lock() {
            return Ok(guard);
        }
    }
    Err("Connection timeout".into())
}

impl MailBackend for ImapType {
    fn is_online(&self) -> Result<()> {
        if let Ok(mut g) = try_lock(&self.connection, None) {
            let _ = g.connect();
        }
        try_lock(&self.uid_store.is_online, None)?.1.clone()
    }

    fn connect(&mut self) {
        if self.is_online().is_err() {
            if Instant::now().duration_since(self.uid_store.is_online.lock().unwrap().0)
                >= std::time::Duration::new(2, 0)
            {
                if let Ok(mut g) = try_lock(&self.connection, None) {
                    let _ = g.connect();
                }
            }
        }
    }

    fn get(&mut self, mailbox: &Mailbox) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let handle = {
            let tx = w.tx();
            let uid_store = self.uid_store.clone();
            let can_create_flags = self.can_create_flags.clone();
            let mailbox_hash = mailbox.hash();
            let (permissions, mailbox_path, mailbox_exists, no_select, unseen) = {
                let f = &self.uid_store.mailboxes.read().unwrap()[&mailbox_hash];
                (
                    f.permissions.clone(),
                    f.imap_path().to_string(),
                    f.exists.clone(),
                    f.no_select,
                    f.unseen.clone(),
                )
            };
            let connection = self.connection.clone();
            let closure = move |_work_context| {
                if no_select {
                    tx.send(AsyncStatus::Payload(Ok(Vec::new()))).unwrap();
                    tx.send(AsyncStatus::Finished).unwrap();
                    return;
                }
                let _tx = tx.clone();
                if let Err(err) = (move || {
                    let tx = _tx;
                    let mut our_unseen = 0;
                    let mut max_uid: cache::MaxUID = 0;
                    let mut valid_hash_set: HashSet<EnvelopeHash> = HashSet::default();
                    let cached_hash_set: HashSet<EnvelopeHash> =
                        (|max_uid: &mut cache::MaxUID| -> Result<HashSet<EnvelopeHash>> {
                            if !uid_store.cache_headers {
                                return Ok(HashSet::default());
                            }

                            let uidvalidities = uid_store.uidvalidity.lock().unwrap();

                            let v = if let Some(v) = uidvalidities.get(&mailbox_hash) {
                                v
                            } else {
                                return Ok(HashSet::default());
                            };
                            let cached_envs: (cache::MaxUID, Vec<(UID, Envelope)>);
                            cache::save_envelopes(uid_store.account_hash, mailbox_hash, *v, &[])?;
                            cached_envs =
                                cache::get_envelopes(uid_store.account_hash, mailbox_hash, *v)?;
                            let (_max_uid, envelopes) = debug!(cached_envs);
                            *max_uid = _max_uid;
                            let ret = envelopes.iter().map(|(_, env)| env.hash()).collect();
                            if !envelopes.is_empty() {
                                let mut payload = vec![];
                                for (uid, env) in envelopes {
                                    if !env.flags().contains(Flag::SEEN) {
                                        our_unseen += 1;
                                    }
                                    uid_store
                                        .hash_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), (uid, mailbox_hash));
                                    uid_store.uid_index.lock().unwrap().insert(uid, env.hash());
                                    payload.push(env);
                                }
                                debug!("sending cached payload for {}", mailbox_hash);

                                *unseen.lock().unwrap() = our_unseen;
                                tx.send(AsyncStatus::Payload(Ok(payload))).unwrap();
                            }
                            Ok(ret)
                        })(&mut max_uid)
                        .unwrap_or_default();

                    let mut conn = connection.lock()?;
                    debug!("locked for get {}", mailbox_path);
                    let mut response = String::with_capacity(8 * 1024);

                    /* first SELECT the mailbox to get READ/WRITE permissions (because EXAMINE only
                     * returns READ-ONLY for both cases) */
                    conn.select_mailbox(mailbox_hash, &mut response)?;
                    let examine_response = protocol_parser::select_response(&response)?;
                    *can_create_flags.lock().unwrap() = examine_response.can_create_flags;
                    debug!(
                        "mailbox: {} examine_response: {:?}",
                        mailbox_path, examine_response
                    );
                    let mut exists: usize = examine_response.uidnext - 1;
                    {
                        let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

                        let v = uidvalidities
                            .entry(mailbox_hash)
                            .or_insert(examine_response.uidvalidity);
                        if uid_store.cache_headers {
                            let _ = cache::save_envelopes(
                                uid_store.account_hash,
                                mailbox_hash,
                                examine_response.uidvalidity,
                                &[],
                            );
                        }
                        *v = examine_response.uidvalidity;
                        let mut permissions = permissions.lock().unwrap();
                        permissions.create_messages = !examine_response.read_only;
                        permissions.remove_messages = !examine_response.read_only;
                        permissions.set_flags = !examine_response.read_only;
                        permissions.rename_messages = !examine_response.read_only;
                        permissions.delete_messages = !examine_response.read_only;
                        permissions.delete_messages = !examine_response.read_only;
                        let mut mailbox_exists = mailbox_exists.lock().unwrap();
                        *mailbox_exists = exists;
                    }
                    /* reselecting the same mailbox with EXAMINE prevents expunging it */
                    conn.examine_mailbox(mailbox_hash, &mut response)?;

                    let mut tag_lck = uid_store.tag_index.write().unwrap();

                    while exists > max_uid {
                        let mut envelopes = vec![];
                        debug!("{} exists= {}", mailbox_hash, exists);
                        if exists == 1 {
                            debug!("UID FETCH 1 (UID FLAGS ENVELOPE BODYSTRUCTURE)");
                            conn.send_command(b"UID FETCH 1 (UID FLAGS ENVELOPE BODYSTRUCTURE)")?;
                        } else {
                            conn.send_command(
                                debug!(format!(
                                    "UID FETCH {}:{} (UID FLAGS ENVELOPE BODYSTRUCTURE)",
                                    std::cmp::max(
                                        std::cmp::max(exists.saturating_sub(500), 1),
                                        max_uid + 1
                                    ),
                                    exists
                                ))
                                .as_bytes(),
                            )?
                        };
                        conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)?;
                        debug!(
                            "fetch response is {} bytes and {} lines",
                            response.len(),
                            response.lines().collect::<Vec<&str>>().len()
                        );
                        let (_, v, _) = protocol_parser::uid_fetch_responses(&response)?;
                        debug!("responses len is {}", v.len());
                        for UidFetchResponse {
                            uid,
                            flags,
                            envelope,
                            ..
                        } in v
                        {
                            let mut env = envelope.unwrap();
                            let mut h = DefaultHasher::new();
                            h.write_usize(uid);
                            h.write(mailbox_path.as_bytes());
                            env.set_hash(h.finish());
                            valid_hash_set.insert(env.hash());
                            if let Some((flags, keywords)) = flags {
                                if !flags.contains(Flag::SEEN) {
                                    our_unseen += 1;
                                }
                                env.set_flags(flags);
                                for f in keywords {
                                    let hash = tag_hash!(f);
                                    if !tag_lck.contains_key(&hash) {
                                        tag_lck.insert(hash, f);
                                    }
                                    env.labels_mut().push(hash);
                                }
                            }
                            uid_store
                                .hash_index
                                .lock()
                                .unwrap()
                                .insert(env.hash(), (uid, mailbox_hash));
                            uid_store.uid_index.lock().unwrap().insert(uid, env.hash());
                            envelopes.push((uid, env));
                        }
                        exists =
                            std::cmp::max(std::cmp::max(exists.saturating_sub(500), 1), max_uid);
                        debug!("sending payload for {}", mailbox_hash);
                        if uid_store.cache_headers {
                            cache::save_envelopes(
                                uid_store.account_hash,
                                mailbox_hash,
                                examine_response.uidvalidity,
                                &envelopes
                                    .iter()
                                    .map(|(uid, env)| (*uid, env))
                                    .collect::<SmallVec<[(UID, &Envelope); 1024]>>(),
                            )?;
                        }
                        for &env_hash in cached_hash_set.difference(&valid_hash_set) {
                            conn.add_refresh_event(RefreshEvent {
                                account_hash: uid_store.account_hash,
                                mailbox_hash,
                                kind: RefreshEventKind::Remove(env_hash),
                            });
                        }
                        *unseen.lock().unwrap() = our_unseen;
                        let progress = envelopes.len();
                        tx.send(AsyncStatus::Payload(Ok(envelopes
                            .into_iter()
                            .map(|(_, env)| env)
                            .collect::<Vec<Envelope>>())))
                            .unwrap();
                        tx.send(AsyncStatus::ProgressReport(progress)).unwrap();
                        if exists == 1 {
                            break;
                        }
                    }
                    drop(conn);
                    Ok(())
                })() {
                    debug!("sending error payload for {}: {:?}", mailbox_hash, &err);
                    tx.send(AsyncStatus::Payload(Err(err))).unwrap();
                }
                tx.send(AsyncStatus::Finished).unwrap();
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn refresh(
        &mut self,
        mailbox_hash: MailboxHash,
        sender: RefreshEventConsumer,
    ) -> Result<Async<()>> {
        let inbox = self
            .uid_store
            .mailboxes
            .read()
            .unwrap()
            .get(&mailbox_hash)
            .map(std::clone::Clone::clone)
            .unwrap();
        let main_conn = self.connection.clone();
        *self.uid_store.sender.write().unwrap() = Some(sender);
        let uid_store = self.uid_store.clone();
        let account_name = self.account_name.clone();
        let account_hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(self.account_name.as_bytes());
            hasher.finish()
        };
        let w = AsyncBuilder::new();
        let closure = move |work_context: WorkContext| {
            let thread = std::thread::current();
            let mut conn = match try_lock(&main_conn, Some(std::time::Duration::new(2, 0))) {
                Ok(conn) => conn,
                Err(err) => {
                    uid_store
                        .sender
                        .read()
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .send(RefreshEvent {
                            account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Failure(err.clone()),
                        });

                    return;
                }
            };
            work_context
                .set_name
                .send((
                    thread.id(),
                    format!("refreshing {} imap connection", account_name.as_str(),),
                ))
                .unwrap();

            work_context
                .set_status
                .send((thread.id(), "refresh".to_string()))
                .unwrap();
            watch::examine_updates(account_hash, &inbox, &mut conn, &uid_store, &work_context)
                .ok()
                .take();
        };
        Ok(w.build(Box::new(closure)))
    }

    fn watch(
        &self,
        sender: RefreshEventConsumer,
        work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        let conn = ImapConnection::new_connection(&self.server_conf, self.uid_store.clone());
        let main_conn = self.connection.clone();
        *self.uid_store.sender.write().unwrap() = Some(sender);
        let uid_store = self.uid_store.clone();
        let account_hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(self.account_name.as_bytes());
            hasher.finish()
        };
        let handle = std::thread::Builder::new()
            .name(format!("{} imap connection", self.account_name.as_str(),))
            .spawn(move || {
                let thread = std::thread::current();
                work_context
                    .set_status
                    .send((thread.id(), "watching".to_string()))
                    .unwrap();
                let has_idle: bool = main_conn
                    .lock()
                    .unwrap()
                    .capabilities
                    .iter()
                    .any(|cap| cap.eq_ignore_ascii_case(b"IDLE"));
                let kit = ImapWatchKit {
                    conn,
                    main_conn,
                    uid_store,
                    work_context,
                    account_hash,
                };
                if has_idle {
                    idle(kit).ok().take();
                } else {
                    poll_with_examine(kit).ok().take();
                }
            })?;
        Ok(handle.thread().id())
    }

    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>> {
        {
            let mailboxes = self.uid_store.mailboxes.read().unwrap();
            if !mailboxes.is_empty() {
                return Ok(mailboxes
                    .iter()
                    .map(|(h, f)| (*h, Box::new(Clone::clone(f)) as Mailbox))
                    .collect());
            }
        }
        let new_mailboxes = ImapType::imap_mailboxes(&self.connection)?;
        let mut mailboxes = self.uid_store.mailboxes.write()?;
        *mailboxes = new_mailboxes;
        mailboxes.retain(|_, f| (self.is_subscribed)(f.path()));
        let keys = mailboxes.keys().cloned().collect::<HashSet<MailboxHash>>();
        let mut uid_lock = self.uid_store.uidvalidity.lock().unwrap();
        for f in mailboxes.values_mut() {
            uid_lock.entry(f.hash()).or_default();
            f.children.retain(|c| keys.contains(c));
        }
        drop(uid_lock);
        Ok(mailboxes
            .iter()
            .filter(|(_, f)| f.is_subscribed)
            .map(|(h, f)| (*h, Box::new(Clone::clone(f)) as Mailbox))
            .collect())
    }

    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        let (uid, mailbox_hash) = self.uid_store.hash_index.lock().unwrap()[&hash];
        Box::new(ImapOp::new(
            uid,
            self.uid_store.mailboxes.read().unwrap()[&mailbox_hash]
                .imap_path()
                .to_string(),
            mailbox_hash,
            self.connection.clone(),
            self.uid_store.clone(),
        ))
    }

    fn save(&self, bytes: &[u8], mailbox: &str, flags: Option<Flag>) -> Result<()> {
        let path = {
            let mailboxes = self.uid_store.mailboxes.read().unwrap();

            let f_result = mailboxes
                .values()
                .find(|v| v.path == mailbox || v.name == mailbox);
            if f_result
                .map(|f| !f.permissions.lock().unwrap().create_messages)
                .unwrap_or(false)
            {
                return Err(MeliError::new(format!(
                    "You are not allowed to create messages in mailbox {}",
                    mailbox
                )));
            }

            f_result
                .map(|v| v.imap_path().to_string())
                .ok_or(MeliError::new(format!(
                    "Mailbox with name {} not found.",
                    mailbox
                )))?
        };
        let mut response = String::with_capacity(8 * 1024);
        let mut conn = try_lock(&self.connection, Some(std::time::Duration::new(5, 0)))?;
        let flags = flags.unwrap_or(Flag::empty());
        conn.send_command(
            format!(
                "APPEND \"{}\" ({}) {{{}}}",
                &path,
                flags_to_imap_list!(flags),
                bytes.len()
            )
            .as_bytes(),
        )?;
        // wait for "+ Ready for literal data" reply
        conn.wait_for_continuation_request()?;
        conn.send_literal(bytes)?;
        conn.read_response(&mut response, RequiredResponses::empty())?;
        Ok(())
    }

    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn::std::any::Any {
        self
    }

    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        if *self.can_create_flags.lock().unwrap() {
            Some(self.uid_store.tag_index.clone())
        } else {
            None
        }
    }

    fn create_mailbox(
        &mut self,
        mut path: String,
    ) -> Result<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        /* Must transform path to something the IMAP server will accept
         *
         * Each root mailbox has a hierarchy delimeter reported by the LIST entry. All paths
         * must use this delimeter to indicate children of this mailbox.
         *
         * A new root mailbox should have the default delimeter, which can be found out by issuing
         * an empty LIST command as described in RFC3501:
         * C: A101 LIST "" ""
         * S: * LIST (\Noselect) "/" ""
         *
         * The default delimiter for us is '/' just like UNIX paths. I apologise if this
         * decision is unpleasant for you.
         */

        let mut mailboxes = self.uid_store.mailboxes.write().unwrap();
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

        if mailboxes.values().any(|f| f.path == path) {
            return Err(MeliError::new(format!(
                "Mailbox named `{}` in account `{}` already exists.",
                path, self.account_name,
            )));
        }

        let mut response = String::with_capacity(8 * 1024);
        {
            let mut conn_lck = try_lock(&self.connection, None)?;

            conn_lck.send_command(format!("CREATE \"{}\"", path,).as_bytes())?;
            conn_lck.read_response(&mut response, RequiredResponses::empty())?;
            conn_lck.send_command(format!("SUBSCRIBE \"{}\"", path,).as_bytes())?;
            conn_lck.read_response(&mut response, RequiredResponses::empty())?;
        }
        let ret: Result<()> = ImapResponse::from(&response).into();
        ret?;
        let new_hash = get_path_hash!(path.as_str());
        mailboxes.clear();
        drop(mailboxes);
        Ok((new_hash, self.mailboxes().map_err(|err| MeliError::new(format!("Mailbox create was succesful (returned `{}`) but listing mailboxes afterwards returned `{}`", response, err)))?))
    }

    fn delete_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<HashMap<MailboxHash, Mailbox>> {
        let mailboxes = self.uid_store.mailboxes.read().unwrap();
        let permissions = mailboxes[&mailbox_hash].permissions();
        if !permissions.delete_mailbox {
            return Err(MeliError::new(format!("You do not have permission to delete `{}`. Set permissions for this mailbox are {}", mailboxes[&mailbox_hash].name(), permissions)));
        }
        let mut response = String::with_capacity(8 * 1024);
        {
            let mut conn_lck = try_lock(&self.connection, None)?;
            if !mailboxes[&mailbox_hash].no_select && conn_lck.current_mailbox == Some(mailbox_hash)
            {
                /* make sure mailbox is not selected before it gets deleted, otherwise
                 * connection gets dropped by server */
                conn_lck.unselect()?;
            }
            if mailboxes[&mailbox_hash].is_subscribed() {
                conn_lck.send_command(
                    format!("UNSUBSCRIBE \"{}\"", mailboxes[&mailbox_hash].imap_path()).as_bytes(),
                )?;
                conn_lck.read_response(&mut response, RequiredResponses::empty())?;
            }

            conn_lck.send_command(
                debug!(format!(
                    "DELETE \"{}\"",
                    mailboxes[&mailbox_hash].imap_path()
                ))
                .as_bytes(),
            )?;
            conn_lck.read_response(&mut response, RequiredResponses::empty())?;
        }
        let ret: Result<()> = ImapResponse::from(&response).into();
        ret?;
        let mut mailboxes = self.uid_store.mailboxes.write().unwrap();
        mailboxes.clear();
        drop(mailboxes);
        self.mailboxes().map_err(|err| format!("Mailbox delete was succesful (returned `{}`) but listing mailboxes afterwards returned `{}`", response, err).into())
    }

    fn set_mailbox_subscription(&mut self, mailbox_hash: MailboxHash, new_val: bool) -> Result<()> {
        let mut mailboxes = self.uid_store.mailboxes.write().unwrap();
        if mailboxes[&mailbox_hash].is_subscribed() == new_val {
            return Ok(());
        }

        let mut response = String::with_capacity(8 * 1024);
        {
            let mut conn_lck = try_lock(&self.connection, None)?;
            if new_val {
                conn_lck.send_command(
                    format!("SUBSCRIBE \"{}\"", mailboxes[&mailbox_hash].imap_path()).as_bytes(),
                )?;
            } else {
                conn_lck.send_command(
                    format!("UNSUBSCRIBE \"{}\"", mailboxes[&mailbox_hash].imap_path()).as_bytes(),
                )?;
            }
            conn_lck.read_response(&mut response, RequiredResponses::empty())?;
        }

        let ret: Result<()> = ImapResponse::from(&response).into();
        if ret.is_ok() {
            mailboxes.entry(mailbox_hash).and_modify(|entry| {
                let _ = entry.set_is_subscribed(new_val);
            });
        }
        ret
    }

    fn rename_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        mut new_path: String,
    ) -> Result<Mailbox> {
        let mut mailboxes = self.uid_store.mailboxes.write().unwrap();
        let permissions = mailboxes[&mailbox_hash].permissions();
        if !permissions.delete_mailbox {
            return Err(MeliError::new(format!("You do not have permission to rename mailbox `{}` (rename is equivalent to delete + create). Set permissions for this mailbox are {}", mailboxes[&mailbox_hash].name(), permissions)));
        }
        let mut response = String::with_capacity(8 * 1024);
        if mailboxes[&mailbox_hash].separator != b'/' {
            new_path = new_path.replace(
                '/',
                (mailboxes[&mailbox_hash].separator as char).encode_utf8(&mut [0; 4]),
            );
        }
        {
            let mut conn_lck = try_lock(&self.connection, None)?;
            conn_lck.send_command(
                debug!(format!(
                    "RENAME \"{}\" \"{}\"",
                    mailboxes[&mailbox_hash].imap_path(),
                    new_path
                ))
                .as_bytes(),
            )?;
            conn_lck.read_response(&mut response, RequiredResponses::empty())?;
        }
        let new_hash = get_path_hash!(new_path.as_str());
        let ret: Result<()> = ImapResponse::from(&response).into();
        ret?;
        mailboxes.clear();
        drop(mailboxes);
        self.mailboxes().map_err(|err| format!("Mailbox rename was succesful (returned `{}`) but listing mailboxes afterwards returned `{}`", response, err))?;
        Ok(BackendMailbox::clone(
            &self.uid_store.mailboxes.read().unwrap()[&new_hash],
        ))
    }

    fn set_mailbox_permissions(
        &mut self,
        mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> Result<()> {
        let mailboxes = self.uid_store.mailboxes.write().unwrap();
        let permissions = mailboxes[&mailbox_hash].permissions();
        if !permissions.change_permissions {
            return Err(MeliError::new(format!("You do not have permission to change permissions for mailbox `{}`. Set permissions for this mailbox are {}", mailboxes[&mailbox_hash].name(), permissions)));
        }

        Err(MeliError::new("Unimplemented."))
    }

    fn search(
        &self,
        query: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> Result<SmallVec<[EnvelopeHash; 512]>> {
        if mailbox_hash.is_none() {
            return Err(MeliError::new(
                "Cannot search without specifying mailbox on IMAP",
            ));
        }
        let mailbox_hash = mailbox_hash.unwrap();
        fn rec(q: &crate::search::Query, s: &mut String) {
            use crate::search::{escape_double_quote, Query::*};
            match q {
                Subject(t) => {
                    s.push_str(" SUBJECT \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\"");
                }
                From(t) => {
                    s.push_str(" FROM \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\"");
                }
                To(t) => {
                    s.push_str(" TO \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\"");
                }
                Cc(t) => {
                    s.push_str(" CC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\"");
                }
                Bcc(t) => {
                    s.push_str(" BCC \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\"");
                }
                AllText(t) => {
                    s.push_str(" TEXT \"");
                    s.extend(escape_double_quote(t).chars());
                    s.push_str("\"");
                }
                Flags(v) => {
                    for f in v {
                        match f.as_str() {
                            "draft" => {
                                s.push_str(" DRAFT ");
                            }
                            "deleted" => {
                                s.push_str(" DELETED ");
                            }
                            "flagged" => {
                                s.push_str(" FLAGGED ");
                            }
                            "recent" => {
                                s.push_str(" RECENT ");
                            }
                            "seen" | "read" => {
                                s.push_str(" SEEN ");
                            }
                            "unseen" | "unread" => {
                                s.push_str(" UNSEEN ");
                            }
                            "answered" => {
                                s.push_str(" ANSWERED ");
                            }
                            "unanswered" => {
                                s.push_str(" UNANSWERED ");
                            }
                            keyword => {
                                s.push_str(" KEYWORD ");
                                s.extend(keyword.chars());
                                s.push_str(" ");
                            }
                        }
                    }
                }
                And(q1, q2) => {
                    rec(q1, s);
                    s.push_str(" ");
                    rec(q2, s);
                }
                Or(q1, q2) => {
                    s.push_str(" OR ");
                    rec(q1, s);
                    s.push_str(" ");
                    rec(q2, s);
                }
                Not(q) => {
                    s.push_str(" NOT ");
                    rec(q, s);
                }
                _ => {}
            }
        }
        let mut query_str = String::new();
        rec(&query, &mut query_str);

        let mut response = String::with_capacity(8 * 1024);
        let mut conn = try_lock(&self.connection, Some(std::time::Duration::new(2, 0)))?;
        conn.examine_mailbox(mailbox_hash, &mut response)?;
        conn.send_command(format!("UID SEARCH CHARSET UTF-8 {}", query_str).as_bytes())?;
        conn.read_response(&mut response, RequiredResponses::SEARCH)?;
        debug!(&response);

        let mut lines = response.lines();
        for l in lines.by_ref() {
            if l.starts_with("* SEARCH") {
                use std::iter::FromIterator;
                let uid_index = self.uid_store.uid_index.lock()?;
                return Ok(SmallVec::from_iter(
                    l["* SEARCH".len()..]
                        .trim()
                        .split_whitespace()
                        .map(usize::from_str)
                        .filter_map(std::result::Result::ok)
                        .filter_map(|uid| uid_index.get(&uid))
                        .map(|env_hash_ref| *env_hash_ref),
                ));
            }
        }
        Err(MeliError::new(response))
    }
}

impl ImapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
    ) -> Result<Box<dyn MailBackend>> {
        let server_hostname = get_conf_val!(s["server_hostname"])?;
        let server_username = get_conf_val!(s["server_username"])?;
        let server_password = if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"])?.to_string()
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
        let server_port = get_conf_val!(s["server_port"], 143)?;
        let use_starttls = get_conf_val!(s["use_starttls"], !(server_port == 993))?;
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        let server_conf = ImapServerConf {
            server_hostname: server_hostname.to_string(),
            server_username: server_username.to_string(),
            server_password,
            server_port,
            use_starttls,
            danger_accept_invalid_certs,
            protocol: ImapProtocol::IMAP,
        };
        let account_hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(s.name.as_bytes());
            hasher.finish()
        };
        let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
            account_hash,
            cache_headers: get_conf_val!(s["X_header_caching"], false)?,
            ..UIDStore::default()
        });
        let connection = ImapConnection::new_connection(&server_conf, uid_store.clone());

        Ok(Box::new(ImapType {
            account_name: s.name().to_string(),
            server_conf,
            is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),

            can_create_flags: Arc::new(Mutex::new(false)),
            connection: Arc::new(Mutex::new(connection)),
            uid_store,
        }))
    }

    pub fn shell(&mut self) {
        let mut conn = ImapConnection::new_connection(&self.server_conf, self.uid_store.clone());
        conn.connect().unwrap();
        let mut res = String::with_capacity(8 * 1024);
        conn.send_command(b"NOOP").unwrap();
        conn.read_response(&mut res, RequiredResponses::empty())
            .unwrap();

        let mut input = String::new();
        loop {
            use std::io;
            input.clear();

            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    if input.trim().eq_ignore_ascii_case("logout") {
                        break;
                    }
                    conn.send_command(input.as_bytes()).unwrap();
                    conn.read_lines(&mut res, String::new()).unwrap();
                    if input.trim() == "IDLE" {
                        let mut iter = ImapBlockingConnection::from(conn);
                        while let Some(line) = iter.next() {
                            debug!("out: {}", unsafe { std::str::from_utf8_unchecked(&line) });
                        }
                        conn = iter.into_conn();
                    }
                    debug!("out: {}", &res);
                }
                Err(error) => debug!("error: {}", error),
            }
        }
    }

    pub fn imap_mailboxes(
        connection: &Arc<Mutex<ImapConnection>>,
    ) -> Result<HashMap<MailboxHash, ImapMailbox>> {
        let mut mailboxes: HashMap<MailboxHash, ImapMailbox> = Default::default();
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = try_lock(&connection, Some(std::time::Duration::new(2, 0)))?;
        conn.send_command(b"LIST \"\" \"*\"")?;
        conn.read_response(&mut res, RequiredResponses::LIST_REQUIRED)?;
        debug!("out: {}", &res);
        let mut lines = res.lines();
        /* Remove "M__ OK .." line */
        lines.next_back();
        for l in lines.map(|l| l.trim()) {
            if let Ok(mut mailbox) =
                protocol_parser::list_mailbox_result(l.as_bytes()).map(|(_, v)| v)
            {
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
            } else {
                debug!("parse error for {:?}", l);
            }
        }
        mailboxes.retain(|_, v| v.hash != 0);
        conn.send_command(b"LSUB \"\" \"*\"")?;
        conn.read_response(&mut res, RequiredResponses::LSUB_REQUIRED)?;
        debug!("out: {}", &res);
        let mut lines = res.lines();
        /* Remove "M__ OK .." line */
        lines.next_back();
        for l in lines.map(|l| l.trim()) {
            if let Ok(subscription) =
                protocol_parser::list_mailbox_result(l.as_bytes()).map(|(_, v)| v)
            {
                if let Some(f) = mailboxes.get_mut(&subscription.hash()) {
                    if subscription.no_select {
                        continue;
                    }
                    f.is_subscribed = true;
                }
            } else {
                debug!("parse error for {:?}", l);
            }
        }
        Ok(debug!(mailboxes))
    }

    pub fn capabilities(&self) -> Vec<String> {
        try_lock(&self.connection, Some(std::time::Duration::new(2, 0)))
            .map(|c| {
                c.capabilities
                    .iter()
                    .map(|c| String::from_utf8_lossy(c).into())
                    .collect::<Vec<String>>()
            })
            .unwrap_or_default()
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"])?;
        if !s.extra.contains_key("server_password_command") {
            get_conf_val!(s["server_password"])?;
        } else if s.extra.contains_key("server_password") {
            return Err(MeliError::new(format!(
                "Configuration error ({}): both server_password and server_password_command are set, cannot choose",
                s.name.as_str(),
            )));
        }
        get_conf_val!(s["server_port"], 143)?;
        get_conf_val!(s["use_starttls"], false)?;
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        get_conf_val!(s["X_header_caching"], false)?;
        Ok(())
    }
}
