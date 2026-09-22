/*
 * meli - notmuch backend
 *
 * Copyright 2019 - 2020 Manos Pitsidianakis
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
    collections::{BTreeMap, BTreeSet, HashMap},
    ffi::{CStr, CString, OsStr},
    io::Read,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    ptr::NonNull,
    sync::{Arc, Mutex, RwLock},
};

use futures::{channel::mpsc, SinkExt, StreamExt};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};

use crate::{
    backends::prelude::*,
    error::{Error, ErrorKind, IntoError, Result},
    utils::shellexpand::ShellExpandTrait,
};

macro_rules! try_call {
    ($lib:expr, $call:expr) => {{
        let status = $call;
        if status == $crate::notmuch::ffi::NOTMUCH_STATUS_SUCCESS {
            Ok(())
        } else {
            let c_str = ($lib.status_to_string())(status);
            Err($crate::notmuch::NotmuchError(
                std::ffi::CStr::from_ptr(c_str)
                    .to_string_lossy()
                    .into_owned(),
            ))
        }
    }};
}

pub mod query;
use query::{MelibQueryToNotmuchQuery, Query};
pub mod mailbox;
use mailbox::NotmuchMailbox;
pub mod api;
pub mod ffi;

use api::NotmuchLibrary;

mod directory;
mod message;
mod snapshot;
mod tags;
mod thread;

pub use directory::*;
pub use message::*;
pub use snapshot::*;
pub use tags::*;
pub use thread::*;

#[derive(Debug)]
pub struct DbPointer(pub NonNull<ffi::notmuch_database_t>, Arc<NotmuchLibrary>);

unsafe impl Send for DbPointer {}
unsafe impl Sync for DbPointer {}

impl DbPointer {
    #[inline]
    pub(self) fn as_mut(&mut self) -> *mut ffi::notmuch_database_t {
        unsafe { self.0.as_mut() }
    }
}

impl Drop for DbPointer {
    fn drop(&mut self) {
        unsafe {
            if let Err(err) = try_call!(self.1, (self.1.database_close())(self.0.as_mut())) {
                log::error!("Could not call C notmuch_database_close: {err}");
                return;
            }
            if let Err(err) = try_call!(self.1, (self.1.database_destroy())(self.0.as_mut())) {
                log::error!("Could not call C notmuch_database_destroy: {err}");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct DbConnection {
    pub lib: Arc<NotmuchLibrary>,
    pub inner: Arc<Mutex<DbPointer>>,
}

impl DbConnection {
    pub fn new(path: &Path, lib: Arc<NotmuchLibrary>, write: bool) -> Result<Self> {
        let path_c = CString::new(path.to_str().unwrap()).unwrap();
        let path_ptr = path_c.as_ptr();
        let mut database: *mut ffi::notmuch_database_t = std::ptr::null_mut();
        let status = unsafe {
            (lib.database_open())(
                path_ptr,
                if write {
                    ffi::NOTMUCH_DATABASE_MODE_READ_WRITE
                } else {
                    ffi::NOTMUCH_DATABASE_MODE_READ_ONLY
                },
                std::ptr::addr_of_mut!(database),
            )
        };
        if status != 0 {
            return Err(Error::new(format!(
                "Could not open notmuch database at path {}. notmuch_database_open returned \
                 {status}.",
                path.display()
            )));
        }
        let database = NonNull::new(database)
            .map(|ptr| DbPointer(ptr, lib.clone()))
            .ok_or_else(|| {
                Error::new("notmuch_database_open returned a NULL pointer and status = 0")
                    .set_kind(ErrorKind::LinkedLibrary("notmuch"))
                    .set_details(
                        "libnotmuch exhibited an unexpected and unrecoverable error. Make sure \
                         your libnotmuch version is compatible with this release.",
                    )
            })?;
        let ret = Self {
            lib,
            inner: Arc::new(Mutex::new(database)),
        };
        Ok(ret)
    }

    pub fn reopen(&mut self, write: bool) -> Result<()> {
        unsafe {
            try_call!(
                self.lib,
                (self.lib.database_reopen())(
                    self.inner.lock().unwrap().as_mut(),
                    if write {
                        ffi::NOTMUCH_DATABASE_MODE_READ_WRITE
                    } else {
                        ffi::NOTMUCH_DATABASE_MODE_READ_ONLY
                    },
                )
            )
        }?;
        Ok(())
    }

    fn refresh(
        &self,
        mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
        snapshot: &mut Snapshot,
        account_hash: AccountHash,
    ) -> Result<Option<BackendEvent>> {
        let mailbox_queries = mailboxes
            .read()
            .unwrap()
            .iter()
            .map(|(k, v)| {
                (
                    *k,
                    (v.total.lock().unwrap().set.clone(), v.query_str.to_string()),
                )
            })
            .collect::<HashMap<MailboxHash, (BTreeSet<EnvelopeHash>, String)>>();
        let mut events = IndexMap::new();
        for (mailbox_hash, (mut current, query_str)) in mailbox_queries {
            let mailboxes_lck = mailboxes.read().unwrap();
            let mut total_lck = mailboxes_lck[&mailbox_hash].total.lock().unwrap();
            let mut unseen_lck = mailboxes_lck[&mailbox_hash].unseen.lock().unwrap();

            let query: Query = Query::new(self, &query_str)?;

            for message in query.search()? {
                let env_hash = message.env_hash();
                if !current.remove(&env_hash) {
                    let env = snapshot.insert_envelope(&message, mailbox_hash);
                    total_lck.insert_new(env.hash());
                    if !env.is_seen() {
                        unseen_lck.insert_new(env.hash());
                    }
                    events.insert(
                        (mailbox_hash, env.hash()),
                        RefreshEventKind::Create(Box::new(env)),
                    );
                } else {
                    let (flags, tags) = message.tags().collect_flags_and_tags();
                    let prev_message =
                        Message::find_message(&snapshot.connection, message.msg_id_cstr())?;
                    let (prev_flags, prev_tags) = prev_message.tags().collect_flags_and_tags();
                    if (&flags, &tags) != (&prev_flags, &prev_tags) {
                        events.insert(
                            (mailbox_hash, env_hash),
                            RefreshEventKind::NewFlags(env_hash, (flags, tags)),
                        );
                    }
                }
            }

            for removed_hash in current {
                snapshot
                    .env_to_mailbox_index
                    .entry(removed_hash)
                    .or_default()
                    .remove(&mailbox_hash);
                total_lck.remove(removed_hash);
                unseen_lck.remove(removed_hash);
                events.insert(
                    (mailbox_hash, removed_hash),
                    RefreshEventKind::Remove(removed_hash),
                );
                snapshot.message_id_index.remove(&removed_hash);
                snapshot.env_to_mailbox_index.remove(&removed_hash);
            }
        }

        Ok(events
            .into_iter()
            .map(|((mailbox_hash, _), kind)| RefreshEvent {
                account_hash,
                mailbox_hash,
                kind,
            })
            .collect::<Vec<_>>()
            .try_into()
            .ok())
    }

    /// Return the mail root
    /// ([`NOTMUCH_CONFIG_MAIL_ROOT`](ffi::notmuch_config_key_t::NOTMUCH_CONFIG_MAIL_ROOT))
    /// of the given database's configuration.
    pub fn mail_root(&self) -> Result<CString> {
        if let Some(v) = self.lib.notmuch_config_get(
            &mut self.inner.lock().unwrap(),
            ffi::notmuch_config_key_t::NOTMUCH_CONFIG_MAIL_ROOT,
        ) {
            return Ok(v);
        }
        Err(
            Error::new("Notmuch database config has no NOTMUCH_CONFIG_MAIL_ROOT key set")
                .set_kind(ErrorKind::ValueError),
        )
    }

    /// Return mail root path of database as a [`NotmuchDirectory`].
    ///
    /// This function might return `None` if the directory is not in the
    /// database yet, for example if the database has no emails.
    pub fn root_directory(&self) -> Result<Option<NotmuchDirectory>> {
        let mut ptr = std::ptr::null_mut();
        let path = self.mail_root()?;
        unsafe {
            try_call!(
                self.lib,
                (self.lib.database_get_directory())(
                    self.inner.lock().unwrap().as_mut(),
                    path.as_ptr(),
                    &raw mut ptr
                )
            )
        }?;
        Ok(NonNull::new(ptr).map(|inner| NotmuchDirectory {
            lib: self.lib.clone(),
            path,
            db: self.inner.clone(),
            inner,
        }))
    }

    /// Return path of database as a [`NotmuchDirectory`].
    ///
    /// This function might return `None` if the directory is not in the
    /// database yet, for example if the directory has no emails.
    pub fn directory(&self, path: &CStr) -> Result<Option<NotmuchDirectory>> {
        let mut ptr = std::ptr::null_mut();
        unsafe {
            try_call!(
                self.lib,
                (self.lib.database_get_directory())(
                    self.inner.lock().unwrap().as_mut(),
                    path.as_ptr(),
                    &raw mut ptr
                )
            )
        }?;
        Ok(NonNull::new(ptr).map(|inner| NotmuchDirectory {
            lib: self.lib.clone(),
            path: path.into(),
            db: self.inner.clone(),
            inner,
        }))
    }
}

#[derive(Debug)]
pub struct NotmuchError(String);

impl std::fmt::Display for NotmuchError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl std::error::Error for NotmuchError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl From<NotmuchError> for Error {
    fn from(err: NotmuchError) -> Self {
        Self::new(err.0)
    }
}

#[derive(Debug)]
pub struct NotmuchDb {
    #[allow(dead_code)]
    lib: Arc<NotmuchLibrary>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
    snapshot: Arc<RwLock<Snapshot>>,
    collection: Collection,
    path: PathBuf,
    _account_name: Arc<str>,
    account_hash: AccountHash,
    event_consumer: BackendEventConsumer,
    save_messages_to: Option<PathBuf>,
}

impl NotmuchDb {
    #[cfg(target_os = "linux")]
    pub const DEFAULT_DYLIB_NAME: &'static str = "libnotmuch.so.5";
    #[cfg(target_os = "macos")]
    pub const DEFAULT_DYLIB_NAME: &'static str = "libnotmuch.5.dylib";
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    pub const DEFAULT_DYLIB_NAME: &'static str = "libnotmuch.so";

    pub fn new(
        s: &AccountSettings,
        _is_subscribed: IsSubscribedFn,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<Self>> {
        let mut dlpath = Cow::Borrowed(Self::DEFAULT_DYLIB_NAME);
        let custom_dlpath = if let Some(lib_path) =
            s.deserialize_extra_field::<Cow<'_, str>>("library_file_path")?
        {
            let expanded_path = Path::new(lib_path.as_ref()).expand();
            let expanded_path_string = expanded_path.display().to_string();
            dlpath = if expanded_path_string != lib_path.as_ref()
                && expanded_path.try_exists().unwrap_or(false)
            {
                Cow::Owned(expanded_path_string)
            } else {
                Cow::Owned(lib_path.to_string())
            };
            true
        } else {
            false
        };
        let lib = Arc::new(NotmuchLibrary::new(
            unsafe {
                match libloading::Library::new(dlpath.as_ref()) {
                    Ok(l) => l,
                    Err(err) => {
                        if custom_dlpath {
                            return Err(Error::new(format!(
                                "Notmuch `library_file_path` setting value `{dlpath}` for account \
                                 {} does not exist or is a directory or not a valid library file.",
                                s.name
                            ))
                            .set_kind(ErrorKind::Configuration)
                            .set_source(Some(Arc::new(err))));
                        } else {
                            return Err(Error::new("Could not load libnotmuch!")
                                .set_details(super::NOTMUCH_ERROR_DETAILS)
                                .set_source(Some(Arc::new(err))));
                        }
                    }
                }
            },
            dlpath,
        ));
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not exist.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_related_path(Some(path))
            .set_kind(ErrorKind::Configuration));
        }
        if !path.is_dir() {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} is not a directory.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_related_path(Some(path))
            .set_kind(ErrorKind::Configuration));
        }
        path.push(".notmuch");
        if !path.try_exists().unwrap_or(false) || !path.is_dir() {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not contain a `.notmuch` \
                 subdirectory.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_related_path(Some(path))
            .set_kind(ErrorKind::Configuration));
        }
        path.pop();

        if s.mailboxes.is_empty() {
            return Err(Error::new(format!(
                "Notmuch account `{}` requires mailboxes explicitly set, since they are virtual, \
                 but none are configured. Try adding some.",
                s.name
            ))
            .set_kind(ErrorKind::Configuration));
        }
        let mut mailboxes = HashMap::with_capacity(s.mailboxes.len());
        let mut parents: Vec<(MailboxHash, &str)> = Vec::with_capacity(s.mailboxes.len());
        for (k, f) in s.mailboxes.iter() {
            if let Some(query_str) = f.extra.get("query") {
                let hash = MailboxHash::from_bytes(k.as_bytes());
                if let Some(parent) = f.extra.get("parent") {
                    parents.push((hash, parent));
                }
                mailboxes.insert(
                    hash,
                    NotmuchMailbox {
                        hash,
                        name: k.to_string(),
                        path: k.to_string(),
                        children: vec![],
                        parent: None,
                        query_str: query_str.to_string(),
                        usage: Arc::new(RwLock::new(SpecialUsageMailbox::Normal)),
                        total: Arc::new(Mutex::new(LazyCountSet::new())),
                        unseen: Arc::new(Mutex::new(LazyCountSet::new())),
                    },
                );
            } else {
                return Err(Error::new(format!(
                    "notmuch mailbox configuration entry `{k}` for account {} should have a \
                     `query` value set.",
                    s.name,
                ))
                .set_kind(ErrorKind::Configuration));
            }
        }
        for (hash, parent) in parents {
            if let Some(&parent_hash) = mailboxes
                .iter()
                .find(|(_, v)| v.name == parent)
                .map(|(k, _)| k)
            {
                mailboxes
                    .entry(parent_hash)
                    .or_default()
                    .children
                    .push(hash);
                mailboxes.entry(hash).or_default().parent = Some(parent_hash);
            } else {
                return Err(Error::new(format!(
                    "Mailbox configuration for `{}` defines its parent mailbox as `{parent}` but \
                     no mailbox exists with this exact name.",
                    mailboxes[&hash].name()
                ))
                .set_kind(ErrorKind::Configuration));
            }
        }

        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        let connection = DbConnection::new(path.as_path(), lib.clone(), false)?;
        let collection = Collection::default();
        Ok(Box::new(Self {
            lib,
            path,
            snapshot: Arc::new(RwLock::new(Snapshot {
                connection,
                message_id_index: Default::default(),
                env_to_mailbox_index: Default::default(),
                tag_index: collection.tag_index.clone(),
                account_hash,
            })),
            collection,
            mailboxes: Arc::new(RwLock::new(mailboxes)),
            save_messages_to: None,
            _account_name: s.name.to_string().into(),
            account_hash,
            event_consumer,
        }))
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not exist.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_related_path(Some(path))
            .set_kind(ErrorKind::Configuration));
        }
        if !path.is_dir() {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} is not a directory.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_related_path(Some(path))
            .set_kind(ErrorKind::Configuration));
        }
        path.push(".notmuch");
        if !path.try_exists().unwrap_or(false) || !path.is_dir() {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not contain a `.notmuch` \
                 subdirectory.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_related_path(Some(path))
            .set_kind(ErrorKind::Configuration));
        }
        path.pop();

        let account_name = s.name.to_string();
        s.validator::<Cow<'_, str>>("library_file_path", "string")
            .validation_fn(|value| {
                let lib_path: &str = value.as_ref();

                let expanded_path = Path::new(lib_path).expand();
                if (!Path::new(lib_path).try_exists().unwrap_or(false)
                    || Path::new(lib_path).is_dir())
                    && !Path::new(&expanded_path).try_exists().unwrap_or(false)
                    || Path::new(&expanded_path).is_dir()
                {
                    return Err(Error::new(format!(
                        "Notmuch `library_file_path` setting value `{lib_path}` does not exist or \
                         is a directory.",
                    ))
                    .set_related_path(Some(lib_path))
                    .set_kind(ErrorKind::Configuration));
                }
                Ok(())
            })
            .ignore_missing()?;
        if s.mailboxes.is_empty() {
            return Err(Error::new(format!(
                "Notmuch account `{account_name}` requires mailboxes explicitly set, since they \
                 are virtual, but none are configured. Try adding some."
            ))
            .set_kind(ErrorKind::Configuration));
        }
        let mut parents: Vec<(String, String)> = Vec::with_capacity(s.mailboxes.len());
        for (k, f) in s.mailboxes.iter_mut() {
            if f.extra.swap_remove("query").is_none() {
                return Err(Error::new(format!(
                    "notmuch mailbox configuration entry `{k}` for account {account_name} should \
                     have a `query` value set."
                ))
                .set_kind(ErrorKind::Configuration));
            }
            if let Some(parent) = f.extra.swap_remove("parent") {
                parents.push((k.clone(), parent));
            }
        }
        let mut path = Vec::with_capacity(8);
        for (mbox, parent) in parents.iter() {
            if !s.mailboxes.contains_key(parent) {
                return Err(Error::new(format!(
                    "Mailbox configuration for `{mbox}` defines its parent mailbox as `{parent}` \
                     but no mailbox exists with this exact name."
                ))
                .set_kind(ErrorKind::Configuration));
            }
            path.clear();
            path.push(mbox.as_str());
            let mut iter = parent.as_str();
            while let Some((k, v)) = parents.iter().find(|(k, _v)| k == iter) {
                if k == mbox {
                    return Err(Error::new(format!(
                        "Found cycle in mailbox hierarchy: {}",
                        path.join("->")
                    ))
                    .set_kind(ErrorKind::Configuration));
                }
                path.push(k.as_str());
                iter = v.as_str();
            }
        }
        Ok(())
    }
}

impl MailBackend for NotmuchDb {
    fn capabilities(&mut self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            supports_search: true,
            supports_raw_search: true,
            supports_tags: true,
            ..crate::backends::EMPTY_MAIL_BACKEND_CAPABILITIES
        };
        CAPABILITIES
    }

    fn is_online(&mut self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        let snapshot = self.snapshot.clone();
        struct FetchState {
            mailbox_hash: MailboxHash,
            database: Arc<DbConnection>,
            snapshot: Arc<RwLock<Snapshot>>,
            mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
            iter: std::vec::IntoIter<CString>,
        }
        impl FetchState {
            async fn fetch(&mut self) -> Result<Option<Vec<Envelope>>> {
                let chunk_size = 250;
                let mut snapshot = self.snapshot.write().unwrap();
                let mut ret: Vec<Envelope> = Vec::with_capacity(chunk_size);
                let mut done: bool = false;
                for _ in 0..chunk_size {
                    if let Some(message_id) = self.iter.next() {
                        let Ok(message) = Message::find_message(&self.database, &message_id) else {
                            continue;
                        };
                        ret.push(snapshot.insert_envelope(&message, self.mailbox_hash));
                    } else {
                        done = true;
                        break;
                    }
                }
                {
                    let mailboxes_lck = self.mailboxes.read().unwrap();
                    let mailbox = mailboxes_lck.get(&self.mailbox_hash).unwrap();
                    mailbox.unseen.lock().unwrap().insert_set(
                        ret.iter()
                            .filter_map(|env| {
                                if !env.is_seen() {
                                    Some(env.hash())
                                } else {
                                    None
                                }
                            })
                            .collect(),
                    );
                    mailbox
                        .total
                        .lock()
                        .unwrap()
                        .insert_set(ret.iter().map(|env| env.hash()).collect());
                }
                if done && ret.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(ret))
                }
            }
        }
        let database = Arc::new(DbConnection::new(
            self.path.as_path(),
            self.lib.clone(),
            false,
        )?);
        let mailboxes = self.mailboxes.clone();
        let v: Vec<CString>;
        {
            let mailboxes_lck = mailboxes.read().unwrap();
            let mailbox = mailboxes_lck.get(&mailbox_hash).unwrap();
            let query: Query = Query::new(&database, mailbox.query_str.as_str())?;
            {
                let mut total_lck = mailbox.total.lock().unwrap();
                total_lck.clear();
                total_lck.set_not_yet_seen(query.count()? as usize);
                mailbox.unseen.lock().unwrap().clear()
            }
            let mut snapshot = snapshot.write().unwrap();
            v = query
                .search()?
                .map(|m| {
                    snapshot
                        .message_id_index
                        .insert(m.env_hash(), m.msg_id_cstr().into());
                    m.msg_id_cstr().into()
                })
                .collect();
        }

        let mut state = FetchState {
            mailbox_hash,
            mailboxes,
            database,
            snapshot,
            iter: v.into_iter(),
        };
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            while let Some(res) = state.fetch().await.inspect_err(|err| {
                log::debug!("fetch err {:?}", err);
            })? {
                emitter.emit(res).await;
            }
            Ok(())
        })))
    }

    fn refresh(&mut self, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let account_hash = self.account_hash;
        let new_connection = DbConnection::new(self.path.as_path(), self.lib.clone(), false)?;
        let snapshot = self.snapshot.clone();
        let mailboxes = self.mailboxes.clone();
        let event_consumer = self.event_consumer.clone();
        Ok(Box::pin(async move {
            let events = {
                let mut snapshot_lck = snapshot.write().unwrap();
                let events =
                    new_connection.refresh(mailboxes.clone(), &mut snapshot_lck, account_hash)?;
                if events.is_some() {
                    snapshot_lck.connection = new_connection;
                }
                events
            };
            if let Some(evn) = events {
                (event_consumer)(account_hash, evn);
            }
            Ok(())
        }))
    }

    fn watch(&mut self) -> ResultStream<BackendEvent> {
        let account_hash = self.account_hash;
        let snapshot = self.snapshot.clone();
        let path = self.path.clone();
        let lib = self.lib.clone();
        let mailboxes = self.mailboxes.clone();

        let (mut tx, mut rx) = mpsc::channel(16);
        let watcher = RecommendedWatcher::new(
            move |res: notify::Result<notify::Event>| {
                use notify::event::EventKind;

                if matches!(res, Ok(ref ev) if matches!(ev.kind, EventKind::Access(_) | EventKind::Other)) {
                    return;
                }
                futures::executor::block_on(async {
                    _ = tx.send(res).await;
                })
            },
            notify::Config::default().with_poll_interval(std::time::Duration::from_secs(2)),
        )
        .and_then(|mut watcher| {
            let mut path = path.clone();
            path.push(".notmuch");
            watcher.watch(&path, RecursiveMode::Recursive)?;
            Ok(watcher)
        })
        .map_err(|err| err.set_err_details("Failed to create file change monitor."))?;
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            // Move watcher to prevent it being Dropped.
            let _watcher = watcher;
            // Set to true whenever a filesystem event is received, and poll more
            // frequently as long as it is true. This allows us to be more sensitive
            // about updates whenever notmuch-new is more likely to have been
            // called.
            let mut is_fs_event: bool = true;
            loop {
                let sleep_fut =
                    crate::utils::futures::sleep(std::time::Duration::from_secs(if is_fs_event {
                        is_fs_event = false;
                        2
                    } else {
                        30
                    }));
                match futures::future::select(rx.next(), std::pin::pin!(sleep_fut)).await {
                    futures::future::Either::Left((None, _)) => {
                        break;
                    }
                    futures::future::Either::Left((Some(ev), _)) => {
                        is_fs_event = true;
                        ev?;
                    }
                    futures::future::Either::Right((_, _)) => {}
                }

                let events = {
                    let new_connection = DbConnection::new(path.as_path(), lib.clone(), false)?;
                    let mut snapshot_lck = snapshot.write().unwrap();
                    let events = new_connection.refresh(
                        mailboxes.clone(),
                        &mut snapshot_lck,
                        account_hash,
                    )?;
                    if events.is_some() {
                        snapshot_lck.connection = new_connection;
                    }
                    events
                };
                if let Some(evn) = events {
                    emitter.emit(evn).await;
                }
            }
            Ok(())
        })))
    }

    fn mailboxes(&mut self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let ret = Ok(self
            .mailboxes
            .read()
            .unwrap()
            .iter()
            .map(|(k, f)| (*k, BackendMailbox::clone(f)))
            .collect());
        Ok(Box::pin(async { ret }))
    }

    fn envelope_bytes_by_hash(&mut self, hash: EnvelopeHash) -> ResultFuture<Vec<u8>> {
        let op = NotmuchOp {
            database: Arc::new(DbConnection::new(
                self.path.as_path(),
                self.lib.clone(),
                true,
            )?),
            lib: self.lib.clone(),
            hash,
            snapshot: self.snapshot.clone(),
        };

        Ok(Box::pin(async move { op.as_bytes().await }))
    }

    fn save(
        &mut self,
        bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()> {
        // [ref:FIXME]: call notmuch_database_index_file ?
        let path = self
            .save_messages_to
            .as_ref()
            .unwrap_or(&self.path)
            .to_path_buf();
        crate::maildir::MaildirType::save_to_mailbox(path, bytes, flags)?;
        Ok(Box::pin(async { Ok(()) }))
    }

    fn copy_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _source_mailbox_hash: MailboxHash,
        _destination_mailbox_hash: MailboxHash,
        _move_: bool,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Copying messages is currently unimplemented for notmuch backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
        flags: Vec<FlagOp>,
    ) -> ResultFuture<()> {
        let database = DbConnection::new(self.path.as_path(), self.lib.clone(), true)?;
        let tag_index = self.collection.clone().tag_index;
        let snapshot = self.snapshot.clone();

        Ok(Box::pin(async move {
            let mut snapshot = snapshot.write().unwrap();
            for env_hash in env_hashes.iter() {
                let message =
                    match Message::find_message(&database, &snapshot.message_id_index[&env_hash]) {
                        Ok(v) => v,
                        Err(err) => {
                            log::debug!("not found {err}");
                            continue;
                        }
                    };
                message.tags_to_maildir_flags()?;
                message.freeze();

                let tags = message.tags().collect::<Vec<&CStr>>();

                macro_rules! add_tag {
                    ($l:expr) => {{
                        let l = &$l;
                        if tags.contains(l) {
                            continue;
                        }
                        message.add_tag(l)?;
                    }};
                }
                macro_rules! remove_tag {
                    ($l:expr) => {{
                        let l = &$l;
                        if !tags.contains(l) {
                            continue;
                        }
                        message.remove_tag(l)?;
                    }};
                }

                for op in flags.iter() {
                    match op {
                        FlagOp::Set(Flag::DRAFT) => add_tag!(c"draft"),
                        FlagOp::UnSet(Flag::DRAFT) => remove_tag!(c"draft"),
                        FlagOp::Set(Flag::FLAGGED) => add_tag!(c"flagged"),
                        FlagOp::UnSet(Flag::FLAGGED) => remove_tag!(c"flagged"),
                        FlagOp::Set(Flag::PASSED) => add_tag!(c"passed"),
                        FlagOp::UnSet(Flag::PASSED) => remove_tag!(c"passed"),
                        FlagOp::Set(Flag::REPLIED) => add_tag!(c"replied"),
                        FlagOp::UnSet(Flag::REPLIED) => remove_tag!(c"replied"),
                        FlagOp::Set(Flag::SEEN) => remove_tag!(c"unread"),
                        FlagOp::UnSet(Flag::SEEN) => add_tag!(c"unread"),
                        FlagOp::Set(Flag::TRASHED) => add_tag!(c"trashed"),
                        FlagOp::UnSet(Flag::TRASHED) => remove_tag!(c"trashed"),
                        FlagOp::SetTag(tag) => {
                            let c_tag = CString::new(tag.as_str()).unwrap();
                            add_tag!(&c_tag.as_ref());
                        }
                        FlagOp::UnSetTag(tag) => {
                            let c_tag = CString::new(tag.as_str()).unwrap();
                            remove_tag!(&c_tag.as_ref());
                        }
                        _ => log::debug!("flag_op is {:?}", op),
                    }
                }

                message.thaw();

                /* Update message filesystem path. */
                message.tags_to_maildir_flags()?;

                let msg_id = message.msg_id_cstr();
                if let Some(p) = snapshot.message_id_index.get_mut(&env_hash) {
                    *p = msg_id.into();
                }
            }
            for op in flags.iter() {
                if let FlagOp::SetTag(tag) = op {
                    let hash = TagHash::from_bytes(tag.as_bytes());
                    tag_index.write().unwrap().insert(hash, tag.to_string());
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
        Err(
            Error::new("Deleting messages is currently unimplemented for notmuch backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn search(
        &mut self,
        melib_query: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<Vec<EnvelopeHash>> {
        let database = DbConnection::new(self.path.as_path(), self.lib.clone(), false)?;
        let mailboxes = self.mailboxes.clone();
        Ok(Box::pin(async move {
            let mut query_s = if let Some(mailbox_hash) = mailbox_hash {
                if let Some(m) = mailboxes.read().unwrap().get(&mailbox_hash) {
                    let mut s = m.query_str.clone();
                    s.push(' ');
                    s
                } else {
                    return Err(
                        Error::new(format!("Mailbox with hash {mailbox_hash} not found!"))
                            .set_kind(ErrorKind::NotFound),
                    );
                }
            } else {
                String::new()
            };
            let wasnt_empty = !query_s.trim().is_empty();
            if wasnt_empty {
                query_s.push_str(" AND (");
            }
            melib_query.query_to_string(&mut query_s)?;
            if wasnt_empty {
                query_s.push(')');
            }
            let query: Query = Query::new(&database, &query_s)?;
            Ok(query.search()?.map(|message| message.env_hash()).collect())
        }))
    }

    fn raw_search(
        &mut self,
        query_str: String,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<Vec<EnvelopeHash>> {
        let database = DbConnection::new(self.path.as_path(), self.lib.clone(), false)?;
        let mailboxes = self.mailboxes.clone();
        Ok(Box::pin(async move {
            let mailbox_query_s = if let Some(mailbox_hash) = mailbox_hash {
                if let Some(m) = mailboxes.read().unwrap().get(&mailbox_hash) {
                    let mut s = m.query_str.clone();
                    s.push(' ');
                    s
                } else {
                    return Err(
                        Error::new(format!("Mailbox with hash {mailbox_hash} not found!"))
                            .set_kind(ErrorKind::NotFound),
                    );
                }
            } else {
                String::new()
            };
            let query_s = if mailbox_query_s.trim().is_empty() {
                query_str
            } else {
                format!("{mailbox_query_s} AND ({query_str})")
            };
            let query: Query = Query::new(&database, &query_s)?;
            Ok(query.search()?.map(|message| message.env_hash()).collect())
        }))
    }

    fn collection(&self) -> Collection {
        self.collection.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(
            Error::new("Deleting mailboxes is currently unimplemented for notmuch backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Mailbox subscriptions are not possible for the notmuch backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(
            Error::new("Renaming mailboxes is currently unimplemented for notmuch backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Setting mailbox permissions is not possible for the notmuch backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn create_mailbox(
        &mut self,
        _new_path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(
            Error::new("Creating mailboxes is unimplemented for the notmuch backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }
}

#[derive(Clone, Debug)]
struct NotmuchOp {
    hash: EnvelopeHash,
    snapshot: Arc<RwLock<Snapshot>>,
    database: Arc<DbConnection>,
    #[allow(dead_code)]
    lib: Arc<NotmuchLibrary>,
}

impl NotmuchOp {
    async fn as_bytes(&self) -> Result<Vec<u8>> {
        let _self = self.clone();
        smol::unblock(move || {
            let snapshot = _self.snapshot.write().unwrap();
            let message =
                Message::find_message(&_self.database, &snapshot.message_id_index[&_self.hash])?;
            let mut f = std::fs::File::open(message.get_filename())?;
            let mut response = Vec::new();
            f.read_to_end(&mut response)?;
            Ok(response)
        })
        .await
    }
}
