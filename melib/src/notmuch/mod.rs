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
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
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

macro_rules! call {
    ($lib:expr, $func:ty) => {{
        const S: &str = stringify!($func);
        let func: libloading::Symbol<$func> = $lib
            .inner
            .get(S.split("::").last().unwrap_or(S).trim().as_bytes())
            .unwrap();
        func
    }};
}

macro_rules! try_call {
    ($lib:expr, $call:expr) => {{
        let status = $call;
        if status == $crate::notmuch::ffi::NOTMUCH_STATUS_SUCCESS {
            Ok(())
        } else {
            let c_str = call!($lib, $crate::notmuch::ffi::notmuch_status_to_string)(status);
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
pub mod ffi;
use ffi::{notmuch_database_close, notmuch_database_destroy, notmuch_database_open};

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
#[repr(transparent)]
pub struct DbPointer(pub NonNull<ffi::notmuch_database_t>);

unsafe impl Send for DbPointer {}
unsafe impl Sync for DbPointer {}

impl DbPointer {
    #[inline]
    pub(self) fn as_mut(&mut self) -> *mut ffi::notmuch_database_t {
        unsafe { self.0.as_mut() }
    }
}

#[derive(Debug)]
pub struct NotmuchLibrary {
    pub inner: libloading::Library,
    pub dlpath: Cow<'static, str>,
}

#[derive(Debug)]
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
            call!(lib, notmuch_database_open)(
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
        #[cfg(debug_assertions)]
        let database = DbPointer(
            NonNull::new(database)
                .expect("notmuch_database_open returned a NULL pointer and status = 0"),
        );
        #[cfg(not(debug_assertions))]
        let database = NonNull::new(database).map(DbPointer).ok_or_else(|| {
            Error::new("notmuch_database_open returned a NULL pointer and status = 0")
                .set_kind(ErrorKind::LinkedLibrary("notmuch"))
                .set_details(
                    "libnotmuch exhibited an unexpected and unrecoverable error. Make sure your \
                     libnotmuch version is compatible with this release.",
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
                call!(self.lib, ffi::notmuch_database_reopen)(
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
        let mut stack: VecDeque<CString> = VecDeque::new();
        let mut events = vec![];
        stack.push_back(self.mail_root()?.into());
        while let Some(path) = stack.pop_front() {
            let mut directory_snapshot: Option<NotmuchDirectory> =
                snapshot.connection.directory(&path)?;
            let mut directory_current: Option<NotmuchDirectory> = self.directory(&path)?;
            let mtime_current: libc::time_t =
                directory_current.as_mut().map(|d| d.mtime()).unwrap_or(0);
            let mtime_snapshot: libc::time_t =
                directory_snapshot.as_mut().map(|d| d.mtime()).unwrap_or(0);
            let (current_files, current_subdirs): (HashSet<CString>, HashSet<CString>) =
                if let Some(directory) = directory_current.as_mut() {
                    assert_eq!(&directory.path, &path);
                    let current_files = directory.child_files_paths().collect();
                    let current_subdirs = directory
                        .child_directories_paths()
                        .collect::<HashSet<CString>>();
                    for subdir in &current_subdirs {
                        stack.push_back(subdir.clone());
                    }
                    (current_files, current_subdirs)
                } else {
                    let mut current_files = HashSet::new();
                    let mut current_subdirs = HashSet::new();
                    for entry in std::fs::read_dir(OsStr::from_bytes(path.as_bytes()))? {
                        let dir = entry?;
                        let entry_path = CString::new(dir.path().as_os_str().as_bytes()).unwrap();
                        if dir.file_type()?.is_dir() {
                            // Pass 1: For each directory in current_subdirs, add to stack to visit
                            // in the next loops
                            current_subdirs.insert(entry_path.clone());
                            stack.push_back(entry_path);
                        } else {
                            current_files.insert(entry_path);
                        }
                    }
                    (current_files, current_subdirs)
                };

            // Compare mtime_snapshot to mtime_current. If they are equivalent, terminate
            // the algorithm at this point, (this directory has not been updated
            // in the filesystem since the last database scan of PASS 2).
            //
            // If the directory's modification time in the filesystem is the same as what we
            // recorded in the database the last time we scanned it, then we can skip the
            // second pass entirely.
            //
            // We test for strict equality here to avoid a bug that can happen if the system
            // clock jumps backward, (preventing new mail from being discovered
            // until the clock catches up and the directory is modified again).

            if directory_snapshot.is_some()
                && directory_current.is_some()
                && mtime_snapshot == mtime_current
                && mtime_snapshot != 0
            {
                continue;
            }
            // Ask the snapshot database for files and directories within 'path'
            // (snapshot_files and snapshot_subdirs)
            let (snapshot_files, snapshot_subdirs): (HashSet<CString>, HashSet<CString>) =
                if let Some(directory) = directory_snapshot.as_mut() {
                    (
                        directory.child_files_paths().collect(),
                        directory.child_directories_paths().collect(),
                    )
                } else {
                    // If the database has never seen this directory before, we can
                    // simply leave snapshot_files and snapshot_subdirs None.
                    (HashSet::default(), HashSet::default())
                };

            // Pass 2: Walk current_files simultaneously with snapshot_files current_subdirs
            // with snapshot_subdirs. Look for one of three interesting cases:

            // 1. Regular file in current_files and not in snapshot_files This is a new file
            //    to add_message into the database.
            for new_message in current_files.difference(&snapshot_files) {
                let Some(message) = Message::find_message_by_path(self, new_message)? else {
                    // Path does not correspond to a message.
                    continue;
                };
                for (&mailbox_hash, m) in mailboxes.read().unwrap().iter() {
                    let query_str = format!("{} id:{}", m.query_str.as_str(), message.msg_id_str());
                    let query: Query = Query::new(self, &query_str)?;
                    if query.count().unwrap_or(0) > 0 {
                        let env = snapshot.insert_envelope(&message);
                        snapshot
                            .env_to_mailbox_index
                            .entry(env.hash())
                            .or_default()
                            .push(mailbox_hash);
                        let mut total_lck = m.total.lock().unwrap();
                        let mut unseen_lck = m.unseen.lock().unwrap();
                        *total_lck += 1;
                        if !env.is_seen() {
                            *unseen_lck += 1;
                        }
                        events.push(RefreshEvent {
                            account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Create(Box::new(env)),
                        });
                    }
                }
            }

            let mut removed_dir_stack = VecDeque::new();
            let mut removed_files: HashSet<Cow<'_, CStr>> = HashSet::default();
            // 2. Filename in snapshot_files not in current_files. This is a file that has
            //    been removed from the mail store.
            for removed_file in snapshot_files.difference(&current_files) {
                removed_files.insert(Cow::Borrowed(removed_file));
            }

            // 3. Directory in snapshot_subdirs not in current_subdirs This is a directory
            //    that has been removed from the mail store.
            for removed_dir in snapshot_subdirs.difference(&current_subdirs) {
                removed_dir_stack.push_back(Cow::Borrowed(removed_dir));
            }
            while let Some(removed_dir) = removed_dir_stack.pop_front() {
                let mut directory_snapshot: Option<NotmuchDirectory> =
                    snapshot.connection.directory(&removed_dir)?;
                if let Some(directory) = directory_snapshot.as_mut() {
                    removed_files.extend(directory.child_files_paths().map(Cow::Owned));
                    removed_dir_stack.extend(directory.child_directories_paths().map(Cow::Owned));
                }
            }
            for removed_file in removed_files {
                let env_hash = {
                    let Some(message) =
                        Message::find_message_by_path(&snapshot.connection, &removed_file)?
                    else {
                        // Path does not correspond to a message.
                        continue;
                    };
                    message.env_hash()
                };
                events.extend(snapshot.remove_envelope(env_hash));
            }
        }

        Ok(events.try_into().ok())
    }

    /// Return the mail root
    /// ([`NOTMUCH_CONFIG_MAIL_ROOT`](ffi::notmuch_config_key_t::NOTMUCH_CONFIG_MAIL_ROOT))
    /// of the given database's configuration.
    pub fn mail_root(&self) -> Result<&CStr> {
        let ptr = unsafe {
            call!(self.lib, ffi::notmuch_config_get)(
                self.inner.lock().unwrap().as_mut(),
                ffi::notmuch_config_key_t::NOTMUCH_CONFIG_MAIL_ROOT,
            )
        };
        // let ptr = unsafe {
        //     call!(self.lib,
        // ffi::notmuch_database_get_path)(self.inner.lock().unwrap().as_mut())
        // };
        Ok(unsafe { CStr::from_ptr(ptr) })
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
                call!(self.lib, ffi::notmuch_database_get_directory)(
                    self.inner.lock().unwrap().as_mut(),
                    path.as_ptr(),
                    &mut ptr
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

    /// Return path of database as a [`NotmuchDirectory`].
    ///
    /// This function might return `None` if the directory is not in the
    /// database yet, for example if the directory has no emails.
    pub fn directory(&self, path: &CStr) -> Result<Option<NotmuchDirectory>> {
        let mut ptr = std::ptr::null_mut();
        unsafe {
            try_call!(
                self.lib,
                call!(self.lib, ffi::notmuch_database_get_directory)(
                    self.inner.lock().unwrap().as_mut(),
                    path.as_ptr(),
                    &mut ptr
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

impl Drop for DbConnection {
    fn drop(&mut self) {
        let mut inner = self.inner.lock().unwrap();
        unsafe {
            if let Err(err) = try_call!(
                self.lib,
                call!(self.lib, notmuch_database_close)(inner.as_mut())
            ) {
                log::error!("Could not call C notmuch_database_close: {err}");
                return;
            }
            if let Err(err) = try_call!(
                self.lib,
                call!(self.lib, notmuch_database_destroy)(inner.as_mut())
            ) {
                log::error!("Could not call C notmuch_database_destroy: {err}");
            }
        }
    }
}

#[derive(Debug)]
pub struct NotmuchDb {
    #[allow(dead_code)]
    lib: Arc<NotmuchLibrary>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
    snapshot: Arc<RwLock<Snapshot>>,
    index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    mailbox_index: Arc<RwLock<HashMap<EnvelopeHash, SmallVec<[MailboxHash; 16]>>>>,
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
        let mut custom_dlpath = false;
        if let Some(lib_path) = s.extra.get("library_file_path") {
            let expanded_path = Path::new(lib_path).expand();
            let expanded_path_string = expanded_path.display().to_string();
            dlpath = if &expanded_path_string != lib_path
                && expanded_path.try_exists().unwrap_or(false)
            {
                Cow::Owned(expanded_path_string)
            } else {
                Cow::Owned(lib_path.to_string())
            };
            custom_dlpath = true;
        }
        let lib = Arc::new(NotmuchLibrary {
            inner: unsafe {
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
        });
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not exist.",
                s.root_mailbox.as_str(),
                s.name
            ))
            .set_kind(ErrorKind::Configuration));
        }
        if !path.is_dir() {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} is not a directory.",
                s.root_mailbox.as_str(),
                s.name
            ))
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
            .set_kind(ErrorKind::Configuration));
        }
        path.pop();

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
                        total: Arc::new(Mutex::new(0)),
                        unseen: Arc::new(Mutex::new(0)),
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
            index: Arc::new(RwLock::new(Default::default())),
            mailbox_index: Arc::new(RwLock::new(Default::default())),
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
            .set_kind(ErrorKind::Configuration));
        }
        if !path.is_dir() {
            return Err(Error::new(format!(
                "Notmuch `root_mailbox` {} for account {} is not a directory.",
                s.root_mailbox.as_str(),
                s.name
            ))
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
            .set_kind(ErrorKind::Configuration));
        }
        path.pop();

        let account_name = s.name.to_string();
        if let Some(lib_path) = s.extra.swap_remove("library_file_path") {
            let expanded_path = Path::new(&lib_path).expand();
            if (!Path::new(&lib_path).try_exists().unwrap_or(false)
                || Path::new(&lib_path).is_dir())
                && !Path::new(&expanded_path).try_exists().unwrap_or(false)
                || Path::new(&expanded_path).is_dir()
            {
                return Err(Error::new(format!(
                    "Notmuch `library_file_path` setting value `{lib_path}` for account {} does \
                     not exist or is a directory.",
                    s.name
                ))
                .set_kind(ErrorKind::Configuration));
            }
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
    fn capabilities(&self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            is_async: false,
            is_remote: false,
            supports_search: true,
            extensions: None,
            supports_tags: true,
            supports_submission: false,
            extra_submission_headers: &[],
            metadata: None,
        };
        CAPABILITIES
    }

    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        struct FetchState {
            mailbox_hash: MailboxHash,
            database: Arc<DbConnection>,
            index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
            mailbox_index: Arc<RwLock<HashMap<EnvelopeHash, SmallVec<[MailboxHash; 16]>>>>,
            mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
            tag_index: Arc<RwLock<BTreeMap<TagHash, String>>>,
            iter: std::vec::IntoIter<CString>,
        }
        impl FetchState {
            async fn fetch(&mut self) -> Result<Option<Vec<Envelope>>> {
                let mut unseen_count = 0;
                let chunk_size = 250;
                let mut mailbox_index_lck = self.mailbox_index.write().unwrap();
                let mut ret: Vec<Envelope> = Vec::with_capacity(chunk_size);
                let mut done: bool = false;
                for _ in 0..chunk_size {
                    if let Some(message_id) = self.iter.next() {
                        let message =
                            if let Ok(v) = Message::find_message(&self.database, &message_id) {
                                v
                            } else {
                                continue;
                            };
                        let env = message.into_envelope(&self.index, &self.tag_index);
                        mailbox_index_lck
                            .entry(env.hash())
                            .or_default()
                            .push(self.mailbox_hash);
                        if !env.is_seen() {
                            unseen_count += 1;
                        }
                        ret.push(env);
                    } else {
                        done = true;
                        break;
                    }
                }
                {
                    let mailboxes_lck = self.mailboxes.read().unwrap();
                    let mailbox = mailboxes_lck.get(&self.mailbox_hash).unwrap();
                    let mut unseen_lck = mailbox.unseen.lock().unwrap();
                    *unseen_lck += unseen_count;
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
        let index = self.index.clone();
        let mailbox_index = self.mailbox_index.clone();
        let tag_index = self.collection.tag_index.clone();
        let mailboxes = self.mailboxes.clone();
        let v: Vec<CString>;
        {
            let mailboxes_lck = mailboxes.read().unwrap();
            let mailbox = mailboxes_lck.get(&mailbox_hash).unwrap();
            let query: Query = Query::new(&database, mailbox.query_str.as_str())?;
            {
                let mut total_lck = mailbox.total.lock().unwrap();
                let mut unseen_lck = mailbox.unseen.lock().unwrap();
                *total_lck = query.count()? as usize;
                *unseen_lck = 0;
            }
            let mut index_lck = index.write().unwrap();
            v = query
                .search()?
                .map(|m| {
                    index_lck.insert(m.env_hash(), m.msg_id_cstr().into());
                    m.msg_id_cstr().into()
                })
                .collect();
        }

        let mut state = FetchState {
            mailbox_hash,
            mailboxes,
            database,
            index,
            mailbox_index,
            tag_index,
            iter: v.into_iter(),
        };
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            while let Some(res) = state.fetch().await.map_err(|err| {
                log::debug!("fetch err {:?}", &err);
                err
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

    fn watch(&self) -> ResultStream<BackendEvent> {
        let account_hash = self.account_hash;
        let snapshot = self.snapshot.clone();
        let path = self.path.clone();
        let lib = self.lib.clone();
        let mailboxes = self.mailboxes.clone();

        let (mut tx, mut rx) = mpsc::channel(16);
        let watcher = RecommendedWatcher::new(
            move |res| {
                futures::executor::block_on(async {
                    _ = tx.send(res).await;
                })
            },
            notify::Config::default().with_poll_interval(std::time::Duration::from_secs(2)),
        )
        .and_then(|mut watcher| {
            watcher.watch(&self.path, RecursiveMode::Recursive)?;
            Ok(watcher)
        })
        .map_err(|err| err.set_err_details("Failed to create file change monitor."))?;
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            // Move watcher to prevent it being Dropped.
            let _watcher = watcher;
            // Set to non-zero value whenever a filesystem event is received, and poll more
            // frequently as long as it is non-zero. This allows us to be more sensitive
            // about updates whenever notmuch-new is more likely to have been
            // called.
            let mut fs_event_counter: u32 = 10;
            loop {
                let sleep_fut = crate::utils::futures::sleep(if fs_event_counter > 0 {
                    std::time::Duration::from_secs(2)
                } else {
                    std::time::Duration::from_secs(30)
                });
                //sleep_fut.await;
                match futures::future::select(rx.next(), std::pin::pin!(sleep_fut)).await {
                    futures::future::Either::Left((None, _)) => {
                        break;
                    }
                    futures::future::Either::Left((Some(ev), _)) => {
                        fs_event_counter = 10;
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

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let ret = Ok(self
            .mailboxes
            .read()
            .unwrap()
            .iter()
            .map(|(k, f)| (*k, BackendMailbox::clone(f)))
            .collect());
        Ok(Box::pin(async { ret }))
    }

    fn envelope_bytes_by_hash(&self, hash: EnvelopeHash) -> ResultFuture<Vec<u8>> {
        let op = NotmuchOp {
            database: Arc::new(DbConnection::new(
                self.path.as_path(),
                self.lib.clone(),
                true,
            )?),
            lib: self.lib.clone(),
            hash,
            index: self.index.clone(),
        };

        Ok(Box::pin(async move { op.as_bytes().await }))
    }

    fn save(
        &self,
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
        mailbox_hash: MailboxHash,
        flags: Vec<FlagOp>,
    ) -> ResultFuture<()> {
        let database = DbConnection::new(self.path.as_path(), self.lib.clone(), true)?;
        let tag_index = self.collection.clone().tag_index;
        let mailboxes = self.mailboxes.clone();
        let mailbox_index = self.mailbox_index.clone();
        let index = self.index.clone();

        Ok(Box::pin(async move {
            let mut index_lck = index.write().unwrap();
            let mut has_seen_changes_mailboxes_set = BTreeSet::new();
            for env_hash in env_hashes.iter() {
                let message = match Message::find_message(&database, &index_lck[&env_hash]) {
                    Ok(v) => v,
                    Err(err) => {
                        log::debug!("not found {err}");
                        continue;
                    }
                };
                message.freeze();

                let tags = message.tags().collect::<Vec<&CStr>>();

                macro_rules! cstr {
                    ($l:literal) => {
                        &CStr::from_bytes_with_nul_unchecked($l)
                    };
                }
                macro_rules! add_tag {
                    ($l:literal) => {{
                        add_tag!(unsafe { cstr!($l) })
                    }};
                    ($l:expr) => {{
                        let l = $l;
                        if tags.contains(l) {
                            continue;
                        }
                        message.add_tag(l)?;
                    }};
                }
                macro_rules! remove_tag {
                    ($l:literal) => {{
                        remove_tag!(unsafe { cstr!($l) })
                    }};
                    ($l:expr) => {{
                        let l = $l;
                        if !tags.contains(l) {
                            continue;
                        }
                        message.remove_tag(l)?;
                    }};
                }

                let mut has_seen_changes = false;
                for op in flags.iter() {
                    has_seen_changes |=
                        matches!(op, FlagOp::Set(Flag::SEEN) | FlagOp::UnSet(Flag::SEEN));
                    match op {
                        FlagOp::Set(Flag::DRAFT) => add_tag!(b"draft\0"),
                        FlagOp::UnSet(Flag::DRAFT) => remove_tag!(b"draft\0"),
                        FlagOp::Set(Flag::FLAGGED) => add_tag!(b"flagged\0"),
                        FlagOp::UnSet(Flag::FLAGGED) => remove_tag!(b"flagged\0"),
                        FlagOp::Set(Flag::PASSED) => add_tag!(b"passed\0"),
                        FlagOp::UnSet(Flag::PASSED) => remove_tag!(b"passed\0"),
                        FlagOp::Set(Flag::REPLIED) => add_tag!(b"replied\0"),
                        FlagOp::UnSet(Flag::REPLIED) => remove_tag!(b"replied\0"),
                        FlagOp::Set(Flag::SEEN) => remove_tag!(b"unread\0"),
                        FlagOp::UnSet(Flag::SEEN) => add_tag!(b"unread\0"),
                        FlagOp::Set(Flag::TRASHED) => add_tag!(b"trashed\0"),
                        FlagOp::UnSet(Flag::TRASHED) => remove_tag!(b"trashed\0"),
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
                if let Some(p) = index_lck.get_mut(&env_hash) {
                    *p = msg_id.into();
                }

                if has_seen_changes {
                    let mailbox_index_lck = mailbox_index.read().unwrap();
                    has_seen_changes_mailboxes_set.insert(mailbox_hash);
                    has_seen_changes_mailboxes_set
                        .extend(mailbox_index_lck.values().flat_map(|hs| hs.iter().cloned()));
                }
            }
            for op in flags.iter() {
                if let FlagOp::SetTag(tag) = op {
                    let hash = TagHash::from_bytes(tag.as_bytes());
                    tag_index.write().unwrap().insert(hash, tag.to_string());
                }
            }
            if !has_seen_changes_mailboxes_set.is_empty() {
                let mailboxes_lck = mailboxes.write().unwrap();
                for mh in has_seen_changes_mailboxes_set {
                    mailboxes_lck[&mh].update_counts(&database)?;
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
        &self,
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
            melib_query.query_to_string(&mut query_s)?;
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
    index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    database: Arc<DbConnection>,
    #[allow(dead_code)]
    lib: Arc<NotmuchLibrary>,
}

impl NotmuchOp {
    async fn as_bytes(&self) -> Result<Vec<u8>> {
        let _self = self.clone();
        smol::unblock(move || {
            let index_lck = _self.index.write().unwrap();
            let message = Message::find_message(&_self.database, &index_lck[&_self.hash])?;
            let mut f = std::fs::File::open(message.get_filename())?;
            let mut response = Vec::new();
            f.read_to_end(&mut response)?;
            Ok(response)
        })
        .await
    }
}
