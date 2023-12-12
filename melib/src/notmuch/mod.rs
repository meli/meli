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
    collections::{hash_map::HashMap, BTreeMap, BTreeSet},
    ffi::{CStr, CString, OsStr},
    io::Read,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    pin::Pin,
    ptr::NonNull,
    sync::{Arc, Mutex, RwLock},
};

use futures::Stream;
use smallvec::SmallVec;

use crate::{
    backends::*,
    conf::AccountSettings,
    email::{Envelope, EnvelopeHash, Flag},
    error::{Error, Result},
    utils::shellexpand::ShellExpandTrait,
    Collection, ErrorKind,
};

macro_rules! call {
    ($lib:expr, $func:ty) => {{
        #[cfg(debug_assertions)]
        debug_assert!(
            !stringify!($func).starts_with("ffi::"),
            "{} must be a valid FFI symbol.",
            stringify!($func)
        );
        let func: libloading::Symbol<$func> = $lib.get(stringify!($func).as_bytes()).unwrap();
        func
    }};
}

macro_rules! try_call {
    ($lib:expr, $call:expr) => {{
        let status = $call;
        if status == $crate::notmuch::ffi::_notmuch_status_NOTMUCH_STATUS_SUCCESS {
            Ok(())
        } else {
            let c_str = call!($lib, notmuch_status_to_string)(status);
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
use ffi::{
    notmuch_database_close, notmuch_database_destroy, notmuch_database_get_revision,
    notmuch_database_open, notmuch_status_to_string,
};
mod message;
pub use message::*;
mod tags;
pub use tags::*;
mod thread;
pub use thread::*;

#[derive(Debug)]
#[repr(transparent)]
struct DbPointer(NonNull<ffi::notmuch_database_t>);

unsafe impl Send for DbPointer {}
unsafe impl Sync for DbPointer {}

impl DbPointer {
    #[inline]
    pub(self) fn as_mut(&mut self) -> *mut ffi::notmuch_database_t {
        unsafe { self.0.as_mut() }
    }
}

#[derive(Debug)]
pub struct DbConnection {
    pub lib: Arc<libloading::Library>,
    inner: Arc<Mutex<DbPointer>>,
    pub revision_uuid: Arc<RwLock<u64>>,
}

impl DbConnection {
    pub fn get_revision_uuid(&self) -> u64 {
        unsafe {
            call!(self.lib, notmuch_database_get_revision)(
                self.inner.lock().unwrap().as_mut(),
                std::ptr::null_mut(),
            )
        }
    }

    #[allow(clippy::too_many_arguments)] // Don't judge me clippy.
    fn refresh(
        &mut self,
        mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
        index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
        mailbox_index: Arc<RwLock<HashMap<EnvelopeHash, SmallVec<[MailboxHash; 16]>>>>,
        tag_index: Arc<RwLock<BTreeMap<TagHash, String>>>,
        account_hash: AccountHash,
        event_consumer: BackendEventConsumer,
        new_revision_uuid: u64,
    ) -> Result<()> {
        use RefreshEventKind::*;
        let query_str = format!(
            "lastmod:{}..{}",
            *self.revision_uuid.read().unwrap(),
            new_revision_uuid
        );
        let query: Query = Query::new(self, &query_str)?;
        let iter = query.search()?;
        let mailbox_index_lck = mailbox_index.write().unwrap();
        let mailboxes_lck = mailboxes.read().unwrap();
        for message in iter {
            let env_hash = message.env_hash();
            if let Some(mailbox_hashes) = mailbox_index_lck.get(&env_hash) {
                let tags: (Flag, Vec<String>) = message.tags().collect_flags_and_tags();
                let mut tag_lock = tag_index.write().unwrap();
                for tag in tags.1.iter() {
                    let num = TagHash::from_bytes(tag.as_bytes());
                    tag_lock.entry(num).or_insert_with(|| tag.clone());
                }
                for &mailbox_hash in mailbox_hashes {
                    (event_consumer)(
                        account_hash,
                        BackendEvent::Refresh(RefreshEvent {
                            account_hash,
                            mailbox_hash,
                            kind: NewFlags(env_hash, tags.clone()),
                        }),
                    );
                }
            } else {
                let message_id = message.msg_id_cstr().to_string_lossy().to_string();
                let env = message.into_envelope(&index, &tag_index);
                for (&mailbox_hash, m) in mailboxes_lck.iter() {
                    let query_str = format!("{} id:{}", m.query_str.as_str(), &message_id);
                    let query: Query = Query::new(self, &query_str)?;
                    if query.count().unwrap_or(0) > 0 {
                        let mut total_lck = m.total.lock().unwrap();
                        let mut unseen_lck = m.unseen.lock().unwrap();
                        *total_lck += 1;
                        if !env.is_seen() {
                            *unseen_lck += 1;
                        }
                        (event_consumer)(
                            account_hash,
                            BackendEvent::Refresh(RefreshEvent {
                                account_hash,
                                mailbox_hash,
                                kind: Create(Box::new(env.clone())),
                            }),
                        );
                    }
                }
            }
        }
        drop(query);
        index.write().unwrap().retain(|&env_hash, msg_id| {
            if Message::find_message(self, msg_id).is_err() {
                if let Some(mailbox_hashes) = mailbox_index_lck.get(&env_hash) {
                    for &mailbox_hash in mailbox_hashes {
                        let m = &mailboxes_lck[&mailbox_hash];
                        let mut total_lck = m.total.lock().unwrap();
                        *total_lck = total_lck.saturating_sub(1);
                        (event_consumer)(
                            account_hash,
                            BackendEvent::Refresh(RefreshEvent {
                                account_hash,
                                mailbox_hash,
                                kind: Remove(env_hash),
                            }),
                        );
                    }
                }
                false
            } else {
                true
            }
        });
        Ok(())
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

impl Drop for DbConnection {
    fn drop(&mut self) {
        let mut inner = self.inner.lock().unwrap();
        unsafe {
            if let Err(err) = try_call!(
                self.lib,
                call!(self.lib, notmuch_database_close)(inner.as_mut())
            ) {
                log::error!("Could not call C notmuch_database_close: {}", err);
                return;
            }
            if let Err(err) = try_call!(
                self.lib,
                call!(self.lib, notmuch_database_destroy)(inner.as_mut())
            ) {
                log::error!("Could not call C notmuch_database_destroy: {}", err);
            }
        }
    }
}

#[derive(Debug)]
pub struct NotmuchDb {
    #[allow(dead_code)]
    lib: Arc<libloading::Library>,
    revision_uuid: Arc<RwLock<u64>>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
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
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        #[cfg(target_os = "linux")]
        let mut dlpath = "libnotmuch.so.5";
        #[cfg(target_os = "macos")]
        let mut dlpath = "libnotmuch.5.dylib";
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        let mut dlpath = "libnotmuch.so";
        let mut custom_dlpath = false;
        if let Some(lib_path) = s.extra.get("library_file_path") {
            dlpath = lib_path.as_str();
            custom_dlpath = true;
        }
        let lib = Arc::new(unsafe {
            match libloading::Library::new(dlpath) {
                Ok(l) => l,
                Err(err) => {
                    if custom_dlpath {
                        return Err(Error::new(format!(
                            "Notmuch `library_file_path` setting value `{}` for account {} does \
                             not exist or is a directory or not a valid library file.",
                            dlpath, s.name
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
        });
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.exists() {
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
        if !path.exists() || !path.is_dir() {
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
                    "notmuch mailbox configuration entry `{}` for account {} should have a \
                     `query` value set.",
                    k, s.name,
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
                    "Mailbox configuration for `{}` defines its parent mailbox as `{}` but no \
                     mailbox exists with this exact name.",
                    mailboxes[&hash].name(),
                    parent
                ))
                .set_kind(ErrorKind::Configuration));
            }
        }

        let account_hash = AccountHash::from_bytes(s.name.as_bytes());
        Ok(Box::new(Self {
            lib,
            revision_uuid: Arc::new(RwLock::new(0)),
            path,
            index: Arc::new(RwLock::new(Default::default())),
            mailbox_index: Arc::new(RwLock::new(Default::default())),
            collection: Collection::default(),

            mailboxes: Arc::new(RwLock::new(mailboxes)),
            save_messages_to: None,
            _account_name: s.name.to_string().into(),
            account_hash,
            event_consumer,
        }))
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.exists() {
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
        if !path.exists() || !path.is_dir() {
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
        if let Some(lib_path) = s.extra.remove("library_file_path") {
            if !Path::new(&lib_path).exists() || Path::new(&lib_path).is_dir() {
                return Err(Error::new(format!(
                    "Notmuch `library_file_path` setting value `{}` for account {} does not exist \
                     or is a directory.",
                    &lib_path, s.name
                ))
                .set_kind(ErrorKind::Configuration));
            }
        }
        let mut parents: Vec<(String, String)> = Vec::with_capacity(s.mailboxes.len());
        for (k, f) in s.mailboxes.iter_mut() {
            if f.extra.remove("query").is_none() {
                return Err(Error::new(format!(
                    "notmuch mailbox configuration entry `{}` for account {} should have a \
                     `query` value set.",
                    k, account_name,
                ))
                .set_kind(ErrorKind::Configuration));
            }
            if let Some(parent) = f.extra.remove("parent") {
                parents.push((k.clone(), parent));
            }
        }
        let mut path = Vec::with_capacity(8);
        for (mbox, parent) in parents.iter() {
            if !s.mailboxes.contains_key(parent) {
                return Err(Error::new(format!(
                    "Mailbox configuration for `{}` defines its parent mailbox as `{}` but no \
                     mailbox exists with this exact name.",
                    mbox, parent
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

    fn new_connection(
        path: &Path,
        revision_uuid: Arc<RwLock<u64>>,
        lib: Arc<libloading::Library>,
        write: bool,
    ) -> Result<DbConnection> {
        let path_c = CString::new(path.to_str().unwrap()).unwrap();
        let path_ptr = path_c.as_ptr();
        let mut database: *mut ffi::notmuch_database_t = std::ptr::null_mut();
        let status = unsafe {
            call!(lib, notmuch_database_open)(
                path_ptr,
                if write {
                    ffi::notmuch_database_mode_t_NOTMUCH_DATABASE_MODE_READ_WRITE
                } else {
                    ffi::notmuch_database_mode_t_NOTMUCH_DATABASE_MODE_READ_ONLY
                },
                std::ptr::addr_of_mut!(database),
            )
        };
        if status != 0 {
            return Err(Error::new(format!(
                "Could not open notmuch database at path {}. notmuch_database_open returned {}.",
                path.display(),
                status
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
                .set_kind(ErrorKind::External)
                .set_details(
                    "libnotmuch exhibited an unexpected and unrecoverable error. Make sure your \
                     libnotmuch version is compatible with this release.",
                )
        })?;
        let ret = DbConnection {
            lib,
            revision_uuid,
            inner: Arc::new(Mutex::new(database)),
        };
        if *ret.revision_uuid.read().unwrap() == 0 {
            let new = ret.get_revision_uuid();
            *ret.revision_uuid.write().unwrap() = new;
        }
        Ok(ret)
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
        };
        CAPABILITIES
    }

    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
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
        let database = Arc::new(Self::new_connection(
            self.path.as_path(),
            self.revision_uuid.clone(),
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
        Ok(Box::pin(async_stream::try_stream! {
            while let Some(res) = state.fetch().await.map_err(|err| { log::debug!("fetch err {:?}", &err); err})? {
                yield res;
            }
        }))
    }

    fn refresh(&mut self, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let account_hash = self.account_hash;
        let mut database = Self::new_connection(
            self.path.as_path(),
            self.revision_uuid.clone(),
            self.lib.clone(),
            false,
        )?;
        let mailboxes = self.mailboxes.clone();
        let index = self.index.clone();
        let mailbox_index = self.mailbox_index.clone();
        let tag_index = self.collection.tag_index.clone();
        let event_consumer = self.event_consumer.clone();
        Ok(Box::pin(async move {
            let new_revision_uuid = database.get_revision_uuid();
            if new_revision_uuid > *database.revision_uuid.read().unwrap() {
                database.refresh(
                    mailboxes,
                    index,
                    mailbox_index,
                    tag_index,
                    account_hash,
                    event_consumer,
                    new_revision_uuid,
                )?;
                *database.revision_uuid.write().unwrap() = new_revision_uuid;
            }
            Ok(())
        }))
    }

    fn watch(&self) -> ResultFuture<()> {
        extern crate notify;
        use notify::{watcher, RecursiveMode, Watcher};

        let account_hash = self.account_hash;
        let tag_index = self.collection.tag_index.clone();
        let lib = self.lib.clone();
        let path = self.path.clone();
        let revision_uuid = self.revision_uuid.clone();
        let mailboxes = self.mailboxes.clone();
        let index = self.index.clone();
        let mailbox_index = self.mailbox_index.clone();
        let event_consumer = self.event_consumer.clone();

        let (tx, rx) = std::sync::mpsc::channel();
        let mut watcher = watcher(tx, std::time::Duration::from_secs(2)).unwrap();
        watcher.watch(&self.path, RecursiveMode::Recursive).unwrap();
        Ok(Box::pin(async move {
            let _watcher = watcher;
            let rx = rx;
            loop {
                let _ = rx.recv().map_err(|err| err.to_string())?;
                {
                    let mut database = Self::new_connection(
                        path.as_path(),
                        revision_uuid.clone(),
                        lib.clone(),
                        false,
                    )?;
                    let new_revision_uuid = database.get_revision_uuid();
                    if new_revision_uuid > *database.revision_uuid.read().unwrap() {
                        database.refresh(
                            mailboxes.clone(),
                            index.clone(),
                            mailbox_index.clone(),
                            tag_index.clone(),
                            account_hash,
                            event_consumer.clone(),
                            new_revision_uuid,
                        )?;
                        *revision_uuid.write().unwrap() = new_revision_uuid;
                    }
                }
            }
        }))
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

    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        Ok(Box::new(NotmuchOp {
            database: Arc::new(Self::new_connection(
                self.path.as_path(),
                self.revision_uuid.clone(),
                self.lib.clone(),
                true,
            )?),
            lib: self.lib.clone(),
            hash,
            index: self.index.clone(),
            bytes: None,
        }))
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
        Err(Error::new(
            "Copying messages is currently unimplemented for notmuch backend",
        ))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()> {
        let database = Self::new_connection(
            self.path.as_path(),
            self.revision_uuid.clone(),
            self.lib.clone(),
            true,
        )?;
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
                        log::debug!("not found {}", err);
                        continue;
                    }
                };

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
        Err(Error::new(
            "Deleting messages is currently unimplemented for notmuch backend",
        ))
    }

    fn search(
        &self,
        melib_query: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        let database = Self::new_connection(
            self.path.as_path(),
            self.revision_uuid.clone(),
            self.lib.clone(),
            false,
        )?;
        let mailboxes = self.mailboxes.clone();
        Ok(Box::pin(async move {
            let mut ret = SmallVec::new();
            let mut query_s = if let Some(mailbox_hash) = mailbox_hash {
                if let Some(m) = mailboxes.read().unwrap().get(&mailbox_hash) {
                    let mut s = m.query_str.clone();
                    s.push(' ');
                    s
                } else {
                    return Err(Error::new(format!(
                        "Mailbox with hash {} not found!",
                        mailbox_hash
                    ))
                    .set_kind(crate::error::ErrorKind::Bug));
                }
            } else {
                String::new()
            };
            melib_query.query_to_string(&mut query_s);
            let query: Query = Query::new(&database, &query_s)?;
            let iter = query.search()?;
            for message in iter {
                ret.push(message.env_hash());
            }

            Ok(ret)
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
        Err(Error::new(
            "Deleting mailboxes is currently unimplemented for notmuch backend.",
        ))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Mailbox subscriptions are not possible for the notmuch backend.",
        ))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(Error::new(
            "Renaming mailboxes is currently unimplemented for notmuch backend.",
        ))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox permissions is not possible for the notmuch backend.",
        ))
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

#[derive(Debug)]
struct NotmuchOp {
    hash: EnvelopeHash,
    index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    database: Arc<DbConnection>,
    bytes: Option<Vec<u8>>,
    #[allow(dead_code)]
    lib: Arc<libloading::Library>,
}

impl BackendOp for NotmuchOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        let index_lck = self.index.write().unwrap();
        let message = Message::find_message(&self.database, &index_lck[&self.hash])?;
        let mut f = std::fs::File::open(message.get_filename())?;
        let mut response = Vec::new();
        f.read_to_end(&mut response)?;
        self.bytes = Some(response);
        let ret = Ok(self.bytes.as_ref().unwrap().to_vec());
        Ok(Box::pin(async move { ret }))
    }
}
