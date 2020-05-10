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

use crate::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use crate::backends::MailboxHash;
use crate::backends::{
    BackendMailbox, BackendOp, MailBackend, Mailbox, MailboxPermissions, RefreshEventConsumer,
    SpecialUsageMailbox,
};
use crate::conf::AccountSettings;
use crate::email::{Envelope, EnvelopeHash, Flag};
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;
use smallvec::SmallVec;
use std::collections::{
    hash_map::{DefaultHasher, HashMap},
    BTreeMap,
};
use std::error::Error;
use std::ffi::{CStr, CString, OsStr};
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

pub mod bindings;
use bindings::*;

#[derive(Debug, Clone)]
struct DbConnection {
    lib: Arc<libloading::Library>,
    inner: Arc<RwLock<*mut notmuch_database_t>>,
    database_ph: std::marker::PhantomData<&'static mut notmuch_database_t>,
}

unsafe impl Send for DbConnection {}
unsafe impl Sync for DbConnection {}

macro_rules! call {
    ($lib:expr, $func:ty) => {{
        let func: libloading::Symbol<$func> = $lib.get(stringify!($func).as_bytes()).unwrap();
        func
    }};
}

#[derive(Debug)]
pub struct NotmuchError(String);

impl std::fmt::Display for NotmuchError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl Error for NotmuchError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

macro_rules! try_call {
    ($lib:expr, $call:expr) => {{
        let status = unsafe { $call };
        if status == _notmuch_status_NOTMUCH_STATUS_SUCCESS {
            Ok(())
        } else {
            let c_str = unsafe { call!($lib, notmuch_status_to_string)(status) };
            Err(NotmuchError(unsafe {
                CStr::from_ptr(c_str).to_string_lossy().into_owned()
            }))
        }
    }};
}

impl Drop for DbConnection {
    fn drop(&mut self) {
        let inner = self.inner.write().unwrap();
        unsafe {
            call!(self.lib, notmuch_database_close)(*inner);
            call!(self.lib, notmuch_database_destroy)(*inner);
        }
    }
}

#[derive(Debug)]
pub struct NotmuchDb {
    lib: Arc<libloading::Library>,
    revision_uuid: Arc<RwLock<u64>>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
    index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    tag_index: Arc<RwLock<BTreeMap<u64, String>>>,
    path: PathBuf,
    save_messages_to: Option<PathBuf>,
}

unsafe impl Send for NotmuchDb {}
unsafe impl Sync for NotmuchDb {}

#[derive(Debug, Clone, Default)]
struct NotmuchMailbox {
    hash: MailboxHash,
    children: Vec<MailboxHash>,
    parent: Option<MailboxHash>,
    name: String,
    path: String,
    query_str: String,
    usage: Arc<RwLock<SpecialUsageMailbox>>,

    total: Arc<Mutex<usize>>,
    unseen: Arc<Mutex<usize>>,
}

impl BackendMailbox for NotmuchMailbox {
    fn hash(&self) -> MailboxHash {
        self.hash
    }

    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn path(&self) -> &str {
        self.path.as_str()
    }

    fn change_name(&mut self, _s: &str) {}

    fn clone(&self) -> Mailbox {
        Box::new(std::clone::Clone::clone(self))
    }

    fn children(&self) -> &[MailboxHash] {
        &self.children
    }

    fn parent(&self) -> Option<MailboxHash> {
        self.parent
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        *self.usage.read().unwrap()
    }

    fn permissions(&self) -> MailboxPermissions {
        MailboxPermissions::default()
    }

    fn is_subscribed(&self) -> bool {
        true
    }

    fn set_is_subscribed(&mut self, _new_val: bool) -> Result<()> {
        Ok(())
    }

    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()> {
        *self.usage.write()? = new_val;
        Ok(())
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((*self.unseen.lock()?, *self.total.lock()?))
    }
}

unsafe impl Send for NotmuchMailbox {}
unsafe impl Sync for NotmuchMailbox {}

impl NotmuchDb {
    pub fn new(
        s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
    ) -> Result<Box<dyn MailBackend>> {
        let lib = Arc::new(libloading::Library::new("libnotmuch.so.5")?);
        let path = Path::new(s.root_mailbox.as_str()).expand().to_path_buf();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "\"root_mailbox\" {} for account {} is not a valid path.",
                s.root_mailbox.as_str(),
                s.name()
            )));
        }

        let mut mailboxes = HashMap::default();
        for (k, f) in s.mailboxes.iter() {
            if let Some(query_str) = f.extra.get("query") {
                let hash = {
                    let mut h = DefaultHasher::new();
                    k.hash(&mut h);
                    h.finish()
                };
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
                return Err(MeliError::new(format!(
                    "notmuch mailbox configuration entry \"{}\" should have a \"query\" value set.",
                    k
                )));
            }
        }
        Ok(Box::new(NotmuchDb {
            lib,
            revision_uuid: Arc::new(RwLock::new(0)),
            path,
            index: Arc::new(RwLock::new(Default::default())),
            tag_index: Arc::new(RwLock::new(Default::default())),

            mailboxes: Arc::new(RwLock::new(mailboxes)),
            save_messages_to: None,
        }))
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        let path = Path::new(s.root_mailbox.as_str()).expand().to_path_buf();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "\"root_mailbox\" {} for account {} is not a valid path.",
                s.root_mailbox.as_str(),
                s.name()
            )));
        }
        for (k, f) in s.mailboxes.iter() {
            if f.extra.get("query").is_none() {
                return Err(MeliError::new(format!(
                    "notmuch mailbox configuration entry \"{}\" should have a \"query\" value set.",
                    k
                )));
            }
        }
        Ok(())
    }

    pub fn search(&self, query_s: &str) -> Result<SmallVec<[EnvelopeHash; 512]>> {
        let database = Self::new_connection(self.path.as_path(), self.lib.clone(), false)?;
        let database_lck = database.inner.read().unwrap();
        let query: Query = Query::new(self.lib.clone(), &database_lck, query_s)?;
        let mut ret = SmallVec::new();
        let iter = query.search()?;
        for message in iter {
            let msg_id = unsafe { call!(self.lib, notmuch_message_get_message_id)(message) };
            let c_str = unsafe { CStr::from_ptr(msg_id) };
            let env_hash = {
                let mut hasher = DefaultHasher::default();
                c_str.hash(&mut hasher);
                hasher.finish()
            };
            ret.push(env_hash);
        }

        Ok(ret)
    }

    fn new_connection(
        path: &Path,
        lib: Arc<libloading::Library>,
        write: bool,
    ) -> Result<DbConnection> {
        let path_c = std::ffi::CString::new(path.to_str().unwrap()).unwrap();
        let path_ptr = path_c.as_ptr();
        let mut database: *mut notmuch_database_t = std::ptr::null_mut();
        let status = unsafe {
            call!(lib, notmuch_database_open)(
                path_ptr,
                if write {
                    notmuch_database_mode_t_NOTMUCH_DATABASE_MODE_READ_WRITE
                } else {
                    notmuch_database_mode_t_NOTMUCH_DATABASE_MODE_READ_ONLY
                },
                &mut database as *mut _,
            )
        };
        if status != 0 {
            return Err(MeliError::new(format!(
                "Could not open notmuch database at path {}. notmuch_database_open returned {}.",
                path.display(),
                status
            )));
        }
        assert!(!database.is_null());
        Ok(DbConnection {
            lib,
            inner: Arc::new(RwLock::new(database)),
            database_ph: std::marker::PhantomData,
        })
    }
}

impl MailBackend for NotmuchDb {
    fn is_online(&self) -> Result<()> {
        Ok(())
    }
    fn get(&mut self, mailbox: &Mailbox) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let mailbox_hash = mailbox.hash();
        let database = NotmuchDb::new_connection(self.path.as_path(), self.lib.clone(), false);
        let index = self.index.clone();
        let tag_index = self.tag_index.clone();
        let mailboxes = self.mailboxes.clone();
        let lib = self.lib.clone();
        let handle = {
            let tx = w.tx();
            let closure = move |_work_context| {
                if let Err(err) = database {
                    tx.send(AsyncStatus::Payload(Err(err))).unwrap();
                    tx.send(AsyncStatus::Finished).unwrap();
                    return;
                }
                let database = Arc::new(database.unwrap());
                let mut ret: Vec<Envelope> = Vec::new();
                let database_lck = database.inner.read().unwrap();
                let mut mailboxes_lck = mailboxes.write().unwrap();
                let mailbox = mailboxes_lck.get_mut(&mailbox_hash).unwrap();
                let query: Query =
                    match Query::new(lib.clone(), &database_lck, mailbox.query_str.as_str()) {
                        Ok(q) => q,
                        Err(err) => {
                            tx.send(AsyncStatus::Payload(Err(err))).unwrap();
                            tx.send(AsyncStatus::Finished).unwrap();
                            return;
                        }
                    };
                let iter = match query.search() {
                    Ok(i) => i,
                    Err(err) => {
                        tx.send(AsyncStatus::Payload(Err(err))).unwrap();
                        tx.send(AsyncStatus::Finished).unwrap();
                        return;
                    }
                };
                for message in iter {
                    let mut response = String::new();
                    let fs_path = unsafe { call!(lib, notmuch_message_get_filename)(message) };
                    let mut f = match std::fs::File::open(unsafe {
                        CStr::from_ptr(fs_path)
                            .to_string_lossy()
                            .into_owned()
                            .as_str()
                    }) {
                        Ok(f) => f,
                        Err(e) => {
                            debug!("could not open fs_path {:?} {}", fs_path, e);
                            continue;
                        }
                    };
                    response.clear();
                    if let Err(e) = f.read_to_string(&mut response) {
                        debug!("could not read fs_path {:?} {}", fs_path, e);
                        continue;
                    }
                    let env_hash = {
                        let msg_id = unsafe { call!(lib, notmuch_message_get_message_id)(message) };
                    let c_str = unsafe { CStr::from_ptr(msg_id) };
                        let mut hasher = DefaultHasher::default();
                        c_str.hash(&mut hasher);
                        hasher.finish()
                    };
                    let c_str = unsafe { CStr::from_ptr(fs_path) };
                    index.write().unwrap().insert(env_hash, c_str.into());
                    let op = Box::new(NotmuchOp {
                        database: database.clone(),
                        lib: lib.clone(),
                        hash: env_hash,
                        index: index.clone(),
                        bytes: Some(response),
                        tag_index: tag_index.clone(),
                    });
                    if let Some(mut env) = Envelope::from_token(op, env_hash) {
                        let mut tag_lock = tag_index.write().unwrap();
                        for tag in TagIterator::new(lib.clone(), message) {
                            let tag = tag.to_string_lossy().into_owned();

                            let mut hasher = DefaultHasher::new();
                            hasher.write(tag.as_bytes());
                            let num = hasher.finish();
                            if !tag_lock.contains_key(&num) {
                                tag_lock.insert(num, tag);
                            }
                            env.labels_mut().push(num);
                        }
                        ret.push(env);
                    } else {
                        debug!("could not parse path {:?}", c_str);
                        index.write().unwrap().remove(&env_hash);
                    }
                }
                tx.send(AsyncStatus::Payload(Ok(ret))).unwrap();
                tx.send(AsyncStatus::Finished).unwrap();
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn watch(
        &self,
        sender: RefreshEventConsumer,
        _work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        extern crate notify;
        use crate::backends::{RefreshEvent, RefreshEventKind::*};
        use notify::{watcher, RecursiveMode, Watcher};
        let (tx, rx) = std::sync::mpsc::channel();
        let mut watcher = watcher(tx, std::time::Duration::from_secs(2)).unwrap();
        watcher.watch(&self.path, RecursiveMode::Recursive).unwrap();
        let path = self.path.clone();
        let lib = self.lib.clone();
        {
            let database = NotmuchDb::new_connection(path.as_path(), lib.clone(), false)?;
            let mut revision_uuid_lck = self.revision_uuid.write().unwrap();

            *revision_uuid_lck = unsafe {
                call!(lib, notmuch_database_get_revision)(
                    *database.inner.read().unwrap(),
                    std::ptr::null_mut(),
                )
            };
            debug!(*revision_uuid_lck);
        }
        let revision_uuid = self.revision_uuid.clone();

        let handle = std::thread::Builder::new()
            .name(format!(
                "watching {}",
                self.path.file_name().unwrap().to_str().unwrap()
            ))
            .spawn(move || {
                let _watcher = watcher;
                let c = move || -> std::result::Result<(), MeliError> {
                    loop {
                        let _ = rx.recv().map_err(|err| err.to_string())?;
                        let database =
                            NotmuchDb::new_connection(path.as_path(), lib.clone(), false)?;
                        let database_lck = database.inner.read().unwrap();
                        let mut revision_uuid_lck = revision_uuid.write().unwrap();

                        let new_revision = unsafe {
                            call!(lib, notmuch_database_get_revision)(
                                *database_lck,
                                std::ptr::null_mut(),
                            )
                        };
                        if new_revision > *revision_uuid_lck {
                            debug!(new_revision);
                            debug!(*revision_uuid_lck);
                            let query_str =
                                format!("lastmod:{}..{}", *revision_uuid_lck, new_revision);
                            let query: Query = Query::new(lib.clone(), &database_lck, &query_str)?;
                            let iter = query.search()?;
                            *revision_uuid_lck = new_revision;
                        }
                    }
                };

                if let Err(err) = c() {
                    sender.send(RefreshEvent {
                        hash: 0,
                        kind: Failure(err.into()),
                    });
                }
            })?;
        Ok(handle.thread().id())
    }
    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>> {
        Ok(self
            .mailboxes
            .read()
            .unwrap()
            .iter()
            .map(|(k, f)| (*k, BackendMailbox::clone(f)))
            .collect())
    }
    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        Box::new(NotmuchOp {
            database: Arc::new(
                Self::new_connection(self.path.as_path(), self.lib.clone(), true).unwrap(),
            ),
            lib: self.lib.clone(),
            hash,
            index: self.index.clone(),
            bytes: None,
            tag_index: self.tag_index.clone(),
        })
    }

    fn save(&self, bytes: &[u8], _mailbox: &str, flags: Option<Flag>) -> Result<()> {
        let path = self
            .save_messages_to
            .as_ref()
            .unwrap_or(&self.path)
            .to_path_buf();
        crate::backends::MaildirType::save_to_mailbox(path, bytes, flags)
    }

    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }

    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        Some(self.tag_index.clone())
    }
}

#[derive(Debug)]
struct NotmuchOp {
    hash: EnvelopeHash,
    index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    tag_index: Arc<RwLock<BTreeMap<u64, String>>>,
    database: Arc<DbConnection>,
    bytes: Option<String>,
    lib: Arc<libloading::Library>,
}

impl BackendOp for NotmuchOp {
    fn description(&self) -> String {
        String::new()
    }

    fn as_bytes(&mut self) -> Result<&[u8]> {
        let path = &self.index.read().unwrap()[&self.hash];
        let mut f = std::fs::File::open(&OsStr::from_bytes(path.to_bytes()))?;
        let mut response = String::new();
        f.read_to_string(&mut response)?;
        self.bytes = Some(response);
        Ok(self.bytes.as_ref().unwrap().as_bytes())
    }

    fn fetch_flags(&self) -> Flag {
        let mut flag = Flag::default();
        let path = &self.index.read().unwrap()[&self.hash];
        for f in path.to_bytes().iter().rev() {
            match f {
                b',' => break,
                b'D' => flag |= Flag::DRAFT,
                b'F' => flag |= Flag::FLAGGED,
                b'P' => flag |= Flag::PASSED,
                b'R' => flag |= Flag::REPLIED,
                b'S' => flag |= Flag::SEEN,
                b'T' => flag |= Flag::TRASHED,
                _ => {
                    debug!(
                        "DEBUG: in fetch_flags, unknown flag '{}' path is {:?}",
                        f, path
                    );
                }
            }
        }

        flag
    }

    fn set_flag(&mut self, _envelope: &mut Envelope, f: Flag, value: bool) -> Result<()> {
        let mut message: *mut notmuch_message_t = std::ptr::null_mut();
        let mut index_lck = self.index.write().unwrap();
        unsafe {
            call!(self.lib, notmuch_database_find_message_by_filename)(
                *self.database.inner.read().unwrap(),
                index_lck[&self.hash].as_ptr(),
                &mut message as *mut _,
            )
        };
        if message.is_null() {
            return Err(MeliError::new(format!(
                "Error, message with path {:?} not found in notmuch database.",
                index_lck[&self.hash]
            )));
        }

        // TODO: freeze/thaw message.
        let tags = TagIterator::new(self.lib.clone(), message).collect::<Vec<&CStr>>();
        debug!(&tags);

        macro_rules! cstr {
            ($l:literal) => {
                CStr::from_bytes_with_nul_unchecked($l)
            };
        }
        macro_rules! add_tag {
            ($l:literal) => {{
                if tags.contains(unsafe { &cstr!($l) }) {
                    return Ok(());
                }
                if let Err(err) = try_call!(
                    self.lib,
                    call!(self.lib, notmuch_message_add_tag)(message, cstr!($l).as_ptr())
                ) {
                    return Err(
                        MeliError::new("Could not set tag.").set_source(Some(Arc::new(err)))
                    );
                }
            }};
        }
        macro_rules! remove_tag {
            ($l:literal) => {{
                if !tags.contains(unsafe { &cstr!($l) }) {
                    return Ok(());
                }
                if let Err(err) = try_call!(
                    self.lib,
                    call!(self.lib, notmuch_message_remove_tag)(message, cstr!($l).as_ptr())
                ) {
                    return Err(
                        MeliError::new("Could not set tag.").set_source(Some(Arc::new(err)))
                    );
                }
            }};
        }

        match f {
            Flag::DRAFT if value => add_tag!(b"draft\0"),
            Flag::DRAFT => remove_tag!(b"draft\0"),
            Flag::FLAGGED if value => add_tag!(b"flagged\0"),
            Flag::FLAGGED => remove_tag!(b"flagged\0"),
            Flag::PASSED if value => add_tag!(b"passed\0"),
            Flag::PASSED => remove_tag!(b"passed\0"),
            Flag::REPLIED if value => add_tag!(b"replied\0"),
            Flag::REPLIED => remove_tag!(b"replied\0"),
            Flag::SEEN if value => remove_tag!(b"unread\0"),
            Flag::SEEN => add_tag!(b"unread\0"),
            Flag::TRASHED if value => add_tag!(b"trashed\0"),
            Flag::TRASHED => remove_tag!(b"trashed\0"),
            _ => debug!("flags is {:?} value = {}", f, value),
        }

        /* Update message filesystem path. */
        if let Err(err) = try_call!(
            self.lib,
            call!(self.lib, notmuch_message_tags_to_maildir_flags)(message)
        ) {
            return Err(MeliError::new("Could not set tag.").set_source(Some(Arc::new(err))));
        }

        let fs_path = unsafe { call!(self.lib, notmuch_message_get_filename)(message) };
        let c_str = unsafe { CStr::from_ptr(fs_path) };
        if let Some(p) = index_lck.get_mut(&self.hash) {
            *p = c_str.into();
        }

        Ok(())
    }

    fn set_tag(&mut self, envelope: &mut Envelope, tag: String, value: bool) -> Result<()> {
        let mut message: *mut notmuch_message_t = std::ptr::null_mut();
        let index_lck = self.index.read().unwrap();
        unsafe {
            call!(self.lib, notmuch_database_find_message_by_filename)(
                *self.database.inner.read().unwrap(),
                index_lck[&self.hash].as_ptr(),
                &mut message as *mut _,
            )
        };
        if message.is_null() {
            return Err(MeliError::new(format!(
                "Error, message with path {:?} not found in notmuch database.",
                index_lck[&self.hash]
            )));
        }
        if value {
            if let Err(err) = try_call!(
                self.lib,
                call!(self.lib, notmuch_message_add_tag)(
                    message,
                    CString::new(tag.as_str()).unwrap().as_ptr(),
                )
            ) {
                return Err(MeliError::new("Could not set tag.").set_source(Some(Arc::new(err))));
            }
            debug!("added tag {}", &tag);
        } else {
            if let Err(err) = try_call!(
                self.lib,
                call!(self.lib, notmuch_message_remove_tag)(
                    message,
                    CString::new(tag.as_str()).unwrap().as_ptr(),
                )
            ) {
                return Err(MeliError::new("Could not set tag.").set_source(Some(Arc::new(err))));
            }
            debug!("removed tag {}", &tag);
        }
        let hash = tag_hash!(tag);
        if value {
            self.tag_index.write().unwrap().insert(hash, tag);
        } else {
            self.tag_index.write().unwrap().remove(&hash);
        }
        if !envelope.labels().iter().any(|&h_| h_ == hash) {
            if value {
                envelope.labels_mut().push(hash);
            }
        }
        if !value {
            if let Some(pos) = envelope.labels().iter().position(|&h_| h_ == hash) {
                envelope.labels_mut().remove(pos);
            }
        }
        Ok(())
    }
}

pub struct MessageIterator {
    lib: Arc<libloading::Library>,
    messages: *mut notmuch_messages_t,
}

impl Iterator for MessageIterator {
    type Item = *mut notmuch_message_t;
    fn next(&mut self) -> Option<Self::Item> {
        if self.messages.is_null() {
            None
        } else if unsafe { call!(self.lib, notmuch_messages_valid)(self.messages) } == 1 {
            let ret = Some(unsafe { call!(self.lib, notmuch_messages_get)(self.messages) });
            unsafe {
                call!(self.lib, notmuch_messages_move_to_next)(self.messages);
            }
            ret
        } else {
            self.messages = std::ptr::null_mut();
            None
        }
    }
}

pub struct TagIterator {
    lib: Arc<libloading::Library>,
    tags: *mut notmuch_tags_t,
}

impl TagIterator {
    fn new(lib: Arc<libloading::Library>, message: *mut notmuch_message_t) -> Self {
        TagIterator {
            tags: unsafe { call!(lib, notmuch_message_get_tags)(message) },
            lib,
        }
    }
}

impl Iterator for TagIterator {
    type Item = &'static CStr;
    fn next(&mut self) -> Option<Self::Item> {
        if self.tags.is_null() {
            None
        } else if unsafe { call!(self.lib, notmuch_tags_valid)(self.tags) } == 1 {
            let ret = Some(unsafe { CStr::from_ptr(call!(self.lib, notmuch_tags_get)(self.tags)) });
            unsafe {
                call!(self.lib, notmuch_tags_move_to_next)(self.tags);
            }
            ret
        } else {
            self.tags = std::ptr::null_mut();
            None
        }
    }
}

pub struct Query<'s> {
    lib: Arc<libloading::Library>,
    ptr: *mut notmuch_query_t,
    query_str: &'s str,
}

impl<'s> Query<'s> {
    fn new(
        lib: Arc<libloading::Library>,
        database: &*mut notmuch_database_t,
        query_str: &'s str,
    ) -> Result<Self> {
        let query_cstr = std::ffi::CString::new(query_str)?;
        let query: *mut notmuch_query_t =
            unsafe { call!(lib, notmuch_query_create)(*database, query_cstr.as_ptr()) };
        if query.is_null() {
            return Err(MeliError::new("Could not create query. Out of memory?"));
        }
        Ok(Query {
            lib,
            ptr: query,
            query_str,
        })
    }

    fn search(&self) -> Result<MessageIterator> {
        let mut messages: *mut notmuch_messages_t = std::ptr::null_mut();
        let status = unsafe {
            call!(self.lib, notmuch_query_search_messages)(self.ptr, &mut messages as *mut _)
        };
        if status != 0 {
            return Err(MeliError::new(format!(
                "Search for {} returned {}",
                self.query_str, status,
            )));
        }
        assert!(!messages.is_null());
        Ok(MessageIterator {
            messages,
            lib: self.lib.clone(),
        })
    }
}

impl Drop for Query<'_> {
    fn drop(&mut self) {
        unsafe {
            call!(self.lib, notmuch_query_destroy)(self.ptr);
        }
    }
}
