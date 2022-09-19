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

use crate::conf::AccountSettings;
use crate::email::{Envelope, EnvelopeHash, Flag};
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;
use crate::{backends::*, Collection};
use smallvec::SmallVec;
use std::collections::hash_map::{DefaultHasher, HashMap};
use std::error::Error;
use std::ffi::{CStr, CString, OsStr};
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

macro_rules! call {
    ($lib:expr, $func:ty) => {{
        let func: libloading::Symbol<$func> = $lib.get(stringify!($func).as_bytes()).unwrap();
        func
    }};
}

macro_rules! try_call {
    ($lib:expr, $call:expr) => {{
        let status = $call;
        if status == _notmuch_status_NOTMUCH_STATUS_SUCCESS {
            Ok(())
        } else {
            let c_str = call!($lib, notmuch_status_to_string)(status);
            Err(NotmuchError(
                CStr::from_ptr(c_str).to_string_lossy().into_owned(),
            ))
        }
    }};
}

pub mod bindings;
use bindings::*;
mod message;
pub use message::*;
mod tags;
pub use tags::*;
mod thread;
pub use thread::*;
mod query;
pub use query::*;

#[derive(Debug)]
pub struct DbConnection {
    pub state: Arc<NotmuchState>,
    pub inner: Arc<RwLock<*mut notmuch_database_t>>,
    pub database_ph: std::marker::PhantomData<&'static mut notmuch_database_t>,
}

impl DbConnection {
    pub fn get_revision_uuid(&self) -> u64 {
        unsafe {
            call!(self.state.lib, notmuch_database_get_revision)(
                *self.inner.read().unwrap(),
                std::ptr::null_mut(),
            )
        }
    }

    fn refresh(&mut self, new_revision_uuid: u64) -> Result<()> {
        let query_str = format!(
            "lastmod:{}..{}",
            *self.state.revision_uuid.read().unwrap(),
            new_revision_uuid
        );
        let query: Query = Query::new(self, &query_str)?;
        let iter = query.search()?;
        for message in iter {
            self.state.update_message_status(self, message)?;
        }
        drop(query);
        Ok(())
    }
}

unsafe impl Send for DbConnection {}
unsafe impl Sync for DbConnection {}
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

impl Drop for DbConnection {
    fn drop(&mut self) {
        let inner = self.inner.write().unwrap();
        unsafe {
            if let Err(err) = try_call!(
                self.state.lib,
                call!(self.state.lib, notmuch_database_close)(*inner)
            ) {
                debug!(err);
                return;
            }
            if let Err(err) = try_call!(
                self.state.lib,
                call!(self.state.lib, notmuch_database_destroy)(*inner)
            ) {
                debug!(err);
            }
        }
    }
}

#[derive(Debug)]
pub struct NotmuchDb {
    #[allow(dead_code)]
    lib: Arc<libloading::Library>,
    state: Arc<NotmuchState>,
    collection: Collection,
    path: PathBuf,
    #[allow(dead_code)]
    account_name: Arc<String>,
    #[allow(dead_code)]
    account_hash: AccountHash,
    #[allow(dead_code)]
    event_consumer: BackendEventConsumer,
    save_messages_to: Option<PathBuf>,
}

unsafe impl Send for NotmuchDb {}
unsafe impl Sync for NotmuchDb {}

#[derive(Debug, Clone)]
pub struct NotmuchState {
    #[allow(dead_code)]
    lib: Arc<libloading::Library>,
    revision_uuid: Arc<RwLock<u64>>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
    index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    collection: Collection,
    path: PathBuf,
    #[allow(dead_code)]
    account_name: Arc<String>,
    #[allow(dead_code)]
    account_hash: AccountHash,
    event_consumer: BackendEventConsumer,
    #[allow(dead_code)]
    save_messages_to: Option<PathBuf>,
}

impl NotmuchState {
    pub fn into_envelope(&self, message: Message<'_>) -> Envelope {
        let env_hash = message.env_hash();
        self.index
            .write()
            .unwrap()
            .insert(env_hash, message.msg_id_cstr().into());
        let mut tag_lock = self.collection.tag_index.write().unwrap();
        let (_, tags) = TagIterator::new(&message).collect_flags_and_tags();
        for tag in tags {
            let mut hasher = DefaultHasher::new();
            hasher.write(tag.as_bytes());
            let num = hasher.finish();
            tag_lock.entry(num).or_insert(tag);
        }
        message.into_envelope()
    }

    pub fn new_flags(&self, message: &Message<'_>) -> Result<()> {
        let (_, tags): (Flag, Vec<String>) = message.tags().collect_flags_and_tags();
        {
            let mut tag_lock = self.collection.tag_index.write().unwrap();
            for tag in tags.iter() {
                let mut hasher = DefaultHasher::new();
                hasher.write(tag.as_bytes());
                let num = hasher.finish();
                tag_lock.entry(num).or_insert_with(|| tag.clone());
            }
        }
        Ok(())
    }

    pub fn update_message_status(
        &self,
        database: &DbConnection,
        message: Message<'_>,
    ) -> Result<()> {
        use RefreshEventKind::*;

        self.new_flags(&message)?;
        let account_hash = self.account_hash;
        let message_id = message.msg_id_cstr().to_string_lossy();
        let env_hash = message.env_hash();

        for (&mailbox_hash, m) in self.mailboxes.read().unwrap().iter() {
            let query_str = format!("{} id:{}", m.query_str.as_str(), &message_id);
            let query: Query = Query::new(database, &query_str)?;
            if query.count().unwrap_or(0) > 0 {
                if m.contains(&env_hash) {
                    let tags: (Flag, Vec<String>) = message.tags().collect_flags_and_tags();
                    (self.event_consumer)(
                        account_hash,
                        BackendEvent::Refresh(RefreshEvent {
                            account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::NewFlags(env_hash, tags.clone()),
                        }),
                    );
                } else {
                    let env = self.into_envelope(message.clone());
                    let mut total_lck = m.total.lock().unwrap();
                    let mut unseen_lck = m.unseen.lock().unwrap();
                    *total_lck += 1;
                    if !env.is_seen() {
                        *unseen_lck += 1;
                    }
                    m.insert(env.hash());
                    (self.event_consumer)(
                        account_hash,
                        BackendEvent::Refresh(RefreshEvent {
                            account_hash,
                            mailbox_hash,
                            kind: Create(Box::new(env)),
                        }),
                    );
                }
            } else if m.remove(&env_hash) {
                (self.event_consumer)(
                    account_hash,
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Remove(env_hash),
                    }),
                );
            }
        }
        Ok(())
    }

    pub fn new_message(&self, database: &DbConnection, message: Message<'_>) -> Result<()> {
        use RefreshEventKind::*;
        let account_hash = self.account_hash;
        let message_id = message.msg_id_cstr().to_string_lossy().to_string();
        let env = self.into_envelope(message);
        for (&mailbox_hash, m) in self.mailboxes.read().unwrap().iter() {
            let query_str = format!("{} id:{}", m.query_str.as_str(), &message_id);
            let query: Query = Query::new(database, &query_str)?;
            if query.count().unwrap_or(0) > 0 {
                let mut total_lck = m.total.lock().unwrap();
                let mut unseen_lck = m.unseen.lock().unwrap();
                *total_lck += 1;
                if !env.is_seen() {
                    *unseen_lck += 1;
                }
                m.insert(env.hash());
                (self.event_consumer)(
                    account_hash,
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Create(Box::new(env.clone())),
                    }),
                );
            }
        }
        Ok(())
    }

    pub fn remove(&self, env_hash: EnvelopeHash) {
        use RefreshEventKind::*;
        let account_hash = self.account_hash;
        self.index.write().unwrap().remove(&env_hash);

        for (&mailbox_hash, m) in self.mailboxes.write().unwrap().iter_mut() {
            if m.remove(&env_hash) {
                let mut total_lck = m.total.lock().unwrap();
                //let mut unseen_lck = m.unseen.lock().unwrap();
                *total_lck = total_lck.saturating_sub(1);
                (self.event_consumer)(
                    account_hash,
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Remove(env_hash),
                    }),
                );
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
struct NotmuchMailbox {
    hash: MailboxHash,
    children: Vec<MailboxHash>,
    parent: Option<MailboxHash>,
    name: String,
    path: String,
    query_str: String,
    usage: Arc<RwLock<SpecialUsageMailbox>>,
    envelopes: Arc<RwLock<BTreeSet<EnvelopeHash>>>,
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

impl NotmuchMailbox {
    pub fn contains(&self, env_hash: &EnvelopeHash) -> bool {
        self.envelopes.read().unwrap().contains(env_hash)
    }

    pub fn insert(&self, env_hash: EnvelopeHash) {
        self.envelopes.write().unwrap().insert(env_hash);
    }

    pub fn remove(&self, env_hash: &EnvelopeHash) -> bool {
        self.envelopes.write().unwrap().remove(env_hash)
    }
}

impl NotmuchDb {
    pub fn new(
        s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        #[cfg(not(target_os = "macos"))]
        let mut dlpath = "libnotmuch.so.5";
        #[cfg(target_os = "macos")]
        let mut dlpath = "libnotmuch.5.dylib";
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
                        return Err(MeliError::new(format!("Notmuch `library_file_path` setting value `{}` for account {} does not exist or is a directory or not a valid library file.",dlpath, s.name()))
                            .set_kind(ErrorKind::Configuration)
                            .set_source(Some(Arc::new(err))));
                    } else {
                        return Err(MeliError::new("Could not load libnotmuch!")
                            .set_details(super::NOTMUCH_ERROR_DETAILS)
                            .set_source(Some(Arc::new(err))));
                    }
                }
            }
        });
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not exist.",
                s.root_mailbox.as_str(),
                s.name()
            ))
            .set_kind(ErrorKind::Configuration));
        }
        if !path.is_dir() {
            return Err(MeliError::new(format!(
                "Notmuch `root_mailbox` {} for account {} is not a directory.",
                s.root_mailbox.as_str(),
                s.name()
            ))
            .set_kind(ErrorKind::Configuration));
        }
        path.push(".notmuch");
        if !path.exists() || !path.is_dir() {
            return Err(MeliError::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not contain a `.notmuch` subdirectory.",
                s.root_mailbox.as_str(),
                s.name()
            )).set_kind(ErrorKind::Configuration));
        }
        path.pop();

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
                        envelopes: Arc::new(RwLock::new(BTreeSet::default())),
                        total: Arc::new(Mutex::new(0)),
                        unseen: Arc::new(Mutex::new(0)),
                    },
                );
            } else {
                return Err(MeliError::new(format!(
                    "notmuch mailbox configuration entry `{}` for account {} should have a `query` value set.",
                    k,
                    s.name(),
                ))
                .set_kind(ErrorKind::Configuration));
            }
        }

        let account_hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(s.name().as_bytes());
            hasher.finish()
        };
        let account_name = Arc::new(s.name().to_string());

        let collection = Collection::default();
        let state = Arc::new(NotmuchState {
            lib: lib.clone(),
            revision_uuid: Arc::new(RwLock::new(0)),
            mailboxes: Arc::new(RwLock::new(mailboxes)),
            index: Arc::new(RwLock::new(Default::default())),
            collection: collection.clone(),
            path: path.clone(),
            account_name: account_name.clone(),
            account_hash,
            event_consumer: event_consumer.clone(),
            save_messages_to: None,
        });
        Ok(Box::new(NotmuchDb {
            lib,
            state,
            collection,
            path,
            account_name,
            account_hash,
            event_consumer,
            save_messages_to: None,
        }))
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let mut path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not exist.",
                s.root_mailbox.as_str(),
                s.name()
            ))
            .set_kind(ErrorKind::Configuration));
        }
        if !path.is_dir() {
            return Err(MeliError::new(format!(
                "Notmuch `root_mailbox` {} for account {} is not a directory.",
                s.root_mailbox.as_str(),
                s.name()
            ))
            .set_kind(ErrorKind::Configuration));
        }
        path.push(".notmuch");
        if !path.exists() || !path.is_dir() {
            return Err(MeliError::new(format!(
                "Notmuch `root_mailbox` {} for account {} does not contain a `.notmuch` subdirectory.",
                s.root_mailbox.as_str(),
                s.name()
            )).set_kind(ErrorKind::Configuration));
        }
        path.pop();

        let account_name = s.name().to_string();
        if let Some(lib_path) = s.extra.remove("library_file_path") {
            if !Path::new(&lib_path).exists() || Path::new(&lib_path).is_dir() {
                return Err(MeliError::new(format!(
                            "Notmuch `library_file_path` setting value `{}` for account {} does not exist or is a directory.",
                            &lib_path,
                            s.name()
                )).set_kind(ErrorKind::Configuration));
            }
        }
        for (k, f) in s.mailboxes.iter_mut() {
            if f.extra.remove("query").is_none() {
                return Err(MeliError::new(format!(
                    "notmuch mailbox configuration entry `{}` for account {} should have a `query` value set.",
                    k,
                    account_name,
                ))
                .set_kind(ErrorKind::Configuration));
            }
        }
        Ok(())
    }

    fn new_connection(state: Arc<NotmuchState>, write: bool) -> Result<DbConnection> {
        let path_c = std::ffi::CString::new(state.path.to_str().unwrap()).unwrap();
        let path_ptr = path_c.as_ptr();
        let mut database: *mut notmuch_database_t = std::ptr::null_mut();
        let status = unsafe {
            call!(state.lib, notmuch_database_open)(
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
                state.path.display(),
                status
            )));
        }
        assert!(!database.is_null());
        let ret = DbConnection {
            state,
            inner: Arc::new(RwLock::new(database)),
            database_ph: std::marker::PhantomData,
        };
        if *ret.state.revision_uuid.read().unwrap() == 0 {
            let new = ret.get_revision_uuid();
            *ret.state.revision_uuid.write().unwrap() = new;
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
            state: Arc<NotmuchState>,
            iter: std::vec::IntoIter<CString>,
        }
        impl FetchState {
            async fn fetch(&mut self) -> Result<Option<Vec<Envelope>>> {
                let mut unseen_count = 0;
                let chunk_size = 250;
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
                        let env = self.state.into_envelope(message);
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
                    let mailboxes_lck = self.state.mailboxes.read().unwrap();
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
        let database = Arc::new(NotmuchDb::new_connection(self.state.clone(), false)?);
        let v: Vec<CString>;
        let state = self.state.clone();
        {
            let mailboxes_lck = state.mailboxes.read().unwrap();
            let mailbox = mailboxes_lck.get(&mailbox_hash).unwrap();
            let query: Query = Query::new(&database, mailbox.query_str.as_str())?;
            {
                let mut total_lck = mailbox.total.lock().unwrap();
                let mut unseen_lck = mailbox.unseen.lock().unwrap();
                *total_lck = query.count()? as usize;
                *unseen_lck = 0;
            }
            let mut index_lck = state.index.write().unwrap();
            v = query
                .search()?
                .into_iter()
                .map(|m| {
                    index_lck.insert(m.env_hash(), m.msg_id_cstr().into());
                    m.msg_id_cstr().into()
                })
                .collect();
        }

        let mut fetch_state = FetchState {
            mailbox_hash,
            state,
            database,
            iter: v.into_iter(),
        };
        Ok(Box::pin(async_stream::try_stream! {
            while let Some(res) = fetch_state.fetch().await.map_err(|err| { debug!("fetch err {:?}", &err); err})? {
                yield res;
            }
        }))
    }

    fn refresh(&mut self, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let mut database = NotmuchDb::new_connection(self.state.clone(), false)?;
        let state = self.state.clone();
        Ok(Box::pin(async move {
            let new_revision_uuid = database.get_revision_uuid();
            if new_revision_uuid > *state.revision_uuid.read().unwrap() {
                database.refresh(new_revision_uuid)?;
                *state.revision_uuid.write().unwrap() = new_revision_uuid;
            }
            Ok(())
        }))
    }

    fn watch(&self) -> ResultFuture<()> {
        extern crate notify;
        use notify::{watcher, RecursiveMode, Watcher};

        let state = self.state.clone();

        let (tx, rx) = std::sync::mpsc::channel();
        let mut watcher = watcher(tx, std::time::Duration::from_secs(2)).unwrap();
        watcher.watch(&self.path, RecursiveMode::Recursive).unwrap();
        Ok(Box::pin(async move {
            let _watcher = watcher;
            let rx = rx;
            loop {
                let _ = rx.recv().map_err(|err| err.to_string())?;
                {
                    let mut database = NotmuchDb::new_connection(state.clone(), false)?;
                    let new_revision_uuid = database.get_revision_uuid();
                    if new_revision_uuid > *state.revision_uuid.read().unwrap() {
                        database.refresh(new_revision_uuid)?;
                        *state.revision_uuid.write().unwrap() = new_revision_uuid;
                    }
                }
            }
        }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let ret = Ok(self
            .state
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
            database: Arc::new(Self::new_connection(self.state.clone(), true)?),
            hash,
            bytes: None,
        }))
    }

    fn save(
        &self,
        bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()> {
        // FIXME call notmuch_database_index_file ?
        let path = self
            .save_messages_to
            .as_ref()
            .unwrap_or(&self.path)
            .to_path_buf();
        MaildirType::save_to_mailbox(path, bytes, flags)?;
        Ok(Box::pin(async { Ok(()) }))
    }

    fn copy_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _source_mailbox_hash: MailboxHash,
        _destination_mailbox_hash: MailboxHash,
        _move_: bool,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
        flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()> {
        let database = Self::new_connection(self.state.clone(), true)?;
        let state = self.state.clone();

        Ok(Box::pin(async move {
            for env_hash in env_hashes.iter() {
                debug!(&env_hash);
                let message =
                    match Message::find_message(&database, &state.index.read().unwrap()[&env_hash])
                    {
                        Ok(v) => v,
                        Err(err) => {
                            debug!("not found {}", err);
                            continue;
                        }
                    };

                let tags = message.tags().collect::<Vec<&CStr>>();
                //flags.set(f, value);

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

                for (f, v) in flags.iter() {
                    let value = *v;
                    match f {
                        Ok(Flag::DRAFT) if value => add_tag!(b"draft\0"),
                        Ok(Flag::DRAFT) => remove_tag!(b"draft\0"),
                        Ok(Flag::FLAGGED) if value => add_tag!(b"flagged\0"),
                        Ok(Flag::FLAGGED) => remove_tag!(b"flagged\0"),
                        Ok(Flag::PASSED) if value => add_tag!(b"passed\0"),
                        Ok(Flag::PASSED) => remove_tag!(b"passed\0"),
                        Ok(Flag::REPLIED) if value => add_tag!(b"replied\0"),
                        Ok(Flag::REPLIED) => remove_tag!(b"replied\0"),
                        Ok(Flag::SEEN) if value => remove_tag!(b"unread\0"),
                        Ok(Flag::SEEN) => add_tag!(b"unread\0"),
                        Ok(Flag::TRASHED) if value => add_tag!(b"trashed\0"),
                        Ok(Flag::TRASHED) => remove_tag!(b"trashed\0"),
                        Ok(_) => debug!("flags is {:?} value = {}", f, value),
                        Err(tag) if value => {
                            let c_tag = CString::new(tag.as_str()).unwrap();
                            add_tag!(&c_tag.as_ref());
                        }
                        Err(tag) => {
                            let c_tag = CString::new(tag.as_str()).unwrap();
                            remove_tag!(&c_tag.as_ref());
                        }
                    }
                }

                /* Update message filesystem path. */
                message.tags_to_maildir_flags()?;
                state.update_message_status(&database, message)?;
            }

            Ok(())
        }))
    }

    fn delete_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn search(
        &self,
        melib_query: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        let database = NotmuchDb::new_connection(self.state.clone(), false)?;
        let state = self.state.clone();
        Ok(Box::pin(async move {
            let mut ret = SmallVec::new();
            let mut query_s = if let Some(mailbox_hash) = mailbox_hash {
                if let Some(m) = state.mailboxes.read().unwrap().get(&mailbox_hash) {
                    let mut s = m.query_str.clone();
                    s.push(' ');
                    s
                } else {
                    return Err(MeliError::new("Mailbox with hash {} not found!")
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Debug)]
struct NotmuchOp {
    hash: EnvelopeHash,
    database: Arc<DbConnection>,
    bytes: Option<Vec<u8>>,
}

impl BackendOp for NotmuchOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        let message = Message::find_message(
            &self.database,
            &self.database.state.index.write().unwrap()[&self.hash],
        )?;
        let mut f = std::fs::File::open(message.get_filename())?;
        let mut response = Vec::new();
        f.read_to_end(&mut response)?;
        self.bytes = Some(response);
        let ret = Ok(self.bytes.as_ref().unwrap().to_vec());
        Ok(Box::pin(async move { ret }))
    }

    fn fetch_flags(&self) -> ResultFuture<Flag> {
        let message = Message::find_message(
            &self.database,
            &self.database.state.index.write().unwrap()[&self.hash],
        )?;
        let (flags, _tags) = message.tags().collect_flags_and_tags();
        Ok(Box::pin(async move { Ok(flags) }))
    }
}
