use crate::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use crate::backends::FolderHash;
use crate::backends::{
    BackendFolder, BackendOp, Folder, FolderPermissions, MailBackend, RefreshEventConsumer,
};
use crate::conf::AccountSettings;
use crate::email::{Envelope, EnvelopeHash, Flag};
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;
use fnv::FnvHashMap;
use std::collections::hash_map::DefaultHasher;
use std::ffi::CStr;
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

pub mod bindings;
use bindings::*;

#[derive(Debug, Clone)]
struct DbWrapper {
    inner: Arc<RwLock<*mut notmuch_database_t>>,
    database_ph: std::marker::PhantomData<&'static mut notmuch_database_t>,
}

unsafe impl Send for DbWrapper {}
unsafe impl Sync for DbWrapper {}

#[derive(Debug)]
pub struct NotmuchDb {
    database: DbWrapper,
    folders: Arc<RwLock<FnvHashMap<FolderHash, NotmuchFolder>>>,
    index: Arc<RwLock<FnvHashMap<EnvelopeHash, &'static CStr>>>,
    path: PathBuf,
    save_messages_to: Option<PathBuf>,
}

unsafe impl Send for NotmuchDb {}
unsafe impl Sync for NotmuchDb {}

impl Drop for NotmuchDb {
    fn drop(&mut self) {
        for f in self.folders.write().unwrap().values_mut() {
            if let Some(query) = f.query.take() {
                unsafe {
                    notmuch_query_destroy(query);
                }
            }
        }
        let inner = self.database.inner.write().unwrap();
        unsafe {
            notmuch_database_close(*inner);
            notmuch_database_destroy(*inner);
        }
    }
}

#[derive(Debug, Clone, Default)]
struct NotmuchFolder {
    hash: FolderHash,
    children: Vec<FolderHash>,
    parent: Option<FolderHash>,
    name: String,
    path: String,
    query_str: String,
    query: Option<*mut notmuch_query_t>,
    phantom: std::marker::PhantomData<&'static mut notmuch_query_t>,
}

impl BackendFolder for NotmuchFolder {
    fn hash(&self) -> FolderHash {
        self.hash
    }

    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn path(&self) -> &str {
        self.path.as_str()
    }

    fn change_name(&mut self, _s: &str) {}

    fn clone(&self) -> Folder {
        Box::new(std::clone::Clone::clone(self))
    }

    fn children(&self) -> &[FolderHash] {
        &self.children
    }

    fn parent(&self) -> Option<FolderHash> {
        self.parent
    }

    fn permissions(&self) -> FolderPermissions {
        FolderPermissions::default()
    }
}

unsafe impl Send for NotmuchFolder {}
unsafe impl Sync for NotmuchFolder {}

impl NotmuchDb {
    pub fn new(
        s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
    ) -> Result<Box<dyn MailBackend>> {
        let mut database: *mut notmuch_database_t = std::ptr::null_mut();
        let path = Path::new(s.root_folder.as_str()).expand().to_path_buf();
        if !path.exists() {
            panic!(
                "\"root_folder\" {} for account {} is not a valid path.",
                s.root_folder.as_str(),
                s.name()
            );
        }

        let path_c = std::ffi::CString::new(path.to_str().unwrap()).unwrap();
        let path_ptr = path_c.as_ptr();
        let status = unsafe {
            bindings::notmuch_database_open(
                path_ptr,
                notmuch_database_mode_t_NOTMUCH_DATABASE_MODE_READ_WRITE,
                &mut database as *mut _,
            )
        };
        if status != 0 {
            return Err(MeliError::new(format!("Could not open notmuch database at path {}. notmuch_database_open returned {}.", s.root_folder.as_str(), status);
        }
        assert!(!database.is_null());
        let mut folders = FnvHashMap::default();
        for (k, f) in s.folders.iter() {
            if let Some(query_str) = f.extra.get("query") {
                let hash = {
                    let mut h = DefaultHasher::new();
                    k.hash(&mut h);
                    h.finish()
                };
                folders.insert(
                    hash,
                    NotmuchFolder {
                        hash,
                        name: k.to_string(),
                        path: k.to_string(),
                        children: vec![],
                        parent: None,
                        query: None,
                        query_str: query_str.to_string(),
                        phantom: std::marker::PhantomData,
                    },
                );
            } else {
                return Err(MeliError::new(format!(
                    "notmuch folder configuration entry \"{}\" should have a \"query\" value set.",
                    k
                )));
            }
        }
        Ok(Box::new(NotmuchDb {
            database: DbWrapper {
                inner: Arc::new(RwLock::new(database)),
                database_ph: std::marker::PhantomData,
            },
            path,
            index: Arc::new(RwLock::new(Default::default())),
            folders: Arc::new(RwLock::new(folders)),
            save_messages_to: None,
        }))
    }
}

impl MailBackend for NotmuchDb {
    fn is_online(&self) -> bool {
        true
    }
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let folder_hash = folder.hash();
        let database = self.database.clone();
        let index = self.index.clone();
        let folders = self.folders.clone();
        let handle = {
            let tx = w.tx();
            let closure = move |_work_context| {
                let mut ret: Vec<Envelope> = Vec::new();
                let database = database.clone();
                let database_lck = database.inner.read().unwrap();
                let folders = folders.clone();
                let tx = tx.clone();
                let mut folders_lck = folders.write().unwrap();
                let folder = folders_lck.get_mut(&folder_hash).unwrap();
                let query_str = std::ffi::CString::new(folder.query_str.as_str()).unwrap();
                let query: *mut notmuch_query_t =
                    unsafe { notmuch_query_create(*database_lck, query_str.as_ptr()) };
                if query.is_null() {
                    panic!("Out of memory.");
                }
                let mut messages: *mut notmuch_messages_t = std::ptr::null_mut();
                let status =
                    unsafe { notmuch_query_search_messages(query, &mut messages as *mut _) };
                if status != 0 {
                    panic!(status);
                }
                assert!(!messages.is_null());
                let iter = MessageIterator { messages };
                for message in iter {
                    let mut response = String::new();
                    let fs_path = unsafe { notmuch_message_get_filename(message) };
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
                    let c_str = unsafe { CStr::from_ptr(fs_path) };
                    let env_hash = {
                        let mut hasher = DefaultHasher::default();
                        c_str.hash(&mut hasher);
                        hasher.finish()
                    };
                    index.write().unwrap().insert(env_hash, c_str);
                    let op = Box::new(NotmuchOp {
                        database: database.clone(),
                        hash: env_hash,
                        index: index.clone(),
                        bytes: Some(response),
                    });
                    if let Some(env) = Envelope::from_token(op, env_hash) {
                        ret.push(env);
                    } else {
                        debug!("could not parse path {:?}", c_str);
                        index.write().unwrap().remove(&env_hash);
                    }
                }
                folder.query = Some(query);
                tx.send(AsyncStatus::Payload(Ok(ret))).unwrap();
                tx.send(AsyncStatus::Finished).unwrap();
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn watch(
        &self,
        _sender: RefreshEventConsumer,
        _work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        let handle = std::thread::Builder::new()
            .name(format!(
                "watching {}",
                self.path.file_name().unwrap().to_str().unwrap()
            ))
            .spawn(move || {})?;
        Ok(handle.thread().id())
    }
    fn folders(&self) -> Result<FnvHashMap<FolderHash, Folder>> {
        Ok(self.folders
            .read()
            .unwrap()
            .iter()
            .map(|(k, f)| (*k, BackendFolder::clone(f)))
            .collect())
    }
    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        Box::new(NotmuchOp {
            database: self.database.clone(),
            hash,
            index: self.index.clone(),
            bytes: None,
        })
    }

    fn save(&self, bytes: &[u8], _folder: &str, flags: Option<Flag>) -> Result<()> {
        let mut path = self
            .save_messages_to
            .as_ref()
            .unwrap_or(&self.path)
            .to_path_buf();
        if !(path.ends_with("cur") || path.ends_with("new") || path.ends_with("tmp")) {
            for d in &["cur", "new", "tmp"] {
                path.push(d);
                if !path.is_dir() {
                    return Err(MeliError::new(format!(
                        "{} is not a valid maildir folder",
                        path.display()
                    )));
                }
                path.pop();
            }
            path.push("cur");
        }
        crate::backends::MaildirType::save_to_folder(path, bytes, flags)
    }

    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }
}

#[derive(Debug)]
struct NotmuchOp {
    hash: EnvelopeHash,
    index: Arc<RwLock<FnvHashMap<EnvelopeHash, &'static CStr>>>,
    database: DbWrapper,
    bytes: Option<String>,
}

impl BackendOp for NotmuchOp {
    fn description(&self) -> String {
        String::new()
    }

    fn as_bytes(&mut self) -> Result<&[u8]> {
        let path = &self.index.read().unwrap()[&self.hash];
        let mut f = std::fs::File::open(path.to_str().unwrap())?;
        let mut response = String::new();
        f.read_to_string(&mut response)?;
        self.bytes = Some(response);
        Ok(self.bytes.as_ref().unwrap().as_bytes())
    }

    fn fetch_headers(&mut self) -> Result<&[u8]> {
        let raw = self.as_bytes()?;
        let result = crate::email::parser::headers_raw(raw).to_full_result()?;
        Ok(result)
    }

    fn fetch_body(&mut self) -> Result<&[u8]> {
        let raw = self.as_bytes()?;
        let result = crate::email::parser::body_raw(raw).to_full_result()?;
        Ok(result)
    }

    fn fetch_flags(&self) -> Flag {
        let mut flag = Flag::default();
        let path = self.index.read().unwrap()[&self.hash].to_str().unwrap();
        if !path.contains(":2,") {
            return flag;
        }

        for f in path.chars().rev() {
            match f {
                ',' => break,
                'D' => flag |= Flag::DRAFT,
                'F' => flag |= Flag::FLAGGED,
                'P' => flag |= Flag::PASSED,
                'R' => flag |= Flag::REPLIED,
                'S' => flag |= Flag::SEEN,
                'T' => flag |= Flag::TRASHED,
                _ => {
                    debug!("DEBUG: in fetch_flags, path is {}", path);
                }
            }
        }

        flag
    }

    fn set_flag(&mut self, _envelope: &mut Envelope, f: Flag, value: bool) -> Result<()> {
        let mut message: *mut notmuch_message_t = std::ptr::null_mut();
        let mut index_lck = self.index.write().unwrap();
        unsafe {
            notmuch_database_find_message_by_filename(
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

        let tags = (TagIterator {
            tags: unsafe { notmuch_message_get_tags(message) },
        })
        .collect::<Vec<&CStr>>();
        debug!(&tags);

        macro_rules! cstr {
            ($l:literal) => {
                CStr::from_bytes_with_nul_unchecked($l)
            };
        }
        macro_rules! add_tag {
            ($l:literal) => {
                unsafe {
                    if tags.contains(&cstr!($l)) {
                        return Ok(());
                    }
                    if notmuch_message_add_tag(message, cstr!($l).as_ptr())
                        != _notmuch_status_NOTMUCH_STATUS_SUCCESS
                    {
                        return Err(MeliError::new("Could not set tag."));
                    }
                }
            };
        }
        macro_rules! remove_tag {
            ($l:literal) => {
                unsafe {
                    if !tags.contains(&cstr!($l)) {
                        return Ok(());
                    }
                    if notmuch_message_remove_tag(message, cstr!($l).as_ptr())
                        != _notmuch_status_NOTMUCH_STATUS_SUCCESS
                    {
                        return Err(MeliError::new("Could not set tag."));
                    }
                }
            };
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
        if unsafe { notmuch_message_tags_to_maildir_flags(message) }
            != _notmuch_status_NOTMUCH_STATUS_SUCCESS
        {
            return Err(MeliError::new("Could not set tag."));
        }

        let fs_path = unsafe { notmuch_message_get_filename(message) };
        let c_str = unsafe { CStr::from_ptr(fs_path) };
        if let Some(p) = index_lck.get_mut(&self.hash) {
            *p = c_str;
        }
        let new_hash = {
            let mut hasher = DefaultHasher::default();
            c_str.hash(&mut hasher);
            hasher.finish()
        };
        index_lck.insert(new_hash, c_str);

        Ok(())
    }
}

pub struct MessageIterator {
    messages: *mut notmuch_messages_t,
}

impl Iterator for MessageIterator {
    type Item = *mut notmuch_message_t;
    fn next(&mut self) -> Option<Self::Item> {
        if self.messages.is_null() {
            None
        } else if unsafe { notmuch_messages_valid(self.messages) } == 1 {
            let ret = Some(unsafe { notmuch_messages_get(self.messages) });
            unsafe {
                notmuch_messages_move_to_next(self.messages);
            }
            ret
        } else {
            self.messages = std::ptr::null_mut();
            None
        }
    }
}

pub struct TagIterator {
    tags: *mut notmuch_tags_t,
}

impl Iterator for TagIterator {
    type Item = &'static CStr;
    fn next(&mut self) -> Option<Self::Item> {
        if self.tags.is_null() {
            None
        } else if unsafe { notmuch_tags_valid(self.tags) } == 1 {
            let ret = Some(unsafe { CStr::from_ptr(notmuch_tags_get(self.tags)) });
            unsafe {
                notmuch_tags_move_to_next(self.tags);
            }
            ret
        } else {
            self.tags = std::ptr::null_mut();
            None
        }
    }
}
