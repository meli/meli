/*
 * meli - mailbox module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

use super::{
    BackendFolder, BackendOp, Folder, FolderHash, MailBackend, RefreshEvent, RefreshEventConsumer,
    RefreshEventKind::*,
};
use super::{MaildirFolder, MaildirOp};
use crate::async_workers::*;
use crate::conf::AccountSettings;
use crate::email::{Envelope, EnvelopeHash, Flag};
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;

extern crate notify;
use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::time::Duration;

use std::sync::mpsc::channel;
//use std::sync::mpsc::sync_channel;
//use std::sync::mpsc::SyncSender;
//use std::time::Duration;
use fnv::{FnvHashMap, FnvHashSet, FnvHasher};
use std::collections::hash_map::DefaultHasher;
use std::ffi::OsStr;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::{self, Read, Write};
use std::ops::{Deref, DerefMut};
use std::os::unix::fs::PermissionsExt;
use std::path::{Component, Path, PathBuf};
use std::result;
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum PathMod {
    Path(PathBuf),
    Hash(EnvelopeHash),
}

#[derive(Debug, Default)]
pub struct MaildirPath {
    pub(super) buf: PathBuf,
    pub(super) modified: Option<PathMod>,
    pub(super) removed: bool,
}

impl Deref for MaildirPath {
    type Target = PathBuf;
    fn deref(&self) -> &PathBuf {
        assert!(!(self.removed && self.modified.is_none()));
        &self.buf
    }
}

impl DerefMut for MaildirPath {
    fn deref_mut(&mut self) -> &mut PathBuf {
        assert!(!(self.removed && self.modified.is_none()));
        &mut self.buf
    }
}

impl From<PathBuf> for MaildirPath {
    fn from(val: PathBuf) -> MaildirPath {
        MaildirPath {
            buf: val,
            modified: None,
            removed: false,
        }
    }
}

#[derive(Debug, Default)]
pub struct HashIndex {
    index: FnvHashMap<EnvelopeHash, MaildirPath>,
    hash: FolderHash,
}

impl Deref for HashIndex {
    type Target = FnvHashMap<EnvelopeHash, MaildirPath>;
    fn deref(&self) -> &FnvHashMap<EnvelopeHash, MaildirPath> {
        &self.index
    }
}

impl DerefMut for HashIndex {
    fn deref_mut(&mut self) -> &mut FnvHashMap<EnvelopeHash, MaildirPath> {
        &mut self.index
    }
}

pub type HashIndexes = Arc<Mutex<FnvHashMap<FolderHash, HashIndex>>>;

/// Maildir backend https://cr.yp.to/proto/maildir.html
#[derive(Debug)]
pub struct MaildirType {
    name: String,
    folders: FnvHashMap<FolderHash, MaildirFolder>,
    folder_index: Arc<Mutex<FnvHashMap<EnvelopeHash, FolderHash>>>,
    hash_indexes: HashIndexes,
    path: PathBuf,
}

macro_rules! path_is_new {
    ($path:expr) => {
        if $path.is_dir() {
            false
        } else {
            let mut iter = $path.components().rev();
            iter.next();
            iter.next() == Some(Component::Normal(OsStr::new("new")))
        }
    };
}

#[macro_export]
macro_rules! get_path_hash {
    ($path:expr) => {{
        let mut path = $path.clone();
        if path.is_dir() {
            if path.ends_with("cur") | path.ends_with("new") {
                path.pop();
            }
        } else {
            path.pop();
            path.pop();
        };

        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        hasher.finish()
    }};
}

pub(super) fn get_file_hash(file: &Path) -> EnvelopeHash {
    /*
    let mut buf = Vec::with_capacity(2048);
    let mut f = fs::File::open(&file).unwrap_or_else(|_| panic!("Can't open {}", file.display()));
    f.read_to_end(&mut buf)
        .unwrap_or_else(|_| panic!("Can't read {}", file.display()));
    let mut hasher = FnvHasher::default();
    hasher.write(&buf);
    hasher.finish()
        */
    let mut hasher = FnvHasher::default();
    file.hash(&mut hasher);
    hasher.finish()
}

fn move_to_cur(p: PathBuf) -> Result<PathBuf> {
    let mut new = p.clone();
    let file_name = p.to_string_lossy();
    let slash_pos = file_name.bytes().rposition(|c| c == b'/').unwrap() + 1;
    new.pop();
    new.pop();

    new.push("cur");
    new.push(&file_name[slash_pos..]);
    if !file_name.ends_with(":2,") {
        new.set_extension(":2,");
    }
    debug!("moved to cur: {}", new.display());
    fs::rename(&p, &new)?;
    Ok(new)
}

impl MailBackend for MaildirType {
    fn is_online(&self) -> Result<()> {
        Ok(())
    }

    fn folders(&self) -> Result<FnvHashMap<FolderHash, Folder>> {
        Ok(self
            .folders
            .iter()
            .map(|(h, f)| (*h, f.clone() as Folder))
            .collect())
    }

    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        self.multicore(4, folder)
    }
    fn refresh(
        &mut self,
        folder_hash: FolderHash,
        sender: RefreshEventConsumer,
    ) -> Result<Async<Result<Vec<RefreshEvent>>>> {
        let w = AsyncBuilder::new();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();

        let handle = {
            let folder: &MaildirFolder = &self.folders[&folder_hash];
            let path: PathBuf = folder.fs_path().into();
            let name = format!("refresh {:?}", folder.name());
            let root_path = self.path.to_path_buf();
            let map = self.hash_indexes.clone();
            let folder_index = self.folder_index.clone();
            let cache_dir = cache_dir.clone();
            let sender = Arc::new(sender);

            Box::new(move |work_context: crate::async_workers::WorkContext| {
                let cache_dir = cache_dir.clone();
                let folder_index = folder_index.clone();
                let root_path = root_path.clone();
                let path = path.clone();
                let name = name.clone();
                let map = map.clone();
                let sender = sender.clone();
                work_context
                    .set_name
                    .send((std::thread::current().id(), name.clone()))
                    .unwrap();
                let thunk = move |sender: &RefreshEventConsumer| {
                    debug!("refreshing");
                    let cache_dir = cache_dir.clone();
                    let map = map.clone();
                    let folder_index = folder_index.clone();
                    let folder_hash = folder_hash.clone();
                    let root_path = root_path.clone();
                    let mut path = path.clone();
                    let cache_dir = cache_dir.clone();
                    path.push("new");
                    for d in path.read_dir()? {
                        if let Ok(p) = d {
                            move_to_cur(p.path()).ok().take();
                        }
                    }
                    path.pop();

                    path.push("cur");
                    let iter = path.read_dir()?;
                    let count = path.read_dir()?.count();
                    let mut files: Vec<PathBuf> = Vec::with_capacity(count);
                    for e in iter {
                        let e = e.and_then(|x| {
                            let path = x.path();
                            Ok(path)
                        })?;
                        files.push(e);
                    }
                    let mut current_hashes = {
                        let mut map = map.lock().unwrap();
                        let map = map.entry(folder_hash).or_default();
                        map.keys().cloned().collect::<FnvHashSet<EnvelopeHash>>()
                    };
                    for file in files {
                        let hash = get_file_hash(&file);
                        {
                            let mut map = map.lock().unwrap();
                            let map = map.entry(folder_hash).or_default();
                            if map.contains_key(&hash) {
                                map.remove(&hash);
                                current_hashes.remove(&hash);
                                continue;
                            }
                            (*map).insert(hash, PathBuf::from(&file).into());
                        }
                        let op = Box::new(MaildirOp::new(hash, map.clone(), folder_hash));
                        if let Some(e) = Envelope::from_token(op, hash) {
                            folder_index.lock().unwrap().insert(e.hash(), folder_hash);
                            let file_name = PathBuf::from(file)
                                .strip_prefix(&root_path)
                                .unwrap()
                                .to_path_buf();
                            if let Ok(cached) = cache_dir.place_cache_file(file_name) {
                                /* place result in cache directory */
                                let f = match fs::File::create(cached) {
                                    Ok(f) => f,
                                    Err(e) => {
                                        panic!("{}", e);
                                    }
                                };
                                let metadata = f.metadata().unwrap();
                                let mut permissions = metadata.permissions();

                                permissions.set_mode(0o600); // Read/write for owner only.
                                f.set_permissions(permissions).unwrap();

                                let writer = io::BufWriter::new(f);
                                bincode::serialize_into(writer, &e).unwrap();
                            }
                            sender.send(RefreshEvent {
                                hash: folder_hash,
                                kind: Create(Box::new(e)),
                            });
                        } else {
                            debug!(
                                "DEBUG: hash {}, path: {} couldn't be parsed",
                                hash,
                                file.as_path().display()
                            );
                            continue;
                        }
                    }
                    for ev in current_hashes.into_iter().map(|h| RefreshEvent {
                        hash: folder_hash,
                        kind: Remove(h),
                    }) {
                        sender.send(ev);
                    }
                    Ok(())
                };
                if let Err(err) = thunk(&sender) {
                    sender.send(RefreshEvent {
                        hash: folder_hash,
                        kind: Failure(err),
                    });
                }
            })
        };
        Ok(w.build(handle))
    }
    fn watch(
        &self,
        sender: RefreshEventConsumer,
        work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(2)).unwrap();
        let root_path = self.path.to_path_buf();
        watcher.watch(&root_path, RecursiveMode::Recursive).unwrap();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        debug!("watching {:?}", root_path);
        let hash_indexes = self.hash_indexes.clone();
        let folder_index = self.folder_index.clone();
        let handle = thread::Builder::new()
            .name("folder watch".to_string())
            .spawn(move || {
                // Move `watcher` in the closure's scope so that it doesn't get dropped.
                let _watcher = watcher;
                let _work_context = work_context;
                loop {
                    match rx.recv() {
                        /*
                         * Event types:
                         *
                         * pub enum RefreshEventKind {
                         *     Update(EnvelopeHash, Envelope), // Old hash, new envelope
                         *     Create(Envelope),
                         *     Remove(EnvelopeHash),
                         *     Rescan,
                         * }
                         */
                        Ok(event) => match event {
                            /* Create */
                            DebouncedEvent::Create(mut pathbuf) => {
                                debug!("DebouncedEvent::Create(path = {:?}", pathbuf);
                                if path_is_new!(pathbuf) {
                                    debug!("path_is_new");
                                    /* This creates a Rename event that we will receive later */
                                    pathbuf = match move_to_cur(pathbuf) {
                                        Ok(p) => p,
                                        Err(e) => {
                                            debug!("error: {}", e.to_string());
                                            continue;
                                        }
                                    };


                                }
                                let folder_hash = get_path_hash!(pathbuf);
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                if let Some(env) = add_path_to_index(
                                    &hash_indexes,
                                    folder_hash,
                                    pathbuf.as_path(),
                                    &cache_dir,
                                    file_name,
                                ) {
                                    folder_index.lock().unwrap().insert(env.hash(),folder_hash);
                                    debug!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        pathbuf.display()
                                    );
                                    sender.send(RefreshEvent {
                                        hash: folder_hash,
                                        kind: Create(Box::new(env)),
                                    });
                                }
                            }
                            /* Update */
                            DebouncedEvent::NoticeWrite(pathbuf)
                            | DebouncedEvent::Write(pathbuf) => {
                                debug!("DebouncedEvent::Write(path = {:?}", &pathbuf);
                                let folder_hash = get_path_hash!(pathbuf);
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock =
                                    &mut hash_indexes_lock.entry(folder_hash).or_default();
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                /* Linear search in hash_index to find old hash */
                                let old_hash: EnvelopeHash = {
                                    if let Some((k, v)) =
                                        index_lock.iter_mut().find(|(_, v)| *v.buf == pathbuf)
                                    {
                                        //TODO FIXME This doesn't make sense?
                                        *v = pathbuf.clone().into();
                                        *k
                                    } else {
                                        drop(index_lock);
                                        drop(hash_indexes_lock);
                                        /* Did we just miss a Create event? In any case, create
                                         * envelope. */
                                        if let Some(env) = add_path_to_index(
                                            &hash_indexes,
                                            folder_hash,
                                            pathbuf.as_path(),
                                            &cache_dir,
                                            file_name,
                                        ) {
                                            folder_index.lock().unwrap().insert(env.hash(),folder_hash);
                                            sender.send(RefreshEvent {
                                                hash: folder_hash,
                                                kind: Create(Box::new(env)),
                                            });
                                        }
                                        return;
                                    }
                                };
                                let new_hash: EnvelopeHash = get_file_hash(pathbuf.as_path());
                                if index_lock.get_mut(&new_hash).is_none() {
                                    debug!("write notice");
                                    let op = Box::new(MaildirOp::new(
                                        new_hash,
                                        hash_indexes.clone(),
                                        folder_hash,
                                    ));
                                    if let Some(env) = Envelope::from_token(op, new_hash) {
                                        debug!("{}\t{:?}", new_hash, &pathbuf);
                                        debug!(
                                            "hash {}, path: {:?} couldn't be parsed",
                                            new_hash, &pathbuf
                                        );
                                        index_lock.insert(new_hash, pathbuf.into());

                                        /* Send Write notice */

                                        sender.send(RefreshEvent {
                                            hash: folder_hash,
                                            kind: Update(old_hash, Box::new(env)),
                                        });
                                    }
                                }
                            }
                            /* Remove */
                            DebouncedEvent::NoticeRemove(pathbuf)
                            | DebouncedEvent::Remove(pathbuf) => {
                                debug!("DebouncedEvent::Remove(path = {:?}", pathbuf);
                                let folder_hash = get_path_hash!(pathbuf);
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(folder_hash).or_default();
                                let hash: EnvelopeHash = if let Some((k, _)) =
                                    index_lock.iter().find(|(_, v)| *v.buf == pathbuf)
                                {
                                    *k
                                } else {
                                    debug!("removed but not contained in index");
                                    continue;
                                };
                                if let Some(ref modif) = &index_lock[&hash].modified {
                                    match modif {
                                        PathMod::Path(path) => debug!(
                                            "envelope {} has modified path set {}",
                                            hash,
                                            path.display()
                                        ),
                                        PathMod::Hash(hash) => debug!(
                                            "envelope {} has modified path set {}",
                                            hash,
                                            &index_lock[&hash].buf.display()
                                        ),
                                    }
                                    continue;
                                }

                                index_lock.entry(hash).and_modify(|e| {
                                    e.removed = true;
                                });

                                sender.send(RefreshEvent {
                                    hash: folder_hash,
                                    kind: Remove(hash),
                                });
                            }
                            /* Envelope hasn't changed */
                            DebouncedEvent::Rename(src, dest) => {
                                debug!(
                                    "DebouncedEvent::Rename(src = {:?}, dest = {:?})",
                                    src, dest
                                );
                                let folder_hash = get_path_hash!(src);
                                let old_hash: EnvelopeHash = get_file_hash(src.as_path());
                                let new_hash: EnvelopeHash = get_file_hash(dest.as_path());

                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(folder_hash).or_default();

                                if index_lock.contains_key(&old_hash)
                                    && !index_lock[&old_hash].removed
                                {
                                    debug!("contains_old_key");
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        debug!(&e.modified);
                                        e.modified = Some(PathMod::Hash(new_hash));
                                    });
                                    sender.send(RefreshEvent {
                                        hash: get_path_hash!(dest),
                                        kind: Rename(old_hash, new_hash),
                                    });
                                    folder_index.lock().unwrap().insert(new_hash,get_path_hash!(dest) );
                                    index_lock.insert(new_hash, dest.into());
                                    continue;
                                } else if !index_lock.contains_key(&new_hash)
                                    || index_lock
                                        .get(&old_hash)
                                        .map(|e| e.removed)
                                        .unwrap_or(false)
                                {
                                    if index_lock
                                        .get(&old_hash)
                                        .map(|e| e.removed)
                                        .unwrap_or(false)
                                    {
                                        index_lock.entry(old_hash).and_modify(|e| {
                                            e.modified = Some(PathMod::Hash(new_hash));
                                            e.removed = false;
                                        });
                                        debug!("contains_old_key, key was marked as removed (by external source)");
                                    } else {
                                        debug!("not contains_new_key");
                                    }
                                    let file_name = dest
                                        .as_path()
                                        .strip_prefix(&root_path)
                                        .unwrap()
                                        .to_path_buf();
                                    debug!("filename = {:?}", file_name);
                                    drop(index_lock);
                                    drop(hash_indexes_lock);
                                    if let Some(env) = add_path_to_index(
                                        &hash_indexes,
                                        folder_hash,
                                        dest.as_path(),
                                        &cache_dir,
                                        file_name,
                                    ) {
                                        folder_index.lock().unwrap().insert(env.hash(),folder_hash);
                                        debug!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            dest.display()
                                        );
                                        sender.send(RefreshEvent {
                                            hash: folder_hash,
                                            kind: Create(Box::new(env)),
                                        });
                                        continue;
                                    } else {
                                        debug!("not valid email");
                                    }
                                } else {
                                    sender.send(RefreshEvent {
                                        hash: get_path_hash!(dest),
                                        kind: Rename(old_hash, new_hash),
                                    });
                                    debug!("contains_new_key");
                                }

                                /* Maybe a re-read should be triggered here just to be safe.
                                sender.send(RefreshEvent {
                                    hash: get_path_hash!(dest),
                                    kind: Rescan,
                                });
                                */
                            }
                            /* Trigger rescan of folder */
                            DebouncedEvent::Rescan => {
                                /* Actually should rescan all folders */
                                unreachable!("Unimplemented: rescan of all folders in MaildirType")
                            }
                            _ => {}
                        },
                        Err(e) => debug!("watch error: {:?}", e),
                    }
                }
            })?;
        Ok(handle.thread().id())
    }

    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        Box::new(MaildirOp::new(
            hash,
            self.hash_indexes.clone(),
            self.folder_index.lock().unwrap()[&hash],
        ))
    }

    fn save(&self, bytes: &[u8], folder: &str, flags: Option<Flag>) -> Result<()> {
        for f in self.folders.values() {
            if f.name == folder || f.path.to_str().unwrap() == folder {
                return MaildirType::save_to_folder(f.fs_path.clone(), bytes, flags);
            }
        }

        Err(MeliError::new(format!(
            "'{}' is not a valid folder.",
            folder
        )))
    }

    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }
}

impl MaildirType {
    pub fn new(
        settings: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool>,
    ) -> Result<Box<dyn MailBackend>> {
        let mut folders: FnvHashMap<FolderHash, MaildirFolder> = Default::default();
        fn recurse_folders<P: AsRef<Path>>(
            folders: &mut FnvHashMap<FolderHash, MaildirFolder>,
            settings: &AccountSettings,
            p: P,
        ) -> Result<Vec<FolderHash>> {
            if !p.as_ref().exists() || !p.as_ref().is_dir() {
                return Err(MeliError::new(format!(
                    "Configuration error: Path \"{}\" {}",
                    p.as_ref().display(),
                    if !p.as_ref().exists() {
                        "does not exist."
                    } else {
                        "is not a directory."
                    }
                )));
            }
            let mut children = Vec::new();
            for mut f in fs::read_dir(p).unwrap() {
                'entries: for f in f.iter_mut() {
                    {
                        let path = f.path();
                        if path.ends_with("cur") || path.ends_with("new") || path.ends_with("tmp") {
                            continue 'entries;
                        }
                        if path.is_dir() {
                            if let Ok(mut f) = MaildirFolder::new(
                                path.to_str().unwrap().to_string(),
                                path.file_name().unwrap().to_str().unwrap().to_string(),
                                None,
                                Vec::new(),
                                &settings,
                            ) {
                                f.children = recurse_folders(folders, settings, &path)?;
                                f.children
                                    .iter()
                                    .map(|c| folders.get_mut(c).map(|f| f.parent = Some(f.hash)))
                                    .count();
                                children.push(f.hash);
                                folders.insert(f.hash, f);
                            }
                        }
                    }
                }
            }
            Ok(children)
        };
        let root_path = PathBuf::from(settings.root_folder()).expand();
        if !root_path.exists() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a valid directory.",
                settings.name(),
                settings.root_folder.as_str()
            )));
        } else if !root_path.is_dir() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a directory.",
                settings.name(),
                settings.root_folder.as_str()
            )));
        }

        if let Ok(f) = MaildirFolder::new(
            root_path.to_str().unwrap().to_string(),
            root_path.file_name().unwrap().to_str().unwrap().to_string(),
            None,
            Vec::with_capacity(0),
            settings,
        ) {
            folders.insert(f.hash, f);
        }

        if folders.is_empty() {
            let children = recurse_folders(&mut folders, settings, &root_path)?;
            children
                .iter()
                .map(|c| folders.get_mut(c).map(|f| f.parent = None))
                .count();
        } else {
            let root_hash = *folders.keys().nth(0).unwrap();
            let children = recurse_folders(&mut folders, settings, &root_path)?;
            children
                .iter()
                .map(|c| folders.get_mut(c).map(|f| f.parent = Some(root_hash)))
                .count();
            folders.get_mut(&root_hash).map(|f| f.children = children);
        }
        folders.retain(|_, f| is_subscribed(f.path()));
        let keys = folders.keys().cloned().collect::<FnvHashSet<FolderHash>>();
        for f in folders.values_mut() {
            f.children.retain(|c| keys.contains(c));
        }

        let mut hash_indexes =
            FnvHashMap::with_capacity_and_hasher(folders.len(), Default::default());
        for &fh in folders.keys() {
            hash_indexes.insert(
                fh,
                HashIndex {
                    index: FnvHashMap::with_capacity_and_hasher(0, Default::default()),
                    hash: fh,
                },
            );
        }
        Ok(Box::new(MaildirType {
            name: settings.name().to_string(),
            folders,
            hash_indexes: Arc::new(Mutex::new(hash_indexes)),
            folder_index: Default::default(),
            path: root_path,
        }))
    }
    fn owned_folder_idx(&self, folder: &Folder) -> FolderHash {
        *self
            .folders
            .iter()
            .find(|(_, f)| f.hash() == folder.hash())
            .unwrap()
            .0
    }

    pub fn multicore(&mut self, cores: usize, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();

        let handle = {
            let tx = w.tx();
            // TODO: Avoid clone
            let folder: &MaildirFolder = &self.folders[&self.owned_folder_idx(folder)];
            let folder_hash = folder.hash();
            let tx_final = w.tx();
            let path: PathBuf = folder.fs_path().into();
            let name = format!("parsing {:?}", folder.name());
            let root_path = self.path.to_path_buf();
            let map = self.hash_indexes.clone();
            let folder_index = self.folder_index.clone();

            let closure = move |work_context: crate::async_workers::WorkContext| {
                let name = name.clone();
                work_context
                    .set_name
                    .send((std::thread::current().id(), name.clone()))
                    .unwrap();
                let root_path = root_path.clone();
                let map = map.clone();
                let folder_index = folder_index.clone();
                let tx = tx.clone();
                let cache_dir = cache_dir.clone();
                let path = path.clone();
                let thunk = move || {
                    let mut path = path.clone();
                    let cache_dir = cache_dir.clone();
                    path.push("new");
                    for d in path.read_dir()? {
                        if let Ok(p) = d {
                            move_to_cur(p.path()).ok().take();
                        }
                    }
                    path.pop();

                    path.push("cur");
                    let iter = path.read_dir()?;
                    let count = path.read_dir()?.count();
                    let mut files: Vec<PathBuf> = Vec::with_capacity(count);
                    let mut ret = Vec::with_capacity(count);
                    for e in iter {
                        let e = e.and_then(|x| {
                            let path = x.path();
                            Ok(path)
                        })?;
                        files.push(e);
                    }
                    if !files.is_empty() {
                        crossbeam::scope(|scope| {
                            let mut threads = Vec::with_capacity(cores);
                            let cache_dir = cache_dir.clone();
                            let chunk_size = if count / cores > 0 {
                                count / cores
                            } else {
                                count
                            };
                            for chunk in files.chunks(chunk_size) {
                                let cache_dir = cache_dir.clone();
                                let tx = tx.clone();
                                let map = map.clone();
                                let folder_index = folder_index.clone();
                                let root_path = root_path.clone();
                                let s = scope.builder().name(name.clone()).spawn(move |_| {
                                    let len = chunk.len();
                                    let size = if len <= 100 { 100 } else { (len / 100) * 100 };
                                    let mut local_r: Vec<Envelope> =
                                        Vec::with_capacity(chunk.len());
                                    for c in chunk.chunks(size) {
                                        //thread::yield_now();
                                        let map = map.clone();
                                        let folder_index = folder_index.clone();
                                        let len = c.len();
                                        for file in c {
                                            /* Check if we have a cache file with this email's
                                             * filename */
                                            let file_name = PathBuf::from(file)
                                                .strip_prefix(&root_path)
                                                .unwrap()
                                                .to_path_buf();
                                            if let Some(cached) =
                                                cache_dir.find_cache_file(&file_name)
                                            {
                                                /* Cached struct exists, try to load it */
                                                let reader = io::BufReader::new(
                                                    fs::File::open(&cached).unwrap(),
                                                );
                                                let result: result::Result<Envelope, _> =
                                                    bincode::deserialize_from(reader);
                                                if let Ok(env) = result {
                                                    let mut map = map.lock().unwrap();
                                                    let map = map.entry(folder_hash).or_default();
                                                    let hash = env.hash();
                                                    map.insert(hash, file.clone().into());
                                                    folder_index
                                                        .lock()
                                                        .unwrap()
                                                        .insert(hash, folder_hash);
                                                    local_r.push(env);
                                                    continue;
                                                }
                                            };
                                            let hash = get_file_hash(file);
                                            {
                                                let mut map = map.lock().unwrap();
                                                let map = map.entry(folder_hash).or_default();
                                                (*map).insert(hash, PathBuf::from(file).into());
                                            }
                                            let op = Box::new(MaildirOp::new(
                                                hash,
                                                map.clone(),
                                                folder_hash,
                                            ));
                                            if let Some(e) = Envelope::from_token(op, hash) {
                                                folder_index
                                                    .lock()
                                                    .unwrap()
                                                    .insert(e.hash(), folder_hash);
                                                if let Ok(cached) =
                                                    cache_dir.place_cache_file(file_name)
                                                {
                                                    /* place result in cache directory */
                                                    let f = match fs::File::create(cached) {
                                                        Ok(f) => f,
                                                        Err(e) => {
                                                            panic!("{}", e);
                                                        }
                                                    };
                                                    let metadata = f.metadata().unwrap();
                                                    let mut permissions = metadata.permissions();

                                                    permissions.set_mode(0o600); // Read/write for owner only.
                                                    f.set_permissions(permissions).unwrap();

                                                    let writer = io::BufWriter::new(f);
                                                    bincode::serialize_into(writer, &e).unwrap();
                                                }
                                                local_r.push(e);
                                            } else {
                                                debug!(
                                                    "DEBUG: hash {}, path: {} couldn't be parsed",
                                                    hash,
                                                    file.as_path().display()
                                                );
                                                continue;
                                            }
                                        }
                                        tx.send(AsyncStatus::ProgressReport(len)).unwrap();
                                    }
                                    local_r
                                });
                                threads.push(s.unwrap());
                            }
                            for t in threads {
                                let mut result = t.join().unwrap();
                                ret.append(&mut result);
                                work_context
                                    .set_status
                                    .send((
                                        std::thread::current().id(),
                                        format!("parsing.. {}/{}", ret.len(), files.len()),
                                    ))
                                    .unwrap();
                            }
                        })
                        .unwrap();
                    }
                    Ok(ret)
                };
                let result = thunk();
                tx_final.send(AsyncStatus::Payload(result)).unwrap();
                tx_final.send(AsyncStatus::Finished).unwrap();
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    pub fn save_to_folder(mut path: PathBuf, bytes: &[u8], flags: Option<Flag>) -> Result<()> {
        path.push("cur");
        {
            let mut rand_buf = [0u8; 16];
            let mut f =
                fs::File::open("/dev/urandom").expect("Could not open /dev/urandom for reading");
            f.read_exact(&mut rand_buf)
                .expect("Could not read from /dev/urandom/");
            let mut hostn_buf = String::with_capacity(256);
            let mut f =
                fs::File::open("/etc/hostname").expect("Could not open /etc/hostname for reading");
            f.read_to_string(&mut hostn_buf)
                .expect("Could not read from /etc/hostname");
            let timestamp = std::time::SystemTime::now()
                .duration_since(std::time::SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_millis();
            let mut filename = format!(
                "{}.{:x}_{}.{}:2,",
                timestamp,
                u128::from_be_bytes(rand_buf),
                std::process::id(),
                hostn_buf.trim()
            );
            if let Some(flags) = flags {
                if !(flags & Flag::DRAFT).is_empty() {
                    filename.push('D');
                }
                if !(flags & Flag::FLAGGED).is_empty() {
                    filename.push('F');
                }
                if !(flags & Flag::PASSED).is_empty() {
                    filename.push('P');
                }
                if !(flags & Flag::REPLIED).is_empty() {
                    filename.push('R');
                }
                if !(flags & Flag::SEEN).is_empty() {
                    filename.push('S');
                }
                if !(flags & Flag::TRASHED).is_empty() {
                    filename.push('T');
                }
            }
            path.push(filename);
        }
        debug!("saving at {}", path.display());
        let file = fs::File::create(path).unwrap();
        let metadata = file.metadata()?;
        let mut permissions = metadata.permissions();

        permissions.set_mode(0o600); // Read/write for owner only.
        file.set_permissions(permissions)?;

        let mut writer = io::BufWriter::new(file);
        writer.write_all(bytes).unwrap();
        return Ok(());
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        let root_path = PathBuf::from(s.root_folder()).expand();
        if !root_path.exists() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a valid directory.",
                s.name(),
                s.root_folder.as_str()
            )));
        } else if !root_path.is_dir() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a directory.",
                s.name(),
                s.root_folder.as_str()
            )));
        }

        Ok(())
    }
}

fn add_path_to_index(
    hash_index: &HashIndexes,
    folder_hash: FolderHash,
    path: &Path,
    cache_dir: &xdg::BaseDirectories,
    file_name: PathBuf,
) -> Option<Envelope> {
    let env: Envelope;
    debug!("add_path_to_index path {:?} filename{:?}", path, file_name);
    let hash = get_file_hash(path);
    {
        let mut map = hash_index.lock().unwrap();
        let map = map.entry(folder_hash).or_default();
        map.insert(hash, path.to_path_buf().into());
        debug!(
            "inserted {} in {} map, len={}",
            hash,
            folder_hash,
            map.len()
        );
    }
    let op = Box::new(MaildirOp::new(hash, hash_index.clone(), folder_hash));
    if let Some(e) = Envelope::from_token(op, hash) {
        debug!("add_path_to_index gen {}\t{}", hash, file_name.display());
        if let Ok(cached) = cache_dir.place_cache_file(file_name) {
            debug!("putting in cache");
            /* place result in cache directory */
            let f = match fs::File::create(cached) {
                Ok(f) => f,
                Err(e) => {
                    panic!("{}", e);
                }
            };
            let metadata = f.metadata().unwrap();
            let mut permissions = metadata.permissions();

            permissions.set_mode(0o600); // Read/write for owner only.
            f.set_permissions(permissions).unwrap();
            let writer = io::BufWriter::new(f);
            bincode::serialize_into(writer, &e).unwrap();
        }
        env = e;
    } else {
        debug!(
            "DEBUG: hash {}, path: {} couldn't be parsed in `add_path_to_index`",
            hash,
            path.display()
        );
        return None;
    }
    Some(env)
}
