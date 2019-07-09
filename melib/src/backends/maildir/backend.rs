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
use crate::email::{Envelope, EnvelopeHash};
use crate::error::{MeliError, Result};

extern crate notify;
use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::time::Duration;

use std::sync::mpsc::channel;
//use std::sync::mpsc::sync_channel;
//use std::sync::mpsc::SyncSender;
//use std::time::Duration;
use fnv::{FnvHashMap, FnvHasher};
use std::collections::hash_map::DefaultHasher;
use std::ffi::OsStr;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::{self, Read, Write};
use std::ops::{Deref, DerefMut};
use std::path::{Component, Path, PathBuf};
use std::result;
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Debug, Default)]
pub struct MaildirPath {
    pub(super) buf: PathBuf,
    pub(super) modified: Option<PathBuf>,
}

impl Deref for MaildirPath {
    type Target = PathBuf;
    fn deref(&self) -> &PathBuf {
        &self.buf
    }
}

impl DerefMut for MaildirPath {
    fn deref_mut(&mut self) -> &mut PathBuf {
        &mut self.buf
    }
}

impl From<PathBuf> for MaildirPath {
    fn from(val: PathBuf) -> MaildirPath {
        MaildirPath {
            buf: val,
            modified: None,
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
    //folder_index: FnvHashMap<FolderHash, usize>,
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

fn move_to_cur(p: PathBuf) -> PathBuf {
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
    fs::rename(&p, &new).unwrap();
    new
}

impl MailBackend for MaildirType {
    fn folders(&self) -> FnvHashMap<FolderHash, Folder> {
        self.folders
            .iter()
            .map(|(h, f)| (*h, f.clone() as Folder))
            .collect()
    }
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        self.multicore(4, folder)
    }
    fn watch(&self, sender: RefreshEventConsumer) -> Result<()> {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(2)).unwrap();
        let root_path = self.path.to_path_buf();
        watcher.watch(&root_path, RecursiveMode::Recursive).unwrap();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        debug!("watching {:?}", root_path);
        let hash_indexes = self.hash_indexes.clone();
        thread::Builder::new()
            .name("folder watch".to_string())
            .spawn(move || {
                // Move `watcher` in the closure's scope so that it doesn't get dropped.
                let _watcher = watcher;
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
                                    pathbuf = move_to_cur(pathbuf);
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
                                        /* Did we just miss a Create event? In any case, create
                                         * envelope. */
                                        if let Some(env) = add_path_to_index(
                                            &hash_indexes,
                                            folder_hash,
                                            pathbuf.as_path(),
                                            &cache_dir,
                                            file_name,
                                        ) {
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
                                if let Some(path) = &index_lock[&hash].modified {
                                    debug!(
                                        "envelope {} has modified path set {}",
                                        hash,
                                        path.display()
                                    );
                                    continue;
                                }

                                index_lock.remove(&hash);

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

                                if index_lock.contains_key(&old_hash) {
                                    debug!("contains_old_key");
                                    sender.send(RefreshEvent {
                                        hash: get_path_hash!(dest),
                                        kind: Rename(old_hash, new_hash),
                                    });
                                    index_lock.remove(&old_hash);
                                    index_lock.insert(new_hash, dest.into());
                                    continue;
                                } else if !index_lock.contains_key(&new_hash) {
                                    debug!("not contains_new_key");
                                    let file_name = dest
                                        .as_path()
                                        .strip_prefix(&root_path)
                                        .unwrap()
                                        .to_path_buf();
                                    debug!("filename = {:?}", file_name);
                                    if let Some(env) = add_path_to_index(
                                        &hash_indexes,
                                        folder_hash,
                                        dest.as_path(),
                                        &cache_dir,
                                        file_name,
                                    ) {
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
        Ok(())
    }

    fn operation(&self, hash: EnvelopeHash, folder_hash: FolderHash) -> Box<BackendOp> {
        Box::new(MaildirOp::new(hash, self.hash_indexes.clone(), folder_hash))
    }

    fn save(&self, bytes: &[u8], folder: &str) -> Result<()> {
        for f in self.folders.values() {
            if f.name == folder {
                let mut path = f.path.clone();
                path.push("cur");
                {
                    let mut rand_buf = [0u8; 16];
                    let mut f = fs::File::open("/dev/urandom")
                        .expect("Could not open /dev/urandom for reading");
                    f.read_exact(&mut rand_buf)
                        .expect("Could not read from /dev/urandom/");
                    let mut hostn_buf = String::with_capacity(256);
                    let mut f = fs::File::open("/etc/hostname")
                        .expect("Could not open /etc/hostname for reading");
                    f.read_to_string(&mut hostn_buf)
                        .expect("Could not read from /etc/hostname");
                    let timestamp = std::time::SystemTime::now()
                        .duration_since(std::time::SystemTime::UNIX_EPOCH)
                        .unwrap()
                        .as_millis();
                    path.push(&format!(
                        "{}.{:x}_{}.{}:2,",
                        timestamp,
                        u128::from_be_bytes(rand_buf),
                        std::process::id(),
                        hostn_buf.trim()
                    ));
                }
                debug!("saving at {}", path.display());
                let file = fs::File::create(path).unwrap();
                let mut writer = io::BufWriter::new(file);
                writer.write_all(bytes).unwrap();
                return Ok(());
            }
        }

        Err(MeliError::new(format!(
            "'{}' is not a valid folder.",
            folder
        )))
    }
}

impl MaildirType {
    pub fn new(f: &AccountSettings) -> Self {
        let mut folders: FnvHashMap<FolderHash, MaildirFolder> = Default::default();
        fn recurse_folders<P: AsRef<Path>>(
            folders: &mut FnvHashMap<FolderHash, MaildirFolder>,
            p: P,
        ) -> Vec<FolderHash> {
            let mut children = Vec::new();
            for mut f in fs::read_dir(p).unwrap() {
                'entries: for f in f.iter_mut() {
                    {
                        let path = f.path();
                        if path.ends_with("cur") || path.ends_with("new") || path.ends_with("tmp") {
                            continue 'entries;
                        }
                        if path.is_dir() {
                            let path_children = recurse_folders(folders, &path);
                            if let Ok(f) = MaildirFolder::new(
                                path.to_str().unwrap().to_string(),
                                path.file_name().unwrap().to_str().unwrap().to_string(),
                                None,
                                path_children,
                            ) {
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
            children
        };
        let path = PathBuf::from(f.root_folder());
        if path.is_dir() {
            if let Ok(f) = MaildirFolder::new(
                path.to_str().unwrap().to_string(),
                path.file_name().unwrap().to_str().unwrap().to_string(),
                None,
                Vec::with_capacity(0),
            ) {
                let l: MaildirFolder = f;
                folders.insert(l.hash, l);
            }
        }

        if folders.is_empty() {
            let children = recurse_folders(&mut folders, &path);
            children
                .iter()
                .map(|c| folders.get_mut(c).map(|f| f.parent = None))
                .count();
        } else {
            let root_hash = *folders.keys().nth(0).unwrap();
            let children = recurse_folders(&mut folders, &path);
            children
                .iter()
                .map(|c| folders.get_mut(c).map(|f| f.parent = Some(root_hash)))
                .count();
            folders.get_mut(&root_hash).map(|f| f.children = children);
        }

        let hash_indexes = Arc::new(Mutex::new(FnvHashMap::with_capacity_and_hasher(
            folders.len(),
            Default::default(),
        )));
        {
            let mut hash_indexes = hash_indexes.lock().unwrap();
            for &fh in folders.keys() {
                hash_indexes.insert(
                    fh,
                    HashIndex {
                        index: FnvHashMap::with_capacity_and_hasher(0, Default::default()),
                        hash: fh,
                    },
                );
            }
        }
        MaildirType {
            name: f.name().to_string(),
            folders,
            hash_indexes,
            path: PathBuf::from(f.root_folder()),
        }
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
            let path: PathBuf = folder.path().into();
            let name = format!("parsing {:?}", folder.name());
            let root_path = self.path.to_path_buf();
            let map = self.hash_indexes.clone();

            let closure = move || {
                let name = name.clone();
                let root_path = root_path.clone();
                let map = map.clone();
                let tx = tx.clone();
                let cache_dir = cache_dir.clone();
                let path = path.clone();
                let thunk = move || {
                    let mut path = path.clone();
                    let cache_dir = cache_dir.clone();
                    path.push("new");
                    for d in path.read_dir()? {
                        if let Ok(p) = d {
                            move_to_cur(p.path());
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
                    let mut threads = Vec::with_capacity(cores);
                    if !files.is_empty() {
                        crossbeam::scope(|scope| {
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
                                let root_path = root_path.clone();
                                let s = scope.builder().name(name.clone()).spawn(move || {
                                    let len = chunk.len();
                                    let size = if len <= 100 { 100 } else { (len / 100) * 100 };
                                    let mut local_r: Vec<Envelope> =
                                        Vec::with_capacity(chunk.len());
                                    for c in chunk.chunks(size) {
                                        //thread::yield_now();
                                        let map = map.clone();
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
                                        tx.send(AsyncStatus::ProgressReport(len));
                                    }
                                    local_r
                                });
                                threads.push(s.unwrap());
                            }
                        });
                    }
                    for t in threads {
                        let mut result = t.join();
                        ret.append(&mut result);
                    }
                    Ok(ret)
                };
                let result = thunk();
                tx_final.send(AsyncStatus::Payload(result));
            };
            Box::new(closure)
        };
        w.build(handle)
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
        let map = map.entry(folder_hash).or_default();;
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
