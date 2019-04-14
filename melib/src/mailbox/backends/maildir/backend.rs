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
use crate::error::{MeliError, Result};
use crate::mailbox::email::{Envelope, EnvelopeHash};

use notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
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
pub struct HashIndex {
    index: FnvHashMap<EnvelopeHash, PathBuf>,
    hash: FolderHash,
}

impl Deref for HashIndex {
    type Target = FnvHashMap<EnvelopeHash, PathBuf>;
    fn deref(&self) -> &FnvHashMap<EnvelopeHash, PathBuf> {
        &self.index
    }
}

impl DerefMut for HashIndex {
    fn deref_mut(&mut self) -> &mut FnvHashMap<EnvelopeHash, PathBuf> {
        &mut self.index
    }
}

pub type HashIndexes = Arc<Mutex<FnvHashMap<FolderHash, HashIndex>>>;

/// Maildir backend https://cr.yp.to/proto/maildir.html
#[derive(Debug)]
pub struct MaildirType {
    name: String,
    folders: Vec<MaildirFolder>,
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
    let file_name = p.file_name().unwrap();
    new.pop();
    new.pop();

    new.push("cur");
    new.push(file_name);
    new.set_extension(":2,");
    if cfg!(debug_assertions) {
        eprint!("{}:{}_{}:	", file!(), line!(), column!());
        eprintln!("moved to cur: {}", new.display());
    }
    fs::rename(p, &new).unwrap();
    new
}

impl MailBackend for MaildirType {
    fn folders(&self) -> FnvHashMap<FolderHash, Folder> {
        self.folders.iter().map(|f| (f.hash(), f.clone())).collect()
    }
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        self.multicore(4, folder)
    }
    fn watch(&self, sender: RefreshEventConsumer) -> Result<()> {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(2)).unwrap();
        let root_path = self.path.to_path_buf();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        for f in &self.folders {
            if f.is_valid().is_err() {
                continue;
            }
            if cfg!(debug_assertions) {
                eprint!("{}:{}_{}:	", file!(), line!(), column!());
                eprintln!("watching {:?}", f);
            }
            let mut p = PathBuf::from(&f.path);
            p.push("cur");
            eprint!("{}:{}_{}:	", file!(), line!(), column!());
            eprintln!("watching {:?}", p);
            watcher.watch(&p, RecursiveMode::NonRecursive).unwrap();
            p.pop();
            p.push("new");
            eprint!("{}:{}_{}:	", file!(), line!(), column!());
            eprintln!("watching {:?}", p);
            watcher.watch(&p, RecursiveMode::NonRecursive).unwrap();
        }
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
                            eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("DebouncedEvent::Create(path = {:?}", pathbuf);
                                if path_is_new!(pathbuf) {
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("path_is_new");
                                /* This creates a Rename event that we will receive later */
                                    pathbuf = move_to_cur(pathbuf);
                                }
                                let folder_hash = get_path_hash!(pathbuf);
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("cilename is {:?}", file_name);
                                if let Some(env) = add_path_to_index(
                                    &hash_indexes,
                                    folder_hash,
                                    pathbuf.as_path(),
                                    &cache_dir,
                                    file_name,
                                ) {
                                    if cfg!(debug_assertions) {
                                        eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("Create event {} {} {}", env.hash(), env.subject(), pathbuf.display());
                                    }
                                    sender.send(RefreshEvent {
                                        hash: folder_hash,
                                        kind: Create(Box::new(env)),
                                    });
                                }
                            }
                            /* Update */
                            DebouncedEvent::NoticeWrite(pathbuf)
                            | DebouncedEvent::Write(pathbuf) => {
                            eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("DebouncedEvent::Write(path = {:?}", pathbuf);
                                let folder_hash = get_path_hash!(pathbuf);
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = &mut hash_indexes_lock.entry(folder_hash).or_default();
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                /* Linear search in hash_index to find old hash */
                                let old_hash: EnvelopeHash = {
                                    if let Some((k, v)) =
                                        index_lock.iter_mut().find(|(_, v)| **v == pathbuf)
                                    {
                                        //TODO FIXME This doesn't make sense?
                                        *v = pathbuf.clone();
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
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("write notice");
                                    let op = Box::new(MaildirOp::new(new_hash, hash_indexes.clone(), folder_hash));
                                    if let Some(env) = Envelope::from_token(op, new_hash) {
                                        if cfg!(debug_assertions) {
                                            eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("{}\t{}", new_hash, pathbuf.display());
                                        }
                                        index_lock.insert(new_hash, pathbuf);

                                        /* Send Write notice */

                                        sender.send(RefreshEvent {
                                            hash: folder_hash,
                                            kind: Update(old_hash, Box::new(env)),
                                        });
                                    } else if cfg!(debug_assertions) {
                                        eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("DEBUG: hash {}, path: {} couldn't be parsed in `add_path_to_index`", new_hash, pathbuf.as_path().display());
                                    }
                                }
                            }
                            /* Remove */
                            DebouncedEvent::NoticeRemove(pathbuf)
                            | DebouncedEvent::Remove(pathbuf) => {
                                eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("DebouncedEvent::Remove(path = {:?}", pathbuf);
                                let folder_hash = get_path_hash!(pathbuf);
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(folder_hash).or_default();
                                let hash: EnvelopeHash = if let Some((k, _)) =
                                    index_lock.iter().find(|(_, v)| **v == pathbuf)
                                {
                                    *k
                                } else {
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("removed but not contained in index");
                                    continue;
                                };
                                index_lock.remove(&hash);

                                sender.send(RefreshEvent {
                                    hash: folder_hash,
                                    kind: Remove(hash),
                                });
                            }
                            /* Envelope hasn't changed */
                            DebouncedEvent::Rename(src, dest) => {
                                eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("DebouncedEvent::Rename(src = {:?}, dest = {:?})", src, dest);
                                let folder_hash = get_path_hash!(src);
                                let old_hash: EnvelopeHash = get_file_hash(src.as_path());
                                let new_hash: EnvelopeHash = get_file_hash(dest.as_path());

                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(folder_hash).or_default();

                                if index_lock.contains_key(&old_hash) {
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("contains_key");
                                    sender.send(RefreshEvent {
                                        hash: get_path_hash!(dest),
                                        kind: Rename(old_hash, new_hash),
                                    });
                                    index_lock.remove(&old_hash);
                                    index_lock.insert(new_hash, dest);
                                    continue;
                                } else if !index_lock.contains_key(&new_hash) {
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
                                    eprintln!("not contains_key");
                                    let file_name = dest
                                        .as_path()
                                        .strip_prefix(&root_path)
                                        .unwrap()
                                        .to_path_buf();
                                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
                                    eprintln!("filename = {:?}", file_name);
                                    if let Some(env) = add_path_to_index(
                                        &hash_indexes,
                                        folder_hash,
                                        dest.as_path(),
                                        &cache_dir,
                                        file_name,
                                        ) {
                                        eprint!("{}:{}_{}:	", file!(), line!(), column!());
                                        eprintln!("Create event {} {} {}", env.hash(), env.subject(), dest.display());
                                        if cfg!(debug_assertions) {
                                        }
                                        sender.send(RefreshEvent {
                                            hash: folder_hash,
                                            kind: Create(Box::new(env)),
                                        });
                                        continue;
                                    } else {
                                        eprint!("{}:{}_{}:	", file!(), line!(), column!());
                                        eprintln!("not valid email");
                                    }
                                }
                                /* Maybe a re-read should be triggered here just to be safe. */
                                sender.send(RefreshEvent {
                                    hash: get_path_hash!(dest),
                                    kind: Rescan,
                                });
                            }
                            /* Trigger rescan of folder */
                            DebouncedEvent::Rescan => {
                                /* Actually should rescan all folders */
                                unreachable!("Unimplemented: rescan of all folders in MaildirType")
                            }
                            _ => {}
                        },
                        Err(e) => {eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("watch error: {:?}", e)
                        }
                    }
                }
            })?;
        Ok(())
    }

    fn operation(&self, hash: EnvelopeHash, folder_hash: FolderHash) -> Box<BackendOp> {
        Box::new(MaildirOp::new(hash, self.hash_indexes.clone(), folder_hash))
    }

    fn save(&self, bytes: &[u8], folder: &str) -> Result<()> {
        for f in &self.folders {
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
                if cfg!(debug_assertions) {
                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
                    eprintln!("saving at {}", path.display());
                }
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
        let mut folders: Vec<MaildirFolder> = Vec::new();
        fn recurse_folders<P: AsRef<Path>>(folders: &mut Vec<MaildirFolder>, p: P) -> Vec<usize> {
            let mut children = Vec::new();
            for mut f in fs::read_dir(p).unwrap() {
                for f in f.iter_mut() {
                    {
                        let path = f.path();
                        if path.ends_with("cur") || path.ends_with("new") || path.ends_with("tmp") {
                            continue;
                        }
                        if path.is_dir() {
                            let path_children = recurse_folders(folders, &path);
                            if let Ok(f) = MaildirFolder::new(
                                path.to_str().unwrap().to_string(),
                                path.file_name().unwrap().to_str().unwrap().to_string(),
                                path_children,
                            ) {
                                folders.push(f);
                                children.push(folders.len() - 1);
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
                Vec::with_capacity(0),
            ) {
                if f.is_valid().is_ok() {
                    folders.push(f);
                }
            }
        }

        if folders.is_empty() {
            recurse_folders(&mut folders, &path);
        } else {
            folders[0].children = recurse_folders(&mut folders, &path);
        }

        let hash_indexes = Arc::new(Mutex::new(FnvHashMap::with_capacity_and_hasher(
            folders.len(),
            Default::default(),
        )));
        {
            let mut hash_indexes = hash_indexes.lock().unwrap();
            for f in &folders {
                hash_indexes.insert(
                    f.hash(),
                    HashIndex {
                        index: FnvHashMap::with_capacity_and_hasher(0, Default::default()),
                        hash: f.hash(),
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
    fn owned_folder_idx(&self, folder: &Folder) -> usize {
        self.folders
            .iter()
            .enumerate()
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
            let folder: &MaildirFolder = &self.folders[self.owned_folder_idx(folder)];
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
                                    let mut local_r: Vec<
                                        Envelope,
                                        > = Vec::with_capacity(chunk.len());
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
                                                    let result: result::Result<Envelope, _> = bincode::deserialize_from(reader);
                                                    if let Ok(env) = result {
                                                        let mut map = map.lock().unwrap();
                                                        let map = map.entry(folder_hash).or_default();;
                                                        let hash = env.hash();
                                                        map.insert(hash, file.clone());
                                                        local_r.push(env);
                                                        continue;
                                                    }
                                                };
                                            let hash = get_file_hash(file);
                                            {
                                                let mut map = map.lock().unwrap();
                                                let map = map.entry(folder_hash).or_default();
                                                (*map).insert(hash, PathBuf::from(file));
                                            }
                                            let op =
                                                Box::new(MaildirOp::new(hash, map.clone(), folder_hash));
                                            if let Some(e) = Envelope::from_token(op, hash)
                                            {
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
                                                        bincode::serialize_into(writer, &e)
                                                            .unwrap();
                                                    }
                                                local_r.push(e);
                                            } else {
                                                if cfg!(debug_assertions) {
eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("DEBUG: hash {}, path: {} couldn't be parsed in `add_path_to_index`", hash, file.as_path().display());
}
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
    eprint!("{}:{}_{}:	", file!(), line!(), column!());
    eprintln!("add_path_to_index path {:?} filename{:?}", path, file_name);
    let hash = get_file_hash(path);
    {
        let mut map = hash_index.lock().unwrap();
        let map = map.entry(folder_hash).or_default();;
        map.insert(hash, path.to_path_buf());
        eprint!("{}:{}_{}:	", file!(), line!(), column!());
        eprintln!(
            "inserted {} in {} map, len={}",
            hash,
            folder_hash,
            map.len()
        );
    }
    let op = Box::new(MaildirOp::new(hash, hash_index.clone(), folder_hash));
    if let Some(e) = Envelope::from_token(op, hash) {
        eprint!("{}:{}_{}:	", file!(), line!(), column!());
        eprintln!("add_path_to_index gen {}\t{}", hash, file_name.display());
        if let Ok(cached) = cache_dir.place_cache_file(file_name) {
            eprint!("{}:{}_{}:	", file!(), line!(), column!());
            eprintln!("putting in cache");
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
        eprint!("{}:{}_{}:	", file!(), line!(), column!());
        eprintln!(
            "DEBUG: hash {}, path: {} couldn't be parsed in `add_path_to_index`",
            hash,
            path.display()
        );
        return None;
    }
    Some(env)
}
