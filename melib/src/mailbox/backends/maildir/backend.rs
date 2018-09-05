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

extern crate bincode;
extern crate fnv;
extern crate notify;
extern crate xdg;

use super::{MaildirFolder, MaildirOp};
use async::*;
use conf::AccountSettings;
use error::Result;
use mailbox::backends::{
    BackendFolder, BackendOp, Folder, MailBackend, RefreshEvent, RefreshEventConsumer,
    RefreshEventKind::*,
};
use mailbox::email::{Envelope, EnvelopeHash};

use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::time::Duration;

use std::sync::mpsc::channel;
//use std::sync::mpsc::sync_channel;
//use std::sync::mpsc::SyncSender;
//use std::time::Duration;
use std::thread;
extern crate crossbeam;
use self::fnv::{FnvHashMap, FnvHasher};
use std::collections::hash_map::DefaultHasher;
use std::ffi::OsStr;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::path::{Component, Path, PathBuf};
use std::result;
use std::sync::{Arc, Mutex};

type HashIndex = Arc<Mutex<FnvHashMap<EnvelopeHash, (usize, PathBuf)>>>;

/// Maildir backend https://cr.yp.to/proto/maildir.html
#[derive(Debug)]
pub struct MaildirType {
    name: String,
    folders: Vec<MaildirFolder>,
    hash_index: HashIndex,

    path: PathBuf,
}

macro_rules! path_is_new {
    ($path:expr) => {
        if $path.is_dir() {
            false
        } else {
            let mut iter = $path.components().rev();
            iter.next();
            iter.next();
            iter.next() == Some(Component::Normal(OsStr::new("new")))
        }
    };
}
macro_rules! get_path_hash {
    ($path:expr) => {{
        if $path.is_dir() {
            if $path.ends_with("cur") | $path.ends_with("new") {
                $path.pop();
            }
        } else {
            $path.pop();
            $path.pop();
        };
        eprintln!(" got event in {}", $path.display());

        let mut hasher = DefaultHasher::new();
        $path.hash(&mut hasher);
        hasher.finish()
    }};
}

fn get_file_hash(file: &Path) -> EnvelopeHash {
    let mut buf = Vec::new();
    let mut f = fs::File::open(&file).unwrap_or_else(|_| panic!("Can't open {}", file.display()));
    f.read_to_end(&mut buf)
        .unwrap_or_else(|_| panic!("Can't read {}", file.display()));
    let mut hasher = FnvHasher::default();
    hasher.write(file.as_os_str().as_bytes());
    hasher.write(&buf);
    hasher.finish()
}

fn move_to_cur(p: PathBuf) {
    let mut new = p.clone();
    {
        let file_name = p.file_name().unwrap();
        new.pop();
        new.pop();

        new.push("cur");
        new.push(file_name);
    }
    fs::rename(p, new).unwrap();
}

impl MailBackend for MaildirType {
    fn folders(&self) -> Vec<Folder> {
        self.folders.iter().map(|f| f.clone()).collect()
    }
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        self.multicore(4, folder)
    }
    fn watch(&self, sender: RefreshEventConsumer) -> Result<()> {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(1)).unwrap();
        let root_path = self.path.to_path_buf();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        for f in &self.folders {
            if f.is_valid().is_err() {
                continue;
            }
            eprintln!("watching {:?}", f);
            let mut p = PathBuf::from(&f.path);
            p.push("cur");
            watcher.watch(&p, RecursiveMode::NonRecursive).unwrap();
            p.pop();
            p.push("new");
            watcher.watch(&p, RecursiveMode::NonRecursive).unwrap();
        }
        let map = self.hash_index.clone();
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
                                if path_is_new!(pathbuf) {
                                    move_to_cur(pathbuf);
                                    continue;
                                }
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                if let Some(env) = add_path_to_index(
                                    &map,
                                    pathbuf.as_path(),
                                    &cache_dir,
                                    file_name,
                                ) {
                                    sender.send(RefreshEvent {
                                        hash: get_path_hash!(pathbuf),
                                        kind: Create(env),
                                    });
                                } else {
                                    continue;
                                }
                            }
                            /* Update */
                            DebouncedEvent::NoticeWrite(mut pathbuf)
                            | DebouncedEvent::Write(mut pathbuf) => {
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                /* Linear search in hash_index to find old hash */
                                let old_hash: EnvelopeHash = {
                                    let mut map_lock = map.lock().unwrap();
                                    if let Some((k, v)) =
                                        map_lock.iter_mut().find(|(_, v)| v.1 == pathbuf)
                                    {
                                        v.1 = pathbuf.clone();
                                        *k
                                    } else {
                                        /* Did we just miss a Create event? In any case, create
                                         * envelope. */
                                        if let Some(env) = add_path_to_index(
                                            &map,
                                            pathbuf.as_path(),
                                            &cache_dir,
                                            file_name,
                                        ) {
                                            sender.send(RefreshEvent {
                                                hash: get_path_hash!(pathbuf),
                                                kind: Create(env),
                                            });
                                        }
                                        return;
                                    }
                                };
                                let new_hash: EnvelopeHash = get_file_hash(pathbuf.as_path());
                                let mut map_lock = map.lock().unwrap();
                                if map_lock.get_mut(&new_hash).is_none() {
                                    let op = Box::new(MaildirOp::new(new_hash, map.clone()));
                                    if let Some(env) = Envelope::from_token(op, new_hash) {
                                        map_lock.insert(new_hash, (0, pathbuf.clone()));

                                        /* Send Write notice */

                                        sender.send(RefreshEvent {
                                            hash: get_path_hash!(pathbuf),
                                            kind: Update(old_hash, env),
                                        });
                                    }
                                }
                            }
                            /* Remove */
                            DebouncedEvent::NoticeRemove(mut pathbuf)
                            | DebouncedEvent::Remove(mut pathbuf) => {
                                let map = map.lock().unwrap();
                                let hash: EnvelopeHash = if let Some((k, _)) =
                                    map.iter().find(|(_, v)| v.1 == pathbuf)
                                {
                                    *k
                                } else {
                                    continue;
                                };

                                sender.send(RefreshEvent {
                                    hash: get_path_hash!(pathbuf),
                                    kind: Remove(hash),
                                });
                            }
                            /* Envelope hasn't changed, so handle this here */
                            DebouncedEvent::Rename(_, mut dest) => {
                                let new_hash: EnvelopeHash = get_file_hash(dest.as_path());
                                let mut map = map.lock().unwrap();
                                if let Some(v) = map.get_mut(&new_hash) {
                                    v.1 = dest;
                                } else {
                                    /* Maybe a re-read should be triggered here just to be safe. */
                                    sender.send(RefreshEvent {
                                        hash: get_path_hash!(dest),
                                        kind: Rescan,
                                    });
                                }
                            }
                            /* Trigger rescan of folder */
                            DebouncedEvent::Rescan => {
                                /* Actually should rescan all folders */
                                unreachable!("Unimplemented: rescan of all folders in MaildirType")
                            }
                            _ => {}
                        },
                        Err(e) => eprintln!("watch error: {:?}", e),
                    }
                }
            })?;
        Ok(())
    }
    fn operation(&self, hash: EnvelopeHash) -> Box<BackendOp> {
        Box::new(MaildirOp::new(hash, self.hash_index.clone()))
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
                folders.push(f);
            }
        }
        folders[0].children = recurse_folders(&mut folders, &path);
        MaildirType {
            name: f.name().to_string(),
            folders,
            hash_index: Arc::new(Mutex::new(FnvHashMap::with_capacity_and_hasher(
                0,
                Default::default(),
            ))),
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
        let root_path = self.path.to_path_buf();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        let handle = {
            let tx = w.tx();
            // TODO: Avoid clone
            let folder: &MaildirFolder = &self.folders[self.owned_folder_idx(folder)];
            let mut path: PathBuf = folder.path().into();
            let name = format!("parsing {:?}", folder.name());
            let map = self.hash_index.clone();
            let map2 = self.hash_index.clone();

            thread::Builder::new()
                .name(name.clone())
                .spawn(move || {
                    let cache_dir = cache_dir.clone();
                    {
                        path.push("new");
                        for d in path.read_dir()? {
                            if let Ok(p) = d {
                                move_to_cur(p.path());
                            }
                        }
                        path.pop();
                    }
                    path.push("cur");
                    let iter = path.read_dir()?;
                    let count = path.read_dir()?.count();
                    let mut files: Vec<PathBuf> = Vec::with_capacity(count);
                    let mut r = Vec::with_capacity(count);
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
                                let mut tx = tx.clone();
                                let map = map.clone();
                                let root_path = root_path.clone();
                                let s = scope.builder().name(name.clone()).spawn(move || {
                                    let len = chunk.len();
                                    let size = if len <= 100 { 100 } else { (len / 100) * 100 };
                                    let mut local_r: Vec<
                                        Envelope,
                                    > = Vec::with_capacity(chunk.len());
                                    for c in chunk.chunks(size) {
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
                                                let reader = io::BufReader::new(
                                                    fs::File::open(cached).unwrap(),
                                                );
                                                let result: result::Result<Envelope, _> = bincode::deserialize_from(reader);
                                                if let Ok(env) = result {
                                                    let mut map = map.lock().unwrap();
                                                    let hash = env.hash();
                                                    if (*map).contains_key(&hash) {
                                                        continue;
                                                    }
                                                    (*map).insert(hash, (0, file.clone()));
                                                    local_r.push(env);
                                                    continue;
                                                }
                                            }
                                            {
                                                let hash = get_file_hash(file);
                                                {
                                                    let mut map = map.lock().unwrap();
                                                    if (*map).contains_key(&hash) {
                                                        continue;
                                                    }
                                                    (*map).insert(hash, (0, PathBuf::from(file)));
                                                }
                                                let op =
                                                    Box::new(MaildirOp::new(hash, map.clone()));
                                                if let Some(mut e) = Envelope::from_token(op, hash)
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
                                                    continue;
                                                }
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
                        r.append(&mut result);
                    }
                    let mut map = map2.lock().unwrap();
                    for (idx, e) in r.iter().enumerate() {
                        let mut y = (*map)[&e.hash()].clone();
                        y.0 = idx;
                        (*map).insert(e.hash(), y);
                    }
                    tx.send(AsyncStatus::Finished);
                    Ok(r)
                })
                .unwrap()
        };
        w.build(handle)
    }
}

fn add_path_to_index(
    map: &HashIndex,
    path: &Path,
    cache_dir: &xdg::BaseDirectories,
    file_name: PathBuf,
) -> Option<Envelope> {
    let env: Envelope;
    let hash = get_file_hash(path);
    {
        let mut map = map.lock().unwrap();
        if (*map).contains_key(&hash) {
            return None;
        }
        (*map).insert(hash, (0, path.to_path_buf()));
    }
    let op = Box::new(MaildirOp::new(hash, map.clone()));
    if let Some(e) = Envelope::from_token(op, hash) {
        if let Ok(cached) = cache_dir.place_cache_file(file_name) {
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
        return None;
    }
    Some(env)
}
