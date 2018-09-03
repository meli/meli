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
extern crate xdg;

use async::*;
use conf::AccountSettings;
use error::{MeliError, Result};
use mailbox::backends::{
    BackendFolder, BackendOp, Folder, MailBackend, RefreshEvent, RefreshEventConsumer,
};
use mailbox::email::parser;
use mailbox::email::{Envelope, Flag};

extern crate notify;

use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::time::Duration;

use std::sync::mpsc::channel;
//use std::sync::mpsc::sync_channel;
//use std::sync::mpsc::SyncSender;
//use std::time::Duration;
use std::thread;
extern crate crossbeam;
use memmap::{Mmap, Protection};
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::result;
use std::sync::{Arc, Mutex};
extern crate fnv;
use self::fnv::{FnvHashMap, FnvHasher};

/// `BackendOp` implementor for Maildir
#[derive(Debug)]
pub struct MaildirOp {
    hash_index: Arc<Mutex<FnvHashMap<u64, (usize, PathBuf)>>>,
    hash: u64,
    slice: Option<Mmap>,
}

impl Clone for MaildirOp {
    fn clone(&self) -> Self {
        MaildirOp {
            hash_index: self.hash_index.clone(),
            hash: self.hash,
            slice: None,
        }
    }
}

impl MaildirOp {
    pub fn new(hash: u64, hash_index: Arc<Mutex<FnvHashMap<u64, (usize, PathBuf)>>>) -> Self {
        MaildirOp {
            hash_index,
            hash,
            slice: None,
        }
    }
    fn path(&self) -> PathBuf {
        let hash_index = self.hash_index.clone();
        let map = hash_index.lock().unwrap();
        map.get(&self.hash).unwrap().1.clone()
    }
}

impl<'a> BackendOp for MaildirOp {
    fn description(&self) -> String {
        format!("Path of file: {}", self.path().display())
    }
    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.slice.is_none() {
            self.slice = Some(Mmap::open_path(self.path(), Protection::Read)?);
        }
        /* Unwrap is safe since we use ? above. */
        Ok(unsafe { self.slice.as_ref().unwrap().as_slice() })
    }
    fn fetch_headers(&mut self) -> Result<&[u8]> {
        let raw = self.as_bytes()?;
        let result = parser::headers_raw(raw).to_full_result()?;
        Ok(result)
    }
    fn fetch_body(&mut self) -> Result<&[u8]> {
        let raw = self.as_bytes()?;
        let result = parser::headers_raw(raw).to_full_result()?;
        Ok(result)
    }
    fn fetch_flags(&self) -> Flag {
        let mut flag = Flag::default();
        let path = self.path();
        let path = path.to_str().unwrap(); // Assume UTF-8 validity
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
                _ => panic!(),
            }
        }

        flag
    }

    fn set_flag(&mut self, envelope: &mut Envelope, f: &Flag) -> Result<()> {
        let path = self.path();
        let path = path.to_str().unwrap(); // Assume UTF-8 validity
        let idx: usize = path
            .rfind(":2,")
            .ok_or_else(|| MeliError::new(format!("Invalid email filename: {:?}", self)))?
            + 3;
        let mut new_name: String = path[..idx].to_string();
        let mut flags = self.fetch_flags();
        if !(flags & *f).is_empty() {
            return Ok(());
        }
        flags.toggle(*f);
        if !(flags & Flag::DRAFT).is_empty() {
            new_name.push('D');
        }
        if !(flags & Flag::FLAGGED).is_empty() {
            new_name.push('F');
        }
        if !(flags & Flag::PASSED).is_empty() {
            new_name.push('P');
        }
        if !(flags & Flag::REPLIED).is_empty() {
            new_name.push('R');
        }
        if !(flags & Flag::SEEN).is_empty() {
            new_name.push('S');
        }
        if !(flags & Flag::TRASHED).is_empty() {
            new_name.push('T');
        }

        fs::rename(&path, &new_name)?;
        let hash = envelope.hash();
        let hash_index = self.hash_index.clone();
        let mut map = hash_index.lock().unwrap();
        map.get_mut(&hash).unwrap().1 = PathBuf::from(new_name);
        Ok(())
    }
}

/// Maildir backend https://cr.yp.to/proto/maildir.html
#[derive(Debug)]
pub struct MaildirType {
    name: String,
    folders: Vec<MaildirFolder>,
    hash_index: Arc<Mutex<FnvHashMap<u64, (usize, PathBuf)>>>,

    path: PathBuf,
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
        thread::Builder::new()
            .name("folder watch".to_string())
            .spawn(move || {
                // Move `watcher` in the closure's scope so that it doesn't get dropped.
                let _watcher = watcher;
                loop {
                    match rx.recv() {
                        Ok(event) => match event {
                            DebouncedEvent::Create(mut pathbuf)
                            | DebouncedEvent::Remove(mut pathbuf) => {
                                if pathbuf.is_dir() {
                                    if pathbuf.ends_with("cur") | pathbuf.ends_with("new") {
                                        pathbuf.pop();
                                    }
                                } else {
                                    pathbuf.pop();
                                    pathbuf.pop();
                                };
                                eprintln!(" got event in {}", pathbuf.display());

                                let mut hasher = DefaultHasher::new();
                                pathbuf.hash(&mut hasher);
                                sender.send(RefreshEvent {
                                    folder: format!("{}", pathbuf.display()),
                                    hash: hasher.finish(),
                                });
                            }
                            _ => {}
                        },
                        Err(e) => eprintln!("watch error: {:?}", e),
                    }
                }
            })?;
        Ok(())
    }
    fn operation(&self, hash: u64) -> Box<BackendOp> {
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
        for (idx, f) in self.folders.iter().enumerate() {
            if f.hash() == folder.hash() {
                return idx;
            }
        }
        unreachable!()
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
                                                let hash = {
                                                    let mut buf = Vec::new();
                                                    let mut f = fs::File::open(&file)
                                                        .unwrap_or_else(|_| {
                                                            panic!("Can't open {}", file.display())
                                                        });
                                                    f.read_to_end(&mut buf).unwrap_or_else(|_| {
                                                        panic!("Can't read {}", file.display())
                                                    });
                                                    let mut hasher = FnvHasher::default();
                                                    hasher.write(file.as_os_str().as_bytes());
                                                    hasher.write(&buf);
                                                    hasher.finish()
                                                };
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

#[derive(Debug, Default)]
pub struct MaildirFolder {
    hash: u64,
    name: String,
    path: PathBuf,
    children: Vec<usize>,
}

impl MaildirFolder {
    pub fn new(path: String, file_name: String, children: Vec<usize>) -> Result<Self> {
        let pathbuf = PathBuf::from(path);
        let mut h = DefaultHasher::new();
        pathbuf.hash(&mut h);

        let ret = MaildirFolder {
            hash: h.finish(),
            name: file_name,
            path: pathbuf,
            children,
        };
        ret.is_valid()?;
        Ok(ret)
    }
    pub fn path(&self) -> &Path {
        self.path.as_path()
    }
    fn is_valid(&self) -> Result<()> {
        let path = self.path();
        let mut p = PathBuf::from(path);
        for d in &["cur", "new", "tmp"] {
            p.push(d);
            if !p.is_dir() {
                return Err(MeliError::new(format!(
                    "{} is not a valid maildir folder",
                    path.display()
                )));
            }
            p.pop();
        }
        Ok(())
    }
}
impl BackendFolder for MaildirFolder {
    fn hash(&self) -> u64 {
        self.hash
    }
    fn name(&self) -> &str {
        &self.name
    }
    fn change_name(&mut self, s: &str) {
        self.name = s.to_string();
    }
    fn children(&self) -> &Vec<usize> {
        &self.children
    }
    fn clone(&self) -> Folder {
        Box::new(MaildirFolder {
            hash: self.hash,
            name: self.name.clone(),
            path: self.path.clone(),
            children: self.children.clone(),
        })
    }
}
