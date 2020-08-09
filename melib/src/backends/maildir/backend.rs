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

use super::{MaildirMailbox, MaildirOp, MaildirPathTrait};
use crate::async_workers::*;
use crate::backends::{RefreshEventKind::*, *};
use crate::conf::AccountSettings;
use crate::email::{Envelope, EnvelopeHash, Flag};
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;
use futures::prelude::Stream;

extern crate notify;
use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::time::Duration;

use std::collections::{hash_map::DefaultHasher, HashMap, HashSet};
use std::ffi::OsStr;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::{self, Read, Write};
use std::ops::{Deref, DerefMut};
use std::os::unix::fs::PermissionsExt;
use std::path::{Component, Path, PathBuf};
use std::result;
use std::sync::mpsc::channel;
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
    index: HashMap<EnvelopeHash, MaildirPath>,
    hash: MailboxHash,
}

impl Deref for HashIndex {
    type Target = HashMap<EnvelopeHash, MaildirPath>;
    fn deref(&self) -> &HashMap<EnvelopeHash, MaildirPath> {
        &self.index
    }
}

impl DerefMut for HashIndex {
    fn deref_mut(&mut self) -> &mut HashMap<EnvelopeHash, MaildirPath> {
        &mut self.index
    }
}

pub type HashIndexes = Arc<Mutex<HashMap<MailboxHash, HashIndex>>>;

/// Maildir backend https://cr.yp.to/proto/maildir.html
#[derive(Debug)]
pub struct MaildirType {
    name: String,
    mailboxes: HashMap<MailboxHash, MaildirMailbox>,
    mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
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

        crate::get_path_hash!(path)
    }};
}

pub(super) fn get_file_hash(file: &Path) -> EnvelopeHash {
    /*
    let mut buf = Vec::with_capacity(2048);
    let mut f = fs::File::open(&file).unwrap_or_else(|_| panic!("Can't open {}", file.display()));
    f.read_to_end(&mut buf)
        .unwrap_or_else(|_| panic!("Can't read {}", file.display()));
    let mut hasher = DefaultHasher::default();
    hasher.write(&buf);
    hasher.finish()
        */
    let mut hasher = DefaultHasher::default();
    file.hash(&mut hasher);
    hasher.finish()
}

pub fn move_to_cur(p: PathBuf) -> Result<PathBuf> {
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
    fn capabilities(&self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            is_async: false,
            is_remote: false,
            supports_search: false,
            extensions: None,
            supports_tags: false,
            supports_submission: false,
        };
        CAPABILITIES
    }

    fn is_online(&self) -> Result<()> {
        Ok(())
    }

    fn is_online_async(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>> {
        Ok(self
            .mailboxes
            .iter()
            .map(|(h, f)| (*h, BackendMailbox::clone(f)))
            .collect())
    }
    fn mailboxes_async(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let res = self.mailboxes();
        Ok(Box::pin(async { res }))
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> Result<Async<Result<Vec<Envelope>>>> {
        Ok(self.multicore(4, mailbox_hash))
    }

    fn fetch_async(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<core::pin::Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>>
    {
        let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
        let unseen = mailbox.unseen.clone();
        let total = mailbox.total.clone();
        let path: PathBuf = mailbox.fs_path().into();
        let root_path = self.path.to_path_buf();
        let map = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();
        super::stream::MaildirStream::new(&self.name, mailbox_hash, unseen, total, path, root_path, map, mailbox_index)
    }

    fn refresh(
        &mut self,
        mailbox_hash: MailboxHash,
        sender: RefreshEventConsumer,
    ) -> Result<Async<()>> {
        let w = AsyncBuilder::new();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        let account_hash = {
            let mut hasher = DefaultHasher::default();
            hasher.write(self.name.as_bytes());
            hasher.finish()
        };

        let handle = {
            let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
            let path: PathBuf = mailbox.fs_path().into();
            let name = format!("refresh {:?}", mailbox.name());
            let root_path = self.path.to_path_buf();
            let map = self.hash_indexes.clone();
            let mailbox_index = self.mailbox_index.clone();

            Box::new(move |work_context: crate::async_workers::WorkContext| {
                work_context
                    .set_name
                    .send((std::thread::current().id(), name.clone()))
                    .unwrap();
                let thunk = move |sender: &RefreshEventConsumer| {
                    debug!("refreshing");
                    let mut path = path.clone();
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
                        let map = map.entry(mailbox_hash).or_default();
                        map.keys().cloned().collect::<HashSet<EnvelopeHash>>()
                    };
                    for file in files {
                        let hash = get_file_hash(&file);
                        {
                            let mut map = map.lock().unwrap();
                            let map = map.entry(mailbox_hash).or_default();
                            if map.contains_key(&hash) {
                                map.remove(&hash);
                                current_hashes.remove(&hash);
                                continue;
                            }
                            (*map).insert(hash, PathBuf::from(&file).into());
                        }
                        let op = Box::new(MaildirOp::new(hash, map.clone(), mailbox_hash));
                        if let Ok(e) = Envelope::from_token(op, hash) {
                            mailbox_index.lock().unwrap().insert(e.hash(), mailbox_hash);
                            let file_name = file.strip_prefix(&root_path).unwrap().to_path_buf();
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
                                account_hash,
                                mailbox_hash,
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
                        account_hash,
                        mailbox_hash,
                        kind: Remove(h),
                    }) {
                        sender.send(ev);
                    }
                    Ok(())
                };
                if let Err(err) = thunk(&sender) {
                    sender.send(RefreshEvent {
                        account_hash,
                        mailbox_hash,
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
        let account_hash = {
            let mut hasher = DefaultHasher::default();
            hasher.write(self.name.as_bytes());
            hasher.finish()
        };
        let root_path = self.path.to_path_buf();
        watcher.watch(&root_path, RecursiveMode::Recursive).unwrap();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        debug!("watching {:?}", root_path);
        let hash_indexes = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();
        let root_mailbox_hash: MailboxHash = self.mailboxes.values().find(|m| m.parent.is_none()).map(|m| m.hash()).unwrap();
        let mailbox_counts = self
            .mailboxes
            .iter()
            .map(|(&k, v)| (k, (v.unseen.clone(), v.total.clone())))
            .collect::<HashMap<MailboxHash, (Arc<Mutex<usize>>, Arc<Mutex<usize>>)>>();
        let handle = thread::Builder::new()
            .name("mailbox watch".to_string())
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
                                let mailbox_hash = get_path_hash!(pathbuf);
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                if let Some(env) = add_path_to_index(
                                    &hash_indexes,
                                    mailbox_hash,
                                    pathbuf.as_path(),
                                    &cache_dir,
                                    file_name,
                                ) {
                                    mailbox_index.lock().unwrap().insert(env.hash(),mailbox_hash);
                                    debug!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        pathbuf.display()
                                    );
                                        if !env.is_seen() {
                                            *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                        }
                                            *mailbox_counts[&mailbox_hash].1.lock().unwrap() += 1;
                                    sender.send(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Create(Box::new(env)),
                                    });
                                }
                            }
                            /* Update */
                            DebouncedEvent::NoticeWrite(pathbuf)
                            | DebouncedEvent::Write(pathbuf) => {
                                debug!("DebouncedEvent::Write(path = {:?}", &pathbuf);
                                let mailbox_hash = get_path_hash!(pathbuf);
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock =
                                    &mut hash_indexes_lock.entry(mailbox_hash).or_default();
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
                                        drop(hash_indexes_lock);
                                        /* Did we just miss a Create event? In any case, create
                                         * envelope. */
                                        if let Some(env) = add_path_to_index(
                                            &hash_indexes,
                                            mailbox_hash,
                                            pathbuf.as_path(),
                                            &cache_dir,
                                            file_name,
                                        ) {
                                            mailbox_index.lock().unwrap().insert(env.hash(),mailbox_hash);
                                            sender.send(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
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
                                        mailbox_hash,
                                    ));
                                    if let Ok(env) = Envelope::from_token(op, new_hash) {
                                        debug!("{}\t{:?}", new_hash, &pathbuf);
                                        debug!(
                                            "hash {}, path: {:?} couldn't be parsed",
                                            new_hash, &pathbuf
                                        );
                                        index_lock.insert(new_hash, pathbuf.into());

                                        /* Send Write notice */

                                        sender.send(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Update(old_hash, Box::new(env)),
                                        });
                                    }
                                }
                            }
                            /* Remove */
                            DebouncedEvent::NoticeRemove(pathbuf)
                            | DebouncedEvent::Remove(pathbuf) => {
                                debug!("DebouncedEvent::Remove(path = {:?}", pathbuf);
                                let mailbox_hash = get_path_hash!(pathbuf);
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
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
                                    index_lock.entry(hash).and_modify(|e| {
                                        e.removed = false;
                                    });
                                    continue;
                                }
                                *mailbox_counts[&mailbox_hash].1.lock().unwrap() -= 1;
                                if !pathbuf.flags().contains(Flag::SEEN) {
                                    *mailbox_counts[&mailbox_hash].0.lock().unwrap() -= 1;
                                }

                                index_lock.entry(hash).and_modify(|e| {
                                    e.removed = true;
                                });

                                sender.send(RefreshEvent {
                                    account_hash,
                                    mailbox_hash,
                                    kind: Remove(hash),
                                });
                            }
                            /* Envelope hasn't changed */
                            DebouncedEvent::Rename(src, dest) => {
                                debug!(
                                    "DebouncedEvent::Rename(src = {:?}, dest = {:?})",
                                    src, dest
                                );
                                let mailbox_hash = get_path_hash!(src);
                                let old_hash: EnvelopeHash = get_file_hash(src.as_path());
                                let new_hash: EnvelopeHash = get_file_hash(dest.as_path());

                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                                let old_flags = src.flags();
                                let new_flags = dest.flags();
                                let was_seen: bool = old_flags.contains(Flag::SEEN);
                                let is_seen: bool = new_flags.contains(Flag::SEEN);

                                if index_lock.contains_key(&old_hash)
                                    && !index_lock[&old_hash].removed
                                {
                                    debug!("contains_old_key");
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        debug!(&e.modified);
                                        e.modified = Some(PathMod::Hash(new_hash));
                                    });
                                    sender.send(RefreshEvent {
                                        account_hash,
                                        mailbox_hash: get_path_hash!(dest),
                                        kind: Rename(old_hash, new_hash),
                                    });
                                    if !was_seen && is_seen {
                                        let mut lck = mailbox_counts[&mailbox_hash].0.lock().unwrap();
                                        *lck = lck.saturating_sub(1);
                                    } else if was_seen && !is_seen {
                                        *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                    }
                                    if old_flags != new_flags {
                                        sender.send(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: get_path_hash!(dest),
                                            kind: NewFlags(new_hash, (new_flags, vec![])),
                                        });
                                    }
                                    mailbox_index.lock().unwrap().insert(new_hash,get_path_hash!(dest) );
                                    index_lock.insert(new_hash, dest.into());
                                    continue;
                                } else if !index_lock.contains_key(&new_hash)
                                    && index_lock
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
                                    drop(hash_indexes_lock);
                                    if let Some(env) = add_path_to_index(
                                        &hash_indexes,
                                        mailbox_hash,
                                        dest.as_path(),
                                        &cache_dir,
                                        file_name,
                                    ) {
                                        mailbox_index.lock().unwrap().insert(env.hash(), mailbox_hash);
                                        debug!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            dest.display()
                                        );
                                        if !env.is_seen() {
                                            *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                        }
                                        *mailbox_counts[&mailbox_hash].1.lock().unwrap() += 1;
                                        sender.send(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Create(Box::new(env)),
                                        });
                                        continue;
                                    } else {
                                        debug!("not valid email");
                                    }
                                } else {
                                    if was_seen && !is_seen {
                                        *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                    }
                                    sender.send(RefreshEvent {
                                        account_hash,
                                        mailbox_hash: get_path_hash!(dest),
                                        kind: Rename(old_hash, new_hash),
                                    });
                                    debug!("contains_new_key");
                                    if old_flags != new_flags {
                                        sender.send(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: get_path_hash!(dest),
                                            kind: NewFlags(new_hash, (new_flags, vec![])),
                                        });
                                    }
                                }

                                /* Maybe a re-read should be triggered here just to be safe.
                                sender.send(RefreshEvent {
                                    account_hash,
                                    mailbox_hash: get_path_hash!(dest),
                                    kind: Rescan,
                                });
                                */
                            }
                            /* Trigger rescan of mailbox */
                            DebouncedEvent::Rescan => {
                                sender.send(RefreshEvent {
                                    account_hash,
                                    mailbox_hash: root_mailbox_hash,
                                    kind: Rescan,
                                });
                            }
                            _ => {}
                        },
                        Err(e) => debug!("watch error: {:?}", e),
                    }
                }
            })?;
        Ok(handle.thread().id())
    }

    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        Ok(Box::new(MaildirOp::new(
            hash,
            self.hash_indexes.clone(),
            self.mailbox_index.lock().unwrap()[&hash],
        )))
    }

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let path = self.mailboxes[&mailbox_hash].fs_path.clone();
        Ok(Box::pin(async move {
            MaildirType::save_to_mailbox(path, bytes, flags)
        }))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()> {
        let hash_index = self.hash_indexes.clone();
        if flags.iter().any(|(f, _)| f.is_err()) {
            return Err(MeliError::new("Maildir doesn't support tags."));
        }

        Ok(Box::pin(async move {
            let mut hash_indexes_lck = hash_index.lock().unwrap();
            let hash_index = hash_indexes_lck.entry(mailbox_hash).or_default();

            for env_hash in env_hashes.iter() {
                let _path = {
                    if !hash_index.contains_key(&env_hash) {
                        continue;
                    }
                    if let Some(modif) = &hash_index[&env_hash].modified {
                        match modif {
                            PathMod::Path(ref path) => path.clone(),
                            PathMod::Hash(hash) => hash_index[&hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };
                let mut env_flags = _path.flags();
                let path = _path.to_str().unwrap(); // Assume UTF-8 validity
                let idx: usize = path
                    .rfind(":2,")
                    .ok_or_else(|| MeliError::new(format!("Invalid email filename: {:?}", path)))?
                    + 3;
                let mut new_name: String = path[..idx].to_string();
                for (f, value) in flags.iter() {
                    env_flags.set(*f.as_ref().unwrap(), *value);
                }

                if !(env_flags & Flag::DRAFT).is_empty() {
                    new_name.push('D');
                }
                if !(env_flags & Flag::FLAGGED).is_empty() {
                    new_name.push('F');
                }
                if !(env_flags & Flag::PASSED).is_empty() {
                    new_name.push('P');
                }
                if !(env_flags & Flag::REPLIED).is_empty() {
                    new_name.push('R');
                }
                if !(env_flags & Flag::SEEN).is_empty() {
                    new_name.push('S');
                }
                if !(env_flags & Flag::TRASHED).is_empty() {
                    new_name.push('T');
                }
                let new_name: PathBuf = new_name.into();
                hash_index.entry(env_hash).or_default().modified =
                    Some(PathMod::Path(new_name.clone()));

                debug!("renaming {:?} to {:?}", path, new_name);
                fs::rename(&path, &new_name)?;
                debug!("success in rename");
            }
            Ok(())
        }))
    }

    fn as_any(&self) -> &dyn ::std::any::Any {
        self
    }

    fn create_mailbox(
        &mut self,
        new_path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        let mut path = self.path.clone();
        path.push(&new_path);
        if !path.starts_with(&self.path) {
            return Err(MeliError::new(format!("Path given (`{}`) is absolute. Please provide a path relative to the account's root mailbox.", &new_path)));
        }

        std::fs::create_dir(&path)?;
        /* create_dir does not create intermediate directories (like `mkdir -p`), so the parent must be a valid
         * mailbox at this point. */

        let parent = path.parent().and_then(|p| {
            self.mailboxes
                .iter()
                .find(|(_, f)| f.fs_path == p)
                .map(|item| *item.0)
        });

        let mailbox_hash = get_path_hash!(&path);
        if let Some(parent) = parent {
            self.mailboxes
                .entry(parent)
                .and_modify(|entry| entry.children.push(mailbox_hash));
        }
        let new_mailbox = MaildirMailbox {
            hash: mailbox_hash,
            path: PathBuf::from(&new_path),
            name: new_path,
            fs_path: path,
            parent,
            children: vec![],
            usage: Default::default(),
            is_subscribed: true,
            permissions: Default::default(),
            unseen: Default::default(),
            total: Default::default(),
        };

        self.mailboxes.insert(mailbox_hash, new_mailbox);
        let ret = Ok((mailbox_hash, self.mailboxes()?));
        Ok(Box::pin(async { ret }))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
}

impl MaildirType {
    pub fn new(
        settings: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool>,
    ) -> Result<Box<dyn MailBackend>> {
        let mut mailboxes: HashMap<MailboxHash, MaildirMailbox> = Default::default();
        fn recurse_mailboxes<P: AsRef<Path>>(
            mailboxes: &mut HashMap<MailboxHash, MaildirMailbox>,
            settings: &AccountSettings,
            p: P,
        ) -> Result<Vec<MailboxHash>> {
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
            for mut f in fs::read_dir(&p).unwrap() {
                'entries: for f in f.iter_mut() {
                    {
                        let path = f.path();
                        if path.ends_with("cur") || path.ends_with("new") || path.ends_with("tmp") {
                            continue 'entries;
                        }
                        if path.is_dir() {
                            if let Ok(mut f) = MaildirMailbox::new(
                                path.to_str().unwrap().to_string(),
                                path.file_name().unwrap().to_str().unwrap().to_string(),
                                None,
                                Vec::new(),
                                false,
                                &settings,
                            ) {
                                f.children = recurse_mailboxes(mailboxes, settings, &path)?;
                                for c in &f.children {
                                    if let Some(f) = mailboxes.get_mut(c) {
                                        f.parent = Some(f.hash);
                                    }
                                }
                                children.push(f.hash);
                                mailboxes.insert(f.hash, f);
                            } else {
                                /* If directory is invalid (i.e. has no {cur,new,tmp} subfolders),
                                 * accept it ONLY if it contains subdirs of any depth that are
                                 * valid maildir paths
                                 */
                                let subdirs = recurse_mailboxes(mailboxes, settings, &path)?;
                                if !subdirs.is_empty() {
                                    if let Ok(f) = MaildirMailbox::new(
                                        path.to_str().unwrap().to_string(),
                                        path.file_name().unwrap().to_str().unwrap().to_string(),
                                        None,
                                        subdirs,
                                        true,
                                        &settings,
                                    ) {
                                        for c in &f.children {
                                            if let Some(f) = mailboxes.get_mut(c) {
                                                f.parent = Some(f.hash);
                                            }
                                        }
                                        children.push(f.hash);
                                        mailboxes.insert(f.hash, f);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Ok(children)
        };
        let root_path = PathBuf::from(settings.root_mailbox()).expand();
        if !root_path.exists() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a valid directory.",
                settings.name(),
                settings.root_mailbox.as_str()
            )));
        } else if !root_path.is_dir() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a directory.",
                settings.name(),
                settings.root_mailbox.as_str()
            )));
        }

        if let Ok(f) = MaildirMailbox::new(
            root_path.to_str().unwrap().to_string(),
            root_path.file_name().unwrap().to_str().unwrap().to_string(),
            None,
            Vec::with_capacity(0),
            false,
            settings,
        ) {
            mailboxes.insert(f.hash, f);
        }

        if mailboxes.is_empty() {
            let children = recurse_mailboxes(&mut mailboxes, settings, &root_path)?;
            for c in &children {
                if let Some(f) = mailboxes.get_mut(c) {
                    f.parent = None;
                }
            }
        } else {
            let root_hash = *mailboxes.keys().next().unwrap();
            let children = recurse_mailboxes(&mut mailboxes, settings, &root_path)?;
            for c in &children {
                if let Some(f) = mailboxes.get_mut(c) {
                    f.parent = Some(root_hash);
                }
            }
            if let Some(f) = mailboxes.get_mut(&root_hash) {
                f.children = children;
            }
        }
        for f in mailboxes.values_mut() {
            if is_subscribed(f.path()) {
                f.is_subscribed = true;
            }
        }

        let mut hash_indexes =
            HashMap::with_capacity_and_hasher(mailboxes.len(), Default::default());
        for &fh in mailboxes.keys() {
            hash_indexes.insert(
                fh,
                HashIndex {
                    index: HashMap::with_capacity_and_hasher(0, Default::default()),
                    hash: fh,
                },
            );
        }
        Ok(Box::new(MaildirType {
            name: settings.name().to_string(),
            mailboxes,
            hash_indexes: Arc::new(Mutex::new(hash_indexes)),
            mailbox_index: Default::default(),
            path: root_path,
        }))
    }

    pub fn multicore(&mut self, cores: usize, mailbox_hash: MailboxHash) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();

        let handle = {
            let tx = w.tx();
            let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
            let unseen = mailbox.unseen.clone();
            let total = mailbox.total.clone();
            let tx_final = w.tx();
            let mut path: PathBuf = mailbox.fs_path().into();
            let name = format!("parsing {:?}", mailbox.name());
            let root_path = self.path.to_path_buf();
            let map = self.hash_indexes.clone();
            let mailbox_index = self.mailbox_index.clone();

            let closure = move |work_context: crate::async_workers::WorkContext| {
                work_context
                    .set_name
                    .send((std::thread::current().id(), name.clone()))
                    .unwrap();
                let mut thunk = move || {
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
                            let chunk_size = if count / cores > 0 {
                                count / cores
                            } else {
                                count
                            };
                            for chunk in files.chunks(chunk_size) {
                                let unseen = unseen.clone();
                                let total = total.clone();
                                let cache_dir = cache_dir.clone();
                                let tx = tx.clone();
                                let map = map.clone();
                                let mailbox_index = mailbox_index.clone();
                                let root_path = root_path.clone();
                                let s = scope.builder().name(name.clone()).spawn(move |_| {
                                    let len = chunk.len();
                                    let size = if len <= 100 { 100 } else { (len / 100) * 100 };
                                    let mut local_r: Vec<Envelope> =
                                        Vec::with_capacity(chunk.len());
                                    for c in chunk.chunks(size) {
                                        //thread::yield_now();
                                        let map = map.clone();
                                        let mailbox_index = mailbox_index.clone();
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
                                                    let map = map.entry(mailbox_hash).or_default();
                                                    let hash = env.hash();
                                                    map.insert(hash, file.clone().into());
                                                    mailbox_index
                                                        .lock()
                                                        .unwrap()
                                                        .insert(hash, mailbox_hash);
                                                    if !env.is_seen() {
                                                        *unseen.lock().unwrap() += 1;
                                                    }
                                                    *total.lock().unwrap() += 1;
                                                    local_r.push(env);
                                                    continue;
                                                }
                                            };
                                            let hash = get_file_hash(file);
                                            {
                                                let mut map = map.lock().unwrap();
                                                let map = map.entry(mailbox_hash).or_default();
                                                (*map).insert(hash, PathBuf::from(file).into());
                                            }
                                            let op = Box::new(MaildirOp::new(
                                                hash,
                                                map.clone(),
                                                mailbox_hash,
                                            ));
                                            if let Ok(e) = Envelope::from_token(op, hash) {
                                                mailbox_index
                                                    .lock()
                                                    .unwrap()
                                                    .insert(e.hash(), mailbox_hash);
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
                                                if !e.is_seen() {
                                                    *unseen.lock().unwrap() += 1;
                                                }
                                                *total.lock().unwrap() += 1;
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

    pub fn save_to_mailbox(mut path: PathBuf, bytes: Vec<u8>, flags: Option<Flag>) -> Result<()> {
        for d in &["cur", "new", "tmp"] {
            path.push(d);
            if !path.is_dir() {
                return Err(MeliError::new(format!(
                    "{} is not a valid maildir mailbox",
                    path.display()
                )));
            }
            path.pop();
        }
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
        writer.write_all(&bytes).unwrap();
        Ok(())
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        let root_path = PathBuf::from(s.root_mailbox()).expand();
        if !root_path.exists() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a valid directory.",
                s.name(),
                s.root_mailbox.as_str()
            )));
        } else if !root_path.is_dir() {
            return Err(MeliError::new(format!(
                "Configuration error ({}): root_path `{}` is not a directory.",
                s.name(),
                s.root_mailbox.as_str()
            )));
        }

        Ok(())
    }
}

fn add_path_to_index(
    hash_index: &HashIndexes,
    mailbox_hash: MailboxHash,
    path: &Path,
    cache_dir: &xdg::BaseDirectories,
    file_name: PathBuf,
) -> Option<Envelope> {
    let env: Envelope;
    debug!("add_path_to_index path {:?} filename{:?}", path, file_name);
    let hash = get_file_hash(path);
    {
        let mut map = hash_index.lock().unwrap();
        let map = map.entry(mailbox_hash).or_default();
        map.insert(hash, path.to_path_buf().into());
        debug!(
            "inserted {} in {} map, len={}",
            hash,
            mailbox_hash,
            map.len()
        );
    }
    let op = Box::new(MaildirOp::new(hash, hash_index.clone(), mailbox_hash));
    if let Ok(e) = Envelope::from_token(op, hash) {
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
