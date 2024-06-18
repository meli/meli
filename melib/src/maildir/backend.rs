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

//! # Maildir Backend
//!
//! This module implements a maildir backend according to the maildir
//! specification. <https://cr.yp.to/proto/maildir.html>

use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    ffi::OsStr,
    fs,
    hash::{Hash, Hasher},
    io::{self, Read, Write},
    ops::{Deref, DerefMut},
    os::unix::fs::PermissionsExt,
    path::{Component, Path, PathBuf},
    sync::{mpsc::channel, Arc, Mutex},
    time::Duration,
};

use notify::{event::EventKind as NotifyEvent, RecommendedWatcher, RecursiveMode, Watcher};

use super::{MaildirMailbox, MaildirOp, MaildirPathTrait};
use crate::{
    backends::{prelude::*, RefreshEventKind::*},
    error::{Error, ErrorKind, IntoError, Result},
    utils::shellexpand::ShellExpandTrait,
};

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

    fn deref(&self) -> &Self::Target {
        assert!(!(self.removed && self.modified.is_none()));
        &self.buf
    }
}

impl DerefMut for MaildirPath {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(!(self.removed && self.modified.is_none()));
        &mut self.buf
    }
}

impl From<PathBuf> for MaildirPath {
    fn from(val: PathBuf) -> Self {
        Self {
            buf: val,
            modified: None,
            removed: false,
        }
    }
}

#[derive(Debug, Default)]
pub struct HashIndex {
    index: HashMap<EnvelopeHash, MaildirPath>,
    _hash: MailboxHash,
}

impl Deref for HashIndex {
    type Target = HashMap<EnvelopeHash, MaildirPath>;

    fn deref(&self) -> &Self::Target {
        &self.index
    }
}

impl DerefMut for HashIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.index
    }
}

pub type HashIndexes = Arc<Mutex<HashMap<MailboxHash, HashIndex>>>;

/// The maildir backend instance type.
#[derive(Debug)]
pub struct MaildirType {
    name: String,
    mailboxes: HashMap<MailboxHash, MaildirMailbox>,
    mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    hash_indexes: HashIndexes,
    event_consumer: BackendEventConsumer,
    collection: Collection,
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

pub fn get_file_hash(file: &Path) -> EnvelopeHash {
    let mut hasher = DefaultHasher::default();
    file.hash(&mut hasher);
    EnvelopeHash(hasher.finish())
}

pub fn move_to_cur(p: &Path) -> Result<PathBuf> {
    let mut new = p.to_path_buf();
    let file_name = p.to_string_lossy();
    let slash_pos = file_name.bytes().rposition(|c| c == b'/').unwrap() + 1;
    new.pop();
    new.pop();

    new.push("cur");
    new.push(&file_name[slash_pos..]);
    if !file_name.ends_with(":2,") {
        new.set_extension(":2,");
    }
    log::trace!("moved to cur: {}", new.display());
    fs::rename(p, &new)?;
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
            extra_submission_headers: &[],
        };
        CAPABILITIES
    }

    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let res = Ok(self
            .mailboxes
            .iter()
            .map(|(h, f)| (*h, BackendMailbox::clone(f)))
            .collect());
        Ok(Box::pin(async { res }))
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<std::pin::Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
        let unseen = mailbox.unseen.clone();
        let total = mailbox.total.clone();
        let path: PathBuf = mailbox.fs_path().into();
        let map = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();
        super::stream::MaildirStream::new(mailbox_hash, unseen, total, path, map, mailbox_index)
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let account_hash = AccountHash::from_bytes(self.name.as_bytes());
        let sender = self.event_consumer.clone();

        let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
        let path: PathBuf = mailbox.fs_path().into();
        let map = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();

        Ok(Box::pin(async move {
            let thunk = move |sender: &BackendEventConsumer| {
                log::trace!("refreshing");
                let mut buf = Vec::with_capacity(4096);
                let files = Self::list_mail_in_maildir_fs(path.clone(), false)?;
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
                    let mut reader = io::BufReader::new(fs::File::open(&file)?);
                    buf.clear();
                    reader.read_to_end(&mut buf)?;
                    if let Ok(mut env) = Envelope::from_bytes(buf.as_slice(), Some(file.flags())) {
                        env.set_hash(hash);
                        mailbox_index
                            .lock()
                            .unwrap()
                            .insert(env.hash(), mailbox_hash);
                        (sender)(
                            account_hash,
                            BackendEvent::Refresh(RefreshEvent {
                                account_hash,
                                mailbox_hash,
                                kind: Create(Box::new(env)),
                            }),
                        );
                    } else {
                        log::trace!(
                            "DEBUG: hash {}, path: {} couldn't be parsed",
                            hash,
                            file.as_path().display()
                        );
                        continue;
                    }
                }
                for ev in current_hashes.into_iter().map(|h| {
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Remove(h),
                    })
                }) {
                    (sender)(account_hash, ev);
                }
                Ok(())
            };
            if let Err(err) = thunk(&sender) {
                (sender)(
                    account_hash,
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Failure(err),
                    }),
                );
            }
            Ok(())
        }))
    }

    fn watch(&self) -> ResultFuture<()> {
        let account_hash = AccountHash::from_bytes(self.name.as_bytes());
        let root_mailbox = self.path.to_path_buf();
        let sender = self.event_consumer.clone();
        let (tx, rx) = channel();
        let watcher = RecommendedWatcher::new(
            tx,
            notify::Config::default().with_poll_interval(Duration::from_secs(2)),
        )
        .and_then(|mut watcher| {
            watcher.watch(&root_mailbox, RecursiveMode::Recursive)?;
            Ok(watcher)
        })
        .map_err(|err| err.set_err_details("Failed to create file change monitor."))?;
        let hash_indexes = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();
        let root_mailbox_hash: MailboxHash = self
            .mailboxes
            .values()
            .find(|m| m.parent.is_none())
            .map(|m| m.hash())
            .unwrap();
        let mailbox_counts = self
            .mailboxes
            .iter()
            .map(|(&k, v)| (k, (v.unseen.clone(), v.total.clone())))
            .collect::<HashMap<MailboxHash, (Arc<Mutex<usize>>, Arc<Mutex<usize>>)>>();
        Ok(Box::pin(async move {
            // Move `watcher` in the closure's scope so that it doesn't get dropped.
            let _watcher = watcher;
            let mut buf = Vec::with_capacity(4096);
            loop {
                match rx.recv() {
                    Ok(Ok(event)) => match event.kind {
                        /* Create */
                        NotifyEvent::Create(_) => {
                            log::debug!("Create events: (path = {:?})", event.paths);
                            for mut pathbuf in event.paths {
                                if path_is_new!(pathbuf) {
                                    // This creates a Rename event that we will receive later
                                    pathbuf = match move_to_cur(&pathbuf) {
                                        Ok(p) => p,
                                        Err(err) => {
                                            log::error!(
                                                "Could not move {} to /cur: {}",
                                                pathbuf.display(),
                                                err
                                            );
                                            pathbuf
                                        }
                                    };
                                }
                                let mailbox_hash = MailboxHash(get_path_hash!(pathbuf));
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_mailbox)
                                    .unwrap()
                                    .to_path_buf();
                                if let Ok(env) = add_path_to_index(
                                    &hash_indexes,
                                    mailbox_hash,
                                    pathbuf.as_path(),
                                    file_name,
                                    &mut buf,
                                ) {
                                    mailbox_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), mailbox_hash);
                                    log::debug!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        pathbuf.display()
                                    );
                                    if !env.is_seen() {
                                        *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                    }
                                    *mailbox_counts[&mailbox_hash].1.lock().unwrap() += 1;
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Create(Box::new(env)),
                                        }),
                                    );
                                }
                            }
                        }
                        NotifyEvent::Modify(
                            notify::event::ModifyKind::Any
                            | notify::event::ModifyKind::Data(_)
                            | notify::event::ModifyKind::Other,
                        ) => {
                            log::debug!("Modify events: (path = {:?})", event.paths);
                            for pathbuf in event.paths {
                                let mailbox_hash = MailboxHash(get_path_hash!(pathbuf));
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock =
                                    &mut hash_indexes_lock.entry(mailbox_hash).or_default();
                                let file_name = pathbuf
                                    .as_path()
                                    .strip_prefix(&root_mailbox)
                                    .unwrap()
                                    .to_path_buf();
                                /* Linear search in hash_index to find old hash */
                                let old_hash: EnvelopeHash = {
                                    if let Some((k, v)) =
                                        index_lock.iter_mut().find(|(_, v)| *v.buf == pathbuf)
                                    {
                                        *v = pathbuf.clone().into();
                                        *k
                                    } else {
                                        drop(hash_indexes_lock);
                                        /* Did we just miss a Create event? In any case, create
                                         * envelope. */
                                        if let Ok(env) = add_path_to_index(
                                            &hash_indexes,
                                            mailbox_hash,
                                            pathbuf.as_path(),
                                            file_name,
                                            &mut buf,
                                        ) {
                                            mailbox_index
                                                .lock()
                                                .unwrap()
                                                .insert(env.hash(), mailbox_hash);
                                            (sender)(
                                                account_hash,
                                                BackendEvent::Refresh(RefreshEvent {
                                                    account_hash,
                                                    mailbox_hash,
                                                    kind: Create(Box::new(env)),
                                                }),
                                            );
                                        }
                                        continue;
                                    }
                                };
                                let new_hash: EnvelopeHash = get_file_hash(pathbuf.as_path());
                                let mut reader = io::BufReader::new(fs::File::open(&pathbuf)?);
                                buf.clear();
                                reader.read_to_end(&mut buf)?;
                                if index_lock.get_mut(&new_hash).is_none() {
                                    if let Ok(mut env) =
                                        Envelope::from_bytes(buf.as_slice(), Some(pathbuf.flags()))
                                    {
                                        env.set_hash(new_hash);
                                        index_lock.insert(new_hash, pathbuf.into());
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: Update(old_hash, Box::new(env)),
                                            }),
                                        );
                                    }
                                }
                            }
                        }
                        NotifyEvent::Remove(_) => {
                            for pathbuf in event.paths {
                                log::debug!("NotifyEvent::Remove(path = {:?}", pathbuf);
                                let mailbox_hash = MailboxHash(get_path_hash!(pathbuf));
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                                let hash: EnvelopeHash = if let Some((k, _)) =
                                    index_lock.iter().find(|(_, v)| *v.buf == pathbuf)
                                {
                                    *k
                                } else {
                                    log::debug!("removed but not contained in index");
                                    continue;
                                };
                                if let Some(ref modif) = &index_lock[&hash].modified {
                                    match modif {
                                        PathMod::Path(path) => log::trace!(
                                            "envelope {} has modified path set {}",
                                            hash,
                                            path.display()
                                        ),
                                        PathMod::Hash(hash) => log::trace!(
                                            "envelope {} has modified path set {}",
                                            hash,
                                            &index_lock[hash].buf.display()
                                        ),
                                    }
                                    index_lock.entry(hash).and_modify(|e| {
                                        e.removed = false;
                                    });
                                    continue;
                                }
                                {
                                    let mut lck = mailbox_counts[&mailbox_hash].1.lock().unwrap();
                                    *lck = lck.saturating_sub(1);
                                }
                                if !pathbuf.flags().contains(Flag::SEEN) {
                                    let mut lck = mailbox_counts[&mailbox_hash].0.lock().unwrap();
                                    *lck = lck.saturating_sub(1);
                                }

                                index_lock.entry(hash).and_modify(|e| {
                                    e.removed = true;
                                });

                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Remove(hash),
                                    }),
                                );
                            }
                        }
                        NotifyEvent::Modify(notify::event::ModifyKind::Name(
                            notify::event::RenameMode::Both,
                        )) if event.paths.len() == 2 => {
                            let [ref src, ref dest] = event.paths[..] else {
                                unreachable!()
                            };
                            log::debug!("NotifyEvent::Rename(src = {:?}, dest = {:?})", src, dest);
                            let mailbox_hash = MailboxHash(get_path_hash!(src));
                            let dest_mailbox = {
                                let dest_mailbox = MailboxHash(get_path_hash!(dest));
                                if dest_mailbox == mailbox_hash {
                                    None
                                } else {
                                    Some(dest_mailbox)
                                }
                            };
                            let old_hash: EnvelopeHash = get_file_hash(src.as_path());
                            let new_hash: EnvelopeHash = get_file_hash(dest.as_path());

                            let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                            let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                            let old_flags = src.flags();
                            let new_flags = dest.flags();
                            let was_seen: bool = old_flags.contains(Flag::SEEN);
                            let is_seen: bool = new_flags.contains(Flag::SEEN);

                            if index_lock.contains_key(&old_hash) && !index_lock[&old_hash].removed
                            {
                                if let Some(dest_mailbox) = dest_mailbox {
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        e.removed = true;
                                    });

                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Remove(old_hash),
                                        }),
                                    );
                                    let file_name = dest
                                        .as_path()
                                        .strip_prefix(&root_mailbox)
                                        .unwrap()
                                        .to_path_buf();
                                    drop(hash_indexes_lock);
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        dest_mailbox,
                                        dest.as_path(),
                                        file_name,
                                        &mut buf,
                                    ) {
                                        mailbox_index
                                            .lock()
                                            .unwrap()
                                            .insert(env.hash(), dest_mailbox);
                                        log::trace!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            dest.display()
                                        );
                                        if !env.is_seen() {
                                            *mailbox_counts[&dest_mailbox].0.lock().unwrap() += 1;
                                        }
                                        *mailbox_counts[&dest_mailbox].1.lock().unwrap() += 1;
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash: dest_mailbox,
                                                kind: Create(Box::new(env)),
                                            }),
                                        );
                                    }
                                } else {
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        e.modified = Some(PathMod::Hash(new_hash));
                                    });
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Rename(old_hash, new_hash),
                                        }),
                                    );
                                    if !was_seen && is_seen {
                                        let mut lck =
                                            mailbox_counts[&mailbox_hash].0.lock().unwrap();
                                        *lck = lck.saturating_sub(1);
                                    } else if was_seen && !is_seen {
                                        *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                    }
                                    if old_flags != new_flags {
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: NewFlags(new_hash, (new_flags, vec![])),
                                            }),
                                        );
                                    }
                                    mailbox_index.lock().unwrap().insert(new_hash, mailbox_hash);
                                    index_lock.insert(new_hash, dest.to_path_buf().into());
                                }
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
                                    log::trace!(
                                        "contains_old_key, key was marked as removed (by external \
                                         source)"
                                    );
                                } else {
                                    log::trace!("not contains_new_key");
                                }
                                let file_name = dest
                                    .as_path()
                                    .strip_prefix(&root_mailbox)
                                    .unwrap()
                                    .to_path_buf();
                                log::trace!("filename = {:?}", file_name);
                                drop(hash_indexes_lock);
                                if let Ok(env) = add_path_to_index(
                                    &hash_indexes,
                                    dest_mailbox.unwrap_or(mailbox_hash),
                                    dest.as_path(),
                                    file_name,
                                    &mut buf,
                                ) {
                                    mailbox_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), dest_mailbox.unwrap_or(mailbox_hash));
                                    log::trace!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        dest.display()
                                    );
                                    if !env.is_seen() {
                                        *mailbox_counts[&dest_mailbox.unwrap_or(mailbox_hash)]
                                            .0
                                            .lock()
                                            .unwrap() += 1;
                                    }
                                    *mailbox_counts[&dest_mailbox.unwrap_or(mailbox_hash)]
                                        .1
                                        .lock()
                                        .unwrap() += 1;
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: dest_mailbox.unwrap_or(mailbox_hash),
                                            kind: Create(Box::new(env)),
                                        }),
                                    );
                                    continue;
                                } else {
                                    log::trace!("not valid email");
                                }
                            } else if let Some(dest_mailbox) = dest_mailbox {
                                drop(hash_indexes_lock);
                                let file_name = dest
                                    .as_path()
                                    .strip_prefix(&root_mailbox)
                                    .unwrap()
                                    .to_path_buf();
                                if let Ok(env) = add_path_to_index(
                                    &hash_indexes,
                                    dest_mailbox,
                                    dest.as_path(),
                                    file_name,
                                    &mut buf,
                                ) {
                                    mailbox_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), dest_mailbox);
                                    log::trace!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        dest.display()
                                    );
                                    if !env.is_seen() {
                                        *mailbox_counts[&dest_mailbox].0.lock().unwrap() += 1;
                                    }
                                    *mailbox_counts[&dest_mailbox].1.lock().unwrap() += 1;
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: dest_mailbox,
                                            kind: Create(Box::new(env)),
                                        }),
                                    );
                                }
                            } else {
                                if was_seen && !is_seen {
                                    *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                }
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Rename(old_hash, new_hash),
                                    }),
                                );
                                log::trace!("contains_new_key");
                                if old_flags != new_flags {
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: NewFlags(new_hash, (new_flags, vec![])),
                                        }),
                                    );
                                }
                            }
                        }
                        _ => {
                            log::debug!("Received unexpected fs watcher notify event: {:?}", event);
                            /* Trigger rescan of mailbox */
                            (sender)(
                                account_hash,
                                BackendEvent::Refresh(RefreshEvent {
                                    account_hash,
                                    mailbox_hash: root_mailbox_hash,
                                    kind: Rescan,
                                }),
                            );
                        }
                    },
                    Ok(Err(e)) => log::debug!("watch error: {:?}", e),
                    Err(e) => log::debug!("watch error: {:?}", e),
                }
            }
        }))
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
            Self::save_to_mailbox(path, bytes, flags)
        }))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()> {
        let hash_index = self.hash_indexes.clone();
        if flags.iter().any(|op| op.is_tag()) {
            return Err(Error::new("Maildir doesn't support tags."));
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
                            PathMod::Hash(hash) => hash_index[hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };
                let mut env_flags = _path.flags();
                let path = _path.to_str().unwrap(); // Assume UTF-8 validity
                let idx: usize = path
                    .rfind(":2,")
                    .ok_or_else(|| Error::new(format!("Invalid email filename: {:?}", path)))?
                    + 3;
                let mut new_name: String = path[..idx].to_string();
                for op in flags.iter() {
                    if let FlagOp::Set(f) | FlagOp::UnSet(f) = op {
                        env_flags.set(*f, op.as_bool());
                    }
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

                log::debug!("renaming {:?} to {:?}", path, new_name);
                fs::rename(path, &new_name)?;
                log::debug!("success in rename");
            }
            Ok(())
        }))
    }

    fn delete_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        let hash_index = self.hash_indexes.clone();
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
                            PathMod::Hash(hash) => hash_index[hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };

                fs::remove_file(&_path)?;
            }
            Ok(())
        }))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()> {
        let hash_index = self.hash_indexes.clone();
        if !self.mailboxes.contains_key(&source_mailbox_hash) {
            return Err(Error::new("Invalid source mailbox hash").set_kind(ErrorKind::Bug));
        } else if !self.mailboxes.contains_key(&destination_mailbox_hash) {
            return Err(Error::new("Invalid destination mailbox hash").set_kind(ErrorKind::Bug));
        }
        let mut dest_path: PathBuf = self.mailboxes[&destination_mailbox_hash].fs_path().into();
        dest_path.push("cur");
        Ok(Box::pin(async move {
            let mut hash_indexes_lck = hash_index.lock().unwrap();
            let hash_index = hash_indexes_lck.entry(source_mailbox_hash).or_default();

            for env_hash in env_hashes.iter() {
                let path_src = {
                    if !hash_index.contains_key(&env_hash) {
                        continue;
                    }
                    if let Some(modif) = &hash_index[&env_hash].modified {
                        match modif {
                            PathMod::Path(ref path) => path.clone(),
                            PathMod::Hash(hash) => hash_index[hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };
                let filename = path_src.file_name().ok_or_else(|| {
                    format!("Could not get filename of `{}`", path_src.display(),)
                })?;
                dest_path.push(filename);
                hash_index.entry(env_hash).or_default().modified =
                    Some(PathMod::Path(dest_path.clone()));
                if move_ {
                    log::trace!("renaming {:?} to {:?}", path_src, dest_path);
                    fs::rename(&path_src, &dest_path)?;
                    log::trace!("success in rename");
                } else {
                    log::trace!("copying {:?} to {:?}", path_src, dest_path);
                    fs::copy(&path_src, &dest_path)?;
                    log::trace!("success in copy");
                }
                dest_path.pop();
            }
            Ok(())
        }))
    }

    fn collection(&self) -> Collection {
        self.collection.clone()
    }

    fn create_mailbox(
        &mut self,
        new_path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        let mut path = self.path.clone();
        path.push(&new_path);
        if !path.starts_with(&self.path) {
            return Err(Error::new(format!(
                "Path given (`{}`) is absolute. Please provide a path relative to the account's \
                 root mailbox.",
                &new_path
            )));
        }

        std::fs::create_dir(&path)?;
        /* create_dir does not create intermediate directories (like `mkdir -p`), so
         * the parent must be a valid mailbox at this point. */

        let parent = path.parent().and_then(|p| {
            self.mailboxes
                .iter()
                .find(|(_, f)| f.fs_path == p)
                .map(|item| *item.0)
        });

        let mailbox_hash = MailboxHash(get_path_hash!(&path));
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
        let ret = self.mailboxes()?;
        Ok(Box::pin(async move { Ok((mailbox_hash, ret.await?)) }))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(Error::new(
            "Deleting mailboxes is currently unimplemented for maildir backend.",
        ))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Mailbox subscriptions are not possible for the maildir backend.",
        ))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(Error::new(
            "Renaming mailboxes is currently unimplemented for maildir backend.",
        ))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox permissions is not possible for the maildir backend.",
        ))
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        Err(
            Error::new("Search is unimplemented for the maildir backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl MaildirType {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        settings: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        let mut mailboxes: HashMap<MailboxHash, MaildirMailbox> = Default::default();
        fn recurse_mailboxes<P: AsRef<Path>>(
            mailboxes: &mut HashMap<MailboxHash, MaildirMailbox>,
            settings: &AccountSettings,
            p: P,
        ) -> Result<Vec<MailboxHash>> {
            if !p.as_ref().try_exists().unwrap_or(false) || !p.as_ref().is_dir() {
                return Err(Error::new(format!(
                    "Configuration error: Path \"{}\" {}",
                    p.as_ref().display(),
                    if !p.as_ref().try_exists().unwrap_or(false) {
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
                                settings,
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
                                /* If directory is invalid (i.e. has no {cur,new,tmp}
                                 * subfolders), accept it ONLY if
                                 * it contains subdirs of any depth that are
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
                                        settings,
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
        }
        let root_mailbox = PathBuf::from(&settings.root_mailbox).expand();
        if !root_mailbox.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a valid directory.",
                settings.name,
                settings.root_mailbox.as_str()
            )));
        } else if !root_mailbox.is_dir() {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a directory.",
                settings.name,
                settings.root_mailbox.as_str()
            )));
        }

        if let Ok(f) = MaildirMailbox::new(
            root_mailbox.to_str().unwrap().to_string(),
            root_mailbox
                .file_name()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default()
                .to_string(),
            None,
            Vec::with_capacity(0),
            false,
            settings,
        ) {
            mailboxes.insert(f.hash, f);
        }

        if mailboxes.is_empty() {
            let children = recurse_mailboxes(&mut mailboxes, settings, &root_mailbox)?;
            for c in &children {
                if let Some(f) = mailboxes.get_mut(c) {
                    f.parent = None;
                }
            }
        } else {
            let root_hash = *mailboxes.keys().next().unwrap();
            let children = recurse_mailboxes(&mut mailboxes, settings, &root_mailbox)?;
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
                    _hash: fh,
                },
            );
        }
        Ok(Box::new(Self {
            name: settings.name.to_string(),
            mailboxes,
            hash_indexes: Arc::new(Mutex::new(hash_indexes)),
            mailbox_index: Default::default(),
            event_consumer,
            collection: Default::default(),
            path: root_mailbox,
        }))
    }

    pub fn save_to_mailbox(mut path: PathBuf, bytes: Vec<u8>, flags: Option<Flag>) -> Result<()> {
        for d in &["cur", "new", "tmp"] {
            path.push(d);
            if !path.is_dir() {
                return Err(Error::new(format!(
                    "{} is not a valid maildir mailbox",
                    path.display()
                )));
            }
            path.pop();
        }
        path.push("cur");
        {
            type BeBytes128 = [u8; 16];
            debug_assert_eq!(std::mem::size_of::<u64>(), 8);
            debug_assert_eq!(
                std::mem::size_of::<BeBytes128>(),
                2 * std::mem::size_of::<[u8; 8]>()
            );
            let mut rand_num: BeBytes128 = [0u8; 16];
            rand_num[0..8].copy_from_slice(&crate::utils::random::random_u64().to_be_bytes());
            rand_num[8..].copy_from_slice(&crate::utils::random::random_u64().to_be_bytes());
            let hostname = crate::utils::hostname::hostname()
                .ok()
                .and_then(|osstr| osstr.into_string().ok().map(Cow::Owned))
                .unwrap_or(Cow::Borrowed("localhost"));
            let mut filename = format!(
                "{}.{:x}_{}.{}:2,",
                crate::utils::random::clock_millis(),
                u128::from_be_bytes(rand_num),
                std::process::id(),
                hostname.trim()
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
        log::trace!("saving at {}", path.display());
        let file = fs::File::create(path).unwrap();
        let metadata = file.metadata()?;
        let mut permissions = metadata.permissions();

        permissions.set_mode(0o600); // Read/write for owner only.
        file.set_permissions(permissions)?;

        let mut writer = io::BufWriter::new(file);
        writer.write_all(&bytes).unwrap();
        Ok(())
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let root_mailbox = PathBuf::from(&s.root_mailbox).expand();
        if !root_mailbox.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a valid directory.",
                s.name,
                s.root_mailbox.as_str()
            )));
        } else if !root_mailbox.is_dir() {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a directory.",
                s.name,
                s.root_mailbox.as_str()
            )));
        }

        Ok(())
    }

    pub fn list_mail_in_maildir_fs(mut path: PathBuf, read_only: bool) -> Result<Vec<PathBuf>> {
        let mut files: Vec<PathBuf> = vec![];
        path.push("new");
        for p in path.read_dir()?.flatten() {
            if !read_only {
                move_to_cur(&p.path()).ok().take();
            } else {
                files.push(p.path());
            }
        }
        path.pop();
        path.push("cur");
        for e in path.read_dir()?.flatten() {
            files.push(e.path());
        }
        Ok(files)
    }
}

fn add_path_to_index(
    hash_index: &HashIndexes,
    mailbox_hash: MailboxHash,
    path: &Path,
    file_name: PathBuf,
    buf: &mut Vec<u8>,
) -> Result<Envelope> {
    log::trace!("add_path_to_index path {:?} filename{:?}", path, file_name);
    let env_hash = get_file_hash(path);
    hash_index
        .lock()
        .unwrap()
        .entry(mailbox_hash)
        .or_default()
        .insert(env_hash, path.to_path_buf().into());
    let mut reader = io::BufReader::new(fs::File::open(path)?);
    buf.clear();
    reader.read_to_end(buf)?;
    let mut env = Envelope::from_bytes(buf.as_slice(), Some(path.flags()))?;
    env.set_hash(env_hash);
    log::trace!(
        "add_path_to_index gen {}\t{}",
        env_hash,
        file_name.display()
    );
    Ok(env)
}
