//
// meli
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{
    collections::HashMap,
    fs,
    io::{self, Read},
    path::Path,
    sync::{Arc, Mutex},
};

use futures::{channel::mpsc, StreamExt};
use notify::{self, event::EventKind as NotifyEvent};

use crate::{
    backends::{prelude::*, RefreshEventKind::*},
    error::Result,
    maildir::{
        utilities::{
            move_to_cur, HashIndex, HashIndexes, MaildirFilePathExt, MaildirMailboxPathExt, PathMod,
        },
        Configuration, MaildirType,
    },
};

pub struct MaildirWatch {
    pub watcher: Box<dyn notify::Watcher + Send>,
    pub account_hash: AccountHash,
    pub rx: mpsc::Receiver<std::result::Result<notify::Event, notify::Error>>,
    pub hash_indexes: HashIndexes,
    pub mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    pub config: Arc<Configuration>,
    #[allow(clippy::type_complexity)]
    pub mailbox_counts: HashMap<MailboxHash, (Arc<Mutex<usize>>, Arc<Mutex<usize>>)>,
}

#[inline]
fn path_to_mailbox_hash(
    path: &Path,
    hash_indexes: &HashMap<MailboxHash, HashIndex>,
) -> Option<MailboxHash> {
    path.to_mailbox_hash().or_else(|| {
        hash_indexes.iter().find_map(|(k, v)| {
            if v.path_to_hash(path).is_some() {
                Some(*k)
            } else {
                None
            }
        })
    })
}

#[inline]
fn find_env_hash(
    path: &Path,
    hash_indexes: &HashMap<MailboxHash, HashIndex>,
) -> Option<(MailboxHash, EnvelopeHash)> {
    hash_indexes
        .iter()
        .find_map(|(k, v)| v.path_to_hash(path).map(|v| (*k, v)))
}

impl MaildirWatch {
    pub fn watch(self) -> impl futures::stream::Stream<Item = Result<BackendEvent>> {
        let Self {
            watcher,
            account_hash,
            mut rx,
            hash_indexes,
            mailbox_index,
            mut mailbox_counts,
            config,
        } = self;

        try_fn_stream(|emitter| async move {
            // Move watcher to prevent it being Dropped.
            let _watcher = watcher;
            let mut buf = Vec::with_capacity(4096);
            let mut events = vec![];
            'watch_loop: loop {
                #[allow(clippy::iter_with_drain)]
                for ev in events.drain(..) {
                    emitter.emit(ev).await;
                }
                while let Some(ev) = rx.next().await {
                    match ev {
                        Ok(event) => match event.kind {
                            NotifyEvent::Any => {
                                log::trace!("Any event: {:?}", event);
                            }
                            NotifyEvent::Access(_) => {
                                // We don't care about access events.
                            }
                            NotifyEvent::Create(_) => {
                                log::trace!("Create events: (paths = {:?})", event.paths);
                                let mut pathbufs = vec![];
                                for mut pathbuf in event.paths {
                                    if pathbuf.is_dir() {
                                        if let Ok(list) = MaildirType::list_mail_in_maildir_fs(
                                            &config, pathbuf, false,
                                        ) {
                                            pathbufs.extend(list);
                                        }
                                        continue;
                                    }
                                    if pathbuf.is_in_new() {
                                        // This creates a Rename event that we will receive later
                                        pathbuf = match move_to_cur(&config, &pathbuf) {
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
                                    pathbufs.push(pathbuf);
                                }
                                for pathbuf in pathbufs {
                                    let Some(mailbox_hash): Option<MailboxHash> =
                                        pathbuf.to_mailbox_hash()
                                    else {
                                        continue;
                                    };
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        mailbox_hash,
                                        pathbuf.as_path(),
                                        &mut buf,
                                    ) {
                                        mailbox_index
                                            .lock()
                                            .unwrap()
                                            .insert(env.hash(), mailbox_hash);
                                        log::trace!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            pathbuf.display()
                                        );
                                        if !env.is_seen() {
                                            *mailbox_counts
                                                .entry(mailbox_hash)
                                                .or_default()
                                                .0
                                                .lock()
                                                .unwrap() += 1;
                                        }
                                        *mailbox_counts
                                            .entry(mailbox_hash)
                                            .or_default()
                                            .1
                                            .lock()
                                            .unwrap() += 1;
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Create(Box::new(env)),
                                        }));
                                    }
                                }
                                if !events.is_empty() {
                                    continue 'watch_loop;
                                }
                            }
                            NotifyEvent::Modify(
                                notify::event::ModifyKind::Any
                                | notify::event::ModifyKind::Data(_)
                                | notify::event::ModifyKind::Other,
                            ) => {
                                log::trace!("Modify events: (path = {:?})", event.paths);
                                for pathbuf in event.paths {
                                    let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                    let Some(mailbox_hash): Option<MailboxHash> =
                                        path_to_mailbox_hash(&pathbuf, &hash_indexes_lock)
                                    else {
                                        continue;
                                    };
                                    // Linear search in hash_index to find old hash
                                    let old_hash: EnvelopeHash = {
                                        let index_lock =
                                            hash_indexes_lock.entry(mailbox_hash).or_default();
                                        if let Some((k, v)) =
                                            index_lock.iter_mut().find(|(_, v)| *v.buf == pathbuf)
                                        {
                                            *v = pathbuf.clone().into();
                                            *k
                                        } else {
                                            // Path is not in hash index.
                                            drop(hash_indexes_lock);
                                            // Did we just miss a Create event? In any case, create
                                            // envelope.
                                            if let Ok(env) = add_path_to_index(
                                                &hash_indexes,
                                                mailbox_hash,
                                                pathbuf.as_path(),
                                                &mut buf,
                                            ) {
                                                mailbox_index
                                                    .lock()
                                                    .unwrap()
                                                    .insert(env.hash(), mailbox_hash);
                                                events.push(BackendEvent::Refresh(RefreshEvent {
                                                    account_hash,
                                                    mailbox_hash,
                                                    kind: Create(Box::new(env)),
                                                }));
                                            } else if let Some((mailbox_hash, env_hash)) =
                                                find_env_hash(
                                                    &pathbuf,
                                                    &hash_indexes.lock().unwrap(),
                                                )
                                            {
                                                {
                                                    let mut mi = mailbox_index.lock().unwrap();
                                                    mi.remove(&env_hash);
                                                }
                                                {
                                                    let mut hi = hash_indexes.lock().unwrap();
                                                    hi.entry(mailbox_hash)
                                                        .or_default()
                                                        .remove_env_hash(&env_hash);
                                                }
                                                events.push(BackendEvent::Refresh(RefreshEvent {
                                                    account_hash,
                                                    mailbox_hash,
                                                    kind: Remove(env_hash),
                                                }));
                                            }
                                            continue;
                                        }
                                    };
                                    drop(hash_indexes_lock);
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        mailbox_hash,
                                        pathbuf.as_path(),
                                        &mut buf,
                                    ) {
                                        {
                                            let mut mi = mailbox_index.lock().unwrap();
                                            mi.remove(&old_hash);
                                            mi.insert(env.hash(), mailbox_hash);
                                        }
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Update(old_hash, Box::new(env)),
                                        }));
                                    } else {
                                        {
                                            let mut mi = mailbox_index.lock().unwrap();
                                            mi.remove(&old_hash);
                                        }
                                        {
                                            let mut hi = hash_indexes.lock().unwrap();
                                            hi.entry(mailbox_hash)
                                                .or_default()
                                                .remove_env_hash(&old_hash);
                                        }
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Remove(old_hash),
                                        }));
                                    }
                                }
                                if !events.is_empty() {
                                    continue 'watch_loop;
                                }
                            }
                            NotifyEvent::Remove(_) => {
                                for pathbuf in event.paths {
                                    log::trace!("NotifyEvent::Remove(path = {:?}", pathbuf);
                                    let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                    let Some(mailbox_hash): Option<MailboxHash> =
                                        path_to_mailbox_hash(&pathbuf, &hash_indexes_lock)
                                    else {
                                        continue;
                                    };
                                    let index_lock =
                                        hash_indexes_lock.entry(mailbox_hash).or_default();
                                    let hash: EnvelopeHash = if let Some((k, _)) =
                                        index_lock.iter().find(|(_, v)| *v.buf == pathbuf)
                                    {
                                        *k
                                    } else {
                                        log::trace!("removed but not contained in index");
                                        continue 'watch_loop;
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
                                        continue 'watch_loop;
                                    }
                                    {
                                        let mut lck = mailbox_counts
                                            .entry(mailbox_hash)
                                            .or_default()
                                            .1
                                            .lock()
                                            .unwrap();
                                        *lck = lck.saturating_sub(1);
                                    }
                                    if !pathbuf.flags().contains(Flag::SEEN) {
                                        let mut lck = mailbox_counts
                                            .entry(mailbox_hash)
                                            .or_default()
                                            .0
                                            .lock()
                                            .unwrap();
                                        *lck = lck.saturating_sub(1);
                                    }

                                    index_lock.entry(hash).and_modify(|e| {
                                        e.removed = true;
                                    });

                                    events.push(BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Remove(hash),
                                    }));
                                }
                                if !events.is_empty() {
                                    continue 'watch_loop;
                                }
                            }
                            NotifyEvent::Modify(notify::event::ModifyKind::Name(
                                notify::event::RenameMode::Both,
                            )) if event.paths.len() == 2 => {
                                let [ref src, ref dest] = event.paths[..] else {
                                    unreachable!()
                                };
                                log::trace!(
                                    "NotifyEvent::Rename(src = {:?}, dest = {:?})",
                                    src,
                                    dest
                                );
                                let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                                let Some(mailbox_hash): Option<MailboxHash> =
                                    path_to_mailbox_hash(src, &hash_indexes_lock)
                                else {
                                    continue;
                                };
                                let dest_mailbox = {
                                    let Some(dest_mailbox): Option<MailboxHash> =
                                        path_to_mailbox_hash(dest, &hash_indexes_lock)
                                    else {
                                        continue;
                                    };
                                    if dest_mailbox == mailbox_hash {
                                        None
                                    } else {
                                        Some(dest_mailbox)
                                    }
                                };
                                let old_hash: EnvelopeHash = src.to_envelope_hash();
                                let new_hash: EnvelopeHash = dest.to_envelope_hash();

                                let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                                let old_flags = src.flags();
                                let new_flags = dest.flags();
                                let was_seen: bool = old_flags.contains(Flag::SEEN);
                                let is_seen: bool = new_flags.contains(Flag::SEEN);

                                if index_lock.contains_key(&old_hash)
                                    && !index_lock[&old_hash].removed
                                {
                                    if let Some(dest_mailbox) = dest_mailbox {
                                        index_lock.entry(old_hash).and_modify(|e| {
                                            e.removed = true;
                                        });

                                        drop(hash_indexes_lock);
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Remove(old_hash),
                                        }));
                                        if let Ok(env) = add_path_to_index(
                                            &hash_indexes,
                                            dest_mailbox,
                                            dest.as_path(),
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
                                                *mailbox_counts
                                                    .entry(dest_mailbox)
                                                    .or_default()
                                                    .0
                                                    .lock()
                                                    .unwrap() += 1;
                                            }
                                            *mailbox_counts
                                                .entry(dest_mailbox)
                                                .or_default()
                                                .1
                                                .lock()
                                                .unwrap() += 1;
                                            events.push(BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash: dest_mailbox,
                                                kind: Create(Box::new(env)),
                                            }));
                                        }
                                    } else {
                                        index_lock.entry(old_hash).and_modify(|e| {
                                            e.modified = Some(PathMod::Hash(new_hash));
                                        });
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Rename(old_hash, new_hash),
                                        }));
                                        if !was_seen && is_seen {
                                            let mut lck = mailbox_counts
                                                .entry(mailbox_hash)
                                                .or_default()
                                                .0
                                                .lock()
                                                .unwrap();
                                            *lck = lck.saturating_sub(1);
                                        } else if was_seen && !is_seen {
                                            *mailbox_counts
                                                .entry(mailbox_hash)
                                                .or_default()
                                                .0
                                                .lock()
                                                .unwrap() += 1;
                                        }
                                        if old_flags != new_flags {
                                            events.push(BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: NewFlags(new_hash, (new_flags, vec![])),
                                            }));
                                        }
                                        mailbox_index
                                            .lock()
                                            .unwrap()
                                            .insert(new_hash, mailbox_hash);
                                        index_lock.insert(new_hash, dest.to_path_buf().into());
                                    }
                                    continue 'watch_loop;
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
                                            "contains_old_key, key was marked as removed (by \
                                             external source)"
                                        );
                                    } else {
                                        log::trace!("not contains_new_key");
                                    }
                                    drop(hash_indexes_lock);
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        dest_mailbox.unwrap_or(mailbox_hash),
                                        dest.as_path(),
                                        &mut buf,
                                    ) {
                                        mailbox_index.lock().unwrap().insert(
                                            env.hash(),
                                            dest_mailbox.unwrap_or(mailbox_hash),
                                        );
                                        log::trace!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            dest.display()
                                        );
                                        if !env.is_seen() {
                                            *mailbox_counts
                                                .entry(dest_mailbox.unwrap_or(mailbox_hash))
                                                .or_default()
                                                .0
                                                .lock()
                                                .unwrap() += 1;
                                        }
                                        *mailbox_counts
                                            .entry(dest_mailbox.unwrap_or(mailbox_hash))
                                            .or_default()
                                            .1
                                            .lock()
                                            .unwrap() += 1;
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: dest_mailbox.unwrap_or(mailbox_hash),
                                            kind: Create(Box::new(env)),
                                        }));
                                        continue 'watch_loop;
                                    } else {
                                        log::trace!("not valid email");
                                    }
                                } else if let Some(dest_mailbox) = dest_mailbox {
                                    drop(hash_indexes_lock);
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        dest_mailbox,
                                        dest.as_path(),
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
                                            *mailbox_counts
                                                .entry(dest_mailbox)
                                                .or_default()
                                                .0
                                                .lock()
                                                .unwrap() += 1;
                                        }
                                        *mailbox_counts
                                            .entry(dest_mailbox)
                                            .or_default()
                                            .1
                                            .lock()
                                            .unwrap() += 1;
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: dest_mailbox,
                                            kind: Create(Box::new(env)),
                                        }));
                                    }
                                } else {
                                    if was_seen && !is_seen {
                                        *mailbox_counts
                                            .entry(mailbox_hash)
                                            .or_default()
                                            .0
                                            .lock()
                                            .unwrap() += 1;
                                    }
                                    events.push(BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Rename(old_hash, new_hash),
                                    }));
                                    log::trace!("contains_new_key");
                                    if old_flags != new_flags {
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: NewFlags(new_hash, (new_flags, vec![])),
                                        }));
                                    }
                                }
                                if !events.is_empty() {
                                    continue 'watch_loop;
                                }
                            }
                            NotifyEvent::Modify(_) => {
                                log::trace!("Ignored Modify event: {:?}", event);
                            }
                            NotifyEvent::Other => {
                                log::trace!("Ignored Other event: {:?}", event);
                            }
                        },
                        Err(err) => {
                            log::debug!("watch error: {}", err);
                            return Err(err.into());
                        }
                    }
                }
                return Ok(());
            }
        })
    }
}

fn add_path_to_index(
    hash_index: &HashIndexes,
    mailbox_hash: MailboxHash,
    path: &Path,
    buf: &mut Vec<u8>,
) -> Result<Envelope> {
    log::trace!(
        "add_path_to_index path {:?} filename{:?}",
        path,
        path.file_name()
    );
    let env_hash = path.to_envelope_hash();
    let mut reader = io::BufReader::new(fs::File::open(path)?);
    buf.clear();
    reader.read_to_end(buf)?;
    let mut env = Envelope::from_bytes(buf.as_slice(), Some(path.flags()))?;
    env.set_hash(env_hash);
    {
        let mut lck = hash_index.lock().unwrap();
        let hi = lck.entry(mailbox_hash).or_default();
        hi.index.insert(env_hash, path.to_path_buf().into());
        hi.reverse_index.insert(path.to_path_buf(), env_hash);
    }
    log::trace!("add_path_to_index gen {}\t{:?}", env_hash, path.file_name());
    Ok(env)
}
