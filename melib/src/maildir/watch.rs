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
    path::{Path, PathBuf},
    sync::{mpsc, Arc, Mutex},
};

use notify::{self, event::EventKind as NotifyEvent};

use crate::{
    backends::{prelude::*, RefreshEventKind::*},
    error,
    maildir::{move_to_cur, HashIndexes, MaildirPathTrait, PathMod},
};

pub struct MaildirWatch {
    pub watcher: Box<dyn notify::Watcher + Send>,
    pub account_hash: AccountHash,
    pub event_consumer: BackendEventConsumer,
    pub root_mailbox: PathBuf,
    pub rx: mpsc::Receiver<std::result::Result<notify::Event, notify::Error>>,
    pub hash_indexes: HashIndexes,
    pub mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    pub root_mailbox_hash: MailboxHash,
    #[allow(clippy::type_complexity)]
    pub mailbox_counts: HashMap<MailboxHash, (Arc<Mutex<usize>>, Arc<Mutex<usize>>)>,
}

impl MaildirWatch {
    pub async fn watch(self) -> error::Result<()> {
        let Self {
            watcher: _watcher,
            account_hash,
            event_consumer,
            root_mailbox: _root_mailbox,
            rx,
            hash_indexes,
            mailbox_index,
            root_mailbox_hash,
            mailbox_counts,
        } = self;

        let mut buf = Vec::with_capacity(4096);
        loop {
            match rx.recv() {
                Ok(Ok(event)) => match event.kind {
                    NotifyEvent::Create(_) => {
                        log::trace!("Create events: (path = {:?})", event.paths);
                        for mut pathbuf in event.paths {
                            if pathbuf.is_in_new() {
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
                            let mailbox_hash: MailboxHash = pathbuf.to_mailbox_hash();
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
                                    *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                }
                                *mailbox_counts[&mailbox_hash].1.lock().unwrap() += 1;
                                (event_consumer)(
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
                        log::trace!("Modify events: (path = {:?})", event.paths);
                        for pathbuf in event.paths {
                            let mailbox_hash: MailboxHash = pathbuf.to_mailbox_hash();
                            let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                            let index_lock =
                                &mut hash_indexes_lock.entry(mailbox_hash).or_default();
                            // Linear search in hash_index to find old hash
                            let old_hash: EnvelopeHash = {
                                if let Some((k, v)) =
                                    index_lock.iter_mut().find(|(_, v)| *v.buf == pathbuf)
                                {
                                    *v = pathbuf.clone().into();
                                    *k
                                } else {
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
                                        (event_consumer)(
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
                            let new_hash: EnvelopeHash = pathbuf.to_envelope_hash();
                            let mut reader = io::BufReader::new(fs::File::open(&pathbuf)?);
                            buf.clear();
                            reader.read_to_end(&mut buf)?;
                            if index_lock.get_mut(&new_hash).is_none() {
                                if let Ok(mut env) =
                                    Envelope::from_bytes(buf.as_slice(), Some(pathbuf.flags()))
                                {
                                    env.set_hash(new_hash);
                                    index_lock.insert(new_hash, pathbuf.into());
                                    (event_consumer)(
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
                            log::trace!("NotifyEvent::Remove(path = {:?}", pathbuf);
                            let mailbox_hash: MailboxHash = pathbuf.to_mailbox_hash();
                            let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                            let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                            let hash: EnvelopeHash = if let Some((k, _)) =
                                index_lock.iter().find(|(_, v)| *v.buf == pathbuf)
                            {
                                *k
                            } else {
                                log::trace!("removed but not contained in index");
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

                            (event_consumer)(
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
                        log::trace!("NotifyEvent::Rename(src = {:?}, dest = {:?})", src, dest);
                        let mailbox_hash: MailboxHash = src.to_mailbox_hash();
                        let dest_mailbox = {
                            let dest_mailbox: MailboxHash = dest.to_mailbox_hash();
                            if dest_mailbox == mailbox_hash {
                                None
                            } else {
                                Some(dest_mailbox)
                            }
                        };
                        let old_hash: EnvelopeHash = src.to_envelope_hash();
                        let new_hash: EnvelopeHash = dest.to_envelope_hash();

                        let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                        let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                        let old_flags = src.flags();
                        let new_flags = dest.flags();
                        let was_seen: bool = old_flags.contains(Flag::SEEN);
                        let is_seen: bool = new_flags.contains(Flag::SEEN);

                        if index_lock.contains_key(&old_hash) && !index_lock[&old_hash].removed {
                            if let Some(dest_mailbox) = dest_mailbox {
                                index_lock.entry(old_hash).and_modify(|e| {
                                    e.removed = true;
                                });

                                (event_consumer)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Remove(old_hash),
                                    }),
                                );
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
                                        *mailbox_counts[&dest_mailbox].0.lock().unwrap() += 1;
                                    }
                                    *mailbox_counts[&dest_mailbox].1.lock().unwrap() += 1;
                                    (event_consumer)(
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
                                (event_consumer)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Rename(old_hash, new_hash),
                                    }),
                                );
                                if !was_seen && is_seen {
                                    let mut lck = mailbox_counts[&mailbox_hash].0.lock().unwrap();
                                    *lck = lck.saturating_sub(1);
                                } else if was_seen && !is_seen {
                                    *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                }
                                if old_flags != new_flags {
                                    (event_consumer)(
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
                            drop(hash_indexes_lock);
                            if let Ok(env) = add_path_to_index(
                                &hash_indexes,
                                dest_mailbox.unwrap_or(mailbox_hash),
                                dest.as_path(),
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
                                (event_consumer)(
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
                                    *mailbox_counts[&dest_mailbox].0.lock().unwrap() += 1;
                                }
                                *mailbox_counts[&dest_mailbox].1.lock().unwrap() += 1;
                                (event_consumer)(
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
                            (event_consumer)(
                                account_hash,
                                BackendEvent::Refresh(RefreshEvent {
                                    account_hash,
                                    mailbox_hash,
                                    kind: Rename(old_hash, new_hash),
                                }),
                            );
                            log::trace!("contains_new_key");
                            if old_flags != new_flags {
                                (event_consumer)(
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
                        // Trigger rescan of mailbox.
                        (event_consumer)(
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
    }
}

fn add_path_to_index(
    hash_index: &HashIndexes,
    mailbox_hash: MailboxHash,
    path: &Path,
    buf: &mut Vec<u8>,
) -> error::Result<Envelope> {
    log::trace!(
        "add_path_to_index path {:?} filename{:?}",
        path,
        path.file_name()
    );
    let env_hash = path.to_envelope_hash();
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
    log::trace!("add_path_to_index gen {}\t{:?}", env_hash, path.file_name());
    Ok(env)
}
