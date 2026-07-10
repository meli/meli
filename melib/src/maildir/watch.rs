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

use std::sync::Arc;

use futures::{channel::mpsc, StreamExt};
use notify::{self, event::EventKind as NotifyEvent};

use crate::{
    backends::{prelude::*, RefreshEventKind::*},
    error::Result,
    maildir::{
        cache::Cache,
        utilities::{move_to_cur, MaildirFilePathExt},
        Configuration, MaildirType,
    },
};

pub struct MaildirWatch {
    pub watcher: Box<dyn notify::Watcher + Send>,
    pub account_hash: AccountHash,
    pub rx: mpsc::Receiver<std::result::Result<notify::Event, notify::Error>>,
    pub cache: Cache,
    pub config: Arc<Configuration>,
}

impl MaildirWatch {
    pub fn watch(self) -> impl futures::stream::Stream<Item = Result<BackendEvent>> {
        let Self {
            watcher,
            account_hash,
            mut rx,
            mut cache,
            config,
        } = self;

        try_fn_stream(|emitter| async move {
            // Move watcher to prevent it being Dropped.
            let _watcher = watcher;
            let mut events = vec![];
            'watch_loop: loop {
                #[allow(clippy::iter_with_drain)]
                for ev in events.drain(..) {
                    emitter.emit(ev).await;
                }
                let Some(ev) = rx.next().await else {
                    return Ok(());
                };
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
                            'path_loop: for mut pathbuf in event.paths {
                                if pathbuf.is_dir() {
                                    if let Ok(list) = MaildirType::list_mail_in_maildir_fs(
                                        &config, pathbuf, false,
                                    ) {
                                        pathbufs.extend(list);
                                    }
                                    continue 'path_loop;
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
                                if let Ok((mailbox_hash, env)) = cache.create(pathbuf.as_path()) {
                                    log::trace!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        pathbuf.display()
                                    );
                                    events.push(BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Create(Box::new(env)),
                                    }));
                                }
                            }
                        }
                        NotifyEvent::Modify(
                            notify::event::ModifyKind::Any
                            | notify::event::ModifyKind::Data(_)
                            | notify::event::ModifyKind::Other,
                        ) => {
                            log::trace!("Modify events: (path = {:?})", event.paths);
                            'path_loop: for pathbuf in event.paths {
                                let Some((mailbox_hash, old_hash)) = cache.remove(&pathbuf) else {
                                    // Did we just miss a Create event? In any case, create
                                    // envelope.
                                    if let Ok((mailbox_hash, env)) = cache.create(pathbuf.as_path())
                                    {
                                        log::trace!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            pathbuf.display()
                                        );
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Create(Box::new(env)),
                                        }));
                                    }
                                    continue 'path_loop;
                                };
                                if let Ok((_, env)) = cache.create(pathbuf.as_path()) {
                                    events.push(BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Update(old_hash, Box::new(env)),
                                    }));
                                } else {
                                    events.push(BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Remove(old_hash),
                                    }));
                                }
                            }
                        }
                        NotifyEvent::Remove(_) => {
                            for pathbuf in event.paths {
                                if let Some((mailbox_hash, env_hash)) = cache.remove(&pathbuf) {
                                    log::trace!("NotifyEvent::Remove(path = {:?}", pathbuf);
                                    events.push(BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Remove(env_hash),
                                    }));
                                }
                            }
                        }
                        NotifyEvent::Modify(notify::event::ModifyKind::Name(
                            notify::event::RenameMode::Both,
                        )) => {
                            let [ref src, ref dest] = event.paths[..] else {
                                continue 'watch_loop;
                            };
                            log::trace!("NotifyEvent::Rename(src = {:?}, dest = {:?})", src, dest);

                            if let Some((mailbox_hash, env_hash)) = cache.remove(dest) {
                                events.push(BackendEvent::Refresh(RefreshEvent {
                                    account_hash,
                                    mailbox_hash,
                                    kind: Remove(env_hash),
                                }));
                            }
                            if let Ok((mailbox_hash, env)) = cache.create(dest.as_path()) {
                                events.push(BackendEvent::Refresh(RefreshEvent {
                                    account_hash,
                                    mailbox_hash,
                                    kind: Create(Box::new(env)),
                                }));
                            }
                        }
                        NotifyEvent::Modify(notify::event::ModifyKind::Name(kind)) => {
                            log::trace!("NotifyEvent::Rename({kind:?})",);
                            'path_loop: for pathbuf in event.paths {
                                if cache.path_to_hash(pathbuf.as_path()).is_some() {
                                    if matches!(pathbuf.try_exists(), Ok(true)) {
                                        continue 'path_loop;
                                    }
                                    if let Some((mailbox_hash, env_hash)) = cache.remove(&pathbuf) {
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Remove(env_hash),
                                        }));
                                    }
                                } else {
                                    if let Ok((mailbox_hash, env)) = cache.create(pathbuf.as_path())
                                    {
                                        events.push(BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Create(Box::new(env)),
                                        }));
                                    }
                                }
                            }
                        }
                        NotifyEvent::Modify(notify::event::ModifyKind::Metadata(_)) => {
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
        })
    }
}
