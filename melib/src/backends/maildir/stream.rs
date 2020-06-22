/*
 * meli - maildir async
 *
 * Copyright 2020 Manos Pitsidianakis
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


use futures::task::{Context, Poll};
use core::pin::Pin;
use super::*;
use std::sync::{Arc,Mutex};
use std::io::{self, Read, Write};
use std::ops::{Deref, DerefMut};
use std::os::unix::fs::PermissionsExt;
use std::path::{Component, Path, PathBuf};
use std::result;
use std::sync::mpsc::channel;
use crate::backends::maildir::backend::move_to_cur;
use core::future::Future;
use futures::stream::FuturesUnordered;

pub struct MaildirStream {
    mailbox_hash: MailboxHash,
    unseen: Arc<Mutex<usize>>,
    total: Arc<Mutex<usize>>,
    path: PathBuf,
    root_path: PathBuf,
    map: HashIndexes,
    mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    payloads: Pin<Box<FuturesUnordered<Pin<Box<dyn Future<Output = Result<Vec<Envelope>>>>>>>>,
 }

impl MaildirStream {
    pub fn new(name: &str, mailbox_hash: MailboxHash, unseen: Arc<Mutex<usize>>, total: Arc<Mutex<usize>>, mut path: PathBuf, root_path: PathBuf, map: HashIndexes, mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>) -> Result<Box<dyn Stream<Item = Result<Vec<Envelope>>>>> {
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
        }let payloads =Box::pin(
        if !files.is_empty() {
            let cores = 4_usize;
            let chunk_size = if count / cores > 0 {
                count / cores
            } else {
                count
            };
            files.chunks(chunk_size).map(|chunk| {
                //Self::chunk(chunk, name, mailbox_hash, unseen, total, path, root_path, map, mailbox_index)})
        let cache_dir = xdg::BaseDirectories::with_profile("meli", &name).unwrap();
                Box::pin(Self::chunk(SmallVec::from(chunk), cache_dir, mailbox_hash, unseen.clone(), total.clone(), path.clone(), root_path.clone(), map.clone(), mailbox_index.clone())) as Pin<Box<dyn Future<Output = _>>>})
                .collect::<_>()


        } else {  FuturesUnordered::new() });
        Ok(Box::new(Self{mailbox_hash, unseen, total, path, root_path, map, mailbox_index, payloads}))
    }

    async fn chunk(chunk:SmallVec<[std::path::PathBuf; 2048]>, cache_dir:xdg::BaseDirectories, mailbox_hash: MailboxHash, unseen: Arc<Mutex<usize>>, total: Arc<Mutex<usize>>, path: PathBuf, root_path: PathBuf, map: HashIndexes, mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>) -> Result<Vec<Envelope>> {
        let unseen = unseen.clone();
        let total = total.clone();
        let map = map.clone();
        let mailbox_index = mailbox_index.clone();
        let root_path = root_path.clone();
        let len = chunk.len();
        let size = if len <= 100 { 100 } else { (len / 100) * 100 };
        let mut local_r: Vec<Envelope> =
            Vec::with_capacity(chunk.len());
        for c in chunk.chunks(size) {
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
                if let Some(e) = Envelope::from_token(op, hash) {
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
        }
        Ok(local_r)
    }
}

impl Stream for MaildirStream {
    type Item = Result<Vec<Envelope>>;
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>>{
        //todo!()
        let payloads = self.payloads.as_mut();
        payloads.poll_next(cx)
    }
}
