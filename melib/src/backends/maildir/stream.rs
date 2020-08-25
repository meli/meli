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

use super::*;
use crate::backends::maildir::backend::move_to_cur;
use core::future::Future;
use core::pin::Pin;
use futures::stream::{FuturesUnordered, StreamExt};
use futures::task::{Context, Poll};
use memmap::{Mmap, Protection};
use std::io::{self};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::result;
use std::sync::{Arc, Mutex};

pub struct MaildirStream {
    payloads: Pin<
        Box<
            FuturesUnordered<Pin<Box<dyn Future<Output = Result<Vec<Envelope>>> + Send + 'static>>>,
        >,
    >,
}

impl MaildirStream {
    pub fn new(
        name: &str,
        mailbox_hash: MailboxHash,
        unseen: Arc<Mutex<usize>>,
        total: Arc<Mutex<usize>>,
        mut path: PathBuf,
        root_path: PathBuf,
        map: HashIndexes,
        mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
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
        let payloads = Box::pin(if !files.is_empty() {
            let cores = 4_usize;
            let chunk_size = if count / cores > 0 {
                count / cores
            } else {
                count
            };
            files
                .chunks(chunk_size)
                .map(|chunk| {
                    //Self::chunk(chunk, name, mailbox_hash, unseen, total, path, root_path, map, mailbox_index)})
                    let cache_dir = xdg::BaseDirectories::with_profile("meli", &name).unwrap();
                    Box::pin(Self::chunk(
                        SmallVec::from(chunk),
                        cache_dir,
                        mailbox_hash,
                        unseen.clone(),
                        total.clone(),
                        root_path.clone(),
                        map.clone(),
                        mailbox_index.clone(),
                    )) as Pin<Box<dyn Future<Output = _> + Send + 'static>>
                })
                .collect::<_>()
        } else {
            FuturesUnordered::new()
        });
        Ok(Self { payloads }.boxed())
    }

    async fn chunk(
        chunk: SmallVec<[std::path::PathBuf; 2048]>,
        cache_dir: xdg::BaseDirectories,
        mailbox_hash: MailboxHash,
        unseen: Arc<Mutex<usize>>,
        total: Arc<Mutex<usize>>,
        root_path: PathBuf,
        map: HashIndexes,
        mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    ) -> Result<Vec<Envelope>> {
        let len = chunk.len();
        let size = if len <= 100 { 100 } else { (len / 100) * 100 };
        let mut local_r: Vec<Envelope> = Vec::with_capacity(chunk.len());
        for c in chunk.chunks(size) {
            let map = map.clone();
            for file in c {
                /* Check if we have a cache file with this email's
                 * filename */
                let file_name = PathBuf::from(file)
                    .strip_prefix(&root_path)
                    .unwrap()
                    .to_path_buf();
                if let Some(cached) = cache_dir.find_cache_file(&file_name) {
                    /* Cached struct exists, try to load it */
                    let reader = io::BufReader::new(fs::File::open(&cached).unwrap());
                    let result: result::Result<Envelope, _> = bincode::deserialize_from(reader);
                    if let Ok(env) = result {
                        let mut map = map.lock().unwrap();
                        let map = map.entry(mailbox_hash).or_default();
                        let hash = env.hash();
                        map.insert(hash, file.clone().into());
                        mailbox_index.lock().unwrap().insert(hash, mailbox_hash);
                        if !env.is_seen() {
                            *unseen.lock().unwrap() += 1;
                        }
                        *total.lock().unwrap() += 1;
                        local_r.push(env);
                        continue;
                    }
                };
                let env_hash = get_file_hash(file);
                {
                    let mut map = map.lock().unwrap();
                    let map = map.entry(mailbox_hash).or_default();
                    (*map).insert(env_hash, PathBuf::from(file).into());
                }
                match Envelope::from_bytes(
                    unsafe { &Mmap::open_path(&file, Protection::Read)?.as_slice() },
                    None,
                ) {
                    Ok(mut env) => {
                        env.set_hash(env_hash);
                        mailbox_index.lock().unwrap().insert(env_hash, mailbox_hash);
                        if let Ok(cached) = cache_dir.place_cache_file(file_name) {
                            /* place result in cache directory */
                            let f = fs::File::create(cached)?;
                            let metadata = f.metadata()?;
                            let mut permissions = metadata.permissions();

                            permissions.set_mode(0o600); // Read/write for owner only.
                            f.set_permissions(permissions)?;

                            let writer = io::BufWriter::new(f);
                            bincode::serialize_into(writer, &env)?;
                        }
                        if !env.is_seen() {
                            *unseen.lock().unwrap() += 1;
                        }
                        *total.lock().unwrap() += 1;
                        local_r.push(env);
                    }
                    Err(err) => {
                        debug!(
                            "DEBUG: hash {}, path: {} couldn't be parsed, {}",
                            env_hash,
                            file.as_path().display(),
                            err,
                        );
                        continue;
                    }
                }
            }
        }
        Ok(local_r)
    }
}

impl Stream for MaildirStream {
    type Item = Result<Vec<Envelope>>;
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        //todo!()
        let payloads = self.payloads.as_mut();
        payloads.poll_next(cx)
    }
}
