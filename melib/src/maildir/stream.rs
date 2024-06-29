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

use std::{
    future::Future,
    io::{self, Read},
    path::PathBuf,
    pin::Pin,
    sync::{Arc, Mutex},
};

use futures::{
    stream::{FuturesUnordered, StreamExt},
    task::{Context, Poll},
};

use super::*;
use crate::maildir::backend::move_to_cur;

type Payload = Pin<Box<dyn Future<Output = Result<Vec<Envelope>>> + Send + 'static>>;

pub struct MaildirStream {
    payloads: Pin<Box<FuturesUnordered<Payload>>>,
}

impl MaildirStream {
    #[allow(clippy::type_complexity, clippy::new_ret_no_self)]
    pub fn new(
        mailbox_hash: MailboxHash,
        unseen: Arc<Mutex<usize>>,
        total: Arc<Mutex<usize>>,
        mut path: PathBuf,
        map: HashIndexes,
        mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let chunk_size = 2048;
        path.push("new");
        for p in path.read_dir()?.flatten() {
            move_to_cur(&p.path()).ok().take();
        }
        path.pop();
        path.push("cur");
        let files: Vec<PathBuf> = path
            .read_dir()?
            .flatten()
            .map(|e| e.path())
            .collect::<Vec<_>>();
        let payloads = Box::pin(if !files.is_empty() {
            files
                .chunks(chunk_size)
                .map(|chunk| {
                    Box::pin(Self::chunk(
                        SmallVec::from(chunk),
                        mailbox_hash,
                        unseen.clone(),
                        total.clone(),
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
        mailbox_hash: MailboxHash,
        unseen: Arc<Mutex<usize>>,
        total: Arc<Mutex<usize>>,
        map: HashIndexes,
        mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    ) -> Result<Vec<Envelope>> {
        let mut local_r: Vec<Envelope> = Vec::with_capacity(chunk.len());
        let mut unseen_total: usize = 0;
        let mut buf = Vec::with_capacity(4096);
        for file in chunk {
            let env_hash = file.to_envelope_hash();
            {
                map.lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .or_default()
                    .insert(env_hash, PathBuf::from(&file).into());
            }
            let mut reader = io::BufReader::new(fs::File::open(&file)?);
            buf.clear();
            reader.read_to_end(&mut buf)?;
            match Envelope::from_bytes(buf.as_slice(), Some(file.flags())) {
                Ok(mut env) => {
                    env.set_hash(env_hash);
                    mailbox_index.lock().unwrap().insert(env_hash, mailbox_hash);
                    if !env.is_seen() {
                        unseen_total += 1;
                    }
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
        *total.lock().unwrap() += local_r.len();
        *unseen.lock().unwrap() += unseen_total;
        Ok(local_r)
    }
}

impl Stream for MaildirStream {
    type Item = Result<Vec<Envelope>>;
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let payloads = self.payloads.as_mut();
        payloads.poll_next(cx)
    }
}
