/*
 * meli - jmap module.
 *
 * Copyright 2017 - 2019 Manos Pitsidianakis
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
use std::cell::Cell;
use std::sync::{Arc, RwLock};

/// `BackendOp` implementor for Imap
#[derive(Debug, Clone)]
pub struct JmapOp {
    hash: EnvelopeHash,
    connection: Arc<JmapConnection>,
    store: Arc<RwLock<Store>>,
    bytes: Option<String>,
    flags: Cell<Option<Flag>>,
    headers: Option<String>,
    body: Option<String>,
}

impl JmapOp {
    pub fn new(
        hash: EnvelopeHash,
        connection: Arc<JmapConnection>,
        store: Arc<RwLock<Store>>,
    ) -> Self {
        JmapOp {
            hash,
            connection,
            store,
            bytes: None,
            headers: None,
            body: None,
            flags: Cell::new(None),
        }
    }
}

impl BackendOp for JmapOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        if self.bytes.is_none() {
            let mut store_lck = self.store.write().unwrap();
            if !(store_lck.byte_cache.contains_key(&self.hash)
                && store_lck.byte_cache[&self.hash].bytes.is_some())
            {
                let blob_id = &store_lck.blob_id_store[&self.hash];
                let res = self
                    .connection
                    .client
                    .lock()
                    .unwrap()
                    .get(&download_request_format(
                        &self.connection.session,
                        self.connection.mail_account_id(),
                        blob_id,
                        None,
                    ))
                    .basic_auth(
                        &self.connection.server_conf.server_username,
                        Some(&self.connection.server_conf.server_password),
                    )
                    .send();

                let res_text = res?.text()?;

                store_lck.byte_cache.entry(self.hash).or_default().bytes = Some(res_text);
            }
            self.bytes = store_lck.byte_cache[&self.hash].bytes.clone();
        }
        let ret = self.bytes.as_ref().unwrap().as_bytes().to_vec();
        Ok(Box::pin(async move { Ok(ret) }))
    }

    fn fetch_flags(&self) -> ResultFuture<Flag> {
        Ok(Box::pin(async { Ok(Flag::default()) }))
    }
}
