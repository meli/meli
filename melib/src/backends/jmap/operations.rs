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

use crate::backends::BackendOp;
use crate::error::Result;
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
    fn description(&self) -> String {
        self.store
            .try_read()
            .and_then(|store_lck| Ok(store_lck.id_store[&self.hash].clone()))
            .unwrap_or(String::new())
    }

    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.bytes.is_none() {
            let mut store_lck = self.store.write().unwrap();
            if !(store_lck.byte_cache.contains_key(&self.hash)
                && store_lck.byte_cache[&self.hash].bytes.is_some())
            {
                let blob_id = &store_lck.blob_id_store[&self.hash];
                let res = self.connection
        .client
        .lock()
        .unwrap()
        .get(&format!("https://jmap-proxy.local/raw/fc32dffe-14e7-11ea-a277-2477037a1804/{blob_id}/{name}", blob_id = blob_id, name = ""))
        .send();

                let res_text = res?.text()?;

                store_lck.byte_cache.entry(self.hash).or_default().bytes = Some(res_text);
            }
            self.bytes = store_lck.byte_cache[&self.hash].bytes.clone();
        }
        Ok(&self.bytes.as_ref().unwrap().as_bytes())
    }

    fn fetch_flags(&self) -> Flag {
        Flag::default()
    }

    fn set_flag(&mut self, _envelope: &mut Envelope, _f: Flag, _value: bool) -> Result<()> {
        Ok(())
    }

    fn set_tag(&mut self, _envelope: &mut Envelope, _tag: String, value: bool) -> Result<()> {
        Ok(())
    }
}
