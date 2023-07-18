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

use std::sync::Arc;

use super::*;

/// `BackendOp` implementor for Imap
#[derive(Debug, Clone)]
pub struct JmapOp {
    hash: EnvelopeHash,
    connection: Arc<FutureMutex<JmapConnection>>,
    store: Arc<Store>,
}

impl JmapOp {
    pub fn new(
        hash: EnvelopeHash,
        connection: Arc<FutureMutex<JmapConnection>>,
        store: Arc<Store>,
    ) -> Self {
        Self {
            hash,
            connection,
            store,
        }
    }
}

impl BackendOp for JmapOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        {
            let byte_lck = self.store.byte_cache.lock().unwrap();
            if let Some(Some(ret)) = byte_lck.get(&self.hash).map(|c| c.bytes.clone()) {
                return Ok(Box::pin(async move { Ok(ret.into_bytes()) }));
            }
        }
        let store = self.store.clone();
        let hash = self.hash;
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let blob_id = store.blob_id_store.lock().unwrap()[&hash].clone();
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let download_url = conn.session.lock().unwrap().download_url.clone();
            let mut res = conn
                .client
                .get_async(&download_request_format(
                    download_url.as_str(),
                    &conn.mail_account_id(),
                    &blob_id,
                    None,
                ))
                .await?;

            let res_text = res.text().await?;

            store
                .byte_cache
                .lock()
                .unwrap()
                .entry(hash)
                .or_default()
                .bytes = Some(res_text.clone());
            Ok(res_text.into_bytes())
        }))
    }
}
