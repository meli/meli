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

use futures::lock::Mutex as FutureMutex;
use isahc::AsyncReadResponseExt;

use crate::{
    backends::prelude::{BackendOp, ResultFuture},
    jmap::{connection::JmapConnection, methods::download_request_format, Store},
    EnvelopeHash,
};

/// `BackendOp` implementor for JMAP
#[derive(Clone, Debug)]
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
    fn as_bytes(&self) -> ResultFuture<Vec<u8>> {
        let store = self.store.clone();
        let hash = self.hash;
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            {
                let byte_lck = store.byte_cache.lock().await;
                if let Some(Some(ret)) = byte_lck.get(&hash).map(|c| c.bytes.clone()) {
                    return Ok(ret.into_bytes());
                }
            }
            let blob_id = store.blob_id_store.lock().await[&hash].clone();
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let (download_url, mail_account_id) = {
                let g = store.online_status.session_guard().await?;
                (g.download_url.clone(), g.mail_account_id())
            };
            let res_text = conn
                .get_async(&download_request_format(
                    &download_url,
                    &mail_account_id,
                    &blob_id,
                    None,
                )?)
                .await?
                .text()
                .await?;

            store.byte_cache.lock().await.entry(hash).or_default().bytes = Some(res_text.clone());
            Ok(res_text.into_bytes())
        }))
    }
}
