/*
 * meli - imap module.
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

use imap_codec::fetch::MessageDataItemName;

use super::*;
use crate::{backends::*, email::*, error::Error};

/// `BackendOp` implementor for Imap
#[derive(Debug, Clone)]
pub struct ImapOp {
    uid: UID,
    mailbox_hash: MailboxHash,
    connection: Arc<FutureMutex<ImapConnection>>,
    uid_store: Arc<UIDStore>,
}

impl ImapOp {
    pub fn new(
        uid: UID,
        mailbox_hash: MailboxHash,
        connection: Arc<FutureMutex<ImapConnection>>,
        uid_store: Arc<UIDStore>,
    ) -> Self {
        Self {
            uid,
            connection,
            mailbox_hash,
            uid_store,
        }
    }
}

impl BackendOp for ImapOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        let connection = self.connection.clone();
        let mailbox_hash = self.mailbox_hash;
        let uid = self.uid;
        let uid_store = self.uid_store.clone();
        Ok(Box::pin(async move {
            let exists_in_cache = {
                let mut bytes_cache = uid_store.byte_cache.lock()?;
                let cache = bytes_cache.entry(uid).or_default();
                cache.bytes.is_some()
            };
            if !exists_in_cache {
                let mut response = Vec::with_capacity(8 * 1024);
                {
                    let mut conn = timeout(uid_store.timeout, connection.lock()).await?;
                    conn.connect().await?;
                    conn.examine_mailbox(mailbox_hash, &mut response, false)
                        .await?;
                    conn.send_command(CommandBody::fetch(
                        uid,
                        vec![MessageDataItemName::Flags, MessageDataItemName::Rfc822],
                        true,
                    )?)
                    .await?;
                    conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                        .await?;
                }
                debug!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    String::from_utf8_lossy(&response).lines().count()
                );
                let mut results = protocol_parser::fetch_responses(&response)?.1;
                if results.len() != 1 {
                    return Err(
                        Error::new(format!("Invalid/unexpected response: {:?}", response))
                            .set_summary(format!("message with UID {} was not found?", uid)),
                    );
                }
                let FetchResponse {
                    uid: _uid,
                    flags: _flags,
                    body,
                    ..
                } = results.pop().unwrap();
                let _uid = _uid.unwrap();
                assert_eq!(_uid, uid);
                assert!(body.is_some());
                let mut bytes_cache = uid_store.byte_cache.lock()?;
                let cache = bytes_cache.entry(uid).or_default();
                if let Some((_flags, _)) = _flags {
                    //flags.lock().await.set(Some(_flags));
                    cache.flags = Some(_flags);
                }
                cache.bytes = Some(body.unwrap().to_vec());
            }
            let mut bytes_cache = uid_store.byte_cache.lock()?;
            let cache = bytes_cache.entry(uid).or_default();
            let ret = cache.bytes.clone().unwrap();
            Ok(ret)
        }))
    }

    fn fetch_flags(&self) -> ResultFuture<Flag> {
        let mut response = Vec::with_capacity(8 * 1024);
        let connection = self.connection.clone();
        let mailbox_hash = self.mailbox_hash;
        let uid = self.uid;
        let uid_store = self.uid_store.clone();

        Ok(Box::pin(async move {
            let exists_in_cache = {
                let mut bytes_cache = uid_store.byte_cache.lock()?;
                let cache = bytes_cache.entry(uid).or_default();
                cache.flags.is_some()
            };
            if !exists_in_cache {
                let mut conn = connection.lock().await;
                conn.connect().await?;
                conn.examine_mailbox(mailbox_hash, &mut response, false)
                    .await?;
                conn.send_command(CommandBody::fetch(
                    uid,
                    vec![MessageDataItemName::Flags],
                    true,
                )?)
                .await?;
                conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                    .await?;
                debug!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    String::from_utf8_lossy(&response).lines().count()
                );
                let v = protocol_parser::uid_fetch_flags_responses(&response)
                    .map(|(_, v)| v)
                    .map_err(Error::from)?;
                if v.len() != 1 {
                    debug!("responses len is {}", v.len());
                    debug!(String::from_utf8_lossy(&response));
                    /* [ref:TODO]: Trigger cache invalidation here. */
                    debug!("message with UID {} was not found", uid);
                    return Err(
                        Error::new(format!("Invalid/unexpected response: {:?}", response))
                            .set_summary(format!("message with UID {} was not found?", uid)),
                    );
                }
                let (_uid, (_flags, _)) = v[0];
                assert_eq!(_uid, uid);
                let mut bytes_cache = uid_store.byte_cache.lock()?;
                let cache = bytes_cache.entry(uid).or_default();
                cache.flags = Some(_flags);
            }
            {
                let val = {
                    let mut bytes_cache = uid_store.byte_cache.lock()?;
                    let cache = bytes_cache.entry(uid).or_default();
                    cache.flags
                };
                Ok(val.unwrap())
            }
        }))
    }
}
