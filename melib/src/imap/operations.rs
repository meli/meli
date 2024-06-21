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

use imap_codec::imap_types::fetch::MessageDataItemName;

use super::*;
use crate::{backends::*, error::Error};

/// `BackendOp` implementor for Imap
#[derive(Clone, Debug)]
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
    fn as_bytes(&self) -> ResultFuture<Vec<u8>> {
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
                log::trace!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    String::from_utf8_lossy(&response).lines().count()
                );
                let mut results = protocol_parser::fetch_responses(&response)?.1;
                if results.len() > 1 {
                    return Err(
                        Error::new(format!("Invalid/unexpected response: {:?}", response))
                            .set_summary(format!("Message with UID {} was not found.", uid))
                            .set_kind(ErrorKind::ProtocolError),
                    );
                }
                let Some(fetch_response) = results.pop() else {
                    // https://datatracker.ietf.org/doc/html/rfc3501#section-6.4.8:
                    //
                    // A non-existent unique identifier is ignored without any error message
                    // generated. Thus, it is possible for a UID FETCH command to return an OK
                    // without any data or a UID COPY or UID STORE to return an OK without
                    // performing any operations.
                    return Err(Error::new("Not found")
                        .set_summary(format!("Message with UID {} was not found.", uid))
                        .set_kind(ErrorKind::NotFound));
                };
                let FetchResponse {
                    uid: Some(_uid),
                    flags: _flags,
                    body: Some(body),
                    ..
                } = fetch_response
                else {
                    return Err(Error::new("Invalid/unexpected response from server")
                        .set_summary(format!("Message with UID {} was not found.", uid))
                        .set_details(format!("Full response: {:?}", fetch_response))
                        .set_kind(ErrorKind::ProtocolError));
                };
                if _uid != uid {
                    return Err(Error::new("Invalid/unexpected response from server")
                        .set_summary(format!("Message with UID {} was not found.", uid))
                        .set_details(format!(
                            "Requested UID {} but UID FETCH response was for UID {}. Full \
                             response: {:?}",
                            uid,
                            _uid,
                            String::from_utf8_lossy(&response)
                        ))
                        .set_kind(ErrorKind::ProtocolError));
                }
                let mut bytes_cache = uid_store.byte_cache.lock()?;
                let cache = bytes_cache.entry(uid).or_default();
                if let Some((_flags, _)) = _flags {
                    cache.flags = Some(_flags);
                }
                cache.bytes = Some(body.to_vec());
                return Ok(body.to_vec());
            }
            let mut bytes_cache = uid_store.byte_cache.lock()?;
            let cache = bytes_cache.entry(uid).or_default();
            let ret = cache.bytes.clone().unwrap();
            Ok(ret)
        }))
    }
}
