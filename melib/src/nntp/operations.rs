/*
 * meli - nntp module.
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
use crate::{backends::*, error::Error};

/// Fetch an article by `UID` as bytes.
#[derive(Clone, Debug)]
pub struct NntpOp {
    uid: usize,
    mailbox_hash: MailboxHash,
    connection: Arc<FutureMutex<NntpConnection>>,
    uid_store: Arc<UIDStore>,
}

impl NntpOp {
    pub fn new(
        uid: usize,
        mailbox_hash: MailboxHash,
        connection: Arc<FutureMutex<NntpConnection>>,
        uid_store: Arc<UIDStore>,
    ) -> Self {
        Self {
            uid,
            connection,
            mailbox_hash,
            uid_store,
        }
    }

    pub async fn as_bytes(&self) -> Result<Vec<u8>> {
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = self.connection.lock().await;
        conn.select_group(self.mailbox_hash, false, &mut res)
            .await?;
        conn.send_command(format!("ARTICLE {}", self.uid).as_bytes())
            .await?;
        conn.read_response(&mut res, true, &["220 "]).await?;
        if !res.starts_with("220 ") {
            let path = self.uid_store.mailboxes.lock().await[&self.mailbox_hash]
                .name()
                .to_string();
            return Err(Error::new(format!(
                "{} Could not select article {}: expected ARTICLE response but got: {}",
                &self.uid_store.account_name, path, res
            )));
        }
        let pos = res.find("\r\n").unwrap_or(0) + 2;

        Ok(res.as_bytes()[pos..].to_vec())
    }
}
