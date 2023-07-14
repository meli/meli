/*
 * ____
 *
 * Copyright ____  Manos Pitsidianakis
 *
 * This file is part of ____.
 *
 * ____ is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ____ is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ____. If not, see <http://www.gnu.org/licenses/>.
 */

use super::*;

#[derive(Debug)]
pub struct ImapCursor {
    pub connection: Arc<FutureMutex<ImapConnection>>,
    pub mailbox_hash: MailboxHash,
    pub uid_store: Arc<UIDStore>,
}

impl crate::cursor::Cursor for ImapCursor {
    fn fetch(&self, start: usize, count: usize) -> Result<EnvelopeStream> {
        let connection = self.connection.clone();
        let mailbox_hash = self.mailbox_hash;
        let uid_store = self.uid_store.clone();
        Ok(Box::pin(async_stream::try_stream! {
            let mut response = Vec::with_capacity(8 * 1024);
            {
                let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                if f.no_select {
                    yield vec![];
                    return;
                }
            };
            loop {
                {
                    let mut conn = connection.lock().await;
                    conn.examine_mailbox(mailbox_hash, &mut response, false)
                        .await?;
                    conn.send_command_raw(b"UID SORT (REVERSE ARRIVAL) UTF-8 ALL").await?;
                    conn.read_response(&mut response, RequiredResponses::empty())
                        .await?;
                }

                yield vec![];
                return;
            }
        }))
    }

    fn total(&self) -> ResultFuture<Option<usize>> {
        Err(Error::new("Cursor not supported in this backend.").set_kind(ErrorKind::NotSupported))
    }

    fn unseen(&self) -> ResultFuture<Option<usize>> {
        Err(Error::new("Cursor not supported in this backend.").set_kind(ErrorKind::NotSupported))
    }

    fn has_updates(&self) -> ResultFuture<bool> {
        Err(Error::new("Cursor not supported in this backend.").set_kind(ErrorKind::NotSupported))
    }

    fn reset(&self) -> ResultFuture<()> {
        Err(Error::new("Cursor not supported in this backend.").set_kind(ErrorKind::NotSupported))
    }

    fn query(&self) -> Result<Arc<Query>> {
        Err(Error::new("Cursor not supported in this backend.").set_kind(ErrorKind::NotSupported))
    }
}
