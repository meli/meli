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

use super::*;

use crate::backends::BackendOp;
use crate::email::*;
use crate::error::{MeliError, Result};
use std::cell::Cell;
use std::sync::{Arc, Mutex};

/// `BackendOp` implementor for Imap
#[derive(Debug, Clone)]
pub struct ImapOp {
    uid: usize,
    bytes: Option<String>,
    headers: Option<String>,
    body: Option<String>,
    mailbox_path: String,
    mailbox_hash: MailboxHash,
    flags: Cell<Option<Flag>>,
    connection: Arc<Mutex<ImapConnection>>,
    uid_store: Arc<UIDStore>,
}

impl ImapOp {
    pub fn new(
        uid: usize,
        mailbox_path: String,
        mailbox_hash: MailboxHash,
        connection: Arc<Mutex<ImapConnection>>,
        uid_store: Arc<UIDStore>,
    ) -> Self {
        ImapOp {
            uid,
            connection,
            bytes: None,
            headers: None,
            body: None,
            mailbox_path,
            mailbox_hash,
            flags: Cell::new(None),
            uid_store,
        }
    }
}

impl BackendOp for ImapOp {
    fn description(&self) -> String {
        format!("Message in mailbox: {}", &self.mailbox_path)
    }

    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.bytes.is_none() {
            let mut bytes_cache = self.uid_store.byte_cache.lock()?;
            let cache = bytes_cache.entry(self.uid).or_default();
            if cache.bytes.is_some() {
                self.bytes = cache.bytes.clone();
            } else {
                drop(cache);
                drop(bytes_cache);
                let mut response = String::with_capacity(8 * 1024);
                {
                    let mut conn =
                        try_lock(&self.connection, Some(std::time::Duration::new(2, 0)))?;
                    conn.examine_mailbox(self.mailbox_hash, &mut response, false)?;
                    conn.send_command(format!("UID FETCH {} (FLAGS RFC822)", self.uid).as_bytes())?;
                    conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)?;
                }
                debug!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    response.lines().collect::<Vec<&str>>().len()
                );
                let UidFetchResponse {
                    uid, flags, body, ..
                } = protocol_parser::uid_fetch_response(&response)?.1;
                assert_eq!(uid, self.uid);
                assert!(body.is_some());
                let mut bytes_cache = self.uid_store.byte_cache.lock()?;
                let cache = bytes_cache.entry(self.uid).or_default();
                if let Some((flags, _)) = flags {
                    self.flags.set(Some(flags));
                    cache.flags = Some(flags);
                }
                cache.bytes =
                    Some(unsafe { std::str::from_utf8_unchecked(body.unwrap()).to_string() });
                self.bytes = cache.bytes.clone();
            }
        }
        Ok(self.bytes.as_ref().unwrap().as_bytes())
    }

    fn fetch_flags(&self) -> Flag {
        macro_rules! or_return_default {
            ($expr:expr) => {
                match $expr {
                    Ok(ok) => ok,
                    Err(_) => return Default::default(),
                }
            };
        }
        if self.flags.get().is_some() {
            return self.flags.get().unwrap();
        }
        let mut bytes_cache = or_return_default!(self.uid_store.byte_cache.lock());
        let cache = bytes_cache.entry(self.uid).or_default();
        if cache.flags.is_some() {
            self.flags.set(cache.flags);
        } else {
            let mut response = String::with_capacity(8 * 1024);
            let mut conn = or_return_default!(try_lock(
                &self.connection,
                Some(std::time::Duration::new(2, 0))
            ));
            or_return_default!(conn.examine_mailbox(self.mailbox_hash, &mut response, false));
            or_return_default!(
                conn.send_command(format!("UID FETCH {} FLAGS", self.uid).as_bytes())
            );
            or_return_default!(conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED));
            debug!(
                "fetch response is {} bytes and {} lines",
                response.len(),
                response.lines().collect::<Vec<&str>>().len()
            );
            match protocol_parser::uid_fetch_flags_response(response.as_bytes())
                .map(|(_, v)| v)
                .map_err(MeliError::from)
            {
                Ok(v) => {
                    if v.len() != 1 {
                        debug!("responses len is {}", v.len());
                        debug!(response);
                        /* TODO: Trigger cache invalidation here. */
                        debug!(format!("message with UID {} was not found", self.uid));
                        return Flag::default();
                    }
                    let (uid, (flags, _)) = v[0];
                    assert_eq!(uid, self.uid);
                    cache.flags = Some(flags);
                    self.flags.set(Some(flags));
                }
                Err(e) => or_return_default!(Err(e)),
            }
        }
        self.flags.get().unwrap()
    }

    fn set_flag(&mut self, _envelope: &mut Envelope, f: Flag, value: bool) -> Result<()> {
        let mut flags = self.fetch_flags();
        flags.set(f, value);

        let mut response = String::with_capacity(8 * 1024);
        let mut conn = try_lock(&self.connection, Some(std::time::Duration::new(2, 0)))?;
        conn.select_mailbox(self.mailbox_hash, &mut response, false)?;
        debug!(&response);
        conn.send_command(
            format!(
                "UID STORE {} FLAGS.SILENT ({})",
                self.uid,
                flags_to_imap_list!(flags)
            )
            .as_bytes(),
        )?;
        conn.read_response(&mut response, RequiredResponses::STORE_REQUIRED)?;
        debug!(&response);
        match protocol_parser::uid_fetch_flags_response(response.as_bytes())
            .map(|(_, v)| v)
            .map_err(MeliError::from)
        {
            Ok(v) => {
                if v.len() == 1 {
                    debug!("responses len is {}", v.len());
                    let (uid, (flags, _)) = v[0];
                    assert_eq!(uid, self.uid);
                    self.flags.set(Some(flags));
                }
            }
            Err(e) => Err(e)?,
        }
        let mut bytes_cache = self.uid_store.byte_cache.lock()?;
        let cache = bytes_cache.entry(self.uid).or_default();
        cache.flags = Some(flags);
        Ok(())
    }

    fn set_tag(&mut self, envelope: &mut Envelope, tag: String, value: bool) -> Result<()> {
        let mut response = String::with_capacity(8 * 1024);
        let mut conn = try_lock(&self.connection, Some(std::time::Duration::new(2, 0)))?;
        conn.select_mailbox(self.mailbox_hash, &mut response, false)?;
        conn.send_command(
            format!(
                "UID STORE {} {}FLAGS.SILENT ({})",
                self.uid,
                if value { "+" } else { "-" },
                &tag
            )
            .as_bytes(),
        )?;
        conn.read_response(&mut response, RequiredResponses::STORE_REQUIRED)?;
        protocol_parser::uid_fetch_flags_response(response.as_bytes())
            .map(|(_, v)| v)
            .map_err(MeliError::from)?;
        let hash = tag_hash!(tag);
        if value {
            self.uid_store.tag_index.write().unwrap().insert(hash, tag);
        } else {
            self.uid_store.tag_index.write().unwrap().remove(&hash);
        }
        if !envelope.labels().iter().any(|&h_| h_ == hash) {
            if value {
                envelope.labels_mut().push(hash);
            }
        }
        if !value {
            if let Some(pos) = envelope.labels().iter().position(|&h_| h_ == hash) {
                envelope.labels_mut().remove(pos);
            }
        }
        Ok(())
    }
}
