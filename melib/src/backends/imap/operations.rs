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
    folder_path: String,
    flags: Cell<Option<Flag>>,
    connection: Arc<Mutex<ImapConnection>>,
    byte_cache: Arc<Mutex<FnvHashMap<UID, EnvelopeCache>>>,
}

impl ImapOp {
    pub fn new(
        uid: usize,
        folder_path: String,
        connection: Arc<Mutex<ImapConnection>>,
        byte_cache: Arc<Mutex<FnvHashMap<UID, EnvelopeCache>>>,
    ) -> Self {
        ImapOp {
            uid,
            connection,
            bytes: None,
            headers: None,
            body: None,
            folder_path,
            flags: Cell::new(None),
            byte_cache,
        }
    }
}

impl BackendOp for ImapOp {
    fn description(&self) -> String {
        unimplemented!();
    }

    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.bytes.is_none() {
            let mut bytes_cache = self.byte_cache.lock()?;
            let cache = bytes_cache.entry(self.uid).or_default();
            if cache.bytes.is_some() {
                self.bytes = cache.bytes.clone();
            } else {
                let mut response = String::with_capacity(8 * 1024);
                {
                    let mut conn = self.connection.lock().unwrap();
                    conn.send_command(format!("SELECT {}", self.folder_path).as_bytes())?;
                    conn.read_response(&mut response)?;
                    conn.send_command(format!("UID FETCH {} (FLAGS RFC822)", self.uid).as_bytes())?;
                    conn.read_response(&mut response)?;
                }
                debug!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    response.lines().collect::<Vec<&str>>().len()
                );
                match protocol_parser::uid_fetch_response(response.as_bytes())
                    .to_full_result()
                    .map_err(MeliError::from)
                {
                    Ok(v) => {
                        if v.len() != 1 {
                            debug!("responses len is {}", v.len());
                            /* TODO: Trigger cache invalidation here. */
                            return Err(MeliError::new(format!(
                                "message with UID {} was not found",
                                self.uid
                            )));
                        }
                        let (uid, flags, b) = v[0];
                        assert_eq!(uid, self.uid);
                        if flags.is_some() {
                            self.flags.set(flags);
                            cache.flags = flags;
                        }
                        cache.bytes = Some(unsafe { std::str::from_utf8_unchecked(b).to_string() });
                    }
                    Err(e) => return Err(e),
                }

                self.bytes = cache.bytes.clone();
            }
        }
        Ok(self.bytes.as_ref().unwrap().as_bytes())
    }

    fn fetch_headers(&mut self) -> Result<&[u8]> {
        if self.bytes.is_some() {
            let result =
                parser::headers_raw(self.bytes.as_ref().unwrap().as_bytes()).to_full_result()?;
            return Ok(result);
        }
        if self.headers.is_none() {
            let mut bytes_cache = self.byte_cache.lock()?;
            let cache = bytes_cache.entry(self.uid).or_default();
            if cache.headers.is_some() {
                self.headers = cache.headers.clone();
            } else {
                let mut response = String::with_capacity(8 * 1024);
                let mut conn = self.connection.lock().unwrap();
                conn.send_command(
                    format!("UID FETCH {} (FLAGS RFC822.HEADER)", self.uid).as_bytes(),
                )?;
                conn.read_response(&mut response)?;
                debug!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    response.lines().collect::<Vec<&str>>().len()
                );
                match protocol_parser::uid_fetch_response(response.as_bytes())
                    .to_full_result()
                    .map_err(MeliError::from)
                {
                    Ok(v) => {
                        if v.len() != 1 {
                            debug!("responses len is {}", v.len());
                            /* TODO: Trigger cache invalidation here. */
                            return Err(MeliError::new(format!(
                                "message with UID {} was not found",
                                self.uid
                            )));
                        }
                        let (uid, flags, b) = v[0];
                        assert_eq!(uid, self.uid);
                        if flags.is_some() {
                            self.flags.set(flags);
                            cache.flags = flags;
                        }
                        cache.headers =
                            Some(unsafe { std::str::from_utf8_unchecked(b).to_string() });
                    }
                    Err(e) => return Err(e),
                }
                self.headers = cache.headers.clone();
            }
        }
        Ok(self.headers.as_ref().unwrap().as_bytes())
    }

    fn fetch_body(&mut self) -> Result<&[u8]> {
        if self.bytes.is_some() {
            let result =
                parser::body_raw(self.bytes.as_ref().unwrap().as_bytes()).to_full_result()?;
            return Ok(result);
        }
        if self.body.is_none() {
            let mut bytes_cache = self.byte_cache.lock()?;
            let cache = bytes_cache.entry(self.uid).or_default();
            if cache.body.is_some() {
                self.body = cache.body.clone();
            } else {
                let mut response = String::with_capacity(8 * 1024);
                let mut conn = self.connection.lock().unwrap();
                conn.send_command(
                    format!("UID FETCH {} (FLAGS RFC822.TEXT)", self.uid).as_bytes(),
                )?;
                conn.read_response(&mut response)?;
                debug!(
                    "fetch response is {} bytes and {} lines",
                    response.len(),
                    response.lines().collect::<Vec<&str>>().len()
                );
                match protocol_parser::uid_fetch_response(response.as_bytes())
                    .to_full_result()
                    .map_err(MeliError::from)
                {
                    Ok(v) => {
                        if v.len() != 1 {
                            debug!("responses len is {}", v.len());
                            /* TODO: Trigger cache invalidation here. */
                            return Err(MeliError::new(format!(
                                "message with UID {} was not found",
                                self.uid
                            )));
                        }
                        let (uid, flags, b) = v[0];
                        assert_eq!(uid, self.uid);
                        if flags.is_some() {
                            self.flags.set(flags);
                        }
                        cache.body = Some(unsafe { std::str::from_utf8_unchecked(b).to_string() });
                    }
                    Err(e) => return Err(e),
                }
                self.body = cache.body.clone();
            }
        }
        Ok(self.body.as_ref().unwrap().as_bytes())
    }

    fn fetch_flags(&self) -> Flag {
        if self.flags.get().is_some() {
            return self.flags.get().unwrap();
        }
        let mut bytes_cache = self.byte_cache.lock().unwrap();
        let cache = bytes_cache.entry(self.uid).or_default();
        if cache.flags.is_some() {
            self.flags.set(cache.flags);
        } else {
            let mut response = String::with_capacity(8 * 1024);
            let mut conn = self.connection.lock().unwrap();
            conn.send_command(format!("UID FETCH {} FLAGS", self.uid).as_bytes())
                .unwrap();
            conn.read_response(&mut response).unwrap();
            debug!(
                "fetch response is {} bytes and {} lines",
                response.len(),
                response.lines().collect::<Vec<&str>>().len()
            );
            match protocol_parser::uid_fetch_response(response.as_bytes())
                .to_full_result()
                .map_err(MeliError::from)
            {
                Ok(v) => {
                    if v.len() != 1 {
                        debug!("responses len is {}", v.len());
                        /* TODO: Trigger cache invalidation here. */
                        panic!(format!("message with UID {} was not found", self.uid));
                    }
                    let (uid, flags, _) = v[0];
                    assert_eq!(uid, self.uid);
                    if flags.is_some() {
                        cache.flags = flags;
                        self.flags.set(flags);
                    }
                }
                Err(e) => Err(e).unwrap(),
            }
        }
        self.flags.get().unwrap()
    }

    fn set_flag(&mut self, _envelope: &mut Envelope, flag: Flag) -> Result<()> {
        let mut response = String::with_capacity(8 * 1024);
        let mut conn = self.connection.lock().unwrap();
        conn.send_command(format!("SELECT \"{}\"", &self.folder_path,).as_bytes())?;
        conn.read_response(&mut response)?;
        debug!(&response);
        conn.send_command(
            format!(
                "UID STORE {} FLAGS.SILENT ({})",
                self.uid,
                flags_to_imap_list!(flag)
            )
            .as_bytes(),
        )?;
        conn.read_response(&mut response)?;
        debug!(&response);
        match protocol_parser::uid_fetch_response(response.as_bytes())
            .to_full_result()
            .map_err(MeliError::from)
        {
            Ok(v) => {
                if v.len() == 1 {
                    debug!("responses len is {}", v.len());
                    let (uid, flags, _) = v[0];
                    assert_eq!(uid, self.uid);
                    if flags.is_some() {
                        self.flags.set(flags);
                    }
                }
            }
            Err(e) => Err(e).unwrap(),
        }
        conn.send_command(format!("EXAMINE \"{}\"", &self.folder_path,).as_bytes())?;
        conn.read_response(&mut response)?;
        let mut bytes_cache = self.byte_cache.lock()?;
        let cache = bytes_cache.entry(self.uid).or_default();
        cache.flags = Some(flag);
        Ok(())
    }
}
