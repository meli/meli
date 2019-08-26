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

use crate::email::parser::BytesExt;
use crate::error::*;
use std::io::Read;
use std::io::Write;

#[derive(Debug)]
pub struct ImapConnection {
    pub cmd_id: usize,
    pub stream: native_tls::TlsStream<std::net::TcpStream>,
}

impl Drop for ImapConnection {
    fn drop(&mut self) {
        self.send_command(b"LOGOUT").ok().take();
    }
}

impl ImapConnection {
    pub fn read_response(&mut self, ret: &mut String) -> Result<()> {
        let id = format!("M{} ", self.cmd_id - 1);
        self.read_lines(ret, id)
    }

    pub fn read_lines(&mut self, ret: &mut String, termination_string: String) -> Result<()> {
        let mut buf: [u8; 1024] = [0; 1024];
        ret.clear();
        let mut last_line_idx: usize = 0;
        loop {
            match self.stream.read(&mut buf) {
                Ok(0) => break,
                Ok(b) => {
                    ret.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..b]) });
                    if let Some(mut pos) = ret[last_line_idx..].rfind("\r\n") {
                        if ret[last_line_idx..].starts_with("* BYE") {
                            return Err(MeliError::new("Disconnected"));
                        }
                        if let Some(prev_line) =
                            ret[last_line_idx..pos + last_line_idx].rfind("\r\n")
                        {
                            last_line_idx += prev_line + 2;
                            pos -= prev_line + 2;
                        }
                        if pos + "\r\n".len() == ret[last_line_idx..].len() {
                            if !termination_string.is_empty()
                                && ret[last_line_idx..].starts_with(termination_string.as_str())
                            {
                                debug!(&ret[last_line_idx..]);
                                ret.replace_range(last_line_idx.., "");
                                break;
                            } else if termination_string.is_empty() {
                                break;
                            }
                        }
                        last_line_idx += pos + 2;
                    }
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    continue;
                }
                Err(e) => return Err(MeliError::from(e)),
            }
        }
        Ok(())
    }

    pub fn wait_for_continuation_request(&mut self) -> Result<()> {
        let term = "+ ".to_string();
        let mut ret = String::new();
        self.read_lines(&mut ret, term)
    }

    pub fn send_command(&mut self, command: &[u8]) -> Result<usize> {
        let command = command.trim();
        self.stream.write_all(b"M")?;
        self.stream.write_all(self.cmd_id.to_string().as_bytes())?;
        self.stream.write_all(b" ")?;
        let ret = self.cmd_id;
        self.cmd_id += 1;
        self.stream.write_all(command)?;
        self.stream.write_all(b"\r\n")?;
        debug!("sent: M{} {}", self.cmd_id - 1, unsafe {
            std::str::from_utf8_unchecked(command)
        });
        Ok(ret)
    }

    pub fn send_literal(&mut self, data: &[u8]) -> Result<()> {
        self.stream.write_all(data)?;
        if !data.ends_with(b"\r\n") {
            self.stream.write_all(b"\r\n")?;
        }
        self.stream.write_all(b"\r\n")?;
        Ok(())
    }

    pub fn send_raw(&mut self, raw: &[u8]) -> Result<()> {
        self.stream.write_all(raw)?;
        self.stream.write_all(b"\r\n")?;
        Ok(())
    }

    pub fn set_nonblocking(&mut self, val: bool) -> Result<()> {
        self.stream.get_mut().set_nonblocking(val)?;
        Ok(())
    }
}

pub struct ImapBlockingConnection {
    buf: [u8; 1024],
    result: Vec<u8>,
    prev_res_length: usize,
    pub conn: ImapConnection,
}

impl From<ImapConnection> for ImapBlockingConnection {
    fn from(mut conn: ImapConnection) -> Self {
        conn.stream
            .get_mut()
            .set_nonblocking(false)
            .expect("set_nonblocking call failed");
        ImapBlockingConnection {
            buf: [0; 1024],
            conn,
            prev_res_length: 0,
            result: Vec::with_capacity(8 * 1024),
        }
    }
}

impl ImapBlockingConnection {
    pub fn into_conn(self) -> ImapConnection {
        self.conn
    }
}

impl Iterator for ImapBlockingConnection {
    type Item = Vec<u8>;
    fn next(&mut self) -> Option<Self::Item> {
        self.result.drain(0..self.prev_res_length);
        self.prev_res_length = 0;
        loop {
            match self.conn.stream.read(&mut self.buf) {
                Ok(0) => continue,
                Ok(b) => {
                    self.result.extend_from_slice(&self.buf[0..b]);
                    debug!(unsafe { std::str::from_utf8_unchecked(&self.result) });
                    if let Some(pos) = self.result.find(b"\r\n") {
                        self.prev_res_length = pos + b"\r\n".len();
                        return Some(self.result[0..self.prev_res_length].to_vec());
                    }
                }
                Err(e) => {
                    debug!(e);
                    return None;
                }
            }
        }
    }
}
