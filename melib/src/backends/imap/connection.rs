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

use super::protocol_parser::{ImapLineSplit, ImapResponse};
use crate::email::parser::BytesExt;
use crate::error::*;
use std::io::Read;
use std::io::Write;
extern crate native_tls;
use fnv::FnvHashSet;
use native_tls::TlsConnector;
use std::iter::FromIterator;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use super::protocol_parser;
use super::{Capabilities, ImapServerConf};

#[derive(Debug)]
pub struct ImapStream {
    cmd_id: usize,
    stream: native_tls::TlsStream<std::net::TcpStream>,
}

#[derive(Debug)]
pub struct ImapConnection {
    pub stream: Result<ImapStream>,
    pub server_conf: ImapServerConf,
    pub capabilities: Capabilities,
    pub online: Arc<Mutex<(Instant, Result<()>)>>,
}

impl Drop for ImapStream {
    fn drop(&mut self) {
        self.send_command(b"LOGOUT").ok().take();
    }
}

impl ImapStream {
    pub fn new_connection(server_conf: &ImapServerConf) -> Result<(Capabilities, ImapStream)> {
        use std::io::prelude::*;
        use std::net::TcpStream;
        let path = &server_conf.server_hostname;

        let mut connector = TlsConnector::builder();
        if server_conf.danger_accept_invalid_certs {
            connector.danger_accept_invalid_certs(true);
        }
        let connector = connector.build()?;

        let addr = if let Ok(a) = lookup_ipv4(path, server_conf.server_port) {
            a
        } else {
            return Err(MeliError::new(format!(
                "Could not lookup address {}",
                &path
            )));
        };

        let mut socket = TcpStream::connect(&addr)?;
        socket.set_read_timeout(Some(std::time::Duration::new(4, 0)))?;
        socket.set_write_timeout(Some(std::time::Duration::new(4, 0)))?;
        let cmd_id = 0;
        if server_conf.use_starttls {
            socket.write_all(format!("M{} STARTTLS\r\n", cmd_id).as_bytes())?;

            let mut buf = vec![0; 1024];
            let mut response = String::with_capacity(1024);
            let mut broken = false;
            let now = std::time::Instant::now();

            while now.elapsed().as_secs() < 3 {
                let len = socket.read(&mut buf)?;
                response.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..len]) });
                if response.starts_with("* OK ") && response.find("\r\n").is_some() {
                    if let Some(pos) = response.as_bytes().find(b"\r\n") {
                        response.drain(0..pos + 2);
                    }
                }
                if response.starts_with("M0 OK") {
                    broken = true;
                    break;
                }
            }
            if !broken {
                return Err(MeliError::new(format!(
                    "Could not initiate TLS negotiation to {}.",
                    path
                )));
            }
        }

        socket
            .set_nonblocking(true)
            .expect("set_nonblocking call failed");
        let stream = {
            let mut conn_result = connector.connect(path, socket);
            if let Err(native_tls::HandshakeError::WouldBlock(midhandshake_stream)) = conn_result {
                let mut midhandshake_stream = Some(midhandshake_stream);
                loop {
                    match midhandshake_stream.take().unwrap().handshake() {
                        Ok(r) => {
                            conn_result = Ok(r);
                            break;
                        }
                        Err(native_tls::HandshakeError::WouldBlock(stream)) => {
                            midhandshake_stream = Some(stream);
                        }
                        p => {
                            p?;
                        }
                    }
                }
            }
            conn_result?
        };
        let mut res = String::with_capacity(8 * 1024);
        let mut ret = ImapStream { cmd_id, stream };
        ret.send_command(b"CAPABILITY")?;
        ret.read_response(&mut res)?;
        let capabilities: std::result::Result<Vec<&[u8]>, _> = res
            .split_rn()
            .find(|l| l.starts_with("* CAPABILITY"))
            .ok_or_else(|| MeliError::new(""))
            .and_then(|res| {
                protocol_parser::capabilities(res.as_bytes())
                    .to_full_result()
                    .map_err(|_| MeliError::new(""))
            });

        if capabilities.is_err() {
            return Err(MeliError::new(format!(
                "Could not connect to {}: expected CAPABILITY response but got:{}",
                &server_conf.server_hostname, res
            )));
        }

        let capabilities = capabilities.unwrap();
        if !capabilities
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case(b"IMAP4rev1"))
        {
            return Err(MeliError::new(format!(
                "Could not connect to {}: server is not IMAP4rev1 compliant",
                &server_conf.server_hostname
            )));
        } else if capabilities
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case(b"LOGINDISABLED"))
        {
            return Err(MeliError::new(format!(
                "Could not connect to {}: server does not accept logins [LOGINDISABLED]",
                &server_conf.server_hostname
            )));
        }

        let mut capabilities = None;
        ret.send_command(
            format!(
                "LOGIN \"{}\" \"{}\"",
                &server_conf.server_username, &server_conf.server_password
            )
            .as_bytes(),
        )?;
        let tag_start = format!("M{} ", (ret.cmd_id - 1));

        loop {
            ret.read_lines(&mut res, &String::new(), false)?;
            let mut should_break = false;
            for l in res.split_rn() {
                if l.starts_with("* CAPABILITY") {
                    capabilities = protocol_parser::capabilities(l.as_bytes())
                        .to_full_result()
                        .map(|capabilities| {
                            FnvHashSet::from_iter(
                                capabilities.into_iter().map(|s: &[u8]| s.to_vec()),
                            )
                        })
                        .ok();
                }

                if l.starts_with(tag_start.as_str()) {
                    if !l[tag_start.len()..].trim().starts_with("OK ") {
                        return Err(MeliError::new(format!(
                            "Could not connect. Server replied with '{}'",
                            l[tag_start.len()..].trim()
                        )));
                    }
                    should_break = true;
                }
            }
            if should_break {
                break;
            }
        }

        if capabilities.is_none() {
            /* sending CAPABILITY after LOGIN automatically is an RFC recommendation, so check
             * for lazy servers */
            drop(capabilities);
            ret.send_command(b"CAPABILITY")?;
            ret.read_response(&mut res).unwrap();
            let capabilities = protocol_parser::capabilities(res.as_bytes()).to_full_result()?;
            let capabilities = FnvHashSet::from_iter(capabilities.into_iter().map(|s| s.to_vec()));
            Ok((capabilities, ret))
        } else {
            let capabilities = capabilities.unwrap();
            Ok((capabilities, ret))
        }
    }

    pub fn read_response(&mut self, ret: &mut String) -> Result<()> {
        let id = format!("M{} ", self.cmd_id - 1);
        self.read_lines(ret, &id, true)
    }

    pub fn read_lines(
        &mut self,
        ret: &mut String,
        termination_string: &str,
        keep_termination_string: bool,
    ) -> Result<()> {
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
                                && ret[last_line_idx..].starts_with(termination_string)
                            {
                                debug!(&ret[last_line_idx..]);
                                if !keep_termination_string {
                                    ret.replace_range(last_line_idx.., "");
                                }
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
                Err(e) => {
                    return Err(MeliError::from(e));
                }
            }
        }
        Ok(())
    }

    pub fn wait_for_continuation_request(&mut self) -> Result<()> {
        let term = "+ ".to_string();
        let mut ret = String::new();
        self.read_lines(&mut ret, &term, false)
    }

    pub fn send_command(&mut self, command: &[u8]) -> Result<()> {
        let command = command.trim();
        self.stream.write_all(b"M")?;
        self.stream.write_all(self.cmd_id.to_string().as_bytes())?;
        self.stream.write_all(b" ")?;
        self.cmd_id += 1;
        self.stream.write_all(command)?;
        self.stream.write_all(b"\r\n")?;
        debug!("sent: M{} {}", self.cmd_id - 1, unsafe {
            std::str::from_utf8_unchecked(command)
        });
        Ok(())
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

impl ImapConnection {
    pub fn new_connection(
        server_conf: &ImapServerConf,
        online: Arc<Mutex<(Instant, Result<()>)>>,
    ) -> ImapConnection {
        ImapConnection {
            stream: Err(MeliError::new("Offline".to_string())),
            server_conf: server_conf.clone(),
            capabilities: Capabilities::default(),
            online,
        }
    }

    pub fn read_response(&mut self, ret: &mut String) -> Result<()> {
        self.try_send(|s| s.read_response(ret))?;
        let r: ImapResponse = ImapResponse::from(&ret);
        if let ImapResponse::Bye(ref response_code) = r {
            self.stream = Err(MeliError::new(format!(
                "Offline: received BYE: {:?}",
                response_code
            )));
        }
        r.into()
    }

    pub fn read_lines(&mut self, ret: &mut String, termination_string: String) -> Result<()> {
        self.try_send(|s| s.read_lines(ret, &termination_string, false))
    }

    pub fn wait_for_continuation_request(&mut self) -> Result<()> {
        self.try_send(|s| s.wait_for_continuation_request())
    }

    pub fn send_command(&mut self, command: &[u8]) -> Result<()> {
        self.try_send(|s| s.send_command(command))
    }

    pub fn send_literal(&mut self, data: &[u8]) -> Result<()> {
        self.try_send(|s| s.send_literal(data))
    }

    pub fn send_raw(&mut self, raw: &[u8]) -> Result<()> {
        self.try_send(|s| s.send_raw(raw))
    }

    pub fn set_nonblocking(&mut self, val: bool) -> Result<()> {
        self.try_send(|s| s.set_nonblocking(val))
    }

    pub fn try_send(
        &mut self,
        mut action: impl FnMut(&mut ImapStream) -> Result<()>,
    ) -> Result<()> {
        if let (instant, ref mut status @ Ok(())) = *self.online.lock().unwrap() {
            if Instant::now().duration_since(instant) >= std::time::Duration::new(60 * 30, 0) {
                *status = Err(MeliError::new("Connection timed out"));
                self.stream = Err(MeliError::new("Connection timed out"));
            }
        }
        if let Ok(ref mut stream) = self.stream {
            if let Ok(_) = action(stream) {
                return Ok(());
            }
        }
        let new_stream = ImapStream::new_connection(&self.server_conf);
        if new_stream.is_err() {
            *self.online.lock().unwrap() = (
                Instant::now(),
                Err(new_stream.as_ref().unwrap_err().clone()),
            );
        } else {
            *self.online.lock().unwrap() = (Instant::now(), Ok(()));
        }
        let (capabilities, stream) = new_stream?;
        self.stream = Ok(stream);
        self.capabilities = capabilities;
        Err(MeliError::new("Connection timed out"))
    }
}

pub struct ImapBlockingConnection {
    buf: [u8; 1024],
    result: Vec<u8>,
    prev_res_length: usize,
    pub conn: ImapConnection,
    err: Option<String>,
}

impl From<ImapConnection> for ImapBlockingConnection {
    fn from(mut conn: ImapConnection) -> Self {
        conn.set_nonblocking(false)
            .expect("set_nonblocking call failed");
        conn.stream
            .as_mut()
            .map(|s| {
                s.stream
                    .get_mut()
                    .set_write_timeout(Some(std::time::Duration::new(30, 0)))
                    .expect("set_write_timeout call failed")
            })
            .expect("set_write_timeout call failed");
        conn.stream
            .as_mut()
            .map(|s| {
                s.stream
                    .get_mut()
                    .set_read_timeout(Some(std::time::Duration::new(30, 0)))
                    .expect("set_read_timeout call failed")
            })
            .expect("set_read_timeout call failed");
        ImapBlockingConnection {
            buf: [0; 1024],
            conn,
            prev_res_length: 0,
            result: Vec::with_capacity(8 * 1024),
            err: None,
        }
    }
}

impl ImapBlockingConnection {
    pub fn into_conn(self) -> ImapConnection {
        self.conn
    }

    pub fn err(&self) -> Option<&str> {
        self.err.as_ref().map(String::as_str)
    }
}

impl Iterator for ImapBlockingConnection {
    type Item = Vec<u8>;
    fn next(&mut self) -> Option<Self::Item> {
        self.result.drain(0..self.prev_res_length);
        self.prev_res_length = 0;
        let ImapBlockingConnection {
            ref mut prev_res_length,
            ref mut result,
            ref mut conn,
            ref mut buf,
            ref mut err,
        } = self;
        loop {
            if conn.stream.is_err() {
                debug!(&conn.stream);
                return None;
            }
            match conn.stream.as_mut().unwrap().stream.read(buf) {
                Ok(0) => return None,
                Ok(b) => {
                    result.extend_from_slice(&buf[0..b]);
                    debug!(unsafe { std::str::from_utf8_unchecked(result) });
                    if let Some(pos) = result.find(b"\r\n") {
                        *prev_res_length = pos + b"\r\n".len();
                        return Some(result[0..*prev_res_length].to_vec());
                    }
                }
                Err(e) => {
                    debug!(&conn.stream);
                    debug!(&e);
                    *err = Some(e.to_string());
                    return None;
                }
            }
        }
    }
}

fn lookup_ipv4(host: &str, port: u16) -> Result<SocketAddr> {
    use std::net::ToSocketAddrs;

    let addrs = (host, port).to_socket_addrs()?;
    for addr in addrs {
        if let SocketAddr::V4(_) = addr {
            return Ok(addr);
        }
    }

    Err(MeliError::new("Cannot lookup address"))
}
