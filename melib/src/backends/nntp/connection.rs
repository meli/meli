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

use crate::backends::MailboxHash;
use crate::connections::{lookup_ipv4, Connection};
use crate::email::parser::BytesExt;
use crate::error::*;
extern crate native_tls;
use futures::io::{AsyncReadExt, AsyncWriteExt};
use native_tls::TlsConnector;
pub use smol::Async as AsyncWrapper;
use std::collections::HashSet;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::time::Instant;

use super::{Capabilities, NntpServerConf, UIDStore};

#[derive(Debug, Clone, Copy)]
pub struct NntpExtensionUse {
    #[cfg(feature = "deflate_compression")]
    pub deflate: bool,
}

impl Default for NntpExtensionUse {
    fn default() -> Self {
        Self {
            #[cfg(feature = "deflate_compression")]
            deflate: true,
        }
    }
}

#[derive(Debug)]
pub struct NntpStream {
    pub stream: AsyncWrapper<Connection>,
    pub extension_use: NntpExtensionUse,
    pub current_mailbox: MailboxSelection,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MailboxSelection {
    None,
    Select(MailboxHash),
    Examine(MailboxHash),
}

impl MailboxSelection {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, MailboxSelection::None)
    }
}

async fn try_await(cl: impl Future<Output = Result<()>> + Send) -> Result<()> {
    cl.await
}

#[derive(Debug)]
pub struct NntpConnection {
    pub stream: Result<NntpStream>,
    pub server_conf: NntpServerConf,
    pub uid_store: Arc<UIDStore>,
}

impl NntpStream {
    pub async fn new_connection(
        server_conf: &NntpServerConf,
    ) -> Result<(Capabilities, NntpStream)> {
        use std::net::TcpStream;
        let path = &server_conf.server_hostname;

        let stream = {
            let addr = lookup_ipv4(path, server_conf.server_port)?;
            AsyncWrapper::new(Connection::Tcp(
                TcpStream::connect_timeout(&addr, std::time::Duration::new(4, 0))
                    .chain_err_kind(crate::error::ErrorKind::Network)?,
            ))
            .chain_err_kind(crate::error::ErrorKind::Network)?
        };
        let mut res = String::with_capacity(8 * 1024);
        let mut ret = NntpStream {
            stream,
            extension_use: server_conf.extension_use,
            current_mailbox: MailboxSelection::None,
        };

        ret.read_response(&mut res, false).await?;
        ret.send_command(b"CAPABILITIES").await?;
        ret.read_response(&mut res, true).await?;
        if !res.starts_with("101 ") {
            return Err(MeliError::new(format!(
                "Could not connect to {}: expected CAPABILITIES response but got:{}",
                &server_conf.server_hostname, res
            )));
        }
        let capabilities: Vec<&str> = res.lines().skip(1).collect();

        if !capabilities
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case("VERSION 2"))
        {
            return Err(MeliError::new(format!(
                "Could not connect to {}: server is not NNTP compliant",
                &server_conf.server_hostname
            )));
        }

        if server_conf.use_tls {
            let mut connector = TlsConnector::builder();
            if server_conf.danger_accept_invalid_certs {
                connector.danger_accept_invalid_certs(true);
            }
            let connector = connector
                .build()
                .chain_err_kind(crate::error::ErrorKind::Network)?;

            if server_conf.use_starttls {
                ret.stream
                    .write_all(b"STARTTLS\r\n")
                    .await
                    .chain_err_kind(crate::error::ErrorKind::Network)?;
                ret.read_response(&mut res, false).await?;
                if !res.starts_with("382 ") {
                    return Err(MeliError::new(format!(
                        "Could not connect to {}: could not begin TLS negotiation, got: {}",
                        &server_conf.server_hostname, res
                    )));
                }
            }

            {
                // FIXME: This is blocking
                let socket = ret
                    .stream
                    .into_inner()
                    .chain_err_kind(crate::error::ErrorKind::Network)?;
                let mut conn_result = connector.connect(path, socket);
                if let Err(native_tls::HandshakeError::WouldBlock(midhandshake_stream)) =
                    conn_result
                {
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
                                p.chain_err_kind(crate::error::ErrorKind::Network)?;
                            }
                        }
                    }
                }
                ret.stream = AsyncWrapper::new(Connection::Tls(
                    conn_result.chain_err_kind(crate::error::ErrorKind::Network)?,
                ))
                .chain_err_kind(crate::error::ErrorKind::Network)?;
            }
        }
        //ret.send_command(
        //    format!(
        //        "LOGIN \"{}\" \"{}\"",
        //        &server_conf.server_username, &server_conf.server_password
        //    )
        //    .as_bytes(),
        //)
        //.await?;

        ret.send_command(b"CAPABILITIES").await?;
        ret.read_response(&mut res, true).await?;
        if !res.starts_with("101 ") {
            return Err(MeliError::new(format!(
                "Could not connect to {}: expected CAPABILITIES response but got:{}",
                &server_conf.server_hostname, res
            )));
        }
        let capabilities: HashSet<String> = res.lines().skip(1).map(|l| l.to_string()).collect();
        #[cfg(feature = "deflate_compression")]
        #[cfg(feature = "deflate_compression")]
        if capabilities.contains("COMPRESS DEFLATE") && ret.extension_use.deflate {
            ret.send_command(b"COMPRESS DEFLATE").await?;
            ret.read_response(&mut res, false).await?;
            if !res.starts_with("206 ") {
                crate::log(
                    format!(
                        "Could not use COMPRESS=DEFLATE in account `{}`: server replied with `{}`",
                        server_conf.server_hostname, res
                    ),
                    crate::LoggingLevel::WARN,
                );
            } else {
                let NntpStream {
                    stream,
                    extension_use,
                    current_mailbox,
                } = ret;
                let stream = stream.into_inner()?;
                return Ok((
                    capabilities,
                    NntpStream {
                        stream: AsyncWrapper::new(stream.deflate())?,
                        extension_use,
                        current_mailbox,
                    },
                ));
            }
        }

        Ok((capabilities, ret))
    }

    pub async fn read_response(&mut self, ret: &mut String, is_multiline: bool) -> Result<()> {
        self.read_lines(ret, is_multiline).await?;
        Ok(())
    }

    pub async fn read_lines(&mut self, ret: &mut String, is_multiline: bool) -> Result<()> {
        let mut buf: Vec<u8> = vec![0; Connection::IO_BUF_SIZE];
        ret.clear();
        let mut last_line_idx: usize = 0;
        loop {
            match self.stream.read(&mut buf).await {
                Ok(0) => break,
                Ok(b) => {
                    ret.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..b]) });
                    if let Some(mut pos) = ret[last_line_idx..].rfind("\r\n") {
                        if !is_multiline {
                            break;
                        } else if let Some(pos) = ret.find("\r\n.\r\n") {
                            ret.replace_range(pos + "\r\n".len()..pos + "\r\n.\r\n".len(), "");
                            break;
                        }
                        if ret[last_line_idx..].starts_with("205 ") {
                            return Err(MeliError::new(format!("Disconnected: {}", ret)));
                        }
                        if let Some(prev_line) =
                            ret[last_line_idx..pos + last_line_idx].rfind("\r\n")
                        {
                            last_line_idx += prev_line + "\r\n".len();
                            pos -= prev_line + "\r\n".len();
                        }
                        last_line_idx += pos + "\r\n".len();
                    }
                }
                Err(e) => {
                    return Err(MeliError::from(e).set_err_kind(crate::error::ErrorKind::Network));
                }
            }
        }
        //debug!("returning nntp response:\n{:?}", &ret);
        Ok(())
    }

    pub async fn send_command(&mut self, command: &[u8]) -> Result<()> {
        if let Err(err) = try_await(async move {
            let command = command.trim();
            self.stream.write_all(command).await?;
            self.stream.write_all(b"\r\n").await?;
            self.stream.flush().await?;
            debug!("sent: {}", unsafe {
                std::str::from_utf8_unchecked(command)
            });
            Ok(())
        })
        .await
        {
            debug!("stream send_command err {:?}", err);
            Err(err.set_err_kind(crate::error::ErrorKind::Network))
        } else {
            Ok(())
        }
    }
}

impl NntpConnection {
    pub fn new_connection(
        server_conf: &NntpServerConf,
        uid_store: Arc<UIDStore>,
    ) -> NntpConnection {
        NntpConnection {
            stream: Err(MeliError::new("Offline".to_string())),
            server_conf: server_conf.clone(),
            uid_store,
        }
    }

    pub fn connect<'a>(&'a mut self) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            if let (instant, ref mut status @ Ok(())) = *self.uid_store.is_online.lock().unwrap() {
                if Instant::now().duration_since(instant) >= std::time::Duration::new(60 * 30, 0) {
                    *status = Err(MeliError::new("Connection timed out"));
                    self.stream = Err(MeliError::new("Connection timed out"));
                }
            }
            if self.stream.is_ok() {
                self.uid_store.is_online.lock().unwrap().0 = Instant::now();
                return Ok(());
            }
            let new_stream = NntpStream::new_connection(&self.server_conf).await;
            if let Err(err) = new_stream.as_ref() {
                *self.uid_store.is_online.lock().unwrap() = (Instant::now(), Err(err.clone()));
            } else {
                *self.uid_store.is_online.lock().unwrap() = (Instant::now(), Ok(()));
            }
            let (capabilities, stream) = new_stream?;
            self.stream = Ok(stream);
            *self.uid_store.capabilities.lock().unwrap() = capabilities;
            Ok(())
        })
    }

    pub fn read_response<'a>(
        &'a mut self,
        ret: &'a mut String,
        is_multiline: bool,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            ret.clear();
            self.stream.as_mut()?.read_response(ret, is_multiline).await
        })
    }

    pub async fn read_lines(&mut self, ret: &mut String, is_multiline: bool) -> Result<()> {
        self.stream.as_mut()?.read_lines(ret, is_multiline).await?;
        Ok(())
    }

    pub async fn send_command(&mut self, command: &[u8]) -> Result<()> {
        if let Err(err) =
            try_await(async { self.stream.as_mut()?.send_command(command).await }).await
        {
            self.stream = Err(err.clone());
            debug!(err.kind);
            if err.kind.is_network() {
                debug!(self.connect().await)?;
            }
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn add_refresh_event(&mut self, ev: crate::backends::RefreshEvent) {
        if let Some(ref sender) = self.uid_store.sender.read().unwrap().as_ref() {
            sender.send(ev);
            for ev in self.uid_store.refresh_events.lock().unwrap().drain(..) {
                sender.send(ev);
            }
        } else {
            self.uid_store.refresh_events.lock().unwrap().push(ev);
        }
    }
}
