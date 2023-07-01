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

use crate::{
    backends::{BackendMailbox, MailboxHash},
    email::parser::BytesExt,
    error::*,
    log,
    utils::connections::{lookup_ipv4, Connection},
};
extern crate native_tls;
use std::{collections::HashSet, future::Future, pin::Pin, sync::Arc, time::Instant};

use futures::io::{AsyncReadExt, AsyncWriteExt};
use native_tls::TlsConnector;
pub use smol::Async as AsyncWrapper;

use super::{Capabilities, NntpServerConf, UIDStore};

#[derive(Debug, Default, Clone, Copy)]
pub struct NntpExtensionUse {
    #[cfg(feature = "deflate_compression")]
    pub deflate: bool,
}

#[derive(Debug)]
pub struct NntpStream {
    pub stream: AsyncWrapper<Connection>,
    pub extension_use: NntpExtensionUse,
    pub current_mailbox: MailboxSelection,
    pub supports_submission: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MailboxSelection {
    None,
    Select(MailboxHash),
}

impl MailboxSelection {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Self::None)
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
    pub async fn new_connection(server_conf: &NntpServerConf) -> Result<(Capabilities, Self)> {
        use std::net::TcpStream;
        let path = &server_conf.server_hostname;

        let stream = {
            let addr = lookup_ipv4(path, server_conf.server_port)?;
            AsyncWrapper::new(Connection::Tcp(TcpStream::connect_timeout(
                &addr,
                std::time::Duration::new(16, 0),
            )?))?
        };
        let mut res = String::with_capacity(8 * 1024);
        let mut ret = Self {
            stream,
            extension_use: server_conf.extension_use,
            current_mailbox: MailboxSelection::None,
            supports_submission: false,
        };

        if server_conf.use_tls {
            let mut connector = TlsConnector::builder();
            if server_conf.danger_accept_invalid_certs {
                connector.danger_accept_invalid_certs(true);
            }
            let connector = connector.build()?;

            if server_conf.use_starttls {
                ret.read_response(&mut res, false, &["200 ", "201 "])
                    .await?;
                ret.supports_submission = res.starts_with("200");

                ret.send_command(b"CAPABILITIES").await?;
                ret.read_response(&mut res, true, command_to_replycodes("CAPABILITIES"))
                    .await?;
                if !res.starts_with("101 ") {
                    return Err(Error::new(format!(
                        "Could not connect to {}: expected CAPABILITIES response but got:{}",
                        &server_conf.server_hostname, res
                    )));
                }
                let capabilities: Vec<&str> = res.lines().skip(1).collect();

                if !capabilities
                    .iter()
                    .any(|cap| cap.eq_ignore_ascii_case("VERSION 2"))
                {
                    return Err(Error::new(format!(
                        "Could not connect to {}: server is not NNTP VERSION 2 compliant",
                        &server_conf.server_hostname
                    )));
                }
                if !capabilities
                    .iter()
                    .any(|cap| cap.eq_ignore_ascii_case("STARTTLS"))
                {
                    return Err(Error::new(format!(
                        "Could not connect to {}: server does not support STARTTLS",
                        &server_conf.server_hostname
                    )));
                }
                ret.stream.write_all(b"STARTTLS\r\n").await?;
                ret.stream.flush().await?;
                ret.read_response(&mut res, false, command_to_replycodes("STARTTLS"))
                    .await?;
                if !res.starts_with("382 ") {
                    return Err(Error::new(format!(
                        "Could not connect to {}: could not begin TLS negotiation, got: {}",
                        &server_conf.server_hostname, res
                    )));
                }
            }

            {
                // FIXME: This is blocking
                let socket = ret.stream.into_inner()?;
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
                                p.chain_err_kind(crate::error::ErrorKind::Network(
                                    crate::error::NetworkErrorKind::InvalidTLSConnection,
                                ))?;
                            }
                        }
                    }
                }
                ret.stream =
                    AsyncWrapper::new(Connection::Tls(conn_result?)).chain_err_summary(|| {
                        format!("Could not initiate TLS negotiation to {}.", path)
                    })?;
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
        if let Err(err) = ret
            .stream
            .get_ref()
            .set_keepalive(Some(std::time::Duration::new(60 * 9, 0)))
        {
            log::warn!("Could not set TCP keepalive in NNTP connection: {}", err);
        }

        ret.read_response(&mut res, false, &["200 ", "201 "])
            .await?;
        ret.supports_submission = res.starts_with("200");
        ret.send_command(b"CAPABILITIES").await?;
        ret.read_response(&mut res, true, command_to_replycodes("CAPABILITIES"))
            .await?;
        if !res.starts_with("101 ") {
            return Err(Error::new(format!(
                "Could not connect to {}: expected CAPABILITIES response but got:{}",
                &server_conf.server_hostname, res
            )));
        }
        let capabilities: HashSet<String> =
            res.lines().skip(1).map(|l| l.trim().to_string()).collect();
        if !capabilities
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case("VERSION 2"))
        {
            return Err(Error::new(format!(
                "Could not connect to {}: server is not NNTP compliant",
                &server_conf.server_hostname
            )));
        }
        if !capabilities
            .iter()
            .any(|cap| cap.eq_ignore_ascii_case("POST"))
        {
            ret.supports_submission = false;
        }

        if server_conf.require_auth {
            if capabilities.iter().any(|c| c.starts_with("AUTHINFO USER")) {
                ret.send_command(
                    format!("AUTHINFO USER {}", server_conf.server_username).as_bytes(),
                )
                .await?;
                ret.read_response(&mut res, false, command_to_replycodes("AUTHINFO USER"))
                    .await
                    .chain_err_summary(|| format!("Authentication state error: {}", res))
                    .chain_err_kind(ErrorKind::Authentication)?;
                if res.starts_with("381 ") {
                    ret.send_command(
                        format!("AUTHINFO PASS {}", server_conf.server_password).as_bytes(),
                    )
                    .await?;
                    ret.read_response(&mut res, false, command_to_replycodes("AUTHINFO PASS"))
                        .await
                        .chain_err_summary(|| format!("Authentication state error: {}", res))
                        .chain_err_kind(ErrorKind::Authentication)?;
                }
            } else {
                return Err(Error::new(format!(
                    "Could not connect: no supported auth mechanisms in server capabilities: {:?}",
                    capabilities
                ))
                .set_err_kind(ErrorKind::Authentication));
            }
        }

        #[cfg(feature = "deflate_compression")]
        if capabilities.contains("COMPRESS DEFLATE") && ret.extension_use.deflate {
            ret.send_command(b"COMPRESS DEFLATE").await?;
            ret.read_response(&mut res, false, command_to_replycodes("COMPRESS DEFLATE"))
                .await
                .chain_err_summary(|| {
                    format!(
                        "Could not use COMPRESS DEFLATE in account `{}`: server replied with `{}`",
                        server_conf.server_hostname, res
                    )
                })?;
            let Self {
                stream,
                extension_use,
                current_mailbox,
                supports_submission,
            } = ret;
            let stream = stream.into_inner()?;
            return Ok((
                capabilities,
                Self {
                    stream: AsyncWrapper::new(stream.deflate())?,
                    extension_use,
                    current_mailbox,
                    supports_submission,
                },
            ));
        }

        Ok((capabilities, ret))
    }

    pub async fn read_response(
        &mut self,
        ret: &mut String,
        is_multiline: bool,
        expected_reply_code: &[&str],
    ) -> Result<()> {
        self.read_lines(ret, is_multiline, expected_reply_code)
            .await?;
        Ok(())
    }

    pub async fn read_lines(
        &mut self,
        ret: &mut String,
        is_multiline: bool,
        expected_reply_code: &[&str],
    ) -> Result<()> {
        let mut buf: Vec<u8> = vec![0; Connection::IO_BUF_SIZE];
        ret.clear();
        let mut last_line_idx: usize = 0;
        loop {
            match self.stream.read(&mut buf).await {
                Ok(0) => break,
                Ok(b) => {
                    ret.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..b]) });
                    if ret.len() > 4 {
                        if ret.starts_with("205 ") {
                            return Err(Error::new(format!("Disconnected: {}", ret)));
                        } else if ret.starts_with("501 ") || ret.starts_with("500 ") {
                            return Err(Error::new(format!("Syntax error: {}", ret)));
                        } else if ret.starts_with("403 ") {
                            return Err(Error::new(format!("Internal error: {}", ret)));
                        } else if ret.starts_with("502 ")
                            || ret.starts_with("480 ")
                            || ret.starts_with("483 ")
                            || ret.starts_with("401 ")
                        {
                            return Err(Error::new(format!("Connection state error: {}", ret))
                                .set_err_kind(ErrorKind::Authentication));
                        } else if !expected_reply_code.iter().any(|r| ret.starts_with(r)) {
                            return Err(Error::new(format!("Unexpected reply code: {}", ret)));
                        }
                    }
                    if let Some(mut pos) = ret[last_line_idx..].rfind("\r\n") {
                        if !is_multiline {
                            break;
                        } else if let Some(pos) = ret.find("\r\n.\r\n") {
                            ret.replace_range(pos + "\r\n".len()..pos + "\r\n.\r\n".len(), "");
                            break;
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
                Err(err) => {
                    return Err(Error::from(err));
                }
            }
        }
        //debug!("returning nntp response:\n{:?}", &ret);
        Ok(())
    }

    pub async fn send_command(&mut self, command: &[u8]) -> Result<()> {
        debug!("sending: {}", unsafe {
            std::str::from_utf8_unchecked(command)
        });
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
            Err(err)
        } else {
            Ok(())
        }
    }

    pub async fn send_multiline_data_block(&mut self, data: &[u8]) -> Result<()> {
        if let Err(err) = try_await(async move {
            let mut ptr = 0;
            while let Some(pos) = data[ptr..].find("\n") {
                let l = &data[ptr..ptr + pos].trim_end();
                if l.starts_with(b".") {
                    self.stream.write_all(b".").await?;
                }
                self.stream.write_all(l).await?;
                self.stream.write_all(b"\r\n").await?;
                ptr += pos + 1;
            }
            let l = &data[ptr..].trim_end();
            if !l.is_empty() {
                if l.starts_with(b".") {
                    self.stream.write_all(b".").await?;
                }
                self.stream.write_all(l).await?;
                self.stream.write_all(b"\r\n").await?;
            }
            self.stream.write_all(b".\r\n").await?;
            self.stream.flush().await?;
            debug!("sent data block {} bytes", data.len());
            Ok(())
        })
        .await
        {
            debug!("stream send_multiline_data_block err {:?}", err);
            Err(err)
        } else {
            Ok(())
        }
    }
}

impl NntpConnection {
    pub fn new_connection(server_conf: &NntpServerConf, uid_store: Arc<UIDStore>) -> Self {
        Self {
            stream: Err(Error::new("Offline".to_string())),
            server_conf: server_conf.clone(),
            uid_store,
        }
    }

    pub fn connect<'a>(&'a mut self) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            if let (instant, ref mut status @ Ok(())) = *self.uid_store.is_online.lock().unwrap() {
                if Instant::now().duration_since(instant) >= std::time::Duration::new(60 * 30, 0) {
                    *status = Err(Error::new("Connection timed out"));
                    self.stream = Err(Error::new("Connection timed out"));
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
        expected_reply_code: &'static [&str],
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            ret.clear();
            self.stream
                .as_mut()?
                .read_response(ret, is_multiline, expected_reply_code)
                .await
        })
    }

    pub async fn read_lines(
        &mut self,
        ret: &mut String,
        is_multiline: bool,
        expected_reply_code: &[&str],
    ) -> Result<()> {
        self.stream
            .as_mut()?
            .read_lines(ret, is_multiline, expected_reply_code)
            .await?;
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
        (self.uid_store.event_consumer)(
            self.uid_store.account_hash,
            crate::backends::BackendEvent::Refresh(ev),
        );
    }

    pub async fn select_group(
        &mut self,
        mailbox_hash: MailboxHash,
        force: bool,
        res: &mut String,
    ) -> Result<()> {
        if !force {
            match self.stream.as_ref()?.current_mailbox {
                MailboxSelection::Select(m) if m == mailbox_hash => return Ok(()),
                _ => {}
            }
        }
        let path = self.uid_store.mailboxes.lock().await[&mailbox_hash]
            .name()
            .to_string();
        self.send_command(format!("GROUP {}", path).as_bytes())
            .await?;
        self.read_response(res, false, command_to_replycodes("GROUP"))
            .await
            .chain_err_summary(|| {
                format!(
                    "{} Could not select newsgroup {}: expected GROUP response but got: {}",
                    &self.uid_store.account_name, path, res
                )
            })?;
        self.stream.as_mut()?.current_mailbox = MailboxSelection::Select(mailbox_hash);
        Ok(())
    }

    pub async fn send_multiline_data_block(&mut self, message: &[u8]) -> Result<()> {
        self.stream
            .as_mut()?
            .send_multiline_data_block(message)
            .await
    }
}

pub fn command_to_replycodes(c: &str) -> &'static [&'static str] {
    if c.starts_with("OVER") {
        &["224 "]
    } else if c.starts_with("LIST") {
        &["215 "]
    } else if c.starts_with("POST") {
        &["340 "]
    } else if c.starts_with("STARTTLS") {
        &["382 "]
    } else if c.starts_with("GROUP") {
        &["211 "]
    } else if c.starts_with("CAPABILITIES") {
        &["101 "]
    } else if c.starts_with("ARTICLE") {
        &["220 "]
    } else if c.starts_with("DATE") {
        &["111 "]
    } else if c.starts_with("NEWNEWS") {
        &["230 "]
    } else if c.starts_with("AUTHINFO USER") {
        &["281 ", "381 "]
    } else if c.starts_with("AUTHINFO PASS") {
        &["281 "]
    } else if c.starts_with("COMPRESS DEFLATE") {
        &["206 "]
    } else {
        &[]
    }
}
