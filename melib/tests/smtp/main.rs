//
// meli
//
// Copyright 2025 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

// [ref:TODO]: add tests for SMTP server timeouts RFC5321 Section 4.5.3.2
// [ref:TODO]: add tests for SMTP server extensions
// [ref:TODO]: add tests for SMTP auth methods
// [ref:TODO]: add tests for STARTTLS/TLS

#![cfg(feature = "smtp")]

use rusty_fork::rusty_fork_test;

rusty_fork_test! {
    #[test]
    fn test_smtp_transaction() {
        tests::run_smtp_transaction();
    }

    #[test]
    fn test_smtp_pipelining() {
        tests::run_smtp_pipelining();
    }
}

pub mod server {
    use std::{
        net::{IpAddr, SocketAddr, TcpListener},
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::UnboundedReceiver,
        future::{self, Either},
        io::{AsyncReadExt as _, AsyncWriteExt as _},
        StreamExt as _,
    };
    use melib::{
        backends::prelude::*,
        imap::ImapLineSplit as _,
        parser::BytesExt as _,
        smtp::{ReplyCode, SmtpAuth, SmtpExtensionSupport, SmtpSecurity},
        utils::connections::Connection,
    };
    use smol::Async;

    #[derive(Debug)]
    pub enum ServerEvent {
        Quit,
    }

    #[derive(Clone, Debug)]
    pub enum Message {
        Helo,
        Mail {
            from: String,
        },
        Rcpt {
            from: String,
            to: Vec<String>,
        },
        DataStart {
            from: String,
            to: Vec<String>,
        },
        Data {
            #[allow(dead_code)]
            from: String,
            to: Vec<String>,
            buf: Vec<u8>,
        },
    }

    const CRLF: &str = "\r\n";

    type QueuedMessage = ((IpAddr, String), Message);

    #[derive(Debug)]
    pub struct ServerState {
        pub messages: Vec<QueuedMessage>,
        pub reject_mailboxes: Vec<String>,
        pub stored: Vec<(String, Envelope)>,
        pub auth: SmtpAuth,
        pub security: SmtpSecurity,
        pub extensions: SmtpExtensionSupport,
    }

    impl ServerState {
        pub fn clear(&mut self) {
            self.messages.clear();
            self.reject_mailboxes.clear();
            self.stored.clear();
        }

        /// <https://www.rfc-editor.org/info/rfc5321/#section-4.1.1.1>
        fn ehlo(&mut self, ip: IpAddr, domain: &str) -> (String, ReplyCode) {
            log::debug!("EHLOing ip {ip:?} domain {domain:?}");
            // Syntax:

            // ehlo           = "EHLO" SP ( Domain / address-literal ) CRLF

            // helo           = "HELO" SP Domain CRLF

            // Normally, the response to EHLO will be a multiline reply.  Each line
            // of the response contains a keyword and, optionally, one or more
            // parameters.  Following the normal syntax for multiline replies, these
            // keywords follow the code (250) and a hyphen for all but the last
            // line, and the code and a space for the last line.  The syntax for a
            // positive response, using the ABNF notation and terminal symbols of
            // RFC 5234 [7], is:

            // ehlo-ok-rsp    = ( "250" SP Domain [ SP ehlo-greet ] CRLF )
            //                  / ( "250-" Domain [ SP ehlo-greet ] CRLF
            //                  *( "250-" ehlo-line CRLF )
            //                  "250" SP ehlo-line CRLF )

            let mut extensions = vec![];
            for (support, ext) in [
                (self.extensions.pipelining, "PIPELINING"),
                (self.extensions.chunking, "CHUNKING"),
                (self.extensions.prdr, "PRDR"),
                (self.extensions._8bitmime, "8BITMIME"),
                (self.extensions.binarymime, "BINARYMIME"),
                (self.extensions.smtputf8, "SMTPUTF8"),
            ] {
                if support {
                    extensions.push(ext);
                }
            }
            self.messages
                .push(((ip, domain.to_string()), Message::Helo));
            let mut extensions = extensions.into_iter().peekable();
            let mut reply = String::new();
            while let Some(ext) = extensions.next() {
                let is_last = extensions.peek().is_none();
                if is_last {
                    reply.push_str("250 ");
                } else {
                    reply.push_str("250-");
                }
                reply.push_str(ext);
                reply.push_str(CRLF);
            }
            (reply, ReplyCode::_250)
        }

        fn mail(&mut self, ip: IpAddr, domain: &str, from: &str) -> ReplyCode {
            log::debug!("mail() ip {ip:?} domain {domain:?} from {from:?}");
            if let Some((_, message)) = self
                .messages
                .iter_mut()
                .rev()
                .find(|((i, d), _)| (i, d.as_str()) == (&ip, domain))
            {
                log::debug!("mail is {:?}", message);
                if matches!(message, Message::Helo) {
                    *message = Message::Mail {
                        from: from.to_string(),
                    };
                    return ReplyCode::_250;
                }
            }
            ReplyCode::_451
        }

        fn rcpt(&mut self, _to: &str) -> ReplyCode {
            log::debug!("rcpt() to {_to:?}");
            if let Some((_, message)) = self.messages.last_mut() {
                if let Some(pos) = self.reject_mailboxes.iter().position(|rej| rej == _to) {
                    self.reject_mailboxes.remove(pos);
                    return ReplyCode::_550;
                }

                log::debug!("rcpt mail is {:?}", message);
                if let Message::Mail { from } = message {
                    *message = Message::Rcpt {
                        from: from.clone(),
                        to: vec![_to.to_string()],
                    };
                    return ReplyCode::_250;
                } else if let Message::Rcpt { to, .. } = message {
                    to.push(_to.to_string());
                    return ReplyCode::_250;
                } else {
                    panic!("unexpected mail {message:?}");
                }
            }
            ReplyCode::_451
        }

        fn data_start(&mut self) -> ReplyCode {
            if let Some(((_, _), ref mut message)) = self.messages.last_mut() {
                log::debug!("data_start mail is {:?}", message);
                if let Message::Rcpt { from, to } = message {
                    *message = Message::DataStart {
                        from: from.to_string(),
                        to: to.to_vec(),
                    };
                    return ReplyCode::_354;
                }
            }
            ReplyCode::_451
        }

        fn data(&mut self, _buf: &[u8]) -> std::result::Result<(), std::io::Error> {
            if let Some(((_, _), ref mut message)) = self.messages.last_mut() {
                if let Message::DataStart { from, to } = message {
                    *message = Message::Data {
                        from: from.to_string(),
                        to: to.clone(),
                        buf: _buf.to_vec(),
                    };
                    return Ok(());
                } else if let Message::Data { buf, .. } = message {
                    buf.extend(_buf.iter());
                    return Ok(());
                }
            }
            Ok(())
        }

        fn data_end(&mut self) -> ReplyCode {
            let last = self.messages.pop();
            if let Some(((ip, domain), Message::Data { from: _, to, buf })) = last {
                for to in to {
                    match Envelope::from_bytes(&buf, None) {
                        Ok(env) => {
                            log::debug!("data_end env is {:?}", env);
                            log::debug!("data_end env.other_headers is {:?}", env.other_headers());
                            self.stored.push((to.clone(), env));
                        }
                        Err(err) => {
                            log::debug!("envelope parse error {err}");
                        }
                    }
                }
                self.messages.push(((ip, domain), Message::Helo));
                return ReplyCode::_250;
            }
            log::debug!("last self.messages item was not Message::Data: {last:?}");
            ReplyCode::_451
        }
    }

    pub struct SmtpServer {
        pub tcp_listener: Async<TcpListener>,
        pub event_receiver: UnboundedReceiver<ServerEvent>,
        pub state: Arc<Mutex<ServerState>>,
        pub addr: SocketAddr,
    }

    impl SmtpServer {
        pub fn new(
            event_receiver: UnboundedReceiver<ServerEvent>,
            auth: SmtpAuth,
            security: SmtpSecurity,
            mut extensions: SmtpExtensionSupport,
        ) -> Self {
            extensions.validate().expect("invalid extensions");
            let tcp_listener =
                smol::Async::new(TcpListener::bind(("127.0.0.1", 0)).unwrap()).unwrap();
            let addr = tcp_listener.as_ref().local_addr().unwrap();
            let state = Arc::new(Mutex::new(ServerState {
                messages: vec![],
                stored: vec![],
                reject_mailboxes: vec![],
                auth,
                security,
                extensions,
            }));
            Self {
                tcp_listener,
                event_receiver,
                state,
                addr,
            }
        }

        async fn handle_connection(
            (mut tcp_stream, socket_addr): (Async<Connection>, SocketAddr),
            server_addr: SocketAddr,
            state: Arc<Mutex<ServerState>>,
        ) -> Result<()> {
            async fn read_line<'a>(
                tcp_stream: &mut Async<Connection>,
                buf: &'a mut [u8],
                start: &mut usize,
                end: &mut usize,
            ) -> Result<Option<&'a [u8]>> {
                if *start == 0 || !buf[*start..*end].contains_subsequence(b"\r\n") {
                    let read_bytes = tcp_stream.read(&mut buf[*end..]).await?;
                    *end += read_bytes;
                    if !buf[*start..*end].contains_subsequence(b"\r\n") {
                        return Ok(None);
                    }
                    let Some(input) = buf[*start..*end].split_rn().next() else {
                        return Ok(None);
                    };
                    *start += input.len();
                    if *start == *end {
                        *start = 0;
                        *end = 0;
                    }
                    Ok(Some(input))
                } else {
                    let rest = &buf[*start..*end];
                    let input = rest.split_rn().next().expect("line");
                    *start += input.len();
                    if *start == *end {
                        *start = 0;
                        *end = 0;
                    }
                    Ok(Some(input))
                }
            }
            async fn write_reply_code(
                tcp_stream: &mut Async<Connection>,
                reply_code: ReplyCode,
            ) -> Result<()> {
                log::debug!("writing reply code {reply_code:?}");
                tcp_stream
                    .write_all(
                        format!("{} {}\r\n", reply_code.value(), reply_code.as_str()).as_bytes(),
                    )
                    .await?;
                tcp_stream.flush().await?;
                Ok(())
            }

            let mut buf = vec![0; 64 * 1024];
            let mut buf_start = 0;
            let mut buf_end = 0;
            tcp_stream
                .write_all(format!("220 {} Service ready\r\n", server_addr.ip()).as_bytes())
                .await
                .unwrap();
            tcp_stream.flush().await.unwrap();
            let mut domain = String::new();
            'main: loop {
                'command_loop: loop {
                    let Some(line) =
                        read_line(&mut tcp_stream, &mut buf, &mut buf_start, &mut buf_end).await?
                    else {
                        continue 'command_loop;
                    };
                    let input = String::from_utf8_lossy(line).to_string();
                    log::debug!("'command_loop received: {input:?}");
                    let (cmd, rest) = input.split_once(' ').unwrap_or((&input, ""));
                    match cmd {
                        "QUIT\r\n" => {
                            write_reply_code(&mut tcp_stream, ReplyCode::_221).await?;
                            break 'main;
                        }
                        "NOOP\r\n" => {
                            write_reply_code(&mut tcp_stream, ReplyCode::_250).await?;
                        }
                        "EHLO" => {
                            domain = rest.trim().to_string();
                            let (reply, reply_code) =
                                state.lock().unwrap().ehlo(socket_addr.ip(), &domain);
                            if reply.is_empty() {
                                write_reply_code(&mut tcp_stream, reply_code).await?;
                            } else {
                                tcp_stream.write_all(reply.as_bytes()).await?;
                                tcp_stream.flush().await?;
                            }
                        }
                        "MAIL" => {
                            let reply_code = state.lock().unwrap().mail(
                                socket_addr.ip(),
                                &domain,
                                rest.strip_prefix("FROM:<")
                                    .unwrap()
                                    .trim_end()
                                    .strip_suffix(">")
                                    .unwrap(),
                            );
                            write_reply_code(&mut tcp_stream, reply_code).await?;
                        }
                        "RCPT" => {
                            let reply_code = state.lock().unwrap().rcpt(
                                rest.strip_prefix("TO:<")
                                    .unwrap()
                                    .trim_end()
                                    .strip_suffix(">")
                                    .unwrap(),
                            );
                            write_reply_code(&mut tcp_stream, reply_code).await?;
                        }
                        "DATA\r\n" => {
                            let reply_code = state.lock().unwrap().data_start();
                            write_reply_code(&mut tcp_stream, reply_code).await?;
                            if matches!(reply_code, ReplyCode::_354) {
                                break 'command_loop;
                            }
                        }
                        other => panic!("Unexpected cmd: {other} {rest:?}"),
                    }
                }
                let mut data_buf = vec![0; 64 * 1024];
                let mut data_cursor = 0;
                'data_loop: loop {
                    let read_bytes = match tcp_stream.read(&mut data_buf[data_cursor..]).await {
                        Ok(b) => b,
                        Err(err) => {
                            log::error!("'data_loop: {err}");
                            return Ok(());
                        }
                    };
                    if read_bytes == 0 {
                        log::error!("read 0 bytes in DATA loop, exiting.");
                        return Ok(());
                    }
                    if !data_buf[..(data_cursor + read_bytes)].ends_with(b"\r\n.\r\n") {
                        data_cursor += read_bytes;
                        continue 'data_loop;
                    }
                    let input = String::from_utf8_lossy(&data_buf[..(data_cursor + read_bytes)]);
                    log::debug!("received DATA: {input:?}");
                    state
                        .lock()
                        .unwrap()
                        .data(&data_buf[..(data_cursor + read_bytes)])
                        .unwrap();
                    let reply_code = state.lock().unwrap().data_end();
                    write_reply_code(&mut tcp_stream, reply_code).await.unwrap();
                    break 'data_loop;
                }
            }
            Ok(())
        }

        pub async fn serve(self) -> Result<()> {
            let Self {
                tcp_listener,
                event_receiver,
                state,
                addr,
            } = self;

            let mut event_receiver = Box::pin(event_receiver.into_future());
            let mut accept = Box::pin(tcp_listener.accept());
            loop {
                match future::select(accept.as_mut(), event_receiver.as_mut()).await {
                    Either::Left((tcp_stream, _)) => {
                        accept = Box::pin(tcp_listener.accept());
                        let (tcp_stream, socket_addr) = tcp_stream?;
                        let mut connection = Connection::new_tcp(tcp_stream.into_inner().unwrap());
                        connection.set_trace(true);
                        Self::handle_connection(
                            (Async::new(connection).unwrap(), socket_addr),
                            addr,
                            state.clone(),
                        )
                        .await?;
                    }
                    Either::Right(((event, stream), _)) => {
                        *event_receiver = stream.into_future();
                        let Some(event) = event else {
                            continue;
                        };
                        match event {
                            ServerEvent::Quit => {
                                return Ok(());
                            }
                        }
                    }
                }
            }
        }
    }
}

pub mod tests {
    use std::thread;

    use futures::{channel::mpsc::unbounded, executor::block_on};
    use melib::{
        conf::Secret,
        email::Address,
        smtp::*,
        utils::logging::{LogLevel, Logger},
    };

    use super::server::*;

    /// Run a simple SMTP transaction without credential authentication.
    pub fn run_smtp_transaction() {
        let mut _logger = Logger::new_with(LogLevel::TRACE, true);
        let (server_event_sender, server_event_receiver) = unbounded();
        let server = SmtpServer::new(
            server_event_receiver,
            SmtpAuth::None,
            SmtpSecurity::None,
            SmtpExtensionSupport {
                pipelining: false,
                chunking: false,
                _8bitmime: false,
                prdr: false,
                binarymime: false,
                smtputf8: false,
                auth: false,
                dsn_notify: None,
            },
        );
        let server_state = server.state.clone();

        let smtp_server_conf = SmtpServerConf {
            hostname: Secret::Value(server.addr.ip().to_string()),
            port: server.addr.port(),
            envelope_from: "user@example.com".into(),
            auth: SmtpAuth::None,
            security: SmtpSecurity::None,
            extensions: Default::default(),
            trace: true,
        };
        let smtp_handle = thread::spawn(move || block_on(server.serve()));
        let new_mail = r#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#;
        block_on(async move {
            {
                let mut connection = SmtpConnection::new_connection(smtp_server_conf.clone())
                    .await
                    .unwrap();
                connection
                    .mail_transaction(
                        new_mail,
                        /* tos */
                        Some(&[
                            Address::new(None::<&str>, "user2@example.com"),
                            Address::new(None::<&str>, "postmaster@example.com"),
                        ]),
                    )
                    .await
                    .unwrap();
                connection.quit().await.unwrap();
                let stored = std::mem::take(&mut server_state.lock().unwrap().stored);
                assert_eq!(stored.len(), 2);
                assert_eq!(stored[0].0, "user2@example.com");
                assert_eq!(stored[1].0, "postmaster@example.com");
            }
            {
                let mut connection = SmtpConnection::new_connection(smtp_server_conf.clone())
                    .await
                    .unwrap();
                connection
                    .mail_transaction(new_mail, /* tos */ None)
                    .await
                    .unwrap();
                connection.quit().await.unwrap();
                let stored = std::mem::take(&mut server_state.lock().unwrap().stored);
                assert_eq!(stored.len(), 1);
                assert_eq!(stored[0].0, "myself@example.com");
            }
            let new_mail = r#"From: "some name" <some@example.com>
To: "first m. name" <myself@example.com>
Cc: "first m. name" <myself@example.com>, "Bj=?UTF-8?B?w7Y=?=rn" <bjorn@example.com>
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#;
            {
                let mut connection = SmtpConnection::new_connection(smtp_server_conf)
                    .await
                    .unwrap();
                connection
                    .mail_transaction(new_mail, /* tos */ None)
                    .await
                    .unwrap();
                connection.quit().await.unwrap();
                let stored = std::mem::take(&mut server_state.lock().unwrap().stored);
                assert_eq!(stored.len(), 3);
                assert_eq!(stored[0].0, "myself@example.com");
                assert_eq!(stored[1].0, "myself@example.com");
                assert_eq!(stored[2].0, "bjorn@example.com");
            }
        });
        server_event_sender
            .unbounded_send(ServerEvent::Quit)
            .unwrap();
        _ = smtp_handle.join().unwrap();
    }

    pub fn run_smtp_pipelining() {
        let mut _logger = Logger::new_with(LogLevel::TRACE, true);
        let (server_event_sender, server_event_receiver) = unbounded();
        let server = SmtpServer::new(
            server_event_receiver,
            SmtpAuth::None,
            SmtpSecurity::None,
            SmtpExtensionSupport {
                pipelining: true,
                chunking: false,
                _8bitmime: false,
                prdr: false,
                binarymime: false,
                smtputf8: false,
                auth: false,
                dsn_notify: None,
            },
        );
        let server_state = server.state.clone();

        let smtp_server_conf = SmtpServerConf {
            hostname: Secret::Value(server.addr.ip().to_string()),
            port: server.addr.port(),
            envelope_from: "user@example.com".into(),
            auth: SmtpAuth::None,
            security: SmtpSecurity::None,
            extensions: Default::default(),
            trace: true,
        };
        let smtp_handle = thread::spawn(move || block_on(server.serve()));
        let new_mail = r#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#;
        block_on(async move {
            {
                let mut connection = SmtpConnection::new_connection(smtp_server_conf.clone())
                    .await
                    .unwrap();
                connection
                    .mail_transaction(
                        new_mail,
                        /* tos */
                        Some(&[
                            Address::new(None::<&str>, "user2@example.com"),
                            Address::new(None::<&str>, "postmaster@example.com"),
                        ]),
                    )
                    .await
                    .unwrap();
                connection.quit().await.unwrap();
                let stored = std::mem::take(&mut server_state.lock().unwrap().stored);
                assert_eq!(stored.len(), 2);
                assert_eq!(stored[0].0, "user2@example.com");
                assert_eq!(stored[1].0, "postmaster@example.com");
            }
            {
                server_state
                    .lock()
                    .unwrap()
                    .reject_mailboxes
                    .push("user2@example.com".to_string());
                let mut connection = SmtpConnection::new_connection(smtp_server_conf.clone())
                    .await
                    .unwrap();
                let err = connection
                    .mail_transaction(
                        new_mail,
                        /* tos */
                        Some(&[
                            Address::new(None::<&str>, "user2@example.com"),
                            Address::new(None::<&str>, "postmaster@example.com"),
                        ]),
                    )
                    .await
                    .unwrap_err();
                assert!(
                    err.details.as_ref().unwrap().starts_with("SMTP 550 error"),
                    "{err:?}"
                );
                connection.quit().await.unwrap();
                let mut server_state = server_state.lock().unwrap();
                let stored = std::mem::take(&mut server_state.stored);
                assert_eq!(stored.len(), 0);
                server_state.clear();
            }
            {
                server_state
                    .lock()
                    .unwrap()
                    .reject_mailboxes
                    .push("postmaster@example.com".to_string());
                let mut connection = SmtpConnection::new_connection(smtp_server_conf.clone())
                    .await
                    .unwrap();
                let err = connection
                    .mail_transaction(
                        new_mail,
                        /* tos */
                        Some(&[
                            Address::new(None::<&str>, "user2@example.com"),
                            Address::new(None::<&str>, "postmaster@example.com"),
                        ]),
                    )
                    .await
                    .unwrap_err();
                assert!(
                    err.details.as_ref().unwrap().starts_with("SMTP 550 error"),
                    "{err:?}"
                );
                connection.quit().await.unwrap();
                let mut server_state = server_state.lock().unwrap();
                let stored = std::mem::take(&mut server_state.stored);
                assert_eq!(stored.len(), 0);
                server_state.clear();
            }
        });
        server_event_sender
            .unbounded_send(ServerEvent::Quit)
            .unwrap();
        _ = smtp_handle.join().unwrap();
    }
}
