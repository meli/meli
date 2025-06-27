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

#[cfg(feature = "smtp")]
use rusty_fork::rusty_fork_test;

#[cfg(feature = "smtp")]
rusty_fork_test! {
    #[test]
    fn test_smtp_transaction() {
        tests::run_smtp_transaction();
    }
}

#[cfg(feature = "smtp")]
pub mod server {
    use std::{
        net::{IpAddr, SocketAddr, TcpListener, TcpStream},
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::UnboundedReceiver,
        future::{self, Either},
        io::{AsyncReadExt, AsyncWriteExt},
        StreamExt,
    };
    use melib::{backends::prelude::*, smol::Async, smtp::*};

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

    type QueuedMail = ((IpAddr, String), Message);

    #[derive(Debug)]
    pub struct ServerState {
        pub mails: Vec<QueuedMail>,
        pub stored: Vec<(String, Envelope)>,
    }

    impl ServerState {
        fn helo(&mut self, ip: IpAddr, domain: &str) -> ReplyCode {
            eprintln!("helo ip {ip:?} domain {domain:?}");
            self.mails.push(((ip, domain.to_string()), Message::Helo));
            ReplyCode::_250
        }

        fn mail(&mut self, ip: IpAddr, domain: &str, from: &str) -> ReplyCode {
            eprintln!("mail() ip {ip:?} domain {domain:?} from {from:?}");
            if let Some((_, message)) = self
                .mails
                .iter_mut()
                .rev()
                .find(|((i, d), _)| (i, d.as_str()) == (&ip, domain))
            {
                eprintln!("mail is {:?}", &message);
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
            eprintln!("rcpt() to {_to:?}");
            if let Some((_, message)) = self.mails.last_mut() {
                eprintln!("rcpt mail is {:?}", &message);
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
            if let Some(((_, _), ref mut message)) = self.mails.last_mut() {
                eprintln!("data_start mail is {:?}", &message);
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
            if let Some(((_, _), ref mut message)) = self.mails.last_mut() {
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
            let last = self.mails.pop();
            if let Some(((ip, domain), Message::Data { from: _, to, buf })) = last {
                for to in to {
                    match Envelope::from_bytes(&buf, None) {
                        Ok(env) => {
                            eprintln!("data_end env is {:?}", &env);
                            eprintln!("data_end env.other_headers is {:?}", env.other_headers());
                            self.stored.push((to.clone(), env));
                        }
                        Err(err) => {
                            eprintln!("envelope parse error {err}");
                        }
                    }
                }
                self.mails.push(((ip, domain), Message::Helo));
                return ReplyCode::_250;
            }
            eprintln!("last self.mails item was not Message::Data: {last:?}");
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
        pub fn new(event_receiver: UnboundedReceiver<ServerEvent>) -> Self {
            let tcp_listener =
                smol::Async::new(TcpListener::bind(("0.0.0.0", 0)).unwrap()).unwrap();
            let addr = tcp_listener.as_ref().local_addr().unwrap();
            let state = Arc::new(Mutex::new(ServerState {
                mails: vec![],
                stored: vec![],
            }));
            Self {
                tcp_listener,
                event_receiver,
                state,
                addr,
            }
        }

        async fn handle_connection(
            (mut tcp_stream, socket_addr): (Async<TcpStream>, SocketAddr),
            server_addr: SocketAddr,
            state: Arc<Mutex<ServerState>>,
        ) -> Result<()> {
            async fn write_reply_code(tcp_stream: &mut Async<TcpStream>, reply_code: ReplyCode) {
                tcp_stream
                    .write_all(
                        format!("{} {}\r\n", reply_code.value(), reply_code.as_str()).as_bytes(),
                    )
                    .await
                    .unwrap();
                tcp_stream.flush().await.unwrap();
            }

            let mut buf = vec![0; 64 * 1024];
            let mut buf_cursor = 0;
            // let bytes = tcp_stream.read(buf).await?;
            // let lossy = String::from_utf8_lossy(&buf[..bytes]);
            // eprintln!("loop_handler read:\n{}", lossy);
            tcp_stream
                .write_all(format!("220 {} Service ready\r\n", server_addr.ip()).as_bytes())
                .await
                .unwrap();
            tcp_stream.flush().await?;
            let mut domain = String::new();
            'main: loop {
                'command_loop: loop {
                    let read_bytes = tcp_stream.read(&mut buf[buf_cursor..]).await.unwrap();
                    let input = String::from_utf8_lossy(&buf[..(buf_cursor + read_bytes)]);
                    if input.is_empty() {
                        return Ok(());
                    }
                    eprintln!("received: {input:?}");
                    if !input.ends_with("\r\n") {
                        buf_cursor += read_bytes;
                        continue 'command_loop;
                    }
                    buf_cursor = 0;
                    let (cmd, rest) = input.split_once(' ').unwrap_or((&input, ""));
                    match cmd {
                        "QUIT\r\n" => {
                            write_reply_code(&mut tcp_stream, ReplyCode::_221).await;
                            break 'main;
                        }
                        "NOOP\r\n" => {
                            write_reply_code(&mut tcp_stream, ReplyCode::_250).await;
                        }
                        "EHLO" => {
                            domain = rest.trim().to_string();
                            let reply_code = state.lock().unwrap().helo(socket_addr.ip(), &domain);
                            write_reply_code(&mut tcp_stream, reply_code).await;
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
                            write_reply_code(&mut tcp_stream, reply_code).await;
                        }
                        "RCPT" => {
                            let reply_code = state.lock().unwrap().rcpt(
                                rest.strip_prefix("TO:<")
                                    .unwrap()
                                    .trim_end()
                                    .strip_suffix(">")
                                    .unwrap(),
                            );
                            write_reply_code(&mut tcp_stream, reply_code).await;
                        }
                        "DATA\r\n" => {
                            let reply_code = state.lock().unwrap().data_start();
                            write_reply_code(&mut tcp_stream, reply_code).await;
                            if matches!(reply_code, ReplyCode::_354) {
                                break 'command_loop;
                            }
                        }
                        other => panic!("Unexpected cmd: {other} {rest:?}"),
                    }
                }
                'data_loop: loop {
                    let read_bytes = tcp_stream.read(&mut buf[buf_cursor..]).await.unwrap();
                    if read_bytes == 0 {
                        eprintln!("read 0 bytes in DATA loop, exiting.");
                        return Ok(());
                    }
                    if !buf[..(buf_cursor + read_bytes)].ends_with(b"\r\n.\r\n") {
                        buf_cursor += read_bytes;
                        continue 'data_loop;
                    }
                    let input = String::from_utf8_lossy(&buf[..(buf_cursor + read_bytes)]);
                    eprintln!("received DATA: {input:?}");
                    state
                        .lock()
                        .unwrap()
                        .data(&buf[..(buf_cursor + read_bytes)])
                        .unwrap();
                    let reply_code = state.lock().unwrap().data_end();
                    write_reply_code(&mut tcp_stream, reply_code).await;
                    buf_cursor = 0;
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
                    Either::Left((request, _)) => {
                        accept = Box::pin(tcp_listener.accept());
                        Self::handle_connection(request?, addr, state.clone()).await?;
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

#[cfg(feature = "smtp")]
pub mod tests {
    use std::thread;

    use futures::{channel::mpsc::unbounded, executor::block_on};
    use melib::{
        email::Address,
        smtp::*,
        utils::logging::{LogLevel, StderrLogger},
    };

    use super::server::*;

    /// Run a simple SMTP transaction without credential authentication.
    pub fn run_smtp_transaction() {
        let mut _logger = StderrLogger::new(LogLevel::TRACE);
        let (server_event_sender, server_event_receiver) = unbounded();
        let server = SmtpServer::new(server_event_receiver);
        let server_state = server.state.clone();

        let smtp_server_conf = SmtpServerConf {
            hostname: server.addr.ip().to_string(),
            port: server.addr.port(),
            envelope_from: "user@example.com".into(),
            auth: SmtpAuth::None,
            security: SmtpSecurity::None,
            extensions: Default::default(),
        };
        let _smtp_handle = thread::spawn(move || block_on(server.serve()));
        let new_mail = r#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#;
        let mut connection = block_on(SmtpConnection::new_connection(smtp_server_conf)).unwrap();
        block_on(connection.mail_transaction(
            new_mail,
            /* tos */
            Some(&[
                Address::new(None, "user2@example.com".to_string()),
                Address::new(None, "postmaster@example.com".to_string()),
            ]),
        ))
        .unwrap();
        block_on(connection.quit()).unwrap();
        let stored = std::mem::take(&mut server_state.lock().unwrap().stored);
        assert_eq!(stored.len(), 2);
        assert_eq!(stored[0].0, "user2@example.com");
        assert_eq!(stored[1].0, "postmaster@example.com");
        server_event_sender
            .unbounded_send(ServerEvent::Quit)
            .unwrap();
    }
}
