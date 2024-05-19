//
// melib
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use std::{
    net::IpAddr,
    sync::{Arc, Mutex},
    thread,
};

use mailin_embedded::{
    response::{INTERNAL_ERROR, OK},
    Handler, Response, Server, SslConfig,
};

use super::*;

const ADDRESS: &str = "localhost:8825";
#[derive(Clone, Debug)]
enum Message {
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

#[derive(Clone, Debug)]
struct MyHandler {
    mails: Arc<Mutex<Vec<QueuedMail>>>,
    stored: Arc<Mutex<Vec<(String, crate::Envelope)>>>,
}

impl Handler for MyHandler {
    fn helo(&mut self, ip: IpAddr, domain: &str) -> Response {
        eprintln!("helo ip {:?} domain {:?}", ip, domain);
        self.mails
            .lock()
            .unwrap()
            .push(((ip, domain.to_string()), Message::Helo));
        OK
    }

    fn mail(&mut self, ip: IpAddr, domain: &str, from: &str) -> Response {
        eprintln!("mail() ip {:?} domain {:?} from {:?}", ip, domain, from);
        if let Some((_, message)) = self
            .mails
            .lock()
            .unwrap()
            .iter_mut()
            .rev()
            .find(|((i, d), _)| (i, d.as_str()) == (&ip, domain))
        {
            eprintln!("mail is {:?}", &message);
            if matches!(message, Message::Helo) {
                *message = Message::Mail {
                    from: from.to_string(),
                };
                return OK;
            }
        }
        INTERNAL_ERROR
    }

    fn rcpt(&mut self, _to: &str) -> Response {
        eprintln!("rcpt() to {:?}", _to);
        if let Some((_, message)) = self.mails.lock().unwrap().last_mut() {
            eprintln!("rcpt mail is {:?}", &message);
            if let Message::Mail { from } = message {
                *message = Message::Rcpt {
                    from: from.clone(),
                    to: vec![_to.to_string()],
                };
                return OK;
            } else if let Message::Rcpt { to, .. } = message {
                to.push(_to.to_string());
                return OK;
            }
        }
        INTERNAL_ERROR
    }

    fn data_start(
        &mut self,
        _domain: &str,
        _from: &str,
        _is8bit: bool,
        _to: &[String],
    ) -> Response {
        eprintln!(
            "data_start() domain {:?} from {:?} is8bit {:?} to {:?}",
            _domain, _from, _is8bit, _to
        );
        if let Some(((_, d), ref mut message)) = self.mails.lock().unwrap().last_mut() {
            if d != _domain {
                return INTERNAL_ERROR;
            }
            eprintln!("data_start mail is {:?}", &message);
            if let Message::Rcpt { from, to } = message {
                *message = Message::DataStart {
                    from: from.to_string(),
                    to: to.to_vec(),
                };
                return OK;
            }
        }
        INTERNAL_ERROR
    }

    fn data(&mut self, _buf: &[u8]) -> std::result::Result<(), std::io::Error> {
        if let Some(((_, _), ref mut message)) = self.mails.lock().unwrap().last_mut() {
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

    fn data_end(&mut self) -> Response {
        let last = self.mails.lock().unwrap().pop();
        if let Some(((ip, domain), Message::Data { from: _, to, buf })) = last {
            for to in to {
                match crate::Envelope::from_bytes(&buf, None) {
                    Ok(env) => {
                        eprintln!("data_end env is {:?}", &env);
                        eprintln!("data_end env.other_headers is {:?}", env.other_headers());
                        self.stored.lock().unwrap().push((to.clone(), env));
                    }
                    Err(err) => {
                        eprintln!("envelope parse error {}", err);
                    }
                }
            }
            self.mails
                .lock()
                .unwrap()
                .push(((ip, domain), Message::Helo));
            return OK;
        }
        eprintln!("last self.mails item was not Message::Data: {last:?}");
        INTERNAL_ERROR
    }
}

fn get_smtp_conf() -> SmtpServerConf {
    SmtpServerConf {
        hostname: "localhost".into(),
        port: 8825,
        envelope_from: "foo-chat@example.com".into(),
        auth: SmtpAuth::None,
        security: SmtpSecurity::None,
        extensions: Default::default(),
    }
}

#[test]
#[ignore]
fn test_smtp() {
    stderrlog::new()
        .quiet(false)
        .verbosity(0)
        .show_module_names(true)
        .timestamp(stderrlog::Timestamp::Millisecond)
        .init()
        .unwrap();

    let handler = MyHandler {
        mails: Arc::new(Mutex::new(vec![])),
        stored: Arc::new(Mutex::new(vec![])),
    };
    let handler2 = handler.clone();
    let _smtp_handle = thread::spawn(move || {
        let mut server = Server::new(handler2);

        server
            .with_name("test")
            .with_ssl(SslConfig::None)
            .unwrap()
            .with_addr(ADDRESS)
            .unwrap();
        eprintln!("Running smtp server at {}", ADDRESS);
        server.serve().expect("Could not run server");
    });

    let smtp_server_conf = get_smtp_conf();
    let input_str = include_str!("../../tests/data/test_sample_longmessage.eml");
    match crate::Envelope::from_bytes(input_str.as_bytes(), None) {
        Ok(_envelope) => {}
        Err(err) => {
            panic!("Could not parse message: {}", err);
        }
    }
    let mut connection =
        futures::executor::block_on(SmtpConnection::new_connection(smtp_server_conf)).unwrap();
    futures::executor::block_on(connection.mail_transaction(
        input_str,
        /* tos */
        Some(&[
            Address::try_from("foo-chat@example.com").unwrap(),
            Address::try_from("webmaster@example.com").unwrap(),
        ]),
    ))
    .unwrap();
    futures::executor::block_on(connection.quit()).unwrap();
    assert_eq!(handler.stored.lock().unwrap().len(), 2);
}
