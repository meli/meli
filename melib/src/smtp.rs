/*
 * meli - smtp
 *
 * Copyright 2020 Manos Pitsidianakis
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

#![allow(clippy::just_underscores_and_digits)]
#![allow(clippy::needless_lifetimes)]

//! SMTP client support
//!
//! This module implements a client for the SMTP protocol as specified by [RFC
//! 5321 Simple Mail Transfer Protocol](https://www.rfc-editor.org/rfc/rfc5321).
//!
//! The connection and methods are `async` and uses the `smol` runtime.
//!# Example
//!
//! ```not_run
//! extern crate melib;
//!
//! use melib::futures;
//! use melib::smol;
//! use melib::smtp::*;
//! use melib::Result;
//! let conf = SmtpServerConf {
//!     hostname: "smtp.example.com".into(),
//!     port: 587,
//!     security: SmtpSecurity::StartTLS {
//!         danger_accept_invalid_certs: false,
//!     },
//!     extensions: SmtpExtensionSupport::default(),
//!     auth: SmtpAuth::Auto {
//!         username: "l15".into(),
//!         password: Password::CommandEval(
//!             "gpg2 --no-tty -q -d ~/.passwords/mail.gpg".into(),
//!         ),
//!         require_auth: true,
//!     },
//! };
//!
//! std::thread::Builder::new().spawn(move || {
//!     let ex = smol::Executor::new();
//!     futures::executor::block_on(ex.run(futures::future::pending::<()>()));
//! }).unwrap();
//!
//! let mut conn = futures::executor::block_on(SmtpConnection::new_connection(conf)).unwrap();
//! futures::executor::block_on(conn.mail_transaction(r#"To: l10@example.com
//! Subject: Fwd: SMTP TEST
//! From: Me <l15@example.com>
//! Message-Id: <E1hSjnr-0003fN-RL@example.com>
//! Date: Mon, 13 Jul 2020 09:02:15 +0300
//!
//! Prescriptions-R-X"#,
//!     b"l15@example.com",
//!     b"l10@example.com",
//! )).unwrap();
//! Ok(())
//! ```

use std::{borrow::Cow, convert::TryFrom, net::TcpStream, process::Command};

use futures::io::{AsyncReadExt, AsyncWriteExt};
use native_tls::TlsConnector;
use smallvec::SmallVec;
use smol::{unblock, Async as AsyncWrapper};

use crate::{
    email::{parser::BytesExt, Address, Envelope},
    error::{Error, Result, ResultIntoError},
    utils::connections::{lookup_ipv4, Connection},
};

/// Kind of server security (StartTLS/TLS/None) the client should attempt
#[derive(Debug, Copy, PartialEq, Eq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum SmtpSecurity {
    #[serde(alias = "starttls", alias = "STARTTLS")]
    StartTLS {
        #[serde(default = "crate::conf::false_val")]
        danger_accept_invalid_certs: bool,
    },
    #[serde(alias = "auto")]
    Auto {
        #[serde(default = "crate::conf::false_val")]
        danger_accept_invalid_certs: bool,
    },
    #[serde(alias = "tls", alias = "TLS")]
    Tls {
        #[serde(default = "crate::conf::false_val")]
        danger_accept_invalid_certs: bool,
    },
    #[serde(alias = "none")]
    None,
}

impl Default for SmtpSecurity {
    fn default() -> Self {
        Self::Auto {
            danger_accept_invalid_certs: false,
        }
    }
}

/// Source of user's password for SMTP authentication
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum Password {
    #[serde(alias = "raw")]
    Raw(String),
    #[serde(alias = "command_evaluation", alias = "command_eval")]
    CommandEval(String),
}

/// Kind of server authentication the client should attempt
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum SmtpAuth {
    #[serde(alias = "none")]
    None,
    #[serde(alias = "auto")]
    Auto {
        username: String,
        password: Password,
        #[serde(default = "crate::conf::true_val")]
        require_auth: bool,
        #[serde(skip_serializing, skip_deserializing, default)]
        auth_type: SmtpAuthType,
    },
    #[serde(alias = "xoauth2")]
    XOAuth2 {
        token_command: String,
        #[serde(default = "crate::conf::true_val")]
        require_auth: bool,
    },
    // md5, sasl, etc
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Default)]
pub struct SmtpAuthType {
    plain: bool,
    login: bool,
}

impl SmtpAuth {
    pub const fn require_auth(&self) -> bool {
        use SmtpAuth::*;
        match self {
            None => false,
            Auto { require_auth, .. } | XOAuth2 { require_auth, .. } => *require_auth,
        }
    }
}

/// Server configuration for connecting the SMTP client
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SmtpServerConf {
    pub hostname: String,
    pub port: u16,
    #[serde(default)]
    pub envelope_from: String,
    pub auth: SmtpAuth,
    #[serde(default)]
    pub security: SmtpSecurity,
    #[serde(default)]
    pub extensions: SmtpExtensionSupport,
}

//example: "SIZE 52428800", "8BITMIME", "PIPELINING", "CHUNKING", "PRDR",
/// Configured SMTP extensions to use
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SmtpExtensionSupport {
    #[serde(default = "crate::conf::true_val")]
    pipelining: bool,
    #[serde(default = "crate::conf::true_val")]
    chunking: bool,
    /// [RFC 6152: SMTP Service Extension for 8-bit MIME Transport](https://www.rfc-editor.org/rfc/rfc6152)
    #[serde(default = "crate::conf::true_val")]
    _8bitmime: bool,
    /// Essentially, the PRDR extension to SMTP allows (but does not require) an
    /// SMTP server to issue multiple responses after a message has been
    /// transferred, by mutual consent of the client and server. SMTP
    /// clients that support the PRDR extension then use the expanded
    /// responses as supplemental data to the responses that were received
    /// during the earlier envelope exchange.
    #[serde(default = "crate::conf::true_val")]
    prdr: bool,
    #[serde(default = "crate::conf::true_val")]
    binarymime: bool,
    /// Resources:
    /// - <http://www.postfix.org/SMTPUTF8_README.html>
    #[serde(default = "crate::conf::true_val")]
    smtputf8: bool,
    #[serde(default = "crate::conf::true_val")]
    auth: bool,
    #[serde(default = "default_dsn")]
    dsn_notify: Option<Cow<'static, str>>,
}

fn default_dsn() -> Option<Cow<'static, str>> {
    Some("FAILURE".into())
}

impl Default for SmtpExtensionSupport {
    fn default() -> Self {
        Self {
            pipelining: true,
            chunking: true,
            prdr: true,
            _8bitmime: true,
            binarymime: true,
            smtputf8: true,
            auth: true,
            dsn_notify: Some("FAILURE".into()),
        }
    }
}

/// SMTP client session object.
///
/// See module-wide documentation.
#[derive(Debug)]
pub struct SmtpConnection {
    stream: AsyncWrapper<Connection>,
    read_buffer: String,
    server_conf: SmtpServerConf,
}

impl SmtpConnection {
    /// Performs connection and if configured: TLS negotiation and SMTP AUTH
    pub async fn new_connection(mut server_conf: SmtpServerConf) -> Result<Self> {
        let path = &server_conf.hostname;
        let mut res = String::with_capacity(8 * 1024);
        let stream = match server_conf.security {
            SmtpSecurity::Auto {
                danger_accept_invalid_certs,
            }
            | SmtpSecurity::Tls {
                danger_accept_invalid_certs,
            }
            | SmtpSecurity::StartTLS {
                danger_accept_invalid_certs,
            } => {
                let mut connector = TlsConnector::builder();
                if danger_accept_invalid_certs {
                    connector.danger_accept_invalid_certs(true);
                }
                let connector = connector.build()?;

                let addr = lookup_ipv4(path, server_conf.port)?;
                let mut socket = AsyncWrapper::new(Connection::Tcp(TcpStream::connect_timeout(
                    &addr,
                    std::time::Duration::new(4, 0),
                )?))?;
                let pre_ehlo_extensions_reply = read_lines(
                    &mut socket,
                    &mut res,
                    Some((ReplyCode::_220, &[])),
                    &mut String::new(),
                )
                .await?;
                drop(pre_ehlo_extensions_reply);

                if matches!(server_conf.security, SmtpSecurity::Auto { .. }) {
                    if server_conf.port == 465 {
                        server_conf.security = SmtpSecurity::Tls {
                            danger_accept_invalid_certs,
                        };
                    } else if server_conf.port == 587 {
                        server_conf.security = SmtpSecurity::StartTLS {
                            danger_accept_invalid_certs,
                        };
                    } else {
                        return Err(Error::new(
                            "Please specify what SMTP security transport to use explicitly \
                             instead of `auto`.",
                        ));
                    }
                }
                socket.write_all(b"EHLO meli.delivery\r\n").await?;
                if matches!(server_conf.security, SmtpSecurity::StartTLS { .. }) {
                    let pre_tls_extensions_reply = read_lines(
                        &mut socket,
                        &mut res,
                        Some((ReplyCode::_250, &[])),
                        &mut String::new(),
                    )
                    .await?;
                    drop(pre_tls_extensions_reply);
                    //debug!(pre_tls_extensions_reply);
                    socket.write_all(b"STARTTLS\r\n").await?;
                    let _post_starttls_extensions_reply = read_lines(
                        &mut socket,
                        &mut res,
                        Some((ReplyCode::_220, &[])),
                        &mut String::new(),
                    )
                    .await?;
                    //debug!(post_starttls_extensions_reply);
                }

                let mut ret = {
                    let socket = socket.into_inner()?;
                    let _path = path.clone();

                    socket.set_nonblocking(false)?;
                    let conn = unblock(move || connector.connect(&_path, socket)).await?;
                    /*
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
                        */
                    AsyncWrapper::new(Connection::Tls(conn))?
                };
                ret.write_all(b"EHLO meli.delivery\r\n").await?;
                ret
            }
            SmtpSecurity::None => {
                let addr = lookup_ipv4(path, server_conf.port)?;
                let mut ret = AsyncWrapper::new(Connection::Tcp(TcpStream::connect_timeout(
                    &addr,
                    std::time::Duration::new(4, 0),
                )?))?;
                res.clear();
                let reply = read_lines(
                    &mut ret,
                    &mut res,
                    Some((ReplyCode::_220, &[])),
                    &mut String::new(),
                )
                .await?;
                let code = reply.code;
                let result: Result<ReplyCode> = reply.into();
                result?;
                if code != ReplyCode::_220 {
                    return Err(Error::new(format!(
                        "SMTP Server didn't reply with a 220 greeting: {:?}",
                        Reply::new(&res, code)
                    )));
                }
                ret.write_all(b"EHLO meli.delivery\r\n").await?;
                ret
            }
        };
        let mut ret = Self {
            stream,
            read_buffer: String::new(),
            server_conf: server_conf.clone(),
        };
        let no_auth_needed: bool;
        {
            let pre_auth_extensions_reply = ret
                .read_lines(&mut res, Some((ReplyCode::_250, &[])))
                .await?;
            if ret.server_conf.auth != SmtpAuth::None
                && ret.server_conf.auth.require_auth()
                && !pre_auth_extensions_reply
                    .lines
                    .iter()
                    .any(|l| l.starts_with("AUTH"))
            {
                return Err(Error::new(format!(
                    "SMTP Server doesn't advertise Authentication support. Server response was: \
                     {:?}",
                    pre_auth_extensions_reply
                ))
                .set_kind(crate::error::ErrorKind::Authentication));
            }
            no_auth_needed =
                ret.server_conf.auth == SmtpAuth::None || !ret.server_conf.auth.require_auth();
            if no_auth_needed {
                ret.set_extension_support(pre_auth_extensions_reply);
            } else if let SmtpAuth::Auto {
                ref mut auth_type, ..
            } = ret.server_conf.auth
            {
                if let Some(l) = pre_auth_extensions_reply
                    .lines
                    .iter()
                    .find(|l| l.starts_with("AUTH"))
                {
                    let l = l["AUTH ".len()..].trim();
                    for _type in l.split_whitespace() {
                        if _type == "PLAIN" {
                            auth_type.plain = true;
                        } else if _type == "LOGIN" {
                            auth_type.login = true;
                        }
                    }
                }
            }
        }
        if !no_auth_needed {
            match &ret.server_conf.auth {
                SmtpAuth::None => {}
                SmtpAuth::Auto {
                    username,
                    password,
                    auth_type,
                    ..
                } => {
                    let password = match password {
                        Password::Raw(p) => p.as_bytes().to_vec(),
                        Password::CommandEval(command) => {
                            let _command = command.clone();

                            let mut output = unblock(move || {
                                Command::new("sh")
                                    .args(["-c", &_command])
                                    .stdin(std::process::Stdio::piped())
                                    .stdout(std::process::Stdio::piped())
                                    .stderr(std::process::Stdio::piped())
                                    .output()
                            })
                            .await?;
                            if !output.status.success() {
                                return Err(Error::new(format!(
                                    "SMTP password evaluation command `{}` returned {}: {}",
                                    command,
                                    output.status,
                                    String::from_utf8_lossy(&output.stderr)
                                )));
                            }
                            if output.stdout.ends_with(b"\n") {
                                output.stdout.pop();
                            }
                            output.stdout
                        }
                    };
                    if auth_type.login {
                        let username = username.to_string();
                        ret.send_command(&[b"AUTH LOGIN"]).await?;
                        ret.read_lines(&mut res, Some((ReplyCode::_334, &[])))
                            .await
                            .chain_err_kind(crate::error::ErrorKind::Authentication)?;
                        let buf = base64::encode(&username);
                        ret.send_command(&[buf.as_bytes()]).await?;
                        ret.read_lines(&mut res, Some((ReplyCode::_334, &[])))
                            .await
                            .chain_err_kind(crate::error::ErrorKind::Authentication)?;
                        let buf = base64::encode(&password);
                        ret.send_command(&[buf.as_bytes()]).await?;
                    } else {
                        // # RFC 4616 The PLAIN SASL Mechanism
                        // # https://www.ietf.org/rfc/rfc4616.txt
                        // message   = [authzid] UTF8NUL authcid UTF8NUL passwd
                        // authcid   = 1*SAFE ; MUST accept up to 255 octets
                        // authzid   = 1*SAFE ; MUST accept up to 255 octets
                        // passwd    = 1*SAFE ; MUST accept up to 255 octets
                        // UTF8NUL   = %x00 ; UTF-8 encoded NUL character
                        let username_password = {
                            let mut buf = Vec::with_capacity(2 + username.len() + password.len());
                            buf.push(b'\0');
                            buf.extend(username.as_bytes().to_vec());
                            buf.push(b'\0');
                            buf.extend(password);
                            base64::encode(buf)
                        };
                        ret.send_command(&[b"AUTH PLAIN ", username_password.as_bytes()])
                            .await?;
                    }
                    ret.read_lines(&mut res, Some((ReplyCode::_235, &[])))
                        .await
                        .chain_err_kind(crate::error::ErrorKind::Authentication)?;
                    ret.send_command(&[b"EHLO meli.delivery"]).await?;
                }
                SmtpAuth::XOAuth2 { token_command, .. } => {
                    let password_token = {
                        let _token_command = token_command.clone();
                        let mut output = unblock(move || {
                            Command::new("sh")
                                .args(["-c", &_token_command])
                                .stdin(std::process::Stdio::piped())
                                .stdout(std::process::Stdio::piped())
                                .stderr(std::process::Stdio::piped())
                                .output()
                        })
                        .await?;
                        if !output.status.success() {
                            return Err(Error::new(format!(
                                "SMTP XOAUTH2 token evaluation command `{}` returned {}: {}",
                                &token_command,
                                output.status,
                                String::from_utf8_lossy(&output.stderr)
                            )));
                        }
                        if output.stdout.ends_with(b"\n") {
                            output.stdout.pop();
                        }
                        output.stdout
                    };
                    // https://developers.google.com/gmail/imap/xoauth2-protocol#smtp_protocol_exchange
                    ret.send_command(&[b"AUTH XOAUTH2 ", &password_token])
                        .await?;
                    ret.read_lines(&mut res, Some((ReplyCode::_235, &[])))
                        .await
                        .chain_err_kind(crate::error::ErrorKind::Authentication)?;
                    ret.send_command(&[b"EHLO meli.delivery"]).await?;
                }
            }
            {
                let extensions_reply = ret
                    .read_lines(&mut res, Some((ReplyCode::_250, &[])))
                    .await?;
                ret.set_extension_support(extensions_reply);
            }
        }
        //debug!(&res);
        Ok(ret)
    }

    /// Set a new value for `envelope_from`.
    pub fn set_envelope_from(&mut self, envelope_from: String) {
        self.server_conf.envelope_from = envelope_from;
    }

    fn set_extension_support(&mut self, reply: Reply) {
        debug_assert_eq!(reply.code, ReplyCode::_250);
        self.server_conf.extensions.pipelining &= reply.lines.contains(&"PIPELINING");
        self.server_conf.extensions.chunking &= reply.lines.contains(&"CHUNKING");
        self.server_conf.extensions.prdr &= reply.lines.contains(&"PRDR");
        self.server_conf.extensions._8bitmime &= reply.lines.contains(&"8BITMIME");
        self.server_conf.extensions.binarymime &= reply.lines.contains(&"BINARYMIME");
        self.server_conf.extensions.smtputf8 &= reply.lines.contains(&"SMTPUTF8");
        if !reply.lines.contains(&"DSN") {
            self.server_conf.extensions.dsn_notify = None;
        }
    }

    pub async fn read_lines<'r>(
        &mut self,
        ret: &'r mut String,
        expected_reply_code: Option<(ReplyCode, &[ReplyCode])>,
    ) -> Result<Reply<'r>> {
        read_lines(
            &mut self.stream,
            ret,
            expected_reply_code,
            &mut self.read_buffer,
        )
        .await
    }

    pub async fn send_command(&mut self, command: &[&[u8]]) -> Result<()> {
        //debug!(
        //    "sending command: {}",
        //    command
        //        .iter()
        //        .fold(String::new(), |mut acc, b| {
        //            acc.push_str(unsafe { std::str::from_utf8_unchecked(b) });
        //            acc
        //        })
        //        .trim()
        //);
        for c in command {
            self.stream.write_all(c).await?;
        }
        self.stream.write_all(b"\r\n").await?;
        Ok(())
    }

    /// Sends mail
    pub async fn mail_transaction(&mut self, mail: &str, tos: Option<&[Address]>) -> Result<()> {
        let mut res = String::with_capacity(8 * 1024);
        let mut pipelining_queue: SmallVec<[ExpectedReplyCode; 16]> = SmallVec::new();
        let mut pipelining_results: SmallVec<[Result<ReplyCode>; 16]> = SmallVec::new();
        let mut prdr_results: SmallVec<[Result<ReplyCode>; 16]> = SmallVec::new();
        let dsn_notify = self.server_conf.extensions.dsn_notify.clone();
        let envelope_from = self.server_conf.envelope_from.clone();
        let envelope = Envelope::from_bytes(mail.as_bytes(), None)
            .chain_err_summary(|| "SMTP submission was aborted")?;
        let tos = tos.unwrap_or_else(|| envelope.to());
        if tos.is_empty() && envelope.cc().is_empty() && envelope.bcc().is_empty() {
            return Err(Error::new(
                "SMTP submission was aborted because there was no e-mail address found in the To: \
                 header field. Consider adding recipients.",
            ));
        }
        let mut current_command: SmallVec<[&[u8]; 16]> = SmallVec::new();
        //first step in the procedure is the MAIL command.
        // `MAIL FROM:<reverse-path> [SP <mail-parameters> ] <CRLF>`
        current_command.push(b"MAIL FROM:<");
        if !envelope_from.is_empty() {
            current_command.push(envelope_from.trim().as_bytes());
        } else {
            if envelope.from().is_empty() {
                return Err(Error::new(
                    "SMTP submission was aborted because there was no e-mail address found in the \
                     From: header field. Consider adding a valid value or setting `envelope_from` \
                     in SMTP client settings",
                ));
            } else if envelope.from().len() != 1 {
                return Err(Error::new(
                    "SMTP submission was aborted because there was more than one e-mail address \
                     found in the From: header field. Consider setting `envelope_from` in SMTP \
                     client settings",
                ));
            }
            current_command.push(envelope.from()[0].address_spec_raw().trim());
        }
        current_command.push(b">");
        if self.server_conf.extensions.prdr {
            current_command.push(b" PRDR");
        }
        if self.server_conf.extensions.binarymime {
            current_command.push(b" BODY=BINARYMIME");
        } else if self.server_conf.extensions._8bitmime {
            current_command.push(b" BODY=8BITMIME");
        }
        self.send_command(&current_command).await?;
        current_command.clear();
        if !self.server_conf.extensions.pipelining {
            self.read_lines(&mut res, Some((ReplyCode::_250, &[])))
                .await?;
        } else {
            pipelining_queue.push(Some((ReplyCode::_250, &[])));
        }
        //The second step in the procedure is the RCPT command. This step of the
        // procedure can be repeated any number of times. If accepted, the SMTP
        // server returns a "250 OK" reply. If the mailbox specification is not
        // acceptable for some reason, the server MUST return a reply indicating
        // whether the failure is permanent (i.e., will occur again if
        // the client tries to send the same address again) or temporary (i.e., the
        // address might be accepted if the client tries again later).
        for addr in tos
            .iter()
            .chain(envelope.cc().iter())
            .chain(envelope.bcc().iter())
        {
            current_command.clear();
            current_command.push(b"RCPT TO:<");
            current_command.push(addr.address_spec_raw().trim());
            if let Some(dsn_notify) = dsn_notify.as_ref() {
                current_command.push(b"> NOTIFY=");
                current_command.push(dsn_notify.as_bytes());
            } else {
                current_command.push(b">");
            }
            self.send_command(&current_command).await?;

            //`RCPT TO:<forward-path> [ SP <rcpt-parameters> ] <CRLF>`
            // If accepted, the SMTP server returns a "250 OK" reply and stores the
            // forward-path.
            if !self.server_conf.extensions.pipelining {
                self.read_lines(&mut res, Some((ReplyCode::_250, &[])))
                    .await?;
            } else {
                pipelining_queue.push(Some((ReplyCode::_250, &[])));
            }
        }

        // Since it has been a common source of errors, it is worth noting that spaces
        // are not permitted on either side of the colon following FROM in the
        // MAIL command or TO in the RCPT command. The syntax is exactly as
        // given above.

        if self.server_conf.extensions.binarymime {
            let mail_length = format!("{}", mail.as_bytes().len());
            self.send_command(&[b"BDAT", mail_length.as_bytes(), b"LAST"])
                .await?;
            self.stream.write_all(mail.as_bytes()).await?;
        } else {
            //The third step in the procedure is the DATA command
            //(or some alternative specified in a service extension).
            //DATA `<CRLF>`
            self.send_command(&[b"DATA"]).await?;
            //Client SMTP implementations that employ pipelining MUST check ALL statuses
            // associated with each command in a group. For example, if none of
            // the RCPT TO recipient addresses were accepted the client must
            // then check the response to the DATA command -- the client
            // cannot assume that the DATA command will be rejected just because none of the
            // RCPT TO commands worked. If the DATA command was properly
            // rejected the client SMTP can just issue RSET, but if the DATA
            // command was accepted the client SMTP should send a single dot.
            let mut _all_error = self.server_conf.extensions.pipelining;
            let mut _any_error = false;
            let mut ignore_mailfrom = true;
            for expected_reply_code in pipelining_queue {
                let reply = self.read_lines(&mut res, expected_reply_code).await?;
                if !ignore_mailfrom {
                    _all_error &= reply.code.is_err();
                    _any_error |= reply.code.is_err();
                }
                ignore_mailfrom = false;
                pipelining_results.push(reply.into());
            }

            //If accepted, the SMTP server returns a 354 Intermediate reply and considers
            // all succeeding lines up to but not including the end of mail data
            // indicator to be the message text. When the end of text is
            // successfully received and stored, the SMTP-receiver sends a "250
            // OK" reply.
            self.read_lines(&mut res, Some((ReplyCode::_354, &[])))
                .await?;

            //Before sending a line of mail text, the SMTP client checks the first
            // character of the line.If it is a period, one additional period is
            // inserted at the beginning of the line.
            for line in mail.lines() {
                if line.starts_with('.') {
                    self.stream.write_all(b".").await?;
                }
                self.stream.write_all(line.as_bytes()).await?;
                self.stream.write_all(b"\r\n").await?;
            }

            if !mail.ends_with('\n') {
                self.stream.write_all(b".\r\n").await?;
            }

            //The mail data are terminated by a line containing only a period, that is, the
            // character sequence "`<CRLF>`.`<CRLF>`", where the first `<CRLF>` is
            // actually the terminator of the previous line (see Section 4.5.2).
            // This is the end of mail data indication.
            self.stream.write_all(b".\r\n").await?;
        }

        //The end of mail data indicator also confirms the mail transaction and tells
        // the SMTP server to now process the stored recipients and mail data.
        // If accepted, the SMTP server returns a "250 OK" reply.
        let reply_code = self
            .read_lines(
                &mut res,
                Some((
                    ReplyCode::_250,
                    if self.server_conf.extensions.prdr {
                        &[ReplyCode::_353]
                    } else {
                        &[]
                    },
                )),
            )
            .await?
            .code;
        // PRDR extension only:
        if reply_code == ReplyCode::_353 {
            // Read one line for each accepted recipient.
            for pipe_result in pipelining_results.iter().skip(1) {
                if pipe_result.is_err() {
                    continue;
                }
                prdr_results.push(self.read_lines(&mut res, None).await?.into());
            }
        }
        Ok(())
    }
}

/// Expected reply code in a single or multi-line reply by the server
pub type ExpectedReplyCode = Option<(ReplyCode, &'static [ReplyCode])>;

/// Recognized kinds of SMTP reply codes
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ReplyCode {
    /// System status, or system help reply
    _211,
    /// Help message (Information on how to use the receiver or the meaning of a
    /// particular non-standard command; this reply is useful only to the human
    /// user)
    _214,
    /// `<domain>` Service ready
    _220,
    /// `<domain>` Service closing transmission channel
    _221,
    /// Authentication successful,
    _235,
    /// Requested mail action okay, completed
    _250,
    /// User not local; will forward to `<forward-path>` (See Section 3.4)
    _251,
    /// Cannot VRFY user, but will accept message and attempt delivery (See
    /// Section 3.5.3)
    _252,
    /// rfc4954 AUTH continuation request
    _334,
    /// PRDR specific, eg "content analysis has started|
    _353,
    /// Start mail input; end with `<CRLF>`.`<CRLF>`
    _354,
    /// `<domain>` Service not available, closing transmission channel (This may
    /// be a reply to any command if the service knows it must shut down)
    _421,
    /// Requested mail action not taken: mailbox unavailable (e.g., mailbox busy
    /// or temporarily blocked for policy reasons)
    _450,
    /// Requested action aborted: local error in processing
    _451,
    /// Requested action not taken: insufficient system storage
    _452,
    /// Server unable to accommodate parameters
    _455,
    /// Syntax error, command unrecognized (This may include errors such as
    /// command line too long)
    _500,
    /// Syntax error in parameters or arguments
    _501,
    /// Command not implemented (see Section 4.2.4)
    _502,
    /// Bad sequence of commands
    _503,
    /// Command parameter not implemented
    _504,
    /// Authentication failed
    _535,
    /// Requested action not taken: mailbox unavailable (e.g., mailbox not
    /// found, no access, or command rejected for policy reasons)
    _550,
    /// User not local; please try `<forward-path>` (See Section 3.4)
    _551,
    /// Requested mail action aborted: exceeded storage allocation
    _552,
    /// Requested action not taken: mailbox name not allowed (e.g., mailbox
    /// syntax incorrect)
    _553,
    /// Transaction failed (Or, in the case of a connection-opening response,
    /// "No SMTP service here")
    _554,
    /// MAIL FROM/RCPT TO parameters not recognized or not implemented
    _555,
    /// Must issue a STARTTLS command first
    _530,
}

impl ReplyCode {
    pub const fn as_str(&self) -> &'static str {
        use ReplyCode::*;
        match self {
            _211 => "System status, or system help reply",
            _214 => "Help message",
            _220 => "Service ready",
            _221 => "Service closing transmission channel",
            _250 => "Requested mail action okay, completed",
            _235 => "Authentication successful",
            _251 => "User not local; will forward",
            _252 => "Cannot VRFY user, but will accept message and attempt delivery",
            _334 => "Intermediate response to the AUTH command",
            _353 => "PRDR specific notice",
            _354 => "Start mail input; end with <CRLF>.<CRLF>",
            _421 => "Service not available, closing transmission channel",
            _450 => "Requested mail action not taken: mailbox unavailable",
            _451 => "Requested action aborted: local error in processing",
            _452 => "Requested action not taken: insufficient system storage",
            _455 => "Server unable to accommodate parameters",
            _500 => "Syntax error, command unrecognized",
            _501 => "Syntax error in parameters or arguments",
            _502 => "Command not implemented",
            _503 => "Bad sequence of commands",
            _504 => "Command parameter not implemented",
            _535 => "Authentication failed",
            _550 => {
                "Requested action not taken: mailbox unavailable (e.g., mailbox not found, no \
                 access, or command rejected for policy reasons)"
            }
            _551 => "User not local",
            _552 => "Requested mail action aborted: exceeded storage allocation",
            _553 => {
                "Requested action not taken: mailbox name not allowed (e.g., mailbox syntax \
                 incorrect)"
            }
            _554 => "Transaction failed",
            _555 => "MAIL FROM/RCPT TO parameters not recognized or not implemented",
            _530 => "Must issue a STARTTLS command first",
        }
    }

    pub const fn is_err(&self) -> bool {
        use ReplyCode::*;
        matches!(
            self,
            _421 | _450
                | _451
                | _452
                | _455
                | _500
                | _501
                | _502
                | _503
                | _504
                | _535
                | _550
                | _551
                | _552
                | _553
                | _554
                | _555
                | _530
        )
    }
}

impl TryFrom<&'_ str> for ReplyCode {
    type Error = Error;
    fn try_from(val: &'_ str) -> Result<Self> {
        if val.len() != 3 {
            debug!("{}", val);
        }
        debug_assert!(val.len() == 3);
        use ReplyCode::*;
        match val {
            "211" => Ok(_211),
            "214" => Ok(_214),
            "220" => Ok(_220),
            "221" => Ok(_221),
            "235" => Ok(_235),
            "250" => Ok(_250),
            "251" => Ok(_251),
            "252" => Ok(_252),
            "334" => Ok(_334),
            "354" => Ok(_354),
            "421" => Ok(_421),
            "450" => Ok(_450),
            "451" => Ok(_451),
            "452" => Ok(_452),
            "455" => Ok(_455),
            "500" => Ok(_500),
            "501" => Ok(_501),
            "502" => Ok(_502),
            "503" => Ok(_503),
            "504" => Ok(_504),
            "535" => Ok(_535),
            "550" => Ok(_550),
            "551" => Ok(_551),
            "552" => Ok(_552),
            "553" => Ok(_553),
            "554" => Ok(_554),
            "555" => Ok(_555),
            _ => Err(Error::new(format!("Unknown SMTP reply code: {}", val))),
        }
    }
}

/// A single line or multi-line server reply, along with its reply code
#[derive(Debug, Clone)]
pub struct Reply<'s> {
    pub code: ReplyCode,
    pub lines: SmallVec<[&'s str; 16]>,
}

impl<'s> From<Reply<'s>> for Result<ReplyCode> {
    fn from(val: Reply<'s>) -> Self {
        if val.code.is_err() {
            Err(Error::new(val.lines.join("\n")).set_summary(val.code.as_str()))
        } else {
            Ok(val.code)
        }
    }
}

impl<'s> Reply<'s> {
    /// `s` must be raw SMTP output i.e each line must start with 3 digit reply
    /// code, a space or '-' and end with '\r\n'
    pub fn new(s: &'s str, code: ReplyCode) -> Self {
        let lines: SmallVec<_> = s.lines().map(|l| &l[4..l.len()]).collect();
        Self { lines, code }
    }
}

async fn read_lines<'r>(
    _self: &mut (impl futures::io::AsyncRead + std::marker::Unpin + Send),
    ret: &'r mut String,
    expected_reply_code: Option<(ReplyCode, &[ReplyCode])>,
    buffer: &mut String,
) -> Result<Reply<'r>> {
    let mut buf: [u8; 1024] = [0; 1024];
    ret.clear();
    ret.extend(buffer.drain(..));
    let mut last_line_idx: usize = 0;
    let mut returned_code: Option<ReplyCode> = None;
    'read_loop: loop {
        while let Some(pos) = ret[last_line_idx..].find("\r\n") {
            // "Formally, a reply is defined to be the sequence: a three-digit code, `<SP>`,
            // one line of text, and `<CRLF>`, or a multiline reply (as defined in the same
            // section)."
            if ret[last_line_idx..].len() < 4
                || !ret[last_line_idx..]
                    .chars()
                    .take(3)
                    .all(|c| c.is_ascii_digit())
            {
                return Err(Error::new(format!("Invalid SMTP reply: {}", ret)));
            }
            if let Some(ref returned_code) = returned_code {
                if ReplyCode::try_from(ret[last_line_idx..].get(..3).unwrap())? != *returned_code {
                    buffer.extend(ret.drain(last_line_idx..));
                    if ret.lines().last().unwrap().chars().nth(4).unwrap() != ' ' {
                        return Err(Error::new(format!("Invalid SMTP reply: {}", ret)));
                    }
                    break 'read_loop;
                }
                if ret[last_line_idx + 3..].starts_with(' ') {
                    buffer.extend(ret.drain(last_line_idx + pos + "\r\n".len()..));
                    break 'read_loop;
                }
            } else {
                if ret[last_line_idx + 3..].starts_with(' ') {
                    buffer.extend(ret.drain(last_line_idx + pos + "\r\n".len()..));
                    break 'read_loop;
                }
                returned_code = Some(ReplyCode::try_from(&ret[last_line_idx..3])?);
            }
            last_line_idx += pos + "\r\n".len();
        }
        match _self.read(&mut buf).await {
            Ok(0) => break,
            Ok(b) => {
                ret.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..b]) });
            }
            Err(err) => {
                return Err(Error::from(err));
            }
        }
    }
    if ret.len() < 3 {
        return Err(Error::new(format!("Invalid SMTP reply: {}", ret)));
    }
    let code = ReplyCode::try_from(&ret[..3])?;
    let reply = Reply::new(ret, code);
    //debug!(&reply);
    if expected_reply_code
        .map(|(exp, exp_list)| exp != reply.code && !exp_list.contains(&reply.code))
        .unwrap_or(false)
    {
        let result: Result<ReplyCode> = reply.clone().into();
        result?;
        return Err(Error::new(format!(
            "SMTP Server didn't reply with expected greeting code {:?}: {:?}",
            expected_reply_code.unwrap(),
            reply
        )));
    }
    Ok(reply)
}

#[cfg(test)]
mod test {
    use std::net::IpAddr; //, Ipv4Addr, Ipv6Addr};
    use std::{
        sync::{Arc, Mutex},
        thread,
    };

    use mailin_embedded::{
        response::{INTERNAL_ERROR, OK},
        Handler, Response, Server, SslConfig,
    };

    use super::*;

    const ADDRESS: &str = "127.0.0.1:8825";
    #[derive(Debug, Clone)]
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

    #[derive(Debug, Clone)]
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
            hostname: "127.0.0.1".into(),
            port: 8825,
            envelope_from: "foo-chat@example.com".into(),
            auth: SmtpAuth::None,
            security: SmtpSecurity::None,
            extensions: Default::default(),
        }
    }

    #[test]
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
                .with_name("example.com")
                .with_ssl(SslConfig::None)
                .unwrap()
                .with_addr(ADDRESS)
                .unwrap();
            eprintln!("Running smtp server at {}", ADDRESS);
            server.serve().expect("Could not run server");
        });

        let smtp_server_conf = get_smtp_conf();
        let input_str = include_str!("../tests/test_sample_longmessage.eml");
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
        assert_eq!(handler.stored.lock().unwrap().len(), 2);
    }
}
