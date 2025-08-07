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

#[cfg(feature = "imap")]
use rusty_fork::rusty_fork_test;

#[cfg(feature = "imap")]
rusty_fork_test! {
    #[test]
    fn test_imap_watch() {
        tests::run_imap_watch();
    }
}

#[cfg(feature = "imap")]
pub mod server {
    use std::{
        convert::TryInto,
        net::{TcpListener, TcpStream},
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::{UnboundedReceiver, UnboundedSender},
        executor::block_on,
        future::{self, Either},
        io::{AsyncReadExt, AsyncWriteExt},
        pin_mut, Future, StreamExt,
    };
    use imap_codec::{
        encode::{Encoder, Fragment},
        imap_types, ResponseCodec,
    };
    use imap_types::{
        core::{LiteralMode, NString},
        fetch::MessageDataItem,
        response::{Data, Response},
    };
    use melib::{backends::prelude::*, imap::*, smol::Async, Mail};

    pub enum SessionState {
        // Unauthenticated,
        Authenticated,
        SelectedMailbox,
    }

    /// Server state with only one mailbox (INBOX).
    pub struct ServerState {
        pub envelopes: IndexMap<UID, Mail>,
        pub next_uid: UID,
        pub uidvalidity: UID,
    }

    impl ServerState {
        pub fn insert(&mut self, new: Box<Mail>) -> UID {
            let uid = self.next_uid;
            self.envelopes.insert(uid, *new);
            self.next_uid += 1;
            uid
        }
    }

    trait AsImapResponseItem {
        fn as_envelope(&'_ self) -> imap_types::envelope::Envelope<'_>;
        fn as_flags(&'_ self) -> Vec<imap_types::flag::FlagFetch<'_>>;
        fn as_bodystructure(&'_ self) -> imap_types::body::BodyStructure<'_>;
        fn as_body_peek_references(&self) -> imap_types::fetch::MessageDataItem<'_>;
    }

    impl AsImapResponseItem for Mail {
        fn as_envelope(&'_ self) -> imap_types::envelope::Envelope<'_> {
            macro_rules! address {
                ($a:expr) => {{
                    imap_types::envelope::Address {
                        name: $a.display_name().to_string().try_into().unwrap(),
                        adl: NString(None),
                        mailbox: String::from_utf8_lossy($a.address_spec_raw())
                            .split_once('@')
                            .unwrap()
                            .0
                            .to_string()
                            .try_into()
                            .unwrap(),
                        host: String::from_utf8_lossy($a.address_spec_raw())
                            .split_once('@')
                            .unwrap()
                            .1
                            .to_string()
                            .try_into()
                            .unwrap(),
                    }
                }};
            }
            imap_types::envelope::Envelope {
                date: self.date_as_str().try_into().unwrap(),
                subject: self.subject().as_ref().to_string().try_into().unwrap(),
                from: self.from().iter().map(|a| address! {a}).collect(),
                sender: self.from().iter().map(|a| address! {a}).collect(),
                reply_to: vec![],
                to: self.to().iter().map(|a| address! {a}).collect(),
                cc: self.cc().iter().map(|a| address! {a}).collect(),
                bcc: self.bcc().iter().map(|a| address! {a}).collect(),
                in_reply_to: NString(None),
                message_id: self.message_id().to_string().try_into().unwrap(),
            }
        }

        fn as_flags(&'_ self) -> Vec<imap_types::flag::FlagFetch<'_>> {
            let flags: Vec<imap_types::flag::Flag<'static>> = self.flags().into();
            flags
                .into_iter()
                .map(imap_types::flag::FlagFetch::Flag)
                .collect()
        }

        fn as_bodystructure(&'_ self) -> imap_types::body::BodyStructure<'_> {
            imap_types::body::BodyStructure::Single {
                body: imap_types::body::Body {
                    basic: imap_types::body::BasicFields {
                        parameter_list: vec![],
                        id: NString(None),
                        description: NString(None),
                        content_transfer_encoding: "7BIT".try_into().unwrap(),
                        size: self.bytes.len() as u32,
                    },
                    specific: imap_types::body::SpecificFields::Text {
                        subtype: "plain".try_into().unwrap(),
                        number_of_lines: 1,
                    },
                },
                extension_data: None,
            }
        }

        fn as_body_peek_references(&self) -> imap_types::fetch::MessageDataItem<'_> {
            imap_types::fetch::MessageDataItem::BodyExt {
                section: Some(imap_types::fetch::Section::HeaderFields(
                    None,
                    vec!["REFERENCES".try_into().unwrap()].try_into().unwrap(),
                )),
                origin: None,
                data: self
                    .other_headers()
                    .get(HeaderName::REFERENCES)
                    .map(|s| s.to_string().try_into().unwrap())
                    .unwrap_or(NString(None)),
            }
        }
    }

    #[derive(Debug)]
    pub enum ServerEvent {
        New(Box<Mail>),
        Delete(UID),
        Quit,
    }

    pub struct ImapServerStream {
        pub tcp_stream: Async<TcpStream>,
        pub command_receiver: UnboundedReceiver<ServerEvent>,
        pub command_sender: UnboundedSender<ServerEvent>,
        pub state: Arc<Mutex<ServerState>>,
        pub session_state: SessionState,
        pub buf: Vec<u8>,
    }

    impl ImapServerStream {
        pub fn new<T: 'static, F: Future<Output = T> + std::marker::Unpin>(
            listener: &Async<TcpListener>,
            fut: F,
            (command_sender, command_receiver): (
                UnboundedSender<ServerEvent>,
                UnboundedReceiver<ServerEvent>,
            ),
            state: Arc<Mutex<ServerState>>,
        ) -> Self {
            let mut buf = vec![0; 64 * 1024];
            let tcp_stream = {
                let (mut tcp_stream, next_fut) = {
                    let accept_fut = listener.accept();
                    pin_mut!(accept_fut);

                    match block_on(future::select(fut, accept_fut)) {
                        Either::Left((_, _)) => {
                            unreachable!();
                        }
                        Either::Right((value2, fut)) => (value2.unwrap().0, fut),
                    }
                };
                {
                    let read_fut = tcp_stream.read(&mut buf);
                    pin_mut!(read_fut);
                    let next_fut = match block_on(future::select(next_fut, read_fut)) {
                        Either::Left((_, _)) => {
                            unreachable!();
                        }
                        Either::Right((value2, fut)) => {
                            let read_bytes = value2.unwrap();
                            assert_eq!(&buf[..read_bytes], b"M1 CAPABILITY\r\n");
                            fut
                        }
                    };
                    block_on(tcp_stream.write_all(
                            b"* CAPABILITY IMAP4rev1 AUTH=PLAIN SASL-IR\r\nM1 OK CAPABILITY completed\r\n",
                        ))
                        .unwrap();
                    let read_fut = tcp_stream.read(&mut buf);
                    pin_mut!(read_fut);
                    let next_fut = match block_on(future::select(next_fut, read_fut)) {
                        Either::Left((_, _)) => {
                            unreachable!();
                        }
                        Either::Right((value2, fut)) => {
                            let read_bytes = value2.unwrap();
                            assert_eq!(
                                &buf[..read_bytes],
                                b"M2 AUTHENTICATE PLAIN AHVzZXIAcGFzc3dvcmQ=\r\n"
                            );
                            fut
                        }
                    };
                    block_on(tcp_stream.write_all(b"M2 OK Success\r\n")).unwrap();
                    let read_fut = tcp_stream.read(&mut buf);
                    pin_mut!(read_fut);
                    match block_on(future::select(next_fut, read_fut)) {
                        Either::Left((_, _)) => {
                            unreachable!();
                        }
                        Either::Right((value2, _)) => {
                            let read_bytes = value2.unwrap();
                            assert_eq!(&buf[..read_bytes], b"M3 CAPABILITY\r\n");
                        }
                    };
                    block_on(
                        tcp_stream.write_all(
                            b"* CAPABILITY IMAP4rev1 ID IDLE ENABLE\r\nM3 OK Success\r\n",
                        ),
                    )
                    .unwrap();
                    tcp_stream
                }
            };
            Self {
                tcp_stream,
                command_receiver,
                command_sender,
                state,
                session_state: SessionState::Authenticated,
                buf,
            }
        }

        pub async fn loop_handler(self, name: &'static str) {
            let Self {
                mut tcp_stream,
                mut command_receiver,
                command_sender,
                state,
                mut session_state,
                mut buf,
            } = self;
            let mut buf_cursor = 0;
            'outer: loop {
                let idle_cmd_id = 'main: loop {
                    let read_bytes = tcp_stream.read(&mut buf[buf_cursor..]).await.unwrap();
                    let input = String::from_utf8_lossy(&buf[..(buf_cursor + read_bytes)]);
                    if input.is_empty() {
                        // peek into command receiver for a Quit event
                        let command = command_receiver.try_next().unwrap();
                        let Some(command) = command else {
                            continue 'main;
                        };
                        if matches!(command, ServerEvent::Quit) {
                            tcp_stream.write_all(b"* BYE world\r\n").await.unwrap();
                            tcp_stream.flush().await.unwrap();
                            break 'outer;
                        }
                        command_sender.unbounded_send(command).unwrap();
                    }
                    eprintln!("{name} loop_handler 'main received: {input:?}");
                    if !input.ends_with("\r\n") {
                        buf_cursor += read_bytes;
                        continue 'main;
                    }
                    buf_cursor = 0;
                    let (id, cmd) = input.split_once(' ').unwrap();
                    match cmd {
                        "IDLE\r\n" => {
                            break 'main id.to_string();
                        }
                        "NOOP\r\n" => {
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK NOOP completed\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "LOGOUT\r\n" => {
                            tcp_stream.write_all(b"* BYE world\r\n").await.unwrap();
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK LOGOUT completed\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                            break 'outer;
                        }
                        "LIST \"\" *\r\n" => {
                            tcp_stream
                                .write_all(b"* LIST () \"/\" \"inbox\"\r\n")
                                .await
                                .unwrap();
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK LIST completed\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "LSUB \"\" *\r\n" => {
                            tcp_stream
                                .write_all(b"* LSUB () \".\" \"inbox\"\r\n")
                                .await
                                .unwrap();
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK LSUB completed\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "EXAMINE INBOX\r\n" => {
                            session_state = SessionState::SelectedMailbox;
                            let (exists, recent, uidvalidity) = {
                                let state_lck = state.lock().unwrap();
                                let uidvalidity = state_lck.uidvalidity;
                                let exists = state_lck.envelopes.len();
                                let recent = 0;
                                (exists, recent, uidvalidity)
                            };
                            tcp_stream
                                .write_all(
                                    format!(
                                        "* {exists} EXISTS\r\n* {recent} RECENT\r\n* OK \
                                         [UIDVALIDITY {uidvalidity}] UIDs valid\r\n* FLAGS \
                                         (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK \
                                         [PERMANENTFLAGS ()] No permanent flags permitted\r\n{id} \
                                         OK [READ-ONLY] EXAMINE completed\r\n"
                                    )
                                    .as_bytes(),
                                )
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "SELECT INBOX\r\n" => {
                            session_state = SessionState::SelectedMailbox;
                            let (exists, recent, uidvalidity) = {
                                let state_lck = state.lock().unwrap();
                                let uidvalidity = state_lck.uidvalidity;
                                let exists = state_lck.envelopes.len();
                                let recent = 0;
                                (exists, recent, uidvalidity)
                            };
                            tcp_stream
                                .write_all(
                                    format!(
                                        "* {exists} EXISTS\r\n* {recent} RECENT\r\n* OK \
                                         [UIDVALIDITY {uidvalidity}] UIDs valid\r\n* FLAGS \
                                         (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK \
                                         [PERMANENTFLAGS ()] No permanent flags permitted\r\n{id} \
                                         OK SELECT completed\r\n"
                                    )
                                    .as_bytes(),
                                )
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "UNSELECT\r\n" => {
                            if !matches!(session_state, SessionState::SelectedMailbox) {
                                tcp_stream.write_all(id.as_bytes()).await.unwrap();
                                tcp_stream
                                    .write_all(b" BAD no mailbox is selected\r\n")
                                    .await
                                    .unwrap();
                                tcp_stream.flush().await.unwrap();
                                continue 'main;
                            }
                            session_state = SessionState::Authenticated;
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK UNSELECT succeeded\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "CLOSE\r\n" => {
                            session_state = SessionState::Authenticated;
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK CLOSE succeeded\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "EXPUNGE\r\n" => {
                            if !matches!(session_state, SessionState::SelectedMailbox) {
                                tcp_stream.write_all(id.as_bytes()).await.unwrap();
                                tcp_stream
                                    .write_all(b" BAD no mailbox is selected\r\n")
                                    .await
                                    .unwrap();
                                tcp_stream.flush().await.unwrap();
                                continue 'main;
                            }
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK EXPUNGE succeeded\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "UID SEARCH 1:*\r\n" => {
                            if !matches!(session_state, SessionState::SelectedMailbox) {
                                tcp_stream.write_all(id.as_bytes()).await.unwrap();
                                tcp_stream
                                    .write_all(b" BAD no mailbox is selected\r\n")
                                    .await
                                    .unwrap();
                                tcp_stream.flush().await.unwrap();
                                continue 'main;
                            }
                            let uids = state
                                .lock()
                                .unwrap()
                                .envelopes
                                .iter()
                                .map(|(u, _)| (*u))
                                .collect::<Vec<_>>();
                            if uids.is_empty() {
                                tcp_stream.write_all(b"* SEARCH\r\n").await.unwrap();
                            } else {
                                tcp_stream.write_all(b"* SEARCH ").await.unwrap();
                                for uid in uids {
                                    tcp_stream
                                        .write_all(format!("{uid}").as_bytes())
                                        .await
                                        .unwrap();
                                }
                                tcp_stream.write_all(b"\r\n").await.unwrap();
                            }
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK SEARCH completed\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        "SEARCH UNSEEN\r\n" => {
                            if !matches!(session_state, SessionState::SelectedMailbox) {
                                tcp_stream.write_all(id.as_bytes()).await.unwrap();
                                tcp_stream
                                    .write_all(b" BAD no mailbox is selected\r\n")
                                    .await
                                    .unwrap();
                                tcp_stream.flush().await.unwrap();
                                continue 'main;
                            }
                            let msns = state
                                .lock()
                                .unwrap()
                                .envelopes
                                .values()
                                .enumerate()
                                .filter(|(_, env)| !env.is_seen())
                                .map(|(i, _)| i + 1)
                                .collect::<Vec<_>>();
                            if msns.is_empty() {
                                tcp_stream.write_all(b"* SEARCH\r\n").await.unwrap();
                            } else {
                                tcp_stream.write_all(b"* SEARCH ").await.unwrap();
                                for msn in msns {
                                    tcp_stream
                                        .write_all(format!("{msn}").as_bytes())
                                        .await
                                        .unwrap();
                                }
                                tcp_stream.write_all(b"\r\n").await.unwrap();
                            }
                            tcp_stream.write_all(id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK SEARCH completed\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        fetch
                            if fetch.starts_with("FETCH ")
                                && fetch.ends_with(
                                    " (UID FLAGS ENVELOPE BODY.PEEK[HEADER.FIELDS (REFERENCES)] \
                                     BODYSTRUCTURE)\r\n",
                                ) =>
                        {
                            if !matches!(session_state, SessionState::SelectedMailbox) {
                                tcp_stream.write_all(id.as_bytes()).await.unwrap();
                                tcp_stream
                                    .write_all(b" BAD no mailbox is selected\r\n")
                                    .await
                                    .unwrap();
                                tcp_stream.flush().await.unwrap();
                                continue 'main;
                            }
                            let msn_to_fetch = fetch
                                .strip_prefix("FETCH ")
                                .unwrap()
                                .strip_suffix(
                                    " (UID FLAGS ENVELOPE BODY.PEEK[HEADER.FIELDS (REFERENCES)] \
                                     BODYSTRUCTURE)\r\n",
                                )
                                .unwrap()
                                .parse::<usize>()
                                .unwrap();
                            eprintln!("{name} loop_handler got FETCH for msn {msn_to_fetch}");
                            let (uid, mail) = state
                                .lock()
                                .unwrap()
                                .envelopes
                                .get_index(msn_to_fetch.saturating_sub(1))
                                .map(|(u, m)| (*u, m.clone()))
                                .unwrap();
                            let references = mail.as_body_peek_references();
                            let response = Response::Data(Data::Fetch {
                                seq: (msn_to_fetch as u32).try_into().unwrap(),
                                items: vec![
                                    MessageDataItem::Uid((uid as u32).try_into().unwrap()),
                                    MessageDataItem::Flags(mail.as_flags()),
                                    MessageDataItem::Envelope(mail.as_envelope()),
                                    MessageDataItem::BodyStructure(mail.as_bodystructure()),
                                    references,
                                ]
                                .try_into()
                                .unwrap(),
                            });
                            eprintln!(
                                "fragment raw: {:?}",
                                String::from_utf8_lossy(
                                    &ResponseCodec::new().encode(&response).dump()
                                )
                            );
                            for fragment in ResponseCodec::new().encode(&response) {
                                match fragment {
                                    Fragment::Line { data } => {
                                        tcp_stream.write_all(&data).await.unwrap();
                                        tcp_stream.flush().await.unwrap();
                                    }
                                    Fragment::Literal { data, mode } => match mode {
                                        LiteralMode::Sync => {
                                            // Wait for a continuation request.
                                            todo!()
                                        }
                                        LiteralMode::NonSync => {
                                            // We don't need to wait for a continuation request
                                            // as the server will also not send it.
                                            tcp_stream.write_all(&data).await.unwrap();
                                            tcp_stream.flush().await.unwrap();
                                        }
                                    },
                                }
                            }
                            tcp_stream
                                .write_all(format!("{id} OK FETCH completed\r\n").as_bytes())
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
                        other => panic!("Unexpected cmd: {id} {other:?}"),
                    }
                };
                eprintln!("{name} loop_handler is now idling");
                tcp_stream.write_all(b"+ idling\r\n").await.unwrap();
                tcp_stream.flush().await.unwrap();
                'idle: loop {
                    let read_fut = tcp_stream.read(&mut buf[buf_cursor..]);
                    pin_mut!(read_fut);
                    let read_bytes = match future::select(read_fut, command_receiver.next()).await {
                        Either::Left((value1, _)) => value1.unwrap(),
                        Either::Right((value2, _)) => {
                            match value2.unwrap() {
                                ServerEvent::New(new_mail) => {
                                    let exists_msn = {
                                        let mut state_lck = state.lock().unwrap();
                                        let new_uid = state_lck.insert(new_mail);
                                        let msn = state_lck.envelopes.len();
                                        eprintln!("{name} EXISTS uid = {new_uid} msn = {msn}");
                                        msn
                                    };
                                    tcp_stream
                                        .write_all(format!("* {exists_msn} EXISTS\r\n").as_bytes())
                                        .await
                                        .unwrap();
                                    tcp_stream.flush().await.unwrap();
                                }
                                ServerEvent::Delete(uid) => {
                                    let msn = {
                                        let mut state_lck = state.lock().unwrap();
                                        let msn =
                                            state_lck.envelopes.get_index_of(&uid).unwrap() + 1;
                                        eprintln!(
                                            "{name} removing msn = {} uid = {} mail = {:?}",
                                            msn,
                                            uid,
                                            state_lck.envelopes.shift_remove(&uid)
                                        );
                                        msn
                                    };
                                    tcp_stream
                                        .write_all(format!("* {msn} EXPUNGE\r\n").as_bytes())
                                        .await
                                        .unwrap();
                                    tcp_stream.flush().await.unwrap();
                                }
                                ServerEvent::Quit => {
                                    tcp_stream.write_all(b"* BYE world\r\n").await.unwrap();
                                    tcp_stream.write_all(idle_cmd_id.as_bytes()).await.unwrap();
                                    tcp_stream
                                        .write_all(b" OK IDLE terminated\r\n")
                                        .await
                                        .unwrap();
                                    tcp_stream.flush().await.unwrap();
                                    break 'outer;
                                }
                            }
                            continue 'idle;
                        }
                    };
                    let input = String::from_utf8_lossy(&buf[..(buf_cursor + read_bytes)]);
                    eprintln!("{name} loop_handler 'idle received: {input:?}");
                    if !input.ends_with("\r\n") {
                        buf_cursor += read_bytes;
                        continue 'idle;
                    }
                    buf_cursor = 0;
                    if input == "DONE\r\n" {
                        tcp_stream.write_all(idle_cmd_id.as_bytes()).await.unwrap();
                        tcp_stream
                            .write_all(b" OK IDLE terminated\r\n")
                            .await
                            .unwrap();
                        continue 'outer;
                    }
                }
            }
        }
    }
}

#[cfg(feature = "imap")]
mod tests {
    use std::{
        net::TcpListener,
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::unbounded,
        executor::block_on,
        future::{self, Either},
        pin_mut, StreamExt,
    };
    use melib::{
        backends::prelude::*,
        imap::*,
        utils::logging::{LogLevel, StderrLogger},
        Mail,
    };
    use tempfile::TempDir;

    use super::server::*;

    /// Test that `ImapType::watch` `Stream` returns the expected `Refresh`
    /// events when altering the mail store in the IMAP server.
    pub(crate) fn run_imap_watch() {
        let mut _logger = StderrLogger::new(LogLevel::TRACE);
        let temp_dir = TempDir::new().unwrap();
        let backend_event_queue =
            Arc::new(Mutex::new(std::collections::VecDeque::with_capacity(16)));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                if matches!(be, BackendEvent::Refresh(_)) {
                    panic!("Unexpected Refresh event received via BackendEventConsumer");
                }
                eprintln!("BackendEventConsumer: ah {ah:?} be {be:?}");
                backend_event_queue.lock().unwrap().push_back((ah, be));
            }))
        };

        for var in [
            "HOME",
            "XDG_CACHE_HOME",
            "XDG_STATE_HOME",
            "XDG_CONFIG_DIRS",
            "XDG_CONFIG_HOME",
            "XDG_DATA_DIRS",
            "XDG_DATA_HOME",
        ] {
            std::env::remove_var(var);
        }
        for (var, dir) in [
            ("HOME", temp_dir.path().to_path_buf()),
            ("XDG_CACHE_HOME", temp_dir.path().join(".cache")),
            ("XDG_STATE_HOME", temp_dir.path().join(".local/state")),
            ("XDG_CONFIG_HOME", temp_dir.path().join(".config")),
            ("XDG_DATA_HOME", temp_dir.path().join(".local/share")),
        ] {
            std::fs::create_dir_all(&dir).unwrap_or_else(|err| {
                panic!("Could not create {} path, {}: {}", var, dir.display(), err);
            });
            std::env::set_var(var, &dir);
        }

        let server_state = Arc::new(Mutex::new(ServerState {
            envelopes: indexmap::indexmap! {},
            next_uid: 1,
            uidvalidity: 1,
        }));

        let listener = TcpListener::bind(("0.0.0.0", 0)).unwrap();
        let local_addr = listener.local_addr().unwrap();
        let account_conf = AccountSettings {
            name: "test".to_string(),
            root_mailbox: "INBOX".to_string(),
            format: "imap".to_string(),
            identity: "user@example.com".to_string(),
            extra_identities: vec![],
            read_only: false,
            display_name: None,
            order: Default::default(),
            subscribed_mailboxes: vec![],
            mailboxes: indexmap::indexmap! {},
            manual_refresh: false,
            extra: indexmap::indexmap! {
                "server_hostname".to_string() => local_addr.ip().to_string(),
                "server_username".to_string() => "user".to_string(),
                "server_password".to_string() => "password".to_string(),
                "server_port".to_string() => local_addr.port().to_string(),
                "use_starttls".to_string() => "false".to_string(),
                "use_tls".to_string() => "false".to_string(),
                // Important for testing, because we expect only one connection to be used.
                "use_connection_pool".to_string() => "false".to_string(),
            },
        };

        let imap =
            ImapType::new(&account_conf, Default::default(), backend_event_consumer).unwrap();
        let listener = smol::Async::new(listener).unwrap();
        let mut is_online_fut = imap.is_online().unwrap();
        let (main_conn_sender, main_conn_receiver) = unbounded();
        let main_conn = ImapServerStream::new(
            &listener,
            &mut is_online_fut,
            (main_conn_sender.clone(), main_conn_receiver),
            Arc::clone(&server_state),
        );
        block_on(is_online_fut).unwrap();
        let mut mailboxes_fut = imap.mailboxes().unwrap();
        let mut main_conn_loop = Box::pin(main_conn.loop_handler("main"));
        let _mailboxes = match block_on(future::select(
            mailboxes_fut.as_mut(),
            main_conn_loop.as_mut(),
        )) {
            Either::Left((value1, _)) => value1.unwrap(),
            Either::Right((value2, _)) => {
                unreachable!("{:?}", value2);
            }
        };

        let mut watch_fut = imap.watch().unwrap().into_future();
        let (watch_conn_sender, watch_conn_receiver) = unbounded();
        let watch_conn = ImapServerStream::new(
            &listener,
            &mut watch_fut,
            (watch_conn_sender.clone(), watch_conn_receiver),
            Arc::clone(&server_state),
        );
        let new_mail = Box::new(
            Mail::new(
                br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#
                .to_vec(),
                None,
            )
            .unwrap(),
        );
        watch_conn_sender
            .unbounded_send(ServerEvent::New(new_mail))
            .unwrap();
        let watch_conn_loop = watch_conn.loop_handler("watch");
        let loops = loops_fut(main_conn_loop, watch_conn_loop);
        std::thread::spawn(move || {
            block_on(loops);
        });
        let hash;
        let watch_fut = {
            let (value1, rest) = block_on(watch_fut);
            let backend_event = value1.unwrap().unwrap();
            let BackendEvent::Refresh(refresh_event) = backend_event else {
                panic!("Expected Refresh event, got: {backend_event:?}");
            };
            let RefreshEventKind::Create(ref env) = refresh_event.kind else {
                panic!("Expected Create event, got: {refresh_event:?}");
            };
            assert_eq!(env.subject(), "RE: your e-mail");
            assert_eq!(env.message_id(), "h2g7f.z0gy2pgaen5m@example.com");
            hash = env.hash();
            let uid = {
                let state_lck = server_state.lock().unwrap();
                state_lck
                    .envelopes
                    .iter()
                    .find_map(|(uid, env)| {
                        if env.message_id() == "h2g7f.z0gy2pgaen5m@example.com" {
                            Some(*uid)
                        } else {
                            None
                        }
                    })
                    .unwrap()
            };
            watch_conn_sender
                .unbounded_send(ServerEvent::Delete(uid))
                .unwrap();
            rest.into_future()
        };
        {
            let mailbox = block_on(imap.uid_store.mailboxes.lock());
            let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
            assert_eq!(exists_lck.len(), 1);
            let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
            assert_eq!(unseen_lck.len(), 1);
        }
        let watch_fut = {
            let (value1, rest) = block_on(watch_fut);
            let backend_event = value1.unwrap().unwrap();
            let BackendEvent::Refresh(refresh_event) = backend_event else {
                panic!("Expected Refresh event, got: {backend_event:?}");
            };
            let RefreshEventKind::Remove(ref env_hash) = refresh_event.kind else {
                panic!("Expected Remove event, got: {refresh_event:?}");
            };
            assert_eq!(*env_hash, hash);
            rest.into_future()
        };
        {
            let mailbox = block_on(imap.uid_store.mailboxes.lock());
            let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
            assert_eq!(exists_lck.len(), 0);
            let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
            assert_eq!(unseen_lck.len(), 0);
        }
        watch_conn_sender.unbounded_send(ServerEvent::Quit).unwrap();
        main_conn_sender.unbounded_send(ServerEvent::Quit).unwrap();
        let (value1, _rest) = block_on(watch_fut);
        if let Some(val) = value1 {
            if !matches!(val, Err(ref err) if err.kind == ErrorKind::OSError(nix::errno::Errno::EPIPE))
            {
                panic!(
                    "Expected watch TCP connection to have disconnected with EPIPE, got: {val:?}"
                );
            }
        }
    }

    async fn loops_fut(
        main_conn_loop: impl futures::Future<Output = ()>,
        watch_conn_loop: impl futures::Future<Output = ()>,
    ) {
        pin_mut!(main_conn_loop);
        pin_mut!(watch_conn_loop);
        match future::select(main_conn_loop.as_mut(), watch_conn_loop.as_mut()).await {
            Either::Left((_, watch_conn)) => {
                eprintln!("loops fut loop finished with main_conn",);
                watch_conn.await;
                eprintln!("loops fut loop finished with watch_conn",);
            }
            Either::Right((_, main_conn)) => {
                eprintln!("loops fut loop finished with watch_conn",);
                main_conn.await;
                eprintln!("loops fut loop finished with main_conn",);
            }
        }
    }
}
