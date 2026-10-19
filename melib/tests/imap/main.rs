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

#![cfg(feature = "imap")]

use rusty_fork::rusty_fork_test;

rusty_fork_test! {
    #[test]
    fn test_imap_watch() {
        tests::run_imap_watch();
    }

    #[test]
    fn test_imap_fetch() {
        tests::run_imap_fetch();
    }

    #[test]
    fn test_imap_auth_mechanisms() {
        tests::run_imap_auth_mechanisms();
    }

    #[test]
    fn test_imap_tls() {
        tests::run_imap_tls();
    }
}

pub mod server {
    use std::{
        collections::{HashSet, VecDeque},
        convert::TryInto as _,
        net::{TcpListener, TcpStream},
        num::NonZeroU32,
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::{unbounded, UnboundedReceiver, UnboundedSender},
        executor::block_on,
        future::{self, Either},
        io::{AsyncReadExt as _, AsyncWriteExt as _},
        stream::{FuturesUnordered, StreamExt as _},
        FutureExt as _,
    };
    use imap_codec::{
        decode::Decoder as _,
        encode::{Encoder as _, Fragment},
        imap_types, CommandCodec, ResponseCodec,
    };
    use imap_types::IntoStatic as _;
    pub use imap_types::{
        auth::{AuthMechanism, AuthMechanismOther},
        core::{Atom, LiteralMode, NString, Tag, Vec1},
        fetch::MessageDataItem,
        response::{Capability, Code, CommandContinuationRequest, Data, Response, Status},
    };
    use melib::{
        backends::prelude::*, imap::*, parser::BytesExt as _, utils::connections::Connection, Mail,
    };
    use smol::Async;

    #[derive(Debug)]
    pub enum SessionState {
        Unauthenticated,
        Authenticated,
        SelectedMailbox,
        ExaminedMailbox,
    }

    impl SessionState {
        #[inline(always)]
        pub const fn is_authenticated(&self) -> bool {
            !matches!(self, Self::Unauthenticated)
        }

        #[inline(always)]
        pub const fn is_selected(&self) -> bool {
            matches!(self, Self::SelectedMailbox | Self::ExaminedMailbox)
        }
    }

    #[derive(Debug)]
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

        fn recv(&mut self, server_event: ServerStateEvent) -> StreamEvent {
            match server_event {
                ServerStateEvent::New(new_mail) => {
                    let uid = self.insert(new_mail);
                    StreamEvent::Untagged(UntaggedEvent::New(uid))
                }
                ServerStateEvent::Delete(uid, on_success) => {
                    log::debug!(
                        "removing uid = {uid} mail = {:?}",
                        self.envelopes.shift_remove(&uid)
                    );
                    _ = on_success.send(true);
                    StreamEvent::Untagged(UntaggedEvent::Delete(uid))
                }
                ServerStateEvent::Expunge(reply) => {
                    let uids = self
                        .envelopes
                        .iter()
                        .filter_map(|(uid, env)| {
                            if env.flags.is_trashed() {
                                Some(*uid)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<UID>>();
                    for uid in &uids {
                        log::debug!(
                            "removing uid = {uid} mail = {:?}",
                            self.envelopes.shift_remove(uid)
                        );
                    }
                    _ = reply.send(uids.clone());

                    StreamEvent::Untagged(UntaggedEvent::Deletes(uids))
                }
                ServerStateEvent::WaitForCommand(cmd, notifier) => {
                    StreamEvent::WaitForCommand(cmd, notifier)
                }
            }
        }
    }

    trait AsImapResponseItem {
        fn as_envelope(&self) -> imap_types::envelope::Envelope<'static>;
        fn as_flags(&self) -> Vec<imap_types::flag::FlagFetch<'static>>;
        fn as_bodystructure(&self) -> imap_types::body::BodyStructure<'static>;
        fn as_body_peek_references(&self) -> imap_types::fetch::MessageDataItem<'static>;
    }

    impl AsImapResponseItem for Mail {
        fn as_envelope(&self) -> imap_types::envelope::Envelope<'static> {
            macro_rules! address {
                ($a:expr) => {{
                    imap_types::envelope::Address {
                        name: $a.display_name().to_string().try_into().unwrap(),
                        adl: NString(None),
                        mailbox: $a
                            .get_email()
                            .split_once('@')
                            .unwrap()
                            .0
                            .to_string()
                            .try_into()
                            .unwrap(),
                        host: $a
                            .get_email()
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
                date: self.date_as_str().to_string().try_into().unwrap(),
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

        fn as_flags(&self) -> Vec<imap_types::flag::FlagFetch<'static>> {
            let flags: Vec<imap_types::flag::Flag<'static>> = self.flags().into();
            flags
                .into_iter()
                .map(imap_types::flag::FlagFetch::Flag)
                .collect()
        }

        fn as_bodystructure(&self) -> imap_types::body::BodyStructure<'static> {
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

        fn as_body_peek_references(&self) -> imap_types::fetch::MessageDataItem<'static> {
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
        State(ServerStateEvent),
        ChangeCapabilities {
            capabilities: Vec1<Capability<'static>>,
            authenticated_capabilities: Vec1<Capability<'static>>,
        },
        SendCapabilitiesOnAuth {
            send_capabilities_on_auth: bool,
        },
        Quit,
    }

    impl From<ServerStateEvent> for ServerEvent {
        fn from(val: ServerStateEvent) -> Self {
            Self::State(val)
        }
    }

    #[derive(Debug)]
    pub enum ServerStateEvent {
        New(Box<Mail>),
        Delete(UID, futures::channel::oneshot::Sender<bool>),
        Expunge(futures::channel::oneshot::Sender<Vec<UID>>),
        WaitForCommand(
            &'static str,
            Arc<Mutex<Option<futures::channel::oneshot::Sender<()>>>>,
        ),
    }

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    enum UntaggedEvent {
        New(UID),
        Delete(UID),
        Deletes(Vec<UID>),
    }

    #[derive(Clone, Debug)]
    enum StreamEvent {
        Untagged(UntaggedEvent),
        WaitForCommand(
            &'static str,
            Arc<Mutex<Option<futures::channel::oneshot::Sender<()>>>>,
        ),
        Quit,
    }

    #[derive(Clone)]
    pub struct ImapServerConfig {
        pub capabilities: Vec1<Capability<'static>>,
        pub authenticated_capabilities: Vec1<Capability<'static>>,
        pub hostname: String,
        pub port: u16,
        pub tls_acceptor: Option<native_tls::TlsAcceptor>,
        pub greet_clients: bool,
        pub send_capabilities_on_auth: bool,
    }

    pub struct ImapServer {
        pub listener: Async<TcpListener>,
        pub command_receiver: UnboundedReceiver<ServerEvent>,
        pub command_sender: UnboundedSender<ServerEvent>,
        pub state: Arc<Mutex<ServerState>>,
        pub config: ImapServerConfig,
    }

    impl ImapServer {
        pub fn new(
            listener: Async<TcpListener>,
            (command_sender, command_receiver): (
                UnboundedSender<ServerEvent>,
                UnboundedReceiver<ServerEvent>,
            ),
            state: Arc<Mutex<ServerState>>,
        ) -> Self {
            let capabilities = vec![
                Capability::Imap4Rev1,
                Capability::Auth(AuthMechanism::Plain),
                Capability::SaslIr,
                Capability::Id,
            ]
            .try_into()
            .unwrap();
            let authenticated_capabilities = vec![
                Capability::Imap4Rev1,
                Capability::Id,
                Capability::Idle,
                Capability::Enable,
            ]
            .try_into()
            .unwrap();
            let local_addr = listener.as_ref().local_addr().unwrap();
            let hostname = local_addr.ip().to_string();
            let port = local_addr.port();
            Self {
                listener,
                command_sender,
                command_receiver,
                state,
                config: ImapServerConfig {
                    capabilities,
                    authenticated_capabilities,
                    hostname,
                    port,
                    tls_acceptor: None,
                    greet_clients: true,
                    send_capabilities_on_auth: false,
                },
            }
        }

        pub fn spawn(self) -> std::thread::JoinHandle<()> {
            struct StreamHandle {
                join_handle: std::thread::JoinHandle<()>,
                sender: UnboundedSender<StreamEvent>,
            }
            std::thread::spawn(move || {
                block_on(async move {
                    let Self {
                        listener,
                        command_sender,
                        mut command_receiver,
                        state,
                        mut config,
                    } = self;

                    let mut streams: HashMap<usize, StreamHandle> = HashMap::new();
                    let mut queue = VecDeque::new();
                    #[derive(Debug)]
                    enum FutResult {
                        Event(UnboundedReceiver<ServerEvent>, ServerEvent),
                        NewStream(Async<TcpListener>, Async<TcpStream>),
                    }
                    let mut fut_set: FuturesUnordered<futures::future::BoxFuture<'_, FutResult>> =
                        FuturesUnordered::new();
                    fut_set.push(
                        {
                            async move {
                                let (stream, _) = listener.accept().await.unwrap();
                                FutResult::NewStream(listener, stream)
                            }
                        }
                        .boxed(),
                    );
                    fut_set.push(
                        {
                            async move {
                                let command = command_receiver.next().await.unwrap();
                                FutResult::Event(command_receiver, command)
                            }
                        }
                        .boxed(),
                    );
                    'server_loop: loop {
                        while let Some(server_event) = queue.pop_front() {
                            let quit = matches!(server_event, ServerEvent::Quit);
                            log::debug!("server got {server_event:?}");
                            let event = match server_event {
                                ServerEvent::Quit => StreamEvent::Quit,
                                ServerEvent::ChangeCapabilities {
                                    capabilities,
                                    authenticated_capabilities,
                                } => {
                                    config.capabilities = capabilities;
                                    config.authenticated_capabilities = authenticated_capabilities;
                                    StreamEvent::Quit
                                }
                                ServerEvent::SendCapabilitiesOnAuth {
                                    send_capabilities_on_auth,
                                } => {
                                    config.send_capabilities_on_auth = send_capabilities_on_auth;
                                    StreamEvent::Quit
                                }
                                ServerEvent::State(other) => state.lock().unwrap().recv(other),
                            };
                            for (i, stream) in streams.values_mut().enumerate() {
                                if stream.sender.unbounded_send(event.clone()).is_err() {
                                    log::warn!(
                                        "Did not send event {event:?} to stream {i} because it \
                                         disconnected"
                                    );
                                }
                            }
                            if quit {
                                break 'server_loop;
                            }
                        }
                        if let Some(next) = fut_set.next().await {
                            match next {
                                FutResult::NewStream(listener, tcp_stream) => {
                                    fut_set.push(
                                        {
                                            async move {
                                                let (stream, _) = listener.accept().await.unwrap();
                                                FutResult::NewStream(listener, stream)
                                            }
                                        }
                                        .boxed(),
                                    );
                                    let tcp_stream =
                                        Connection::new_tcp(tcp_stream.into_inner().unwrap());
                                    let tcp_stream = if let Some(acceptor) = &config.tls_acceptor {
                                        // Need to perform TLS handshake which might block.
                                        let tcp_stream = match acceptor.accept(tcp_stream) {
                                            Err(native_tls::HandshakeError::WouldBlock(
                                                midhandshake_stream,
                                            )) => {
                                                let mut midhandshake_stream =
                                                    Some(midhandshake_stream);
                                                loop {
                                                    match midhandshake_stream
                                                        .take()
                                                        .unwrap()
                                                        .handshake()
                                                    {
                                                        Ok(r) => {
                                                            break Ok(r);
                                                        }
                                                        Err(
                                                            native_tls::HandshakeError::WouldBlock(
                                                                stream,
                                                            ),
                                                        ) => {
                                                            midhandshake_stream = Some(stream);
                                                        }
                                                        Err(err) => {
                                                            break Err(Error::from(err).set_kind(ErrorKind::Network(
                                                                        NetworkErrorKind::InvalidTLSConnection,
                                                            )));
                                                        }
                                                    }
                                                }
                                            }
                                            Err(err) => {
                                                Err(Error::from(err).set_kind(ErrorKind::Network(
                                                    NetworkErrorKind::InvalidTLSConnection,
                                                )))
                                            }
                                            Ok(val) => Ok(val),
                                        };
                                        tcp_stream.map(|tcp_stream| {
                                            Async::new(Connection::new_tls(tcp_stream)).unwrap()
                                        })
                                    } else {
                                        Ok(Async::new(tcp_stream).unwrap())
                                    };
                                    match tcp_stream {
                                        Ok(tcp_stream) => {
                                            let idx = streams.len();
                                            let (sender, stream_receiver) = unbounded();
                                            let mut stream = ImapServerStream::new(
                                                tcp_stream,
                                                state.clone(),
                                                command_sender.clone(),
                                                config.clone(),
                                                stream_receiver,
                                            );
                                            let greet = stream.greet().await;
                                            match greet {
                                                Ok(()) => {
                                                    let join_handle =
                                                        std::thread::spawn(move || {
                                                            block_on(async move {
                                                                while !stream.next().await {}
                                                            });
                                                        });
                                                    streams.insert(
                                                        idx,
                                                        StreamHandle {
                                                            sender,
                                                            join_handle,
                                                        },
                                                    );
                                                }
                                                Err(err) => {
                                                    log::warn!(
                                                        "Could not greet client {idx}: {err}"
                                                    );
                                                }
                                            }
                                        }
                                        Err(err) => {
                                            log::error!(
                                                "Could not perform TLS handshake with client: \
                                                 {err}"
                                            );
                                        }
                                    }
                                }
                                FutResult::Event(mut command_receiver, server_event) => {
                                    queue.push_back(server_event);
                                    fut_set.push(
                                        {
                                            async move {
                                                let command =
                                                    command_receiver.next().await.unwrap();
                                                FutResult::Event(command_receiver, command)
                                            }
                                        }
                                        .boxed(),
                                    );
                                }
                            }
                        }
                    }
                    log::debug!("waiting on {} stream handles", streams.len());
                    for handle in streams.into_values() {
                        handle.join_handle.join().unwrap();
                    }
                })
            })
        }
    }

    #[derive(Eq, Hash, PartialEq)]
    enum Untagged {
        Exists,
        Expunge(UID),
    }

    #[derive(Debug)]
    enum ImapServerStreamState {
        Idle {
            cmd_id: Tag<'static>,
        },
        AuthResponse {
            cmd_id: Tag<'static>,
            mechanism: AuthMechanism<'static>,
        },
    }

    struct ImapServerStream {
        tcp_stream: Async<Connection>,
        cmd_state: Option<ImapServerStreamState>,
        state: Arc<Mutex<ServerState>>,
        stream_receiver: UnboundedReceiver<StreamEvent>,
        server_sender: UnboundedSender<ServerEvent>,
        session_state: SessionState,
        config: ImapServerConfig,
        untagged: VecDeque<Untagged>,
        our_untagged: HashSet<UntaggedEvent>,
        #[allow(clippy::type_complexity)]
        command_notifiers: Vec<(
            &'static str,
            Arc<Mutex<Option<futures::channel::oneshot::Sender<()>>>>,
        )>,
        msn_map: MessageSequenceNumberMap,
        buf: Vec<u8>,
        buf_start: usize,
        buf_end: usize,
        events: VecDeque<StreamEvent>,
    }

    impl std::fmt::Debug for ImapServerStream {
        fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
            fmt.debug_struct("ImapServerStream")
                .field("tcp_stream", &self.tcp_stream)
                .field("cmd_state", &self.cmd_state)
                .field("state", &self.state)
                .field("session_state", &self.session_state)
                .finish_non_exhaustive()
        }
    }

    impl ImapServerStream {
        fn new(
            tcp_stream: Async<Connection>,
            state: Arc<Mutex<ServerState>>,
            server_sender: UnboundedSender<ServerEvent>,
            config: ImapServerConfig,
            stream_receiver: UnboundedReceiver<StreamEvent>,
        ) -> Self {
            let msn_map = {
                let mut msn_map = MessageSequenceNumberMap::default();
                let state_lck = state.lock().unwrap();
                for uid in state_lck.envelopes.keys() {
                    let new_exists = msn_map.exists().copied().unwrap_or(0) + 1;
                    assert!(msn_map.insert(new_exists, *uid));
                }
                msn_map
            };
            Self {
                tcp_stream,
                cmd_state: None,
                state,
                server_sender,
                session_state: SessionState::Unauthenticated,
                config,
                untagged: Default::default(),
                our_untagged: Default::default(),
                command_notifiers: vec![],
                msn_map,
                stream_receiver,
                buf: vec![0; 64 * 1024],
                buf_start: 0,
                buf_end: 0,
                events: VecDeque::new(),
            }
        }

        async fn next(&mut self) -> bool {
            async fn read_line<'a>(
                tcp_stream: &mut Async<Connection>,
                buf: &'a mut [u8],
                start: &mut usize,
                end: &mut usize,
            ) -> Option<&'a [u8]> {
                // log::trace!(
                //     "read_line: buf={:?} start = {start:?} end = {end:?}",
                //     String::from_utf8_lossy(&buf[..*end])
                // );
                if *start == 0 || !buf[*start..*end].contains_subsequence(b"\r\n") {
                    let read_bytes = tcp_stream.read(&mut buf[*end..]).await.unwrap();
                    *end += read_bytes;
                    // log::trace!(
                    //     "read_line: read_bytes = {read_bytes:?} buf = {:?}",
                    //     String::from_utf8_lossy(&buf[..*end])
                    // );
                    if !buf[*start..*end].contains_subsequence(b"\r\n") {
                        // log::trace!("read_line: returning None");
                        return None;
                    }
                    let Some(input) = buf[*start..*end].split_rn().next() else {
                        // log::trace!("read_line: returning None");
                        return None;
                    };
                    *start += input.len();
                    if *start == *end {
                        *start = 0;
                        *end = 0;
                    }
                    // log::trace!("read_line: returning {:?}", String::from_utf8_lossy(input));
                    Some(input)
                } else {
                    let rest = &buf[*start..*end];
                    let input = rest.split_rn().next().unwrap();
                    *start += input.len();
                    if *start == *end {
                        *start = 0;
                        *end = 0;
                    }
                    // log::trace!("read_line: returning {:?}", String::from_utf8_lossy(input));
                    Some(input)
                }
            }
            'outer: loop {
                if let Some(ret) = self.recv().await {
                    return ret;
                }
                let Self {
                    ref mut tcp_stream,
                    ref mut cmd_state,
                    ref state,
                    ref mut session_state,
                    ref config,
                    ref mut buf,
                    ref mut events,
                    ref mut command_notifiers,
                    ref mut stream_receiver,
                    ref mut server_sender,
                    ref mut buf_start,
                    ref mut buf_end,
                    ref mut untagged,
                    ref mut our_untagged,
                    ref mut msn_map,
                } = self;
                if cmd_state.is_some() {
                    {
                        let mut silence_exists = false;
                        while let Some(untagged) = untagged.pop_front() {
                            match untagged {
                                Untagged::Expunge(uid) => {
                                    let msn = msn_map.get(&uid).copied().unwrap();
                                    assert_eq!(msn_map.expunge(&msn), Some(uid));
                                    tcp_stream
                                        .write_all(format!("* {msn} EXPUNGE\r\n").as_bytes())
                                        .await
                                        .unwrap();
                                    tcp_stream.flush().await.unwrap();
                                    silence_exists = false;
                                }
                                Untagged::Exists => {
                                    if !silence_exists {
                                        if let Some(exists) = msn_map.exists() {
                                            tcp_stream
                                                .write_all(
                                                    format!("* {exists} EXISTS\r\n").as_bytes(),
                                                )
                                                .await
                                                .unwrap();
                                            tcp_stream.flush().await.unwrap();
                                        }
                                        silence_exists = true;
                                    }
                                }
                            }
                        }
                    }
                    let mut read_fut = Box::pin(read_line(tcp_stream, buf, buf_start, buf_end));
                    let input = match future::select(&mut read_fut, stream_receiver.next()).await {
                        Either::Left((value1, _)) => {
                            if value1.is_none() {
                                continue 'outer;
                            }
                            drop(read_fut);
                            value1.unwrap()
                        }
                        Either::Right((event, _)) => {
                            drop(read_fut);
                            events.push_back(event.unwrap());
                            continue 'outer;
                        }
                    };
                    let input = String::from_utf8_lossy(input).to_string();
                    let mut responses = vec![];
                    let response = match self.cmd_state.take().unwrap() {
                        ImapServerStreamState::Idle { cmd_id } => {
                            log::debug!("idle handler received: {input:?}");
                            let (rest, _) = imap_codec::IdleDoneCodec::default()
                                .decode(input.as_bytes())
                                .unwrap();
                            assert!(rest.is_empty(), "{:?}", String::from_utf8_lossy(rest));
                            Response::Status(
                                Status::ok(Some(cmd_id), None, "IDLE terminated").unwrap(),
                            )
                        }
                        ImapServerStreamState::AuthResponse { cmd_id, mechanism } => {
                            log::debug!("waiting for auth response handler received: {input:?}");
                            let (rest, data) = imap_codec::AuthenticateDataCodec::default()
                                .decode(input.as_bytes())
                                .unwrap();
                            assert!(rest.is_empty(), "{:?}", String::from_utf8_lossy(rest));
                            match data {
                                imap_types::auth::AuthenticateData::Continue(secret) => {
                                    match mechanism {
                                        AuthMechanism::Plain => {
                                            let password = secret.declassify();
                                            assert_eq!(password.as_ref(), b"\0user\0password");
                                        }
                                        AuthMechanism::OAuthBearer => {
                                            let password = secret.declassify();
                                            #[rustfmt::skip]
                                            let expected_response =
                                                format!(
                                                    "n,a=user,\x01host=127.0.0.1\x01port={port}\x01auth=Bearer password\x01\x01",
                                                    port = config.port
                                                );
                                            assert_eq!(
                                                password.as_ref(),
                                                expected_response.as_bytes()
                                            );
                                        }
                                        other @ AuthMechanism::Other(_)
                                            if other.as_ref() == "ANONYMOUS" =>
                                        {
                                            // Got optional trace information.
                                            let trace =
                                                String::from_utf8_lossy(secret.declassify());
                                            log::debug!(
                                                "AUTHENTICATE ANONYMOUS with trace information \
                                                 {trace}"
                                            );
                                        }
                                        other => panic!("unimplemented auth method {other:?}"),
                                    }
                                    *session_state = SessionState::Authenticated;
                                    if config.send_capabilities_on_auth {
                                        responses.push(Response::Data(Data::Capability(
                                            config.authenticated_capabilities.clone(),
                                        )));
                                    }
                                    Response::Status(
                                        Status::ok(Some(cmd_id), None, "Welcome").unwrap(),
                                    )
                                }
                                imap_types::auth::AuthenticateData::Cancel => {
                                    log::debug!("Client cancelled auth {input:?}");
                                    Response::Status(
                                        Status::bad(Some(cmd_id), None, "AUTHENTICATE cancelled")
                                            .unwrap(),
                                    )
                                }
                            }
                        }
                    };
                    responses.push(response);
                    for response in responses {
                        for fragment in ResponseCodec::new().encode(&response) {
                            match fragment {
                                Fragment::Line { data } => {
                                    tcp_stream.write_all(&data).await.unwrap();
                                    tcp_stream.flush().await.unwrap();
                                }
                                Fragment::Literal { data, mode } => match mode {
                                    LiteralMode::Sync => {
                                        // Wait for a continuation request.
                                        panic!()
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
                    }
                    return false;
                } else {
                    let mut read_fut = Box::pin(read_line(tcp_stream, buf, buf_start, buf_end));
                    let input = match future::select(&mut read_fut, stream_receiver.next()).await {
                        Either::Left((value1, _)) => {
                            if value1.is_none() {
                                continue 'outer;
                            }
                            drop(read_fut);
                            value1.unwrap()
                        }
                        Either::Right((event, _)) => {
                            drop(read_fut);
                            events.push_back(event.unwrap());
                            continue 'outer;
                        }
                    };
                    let line = String::from_utf8_lossy(input).to_string();
                    log::debug!("loop_handler 'main received: {line:?}");
                    let codec = CommandCodec::new();
                    let (remainder, cmd) = codec.decode(line.as_bytes()).unwrap();
                    assert_eq!(remainder, b"");
                    let id = cmd.tag;

                    use imap_types::command::CommandBody;

                    let mut logout = false;

                    let mut responses = vec![];
                    {
                        let mut silence_exists = false;
                        while let Some(untagged) = untagged.pop_front() {
                            match untagged {
                                Untagged::Expunge(uid) => {
                                    let msn = msn_map.get(&uid).copied().unwrap();
                                    assert_eq!(msn_map.expunge(&msn), Some(uid));
                                    responses.push(Response::Data(Data::Expunge(
                                        (msn as u32).try_into().unwrap(),
                                    )));
                                    silence_exists = false;
                                }
                                Untagged::Exists => {
                                    if !silence_exists {
                                        if let Some(exists) = msn_map.exists().copied() {
                                            responses
                                                .push(Response::Data(Data::Exists(exists as u32)));
                                        }
                                        silence_exists = true;
                                    }
                                }
                            }
                        }
                    }
                    match cmd.body {
                        CommandBody::Id { parameters: _ } => {
                            if !config
                                .capabilities
                                .as_ref()
                                .iter()
                                .any(|c| c == &Capability::Id)
                            {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "unknown command").unwrap(),
                                ));
                            } else {
                                responses.push(Response::Data(Data::Id { parameters: None }));
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "ID completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Capability => {
                            responses.push(Response::Data(Data::Capability(
                                if !session_state.is_authenticated() {
                                    config.capabilities.clone()
                                } else {
                                    config.authenticated_capabilities.clone()
                                },
                            )));
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "CAPABILITY completed").unwrap(),
                            ));
                        }
                        CommandBody::Authenticate {
                            mechanism,
                            initial_response,
                        } => {
                            if !session_state.is_authenticated() {
                                assert!(
                                    config
                                        .capabilities
                                        .as_ref()
                                        .iter()
                                        .any(|c| *c == Capability::Auth(mechanism.clone())),
                                    "{:?}",
                                    config.capabilities
                                );
                                match mechanism {
                                    AuthMechanism::Plain => match initial_response {
                                        Some(initial_response) => {
                                            assert!(
                                                config
                                                    .capabilities
                                                    .as_ref()
                                                    .contains(&Capability::SaslIr),
                                                "{:?}",
                                                config.capabilities
                                            );
                                            let password = initial_response.declassify();
                                            assert_eq!(password.as_ref(), b"\0user\0password");
                                            *session_state = SessionState::Authenticated;
                                            if config.send_capabilities_on_auth {
                                                responses.push(Response::Data(Data::Capability(
                                                    config.authenticated_capabilities.clone(),
                                                )));
                                            }
                                            responses.push(Response::Status(
                                                Status::ok(Some(id), None, "Welcome").unwrap(),
                                            ));
                                        }
                                        None => {
                                            *cmd_state =
                                                Some(ImapServerStreamState::AuthResponse {
                                                    cmd_id: id.into_static(),
                                                    mechanism: mechanism.into_static(),
                                                });
                                            responses.push(Response::CommandContinuationRequest(
                                                CommandContinuationRequest::basic(
                                                    None,
                                                    "feed me data",
                                                )
                                                .unwrap(),
                                            ));
                                        }
                                    },
                                    AuthMechanism::OAuthBearer => match initial_response {
                                        Some(initial_response) => {
                                            assert!(
                                                config
                                                    .capabilities
                                                    .as_ref()
                                                    .contains(&Capability::SaslIr),
                                                "{:?}",
                                                config.capabilities
                                            );
                                            let password = initial_response.declassify();
                                            #[rustfmt::skip]
                                        let expected_response =
                                            format!(
                                                "n,a=user,\x01host=127.0.0.1\x01port={port}\x01auth=Bearer password\x01\x01",
                                                port = config.port
                                            );
                                            assert_eq!(
                                                password.as_ref(),
                                                expected_response.as_bytes()
                                            );
                                            *session_state = SessionState::Authenticated;
                                            if config.send_capabilities_on_auth {
                                                responses.push(Response::Data(Data::Capability(
                                                    config.authenticated_capabilities.clone(),
                                                )));
                                            }
                                            responses.push(Response::Status(
                                                Status::ok(Some(id), None, "Welcome").unwrap(),
                                            ));
                                        }
                                        None => {
                                            *cmd_state =
                                                Some(ImapServerStreamState::AuthResponse {
                                                    cmd_id: id.into_static(),
                                                    mechanism: mechanism.into_static(),
                                                });
                                            responses.push(Response::CommandContinuationRequest(
                                                CommandContinuationRequest::basic(
                                                    None,
                                                    "feed me data",
                                                )
                                                .unwrap(),
                                            ));
                                        }
                                    },
                                    other @ AuthMechanism::Other(_)
                                        if other.as_ref() == "ANONYMOUS" =>
                                    {
                                        if let Some(initial_response) = initial_response {
                                            assert!(
                                                config
                                                    .capabilities
                                                    .as_ref()
                                                    .contains(&Capability::SaslIr),
                                                "{:?}",
                                                config.capabilities
                                            );
                                            // Got optional trace information.
                                            let trace = String::from_utf8_lossy(
                                                initial_response.declassify(),
                                            );
                                            log::debug!(
                                                "AUTHENTICATE ANONYMOUS with trace information \
                                                 {trace}"
                                            );
                                        }
                                        *session_state = SessionState::Authenticated;
                                        if config.send_capabilities_on_auth {
                                            responses.push(Response::Data(Data::Capability(
                                                config.authenticated_capabilities.clone(),
                                            )));
                                        }
                                        responses.push(Response::Status(
                                            Status::ok(Some(id), None, "Welcome").unwrap(),
                                        ));
                                    }
                                    other => panic!("unimplemented auth method {other:?}"),
                                }
                            } else {
                                panic!();
                            }
                        }
                        CommandBody::Idle => {
                            while let Some(pos) =
                                command_notifiers.iter().position(|(k, _)| *k == "IDLE")
                            {
                                let (_, notifier) = command_notifiers.remove(pos);
                                if let Some(notifier) = notifier.lock().unwrap().take() {
                                    _ = notifier.send(());
                                };
                            }
                            *cmd_state = Some(ImapServerStreamState::Idle {
                                cmd_id: id.into_static(),
                            });
                            log::debug!("loop_handler is now idling");
                            responses.push(Response::CommandContinuationRequest(
                                CommandContinuationRequest::basic(None, "now idling").unwrap(),
                            ));
                        }
                        CommandBody::Noop => {
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "NOOP completed").unwrap(),
                            ));
                        }
                        CommandBody::Logout => {
                            responses
                                .push(Response::Status(Status::bye(None, "cruel world").unwrap()));
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "LOGOUT completed").unwrap(),
                            ));
                            logout = true;
                        }
                        // "LIST \"\" *\r\n"
                        CommandBody::List {
                            ref reference,
                            ref mailbox_wildcard,
                        } if (reference, mailbox_wildcard)
                            == (
                                &imap_types::mailbox::Mailbox::Other("".try_into().unwrap()),
                                &imap_types::mailbox::ListMailbox::Token("*".try_into().unwrap()),
                            ) =>
                        {
                            responses.push(Response::Data(Data::List {
                                items: vec![],
                                delimiter: Some('/'.try_into().unwrap()),
                                mailbox: imap_types::mailbox::Mailbox::Inbox,
                            }));
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "LIST completed").unwrap(),
                            ));
                        }
                        // "LSUB \"\" *\r\n"
                        CommandBody::Lsub {
                            ref reference,
                            ref mailbox_wildcard,
                        } if (reference, mailbox_wildcard)
                            == (
                                &imap_types::mailbox::Mailbox::Other("".try_into().unwrap()),
                                &imap_types::mailbox::ListMailbox::Token("*".try_into().unwrap()),
                            ) =>
                        {
                            responses.push(Response::Data(Data::Lsub {
                                items: vec![],
                                delimiter: Some('/'.try_into().unwrap()),
                                mailbox: imap_types::mailbox::Mailbox::Inbox,
                            }));
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "LSUB completed").unwrap(),
                            ));
                        }
                        CommandBody::Select {
                            ref mailbox,
                            ref parameters,
                        }
                        | CommandBody::Examine {
                            ref mailbox,
                            ref parameters,
                        } => {
                            // 6.3.1.  SELECT Command
                            // 6.3.2.  EXAMINE Command
                            assert_eq!(parameters, &[]);
                            assert_eq!(mailbox, &imap_types::mailbox::Mailbox::Inbox);

                            let is_select = matches!(cmd.body, CommandBody::Select { .. });
                            *session_state = if is_select {
                                SessionState::SelectedMailbox
                            } else {
                                SessionState::ExaminedMailbox
                            };
                            let (exists, recent, uidvalidity, uidnext, unseen) = {
                                let state_lck = state.lock().unwrap();
                                let uidnext = state_lck.next_uid;
                                let uidvalidity = state_lck.uidvalidity;
                                let exists = state_lck.envelopes.len();
                                let unseen = state_lck
                                    .envelopes
                                    .values()
                                    .filter(|env| !env.is_seen())
                                    .count();
                                let recent = 0;
                                (exists, recent, uidvalidity, uidnext, unseen)
                            };

                            // REQUIRED untagged responses: FLAGS, EXISTS, RECENT
                            responses.push(Response::Data(Data::Flags(vec![
                                imap_types::flag::Flag::Answered,
                                imap_types::flag::Flag::Flagged,
                                imap_types::flag::Flag::Deleted,
                                imap_types::flag::Flag::Seen,
                                imap_types::flag::Flag::Draft,
                            ])));
                            responses.push(Response::Data(Data::Exists(exists as u32)));
                            responses.push(Response::Data(Data::Recent(recent as u32)));
                            // REQUIRED OK untagged responses:  UNSEEN,  PERMANENTFLAGS, UIDNEXT,
                            // UIDVALIDITY
                            if let Some(unseen) = NonZeroU32::new(unseen as u32) {
                                responses.push(Response::Status(
                                    Status::ok(None, Some(Code::Unseen(unseen)), "Unseen").unwrap(),
                                ));
                            }
                            responses.push(Response::Status(
                                Status::ok(
                                    None,
                                    Some(Code::PermanentFlags(vec![])),
                                    "No permanent flags permitted",
                                )
                                .unwrap(),
                            ));
                            responses.push(Response::Status(
                                Status::ok(
                                    None,
                                    Some(Code::UidNext((uidnext as u32).try_into().unwrap())),
                                    "Next UID",
                                )
                                .unwrap(),
                            ));
                            responses.push(Response::Status(
                                Status::ok(
                                    None,
                                    Some(Code::UidValidity(
                                        (uidvalidity as u32).try_into().unwrap(),
                                    )),
                                    "UIDs valid",
                                )
                                .unwrap(),
                            ));
                            if is_select {
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "SELECT completed").unwrap(),
                                ));
                            } else {
                                responses.push(Response::Status(
                                    Status::ok(Some(id), Some(Code::ReadOnly), "EXAMINE completed")
                                        .unwrap(),
                                ));
                            }
                        }
                        CommandBody::Unselect => {
                            if !session_state.is_selected() {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                *session_state = SessionState::Authenticated;
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "UNSELECT completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Close => {
                            if matches!(session_state, SessionState::SelectedMailbox) {
                                // 6.4.1.  CLOSE Command
                                let (s, r) = futures::channel::oneshot::channel();
                                server_sender
                                    .unbounded_send(ServerStateEvent::Expunge(s).into())
                                    .unwrap();
                                let expunged = r.await.unwrap();
                                log::debug!("CLOSE silently expunged following uids: {expunged:?}");
                                our_untagged.insert(UntaggedEvent::Deletes(expunged));
                            }

                            *session_state = SessionState::Authenticated;
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "CLOSE completed").unwrap(),
                            ));
                        }
                        CommandBody::Expunge => {
                            if matches!(session_state, SessionState::ExaminedMailbox) {
                                responses.push(Response::Status(
                                    Status::no(Some(id), None, "mailbox is selected read-only")
                                        .unwrap(),
                                ));
                            } else if !matches!(session_state, SessionState::SelectedMailbox) {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                {
                                    let (s, r) = futures::channel::oneshot::channel();
                                    server_sender
                                        .unbounded_send(ServerStateEvent::Expunge(s).into())
                                        .unwrap();
                                    let expunged = r.await.unwrap();
                                    for uid in &expunged {
                                        let msn = msn_map.get(uid).copied().unwrap();
                                        assert_eq!(msn_map.expunge(&msn), Some(*uid));
                                        responses.push(Response::Data(Data::Expunge(
                                            (msn as u32).try_into().unwrap(),
                                        )));
                                    }
                                    our_untagged.insert(UntaggedEvent::Deletes(expunged));
                                }
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "EXPUNGE completed").unwrap(),
                                ));
                            }
                        }
                        // "UID SEARCH 1:*\r\n"
                        CommandBody::Search {
                            charset: None,
                            criteria,
                            uid: true,
                        } if criteria
                            == imap_types::search::SearchKey::SequenceSet(
                                "1:*".try_into().unwrap(),
                            )
                            .into() =>
                        {
                            if !session_state.is_selected() {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                let uids = state
                                    .lock()
                                    .unwrap()
                                    .envelopes
                                    .iter()
                                    .map(|(u, _)| (*u as u32).try_into().unwrap())
                                    .collect::<Vec<_>>();
                                responses.push(Response::Data(Data::Search(uids, None)));
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "SEARCH completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Search {
                            charset: None,
                            criteria,
                            uid: false,
                        } if criteria == imap_types::search::SearchKey::Unseen.into() => {
                            if !session_state.is_selected() {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                let msns = {
                                    let state_lck = state.lock().unwrap();
                                    state_lck
                                        .envelopes
                                        .iter()
                                        .filter(|(_, env)| !env.is_seen())
                                        .map(|(uid, _)| {
                                            (*msn_map.get(uid).unwrap() as u32).try_into().unwrap()
                                        })
                                        .collect::<Vec<_>>()
                                };
                                responses.push(Response::Data(Data::Search(msns, None)));
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "SEARCH completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Status {
                            mailbox: imap_types::mailbox::Mailbox::Inbox,
                            item_names,
                        } if item_names.as_ref()
                            == [imap_types::status::StatusDataItemName::UidNext] =>
                        {
                            let uidnext = state.lock().unwrap().next_uid;
                            responses.push(Response::Data(Data::Status {
                                mailbox: imap_types::mailbox::Mailbox::Inbox,
                                items: vec![imap_types::status::StatusDataItem::UidNext(
                                    (uidnext as u32).try_into().unwrap(),
                                )]
                                .into(),
                            }));
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "STATUS completed").unwrap(),
                            ));
                        }
                        CommandBody::Fetch {
                            sequence_set,
                            macro_or_item_names,
                            modifiers: _,
                            uid: false,
                        } if macro_or_item_names == melib::imap::email::common_attributes().1 => {
                            if !session_state.is_selected() {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                let largest = state.lock().unwrap().envelopes.len() as u32 + 1;
                                'msn_fetch: for msn in
                                    sequence_set.iter(largest.try_into().unwrap())
                                {
                                    let Some((uid, mail)) = state
                                        .lock()
                                        .unwrap()
                                        .envelopes
                                        .get_index(msn.get().saturating_sub(1) as usize)
                                        .map(|(u, m)| (*u, m.clone()))
                                    else {
                                        continue 'msn_fetch;
                                    };
                                    let references = mail.as_body_peek_references();
                                    responses.push(Response::Data(Data::Fetch {
                                        seq: msn,
                                        items: vec![
                                            MessageDataItem::Uid((uid as u32).try_into().unwrap()),
                                            MessageDataItem::Flags(mail.as_flags()),
                                            MessageDataItem::Envelope(mail.as_envelope()),
                                            MessageDataItem::BodyStructure(mail.as_bodystructure()),
                                            references,
                                        ]
                                        .try_into()
                                        .unwrap(),
                                    }));
                                }
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "FETCH completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Fetch {
                            sequence_set,
                            macro_or_item_names,
                            modifiers: _,
                            uid: true,
                        } if macro_or_item_names
                            == vec![imap_types::fetch::MessageDataItemName::Flags].into() =>
                        {
                            if !session_state.is_selected() {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                let largest =
                                    state.lock().unwrap().next_uid.saturating_sub(1) as u32;
                                'uid_fetch_flags: for uid in
                                    sequence_set.iter(largest.try_into().unwrap())
                                {
                                    let Some(mail) = state
                                        .lock()
                                        .unwrap()
                                        .envelopes
                                        .get(&(uid.get() as usize))
                                        .cloned()
                                    else {
                                        continue 'uid_fetch_flags;
                                    };
                                    responses.push(Response::Data(Data::Fetch {
                                        seq: uid,
                                        items: vec![
                                            MessageDataItem::Uid(uid),
                                            MessageDataItem::Flags(mail.as_flags()),
                                        ]
                                        .try_into()
                                        .unwrap(),
                                    }));
                                }
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "FETCH flags completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Fetch {
                            sequence_set,
                            macro_or_item_names,
                            modifiers: _,
                            uid: true,
                        } if macro_or_item_names == melib::imap::email::common_attributes().1 => {
                            if !session_state.is_selected() {
                                responses.push(Response::Status(
                                    Status::bad(Some(id), None, "no mailbox is selected").unwrap(),
                                ));
                            } else {
                                let largest =
                                    state.lock().unwrap().next_uid.saturating_sub(1) as u32;
                                'uid_fetch: for uid in
                                    sequence_set.iter(largest.try_into().unwrap())
                                {
                                    let Some(mail) = state
                                        .lock()
                                        .unwrap()
                                        .envelopes
                                        .get(&(uid.get() as usize))
                                        .cloned()
                                    else {
                                        continue 'uid_fetch;
                                    };
                                    let references = mail.as_body_peek_references();
                                    responses.push(Response::Data(Data::Fetch {
                                        seq: uid,
                                        items: vec![
                                            MessageDataItem::Uid(uid),
                                            MessageDataItem::Flags(mail.as_flags()),
                                            MessageDataItem::Envelope(mail.as_envelope()),
                                            MessageDataItem::BodyStructure(mail.as_bodystructure()),
                                            references,
                                        ]
                                        .try_into()
                                        .unwrap(),
                                    }));
                                }
                                responses.push(Response::Status(
                                    Status::ok(Some(id), None, "UID FETCH completed").unwrap(),
                                ));
                            }
                        }
                        CommandBody::Store {
                            sequence_set,
                            kind,
                            flags,
                            response: imap_types::flag::StoreResponse::Answer,
                            modifiers: _,
                            uid: true,
                        } => {
                            {
                                let mut state_lck = state.lock().unwrap();
                                let largest = state_lck.envelopes.len() as u32 + 1;
                                for uid in sequence_set.iter(largest.try_into().unwrap()) {
                                    match kind {
                                        imap_types::flag::StoreType::Add => {
                                            for flag in &flags {
                                                match flag {
                                                    imap_types::flag::Flag::Deleted => {
                                                        if let Some(env) = state_lck
                                                            .envelopes
                                                            .get_mut(&(uid.get() as usize))
                                                        {
                                                            env.envelope
                                                                .set_flag(Flag::TRASHED, true);
                                                        }
                                                    }
                                                    other => panic!("{other:?}"),
                                                }
                                            }
                                        }
                                        other => panic!("{other:?}"),
                                    }
                                }
                            }
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "UID STORE completed").unwrap(),
                            ));
                        }
                        other => panic!("Unexpected cmd: {id:?} {other:?}"),
                    }
                    for response in responses {
                        for fragment in ResponseCodec::new().encode(&response) {
                            match fragment {
                                Fragment::Line { data } => {
                                    tcp_stream.write_all(&data).await.unwrap();
                                    tcp_stream.flush().await.unwrap();
                                }
                                Fragment::Literal { data, mode } => match mode {
                                    LiteralMode::Sync => {
                                        // Wait for a continuation request.
                                        panic!()
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
                    }
                    return logout;
                }
            }
        }

        async fn greet(&mut self) -> Result<()> {
            use imap_codec::GreetingCodec;
            use imap_types::response::Greeting;

            if !self.config.greet_clients {
                return Ok(());
            }
            for fragment in GreetingCodec::new()
                .encode(&imap_types::response::Greeting::ok(None, "server ready").unwrap())
            {
                match fragment {
                    Fragment::Line { data } => {
                        self.tcp_stream.write_all(&data).await.unwrap();
                        self.tcp_stream.flush().await.unwrap();
                    }
                    Fragment::Literal { data, mode } => match mode {
                        LiteralMode::Sync => {
                            // Wait for a continuation request.
                            panic!()
                        }
                        LiteralMode::NonSync => {
                            // We don't need to wait for a continuation request
                            // as the server will also not send it.
                            self.tcp_stream.write_all(&data).await.unwrap();
                            self.tcp_stream.flush().await.unwrap();
                        }
                    },
                }
            }
            Ok(())
        }

        async fn recv(&mut self) -> Option<bool> {
            let Self {
                ref mut tcp_stream,
                ref mut cmd_state,
                ref mut events,
                ref mut stream_receiver,
                ref mut untagged,
                ref mut our_untagged,
                ref mut msn_map,
                ref mut command_notifiers,
                ..
            } = self;
            while let Ok(event) = stream_receiver.try_recv() {
                events.push_back(event);
            }
            let any = !events.is_empty();
            while let Some(event) = events.pop_front() {
                if let StreamEvent::Untagged(ref untagged) = event {
                    if our_untagged.remove(untagged) {
                        continue;
                    }
                }
                match event {
                    StreamEvent::Quit => {
                        // We ignore I/O errors because client might have already exited
                        if let Some(cmd_state) = cmd_state.take() {
                            match cmd_state {
                                ImapServerStreamState::Idle { cmd_id } => {
                                    log::debug!(
                                        "IDLE handler received StreamEvent::Quit from server \
                                         thread"
                                    );
                                    _ = tcp_stream.write_all(b"* BYE cruel world\r\n").await;
                                    _ = tcp_stream.write_all(cmd_id.inner().as_bytes()).await;
                                    _ = tcp_stream.write_all(b" OK IDLE terminated\r\n").await;
                                }
                                ImapServerStreamState::AuthResponse {
                                    cmd_id,
                                    mechanism: _,
                                } => {
                                    log::debug!(
                                        "Waiting for AuthResponse received StreamEvent::Quit from \
                                         server thread"
                                    );
                                    _ = tcp_stream.write_all(b"* BYE cruel world\r\n").await;
                                    _ = tcp_stream.write_all(cmd_id.inner().as_bytes()).await;
                                    _ = tcp_stream
                                        .write_all(b" OK AUTHENTICATE terminated\r\n")
                                        .await;
                                }
                            }
                        } else {
                            log::debug!("main loop_handler received StreamEvent::Quit");
                            _ = tcp_stream.write_all(b"* BYE cruel world\r\n").await;
                        }
                        _ = tcp_stream.flush().await;
                        return Some(true);
                    }
                    StreamEvent::Untagged(UntaggedEvent::New(uid)) => {
                        assert_eq!(msn_map.get(&uid), None);
                        let new_exists = msn_map.exists().copied().unwrap_or(0) + 1;
                        assert!(msn_map.insert(new_exists, uid));
                        untagged.push_back(Untagged::Exists);
                    }
                    StreamEvent::Untagged(UntaggedEvent::Delete(uid)) => {
                        untagged.push_back(Untagged::Expunge(uid));
                    }
                    StreamEvent::Untagged(UntaggedEvent::Deletes(uids)) => {
                        for uid in uids {
                            untagged.push_back(Untagged::Expunge(uid));
                        }
                    }
                    StreamEvent::WaitForCommand(cmd, notifier) => {
                        if cmd == "IDLE"
                            && matches!(cmd_state, Some(ImapServerStreamState::Idle { .. }))
                        {
                            if let Some(notifier) = notifier.lock().unwrap().take() {
                                _ = notifier.send(());
                            };
                        } else {
                            command_notifiers.push((cmd, notifier));
                        }
                    }
                }
            }
            if any {
                Some(false)
            } else {
                None
            }
        }
    }
}

pub mod tests {
    use std::{
        collections::VecDeque,
        net::TcpListener,
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::{unbounded, UnboundedSender},
        executor::block_on,
        StreamExt as _,
    };
    use melib::{
        backends::prelude::*,
        imap::*,
        utils::logging::{LogLevel, Logger},
        Mail,
    };
    use tempfile::TempDir;

    use super::server::*;

    struct ImapTest {
        _logger: Logger,
        _temp_dir: TempDir,
        server_state: Arc<Mutex<ServerState>>,
        server_sender: UnboundedSender<ServerEvent>,
        account_conf: AccountSettings,
        imap_server: ImapServer,
    }

    fn setup(initial_envelopes: Vec<Mail>) -> ImapTest {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let temp_dir = TempDir::new().unwrap();

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

        let next_uid = initial_envelopes.len() + 1;
        let envelopes = initial_envelopes
            .into_iter()
            .enumerate()
            .map(|(i, env)| (i + 1, env))
            .collect();
        let server_state = Arc::new(Mutex::new(ServerState {
            envelopes,
            next_uid,
            uidvalidity: 1,
        }));

        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let local_addr = listener.local_addr().unwrap();
        let (server_sender, server_receiver) = unbounded();
        let listener = smol::Async::new(listener).unwrap();
        let imap_server = ImapServer::new(
            listener,
            (server_sender.clone(), server_receiver),
            Arc::clone(&server_state),
        );

        let account_conf = AccountSettings {
            name: "test".to_string(),
            root_mailbox: "INBOX".to_string(),
            format: "imap".to_string(),
            identity: "user@example.com".to_string(),
            extra_identities: vec![],
            read_only: false,
            display_name: None,
            subscribed_mailboxes: vec![],
            mailboxes: indexmap::indexmap! {},
            manual_refresh: false,
            contacts: indexmap::indexmap! {},
            extra: indexmap::indexmap! {
                "server_hostname".to_string() => local_addr.ip().to_string().into(),
                "server_username".to_string() => "user".to_string().into(),
                "server_password".to_string() => "password".to_string().into(),
                "server_port".to_string() => local_addr.port().to_string().into(),
                "use_starttls".to_string() => "false".to_string().into(),
                "use_tls".to_string() => "false".to_string().into(),
                // Important for testing, because we expect only one connection to be used.
                "use_connection_pool".to_string() => "false".to_string().into(),
                "timeout".to_string() => 0_u64.to_string().into(),
            },
        };

        ImapTest {
            _logger,
            _temp_dir: temp_dir,
            server_state,
            server_sender,
            imap_server,
            account_conf,
        }
    }

    /// Test that `ImapType::watch` `Stream` returns the expected `Refresh`
    /// events when altering the mail store in the IMAP server.
    pub fn run_imap_watch() {
        let ImapTest {
            _logger,
            _temp_dir,
            server_state,
            server_sender,
            account_conf,
            imap_server,
        } = setup(vec![]);

        let imap_server_handle = imap_server.spawn();
        let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                backend_event_queue.lock().unwrap().push_back((ah, be));
            }))
        };

        let mut imap =
            ImapType::new(&account_conf, Default::default(), backend_event_consumer).unwrap();
        let fut = async move {
            assert_eq!(
                imap.mailboxes().unwrap().await.unwrap_err(),
                Error::new("Offline")
            );
            imap.is_online().unwrap().await.unwrap();
            let mut watch_fut = imap.watch().unwrap().into_future();
            let mailboxes = imap.mailboxes().unwrap().await.unwrap();
            let inbox_hash = *mailboxes.keys().next().unwrap();

            // $ date -R -u -r 0
            let new_mail = Box::new(
                Mail::new(
                    br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Date: Thu, 01 Jan 1970 00:00:00 +0000
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
            let new_mail_2 = Box::new(
                Mail::new(
                    br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Date: Thu, 01 Jan 1970 00:00:01 +0000
Subject: RE: your e-mail 2
Message-ID: <h2g7f.z0gy2pgaen6m@example.com>
Content-Type: text/plain

hello world 2.
"#
                    .to_vec(),
                    None,
                )
                .unwrap(),
            );
            let new_mail_3 = Box::new(
                Mail::new(
                    br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Date: Thu, 01 Jan 1970 00:00:02 +0000
Subject: RE: your e-mail 3
Message-ID: <h2g7f.z0gy2pgaen7m@example.com>
Content-Type: text/plain

hello world 3.
"#
                    .to_vec(),
                    None,
                )
                .unwrap(),
            );
            assert!(melib::utils::futures::timeout(
                Some(std::time::Duration::from_millis(10)),
                &mut watch_fut
            )
            .await
            .is_err());
            let wait_handle = std::thread::spawn({
                let server_sender = server_sender.clone();
                let new_mail = new_mail.clone();
                let new_mail_2 = new_mail_2.clone();
                let new_mail_3 = new_mail_3.clone();
                move || {
                    block_on(async move {
                        let (w_s, w_r) = futures::channel::oneshot::channel();
                        server_sender
                            .unbounded_send(
                                ServerStateEvent::WaitForCommand(
                                    "IDLE",
                                    Arc::new(Mutex::new(Some(w_s))),
                                )
                                .into(),
                            )
                            .unwrap();
                        log::debug!("waiting for IDLE..");
                        _ = w_r.await;
                        log::debug!("done waiting for IDLE");
                        server_sender
                            .unbounded_send(ServerStateEvent::New(new_mail.clone()).into())
                            .unwrap();
                        server_sender
                            .unbounded_send(ServerStateEvent::New(new_mail_2.clone()).into())
                            .unwrap();
                        server_sender
                            .unbounded_send(ServerStateEvent::New(new_mail_3.clone()).into())
                            .unwrap();
                    });
                }
            });
            let hash;
            let mut refresh_events = vec![];
            while refresh_events.len() < 3 {
                let (value1, rest) = melib::utils::futures::timeout(
                    Some(std::time::Duration::from_secs(5)),
                    watch_fut,
                )
                .await
                .unwrap();
                let backend_event = value1.unwrap().unwrap();
                match backend_event {
                    BackendEvent::RefreshBatch(events) => {
                        refresh_events.extend(events);
                    }
                    BackendEvent::Refresh(event) => {
                        refresh_events.push(event);
                    }
                    backend_event => {
                        panic!("Expected Refresh event, got: {backend_event:?}");
                    }
                }
                watch_fut = rest.into_future();
            }
            wait_handle.join().unwrap();
            {
                let mailbox = imap.uid_store.mailboxes.lock().await;
                let counters = mailbox.values().next().unwrap().counters.lock().unwrap();
                assert_eq!(counters.total.len(), 3);
                assert_eq!(counters.unseen.len(), 3);
            }
            {
                let mut fetch_fut = imap.fetch(inbox_hash).unwrap().into_future();
                let mut envelopes: Vec<Envelope> = vec![];
                loop {
                    let (envs, rest) = fetch_fut.await;
                    let Some(envs) = envs else {
                        break;
                    };
                    envelopes.extend(envs.unwrap());

                    fetch_fut = rest.into_future();
                }
                envelopes.sort_by_key(|env| env.date());
                for env in &mut envelopes {
                    env.set_hash(EnvelopeHash(0));
                }
                let mut expected = vec![
                    new_mail.envelope.clone(),
                    new_mail_2.envelope.clone(),
                    new_mail_3.envelope.clone(),
                ];
                for env in &mut expected {
                    env.set_hash(EnvelopeHash(0));
                }
                assert_eq!(envelopes, expected);
            }
            {
                let Some(RefreshEvent { kind: RefreshEventKind::Create(ref env), .. }) = refresh_events.iter().find(|refresh_event| matches!(refresh_event.kind, RefreshEventKind::Create(ref env) if env.subject()== "RE: your e-mail")) else {
                    panic!("Expected Create event, got: {refresh_events:?}");
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
                // Simulate another client deleting an email
                server_sender
                    .unbounded_send(
                        ServerStateEvent::Delete(uid, futures::channel::oneshot::channel().0)
                            .into(),
                    )
                    .unwrap();
            }
            let watch_fut = {
                let (value1, rest) = melib::utils::futures::timeout(
                    Some(std::time::Duration::from_secs(5)),
                    watch_fut,
                )
                .await
                .unwrap();
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
                let mailbox = imap.uid_store.mailboxes.lock().await;
                let counters = mailbox.values().next().unwrap().counters.lock().unwrap();
                assert_eq!(counters.total.len(), 2);
                assert_eq!(counters.unseen.len(), 2);
            }
            {
                let mut fetch_fut = imap.fetch(inbox_hash).unwrap().into_future();
                let mut envelopes: Vec<Envelope> = vec![];
                loop {
                    let (envs, rest) = fetch_fut.await;
                    let Some(envs) = envs else {
                        break;
                    };
                    envelopes.extend(envs.unwrap());

                    fetch_fut = rest.into_future();
                }
                envelopes.sort_by_key(|env| env.date());
                for env in &mut envelopes {
                    env.set_hash(EnvelopeHash(0));
                }
                let mut expected = vec![new_mail_2.envelope.clone(), new_mail_3.envelope.clone()];
                for env in &mut expected {
                    env.set_hash(EnvelopeHash(0));
                }
                assert_eq!(envelopes, expected);
            }
            server_sender.unbounded_send(ServerEvent::Quit).unwrap();
            imap_server_handle.join().unwrap();
            let (mut value1, rest) =
                melib::utils::futures::timeout(Some(std::time::Duration::from_secs(5)), watch_fut)
                    .await
                    .unwrap();
            let watch_fut = rest.into_future();
            let err_check_fn = |err: &Error| -> bool {
                matches!(
                    err.kind,
                    ErrorKind::OSError(nix::errno::Errno::EPIPE | nix::errno::Errno::ECONNRESET)
                ) || err.summary == "Disconnected"
                    || matches!(
                        err.kind,
                        ErrorKind::Network(NetworkErrorKind::ConnectionFailed)
                    )
            };
            if matches!(
                value1,
                Some(Ok(
                    BackendEvent::Refresh(RefreshEvent {
                        kind: RefreshEventKind::Failure(ref err),
                        ..
                    })))
                if err_check_fn(err)
            ) {
                value1 = watch_fut.await.0;
            }
            if let Some(val) = value1 {
                if !matches!(val, Err(ref err) if err_check_fn(err)) {
                    panic!(
                        "Expected watch TCP connection to have disconnected with \
                         EPIPE/ECONNRESET, got: {val:?}"
                    );
                }
            }
        };
        std::thread::spawn(move || {
            block_on(fut);
        })
        .join()
        .unwrap();
    }

    /// Test that initial fetch state as well as cache resync works.
    pub fn run_imap_fetch() {
        // $ date -R -u -r 0
        let new_mail = Mail::new(
            br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Date: Thu, 01 Jan 1970 00:00:00 +0000
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#
            .to_vec(),
            None,
        )
        .unwrap();
        let new_mail_2 = Mail::new(
            br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Date: Thu, 01 Jan 1970 00:00:01 +0000
Subject: RE: your e-mail 2
Message-ID: <h2g7f.z0gy2pgaen6m@example.com>
Content-Type: text/plain

hello world 2.
"#
            .to_vec(),
            None,
        )
        .unwrap();
        let new_mail_3 = Mail::new(
            br#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Date: Thu, 01 Jan 1970 00:00:02 +0000
Subject: RE: your e-mail 3
Message-ID: <h2g7f.z0gy2pgaen7m@example.com>
Content-Type: text/plain

hello world 3.
"#
            .to_vec(),
            None,
        )
        .unwrap();
        let ImapTest {
            _logger,
            _temp_dir,
            server_state,
            server_sender,
            account_conf,
            imap_server,
        } = setup(vec![
            new_mail.clone(),
            new_mail_2.clone(),
            new_mail_3.clone(),
        ]);

        let imap_server_handle = imap_server.spawn();

        let fut = async move {
            // Do initial fetch, verify that we see those 3 e-mails.
            {
                let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };

                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                imap.is_online().unwrap().await.unwrap();
                let mailboxes = imap.mailboxes().unwrap().await.unwrap();
                let inbox_hash = *mailboxes.keys().next().unwrap();

                {
                    let mut fetch_fut = imap.fetch(inbox_hash).unwrap().into_future();
                    let mut envelopes: Vec<Envelope> = vec![];
                    loop {
                        let (envs, rest) = fetch_fut.await;
                        let Some(envs) = envs else {
                            break;
                        };
                        envelopes.extend(envs.unwrap());

                        fetch_fut = rest.into_future();
                    }
                    {
                        let mailbox = imap.uid_store.mailboxes.lock().await;
                        let counters = mailbox.values().next().unwrap().counters.lock().unwrap();
                        assert_eq!(counters.total.len(), 3);
                        assert_eq!(counters.unseen.len(), 3);
                    }
                    envelopes.sort_by_key(|env| env.date());
                    imap.delete_messages(envelopes[0].hash.into(), inbox_hash)
                        .unwrap()
                        .await
                        .unwrap();
                    for env in &mut envelopes {
                        env.set_hash(EnvelopeHash(0));
                    }
                    let mut expected = vec![
                        new_mail.envelope.clone(),
                        new_mail_2.envelope.clone(),
                        new_mail_3.envelope.clone(),
                    ];
                    for env in &mut expected {
                        env.set_hash(EnvelopeHash(0));
                    }
                    assert_eq!(envelopes, expected);
                }
                {
                    let mailbox = imap.uid_store.mailboxes.lock().await;
                    let counters = mailbox.values().next().unwrap().counters.lock().unwrap();
                    assert_eq!(counters.total.len(), 2);
                    assert_eq!(counters.unseen.len(), 2);
                }
                {
                    let mut fetch_fut = imap.fetch(inbox_hash).unwrap().into_future();
                    let mut envelopes: Vec<Envelope> = vec![];
                    loop {
                        let (envs, rest) = fetch_fut.await;
                        let Some(envs) = envs else {
                            break;
                        };
                        envelopes.extend(envs.unwrap());

                        fetch_fut = rest.into_future();
                    }
                    envelopes.sort_by_key(|env| env.date());
                    for env in &mut envelopes {
                        env.set_hash(EnvelopeHash(0));
                    }
                    let mut expected =
                        vec![new_mail_2.envelope.clone(), new_mail_3.envelope.clone()];
                    for env in &mut expected {
                        env.set_hash(EnvelopeHash(0));
                    }
                    assert_eq!(envelopes, expected);
                }
            }
            // Do another fetch, this should be read from cache.
            {
                let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };
                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                imap.is_online().unwrap().await.unwrap();
                let mailboxes = imap.mailboxes().unwrap().await.unwrap();
                let inbox_hash = *mailboxes.keys().next().unwrap();

                {
                    let mut fetch_fut = imap.fetch(inbox_hash).unwrap().into_future();
                    let mut envelopes: Vec<Envelope> = vec![];
                    loop {
                        let (envs, rest) = fetch_fut.await;
                        let Some(envs) = envs else {
                            break;
                        };
                        envelopes.extend(envs.unwrap());

                        fetch_fut = rest.into_future();
                    }
                    {
                        let mailbox = imap.uid_store.mailboxes.lock().await;
                        let counters = mailbox.values().next().unwrap().counters.lock().unwrap();
                        assert_eq!(counters.total.len(), 2);
                        assert_eq!(counters.unseen.len(), 2);
                    }
                    envelopes.sort_by_key(|env| env.date());
                    for env in &mut envelopes {
                        env.set_hash(EnvelopeHash(0));
                    }
                    let mut expected =
                        vec![new_mail_2.envelope.clone(), new_mail_3.envelope.clone()];
                    for env in &mut expected {
                        env.set_hash(EnvelopeHash(0));
                    }
                    assert_eq!(envelopes, expected);
                }
            }
            // Simulate another client deleting an e-mail
            let uid_to_delete = {
                let state_lck = server_state.lock().unwrap();
                state_lck
                    .envelopes
                    .iter()
                    .find_map(|(uid, env)| {
                        if env.message_id() == "h2g7f.z0gy2pgaen7m@example.com" {
                            Some(*uid)
                        } else {
                            None
                        }
                    })
                    .unwrap()
            };
            {
                let (s, r) = futures::channel::oneshot::channel();
                server_sender
                    .unbounded_send(ServerStateEvent::Delete(uid_to_delete, s).into())
                    .unwrap();
                r.await.unwrap();
            }

            // Fetch again, and verify that the removed e-mail is detected when resyncing the cache
            {
                let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };
                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                imap.is_online().unwrap().await.unwrap();
                let mailboxes = imap.mailboxes().unwrap().await.unwrap();
                let inbox_hash = *mailboxes.keys().next().unwrap();

                {
                    let mut fetch_fut = imap.fetch(inbox_hash).unwrap().into_future();
                    let mut envelopes: Vec<Envelope> = vec![];
                    loop {
                        let (envs, rest) = fetch_fut.await;
                        let Some(envs) = envs else {
                            break;
                        };
                        envelopes.extend(envs.unwrap());

                        fetch_fut = rest.into_future();
                    }
                    {
                        let mailbox = imap.uid_store.mailboxes.lock().await;
                        let counters = mailbox.values().next().unwrap().counters.lock().unwrap();
                        assert_eq!(counters.total.len(), 1);
                        assert_eq!(counters.unseen.len(), 1);
                    }
                    envelopes.sort_by_key(|env| env.date());
                    // Check if cache is enabled, otherwise this test is pointless
                    let has_cache = imap.uid_store.offline_cache.lock().unwrap().is_some();
                    if has_cache {
                        let deleted_hash = envelopes
                            .iter()
                            .find_map(|env| {
                                if env.message_id() == "h2g7f.z0gy2pgaen7m@example.com" {
                                    Some(env.hash())
                                } else {
                                    None
                                }
                            })
                            .unwrap();
                        for env in &mut envelopes {
                            env.set_hash(EnvelopeHash(0));
                        }
                        let mut expected =
                            vec![new_mail_2.envelope.clone(), new_mail_3.envelope.clone()];
                        for env in &mut expected {
                            env.set_hash(EnvelopeHash(0));
                        }
                        assert_eq!(envelopes, expected);

                        let mut backend_events = backend_event_queue
                            .lock()
                            .unwrap()
                            .drain(..)
                            .filter_map(|(_, be)| {
                                if matches!(be, BackendEvent::Refresh(_)) {
                                    Some(be)
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>();
                        assert_eq!(backend_events.len(), 1, "{backend_events:?}");
                        let backend_event = backend_events.pop().unwrap();
                        let BackendEvent::Refresh(refresh_event) = backend_event else {
                            panic!("Expected Refresh event, got: {backend_event:?}");
                        };
                        let RefreshEventKind::Remove(ref env_hash) = refresh_event.kind else {
                            panic!("Expected Remove event, got: {refresh_event:?}");
                        };
                        assert_eq!(*env_hash, deleted_hash);
                    }
                }
            }
            server_sender.unbounded_send(ServerEvent::Quit).unwrap();
            imap_server_handle.join().unwrap();
        };
        std::thread::spawn(move || {
            block_on(fut);
        })
        .join()
        .unwrap();
    }

    /// Test that various auth mechanisms work as expected
    pub(crate) fn run_imap_auth_mechanisms() {
        let ImapTest {
            _logger,
            _temp_dir,
            server_state: _,
            server_sender,
            mut account_conf,
            imap_server,
        } = setup(vec![]);

        let hostname = imap_server.config.hostname.clone();

        let fut = async move {
            let imap_server_handle = imap_server.spawn();
            let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
            for send_capabilities_on_auth in [true, false] {
                server_sender
                    .unbounded_send(ServerEvent::SendCapabilitiesOnAuth {
                        send_capabilities_on_auth,
                    })
                    .unwrap();
                server_sender
                    .unbounded_send(ServerEvent::ChangeCapabilities {
                        capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Auth(AuthMechanism::OAuthBearer),
                            Capability::SaslIr,
                            Capability::Id,
                        ]
                        .try_into()
                        .unwrap(),
                        authenticated_capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Id,
                            Capability::Idle,
                            Capability::Enable,
                        ]
                        .try_into()
                        .unwrap(),
                    })
                    .unwrap();
                // Try AUTH=OAUTHBEARER: it will fail because the connection is not TLS encrypted
                {
                    account_conf
                        .extra
                        .insert("use_oauth2".into(), "true".into());
                    let backend_event_consumer = {
                        let backend_event_queue = Arc::clone(&backend_event_queue);

                        BackendEventConsumer::new(Arc::new(move |ah, be| {
                            log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                            backend_event_queue.lock().unwrap().push_back((ah, be));
                        }))
                    };
                    let mut imap =
                        ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                            .unwrap();
                    let tls_err = imap.is_online().unwrap().await.unwrap_err();
                    assert_eq!(
                        &tls_err.summary,
                        &format!(
                            "Cannot authenticate to {hostname} with AUTH=OAUTHBEARER: it is not \
                             encrypted with TLS which is required"
                        )
                    );
                    assert_eq!(tls_err.kind, ErrorKind::Configuration);
                    account_conf.extra.shift_remove("use_oauth2");
                }
                // Try AUTH=PLAIN, it should succeed
                server_sender
                    .unbounded_send(ServerEvent::ChangeCapabilities {
                        capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Auth(AuthMechanism::Plain),
                            Capability::SaslIr,
                            Capability::Id,
                        ]
                        .try_into()
                        .unwrap(),
                        authenticated_capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Id,
                            Capability::Idle,
                            Capability::Enable,
                        ]
                        .try_into()
                        .unwrap(),
                    })
                    .unwrap();
                {
                    let backend_event_consumer = {
                        let backend_event_queue = Arc::clone(&backend_event_queue);

                        BackendEventConsumer::new(Arc::new(move |ah, be| {
                            log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                            backend_event_queue.lock().unwrap().push_back((ah, be));
                        }))
                    };
                    let mut imap =
                        ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                            .unwrap();
                    imap.is_online().unwrap().await.unwrap();
                }
                // Try AUTH=ANONYMOUS, it should succeed
                server_sender
                    .unbounded_send(ServerEvent::ChangeCapabilities {
                        capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Auth(AuthMechanism::from(
                                Atom::try_from("ANONYMOUS").unwrap(),
                            )),
                            Capability::SaslIr,
                            Capability::Id,
                        ]
                        .try_into()
                        .unwrap(),
                        authenticated_capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Id,
                            Capability::Idle,
                            Capability::Enable,
                        ]
                        .try_into()
                        .unwrap(),
                    })
                    .unwrap();
                {
                    account_conf
                        .extra
                        .insert("use_auth_anonymous".into(), "true".into());
                    let backend_event_consumer = {
                        let backend_event_queue = Arc::clone(&backend_event_queue);

                        BackendEventConsumer::new(Arc::new(move |ah, be| {
                            log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                            backend_event_queue.lock().unwrap().push_back((ah, be));
                        }))
                    };
                    let mut imap =
                        ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                            .unwrap();
                    imap.is_online().unwrap().await.unwrap();
                    account_conf.extra.shift_remove("use_auth_anonymous");
                }
                // Try authenticating without any AUTH capabilities and LOGINDISABLED, this should fail
                server_sender
                    .unbounded_send(ServerEvent::ChangeCapabilities {
                        capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::LoginDisabled,
                            Capability::SaslIr,
                            Capability::Id,
                        ]
                        .try_into()
                        .unwrap(),
                        authenticated_capabilities: vec![
                            Capability::Imap4Rev1,
                            Capability::Id,
                            Capability::Idle,
                            Capability::Enable,
                        ]
                        .try_into()
                        .unwrap(),
                    })
                    .unwrap();
                {
                    let backend_event_consumer = {
                        let backend_event_queue = Arc::clone(&backend_event_queue);

                        BackendEventConsumer::new(Arc::new(move |ah, be| {
                            log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                            backend_event_queue.lock().unwrap().push_back((ah, be));
                        }))
                    };
                    let mut imap =
                        ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                            .unwrap();
                    let login_disabled_err = imap.is_online().unwrap().await.unwrap_err();
                    assert_eq!(
                        &login_disabled_err.summary,
                        &format!(
                            "Could not connect to {hostname}: server does not accept the LOGIN \
                             command"
                        )
                    );
                    assert_eq!(
                        &login_disabled_err.details.as_deref().unwrap(),
                        &"Server has the capability LOGINDISABLED set. Returned capabilities \
                          were: IMAP4REV1 LOGINDISABLED SASL-IR ID"
                    );
                    assert_eq!(login_disabled_err.kind, ErrorKind::Configuration);
                }
            }
            server_sender.unbounded_send(ServerEvent::Quit).unwrap();
            imap_server_handle.join().unwrap();
        };
        std::thread::spawn(move || {
            block_on(fut);
        })
        .join()
        .unwrap();
    }

    /// Test that TLS works
    pub(crate) fn run_imap_tls() {
        use native_tls::{Identity, TlsAcceptor};

        // Generated with `openssl req -x509 -newkey rsa:4096 -keyout private.key -out
        // certificate.pem  -nodes`
        const PEM: &[u8] = br#"-----BEGIN CERTIFICATE-----
MIIE+DCCAuACCQDfEGhBTDuNrjANBgkqhkiG9w0BAQsFADA+MQswCQYDVQQGEwJH
UjEPMA0GA1UECAwGQXRoZW5zMQ8wDQYDVQQHDAZBdGhlbnMxDTALBgNVBAoMBG1l
bGkwHhcNMjYxMDE5MDczMTI0WhcNMjYxMTE4MDczMTI0WjA+MQswCQYDVQQGEwJH
UjEPMA0GA1UECAwGQXRoZW5zMQ8wDQYDVQQHDAZBdGhlbnMxDTALBgNVBAoMBG1l
bGkwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQCt4VnZKJ7Hcpn8j6b+
5fgQ/Mru841hhGHgdJF7il2pgvp5+3HjgAjeB5sbvgM8BDRW/9rU6rJ3XaCVIvkT
B74APvtJ6L3pFGwX1BvQb4YiVTK950ZGC1S5lAtZhQzMFdpn8CUEHeVqW4JfAA3e
Jdf/MSDg+End45JMg+2vP8+FVdgSL+OrPib0VyXizXh0DfVnXjd/upIfyE68bZHg
3V4k6v1XsqSAlov3oubO4ZGD1bC7m99HMOPRtrCrWsuWnXvZ6zeNw0H+KPD2p0eY
J86AgVrDnL2+bu+H8GJRhc+CEuu7IIu3iqzhpZWOCXlsZsMbL5u+YuUM5uD6vT6a
BHwGp9CAeU0bH7OTME5A+3wtBJFXlF6Nmmib0VNeA0U6zw7AHFFjuCS400FxSWwF
pVKayO9FpX7jlmblJ31T2+B7ph4KQkKS0ndxQdh1Rpp1axvU0Vf6IHyrVJ+Cu7Ji
2qc+jftx4gd5MVq4NNmY5SxWKqPfo38Mu+jl4inl8rIM3OCF2RXLPOEQPJWoq7ho
ET1+Mf8uXug6fOEUWm4Jz4IDzP5/uRC4yXfue4AUHUNFvUIGZfTKJgiOgQXJadN1
sVoQdptaQtLKEKUIkS9zikIaPtRsFZ2MgrIjBlQjJ8VTmo4m6zBpi3b2fNbSdSz/
UONt9nFNhbJqNAK/0NYCm24JEQIDAQABMA0GCSqGSIb3DQEBCwUAA4ICAQBpQtm/
zgtIn6v1vbvHl1q8vVkYuRSHa52DLRkB/YheViCAgMw5wGobzKZofvr/n1TPXh7A
KhbMJUe/TGJ1aJ10ludRpW/KOJpFT2qMExU8lX/KUuYl7oQORGz1aYbnvq5qhKgk
Mu/Ka3gCpk89iWxpslHwzLYbxg9j4XcUp+Di/55e06+yskZCdCQ4OSIl1PZAJE5e
PJjxZLy9BR097YA60arq2ZnYrn1Ek3O5iQxlsS02YrYVf3QoKz8bRbv1Jv8YERC8
8jQF3MQEKQpk/8YHLi4p26uoku49t/RaNLye/wSmKKWxQCoQ3xB1tUCpKL6ydd+h
eJIi72m5lkbDD3LwNlCXd8n/uqvqQ7PMCrMzPy9+wKLfzrW13tx1MnHDtS+D6N0a
RLcliAbwWloXP8hrQbWUEAjkm2cTLxtADtsGAlDepZqyzQo+mVhrpUpDFdOC58hf
AiDUHWxjCDPXGQT4GM+rmRB89N9FXzyi/+bSMbHg6d08KfnAlzBjeflhf4NFv3R8
VfRG7Fb1xBRMFGrwfQogqgitSAoMyJCYU13k8m9RDyEtHVyLkf+G4vItYJhWPRop
ZYGOwh6iiNLipbDplDiGHJazsWcd2iSiGGZsBRcQ+XR4kqXu+3IMWDdCitmSaA9v
rB+srIfyq321mGBob5t9rOlcMJk7Gunv1Zjmhw==
-----END CERTIFICATE-----
"#;

        const KEY: &[u8] = br#"-----BEGIN PRIVATE KEY-----
MIIJQwIBADANBgkqhkiG9w0BAQEFAASCCS0wggkpAgEAAoICAQCt4VnZKJ7Hcpn8
j6b+5fgQ/Mru841hhGHgdJF7il2pgvp5+3HjgAjeB5sbvgM8BDRW/9rU6rJ3XaCV
IvkTB74APvtJ6L3pFGwX1BvQb4YiVTK950ZGC1S5lAtZhQzMFdpn8CUEHeVqW4Jf
AA3eJdf/MSDg+End45JMg+2vP8+FVdgSL+OrPib0VyXizXh0DfVnXjd/upIfyE68
bZHg3V4k6v1XsqSAlov3oubO4ZGD1bC7m99HMOPRtrCrWsuWnXvZ6zeNw0H+KPD2
p0eYJ86AgVrDnL2+bu+H8GJRhc+CEuu7IIu3iqzhpZWOCXlsZsMbL5u+YuUM5uD6
vT6aBHwGp9CAeU0bH7OTME5A+3wtBJFXlF6Nmmib0VNeA0U6zw7AHFFjuCS400Fx
SWwFpVKayO9FpX7jlmblJ31T2+B7ph4KQkKS0ndxQdh1Rpp1axvU0Vf6IHyrVJ+C
u7Ji2qc+jftx4gd5MVq4NNmY5SxWKqPfo38Mu+jl4inl8rIM3OCF2RXLPOEQPJWo
q7hoET1+Mf8uXug6fOEUWm4Jz4IDzP5/uRC4yXfue4AUHUNFvUIGZfTKJgiOgQXJ
adN1sVoQdptaQtLKEKUIkS9zikIaPtRsFZ2MgrIjBlQjJ8VTmo4m6zBpi3b2fNbS
dSz/UONt9nFNhbJqNAK/0NYCm24JEQIDAQABAoICABaUwHVaOByQvny1LyvFM80I
91XG3WJ/rbJXkRkpI/bCosK6q9SuqPgkprkYmRTvOtjdi07BWAH2b8dObmhRRhSE
GWBE0Hz3gP4rNp2ADjxhMiDFazPnfs5G8p5ikAHlCKMc5cG+HDAOyESMHkDuSznB
6JIuzHD7f+Fd9L0gnVOkjVX3UCrJ/IcY1ju5EV6I/Yi6DJ0S3q0v90GzPRGlzfcf
aV09Pu3QH0c+qZjOZHZphvUmTf3R1JRElHb8aJobw5zOVj2b0PMv3/M4UCzrZn77
zYy/y0hy4bqWtj3+Ea/Dw0de96YTZVY8icqTYmh4g/gA28oCz9o4FZmagAnCcVS5
HpLsmt2lSfj0Sy5Folto2X/txXGScYNjLYfchUBP3FqX1r2hJD+VpOEbIr4YvQMx
xS3eCPa68BYu4fX/1CABIWzTpdjBXNdp19OOQGia1da3bCK7rJRm3s77921WL0cq
1Q7sI1M7EQhMY2TvGsvbjKOOq5nBQzanJF9kFEVr+pYpSFyojvi39q7Wn5M2SPG/
2x0VPTvuBi2q3L+w/wbug99JYOdR/opJMpn8C/W8LTc+T+3K+wXTUXXqVozM3CQu
pl1VBtL9pALZw8/U/lpQ05duwuHH4Q3zA2IqR0SfRKMbSlkiPAwBUu7uYfa6vmO4
uU+mnDGSY7l82nfg4pnNAoIBAQDY0r9yWonrwfy8rqS+UBInIp2FIMgiT1JPR21e
Qh9+3G3kkd2FB2h9Dz/VUodv62Nt6WsP4n+zoJuI8r+3FMC0WGtYT4nAfUzSpL4+
jWK1w5WlAAYcQiNnz9hext4JpihE27bwCgYTmW45Iv2fTQLOVJxb8ZW+All/nDeY
DAoLHbCUVIn0ogYe134+G7FaodNDlTo6QtlrVaH63KfrpRToFCH0RxjcAdN1pSxP
9sVtuYMGTPlAMbEIV0T2EB0chpKHixD9dSfI4LtQYG/hZoUhE0zgfJKZfWoIlTEr
W77S0xp2AaGsfPff3VJ0InbUBAralzq6Wcj5LmDZcMcdr5WzAoIBAQDNTEIW7PcL
eB9cq1qkkGLWTn0zvuOU0kbAAuKYpXoHHYXq06myM2ZbiT+ewXnFxxv1210POWh9
p8i7n4Rb1g1h7C7WnloJLRo3lEkmyud6WNTtZKv708eeRPJg0V8Mmg6NVfCCc2CT
cfXe2NOQt2HZHxLQOb4xN0CLuv5hRqV6QwY9nEo/gSTrFAQwXATzyJL+oWP+zCVD
pxF9EAb4Q834xfocQDQ/s2XGwNoRCaxAZ7b5DUS0H+6i1+3DjESJXi8DeHq9Gr0B
n4fiIYhIVF6MS+W3a52ELS2TBD3Ds3UfCph585pnjv9S7DKSai4cKnrOsu4CqmiL
QEwPP8rdpowrAoIBAQCguqmc1a0EDYuTO0t4oNa0GB74/zIc7PwxNNvIUfATFMiJ
9e9mm/VDL7i8JC+uMapFYhExd0//EoH55SllSIcJqbn5fwy4mqoGRIWazShtYY1f
AUOGUyj33cWhjH4wXPHhCZb1HhkfEk/5Hw0vt+DqnyJBrja44fkCdgVoOxR+5QVv
D7Qsmqc62bgFxnWUn/2CTe8aHVAbFyG6WzRHpNp4p+P7t3CgNuJQhZ7xsBbuneSZ
GRp0EeIQMtE2Lv/Fk6SvwdFYci6J5L42eSa2yk7KFAqbJv1ttuCReDc40wwEMOWe
sNruiL0fQGvNF3dK96wsGos4dTJcNZsp1UZlpTGtAoIBABSMrZDd51yeiYUdIdWX
Hk2VTbMASId9BNfzcfsVlUFhEyaOSU4tFvHSqyuY8jd51rKlYotDk9IAGXeuX/FR
W8WRfp68+wYwHY+yqB/Z75GvkqCwkwaCDFkrTZejXYBFa296SrjEE8augY3s5kB2
GrVBBwEQxpjmju9tqEjUaXvzeYxxOH1iz+838bNlgplkWlJilT/L7272Kin6kLgJ
EXT1XDO3uYMm0ekzJKDP9w37U7VaPqZJC/oA4yvPvoyBgPM3/BExu/mNnmxkLuAO
keg5MKVLgFjarNUqd4Q/UlnpFZofaDU1exkxy14cgsEO/3vxLSHXLN/Db9mW6ReG
tEECggEBAJI5rAsSxG0tq0im4KujBQXtryctU8haC7En9m9A7stUWN+my8j/Kwyr
zalUNzy8CWVC1qMnzziCc/bwHUUEJe5C32qLPWlcIszs4pCtSm0rZLw9hVZOCSAI
QiK6epuCP34r8uaYah16hZNQ3b37GrxaZ4DimP85id7uvENUShDN9kj0hZHVdvxy
k0gQeDODW/TB8aY28lKQEDbmxGEAnA13WgeQxctY+AGGbT+NpBcdZKmt4Vi85qVY
1Wz6gnDXNgV7GvJw2HLmFf0m9lHEeXCqVk6SsJ+d7hXzyhPQ1PflcbKnnVtQO56j
8nQi9oXfBbw4nVdP6YIq7jaJM7mQoF8=
-----END PRIVATE KEY-----
"#;

        let ImapTest {
            _logger,
            _temp_dir,
            server_state: _,
            server_sender,
            mut account_conf,
            mut imap_server,
        } = setup(vec![]);

        let identity = Identity::from_pkcs8(PEM, KEY).unwrap();
        imap_server.config.tls_acceptor = Some(TlsAcceptor::new(identity).unwrap());
        let hostname = imap_server.config.hostname.clone();

        account_conf.extra.insert("use_tls".into(), "true".into());
        account_conf
            .extra
            .insert("danger_accept_invalid_certs".into(), "false".into());
        let fut = async move {
            let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
            let imap_server_handle = imap_server.spawn();
            {
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };
                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                let tls_err = imap.is_online().unwrap().await.unwrap_err();
                assert_eq!(
                    &tls_err.summary,
                    &format!("Could not initiate TLS negotiation to {hostname}.")
                );
                assert_eq!(
                    tls_err.kind,
                    ErrorKind::Network(NetworkErrorKind::InvalidTLSConnection)
                );
            }
            account_conf
                .extra
                .insert("danger_accept_invalid_certs".into(), "true".into());
            {
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };
                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                imap.is_online().unwrap().await.unwrap();
            }
            // Try AUTH=OAUTHBEARER, it should succeed with TLS
            server_sender
                .unbounded_send(ServerEvent::ChangeCapabilities {
                    capabilities: vec![
                        Capability::Imap4Rev1,
                        Capability::Auth(AuthMechanism::OAuthBearer),
                        Capability::SaslIr,
                        Capability::Id,
                    ]
                    .try_into()
                    .unwrap(),
                    authenticated_capabilities: vec![
                        Capability::Imap4Rev1,
                        Capability::Id,
                        Capability::Idle,
                        Capability::Enable,
                    ]
                    .try_into()
                    .unwrap(),
                })
                .unwrap();
            {
                account_conf
                    .extra
                    .insert("use_oauth2".into(), "true".into());
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };
                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                imap.is_online().unwrap().await.unwrap();
                account_conf.extra.shift_remove("use_oauth2");
            }
            // Try AUTH=OAUTHBEARER with SASL-IR off
            server_sender
                .unbounded_send(ServerEvent::ChangeCapabilities {
                    capabilities: vec![
                        Capability::Imap4Rev1,
                        Capability::Auth(AuthMechanism::OAuthBearer),
                        Capability::Id,
                    ]
                    .try_into()
                    .unwrap(),
                    authenticated_capabilities: vec![
                        Capability::Imap4Rev1,
                        Capability::Id,
                        Capability::Idle,
                        Capability::Enable,
                    ]
                    .try_into()
                    .unwrap(),
                })
                .unwrap();
            {
                account_conf
                    .extra
                    .insert("use_oauth2".into(), "true".into());
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        log::debug!("BackendEventConsumer: ah {ah:?} be {be:?}");
                        backend_event_queue.lock().unwrap().push_back((ah, be));
                    }))
                };
                let mut imap =
                    ImapType::new(&account_conf, Default::default(), backend_event_consumer)
                        .unwrap();
                imap.is_online().unwrap().await.unwrap();
                account_conf.extra.shift_remove("use_oauth2");
            }
            server_sender.unbounded_send(ServerEvent::Quit).unwrap();
            imap_server_handle.join().unwrap();
        };
        std::thread::spawn(move || {
            block_on(fut);
        })
        .join()
        .unwrap();
    }
}
