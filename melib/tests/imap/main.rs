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
}

pub mod server {
    use std::{
        collections::{HashSet, VecDeque},
        convert::TryInto,
        net::{TcpListener, TcpStream},
        num::NonZeroU32,
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::{unbounded, UnboundedReceiver, UnboundedSender},
        executor::block_on,
        future::{self, Either},
        io::{AsyncReadExt, AsyncWriteExt},
        stream::{FuturesUnordered, StreamExt},
        FutureExt,
    };
    use imap_codec::{
        decode::Decoder,
        encode::{Encoder, Fragment},
        imap_types, CommandCodec, ResponseCodec,
    };
    use imap_types::{
        auth::AuthMechanism,
        core::{LiteralMode, NString, Vec1},
        fetch::MessageDataItem,
        response::{Capability, Code, CommandContinuationRequest, Data, Response, Status},
    };
    use melib::{backends::prelude::*, imap::*, parser::BytesExt, smol::Async, Mail};

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

        fn recv(&mut self, server_event: ServerEvent) -> StreamEvent {
            match server_event {
                ServerEvent::Quit => StreamEvent::Quit,
                ServerEvent::New(new_mail) => {
                    let uid = self.insert(new_mail);
                    StreamEvent::Untagged(UntaggedEvent::New(uid))
                }
                ServerEvent::Delete(uid, on_success) => {
                    eprintln!(
                        "removing uid = {uid} mail = {:?}",
                        self.envelopes.shift_remove(&uid)
                    );
                    _ = on_success.send(true);
                    StreamEvent::Untagged(UntaggedEvent::Delete(uid))
                }
                ServerEvent::Expunge(reply) => {
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
                        eprintln!(
                            "removing uid = {uid} mail = {:?}",
                            self.envelopes.shift_remove(uid)
                        );
                    }
                    _ = reply.send(uids.clone());

                    StreamEvent::Untagged(UntaggedEvent::Deletes(uids))
                }
                ServerEvent::WaitForCommand(cmd, notifier) => {
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
        New(Box<Mail>),
        Delete(UID, futures::channel::oneshot::Sender<bool>),
        Expunge(futures::channel::oneshot::Sender<Vec<UID>>),
        WaitForCommand(
            &'static str,
            Arc<Mutex<Option<futures::channel::oneshot::Sender<()>>>>,
        ),
        Quit,
    }

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    enum UntaggedEvent {
        New(UID),
        Delete(UID),
        Deletes(Vec<UID>),
    }

    #[derive(Clone)]
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
            Self {
                listener,
                command_sender,
                command_receiver,
                state,
                config: ImapServerConfig {
                    capabilities,
                    authenticated_capabilities,
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
                        config,
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
                            let event = state.lock().unwrap().recv(server_event);
                            for stream in streams.values_mut() {
                                stream.sender.unbounded_send(event.clone()).unwrap();
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
                                    let idx = streams.len();
                                    let (sender, stream_receiver) = unbounded();
                                    let mut stream = ImapServerStream::new(
                                        tcp_stream,
                                        state.clone(),
                                        command_sender.clone(),
                                        config.clone(),
                                        stream_receiver,
                                    );
                                    let join_handle = std::thread::spawn(move || {
                                        block_on(async move { while !stream.next().await {} });
                                    });
                                    streams.insert(
                                        idx,
                                        StreamHandle {
                                            sender,
                                            join_handle,
                                        },
                                    );
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
                    eprintln!("waiting on {} stream handles", streams.len());
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

    struct ImapServerStream {
        tcp_stream: Async<TcpStream>,
        idle_cmd_id: Option<String>,
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
                .field("idle_cmd_id", &self.idle_cmd_id)
                .field("state", &self.state)
                .field("session_state", &self.session_state)
                .finish_non_exhaustive()
        }
    }

    impl ImapServerStream {
        fn new(
            tcp_stream: Async<TcpStream>,
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
                idle_cmd_id: None,
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
                tcp_stream: &mut Async<TcpStream>,
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
                    ref mut idle_cmd_id,
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
                if let Some(ref idle_cmd_id) = idle_cmd_id {
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
                    eprintln!("loop_handler 'idle received: {input:?}");
                    if input == "DONE\r\n" {
                        tcp_stream.write_all(idle_cmd_id.as_bytes()).await.unwrap();
                        tcp_stream
                            .write_all(b" OK IDLE terminated\r\n")
                            .await
                            .unwrap();
                        self.idle_cmd_id.take();
                        return false;
                    }
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
                    eprintln!("loop_handler 'main received: {line:?}");
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
                                assert_eq!(mechanism, AuthMechanism::Plain);
                                let initial_response = initial_response.expect("password");
                                let password = initial_response.declassify();
                                assert_eq!(password.as_ref(), b"\0user\0password");
                                *session_state = SessionState::Authenticated;
                            } else {
                                unimplemented!();
                            }
                            responses.push(Response::Status(
                                Status::ok(Some(id), None, "Welcome").unwrap(),
                            ));
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
                            *idle_cmd_id = Some(id.inner().to_string());
                            eprintln!("loop_handler is now idling");
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
                                    .unbounded_send(ServerEvent::Expunge(s))
                                    .unwrap();
                                let expunged = r.await.unwrap();
                                eprintln!("CLOSE silently expunged following uids: {expunged:?}");
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
                                        .unbounded_send(ServerEvent::Expunge(s))
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
                                                    other => unimplemented!("{other:?}"),
                                                }
                                            }
                                        }
                                        other => unimplemented!("{other:?}"),
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
                    }
                    return logout;
                }
                return false;
            }
        }

        async fn recv(&mut self) -> Option<bool> {
            let Self {
                ref mut tcp_stream,
                ref mut idle_cmd_id,
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
                        if let Some(idle_cmd_id) = idle_cmd_id.take() {
                            eprintln!("IDLE loop_handler received ServerEvent::Quit from 'main");
                            tcp_stream
                                .write_all(b"* BYE cruel world\r\n")
                                .await
                                .unwrap();
                            tcp_stream.write_all(idle_cmd_id.as_bytes()).await.unwrap();
                            tcp_stream
                                .write_all(b" OK IDLE terminated\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        } else {
                            eprintln!("main loop_handler received ServerEvent::Quit");
                            tcp_stream
                                .write_all(b"* BYE cruel world\r\n")
                                .await
                                .unwrap();
                            tcp_stream.flush().await.unwrap();
                        }
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
                        if cmd == "IDLE" && idle_cmd_id.is_some() {
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

mod tests {
    use std::{
        collections::VecDeque,
        net::TcpListener,
        sync::{Arc, Mutex},
    };

    use futures::{
        channel::mpsc::{unbounded, UnboundedSender},
        executor::block_on,
        StreamExt,
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
        imap_server_handle: std::thread::JoinHandle<()>,
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
        let imap_server_handle = ImapServer::new(
            listener,
            (server_sender.clone(), server_receiver),
            Arc::clone(&server_state),
        )
        .spawn();

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
            extra: indexmap::indexmap! {
                "server_hostname".to_string() => local_addr.ip().to_string(),
                "server_username".to_string() => "user".to_string(),
                "server_password".to_string() => "password".to_string(),
                "server_port".to_string() => local_addr.port().to_string(),
                "use_starttls".to_string() => "false".to_string(),
                "use_tls".to_string() => "false".to_string(),
                // Important for testing, because we expect only one connection to be used.
                "use_connection_pool".to_string() => "false".to_string(),
                "timeout".to_string() => 0_u64.to_string(),
            },
        };

        ImapTest {
            _logger,
            _temp_dir: temp_dir,
            server_state,
            server_sender,
            imap_server_handle,
            account_conf,
        }
    }

    /// Test that `ImapType::watch` `Stream` returns the expected `Refresh`
    /// events when altering the mail store in the IMAP server.
    pub(crate) fn run_imap_watch() {
        let ImapTest {
            _logger,
            _temp_dir,
            server_state,
            server_sender,
            account_conf,
            imap_server_handle,
        } = setup(vec![]);

        let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                eprintln!("BackendEventConsumer: ah {ah:?} be {be:?}");
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
                            .unbounded_send(ServerEvent::WaitForCommand(
                                "IDLE",
                                Arc::new(Mutex::new(Some(w_s))),
                            ))
                            .unwrap();
                        eprintln!("waiting for IDLE..");
                        _ = w_r.await;
                        eprintln!("done waiting for IDLE");
                        server_sender
                            .unbounded_send(ServerEvent::New(new_mail.clone()))
                            .unwrap();
                        server_sender
                            .unbounded_send(ServerEvent::New(new_mail_2.clone()))
                            .unwrap();
                        server_sender
                            .unbounded_send(ServerEvent::New(new_mail_3.clone()))
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
                let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
                assert_eq!(exists_lck.len(), 3);
                let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
                assert_eq!(unseen_lck.len(), 3);
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
                    .unbounded_send(ServerEvent::Delete(
                        uid,
                        futures::channel::oneshot::channel().0,
                    ))
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
                let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
                assert_eq!(exists_lck.len(), 2);
                let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
                assert_eq!(unseen_lck.len(), 2);
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
    pub(crate) fn run_imap_fetch() {
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
            imap_server_handle,
        } = setup(vec![
            new_mail.clone(),
            new_mail_2.clone(),
            new_mail_3.clone(),
        ]);

        let fut = async move {
            // Do initial fetch, verify that we see those 3 e-mails.
            {
                let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        eprintln!("BackendEventConsumer: ah {ah:?} be {be:?}");
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
                        let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
                        assert_eq!(exists_lck.len(), 3);
                        let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
                        assert_eq!(unseen_lck.len(), 3);
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
                    let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
                    assert_eq!(exists_lck.len(), 2);
                    let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
                    assert_eq!(unseen_lck.len(), 2);
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
                        eprintln!("BackendEventConsumer: ah {ah:?} be {be:?}");
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
                        let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
                        assert_eq!(exists_lck.len(), 2);
                        let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
                        assert_eq!(unseen_lck.len(), 2);
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
                    .unbounded_send(ServerEvent::Delete(uid_to_delete, s))
                    .unwrap();
                r.await.unwrap();
            }

            // Fetch again, and verify that the removed e-mail is detected when resyncing the cache
            {
                let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));
                let backend_event_consumer = {
                    let backend_event_queue = Arc::clone(&backend_event_queue);

                    BackendEventConsumer::new(Arc::new(move |ah, be| {
                        eprintln!("BackendEventConsumer: ah {ah:?} be {be:?}");
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
                        let exists_lck = mailbox.values().next().unwrap().exists.lock().unwrap();
                        assert_eq!(exists_lck.len(), 1);
                        let unseen_lck = mailbox.values().next().unwrap().unseen.lock().unwrap();
                        assert_eq!(unseen_lck.len(), 1);
                    }
                    envelopes.sort_by_key(|env| env.date());
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
