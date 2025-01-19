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

#[cfg(feature = "maildir")]
use std::{
    collections::VecDeque,
    path::PathBuf,
    sync::{Arc, Mutex},
};

#[cfg(feature = "maildir")]
use futures::{
    executor::block_on,
    future::{self, Either},
    StreamExt,
};
#[cfg(feature = "maildir")]
use melib::{
    backends::prelude::*,
    maildir::*,
    utils::logging::{LogLevel, StderrLogger},
    Mail,
};
#[cfg(feature = "maildir")]
use tempfile::TempDir;

#[cfg(feature = "maildir")]
fn new_maildir_backend(
    temp_dir: &TempDir,
    acc_name: &str,
    event_consumer: BackendEventConsumer,
    with_root_mailbox: bool,
) -> Result<(PathBuf, AccountSettings, Box<MaildirType>)> {
    let root_mailbox = temp_dir.path().join("INBOX");
    {
        std::fs::create_dir(&root_mailbox).expect("Could not create root mailbox directory.");
        if with_root_mailbox {
            for d in &["cur", "new", "tmp"] {
                std::fs::create_dir(root_mailbox.join(d))
                    .expect("Could not create root mailbox directory contents.");
            }
        }
    }
    let subscribed_mailboxes = if with_root_mailbox {
        vec!["INBOX".into()]
    } else {
        vec![]
    };
    let mailboxes = if with_root_mailbox {
        vec![(
            "INBOX".into(),
            melib::conf::MailboxConf {
                extra: indexmap::indexmap! {
                    "path".into() => root_mailbox.display().to_string(),
                },
                ..Default::default()
            },
        )]
        .into_iter()
        .collect()
    } else {
        indexmap::indexmap! {}
    };
    let extra = if with_root_mailbox {
        indexmap::indexmap! {
            "root_mailbox".into() => root_mailbox.display().to_string(),
        }
    } else {
        indexmap::indexmap! {}
    };

    let account_conf = AccountSettings {
        name: acc_name.to_string(),
        root_mailbox: root_mailbox.display().to_string(),
        format: "maildir".to_string(),
        identity: "user@localhost".to_string(),
        extra_identities: vec![],
        read_only: false,
        display_name: None,
        order: Default::default(),
        subscribed_mailboxes,
        mailboxes,
        manual_refresh: true,
        extra,
    };

    let maildir = MaildirType::new(&account_conf, Default::default(), event_consumer)?;
    Ok((root_mailbox, account_conf, maildir))
}

#[cfg(feature = "maildir")]
#[test]
/// Test that `MaildirType::watch` `Stream` returns the expected `Refresh`
/// events when altering the mail store in the filesystem.
fn test_maildir_watch() {
    let mut _logger = StderrLogger::new(LogLevel::TRACE);
    let temp_dir = TempDir::new().unwrap();
    let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));

    let backend_event_consumer = {
        let backend_event_queue = Arc::clone(&backend_event_queue);

        BackendEventConsumer::new(Arc::new(move |ah, be| {
            eprintln!("BackendEventConsumer: ah {:?} be {:?}", ah, be);
            backend_event_queue.lock().unwrap().push_back((ah, be));
        }))
    };

    let (root_mailbox, _settings, mut maildir) =
        new_maildir_backend(&temp_dir, "maildir", backend_event_consumer.clone(), true).unwrap();

    let is_online_fut = maildir.is_online().unwrap();
    block_on(is_online_fut).unwrap();
    let mut mailboxes_fut = maildir.mailboxes().unwrap();
    let mailboxes = block_on(mailboxes_fut.as_mut()).unwrap();
    let inbox_hash: MailboxHash = *mailboxes.keys().next().unwrap();

    let watch_fut = {
        let fut = maildir.watch().unwrap().into_future();
        smol::unblock(move || futures::executor::block_on(fut))
    };
    eprintln!(
        "Create a new email using MaildirType::save_to_mailbox() and assert that the watch stream \
         yields a RefreshEventKind::Create for this envelope."
    );
    let new_mail = Mail::new(
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
    .unwrap();
    MaildirType::save_to_mailbox(root_mailbox.clone(), new_mail.bytes, None).unwrap();
    let (value1, _watch_fut) = block_on(watch_fut);
    let inbox_env: Envelope = {
        let backend_event = value1.unwrap().unwrap();
        let BackendEvent::Refresh(refresh_event) = backend_event else {
            panic!("Expected Refresh event, got: {:?}", backend_event);
        };
        let RefreshEventKind::Create(env) = refresh_event.kind else {
            panic!("Expected Create event, got: {:?}", refresh_event);
        };
        assert_eq!(env.subject(), "RE: your e-mail");
        assert_eq!(env.message_id(), "h2g7f.z0gy2pgaen5m@example.com");
        assert_eq!(inbox_hash, refresh_event.mailbox_hash);
        *env
    };
    eprintln!(
        "Move envelope to newly created Trash folder and assert we receive a Remove event for the \
         original email, and a Create event for the new envelope in the Trash folder"
    );
    let (trash_hash, _mailboxes) =
        block_on(maildir.create_mailbox("Trash".into()).unwrap()).unwrap();
    eprintln!(
        "trash_hash = {:?} inbox_hash = {:?}",
        trash_hash, inbox_hash
    );
    block_on(
        maildir
            .copy_messages(inbox_env.hash().into(), inbox_hash, trash_hash, true)
            .unwrap(),
    )
    .unwrap();
    let (value1, _watch_fut) = block_on(_watch_fut.into_future());
    let mut backend_events = vec![];
    backend_events.push(value1.unwrap().unwrap());
    let (value1, _watch_fut) = block_on(_watch_fut.into_future());
    backend_events.push(value1.unwrap().unwrap());
    {
        let Some(pos) = backend_events.iter().position(|be| matches!(be, BackendEvent::Refresh(ref refresh_event) if matches!(refresh_event.kind, RefreshEventKind::Remove(_)))) else {
        panic!("No Remove event for Inbox found in received backend events: {:?}", backend_events);
    };
        let backend_event = backend_events.remove(pos);
        let BackendEvent::Refresh(refresh_event) = backend_event else {
            panic!("Expected Refresh event, got: {:?}", backend_event);
        };
        let RefreshEventKind::Remove(old_hash) = refresh_event.kind else {
            panic!("Expected Remove event, got: {:?}", refresh_event);
        };
        assert_eq!(old_hash, inbox_env.hash());
        assert_eq!(
            refresh_event.mailbox_hash, inbox_hash,
            "Expected Remove event in Inbox folder, got: {:?}",
            refresh_event
        );
    }
    let trash_env_hash: EnvelopeHash = {
        let Some(pos) = backend_events.iter().position(|be| matches!(be, BackendEvent::Refresh(ref refresh_event) if matches!(refresh_event.kind, RefreshEventKind::Create(_)))) else {
        panic!("No Create event for Trash found in received backend events: {:?}", backend_events);
    };
        let backend_event = backend_events.remove(pos);
        let BackendEvent::Refresh(refresh_event) = backend_event else {
            panic!("Expected Refresh event, got: {:?}", backend_event);
        };
        let RefreshEventKind::Create(ref trash_env) = refresh_event.kind else {
            panic!("Expected Create event, got: {:?}", refresh_event);
        };
        assert_eq!(
            refresh_event.mailbox_hash, trash_hash,
            "Expected Create event in Trash folder, got: {:?}",
            refresh_event
        );
        assert_eq!(trash_env.message_id(), inbox_env.message_id());
        trash_env.hash()
    };
    assert!(backend_events.is_empty());
    drop(backend_events);
    eprintln!("Delete envelope from Trash folder and assert we receive a Remove event");
    block_on(
        maildir
            .delete_messages(trash_env_hash.into(), trash_hash)
            .unwrap(),
    )
    .unwrap();
    let (backend_event, _watch_fut) = match block_on(future::select(
        _watch_fut.into_future(),
        maildir.refresh(trash_hash).unwrap(),
    )) {
        Either::Left((watch_event, _refresh)) => {
            let (value1, watch_rest) = watch_event;
            let backend_event = value1.unwrap().unwrap();
            (backend_event, watch_rest.into_future())
        }
        Either::Right((refresh_success, _watch_fut)) => {
            refresh_success.unwrap();
            (
                backend_event_queue.lock().unwrap().pop_back().unwrap().1,
                _watch_fut,
            )
        }
    };
    let BackendEvent::Refresh(refresh_event) = backend_event else {
        panic!("Expected Refresh event, got: {:?}", backend_event);
    };
    let RefreshEventKind::Remove(old_hash) = refresh_event.kind else {
        panic!("Expected Remove event, got: {:?}", refresh_event);
    };
    assert_eq!(
        refresh_event.mailbox_hash, trash_hash,
        "Expected Remove event in Trash folder, got: {:?}",
        refresh_event
    );
    assert_eq!(old_hash, trash_env_hash);
}
