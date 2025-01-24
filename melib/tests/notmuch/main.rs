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

#[cfg(feature = "notmuch")]
use rusty_fork::rusty_fork_test;

#[cfg(feature = "notmuch")]
rusty_fork_test! {
    #[test]
    fn test_notmuch_watch() {
        tests::run_notmuch_watch();
    }
}

#[cfg(feature = "notmuch")]
mod tests {
    use std::{
        collections::VecDeque,
        path::PathBuf,
        sync::{Arc, Mutex},
    };

    use futures::{executor::block_on, StreamExt};
    use melib::{
        backends::prelude::*,
        maildir::MaildirType,
        notmuch::*,
        utils::logging::{LogLevel, StderrLogger},
        Mail,
    };
    use tempfile::TempDir;

    macro_rules! skip_test_if_notmuch_binary_is_missing {
        () => {{
            if !matches!(std::process::Command::new("sh")
                .arg("-c")
                .arg("command -v notmuch")
                .stdout(std::process::Stdio::null())
                .stdin(std::process::Stdio::null())
                .stderr(std::process::Stdio::null()).output(), Ok(out) if out.status.success())
                {
                    log::info!("'notmuch' binary not found in PATH, skipping test.");
                    return;
                }
        }}
    }

    fn notmuch_new(block: bool) {
        let mut cmd = std::process::Command::new("notmuch");
        cmd.arg("new")
            .arg("--verbose")
            .arg("--full-scan")
            .stdout(std::process::Stdio::piped())
            .stdin(std::process::Stdio::null())
            .stderr(std::process::Stdio::piped());
        if block {
            log::info!(
                "'notmuch new' synchronous output: {:?}",
                cmd.output().unwrap()
            );
        } else {
            std::thread::spawn(move || {
                log::info!(
                    "'notmuch new' asynchronous output: {:?}",
                    cmd.output().unwrap()
                );
            });
        }
    }

    fn new_notmuch_backend(
        temp_dir: &TempDir,
        acc_name: &str,
        event_consumer: BackendEventConsumer,
        with_root_mailbox: bool,
    ) -> Result<(PathBuf, AccountSettings, Box<NotmuchDb>)> {
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
        std::env::set_var("NOTMUCH_CONFIG", temp_dir.path().join("notmuch-config"));
        std::fs::write(
            temp_dir.path().join("notmuch-config"),
            format!(
                r#"[database]
path={}

[user]
name=Test Suite
primary_email=test@example.com
other_email=test2@example.com;test3@example.com
"#,
                root_mailbox.display()
            ),
        )
        .unwrap();
        notmuch_new(true);
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
                        "query".into() => "".to_string(),
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
            format: "notmuch".to_string(),
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

        let notmuch = NotmuchDb::new(&account_conf, Default::default(), event_consumer)?;
        Ok((root_mailbox, account_conf, notmuch))
    }

    /// Test that `NotmuchDb::watch` `Stream` returns the expected `Refresh`
    /// events when altering the mail store in the filesystem.
    pub(crate) fn run_notmuch_watch() {
        let mut _logger = StderrLogger::new(LogLevel::TRACE);
        skip_test_if_notmuch_binary_is_missing!();
        let temp_dir = TempDir::new().unwrap();
        // Store all events in a vector, and compare them at the end with the expected
        // ones.
        let mut backend_events: Vec<BackendEvent> = vec![];
        let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                eprintln!("BackendEventConsumer: ah {:?} be {:?}", ah, be);
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
            "NOTMUCH_CONFIG",
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

        let (root_mailbox, _settings, mut notmuch) =
            new_notmuch_backend(&temp_dir, "notmuch", backend_event_consumer.clone(), true)
                .unwrap();

        let is_online_fut = notmuch.is_online().unwrap();
        block_on(is_online_fut).unwrap();
        let mut mailboxes_fut = notmuch.mailboxes().unwrap();
        let mailboxes = block_on(mailboxes_fut.as_mut()).unwrap();
        let inbox_hash: MailboxHash = *mailboxes.keys().next().unwrap();

        let watch_fut = {
            let fut = notmuch.watch().unwrap().into_future();
            smol::unblock(move || futures::executor::block_on(fut))
        };
        eprintln!(
            "Create a new email using MaildirType::save_to_mailbox() and assert that the watch \
             stream yields a RefreshEventKind::Create for this envelope."
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
        let mail_path =
            MaildirType::save_to_mailbox(root_mailbox.clone(), new_mail.bytes, None).unwrap();
        notmuch_new(false);
        let (value1, _watch_fut) = block_on(watch_fut);
        backend_events.push(value1.unwrap().unwrap());
        eprintln!("Delete envelope from Inbox folder and assert we receive a Remove event");
        std::fs::remove_file(&mail_path).unwrap();
        notmuch_new(false);
        let (value1, _watch_fut) = block_on(_watch_fut.into_future());
        backend_events.push(value1.unwrap().unwrap());
        // We don't expect more events, but if there are, drain them to show them in the
        // failed assertion later.
        backend_events.extend(
            backend_event_queue
                .lock()
                .unwrap()
                .drain(..)
                .map(|(_, ev)| ev),
        );
        assert_eq!(
            backend_events.len(),
            2,
            "Expected two events total; backend_events was: {:?}",
            backend_events
        );
        block_on(notmuch.refresh(inbox_hash).unwrap()).unwrap();
        backend_events.extend(
            backend_event_queue
                .lock()
                .unwrap()
                .drain(..)
                .map(|(_, ev)| ev),
        );
        assert_eq!(
            backend_events.len(),
            2,
            "Expected two events total; backend_events was: {:?}",
            backend_events
        );

        let env = {
            let BackendEvent::Refresh(refresh_event) = &backend_events[0] else {
                panic!("Expected Refresh event, got: {:#?}", &backend_events[0]);
            };
            let RefreshEventKind::Create(ref env) = refresh_event.kind else {
                panic!("Expected Create event, got: {:#?}", refresh_event);
            };
            assert_eq!(env.subject(), "RE: your e-mail");
            assert_eq!(env.message_id(), "h2g7f.z0gy2pgaen5m@example.com");
            assert_eq!(refresh_event.mailbox_hash, inbox_hash);
            env
        };
        {
            let BackendEvent::Refresh(refresh_event) = &backend_events[1] else {
                panic!("Expected Refresh event, got: {:#?}", &backend_events[1]);
            };
            let RefreshEventKind::Remove(old_hash) = refresh_event.kind else {
                panic!("Expected Remove event, got: {:#?}", refresh_event);
            };
            assert_eq!(
                refresh_event.mailbox_hash, inbox_hash,
                "Expected Remove event in Inbox folder, got: {:#?}",
                refresh_event
            );
            assert_eq!(old_hash, env.hash());
        }
    }
}
