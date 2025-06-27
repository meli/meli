//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use std::path::PathBuf;

use melib::{
    backends::{prelude::*, Mailbox, MailboxHash},
    error::Result,
    maildir::MaildirType,
    smol, MailboxPermissions, SpecialUsageMailbox,
};
use tempfile::TempDir;

use crate::{
    accounts::{AccountConf, FileMailboxConf, MailboxEntry, MailboxStatus},
    command::actions::MailboxOperation,
    utilities::tests::{eprint_step_fn, eprintln_ok_fn},
};

#[test]
fn test_mailbox_utf7() {
    #[derive(Debug)]
    struct TestMailbox(String);

    impl melib::BackendMailbox for TestMailbox {
        fn hash(&self) -> MailboxHash {
            unimplemented!()
        }

        fn name(&self) -> &str {
            &self.0
        }

        fn path(&self) -> &str {
            &self.0
        }

        fn children(&self) -> &[MailboxHash] {
            unimplemented!()
        }

        fn clone(&self) -> Mailbox {
            unimplemented!()
        }

        fn special_usage(&self) -> SpecialUsageMailbox {
            unimplemented!()
        }

        fn parent(&self) -> Option<MailboxHash> {
            unimplemented!()
        }

        fn permissions(&self) -> MailboxPermissions {
            unimplemented!()
        }

        fn is_subscribed(&self) -> bool {
            unimplemented!()
        }

        fn set_is_subscribed(&mut self, _: bool) -> Result<()> {
            unimplemented!()
        }

        fn set_special_usage(&mut self, _: SpecialUsageMailbox) -> Result<()> {
            unimplemented!()
        }

        fn count(&self) -> Result<(usize, usize)> {
            unimplemented!()
        }

        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
            self
        }
    }
    for (n, d) in [
        ("~peter/mail/&U,BTFw-/&ZeVnLIqe-", "~peter/mail/台北/日本語"),
        ("&BB4EQgQ,BEAEMAQyBDsENQQ9BD0ESwQ1-", "Отправленные"),
    ] {
        let ref_mbox = TestMailbox(n.to_string());
        let mut conf: melib::MailboxConf = Default::default();
        conf.extra.insert("encoding".to_string(), "utf7".into());

        let entry = MailboxEntry::new(
            MailboxStatus::None,
            n.to_string(),
            Box::new(ref_mbox),
            FileMailboxConf {
                mailbox_conf: conf,
                ..Default::default()
            },
        );
        assert_eq!(&entry.path, d);
    }
}

fn new_maildir_backend(
    temp_dir: &TempDir,
    acc_name: &str,
    event_consumer: BackendEventConsumer,
    with_root_mailbox: bool,
) -> Result<(PathBuf, AccountConf, Box<MaildirType>)> {
    let root_mailbox = temp_dir.path().join("inbox");
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
        vec!["inbox".into()]
    } else {
        vec![]
    };
    let mailboxes = if with_root_mailbox {
        vec![(
            "inbox".into(),
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

    let account_conf = melib::AccountSettings {
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
    Ok((root_mailbox, account_conf.into(), maildir))
}

#[test]
fn test_accounts_mailbox_by_path_error_msg() {
    const ACCOUNT_NAME: &str = "test";

    let eprintln_ok = eprintln_ok_fn();
    let mut eprint_step_closure = eprint_step_fn();
    macro_rules! eprint_step {
        ($($arg:tt)+) => {{
            eprint_step_closure(format_args!($($arg)+));
        }};
    }
    let temp_dir = TempDir::new().unwrap();
    {
        eprint_step!(
            "Create maildir backend with a root mailbox, \"inbox\" which will be a valid maildir \
             folder because it will contain cur, new, tmp subdirectories..."
        );
        let mut ctx = crate::Context::new_mock(&temp_dir);
        let backend_event_queue = Arc::new(std::sync::Mutex::new(
            std::collections::VecDeque::with_capacity(16),
        ));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                backend_event_queue.lock().unwrap().push_back((ah, be));
            }))
        };

        let (root_mailbox, settings, maildir) =
            new_maildir_backend(&temp_dir, ACCOUNT_NAME, backend_event_consumer, true).unwrap();
        eprintln_ok();
        let name = maildir.account_name.to_string();
        let account_hash = maildir.account_hash;
        let backend = maildir as Box<dyn MailBackend>;
        let ref_mailboxes = smol::block_on(backend.mailboxes().unwrap()).unwrap();
        let contacts = melib::contacts::Contacts::new(name.to_string());

        let mut account = super::Account {
            hash: account_hash,
            name: name.into(),
            is_online: super::IsOnline::True,
            mailbox_entries: Default::default(),
            mailboxes_order: Default::default(),
            tree: Default::default(),
            contacts,
            collection: backend.collection(),
            settings,
            main_loop_handler: ctx.main_loop_handler.clone(),
            active_jobs: HashMap::default(),
            active_job_instants: std::collections::BTreeMap::default(),
            event_queue: IndexMap::default(),
            backend_capabilities: backend.capabilities(),
            backend: Arc::new(std::sync::RwLock::new(backend)),
        };
        account.init(ref_mailboxes).unwrap();
        while let Ok(thread_event) = ctx.receiver.try_recv() {
            if let crate::ThreadEvent::JobFinished(job_id) = thread_event {
                if !account.process_event(&job_id) {
                    assert!(
                        ctx.accounts[0].process_event(&job_id),
                        "unclaimed job id: {job_id:?}"
                    );
                }
            }
        }
        eprint_step!("Assert that mailbox_by_path(\"inbox\") returns the root mailbox...");
        account.mailbox_by_path("inbox").unwrap();
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"box\") returns an error mentioning the root mailbox..."
        );
        assert_eq!(
            account.mailbox_by_path("box").unwrap_err().to_string(),
            Error {
                summary: "Mailbox with that path not found.".into(),
                details: Some(
                    "Some matching paths that were found: [\"inbox\"]. You can inspect the list \
                     of mailbox paths of an account with the manage-mailboxes command."
                        .into()
                ),
                source: None,
                inner: None,
                related_path: None,
                kind: ErrorKind::NotFound
            }
            .to_string()
        );
        eprintln_ok();

        macro_rules! wait_for_job {
            ($job_id:expr) => {{
                let wait_for = $job_id;
                while let Ok(thread_event) = ctx.receiver.recv() {
                    if let crate::ThreadEvent::JobFinished(job_id) = thread_event {
                        if !account.process_event(&job_id) {
                            assert!(
                                ctx.accounts[0].process_event(&job_id),
                                "unclaimed job id: {:?}",
                                job_id
                            );
                        } else if job_id == wait_for {
                            break;
                        }
                    }
                }
            }};
        }
        eprint_step!(
            "Create new mailboxes: \"Sent\", \"Trash\", \"Drafts\", \"Archive\", \"Outbox\", \
             \"Archive/Archive (old)\"..."
        );
        wait_for_job!(account
            .mailbox_operation(MailboxOperation::Create("Sent".to_string()))
            .unwrap());
        wait_for_job!(account
            .mailbox_operation(MailboxOperation::Create("Trash".to_string()))
            .unwrap());
        wait_for_job!(account
            .mailbox_operation(MailboxOperation::Create("Drafts".to_string()))
            .unwrap());
        wait_for_job!(account
            .mailbox_operation(MailboxOperation::Create("Archive".to_string()))
            .unwrap());
        wait_for_job!(account
            .mailbox_operation(MailboxOperation::Create("Outbox".to_string()))
            .unwrap());
        wait_for_job!(account
            .mailbox_operation(MailboxOperation::Create(
                "inbox/Archive/Archive (old)".to_string(),
            ))
            .unwrap());
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"rchive\") returns an error and mentions matching \
             archives with mailboxes with the least depth in the tree hierarchy of mailboxes \
             mentioned first..."
        );
        assert_eq!(
            account.mailbox_by_path("rchive").unwrap_err().to_string(),
            Error {
                summary: "Mailbox with that path not found.".into(),
                details: Some(
                    "Some matching paths that were found: [\"inbox/Archive\", \
                     \"inbox/Archive/Archive (old)\"]. You can inspect the list of mailbox paths \
                     of an account with the manage-mailboxes command."
                        .into()
                ),
                source: None,
                inner: None,
                related_path: None,
                kind: ErrorKind::NotFound
            }
            .to_string()
        );
        eprintln_ok();
        eprint_step!("Create \"inbox/Archive/Archive{{1,2,3,4,5,6,7,8,9,10}}\" mailboxes...");
        for i in 1..=10 {
            wait_for_job!(account
                .mailbox_operation(MailboxOperation::Create(format!(
                    "inbox/Archive/Archive{i}"
                )))
                .unwrap());
        }
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"inbox/Archive/Archive{{n}}\") works, i.e. we have to \
             specify the root prefix \"inbox\"..."
        );
        for i in 1..=10 {
            account
                .mailbox_by_path(&format!("inbox/Archive/Archive{i}"))
                .unwrap();
            account
                .mailbox_by_path(&format!("Archive/Archive{i}"))
                .unwrap_err();
        }
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"rchive\") returns and error and truncates the matching \
             mailbox paths to 5 maximum..."
        );
        assert_eq!(
            account.mailbox_by_path("rchive").unwrap_err().to_string(),
            Error {
                summary: "Mailbox with that path not found.".into(),
                details: Some(
                    "Some matching paths that were found: [\"inbox/Archive\", \
                     \"inbox/Archive/Archive1\", \"inbox/Archive/Archive2\", \
                     \"inbox/Archive/Archive3\", \"inbox/Archive/Archive4\"] and 7 others. You \
                     can inspect the list of mailbox paths of an account with the \
                     manage-mailboxes command."
                        .into()
                ),
                source: None,
                inner: None,
                related_path: None,
                kind: ErrorKind::NotFound
            }
            .to_string()
        );
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"inbox/Archive\") returns a valid result (since the \
             root mailbox is a valid maildir folder)..."
        );
        account.mailbox_by_path("inbox/Archive").unwrap();
        eprintln_ok();

        eprint_step!("Cleanup maildir account with valid root mailbox...");
        std::fs::remove_dir_all(root_mailbox).unwrap();
        eprintln_ok();
    }

    {
        eprint_step!(
            "Create maildir backend with a root mailbox, \"inbox\" which will NOT be a valid \
             maildir folder because it will NOT contain cur, new, tmp subdirectories..."
        );
        let mut ctx = crate::Context::new_mock(&temp_dir);
        let backend_event_queue = Arc::new(std::sync::Mutex::new(
            std::collections::VecDeque::with_capacity(16),
        ));

        let backend_event_consumer = {
            let backend_event_queue = Arc::clone(&backend_event_queue);

            BackendEventConsumer::new(Arc::new(move |ah, be| {
                backend_event_queue.lock().unwrap().push_back((ah, be));
            }))
        };

        let (_root_mailbox, settings, maildir) =
            new_maildir_backend(&temp_dir, ACCOUNT_NAME, backend_event_consumer, false).unwrap();
        eprintln_ok();
        let name = maildir.account_name.to_string();
        let account_hash = maildir.account_hash;
        let backend = maildir as Box<dyn MailBackend>;
        let ref_mailboxes = smol::block_on(backend.mailboxes().unwrap()).unwrap();
        eprint_step!("Assert that created account has no mailboxes at all...");
        assert!(
            ref_mailboxes.is_empty(),
            "ref_mailboxes were not empty: {ref_mailboxes:?}"
        );
        eprintln_ok();
        let contacts = melib::contacts::Contacts::new(name.to_string());

        let mut account = super::Account {
            hash: account_hash,
            name: name.into(),
            is_online: super::IsOnline::True,
            mailbox_entries: Default::default(),
            mailboxes_order: Default::default(),
            tree: Default::default(),
            contacts,
            collection: backend.collection(),
            settings,
            main_loop_handler: ctx.main_loop_handler.clone(),
            active_jobs: HashMap::default(),
            active_job_instants: std::collections::BTreeMap::default(),
            event_queue: IndexMap::default(),
            backend_capabilities: backend.capabilities(),
            backend: Arc::new(std::sync::RwLock::new(backend)),
        };
        account.init(ref_mailboxes).unwrap();
        while let Ok(thread_event) = ctx.receiver.try_recv() {
            if let crate::ThreadEvent::JobFinished(job_id) = thread_event {
                if !account.process_event(&job_id) {
                    assert!(
                        ctx.accounts[0].process_event(&job_id),
                        "unclaimed job id: {job_id:?}"
                    );
                }
            }
        }
        eprint_step!(
            "Assert that mailbox_by_path(\"inbox\") does not return a valid result (there are no \
             mailboxes)..."
        );
        assert_eq!(
            account.mailbox_by_path("inbox").unwrap_err().to_string(),
            Error {
                summary: "Mailbox with that path not found.".into(),
                details: Some(
                    "You can inspect the list of mailbox paths of an account with the \
                     manage-mailboxes command."
                        .into()
                ),
                source: None,
                inner: None,
                related_path: None,
                kind: ErrorKind::NotFound
            }
            .to_string()
        );
        eprintln_ok();
        eprint_step!(
            "Create multiple maildir folders \"inbox/Archive{{1,2,3,4,5,6,7,8,9,10}}\"..."
        );
        macro_rules! wait_for_job {
            ($job_id:expr) => {{
                let wait_for = $job_id;
                while let Ok(thread_event) = ctx.receiver.recv() {
                    if let crate::ThreadEvent::JobFinished(job_id) = thread_event {
                        if !account.process_event(&job_id) {
                            assert!(
                                ctx.accounts[0].process_event(&job_id),
                                "unclaimed job id: {:?}",
                                job_id
                            );
                        } else if job_id == wait_for {
                            break;
                        }
                    }
                }
            }};
        }
        for i in 1..=10 {
            wait_for_job!(account
                .mailbox_operation(MailboxOperation::Create(format!("inbox/Archive{i}")))
                .unwrap());
        }
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"Archive{{n}}\") works, and that we don't have to \
             specify the root prefix \"inbox\"..."
        );
        for i in 1..=10 {
            account.mailbox_by_path(&format!("Archive{i}")).unwrap();
        }
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"rchive\") returns an error message with matches..."
        );
        assert_eq!(
            account.mailbox_by_path("rchive").unwrap_err().to_string(),
            Error {
                summary: "Mailbox with that path not found.".into(),
                details: Some(
                    "Some matching paths that were found: [\"Archive1\", \"Archive2\", \
                     \"Archive3\", \"Archive4\", \"Archive5\"] and 5 others. You can inspect the \
                     list of mailbox paths of an account with the manage-mailboxes command."
                        .into()
                ),
                source: None,
                inner: None,
                related_path: None,
                kind: ErrorKind::NotFound
            }
            .to_string()
        );
        eprintln_ok();
        eprint_step!(
            "Assert that mailbox_by_path(\"inbox/Archive{{n}}\") does not return a valid result..."
        );
        assert_eq!(
            account
                .mailbox_by_path("inbox/Archive1")
                .unwrap_err()
                .to_string(),
            Error {
                summary: "Mailbox with that path not found.".into(),
                details: Some(
                    "You can inspect the list of mailbox paths of an account with the \
                     manage-mailboxes command."
                        .into()
                ),
                source: None,
                inner: None,
                related_path: None,
                kind: ErrorKind::NotFound
            }
            .to_string()
        );
        eprintln_ok();
    }
}
