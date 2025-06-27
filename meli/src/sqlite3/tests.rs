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

use std::{
    collections::{HashSet, VecDeque},
    io::Read,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use melib::{backends::prelude::*, maildir::MaildirType};
use rusty_fork::rusty_fork_test;
use tempfile::TempDir;

use super::*;
use crate::utilities::tests::{eprint_step_fn, eprintln_ok_fn};

#[test]
fn test_sqlite3_query_to_sql() {
    use melib::{search::query, utils::parsec::Parser};
    assert_eq!(
        "(subject LIKE \"%test%\" ) AND (body_text LIKE \"%i%\" ) ",
        &query_to_sql(&query().parse_complete("subject:test and i").unwrap().1)
    );
    assert_eq!(
        "(subject LIKE \"%github%\" ) OR ((_from LIKE \"%epilys%\" ) AND ((subject LIKE \"%lib%\" \
         ) OR (subject LIKE \"%meli%\" ) ) ) ",
        &query_to_sql(
            &query()
                .parse_complete("subject:github or (from:epilys and (subject:lib or subject:meli))")
                .unwrap()
                .1
        )
    );
}

fn new_maildir_backend(
    temp_dir: &TempDir,
    acc_name: &str,
    event_consumer: BackendEventConsumer,
) -> Result<Box<MaildirType>> {
    let root_mailbox = temp_dir.path().join("inbox");
    {
        std::fs::create_dir(&root_mailbox).expect("Could not create root mailbox directory.");
        for d in &["cur", "new", "tmp"] {
            std::fs::create_dir(root_mailbox.join(d))
                .expect("Could not create root mailbox directory contents.");
        }
    }

    let account_conf = melib::AccountSettings {
        name: acc_name.to_string(),
        root_mailbox: root_mailbox.display().to_string(),
        format: "maildir".to_string(),
        identity: "user@localhost".to_string(),
        extra_identities: vec![],
        read_only: true,
        display_name: None,
        order: Default::default(),
        subscribed_mailboxes: vec!["inbox".into()],
        mailboxes: vec![(
            "inbox".into(),
            melib::conf::MailboxConf {
                extra: indexmap::indexmap! {
                    "path".into() => root_mailbox.display().to_string(),
                },
                ..Default::default()
            },
        )]
        .into_iter()
        .collect(),
        manual_refresh: true,
        extra: indexmap::indexmap! {
            "root_mailbox".into() => root_mailbox.display().to_string(),
        },
    };

    MaildirType::new(&account_conf, Default::default(), event_consumer)
}

rusty_fork_test! {
#[test]
fn test_sqlite3_reindex() {
    use futures::stream::TryStreamExt;

    /// Account name to use throughout the test.
    const ACCOUNT_NAME: &str = "test";
    const DB_FILE_SHOULD_EXIST_ERR_MSG: &str = "A db file should now exist, after indexing.";

    let eprintln_ok = eprintln_ok_fn();
    let mut eprint_step_closure = eprint_step_fn();
    macro_rules! eprint_step {
        ($($arg:tt)+) => {{
            eprint_step_closure(format_args!($($arg)+));
        }};
    }

    /// Helper functions
    mod helpers {
        use super::*;

        /// Helper to convert a GZipped bytes string to text.
        pub(super) fn gz_to_string(bytes: &'static [u8]) -> String {
            use flate2::bufread::GzDecoder;

            let mut gz = GzDecoder::new(bytes);
            let mut s = String::new();
            gz.read_to_string(&mut s).unwrap();
            s
        }

        /// List file entries of `${XDG_DATA_HOME}` dir to see if there's an
        /// sqlite database there or not.
        pub(super) fn list_xdg_data_home_dir_entries() -> Vec<PathBuf> {
            use std::{
                fs::{self, DirEntry},
                io,
                path::Path,
            };

            fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&DirEntry)) -> io::Result<()> {
                if dir.is_dir() {
                    for entry in fs::read_dir(dir)? {
                        let entry = entry?;
                        let path = entry.path();
                        if path.is_dir() {
                            visit_dirs(&path, cb)?;
                        } else {
                            cb(&entry);
                        }
                    }
                }
                Ok(())
            }

            let xdg_data_home =
                std::env::var_os("XDG_DATA_HOME").expect("env var should be present");
            let mut entries = Vec::with_capacity(1);
            let mut collect_file_entries = |direntry: &DirEntry| {
                entries.push(direntry.path());
            };

            visit_dirs(Path::new(&xdg_data_home), &mut collect_file_entries).unwrap();

            entries.sort();
            entries
        }

        /// Helper function to perform a reindex operation, which is async, by
        /// blocking on it to completion.
        pub(super) fn perform_reindex(
            acc_name: Arc<str>,
            collection: melib::Collection,
            backend_mutex: Arc<RwLock<Box<dyn MailBackend>>>,
        ) {
            let reindex_fut = AccountCache::index(acc_name, collection, Arc::clone(&backend_mutex));
            smol::block_on(reindex_fut).unwrap();
        }

        /// Helper function to perform a search operation, which is async, by
        /// blocking on it to completion.
        pub(super) fn perform_search(acc_name: &Arc<str>, query: Query) -> Vec<EnvelopeHash> {
            let search_fut = AccountCache::search(
                Arc::clone(acc_name),
                query,
                (SortField::Date, SortOrder::Desc),
            );
            smol::block_on(search_fut).unwrap()
        }
    }
    use helpers::*;

    let temp_dir = TempDir::new().unwrap();

    eprint_step!("Sanitize environment...");
    for var in [
        "PAGER",
        "MANPATH",
        "EDITOR",
        "MELI_CONFIG",
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
    std::env::set_var("HOME", temp_dir.path());
    std::env::set_var("XDG_CONFIG_HOME", temp_dir.path().join(".config"));
    std::env::set_var(
        "XDG_DATA_HOME",
        temp_dir.path().join(".local").join(".share"),
    );

    eprintln_ok();
    eprint_step!("Create maildir backend we will use for the sqlite3 index database...");
    let backend_event_queue = Arc::new(Mutex::new(VecDeque::with_capacity(16)));

    let backend_event_consumer = {
        let backend_event_queue = Arc::clone(&backend_event_queue);

        BackendEventConsumer::new(Arc::new(move |ah, be| {
            backend_event_queue.lock().unwrap().push_back((ah, be));
        }))
    };

    let mut maildir = new_maildir_backend(&temp_dir, ACCOUNT_NAME, backend_event_consumer)
        .expect("Could not create Maildir backend instance");
    let acc_name = Arc::<str>::from(maildir.account_name.to_string());
    let collection = melib::Collection::default();

    eprintln_ok();
    eprint_step!(
        "Confirm the root mailbox was created by fetching all mailboxes and inspecting their \
         contents..."
    );
    let mailboxes_fut = maildir
        .mailboxes()
        .expect("Could not create mailboxes future");
    let mut mailboxes: HashMap<MailboxHash, Mailbox> = smol::block_on(mailboxes_fut).unwrap();
    assert_eq!(
        mailboxes.len(),
        1,
        "Only one mailbox was expected but got: {mailboxes:?}"
    );
    let root_mailbox_hash: MailboxHash = *mailboxes.keys().next().unwrap();
    let mailbox = mailboxes.remove(&root_mailbox_hash).unwrap();
    assert!(mailbox.is_subscribed());
    assert_eq!(mailbox.hash(), root_mailbox_hash);
    assert_eq!(mailbox.name(), "inbox");
    assert_eq!(mailbox.path(), "inbox");
    assert_eq!(mailbox.children(), &[]);
    assert_eq!(mailbox.special_usage(), SpecialUsageMailbox::Normal);
    assert_eq!(mailbox.count().unwrap(), (0, 0));

    eprintln_ok();
    eprint_step!("Insert actual email into our backend...");

    macro_rules! batch_entry {
        ($path:literal) => {
            (
                gz_to_string(include_bytes!($path).as_slice()).into_bytes(),
                root_mailbox_hash,
                None,
            )
        };
    }
    let mail_batch: Vec<(Vec<u8>, MailboxHash, Option<Flag>)> = vec![
        batch_entry!("../../../melib/tests/data/PATCH-Put-sha1dc-on-a-diet_op.mbox.gz"),
        batch_entry!("../../../melib/tests/data/PATCH-Put-sha1dc-on-a-diet.mbox.gz"),
        batch_entry!("../../../melib/tests/data/git-am-breakage-with-MIME-decoding_op.mbox.gz"),
        batch_entry!("../../../melib/tests/data/git-am-breakage-with-MIME-decoding.mbox.gz"),
    ];

    eprintln_ok();
    eprint_step!(
        "Perform a save operation for {} emails...",
        mail_batch.len()
    );

    let save_batch_fut = maildir
        .save_batch(mail_batch)
        .expect("Could not create save mail batch future");
    smol::block_on(async move { save_batch_fut.try_collect::<Vec<Result<()>>>().await })
        .unwrap()
        .into_iter()
        .collect::<Result<Vec<()>>>()
        .unwrap();

    eprintln_ok();
    eprint_step!(
        "Perform a manual refresh operation, since we have not spawned any watcher threads with \
         `MailBackend::watch` (we do not need to)..."
    );
    let refresh_fut = maildir
        .refresh(root_mailbox_hash)
        .expect("Could not create refresh future");
    smol::block_on(refresh_fut).unwrap();
    assert_eq!(backend_event_queue.lock().unwrap().len(), 4);
    for (ah, ev) in backend_event_queue.lock().unwrap().drain(0..) {
        assert_eq!(ah, maildir.account_hash);
        match ev {
            BackendEvent::Refresh(RefreshEvent {
                account_hash,
                mailbox_hash,
                kind: RefreshEventKind::Create(env),
            }) => {
                assert_eq!(account_hash, maildir.account_hash);
                assert_eq!(mailbox_hash, root_mailbox_hash);
                collection.insert(*env, mailbox_hash);
            }
            other => {
                panic!(
                    "Got unexpected BackendEvent from maildir backend: {other:?}"
                );
            }
        }
    }

    eprintln_ok();
    eprint_step!("Backend setup over, we're now finally ready to test sqlite3 indexing...");
    let backend_mutex = Arc::new(RwLock::new(maildir as Box<dyn MailBackend>));

    assert_eq!(
        list_xdg_data_home_dir_entries(),
        Vec::<PathBuf>::new(),
        "expected no sqlite3 database in XDG_DATA_HOME dir or any other entries"
    );
    assert_eq!(
        AccountCache::db_path(&acc_name).unwrap(),
        None,
        "There should be no database file because we have not performed an indexing yet."
    );
    perform_reindex(
        Arc::clone(&acc_name),
        collection.clone(),
        Arc::clone(&backend_mutex),
    );
    let db_path = AccountCache::db_path(&acc_name)
        .expect(DB_FILE_SHOULD_EXIST_ERR_MSG)
        .expect(DB_FILE_SHOULD_EXIST_ERR_MSG);

    assert_eq!(
        db_path,
        temp_dir
            .path()
            .join(".local")
            .join(".share")
            .join("meli")
            .join("test_index.db")
    );

    assert_eq!(list_xdg_data_home_dir_entries(), vec![db_path.clone()],);
    eprintln_ok();
    eprint_step!("Ensure re-indexing for a second time does not trigger any errors...");
    perform_reindex(
        Arc::clone(&acc_name),
        collection.clone(),
        Arc::clone(&backend_mutex),
    );
    let db_path_2 = AccountCache::db_path(&acc_name)
        .expect(DB_FILE_SHOULD_EXIST_ERR_MSG)
        .expect(DB_FILE_SHOULD_EXIST_ERR_MSG);
    assert_eq!(db_path, db_path_2);
    assert_eq!(list_xdg_data_home_dir_entries(), vec![db_path],);

    eprintln_ok();
    eprint_step!("Search for all envelopes, as a smoke test...");
    let search_results = perform_search(&acc_name, Query::Body(String::new()));
    assert_eq!(
        search_results.len(),
        collection.len(),
        "Expected search results to return all envelopes, but the results size do not match the \
         envelopes we have in total. Search results were: {search_results:?}"
    );
    assert_eq!(
        search_results
            .clone()
            .into_iter()
            .collect::<HashSet<EnvelopeHash>>(),
        *collection.get_mailbox(root_mailbox_hash)
    );
    eprintln_ok();
    eprint_step!(
        "Search for torvalds as a submitter, since he sent all those patches we inserted into the \
         backend. So this should return all envelopes as well..."
    );
    let torvalds_search_results = perform_search(
        &acc_name,
        Query::From("torvalds@linux-foundation.org".into()),
    );
    assert_eq!(
        search_results
            .into_iter()
            .collect::<HashSet<EnvelopeHash>>(),
        torvalds_search_results
            .into_iter()
            .collect::<HashSet<EnvelopeHash>>(),
    );

    eprintln_ok();
    eprint_step!("Search for only specific recipients, which should not return all envelopes...");
    let search_results = perform_search(&acc_name, Query::To("marc.stevens@cwi.nl".into()));
    assert_eq!(
        search_results.len(),
        2,
        "Expected search results to return 2 envelopes but the results were: {search_results:?}"
    );
    assert_eq!(
        search_results
            .into_iter()
            .collect::<HashSet<EnvelopeHash>>(),
        collection
            .get_mailbox(root_mailbox_hash)
            .clone()
            .into_iter()
            .map(|env_hash| collection.get_env(env_hash).clone())
            .filter(|env| env.other_headers()[HeaderName::TO].contains("marc.stevens@cwi.nl"))
            .map(|env| env.hash())
            .collect::<HashSet<EnvelopeHash>>()
    );
    eprintln_ok();
}
}
