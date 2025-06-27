//
// melib
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of melib.
//
// melib is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// melib is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with melib. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use regex::Regex;
use tempfile::TempDir;

use crate::{
    backends::prelude::*,
    email::Flag,
    error::Result,
    maildir::{
        utilities::{move_to_cur, MaildirFilePathExt, MaildirMailbox},
        Configuration, MaildirType,
    },
};

fn set_flags(config: &Configuration, path: &Path, flag_ops: &[FlagOp]) -> Result<PathBuf> {
    let mut new_flags = path.flags();
    for op in flag_ops.iter() {
        if let FlagOp::Set(f) | FlagOp::UnSet(f) = op {
            new_flags.set(*f, op.as_bool());
        }
    }

    path.set_flags(new_flags, config)
}

#[test]
fn test_maildir_move_to_cur_rename() {
    let config = Configuration::default();
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1:2,FRS")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:2,FRS")
    );
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1:2,")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:2,")
    );
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1:1,")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:1,:2,")
    );
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:2,")
    );
}

#[test]
fn test_maildir_move_to_cur_rename_regexp() {
    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
        ..Configuration::default()
    };
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo:2,S")
    );
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=1:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo:2,S")
    );
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo,U=:2,S")
    );
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo:2,S")
    );
}

#[test]
fn test_maildir_set_flags() {
    let config = Configuration::default();

    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1:2,FRS"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,FRS").to_path_buf()),
        "Setting the same flags should not change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1:2,FRS"),
            &[FlagOp::UnSet(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,").to_path_buf()),
        "UnSetting all the set flags should change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1:2,FRS"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::TRASHED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,FRST").to_path_buf()),
        "Setting new flags should change the path to include them"
    );
}

#[test]
fn test_maildir_set_flags_regexp() {
    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
        ..Configuration::default()
    };

    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,S"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS").to_path_buf()),
        "Setting the same flags should not change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS"),
            &[FlagOp::UnSet(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,").to_path_buf()),
        "UnSetting all the set flags should change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::TRASHED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRST").to_path_buf()),
        "Setting new flags should change the path to include them"
    );
}

#[test]
fn test_maildir_place_in_dir() {
    let config = Configuration::default();

    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1:2,")
            .place_in_dir(Path::new("/path/to/new/"), &config),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change the parent dir",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1:2,")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1:2,").to_path_buf()),
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1:2,FRS")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1:2,FRS").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change flags",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1:2,").to_path_buf()),
        "place_in_dir() should add missing `:2,` substring"
    );
}

#[test]
fn test_maildir_place_in_dir_regexp() {
    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
        ..Configuration::default()
    };

    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,")
            .place_in_dir(Path::new("/path/to/new/"), &config),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo:2,").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change the parent dir",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1.foo:2,").to_path_buf()),
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1.foo:2,FRS").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change flags",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1.foo:2,").to_path_buf()),
        "place_in_dir() should add missing `:2,` substring"
    );
}

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
            crate::conf::MailboxConf {
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

#[test]
fn test_maildir_mailbox_paths() {
    let temp_dir = TempDir::new().unwrap();
    let backend_event_queue = Arc::new(std::sync::Mutex::new(
        std::collections::VecDeque::with_capacity(16),
    ));

    let backend_event_consumer = {
        let backend_event_queue = Arc::clone(&backend_event_queue);

        BackendEventConsumer::new(Arc::new(move |ah, be| {
            backend_event_queue.lock().unwrap().push_back((ah, be));
        }))
    };

    // Perform tests on a maildir backend where the root mailbox, is not a valid
    // maildir mailbox (e.g. has no cur,new,tmp sub directories.
    {
        let (root_mailbox, _settings, maildir) =
            new_maildir_backend(&temp_dir, "maildir", backend_event_consumer.clone(), false)
                .unwrap();
        assert!(!maildir.config.is_root_a_mailbox);
        // Assert that giving a file system path to MaildirBox::new is valid
        let new_mailbox = MaildirMailbox::new(
            root_mailbox.join("Archive").display().to_string(),
            "Archive".into(),
            None,
            vec![],
            true,
            &maildir.config,
        )
        .unwrap();
        assert_eq!(new_mailbox.name, "Archive");
        assert_eq!(&new_mailbox.path, &Path::new("Archive"));
        assert_eq!(&new_mailbox.fs_path, &root_mailbox.join("Archive"));
        // Assert that giving a mailbox path to MaildirBox::new is valid
        let new_mailbox = MaildirMailbox::new(
            "INBOX/Archive".into(),
            "Archive".into(),
            None,
            vec![],
            true,
            &maildir.config,
        )
        .unwrap();
        assert_eq!(new_mailbox.name, "Archive");
        assert_eq!(&new_mailbox.path, &Path::new("Archive"));
        assert_eq!(&new_mailbox.fs_path, &root_mailbox.join("Archive"));
        let mut backend = maildir as Box<dyn MailBackend>;
        let ref_mailboxes = smol::block_on(backend.mailboxes().unwrap()).unwrap();
        // Assert that backend has no mailboxes at all");
        assert!(
            ref_mailboxes.is_empty(),
            "ref_mailboxes were not empty: {ref_mailboxes:?}"
        );
        let (new_hash, ref_mailboxes) =
            smol::block_on(backend.create_mailbox("Archive".into()).unwrap()).unwrap();
        assert_eq!(ref_mailboxes[&new_hash].name(), "Archive");
        assert_eq!(ref_mailboxes[&new_hash].path(), "Archive");
        assert_eq!(
            ref_mailboxes[&new_hash]
                .as_any()
                .downcast_ref::<MaildirMailbox>()
                .unwrap()
                .fs_path(),
            &root_mailbox.join("Archive")
        );
        // Assert that even if we accidentally give a file system path to a maildir
        // backend's create_mailbox() method, it still does the correct thing.
        let (new_hash, ref_mailboxes) = smol::block_on(
            backend
                .create_mailbox(root_mailbox.join("Archive2").display().to_string())
                .unwrap(),
        )
        .unwrap();
        assert_eq!(ref_mailboxes[&new_hash].name(), "Archive2");
        assert_eq!(ref_mailboxes[&new_hash].path(), "Archive2");
        assert_eq!(
            ref_mailboxes[&new_hash]
                .as_any()
                .downcast_ref::<MaildirMailbox>()
                .unwrap()
                .fs_path(),
            &root_mailbox.join("Archive2")
        );
        let ref_mailboxes = smol::block_on(backend.mailboxes().unwrap()).unwrap();
        // Assert that backend has all the created mailboxes so far
        assert_eq!(
            ref_mailboxes.len(),
            2,
            "mailboxes() return value content not what expected: {ref_mailboxes:?}"
        );
        // Assert that giving an absolute path returns an error
        assert_eq!(
            &smol::block_on(backend.create_mailbox("/Archive3".to_string()).unwrap())
                .unwrap_err()
                .to_string(),
            "Path given (`/Archive3`) is absolute. Please provide a path relative to the \
             account's root mailbox."
        );
        // Assert that attempting to create a mailbox outside of the root mailbox
        // returns an error
        assert_eq!(
            &smol::block_on(
                backend
                    .create_mailbox(temp_dir.path().join("Archive3").display().to_string())
                    .unwrap()
            )
            .unwrap_err()
            .to_string(),
            &format!(
                "Path given, `{}`, is not included in the root mailbox path `{}`. A maildir \
                 backend cannot contain mailboxes outside of its root path.",
                temp_dir.path().join("Archive3").display(),
                root_mailbox.display(),
            )
        );

        std::fs::remove_dir_all(root_mailbox).unwrap();
    }

    // Perform same tests on a maildir backend where the root mailbox is a valid
    // maildir mailbox (e.g. has cur,new,tmp sub directories.
    {
        let (root_mailbox, _settings, maildir) =
            new_maildir_backend(&temp_dir, "maildir", backend_event_consumer, true).unwrap();
        assert!(maildir.config.is_root_a_mailbox);
        // Assert that giving a file system path to MaildirBox::new is valid
        let new_mailbox = MaildirMailbox::new(
            root_mailbox.join("Archive").display().to_string(),
            "Archive".into(),
            None,
            vec![],
            true,
            &maildir.config,
        )
        .unwrap();
        assert_eq!(new_mailbox.name, "Archive");
        assert_eq!(&new_mailbox.path, &Path::new("INBOX/Archive"));
        assert_eq!(&new_mailbox.fs_path, &root_mailbox.join("Archive"));
        // Assert that giving a mailbox path to MaildirBox::new is valid
        let new_mailbox = MaildirMailbox::new(
            "INBOX/Archive".into(),
            "Archive".into(),
            None,
            vec![],
            true,
            &maildir.config,
        )
        .unwrap();
        assert_eq!(new_mailbox.name, "Archive");
        assert_eq!(&new_mailbox.path, &Path::new("INBOX/Archive"));
        assert_eq!(&new_mailbox.fs_path, &root_mailbox.join("Archive"));
        let mut backend = maildir as Box<dyn MailBackend>;
        let ref_mailboxes = smol::block_on(backend.mailboxes().unwrap()).unwrap();
        // Assert that backend has only INBOX as a mailbox
        assert_eq!(
            ref_mailboxes.len(),
            1,
            "ref_mailboxes is not just INBOX: {ref_mailboxes:?}"
        );
        // Assert that creating a mailbox without the root mailbox as a prefix does the
        // correct thing.
        let (new_hash, ref_mailboxes) =
            smol::block_on(backend.create_mailbox("Archive".into()).unwrap()).unwrap();
        assert_eq!(ref_mailboxes[&new_hash].name(), "Archive");
        assert_eq!(ref_mailboxes[&new_hash].path(), "INBOX/Archive");
        assert_eq!(
            ref_mailboxes[&new_hash]
                .as_any()
                .downcast_ref::<MaildirMailbox>()
                .unwrap()
                .fs_path(),
            &root_mailbox.join("Archive")
        );
        // Assert that creating a mailbox with the root mailbox as a prefix does the
        // correct thing.
        let (new_hash, ref_mailboxes) =
            smol::block_on(backend.create_mailbox("INBOX/Archive2".into()).unwrap()).unwrap();
        assert_eq!(ref_mailboxes[&new_hash].name(), "Archive2");
        assert_eq!(ref_mailboxes[&new_hash].path(), "INBOX/Archive2");
        assert_eq!(
            ref_mailboxes[&new_hash]
                .as_any()
                .downcast_ref::<MaildirMailbox>()
                .unwrap()
                .fs_path(),
            &root_mailbox.join("Archive2")
        );
        // Assert that even if we accidentally give a file system path to a maildir
        // backend's create_mailbox() method, it still does the correct thing.
        let (new_hash, ref_mailboxes) = smol::block_on(
            backend
                .create_mailbox(root_mailbox.join("Archive3").display().to_string())
                .unwrap(),
        )
        .unwrap();
        assert_eq!(ref_mailboxes[&new_hash].name(), "Archive3");
        assert_eq!(ref_mailboxes[&new_hash].path(), "INBOX/Archive3");
        assert_eq!(
            ref_mailboxes[&new_hash]
                .as_any()
                .downcast_ref::<MaildirMailbox>()
                .unwrap()
                .fs_path(),
            &root_mailbox.join("Archive3")
        );
        let ref_mailboxes = smol::block_on(backend.mailboxes().unwrap()).unwrap();
        // Assert that backend has all the created mailboxes so far
        assert_eq!(
            ref_mailboxes.len(),
            4,
            "mailboxes() return value content not what expected: {ref_mailboxes:?}",
        );
        // Assert that giving an absolute path returns an error
        assert_eq!(
            &smol::block_on(backend.create_mailbox("/Archive4".to_string()).unwrap())
                .unwrap_err()
                .to_string(),
            "Path given (`/Archive4`) is absolute. Please provide a path relative to the \
             account's root mailbox."
        );
        // Assert that attempting to create a mailbox outside of the root mailbox
        // returns an error
        assert_eq!(
            &smol::block_on(
                backend
                    .create_mailbox(temp_dir.path().join("Archive4").display().to_string())
                    .unwrap()
            )
            .unwrap_err()
            .to_string(),
            &format!(
                "Path given, `{}`, is not included in the root mailbox path `{}`. A maildir \
                 backend cannot contain mailboxes outside of its root path.",
                temp_dir.path().join("Archive4").display(),
                root_mailbox.display(),
            )
        );
    }
}
