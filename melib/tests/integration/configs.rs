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

#[cfg(feature = "maildir")]
#[test]
fn test_maildir_config() {
    use melib::maildir::Configuration;
    use regex::Regex;
    use tempfile::TempDir;

    let tmp_dir = TempDir::new().unwrap();

    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
        ..Configuration::default()
    };

    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "foo"
root_mailbox = "{}"
format = "maildir"
identity = "foo@example.com"
subscribed_mailboxes = []
    "#,
        tmp_dir.path().display()
    ))
    .unwrap();

    melib::maildir::MaildirType::validate_config(&mut s).unwrap();
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "foo"
root_mailbox = "{}"
format = "maildir"
identity = "foo@example.com"
subscribed_mailboxes = []
rename_regex = ',U=\d\d*'
    "#,
        tmp_dir.path().display()
    ))
    .unwrap();
    assert_eq!(
        melib::maildir::Configuration::new(&s)
            .unwrap()
            .rename_regex
            .unwrap()
            .as_str(),
        config.rename_regex.as_ref().unwrap().as_str()
    );

    melib::maildir::MaildirType::validate_config(&mut s).unwrap();
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "foo"
root_mailbox = "{}"
format = "maildir"
identity = "foo@example.com"
subscribed_mailboxes = []
rename_regex = ",U=\\d\\d*"
    "#,
        tmp_dir.path().display()
    ))
    .unwrap();
    assert_eq!(
        melib::maildir::Configuration::new(&s)
            .unwrap()
            .rename_regex
            .unwrap()
            .as_str(),
        config.rename_regex.as_ref().unwrap().as_str()
    );

    melib::maildir::MaildirType::validate_config(&mut s).unwrap();
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "foo"
root_mailbox = "{}"
format = "maildir"
identity = "foo@example.com"
subscribed_mailboxes = []
rename_regex = ',U=\d\d*'
    "#,
        tmp_dir.path().display()
    ))
    .unwrap();

    assert_eq!(
        melib::maildir::Configuration::new(&s)
            .unwrap()
            .rename_regex
            .unwrap()
            .as_str(),
        config.rename_regex.as_ref().unwrap().as_str()
    );
    melib::maildir::MaildirType::validate_config(&mut s).unwrap();
    _ = tmp_dir.close();
}

#[cfg(feature = "notmuch")]
#[test]
fn test_notmuch_config() {
    use melib::{
        error::{Error, ErrorKind},
        notmuch::NotmuchDb,
    };
    use tempfile::TempDir;

    let tmp_dir = TempDir::new().unwrap();

    let root_mailbox = tmp_dir.path().join("root_mailbox");
    let account_name = "foo";

    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "{account_name}"
root_mailbox = "{}"
format = "notmuch"
identity = "foo@example.com"
    "#,
        root_mailbox.display()
    ))
    .unwrap();

    let notmuch_db_dir = root_mailbox.join(".notmuch");

    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new(format!(
            "Notmuch `root_mailbox` {} for account {account_name} does not exist.",
            root_mailbox.display()
        ))
        .set_related_path(Some(root_mailbox.clone()))
        .set_kind(ErrorKind::Configuration)
    );
    std::fs::write(&root_mailbox, b"foo").unwrap();
    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new(format!(
            "Notmuch `root_mailbox` {} for account {account_name} is not a directory.",
            root_mailbox.display()
        ))
        .set_related_path(Some(root_mailbox.clone()))
        .set_kind(ErrorKind::Configuration)
    );
    std::fs::remove_file(&root_mailbox).unwrap();
    std::fs::create_dir(&root_mailbox).unwrap();
    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new(format!(
            "Notmuch `root_mailbox` {} for account {account_name} does not contain a `.notmuch` \
             subdirectory.",
            root_mailbox.display()
        ))
        .set_related_path(Some(notmuch_db_dir.clone()))
        .set_kind(ErrorKind::Configuration)
    );
    std::fs::create_dir(&notmuch_db_dir).unwrap();
    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new(format!(
            "Notmuch account `{account_name}` requires mailboxes explicitly set, since they are \
             virtual, but none are configured. Try adding some.",
        ))
        .set_kind(ErrorKind::Configuration)
    );

    // Check for missing "query" settings.
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "{account_name}"
root_mailbox = "{}"
format = "notmuch"
identity = "foo@example.com"
mailboxes = {{ "INBOX" = {{}}, "Drafts" = {{ query="tag:draft" }}}}
    "#,
        root_mailbox.display()
    ))
    .unwrap();

    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new(format!(
            "notmuch mailbox configuration entry `INBOX` for account {account_name} should have a \
             `query` value set."
        ))
        .set_kind(ErrorKind::Configuration)
    );

    // Check for invalid "parent" setting.
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "{account_name}"
root_mailbox = "{}"
format = "notmuch"
identity = "foo@example.com"
mailboxes = {{ "INBOX" = {{ query="tag:inbox", parent = "doesnotexist" }}, "Drafts" = {{ query="tag:draft" }}}}
    "#,
        root_mailbox.display()
    ))
    .unwrap();

    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new(
            "Mailbox configuration for `INBOX` defines its parent mailbox as `doesnotexist` but \
             no mailbox exists with this exact name."
        )
        .set_kind(ErrorKind::Configuration)
    );

    // Check for "parent" cycle
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "{account_name}"
root_mailbox = "{}"
format = "notmuch"
identity = "foo@example.com"
mailboxes = {{ "INBOX" = {{ query="tag:inbox", parent = "Drafts" }}, "Drafts" = {{ query="tag:draft", parent="INBOX"}}}}
    "#,
        root_mailbox.display()
    ))
    .unwrap();

    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new("Found cycle in mailbox hierarchy: INBOX->Drafts")
            .set_kind(ErrorKind::Configuration)
    );

    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "{account_name}"
root_mailbox = "{}"
format = "notmuch"
identity = "foo@example.com"
mailboxes = {{ "INBOX" = {{ query="tag:inbox", parent = "Drafts" }}, "Drafts" = {{ query="tag:draft", parent="Sent"}}, "Sent" = {{ query="tag:sent", parent = "INBOX" }} }}
    "#,
        root_mailbox.display()
    ))
    .unwrap();

    assert_eq!(
        NotmuchDb::validate_config(&mut s).unwrap_err(),
        Error::new("Found cycle in mailbox hierarchy: INBOX->Drafts->Sent")
            .set_kind(ErrorKind::Configuration)
    );

    // Check valid configuration
    let mut s: melib::AccountSettings = toml::from_str(&format!(
        r#"
name = "{account_name}"
root_mailbox = "{}"
format = "notmuch"
identity = "foo@example.com"
mailboxes = {{ "INBOX" = {{  query="tag:inbox", subscribe = true }}, "Drafts" = {{ query="tag:draft", subscribe = true }}, "Sent" = {{ query="from:username@example.com from:username2@example.com", subscribe = true }}}}
    "#,
        root_mailbox.display()
    ))
    .unwrap();
    NotmuchDb::validate_config(&mut s).unwrap();

    _ = tmp_dir.close();
}
