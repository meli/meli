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
