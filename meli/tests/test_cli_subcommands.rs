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

use std::{io::Write, path::Path};

use assert_cmd::{assert::OutputAssertExt, Command};
use predicates::prelude::*;
use tempfile::TempDir;

#[test]
fn test_cli_subcommands() {
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

    fn version() {
        // --version is successful
        for arg in ["--version", "-v"] {
            let mut cmd = Command::cargo_bin("meli").unwrap();
            let output = cmd.arg(arg).output().unwrap().assert();
            output.code(0).stdout(predicates::str::starts_with("meli "));
        }
    }

    fn help() {
        // --help is successful
        for arg in ["--help", "-h"] {
            let mut cmd = Command::cargo_bin("meli").unwrap();
            let output = cmd.arg(arg).output().unwrap().assert();
            output
                .code(0)
                .stdout(predicates::str::contains("terminal mail client"))
                .stdout(predicates::str::contains("USAGE"))
                .stdout(predicates::str::contains("FLAGS"))
                .stdout(predicates::str::contains("OPTIONS"))
                .stdout(predicates::str::contains("SUBCOMMANDS"))
                .stdout(predicates::str::contains("create-config"))
                .stdout(predicates::str::contains("test-config"))
                .stdout(predicates::str::contains("tools"))
                .stdout(predicates::str::contains("man"))
                .stdout(predicates::str::contains("install-man"))
                .stdout(predicates::str::contains("compiled-with"))
                .stdout(predicates::str::contains("edit-config"))
                .stdout(predicates::str::contains("help"))
                .stdout(predicates::str::contains("print-app-directories"))
                .stdout(predicates::str::contains("print-config-path"))
                .stdout(predicates::str::contains("print-default-theme"))
                .stdout(predicates::str::contains("print-loaded-themes"))
                .stdout(predicates::str::contains("print-log-path"))
                .stdout(predicates::str::contains("view"));
        }
    }

    fn test_subcommand_succeeds(arg: &str) {
        let mut cmd = Command::cargo_bin("meli").unwrap();
        let output = cmd.arg(arg).output().unwrap().assert();
        output.code(0).stdout(predicates::str::is_empty().not());
    }

    fn test_subcommand_succeeds_empty(arg: &str) {
        let mut cmd = Command::cargo_bin("meli").unwrap();
        let output = cmd.arg(arg).output().unwrap().assert();
        output.code(0).stdout(predicates::str::is_empty());
    }

    fn test_subcommand_install_man(dir: &Path) {
        let mut cmd = Command::cargo_bin("meli").unwrap();
        let output = cmd.arg("install-man").arg(dir).output().unwrap().assert();
        output.code(0).stdout(predicates::str::is_empty().not());
        let mut path = dir.to_path_buf();
        for (man, dir) in [
            ("meli.1", "man1"),
            ("meli.conf.5", "man5"),
            ("meli-themes.5", "man5"),
            ("meli.7", "man7"),
        ] {
            path.push(dir);
            assert!(path.is_dir());
            path.push(man);
            assert!(path.is_file());
            path.pop();
            path.pop();
        }
    }

    fn test_subcommand_config_stdio() {
        {
            let mut cmd = Command::cargo_bin("meli").unwrap();
            let output = cmd.arg("create-config").arg("-").output().unwrap().assert();
            output.code(0).stdout(predicates::str::is_empty().not());
        }
        {
            let mut cmd = Command::cargo_bin("meli").unwrap();
            let output = cmd
                .arg("test-config")
                .arg("-")
                .write_stdin(
                    br#"
[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password_command = "false"
    "#
                    .as_slice(),
                )
                .output()
                .unwrap()
                .assert();
            output.code(0).stdout(predicates::str::is_empty());
        }
    }

    fn test_subcommand_man() {
        for (man, title) in [
            ("meli.1", "MELI(1)"),
            ("meli.conf.5", "MELI.CONF(5)"),
            ("meli-themes.5", "MELI-THEMES(5)"),
            ("meli.7", "MELI(7)"),
        ] {
            let true_true: &[&str] = &["man", "--no-raw", "--gzipped", man];
            let true_false: &[&str] = &["man", "--no-raw", man];
            let false_false: &[&str] = &["man", man];
            let false_true: &[&str] = &["man", "--gzipped", man];
            for gzipped in [true, false] {
                for no_raw in [true, false] {
                    let mut cmd = Command::cargo_bin("meli").unwrap();
                    let args = match (no_raw, gzipped) {
                        (true, true) => true_true,
                        (true, false) => true_false,
                        (false, false) => false_false,
                        (false, true) => false_true,
                    };
                    let output = cmd.args(args).output().unwrap().assert();
                    output.code(0).stdout(predicate::function(|x: &[u8]| {
                        use std::io::Read;

                        use flate2::bufread::GzDecoder;

                        let mut gz = GzDecoder::new(x);
                        let content = if gzipped {
                            let size = gz.header().unwrap().comment().unwrap();

                            let mut v = String::with_capacity(
                                str::parse::<usize>(
                                    std::str::from_utf8(size)
                                        .expect("was not compressed with size comment header"),
                                )
                                .expect("was not compressed with size comment header"),
                            );
                            gz.read_to_string(&mut v)
                                .expect("expected gzipped output but could not decode it.");
                            v
                        } else {
                            assert_eq!(gz.header(), None);
                            let mut v = String::with_capacity(0);
                            gz.read_to_string(&mut v).unwrap_err();
                            String::from_utf8(x.to_vec()).expect("invalid utf-8 content")
                        };
                        if !no_raw && gzipped {
                            assert!(content.contains(man));
                        } else {
                            assert!(content.contains('\u{8}'));
                            assert!(content.contains(title));
                        }

                        true
                    }));
                }
            }
        }
    }

    version();
    help();
    test_subcommand_succeeds("help");
    test_subcommand_succeeds("compiled-with");
    test_subcommand_succeeds("man");
    test_subcommand_man();
    test_subcommand_config_stdio();

    let tmp_dir = TempDir::new().unwrap();

    test_subcommand_install_man(tmp_dir.path());

    fn config_not_exists(conf: &Path) {
        let mut cmd = Command::cargo_bin("meli").unwrap();
        let output = cmd.arg("-c").arg(conf).output().unwrap().assert();
        output
            .code(1)
            .stderr(predicate::eq(
                "Configuration error: Edit the sample configuration and relaunch meli.\n",
            ))
            .stdout(
                predicate::eq(
                    format!(
                        "No configuration found. Would you like to generate one in {path}? [Y/n] \
                         Written example configuration to {path}",
                        path = conf.display()
                    )
                    .as_str(),
                )
                .trim()
                .normalize(),
            );
    }

    let conf_path = tmp_dir.path().join("conf.toml");
    config_not_exists(&conf_path);
    assert!(conf_path.exists());
    assert!(conf_path.is_file());

    {
        let mut conf_file = std::fs::OpenOptions::new()
            .append(true)
            .create(false)
            .open(&conf_path)
            .unwrap();
        conf_file
            .write_all(
                br#"
[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password_command = "false"
    "#,
            )
            .unwrap();
    }
    std::env::set_var("MELI_CONFIG", &conf_path);

    test_subcommand_succeeds_empty("test-config");

    test_subcommand_succeeds("print-app-directories");
    test_subcommand_succeeds("print-config-path");
    test_subcommand_succeeds("print-default-theme");
    test_subcommand_succeeds("print-loaded-themes");
    test_subcommand_succeeds("print-log-path");

    tmp_dir.close().unwrap();
}
