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
    borrow::Cow,
    fmt::Write as FmtWrite,
    fs::{self, OpenOptions},
    io::Write,
    path::PathBuf,
};

use crate::{
    conf::{themes::*, FileSettings},
    terminal::Color,
};

pub struct ConfigFile {
    pub path: PathBuf,
    pub file: fs::File,
}

impl ConfigFile {
    pub fn new(
        content: &str,
        dir: &tempfile::TempDir,
    ) -> std::result::Result<Self, std::io::Error> {
        let mut filename = String::with_capacity(2 * 16);
        for byte in melib::utils::random::random_u64().to_be_bytes() {
            write!(&mut filename, "{:02X}", byte).unwrap();
        }
        let mut path = dir.path().to_path_buf();
        path.push(&*filename);
        let mut file = OpenOptions::new()
            .create_new(true)
            .append(true)
            .open(&path)?;
        file.write_all(content.as_bytes())?;
        Ok(Self { path, file })
    }
}

impl Drop for ConfigFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

pub const TEST_CONFIG: &str = r#"
[accounts.account-name]
root_mailbox = "/path/to/root/mailbox"
format = "Maildir"
send_mail = 'false'
listing.index_style = "Conversations" # or [plain, threaded, compact]
identity="email@example.com"
display_name = "Name"
subscribed_mailboxes = ["INBOX", "INBOX/Sent", "INBOX/Drafts", "INBOX/Junk"]

# Set mailbox-specific settings
  [accounts.account-name.mailboxes]
  "INBOX" = { rename="Inbox" }
  "drafts" = { rename="Drafts" }
  "foobar-devel" = { ignore = true } # don't show notifications for this mailbox

# Setting up an mbox account
[accounts.mbox]
root_mailbox = "/var/mail/username"
format = "mbox"
send_mail = 'false'
listing.index_style = "Compact"
identity="username@hostname.local"
"#;

pub const EXTRA_CONFIG: &str = r#"
[accounts.mbox]
root_mailbox = "/"
format = "mbox"
send_mail = 'false'
index_style = "Compact"
identity="username@hostname.local"
    "#;
pub const IMAP_CONFIG: &str = r#"
[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password_command = "false"
    "#;

#[test]
fn test_conf_config_parse() {
    let tempdir = tempfile::tempdir().unwrap();
    let new_file = ConfigFile::new(TEST_CONFIG, &tempdir).unwrap();
    let err = FileSettings::validate(new_file.path.clone(), true).unwrap_err();
    assert_eq!(
        err.summary.as_ref(),
        "Configuration error (account-name): root_mailbox `/path/to/root/mailbox` is not a valid \
         directory."
    );

    /* Test unrecognised configuration entries error */

    let new_file = ConfigFile::new(EXTRA_CONFIG, &tempdir).unwrap();
    let err = FileSettings::validate(new_file.path.clone(), true).unwrap_err();
    assert_eq!(
        err.summary.as_ref(),
        "Unrecognised configuration values: {\"index_style\": \"Compact\"}"
    );

    /* Test IMAP config */

    let new_file = ConfigFile::new(IMAP_CONFIG, &tempdir).unwrap();
    FileSettings::validate(new_file.path.clone(), true).expect("could not parse IMAP config");

    /* Test sample config */

    let example_config = FileSettings::EXAMPLE_CONFIG.replace("\n#", "\n");
    let re = regex::Regex::new(r#"root_mailbox\s*=\s*"[^"]*""#).unwrap();
    let example_config = re.replace_all(
        &example_config,
        &format!(r#"root_mailbox = "{}""#, tempdir.path().to_str().unwrap()),
    );

    let new_file = ConfigFile::new(&example_config, &tempdir).unwrap();
    let config = FileSettings::validate(new_file.path.clone(), true)
        .expect("Could not parse example config!");
    for (accname, acc) in config.accounts.iter() {
        if !acc.extra.is_empty() {
            panic!(
                "In example config, account `{}` has unrecognised configuration entries: {:?}",
                accname, acc.extra
            );
        }
    }
    if let Err(err) = tempdir.close() {
        eprintln!("Could not cleanup tempdir: {}", err);
    }
}

#[test]
fn test_conf_theme_parsing() {
    /* MUST SUCCEED: default themes should be valid */
    let def = Themes::default();
    def.validate().unwrap();
    /* MUST SUCCEED: new user theme `hunter2`, theme `dark` has user
     * redefinitions */
    const TEST_STR: &str = r#"[dark]
"mail.listing.tag_default" = { fg = "White", bg = "HotPink3" }
"mail.listing.attachment_flag" = { fg = "mail.listing.tag_default.bg" }
"mail.view.headers" = { bg = "mail.listing.tag_default.fg" }

["hunter2"]
"mail.view.body" = { fg = "Black", bg = "White"}"#;
    let parsed: Themes = toml::from_str(TEST_STR).unwrap();
    assert!(parsed.other_themes.contains_key("hunter2"));
    assert_eq!(
        unlink_bg(
            &parsed.dark,
            &ColorField::Bg,
            &Cow::from("mail.listing.tag_default")
        ),
        Color::Byte(132)
    );
    assert_eq!(
        unlink_fg(
            &parsed.dark,
            &ColorField::Fg,
            &Cow::from("mail.listing.attachment_flag")
        ),
        Color::Byte(132)
    );
    assert_eq!(
        unlink_bg(
            &parsed.dark,
            &ColorField::Bg,
            &Cow::from("mail.view.headers")
        ),
        Color::Byte(15), // White
    );
    parsed.validate().unwrap();
    /* MUST FAIL: theme `dark` contains a cycle */
    const HAS_CYCLE: &str = r#"[dark]
"mail.listing.compact.even" = { fg = "mail.listing.compact.odd" }
"mail.listing.compact.odd" = { fg = "mail.listing.compact.even" }
"#;
    let parsed: Themes = toml::from_str(HAS_CYCLE).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: theme `dark` contains an invalid key */
    const HAS_INVALID_KEYS: &str = r#"[dark]
"asdfsafsa" = { fg = "Black" }
"#;
    let parsed: std::result::Result<Themes, _> = toml::from_str(HAS_INVALID_KEYS);
    parsed.unwrap_err();
    /* MUST SUCCEED: alias $Jebediah resolves to a valid color */
    const TEST_ALIAS_STR: &str = r##"[dark]
color_aliases= { "Jebediah" = "#b4da55" }
"mail.listing.tag_default" = { fg = "$Jebediah" }
"##;
    let parsed: Themes = toml::from_str(TEST_ALIAS_STR).unwrap();
    parsed.validate().unwrap();
    assert_eq!(
        unlink_fg(
            &parsed.dark,
            &ColorField::Fg,
            &Cow::from("mail.listing.tag_default")
        ),
        Color::Rgb(180, 218, 85)
    );
    /* MUST FAIL: Misspell color alias $Jebediah as $Jebedia */
    const TEST_INVALID_ALIAS_STR: &str = r##"[dark]
color_aliases= { "Jebediah" = "#b4da55" }
"mail.listing.tag_default" = { fg = "$Jebedia" }
"##;
    let parsed: Themes = toml::from_str(TEST_INVALID_ALIAS_STR).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: Color alias $Jebediah is defined as itself */
    const TEST_CYCLIC_ALIAS_STR: &str = r#"[dark]
color_aliases= { "Jebediah" = "$Jebediah" }
"mail.listing.tag_default" = { fg = "$Jebediah" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_STR).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: Attr alias $Jebediah is defined as itself */
    const TEST_CYCLIC_ALIAS_ATTR_STR: &str = r#"[dark]
attr_aliases= { "Jebediah" = "$Jebediah" }
"mail.listing.tag_default" = { attrs = "$Jebediah" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_ATTR_STR).unwrap();
    parsed.validate().unwrap_err();
    /* MUST FAIL: alias $Jebediah resolves to a cycle */
    const TEST_CYCLIC_ALIAS_STR_2: &str = r#"[dark]
color_aliases= { "Jebediah" = "$JebediahJr", "JebediahJr" = "mail.listing.tag_default" }
"mail.listing.tag_default" = { fg = "$Jebediah" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_STR_2).unwrap();
    parsed.validate().unwrap_err();
    /* MUST SUCCEED: alias $Jebediah resolves to a key's field */
    const TEST_CYCLIC_ALIAS_STR_3: &str = r#"[dark]
color_aliases= { "Jebediah" = "$JebediahJr", "JebediahJr" = "mail.listing.tag_default.bg" }
"mail.listing.tag_default" = { fg = "$Jebediah", bg = "Black" }
"#;
    let parsed: Themes = toml::from_str(TEST_CYCLIC_ALIAS_STR_3).unwrap();
    parsed.validate().unwrap();
    /* MUST FAIL: alias $Jebediah resolves to an invalid key */
    const TEST_INVALID_LINK_KEY_FIELD_STR: &str = r#"[dark]
color_aliases= { "Jebediah" = "$JebediahJr", "JebediahJr" = "mail.listing.tag_default.attrs" }
"mail.listing.tag_default" = { fg = "$Jebediah", bg = "Black" }
"#;
    let parsed: Themes = toml::from_str(TEST_INVALID_LINK_KEY_FIELD_STR).unwrap();
    parsed.validate().unwrap_err();
}

#[test]
fn test_conf_theme_key_values() {
    use std::{collections::VecDeque, fs::File, io::Read, path::PathBuf};
    let mut rust_files: VecDeque<PathBuf> = VecDeque::new();
    let mut dirs_queue: VecDeque<PathBuf> = VecDeque::new();
    dirs_queue.push_back("src/".into());
    let re_whitespace = regex::Regex::new(r"\s*").unwrap();
    let re_conf = regex::Regex::new(r#"value\([&]?context,"([^"]*)""#).unwrap();

    while let Some(dir) = dirs_queue.pop_front() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                dirs_queue.push_back(path);
            } else if path.extension().map(|os_s| os_s == "rs").unwrap_or(false) {
                rust_files.push_back(path);
            }
        }
    }
    for file_path in rust_files {
        let mut file = File::open(&file_path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        let content = re_whitespace.replace_all(&content, "");
        for mat in re_conf.captures_iter(&content) {
            let theme_key = &mat[1];
            if !DEFAULT_KEYS.contains(&theme_key) {
                panic!(
                    "Source file {} contains a hardcoded theme key str, {:?}, that is not \
                     included in the DEFAULT_KEYS table.",
                    file_path.display(),
                    theme_key
                );
            }
        }
    }
}

#[test]
fn test_conf_progress_spinner_sequence() {
    use crate::{conf::terminal::ProgressSpinnerSequence, utilities::ProgressSpinner};

    let int_0 = ProgressSpinnerSequence::Integer(5);
    assert_eq!(
        toml::Value::try_from(&int_0).unwrap(),
        toml::Value::try_from(5).unwrap()
    );

    let frames = ProgressSpinnerSequence::Custom {
        frames: vec![
            "⠁".to_string(),
            "⠂".to_string(),
            "⠄".to_string(),
            "⡀".to_string(),
            "⢀".to_string(),
            "⠠".to_string(),
            "⠐".to_string(),
            "⠈".to_string(),
        ],
        interval_ms: ProgressSpinner::INTERVAL_MS,
    };
    assert_eq!(frames.interval_ms(), ProgressSpinner::INTERVAL_MS);
    assert_eq!(
        toml::Value::try_from(&frames).unwrap(),
        toml::Value::try_from(["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]).unwrap()
    );
    let frames = ProgressSpinnerSequence::Custom {
        frames: vec![
            "⠁".to_string(),
            "⠂".to_string(),
            "⠄".to_string(),
            "⡀".to_string(),
            "⢀".to_string(),
            "⠠".to_string(),
            "⠐".to_string(),
            "⠈".to_string(),
        ],
        interval_ms: ProgressSpinner::INTERVAL_MS + 1,
    };
    assert_eq!(
        toml::Value::try_from(&frames).unwrap(),
        toml::Value::try_from(indexmap::indexmap! {
            "frames" => toml::Value::try_from(["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]).unwrap(),
            "interval_ms" => toml::Value::try_from(ProgressSpinner::INTERVAL_MS + 1).unwrap()
        })
        .unwrap()
    );
    assert_eq!(
        toml::from_str::<ProgressSpinnerSequence>(
            r#"frames = ["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]
interval_ms = 51"#
        )
        .unwrap(),
        frames
    );
    assert_eq!(
        toml::from_str::<indexmap::IndexMap<String, ProgressSpinnerSequence>>(
            r#"sequence = { frames = ["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"], interval_ms = 51 }"#
        )
        .unwrap(),
        indexmap::indexmap! {
            "sequence".to_string() => frames,
        },
    );
}
