//
// meli
//
// Copyright 2017- Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use melib::{email::MessageID, Flag};

use crate::{
    command::{
        actions::{
            Action, FlagAction, ListingAction, MailingListAction, TabAction, TagAction, ViewAction,
        },
        completions::CompletionsGenerator,
        parse_command,
        parser::{self, lex, LexToken, LexTokenError},
        CommandError,
    },
    utilities::AutoCompleteEntry,
};

#[test]
fn test_command_parser_all() {
    use CommandError::*;

    for (cmd, expected) in [
        ("set unseen", Action::Listing(ListingAction::SetUnseen)),
        (
            "flag set passed",
            Action::Listing(ListingAction::Flag(FlagAction::Set(Flag::PASSED))),
        ),
        ("set seen", Action::Listing(ListingAction::SetSeen)),
        ("set plain", Action::Listing(ListingAction::SetPlain)),
        ("delete", Action::Listing(ListingAction::Delete)),
        (
            "copyto somewhere",
            Action::Listing(ListingAction::CopyTo("somewhere".into())),
        ),
        (
            "copyto account somewhere",
            Action::Listing(ListingAction::CopyToOtherAccount(
                "account".into(),
                "somewhere".into(),
            )),
        ),
        (
            "moveto somewhere",
            Action::Listing(ListingAction::MoveTo("somewhere".into())),
        ),
        (
            "moveto account somewhere",
            Action::Listing(ListingAction::MoveToOtherAccount(
                "account".into(),
                "somewhere".into(),
            )),
        ),
        (
            "import fpath mpath",
            Action::Listing(ListingAction::Import("fpath".into(), "mpath".into())),
        ),
        (
            "search sfjj afas fdas as jfdsaj fdsai jifsa",
            Action::Listing(ListingAction::Search {
                term: "sfjj afas fdas as jfdsaj fdsai jifsa".into(),
                raw_search: false,
            }),
        ),
        (
            "raw-search tag:t and foo",
            Action::Listing(ListingAction::Search {
                term: "tag:t and foo".into(),
                raw_search: true,
            }),
        ),
        (
            "clear-selection",
            Action::Listing(ListingAction::ClearSelection),
        ),
        (
            "select sth",
            Action::Listing(ListingAction::Select {
                term: "sth".into(),
                raw_search: false,
            }),
        ),
        (
            "raw-select sth",
            Action::Listing(ListingAction::Select {
                term: "sth".into(),
                raw_search: true,
            }),
        ),
        (
            "export-mbox path",
            Action::Listing(ListingAction::ExportMbox(
                Some(melib::mbox::MboxFormat::MboxCl2),
                "path".to_string().into(),
            )),
        ),
        (
            "export-thread-mbox path",
            Action::View(ViewAction::ExportThreadMbox(
                Some(melib::mbox::MboxFormat::MboxCl2),
                "path".to_string().into(),
            )),
        ),
        (
            "list-post",
            Action::MailingListAction(MailingListAction::ListPost),
        ),
        ("setenv key=val", Action::SetEnv("key".into(), "val".into())),
        ("printenv key", Action::PrintEnv("key".into())),
        ("cwd", Action::CurrentDirectory),
        (
            "cd somewhere",
            Action::ChangeCurrentDirectory("somewhere".into()),
        ),
        ("close  ", Action::Tab(TabAction::Close)),
        ("go 5", Action::ViewMailbox(5)),
        ("quit", Action::Quit),
    ] {
        assert_eq!(
            parse_command(cmd).unwrap_or_else(|err| panic!("{cmd} failed {err}")),
            expected
        );
    }

    assert_eq!(
        parse_command("setfafsfoo").unwrap_err().to_string(),
        Parsing {
            inner: "setfafsfoo".into(),
            kind: "".into(),
        }
        .to_string(),
    );
    assert_eq!(
        parse_command("set foo").unwrap_err().to_string(),
        BadValue {
            inner: "foo".into(),
            suggestions: Some(&[
                "seen",
                "unseen",
                "plain",
                "threaded",
                "compact",
                "conversations"
            ])
        }
        .to_string(),
    );
    assert_eq!(
        parse_command("moveto ").unwrap_err().to_string(),
        WrongNumberOfArguments {
            too_many: false,
            takes: (1, Some(2)),
            given: 0,
            __func__: "moveto",
            inner: "".into(),
        }
        .to_string(),
    );
    assert_eq!(
        parse_command("reindex 1 2 3").unwrap_err().to_string(),
        WrongNumberOfArguments {
            too_many: true,
            takes: (1, Some(1)),
            given: 2,
            __func__: "reindex",
            inner: "".into(),
        }
        .to_string(),
    );
}

#[test]
fn test_command_parsers() {
    let (rest, parsed) = parser::flag("flag set junk").unwrap();
    assert_eq!(rest, "");
    assert!(
        matches!(
            parsed,
            Ok(Action::Listing(ListingAction::Flag(FlagAction::Set(
                Flag::TRASHED
            ))))
        ),
        "{:?}",
        parsed
    );

    let (rest, parsed) = parser::flag("flag unset junk").unwrap();
    assert_eq!(rest, "");
    assert!(
        matches!(
            parsed,
            Ok(Action::Listing(ListingAction::Flag(FlagAction::Unset(
                Flag::TRASHED
            ))))
        ),
        "{:?}",
        parsed
    );

    let (rest, parsed) = parser::flag("flag set draft").unwrap();
    assert_eq!(rest, "");
    assert!(
        matches!(
            parsed,
            Ok(Action::Listing(ListingAction::Flag(FlagAction::Set(
                Flag::DRAFT
            ))))
        ),
        "{:?}",
        parsed
    );

    let (rest, parsed) = parser::flag("flag set xunk").unwrap();
    assert_eq!(rest, "xunk");
    assert_eq!(
        &parsed.unwrap_err().to_string(),
        "Bad value/argument: xunk is not a valid flag name. Possible values are: passed, replied, \
         seen or read, junk or trash or trashed, draft, flagged"
    );

    let (rest, parsed) = parser::_tag("tag add newsletters").unwrap();
    assert_eq!(rest, "");
    assert!(
        matches!(parsed, Ok(Action::Listing(ListingAction::Tag(TagAction::Add(ref tagname)))) if tagname == "newsletters"),
        "{:?}",
        parsed
    );

    let (rest, parsed) =
        parser::public_inbox_import("public-inbox import foo \"bar\" message@example.com").unwrap();
    assert_eq!(rest, "");
    assert!(
        matches!(
            parsed,
            Ok(Action::Listing(ListingAction::PublicInboxImport {
                thread,
                ref account,
                ref mailbox_path,
                ref message_id,
            })) if (thread, account.as_str(), mailbox_path.as_str(), message_id) == (false, "foo", "bar", &MessageID::new("message@example.com"))
        ),
        "{:?}",
        parsed
    );
    let (rest, parsed) =
        parser::public_inbox_import("public-inbox import-thread foo bar <message@example.com>")
            .unwrap();
    assert_eq!(rest, "");
    assert!(
        matches!(
            parsed,
            Ok(Action::Listing(ListingAction::PublicInboxImport {
                thread,
                ref account,
                ref mailbox_path,
                ref message_id,
            })) if (thread, account.as_str(), mailbox_path.as_str(), message_id) == (true, "foo", "bar", &MessageID::new("message@example.com"))
        ),
        "{:?}",
        parsed
    );

    let (rest, parsed) =
        parser::public_inbox_import("public-inbox import-foo foo bar <message@example.com>")
            .unwrap();
    assert_eq!(rest, "");
    assert_eq!(
        &parsed.unwrap_err().to_string(),
        "Bad value/argument: import-foo. Possible values are: import, import-thread"
    );
}

#[test]
fn test_command_error_display() {
    assert_eq!(
        &CommandError::BadValue {
            inner: "foo".into(),
            suggestions: Some(&[
                "seen",
                "unseen",
                "plain",
                "threaded",
                "compact",
                "conversations"
            ])
        }
        .to_string(),
        "Bad value/argument: foo. Possible values are: seen, unseen, plain, threaded, compact, \
         conversations"
    );
}

#[test]
fn test_command_completions_generate() {
    let mut gen = CompletionsGenerator::default();
    assert_eq!(gen.generate(""), vec![]);
    assert_eq!(
        gen.generate("set se"),
        vec![AutoCompleteEntry {
            entry: "set seen".into(),
            description: "set [seen/unseen], toggles message's Seen flag".into()
        }]
    );

    // Test WhitespaceAndNext case
    assert_eq!(
        gen.generate("set"),
        vec![
            AutoCompleteEntry {
                entry: "set plain".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set threaded".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set compact".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set conversations".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set seen".into(),
                description: "set [seen/unseen], toggles message's Seen flag".into()
            },
            AutoCompleteEntry {
                entry: "set unseen".into(),
                description: "set [seen/unseen], toggles message's Seen flag".into()
            },
            AutoCompleteEntry {
                entry: "setenv".into(),
                description: "setenv VAR=VALUE".into()
            }
        ]
    );

    // A complete command should not generate a suggestion
    assert_eq!(gen.generate("set seen"), vec![]);
    assert_eq!(
        gen.generate("set "),
        vec![
            AutoCompleteEntry {
                entry: "set plain".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set threaded".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set compact".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set conversations".into(),
                description: "set [plain/threaded/compact/conversations] changes the mail listing \
                              view"
                    .into()
            },
            AutoCompleteEntry {
                entry: "set seen".into(),
                description: "set [seen/unseen], toggles message's Seen flag".into()
            },
            AutoCompleteEntry {
                entry: "set unseen".into(),
                description: "set [seen/unseen], toggles message's Seen flag".into()
            }
        ]
    );

    // Check for common prefix
    assert_eq!(
        gen.generate("add"),
        vec![
            AutoCompleteEntry {
                entry: "add-attachment".into(),
                description: "add-attachment PATH, add PATH as an attachment".into()
            },
            AutoCompleteEntry {
                entry: "add-attachment-file-picker".into(),
                description: "launch file picker to select an attachment".into()
            },
            AutoCompleteEntry {
                entry: "add-addresses-to-contacts".into(),
                description: "add-addresses-to-contacts".into()
            }
        ]
    );
    assert_eq!(
        gen.generate("add-"),
        vec![
            AutoCompleteEntry {
                entry: "add-attachment".into(),
                description: "add-attachment PATH, add PATH as an attachment".into()
            },
            AutoCompleteEntry {
                entry: "add-attachment-file-picker".into(),
                description: "launch file picker to select an attachment".into()
            },
            AutoCompleteEntry {
                entry: "add-addresses-to-contacts".into(),
                description: "add-addresses-to-contacts".into()
            }
        ]
    );
    assert_eq!(
        gen.generate("add-at"),
        vec![
            AutoCompleteEntry {
                entry: "add-attachment".into(),
                description: "add-attachment PATH, add PATH as an attachment".into()
            },
            AutoCompleteEntry {
                entry: "add-attachment-file-picker".into(),
                description: "launch file picker to select an attachment".into()
            },
        ]
    );

    // Check filepath completions.

    let tempdir = tempfile::tempdir().unwrap();
    std::fs::write(tempdir.path().join("a"), b"foobar").unwrap();
    std::fs::write(tempdir.path().join("b"), b"foobar").unwrap();

    // Test that without an ending backslash we only get a single suggestion
    assert_eq!(
        gen.generate(&format!("export-mbox {}", tempdir.path().display())),
        vec![AutoCompleteEntry {
            entry: format!("export-mbox {}/", tempdir.path().display()),
            description: "export-mbox PATH, save mail as mbox to PATH".into()
        },]
    );
    // Test that with an ending backslash we get the files
    assert_eq!(
        gen.generate(&format!("export-mbox {}/", tempdir.path().display())),
        vec![
            AutoCompleteEntry {
                entry: format!("export-mbox {}/a", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("export-mbox {}/b", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
        ]
    );
    // Test that with an incomplete quoted argument we get the files
    assert_eq!(
        gen.generate(&format!("export-mbox \"{}/", tempdir.path().display())),
        vec![
            AutoCompleteEntry {
                entry: format!("export-mbox \"{}/a\"", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("export-mbox \"{}/b\"", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
        ]
    );
    // Add a file that includes a space
    std::fs::write(tempdir.path().join(" c"), b"foobar").unwrap();
    // Check that without a quote we get an escaped space suggestion
    assert_eq!(
        gen.generate(&format!("export-mbox {}/", tempdir.path().display())),
        vec![
            AutoCompleteEntry {
                entry: format!("export-mbox {}/\\ c", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("export-mbox {}/a", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("export-mbox {}/b", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
        ]
    );
    // Check that with an incomplete quote we get a fully quoted suggestion
    assert_eq!(
        gen.generate(&format!("export-mbox \"{}/ ", tempdir.path().display())),
        vec![AutoCompleteEntry {
            entry: format!("export-mbox \"{}/ c\"", tempdir.path().display()),
            description: "export-mbox PATH, save mail as mbox to PATH".into()
        },]
    );
    // Check that with an incomplete quote we get fully quoted suggestions
    assert_eq!(
        gen.generate(&format!("export-mbox \"{}/", tempdir.path().display())),
        vec![
            AutoCompleteEntry {
                entry: format!("export-mbox \"{}/ c\"", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("export-mbox \"{}/a\"", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("export-mbox \"{}/b\"", tempdir.path().display()),
                description: "export-mbox PATH, save mail as mbox to PATH".into()
            },
        ]
    );
    // Check that with a complete quote we don't get any suggestion
    assert_eq!(
        gen.generate(&format!("export-mbox \"{}/ d\"", tempdir.path().display())),
        vec![]
    );
    // Add file with dquote
    std::fs::write(tempdir.path().join(" d\""), b"foobar").unwrap();
    assert_eq!(
        gen.generate(&format!("export-mbox \"{}/ d\"", tempdir.path().display())),
        vec![AutoCompleteEntry {
            entry: format!(
                "export-mbox \"{}\"",
                tempdir
                    .path()
                    .join(" d\"")
                    .display()
                    .to_string()
                    .replace('"', "\\\"")
            ),
            description: "export-mbox PATH, save mail as mbox to PATH".into()
        },]
    );

    // Test account/mbox name completion

    gen.add_account(
        "foobar".to_string(),
        vec!["INBOX".to_string(), "Sent".to_string()]
            .into_iter()
            .collect(),
    );
    // WhitespaceAndNext
    assert_eq!(
        gen.generate("create-mailbox"),
        vec![AutoCompleteEntry {
            entry: "create-mailbox foobar".into(),
            description: "create-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    // Next
    assert_eq!(
        gen.generate("create-mailbox "),
        vec![AutoCompleteEntry {
            entry: "create-mailbox foobar".into(),
            description: "create-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    assert_eq!(
        gen.generate("create-mailbox foo"),
        vec![AutoCompleteEntry {
            entry: "create-mailbox foobar".into(),
            description: "create-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar"),
        vec![
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar INBOX".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar Sent".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
        ]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar \"INB"),
        vec![AutoCompleteEntry {
            entry: "subscribe-mailbox foobar \"INBOX\"".into(),
            description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
        },]
    );
    assert_eq!(
        gen.generate(&format!("import {}/", tempdir.path().display())),
        vec![
            AutoCompleteEntry {
                entry: format!("import {}/\\ c", tempdir.path().display()),
                description: "import FILESYSTEM_PATH MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("import {}/\\ d\\\"", tempdir.path().display()),
                description: "import FILESYSTEM_PATH MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("import {}/a", tempdir.path().display()),
                description: "import FILESYSTEM_PATH MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("import {}/b", tempdir.path().display()),
                description: "import FILESYSTEM_PATH MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("import {}/ INBOX", tempdir.path().display()),
                description: "import FILESYSTEM_PATH MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: format!("import {}/ Sent", tempdir.path().display()),
                description: "import FILESYSTEM_PATH MAILBOX_PATH".into()
            },
        ]
    );
    assert_eq!(
        gen.generate(&format!("import {}/ \"INBOX\" ", tempdir.path().display())),
        vec![]
    );
    // Test mailbox path deep hierarchy suggestions
    gen.add_account(
        "foobar".to_string(),
        vec![
            "INBOX".to_string(),
            "INBOX/Sent".to_string(),
            "INBOX/Archives/2019/mailing-list".to_string(),
        ]
        .into_iter()
        .collect(),
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar \"INB"),
        vec![
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar \"INBOX\"".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar \"INBOX/".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
        ]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar \"INBOX/"),
        vec![
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar \"INBOX/Sent\"".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar \"INBOX/Archives/".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            }
        ]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar \"INBOX/Archives\""),
        vec![AutoCompleteEntry {
            entry: "subscribe-mailbox foobar \"INBOX/Archives/".into(),
            description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar \"INBOX/Archives/"),
        vec![AutoCompleteEntry {
            entry: "subscribe-mailbox foobar \"INBOX/Archives/2019/".into(),
            description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar \"INBOX/Archives/2019/"),
        vec![AutoCompleteEntry {
            entry: "subscribe-mailbox foobar \"INBOX/Archives/2019/mailing-list\"".into(),
            description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    // Test mailbox paths with spaces/quotes
    gen.add_account(
        "foobar".to_string(),
        vec![
            "INBOX".to_string(),
            "A mailbox".to_string(),
            "INBOX/Archives/2019/mailing list".to_string(),
        ]
        .into_iter()
        .collect(),
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar "),
        vec![
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar INBOX".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar \"A mailbox\"".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: "subscribe-mailbox foobar INBOX/".into(),
                description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
            }
        ]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar A"),
        vec![AutoCompleteEntry {
            entry: "subscribe-mailbox foobar \"A mailbox\"".into(),
            description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );
    assert_eq!(
        gen.generate("subscribe-mailbox foobar INBOX/Archives/2019/"),
        vec![AutoCompleteEntry {
            entry: "subscribe-mailbox foobar \"INBOX/Archives/2019/mailing list\"".into(),
            description: "subscribe-mailbox ACCOUNT MAILBOX_PATH".into()
        }]
    );

    // Test NewMailboxPath completions
    gen.add_account(
        "foobar".to_string(),
        vec![
            "INBOX".to_string(),
            "A mailbox/foo".to_string(),
            "INBOX/Archives/2019/mailing list".to_string(),
        ]
        .into_iter()
        .collect(),
    );
    assert_eq!(
        gen.generate("create-mailbox foobar "),
        vec![
            AutoCompleteEntry {
                entry: "create-mailbox foobar \"A mailbox/".into(),
                description: "create-mailbox ACCOUNT MAILBOX_PATH".into()
            },
            AutoCompleteEntry {
                entry: "create-mailbox foobar INBOX/".into(),
                description: "create-mailbox ACCOUNT MAILBOX_PATH".into()
            }
        ]
    );
    assert_eq!(
        gen.generate("create-mailbox foobar INBOX/Archives/foo"),
        vec![]
    );
    _ = tempdir.close();
}

#[test]
fn test_command_lexer() {
    macro_rules! lex {
        ($l:literal) => {{
            LexToken::Literal {
                raw: $l,
                unescaped: $l.into(),
            }
        }};
        (ws $l:literal) => {{
            LexToken::Whitespace { raw: $l.into() }
        }};
    }
    for (cmd, expected) in [
        ("set seen", vec![lex!("set"), lex!(ws " "), lex!("seen")]),
        ("delete", vec![lex!("delete")]),
        (
            "moveto  somewhere",
            vec![lex!("moveto"), lex!(ws "  "), lex!("somewhere")],
        ),
        ("close  ", vec![lex!("close"), lex!(ws "  ")]),
        (
            "import \"fpath \" mpath",
            vec![
                lex!("import"),
                lex!(ws " "),
                LexToken::QuotedLiteral {
                    raw: "\"fpath \"",
                    unescaped: "fpath ".into(),
                },
                lex!(ws " "),
                lex!("mpath"),
            ],
        ),
        (
            "import \"fpath \\\" \" mpath",
            vec![
                lex!("import"),
                lex!(ws " "),
                LexToken::QuotedLiteral {
                    raw: "\"fpath \\\" \"",
                    unescaped: "fpath \" ".into(),
                },
                lex!(ws " "),
                lex!("mpath"),
            ],
        ),
        (
            "import /path\\ to/spaces\\\"/welp\\ /something",
            vec![
                lex!("import"),
                lex!(ws " "),
                LexToken::Literal {
                    raw: "/path\\ to/spaces\\\"/welp\\ /something",
                    unescaped: "/path to/spaces\"/welp /something".into(),
                },
            ],
        ),
    ] {
        let mut tokens = vec![];
        let mut input = cmd;
        while !input.is_empty() {
            let (next_token, rest) = lex(input).unwrap();
            tokens.push(next_token);
            input = rest;
        }
        assert_eq!(tokens, expected);
    }
    {
        let cmd = "import \"/path\\ to/spaces\\\"/welp\\ /something";
        let mut tokens = vec![];
        let mut input = cmd;
        let mut err = None;
        while !input.is_empty() {
            let (next_token, rest) = match lex(input) {
                Ok(v) => v,
                Err(v) => {
                    err = Some(v);
                    break;
                }
            };
            tokens.push(next_token);
            input = rest;
        }
        assert_eq!(
            (tokens, err),
            (
                vec![lex!("import"), lex!(ws " ")],
                Some(LexTokenError::QuoteStart {
                    raw: "\"/path\\ to/spaces\\\"/welp\\ /something",
                    unescaped: "/path to/spaces\"/welp /something".into()
                })
            )
        );
    }
}
