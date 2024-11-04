/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

//! A parser module for user commands passed through
//! [`Command`](crate::types::UIMode::Command) mode.

use std::{borrow::Cow, collections::HashSet, str::FromStr};

use melib::{
    nom::{
        self,
        branch::alt,
        bytes::complete::{is_a, is_not, tag, take_until},
        character::complete::{digit1, not_line_ending},
        combinator::{map, map_res},
        error::Error as NomError,
        multi::separated_list1,
        sequence::{pair, preceded, separated_pair},
        IResult,
    },
    parser::BytesExt,
    SortField, SortOrder,
};

#[cfg(test)]
mod tests;

pub mod actions;
#[macro_use]
pub mod error;
#[macro_use]
pub mod argcheck;
pub mod history;
pub mod parser;
use actions::MailboxOperation;
use error::CommandError;
pub use parser::parse_command;

pub use crate::actions::{
    AccountAction::{self, *},
    Action::{self, *},
    ComposeAction::{self, *},
    ComposerTabAction, FlagAction,
    ListingAction::{self, *},
    MailingListAction::{self, *},
    TabAction::{self, *},
    TagAction,
    ViewAction::{self, *},
};

/// Helper macro to convert an array of tokens into a `TokenStream`
macro_rules! to_stream {
    ($token: expr) => {
        TokenStream {
            tokens: &[$token],
        }
    };
    ($($tokens:expr),*) => {
        TokenStream {
            tokens: &[$($tokens),*],
        }
    };
}

/// Macro to create a const table with every command part that can be
/// auto-completed and its description
macro_rules! define_commands {
    ( [$({ tags: [$( $tags:literal),*], desc: $desc:literal, tokens: $tokens:expr, parser: $parser:path}),*]) => {
        pub const COMMAND_COMPLETION: &[(&str, &str, TokenStream, fn(&[u8]) -> IResult<&[u8], Result<Action, CommandError>>)] = &[$($( ($tags, $desc, TokenStream { tokens: $tokens }, $parser) ),*),* ];
    };
}

pub fn quoted_argument(input: &[u8]) -> IResult<&[u8], &str> {
    if input.is_empty() {
        return Err(nom::Err::Error(NomError {
            input,
            code: nom::error::ErrorKind::Tag,
        }));
    }

    if input[0] == b'"' {
        let mut i = 1;
        while i < input.len() {
            if input[i] == b'\"' && input[i - 1] != b'\\' {
                return Ok((&input[i + 1..], unsafe {
                    std::str::from_utf8_unchecked(&input[1..i])
                }));
            }
            i += 1;
        }
        Err(nom::Err::Error(NomError {
            input,
            code: nom::error::ErrorKind::Tag,
        }))
    } else {
        map_res(is_not(" "), std::str::from_utf8)(input)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TokenStream {
    tokens: &'static [TokenAdicity],
}

use Token::*;
use TokenAdicity::*;

impl TokenStream {
    fn matches<'s>(&self, s: &mut &'s str, sugg: &mut HashSet<String>) -> Vec<(&'s str, Token)> {
        let mut tokens = vec![];
        for t in self.tokens.iter() {
            let mut ptr = 0;
            while ptr + 1 < s.len() && s.as_bytes()[ptr].is_ascii_whitespace() {
                ptr += 1;
            }
            *s = &s[ptr..];
            //println!("\t before s.is_empty() {:?} {:?}", t, s);
            if s.is_empty() || *s == " " {
                match t.inner() {
                    Literal(lit) => {
                        sugg.insert(format!("{}{}", if s.is_empty() { " " } else { "" }, lit));
                    }
                    Alternatives(v) => {
                        for t in v.iter() {
                            //println!("adding empty suggestions for {:?}", t);
                            let mut _s = *s;
                            let mut m = t.matches(&mut _s, sugg);
                            tokens.append(&mut m);
                        }
                    }
                    AlternativeStrings(v) => {
                        for t in v.iter() {
                            sugg.insert(format!("{}{}", if s.is_empty() { " " } else { "" }, t));
                        }
                    }
                    Seq(_s) => {}
                    RestOfStringValue => {
                        sugg.insert(String::new());
                    }
                    t @ AttachmentIndexValue
                    | t @ MailboxIndexValue
                    | t @ IndexValue
                    | t @ Filepath
                    | t @ AccountName
                    | t @ MailboxPath
                    | t @ QuotedStringValue
                    | t @ AlphanumericStringValue => {
                        let _t = t;
                        //sugg.insert(format!("{}{:?}", if s.is_empty() { " " }
                        // else { "" }, t));
                    }
                }
                tokens.push((*s, *t.inner()));
                return tokens;
            }
            match t.inner() {
                Literal(lit) => {
                    if lit.starts_with(*s) && lit.len() != s.len() {
                        sugg.insert(lit[s.len()..].to_string());
                        tokens.push((s, *t.inner()));
                        return tokens;
                    } else if s.starts_with(lit) {
                        tokens.push((&s[..lit.len()], *t.inner()));
                        *s = &s[lit.len()..];
                    } else {
                        return vec![];
                    }
                }
                Alternatives(v) => {
                    let mut cont = true;
                    for t in v.iter() {
                        let mut _s = *s;
                        let mut m = t.matches(&mut _s, sugg);
                        if !m.is_empty() {
                            tokens.append(&mut m);
                            //println!("_s is empty {}", _s.is_empty());
                            cont = !_s.is_empty();
                            *s = _s;
                            break;
                        }
                    }
                    if tokens.is_empty() {
                        return tokens;
                    }
                    if !cont {
                        *s = "";
                    }
                }
                AlternativeStrings(v) => {
                    for lit in v.iter() {
                        if lit.starts_with(*s) && lit.len() != s.len() {
                            sugg.insert(lit[s.len()..].to_string());
                            tokens.push((s, *t.inner()));
                            return tokens;
                        } else if s.starts_with(lit) {
                            tokens.push((&s[..lit.len()], *t.inner()));
                            *s = &s[lit.len()..];
                        } else {
                            return vec![];
                        }
                    }
                }
                Seq(_s) => {
                    return vec![];
                }
                RestOfStringValue => {
                    tokens.push((*s, *t.inner()));
                    return tokens;
                }
                AttachmentIndexValue
                | MailboxIndexValue
                | IndexValue
                | Filepath
                | AccountName
                | MailboxPath
                | QuotedStringValue
                | AlphanumericStringValue => {
                    let mut ptr = 0;
                    while ptr + 1 < s.len() && !s.as_bytes()[ptr].is_ascii_whitespace() {
                        ptr += 1;
                    }
                    tokens.push((&s[..ptr + 1], *t.inner()));
                    *s = &s[ptr + 1..];
                }
            }
        }
        tokens
    }
}

/// `Token` wrapper that defines how many times a token is expected to be
/// repeated
#[derive(Clone, Copy, Debug)]
pub enum TokenAdicity {
    ZeroOrOne(Token),
    ZeroOrMore(Token),
    One(Token),
    OneOrMore(Token),
}

impl TokenAdicity {
    fn inner(&self) -> &Token {
        match self {
            ZeroOrOne(ref t) => t,
            ZeroOrMore(ref t) => t,
            One(ref t) => t,
            OneOrMore(ref t) => t,
        }
    }
}

/// A token encountered in the UI's command execution bar
#[derive(Clone, Copy, Debug)]
pub enum Token {
    Literal(&'static str),
    Filepath,
    Alternatives(&'static [TokenStream]),
    AlternativeStrings(&'static [&'static str]),
    Seq(&'static [TokenAdicity]),
    AccountName,
    MailboxPath,
    QuotedStringValue,
    RestOfStringValue,
    AlphanumericStringValue,
    AttachmentIndexValue,
    MailboxIndexValue,
    IndexValue,
}

fn eof(input: &[u8]) -> IResult<&[u8], ()> {
    if input.is_empty() {
        Ok((input, ()))
    } else {
        Err(nom::Err::Error(NomError {
            input,
            code: nom::error::ErrorKind::Tag,
        }))
    }
}

define_commands!([
                 { tags: ["set", "set seen", "set unseen", "set plain", "set threaded", "set compact"],
                   desc: "set [seen/unseen], toggles message's Seen flag. set [plain/threaded/compact/conversations] changes the mail listing view",
                   tokens: &[One(Literal("set")),
                   One(
                       Alternatives(&[
                           to_stream!(One(Literal("seen"))),
                           to_stream!(One(Literal("unseen"))),
                           to_stream!(One(Literal("plain"))),
                           to_stream!(One(Literal("threaded"))),
                           to_stream!(One(Literal("compact"))),
                           to_stream!(One(Literal("conversations")))
                     ])
                      )
                   ],
                   parser: parser::set
                 },
                 { tags: ["delete"],
                   desc: "delete message",
                   tokens: &[One(Literal("delete"))],
                   parser: parser::delete_message
                 },
                 { tags: ["copyto", "moveto"],
                   desc: "copy/move message",
                   tokens: &[One(Alternatives(&[to_stream!(One(Literal("copyto"))), to_stream!(One(Literal("moveto")))])), ZeroOrOne(AccountName), One(MailboxPath)],
                   parser: parser::copymove
                 },
                { tags: ["import "],
                  desc: "import FILESYSTEM_PATH MAILBOX_PATH",
                  tokens: &[One(Literal("import")), One(Filepath), One(MailboxPath)],
                  parser: parser::import
                },
                 { tags: ["close"],
                   desc: "close non-sticky tabs",
                   tokens: &[One(Literal("close"))],
                   parser: parser::close
                 },
                 { tags: ["go"],
                   desc: "go <n>, switch to nth mailbox in this account",
                   tokens: &[One(Literal("goto")), One(MailboxIndexValue)],
                   parser: parser::goto
                 },
                 { tags: ["subsort"],
                   desc: "subsort [date/subject] [asc/desc], sorts first level replies in threads.",
                   tokens: &[One(Literal("subsort")), One(Alternatives(&[to_stream!(One(Literal("date"))), to_stream!(One(Literal("subject")))])), One(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                   parser: parser::subsort
                 },
                { tags: ["sort"],
                  desc: "sort [date/subject] [asc/desc], sorts threads.",
                   tokens: &[One(Literal("sort")), One(Alternatives(&[to_stream!(One(Literal("date"))), to_stream!(One(Literal("subject")))])), One(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                  parser: parser::sort
                },
                { tags: ["sort"],
                  desc: "sort <column index> [asc/desc], sorts table columns.",
                   tokens: &[One(Literal("sort")), One(IndexValue), ZeroOrOne(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                  parser: parser::sort_column
                },
                { tags: ["toggle thread_snooze"],
                  desc: "turn off new notifications for this thread",
                  tokens: &[One(Literal("toggle")), One(Literal("thread_snooze"))],
                  parser: parser::toggle
                },
                { tags: ["search"],
                  desc: "search <TERM>, searches list with given term",
                  tokens: &[One(Literal("search")), One(RestOfStringValue)],
                  parser: parser::search
                },
                { tags: ["clear-selection"],
                  desc: "clear-selection",
                  tokens: &[One(Literal("clear-selection"))],
                  parser: parser::select
                },
                { tags: ["select"],
                  desc: "select <TERM>, selects envelopes matching with given term",
                  tokens: &[One(Literal("select")), One(RestOfStringValue)],
                  parser: parser::select
                },
                { tags: ["export-mbox "],
                  desc: "export-mbox PATH",
                  tokens: &[One(Literal("export-mbox")), One(Filepath)],
                  parser: parser::export_mbox
                },
                { tags: ["list-archive", "list-post", "list-unsubscribe", "list-"],
                  desc: "list-[unsubscribe/post/archive]",
                  tokens: &[One(Alternatives(&[to_stream!(One(Literal("list-archive"))), to_stream!(One(Literal("list-post"))), to_stream!(One(Literal("list-unsubscribe")))]))],
                  parser: parser::mailinglist
                },
                { tags: ["setenv "],
                  desc: "setenv VAR=VALUE",
                  tokens: &[One(Literal("setenv")), OneOrMore(Seq(&[One(AlphanumericStringValue), One(Literal("=")), One(QuotedStringValue)]))],
                  parser: parser::setenv
                },
                { tags: ["printenv "],
                  desc: "printenv VAR",
                  tokens: &[],
                  parser: parser::printenv
                },
                { tags: ["mailto "],
                  desc: "mailto MAILTO_ADDRESS",
                  tokens: &[One(Literal("mailto")), One(QuotedStringValue)],
                  parser: parser::mailto
                },
                /* Pipe pager contents to binary */
                { tags: ["pipe "],
                  desc: "pipe EXECUTABLE ARGS",
                  tokens: &[One(Literal("pipe")), One(Filepath), ZeroOrMore(QuotedStringValue)],
                  parser: parser::pipe
                },
                /* Filter pager contents through binary */
                { tags: ["filter "],
                  desc: "filter EXECUTABLE ARGS",
                  tokens: &[One(Literal("filter")), One(Filepath), ZeroOrMore(QuotedStringValue)],
                  parser: parser::filter
                },
                { tags: ["add-attachment ", "add-attachment-file-picker "],
                  desc: "add-attachment PATH",
                  tokens: &[One(
Alternatives(&[to_stream!(One(Literal("add-attachment")), One(Filepath)), to_stream!(One(Literal("add-attachment-file-picker")))]))],
                  parser: parser::add_attachment
                },
                { tags: ["remove-attachment "],
                  desc: "remove-attachment INDEX",
                  tokens: &[One(Literal("remove-attachment")), One(IndexValue)],
                  parser: parser::remove_attachment
                },
                { tags: ["save-draft"],
                  desc: "save draft",
                  tokens: &[One(Literal("save-draft"))],
                  parser: parser::save_draft
                },
                { tags: ["discard-draft"],
                  desc: "discard draft",
                  tokens: &[One(Literal("discard-draft"))],
                  parser: parser::discard_draft
                },
                { tags: ["toggle sign "],
                  desc: "switch between sign/unsign for this draft",
                  tokens: &[One(Literal("toggle")), One(Literal("sign"))],
                  parser: parser::toggle
                },
                { tags: ["toggle encrypt"],
                  desc: "toggle encryption for this draft",
                  tokens: &[One(Literal("toggle")), One(Literal("encrypt"))],
                  parser: parser::toggle
                },
                { tags: ["create-mailbox "],
                  desc: "create-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("create-mailbox")), One(AccountName), One(MailboxPath)],
                  parser: parser::create_mailbox
                },
                { tags: ["subscribe-mailbox "],
                  desc: "subscribe-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("subscribe-mailbox")), One(AccountName), One(MailboxPath)],
                  parser: parser::sub_mailbox
                },
                { tags: ["unsubscribe-mailbox "],
                  desc: "unsubscribe-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("unsubscribe-mailbox")), One(AccountName), One(MailboxPath)],
                  parser: parser::unsub_mailbox
                },
                { tags: ["rename-mailbox "],
                  desc: "rename-mailbox ACCOUNT MAILBOX_PATH_SRC MAILBOX_PATH_DEST",
                  tokens: &[One(Literal("rename-mailbox")), One(AccountName), One(MailboxPath), One(MailboxPath)],
                  parser: parser::rename_mailbox
                },
                { tags: ["delete-mailbox "],
                  desc: "delete-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("delete-mailbox")), One(AccountName), One(MailboxPath)],
                  parser: parser::delete_mailbox
                },
                { tags: ["reindex "],
                  desc: "reindex ACCOUNT, rebuild account cache in the background",
                  tokens: &[One(Literal("reindex")), One(AccountName)],
                  parser: parser::reindex
                },
                { tags: ["open-in-tab"],
                  desc: "opens envelope view in new tab",
                  tokens: &[One(Literal("open-in-tab"))],
                  parser: parser::open_in_new_tab
                },
                { tags: ["save-attachment "],
                  desc: "save-attachment INDEX PATH",
                  tokens: &[One(Literal("save-attachment")), One(AttachmentIndexValue), One(Filepath)],
                  parser: parser::save_attachment
                },
                { tags: ["export-mail "],
                  desc: "export-mail PATH",
                  tokens: &[One(Literal("export-mail")), One(Filepath)],
                  parser: parser::export_mail
                },
                { tags: ["add-addresses-to-contacts "],
                  desc: "add-addresses-to-contacts",
                  tokens: &[One(Literal("add-addresses-to-contacts"))],
                  parser: parser::add_addresses_to_contacts
                },
                { tags: ["tag", "tag add", "tag remove"],
                   desc: "tag [add/remove], edits message's tags.",
                   tokens: &[One(Literal("tag")), One(Alternatives(&[to_stream!(One(Literal("add"))), to_stream!(One(Literal("remove")))]))],
                   parser: parser::_tag
                },
                { tags: ["print "],
                  desc: "print ACCOUNT SETTING",
                  tokens: &[One(Literal("print")), One(AccountName), One(QuotedStringValue)],
                  parser: parser::print_account_setting
                },
                { tags: ["print "],
                  desc: "print SETTING",
                  tokens: &[One(Literal("print")), One(QuotedStringValue)],
                  parser: parser::print_setting
                },
                { tags: ["toggle mouse"],
                  desc: "toggle mouse support",
                  tokens: &[One(Literal("toggle")), One(Literal("mouse"))],
                  parser: parser::toggle
                },
                { tags: ["manage-mailboxes"],
                  desc: "view and manage mailbox preferences",
                  tokens: &[One(Literal("manage-mailboxes"))],
                  parser: parser::manage_mailboxes
                },
                { tags: ["man"],
                  desc: "read documentation",
                  tokens: {
                      #[cfg(feature = "cli-docs")]
                      {
                          &[One(Literal("man")), One(AlternativeStrings(crate::manpages::POSSIBLE_VALUES))]
                      }
                      #[cfg(not(feature = "cli-docs"))]
                      { &[] }
                  },
                  parser: parser::view_manpage
                },
                { tags: ["manage-jobs"],
                  desc: "view and manage jobs",
                  tokens: &[One(Literal("manage-jobs"))],
                  parser: parser::manage_jobs
                },
                { tags: ["quit"],
                  desc: "quit meli",
                  tokens: &[One(Literal("quit"))],
                  parser: parser::quit
                },
                { tags: ["reload-config"],
                  desc: "reload configuration file",
                  tokens: &[One(Literal("reload-config"))],
                  parser: parser::reload_config
                }
]);

/// Get command suggestions for input
pub fn command_completion_suggestions(input: &str) -> Vec<String> {
    use crate::melib::ShellExpandTrait;
    let mut sugg: HashSet<String> = Default::default();
    for (_tags, _desc, tokens, _) in COMMAND_COMPLETION.iter() {
        let _m = tokens.matches(&mut &(*input), &mut sugg);
        if _m.is_empty() {
            continue;
        }
        if let Some((s, Filepath)) = _m.last() {
            let p = std::path::Path::new(s);
            sugg.extend(p.complete(true, s.ends_with('/')).into_iter());
        }
    }
    sugg.into_iter()
        .map(|s| format!("{}{}", input, s.as_str()))
        .collect::<Vec<String>>()
}
