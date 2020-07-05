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

/*! A parser module for user commands passed through the Execute mode.
*/
use crate::melib::parser::BytesExt;
use melib::nom::{
    self,
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_until},
    character::complete::{digit1, not_line_ending},
    combinator::{map, map_res},
    multi::separated_list,
    sequence::{pair, preceded, separated_pair},
    IResult,
};
pub use melib::thread::{SortField, SortOrder};
use melib::MeliError;
pub mod actions;
use actions::MailboxOperation;
use std::collections::HashSet;
pub mod history;
pub use crate::actions::AccountAction::{self, *};
pub use crate::actions::Action::{self, *};
pub use crate::actions::ComposeAction::{self, *};
pub use crate::actions::ListingAction::{self, *};
pub use crate::actions::MailingListAction::{self, *};
pub use crate::actions::TabAction::{self, *};
pub use crate::actions::TagAction::{self, *};
pub use crate::actions::ViewAction::{self, *};
use std::str::FromStr;

/// Helper macro to convert an array of tokens into a TokenStream
macro_rules! to_stream {
    ($token: expr) => {
        TokenStream {
            tokens: &[$token],
        }
    };
    ($($tokens:expr),*) => {
        TokenStream {
            tokens: &[$($token),*],
        }
    };
}

/// Macro to create a const table with every command part that can be auto-completed and its description
macro_rules! define_commands {
    ( [$({ tags: [$( $tags:literal),*], desc: $desc:literal, tokens: $tokens:expr, parser: ($parser:item)}),*]) => {
        pub const COMMAND_COMPLETION: &[(&str, &str, TokenStream)] = &[$($( ($tags, $desc, TokenStream { tokens: $tokens } ) ),*),* ];
        $( $parser )*
    };
}

pub fn quoted_argument(input: &[u8]) -> IResult<&[u8], &str> {
    if input.is_empty() {
        return Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)));
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
        Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)))
    } else {
        map_res(is_not(" "), std::str::from_utf8)(input)
    }
}

#[derive(Debug, Copy, Clone)]
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
            //println!("{:?} {:?}", t, s);
            if s.is_empty() {
                match t.inner() {
                    Literal(lit) => {
                        sugg.insert(format!(" {}", lit));
                    }
                    Alternatives(v) => {
                        for t in v.iter() {
                            //println!("adding empty suggestions for {:?}", t);
                            let mut _s = *s;
                            t.matches(&mut _s, sugg);
                        }
                    }
                    Seq(_s) => {}
                    RestOfStringValue => {}
                    AttachmentIndexValue
                    | MailboxIndexValue
                    | IndexValue
                    | Filepath
                    | AccountName
                    | MailboxPath
                    | QuotedStringValue
                    | AlphanumericStringValue => {}
                }
                return tokens;
            }
            match t.inner() {
                Literal(lit) => {
                    if lit.starts_with(*s) && lit.len() != s.len() {
                        sugg.insert(lit[s.len()..].to_string());
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
                            tokens.extend(m.drain(..));
                            //println!("_s is empty {}", _s.is_empty());
                            cont = !_s.is_empty();
                            *s = _s;
                            break;
                        }
                    }
                    if !cont {
                        *s = "";
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

/// `Token` wrapper that defines how many times a token is expected to be repeated
#[derive(Debug, Copy, Clone)]
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
#[derive(Debug, Copy, Clone)]
pub enum Token {
    Literal(&'static str),
    Filepath,
    Alternatives(&'static [TokenStream]),
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
        Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)))
    }
}

define_commands!([
                 { tags: ["set"],
                   desc: "set [seen/unseen], toggles message's Seen flag.",
                   tokens: &[One(Literal("set")), One(Alternatives(&[to_stream!(One(Literal("seen"))), to_stream!(One(Literal("unseen")))]))],
                   parser: (
                       fn seen_flag<'a>(input: &'a [u8]) -> IResult<&'a [u8], Action> {
                           let (input, _) = tag("set")(input.trim())?;
                           let (input, _) = is_a(" ")(input)?;
                           alt((map(tag("seen"), |_| Listing(SetSeen)), map(tag("unseen"), |_| Listing(SetUnseen))))(input)
                       }
                     )
                 },
                 { tags: ["delete"],
                   desc: "delete message",
                   tokens: &[One(Literal("delete"))],
                   parser: (
                       fn delete_message<'a>(input: &'a [u8]) -> IResult<&'a [u8], Action> {
                           map(preceded(tag("delete"), eof), |_| Listing(Delete))(input)
                       }
                   )
                 },
                 { tags: ["copyto", "moveto"],
                   desc: "copy/move message",
                   tokens: &[One(Alternatives(&[to_stream!(One(Literal("copyto"))), to_stream!(One(Literal("moveto")))])), ZeroOrOne(AccountName), One(MailboxPath)],
                   parser: (
                       fn copymove<'a>(input: &'a [u8]) -> IResult<&'a [u8], Action> {
                             alt((
                                 |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("copyto")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, path) = quoted_argument(input)?;
                                      let (input, _) = eof(input)?;
                                      Ok( (input, { Listing(CopyTo(path.to_string())) }))
                                 },
                                 |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("copyto")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, account) = quoted_argument(input)?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, path) = quoted_argument(input)?;
                                      let (input, _) = eof(input)?;
                                      Ok( (input, { Listing(CopyToOtherAccount(account.to_string(), path.to_string())) }))
                                 },
                                 |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("moveto")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, path) = quoted_argument(input)?;
                                      let (input, _) = eof(input)?;
                                      Ok( (input, { Listing(MoveTo(path.to_string())) }))
                                 },
                                 |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("moveto")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, account) = quoted_argument(input)?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, path) = quoted_argument(input)?;
                                      let (input, _) = eof(input)?;
                                      Ok( (input, { Listing(MoveToOtherAccount(account.to_string(), path.to_string())) }))
                                 }
                             ))(input)
                       }
                   )
                 },
                 { tags: ["close"],
                   desc: "close non-sticky tabs",
                   tokens: &[One(Literal("close"))],
                   parser: (
                       fn close(input: &[u8]) -> IResult<&[u8], Action> {
                           map(tag("close"), |_| Tab(Close))(input.trim())
                       }
                   )
                 },
                 { tags: ["go"],
                   desc: "go [n], switch to nth mailbox in this account",
                   tokens: &[One(Literal("goto")), One(MailboxIndexValue)],
                   parser: (
                       fn goto(input: &[u8]) -> IResult<&[u8], Action> {
                           let (input, _) = tag("go")(input)?;
                           let (input, _) = is_a(" ")(input)?;
                           map(usize_c, Action::ViewMailbox)(input)
                       }
                   )
                 },
                 { tags: ["subsort"],
                   desc: "subsort [date/subject] [asc/desc], sorts first level replies in threads.",
                   tokens: &[One(Literal("subsort")), One(Alternatives(&[to_stream!(One(Literal("date"))), to_stream!(One(Literal("subject")))])), One(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                   parser: (
                       fn subsort(input: &[u8]) -> IResult<&[u8], Action> {
                           let (input, _) = tag("subsort")(input)?;
                           let (input, _) = is_a(" ")(input)?;
                           let (input, p) = pair(sortfield, sortorder)(input)?;
                           Ok((input, SubSort(p.0, p.1)))
                       }
                   )
                 },
                { tags: ["sort"],
                  desc: "sort [date/subject] [asc/desc], sorts threads.",
                   tokens: &[One(Literal("sort")), One(Alternatives(&[to_stream!(One(Literal("date"))), to_stream!(One(Literal("subject")))])), One(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                  parser: (
                      fn sort(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("sort")(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, p) = separated_pair(sortfield, tag(" "), sortorder)(input)?;
                          Ok((input, (Sort(p.0, p.1))))
                      }
                  )},
                { tags: ["set", "set plain", "set threaded", "set compact"],
                  desc: "set [plain/threaded/compact/conversations], changes the mail listing view",
                  tokens: &[One(Literal("set")), One(Alternatives(&[to_stream!(One(Literal("plain"))), to_stream!(One(Literal("threaded"))), to_stream!(One(Literal("compact"))), to_stream!(One(Literal("conversations")))]))],
                  parser: (
                      fn toggle(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("set")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          alt((threaded, plain, compact, conversations))(input)
                      }
                  )
                },
                { tags: ["toggle_thread_snooze"],
                  desc: "turn off new notifications for this thread",
                  tokens: &[One(Literal("toggle_thread_snooze"))],
                  parser: (
                      fn toggle_thread_snooze(input: &[u8]) -> IResult<&[u8], Action> {
                          map(tag("toggle_thread_snooze"), |_| ToggleThreadSnooze)(input.trim())
                      }
                  )
                },
                { tags: ["search"],
                  desc: "search <TERM>, searches list with given term",
                  tokens: &[One(Literal("search")), One(RestOfStringValue)],
                  parser:(
                      fn search(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("search")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, string) = map_res(not_line_ending, std::str::from_utf8)(input)?;
                          Ok((input, Listing(Search(String::from(string)))))
                      }
                  )
                },
                { tags: ["list-archive", "list-post", "list-unsubscribe", "list-"],
                  desc: "list-[unsubscribe/post/archive]",
                  tokens: &[One(Alternatives(&[to_stream!(One(Literal("list-archive"))), to_stream!(One(Literal("list-post"))), to_stream!(One(Literal("list-unsubscribe")))]))],
                  parser: (
                      fn mailinglist(input: &[u8]) -> IResult<&[u8], Action> {
                          alt((
                              map(tag("list-post"), |_| MailingListAction(ListPost))
                              , map(tag("list-unsubscribe"), |_| MailingListAction(
                                      ListUnsubscribe
                              ))
                              , map(tag("list-archive"), |_| MailingListAction(
                                      ListArchive
                              ))
                          ))(input.trim())
                      }
                  )
                },
                { tags: ["setenv "],
                  desc: "setenv VAR=VALUE",
                  tokens: &[One(Literal("setenv")), OneOrMore(Seq(&[One(AlphanumericStringValue), One(Literal("=")), One(QuotedStringValue)]))],
                  parser: (
                      fn setenv(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("setenv")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, key) = map_res(take_until("="), std::str::from_utf8)(input)?;
                          let (input, _) = tag("=")(input.trim())?;
                          let (input, val) = map_res(not_line_ending, std::str::from_utf8)(input)?;
                          Ok((input, SetEnv(key.to_string(), val.to_string())))
                      }
                  )
                },
                { tags: ["printenv "],
                  desc: "printenv VAR",
                  tokens: &[],
                  parser:(
                      fn printenv(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("printenv")(input.ltrim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, key) = map_res(not_line_ending, std::str::from_utf8)(input.trim())?;
                          Ok((input, PrintEnv(key.to_string())))
                      }
                  )
                },
                /* Pipe pager contents to binary */
                { tags: ["pipe "],
                  desc: "pipe EXECUTABLE ARGS",
                  tokens: &[One(Literal("pipe")), One(Filepath), ZeroOrMore(QuotedStringValue)],
                  parser:(
                      fn pipe<'a>(input: &'a [u8]) -> IResult<&'a [u8], Action> {
                          alt((
                                  |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("pipe")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, bin) = quoted_argument(input)?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, args) = separated_list(is_a(" "), quoted_argument)(input)?;
                                      Ok((input, {
                                          View(Pipe(bin.to_string(), args.into_iter().map(String::from).collect::<Vec<String>>()))
                                      }))
                                  },
                                      |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                          let (input, _) = tag("pipe")(input.trim())?;
                                          let (input, _) = is_a(" ")(input)?;
                                          let (input, bin) = quoted_argument(input.trim())?;
                                          Ok((input, {
                                              View(Pipe(bin.to_string(), Vec::new()))
                                          }))
                                      }
                          ))(input)
                      }
                  )
                },
                { tags: ["add-attachment "],
                  desc: "add-attachment PATH",
                  tokens: &[One(Literal("add-attachment")), One(Filepath)],
                  parser:(
                      fn add_attachment<'a>(input: &'a [u8]) -> IResult<&'a [u8], Action> {
                          alt((
                                  |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("add-attachment")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, _) = tag("<")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, cmd) = quoted_argument(input)?;
                                      Ok((input, Compose(AddAttachmentPipe(cmd.to_string()))))
                                  }, |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                      let (input, _) = tag("add-attachment")(input.trim())?;
                                      let (input, _) = is_a(" ")(input)?;
                                      let (input, path) = quoted_argument(input)?;
                                      Ok((input, Compose(AddAttachment(path.to_string()))))
                                  }
                              ))(input)
                      }
                  )
                },
                { tags: ["remove-attachment "],
                  desc: "remove-attachment INDEX",
                  tokens: &[One(Literal("remove-attachment")), One(IndexValue)],
                  parser:(
                      fn remove_attachment(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("remove-attachment")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, idx) = map_res(quoted_argument, usize::from_str)(input)?;
                          Ok((input, Compose(RemoveAttachment(idx))))
                      }
                  )
                },
                { tags: ["save-draft"],
                  desc: "save draft",
                  tokens: &[One(Literal("save-draft"))],
                  parser:(
                      fn save_draft(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("save-draft")(input.trim())?;
                          let (input, _) = eof(input)?;
                          Ok((input, Compose(SaveDraft)))
                      }
                  )
                },
                { tags: ["toggle sign "],
                  desc: "switch between sign/unsign for this draft",
                  tokens: &[One(Literal("toggle")), One(Literal("sign"))],
                  parser:(
                      fn toggle_sign(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("toggle")(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, _) = tag("sign")(input)?;
                          Ok((input, Compose(ToggleSign)))
                      }
                  )
                },
                { tags: ["create-mailbox "],
                  desc: "create-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("create-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      fn create_mailbox(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("create-mailbox")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, account) = quoted_argument(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, path) = quoted_argument(input)?;
                          Ok((input,Mailbox(account.to_string(), MailboxOperation::Create(path.to_string()))))
                      }
                  )
                },
                { tags: ["subscribe-mailbox "],
                  desc: "subscribe-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("subscribe-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      fn sub_mailbox(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("subscribe-mailbox")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, account) = quoted_argument(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, path) = quoted_argument(input)?;
                          Ok((input,Mailbox(account.to_string(), MailboxOperation::Subscribe(path.to_string()))))
                      }
                  )
                },
                { tags: ["unsubscribe-mailbox "],
                  desc: "unsubscribe-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("unsubscribe-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      fn unsub_mailbox(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("unsubscribe-mailbox")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, account) = quoted_argument(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, path) = quoted_argument(input)?;
                          Ok((input, Mailbox(account.to_string(), MailboxOperation::Unsubscribe(path.to_string()))))
                      }
                  )
                },
                { tags: ["rename-mailbox "],
                  desc: "rename-mailbox ACCOUNT MAILBOX_PATH_SRC MAILBOX_PATH_DEST",
                  tokens: &[One(Literal("rename-mailbox")), One(AccountName), One(MailboxPath), One(MailboxPath)],
                  parser:(
                      fn rename_mailbox(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("rename-mailbox")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, account) = quoted_argument(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, src) = quoted_argument(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, dest) = quoted_argument(input)?;
                          Ok((input, Mailbox(account.to_string(), MailboxOperation::Rename(src.to_string(), dest.to_string()))))
                      }
                  )
                },
                { tags: ["delete-mailbox "],
                  desc: "delete-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("delete-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      fn delete_mailbox(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("delete-mailbox")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, account) = quoted_argument(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, path) = quoted_argument(input)?;
                          Ok ((input, Mailbox(account.to_string(), MailboxOperation::Delete(path.to_string()))))
                      }
                  )
                },
                { tags: ["reindex "],
                  desc: "reindex ACCOUNT, rebuild account cache in the background",
                  tokens: &[One(Literal("reindex")), One(AccountName)],
                  parser:(
                      fn reindex(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("reindex")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, account) = quoted_argument(input)?;
                          Ok( (input, AccountAction(account.to_string(), ReIndex)))
                      }
                  )
                },
                { tags: ["open-in-tab"],
                  desc: "opens envelope view in new tab",
                  tokens: &[One(Literal("open-in-tab"))],
                  parser:(
                      fn open_in_new_tab(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("open-in-tab")(input.trim())?;
                          Ok((input, Listing(OpenInNewTab)))
                      }
                  )
                },
                { tags: ["save-attachment "],
                  desc: "save-attachment INDEX PATH",
                  tokens: &[One(Literal("save-attachment")), One(AttachmentIndexValue), One(Filepath)],
                  parser:(
                      fn save_attachment(input: &[u8]) -> IResult<&[u8], Action> {
                          let (input, _) = tag("save-attachment")(input.trim())?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, idx) = map_res(quoted_argument, usize::from_str)(input)?;
                          let (input, _) = is_a(" ")(input)?;
                          let (input, path) = quoted_argument(input.trim())?;
                          Ok((input, View(SaveAttachment(idx, path.to_string()))))
                      }
                  )
                },
                { tags: ["tag", "tag add", "tag remove"],
                   desc: "tag [add/remove], edits message's tags.",
                   tokens: &[One(Literal("tag")), One(Alternatives(&[to_stream!(One(Literal("add"))), to_stream!(One(Literal("remove")))]))],
                   parser: (
                       fn _tag<'a>(input: &'a [u8]) -> IResult<&'a [u8], Action> {
                           preceded(
                               tag("tag"),
                               alt((|input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                   let (input, _) = tag("add")(input.trim())?;
                                   let (input, _) = is_a(" ")(input)?;
                                   let (input, tag) = quoted_argument(input.trim())?;
                                   Ok((input, Listing(Tag(Add(tag.to_string())))))
                               }, |input: &'a [u8]| -> IResult<&'a [u8], Action> {
                                   let (input, _) = tag("remove")(input.trim())?;
                                   let (input, _) = is_a(" ")(input)?;
                                   let (input, tag) = quoted_argument(input.trim())?;
                                   Ok((input, Listing(Tag(Remove(tag.to_string())))))
                               }
                               ))
                           )(input.trim())
                       }
                   )
                 }
]);

fn usize_c(input: &[u8]) -> IResult<&[u8], usize> {
    map_res(
        map_res(digit1, std::str::from_utf8),
        std::str::FromStr::from_str,
    )(input.trim())
}

fn sortfield(input: &[u8]) -> IResult<&[u8], SortField> {
    map_res(
        map_res(take_until(" "), std::str::from_utf8),
        std::str::FromStr::from_str,
    )(input.trim())
}

fn sortorder(input: &[u8]) -> IResult<&[u8], SortOrder> {
    map_res(
        map_res(not_line_ending, std::str::from_utf8),
        std::str::FromStr::from_str,
    )(input)
}

fn threaded(input: &[u8]) -> IResult<&[u8], Action> {
    map(tag("threaded"), |_| Listing(SetThreaded))(input.trim())
}

fn plain(input: &[u8]) -> IResult<&[u8], Action> {
    map(tag("plain"), |_| Listing(SetPlain))(input.trim())
}

fn compact(input: &[u8]) -> IResult<&[u8], Action> {
    map(tag("compact"), |_| Listing(SetCompact))(input.trim())
}

fn conversations(input: &[u8]) -> IResult<&[u8], Action> {
    map(tag("conversations"), |_| Listing(SetConversations))(input.trim())
}

fn listing_action(input: &[u8]) -> IResult<&[u8], Action> {
    alt((
        toggle,
        seen_flag,
        delete_message,
        copymove,
        search,
        toggle_thread_snooze,
        open_in_new_tab,
        _tag,
    ))(input)
}

fn compose_action(input: &[u8]) -> IResult<&[u8], Action> {
    alt((add_attachment, remove_attachment, toggle_sign, save_draft))(input)
}

fn account_action(input: &[u8]) -> IResult<&[u8], Action> {
    reindex(input)
}

fn view(input: &[u8]) -> IResult<&[u8], Action> {
    alt((pipe, save_attachment))(input)
}

pub fn parse_command(input: &[u8]) -> Result<Action, MeliError> {
    alt((
        goto,
        listing_action,
        sort,
        subsort,
        close,
        mailinglist,
        setenv,
        printenv,
        view,
        compose_action,
        create_mailbox,
        sub_mailbox,
        unsub_mailbox,
        delete_mailbox,
        rename_mailbox,
        account_action,
    ))(input)
    .map(|(_, v)| v)
    .map_err(|err| err.into())
}

#[test]
#[ignore]
fn test_parser() {
    use std::io;
    let mut input = String::new();
    loop {
        input.clear();
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                let mut sugg = Default::default();
                //print!("{}", input);
                for (_tags, desc, tokens) in COMMAND_COMPLETION.iter() {
                    let m = tokens.matches(&mut input.as_str().trim(), &mut sugg);
                    if !m.is_empty() {
                        print!("{:?} ", desc);
                        println!(" result = {:#?}\n\n", m);
                    }
                }
                println!(
                    "suggestions = {:#?}",
                    sugg.into_iter()
                        .map(|s| format!(
                            "{}{}",
                            input.as_str().trim(),
                            if input.trim().is_empty() {
                                s.trim()
                            } else {
                                s.as_str()
                            }
                        ))
                        .collect::<Vec<String>>()
                );
                if input.trim() == "quit" {
                    break;
                }
            }
            Err(error) => println!("error: {}", error),
        }
    }
    println!("alright");
}

/// Get command suggestions for input
pub fn command_completion_suggestions(input: &str) -> Vec<String> {
    use crate::melib::ShellExpandTrait;
    let mut sugg = Default::default();
    for (_tags, _desc, tokens) in COMMAND_COMPLETION.iter() {
        let _m = tokens.matches(&mut &(*input), &mut sugg);
        if _m.is_empty() {
            continue;
        }
        if let Some((s, Filepath)) = _m.last() {
            let p = std::path::Path::new(s);
            sugg.extend(p.complete(true).into_iter().map(|m| m.into()));
        }
    }
    sugg.into_iter()
        .map(|s| {
            format!(
                "{}{}",
                input.trim(),
                if input.trim().is_empty() {
                    s.trim()
                } else {
                    s.as_str()
                }
            )
        })
        .collect::<Vec<String>>()
}
