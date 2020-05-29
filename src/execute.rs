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
pub use melib::thread::{SortField, SortOrder};
use nom::{digit, not_line_ending, IResult};
use std;
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
        return IResult::Error(nom::ErrorKind::Custom(0));
    }

    if input[0] == b'"' {
        let mut i = 1;
        while i < input.len() {
            if input[i] == b'\"' && input[i - 1] != b'\\' {
                return IResult::Done(&input[i + 1..], unsafe {
                    std::str::from_utf8_unchecked(&input[1..i])
                });
            }
            i += 1;
        }
        return IResult::Error(nom::ErrorKind::Custom(0));
    } else {
        return map_res!(input, is_not!(" "), std::str::from_utf8);
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
                    IndexValue
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
                IndexValue
                | Filepath
                | AccountName
                | MailboxPath
                | QuotedStringValue
                | AlphanumericStringValue => {
                    let mut ptr = 0;
                    while ptr + 1 < s.len() && !s.as_bytes()[ptr].is_ascii_whitespace() {
                        ptr += 1;
                    }
                    tokens.push((&s[..ptr], *t.inner()));
                    *s = &s[ptr..];
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
    IndexValue,
}

define_commands!([
                 { tags: ["set"],
                   desc: "set [seen/unseen], toggles message's Seen flag.",
                   tokens: &[One(Literal("set")), One(Alternatives(&[to_stream!(One(Literal("seen"))), to_stream!(One(Literal("unseen")))]))],
                   parser:
                     ( named!(
                             envelope_action<Action>,
                             alt_complete!(
                                 preceded!(
                                     ws!(tag!("set")),
                                     alt_complete!(
                                         map!(ws!(tag!("seen")), |_| Listing(SetSeen))
                                         | map!(ws!(tag!("unseen")), |_| Listing(SetUnseen))
                                     )
                                 ) | map!(preceded!(tag!("delete"), eof!()), |_| Listing(Delete))
                                  | do_parse!(tag!("copyto")
                                      >> is_a!(" ")
                                      >> path: quoted_argument
                                      >> ({ Listing(CopyTo(path.to_string())) }))
                             )
                     ); )
                 },
                 { tags: ["close"],
                   desc: "close non-sticky tabs",
                   tokens: &[One(Literal("close"))],
                   parser: (
                       named!(close<Action>, map!(ws!(tag!("close")), |_| Tab(Close)));
                   )
                 },
                 { tags: ["go"],
                   desc: "go [n], switch to nth mailbox in this account",
                   tokens: &[One(Literal("goto")), One(IndexValue)],
                   parser: (
                       named!(
                           goto<Action>,
                           preceded!(tag!("go "), map!(call!(usize_c), Action::ViewMailbox))
                       );
                   )
                 },
                 { tags: ["subsort"],
                   desc: "subsort [date/subject] [asc/desc], sorts first level replies in threads.",
                   tokens: &[One(Literal("subsort")), One(Alternatives(&[to_stream!(One(Literal("date"))), to_stream!(One(Literal("subject")))])), One(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                   parser: (
                       named!(
                           subsort<Action>,
                           do_parse!(tag!("subsort ") >> p: pair!(sortfield, sortorder) >> (SubSort(p.0, p.1)))
                       );
                   )
                 },
                { tags: ["sort"],
                  desc: "sort [date/subject] [asc/desc], sorts threads.",
                   tokens: &[One(Literal("sort")), One(Alternatives(&[to_stream!(One(Literal("date"))), to_stream!(One(Literal("subject")))])), One(Alternatives(&[to_stream!(One(Literal("asc"))), to_stream!(One(Literal("desc")))])) ],
                  parser: (
                      named!(
                          sort<Action>,
                          do_parse!(
                              tag!("sort ") >> p: separated_pair!(sortfield, tag!(" "), sortorder) >> (Sort(p.0, p.1))
                          )
                      );
                  )
                },
                { tags: ["set", "set plain", "set threaded", "set compact"],
                  desc: "set [plain/threaded/compact/conversations], changes the mail listing view",
                  tokens: &[One(Literal("set")), One(Alternatives(&[to_stream!(One(Literal("plain"))), to_stream!(One(Literal("threaded"))), to_stream!(One(Literal("compact"))), to_stream!(One(Literal("conversations")))]))],
                  parser: (
                      named!(
                          toggle<Action>,
                          preceded!(tag!("set "), alt_complete!(threaded | plain | compact | conversations))
                      );
                  )
                },
                { tags: ["toggle_thread_snooze"],
                  desc: "turn off new notifications for this thread",
                  tokens: &[One(Literal("toggle_thread_snooze"))],
                  parser: (
                      named!(toggle_thread_snooze<Action>,
                             map!(ws!(tag!("toggle_thread_snooze")), |_| ToggleThreadSnooze)
                      );
                  )
                },
                { tags: ["search"],
                  desc: "search <TERM>, searches list with given term",
                  tokens: &[One(Literal("search")), One(RestOfStringValue)],
                  parser:(
                      named!(search<Action>,
                             do_parse!(
                                 ws!(tag!("search"))
                                 >> string: map_res!(call!(not_line_ending), std::str::from_utf8)
                                 >> (Listing(Search(String::from(string))))
                             )
                      );
                  )
                },
                { tags: ["list-archive", "list-post", "list-unsubscribe", "list-"],
                  desc: "list-[unsubscribe/post/archive]",
                  tokens: &[One(Alternatives(&[to_stream!(One(Literal("list-archive"))), to_stream!(One(Literal("list-post"))), to_stream!(One(Literal("list-unsubscribe")))]))],
                  parser: (
                      named!(
                          mailinglist<Action>,
                          alt_complete!(
                              map!(ws!(tag!("list-post")), |_| MailingListAction(ListPost))
                              | map!(ws!(tag!("list-unsubscribe")), |_| MailingListAction(
                                      ListUnsubscribe
                              ))
                              | map!(ws!(tag!("list-archive")), |_| MailingListAction(
                                      ListArchive
                              ))
                          )
                      );
                  )
                },
                { tags: ["setenv "],
                  desc: "setenv VAR=VALUE",
                  tokens: &[One(Literal("setenv")), OneOrMore(Seq(&[One(AlphanumericStringValue), One(Literal("=")), One(QuotedStringValue)]))],
                  parser: (
                      named!( setenv<Action>,
                              do_parse!(
                                  ws!(tag!("setenv"))
                                  >> key: map_res!(take_until1!("="), std::str::from_utf8)
                                  >> ws!(tag!("="))
                                  >> val: map_res!(call!(not_line_ending), std::str::from_utf8)
                                  >> (SetEnv(key.to_string(), val.to_string()))
                              )
                      );
                  )
                },
                { tags: ["printenv "],
                  desc: "printenv VAR",
                  tokens: &[],
                  parser:(
                      named!( printenv<Action>,
                              do_parse!(
                                  ws!(tag!("env"))
                                  >> key: map_res!(call!(not_line_ending), std::str::from_utf8)
                                  >> (PrintEnv(key.to_string()))
                              )
                      );
                  )
                },
                /* Pipe pager contents to binary */
                { tags: ["pipe "],
                  desc: "pipe EXECUTABLE ARGS",
                  tokens: &[One(Literal("pipe")), One(Filepath), ZeroOrMore(QuotedStringValue)],
                  parser:(
                      named!( pipe<Action>,
                              alt_complete!(
                                  do_parse!(
                                  ws!(tag!("pipe"))
                                  >> bin: quoted_argument
                                  >> is_a!(" ")
                                  >> args: separated_list!(is_a!(" "), quoted_argument)
                                  >> ({
                                      View(Pipe(bin.to_string(), args.into_iter().map(String::from).collect::<Vec<String>>()))
                                  })) | do_parse!(
                                          ws!(tag!("pipe"))
                                          >> bin: ws!(quoted_argument)
                                          >> ({
                                              View(Pipe(bin.to_string(), Vec::new()))
                                          })
                                  ))
                      );
                  )
                },
                { tags: ["add-attachment "],
                  desc: "add-attachment PATH",
                  tokens: &[One(Literal("add-attachment")), One(Filepath)],
                  parser:(
                      named!( add_attachment<Action>,
                              alt_complete!(
                                   do_parse!(
                                  ws!(tag!("add-attachment"))
                                  >> ws!(tag!("<"))
                                  >> cmd: quoted_argument
                                  >> (Compose(AddAttachmentPipe(cmd.to_string()))))
                                  | do_parse!(
                                  ws!(tag!("add-attachment"))
                                  >> path: quoted_argument
                                  >> (Compose(AddAttachment(path.to_string()))))
                              )
                      );
                  )
                },
                { tags: ["remove-attachment "],
                  desc: "remove-attachment INDEX",
                  tokens: &[One(Literal("remove-attachment")), One(IndexValue)],
                  parser:(
                      named!( remove_attachment<Action>,
                              do_parse!(
                                  ws!(tag!("remove-attachment"))
                                  >> idx: map_res!(quoted_argument, usize::from_str)
                                  >> (Compose(RemoveAttachment(idx)))
                              )
                      );
                  )
                },
                { tags: ["toggle sign "],
                  desc: "switch between sign/unsign for this draft",
                  tokens: &[One(Literal("toggle")), One(Literal("sign"))],
                  parser:(
                      named!( toggle_sign<Action>,
                              do_parse!(
                                  ws!(tag!("toggle sign"))
                                  >> (Compose(ToggleSign))
                              )
                      );
                  )
                },
                { tags: ["create-mailbox "],
                  desc: "create-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("create-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      named!( create_mailbox<Action>,
                              do_parse!(
                                  ws!(tag!("create-mailbox"))
                                  >> account: quoted_argument
                                  >> is_a!(" ")
                                  >> path: quoted_argument
                                  >> (Mailbox(account.to_string(), MailboxOperation::Create(path.to_string())))
                              )
                      );
                  )
                },
                { tags: ["subscribe-mailbox "],
                  desc: "subscribe-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("subscribe-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      named!( sub_mailbox<Action>,
                              do_parse!(
                                  ws!(tag!("subscribe-mailbox"))
                                  >> account: quoted_argument
                                  >> is_a!(" ")
                                  >> path: quoted_argument
                                  >> (Mailbox(account.to_string(), MailboxOperation::Subscribe(path.to_string())))
                              )
                      );
                  )
                },
                { tags: ["unsubscribe-mailbox "],
                  desc: "unsubscribe-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("unsubscribe-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      named!( unsub_mailbox<Action>,
                              do_parse!(
                                  ws!(tag!("unsubscribe-mailbox"))
                                  >> account: quoted_argument
                                  >> is_a!(" ")
                                  >> path: quoted_argument
                                  >> (Mailbox(account.to_string(), MailboxOperation::Unsubscribe(path.to_string())))
                              )
                      );
                  )
                },
                { tags: ["rename-mailbox "],
                  desc: "rename-mailbox ACCOUNT MAILBOX_PATH_SRC MAILBOX_PATH_DEST",
                  tokens: &[One(Literal("rename-mailbox")), One(AccountName), One(MailboxPath), One(MailboxPath)],
                  parser:(
                      named!( rename_mailbox<Action>,
                              do_parse!(
                                  ws!(tag!("rename-mailbox"))
                                  >> account: quoted_argument
                                  >> is_a!(" ")
                                  >> src: quoted_argument
                                  >> is_a!(" ")
                                  >> dest: quoted_argument
                                  >> (Mailbox(account.to_string(), MailboxOperation::Rename(src.to_string(), dest.to_string())))
                              )
                      );
                  )
                },
                { tags: ["delete-mailbox "],
                  desc: "delete-mailbox ACCOUNT MAILBOX_PATH",
                  tokens: &[One(Literal("delete-mailbox")), One(AccountName), One(MailboxPath)],
                  parser:(
                      named!( delete_mailbox<Action>,
                              do_parse!(
                                  ws!(tag!("delete-mailbox"))
                                  >> account: quoted_argument
                                  >> is_a!(" ")
                                  >> path: quoted_argument
                                  >> (Mailbox(account.to_string(), MailboxOperation::Delete(path.to_string())))
                              )
                      );
                  )
                },
                { tags: ["reindex "],
                  desc: "reindex ACCOUNT, rebuild account cache in the background",
                  tokens: &[One(Literal("reindex")), One(AccountName)],
                  parser:(
                      named!( reindex<Action>,
                              do_parse!(
                                  ws!(tag!("reindex"))
                                  >> account: quoted_argument
                                  >> (AccountAction(account.to_string(), ReIndex))
                              )
                      );
                  )
                },
                { tags: ["open-in-tab"],
                  desc: "opens envelope view in new tab",
                  tokens: &[One(Literal("open-in-tab"))],
                  parser:(
                      named!( open_in_new_tab<Action>,
                              do_parse!(
                                  ws!(tag!("open-in-tab"))
                                  >> (Listing(OpenInNewTab))
                              )
                      );
                  )
                },
                { tags: ["save-attachment "],
                  desc: "save-attachment INDEX PATH",
                  tokens: &[One(Literal("save-attachment")), One(IndexValue), One(Filepath)],
                  parser:(
                      named!( save_attachment<Action>,
                              do_parse!(
                                  ws!(tag!("save-attachment"))
                                  >> idx: map_res!(quoted_argument, usize::from_str)
                                  >> path: ws!(quoted_argument)
                                  >> (View(SaveAttachment(idx, path.to_string())))
                              )
                      );
                  )
                },
                { tags: ["tag", "tag add", "tag remove"],
                   desc: "tag [add/remove], edits message's tags.",
                   tokens: &[One(Literal("tag")), One(Alternatives(&[to_stream!(One(Literal("add"))), to_stream!(One(Literal("remove")))]))],
                   parser:
                     ( named!(
                             tag<Action>,
                                 preceded!(
                                     ws!(tag!("tag")),
                                     alt_complete!(
                                         do_parse!(
                                         ws!(tag!("add"))
                                         >> tag: ws!(quoted_argument)
                                         >> (Listing(Tag(Add(tag.to_string())))))
                                         | do_parse!(
                                         ws!(tag!("remove"))
                                         >> tag: ws!(quoted_argument)
                                         >> (Listing(Tag(Remove(tag.to_string())))))

                                     )
                                 )
                     ); )
                 }
]);

named!(
    usize_c<usize>,
    map_res!(
        map_res!(ws!(digit), std::str::from_utf8),
        std::str::FromStr::from_str
    )
);

named!(
    sortfield<SortField>,
    map_res!(
        map_res!(take_until_s!(" "), std::str::from_utf8),
        std::str::FromStr::from_str
    )
);

named!(
    sortorder<SortOrder>,
    map_res!(
        map_res!(call!(not_line_ending), std::str::from_utf8),
        std::str::FromStr::from_str
    )
);

named!(
    threaded<Action>,
    map!(ws!(tag!("threaded")), |_| Listing(SetThreaded))
);

named!(
    plain<Action>,
    map!(ws!(tag!("plain")), |_| Listing(SetPlain))
);

named!(
    compact<Action>,
    map!(ws!(tag!("compact")), |_| Listing(SetCompact))
);

named!(
    conversations<Action>,
    map!(ws!(tag!("conversations")), |_| Listing(SetConversations))
);

named!(
    listing_action<Action>,
    alt_complete!(toggle | envelope_action | search | toggle_thread_snooze | open_in_new_tab | tag)
);

named!(
    compose_action<Action>,
    alt_complete!(add_attachment | remove_attachment | toggle_sign)
);

named!(account_action<Action>, alt_complete!(reindex));

named!(view<Action>, alt_complete!(pipe | save_attachment));

named!(pub parse_command<Action>,
       alt_complete!( goto | listing_action | sort | subsort | close | mailinglist | setenv | printenv | view | compose_action | create_mailbox | sub_mailbox | unsub_mailbox | delete_mailbox | rename_mailbox | account_action )
);

#[test]
fn test_parser() {
    use std::io::{self, Read};
    let mut state: Vec<Token> = vec![];
    let mut buffer = String::new();
    let mut input = String::new();
    loop {
        input.clear();
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
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
    let mut sugg = Default::default();
    for (_tags, _desc, tokens) in COMMAND_COMPLETION.iter() {
        let _m = tokens.matches(&mut &(*input), &mut sugg);
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
