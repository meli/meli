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

/* Create a const table with every command part that can be auto-completed and its description */
macro_rules! define_commands {
    ( [$({ tags: [$( $tags:literal),*], desc: $desc:literal, parser: ($parser:item)}),*]) => {
        pub const COMMAND_COMPLETION: &[(&str, &str)] = &[$($( ($tags, $desc ) ),*),* ];
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
define_commands!([
                 { tags: ["set"],
                   desc: "set [seen/unseen], toggles message's Seen flag.",
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
                   parser: (
                       named!(close<Action>, map!(ws!(tag!("close")), |_| Tab(Close)));
                   )
                 },
                 { tags: ["goto"],
                   desc: "goto [n], switch to nth mailbox in this account",
                   parser: (
                       named!(
                           goto<Action>,
                           preceded!(tag!("go "), map!(call!(usize_c), Action::ViewMailbox))
                       );
                   )
                 },
                 { tags: ["subsort"],
                   desc: "subsort [date/subject] [asc/desc], sorts first level replies in threads.",
                   parser: (
                       named!(
                           subsort<Action>,
                           do_parse!(tag!("subsort ") >> p: pair!(sortfield, sortorder) >> (SubSort(p.0, p.1)))
                       );
                   )
                 },
                { tags: ["sort"],
                  desc: "sort [date/subject] [asc/desc], sorts threads.",
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
                  parser: (
                      named!(
                          toggle<Action>,
                          preceded!(tag!("set "), alt_complete!(threaded | plain | compact | conversations))
                      );
                  )
                },
                { tags: ["toggle_thread_snooze"],
                  desc: "turn off new notifications for this thread",
                  parser: (
                      named!(toggle_thread_snooze<Action>,
                             map!(ws!(tag!("toggle_thread_snooze")), |_| ToggleThreadSnooze)
                      );
                  )
                },
                { tags: ["search"],
                  desc: "search <TERM>, searches list with given term",
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
                  desc:"setenv VAR=VALUE",
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
