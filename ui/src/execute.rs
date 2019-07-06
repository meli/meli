/*
 * meli - ui crate.
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

/*! A parser module for user commands passed through the Ex mode.
*/
pub use melib::mailbox::{SortField, SortOrder};
use nom::{digit, not_line_ending};
use std;
pub mod actions;
pub use crate::actions::Action::{self, *};
pub use crate::actions::ListingAction::{self, *};
pub use crate::actions::MailingListAction::{self, *};
pub use crate::actions::TabAction::{self, *};

/* Create a const table with every command part that can be auto-completed and its description */
macro_rules! define_commands {
    ( [$({ tags: [$( $tags:literal),*], desc: $desc:literal, parser: ($parser:item)}),*]) => {
        pub const COMMAND_COMPLETION: &[(&str, &str)] = &[$($( ($tags, $desc ) ),*),* ];
        $( $parser )*
    };
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
                                         map!(ws!(tag!("read")), |_| Listing(SetRead))
                                         | map!(ws!(tag!("unread")), |_| Listing(SetUnread))
                                     )
                                 ) | map!(ws!(tag!("delete")), |_| Listing(Delete))
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
                  desc: "set [plain/threaded/compact], changes the mail listing view",
                  parser: (
                      named!(
                          toggle<Action>,
                          preceded!(tag!("set "), alt_complete!(threaded | plain | compact))
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
                { tags: ["filter"],
                  desc: "filter <TERM>, filters list with given term",
                  parser:(
                      named!(filter<Action>,
                             do_parse!(
                                 ws!(tag!("filter"))
                                 >> string: map_res!(call!(not_line_ending), std::str::from_utf8)
                                 >> (Listing(Filter(String::from(string))))
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
    listing_action<Action>,
    alt_complete!(toggle | envelope_action | filter | toggle_thread_snooze)
);
named!(pub parse_command<Action>,
       alt_complete!( goto | listing_action | sort | subsort | close | mailinglist | setenv | printenv)
);
