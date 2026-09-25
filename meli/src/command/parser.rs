/*
 * meli
 *
 * Copyright 2017 Manos Pitsidianakis
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

//! Command parsing.

use std::borrow::Cow;

use melib::{
    nom::{
        self,
        branch::alt,
        bytes::complete::{is_a, tag, take_until},
        character::complete::{digit1, not_line_ending},
        combinator::{map, map_res},
        error::{Error as NomError, FromExternalError},
        multi::separated_list1,
        sequence::{pair, preceded, separated_pair},
        IResult,
    },
    SortField, SortOrder,
};

use super::*;
use crate::{
    actions::FileAction,
    command::{argcheck::*, error::*},
};

const FLAG_SUGGESTIONS: &[&str] = &[
    "passed",
    "replied",
    "seen or read",
    "junk or trash or trashed",
    "draft",
    "flagged",
];

macro_rules! command_err {
    (nom $b:expr, $input: expr, $msg:expr, $suggs:expr) => {{
        let evaluated: IResult<&str, _> = { $b };
        match evaluated {
            Err(_) => {
                let err = CommandError::BadValue {
                    inner: $msg.into(),
                    suggestions: $suggs,
                };
                return Ok(($input, Err(err)));
            }
            Ok(v) => v,
        }
    }};
    ($b:expr, $input: expr, $msg:expr, $suggs:expr) => {{
        let evaluated = { $b };
        match evaluated {
            Err(_) => {
                let err = CommandError::BadValue {
                    inner: $msg.into(),
                    suggestions: $suggs,
                };
                return Ok(($input, Err(err)));
            }
            Ok(v) => v,
        }
    }};
}

macro_rules! tag {
    () => {{
        tag::<&'_ str, &'_ str, melib::nom::error::Error<&str>>
    }};
}

fn usize_c(input: &str) -> IResult<&str, usize> {
    let (input, digits) = digit1(input)?;
    let digits = digits.parse().map_err(|err| {
        nom::Err::Error(melib::nom::error::Error::from_external_error(
            digits,
            nom::error::ErrorKind::MapRes,
            err,
        ))
    })?;
    Ok((input, digits))
}

fn eof(input: &str) -> IResult<&str, ()> {
    if input.is_empty() {
        Ok((input, ()))
    } else {
        Err(nom::Err::Error(NomError {
            input,
            code: nom::error::ErrorKind::Tag,
        }))
    }
}

fn quoted_argument(input: &'_ str) -> IResult<&str, LexToken<'_>> {
    let mut lexer = Lexer::new(input);
    if input.is_empty() {
        return Err(nom::Err::Error(NomError {
            input,
            code: nom::error::ErrorKind::Tag,
        }));
    }
    let lex_token = lexer
        .next()
        .ok_or(nom::Err::Error(NomError {
            input,
            code: nom::error::ErrorKind::Tag,
        }))?
        .map_err(|_| {
            nom::Err::Error(NomError {
                input,
                code: nom::error::ErrorKind::Tag,
            })
        })?;

    let rest = input.strip_prefix(lex_token.raw()).unwrap_or(input);

    Ok((rest, lex_token))
}

fn sortfield(input: &str) -> IResult<&str, SortField> {
    map_res(take_until(" "), std::str::FromStr::from_str)(input.trim())
}

fn sortorder(input: &str) -> IResult<&str, SortOrder> {
    map_res(not_line_ending, std::str::FromStr::from_str)(input)
}

fn listing_action(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    alt((
        set,
        delete_message,
        copymove,
        import,
        search,
        select,
        open_in_new_tab,
        export_mbox,
        _tag,
        flag,
    ))(input)
}

fn compose_action(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    alt((
        add_attachment,
        mailto,
        remove_attachment,
        save_draft,
        discard_draft,
    ))(input)
}

fn account_action(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    alt((reindex, print_account_setting))(input)
}

fn view(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    alt((
        filter,
        pipe,
        save_attachment,
        pipe_attachment,
        export_mail,
        export_thread,
        export_thread_mbox,
        add_addresses_to_contacts,
    ))(input)
}

fn new_tab(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    alt((manage_mailboxes, manage_jobs, compose_action, view_manpage))(input)
}

pub fn parse_command(input: &str) -> Result<Action, CommandError> {
    alt((
        goto,
        listing_action,
        sort,
        sort_column,
        subsort,
        close,
        mailinglist,
        setenv,
        alt((printenv, currentdir, change_currentdir)),
        view,
        create_mailbox,
        sub_mailbox,
        unsub_mailbox,
        delete_mailbox,
        rename_mailbox,
        new_tab,
        account_action,
        print_setting,
        toggle,
        reload_config,
        quit,
    ))(input)
    .map_err(|err| err.into())
    .and_then(|(_, v)| v)
}

pub(super) fn flag<'a>(input: &'a str) -> IResult<&'a str, Result<Action, CommandError>> {
    use melib::Flag;

    fn parse_flag(s: &str) -> Option<Flag> {
        match s {
            o if o.eq_ignore_ascii_case("passed") => Some(Flag::PASSED),
            o if o.eq_ignore_ascii_case("replied") => Some(Flag::REPLIED),
            o if o.eq_ignore_ascii_case("seen") => Some(Flag::SEEN),
            o if o.eq_ignore_ascii_case("read") => Some(Flag::SEEN),
            o if o.eq_ignore_ascii_case("junk") => Some(Flag::TRASHED),
            o if o.eq_ignore_ascii_case("trash") => Some(Flag::TRASHED),
            o if o.eq_ignore_ascii_case("trashed") => Some(Flag::TRASHED),
            o if o.eq_ignore_ascii_case("draft") => Some(Flag::DRAFT),
            o if o.eq_ignore_ascii_case("flagged") => Some(Flag::FLAGGED),
            _ => None,
        }
    }

    preceded(
        tag("flag"),
        alt((
            |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:2, max_arg: 2, flag};
                let (input, _) = tag("set")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let flag_input = input;
                let (input, flag) = quoted_argument(flag_input)?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                let Some(flag) = parse_flag(flag.value()) else {
                    return Ok((
                        flag_input,
                        Err(CommandError::BadValue {
                            inner: format!("{flag} is not a valid flag name").into(),
                            suggestions: Some(FLAG_SUGGESTIONS),
                        }),
                    ));
                };
                Ok((input, Ok(Listing(Flag(FlagAction::Set(flag))))))
            },
            |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:2, max_arg: 2, flag};
                let (input, _) = tag("unset")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let flag_input = input;
                let (input, flag) = quoted_argument(flag_input)?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                let Some(flag) = parse_flag(flag.value()) else {
                    return Ok((
                        flag_input,
                        Err(CommandError::BadValue {
                            inner: format!("{flag} is not a valid flag name").into(),
                            suggestions: Some(FLAG_SUGGESTIONS),
                        }),
                    ));
                };
                Ok((input, Ok(Listing(Flag(FlagAction::Unset(flag))))))
            },
        )),
    )(input.trim())
}

pub(super) fn set(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    fn toggle(input: &str) -> IResult<&str, Result<Action, CommandError>> {
        let mut check = arg_init! { min_arg:1, max_arg: 1, set};
        let (input, _) = tag("set")(input.trim())?;
        arg_chk!(start check, input);
        let (input, _) = is_a(" ")(input)?;
        arg_chk!(inc check, input);
        let (input, ret) = alt((
            map(tag("threaded"), |_| Ok(Listing(SetThreaded))),
            map(tag("plain"), |_| Ok(Listing(SetPlain))),
            map(tag("compact"), |_| Ok(Listing(SetCompact))),
            map(tag("conversations"), |_| Ok(Listing(SetConversations))),
        ))(input)?;
        arg_chk!(finish check, input);
        let (input, _) = eof(input)?;
        Ok((input, ret))
    }
    fn seen_flag(input: &'_ str) -> IResult<&'_ str, Result<Action, CommandError>> {
        let mut check = arg_init! { min_arg:1, max_arg: 1, set_seen_flag};
        let (input, _) = tag("set")(input.trim())?;
        arg_chk!(start check, input);
        let (input, _) = is_a(" ")(input)?;
        arg_chk!(inc check, input);
        let (input, ret) = command_err!(nom
                                   alt((
                                           map(tag!{}("seen"), |_| Listing(SetSeen)),
                                           map(tag!{}("unseen"), |_| Listing(SetUnseen)
                                   )))(input),
                                   input,
                                   input.trim().to_string(),
                                   Some(&["seen", "unseen", "plain", "threaded", "compact", "conversations"]));
        arg_chk!(finish check, input);
        let (input, _) = eof(input)?;
        Ok((input, Ok(ret)))
    }
    if let val @ Ok((_, Ok(_))) = toggle(input) {
        return val;
    }
    seen_flag(input)
}

pub(super) fn delete_message(input: &'_ str) -> IResult<&'_ str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, delete_message};
    let (input, ret) = map(preceded(tag("delete"), eof), |_| Listing(Delete))(input)?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(ret)))
}

pub(super) fn copymove<'a>(input: &'a str) -> IResult<&'a str, Result<Action, CommandError>> {
    alt((
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 2, copymove};
            let (input, _) = tag("copyto")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, account) = quoted_argument(input)?;
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, path) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(Listing(CopyToOtherAccount(
                    account.to_string(),
                    path.to_string(),
                ))),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, copymove};
            let (input, _) = tag("copyto")(input.trim())?;
            arg_chk!(start check, input);
            arg_chk!(inc check, input);
            let (input, _) = is_a(" ")(input)?;
            let (input, path) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((input, Ok(Listing(CopyTo(path.to_string())))))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 2, moveto};
            let (input, _) = tag("moveto")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, account) = quoted_argument(input)?;
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, path) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(Listing(MoveToOtherAccount(
                    account.to_string(),
                    path.to_string(),
                ))),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 2, moveto};
            let (input, _) = tag("moveto")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, path) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((input, Ok(Listing(MoveTo(path.to_string())))))
        },
    ))(input)
}

pub(super) fn close(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, close};
    let (input, _) = tag("close")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(Close))))
}

pub(super) fn goto(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, goto};
    let (input, _) = tag("go")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, nth) = command_err!(nom
                               usize_c(input),
                               input,
                               "Argument must be an integer.",
                               None);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Action::ViewMailbox(nth))))
}

pub(super) fn subsort(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 2, subsort};
    let (input, _) = tag("subsort")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, p) = pair(sortfield, sortorder)(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(SubSort(p.0, p.1))))
}

pub(super) fn sort(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 2, sort};
    let (input, _) = tag("sort")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, p) = separated_pair(sortfield, tag(" "), sortorder)(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Sort(p.0, p.1))))
}

pub(super) fn sort_column(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, sort_column};
    let (input, _) = tag("sort")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, i) = usize_c(input)?;
    let (input, order) = if input.trim().is_empty() {
        (input, SortOrder::Desc)
    } else {
        let (input, (_, order)) = pair(is_a(" "), sortorder)(input)?;
        (input, order)
    };
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(SortColumn(i, order))))
}

pub(super) fn search(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg:{ u8::MAX}, search};
    let (input, raw_search) = if let Some(input) = input.trim().strip_prefix("raw-") {
        (input, true)
    } else {
        (input, false)
    };
    let (input, _) = tag("search")(input)?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, string) = not_line_ending(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Listing(Search {
            term: String::from(string),
            raw_search,
        })),
    ))
}

pub(super) fn select(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    #[inline]
    fn clear_selection(input: &str) -> Option<IResult<&str, Result<Action, CommandError>>> {
        if !input.trim().starts_with("clear-selection") {
            return None;
        }
        #[inline]
        fn inner(input: &str) -> IResult<&str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:0, max_arg: 0, clear_selection};
            let (input, _) = tag("clear-selection")(input)?;
            arg_chk!(start check, input);
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((input, Ok(Listing(ListingAction::ClearSelection))))
        }
        Some(inner(input))
    }
    if let Some(retval) = clear_selection(input) {
        return retval;
    }

    let mut check = arg_init! { min_arg:1, max_arg: {u8::MAX}, select};
    let (input, raw_search) = if let Some(input) = input.trim().strip_prefix("raw-") {
        (input, true)
    } else {
        (input, false)
    };
    let (input, _) = tag("select")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, string) = not_line_ending(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Listing(Select {
            term: String::from(string),
            raw_search,
        })),
    ))
}

pub(super) fn export_mbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, export_mbox};
    let (input, _) = tag("export-mbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Listing(ExportMbox(
            Some(melib::mbox::MboxFormat::MboxCl2),
            path.to_string().into(),
        ))),
    ))
}

pub(super) fn export_thread_mbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, export_thread_mbox};
    let (input, _) = tag("export-thread-mbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(View(ExportThreadMbox(
            Some(melib::mbox::MboxFormat::MboxCl2),
            path.to_string().into(),
        ))),
    ))
}

pub(super) fn mailinglist(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, mailinglist};
    arg_chk!(start check, input);
    let (input, ret) = alt((
        map(tag("list-post"), |_| MailingListAction(ListPost)),
        map(tag("list-unsubscribe"), |_| {
            MailingListAction(ListUnsubscribe)
        }),
        map(tag("list-archive"), |_| MailingListAction(ListArchive)),
    ))(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(ret)))
}

pub(super) fn setenv(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, setenv};
    let (input, _) = tag("setenv")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, key) = take_until("=")(input)?;
    let (input, _) = tag("=")(input.trim())?;
    let (input, val) = not_line_ending(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(SetEnv(key.to_string(), val.to_string()))))
}

pub(super) fn printenv(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, printenv};
    let (input, _) = tag("printenv")(input)?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, key) = not_line_ending(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(PrintEnv(key.to_string()))))
}

pub(super) fn currentdir(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, pwd};
    let (input, _) = alt((tag("cwd"), tag("pwd")))(input)?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(CurrentDirectory)))
}

pub(super) fn change_currentdir(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg: 1, max_arg: 1, cd};
    let (input, _) = tag("cd")(input)?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, d) = not_line_ending(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(ChangeCurrentDirectory(d.into()))))
}

pub(super) fn mailto(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, mailto};
    use melib::email::parser::generic::mailto as parser;
    let (input, _) = tag("mailto")(input)?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, raw_val) = not_line_ending(input.trim())?;
    arg_chk!(finish check, input);
    let (_empty, _) = eof(input)?;
    let (input, val) = command_err!(
        parser(raw_val.as_bytes()),
        raw_val,
        "Could not parse mailto value. If the value is valid, please report this bug.",
        None
    );
    let input = std::str::from_utf8(input).map_err(|err| {
        nom::Err::Error(melib::nom::error::Error::from_external_error(
            raw_val,
            nom::error::ErrorKind::MapRes,
            err,
        ))
    })?;
    Ok((input, Ok(Compose(Mailto(val)))))
}

pub(super) fn pipe<'a>(input: &'a str) -> IResult<&'a str, Result<Action, CommandError>> {
    alt((
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: { u8::MAX }, pipe};
            let (input, _) = tag("pipe")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, bin) = quoted_argument(input)?;
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, args) = separated_list1(is_a(" "), quoted_argument)(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(View(Pipe(
                    bin.to_string(),
                    args.into_iter().map(String::from).collect::<Vec<String>>(),
                ))),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, pipe};
            let (input, _) = tag("pipe")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, bin) = quoted_argument(input.trim())?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((input, Ok(View(Pipe(bin.to_string(), Vec::new())))))
        },
    ))(input)
}

pub(super) fn filter(input: &'_ str) -> IResult<&'_ str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg:255, filter};
    let (input, _) = tag("filter")(input.trim())?;
    arg_chk!(start check, input);
    if let Ok((input, _)) = eof(input) {
        arg_chk!(finish check, input);
        return Ok((input, Ok(View(Filter(None)))));
    }
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, cmd) = not_line_ending(input)?;
    arg_chk!(finish check, input);
    Ok((input, Ok(View(Filter(Some(cmd.to_string()))))))
}

pub(super) fn add_attachment<'a>(input: &'a str) -> IResult<&'a str, Result<Action, CommandError>> {
    alt((
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, add_attachment};
            let (input, _) = tag("add-attachment")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            let (input, _) = tag("<")(input.trim())?;
            arg_chk!(inc check, input);
            let (input, _) = is_a(" ")(input)?;
            let (input, cmd) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(Tab(ComposerAction(ComposerTabAction::AddAttachmentPipe(
                    cmd.to_string(),
                )))),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, add_attachment};
            let (input, _) = tag("add-attachment")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, path) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(Tab(ComposerAction(ComposerTabAction::AddAttachment(
                    FileAction::Path(path.to_string()),
                )))),
            ))
        },
        alt((
            |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:1, max_arg: 1, add_attachment_file_picker};
                let (input, _) = tag("add-attachment-file-picker")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                let (input, _) = tag("<")(input.trim())?;
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let (input, shell) = not_line_ending(input)?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                Ok((
                    input,
                    Ok(Tab(ComposerAction(ComposerTabAction::AddAttachment(
                        FileAction::FilePicker(Some(shell.to_string())),
                    )))),
                ))
            },
            |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:0, max_arg: 0, add_attachment};
                let (input, _) = tag("add-attachment-file-picker")(input.trim())?;
                arg_chk!(start check, input);
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                Ok((
                    input,
                    Ok(Tab(ComposerAction(ComposerTabAction::AddAttachment(
                        FileAction::FilePicker(None),
                    )))),
                ))
            },
        )),
    ))(input)
}

pub(super) fn remove_attachment(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, remove_attachment};
    let (input, _) = tag("remove-attachment")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, idx) = usize_c(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Tab(ComposerAction(ComposerTabAction::RemoveAttachment(
            idx,
        )))),
    ))
}

pub(super) fn save_draft(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, save_draft };
    let (input, _) = tag("save-draft")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(ComposerAction(ComposerTabAction::SaveDraft)))))
}

pub(super) fn discard_draft(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, discard_draft };
    let (input, _) = tag("discard-draft")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Tab(ComposerAction(ComposerTabAction::DiscardDraft))),
    ))
}

pub(super) fn create_mailbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, create_malbox};
    let (input, _) = tag("create-mailbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Mailbox(
            account.to_string(),
            MailboxOperation::Create(path.to_string()),
        )),
    ))
}

pub(super) fn sub_mailbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg: 2, sub_mailbox};
    let (input, _) = tag("subscribe-mailbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Mailbox(
            account.to_string(),
            MailboxOperation::Subscribe(path.to_string()),
        )),
    ))
}

pub(super) fn unsub_mailbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg: 2, unsub_mailbox};
    let (input, _) = tag("unsubscribe-mailbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Mailbox(
            account.to_string(),
            MailboxOperation::Unsubscribe(path.to_string()),
        )),
    ))
}

pub(super) fn rename_mailbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:3, max_arg: 3, rename_mailbox};
    let (input, _) = tag("rename-mailbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, src) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, dest) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Mailbox(
            account.to_string(),
            MailboxOperation::Rename(src.to_string(), dest.to_string()),
        )),
    ))
}

pub(super) fn delete_mailbox(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg: 2, delete_mailbox};
    let (input, _) = tag("delete-mailbox")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Mailbox(
            account.to_string(),
            MailboxOperation::Delete(path.to_string()),
        )),
    ))
}

pub(super) fn reindex(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, reindex};
    let (input, _) = tag("reindex")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(AccountAction(account.to_string(), ReIndex))))
}

pub(super) fn open_in_new_tab(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, open_in_tab};
    let (input, _) = tag("open-in-tab")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Listing(OpenInNewTab))))
}

pub(super) fn save_attachment<'a>(
    input: &'a str,
) -> IResult<&'a str, Result<Action, CommandError>> {
    alt((
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:2, max_arg: 2, save_attachment};
            let (input, _) = tag("save-attachment")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, idx) = usize_c(input)?;
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, path) = quoted_argument(input.trim())?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(View(SaveAttachment(
                    idx,
                    FileAction::Path(path.to_string()),
                ))),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:2, max_arg: 2, save_attachment_picker};
            let (input, _) = tag("save-attachment-picker")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, idx) = usize_c(input)?;
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, shell) = not_line_ending(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(View(SaveAttachment(
                    idx,
                    FileAction::FilePicker(Some(shell.to_string())),
                ))),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, save_attachment_picker};
            let (input, _) = tag("save-attachment-picker")(input.trim())?;
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, idx) = usize_c(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((
                input,
                Ok(View(SaveAttachment(idx, FileAction::FilePicker(None)))),
            ))
        },
    ))(input)
}

pub(super) fn pipe_attachment<'a>(
    input: &'a str,
) -> IResult<&'a str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg:{u8::MAX}, pipe_attachment};
    let (input, _) = tag("pipe-attachment")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, idx) = usize_c(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, bin) = quoted_argument(input)?;
    arg_chk!(inc check, input);
    let (input, args) = alt((
        |input: &'a str| -> IResult<&'a str, Vec<String>> {
            let (input, _) = is_a(" ")(input)?;
            let (input, args) = separated_list1(is_a(" "), quoted_argument)(input)?;
            let (input, _) = eof(input)?;
            Ok((
                input,
                args.into_iter().map(String::from).collect::<Vec<String>>(),
            ))
        },
        |input: &'a str| -> IResult<&'a str, Vec<String>> {
            let (input, _) = eof(input)?;
            Ok((input, Vec::with_capacity(0)))
        },
    ))(input)?;
    arg_chk!(finish check, input);
    Ok((input, Ok(View(PipeAttachment(idx, bin.to_string(), args)))))
}

pub(super) fn export_mail(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, export_mail};
    let (input, _) = tag("export-mail")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(View(ExportMail(path.to_string())))))
}

pub(super) fn export_thread(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, export_thread};
    let (input, _) = tag("export-thread")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(View(ExportThread(path.to_string())))))
}

pub(super) fn add_addresses_to_contacts(
    input: &str,
) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, add_addresses_to_contacts};
    let (input, _) = tag("add-addresses-to-contacts")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(View(AddAddressesToContacts))))
}

pub(super) fn _tag<'a>(input: &'a str) -> IResult<&'a str, Result<Action, CommandError>> {
    preceded(
        tag("tag"),
        alt((
            |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:2, max_arg: 2, tag};
                let (input, _) = tag("add")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let (input, tag) = quoted_argument(input.trim())?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                Ok((input, Ok(Listing(Tag(TagAction::Add(tag.to_string()))))))
            },
            |input: &'a str| -> IResult<&'a str, Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:2, max_arg: 2, tag};
                let (input, _) = tag("remove")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let (input, tag) = quoted_argument(input.trim())?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                Ok((input, Ok(Listing(Tag(TagAction::Remove(tag.to_string()))))))
            },
        )),
    )(input.trim())
}

pub(super) fn print_account_setting(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg: 2, print};
    let (input, _) = tag("print")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, account) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, setting) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(AccountAction(
            account.to_string(),
            PrintAccountSetting(setting.to_string()),
        )),
    ))
}

pub(super) fn print_setting(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, print};
    let (input, _) = tag("print")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, setting) = quoted_argument(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(PrintSetting(setting.to_string()))))
}

pub(super) fn toggle(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, toggle };
    let (input, _) = tag("toggle")(input.trim())?;
    arg_chk!(start check, input);
    let (mut input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let mut retval = if tag!()("thread_snooze")(input).is_ok() {
        Some(Listing(ToggleThreadSnooze))
    } else {
        None
    };
    for (tok, action) in [
        ("thread_snooze", Listing(ToggleThreadSnooze)),
        ("mouse", ToggleMouse),
        ("sign", Tab(ComposerAction(ComposerTabAction::ToggleSign))),
        (
            "encrypt",
            Tab(ComposerAction(ComposerTabAction::ToggleEncrypt)),
        ),
    ] {
        if let Ok((inner_input, _)) = tag!()(tok)(input.trim()) {
            input = inner_input;
            retval = Some(action);
            break;
        }
    }
    let retval = match retval {
        None => {
            return Ok((
                input,
                Err(CommandError::BadValue {
                    inner: input.to_string().into(),
                    suggestions: Some(&["thread_snooze", "mouse", "sign", "encrypt"]),
                }),
            ));
        }
        Some(v) => v,
    };

    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(retval)))
}

pub(super) fn manage_mailboxes(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, manage_mailboxes};
    let (input, _) = tag("manage-mailboxes")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(ManageMailboxes))))
}

pub(super) fn manage_jobs(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, manage_jobs};
    let (input, _) = tag("manage-jobs")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(ManageJobs))))
}

pub(super) fn view_manpage(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, view_manpage };
    let (input, _) = tag("man")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    #[allow(unused_variables)]
    let (input, manpage) = not_line_ending(input.trim())?;
    let (input, _) = eof(input)?;
    arg_chk!(finish check, input);
    #[cfg(feature = "cli-docs")]
    {
        match crate::manpages::parse_manpage(manpage) {
            Ok(m) => Ok((input, Ok(Tab(Man(m))))),
            Err(err) => Ok((
                input,
                Err(CommandError::BadValue {
                    inner: err.to_string().into(),
                    suggestions: Some(crate::manpages::POSSIBLE_VALUES),
                }),
            )),
        }
    }
    #[cfg(not(feature = "cli-docs"))]
    {
        Ok((
            input,
            Err(CommandError::Other {
                inner: "this meli binary has not been compiled with the cli-docs feature".into(),
            }),
        ))
    }
}

pub(super) fn quit(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, quit};
    let (input, _) = tag("quit")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input.trim())?;
    Ok((input, Ok(Quit)))
}

pub(super) fn reload_config(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, reload_config};
    let (input, _) = tag("reload-config")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input.trim())?;
    Ok((input, Ok(ReloadConfiguration)))
}

pub(super) fn import(input: &str) -> IResult<&str, Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg: 2, import};
    let (input, _) = tag("import")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, file) = quoted_argument(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, mailbox_path) = quoted_argument(input)?;
    let (input, _) = eof(input)?;
    arg_chk!(finish check, input);
    Ok((
        input,
        Ok(Listing(Import(
            file.to_string().into(),
            mailbox_path.to_string(),
        ))),
    ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexToken<'a> {
    Literal {
        raw: &'a str,
        unescaped: Cow<'a, str>,
    },
    QuotedLiteral {
        raw: &'a str,
        unescaped: Cow<'a, str>,
    },
    Whitespace {
        raw: &'a str,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexTokenError<'a> {
    EscapeStart {
        raw: &'a str,
        unescaped: Cow<'a, str>,
    },
    QuoteStart {
        raw: &'a str,
        unescaped: Cow<'a, str>,
    },
    Invalid,
}

impl<'a> LexTokenError<'a> {
    /// Get the possibly escaped raw value
    pub const fn raw(&'a self) -> Option<&'a str> {
        match self {
            Self::QuoteStart { raw, .. } => Some(raw),
            _ => None,
        }
    }

    /// Get the unescaped value
    pub fn value(&'a self) -> Option<&'a str> {
        match self {
            Self::QuoteStart { ref unescaped, .. } => Some(unescaped.as_ref()),
            _ => None,
        }
    }
}

impl<'a> LexToken<'a> {
    /// Check whether this lexeme is pure whitespace
    pub const fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace { .. })
    }

    /// Get the possibly escaped raw value
    pub const fn raw(&'a self) -> &'a str {
        match self {
            Self::QuotedLiteral { raw, .. }
            | Self::Literal { raw, .. }
            | Self::Whitespace { raw } => raw,
        }
    }

    /// Get the unescaped value
    pub fn value(&'a self) -> &'a str {
        match self {
            Self::QuotedLiteral { ref unescaped, .. } | Self::Literal { ref unescaped, .. } => {
                unescaped.as_ref()
            }
            Self::Whitespace { raw } => raw,
        }
    }

    /// Take ownership of unescaped value
    #[allow(clippy::inherent_to_string_shadow_display)]
    pub fn to_string(self) -> String {
        match self {
            Self::QuotedLiteral { unescaped, .. } | Self::Literal { unescaped, .. } => {
                unescaped.into_owned()
            }
            Self::Whitespace { raw } => raw.to_string(),
        }
    }

    /// Generate a completion by appending a string without changing own value
    pub fn append(&self, m: &str) -> String {
        match self {
            Self::Literal { raw, .. } | Self::Whitespace { raw } => {
                let escaped = m.replace('"', "\\\"").replace(' ', "\\ ");
                format!("{raw}{escaped}")
            }
            Self::QuotedLiteral { raw, .. } => {
                let escaped = m.replace('"', "\\\"");
                let raw = raw.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
                format!("\"{raw}{escaped}\"")
            }
        }
    }

    /// Generate a completion
    pub fn complete(&self, m: &str) -> String {
        match self {
            Self::Literal { raw, .. } | Self::Whitespace { raw } => {
                let escaped = m.replace('"', "\\\"").replace(' ', "\\ ");
                let escaped = escaped.strip_prefix(raw).unwrap_or(&escaped);
                format!("{raw}{escaped}")
            }
            Self::QuotedLiteral { raw, .. } => {
                let escaped = m.replace('"', "\\\"");
                let raw = raw.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
                let escaped = escaped.strip_prefix(raw).unwrap_or(&escaped);
                format!("\"{raw}{escaped}\"")
            }
        }
    }

    /// Generate an incomplete completion (i.e. don't close with double quote)
    pub fn incomplete(&self, m: &str) -> String {
        match self {
            Self::Literal { raw, .. } | Self::Whitespace { raw } => {
                let escaped = m.replace('"', "\\\"").replace(' ', "\\ ");
                let escaped = escaped.strip_prefix(raw).unwrap_or(&escaped);
                format!("{raw}{escaped}")
            }
            Self::QuotedLiteral { raw, .. } => {
                let escaped = m.replace('"', "\\\"");
                let raw = raw.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
                let escaped = escaped.strip_prefix(raw).unwrap_or(&escaped);
                format!("\"{raw}{escaped}")
            }
        }
    }
}

impl<'a> std::fmt::Display for LexToken<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.value().fmt(fmt)
    }
}

impl<'a> From<LexToken<'a>> for String {
    fn from(lex_token: LexToken<'a>) -> Self {
        lex_token.to_string()
    }
}

pub(super) fn lex<'a>(
    input: &'a str,
) -> std::result::Result<(LexToken<'a>, &'a str), LexTokenError<'a>> {
    fn quoted_escaped_lex<'a>(
        input: &'a str,
    ) -> std::result::Result<(LexToken<'a>, &'a str), LexTokenError<'a>> {
        debug_assert!(input.starts_with('"'));
        let mut start = 1;
        let mut unescaped = String::new();
        let mut escaped = false;
        while let Some(char) = input[start..].chars().next() {
            if escaped {
                escaped = false;
                unescaped.push(char);
            } else {
                if char == '"' {
                    return Ok((
                        LexToken::QuotedLiteral {
                            raw: &input[0..=start],
                            unescaped: Cow::Owned(unescaped),
                        },
                        &input[start + 1..],
                    ));
                }
                if char == '\\' {
                    escaped = true;
                } else {
                    unescaped.push(char);
                }
            }
            start += char.len_utf8();
        }

        Err(LexTokenError::QuoteStart {
            raw: &input[0..start],
            unescaped: Cow::Owned(unescaped),
        })
    }

    fn quoted_lex<'a>(
        input: &'a str,
    ) -> std::result::Result<(LexToken<'a>, &'a str), LexTokenError<'a>> {
        debug_assert!(input.starts_with('"'));
        let mut start = 1;
        while let Some(char) = input[start..].chars().next() {
            if char == '"' {
                return Ok((
                    LexToken::QuotedLiteral {
                        raw: &input[0..=start],
                        unescaped: Cow::Borrowed(&input[1..start]),
                    },
                    &input[start + 1..],
                ));
            }
            if char == '\\' {
                return quoted_escaped_lex(input);
            }
            start += char.len_utf8();
        }

        Err(LexTokenError::QuoteStart {
            raw: &input[0..start],
            unescaped: Cow::Borrowed(&input[1..start]),
        })
    }

    fn escaped_lex<'a>(
        input: &'a str,
    ) -> std::result::Result<(LexToken<'a>, &'a str), LexTokenError<'a>> {
        let mut start = 0;
        let mut unescaped = String::new();
        let mut escaped = false;
        while let Some(char) = input[start..].chars().next() {
            if escaped {
                escaped = false;
                unescaped.push(char);
            } else {
                if char == '"' {
                    return Err(LexTokenError::Invalid);
                }
                if char == '\\' {
                    escaped = true;
                } else {
                    unescaped.push(char);
                }
                if char == ' ' {
                    return Ok((
                        LexToken::Literal {
                            raw: &input[..start],
                            unescaped: Cow::Owned(unescaped),
                        },
                        &input[start..],
                    ));
                }
            }
            start += char.len_utf8();
        }

        Ok((
            LexToken::Literal {
                raw: &input[..start],
                unescaped: Cow::Owned(unescaped),
            },
            &input[start..],
        ))
    }

    if input.starts_with('"') {
        return quoted_lex(input);
    }

    if input.starts_with(' ') {
        let mut start = 0;
        while input[start..].starts_with(' ') {
            start += 1;
        }
        return Ok((
            LexToken::Whitespace {
                raw: &input[..start],
            },
            &input[start..],
        ));
    }
    let mut start = 0;

    while let Some(char) = input[start..].chars().next() {
        if char == '\\' {
            return escaped_lex(input);
        }
        if char == '"' {
            return Err(LexTokenError::Invalid);
        }
        if char == ' ' {
            return Ok((
                LexToken::Literal {
                    raw: &input[..start],
                    unescaped: Cow::Borrowed(&input[..start]),
                },
                &input[start..],
            ));
        }
        start += char.len_utf8();
    }
    Ok((
        LexToken::Literal {
            raw: &input[..start],
            unescaped: Cow::Borrowed(&input[..start]),
        },
        &input[start..],
    ))
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = std::result::Result<LexToken<'a>, LexTokenError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            return None;
        }
        match lex(self.input) {
            Ok((next_token, rest)) => {
                self.input = rest;
                Some(Ok(next_token))
            }
            Err(err) => {
                self.input = "";
                Some(Err(err))
            }
        }
    }
}
