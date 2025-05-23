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

use super::*;
use crate::command::{argcheck::*, error::*};

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
        let evaluated: IResult<&'_ [u8], _> = { $b };
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
        tag::<&'_ str, &'_ [u8], melib::nom::error::Error<&[u8]>>
    }};
}

pub fn usize_c(input: &[u8]) -> IResult<&[u8], usize> {
    map_res(
        map_res(digit1, std::str::from_utf8),
        std::str::FromStr::from_str,
    )(input.trim())
}

pub fn sortfield(input: &[u8]) -> IResult<&[u8], SortField> {
    map_res(
        map_res(take_until(" "), std::str::from_utf8),
        std::str::FromStr::from_str,
    )(input.trim())
}

pub fn sortorder(input: &[u8]) -> IResult<&[u8], SortOrder> {
    map_res(
        map_res(not_line_ending, std::str::from_utf8),
        std::str::FromStr::from_str,
    )(input)
}

pub fn threaded(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    map(tag("threaded"), |_| Ok(Listing(SetThreaded)))(input.trim())
}

pub fn plain(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    map(tag("plain"), |_| Ok(Listing(SetPlain)))(input.trim())
}

pub fn compact(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    map(tag("compact"), |_| Ok(Listing(SetCompact)))(input.trim())
}

pub fn conversations(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    map(tag("conversations"), |_| Ok(Listing(SetConversations)))(input.trim())
}

pub fn listing_action(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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

pub fn compose_action(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    alt((
        add_attachment,
        mailto,
        remove_attachment,
        save_draft,
        discard_draft,
    ))(input)
}

pub fn account_action(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    alt((reindex, print_account_setting))(input)
}

pub fn view(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    alt((
        filter,
        pipe,
        save_attachment,
        pipe_attachment,
        export_mail,
        add_addresses_to_contacts,
    ))(input)
}

pub fn new_tab(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    alt((manage_mailboxes, manage_jobs, compose_action, view_manpage))(input)
}

pub fn parse_command(input: &[u8]) -> Result<Action, CommandError> {
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

/// Set/unset a flag.
///
/// # Example
///
/// ```
/// # use meli::{melib::Flag, command::{Action,ListingAction, FlagAction, parser}};
///
/// let (rest, parsed) = parser::flag(b"flag set junk").unwrap();
/// assert_eq!(rest, b"");
/// assert!(
///     matches!(
///         parsed,
///         Ok(Action::Listing(ListingAction::Flag(FlagAction::Set(
///             Flag::TRASHED
///         ))))
///     ),
///     "{:?}",
///     parsed
/// );
///
/// let (rest, parsed) = parser::flag(b"flag unset junk").unwrap();
/// assert_eq!(rest, b"");
/// assert!(
///     matches!(
///         parsed,
///         Ok(Action::Listing(ListingAction::Flag(FlagAction::Unset(
///             Flag::TRASHED
///         ))))
///     ),
///     "{:?}",
///     parsed
/// );
///
/// let (rest, parsed) = parser::flag(b"flag set draft").unwrap();
/// assert_eq!(rest, b"");
/// assert!(
///     matches!(
///         parsed,
///         Ok(Action::Listing(ListingAction::Flag(FlagAction::Set(
///             Flag::DRAFT
///         ))))
///     ),
///     "{:?}",
///     parsed
/// );
///
/// let (rest, parsed) = parser::flag(b"flag set xunk").unwrap();
/// assert_eq!(rest, b"");
/// assert_eq!(
///     &parsed.unwrap_err().to_string(),
///     "Bad value/argument: xunk is not a valid flag name. Possible values are: passed, replied, \
///      seen or read, junk or trash or trashed, draft, flagged"
/// );
/// ```
pub fn flag<'a>(input: &'a [u8]) -> IResult<&'a [u8], Result<Action, CommandError>> {
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
            |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:2, max_arg: 2, flag};
                let (input, _) = tag("set")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let (input, flag) = quoted_argument(input.trim())?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                let Some(flag) = parse_flag(flag) else {
                    return Ok((
                        b"",
                        Err(CommandError::BadValue {
                            inner: format!("{flag} is not a valid flag name").into(),
                            suggestions: Some(FLAG_SUGGESTIONS),
                        }),
                    ));
                };
                Ok((input, Ok(Listing(Flag(FlagAction::Set(flag))))))
            },
            |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:2, max_arg: 2, flag};
                let (input, _) = tag("unset")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let (input, flag) = quoted_argument(input.trim())?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                let Some(flag) = parse_flag(flag) else {
                    return Ok((
                        b"",
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

pub fn set(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    fn toggle(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
        let mut check = arg_init! { min_arg:1, max_arg: 1, set};
        let (input, _) = tag("set")(input.trim())?;
        arg_chk!(start check, input);
        let (input, _) = is_a(" ")(input)?;
        arg_chk!(inc check, input);
        let (input, ret) = alt((threaded, plain, compact, conversations))(input)?;
        arg_chk!(finish check, input);
        let (input, _) = eof(input)?;
        Ok((input, ret))
    }
    fn seen_flag(input: &'_ [u8]) -> IResult<&'_ [u8], Result<Action, CommandError>> {
        let mut check = arg_init! { min_arg:1, max_arg: 1, set_seen_flag};
        let (input, _) = tag("set")(input.trim())?;
        arg_chk!(start check, input);
        let (input, _) = is_a(" ")(input)?;
        arg_chk!(inc check, input);
        let (input, ret) = command_err!(nom
                                   alt((
                                           map(tag("seen"), |_| Listing(SetSeen)),
                                           map(tag("unseen"), |_| Listing(SetUnseen)
                                   )))(input),
                                   input,
                                   String::from_utf8_lossy(input.trim()).to_string(),
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

pub fn delete_message(input: &'_ [u8]) -> IResult<&'_ [u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, delete_message};
    let (input, ret) = map(preceded(tag("delete"), eof), |_| Listing(Delete))(input)?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(ret)))
}

pub fn copymove<'a>(input: &'a [u8]) -> IResult<&'a [u8], Result<Action, CommandError>> {
    alt((
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:2, max_arg: 2, copymove};
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
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, moveto};
            let (input, _) = tag("moveto")(input.trim())?;
            println!("input len is {}", input.len());
            arg_chk!(start check, input);
            let (input, _) = is_a(" ")(input)?;
            arg_chk!(inc check, input);
            let (input, path) = quoted_argument(input)?;
            arg_chk!(finish check, input);
            let (input, _) = eof(input)?;
            Ok((input, Ok(Listing(MoveTo(path.to_string())))))
        },
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:1, max_arg: 1, moveto};
            let (input, _) = tag("moveto")(input.trim())?;
            println!("input len is {}", input.len());
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
    ))(input)
}
pub fn close(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, close};
    let (input, _) = tag("close")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(Close))))
}
pub fn goto(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn subsort(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn sort(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn sort_column(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn search(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg:{ u8::MAX}, search};
    let (input, _) = tag("search")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, string) = map_res(not_line_ending, std::str::from_utf8)(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Listing(Search(String::from(string))))))
}
pub fn select(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    #[inline]
    fn clear_selection(input: &[u8]) -> Option<IResult<&[u8], Result<Action, CommandError>>> {
        if !input.trim().starts_with(b"clear-selection") {
            return None;
        }
        #[inline]
        fn inner(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
            let mut check = arg_init! { min_arg:0, max_arg: 0, clear_selection};
            let (input, _) = tag("clear-selection")(input.ltrim())?;
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
    let (input, _) = tag("select")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, string) = map_res(not_line_ending, std::str::from_utf8)(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Listing(Select(String::from(string))))))
}
pub fn export_mbox(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn mailinglist(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn setenv(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, setenv};
    let (input, _) = tag("setenv")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, key) = map_res(take_until("="), std::str::from_utf8)(input)?;
    let (input, _) = tag("=")(input.trim())?;
    let (input, val) = map_res(not_line_ending, std::str::from_utf8)(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(SetEnv(key.to_string(), val.to_string()))))
}
pub fn printenv(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, printenv};
    let (input, _) = tag("printenv")(input.ltrim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, key) = map_res(not_line_ending, std::str::from_utf8)(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(PrintEnv(key.to_string()))))
}
pub fn currentdir(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, pwd};
    let (input, _) = alt((tag("cwd"), tag("pwd")))(input.ltrim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(CurrentDirectory)))
}
pub fn change_currentdir(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg: 1, max_arg: 1, cd};
    let (input, _) = tag("cd")(input.ltrim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, d) = map_res(not_line_ending, std::str::from_utf8)(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(ChangeCurrentDirectory(d.into()))))
}
pub fn mailto(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, mailto};
    use melib::email::parser::generic::mailto as parser;
    let (input, _) = tag("mailto")(input.ltrim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, val) = map_res(not_line_ending, std::str::from_utf8)(input.trim())?;
    arg_chk!(finish check, input);
    let (_empty, _) = eof(input)?;
    let (input, val) = command_err!(
        parser(val.as_bytes()),
        val.as_bytes(),
        "Could not parse mailto value. If the value is valid, please report this bug.",
        None
    );
    Ok((input, Ok(Compose(Mailto(val)))))
}
pub fn pipe<'a>(input: &'a [u8]) -> IResult<&'a [u8], Result<Action, CommandError>> {
    alt((
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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
pub fn filter(input: &'_ [u8]) -> IResult<&'_ [u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg:255, filter};
    let (input, _) = tag("filter")(input.trim())?;
    arg_chk!(start check, input);
    if let Ok((input, _)) = eof(input) {
        arg_chk!(finish check, input);
        return Ok((input, Ok(View(Filter(None)))));
    }
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, cmd) = map_res(not_line_ending, std::str::from_utf8)(input)?;
    arg_chk!(finish check, input);
    Ok((input, Ok(View(Filter(Some(cmd.to_string()))))))
}
pub fn add_attachment<'a>(input: &'a [u8]) -> IResult<&'a [u8], Result<Action, CommandError>> {
    alt((
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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
        |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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
                    path.to_string(),
                )))),
            ))
        },
        alt((
            |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:1, max_arg: 1, add_attachment_file_picker};
                let (input, _) = tag("add-attachment-file-picker")(input.trim())?;
                arg_chk!(start check, input);
                let (input, _) = is_a(" ")(input)?;
                let (input, _) = tag("<")(input.trim())?;
                let (input, _) = is_a(" ")(input)?;
                arg_chk!(inc check, input);
                let (input, shell) = map_res(not_line_ending, std::str::from_utf8)(input)?;
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                Ok((
                    input,
                    Ok(Tab(ComposerAction(
                        ComposerTabAction::AddAttachmentFilePicker(Some(shell.to_string())),
                    ))),
                ))
            },
            |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
                let mut check = arg_init! { min_arg:0, max_arg: 0, add_attachment};
                let (input, _) = tag("add-attachment-file-picker")(input.trim())?;
                arg_chk!(start check, input);
                arg_chk!(finish check, input);
                let (input, _) = eof(input)?;
                Ok((
                    input,
                    Ok(Tab(ComposerAction(
                        ComposerTabAction::AddAttachmentFilePicker(None),
                    ))),
                ))
            },
        )),
    ))(input)
}
pub fn remove_attachment(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, remove_attachment};
    let (input, _) = tag("remove-attachment")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, idx) = map_res(quoted_argument, usize::from_str)(input)?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((
        input,
        Ok(Tab(ComposerAction(ComposerTabAction::RemoveAttachment(
            idx,
        )))),
    ))
}
pub fn save_draft(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, save_draft };
    let (input, _) = tag("save-draft")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(ComposerAction(ComposerTabAction::SaveDraft)))))
}
pub fn discard_draft(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn create_mailbox(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn sub_mailbox(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn unsub_mailbox(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn rename_mailbox(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn delete_mailbox(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn reindex(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn open_in_new_tab(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, open_in_tab};
    let (input, _) = tag("open-in-tab")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Listing(OpenInNewTab))))
}
pub fn save_attachment(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg: 2, save_attachment};
    let (input, _) = tag("save-attachment")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, idx) = map_res(quoted_argument, usize::from_str)(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, path) = quoted_argument(input.trim())?;
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(View(SaveAttachment(idx, path.to_string())))))
}
pub fn pipe_attachment<'a>(input: &'a [u8]) -> IResult<&'a [u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:2, max_arg:{u8::MAX}, pipe_attachment};
    let (input, _) = tag("pipe-attachment")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, idx) = map_res(quoted_argument, usize::from_str)(input)?;
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let (input, bin) = quoted_argument(input)?;
    arg_chk!(inc check, input);
    let (input, args) = alt((
        |input: &'a [u8]| -> IResult<&'a [u8], Vec<String>> {
            let (input, _) = is_a(" ")(input)?;
            let (input, args) = separated_list1(is_a(" "), quoted_argument)(input)?;
            let (input, _) = eof(input)?;
            Ok((
                input,
                args.into_iter().map(String::from).collect::<Vec<String>>(),
            ))
        },
        |input: &'a [u8]| -> IResult<&'a [u8], Vec<String>> {
            let (input, _) = eof(input)?;
            Ok((input, Vec::with_capacity(0)))
        },
    ))(input)?;
    arg_chk!(finish check, input);
    Ok((input, Ok(View(PipeAttachment(idx, bin.to_string(), args)))))
}
pub fn export_mail(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn add_addresses_to_contacts(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, add_addresses_to_contacts};
    let (input, _) = tag("add-addresses-to-contacts")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(View(AddAddressesToContacts))))
}

/// Set/unset a tag.
///
/// # Example
///
/// ```
/// # use meli::command::{Action,ListingAction, TagAction, parser::_tag};
///
/// let (rest, parsed) = _tag(b"tag add newsletters").unwrap();
/// println!("parsed is {:?}", parsed);
/// assert_eq!(rest, b"");
/// assert!(matches!(parsed, Ok(Action::Listing(ListingAction::Tag(TagAction::Add(ref tagname)))) if tagname == "newsletters"), "{:?}", parsed);
/// ```
pub fn _tag<'a>(input: &'a [u8]) -> IResult<&'a [u8], Result<Action, CommandError>> {
    preceded(
        tag("tag"),
        alt((
            |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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
            |input: &'a [u8]| -> IResult<&'a [u8], Result<Action, CommandError>> {
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

pub fn print_account_setting(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn print_setting(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
pub fn toggle(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, toggle };
    let (input, _) = tag("toggle")(input.trim())?;
    arg_chk!(start check, input);
    let (mut input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    let mut retval = if tag!()("thread_snooze")(input.ltrim()).is_ok() {
        Some(Listing(ToggleThreadSnooze))
    } else {
        None
    };
    for (tok, action) in [
        ("thread_snooze", Listing(ToggleThreadSnooze)),
        ("mouse", ToggleMouse),
        #[cfg(feature = "gpgme")]
        ("sign", Tab(ComposerAction(ComposerTabAction::ToggleSign))),
        #[cfg(feature = "gpgme")]
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
                    inner: String::from_utf8_lossy(input).to_string().into(),
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
pub fn manage_mailboxes(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, manage_mailboxes};
    let (input, _) = tag("manage-mailboxes")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(ManageMailboxes))))
}
pub fn manage_jobs(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, manage_jobs};
    let (input, _) = tag("manage-jobs")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input)?;
    Ok((input, Ok(Tab(ManageJobs))))
}

pub fn view_manpage(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:1, max_arg: 1, view_manpage };
    let (input, _) = tag("man")(input.trim())?;
    arg_chk!(start check, input);
    let (input, _) = is_a(" ")(input)?;
    arg_chk!(inc check, input);
    #[allow(unused_variables)]
    let (input, manpage) = map_res(not_line_ending, std::str::from_utf8)(input.trim())?;
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

pub fn quit(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, quit};
    let (input, _) = tag("quit")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input.trim())?;
    Ok((input, Ok(Quit)))
}
pub fn reload_config(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
    let mut check = arg_init! { min_arg:0, max_arg: 0, reload_config};
    let (input, _) = tag("reload-config")(input.trim())?;
    arg_chk!(start check, input);
    arg_chk!(finish check, input);
    let (input, _) = eof(input.trim())?;
    Ok((input, Ok(ReloadConfiguration)))
}
pub fn import(input: &[u8]) -> IResult<&[u8], Result<Action, CommandError>> {
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
