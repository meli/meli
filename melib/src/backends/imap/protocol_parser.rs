/*
 * meli - melib crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

use super::*;
use crate::email::parser::BytesExt;
use crate::get_path_hash;
use nom::{digit, is_digit, rest, IResult};
use std::str::FromStr;

#[derive(Debug)]
pub struct Alert(String);
pub type ImapParseResult<'a, T> = Result<(&'a str, T, Option<Alert>)>;
pub struct ImapLineIterator<'a> {
    slice: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum ResponseCode {
    ///The human-readable text contains a special alert that MUST be presented to the user in a fashion that calls the user's attention to the message.
    Alert(String),

    ///Optionally followed by a parenthesized list of charsets.  A SEARCH failed because the given charset is not supported by this implementation.  If the optional list of charsets is given, this lists the charsets that are supported by this implementation.
    Badcharset,

    /// Followed by a list of capabilities.  This can appear in the initial OK or PREAUTH response to transmit an initial capabilities list.  This makes it unnecessary for a client to send a separate CAPABILITY command if it recognizes this response.
    Capability,

    /// The human-readable text represents an error in parsing the [RFC-2822] header or [MIME-IMB] headers of a message in the mailbox.
    Parse(String),

    /// Followed by a parenthesized list of flags, indicates which of the known flags the client can change permanently.  Any flags that are in the FLAGS untagged response, but not the PERMANENTFLAGS list, can not be set permanently.  If the client attempts to STORE a flag that is not in the PERMANENTFLAGS list, the server will either ignore the change or store the state change for the remainder of the current session only.  The PERMANENTFLAGS list can also include the special flag \*, which indicates that it is possible to create new keywords by attempting to store those flags in the mailbox.
    Permanentflags,

    /// The mailbox is selected read-only, or its access while selected has changed from read-write to read-only.
    ReadOnly,

    /// The mailbox is selected read-write, or its access while selected has changed from read-only to read-write.
    ReadWrite,

    /// An APPEND or COPY attempt is failing because the target mailbox does not exist (as opposed to some other reason).  This is a hint to the client that the operation can succeed if the mailbox is first created by the CREATE command.
    Trycreate,

    /// Followed by a decimal number, indicates the next unique identifier value.  Refer to section 2.3.1.1 for more information.
    Uidnext(UID),
    /// Followed by a decimal number, indicates the unique identifier validity value.  Refer to section 2.3.1.1 for more information.
    Uidvalidity(UID),
    /// Followed by a decimal number, indicates the number of the first message without the \Seen flag set.
    Unseen(usize),

    None,
}

impl ResponseCode {
    fn from(val: &str) -> ResponseCode {
        use ResponseCode::*;
        if !val.starts_with("[") {
            return None;
        }

        let val = &val[1..];
        if val.starts_with("BADCHARSET") {
            Badcharset
        } else if val.starts_with("READONLY") {
            ReadOnly
        } else if val.starts_with("READWRITE") {
            ReadWrite
        } else if val.starts_with("TRYCREATE") {
            Trycreate
        } else if val.starts_with("UIDNEXT") {
            //FIXME
            Uidnext(0)
        } else if val.starts_with("UIDVALIDITY") {
            //FIXME
            Uidvalidity(0)
        } else if val.starts_with("UNSEEN") {
            //FIXME
            Unseen(0)
        } else {
            let msg = &val[val.as_bytes().find(b"] ").unwrap() + 1..].trim();
            Alert(msg.to_string())
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ImapResponse {
    Ok(ResponseCode),
    No(ResponseCode),
    Bad(ResponseCode),
    Preauth(ResponseCode),
    Bye(ResponseCode),
}

impl<T: AsRef<str>> From<T> for ImapResponse {
    fn from(val: T) -> ImapResponse {
        let val: &str = val.as_ref().split_rn().last().unwrap_or(val.as_ref());
        debug!(&val);
        let mut val = val[val.as_bytes().find(b" ").unwrap() + 1..].trim();
        // M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters (0.000 + 0.098 + 0.097 secs).\r\n
        if val.ends_with(" secs).") {
            val = &val[..val.as_bytes().rfind(b"(").unwrap()];
        }

        if val.starts_with("OK") {
            Self::Ok(ResponseCode::from(&val["OK ".len()..]))
        } else if val.starts_with("NO") {
            Self::No(ResponseCode::from(&val["NO ".len()..]))
        } else if val.starts_with("BAD") {
            Self::Bad(ResponseCode::from(&val["BAD ".len()..]))
        } else if val.starts_with("PREAUTH") {
            Self::Preauth(ResponseCode::from(&val["PREAUTH ".len()..]))
        } else if val.starts_with("BYE") {
            Self::Bye(ResponseCode::from(&val["BYE ".len()..]))
        } else {
            panic!("Unknown IMAP response: `{}`", val);
        }
    }
}

impl Into<Result<()>> for ImapResponse {
    fn into(self) -> Result<()> {
        match self {
            Self::Ok(_) | Self::Preauth(_) | Self::Bye(_) => Ok(()),
            Self::No(ResponseCode::Alert(msg)) | Self::Bad(ResponseCode::Alert(msg)) => {
                Err(MeliError::new(msg))
            }
            Self::No(_) => Err(MeliError::new("IMAP NO Response.")),
            Self::Bad(_) => Err(MeliError::new("IMAP BAD Response.")),
        }
    }
}

#[test]
fn test_imap_response() {
    assert_eq!(ImapResponse::from("M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters (0.000 + 0.098 + 0.097 secs).\r\n"), ImapResponse::No(ResponseCode::Alert("Invalid mailbox name: Name must not have '/' characters".to_string())));
}

impl<'a> Iterator for ImapLineIterator<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        if self.slice.is_empty() {
            None
        } else if let Some(pos) = self.slice.find("\r\n") {
            let ret = &self.slice[..pos + 2];
            self.slice = &self.slice[pos + 2..];
            Some(ret)
        } else {
            let ret = self.slice;
            self.slice = &self.slice[ret.len()..];
            Some(ret)
        }
    }
}

pub trait ImapLineSplit {
    fn split_rn(&self) -> ImapLineIterator;
}

impl ImapLineSplit for str {
    fn split_rn(&self) -> ImapLineIterator {
        ImapLineIterator { slice: self }
    }
}

macro_rules! to_str (
    ($v:expr) => (unsafe{ std::str::from_utf8_unchecked($v) })
);

macro_rules! dbg_dmp (
  ($i: expr, $submac:ident!( $($args:tt)* )) => (
    {
      let l = line!();
      match $submac!($i, $($args)*) {
        nom::IResult::Error(a) => {
          debug!("Error({:?}) at l.{} by ' {} '\n{}", a, l, stringify!($submac!($($args)*)), unsafe{ std::str::from_utf8_unchecked($i) });
          nom::IResult::Error(a)
        },
        nom::IResult::Incomplete(a) => {
          debug!("Incomplete({:?}) at {} by ' {} '\n{}", a, l, stringify!($submac!($($args)*)), unsafe{ std::str::from_utf8_unchecked($i) });
          nom::IResult::Incomplete(a)
        },
        a => a
      }
    }
  );

  ($i:expr, $f:ident) => (
      dbg_dmp!($i, call!($f));
  );
);

/*
* LIST (\HasNoChildren) "." INBOX.Sent
* LIST (\HasChildren) "." INBOX
 */
named!(
    pub list_mailbox_result<ImapMailbox>,
    do_parse!(
        ws!(alt_complete!(tag!("* LIST (") | tag!("* LSUB (")))
            >> properties: take_until!(&b")"[0..])
            >> tag!(b") ")
            >> separator: delimited!(tag!(b"\""), take!(1), tag!(b"\""))
            >> take!(1)
            >> path: alt_complete!(delimited!(tag!("\""), is_not!("\""), tag!("\"")) | call!(rest))
            >> ({
                let separator: u8 = separator[0];
                let mut f = ImapMailbox::default();
                f.no_select = false;
                f.is_subscribed = false;
                for p in properties.split(|&b| b == b' ') {
                    use crate::backends::SpecialUsageMailbox;
                    if p.eq_ignore_ascii_case(b"\\NoSelect") {
                        f.no_select = true;
                    } else if p.eq_ignore_ascii_case(b"\\Sent") {
                        let _ = f.set_special_usage(SpecialUsageMailbox::Sent);
                    } else if p.eq_ignore_ascii_case(b"\\Junk") {
                        let _ = f.set_special_usage(SpecialUsageMailbox::Trash);
                    } else if p.eq_ignore_ascii_case(b"\\Drafts") {
                        let _ = f.set_special_usage(SpecialUsageMailbox::Drafts);
                    }
                }
                f.imap_path = String::from_utf8_lossy(path).into();
                f.hash = get_path_hash!(&f.imap_path);
                f.path = if separator == b'/' {
                    f.imap_path.clone()
                } else {
                    f.imap_path.replace(separator as char, "/")
                };
                f.name = if let Some(pos) = f.imap_path.as_bytes().iter().rposition(|&c| c == separator) {
                    f.parent = Some(get_path_hash!(&f.imap_path[..pos]));
                    f.imap_path[pos + 1..].to_string()
                } else {
                    f.imap_path.clone()
                };
                f.separator = separator;

                debug!(f)
            })
    )
);

named!(
    my_flags<Flag>,
    do_parse!(
        flags: separated_list!(tag!(" "), preceded!(tag!("\\"), is_not!(")")))
            >> ({
                let mut ret = Flag::default();
                for f in flags {
                    match f {
                        b"Answered" => {
                            ret.set(Flag::REPLIED, true);
                        }
                        b"Flagged" => {
                            ret.set(Flag::FLAGGED, true);
                        }
                        b"Deleted" => {
                            ret.set(Flag::TRASHED, true);
                        }
                        b"Seen" => {
                            ret.set(Flag::SEEN, true);
                        }
                        b"Draft" => {
                            ret.set(Flag::DRAFT, true);
                        }
                        f => {
                            debug!("unknown Flag token value: {}", unsafe {
                                std::str::from_utf8_unchecked(f)
                            });
                        }
                    }
                }
                ret
            })
    )
);

#[derive(Debug)]
pub struct UidFetchResponse<'a> {
    pub uid: UID,
    pub flags: Option<(Flag, Vec<String>)>,
    pub body: Option<&'a [u8]>,
    pub envelope: Option<Envelope>,
}

pub fn uid_fetch_response(input: &str) -> ImapParseResult<UidFetchResponse<'_>> {
    macro_rules! should_start_with {
        ($input:expr, $tag:literal) => {
            if !$input.starts_with($tag) {
                return Err(MeliError::new(format!(
                    "Expected `{}` but got `{:.50}`",
                    $tag, &$input
                )));
            }
        };
    }
    should_start_with!(input, "* ");

    let mut i = "* ".len();
    macro_rules! bounds {
        () => {
            if i == input.len() {
                return Err(MeliError::new(format!(
                    "Expected more input. Got: `{:.50}`",
                    input
                )));
            }
        };
        (break) => {
            if i == input.len() {
                break;
            }
        };
    }

    macro_rules! eat_whitespace {
        () => {
            while (input.as_bytes()[i] as char).is_whitespace() {
                i += 1;
                bounds!();
            }
        };
        (break) => {
            while (input.as_bytes()[i] as char).is_whitespace() {
                i += 1;
                bounds!(break);
            }
        };
    }

    while (input.as_bytes()[i] as char).is_numeric() {
        i += 1;
        bounds!();
    }

    eat_whitespace!();
    should_start_with!(input[i..], "FETCH (");
    i += "FETCH (".len();

    let mut ret = UidFetchResponse {
        uid: 0,
        flags: None,
        body: None,
        envelope: None,
    };
    let mut has_attachments = false;
    while i < input.len() {
        eat_whitespace!(break);

        if input[i..].starts_with("UID ") {
            i += "UID ".len();
            if let IResult::Done(rest, uid) = take_while!(input[i..].as_bytes(), call!(is_digit)) {
                i += input.len() - i - rest.len();
                ret.uid = usize::from_str(unsafe { std::str::from_utf8_unchecked(uid) }).unwrap();
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    input
                ))));
            }
        } else if input[i..].starts_with("FLAGS (") {
            i += "FLAGS (".len();
            if let IResult::Done(rest, flags) = flags(&input[i..]) {
                ret.flags = Some(flags);
                i += (input.len() - i - rest.len()) + 1;
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    input
                ))));
            }
        } else if input[i..].starts_with("RFC822 {") {
            i += "RFC822 ".len();
            if let IResult::Done(rest, body) = length_bytes!(
                input[i..].as_bytes(),
                delimited!(
                    tag!("{"),
                    map_res!(digit, |s| {
                        usize::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                    }),
                    tag!("}\r\n")
                )
            ) {
                ret.body = Some(body);
                i += input.len() - i - rest.len();
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    input
                ))));
            }
        } else if input[i..].starts_with("ENVELOPE (") {
            i += "ENVELOPE ".len();
            if let IResult::Done(rest, envelope) = envelope(input[i..].as_bytes()) {
                ret.envelope = Some(envelope);
                i += input.len() - i - rest.len();
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    &input[i..]
                ))));
            }
        } else if input[i..].starts_with("BODYSTRUCTURE ") {
            i += "BODYSTRUCTURE ".len();
            let mut struct_ptr = i;
            let mut parenth_level = 0;
            let mut inside_quote = false;
            while struct_ptr != input.len() {
                if !inside_quote {
                    if input.as_bytes()[struct_ptr] == b'(' {
                        parenth_level += 1;
                    } else if input.as_bytes()[struct_ptr] == b')' {
                        if parenth_level == 0 {
                            return debug!(Err(MeliError::new(format!(
                                "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                                &input[struct_ptr..]
                            ))));
                        }
                        parenth_level -= 1;
                        if parenth_level == 0 {
                            struct_ptr += 1;
                            break;
                        }
                    } else if input.as_bytes()[struct_ptr] == b'"' {
                        inside_quote = true;
                    }
                } else if input.as_bytes()[struct_ptr] == b'\"'
                    && (struct_ptr == 0 || (input.as_bytes()[struct_ptr - 1] != b'\\'))
                {
                    inside_quote = false;
                }
                struct_ptr += 1;
            }

            has_attachments = bodystructure_has_attachments(&input.as_bytes()[i..struct_ptr]);
            i = struct_ptr;
        } else if input[i..].starts_with(")\r\n") {
            i += ")\r\n".len();
            break;
        } else {
            debug!(
                "Got unexpected token while parsing UID FETCH response:\n`{}`\n",
                input
            );
            return debug!(Err(MeliError::new(format!(
                "Got unexpected token while parsing UID FETCH response: `{:.40}`",
                &input[i..]
            ))));
        }
    }

    if let Some(env) = ret.envelope.as_mut() {
        env.set_has_attachments(has_attachments);
    }

    Ok((&input[i..], ret, None))
}

pub fn uid_fetch_responses(mut input: &str) -> ImapParseResult<Vec<UidFetchResponse<'_>>> {
    let mut ret = Vec::new();
    let mut alert: Option<Alert> = None;

    while let Ok((rest, el, el_alert)) = uid_fetch_response(input) {
        if let Some(el_alert) = el_alert {
            match &mut alert {
                Some(Alert(ref mut alert)) => {
                    alert.extend(el_alert.0.chars());
                }
                a @ None => *a = Some(el_alert),
            }
        }
        input = rest;
        ret.push(el);
    }

    if !input.is_empty() && ret.is_empty() {
        return Err(MeliError::new(format!(
            "310Unexpected input while parsing UID FETCH responses: `{:.40}`",
            input
        )));
    }
    Ok((input, ret, None))
}

/*
 *
 * "* 1 FETCH (FLAGS (\Seen) UID 1 RFC822.HEADER {5224}"
*/
named!(
    pub uid_fetch_response_<Vec<(usize, Option<(Flag, Vec<String>)>, &[u8])>>,
    many0!(
        do_parse!(
            tag!("* ")
            >> take_while!(call!(is_digit))
            >> tag!(" FETCH (")
            >> result: permutation!(preceded!(ws!(tag!("UID ")), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) })), opt!(preceded!(ws!(tag!("FLAGS ")), delimited!(tag!("("), byte_flags, tag!(")")))),
            ws!(length_bytes!(delimited!(tag!("{"), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) }), tag!("}\r\n")))))
            >> tag!(")\r\n")
            >> ((result.0, result.1, result.2))
        )
    )
);

named!(
    pub uid_fetch_flags_response<Vec<(usize, (Flag, Vec<String>))>>,
    many0!(
        do_parse!(
            tag!("* ")
            >> take_while!(call!(is_digit))
            >> tag!(" FETCH (")
            >> uid_flags: permutation!(preceded!(ws!(tag!("UID ")), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) })), preceded!(ws!(tag!("FLAGS ")), delimited!(tag!("("), byte_flags, tag!(")"))))
            >> tag!(")\r\n")
            >> ((uid_flags.0, uid_flags.1))
        )
    )
);

macro_rules! flags_to_imap_list {
    ($flags:ident) => {{
        let mut ret = String::new();
        if !($flags & Flag::REPLIED).is_empty() {
            ret.push_str("\\Answered");
        }
        if !($flags & Flag::FLAGGED).is_empty() {
            if !ret.is_empty() {
                ret.push(' ');
            }
            ret.push_str("\\Flagged");
        }
        if !($flags & Flag::TRASHED).is_empty() {
            if !ret.is_empty() {
                ret.push(' ');
            }
            ret.push_str("\\Deleted");
        }
        if !($flags & Flag::SEEN).is_empty() {
            if !ret.is_empty() {
                ret.push(' ');
            }
            ret.push_str("\\Seen");
        }
        if !($flags & Flag::DRAFT).is_empty() {
            if !ret.is_empty() {
                ret.push(' ');
            }
            ret.push_str("\\Draft");
        }
        ret
    }};
}

/* Input Example:
 * ==============
 *
 *  "M0 OK [CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE IDLE SORT SORT=DISPLAY THREAD=REFERENCES THREAD=REFS THREAD=ORDEREDSUBJECT MULTIAPPEND URL-PARTIAL CATENATE UNSELECT CHILDREN NAMESPACE UIDPLUS LIST-EXTENDED I18NLEVEL=1 CONDSTORE QRESYNC ESEARCH ESORT SEARCHRES WITHIN CONTEXT=SEARCH LIST-STATUS BINARY MOVE SPECIAL-USE] Logged in\r\n"
*   "* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN X-GM-EXT-1 XYZZY SASL-IR AUTH=XOAUTH2 AUTH=PLAIN AUTH=PLAIN-CLIENT TOKEN AUTH=OAUTHBEARER AUTH=XOAUTH\r\n"
*   "* CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE IDLE AUTH=PLAIN\r\n"
 */

named!(
    pub capabilities<Vec<&[u8]>>,
    do_parse!(
        take_until!("CAPABILITY ")
        >> tag!("CAPABILITY ")
        >> ret: separated_nonempty_list_complete!(tag!(" "), is_not!(" ]\r\n"))
        >> take_until!("\r\n")
        >> tag!("\r\n")
        >> ({ ret })
    )
);

/// This enum represents the server's untagged responses detailed in `7. Server Responses` of RFC 3501   INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1
pub enum UntaggedResponse {
    /// ```text
    ///    7.4.1.  EXPUNGE Response
    ///
    ///    The EXPUNGE response reports that the specified message sequence
    ///    number has been permanently removed from the mailbox.  The message
    ///    sequence number for each successive message in the mailbox is
    ///    immediately decremented by 1, and this decrement is reflected in
    ///    message sequence numbers in subsequent responses (including other
    ///    untagged EXPUNGE responses).
    ///
    ///    The EXPUNGE response also decrements the number of messages in the
    ///    mailbox; it is not necessary to send an EXISTS response with the
    ///    new value.
    ///
    ///    As a result of the immediate decrement rule, message sequence
    ///    numbers that appear in a set of successive EXPUNGE responses
    ///    depend upon whether the messages are removed starting from lower
    ///    numbers to higher numbers, or from higher numbers to lower
    ///    numbers.  For example, if the last 5 messages in a 9-message
    ///    mailbox are expunged, a "lower to higher" server will send five
    ///    untagged EXPUNGE responses for message sequence number 5, whereas
    ///    a "higher to lower server" will send successive untagged EXPUNGE
    ///    responses for message sequence numbers 9, 8, 7, 6, and 5.
    ///
    ///    An EXPUNGE response MUST NOT be sent when no command is in
    ///    progress, nor while responding to a FETCH, STORE, or SEARCH
    ///    command.  This rule is necessary to prevent a loss of
    ///    synchronization of message sequence numbers between client and
    ///    server.  A command is not "in progress" until the complete command
    ///    has been received; in particular, a command is not "in progress"
    ///    during the negotiation of command continuation.
    ///
    ///         Note: UID FETCH, UID STORE, and UID SEARCH are different
    ///         commands from FETCH, STORE, and SEARCH.  An EXPUNGE
    ///         response MAY be sent during a UID command.
    ///
    ///    The update from the EXPUNGE response MUST be recorded by the
    ///    client.
    /// ```
    Expunge(usize),
    /// ```text
    ///     7.3.1.  EXISTS Response
    ///
    ///     The EXISTS response reports the number of messages in the mailbox.
    ///     This response occurs as a result of a SELECT or EXAMINE command,
    ///     and if the size of the mailbox changes (e.g., new messages).
    ///
    ///     The update from the EXISTS response MUST be recorded by the
    ///     client.
    /// ```
    Exists(usize),
    /// ```text
    /// 7.3.2. RECENT Response
    /// The RECENT response reports the number of messages with the
    /// \Recent flag set.  This response occurs as a result of a SELECT or
    /// EXAMINE command, and if the size of the mailbox changes (e.g., new
    /// messages).
    /// ```
    Recent(usize),
}

named!(
    pub untagged_responses<Option<UntaggedResponse>>,
    do_parse!(
        tag!("* ")
        >> num: map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) })
        >> tag!(" ")
        >> tag: take_until!("\r\n")
        >> tag!("\r\n")
        >> ({
            use UntaggedResponse::*;
            match tag {
                b"EXPUNGE" => Some(Expunge(num)),
                b"EXISTS" => Some(Exists(num)),
                b"RECENT" => Some(Recent(num)),
                _ => {
                    debug!("unknown untagged_response: {}", unsafe { std::str::from_utf8_unchecked(tag) });
                    None
                }
            }
        })
    )
);

named!(
    pub search_results<Vec<usize>>,
    alt_complete!(do_parse!( tag!("* SEARCH")
                            >> list: separated_nonempty_list_complete!(tag!(" "), map_res!(is_not!("\r\n"), |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) }))
                            >> tag!("\r\n")
                            >> ({ list })) |
    do_parse!(tag!("* SEARCH\r\n") >> ({ Vec::new() })))
);

named!(
    pub search_results_raw<&[u8]>,
    alt_complete!(do_parse!( tag!("* SEARCH ")
                            >> list: take_until!("\r\n")
                            >> tag!("\r\n")
                            >> ({ list })) |
    do_parse!(tag!("* SEARCH\r\n") >> ({ &b""[0..] })))
);

#[derive(Debug, Default, Clone)]
pub struct SelectResponse {
    pub exists: usize,
    pub recent: usize,
    pub flags: (Flag, Vec<String>),
    pub unseen: usize,
    pub uidvalidity: usize,
    pub uidnext: usize,
    pub permanentflags: (Flag, Vec<String>),
    /// if SELECT returns \* we can set arbritary flags permanently.
    pub can_create_flags: bool,
    pub read_only: bool,
}

/*
 *  Example: C: A142 SELECT INBOX
 *           S: * 172 EXISTS
 *           S: * 1 RECENT
 *           S: * OK [UNSEEN 12] Message 12 is first unseen
 *           S: * OK [UIDVALIDITY 3857529045] UIDs valid
 *           S: * OK [UIDNEXT 4392] Predicted next UID
 *           S: * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
 *           S: * OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited
 *           S: A142 OK [READ-WRITE] SELECT completed
 */

/*
 *
 *  * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
 *  * OK [PERMANENTFLAGS (\Answered \Flagged \Deleted \Seen \Draft \*)] Flags permitted.
 *  * 45 EXISTS
 *  * 0 RECENT
 *  * OK [UNSEEN 16] First unseen.
 *  * OK [UIDVALIDITY 1554422056] UIDs valid
 *  * OK [UIDNEXT 50] Predicted next UID
 */
pub fn select_response(input: &str) -> Result<SelectResponse> {
    if input.contains("* OK") {
        let mut ret = SelectResponse::default();
        for l in input.split("\r\n") {
            if l.starts_with("* ") && l.ends_with(" EXISTS") {
                ret.exists = usize::from_str(&l["* ".len()..l.len() - " EXISTS".len()])?;
            } else if l.starts_with("* ") && l.ends_with(" RECENT") {
                ret.recent = usize::from_str(&l["* ".len()..l.len() - " RECENT".len()])?;
            } else if l.starts_with("* FLAGS (") {
                ret.flags = flags(&l["* FLAGS (".len()..l.len() - ")".len()]).to_full_result()?;
            } else if l.starts_with("* OK [UNSEEN ") {
                ret.unseen = usize::from_str(&l["* OK [UNSEEN ".len()..l.find(']').unwrap()])?;
            } else if l.starts_with("* OK [UIDVALIDITY ") {
                ret.uidvalidity =
                    usize::from_str(&l["* OK [UIDVALIDITY ".len()..l.find(']').unwrap()])?;
            } else if l.starts_with("* OK [UIDNEXT ") {
                ret.uidnext = usize::from_str(&l["* OK [UIDNEXT ".len()..l.find(']').unwrap()])?;
            } else if l.starts_with("* OK [PERMANENTFLAGS (") {
                ret.permanentflags =
                    flags(&l["* OK [PERMANENTFLAGS (".len()..l.find(')').unwrap()])
                        .to_full_result()?;
                ret.can_create_flags = l.contains("\\*");
            } else if l.contains("OK [READ-WRITE]") {
                ret.read_only = false;
            } else if l.contains("OK [READ-ONLY]") {
                ret.read_only = true;
            } else if !l.is_empty() {
                debug!("select response: {}", l);
            }
        }
        Ok(ret)
    } else {
        debug!("BAD/NO response in select: {}", input);
        Err(MeliError::new(input.to_string()))
    }
}

pub fn flags(input: &str) -> IResult<&str, (Flag, Vec<String>)> {
    let mut ret = Flag::default();
    let mut keywords = Vec::new();

    let mut input = input;
    while !input.starts_with(")") && !input.is_empty() {
        if input.starts_with("\\") {
            input = &input[1..];
        }
        let mut match_end = 0;
        while match_end < input.len() {
            if input[match_end..].starts_with(" ") || input[match_end..].starts_with(")") {
                break;
            }
            match_end += 1;
        }

        match &input[..match_end] {
            "Answered" => {
                ret.set(Flag::REPLIED, true);
            }
            "Flagged" => {
                ret.set(Flag::FLAGGED, true);
            }
            "Deleted" => {
                ret.set(Flag::TRASHED, true);
            }
            "Seen" => {
                ret.set(Flag::SEEN, true);
            }
            "Draft" => {
                ret.set(Flag::DRAFT, true);
            }
            f => {
                keywords.push(f.to_string());
            }
        }
        input = &input[match_end..];
        input = input.trim_start();
    }
    IResult::Done(input, (ret, keywords))
}

pub fn byte_flags(input: &[u8]) -> IResult<&[u8], (Flag, Vec<String>)> {
    let i = unsafe { std::str::from_utf8_unchecked(input) };
    match flags(i) {
        IResult::Done(rest, ret) => IResult::Done(rest.as_bytes(), ret),
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(e) => IResult::Incomplete(e),
    }
}

/*
* The fields of the envelope structure are in the following
* order: date, subject, from, sender, reply-to, to, cc, bcc,
* in-reply-to, and message-id. The date, subject, in-reply-to,
* and message-id fields are strings. The from, sender, reply-to,
* to, cc, and bcc fields are parenthesized lists of address
* structures.
* An address structure is a parenthesized list that describes an
* electronic mail address. The fields of an address structure
* are in the following order: personal name, [SMTP]
* at-domain-list (source route), mailbox name, and host name.
*/

/*
*  * 12 FETCH (FLAGS (\Seen) INTERNALDATE "17-Jul-1996 02:44:25 -0700"
*  RFC822.SIZE 4286 ENVELOPE ("Wed, 17 Jul 1996 02:23:25 -0700 (PDT)"
*  "IMAP4rev1 WG mtg summary and minutes"
*  (("Terry Gray" NIL "gray" "cac.washington.edu"))
*  (("Terry Gray" NIL "gray" "cac.washington.edu"))
*  (("Terry Gray" NIL "gray" "cac.washington.edu"))
*  ((NIL NIL "imap" "cac.washington.edu"))
*  ((NIL NIL "minutes" "CNRI.Reston.VA.US")
*  ("John Klensin" NIL "KLENSIN" "MIT.EDU")) NIL NIL
*  "<B27397-0100000@cac.washington.edu>")
*/

named!(
    pub envelope<Envelope>,
    do_parse!(
        tag!("(")
        >>  opt!(is_a!("\r\n\t "))
        >>  date: quoted_or_nil
        >>  opt!(is_a!("\r\n\t "))
        >>  subject: quoted_or_nil
        >>  opt!(is_a!("\r\n\t "))
        >>  from: envelope_addresses
        >>  opt!(is_a!("\r\n\t "))
        >>  sender: envelope_addresses
        >>  opt!(is_a!("\r\n\t "))
        >>  reply_to: envelope_addresses
        >>  opt!(is_a!("\r\n\t "))
        >>  to: envelope_addresses
        >>  opt!(is_a!("\r\n\t "))
        >>  cc: envelope_addresses
        >>  opt!(is_a!("\r\n\t "))
        >>  bcc: envelope_addresses
        >>  opt!(is_a!("\r\n\t "))
        >>  in_reply_to: quoted_or_nil
        >>  opt!(is_a!("\r\n\t "))
        >>  message_id: quoted_or_nil
        >>  opt!(is_a!("\r\n\t "))
        >> tag!(")")
        >> ({
            let mut env = Envelope::new(0);
            if let Some(date) = date {
                env.set_date(&date);
                if let Ok(d) = crate::email::parser::date(env.date_as_str().as_bytes()) {
                    env.set_datetime(d);
                }
            }

            if let Some(subject) = subject {
                env.set_subject(subject.to_vec());
            }

            if let Some(from) = from {
                env.set_from(from);
            }
            if let Some(to) = to {
                env.set_to(to);
            }

            if let Some(cc) = cc {
                env.set_cc(cc);
            }

            if let Some(bcc) = bcc {
                env.set_bcc(bcc);
            }
            if let Some(in_reply_to) = in_reply_to {
                env.set_in_reply_to(&in_reply_to);
                env.push_references(&in_reply_to);
            }

            if let Some(message_id) = message_id {
                env.set_message_id(&message_id);
            }
            env
        })
));

/* Helper to build StrBuilder for Address structs */
macro_rules! str_builder {
    ($offset:expr, $length:expr) => {
        StrBuilder {
            offset: $offset,
            length: $length,
        }
    };
}

// Parse a list of addresses in the format of the ENVELOPE structure
named!(pub envelope_addresses<Option<Vec<Address>>>,
       alt_complete!(map!(tag!("NIL"), |_| None) | 
                     do_parse!(
                         tag!("(")
                         >> envelopes: many1!(delimited!(ws!(tag!("(")), envelope_address, tag!(")")))
                         >> tag!(")")
                         >> ({
                             Some(envelopes)
                         })
                     )
));

// Parse an address in the format of the ENVELOPE structure eg
// ("Terry Gray" NIL "gray" "cac.washington.edu")
named!(
    pub envelope_address<Address>,
    do_parse!(
        name: alt_complete!(quoted | map!(tag!("NIL"), |_| Vec::new()))
        >>  is_a!("\r\n\t ")
        >>  alt_complete!(quoted| map!(tag!("NIL"), |_| Vec::new()))
        >>  is_a!("\r\n\t ")
        >>  mailbox_name: dbg_dmp!(alt_complete!(quoted | map!(tag!("NIL"), |_| Vec::new())))
        >>  is_a!("\r\n\t ")
        >>  host_name: alt_complete!(quoted | map!(tag!("NIL"), |_| Vec::new()))
        >>  ({
        Address::Mailbox(MailboxAddress {
            raw: format!("{}{}<{}@{}>", to_str!(&name), if name.is_empty() { "" } else { " " }, to_str!(&mailbox_name), to_str!(&host_name)).into_bytes(), 
            display_name: str_builder!(0, name.len()),
            address_spec: str_builder!(if name.is_empty() { 1 } else { name.len() + 2 }, mailbox_name.len() + host_name.len() + 1),
        })
    })
));

// Read a literal ie a byte sequence prefixed with a tag containing its length delimited in {}s
named!(pub literal<&[u8]>,length_bytes!(delimited!(tag!("{"), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) }), tag!("}\r\n"))));

// Return a byte sequence surrounded by "s and decoded if necessary
pub fn quoted(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if let IResult::Done(r, o) = literal(input) {
        return match crate::email::parser::phrase(o) {
            IResult::Done(_, out) => IResult::Done(r, out),
            e => e,
        };
    }
    if input.is_empty() || input[0] != b'"' {
        return IResult::Error(nom::ErrorKind::Custom(0));
    }

    let mut i = 1;
    while i < input.len() {
        if input[i] == b'\"' && (i == 0 || (input[i - 1] != b'\\')) {
            return match crate::email::parser::phrase(&input[1..i]) {
                IResult::Done(_, out) => IResult::Done(&input[i + 1..], out),
                e => e,
            };
        }
        i += 1;
    }

    return IResult::Error(nom::ErrorKind::Custom(0));
}

named!(
    pub quoted_or_nil<Option<Vec<u8>>>,
    alt_complete!(map!(ws!(tag!("NIL")), |_| None) | map!(quoted, |v| Some(v))));

named!(
    pub uid_fetch_envelopes_response<Vec<(usize, Option<(Flag, Vec<String>)>, Envelope)>>,
    many0!(
        do_parse!(
            tag!("* ")
            >> take_while!(call!(is_digit))
            >> tag!(" FETCH (")
            >> uid_flags: permutation!(preceded!(ws!(tag!("UID ")), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) })), opt!(preceded!(ws!(tag!("FLAGS ")), delimited!(tag!("("), byte_flags, tag!(")")))))
            >> tag!(" ENVELOPE ")
            >> env: ws!(envelope)
            >> tag!("BODYSTRUCTURE ")
            >> bodystructure: take_until!(")\r\n")
            >> tag!(")\r\n")
            >> ({
                let mut env = env;
                let has_attachments = bodystructure_has_attachments(bodystructure);
                env.set_has_attachments(has_attachments);
                (uid_flags.0, uid_flags.1, env)
            })
        )
    )
);

pub fn bodystructure_has_attachments(input: &[u8]) -> bool {
    input.rfind(b" \"mixed\" ").is_some() || input.rfind(b" \"MIXED\" ").is_some()
}
