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

#![allow(clippy::type_complexity)]

use std::{convert::TryFrom, str::FromStr};

pub mod id_ext;
#[cfg(test)]
mod tests;
pub mod utils;

use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_a, is_not, tag, take, take_until, take_while},
    character::{complete::digit1, is_digit},
    combinator::{map, map_res, opt},
    multi::{fold_many1, length_data, many0, many1, separated_list1},
    sequence::{delimited, preceded},
};

use super::*;
use crate::{
    email::{
        address::{Address, MailboxAddress},
        parser::{
            generic::{byte_in_range, byte_in_slice},
            BytesExt, IResult,
        },
    },
    error::ResultIntoError,
    text::Truncate,
    utils::parsec::CRLF,
};

const UNTAGGED_PREFIX: &[u8] = b"* ";

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct RequiredResponses: u64 {
        /// Require a **tagged** `NO` response.
        const NO                  = 0;
        /// Require an *untagged* `CAPABILITY` response.
        const CAPABILITY          = 0b0000_0000_0000_0000_0001;
        /// Require an *untagged* `BYE` response.
        const BYE                 = 0b0000_0000_0000_0000_0010;
        /// Require an *untagged* `FLAGS` response as part of a `SELECT`/`EXAMINE` response.
        const FLAGS               = 0b0000_0000_0000_0000_0100;
        /// Require an *untagged* `EXISTS` response as part of a `SELECT`/`EXAMINE` response or when
        /// the size of the mailbox changes.
        const EXISTS              = 0b0000_0000_0000_0000_1000;
        /// Require an *untagged* `RECENT` response as part of a `SELECT`/`EXAMINE` response.
        const RECENT              = 0b0000_0000_0000_0001_0000;
        /// Require an *untagged* `UNSEEN` response as part of a `SELECT`/`EXAMINE` response.
        const UNSEEN              = 0b0000_0000_0000_0010_0000;
        /// Require an *untagged* `PERMANENTFLAGS` response as part of a `SELECT`/`EXAMINE`
        /// response.
        const PERMANENTFLAGS      = 0b0000_0000_0000_0100_0000;
        /// Require an *untagged* `UIDNEXT` response as part of a `SELECT`/`EXAMINE` response.
        const UIDNEXT             = 0b0000_0000_0000_1000_0000;
        /// Require an *untagged* `UIDVALIDITY` response as part of a `SELECT`/`EXAMINE` response.
        const UIDVALIDITY         = 0b0000_0000_0001_0000_0000;
        /// Require an *untagged* `LIST` response as part of a `LIST` response.
        const LIST                = 0b0000_0000_0010_0000_0000;
        /// Require an *untagged* `LSUB` response as part of a `LSUB` response.
        const LSUB                = 0b0000_0000_0100_0000_0000;
        /// Require an *untagged* `STATUS` response as part of a `STATUS` response.
        const STATUS              = 0b0000_0000_1000_0000_0000;
        /// Require an *untagged* `SEARCH` response as part of a `SEARCH` response.
        const SEARCH              = 0b0000_0001_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `UID` item included.
        const FETCH_UID           = 0b0000_0010_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `MODSEQ` item included.
        const FETCH_MODSEQ        = 0b0000_0100_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `FLAGS` item included.
        const FETCH_FLAGS         = 0b0000_1000_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `BODY`/`RFC822` item included.
        const FETCH_BODY          = 0b0001_0000_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `BODY[HEADER.FIELDS (REFERENCES)]` item included.
        const FETCH_REFERENCES    = 0b0010_0000_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `BODYSTRUCTURE` item included.
        const FETCH_BODYSTRUCTURE = 0b0100_0000_0000_0000_0000;
        /// Require an *untagged* `FETCH` response with a `ENVELOPE` item included.
        const FETCH_ENVELOPE      = 0b1000_0000_0000_0000_0000;
        /// Require any `SELECT` related reponse.
        const SELECT              = Self::FLAGS.bits() | Self::EXISTS.bits() | Self::RECENT.bits() | Self::UNSEEN.bits() | Self::PERMANENTFLAGS.bits() | Self::UIDNEXT.bits() | Self::UIDVALIDITY.bits();
        /// Require any `EXAMINE` related reponse.
        const EXAMINE             = Self::FLAGS.bits() | Self::EXISTS.bits() | Self::RECENT.bits() | Self::UNSEEN.bits() | Self::PERMANENTFLAGS.bits() | Self::UIDNEXT.bits() | Self::UIDVALIDITY.bits();
    }
}

impl RequiredResponses {
    pub fn check(&self, line: &[u8]) -> bool {
        if *self == Self::NO {
            return matches!(line.splitn(2, |b| *b == b' ').nth(1), Some(l) if l.starts_with(b"NO "));
        }
        let Some(stripped) = line.strip_prefix(UNTAGGED_PREFIX) else {
            return false;
        };
        macro_rules! check {
            ($($flag:expr => $cond:expr),*$(,)?) => {{
                $(
                    if self.intersects($flag) && $cond {
                        return true;
                    }
                )*
            }};
        }
        check! {
            Self::CAPABILITY => stripped.starts_with(b"CAPABILITY"),
            Self::BYE => stripped.starts_with(b"BYE"),
            Self::FLAGS => stripped.starts_with(b"FLAGS ("),
            Self::EXISTS => stripped.ends_with(b"EXISTS\r\n"),
            Self::RECENT => stripped.ends_with(b"RECENT\r\n"),
            Self::UNSEEN => stripped.starts_with(b"OK [UNSEEN "),
            Self::PERMANENTFLAGS =>  stripped.starts_with(b"OK [PERMANENTFLAGS "),
            Self::UIDNEXT => stripped.starts_with(b"OK [UIDNEXT "),
            Self::UIDVALIDITY => stripped.starts_with(b"OK [UIDVALIDITY "),
            Self::LIST => stripped.starts_with(b"LIST"),
            Self::LSUB => stripped.starts_with(b"LSUB"),
            Self::STATUS => stripped.starts_with(b"STATUS"),
            Self::SEARCH => stripped.starts_with(b"SEARCH"),
        };
        if !self.intersects(
            Self::FETCH_UID
                | Self::FETCH_MODSEQ
                | Self::FETCH_FLAGS
                | Self::FETCH_BODY
                | Self::FETCH_REFERENCES
                | Self::FETCH_BODYSTRUCTURE
                | Self::FETCH_ENVELOPE,
        ) {
            return false;
        }
        let Ok((_, response, _)) = fetch_response(line) else {
            return false;
        };
        if self.intersects(Self::FETCH_UID) && response.uid.is_none() {
            return false;
        }
        if self.intersects(Self::FETCH_MODSEQ) && response.modseq.is_none() {
            return false;
        }
        if self.intersects(Self::FETCH_FLAGS) && response.flags.is_none() {
            return false;
        }
        if self.intersects(Self::FETCH_BODY) && response.body.is_none() {
            return false;
        }
        if self.intersects(Self::FETCH_REFERENCES) && response.references.is_none() {
            return false;
        }
        if self.intersects(Self::FETCH_BODYSTRUCTURE) && !response.bodystructure {
            return false;
        }
        if self.intersects(Self::FETCH_ENVELOPE) && response.envelope.is_none() {
            return false;
        }
        true
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Alert(String);

pub type ImapParseResult<'a, T> = Result<(&'a [u8], T, Option<Alert>)>;
pub struct ImapLineIterator<'a> {
    slice: &'a [u8],
}

#[derive(Debug, Eq, PartialEq)]
pub enum ResponseCode {
    ///The human-readable text contains a special alert that MUST be presented
    /// to the user in a fashion that calls the user's attention to the message.
    Alert(String),

    ///Optionally followed by a parenthesized list of charsets.  A SEARCH
    /// failed because the given charset is not supported by this
    /// implementation.  If the optional list of charsets is given, this lists
    /// the charsets that are supported by this implementation.
    Badcharset(Option<String>),

    /// Followed by a list of capabilities.  This can appear in the initial OK
    /// or PREAUTH response to transmit an initial capabilities list.  This
    /// makes it unnecessary for a client to send a separate CAPABILITY command
    /// if it recognizes this response.
    Capability,

    /// The human-readable text represents an error in parsing the [RFC-2822]
    /// header or [MIME-IMB] headers of a message in the mailbox.
    Parse(String),

    /// Followed by a parenthesized list of flags, indicates which of the known
    /// flags the client can change permanently.  Any flags that are in the
    /// FLAGS untagged response, but not the PERMANENTFLAGS list, can not be set
    /// permanently.  If the client attempts to STORE a flag that is not in the
    /// PERMANENTFLAGS list, the server will either ignore the change or store
    /// the state change for the remainder of the current session only.  The
    /// PERMANENTFLAGS list can also include the special flag \*, which
    /// indicates that it is possible to create new keywords by attempting to
    /// store those flags in the mailbox.
    Permanentflags(String),

    /// The mailbox is selected read-only, or its access while selected has
    /// changed from read-write to read-only.
    ReadOnly,

    /// The mailbox is selected read-write, or its access while selected has
    /// changed from read-only to read-write.
    ReadWrite,

    /// An APPEND or COPY attempt is failing because the target mailbox does not
    /// exist (as opposed to some other reason).  This is a hint to the client
    /// that the operation can succeed if the mailbox is first created by the
    /// CREATE command.
    Trycreate,

    /// Followed by a decimal number, indicates the next unique identifier
    /// value.  Refer to section 2.3.1.1 for more information.
    Uidnext(UID),
    /// Followed by a decimal number, indicates the unique identifier validity
    /// value.  Refer to section 2.3.1.1 for more information.
    Uidvalidity(UID),
    /// Followed by a decimal number, indicates the number of the first message
    /// without the \Seen flag set.
    Unseen(ImapNum),
}

impl std::fmt::Display for ResponseCode {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ResponseCode::*;
        match self {
            Alert(s) => write!(fmt, "ALERT: {}", s),
            Badcharset(None) => write!(fmt, "Given charset is not supported by this server."),
            Badcharset(Some(s)) => write!(
                fmt,
                "Given charset is not supported by this server. Supported ones are: {}",
                s
            ),
            Capability => write!(fmt, "Capability response"),
            Parse(s) => write!(fmt, "Server error in parsing message headers: {}", s),
            Permanentflags(s) => write!(fmt, "Mailbox supports these flags: {}", s),
            ReadOnly => write!(fmt, "This mailbox is selected read-only."),
            ReadWrite => write!(fmt, "This mailbox is selected with read-write permissions."),
            Trycreate => write!(
                fmt,
                "Failed to operate on the target mailbox because it doesn't exist. Try creating \
                 it first."
            ),
            Uidnext(uid) => write!(fmt, "Next UID value is {}", uid),
            Uidvalidity(uid) => write!(fmt, "Next UIDVALIDITY value is {}", uid),
            Unseen(uid) => write!(fmt, "First message without the \\Seen flag is {}", uid),
        }
    }
}

impl ResponseCode {
    fn from(val: &[u8]) -> Self {
        use ResponseCode::*;
        if !val.starts_with(b"[") {
            let msg = val.trim();
            return Alert(String::from_utf8_lossy(msg).to_string());
        }

        let val = &val[1..];
        if val.starts_with(b"BADCHARSET") {
            let charsets = val.find(b"(").map(|pos| val[pos + 1..].trim());
            Badcharset(charsets.map(|charsets| String::from_utf8_lossy(charsets).to_string()))
        } else if val.starts_with(b"READONLY") {
            ReadOnly
        } else if val.starts_with(b"READWRITE") {
            ReadWrite
        } else if val.starts_with(b"TRYCREATE") {
            Trycreate
        } else if val.starts_with(b"UIDNEXT") {
            Uidnext(
                UID::from_str(&String::from_utf8_lossy(
                    val.find(b"]")
                        .map(|end| &val[b"UIDNEXT ".len()..end])
                        .unwrap_or(b"0".as_slice()),
                ))
                .unwrap_or(0),
            )
        } else if val.starts_with(b"UIDVALIDITY") {
            Uidvalidity(
                UIDVALIDITY::from_str(&String::from_utf8_lossy(
                    val.find(b"]")
                        .map(|end| &val[b"UIDVALIDITY ".len()..end])
                        .unwrap_or(b"0".as_slice()),
                ))
                .unwrap_or(0),
            )
        } else if val.starts_with(b"UNSEEN") {
            Unseen(
                usize::from_str(&String::from_utf8_lossy(
                    val.find(b"]")
                        .map(|end| &val[b"UNSEEN ".len()..end])
                        .unwrap_or(b"0".as_slice()),
                ))
                .unwrap_or(0),
            )
        } else {
            let msg = &val[val.find(b"] ").unwrap() + 1..].trim();
            Alert(String::from_utf8_lossy(msg).to_string())
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ImapResponse {
    Ok(ResponseCode),
    No(ResponseCode),
    Bad(ResponseCode),
    Preauth(ResponseCode),
    Bye(ResponseCode),
}

impl TryFrom<&'_ [u8]> for ImapResponse {
    type Error = Error;
    fn try_from(original_val: &'_ [u8]) -> Result<Self> {
        let val: &[u8] = original_val.split_rn().last().unwrap_or(original_val);
        let mut val = val[val.find(b" ").ok_or_else(|| {
            Error::new(format!(
                "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                String::from_utf8_lossy(original_val)
            ))
        })? + 1..]
            .trim();
        // M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters
        // (0.000 + 0.098 + 0.097 secs).\r\n
        if val.ends_with(b" secs).") {
            val = &val[..val.rfind(b"(").ok_or_else(|| {
                Error::new(format!(
                    "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                    String::from_utf8_lossy(original_val)
                ))
            })?];
        }

        Ok(if val.starts_with(b"OK") {
            Self::Ok(ResponseCode::from(&val[b"OK ".len()..]))
        } else if val.starts_with(b"NO") {
            Self::No(ResponseCode::from(&val[b"NO ".len()..]))
        } else if val.starts_with(b"BAD") {
            Self::Bad(ResponseCode::from(&val[b"BAD ".len()..]))
        } else if val.starts_with(b"PREAUTH") {
            Self::Preauth(ResponseCode::from(&val[b"PREAUTH ".len()..]))
        } else if val.starts_with(b"BYE") {
            Self::Bye(ResponseCode::from(&val[b"BYE ".len()..]))
        } else {
            return Err(Error::new(format!(
                "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                String::from_utf8_lossy(original_val)
            )));
        })
    }
}

impl From<ImapResponse> for Result<()> {
    fn from(resp: ImapResponse) -> Self {
        match resp {
            ImapResponse::Ok(_) | ImapResponse::Preauth(_) | ImapResponse::Bye(_) => Ok(()),
            ImapResponse::No(ResponseCode::Alert(msg))
            | ImapResponse::Bad(ResponseCode::Alert(msg)) => Err(Error::new(msg)),
            ImapResponse::No(err) => Err(Error::new(format!("{:?}", err)))
                .chain_err_summary(|| "IMAP NO Response.".to_string()),
            ImapResponse::Bad(err) => Err(Error::new(format!("{:?}", err)))
                .chain_err_summary(|| "IMAP BAD Response.".to_string()),
        }
    }
}

impl<'a> Iterator for ImapLineIterator<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<&'a [u8]> {
        if self.slice.is_empty() {
            return None;
        }
        let mut i = 0;
        loop {
            let cur_slice = &self.slice[i..];
            if let Some(pos) = cur_slice.find(CRLF) {
                // Skip literal continuation line
                if let Some(literal_start) = cur_slice[..pos].find(b"{") {
                    if let Ok((_, len)) =
                        delimited::<_, _, _, _, (&[u8], nom::error::ErrorKind), _, _, _>(
                            tag("{"),
                            map_res(digit1, |s| {
                                usize::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                            }),
                            tag("}\r\n"),
                        )(&cur_slice[literal_start..])
                    {
                        i += pos + 2 + len;
                        continue;
                    }
                }
                let ret = self.slice.get(..i + pos + 2).unwrap_or_default();
                self.slice = self.slice.get(i + pos + 2..).unwrap_or_default();
                return Some(ret);
            } else {
                let ret = self.slice;
                self.slice = self.slice.get(ret.len()..).unwrap_or_default();
                return Some(ret);
            }
        }
    }
}

pub trait ImapLineSplit {
    fn split_rn(&self) -> ImapLineIterator;
}

impl ImapLineSplit for [u8] {
    fn split_rn(&self) -> ImapLineIterator {
        ImapLineIterator { slice: self }
    }
}

macro_rules! to_str (
    ($v:expr) => (unsafe{ std::str::from_utf8_unchecked($v) })
);

/*macro_rules! dbg_dmp (
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
*/

/*
 * LIST (\HasNoChildren) "." INBOX.Sent
 * LIST (\HasChildren) "." INBOX
 */

pub fn list_mailbox_result(input: &[u8]) -> IResult<&[u8], ImapMailbox> {
    let (input, _) = alt((tag("* LIST ("), tag("* LSUB (")))(input.ltrim())?;
    let (input, properties) = take_until(&b")"[0..])(input)?;
    let (input, _) = tag(b") ")(input)?;
    let (input, separator) = delimited(tag(b"\""), take(1_u32), tag(b"\""))(input)?;
    let (input, _) = take(1_u32)(input)?;
    let (input, path) = mailbox_token(input)?;
    let (input, _) = tag(CRLF)(input)?;
    Ok((
        input,
        ({
            let separator: u8 = separator[0];
            let mut f = ImapMailbox {
                no_select: false,
                is_subscribed: false,
                ..ImapMailbox::default()
            };

            if path.eq_ignore_ascii_case("INBOX") {
                f.is_subscribed = true;
                let _ = f.set_special_usage(SpecialUsageMailbox::Inbox);
            }

            for p in properties.split(|&b| b == b' ') {
                if p.eq_ignore_ascii_case(b"\\NoSelect") || p.eq_ignore_ascii_case(b"\\NonExistent")
                {
                    f.no_select = true;
                } else if p.eq_ignore_ascii_case(b"\\Subscribed") {
                    f.is_subscribed = true;
                } else if p.eq_ignore_ascii_case(b"\\Sent") {
                    let _ = f.set_special_usage(SpecialUsageMailbox::Sent);
                } else if p.eq_ignore_ascii_case(b"\\Junk") {
                    let _ = f.set_special_usage(SpecialUsageMailbox::Junk);
                } else if p.eq_ignore_ascii_case(b"\\Trash") {
                    let _ = f.set_special_usage(SpecialUsageMailbox::Trash);
                } else if p.eq_ignore_ascii_case(b"\\Drafts") {
                    let _ = f.set_special_usage(SpecialUsageMailbox::Drafts);
                } else if p.eq_ignore_ascii_case(b"\\Flagged") {
                    let _ = f.set_special_usage(SpecialUsageMailbox::Flagged);
                } else if p.eq_ignore_ascii_case(b"\\Archive") {
                    let _ = f.set_special_usage(SpecialUsageMailbox::Archive);
                }
            }
            f.imap_path = path.to_string();
            f.hash = MailboxHash::from_bytes(f.imap_path.as_bytes());
            f.path = if separator == b'/' {
                f.imap_path.clone()
            } else {
                f.imap_path.replace(separator as char, "/")
            };
            f.name = if let Some(pos) = f.imap_path.as_bytes().iter().rposition(|&c| c == separator)
            {
                f.parent = Some(MailboxHash::from_bytes(&f.imap_path.as_bytes()[..pos]));
                f.imap_path[pos + 1..].to_string()
            } else {
                f.imap_path.clone()
            };
            f.separator = separator;

            f
        }),
    ))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FetchResponse<'a> {
    pub uid: Option<UID>,
    pub message_sequence_number: MessageSequenceNumber,
    pub modseq: Option<ModSequence>,
    pub flags: Option<(Flag, Vec<String>)>,
    pub body: Option<&'a [u8]>,
    pub references: Option<&'a [u8]>,
    pub envelope: Option<Envelope>,
    pub bodystructure: bool,
    pub raw_fetch_value: &'a [u8],
}

pub fn fetch_response(input: &[u8]) -> ImapParseResult<FetchResponse<'_>> {
    macro_rules! should_start_with {
        ($input:expr, $tag:tt) => {
            if !$input.starts_with($tag) {
                return Err(Error::new(format!(
                    "Expected `{}` but got `{:.50}`",
                    String::from_utf8_lossy($tag),
                    String::from_utf8_lossy(&$input)
                )));
            }
        };
    }
    should_start_with!(input, UNTAGGED_PREFIX);

    let mut i = UNTAGGED_PREFIX.len();
    macro_rules! bounds {
        () => {
            if i == input.len() {
                return Err(Error::new(format!(
                    "Expected more input. Got: `{:.50}`",
                    String::from_utf8_lossy(&input)
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
            while (input[i] as char).is_whitespace() {
                i += 1;
                bounds!();
            }
        };
        (break) => {
            while (input[i] as char).is_whitespace() {
                i += 1;
                bounds!(break);
            }
        };
    }

    let mut ret = FetchResponse {
        uid: None,
        message_sequence_number: 0,
        modseq: None,
        flags: None,
        body: None,
        references: None,
        envelope: None,
        bodystructure: false,
        raw_fetch_value: &[],
    };

    while input[i].is_ascii_digit() {
        let b: u8 = input[i] - 0x30;
        ret.message_sequence_number *= 10;
        ret.message_sequence_number += b as MessageSequenceNumber;
        i += 1;
        bounds!();
    }

    eat_whitespace!();
    should_start_with!(&input[i..], b"FETCH (");
    i += b"FETCH (".len();
    let mut has_attachments = false;
    while i < input.len() {
        eat_whitespace!(break);
        bounds!(break);

        if input[i..].starts_with(b"UID ") {
            i += b"UID ".len();
            if let Ok((rest, uid)) =
                take_while::<_, &[u8], (&[u8], nom::error::ErrorKind)>(is_digit)(&input[i..])
            {
                i += input.len() - i - rest.len();
                ret.uid =
                    Some(UID::from_str(unsafe { std::str::from_utf8_unchecked(uid) }).unwrap());
            } else {
                log::debug!(
                    "Unexpected input while parsing UID FETCH response. Got: `{}`",
                    String::from_utf8_lossy(input)
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{}`",
                    String::from_utf8_lossy(input).as_ref().trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b"FLAGS (") {
            i += b"FLAGS (".len();
            if let Ok((rest, flags)) = flags(&input[i..]) {
                ret.flags = Some(flags);
                i += (input.len() - i - rest.len()) + 1;
            } else {
                log::debug!(
                    "Unexpected input while parsing UID FETCH response. Could not parse FLAGS: {}.",
                    String::from_utf8_lossy(&input[i..])
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse FLAGS: \
                     `{}`.",
                    String::from_utf8_lossy(&input[i..])
                        .as_ref()
                        .trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b"MODSEQ (") {
            i += b"MODSEQ (".len();
            if let Ok((rest, modseq)) =
                take_while::<_, &[u8], (&[u8], nom::error::ErrorKind)>(is_digit)(&input[i..])
            {
                i += (input.len() - i - rest.len()) + 1;
                ret.modseq = u64::from_str(to_str!(modseq))
                    .ok()
                    .and_then(std::num::NonZeroU64::new)
                    .map(ModSequence);
            } else {
                log::debug!(
                    "Unexpected input while parsing MODSEQ in UID FETCH response. Got: `{}`",
                    String::from_utf8_lossy(input)
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing MODSEQ in UID FETCH response. Got: `{}`",
                    String::from_utf8_lossy(input).as_ref().trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b"BODY[] {") || input[i..].starts_with(b"RFC822 {") {
            // b"BODY[] ".len() == b"RFC822 ".len()
            i += b"BODY[] ".len();
            if let Ok((rest, body)) =
                length_data::<_, _, (&[u8], nom::error::ErrorKind), _>(delimited(
                    tag("{"),
                    map_res(digit1, |s| {
                        usize::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                    }),
                    tag("}\r\n"),
                ))(&input[i..])
            {
                ret.body = Some(body);
                i += input.len() - i - rest.len();
            } else {
                log::debug!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     RFC822/BODY: {}",
                    String::from_utf8_lossy(&input[i..])
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     RFC822/BODY: {}",
                    String::from_utf8_lossy(&input[i..])
                        .as_ref()
                        .trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b"ENVELOPE (") {
            i += b"ENVELOPE ".len();
            if let Ok((rest, envelope)) = envelope(&input[i..]) {
                ret.envelope = Some(envelope);
                i += input.len() - i - rest.len();
            } else {
                log::debug!(
                    "Unexpected input while parsing UID FETCH response. Could not parse ENVELOPE: \
                     {}",
                    String::from_utf8_lossy(&input[i..])
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse ENVELOPE: \
                     {}",
                    String::from_utf8_lossy(&input[i..])
                        .as_ref()
                        .trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b"BODYSTRUCTURE ") {
            i += b"BODYSTRUCTURE ".len();

            let (rest, _has_attachments) = bodystructure_has_attachments(&input[i..])?;
            has_attachments = _has_attachments;
            ret.bodystructure = true;
            i += input[i..].len() - rest.len();
        } else if input[i..].starts_with(b"BODY[HEADER.FIELDS (REFERENCES)] ") {
            i += b"BODY[HEADER.FIELDS (REFERENCES)] ".len();
            if let Ok((rest, mut references)) = astring_token(&input[i..]) {
                if !references.trim().is_empty() {
                    if let Ok((_, (_, v))) = crate::email::parser::headers::header(references) {
                        references = v;
                    }
                    ret.references = Some(references);
                } else {
                    ret.references = Some(references.trim());
                }
                i += input.len() - i - rest.len();
            } else {
                log::debug!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     BODY[HEADER.FIELDS (REFERENCES)]: {}",
                    String::from_utf8_lossy(&input[i..])
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     BODY[HEADER.FIELDS (REFERENCES)]: {}",
                    String::from_utf8_lossy(&input[i..])
                        .as_ref()
                        .trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b"BODY[HEADER.FIELDS (\"REFERENCES\")] ") {
            i += b"BODY[HEADER.FIELDS (\"REFERENCES\")] ".len();
            if let Ok((rest, mut references)) = astring_token(&input[i..]) {
                if !references.trim().is_empty() {
                    if let Ok((_, (_, v))) = crate::email::parser::headers::header(references) {
                        references = v;
                    }
                    ret.references = Some(references);
                }
                i += input.len() - i - rest.len();
            } else {
                log::debug!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     BODY[HEADER.FIELDS (\"REFERENCES\"): {}",
                    String::from_utf8_lossy(&input[i..])
                );
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     BODY[HEADER.FIELDS (\"REFERENCES\"): {}",
                    String::from_utf8_lossy(&input[i..])
                        .as_ref()
                        .trim_at_boundary(40)
                )));
            }
        } else if input[i..].starts_with(b")\r\n") {
            i += b")\r\n".len();
            break;
        } else {
            log::debug!(
                "Got unexpected token while parsing UID FETCH response:\n`{}`\n",
                String::from_utf8_lossy(input)
            );
            return Err(Error::new(format!(
                "Got unexpected token while parsing UID FETCH response: `{}`",
                String::from_utf8_lossy(&input[i..])
                    .as_ref()
                    .trim_at_boundary(40)
            )));
        }
    }
    ret.raw_fetch_value = &input[..i];

    if let Some(env) = ret.envelope.as_mut() {
        env.set_has_attachments(has_attachments);
    }

    Ok((&input[i..], ret, None))
}

pub fn fetch_responses(mut input: &[u8]) -> ImapParseResult<Vec<FetchResponse<'_>>> {
    let mut ret = Vec::new();
    let mut alert: Option<Alert> = None;

    while input.starts_with(UNTAGGED_PREFIX) {
        let next_response = fetch_response(input);
        match next_response {
            Ok((rest, el, el_alert)) => {
                if let Some(el_alert) = el_alert {
                    match &mut alert {
                        Some(Alert(ref mut alert)) => {
                            alert.push_str(&el_alert.0);
                        }
                        a @ None => *a = Some(el_alert),
                    }
                }
                input = rest;
                ret.push(el);
            }
            Err(err) => {
                return Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH responses: {} `{:.40}`",
                    err,
                    String::from_utf8_lossy(input),
                )));
            }
        }
    }

    if !input.is_empty()
        && ret.is_empty()
        && !matches!(ImapResponse::try_from(input), Ok(ImapResponse::Ok(_)))
    {
        return Err(Error::new(format!(
            "810Unexpected input while parsing UID FETCH responses: `{:.40}`",
            String::from_utf8_lossy(input)
        )));
    }
    Ok((input, ret, alert))
}

pub fn uid_fetch_flags_responses(input: &[u8]) -> IResult<&[u8], Vec<(UID, (Flag, Vec<String>))>> {
    many0(uid_fetch_flags_response)(input)
}

pub fn uid_fetch_flags_response(input: &[u8]) -> IResult<&[u8], (UID, (Flag, Vec<String>))> {
    let (input, _) = tag("* ")(input)?;
    let (input, _msn) = take_while(is_digit)(input)?;
    let (input, _) = tag(" FETCH (")(input)?;
    let (input, uid_flags) = permutation((
        preceded(
            alt((tag("UID "), tag(" UID "))),
            map_res(digit1, |s| UID::from_str(to_str!(s))),
        ),
        preceded(
            alt((tag("FLAGS "), tag(" FLAGS "))),
            delimited(tag("("), byte_flags, tag(")")),
        ),
    ))(input)?;
    let (input, _) = tag(")\r\n")(input)?;
    Ok((input, (uid_flags.0, uid_flags.1)))
}

/* Input Example:
 * ==============
 *
 *  "M0 OK [CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE
 * IDLE SORT SORT=DISPLAY THREAD=REFERENCES THREAD=REFS THREAD=ORDEREDSUBJECT
 * MULTIAPPEND URL-PARTIAL CATENATE UNSELECT CHILDREN NAMESPACE UIDPLUS
 * LIST-EXTENDED I18NLEVEL=1 CONDSTORE QRESYNC ESEARCH ESORT SEARCHRES WITHIN
 * CONTEXT=SEARCH LIST-STATUS BINARY MOVE SPECIAL-USE] Logged in\r\n"
 *   "* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN
 * X-GM-EXT-1 XYZZY SASL-IR AUTH=XOAUTH2 AUTH=PLAIN AUTH=PLAIN-CLIENT TOKEN
 * AUTH=OAUTHBEARER AUTH=XOAUTH\r\n"   "* CAPABILITY IMAP4rev1 LITERAL+
 * SASL-IR LOGIN-REFERRALS ID ENABLE IDLE AUTH=PLAIN\r\n"
 */

pub fn capabilities(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
    let (input, _) = take_until("CAPABILITY ")(input)?;
    let (input, _) = tag("CAPABILITY ")(input)?;
    let (input, ret) = separated_list1(tag(" "), is_not(" ]\r\n"))(input)?;
    let (input, _) = take_until(CRLF)(input)?;
    let (input, _) = tag(CRLF)(input)?;
    Ok((input, ret))
}

/// This enum represents the server's untagged responses detailed in `7. Server
/// Responses` of RFC 3501   INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1
#[derive(Debug, Eq, PartialEq)]
pub enum UntaggedResponse<'s> {
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
    Expunge(MessageSequenceNumber),
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
    Exists(ImapNum),
    /// ```text
    /// 7.3.2. RECENT Response
    /// The RECENT response reports the number of messages with the
    /// \Recent flag set.  This response occurs as a result of a SELECT or
    /// EXAMINE command, and if the size of the mailbox changes (e.g., new
    /// messages).
    /// ```
    Recent(ImapNum),
    Fetch(Box<FetchResponse<'s>>),
    Bye {
        reason: &'s str,
    },
}

pub fn untagged_responses(input: &[u8]) -> ImapParseResult<Option<UntaggedResponse<'_>>> {
    let orig_input = input;
    let (input, _) = tag::<_, &[u8], (&[u8], nom::error::ErrorKind)>(UNTAGGED_PREFIX)(input)?;
    let (input, num) = map_res::<_, _, _, (&[u8], nom::error::ErrorKind), _, _, _>(digit1, |s| {
        ImapNum::from_str(unsafe { std::str::from_utf8_unchecked(s) })
    })(input)?;
    let (input, _) = tag::<_, &[u8], (&[u8], nom::error::ErrorKind)>(b" ")(input)?;
    let (input, _tag) = take_until::<_, &[u8], (&[u8], nom::error::ErrorKind)>(CRLF)(input)?;
    let (input, _) = tag::<_, &[u8], (&[u8], nom::error::ErrorKind)>(CRLF)(input)?;
    log::trace!(
        "Parse untagged response from {:?}",
        String::from_utf8_lossy(orig_input)
    );
    Ok((
        input,
        {
            use UntaggedResponse::*;
            match _tag {
                b"EXPUNGE" => Some(Expunge(num)),
                b"EXISTS" => Some(Exists(num)),
                b"RECENT" => Some(Recent(num)),
                _ if _tag.starts_with(b"FETCH ") => {
                    Some(Fetch(Box::new(fetch_response(orig_input)?.1)))
                }
                _ => {
                    log::error!(
                        "unknown untagged_response: {}, message was {:?}",
                        String::from_utf8_lossy(_tag),
                        String::from_utf8_lossy(orig_input)
                    );
                    None
                }
            }
        },
        None,
    ))
}

pub fn search_results<'a>(input: &'a [u8]) -> IResult<&'a [u8], Vec<ImapNum>> {
    alt((
        |input: &'a [u8]| -> IResult<&'a [u8], Vec<ImapNum>> {
            let (input, _) = tag("* SEARCH ")(input)?;
            let (input, list) = separated_list1(
                tag(b" "),
                map_res(is_not(" \r\n"), |s: &[u8]| {
                    ImapNum::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                }),
            )(input)?;
            let (input, _) = tag(CRLF)(input)?;
            Ok((input, list))
        },
        |input: &'a [u8]| -> IResult<&'a [u8], Vec<ImapNum>> {
            let (input, _) = tag("* SEARCH\r\n")(input)?;
            Ok((input, vec![]))
        },
    ))(input)
}

pub fn search_results_raw<'a>(input: &'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
    alt((
        |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
            let (input, _) = tag("* SEARCH ")(input)?;
            let (input, list) = take_until(CRLF)(input)?;
            let (input, _) = tag(CRLF)(input)?;
            Ok((input, list))
        },
        |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
            let (input, _) = tag("* SEARCH\r\n")(input)?;
            Ok((input, &b""[0..]))
        },
    ))(input)
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SelectResponse {
    pub exists: ImapNum,
    pub recent: ImapNum,
    pub flags: (Flag, Vec<String>),
    pub first_unseen: MessageSequenceNumber,
    pub uidvalidity: UIDVALIDITY,
    pub uidnext: UID,
    pub permanentflags: (Flag, Vec<String>),
    /// if `SELECT` returns `\*` we can set arbitrary flags permanently.
    pub can_create_flags: bool,
    pub read_only: bool,
    pub highestmodseq: Option<std::result::Result<ModSequence, ()>>,
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
 *  * OK [PERMANENTFLAGS (\Answered \Flagged \Deleted \Seen \Draft \*)] Flags
 *    permitted.
 *  * 45 EXISTS
 *  * 0 RECENT
 *  * OK [UNSEEN 16] First unseen.
 *  * OK [UIDVALIDITY 1554422056] UIDs valid
 *  * OK [UIDNEXT 50] Predicted next UID
 */
pub fn select_response(input: &[u8]) -> Result<SelectResponse> {
    if input.contains_subsequence(b"* OK") {
        let mut ret = SelectResponse::default();
        for l in input.split_rn() {
            if l.starts_with(UNTAGGED_PREFIX) && l.ends_with(b" EXISTS\r\n") {
                ret.exists = ImapNum::from_str(&String::from_utf8_lossy(
                    &l[UNTAGGED_PREFIX.len()..l.len() - b" EXISTS\r\n".len()],
                ))?;
            } else if l.starts_with(UNTAGGED_PREFIX) && l.ends_with(b" RECENT\r\n") {
                ret.recent = ImapNum::from_str(&String::from_utf8_lossy(
                    &l[UNTAGGED_PREFIX.len()..l.len() - b" RECENT\r\n".len()],
                ))?;
            } else if l.starts_with(b"* FLAGS (") {
                ret.flags = flags(&l[b"* FLAGS (".len()..l.len() - b")".len()]).map(|(_, v)| v)?;
            } else if l.starts_with(b"* OK [UNSEEN ") {
                ret.first_unseen = MessageSequenceNumber::from_str(&String::from_utf8_lossy(
                    &l[b"* OK [UNSEEN ".len()..l.find(b"]").unwrap()],
                ))?;
            } else if l.starts_with(b"* OK [UIDVALIDITY ") {
                ret.uidvalidity = UIDVALIDITY::from_str(&String::from_utf8_lossy(
                    &l[b"* OK [UIDVALIDITY ".len()..l.find(b"]").unwrap()],
                ))?;
            } else if l.starts_with(b"* OK [UIDNEXT ") {
                ret.uidnext = UID::from_str(&String::from_utf8_lossy(
                    &l[b"* OK [UIDNEXT ".len()..l.find(b"]").unwrap()],
                ))?;
            } else if l.starts_with(b"* OK [PERMANENTFLAGS (") {
                ret.permanentflags =
                    flags(&l[b"* OK [PERMANENTFLAGS (".len()..l.find(b")").unwrap()])
                        .map(|(_, v)| v)?;
                ret.can_create_flags = l.contains_subsequence(b"\\*");
            } else if l.contains_subsequence(b"OK [READ-WRITE]" as &[u8]) {
                ret.read_only = false;
            } else if l.contains_subsequence(b"OK [READ-ONLY]") {
                ret.read_only = true;
            } else if l.starts_with(b"* OK [HIGHESTMODSEQ ") {
                let res: IResult<&[u8], &[u8]> =
                    take_until(&b"]"[..])(&l[b"* OK [HIGHESTMODSEQ ".len()..]);
                let (_, highestmodseq) = res?;
                ret.highestmodseq = Some(
                    std::num::NonZeroU64::new(u64::from_str(&String::from_utf8_lossy(
                        highestmodseq,
                    ))?)
                    .map(|u| Ok(ModSequence(u)))
                    .unwrap_or(Err(())),
                );
            } else if l.starts_with(b"* OK [NOMODSEQ") {
                ret.highestmodseq = Some(Err(()));
            } else if !l.is_empty() {
                log::trace!("select response: {}", String::from_utf8_lossy(l));
            }
        }
        Ok(ret)
    } else {
        let ret = String::from_utf8_lossy(input).to_string();
        log::error!("BAD/NO response in select: {}", &ret);
        Err(Error::new(ret))
    }
}

pub fn flags(input: &[u8]) -> IResult<&[u8], (Flag, Vec<String>)> {
    let mut ret = Flag::default();
    let mut keywords = Vec::new();

    let mut input = input;
    while !input.starts_with(b")") && !input.is_empty() {
        let is_system_flag = input.starts_with(b"\\");
        if is_system_flag {
            input = &input[1..];
        }
        let mut match_end = 0;
        while match_end < input.len() {
            if input[match_end..].starts_with(b" ") || input[match_end..].starts_with(b")") {
                break;
            }
            match_end += 1;
        }

        match (is_system_flag, &input[..match_end]) {
            (true, t) if t.eq_ignore_ascii_case(b"Answered") => {
                ret.set(Flag::REPLIED, true);
            }
            (true, t) if t.eq_ignore_ascii_case(b"Flagged") => {
                ret.set(Flag::FLAGGED, true);
            }
            (true, t) if t.eq_ignore_ascii_case(b"Deleted") => {
                ret.set(Flag::TRASHED, true);
            }
            (true, t) if t.eq_ignore_ascii_case(b"Seen") => {
                ret.set(Flag::SEEN, true);
            }
            (true, t) if t.eq_ignore_ascii_case(b"Draft") => {
                ret.set(Flag::DRAFT, true);
            }
            (true, t) if t.eq_ignore_ascii_case(b"Recent") => { /* ignore */ }
            (_, f) => {
                keywords.push(String::from_utf8_lossy(f).into());
            }
        }
        input = &input[match_end..];
        input = input.trim_start();
    }
    Ok((input, (ret, keywords)))
}

pub fn byte_flags(input: &[u8]) -> IResult<&[u8], (Flag, Vec<String>)> {
    match flags(input) {
        Ok((rest, ret)) => Ok((rest, ret)),
        Err(nom::Err::Error(err)) => Err(nom::Err::Error(err)),
        Err(nom::Err::Failure(err)) => Err(nom::Err::Error(err)),
        Err(nom::Err::Incomplete(_)) => {
            Err(nom::Err::Error((input, "byte_flags(): incomplete").into()))
        }
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

pub fn envelope(input: &[u8]) -> IResult<&[u8], Envelope> {
    const WS: &[u8] = b"\r\n\t ";
    let (input, _) = tag("(")(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, date) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, subject) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, from) = envelope_addresses(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, _sender) = envelope_addresses(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, _reply_to) = envelope_addresses(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, to) = envelope_addresses(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, cc) = envelope_addresses(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, bcc) = envelope_addresses(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, in_reply_to) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, message_id) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a(WS))(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((
        input,
        ({
            let mut env = Envelope::new(EnvelopeHash::default());
            if let Some(date) = date {
                env.set_date(&date);
                if let Ok(d) =
                    crate::email::parser::dates::rfc5322_date(env.date_as_str().as_bytes())
                {
                    env.set_datetime(d);
                }
                env.other_headers_mut()
                    .insert(HeaderName::DATE, String::from_utf8_lossy(&date).to_string());
            }

            if let Some(subject) = subject {
                env.set_subject(subject.to_vec());
                env.other_headers_mut().insert(
                    HeaderName::SUBJECT,
                    String::from_utf8_lossy(&subject).to_string(),
                );
            }

            if let Some(from) = from {
                env.other_headers_mut()
                    .insert(HeaderName::FROM, Address::display_slice(&from, None));
                env.set_from(from);
            }
            if let Some(to) = to {
                env.other_headers_mut()
                    .insert(HeaderName::TO, Address::display_slice(&to, None));
                env.set_to(to);
            }

            if let Some(cc) = cc {
                env.other_headers_mut()
                    .insert(HeaderName::CC, Address::display_slice(&cc, None));
                env.set_cc(cc);
            }

            if let Some(bcc) = bcc {
                env.set_bcc(bcc.to_vec());
                env.other_headers_mut()
                    .insert(HeaderName::BCC, Address::display_slice(&bcc, None));
            }
            if let Some(in_reply_to) = in_reply_to {
                env.set_in_reply_to(&in_reply_to);
                if let Some(in_reply_to) = env.in_reply_to().map(|r| r.as_ref().clone()) {
                    env.push_references(&in_reply_to);
                }
            }

            if let Some(message_id) = message_id {
                env.set_message_id(&message_id);
            }
            env
        }),
    ))
}

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
pub fn envelope_addresses<'a>(
    input: &'a [u8],
) -> IResult<&'a [u8], Option<SmallVec<[Address; 1]>>> {
    alt((
        map(tag("NIL"), |_| None),
        |input: &'a [u8]| -> IResult<&'a [u8], Option<SmallVec<[Address; 1]>>> {
            let (input, _) = tag("(")(input)?;
            let (input, envelopes) = fold_many1(
                delimited(tag("("), envelope_address, alt((tag(") "), tag(")")))),
                SmallVec::new,
                |mut acc, item| {
                    acc.push(item);
                    acc
                },
            )(input.ltrim())?;
            let (input, _) = tag(")")(input)?;
            Ok((input, Some(envelopes)))
        },
        map(tag("\"\""), |_| None),
    ))(input)
}

// Parse an address in the format of the ENVELOPE structure eg
// ("Terry Gray" NIL "gray" "cac.washington.edu")
pub fn envelope_address(input: &[u8]) -> IResult<&[u8], Address> {
    const WS: &[u8] = b"\r\n\t ";
    let (input, name) = utils::nil_to_default(quoted)(input)?;
    let (input, _) = is_a(WS)(input)?;
    let (input, _) = utils::nil_to_default(quoted)(input)?;
    let (input, _) = is_a(WS)(input)?;
    let (input, mailbox_name) = utils::nil_to_default(quoted)(input)?;
    let (input, host_name) = opt(preceded(is_a(WS), utils::nil_to_default(quoted)))(input)?;
    Ok((
        input,
        Address::Mailbox(MailboxAddress {
            raw: if let Some(host_name) = host_name.as_ref() {
                format!(
                    "{}{}<{}@{}>",
                    to_str!(&name),
                    if name.is_empty() { "" } else { " " },
                    to_str!(&mailbox_name),
                    to_str!(host_name)
                )
                .into_bytes()
            } else {
                format!(
                    "{}{}{}",
                    to_str!(&name),
                    if name.is_empty() { "" } else { " " },
                    to_str!(&mailbox_name),
                )
                .into_bytes()
            },
            display_name: str_builder!(0, name.len()),
            address_spec: if let Some(host_name) = host_name.as_ref() {
                str_builder!(
                    if name.is_empty() { 1 } else { name.len() + 2 },
                    mailbox_name.len() + host_name.len() + 1
                )
            } else {
                str_builder!(
                    if name.is_empty() { 0 } else { name.len() + 1 },
                    mailbox_name.len()
                )
            },
        }),
    ))
}

// Read a literal ie a byte sequence prefixed with a tag containing its length
// delimited in {}s
pub fn literal(input: &[u8]) -> IResult<&[u8], &[u8]> {
    length_data(delimited(
        tag("{"),
        map_res(digit1, |s| {
            usize::from_str(unsafe { std::str::from_utf8_unchecked(s) })
        }),
        tag("}\r\n"),
    ))(input)
}

// Return a byte sequence surrounded by "s and decoded if necessary
pub fn quoted(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if let Ok((r, o)) = literal(input) {
        return match crate::email::parser::encodings::phrase(o, false) {
            Ok((_, mut out)) => {
                if out.contains_subsequence(b"\\\\") {
                    out = out.replace(b"\\\\", b"\\");
                }
                if out.contains_subsequence(b"\\\"") {
                    out = out.replace(b"\\\"", b"\"");
                }
                Ok((r, out))
            }
            e => e,
        };
    }
    if input.is_empty() || input[0] != b'"' {
        return Err(nom::Err::Error((input, "quoted(): EOF").into()));
    }

    let mut i = 1;
    let mut escape_next = false;
    while i < input.len() {
        match (input[i], escape_next) {
            (b'\\', false) => {
                escape_next = true;
            }
            (b'\\', true) => {
                escape_next = false;
            }
            (b'\"', false) => {
                return match crate::email::parser::encodings::phrase(&input[1..i], false) {
                    Ok((_, mut out)) => {
                        if out.contains_subsequence(b"\\\\") {
                            out = out.replace(b"\\\\", b"\\");
                        }
                        if out.contains_subsequence(b"\\\"") {
                            out = out.replace(b"\\\"", b"\"");
                        }
                        Ok((&input[i + 1..], out))
                    }
                    e => e,
                };
            }
            _ => {
                escape_next = false;
            }
        }
        i += 1;
    }

    Err(nom::Err::Error(
        (input, "quoted(): not a quoted phrase").into(),
    ))
}

#[inline]
pub fn quoted_or_nil(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    utils::nil_to_none(quoted)(input.ltrim())
}

pub fn uid_fetch_envelopes_response<'a>(
    input: &'a [u8],
) -> IResult<&'a [u8], Vec<(UID, Option<(Flag, Vec<String>)>, Envelope)>> {
    many0(
        |input: &'a [u8]| -> IResult<&'a [u8], (UID, Option<(Flag, Vec<String>)>, Envelope)> {
            let (input, _) = tag("* ")(input)?;
            let (input, _) = take_while(is_digit)(input)?;
            let (input, _) = tag(" FETCH (")(input)?;
            let (input, uid_flags) = permutation((
                preceded(
                    alt((tag("UID "), tag(" UID "))),
                    map_res(digit1, |s| {
                        UID::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                    }),
                ),
                opt(preceded(
                    alt((tag("FLAGS "), tag(" FLAGS "))),
                    delimited(tag("("), byte_flags, tag(")")),
                )),
            ))(input.ltrim())?;
            let (input, _) = tag(" ENVELOPE ")(input)?;
            let (input, env) = envelope(input.ltrim())?;
            let (input, _) = tag("BODYSTRUCTURE ")(input)?;
            let (input, has_attachments) = bodystructure_has_attachments(input)?;
            let (input, _) = tag(")\r\n")(input)?;
            Ok((input, {
                let mut env = env;
                env.set_has_attachments(has_attachments);
                (uid_flags.0, uid_flags.1, env)
            }))
        },
    )(input)
}

pub fn bodystructure_has_attachments(input: &[u8]) -> IResult<&[u8], bool> {
    let (input, _) = eat_whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (mut input, _) = eat_whitespace(input)?;
    let mut has_attachments = false;
    let mut first_in_line = true;
    while !input.is_empty() && !input.starts_with(b")") {
        if input.starts_with(b"\"") || input[0].is_ascii_alphanumeric() || input[0] == b'{' {
            let (_input, token) = astring_token(input)?;
            input = _input;
            if first_in_line {
                has_attachments |= token.eq_ignore_ascii_case(b"attachment");
            }
        } else if input.starts_with(b"(") {
            let (_input, _has_attachments) = bodystructure_has_attachments(input)?;
            has_attachments |= _has_attachments;
            input = _input;
        }
        let (_input, _) = eat_whitespace(input)?;
        input = _input;
        first_in_line = false;
    }
    let (input, _) = tag(")")(input)?;
    Ok((input, has_attachments))
}

fn eat_whitespace(mut input: &[u8]) -> IResult<&[u8], ()> {
    while !input.is_empty() {
        if input[0] == b' ' || input[0] == b'\n' || input[0] == b'\t' {
            input = &input[1..];
        } else if input.starts_with(CRLF) {
            input = &input[2..];
        } else {
            break;
        }
    }
    Ok((input, ()))
}

#[derive(Clone, Debug, Default)]
pub struct StatusResponse {
    pub mailbox: Option<MailboxHash>,
    pub messages: Option<ImapNum>,
    pub recent: Option<ImapNum>,
    pub uidnext: Option<UID>,
    pub uidvalidity: Option<UID>,
    pub unseen: Option<ImapNum>,
}

// status = "STATUS" SP mailbox SP "(" status-att *(SP status-att) ")"
// status-att = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" / "UNSEEN"
//* STATUS INBOX (MESSAGES 1057 UNSEEN 0)
pub fn status_response(input: &[u8]) -> IResult<&[u8], StatusResponse> {
    let (input, _) = tag("* STATUS ")(input)?;
    let (input, mailbox) = take_until(" (")(input)?;
    let mailbox = mailbox_token(mailbox)
        .map(|(_, m)| MailboxHash::from_bytes(m.as_bytes()))
        .ok();
    let (input, _) = tag(" (")(input)?;
    let (input, result) = permutation((
        opt(preceded(
            alt((tag("MESSAGES "), tag(" MESSAGES "))),
            map_res(digit1, |s| {
                ImapNum::from_str(unsafe { std::str::from_utf8_unchecked(s) })
            }),
        )),
        opt(preceded(
            alt((tag("RECENT "), tag(" RECENT "))),
            map_res(digit1, |s| {
                ImapNum::from_str(unsafe { std::str::from_utf8_unchecked(s) })
            }),
        )),
        opt(preceded(
            alt((tag("UIDNEXT "), tag(" UIDNEXT "))),
            map_res(digit1, |s| {
                UID::from_str(unsafe { std::str::from_utf8_unchecked(s) })
            }),
        )),
        opt(preceded(
            alt((tag("UIDVALIDITY "), tag(" UIDVALIDITY "))),
            map_res(digit1, |s| {
                UIDVALIDITY::from_str(unsafe { std::str::from_utf8_unchecked(s) })
            }),
        )),
        opt(preceded(
            alt((tag("UNSEEN "), tag(" UNSEEN "))),
            map_res(digit1, |s| {
                ImapNum::from_str(unsafe { std::str::from_utf8_unchecked(s) })
            }),
        )),
    ))(input)?;
    let (input, _) = tag(")\r\n")(input)?;
    Ok((
        input,
        StatusResponse {
            mailbox,
            messages: result.0,
            recent: result.1,
            uidnext: result.2,
            uidvalidity: result.3,
            unseen: result.4,
        },
    ))
}

// mailbox = "INBOX" / astring
//           ; INBOX is case-insensitive. All case variants of
//           ; INBOX (e.g., "iNbOx") MUST be interpreted as INBOX
//           ; not as an astring. An astring which consists of
//           ; the case-insensitive sequence "I" "N" "B" "O" "X"
//           ; is considered to be INBOX and not an astring.
//           ; Refer to section 5.1 for further
//           ; semantic details of mailbox names.
pub fn mailbox_token(input: &'_ [u8]) -> IResult<&'_ [u8], std::borrow::Cow<'_, str>> {
    let (input, astring) = astring_token(input)?;
    if astring.eq_ignore_ascii_case(b"INBOX") {
        return Ok((input, "INBOX".into()));
    }
    Ok((input, String::from_utf8_lossy(astring)))
}

// astring = 1*ASTRING-CHAR / string
pub fn astring_token(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((string_token, astring_char))(input)
}

// string = quoted / literal
pub fn string_token(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if let Ok((r, o)) = literal(input) {
        return Ok((r, o));
    }
    if input.is_empty() || input[0] != b'"' {
        return Err(nom::Err::Error((input, "string_token(): EOF").into()));
    }

    let mut i = 1;
    while i < input.len() {
        if input[i] == b'\"' && input[i - 1] != b'\\' {
            return Ok((&input[i + 1..], &input[1..i]));
        }
        i += 1;
    }

    Err(nom::Err::Error(
        (input, "string_token(): not a quoted phrase").into(),
    ))
}

// ASTRING-CHAR = ATOM-CHAR / resp-specials
// atom = 1*ATOM-CHAR
// ATOM-CHAR = <any CHAR except atom-specials>
// atom-specials = "(" / ")" / "{" / SP / CTL / list-wildcards / quoted-specials
// / resp-specials
fn astring_char(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rest, chars) = many1(atom_char)(input)?;
    Ok((rest, &input[0..chars.len()]))
}

fn atom_char(mut input: &[u8]) -> IResult<&[u8], u8> {
    if input.is_empty() {
        return Err(nom::Err::Error(
            (input, "astring_char_tokens(): EOF").into(),
        ));
    }
    if atom_specials(input).is_ok() {
        return Err(nom::Err::Error(
            (input, "astring_char_tokens(): invalid input").into(),
        ));
    }
    let ret = input[0];
    input = &input[1..];
    Ok((input, ret))
}

#[inline(always)]
fn atom_specials(input: &[u8]) -> IResult<&[u8], u8> {
    alt((
        raw_chars,
        ctl,
        list_wildcards,
        quoted_specials,
        resp_specials,
    ))(input)
}

#[inline(always)]
fn raw_chars(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(b"(){ ")(input)
}

#[inline(always)]
fn list_wildcards(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(b"%*")(input)
}

#[inline(always)]
fn quoted_specials(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(b"\"\\")(input)
}

#[inline(always)]
fn resp_specials(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(b"]")(input)
}

#[inline(always)]
fn ctl(input: &[u8]) -> IResult<&[u8], u8> {
    //U+0000—U+001F (C0 controls), U+007F (delete), and U+0080—U+009F (C1 controls
    alt((
        byte_in_range(0, 0x1f),
        byte_in_range(0x7f, 0x7f),
        byte_in_range(0x80, 0x9f),
    ))(input)
}

pub fn generate_envelope_hash(mailbox_path: &str, uid: &UID) -> EnvelopeHash {
    let mut h = DefaultHasher::new();
    h.write_usize(*uid);
    h.write(mailbox_path.as_bytes());
    EnvelopeHash(h.finish())
}
