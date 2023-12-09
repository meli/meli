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
    utils::parsec::CRLF,
};

const UNTAGGED_PREFIX: &[u8] = b"* ";

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct RequiredResponses: u64 {
        const CAPABILITY          = 0b0000_0000_0000_0001;
        const BYE                 = 0b0000_0000_0000_0010;
        const FLAGS               = 0b0000_0000_0000_0100;
        const EXISTS              = 0b0000_0000_0000_1000;
        const RECENT              = 0b0000_0000_0001_0000;
        const UNSEEN              = 0b0000_0000_0010_0000;
        const PERMANENTFLAGS      = 0b0000_0000_0100_0000;
        const UIDNEXT             = 0b0000_0000_1000_0000;
        const UIDVALIDITY         = 0b0000_0001_0000_0000;
        const LIST                = 0b0000_0010_0000_0000;
        const LSUB                = 0b0000_0100_0000_0000;
        const STATUS              = 0b0000_1000_0000_0000;
        const EXPUNGE             = 0b0001_0000_0000_0000;
        const SEARCH              = 0b0010_0000_0000_0000;
        const FETCH               = 0b0100_0000_0000_0000;
        const NO_REQUIRED         = 0b1000_0000_0000_0000;
        const CAPABILITY_REQUIRED = Self::CAPABILITY.bits();
        const LOGOUT_REQUIRED     = Self::BYE.bits();
        const SELECT_REQUIRED     = Self::FLAGS.bits() | Self::EXISTS.bits() | Self::RECENT.bits() | Self::UNSEEN.bits() | Self::PERMANENTFLAGS.bits() | Self::UIDNEXT.bits() | Self::UIDVALIDITY.bits();
        const EXAMINE_REQUIRED    = Self::FLAGS.bits() | Self::EXISTS.bits() | Self::RECENT.bits() | Self::UNSEEN.bits() | Self::PERMANENTFLAGS.bits() | Self::UIDNEXT.bits() | Self::UIDVALIDITY.bits();
        const LIST_REQUIRED       = Self::LIST.bits();
        const LSUB_REQUIRED       = Self::LSUB.bits();
        const FETCH_REQUIRED      = Self::FETCH.bits();
    }
}

impl RequiredResponses {
    pub fn check(&self, line: &[u8]) -> bool {
        if !line.starts_with(UNTAGGED_PREFIX) {
            return false;
        }
        let line = &line[UNTAGGED_PREFIX.len()..];
        let mut ret = false;
        if self.intersects(Self::CAPABILITY) {
            ret |= line.starts_with(b"CAPABILITY");
        }
        if self.intersects(Self::BYE) {
            ret |= line.starts_with(b"BYE");
        }
        if self.intersects(Self::FLAGS) {
            ret |= line.starts_with(b"FLAGS");
        }
        if self.intersects(Self::EXISTS) {
            ret |= line.ends_with(b"EXISTS\r\n");
        }
        if self.intersects(Self::RECENT) {
            ret |= line.ends_with(b"RECENT\r\n");
        }
        if self.intersects(Self::UNSEEN) {
            ret |= line.starts_with(b"UNSEEN");
        }
        if self.intersects(Self::PERMANENTFLAGS) {
            ret |= line.starts_with(b"PERMANENTFLAGS");
        }
        if self.intersects(Self::UIDNEXT) {
            ret |= line.starts_with(b"UIDNEXT");
        }
        if self.intersects(Self::UIDVALIDITY) {
            ret |= line.starts_with(b"UIDVALIDITY");
        }
        if self.intersects(Self::LIST) {
            ret |= line.starts_with(b"LIST");
        }
        if self.intersects(Self::LSUB) {
            ret |= line.starts_with(b"LSUB");
        }
        if self.intersects(Self::STATUS) {
            ret |= line.starts_with(b"STATUS");
        }
        if self.intersects(Self::EXPUNGE) {
            ret |= line.ends_with(b"EXPUNGE\r\n");
        }
        if self.intersects(Self::SEARCH) {
            ret |= line.starts_with(b"SEARCH");
        }
        if self.intersects(Self::FETCH) {
            let mut ptr = 0;
            for (i, l) in line.iter().enumerate() {
                if !l.is_ascii_digit() {
                    ptr = i;
                    break;
                }
            }
            ret |= line[ptr..].trim_start().starts_with(b"FETCH");
        }
        ret
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
    fn try_from(val: &'_ [u8]) -> Result<Self> {
        let val: &[u8] = val.split_rn().last().unwrap_or(val);
        let mut val = val[val.find(b" ").ok_or_else(|| {
            Error::new(format!(
                "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                val
            ))
        })? + 1..]
            .trim();
        // M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters
        // (0.000 + 0.098 + 0.097 secs).\r\n
        if val.ends_with(b" secs).") {
            val = &val[..val.rfind(b"(").ok_or_else(|| {
                Error::new(format!(
                    "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                    val
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
                val
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
                /* Skip literal continuation line */
                if cur_slice.get(pos.saturating_sub(1)) == Some(&b'}') {
                    i += pos + 2;
                    continue;
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
                f.parent = Some(MailboxHash::from_bytes(f.imap_path[..pos].as_bytes()));
                f.imap_path[pos + 1..].to_string()
            } else {
                f.imap_path.clone()
            };
            f.separator = separator;

            debug!(f)
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
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    String::from_utf8_lossy(input)
                ))));
            }
        } else if input[i..].starts_with(b"FLAGS (") {
            i += b"FLAGS (".len();
            if let Ok((rest, flags)) = flags(&input[i..]) {
                ret.flags = Some(flags);
                i += (input.len() - i - rest.len()) + 1;
            } else {
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse FLAGS: \
                     {:.40}.",
                    String::from_utf8_lossy(&input[i..])
                ))));
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
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing MODSEQ in UID FETCH response. Got: `{:.40}`",
                    String::from_utf8_lossy(input)
                ))));
            }
        } else if input[i..].starts_with(b"RFC822 {") {
            i += b"RFC822 ".len();
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
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse RFC822: \
                     {:.40}",
                    String::from_utf8_lossy(&input[i..])
                ))));
            }
        } else if input[i..].starts_with(b"ENVELOPE (") {
            i += b"ENVELOPE ".len();
            if let Ok((rest, envelope)) = envelope(&input[i..]) {
                ret.envelope = Some(envelope);
                i += input.len() - i - rest.len();
            } else {
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse ENVELOPE: \
                     {:.40}",
                    String::from_utf8_lossy(&input[i..])
                ))));
            }
        } else if input[i..].starts_with(b"BODYSTRUCTURE ") {
            i += b"BODYSTRUCTURE ".len();

            let (rest, _has_attachments) = bodystructure_has_attachments(&input[i..])?;
            has_attachments = _has_attachments;
            i += input[i..].len() - rest.len();
        } else if input[i..].starts_with(b"BODY[HEADER.FIELDS (REFERENCES)] ") {
            i += b"BODY[HEADER.FIELDS (REFERENCES)] ".len();
            if let Ok((rest, mut references)) = astring_token(&input[i..]) {
                if !references.trim().is_empty() {
                    if let Ok((_, (_, v))) = crate::email::parser::headers::header(references) {
                        references = v;
                    }
                    ret.references = Some(references);
                }
                i += input.len() - i - rest.len();
            } else {
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     BODY[HEADER.FIELDS (REFERENCES)]: {:.40}",
                    String::from_utf8_lossy(&input[i..])
                ))));
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
                return debug!(Err(Error::new(format!(
                    "Unexpected input while parsing UID FETCH response. Could not parse \
                     BODY[HEADER.FIELDS (\"REFERENCES\"): {:.40}",
                    String::from_utf8_lossy(&input[i..])
                ))));
            }
        } else if input[i..].starts_with(b")\r\n") {
            i += b")\r\n".len();
            break;
        } else {
            debug!(
                "Got unexpected token while parsing UID FETCH response:\n`{}`\n",
                String::from_utf8_lossy(input)
            );
            return debug!(Err(Error::new(format!(
                "Got unexpected token while parsing UID FETCH response: `{:.40}`",
                String::from_utf8_lossy(&input[i..])
            ))));
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

    if !input.is_empty() && ret.is_empty() {
        if let Ok(ImapResponse::Ok(_)) = ImapResponse::try_from(input) {
        } else {
            return Err(Error::new(format!(
                "310Unexpected input while parsing UID FETCH responses: `{:.40}`",
                String::from_utf8_lossy(input)
            )));
        }
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
    Fetch(FetchResponse<'s>),
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
    debug!(
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
                _ if _tag.starts_with(b"FETCH ") => Some(Fetch(fetch_response(orig_input)?.1)),
                _ => {
                    debug!(
                        "unknown untagged_response: {}",
                        String::from_utf8_lossy(_tag)
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
    /// if SELECT returns \* we can set arbritary flags permanently.
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
                debug!("select response: {}", String::from_utf8_lossy(l));
            }
        }
        Ok(ret)
    } else {
        let ret = String::from_utf8_lossy(input).to_string();
        debug!("BAD/NO response in select: {}", &ret);
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
                env.set_bcc(bcc.to_vec());
            }
            if let Some(in_reply_to) = in_reply_to {
                env.set_in_reply_to(&in_reply_to);
                if let Some(in_reply_to) = env.in_reply_to().cloned() {
                    env.push_references(in_reply_to);
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
    let (input, name) = alt((quoted, map(tag("NIL"), |_| Vec::new())))(input)?;
    let (input, _) = is_a(WS)(input)?;
    let (input, _) = alt((quoted, map(tag("NIL"), |_| Vec::new())))(input)?;
    let (input, _) = is_a(WS)(input)?;
    let (input, mailbox_name) = alt((quoted, map(tag("NIL"), |_| Vec::new())))(input)?;
    let (input, host_name) = opt(preceded(
        is_a(WS),
        alt((quoted, map(tag("NIL"), |_| Vec::new()))),
    ))(input)?;
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
            Ok((_, out)) => Ok((r, out)),
            e => e,
        };
    }
    if input.is_empty() || input[0] != b'"' {
        return Err(nom::Err::Error((input, "quoted(): EOF").into()));
    }

    let mut i = 1;
    while i < input.len() {
        if input[i] == b'\"' && input[i - 1] != b'\\' {
            return match crate::email::parser::encodings::phrase(&input[1..i], false) {
                Ok((_, out)) => Ok((&input[i + 1..], out)),
                e => e,
            };
        }
        i += 1;
    }

    Err(nom::Err::Error(
        (input, "quoted(): not a quoted phrase").into(),
    ))
}

pub fn quoted_or_nil(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    alt((map(tag("NIL"), |_| None), map(quoted, Some)))(input.ltrim())
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
    byte_in_slice(&[b'(', b')', b'{', b' '])(input)
}

#[inline(always)]
fn list_wildcards(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(&[b'%', b'*'])(input)
}

#[inline(always)]
fn quoted_specials(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(&[b'"', b'\\'])(input)
}

#[inline(always)]
fn resp_specials(input: &[u8]) -> IResult<&[u8], u8> {
    byte_in_slice(&[b']'])(input)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_imap_response() {
        assert_eq!(ImapResponse::try_from(&b"M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters (0.000 + 0.098 + 0.097 secs).\r\n"[..]).unwrap(),
        ImapResponse::No(ResponseCode::Alert("Invalid mailbox name: Name must not have '/' characters".to_string())));

        assert_eq!(
            ImapResponse::try_from(&b"M13 OK [UIDNEXT 4392] Predicted next UID\r\n"[..]).unwrap(),
            ImapResponse::Ok(ResponseCode::Uidnext(4392))
        );

        assert_eq!(
            ImapResponse::try_from(&b"M14 OK [UIDVALIDITY 3857529045] UIDs valid\r\n"[..]).unwrap(),
            ImapResponse::Ok(ResponseCode::Uidvalidity(3857529045))
        );
    }

    #[test]
    fn test_imap_required_responses() {
        let mut ret = Vec::new();
        let required_responses = RequiredResponses::FETCH_REQUIRED;
        let response =
            &b"* 1040 FETCH (UID 1064 FLAGS ())\r\nM15 OK Fetch completed (0.001 + 0.299 secs).\r\n"[..];
        for l in response.split_rn() {
            /* debug!("check line: {}", &l); */
            if required_responses.check(l) {
                ret.extend_from_slice(l);
            }
        }
        assert_eq!(ret.as_slice(), &b"* 1040 FETCH (UID 1064 FLAGS ())\r\n"[..]);
        let v = protocol_parser::uid_fetch_flags_responses(response)
            .unwrap()
            .1;
        assert_eq!(v.len(), 1);
    }

    #[test]
    fn test_imap_line_iterator() {
        {
            let s = b"* 1429 FETCH (UID 1505 FLAGS (\\Seen) RFC822 {26}\r\nReturn-Path: <blah blah...\r\n* 1430 FETCH (UID 1506 FLAGS (\\Seen)\r\n* 1431 FETCH (UID 1507 FLAGS (\\Seen)\r\n* 1432 FETCH (UID 1500 FLAGS (\\Seen) RFC822 {4}\r\nnull\r\n";
            let line_a =
                b"* 1429 FETCH (UID 1505 FLAGS (\\Seen) RFC822 {26}\r\nReturn-Path: <blah blah...\r\n";
            let line_b = b"* 1430 FETCH (UID 1506 FLAGS (\\Seen)\r\n";
            let line_c = b"* 1431 FETCH (UID 1507 FLAGS (\\Seen)\r\n";
            let line_d = b"* 1432 FETCH (UID 1500 FLAGS (\\Seen) RFC822 {4}\r\nnull\r\n";

            let mut iter = s.split_rn();

            assert_eq!(to_str!(iter.next().unwrap()), to_str!(line_a));
            assert_eq!(to_str!(iter.next().unwrap()), to_str!(line_b));
            assert_eq!(to_str!(iter.next().unwrap()), to_str!(line_c));
            assert_eq!(to_str!(iter.next().unwrap()), to_str!(line_d));
            assert!(iter.next().is_none());
        }

        {
            let s = b"* 23 FETCH (FLAGS (\\Seen) RFC822.SIZE 44827)\r\n";
            let mut iter = s.split_rn();
            assert_eq!(to_str!(iter.next().unwrap()), to_str!(s));
            assert!(iter.next().is_none());
        }

        {
            let s = b"";
            let mut iter = s.split_rn();
            assert!(iter.next().is_none());
        }
        {
            let s = b"* 172 EXISTS\r\n* 1 RECENT\r\n* OK [UNSEEN 12] Message 12 is first unseen\r\n* OK [UIDVALIDITY 3857529045] UIDs valid\r\n* OK [UIDNEXT 4392] Predicted next UID\r\n* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n* OK [NOMODSEQ] Sorry, this mailbox format doesn't support modsequences\r\n* A142 OK [READ-WRITE] SELECT completed\r\n";
            let mut iter = s.split_rn();
            for l in &[
                &b"* 172 EXISTS\r\n"[..],
                &b"* 1 RECENT\r\n"[..],
                &b"* OK [UNSEEN 12] Message 12 is first unseen\r\n"[..],
                &b"* OK [UIDVALIDITY 3857529045] UIDs valid\r\n"[..],
                &b"* OK [UIDNEXT 4392] Predicted next UID\r\n"[..],
                &b"* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n"[..],
                &b"* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n"[..],
                &b"* OK [NOMODSEQ] Sorry, this mailbox format doesn't support modsequences\r\n"[..],
                &b"* A142 OK [READ-WRITE] SELECT completed\r\n"[..],
            ] {
                assert_eq!(to_str!(iter.next().unwrap()), to_str!(l));
            }
            assert!(iter.next().is_none());
        }
    }

    #[test]
    fn test_imap_untagged_responses() {
        use UntaggedResponse::*;
        assert_eq!(
            untagged_responses(b"* 2 EXISTS\r\n")
                .map(|(_, v, _)| v)
                .unwrap()
                .unwrap(),
            Exists(2)
        );
        assert_eq!(
            untagged_responses(b"* 1079 FETCH (UID 1103 MODSEQ (1365) FLAGS (\\Seen))\r\n")
                .map(|(_, v, _)| v)
                .unwrap()
                .unwrap(),
            Fetch(FetchResponse {
                uid: Some(1103),
                message_sequence_number: 1079,
                modseq: Some(ModSequence(std::num::NonZeroU64::new(1365_u64).unwrap())),
                flags: Some((Flag::SEEN, vec![])),
                body: None,
                references: None,
                envelope: None,
                raw_fetch_value: &b"* 1079 FETCH (UID 1103 MODSEQ (1365) FLAGS (\\Seen))\r\n"[..],
            })
        );
        assert_eq!(
            untagged_responses(b"* 1 FETCH (FLAGS (\\Seen))\r\n")
                .map(|(_, v, _)| v)
                .unwrap()
                .unwrap(),
            Fetch(FetchResponse {
                uid: None,
                message_sequence_number: 1,
                modseq: None,
                flags: Some((Flag::SEEN, vec![])),
                body: None,
                references: None,
                envelope: None,
                raw_fetch_value: &b"* 1 FETCH (FLAGS (\\Seen))\r\n"[..],
            })
        );
    }

    #[test]
    fn test_imap_fetch_response() {
        #[rustfmt::skip]
        let input: &[u8] = b"* 198 FETCH (UID 7608 FLAGS (\\Seen) ENVELOPE (\"Fri, 24 Jun 2011 10:09:10 +0000\" \"xxxx/xxxx\" ((\"xx@xx.com\" NIL \"xx\" \"xx.com\")) NIL NIL ((\"xx@xx\" NIL \"xx\" \"xx.com\")) ((\"'xx, xx'\" NIL \"xx.xx\" \"xx.com\")(\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")(\"'xx'\" NIL \"xx.xx\" \"xx.com\")(\"'xx xx'\" NIL \"xx.xx\" \"xx.com\")(\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")) NIL NIL \"<xx@xx.com>\") BODY[HEADER.FIELDS (REFERENCES)] {2}\r\n\r\n BODYSTRUCTURE ((\"text\" \"html\" (\"charset\" \"us-ascii\") \"<xx@xx>\" NIL \"7BIT\" 17236 232 NIL NIL NIL NIL)(\"image\" \"jpeg\" (\"name\" \"image001.jpg\") \"<image001.jpg@xx.xx>\" \"image001.jpg\" \"base64\" 1918 NIL (\"inline\" (\"filename\" \"image001.jpg\" \"size\" \"1650\" \"creation-date\" \"Sun, 09 Aug 2015 20:56:04 GMT\" \"modification-date\" \"Sun, 14 Aug 2022 22:11:45 GMT\")) NIL NIL) \"related\" (\"boundary\" \"xx--xx\" \"type\" \"text/html\") NIL \"en-US\"))\r\n";
        #[rustfmt::skip]
        //                                                                  ----------------------------------- ------------- --------------------------------------- --- --- ----------------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- --- --- ---------------
        //                                                                  date                                subject       from                                    |   |   to                                  cc                                                                                                                                                                                                       bcc irt message-id
        //                                                                                                                                                            |   reply-to
        //                                                                                                                                                            sender

        let mut address = SmallVec::new();
        address.push(Address::new(None, "xx@xx.com".to_string()));
        let mut env = Envelope::new(EnvelopeHash::default());
        env.set_subject(b"xxxx/xxxx".to_vec());
        env.set_date(b"Fri, 24 Jun 2011 10:09:10 +0000");
        env.set_from(address.clone());
        env.set_to(address);
        env.set_message_id(b"<xx@xx.com>");
        assert_eq!(
            fetch_response(input).unwrap(),
            (
                &b""[..],
                FetchResponse {
                    uid: Some(7608),
                    message_sequence_number: 198,
                    flags: Some((Flag::SEEN, vec![])),
                    modseq: None,
                    body: None,
                    references: None,
                    envelope: Some(env),
                    raw_fetch_value: input,
                },
                None
            )
        );
    }

    #[test]
    fn test_imap_search() {
        assert_eq!(search_results(b"* SEARCH\r\n").map(|(_, v)| v), Ok(vec![]));
        assert_eq!(
            search_results(b"* SEARCH 1\r\n").map(|(_, v)| v),
            Ok(vec![1])
        );
        assert_eq!(
            search_results(b"* SEARCH 1 2 3 4\r\n").map(|(_, v)| v),
            Ok(vec![1, 2, 3, 4])
        );
        assert_eq!(
            search_results_raw(b"* SEARCH 1 2 3 4\r\n").map(|(_, v)| v),
            Ok(&b"1 2 3 4"[..])
        );
    }

    #[test]
    fn test_imap_select_response() {
        let r = b"* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*)] Flags permitted.\r\n* 45 EXISTS\r\n* 0 RECENT\r\n* OK [UNSEEN 16] First unseen.\r\n* OK [UIDVALIDITY 1554422056] UIDs valid\r\n* OK [UIDNEXT 50] Predicted next UID\r\n";

        assert_eq!(
            select_response(r).expect("Could not parse IMAP select response"),
            SelectResponse {
                exists: 45,
                recent: 0,
                flags: (
                    Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                    Vec::new()
                ),
                first_unseen: 16,
                uidvalidity: 1554422056,
                uidnext: 50,
                permanentflags: (
                    Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                    vec!["*".into()]
                ),
                can_create_flags: true,
                read_only: false,
                highestmodseq: None
            }
        );
        let r = b"* 172 EXISTS\r\n* 1 RECENT\r\n* OK [UNSEEN 12] Message 12 is first unseen\r\n* OK [UIDVALIDITY 3857529045] UIDs valid\r\n* OK [UIDNEXT 4392] Predicted next UID\r\n* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n* OK [HIGHESTMODSEQ 715194045007]\r\n* A142 OK [READ-WRITE] SELECT completed\r\n";

        assert_eq!(
            select_response(r).expect("Could not parse IMAP select response"),
            SelectResponse {
                exists: 172,
                recent: 1,
                flags: (
                    Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                    Vec::new()
                ),
                first_unseen: 12,
                uidvalidity: 3857529045,
                uidnext: 4392,
                permanentflags: (Flag::SEEN | Flag::TRASHED, vec!["*".into()]),
                can_create_flags: true,
                read_only: false,
                highestmodseq: Some(Ok(ModSequence(
                    std::num::NonZeroU64::new(715194045007_u64).unwrap()
                ))),
            }
        );
        let r = b"* 172 EXISTS\r\n* 1 RECENT\r\n* OK [UNSEEN 12] Message 12 is first unseen\r\n* OK [UIDVALIDITY 3857529045] UIDs valid\r\n* OK [UIDNEXT 4392] Predicted next UID\r\n* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n* OK [NOMODSEQ] Sorry, this mailbox format doesn't support modsequences\r\n* A142 OK [READ-WRITE] SELECT completed\r\n";

        assert_eq!(
            select_response(r).expect("Could not parse IMAP select response"),
            SelectResponse {
                exists: 172,
                recent: 1,
                flags: (
                    Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                    Vec::new()
                ),
                first_unseen: 12,
                uidvalidity: 3857529045,
                uidnext: 4392,
                permanentflags: (Flag::SEEN | Flag::TRASHED, vec!["*".into()]),
                can_create_flags: true,
                read_only: false,
                highestmodseq: Some(Err(())),
            }
        );
    }

    #[test]
    fn test_imap_envelope() {
        let input: &[u8] = b"(\"Fri, 24 Jun 2011 10:09:10 +0000\" \"xxxx/xxxx\" ((\"xx@xx.com\" NIL \"xx\" \"xx.com\")) NIL NIL ((\"xx@xx\" NIL \"xx\" \"xx.com\")) ((\"'xx, xx'\" NIL \"xx.xx\" \"xx.com\") (\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\") (\"'xx'\" NIL \"xx.xx\" \"xx.com\") (\"'xx xx'\" NIL \"xx.xx\" \"xx.com\") (\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")) NIL NIL \"<xx@xx.com>\")";
        _ = envelope(input).unwrap();
    }
}
