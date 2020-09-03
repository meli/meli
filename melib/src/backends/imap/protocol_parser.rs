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
use crate::email::parser::{BytesExt, IResult};
use crate::error::ResultIntoMeliError;
use crate::get_path_hash;
use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_a, is_not, tag, take, take_until, take_while},
    character::complete::digit1,
    character::is_digit,
    combinator::{map, map_res, opt},
    multi::{fold_many1, length_data, many0, separated_nonempty_list},
    sequence::{delimited, preceded},
};
use std::convert::TryFrom;
use std::str::FromStr;

bitflags! {
    #[derive(Default, Serialize, Deserialize)]
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
        const CAPABILITY_REQUIRED = Self::CAPABILITY.bits;
        const LOGOUT_REQUIRED     = Self::BYE.bits;
        const SELECT_REQUIRED     = Self::FLAGS.bits | Self::EXISTS.bits | Self::RECENT.bits | Self::UNSEEN.bits | Self::PERMANENTFLAGS.bits | Self::UIDNEXT.bits | Self::UIDVALIDITY.bits;
        const EXAMINE_REQUIRED    = Self::FLAGS.bits | Self::EXISTS.bits | Self::RECENT.bits | Self::UNSEEN.bits | Self::PERMANENTFLAGS.bits | Self::UIDNEXT.bits | Self::UIDVALIDITY.bits;
        const LIST_REQUIRED       = Self::LIST.bits;
        const LSUB_REQUIRED       = Self::LSUB.bits;
        const FETCH_REQUIRED      = Self::FETCH.bits;
    }
}

impl RequiredResponses {
    pub fn check(&self, line: &str) -> bool {
        if !line.starts_with("* ") {
            return false;
        }
        let line = &line["* ".len()..];
        let mut ret = false;
        if self.intersects(RequiredResponses::CAPABILITY) {
            ret |= line.starts_with("CAPABILITY");
        }
        if self.intersects(RequiredResponses::BYE) {
            ret |= line.starts_with("BYE");
        }
        if self.intersects(RequiredResponses::FLAGS) {
            ret |= line.starts_with("FLAGS");
        }
        if self.intersects(RequiredResponses::EXISTS) {
            ret |= line.ends_with("EXISTS\r\n");
        }
        if self.intersects(RequiredResponses::RECENT) {
            ret |= line.ends_with("RECENT\r\n");
        }
        if self.intersects(RequiredResponses::UNSEEN) {
            ret |= line.starts_with("UNSEEN");
        }
        if self.intersects(RequiredResponses::PERMANENTFLAGS) {
            ret |= line.starts_with("PERMANENTFLAGS");
        }
        if self.intersects(RequiredResponses::UIDNEXT) {
            ret |= line.starts_with("UIDNEXT");
        }
        if self.intersects(RequiredResponses::UIDVALIDITY) {
            ret |= line.starts_with("UIDVALIDITY");
        }
        if self.intersects(RequiredResponses::LIST) {
            ret |= line.starts_with("LIST");
        }
        if self.intersects(RequiredResponses::LSUB) {
            ret |= line.starts_with("LSUB");
        }
        if self.intersects(RequiredResponses::STATUS) {
            ret |= line.starts_with("STATUS");
        }
        if self.intersects(RequiredResponses::EXPUNGE) {
            ret |= line.ends_with("EXPUNGE\r\n");
        }
        if self.intersects(RequiredResponses::SEARCH) {
            ret |= line.starts_with("SEARCH");
        }
        if self.intersects(RequiredResponses::FETCH) {
            let mut ptr = 0;
            for i in 0..line.len() {
                if !line.as_bytes()[i].is_ascii_digit() {
                    ptr = i;
                    break;
                }
            }
            ret |= line[ptr..].trim_start().starts_with("FETCH");
        }
        ret
    }
}

#[test]
fn test_imap_required_responses() {
    let mut ret = String::new();
    let required_responses = RequiredResponses::FETCH_REQUIRED;
    let response =
        &"* 1040 FETCH (UID 1064 FLAGS ())\r\nM15 OK Fetch completed (0.001 + 0.299 secs).\r\n"
            [0..];
    for l in response.split_rn() {
        /*debug!("check line: {}", &l);*/
        if required_responses.check(l) {
            ret.push_str(l);
        }
    }
    assert_eq!(&ret, "* 1040 FETCH (UID 1064 FLAGS ())\r\n");
    let v = protocol_parser::uid_fetch_flags_responses(response.as_bytes())
        .unwrap()
        .1;
    assert_eq!(v.len(), 1);
}

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
    Badcharset(Option<String>),

    /// Followed by a list of capabilities.  This can appear in the initial OK or PREAUTH response to transmit an initial capabilities list.  This makes it unnecessary for a client to send a separate CAPABILITY command if it recognizes this response.
    Capability,

    /// The human-readable text represents an error in parsing the [RFC-2822] header or [MIME-IMB] headers of a message in the mailbox.
    Parse(String),

    /// Followed by a parenthesized list of flags, indicates which of the known flags the client can change permanently.  Any flags that are in the FLAGS untagged response, but not the PERMANENTFLAGS list, can not be set permanently.  If the client attempts to STORE a flag that is not in the PERMANENTFLAGS list, the server will either ignore the change or store the state change for the remainder of the current session only.  The PERMANENTFLAGS list can also include the special flag \*, which indicates that it is possible to create new keywords by attempting to store those flags in the mailbox.
    Permanentflags(String),

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
    Unseen(ImapNum),
}

impl std::fmt::Display for ResponseCode {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ResponseCode::*;
        match self {
            Alert(s)=> write!(fmt, "ALERT: {}", s),
            Badcharset(None)=> write!(fmt, "Given charset is not supported by this server."),
            Badcharset(Some(s))=> write!(fmt, "Given charset is not supported by this server. Supported ones are: {}", s),
            Capability => write!(fmt, "Capability response"),
            Parse(s) => write!(fmt, "Server error in parsing message headers: {}", s),
            Permanentflags(s) => write!(fmt, "Mailbox supports these flags: {}", s),
            ReadOnly=> write!(fmt, "This mailbox is selected read-only."),
            ReadWrite => write!(fmt, "This mailbox is selected with read-write permissions."),
            Trycreate => write!(fmt, "Failed to operate on the target mailbox because it doesn't exist. Try creating it first."),
            Uidnext(uid) => write!(fmt, "Next UID value is {}", uid),
            Uidvalidity(uid) => write!(fmt, "Next UIDVALIDITY value is {}", uid),
            Unseen(uid) => write!(fmt, "First message without the \\Seen flag is {}", uid),
        }
    }
}

impl ResponseCode {
    fn from(val: &str) -> ResponseCode {
        use ResponseCode::*;
        if !val.starts_with('[') {
            let msg = val.trim();
            return Alert(msg.to_string());
        }

        let val = &val[1..];
        if val.starts_with("BADCHARSET") {
            let charsets = val
                .as_bytes()
                .find(b"(")
                .map(|pos| val[pos + 1..].trim().to_string());
            Badcharset(charsets)
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

impl TryFrom<&'_ str> for ImapResponse {
    type Error = MeliError;
    fn try_from(val: &'_ str) -> Result<ImapResponse> {
        let val: &str = val.split_rn().last().unwrap_or(val.as_ref());
        debug!(&val);
        let mut val = val[val.as_bytes().find(b" ").ok_or_else(|| {
            MeliError::new(format!(
                "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                val
            ))
        })? + 1..]
            .trim();
        // M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters (0.000 + 0.098 + 0.097 secs).\r\n
        if val.ends_with(" secs).") {
            val = &val[..val.as_bytes().rfind(b"(").ok_or_else(|| {
                MeliError::new(format!(
                    "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                    val
                ))
            })?];
        }

        Ok(if val.starts_with("OK") {
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
            return Err(MeliError::new(format!(
                "Expected tagged IMAP response (OK,NO,BAD, etc) but found {:?}",
                val
            )));
        })
    }
}

impl Into<Result<()>> for ImapResponse {
    fn into(self) -> Result<()> {
        match self {
            Self::Ok(_) | Self::Preauth(_) | Self::Bye(_) => Ok(()),
            Self::No(ResponseCode::Alert(msg)) | Self::Bad(ResponseCode::Alert(msg)) => {
                Err(MeliError::new(msg))
            }
            Self::No(err) => Err(MeliError::new(format!("{:?}", err)))
                .chain_err_summary(|| "IMAP NO Response.".to_string()),
            Self::Bad(err) => Err(MeliError::new(format!("{:?}", err)))
                .chain_err_summary(|| "IMAP BAD Response.".to_string()),
        }
    }
}

#[test]
fn test_imap_response() {
    assert_eq!(ImapResponse::try_from("M12 NO [CANNOT] Invalid mailbox name: Name must not have \'/\' characters (0.000 + 0.098 + 0.097 secs).\r\n").unwrap(), ImapResponse::No(ResponseCode::Alert("Invalid mailbox name: Name must not have '/' characters".to_string())));
}

impl<'a> std::iter::DoubleEndedIterator for ImapLineIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.slice.is_empty() {
            None
        } else if let Some(pos) = self.slice.rfind("\r\n") {
            if self.slice[..pos].is_empty() {
                self.slice = &self.slice[..pos];
                None
            } else if let Some(prev_pos) = self.slice[..pos].rfind("\r\n") {
                let ret = &self.slice[prev_pos + 2..pos + 2];
                self.slice = &self.slice[..prev_pos + 2];
                Some(ret)
            } else {
                let ret = self.slice;
                self.slice = &self.slice[ret.len()..];
                Some(ret)
            }
        } else {
            let ret = self.slice;
            self.slice = &self.slice[ret.len()..];
            Some(ret)
        }
    }
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
    let (input, _) = tag("\r\n")(input)?;
    Ok((
        input,
        ({
            let separator: u8 = separator[0];
            let mut f = ImapMailbox::default();
            f.no_select = false;
            f.is_subscribed = false;
            for p in properties.split(|&b| b == b' ') {
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
            f.is_subscribed = path == "INBOX";
            f.imap_path = path.into();
            f.hash = get_path_hash!(&f.imap_path);
            f.path = if separator == b'/' {
                f.imap_path.clone()
            } else {
                f.imap_path.replace(separator as char, "/")
            };
            f.name = if let Some(pos) = f.imap_path.as_bytes().iter().rposition(|&c| c == separator)
            {
                f.parent = Some(get_path_hash!(&f.imap_path[..pos]));
                f.imap_path[pos + 1..].to_string()
            } else {
                f.imap_path.clone()
            };
            f.separator = separator;

            debug!(f)
        }),
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub struct FetchResponse<'a> {
    pub uid: Option<UID>,
    pub message_sequence_number: MessageSequenceNumber,
    pub modseq: Option<ModSequence>,
    pub flags: Option<(Flag, Vec<String>)>,
    pub body: Option<&'a [u8]>,
    pub envelope: Option<Envelope>,
}

pub fn fetch_response(input: &str) -> ImapParseResult<FetchResponse<'_>> {
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

    let mut ret = FetchResponse {
        uid: None,
        message_sequence_number: 0,
        modseq: None,
        flags: None,
        body: None,
        envelope: None,
    };

    while input.as_bytes()[i].is_ascii_digit() {
        let b: u8 = input.as_bytes()[i] - 0x30;
        ret.message_sequence_number *= 10;
        ret.message_sequence_number += b as MessageSequenceNumber;
        i += 1;
        bounds!();
    }

    eat_whitespace!();
    should_start_with!(input[i..], "FETCH (");
    i += "FETCH (".len();
    let mut has_attachments = false;
    while i < input.len() {
        eat_whitespace!(break);
        bounds!(break);

        if input[i..].starts_with("UID ") {
            i += "UID ".len();
            if let Ok((rest, uid)) = take_while::<_, &[u8], (&[u8], nom::error::ErrorKind)>(
                is_digit,
            )(input[i..].as_bytes())
            {
                i += input.len() - i - rest.len();
                ret.uid =
                    Some(UID::from_str(unsafe { std::str::from_utf8_unchecked(uid) }).unwrap());
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    input
                ))));
            }
        } else if input[i..].starts_with("FLAGS (") {
            i += "FLAGS (".len();
            if let Ok((rest, flags)) = flags(&input[i..]) {
                ret.flags = Some(flags);
                i += (input.len() - i - rest.len()) + 1;
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH response. Got: `{:.40}`",
                    input
                ))));
            }
        } else if input[i..].starts_with("MODSEQ (") {
            i += "MODSEQ (".len();
            if let Ok((rest, modseq)) = take_while::<_, &[u8], (&[u8], nom::error::ErrorKind)>(
                is_digit,
            )(input[i..].as_bytes())
            {
                i += (input.len() - i - rest.len()) + 1;
                ret.modseq = u64::from_str(to_str!(modseq))
                    .ok()
                    .and_then(std::num::NonZeroU64::new)
                    .map(ModSequence);
            } else {
                return debug!(Err(MeliError::new(format!(
                    "Unexpected input while parsing MODSEQ in UID FETCH response. Got: `{:.40}`",
                    input
                ))));
            }
        } else if input[i..].starts_with("RFC822 {") {
            i += "RFC822 ".len();
            if let Ok((rest, body)) =
                length_data::<_, _, (&[u8], nom::error::ErrorKind), _>(delimited(
                    tag("{"),
                    map_res(digit1, |s| {
                        usize::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                    }),
                    tag("}\r\n"),
                ))(input[i..].as_bytes())
            {
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
            if let Ok((rest, envelope)) = envelope(input[i..].as_bytes()) {
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

pub fn fetch_responses(mut input: &str) -> ImapParseResult<Vec<FetchResponse<'_>>> {
    let mut ret = Vec::new();
    let mut alert: Option<Alert> = None;

    while input.starts_with("* ") {
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
                return Err(MeliError::new(format!(
                    "Unexpected input while parsing UID FETCH responses: `{:.40}`, {}",
                    input, err
                )));
            }
        }
    }

    if !input.is_empty() && ret.is_empty() {
        if let Ok(ImapResponse::Ok(_)) = ImapResponse::try_from(input) {
        } else {
            return Err(MeliError::new(format!(
                "310Unexpected input while parsing UID FETCH responses: `{:.40}`",
                input
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

pub fn capabilities(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
    let (input, _) = take_until("CAPABILITY ")(input)?;
    let (input, _) = tag("CAPABILITY ")(input)?;
    let (input, ret) = separated_nonempty_list(tag(" "), is_not(" ]\r\n"))(input)?;
    let (input, _) = take_until("\r\n")(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, ret))
}

/// This enum represents the server's untagged responses detailed in `7. Server Responses` of RFC 3501   INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1
#[derive(Debug, PartialEq)]
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

pub fn untagged_responses(input: &str) -> ImapParseResult<Option<UntaggedResponse<'_>>> {
    let orig_input = input;
    let (input, _) = tag::<_, &str, (&str, nom::error::ErrorKind)>("* ")(input)?;
    let (input, num) = map_res::<_, _, _, (&str, nom::error::ErrorKind), _, _, _>(digit1, |s| {
        ImapNum::from_str(s)
    })(input)?;
    let (input, _) = tag::<_, &str, (&str, nom::error::ErrorKind)>(" ")(input)?;
    let (input, _tag) = take_until::<_, &str, (&str, nom::error::ErrorKind)>("\r\n")(input)?;
    let (input, _) = tag::<_, &str, (&str, nom::error::ErrorKind)>("\r\n")(input)?;
    debug!("Parse untagged response from {:?}", orig_input);
    Ok((
        input,
        {
            use UntaggedResponse::*;
            match _tag {
                "EXPUNGE" => Some(Expunge(num)),
                "EXISTS" => Some(Exists(num)),
                "RECENT" => Some(Recent(num)),
                _ if _tag.starts_with("FETCH ") => Some(Fetch(fetch_response(orig_input)?.1)),
                _ => {
                    debug!("unknown untagged_response: {}", _tag);
                    None
                }
            }
        },
        None,
    ))
}

#[test]
fn test_untagged_responses() {
    use std::convert::TryInto;
    use UntaggedResponse::*;
    assert_eq!(
        untagged_responses("* 2 EXISTS\r\n")
            .map(|(_, v, _)| v)
            .unwrap()
            .unwrap(),
        Exists(2)
    );
    assert_eq!(
        untagged_responses("* 1079 FETCH (UID 1103 MODSEQ (1365) FLAGS (\\Seen))\r\n")
            .map(|(_, v, _)| v)
            .unwrap()
            .unwrap(),
        Fetch(FetchResponse {
            uid: Some(1103),
            message_sequence_number: 1079,
            modseq: Some(ModSequence(std::num::NonZeroU64::new(1365_u64).unwrap())),
            flags: Some((Flag::SEEN, vec![])),
            body: None,
            envelope: None
        })
    );
    assert_eq!(
        untagged_responses("* 1 FETCH (FLAGS (\\Seen))\r\n")
            .map(|(_, v, _)| v)
            .unwrap()
            .unwrap(),
        Fetch(FetchResponse {
            uid: None,
            message_sequence_number: 1,
            modseq: None,
            flags: Some((Flag::SEEN, vec![])),
            body: None,
            envelope: None
        })
    );
}

pub fn search_results<'a>(input: &'a [u8]) -> IResult<&'a [u8], Vec<ImapNum>> {
    alt((
        |input: &'a [u8]| -> IResult<&'a [u8], Vec<ImapNum>> {
            let (input, _) = tag("* SEARCH ")(input)?;
            let (input, list) = separated_nonempty_list(
                tag(b" "),
                map_res(is_not(" \r\n"), |s: &[u8]| {
                    ImapNum::from_str(unsafe { std::str::from_utf8_unchecked(s) })
                }),
            )(input)?;
            let (input, _) = tag("\r\n")(input)?;
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
            let (input, list) = take_until("\r\n")(input)?;
            let (input, _) = tag("\r\n")(input)?;
            Ok((input, list))
        },
        |input: &'a [u8]| -> IResult<&'a [u8], &'a [u8]> {
            let (input, _) = tag("* SEARCH\r\n")(input)?;
            Ok((input, &b""[0..]))
        },
    ))(input)
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

#[derive(Debug, Default, PartialEq, Clone)]
pub struct SelectResponse {
    pub exists: ImapNum,
    pub recent: ImapNum,
    pub flags: (Flag, Vec<String>),
    pub unseen: MessageSequenceNumber,
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
        for l in input.split_rn() {
            if l.starts_with("* ") && l.ends_with(" EXISTS\r\n") {
                ret.exists = ImapNum::from_str(&l["* ".len()..l.len() - " EXISTS\r\n".len()])?;
            } else if l.starts_with("* ") && l.ends_with(" RECENT\r\n") {
                ret.recent = ImapNum::from_str(&l["* ".len()..l.len() - " RECENT\r\n".len()])?;
            } else if l.starts_with("* FLAGS (") {
                ret.flags = flags(&l["* FLAGS (".len()..l.len() - ")".len()]).map(|(_, v)| v)?;
            } else if l.starts_with("* OK [UNSEEN ") {
                ret.unseen = MessageSequenceNumber::from_str(
                    &l["* OK [UNSEEN ".len()..l.find(']').unwrap()],
                )?;
            } else if l.starts_with("* OK [UIDVALIDITY ") {
                ret.uidvalidity =
                    UIDVALIDITY::from_str(&l["* OK [UIDVALIDITY ".len()..l.find(']').unwrap()])?;
            } else if l.starts_with("* OK [UIDNEXT ") {
                ret.uidnext = UID::from_str(&l["* OK [UIDNEXT ".len()..l.find(']').unwrap()])?;
            } else if l.starts_with("* OK [PERMANENTFLAGS (") {
                ret.permanentflags =
                    flags(&l["* OK [PERMANENTFLAGS (".len()..l.find(')').unwrap()])
                        .map(|(_, v)| v)?;
                ret.can_create_flags = l.contains("\\*");
            } else if l.contains("OK [READ-WRITE]") {
                ret.read_only = false;
            } else if l.contains("OK [READ-ONLY]") {
                ret.read_only = true;
            } else if l.starts_with("* OK [HIGHESTMODSEQ ") {
                let res: IResult<&str, &str> = take_until("]")(&l["* OK [HIGHESTMODSEQ ".len()..]);
                let (_, highestmodseq) = res?;
                ret.highestmodseq = Some(
                    std::num::NonZeroU64::new(u64::from_str(&highestmodseq)?)
                        .map(|u| Ok(ModSequence(u)))
                        .unwrap_or(Err(())),
                );
            } else if l.starts_with("* OK [NOMODSEQ") {
                ret.highestmodseq = Some(Err(()));
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

#[test]
fn test_select_response() {
    use std::convert::TryInto;
    let r = "* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*)] Flags permitted.\r\n* 45 EXISTS\r\n* 0 RECENT\r\n* OK [UNSEEN 16] First unseen.\r\n* OK [UIDVALIDITY 1554422056] UIDs valid\r\n* OK [UIDNEXT 50] Predicted next UID\r\n";

    assert_eq!(
        select_response(r).expect("Could not parse IMAP select response"),
        SelectResponse {
            exists: 45,
            recent: 0,
            flags: (
                Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                Vec::new()
            ),
            unseen: 16,
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
    let r = "* 172 EXISTS\r\n* 1 RECENT\r\n* OK [UNSEEN 12] Message 12 is first unseen\r\n* OK [UIDVALIDITY 3857529045] UIDs valid\r\n* OK [UIDNEXT 4392] Predicted next UID\r\n* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n* OK [HIGHESTMODSEQ 715194045007]\r\n* A142 OK [READ-WRITE] SELECT completed\r\n";

    assert_eq!(
        select_response(r).expect("Could not parse IMAP select response"),
        SelectResponse {
            exists: 172,
            recent: 1,
            flags: (
                Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                Vec::new()
            ),
            unseen: 12,
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
    let r = "* 172 EXISTS\r\n* 1 RECENT\r\n* OK [UNSEEN 12] Message 12 is first unseen\r\n* OK [UIDVALIDITY 3857529045] UIDs valid\r\n* OK [UIDNEXT 4392] Predicted next UID\r\n* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n* OK [NOMODSEQ] Sorry, this mailbox format doesn't support modsequences\r\n* A142 OK [READ-WRITE] SELECT completed\r\n";

    assert_eq!(
        select_response(r).expect("Could not parse IMAP select response"),
        SelectResponse {
            exists: 172,
            recent: 1,
            flags: (
                Flag::REPLIED | Flag::SEEN | Flag::TRASHED | Flag::DRAFT | Flag::FLAGGED,
                Vec::new()
            ),
            unseen: 12,
            uidvalidity: 3857529045,
            uidnext: 4392,
            permanentflags: (Flag::SEEN | Flag::TRASHED, vec!["*".into()]),
            can_create_flags: true,
            read_only: false,
            highestmodseq: Some(Err(())),
        }
    );
}

pub fn flags(input: &str) -> IResult<&str, (Flag, Vec<String>)> {
    let mut ret = Flag::default();
    let mut keywords = Vec::new();

    let mut input = input;
    while !input.starts_with(')') && !input.is_empty() {
        if input.starts_with('\\') {
            input = &input[1..];
        }
        let mut match_end = 0;
        while match_end < input.len() {
            if input[match_end..].starts_with(' ') || input[match_end..].starts_with(')') {
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
    Ok((input, (ret, keywords)))
}

pub fn byte_flags(input: &[u8]) -> IResult<&[u8], (Flag, Vec<String>)> {
    let i = unsafe { std::str::from_utf8_unchecked(input) };
    match flags(i) {
        Ok((rest, ret)) => Ok((rest.as_bytes(), ret)),
        Err(nom::Err::Error(err)) => Err(nom::Err::Error(err.as_bytes())),
        Err(nom::Err::Failure(err)) => Err(nom::Err::Error(err.as_bytes())),
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
    let (input, _) = tag("(")(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, date) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, subject) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, from) = envelope_addresses(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, _sender) = envelope_addresses(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, _reply_to) = envelope_addresses(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, to) = envelope_addresses(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, cc) = envelope_addresses(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, bcc) = envelope_addresses(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, in_reply_to) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, message_id) = quoted_or_nil(input)?;
    let (input, _) = opt(is_a("\r\n\t "))(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((
        input,
        ({
            let mut env = Envelope::new(0);
            if let Some(date) = date {
                env.set_date(&date);
                if let Ok(d) = crate::email::parser::generic::date(env.date_as_str().as_bytes()) {
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
                env.push_references(env.in_reply_to().unwrap().clone());
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
                delimited(tag("("), envelope_address, tag(")")),
                SmallVec::new(),
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
    let (input, name) = alt((quoted, map(tag("NIL"), |_| Vec::new())))(input)?;
    let (input, _) = is_a("\r\n\t ")(input)?;
    let (input, _) = alt((quoted, map(tag("NIL"), |_| Vec::new())))(input)?;
    let (input, _) = is_a("\r\n\t ")(input)?;
    let (input, mailbox_name) = alt((quoted, map(tag("NIL"), |_| Vec::new())))(input)?;
    let (input, host_name) = opt(preceded(
        is_a("\r\n\t "),
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
                    to_str!(&host_name)
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

// Read a literal ie a byte sequence prefixed with a tag containing its length delimited in {}s
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
    alt((map(tag("NIL"), |_| None), map(quoted, |v| Some(v))))(input.ltrim())
}

pub fn uid_fetch_envelopes_response(
    input: &[u8],
) -> IResult<&[u8], Vec<(UID, Option<(Flag, Vec<String>)>, Envelope)>> {
    many0(
        |input: &[u8]| -> IResult<&[u8], (UID, Option<(Flag, Vec<String>)>, Envelope)> {
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
            let (input, bodystructure) = take_until(")\r\n")(input)?;
            let (input, _) = tag(")\r\n")(input)?;
            Ok((input, {
                let mut env = env;
                let has_attachments = bodystructure_has_attachments(bodystructure);
                env.set_has_attachments(has_attachments);
                (uid_flags.0, uid_flags.1, env)
            }))
        },
    )(input)
}

pub fn bodystructure_has_attachments(input: &[u8]) -> bool {
    input.rfind(b" \"mixed\" ").is_some() || input.rfind(b" \"MIXED\" ").is_some()
}

#[derive(Debug, Default, Clone)]
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
    let mailbox = mailbox_token(mailbox).map(|(_, m)| get_path_hash!(m)).ok();
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
pub fn mailbox_token<'i>(input: &'i [u8]) -> IResult<&'i [u8], &'i str> {
    let (input, astring) = astring_token(input)?;
    if astring.eq_ignore_ascii_case(b"INBOX") {
        return Ok((input, "INBOX"));
    }
    Ok((input, to_str!(astring)))
}

// astring = 1*ASTRING-CHAR / string
fn astring_token(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((string_token, astring_char_tokens))(input)
}

// string = quoted / literal
fn string_token(input: &[u8]) -> IResult<&[u8], &[u8]> {
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
// atom-specials = "(" / ")" / "{" / SP / CTL / list-wildcards / quoted-specials / resp-specials
fn astring_char_tokens(input: &[u8]) -> IResult<&[u8], &[u8]> {
    // FIXME
    is_not(" \r\n")(input)
}

pub fn generate_envelope_hash(mailbox_path: &str, uid: &UID) -> EnvelopeHash {
    let mut h = DefaultHasher::new();
    h.write_usize(*uid);
    h.write(mailbox_path.as_bytes());
    h.finish()
}
