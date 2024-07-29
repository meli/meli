/*
 * meli - mailbox module.
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

//! # Mbox formats
//!
//! ## Resources
//!
//! [^0]: <https://web.archive.org/web/20160812091518/https://jdebp.eu./FGA/mail-mbox-formats.html>
//! [^1]: <https://wiki2.dovecot.org/MailboxFormat/mbox>
//! [^2]: <https://manpages.debian.org/buster/mutt/mbox.5.en.html>
//!
//! ## `mbox` format
//!
//! `mbox` describes a family of incompatible legacy formats.
//!
//! "All of the 'mbox' formats store all of the messages in the mailbox in a
//! single file. Delivery appends new messages to the end of the file." [^0]
//!
//! "Each message is preceded by a From_ line and followed by a blank line. A
//! From_ line is a line that begins with the five characters 'F', 'r', 'o',
//! 'm', and ' '." [^0]
//!
//! ## `From ` / postmark line
//!
//! "An mbox is a text file containing an arbitrary number of e-mail messages.
//! Each message consists of a postmark, followed by an e-mail message formatted
//! according to RFC822, RFC2822. The file format is line-oriented. Lines are
//! separated by line feed characters (ASCII 10).
//!
//! "A postmark line consists of the four characters 'From', followed by a space
//! character, followed by the message's envelope sender address, followed by
//! whitespace, and followed by a time stamp. This line is often called From_
//! line.
//!
//! "The sender address is expected to be addr-spec as defined in RFC2822 3.4.1.
//! The date is expected to be date-time as output by asctime(3). For
//! compatibility reasons with legacy software, two-digit years greater than or
//! equal to 70 should be interpreted as the years 1970+, while two-digit years
//! less than 70 should be interpreted as the years 2000-2069. Software reading
//! files in this format should also be prepared to accept non-numeric timezone
//! information such as 'CET DST' for Central European Time, daylight saving
//! time.
//!
//! "Example:
//!
//!```text
//! From example@example.com Fri Jun 23 02:56:55 2000
//! ```
//!
//! "In order to avoid misinterpretation of lines in message bodies which begin
//! with the four characters 'From', followed by a space character, the mail
//! delivery agent must quote any occurrence of 'From ' at the start of a body
//! line." [^2]
//!
//! ## Metadata
//!
//! `melib` recognizes the `CClient` (a [Pine client API](https://web.archive.org/web/20050203003235/http://www.washington.edu/imap/)) convention for metadata in `mbox` format:
//!
//! - `Status`: R (Seen) and O (non-Recent) flags
//! - `X-Status`: A (Answered), F (Flagged), T (Draft) and D (Deleted) flags
//! - `X-Keywords`: Message’s keywords
//!
//! ## Parsing an mbox file
//!
//! ```
//! # use melib::{Result, Envelope, EnvelopeHash, mbox::*};
//! # use std::collections::HashMap;
//! # use std::sync::{Arc, Mutex};
//! let file_contents = vec![]; // Replace with actual mbox file contents
//! let index: Arc<Mutex<HashMap<EnvelopeHash, (Offset, Length)>>> =
//!     Arc::new(Mutex::new(HashMap::default()));
//! let mut message_iter = MessageIterator {
//!     index: index.clone(),
//!     input: &file_contents.as_slice(),
//!     offset: 0,
//!     file_offset: 0,
//!     format: Some(MboxFormat::MboxCl2),
//! };
//! let envelopes: Result<Vec<Envelope>> = message_iter.collect();
//! ```
//!
//! ## Writing / Appending an mbox file
//!
//! ```no_run
//! # use melib::mbox::*;
//! # use std::io::Write;
//! let mbox_1: &[u8] = br#"From: <a@b.c>\n\nHello World"#;
//! let mbox_2: &[u8] = br#"From: <d@e.f>\n\nHello World #2"#;
//! let mut file = std::io::BufWriter::new(std::fs::File::create(&"out.mbox")?);
//! let format = MboxFormat::MboxCl2;
//! format.append(
//!     &mut file,
//!     mbox_1,
//!     None,                                // Envelope From
//!     Some(melib::utils::datetime::now()), // Delivered date
//!     Default::default(),                  // Flags and tags
//!     MboxMetadata::None,
//!     true,
//!     false,
//! )?;
//! format.append(
//!     &mut file,
//!     mbox_2,
//!     None,
//!     Some(melib::utils::datetime::now()),
//!     Default::default(), // Flags and tags
//!     MboxMetadata::None,
//!     false,
//!     false,
//! )?;
//! file.flush()?;
//! # Ok::<(), melib::Error>(())
//! ```

use std::{
    collections::hash_map::HashMap,
    fs::File,
    io::{BufReader, Read},
    os::unix::io::AsRawFd,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{mpsc::channel, Arc, Mutex, RwLock},
};

use nom::{
    self,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::map_res,
    error::{Error as NomError, ErrorKind as NomErrorKind},
    IResult,
};
use notify::{event::EventKind as NotifyEvent, RecommendedWatcher, RecursiveMode, Watcher};

use crate::{
    backends::prelude::*,
    email::{parser::BytesExt, *},
    error::{Error, ErrorKind, IntoError, Result},
    get_path_hash,
    utils::shellexpand::ShellExpandTrait,
};

pub mod write;

pub type Offset = usize;
pub type Length = usize;

#[cfg(target_os = "linux")]
const F_OFD_SETLKW: libc::c_int = 38;

// Open file description locking
// # man fcntl
fn get_rw_lock_blocking(f: &File, path: &Path) -> Result<()> {
    let fd: libc::c_int = f.as_raw_fd();
    let mut flock: libc::flock = libc::flock {
        l_type: libc::F_WRLCK as libc::c_short,
        l_whence: libc::SEEK_SET as libc::c_short,
        l_start: 0,
        l_len: 0, /* "Specifying 0 for l_len has the special meaning: lock all bytes starting at the location
                  specified by l_whence and l_start through to the end of file, no matter how large the file grows." */
        l_pid: 0, /* "By contrast with traditional record locks, the l_pid field of that
                   * structure must be set to zero when using the commands described below." */
        #[cfg(target_os = "freebsd")]
        l_sysid: 0,
    };
    let ptr: *mut libc::flock = &mut flock;
    #[cfg(not(target_os = "linux"))]
    let ret_val = unsafe { libc::fcntl(fd, libc::F_SETLKW, ptr as *mut libc::c_void) };
    #[cfg(target_os = "linux")]
    let ret_val = unsafe { libc::fcntl(fd, F_OFD_SETLKW, ptr as *mut libc::c_void) };
    if ret_val == -1 {
        let err = nix::errno::Errno::from_i32(nix::errno::errno());
        return Err(Error::new(format!(
            "Could not lock {}: fcntl() returned {}",
            path.display(),
            err.desc()
        )));
    }
    Ok(())
}

#[derive(Debug)]
pub struct MboxMailbox {
    pub hash: MailboxHash,
    pub name: String,
    pub path: PathBuf,
    pub fs_path: PathBuf,
    pub content: Vec<u8>,
    pub children: Vec<MailboxHash>,
    pub parent: Option<MailboxHash>,
    pub usage: Arc<RwLock<SpecialUsageMailbox>>,
    pub is_subscribed: bool,
    pub permissions: MailboxPermissions,
    pub total: Arc<Mutex<usize>>,
    pub unseen: Arc<Mutex<usize>>,
    pub index: Arc<Mutex<HashMap<EnvelopeHash, (Offset, Length)>>>,
}

impl BackendMailbox for MboxMailbox {
    fn hash(&self) -> MailboxHash {
        self.hash
    }

    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn path(&self) -> &str {
        /* We know it's valid UTF-8 because we supplied it */
        self.path.to_str().unwrap()
    }

    fn clone(&self) -> Mailbox {
        Box::new(Self {
            hash: self.hash,
            name: self.name.clone(),
            path: self.path.clone(),
            fs_path: self.fs_path.clone(),
            content: self.content.clone(),
            children: self.children.clone(),
            usage: self.usage.clone(),
            is_subscribed: self.is_subscribed,
            parent: self.parent,
            permissions: self.permissions,
            unseen: self.unseen.clone(),
            total: self.total.clone(),
            index: self.index.clone(),
        })
    }

    fn children(&self) -> &[MailboxHash] {
        &self.children
    }

    fn parent(&self) -> Option<MailboxHash> {
        self.parent
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        *self.usage.read().unwrap()
    }

    fn permissions(&self) -> MailboxPermissions {
        self.permissions
    }
    fn is_subscribed(&self) -> bool {
        self.is_subscribed
    }
    fn set_is_subscribed(&mut self, new_val: bool) -> Result<()> {
        self.is_subscribed = new_val;
        Ok(())
    }
    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()> {
        *self.usage.write()? = new_val;
        Ok(())
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((*self.unseen.lock()?, *self.total.lock()?))
    }
}

/// `BackendOp` implementor for Mbox
#[derive(Debug, Clone)]
pub struct MboxOp {
    pub _hash: EnvelopeHash,
    pub path: PathBuf,
    pub offset: Offset,
    pub length: Length,
}

impl MboxOp {
    pub fn new(_hash: EnvelopeHash, path: &Path, offset: Offset, length: Length) -> Self {
        Self {
            _hash,
            path: path.to_path_buf(),
            offset,
            length,
        }
    }
}

impl BackendOp for MboxOp {
    fn as_bytes(&self) -> ResultFuture<Vec<u8>> {
        use std::io::Seek;
        let _self = self.clone();

        Ok(Box::pin(async move {
            smol::unblock(move || {
                let file = std::fs::OpenOptions::new()
                    .read(true)
                    .write(true)
                    .open(&_self.path)?;
                get_rw_lock_blocking(&file, &_self.path)?;
                let mut buf_reader = BufReader::new(file);
                buf_reader.seek(std::io::SeekFrom::Start(_self.offset.try_into().unwrap()))?;
                let mut ret = Vec::new();
                buf_reader
                    .take(_self.length.try_into().unwrap())
                    .read_to_end(&mut ret)?;

                Ok(ret)
            })
            .await
        }))
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MboxMetadata {
    /// Dovecot uses C-Client (ie. UW-IMAP, Pine) compatible headers in mbox
    /// messages to store me
    /// - X-IMAPbase: Contains UIDVALIDITY, last used UID and list of used
    ///   keywords
    /// - X-IMAP: Same as X-IMAPbase but also specifies that the message is a
    ///   “pseudo message”
    /// - X-UID: Message’s allocated UID
    /// - Status: R (Seen) and O (non-Recent) flags
    /// - X-Status: A (Answered), F (Flagged), T (Draft) and D (Deleted) flags
    /// - X-Keywords: Message’s keywords
    /// - Content-Length: Length of the message body in bytes
    CClient,
    None,
}

/// Choose between "mboxo", "mboxrd", "mboxcl", "mboxcl2". For new mailboxes,
/// prefer "mboxcl2" which does not alter the mail body.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MboxFormat {
    MboxO,
    MboxRd,
    MboxCl,
    MboxCl2,
}

impl Default for MboxFormat {
    fn default() -> Self {
        Self::MboxCl2
    }
}

macro_rules! find_From__line {
    ($input:expr) => {{
        //debug!("find_From__line invocation");
        let input = $input;
        let mut ptr = 0;
        let mut found = None;
        while ptr < input.len() {
            // Find next From_ candidate line.
            const TAG: &'static [u8] = b"\n\nFrom ";
            if let Some(end) = input[ptr..].find(TAG) {
                // This candidate is a valid From_ if it ends in a new line and the next line is
                // a header.
                if let Some(line_end) = input[ptr + end + TAG.len()..].find(b"\n") {
                    if crate::email::parser::headers::header(
                        &input[ptr + end + TAG.len() + line_end + 1..],
                    )
                    .is_ok()
                    {
                        found = Some(ptr + end);
                        break;
                    } else {
                        /* Ignore invalid From_ line. */
                        ptr += end + TAG.len() + line_end;
                    }
                } else {
                    /* Ignore invalid From_ line. */
                    ptr += end + TAG.len();
                }
            } else {
                found = Some(input.len());
                break;
            }
        }
        found
    }};
}

impl MboxFormat {
    pub fn parse<'i>(&self, input: &'i [u8]) -> IResult<&'i [u8], Envelope> {
        let orig_input = input;
        let mut input = input;
        match self {
            Self::MboxO => {
                let next_offset: Option<(usize, usize)> = find_From__line!(input)
                    .and_then(|end| input.find(b"\n").map(|start| (start + 1, end)));

                if let Some((start, len)) = next_offset {
                    match Envelope::from_bytes(&input[start..len], None) {
                        Ok(mut env) => {
                            let mut flags = Flag::empty();
                            if env.other_headers().contains_key("Status") {
                                if env.other_headers()["Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                            }
                            if env.other_headers().contains_key("X-Status") {
                                if env.other_headers()["X-Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["X-Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["X-Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["X-Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                                if env.other_headers()["X-Status"].contains('T') {
                                    flags.set(Flag::DRAFT, true);
                                }
                            }
                            env.set_flags(flags);
                            if len == input.len() {
                                Ok((&[], env))
                            } else {
                                input = &input[len + 2..];
                                Ok((input, env))
                            }
                        }
                        Err(err) => {
                            log::debug!("Could not parse mail {:?}", err);
                            Err(nom::Err::Error(NomError {
                                input,
                                code: NomErrorKind::Tag,
                            }))
                        }
                    }
                } else {
                    let start: Offset = input.find(b"\n").map(|v| v + 1).unwrap_or(0);
                    match Envelope::from_bytes(&input[start..], None) {
                        Ok(mut env) => {
                            let mut flags = Flag::empty();
                            if env.other_headers().contains_key("Status") {
                                if env.other_headers()["Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                            }
                            if env.other_headers().contains_key("X-Status") {
                                if env.other_headers()["X-Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["X-Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["X-Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["X-Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                                if env.other_headers()["X-Status"].contains('T') {
                                    flags.set(Flag::DRAFT, true);
                                }
                            }
                            env.set_flags(flags);
                            Ok((&[], env))
                        }
                        Err(err) => {
                            log::debug!("Could not parse mail at {:?}", err);
                            Err(nom::Err::Error(NomError {
                                input,
                                code: NomErrorKind::Tag,
                            }))
                        }
                    }
                }
            }
            Self::MboxRd => {
                let next_offset: Option<(usize, usize)> = find_From__line!(input)
                    .and_then(|end| input.find(b"\n").map(|start| (start + 1, end)));

                if let Some((start, len)) = next_offset {
                    match Envelope::from_bytes(&input[start..len], None) {
                        Ok(mut env) => {
                            let mut flags = Flag::empty();
                            if env.other_headers().contains_key("Status") {
                                if env.other_headers()["Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                            }
                            if env.other_headers().contains_key("X-Status") {
                                if env.other_headers()["X-Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["X-Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["X-Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["X-Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                                if env.other_headers()["X-Status"].contains('T') {
                                    flags.set(Flag::DRAFT, true);
                                }
                            }
                            env.set_flags(flags);
                            if len == input.len() {
                                Ok((&[], env))
                            } else {
                                input = &input[len + 2..];
                                Ok((input, env))
                            }
                        }
                        Err(err) => {
                            log::debug!("Could not parse mail {:?}", err);
                            Err(nom::Err::Error(NomError {
                                input,
                                code: NomErrorKind::Tag,
                            }))
                        }
                    }
                } else {
                    let start: Offset = input.find(b"\n").map(|v| v + 1).unwrap_or(0);
                    match Envelope::from_bytes(&input[start..], None) {
                        Ok(mut env) => {
                            let mut flags = Flag::empty();
                            if env.other_headers().contains_key("Status") {
                                if env.other_headers()["Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                            }
                            if env.other_headers().contains_key("X-Status") {
                                if env.other_headers()["X-Status"].contains('F') {
                                    flags.set(Flag::FLAGGED, true);
                                }
                                if env.other_headers()["X-Status"].contains('A') {
                                    flags.set(Flag::REPLIED, true);
                                }
                                if env.other_headers()["X-Status"].contains('R') {
                                    flags.set(Flag::SEEN, true);
                                }
                                if env.other_headers()["X-Status"].contains('D') {
                                    flags.set(Flag::TRASHED, true);
                                }
                                if env.other_headers()["X-Status"].contains('T') {
                                    flags.set(Flag::DRAFT, true);
                                }
                            }
                            env.set_flags(flags);
                            Ok((&[], env))
                        }
                        Err(err) => {
                            log::debug!("Could not parse mail {:?}", err);
                            Err(nom::Err::Error(NomError {
                                input,
                                code: NomErrorKind::Tag,
                            }))
                        }
                    }
                }
            }
            Self::MboxCl | Self::MboxCl2 => {
                let start: Offset = input.find(b"\n").map(|v| v + 1).unwrap_or(0);
                input = &input[start..];
                let headers_end: usize = input.find(b"\n\n").unwrap_or(input.len());
                let content_length = if let Some(v) = input[..headers_end].find(b"Content-Length: ")
                {
                    v
                } else {
                    // Is not MboxCl{,2}
                    return Self::MboxRd.parse(orig_input);
                };
                let (_input, _) = if let Ok(s) = tag::<_, &[u8], (&[u8], nom::error::ErrorKind)>(
                    "Content-Length:",
                )(&input[content_length..])
                {
                    s
                } else {
                    return Self::MboxRd.parse(orig_input);
                };
                let (_input, bytes) = if let Ok(s) =
                    map_res::<&[u8], _, _, (&[u8], nom::error::ErrorKind), _, _, _>(
                        digit1,
                        |s: &[u8]| String::from_utf8_lossy(s).parse::<usize>(),
                    )(_input.ltrim())
                {
                    s
                } else {
                    return Self::MboxRd.parse(orig_input);
                };

                match Envelope::from_bytes(&input[..headers_end + bytes], None) {
                    Ok(mut env) => {
                        let mut flags = Flag::empty();
                        if env.other_headers().contains_key("Status") {
                            if env.other_headers()["Status"].contains('F') {
                                flags.set(Flag::FLAGGED, true);
                            }
                            if env.other_headers()["Status"].contains('A') {
                                flags.set(Flag::REPLIED, true);
                            }
                            if env.other_headers()["Status"].contains('R') {
                                flags.set(Flag::SEEN, true);
                            }
                            if env.other_headers()["Status"].contains('D') {
                                flags.set(Flag::TRASHED, true);
                            }
                        }
                        if env.other_headers().contains_key("X-Status") {
                            if env.other_headers()["X-Status"].contains('F') {
                                flags.set(Flag::FLAGGED, true);
                            }
                            if env.other_headers()["X-Status"].contains('A') {
                                flags.set(Flag::REPLIED, true);
                            }
                            if env.other_headers()["X-Status"].contains('R') {
                                flags.set(Flag::SEEN, true);
                            }
                            if env.other_headers()["X-Status"].contains('D') {
                                flags.set(Flag::TRASHED, true);
                            }
                            if env.other_headers()["X-Status"].contains('T') {
                                flags.set(Flag::DRAFT, true);
                            }
                        }
                        env.set_flags(flags);
                        if headers_end + 2 + bytes >= input.len() {
                            Ok((&[], env))
                        } else {
                            input = &input[headers_end + 3 + bytes..];
                            Ok((input, env))
                        }
                    }
                    Err(_err) => Self::MboxRd.parse(orig_input),
                }
            }
        }
    }
}

pub fn mbox_parse(
    index: Arc<Mutex<HashMap<EnvelopeHash, (Offset, Length)>>>,
    input: &[u8],
    file_offset: usize,
    format: Option<MboxFormat>,
) -> IResult<&[u8], Vec<Envelope>> {
    if input.is_empty() {
        return Err(nom::Err::Error(NomError {
            input,
            code: NomErrorKind::Tag,
        }));
    }
    let mut offset = 0;
    let mut index = index.lock().unwrap();
    let mut envelopes = Vec::with_capacity(32);

    let format = format.unwrap_or(MboxFormat::MboxCl2);
    while !input[offset + file_offset..].is_empty() {
        let (next_input, env) = match format.parse(&input[offset + file_offset..]) {
            Ok(v) => v,
            Err(e) => {
                // Try to recover from this error by finding a new candidate From_ line
                if let Some(next_offset) = find_From__line!(&input[offset + file_offset..]) {
                    offset += next_offset;
                    if offset != input.len() {
                        // If we are not at EOF, we will be at this point
                        //    "\n\nFrom ..."
                        //     ↑
                        // So, skip those two newlines.
                        offset += 2;
                    }
                } else {
                    return Err(e);
                }
                continue;
            }
        };
        let start: Offset = input[offset + file_offset..]
            .find(b"From ")
            .map(|from_offset| {
                input[offset + file_offset + from_offset..]
                    .find(b"\n")
                    .map(|v| v + 1)
                    .unwrap_or_else(|| {
                        input[offset + file_offset + from_offset..]
                            .len()
                            .saturating_sub(2)
                    })
            })
            .map(|v| v + 2)
            .unwrap_or(0);
        let len = input.len() - next_input.len() - offset - file_offset - start;
        index.insert(env.hash(), (offset + file_offset + start, len));
        offset += len + start;

        envelopes.push(env);
    }
    Ok((&[], envelopes))
}

pub struct MessageIterator<'a> {
    pub index: Arc<Mutex<HashMap<EnvelopeHash, (Offset, Length)>>>,
    pub input: &'a [u8],
    pub file_offset: usize,
    pub offset: usize,
    pub format: Option<MboxFormat>,
}

impl<'a> Iterator for MessageIterator<'a> {
    type Item = Result<Envelope>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            return None;
        }
        let mut index = self.index.lock().unwrap();

        let format = self.format.unwrap_or(MboxFormat::MboxCl2);
        while !self.input[self.offset + self.file_offset..].is_empty() {
            let (next_input, env) =
                match format.parse(&self.input[self.offset + self.file_offset..]) {
                    Ok(v) => v,
                    Err(e) => {
                        // Try to recover from this error by finding a new candidate From_ line
                        if let Some(next_offset) =
                            find_From__line!(&self.input[self.offset + self.file_offset..])
                        {
                            self.offset += next_offset;
                            if self.offset != self.input.len() {
                                // If we are not at EOF, we will be at this point
                                //    "\n\nFrom ..."
                                //     ↑
                                // So, skip those two newlines.
                                self.offset += 2;
                            }
                        } else {
                            self.input = b"";
                            return Some(Err(e.into()));
                        }
                        continue;
                    }
                };
            let start: Offset = self.input[self.offset + self.file_offset..]
                .find(b"From ")
                .map(|from_offset| {
                    self.input[self.offset + self.file_offset + from_offset..]
                        .find(b"\n")
                        .map(|v| v + 1)
                        .unwrap_or_else(|| {
                            self.input[self.offset + self.file_offset + from_offset..]
                                .len()
                                .saturating_sub(2)
                        })
                })
                .map(|v| v + 1)
                .unwrap_or(0);
            let len = self.input.len() - next_input.len() - self.offset - self.file_offset - start;
            index.insert(env.hash(), (self.offset + self.file_offset + start, len));
            self.offset += len + start;

            return Some(Ok(env));
        }
        None
    }
}

/// Mbox backend
#[derive(Debug)]
pub struct MboxType {
    account_name: String,
    path: PathBuf,
    collection: Collection,
    mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    mailboxes: Arc<Mutex<HashMap<MailboxHash, MboxMailbox>>>,
    prefer_mbox_type: Option<MboxFormat>,
    event_consumer: BackendEventConsumer,
}

impl MailBackend for MboxType {
    fn capabilities(&self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            is_async: false,
            is_remote: false,
            supports_search: false,
            extensions: None,
            supports_tags: false,
            supports_submission: false,
            extra_submission_headers: &[],
            metadata: None,
        };
        CAPABILITIES
    }

    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        struct FetchState {
            mailbox_hash: MailboxHash,
            mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
            mailboxes: Arc<Mutex<HashMap<MailboxHash, MboxMailbox>>>,
            prefer_mbox_type: Option<MboxFormat>,
            offset: usize,
            file_offset: usize,
            contents: Vec<u8>,
        }
        impl FetchState {
            async fn fetch(&mut self) -> Result<Option<Vec<Envelope>>> {
                let mailboxes_lck = self.mailboxes.lock().unwrap();
                let index = mailboxes_lck[&self.mailbox_hash].index.clone();
                drop(mailboxes_lck);
                let mut message_iter = MessageIterator {
                    index,
                    input: self.contents.as_slice(),
                    offset: self.offset,
                    file_offset: self.file_offset,
                    format: self.prefer_mbox_type,
                };
                let mut payload = vec![];
                let mut done = false;
                'iter_for_loop: for _i in 0..150 {
                    match message_iter.next() {
                        Some(Ok(env)) => {
                            payload.push(env);
                        }
                        Some(Err(_err)) => {
                            debug!(&_err);
                        }
                        None => {
                            done = true;
                            break 'iter_for_loop;
                        }
                    }
                }
                self.offset = message_iter.offset;
                self.file_offset = message_iter.file_offset;
                {
                    let mut mailbox_index_lck = self.mailbox_index.lock().unwrap();
                    for env in &payload {
                        mailbox_index_lck.insert(env.hash(), self.mailbox_hash);
                    }
                }
                if done {
                    if payload.is_empty() {
                        Ok(None)
                    } else {
                        let contents = std::mem::take(&mut self.contents);
                        self.mailboxes
                            .lock()
                            .unwrap()
                            .entry(self.mailbox_hash)
                            .and_modify(|f| f.content = contents);
                        Ok(Some(payload))
                    }
                } else {
                    Ok(Some(payload))
                }
            }
        }
        let mailboxes = self.mailboxes.clone();

        let mailbox_path = mailboxes.lock().unwrap()[&mailbox_hash].fs_path.clone();
        let file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(&mailbox_path)?;
        get_rw_lock_blocking(&file, &mailbox_path)?;
        let mut buf_reader = BufReader::new(file);
        let mut contents = Vec::new();
        buf_reader.read_to_end(&mut contents)?;
        let mut state = FetchState {
            mailbox_hash,
            mailboxes,
            mailbox_index: self.mailbox_index.clone(),
            prefer_mbox_type: self.prefer_mbox_type,
            contents,
            offset: 0,
            file_offset: 0,
        };
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            loop {
                if let Some(res) = state.fetch().await.map_err(|err| {
                    debug!("fetch err {:?}", &err);
                    err
                })? {
                    emitter.emit(res).await;
                } else {
                    return Ok(());
                }
            }
        })))
    }

    fn refresh(&mut self, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        Err(
            Error::new("Refreshing is currently unimplemented for mbox backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn watch(&self) -> ResultFuture<()> {
        let sender = self.event_consumer.clone();
        let (tx, rx) = channel();
        let watcher = RecommendedWatcher::new(
            tx,
            notify::Config::default().with_poll_interval(std::time::Duration::from_secs(10)),
        )
        .and_then(|mut watcher| {
            {
                let mailboxes_lck = self.mailboxes.lock().unwrap();
                for f in mailboxes_lck.values() {
                    watcher.watch(&f.fs_path, RecursiveMode::Recursive)?;
                    log::debug!("watching {:?}", f.fs_path.as_path());
                }
            }
            Ok(watcher)
        })
        .map_err(|err| err.set_err_details("Failed to create file change monitor."))?;
        let account_hash = AccountHash::from_bytes(self.account_name.as_bytes());
        let mailboxes = self.mailboxes.clone();
        let mailbox_index = self.mailbox_index.clone();
        let prefer_mbox_type = self.prefer_mbox_type;
        Ok(Box::pin(async move {
            let _watcher = watcher;
            loop {
                match rx.recv() {
                    Ok(Ok(event)) => match event.kind {
                        /* Update */
                        NotifyEvent::Modify(
                            notify::event::ModifyKind::Any
                            | notify::event::ModifyKind::Data(_)
                            | notify::event::ModifyKind::Other,
                        ) => {
                            for pathbuf in event.paths {
                                let mailbox_hash = MailboxHash(get_path_hash!(&pathbuf));
                                let file = match std::fs::OpenOptions::new()
                                    .read(true)
                                    .write(true)
                                    .open(&pathbuf)
                                {
                                    Ok(f) => f,
                                    Err(_) => {
                                        continue;
                                    }
                                };
                                get_rw_lock_blocking(&file, &pathbuf)?;
                                let mut mailbox_lock = mailboxes.lock().unwrap();
                                let mut buf_reader = BufReader::new(file);
                                let mut contents = Vec::new();
                                if let Err(e) = buf_reader.read_to_end(&mut contents) {
                                    debug!(e);
                                    continue;
                                };
                                if contents
                                    .starts_with(mailbox_lock[&mailbox_hash].content.as_slice())
                                {
                                    if let Ok((_, envelopes)) = mbox_parse(
                                        mailbox_lock[&mailbox_hash].index.clone(),
                                        &contents,
                                        mailbox_lock[&mailbox_hash].content.len(),
                                        prefer_mbox_type,
                                    ) {
                                        let mut mailbox_index_lck = mailbox_index.lock().unwrap();
                                        for env in envelopes {
                                            mailbox_index_lck.insert(env.hash(), mailbox_hash);
                                            (sender)(
                                                account_hash,
                                                BackendEvent::Refresh(RefreshEvent {
                                                    account_hash,
                                                    mailbox_hash,
                                                    kind: RefreshEventKind::Create(Box::new(env)),
                                                }),
                                            );
                                        }
                                    }
                                } else {
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: RefreshEventKind::Rescan,
                                        }),
                                    );
                                }
                                mailbox_lock
                                    .entry(mailbox_hash)
                                    .and_modify(|f| f.content = contents);
                            }
                        }
                        NotifyEvent::Remove(_) => {
                            for pathbuf in event.paths {
                                if mailboxes
                                    .lock()
                                    .unwrap()
                                    .values()
                                    .any(|f| f.fs_path == pathbuf)
                                {
                                    let mailbox_hash = MailboxHash(get_path_hash!(&pathbuf));
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: RefreshEventKind::Failure(Error::new(format!(
                                                "mbox mailbox {} was removed.",
                                                pathbuf.display()
                                            ))),
                                        }),
                                    );
                                    return Ok(());
                                }
                            }
                        }
                        NotifyEvent::Modify(notify::event::ModifyKind::Name(
                            notify::event::RenameMode::Both,
                        )) if event.paths.len() == 2 => {
                            let [ref src, ref dest] = event.paths[..] else {
                                unreachable!()
                            };
                            if mailboxes
                                .lock()
                                .unwrap()
                                .values()
                                .any(|f| f.fs_path == *src)
                            {
                                let mailbox_hash = MailboxHash(get_path_hash!(&src));
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: RefreshEventKind::Failure(Error::new(format!(
                                            "mbox mailbox {} was renamed to {}.",
                                            src.display(),
                                            dest.display()
                                        ))),
                                    }),
                                );
                                return Ok(());
                            }
                        }
                        _ => {
                            log::debug!("Received unexpected fs watcher notify event: {:?}", event);
                            /* Trigger rescan of mailboxes */
                            let mailboxes_lck = mailboxes.lock().unwrap();
                            for &mailbox_hash in mailboxes_lck.keys() {
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: RefreshEventKind::Rescan,
                                    }),
                                );
                            }
                            return Ok(());
                        }
                    },
                    Ok(Err(err)) => log::debug!("watch error: {:?}", err),
                    Err(err) => {
                        log::debug!("watch error: {:?}", err);
                        return Err(Error::new(format!(
                            "Mbox watching thread exited with error: {err}"
                        )));
                    }
                }
            }
        }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let ret = Ok(self
            .mailboxes
            .lock()
            .unwrap()
            .iter()
            .map(|(h, f)| (*h, f.clone() as Mailbox))
            .collect());
        Ok(Box::pin(async { ret }))
    }

    fn operation(&self, env_hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        let mailbox_hash = self.mailbox_index.lock().unwrap()[&env_hash];
        let mailboxes_lck = self.mailboxes.lock().unwrap();
        let (offset, length) = {
            let index = mailboxes_lck[&mailbox_hash].index.lock().unwrap();
            index[&env_hash]
        };
        let mailbox_path = mailboxes_lck[&mailbox_hash].fs_path.clone();
        Ok(Box::new(MboxOp::new(
            env_hash,
            mailbox_path.as_path(),
            offset,
            length,
        )))
    }

    fn copy_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _source_mailbox_hash: MailboxHash,
        _destination_mailbox_hash: MailboxHash,
        _move_: bool,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Copying messages is currently unimplemented for mbox backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_flags(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
        _flags: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Setting flags is currently unimplemented for mbox backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn delete_messages(
        &mut self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Deleting messages is currently unimplemented for mbox backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn save(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Saving messages is currently unimplemented for mbox backend")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn collection(&self) -> Collection {
        self.collection.clone()
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(
            Error::new("Deleting mailboxes is currently unimplemented for mbox backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Mailbox subscriptions are not possible for the mbox backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(
            Error::new("Renaming mailboxes is currently unimplemented for mbox backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(
            Error::new("Setting mailbox permissions is not possible for the mbox backend.")
                .set_kind(ErrorKind::NotSupported),
        )
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        Err(Error::new("Search is unimplemented for the mbox backend.")
            .set_kind(ErrorKind::NotImplemented))
    }

    fn create_mailbox(
        &mut self,
        _new_path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(
            Error::new("Creating mailboxes is unimplemented for the mbox backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }
}

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            Error::new(format!(
                "Configuration error ({}): mbox backend requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    Error::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

impl MboxType {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>> {
        let path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "\"root_mailbox\" {} for account {} is not a valid path.",
                s.root_mailbox.as_str(),
                s.name
            )));
        }
        let prefer_mbox_type: String = get_conf_val!(s["prefer_mbox_type"], "auto".to_string())?;
        let ret = Self {
            account_name: s.name.to_string(),
            event_consumer,
            path,
            prefer_mbox_type: match prefer_mbox_type.as_str() {
                "auto" => None,
                "mboxo" => Some(MboxFormat::MboxO),
                "mboxrd" => Some(MboxFormat::MboxRd),
                "mboxcl" => Some(MboxFormat::MboxCl),
                "mboxcl2" => Some(MboxFormat::MboxCl2),
                _ => {
                    return Err(Error::new(format!(
                        "{} invalid `prefer_mbox_type` value: `{}`",
                        s.name, prefer_mbox_type,
                    )))
                }
            },
            collection: Collection::default(),
            mailbox_index: Default::default(),
            mailboxes: Default::default(),
        };
        let name: String = ret
            .path
            .file_name()
            .map(|f| f.to_string_lossy().into())
            .unwrap_or_default();
        let hash = MailboxHash(get_path_hash!(&ret.path));

        let read_only = if let Ok(metadata) = std::fs::metadata(&ret.path) {
            metadata.permissions().readonly()
        } else {
            true
        };

        ret.mailboxes.lock().unwrap().insert(
            hash,
            MboxMailbox {
                hash,
                path: name.clone().into(),
                fs_path: ret.path.clone(),
                name,
                content: Vec::new(),
                children: Vec::new(),
                parent: None,
                usage: Arc::new(RwLock::new(SpecialUsageMailbox::Normal)),
                is_subscribed: true,
                permissions: MailboxPermissions {
                    create_messages: !read_only,
                    remove_messages: !read_only,
                    set_flags: !read_only,
                    create_child: !read_only,
                    rename_messages: !read_only,
                    delete_messages: !read_only,
                    delete_mailbox: !read_only,
                    change_permissions: false,
                },
                unseen: Arc::new(Mutex::new(0)),
                total: Arc::new(Mutex::new(0)),
                index: Default::default(),
            },
        );
        /* Look for other mailboxes */
        for (k, f) in s.mailboxes.iter() {
            if let Some(path_str) = f.extra.get("path") {
                let hash = MailboxHash(get_path_hash!(path_str));
                let pathbuf: PathBuf = Path::new(path_str).expand();
                if !pathbuf.try_exists().unwrap_or(false) || pathbuf.is_dir() {
                    return Err(Error::new(format!(
                        "mbox mailbox configuration entry \"{}\" path value {} is not a file.",
                        k, path_str
                    )));
                }
                let read_only = if let Ok(metadata) = std::fs::metadata(&pathbuf) {
                    metadata.permissions().readonly()
                } else {
                    true
                };

                ret.mailboxes.lock().unwrap().insert(
                    hash,
                    MboxMailbox {
                        hash,
                        name: k.to_string(),
                        fs_path: pathbuf,
                        path: k.into(),
                        content: Vec::new(),
                        children: Vec::new(),
                        parent: None,
                        usage: Arc::new(RwLock::new(f.usage.unwrap_or_default())),
                        is_subscribed: f.subscribe.is_true(),
                        permissions: MailboxPermissions {
                            create_messages: !read_only,
                            remove_messages: !read_only,
                            set_flags: !read_only,
                            create_child: !read_only,
                            rename_messages: !read_only,
                            delete_messages: !read_only,
                            delete_mailbox: !read_only,
                            change_permissions: false,
                        },
                        unseen: Arc::new(Mutex::new(0)),
                        total: Arc::new(Mutex::new(0)),
                        index: Default::default(),
                    },
                );
            } else {
                return Err(Error::new(format!(
                    "mbox mailbox configuration entry \"{}\" should have a \"path\" value set \
                     pointing to an mbox file.",
                    k
                )));
            }
        }
        Ok(Box::new(ret))
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        macro_rules! get_conf_val {
            ($s:ident[$var:literal]) => {
                $s.extra.remove($var).ok_or_else(|| {
                    Error::new(format!(
                        "Configuration error ({}): mbox backend requires the field `{}` set",
                        $s.name.as_str(),
                        $var
                    ))
                })
            };
            ($s:ident[$var:literal], $default:expr) => {
                $s.extra
                    .remove($var)
                    .map(|v| {
                        <_>::from_str(&v).map_err(|e| {
                            Error::new(format!(
                                "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                                $s.name.as_str(),
                                $var,
                                v,
                                e
                            ))
                        })
                    })
                    .unwrap_or_else(|| Ok($default))
            };
        }
        let path = Path::new(s.root_mailbox.as_str()).expand();
        if !path.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "\"root_mailbox\" {} for account {} is not a valid path.",
                s.root_mailbox.as_str(),
                s.name
            )));
        }
        let prefer_mbox_type: Result<String> =
            get_conf_val!(s["prefer_mbox_type"], "auto".to_string());
        prefer_mbox_type?;
        Ok(())
    }
}
