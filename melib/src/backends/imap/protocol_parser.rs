use super::*;
use nom::{digit, is_digit, rest, IResult};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

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
macro_rules! get_path_hash {
    ($path:expr) => {{
        let mut hasher = DefaultHasher::new();
        $path.hash(&mut hasher);
        hasher.finish()
    }};
}
/*
* LIST (\HasNoChildren) "." INBOX.Sent
* LIST (\HasChildren) "." INBOX
 */
named!(
    pub list_folder_result<ImapFolder>,
    do_parse!(
        ws!(tag!("* LIST ("))
            >> properties: take_until!(&b")"[0..])
            >> tag!(b") ")
            >> separator: delimited!(tag!(b"\""), take!(1), tag!(b"\""))
            >> take!(1)
            >> path: alt_complete!(delimited!(tag!("\""), is_not!("\""), tag!("\"")) | call!(rest))
            >> ({
                let separator: u8 = separator[0];
                let mut f = ImapFolder::default();
                f.hash = get_path_hash!(path);
                f.path = String::from_utf8_lossy(path).into();
                f.name = if let Some(pos) = path.iter().rposition(|&c| c == separator) {
                    f.parent = Some(get_path_hash!(&path[..pos]));
                    String::from_utf8_lossy(&path[pos + 1..]).into()
                } else {
                    f.path.clone()
                };

                debug!(f)
            })
    )
);

named!(
    my_flags<Flag>,
    do_parse!(
        flags:
            separated_nonempty_list!(
                tag!(" "),
                preceded!(tag!("\\"), is_not!(")"))
            )
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

/*
 *
 * * 1 FETCH (FLAGS (\Seen) UID 1 RFC822.HEADER {5224}
*/
named!(
    pub uid_fetch_response<Vec<(usize, Option<Flag>, &[u8])>>,
    many0!(
        do_parse!(
            tag!("* ")
            >> take_while!(call!(is_digit))
            >> tag!(" FETCH (")
            >> uid_flags: permutation!(preceded!(ws!(tag!("UID ")), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) })), opt!(preceded!(ws!(tag!("FLAGS ")), delimited!(tag!("("), byte_flags, tag!(")")))))
            >> is_not!("{")
            >> body: length_bytes!(delimited!(tag!("{"), map_res!(digit, |s| { usize::from_str(unsafe { std::str::from_utf8_unchecked(s) }) }), tag!("}\r\n")))
            >> tag!(")\r\n")
            >> ((uid_flags.0, uid_flags.1, body))
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
 */

named!(
    pub capabilities<Vec<&[u8]>>,
    do_parse!(
        take_until!("[CAPABILITY ")
        >> tag!("[CAPABILITY ")
        >> ret: terminated!(separated_nonempty_list_complete!(tag!(" "), is_not!(" ]")), tag!("]"))
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


#[derive(Debug, Clone)]
pub enum SelectResponse {
    Ok(SelectResponseOk),
    Bad(SelectResponseBad),
}

#[derive(Debug, Default, Clone)]
pub struct SelectResponseOk {
    pub exists: usize,
    pub recent: usize,
    pub flags: Flag,
    pub unseen: usize,
    pub uidvalidity: usize,
    pub uidnext: usize,
    pub permanentflags: Flag,
}

#[derive(Debug, Default, Clone)]
pub struct SelectResponseBad {
    pub exists: usize,
    pub recent: usize,
    pub flags: Flag,
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
pub fn select_response(input: &str) -> IResult<&str, SelectResponse> {
    if input.contains("* OK") {
        let mut ret = SelectResponseOk::default();
        for l in input.split("\r\n") {
            if l.starts_with("* ") && l.ends_with(" EXISTS") {
                ret.exists = usize::from_str(&l["* ".len()..l.len()-" EXISTS".len()]).unwrap();
            } else if l.starts_with("* ") && l.ends_with(" RECENT") {
                ret.recent = usize::from_str(&l["* ".len()..l.len()-" RECENT".len()]).unwrap();
            } else if l.starts_with("* FLAGS (") {
                ret.flags = flags(&l["* FLAGS (".len()..l.len() - ")".len()]).to_full_result().unwrap();
            } else if l.starts_with("* OK [UNSEEN ") {
                ret.unseen = usize::from_str(&l["* OK [UNSEEN ".len()..l.find(']').unwrap()]).unwrap();
            } else if l.starts_with("* OK [UIDVALIDITY ") {
                ret.uidvalidity = usize::from_str(&l["* OK [UIDVALIDITY ".len()..l.find(']').unwrap()]).unwrap();
            } else if l.starts_with("* OK [UIDNEXT ") {
                ret.uidnext = usize::from_str(&l["* OK [UIDNEXT ".len()..l.find(']').unwrap()]).unwrap();
            } else if l.starts_with("* OK [PERMANENTFLAGS (") {
                ret.permanentflags = flags(&l["* OK [PERMANENTFLAGS (".len()..l.find(')').unwrap()]).to_full_result().unwrap();
            } else if !l.is_empty() {
                debug!("select response: {}", l);
            }
        }
        IResult::Done(&""[0..], SelectResponse::Ok(ret))
    } else {
        let mut ret = SelectResponseBad::default();
        for l in input.split("\r\n") {
            if l.starts_with("* ") && l.ends_with(" EXISTS") {
                ret.exists = usize::from_str(&l["* ".len()..l.len()-" EXISTS".len()]).unwrap();
            } else if l.starts_with("* ") && l.ends_with(" RECENT") {
                ret.recent = usize::from_str(&l["* ".len()..l.len()-" RECENT".len()]).unwrap();
            } else if l.starts_with("* FLAGS (") {
                ret.flags = flags(&l["* FLAGS (".len()..l.len() - ")".len()]).to_full_result().unwrap();
            } else if !l.is_empty() {
                debug!("select response: {}", l);
            }
        }
        IResult::Done(&""[0..], SelectResponse::Bad(ret))
    }
}

pub fn flags(input: &str) -> IResult<&str, Flag> {
    let mut ret = Flag::default();

    let mut input = input;
    while input.starts_with("\\") {
        input = &input[1..];
        let match_end = input.find(|c: char| !c.is_ascii_alphabetic()).or_else(|| input.find(" ")).unwrap_or(input.len());
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
                debug!("unknown Flag token value: {}", f);
                break;
            }
        }
        input = &input[match_end..];
        if input.starts_with(" \\") {
            input = &input[1..];
        }
    }
    IResult::Done(input, ret)
}

pub fn byte_flags(input: &[u8]) -> IResult<&[u8], Flag> {
    let i = unsafe{ std::str::from_utf8_unchecked(input) };
    match flags(i) {
        IResult::Done(rest, ret) => IResult::Done(rest.as_bytes(), ret),
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(e) => IResult::Incomplete(e),

    }
}
