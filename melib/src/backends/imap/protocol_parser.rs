use super::*;
use crate::email::parser::BytesExt;
use nom::{digit, is_digit, rest, IResult};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

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
                f.no_select = false;
                for p in properties.split(|&b| b == b' ') {
                    if p.eq_ignore_ascii_case(b"\\NoSelect") {
                        f.no_select = true;
                    }
                }
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

/*
 *
 * "* 1 FETCH (FLAGS (\Seen) UID 1 RFC822.HEADER {5224}"
*/
named!(
    pub uid_fetch_response<Vec<(usize, Option<(Flag, Vec<String>)>, &[u8])>>,
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
        Err(MeliError::new(input))
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
                if let Some(d) = crate::email::parser::date(env.date_as_str().as_bytes()) {
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
        if input[i] == b'\"' {
            //&& input[i - 1] != b'\\' {
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
