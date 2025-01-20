//
// meli
//
// Copyright 2017- Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use super::*;

macro_rules! to_str (
    ($v:expr) => (unsafe{ std::str::from_utf8_unchecked($v) })
);

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
    assert!(RequiredResponses::NO.check(b"M12 NO [CANNOT] Invalid something\r\n"));
    {
        let mut ret = Vec::new();
        let required_responses = RequiredResponses::FETCH_UID | RequiredResponses::FETCH_FLAGS;
        let response =
            &b"* 1040 FETCH (UID 1064 FLAGS ())\r\nM15 OK Fetch completed (0.001 + 0.299 secs).\r\n"[..];
        for l in response.split_rn() {
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
    assert!(RequiredResponses::CAPABILITY.check(b"* CAPABILITY IMAP4rev1\r\n"));
    assert!(RequiredResponses::BYE.check(b"* BYE love\r\n"));
    assert!(RequiredResponses::FLAGS
        .check(b"* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n"));
    assert!(RequiredResponses::EXISTS.check(b"* 172 EXISTS\r\n"));
    assert!(RequiredResponses::RECENT.check(b"* 0 RECENT\r\n"));
    assert!(RequiredResponses::UNSEEN.check(b"* OK [UNSEEN 12] Message 12 is first unseen\r\n"));
    assert!(RequiredResponses::PERMANENTFLAGS
        .check(b"* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n"));
    assert!(RequiredResponses::UIDNEXT.check(b"* OK [UIDNEXT 4392] Predicted next UID\r\n"));
    assert!(RequiredResponses::UIDVALIDITY.check(b"* OK [UIDVALIDITY 3857529045] UIDs valid\r\n"));
    assert!(RequiredResponses::LIST.check(b"* LIST (\\HasChildren) \".\" INBOX\r\n"));
    assert!(RequiredResponses::LSUB.check(b"* LSUB (\\HasChildren) \".\" INBOX\r\n"));
    assert!(RequiredResponses::STATUS.check(b"* STATUS INBOX (MESSAGES 1057 UNSEEN 0)\r\n"));
    assert!(RequiredResponses::SEARCH.check(b"* SEARCH 1\r\n"));
    assert!(RequiredResponses::SEARCH.check(b"* SEARCH\r\n"));
    {
        let fetch =
            b"* 1429 FETCH (UID 1505 FLAGS (\\Seen) RFC822 {26}\r\nReturn-Path: <blah blah...\r\n";
        assert!(RequiredResponses::FETCH_UID.check(fetch));
        assert!(RequiredResponses::FETCH_FLAGS.check(fetch));
        assert!(RequiredResponses::FETCH_BODY.check(fetch));
        assert!((RequiredResponses::FETCH_UID | RequiredResponses::FETCH_FLAGS).check(fetch));
        assert!((RequiredResponses::FETCH_UID
            | RequiredResponses::FETCH_FLAGS
            | RequiredResponses::FETCH_BODY)
            .check(fetch));
        assert!((RequiredResponses::FETCH_UID | RequiredResponses::FETCH_BODY).check(fetch));
        assert!(!RequiredResponses::FETCH_ENVELOPE.check(fetch));
        assert!(!(RequiredResponses::FETCH_UID | RequiredResponses::FETCH_ENVELOPE).check(fetch));
    }
    {
        let modseq_fetch = b"* 1079 FETCH (UID 1103 MODSEQ (1365) FLAGS (\\Seen))\r\n";
        assert!(RequiredResponses::FETCH_MODSEQ.check(modseq_fetch));
        assert!((RequiredResponses::FETCH_UID
            | RequiredResponses::FETCH_MODSEQ
            | RequiredResponses::FETCH_FLAGS)
            .check(modseq_fetch));
        assert!(!RequiredResponses::FETCH_ENVELOPE.check(modseq_fetch));
        assert!(
            !(RequiredResponses::FETCH_UID | RequiredResponses::FETCH_ENVELOPE).check(modseq_fetch)
        );
    }

    {
        let body_fetch =
            b"* 1429 FETCH (UID 1505 FLAGS (\\Seen) RFC822 {26}\r\nReturn-Path: <blah blah...\r\n";
        assert!(RequiredResponses::FETCH_UID.check(body_fetch));
        assert!(RequiredResponses::FETCH_FLAGS.check(body_fetch));
        assert!(RequiredResponses::FETCH_BODY.check(body_fetch));
        assert!((RequiredResponses::FETCH_UID
            | RequiredResponses::FETCH_FLAGS
            | RequiredResponses::FETCH_BODY)
            .check(body_fetch));
        assert!(!RequiredResponses::FETCH_ENVELOPE.check(body_fetch));
        assert!(
            !(RequiredResponses::FETCH_UID | RequiredResponses::FETCH_ENVELOPE).check(body_fetch)
        );
    }
    {
        let full_fetch =
                    b"* 198 FETCH (UID 7608 FLAGS (\\Seen) ENVELOPE (\"Fri, 24 Jun 2011 10:09:10 +0000\" \"xxxx/xxxx\" ((\"xx@xx.com\" NIL \"xx\" \"xx.com\")) NIL NIL ((\"xx@xx\" NIL \"xx\" \"xx.com\")) ((\"'xx, xx'\" NIL \"xx.xx\" \"xx.com\")(\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")(\"'xx'\" NIL \"xx.xx\" \"xx.com\")(\"'xx xx'\" NIL \"xx.xx\" \"xx.com\")(\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")) NIL NIL \"<xx@xx.com>\") BODY[HEADER.FIELDS (REFERENCES)] {2}\r\n\r\n BODYSTRUCTURE ((\"text\" \"html\" (\"charset\" \"us-ascii\") \"<xx@xx>\" NIL \"7BIT\" 17236 232 NIL NIL NIL NIL)(\"image\" \"jpeg\" (\"name\" \"image001.jpg\") \"<image001.jpg@xx.xx>\" \"image001.jpg\" \"base64\" 1918 NIL (\"inline\" (\"filename\" \"image001.jpg\" \"size\" \"1650\" \"creation-date\" \"Sun, 09 Aug 2015 20:56:04 GMT\" \"modification-date\" \"Sun, 14 Aug 2022 22:11:45 GMT\")) NIL NIL) \"related\" (\"boundary\" \"xx--xx\" \"type\" \"text/html\") NIL \"en-US\"))\r\n";
        assert!(RequiredResponses::FETCH_REFERENCES.check(full_fetch));
        assert!(RequiredResponses::FETCH_BODYSTRUCTURE.check(full_fetch));
        assert!(RequiredResponses::FETCH_ENVELOPE.check(full_fetch));
    }
    {
        let fetch = b"* 2700 FETCH (UID 3223 FLAGS (\\Seen) ENVELOPE (\"Wed, 28 Aug 2024 13:53:11 +0000\" \"=?utf-8?Q?Update:=20Let's=20Talk!?=\" ((\"Newsletter\" NIL \"no-reply\" \"example.com\")) ((\"Newsletter\" NIL \"no-reply\" \"example.com\")) ((\"Newsletter\" NIL \"no-reply\" \"example.com\")) ((NIL NIL \"user\" \"example.com\")) NIL NIL NIL \"<XXXXXXXX.YYYYYYYYYYYYYY.ZZZZZZZZZZZZZZ@HHHH.TLD>\") BODY[HEADER.FIELDS (REFERENCES)] {2}\r\n\r\n BODYSTRUCTURE ((\"text\" \"plain\" (\"charset\" \"utf-8\") NIL NIL \"7bit\" 824 25 NIL NIL NIL NIL)(\"text\" \"html\" (\"charset\" \"utf-8\") NIL NIL \"7bit\" 28037 418 NIL NIL NIL NIL) \"alternative\" (\"boundary\" \"__\") NIL NIL NIL)\r\n";
        let required_responses = RequiredResponses::FETCH_UID
            | RequiredResponses::FETCH_FLAGS
            | RequiredResponses::FETCH_ENVELOPE
            | RequiredResponses::FETCH_REFERENCES
            | RequiredResponses::FETCH_BODYSTRUCTURE;
        assert!(required_responses.check(fetch));
        let lines = fetch.split_rn().collect::<Vec<_>>();
        assert_eq!(
            lines.len(),
            1,
            "Line was not split correctly by ImapLineIterator: {:?}",
            lines.iter().map(|l| to_str!(l)).collect::<Vec<&str>>()
        );
        let mut ret = Vec::new();
        for l in lines {
            if required_responses.check(l) {
                ret.extend_from_slice(l);
            } else {
                panic!("Unexpected response: {:?}", to_str!(l));
            }
        }
        assert_eq!(to_str!(fetch), to_str!(&ret));
    }
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
    {
        let s = b"* 2700 FETCH (UID 3223 FLAGS (\\Seen) ENVELOPE (\"Wed, 28 Aug 2024 13:53:11 +0000\" \"=?utf-8?Q?Update:=20Let's=20Talk!?=\" ((\"Newsletter\" NIL \"no-reply\" \"example.com\")) ((\"Newsletter\" NIL \"no-reply\" \"example.com\")) ((\"Newsletter\" NIL \"no-reply\" \"example.com\")) ((NIL NIL \"user\" \"example.com\")) NIL NIL NIL \"<XXXXXXXX.YYYYYYYYYYYYYY.ZZZZZZZZZZZZZZ@HHHH.TLD>\") BODY[HEADER.FIELDS (REFERENCES)] {2}\r\n\r\n BODYSTRUCTURE ((\"text\" \"plain\" (\"charset\" \"utf-8\") NIL NIL \"7bit\" 824 25 NIL NIL NIL NIL)(\"text\" \"html\" (\"charset\" \"utf-8\") NIL NIL \"7bit\" 28037 418 NIL NIL NIL NIL) \"alternative\" (\"boundary\" \"__\") NIL NIL NIL)\r\n";
        let mut iter = s.split_rn();
        assert_eq!(to_str!(iter.next().unwrap()), to_str!(s));
        assert!(iter.next().is_none());
    }
    {
        let s = b"{6}\r\n\r\n\r\n\r\n rest\r\n";
        let mut iter = s.split_rn();
        assert_eq!(to_str!(iter.next().unwrap()), to_str!(s));
        assert!(iter.next().is_none());
    }
    {
        let s = b"{6}\r\n\r\n\r\n\r\n first\r\nsecond not a literal{ 5}\r\n";
        let mut iter = s.split_rn();
        for l in &[
            &b"{6}\r\n\r\n\r\n\r\n first\r\n"[..],
            &b"second not a literal{ 5}\r\n"[..],
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
            bodystructure: false,
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
            bodystructure: false,
            raw_fetch_value: &b"* 1 FETCH (FLAGS (\\Seen))\r\n"[..],
        })
    );
}

#[test]
fn test_imap_fetch_response() {
    #[rustfmt::skip]
    let input: &[u8] = b"* 198 FETCH (UID 7608 FLAGS (\\Seen) ENVELOPE (\"Fri, 24 Jun 2011 10:09:10 +0000\" \"xxxx/xxxx\" ((\"xx@xx.com\" NIL \"xx\" \"xx.com\")) NIL NIL ((\"xx@xx\" NIL \"xx\" \"xx.com\")) ((\"'xx, xx'\" NIL \"xx.xx\" \"xx.com\")(\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")(\"'xx'\" NIL \"xx.xx\" \"xx.com\")(\"'xx xx'\" NIL \"xx.xx\" \"xx.com\")(\"xx.xx@xx.com\" NIL \"xx.xx\" \"xx.com\")) NIL NIL \"<xx@xx.com>\") BODY[HEADER.FIELDS (REFERENCES)] {2}\r\n\r\n BODYSTRUCTURE ((\"text\" \"html\" (\"charset\" \"us-ascii\") \"<xx@xx>\" NIL \"7BIT\" 17236 232 NIL NIL NIL NIL)(\"image\" \"jpeg\" (\"name\" \"image001.jpg\") \"<image001.jpg@xx.xx>\" \"image001.jpg\" \"base64\" 1918 NIL (\"inline\" (\"filename\" \"image001.jpg\" \"size\" \"1650\" \"creation-date\" \"Sun, 09 Aug 2015 20:56:04 GMT\" \"modification-date\" \"Sun, 14 Aug 2022 22:11:45 GMT\")) NIL NIL) \"related\" (\"boundary\" \"xx--xx\" \"type\" \"text/html\") NIL \"en-US\"))\r\n";
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
                references: Some(b""),
                envelope: Some(env),
                bodystructure: true,
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
