/*
 * meli - parser module
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
use super::*;
use chrono;
use data_encoding::BASE64_MIME;
use encoding::{DecoderTrap, Encoding};
use nom::{is_hex_digit, le_u8};
use nom::{Compare, CompareResult};
use nom::{ErrorKind, IResult, Needed};
use std;
use std::str::from_utf8;

macro_rules! is_whitespace {
    ($var:ident) => {
        $var == b' ' && $var == b'\t' && $var == b'\n' && $var == b'\r'
    };
    ($var:expr) => {
        $var == b' ' && $var == b'\t' && $var == b'\n' && $var == b'\r'
    };
}

pub trait BytesExt {
    fn trim(&self) -> &Self;
    fn find(&self, needle: &[u8]) -> Option<usize>;
    fn replace(&self, from: &[u8], to: &[u8]) -> Vec<u8>;
}

impl BytesExt for [u8] {
    fn trim(&self) -> &[u8] {
        if let Some(first) = self.iter().position(|b| !is_whitespace!(*b)) {
            if let Some(last) = self.iter().rposition(|b| !is_whitespace!(*b)) {
                &self[first..last + 1]
            } else {
                unreachable!();
            }
        } else {
            &[]
        }
    }
    // https://stackoverflow.com/a/35907071
    fn find(&self, needle: &[u8]) -> Option<usize> {
        self.windows(needle.len())
            .position(|window| window == needle)
    }
    fn replace(&self, from: &[u8], to: &[u8]) -> Vec<u8> {
        let mut ret = self.to_vec();
        if let Some(idx) = self.find(from) {
            ret.splice(idx..(idx + from.len()), to.iter().cloned());
        }
        ret
    }
}

fn quoted_printable_byte(input: &[u8]) -> IResult<&[u8], u8> {
    if input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if input[0] == b'=' && is_hex_digit(input[1]) && is_hex_digit(input[2]) {
        let a = if input[1] < b':' {
            input[1] - 48
        } else if input[1] < b'[' {
            input[1] - 55
        } else {
            input[1] - 87
        };
        let b = if input[2] < b':' {
            input[2] - 48
        } else if input[2] < b'[' {
            input[2] - 55
        } else {
            input[2] - 87
        };
        IResult::Done(&input[3..], a * 16 + b)
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

// Parser definition

/* A header can span multiple lines, eg:
 *
 * Received: from -------------------- (-------------------------)
 * 	by --------------------- (--------------------- [------------------]) (-----------------------)
 * 	with ESMTP id ------------ for <------------------->;
 * 	Tue,  5 Jan 2016 21:30:44 +0100 (CET)
 */

fn header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let input_len = input.len();
    for (i, x) in input.iter().enumerate() {
        if *x == b'\n' {
            if ((i + 1) < input_len && input[i + 1] != b' ' && input[i + 1] != b'\t')
                || i + 1 == input_len
            {
                return IResult::Done(&input[(i + 1)..], &input[0..i]);
            }
        }
    }
    IResult::Incomplete(Needed::Unknown)
}

/* Parse the name part of the header -> &str */
named!(name<&[u8]>, is_not!(":\n"));

/* Parse a single header as a tuple -> (&str, Vec<&str>) */
named!(
    header<(&[u8], &[u8])>,
    separated_pair!(complete!(name), ws!(tag!(b":")), complete!(header_value))
);
/* Parse all headers -> Vec<(&str, Vec<&str>)> */
named!(pub headers<std::vec::Vec<(&[u8], &[u8])>>,
       many1!(complete!(header)));

//named!(pub headers_raw<&[u8]>,
//take_until1!("\n\n"));

pub fn headers_raw(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() {
        return IResult::Incomplete(Needed::Unknown);
    }
    for (i, x) in input.iter().enumerate() {
        if *x == b'\n' && i + 1 < input.len() && input[i + 1] == b'\n' {
            return IResult::Done(&input[(i + 1)..], &input[0..i + 1]);
        }
    }
    return IResult::Error(error_code!(ErrorKind::Custom(43)));
}

named!(pub body_raw<&[u8]>,
       do_parse!(
           take_until1!("\n\n") >>
           body: take_while!(call!(|_| true)) >>
           ( { body } )));

named!(pub mail<(std::vec::Vec<(&[u8], &[u8])>, &[u8])>,
       separated_pair!(headers, tag!(b"\n"), take_while!(call!(|_| true))));
named!(pub attachment<(std::vec::Vec<(&[u8], &[u8])>, &[u8])>,
       do_parse!(
            opt!(is_a!(" \n\t\r")) >>
       pair: pair!(many0!(complete!(header)), take_while!(call!(|_| true))) >>
       ( { pair } )));

/* Header parsers */

/* Encoded words
 *"=?charset?encoding?encoded text?=".
 */

/* TODO: make a map of encodings and decoding functions so that they can be reused and easily
 * extended */
use encoding::all::{ISO_8859_1, ISO_8859_2, ISO_8859_7, WINDOWS_1252, WINDOWS_1253, GBK};

fn encoded_word(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if input.len() < 5 {
        return IResult::Incomplete(Needed::Unknown);
    } else if input[0] != b'=' || input[1] != b'?' {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }
    for tag in &[
        "UTF-8",
        "iso-8859-7",
        "windows-1253",
        "iso-8859-1",
        "iso-8859-2",
        "gbk",
    ] {
        if let CompareResult::Ok = (&input[2..]).compare_no_case(*tag) {
            let tag_len = tag.len();
            /* tag must end with ?_? where _ is either Q or B, eg: =?UTF-8?B? */
            if input[2 + tag_len] != b'?' || input[2 + tag_len + 2] != b'?' {
                return IResult::Error(error_code!(ErrorKind::Custom(43)));
            }
            /* See if input ends with "?=" and get ending index */
            let mut encoded_idx = None;
            for i in (5 + tag_len)..input.len() {
                if input[i] == b'?' && i < input.len() && input[i + 1] == b'=' {
                    encoded_idx = Some(i);
                    break;
                }
            }
            if encoded_idx.is_none() {
                return IResult::Error(error_code!(ErrorKind::Custom(43)));
            }
            let encoded = &input[5 + tag_len..encoded_idx.unwrap()];

            let s: Vec<u8> = match input[2 + tag_len + 1] {
                b'b' | b'B' => match BASE64_MIME.decode(encoded) {
                    Ok(v) => v,
                    Err(_) => encoded.to_vec(),
                },
                b'q' | b'Q' => match quoted_printable_bytes_header(encoded) {
                    IResult::Done(b"", s) => s,
                    _ => return IResult::Error(error_code!(ErrorKind::Custom(43))),
                },
                _ => return IResult::Error(error_code!(ErrorKind::Custom(43))),
            };

            match *tag {
                "UTF-8" => {
                    return IResult::Done(&input[encoded_idx.unwrap() + 2..], s);
                }
                "iso-8859-7" => {
                    return if let Ok(v) = ISO_8859_7.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap() + 2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                }
                "windows-1253" => {
                    return if let Ok(v) = WINDOWS_1253.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap() + 2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                }
                "iso-8859-1" => {
                    return if let Ok(v) = ISO_8859_1.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap() + 2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                }
                "iso-8859-2" => {
                    return if let Ok(v) = ISO_8859_2.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap() + 2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                }
                "gbk" => {
                    return if let Ok(v) = GBK.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap() + 2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                }
                _ => {
                    panic!();
                }
            }
        } else {
            continue;
        }
    }
    eprintln!("unknown tag is {:?}", from_utf8(&input[2..20]));
    IResult::Error(error_code!(ErrorKind::Custom(43)))
}

pub fn decode_charset(s: &[u8], charset: Charset) -> Result<String> {
    match charset {
        Charset::UTF8 | Charset::Ascii => Ok(String::from_utf8(s.to_vec()).unwrap()),
        Charset::ISO8859_7 => Ok(ISO_8859_7.decode(s, DecoderTrap::Strict)?),
        Charset::ISO8859_1 => Ok(ISO_8859_1.decode(s, DecoderTrap::Strict)?),
        Charset::ISO8859_2 => Ok(ISO_8859_2.decode(s, DecoderTrap::Strict)?),
        Charset::GBK => Ok(GBK.decode(s, DecoderTrap::Strict)?),
        Charset::Windows1252 => Ok(WINDOWS_1252.decode(s, DecoderTrap::Strict)?),
        Charset::Windows1253 => Ok(WINDOWS_1253.decode(s, DecoderTrap::Strict)?),
        Charset::GB2312 => unimplemented!(),
        Charset::UTF16 => unimplemented!(),
    }
}

fn quoted_printable_soft_break(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.len() < 2 {
        IResult::Incomplete(Needed::Size(1))
    } else if input[0] == b'=' && input[1] == b'\n' {
        IResult::Done(&input[2..], &input[0..2]) // `=\n` is an escaped space character.
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

named!(qp_underscore_header<u8>, do_parse!(tag!("_") >> ({ b' ' })));

// With MIME, headers in quoted printable format can contain underscores that represent spaces.
// In non-header context, an underscore is just a plain underscore.
named!(
    pub quoted_printable_bytes_header<Vec<u8>>,
    many0!(alt_complete!(
        quoted_printable_byte | qp_underscore_header | le_u8
    ))
);

/// For atoms in Header values.
named!(
    pub quoted_printable_bytes<Vec<u8>>,
    many0!(alt_complete!(
        preceded!(quoted_printable_soft_break, quoted_printable_byte) |
        preceded!(quoted_printable_soft_break, le_u8) | quoted_printable_byte | le_u8
    ))
);

named!(
    encoded_word_list<Vec<u8>>,
    ws!(do_parse!(
        list: separated_nonempty_list!(complete!(is_a!(" \n\r\t")), encoded_word) >> ({
            let list_len = list.iter().fold(0, |mut acc, x| {
                acc += x.len();
                acc
            });
            let bytes = list
                .iter()
                .fold(Vec::with_capacity(list_len), |mut acc, x| {
                    acc.append(&mut x.clone());
                    acc
                });
            bytes
        })
    ))
);
named!(
    ascii_token<Vec<u8>>,
    do_parse!(
        word: alt!(
            terminated!(take_until1!("=?"), peek!(tag_no_case!("=?UTF-8?")))
                | take_while!(call!(|_| true))
        ) >> ({ word.into() })
    )
);

fn display_addr(input: &[u8]) -> IResult<&[u8], Address> {
    if input.is_empty() || input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if !is_whitespace!(input[0]) {
        let mut display_name = StrBuilder {
            offset: 0,
            length: 0,
        };
        let mut flag = false;
        for (i, b) in input[0..].iter().enumerate() {
            if *b == b'<' {
                display_name.length = i.saturating_sub(1); // if i != 0 { i - 1 } else { 0 };
                flag = true;
                break;
            }
        }
        if !flag {
            return IResult::Error(error_code!(ErrorKind::Custom(43)));
        }
        let mut end = input.len();
        let mut flag = false;
        for (i, b) in input[display_name.length + 2..].iter().enumerate() {
            if *b == b'@' {
                flag = true;
            }
            if *b == b'>' {
                end = i;
                break;
            }
        }
        if flag {
            let mut address_spec = StrBuilder {
                offset: display_name.length + 2,
                length: end,
            };
            match phrase(&input[0..end + display_name.length + 3]) {
                IResult::Error(e) => IResult::Error(e),
                IResult::Incomplete(i) => IResult::Incomplete(i),
                IResult::Done(rest, raw) => {
                    display_name.length = raw.find(b"<").unwrap().saturating_sub(1);
                    address_spec.offset = display_name.length + 2;
                    address_spec.length = raw
                        .len()
                        .saturating_sub(display_name.length)
                        .saturating_sub(3);
                    IResult::Done(
                        rest,
                        Address::Mailbox(MailboxAddress {
                            raw: raw,
                            display_name: display_name,
                            address_spec: address_spec,
                        }),
                    )
                }
            }
        } else {
            IResult::Error(error_code!(ErrorKind::Custom(43)))
        }
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

fn addr_spec(input: &[u8]) -> IResult<&[u8], Address> {
    if input.is_empty() || input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if !is_whitespace!(input[0]) {
        let mut end = input[1..].len();
        let mut flag = false;
        for (i, b) in input[1..].iter().enumerate() {
            if *b == b'@' {
                flag = true;
            }
            if is_whitespace!(*b) {
                end = i;
                break;
            }
        }
        if flag {
            IResult::Done(
                &input[end..],
                Address::Mailbox(MailboxAddress {
                    raw: input[0..end + 1].into(),
                    display_name: StrBuilder {
                        offset: 0,
                        length: 0,
                    },
                    address_spec: StrBuilder {
                        offset: 0,
                        length: input[0..end + 1].len(),
                    },
                }),
            )
        } else {
            IResult::Error(error_code!(ErrorKind::Custom(43)))
        }
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(42)))
    }
}

named!(
    mailbox<Address>,
    ws!(alt_complete!(display_addr | addr_spec))
);
named!(mailbox_list<Vec<Address>>, many0!(mailbox));

#[test]
fn test_mailbox() {
    {
        let s = b"epilys@postretch";
        let r = mailbox(s).unwrap().1;
        match r {
            Address::Mailbox(ref m) => {
                println!(
                    "----\n`{}`, `{}`\n----",
                    m.display_name.display(&m.raw),
                    m.address_spec.display(&m.raw)
                );
            }
            _ => {}
        }
    }
    let s = b"Manos <epilys@postretch>";
    eprintln!("{:?}", display_addr(s).unwrap());
    let r = display_addr(s).unwrap().1;
    match r {
        Address::Mailbox(ref m) => {
            println!(
                "----\n`{}`, `{}`\n----",
                m.display_name.display(&m.raw),
                m.address_spec.display(&m.raw)
            );
        }
        _ => {}
    }
}

//named!(group_t<GroupAddress>, ws!( do_parse!(
//            display_name: take_until1!(":") >>
//            mailbox_list: many0!(mailbox) >>
//            end: is_a!(";") >>
//            ({
//
//            })
//            )));
//

fn group(input: &[u8]) -> IResult<&[u8], Address> {
    let mut flag = false;
    let mut dlength = 0;
    for (i, b) in input.iter().enumerate() {
        if *b == b':' {
            flag = true;
            dlength = i;
            break;
        }
    }
    if !flag {
        return IResult::Error(error_code!(ErrorKind::Custom(43)));
    }

    match mailbox_list(&input[dlength..]) {
        IResult::Error(e) => {
            return IResult::Error(e);
        }
        IResult::Done(rest, vec) => {
            let size: usize = (rest.as_ptr() as usize) - ((&input[0..] as &[u8]).as_ptr() as usize);
            return IResult::Done(
                rest,
                Address::Group(GroupAddress {
                    raw: input[0..size].into(),
                    display_name: StrBuilder {
                        offset: 0,
                        length: dlength,
                    },
                    mailbox_list: vec,
                }),
            );
        }
        IResult::Incomplete(i) => {
            return IResult::Incomplete(i);
        }
    }
}

named!(address<Address>, ws!(alt_complete!(mailbox | group)));

#[test]
fn test_address() {
    let s = b"Manos Pitsidianakis <el13635@mail.ntua.gr>,
            qemu-devel <qemu-devel@nongnu.org>, qemu-block <qemu-block@nongnu.org>,
            Alberto Garcia <berto@igalia.com>, Stefan Hajnoczi <stefanha@redhat.com>";
    println!("{:?}", rfc2822address_list(s).unwrap());
}

named!(pub rfc2822address_list<Vec<Address>>, ws!( separated_list!(is_a!(","), address)));

named!(pub address_list<String>, ws!(do_parse!(
        list: alt_complete!( encoded_word_list | ascii_token) >>
        ( {
            let list: Vec<&[u8]> = list.split(|c| *c == b',').collect();
            let string_len = list.iter().fold(0, |mut acc, x| { acc+=x.trim().len(); acc }) + list.len() - 1;
            let list_len = list.len();
            let mut i = 0;
            list.iter().fold(String::with_capacity(string_len),
            |acc, x| {
                let mut acc = acc + &String::from_utf8_lossy(x.replace(b"\n", b"").replace(b"\t", b" ").trim());
                if i != list_len - 1 {
                    acc.push_str(" ");
                    i+=1;
                }
                acc
            })
        } )

       )));

named!(pub phrase<Vec<u8>>, ws!(do_parse!(
        list: many0!(alt_complete!( encoded_word_list | ascii_token)) >>
        ( {
            if list.len() == 0 {
               Vec::new()
            } else {
            let string_len = list.iter().fold(0, |mut acc, x| { acc+=x.len(); acc }) + list.len() - 1;
            let list_len = list.len();
            let mut i = 0;
            list.iter().fold(Vec::with_capacity(string_len),
            |mut acc, x| {
                acc.extend(x.replace(b"\n", b"").replace(b"\t", b" "));
                if i != list_len - 1 {
                    acc.push(b' ');
                    i+=1;
                }
                acc
            })
            }
        } )

       )));

#[test]
fn test_phrase() {
    let phrase_s = "list.free.de mailing list memberships reminder".as_bytes();
    assert_eq!(
        (
            &b""[..],
            "list.free.de mailing list memberships reminder".to_string()
        ),
        phrase(phrase_s).unwrap()
    );

    let phrase_s = "=?UTF-8?B?zp3Orc6/IM+Az4HOv8+Dz4nPgM65zrrPjCDOvM6uzr3Phc68zrEgzrHPhs6v?= =?UTF-8?B?z4fOuM63?=".as_bytes();
    assert_eq!(
        (
            &b""[..],
            "Νέο προσωπικό μήνυμα αφίχθη".to_string()
        ),
        phrase(phrase_s).unwrap()
    );

    let phrase_s = "=?utf-8?B?bW9vZGxlOiDOsc69zrHPg866z4zPgM63z4POtyDOv868zqzOtM6xz4Igz4M=?=  =?utf-8?B?z4XOts63z4TOrs+DzrXPic69?=".as_bytes();
    assert_eq!(
        (
            &b""[..],
            "moodle: ανασκόπηση ομάδας συζητήσεων".to_string()
        ),
        phrase(phrase_s).unwrap()
    );

    let phrase_s =
        "=?UTF-8?B?zp3Orc6/IM+Az4HOv8+Dz4nPgM65zrrPjCDOvM6uzr3Phc68zrEgzrHPhs6v?=".as_bytes();
    assert_eq!(
        "Νέο προσωπικό μήνυμα αφί".to_string(),
        from_utf8(&encoded_word(phrase_s).to_full_result().unwrap()).unwrap()
    );
    let phrase_s = "=?UTF-8?Q?=CE=A0=CF=81=CF=8C=CF=83=CE=B8=CE=B5?=".as_bytes();
    assert_eq!(
        "Πρόσθε".to_string(),
        from_utf8(&encoded_word(phrase_s).to_full_result().unwrap()).unwrap()
    );
    let phrase_s = "=?iso-8859-7?B?UmU6INDx/OLr5+zhIOzlIPTn7SDh9fHp4e3eIOLh8eTp4Q==?=".as_bytes();
    assert_eq!(
        "Re: Πρόβλημα με την αυριανή βαρδια".to_string(),
        from_utf8(&encoded_word(phrase_s).to_full_result().unwrap()).unwrap()
    );

    let phrase_s = "=?UTF-8?Q?=CE=A0=CF=81=CF=8C=CF=83=CE=B8=CE=B5?=
 =?UTF-8?Q?=CF=84=CE=B7_=CE=B5=CE=BE=CE=B5=CF=84?=
 =?UTF-8?Q?=CE=B1=CF=83=CF=84=CE=B9=CE=BA=CE=AE?="
        .as_bytes();
    assert_eq!(
        (
            &b""[..],
            "Πρόσθετη εξεταστική".to_string()
        ),
        phrase(phrase_s).unwrap()
    );
}
fn eat_comments(input: &[u8]) -> Vec<u8> {
    let mut in_comment = false;
    input
        .iter()
        .fold(Vec::with_capacity(input.len()), |mut acc, x| {
            if *x == b'(' && !in_comment {
                in_comment = true;
                acc
            } else if *x == b')' && in_comment {
                in_comment = false;
                acc
            } else if in_comment {
                acc
            } else {
                acc.push(*x);
                acc
            }
        })
}

#[test]
fn test_eat_comments() {
    let s = "Mon (Lundi), 4(quatre)May (Mai) 1998(1998-05-04)03 : 04 : 12 +0000";
    assert_eq!(eat_comments(s), "Mon , 4May  199803 : 04 : 12 +0000");
    let s = "Thu, 31 Aug 2017 13:43:37 +0000 (UTC)";
    assert_eq!(eat_comments(s), "Thu, 31 Aug 2017 13:43:37 +0000 ");
}
/*
 * Date should tokenize input and convert the tokens,
 * right now we expect input will have no extra spaces in between tokens
 *
 * We should use a custom parser here*/
pub fn date(input: &[u8]) -> Option<chrono::DateTime<chrono::FixedOffset>> {
    let parsed_result = phrase(&eat_comments(input))
        .to_full_result()
        .unwrap()
        .replace(b"-", b"+");
    chrono::DateTime::parse_from_rfc2822(String::from_utf8_lossy(parsed_result.trim()).as_ref())
        .ok()
}

#[test]
fn test_date() {
    let s = "Thu, 31 Aug 2017 13:43:37 +0000 (UTC)";
    let _s = "Thu, 31 Aug 2017 13:43:37 +0000";
    let __s = "=?utf-8?q?Thu=2C_31_Aug_2017_13=3A43=3A37_-0000?=";
    assert_eq!(date(s).unwrap(), date(_s).unwrap());
    assert_eq!(date(_s).unwrap(), date(__s).unwrap());
}

named!(pub message_id<&[u8]>,
        complete!(delimited!(tag!("<"), take_until1!(">"), tag!(">")))
 );

fn message_id_peek(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let input_length = input.len();
    if input.is_empty() {
        IResult::Incomplete(Needed::Size(1))
    } else if input_length == 2 || input[0] != b'<' {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    } else {
        for (i, &x) in input.iter().take(input_length).enumerate().skip(1) {
            if x == b'>' {
                return IResult::Done(&input[i + 1..], &input[0..i + 1]);
            }
        }
        IResult::Incomplete(Needed::Unknown)
    }
}

named!(pub references<Vec<&[u8]>>, separated_list!(complete!(is_a!(" \n\t\r")), message_id_peek));

named_args!(pub attachments<'a>(boundary: &'a [u8], boundary_end: &'a [u8]) < Vec<&'this_is_probably_unique_i_hope_please [u8]> >,
            alt_complete!(do_parse!(
                take_until!(boundary) >>
                vecs: many0!(complete!(do_parse!(
                            tag!(boundary) >>
                            tag!("\n") >>
                            body: take_until1!(boundary)  >>
                            ( { body } )))) >>
                tag!(boundary_end) >>
                tag!("\n") >>
                take_while!(call!(|_| { true })) >>
                ( {
                    vecs
                } )
            ) | do_parse!(
                        take_until!(boundary_end) >>
                        tag!(boundary_end) >>
                        ( { Vec::<&[u8]>::new() } ))
                    ));
#[test]
fn test_attachments() {
    use std::io::Read;
    let mut buffer: Vec<u8> = Vec::new();
    let _ = std::fs::File::open("test/attachment_test")
        .unwrap()
        .read_to_end(&mut buffer);
    let boundary = "--b1_4382d284f0c601a737bb32aaeda53160--";
    let boundary_len = boundary.len();
    let (_, body) = match mail(&buffer).to_full_result() {
        Ok(v) => v,
        Err(_) => panic!(),
    };
    let attachments = attachments(body, &boundary[0..boundary_len - 2], &boundary)
        .to_full_result()
        .unwrap();
    assert_eq!(attachments.len(), 4);
}

named!(
    content_type_parameter<(&[u8], &[u8])>,
    do_parse!(
        tag!(";") >> name: terminated!(ws!(take_until!("=")), tag!("="))
            >> value:
                ws!(alt_complete!(
                    delimited!(tag!("\""), take_until!("\""), tag!("\"")) | is_not!(";")
                )) >> ({ (name, value) })
    )
);

named!(pub content_type< (&[u8], &[u8], Vec<(&[u8], &[u8])>) >,
       do_parse!(
           _type: take_until!("/") >>
           tag!("/") >>
           _subtype: is_not!(";") >>
           parameters: many0!(complete!(content_type_parameter)) >>
           ( {
               (_type, _subtype, parameters)
           } )
           ));

//named!(pub quoted_printable_text<Vec<u8>>,
//   do_parse!(
//       bytes: many0!(alt_complete!(
//               preceded!(tag!("=\n"), quoted_printable_byte) |
//               preceded!(tag!("=\n"), le_u8) |
//               quoted_printable_byte |
//               le_u8)) >>
//       ( {
//           bytes
//       } )
//   )
//);
