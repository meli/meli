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
use std;
use std::str::from_utf8;
use base64;
use chrono;
use nom::{le_u8, is_hex_digit};
use nom::{IResult,Needed,ErrorKind};
use nom::{Compare, CompareResult};
use encoding::{Encoding, DecoderTrap};

fn quoted_printable_byte(input: &[u8]) -> IResult<&[u8],u8> {
    if input.is_empty() || input.len() < 3 {
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
        IResult::Done(&input[3..], a*16+b)
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


fn header_value(input: &[u8]) -> IResult<&[u8], &str> {
    if input.is_empty() || input[0] == b'\n' {
        IResult::Incomplete(Needed::Unknown)
    } else {
        let input_len = input.len();
        for (i, x) in input.iter().enumerate() {
            if *x == b'\n' {
                if (i + 1) < input_len &&
                    ((input[i+1] != b' ' && input[i+1] != b'\t') || input[i+1] == b'\n') {
                        return match from_utf8(&input[0..i]) {
                            Ok(v) => {
                                IResult::Done(&input[(i+1)..], v)
                            },
                            Err(_) => {
                                IResult::Error(error_code!(ErrorKind::Custom(43)))
                            },
                        }
                    } else if i + 1 > input_len {
                        return IResult::Incomplete(Needed::Size(1));
                    }
            }
        }
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}


/* Parse the name part of the header -> &str */
named!(name<&str>,
       map_res!(is_not!(":\n"), from_utf8));

/* Parse a single header as a tuple -> (&str, Vec<&str>) */
named!(header<(&str, &str)>,
       separated_pair!(complete!(name), ws!(tag!(":")), complete!(header_value)));
/* Parse all headers -> Vec<(&str, Vec<&str>)> */
named!(pub headers<std::vec::Vec<(&str, &str)>>,
       many1!(complete!(header)));

named!(pub headers_raw<&[u8]>,
       take_until1!("\n\n"));

named!(pub body_raw<&[u8]>,
       do_parse!(
           take_until1!("\n\n") >>
           body: take_while!(call!(|_| true)) >>
           ( { body } )));


named!(pub mail<(std::vec::Vec<(&str, &str)>, &[u8])>,
       separated_pair!(headers, tag!("\n"), take_while!(call!(|_| true))));
named!(pub attachment<(std::vec::Vec<(&str, &str)>, &[u8])>,
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
use encoding::all::{ISO_8859_1,ISO_8859_2, ISO_8859_7, WINDOWS_1253, GBK};

fn encoded_word(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if input.len() < 5 {
        return IResult::Incomplete(Needed::Unknown);
    } else if input[0] != b'=' || input[1] != b'?' {
        return IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
    for tag in &["UTF-8", "iso-8859-7", "windows-1253", "iso-8859-1", "iso-8859-2", "gbk"] {
        if let CompareResult::Ok = (&input[2..]).compare_no_case(*tag) {
            let tag_len = tag.len();
            /* tag must end with ?_? where _ is either Q or B, eg: =?UTF-8?B? */
            if input[2+tag_len] != b'?' || input[2+tag_len+2] != b'?' {
                return IResult::Error(error_code!(ErrorKind::Custom(43)))
            }
            /* See if input ends with "?=" and get ending index */
            let mut encoded_idx = None;
            for i in (5+tag_len)..input.len() {
                if input[i] == b'?' && i < input.len() && input[i+1] == b'=' {
                    encoded_idx = Some(i);
                    break;
                }
            };
            if encoded_idx.is_none() {
                return IResult::Error(error_code!(ErrorKind::Custom(43)))
            }
            let encoded = &input[5+tag_len..encoded_idx.unwrap()];

            let s:Vec<u8> = match input[2+tag_len+1] {
                b'b' | b'B' => {
                    match base64::decode(encoded) {
                        Ok(v) => {
                            v
                        },
                        Err(_) => {
                            encoded.to_vec()
                        },
                    }
                },
                b'q' | b'Q' => {
                    match get_quoted_printed_bytes(encoded) {
                        IResult::Done(b"", s) => {
                            s
                        },
                        _ => {
                            return IResult::Error(error_code!(ErrorKind::Custom(43)))
                        },
                    }

                },
                _ => {
                    return IResult::Error(error_code!(ErrorKind::Custom(43)))
                },
            };

            match *tag {
                "UTF-8" => {
                    return IResult::Done(&input[encoded_idx.unwrap()+2..], s);
                },
                "iso-8859-7" => {
                    return if let Ok(v) = ISO_8859_7.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap()+2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                },
                "windows-1253" => {
                    return if let Ok(v) = WINDOWS_1253.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap()+2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                },
                "iso-8859-1" => {
                    return if let Ok(v) = ISO_8859_1.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap()+2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                },
                "iso-8859-2" => {
                    return if let Ok(v) = ISO_8859_2.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap()+2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                },
                "gbk" => {
                    return if let Ok(v) = GBK.decode(&s, DecoderTrap::Strict) {
                        IResult::Done(&input[encoded_idx.unwrap()+2..], v.into_bytes())
                    } else {
                        IResult::Error(error_code!(ErrorKind::Custom(43)))
                    }
                },
                _ => {
                    panic!();
                },
            }
        } else {
            continue;
        }
    }
    eprintln!("unknown tag is {:?}", from_utf8(&input[2..20]));
    IResult::Error(error_code!(ErrorKind::Custom(43)))
}

named!(qp_underscore_header<u8>,
       do_parse!(tag!("_") >> ( { b' ' } )));

named!(get_quoted_printed_bytes<Vec<u8>>, many0!(alt_complete!(quoted_printable_byte | qp_underscore_header | le_u8)));

named!(encoded_word_list<String>, ws!(do_parse!(
        list: separated_nonempty_list!(complete!(is_a!(" \n\r\t")), encoded_word) >>
        ( {
            let list_len = list.iter().fold(0, |mut acc, x| { acc+=x.len(); acc });
            let bytes = list.iter().fold(Vec::with_capacity(list_len), |mut acc, x| { acc.append(&mut x.clone()); acc});
            String::from_utf8_lossy(&bytes).into_owned()
        } )
       )));
named!(ascii_token<String>, do_parse!(
        word: alt!(terminated!(take_until1!("=?"), peek!(tag_no_case!("=?UTF-8?"))) | take_while!(call!(|_| { true }))) >>
        ( {
            String::from_utf8_lossy(word).into_owned()
        } )));

named!(pub subject<String>, ws!(do_parse!(
        list: many0!(alt_complete!( encoded_word_list | ascii_token)) >>
        ( {
            let string_len = list.iter().fold(0, |mut acc, x| { acc+=x.len(); acc }) + list.len() - 1;
            let list_len = list.len();
            let mut i = 0;
            list.iter().fold(String::with_capacity(string_len),
            |acc, x| { 
                let mut acc = acc + &x.replace("\n", ""); 
                if i != list_len - 1 {
                    acc.push_str(" ");
                    i+=1;
                }
                acc
            })
        } )

       )));

#[test]
fn test_subject() {
    let subject_s = "list.free.de mailing list memberships reminder".as_bytes();
    assert_eq!((&b""[..], "list.free.de mailing list memberships reminder".to_string()), subject(subject_s).unwrap());

    let subject_s = "=?UTF-8?B?zp3Orc6/IM+Az4HOv8+Dz4nPgM65zrrPjCDOvM6uzr3Phc68zrEgzrHPhs6v?= =?UTF-8?B?z4fOuM63?=".as_bytes();
    assert_eq!((&b""[..], "Νέο προσωπικό μήνυμα αφίχθη".to_string()), subject(subject_s).unwrap());

    let subject_s = "=?utf-8?B?bW9vZGxlOiDOsc69zrHPg866z4zPgM63z4POtyDOv868zqzOtM6xz4Igz4M=?=  =?utf-8?B?z4XOts63z4TOrs+DzrXPic69?=".as_bytes();
    assert_eq!((&b""[..], "moodle: ανασκόπηση ομάδας συζητήσεων".to_string()), subject(subject_s).unwrap());

    let subject_s = "=?UTF-8?B?zp3Orc6/IM+Az4HOv8+Dz4nPgM65zrrPjCDOvM6uzr3Phc68zrEgzrHPhs6v?=".as_bytes();
    assert_eq!("Νέο προσωπικό μήνυμα αφί".to_string(), from_utf8(&encoded_word(subject_s).to_full_result().unwrap()).unwrap());
    let subject_s = "=?UTF-8?Q?=CE=A0=CF=81=CF=8C=CF=83=CE=B8=CE=B5?=".as_bytes();
    assert_eq!("Πρόσθε".to_string(), from_utf8(&encoded_word(subject_s).to_full_result().unwrap()).unwrap());
    let subject_s = "=?iso-8859-7?B?UmU6INDx/OLr5+zhIOzlIPTn7SDh9fHp4e3eIOLh8eTp4Q==?=".as_bytes();
    assert_eq!("Re: Πρόβλημα με την αυριανή βαρδια".to_string(), from_utf8(&encoded_word(subject_s).to_full_result().unwrap()).unwrap());

    let subject_s = "=?UTF-8?Q?=CE=A0=CF=81=CF=8C=CF=83=CE=B8=CE=B5?=
 =?UTF-8?Q?=CF=84=CE=B7_=CE=B5=CE=BE=CE=B5=CF=84?=
 =?UTF-8?Q?=CE=B1=CF=83=CF=84=CE=B9=CE=BA=CE=AE?=".as_bytes();
    assert_eq!((&b""[..], "Πρόσθετη εξεταστική".to_string()), subject(subject_s).unwrap());
}
fn eat_comments(input: &str) -> String {
    let mut in_comment = false;
    input.chars().fold(String::with_capacity(input.len()), |mut acc, x| {
        if x == '(' && !in_comment {
            in_comment = true;
            acc
        } else if x == ')' && in_comment {
            in_comment = false;
            acc
        } else if in_comment {
            acc
        } else {
            acc.push(x); acc
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
pub fn date(input: &str) -> Option<chrono::DateTime<chrono::FixedOffset>> {
    let parsed_result = subject(eat_comments(input).as_bytes()).to_full_result().unwrap().replace("-", "+");
    chrono::DateTime::parse_from_rfc2822(parsed_result.trim()).ok()
}

#[test]
fn test_date() {
    let s = "Thu, 31 Aug 2017 13:43:37 +0000 (UTC)";
    let _s = "Thu, 31 Aug 2017 13:43:37 +0000";
    let __s = "=?utf-8?q?Thu=2C_31_Aug_2017_13=3A43=3A37_-0000?=";
    assert_eq!(date(s).unwrap(), date(_s).unwrap());
    assert_eq!(date(_s).unwrap(), date(__s).unwrap());
}

named!(pub message_id<&str>,
        map_res!(complete!(delimited!(tag!("<"), take_until1!(">"), tag!(">"))), from_utf8)
 );

fn message_id_peek(input: &[u8]) -> IResult<&[u8],&str> {
    let input_length = input.len();
    if input.is_empty()  {
        IResult::Incomplete(Needed::Size(1))
    } else if input_length == 2 || input[0] != b'<'  {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    } else {
        for (i, &x) in input.iter().take(input_length).enumerate().skip(1) {
            if x == b'>' {
                return IResult::Done(&input[i+1..], from_utf8(&input[0..i+1]).unwrap());
            }
        }
        IResult::Incomplete(Needed::Unknown)
    }
}

named!(pub references<Vec<&str>>, separated_list!(complete!(is_a!(" \n\t\r")), message_id_peek));

named_args!(pub attachments<'a>(boundary: &'a str, boundary_end: &'a str) < Vec<&'this_is_probably_unique_i_hope_please [u8]> >,
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
    let _ = std::fs::File::open("test/attachment_test").unwrap().read_to_end(&mut buffer);
    let boundary = "--b1_4382d284f0c601a737bb32aaeda53160--";
    let boundary_len = boundary.len();
    let (_, body) = match mail(&buffer).to_full_result() {
        Ok(v) => v,
        Err(_) => { panic!() }
     };
    let attachments = attachments(body, &boundary[0..boundary_len-2], &boundary).to_full_result().unwrap();
    assert_eq!(attachments.len(), 4);
}

named!(content_type_parameter< (&str, &str) >,
       do_parse!(
           tag!(";") >>
           name: terminated!(map_res!(ws!(take_until!("=")), from_utf8), tag!("=")) >>
           value: map_res!(ws!(
               alt_complete!(delimited!(tag!("\""), take_until!("\""), tag!("\"")) | is_not!(";"))),
               from_utf8) >>
           ( {
               (name, value)
           } )
           ));


named!(pub content_type< (&str, &str, Vec<(&str, &str)>) >,
       do_parse!(
           _type: map_res!(take_until!("/"), from_utf8) >>
           tag!("/") >>
           _subtype: map_res!(is_not!(";"), from_utf8) >>
           parameters: many0!(complete!(content_type_parameter)) >>
           ( {
               (_type, _subtype, parameters)
           } )
           ));


named!(pub quoted_printable_text<String>,
   do_parse!(
       bytes: many0!(alt_complete!(
               preceded!(tag!("=\n"), quoted_printable_byte) |
               preceded!(tag!("=\n"), le_u8) |
               quoted_printable_byte |
               le_u8)) >>
       ( {
           String::from_utf8_lossy(&bytes).into_owned()
       } )
   )
);
