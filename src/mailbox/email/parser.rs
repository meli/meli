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

fn quoted_printable_byte(input: &[u8]) -> IResult<&[u8],u8> {
    if input.is_empty() || input.len() < 3 {
        IResult::Incomplete(Needed::Size(1))
    } else if input[0] != b'=' {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    } else if is_hex_digit(input[1]) && is_hex_digit(input[2]) {
        let a = if input[1] < b':' {
            input[1] - 48
        } else {
            input[1] - 55
        };
        let b = if input[2] < b':' {
            input[2] - 48
        } else {
            input[2] - 55
        };

        IResult::Done(&input[3..], a*16+b)
    } else {
        IResult::Error(error_code!(ErrorKind::Custom(43)))
    }
}

/*
named!(quoted_printable_byte<u8>, do_parse!(
        p: map_res!(preceded!(tag!("="), verify!(complete!(take!(2)), |s: &[u8]| is_hex_digit(s[0]) && is_hex_digit(s[1]) )), from_utf8) >>
        ( {
            u8::from_str_radix(p, 16).unwrap()
        } )));
        */


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
        IResult::Incomplete(Needed::Size(1))
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
named!(headers<std::vec::Vec<(&str, &str)>>,
       many1!(complete!(header)));

named!(pub mail<(std::vec::Vec<(&str, &str)>, &[u8])>,
       separated_pair!(headers, tag!("\n"), take_while!(call!(|_| { true }))));
named!(pub attachment<(std::vec::Vec<(&str, &str)>, &[u8])>,
       do_parse!(
            opt!(is_a!(" \n\t\r")) >>
       pair: pair!(many0!(complete!(header)), take_while!(call!(|_| { true }))) >>
       ( { pair } )));

/* Header parsers */

/* Encoded words
 *"=?charset?encoding?encoded text?=".
 */
named!(utf8_token_base64<Vec<u8>>, do_parse!(
        encoded: complete!(delimited!(tag_no_case!("=?UTF-8?B?"), take_until1!("?="), tag!("?="))) >>
        ( {
            match base64::decode(encoded) {
                Ok(v) => {
                    v
                },
                Err(_) => {
                    encoded.to_vec()
                },
            }
        } )
        ));

named!(utf8_token_quoted_p_raw<&[u8], &[u8]>,
       complete!(delimited!(tag_no_case!("=?UTF-8?q?"), take_until1!("?="), tag!("?="))));

//named!(utf8_token_quoted_p<String>, escaped_transform!(call!(alpha), '=', quoted_printable_byte));

named!(qp_underscore_header<u8>,
       do_parse!(tag!("_") >> ( { b' ' } )));

named!(utf8_token_quoted_p<Vec<u8>>, do_parse!(
        raw: call!(utf8_token_quoted_p_raw) >>
        ( {
            named!(get_bytes<Vec<u8>>, many0!(alt_complete!(quoted_printable_byte | qp_underscore_header | le_u8)));
            get_bytes(raw).to_full_result().unwrap()
        } )));

named!(utf8_token<Vec<u8>>, alt_complete!(
        utf8_token_base64 |
        call!(utf8_token_quoted_p)));

named!(utf8_token_list<String>, ws!(do_parse!(
        list: separated_nonempty_list!(complete!(is_a!(" \n\r\t")), utf8_token) >>
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

/* Lots of copying here. TODO: fix it */
named!(pub subject<String>, ws!(do_parse!(
        list: many0!(alt_complete!( utf8_token_list | ascii_token)) >>
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

named!(pub references<Vec<&str>>, many0!(preceded!(is_not!("<"), message_id)));

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
