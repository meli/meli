//use memmap::{Mmap, Protection};
use std;
use base64;
use chrono;
use nom::le_u8;

/* Wow this sucks! */
named!(quoted_printable_byte<u8>, do_parse!(
        p: map_res!(preceded!(tag!("="), verify!(complete!(take!(2)), |s: &[u8]| { ::nom::is_hex_digit(s[0]) && ::nom::is_hex_digit(s[1])  })), std::str::from_utf8) >>
        ( {
            u8::from_str_radix(p, 16).unwrap()
        } )));


// Parser definition

/* A header can span multiple lines, eg:
 *
 * Received: from -------------------- (-------------------------)
 * 	by --------------------- (--------------------- [------------------]) (-----------------------)
 * 	with ESMTP id ------------ for <------------------->;
 * 	Tue,  5 Jan 2016 21:30:44 +0100 (CET)
 */

/*
 * if a header value is a Vec<&str>, this is the tail of that Vector
 */
named!(valuelist<&str>, 
       map_res!(delimited!(alt_complete!(tag!("\t") | tag!(" ")), take_until!("\n"), tag!("\n")), std::str::from_utf8)
       );

/* Parse the value part of the header -> Vec<&str> */
named!(value<Vec<&str>>,
       do_parse!(
        head: map_res!(terminated!(take_until!("\n"), tag!("\n")), std::str::from_utf8) >>
        tail: many0!(valuelist) >>
        ( {
            let tail_len = tail.len();
            let tail: Vec<&str> = tail.iter().map(|v| { v.trim()}).collect();
            let mut result = Vec::with_capacity(1 + tail.len());
            result.push(head.trim());
            if tail_len == 1 && tail[0] == "" {
                result
            } else {
                tail.iter().fold(result, |mut acc, x| { acc.push(x); acc})
            }
        } )
        ));

/* Parse the name part of the header -> &str */
named!(name<&str>, 
       terminated!(verify!(map_res!(take_until1!(":"), std::str::from_utf8), | v: &str | { !v.contains("\n")} ), tag!(":")));

/* Parse a single header as a tuple -> (&str, Vec<&str>) */
named!(header<(&str, std::vec::Vec<&str>)>, 
       pair!(complete!(name), complete!(value)));
/* Parse all headers -> Vec<(&str, Vec<&str>)> */
named!(headers<std::vec::Vec<(&str, std::vec::Vec<&str>)>>,
       many1!(complete!(header)));

named!(pub mail<(std::vec::Vec<(&str, std::vec::Vec<&str>)>, &[u8])>, 
       separated_pair!(headers, tag!("\n"), take_while!(call!(|_| { true }))));
named!(pub attachment<(std::vec::Vec<(&str, std::vec::Vec<&str>)>, &[u8])>, 
       do_parse!(
            opt!(is_a!(" \n\t\r")) >>
       pair: pair!(many0!(complete!(header)), take_while!(call!(|_| { true }))) >>
       ( { pair } )));

/* try chrono parse_from_str with several formats 
 * https://docs.rs/chrono/0.4.0/chrono/struct.DateTime.html#method.parse_from_str
 */

/* Header parsers */

/* Encoded words
 *"=?charset?encoding?encoded text?=".
 */
named!(utf8_token_base64<String>, do_parse!(
        encoded: complete!(delimited!(tag_no_case!("=?UTF-8?B?"), take_until1!("?="), tag!("?="))) >>
        ( {
            match base64::decode(encoded) {
                Ok(ref v) => { String::from_utf8_lossy(v).into_owned()
                },
                Err(_) => { String::from_utf8_lossy(encoded).into_owned() }
            }
        } )
        ));

named!(utf8_token_quoted_p_raw<&[u8], &[u8]>, 
       complete!(delimited!(tag_no_case!("=?UTF-8?q?"), take_until1!("?="), tag!("?="))));

//named!(utf8_token_quoted_p<String>, escaped_transform!(call!(alpha), '=', quoted_printable_byte));

named!(utf8_token_quoted_p<String>, do_parse!(
        raw: call!(utf8_token_quoted_p_raw) >>
        ( {
            named!(get_bytes<Vec<u8>>, dbg!(many0!(alt!(quoted_printable_byte | le_u8))));
            let bytes = get_bytes(raw).to_full_result().unwrap();
            String::from_utf8_lossy(&bytes).into_owned()
        } )));

named!(utf8_token<String>, alt_complete!(
        utf8_token_base64 |
        call!(utf8_token_quoted_p)));

named!(utf8_token_list<String>, ws!(do_parse!(
        list: separated_nonempty_list!(complete!(tag!(" ")), utf8_token) >>
        ( { 
            let list_len = list.iter().fold(0, |mut acc, x| { acc+=x.len(); acc });
            list.iter().fold(String::with_capacity(list_len), |mut acc, x| { acc.push_str(x); acc})
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
            let list_len = list.iter().fold(0, |mut acc, x| { acc+=x.len(); acc });
            let s = list.iter().fold(String::with_capacity(list_len), |mut acc, x| { acc.push_str(x); acc.push_str(" "); acc});
            s.trim().to_string() 
        } )

       )));

#[test]
fn test_subject() {
    let subject_s = "list.free.de mailing list memberships reminder".as_bytes();
    assert_eq!((&b""[..], "list.free.de mailing list memberships reminder".to_string()), subject(subject_s).unwrap());
    let subject_s = "=?UTF-8?B?zp3Orc6/IM+Az4HOv8+Dz4nPgM65zrrPjCDOvM6uzr3Phc68zrEgzrHPhs6v?= =?UTF-8?B?z4fOuM63?=".as_bytes();
    assert_eq!((&b""[..], "Νέο προσωπικό μήνυμα αφίχθη".to_string()), subject(subject_s).unwrap());
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
/* Date should tokenize input and convert the tokens, right now we expect input will have no extra
 * spaces in between tokens */
pub fn date(input: &str) -> Option<chrono::DateTime<chrono::FixedOffset>> {
    chrono::DateTime::parse_from_rfc2822(eat_comments(input).trim()).ok()
}

#[test]
fn test_date() {
    let s = "Thu, 31 Aug 2017 13:43:37 +0000 (UTC)";
    let _s = "Thu, 31 Aug 2017 13:43:37 +0000";
    assert_eq!(date(s).unwrap(), date(_s).unwrap());
}

named!(pub message_id<&str>,
        map_res!(complete!(delimited!(tag!("<"), take_until1!(">"), tag!(">"))), std::str::from_utf8) 
 );

named!(pub references<Vec<&str>>, many0!(preceded!(is_not!("<"), message_id)));

named_args!(pub attachments<'a>(boundary: &'a str, boundary_end: &'a str) < Vec<&'this_is_probably_unique_i_hope_please [u8]> >, 
            dbg!(alt_complete!(do_parse!(
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
                    )));
#[test]
fn test_attachments() {
    use std::io::Read;
    let mut buffer: Vec<u8> = Vec::new();
    let _ = std::fs::File::open("test/attachment_test").unwrap().read_to_end(&mut buffer);
    let boundary = "--b1_4382d284f0c601a737bb32aaeda53160";
    let (_, body) = match mail(&buffer).to_full_result() {
        Ok(v) => v,
        Err(_) => { panic!() }
     };
    //eprintln!("{:?}",std::str::from_utf8(body));
    let attachments = attachments(body, boundary).to_full_result().unwrap();
    assert_eq!(attachments.len(), 4);
}


named!(pub content_type< (&str, &str, Vec<(&str, &str)>) >, 
       do_parse!(
           _type: map_res!(take_until!("/"), std::str::from_utf8) >>
           tag!("/") >>
           _subtype: map_res!(is_not!(";"), std::str::from_utf8) >>
           parameters: many0!(preceded!(tag!(";"), pair!(
                       terminated!(map_res!(ws!(take_until!("=")), std::str::from_utf8), tag!("=")),
                       map_res!(ws!(alt_complete!(
                                   delimited!(tag!("\""), take_until!("\""), tag!("\"")) | is_not!(";")
                                   )), std::str::from_utf8)))) >>
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
