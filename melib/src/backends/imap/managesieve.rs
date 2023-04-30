/*
 * meli - managesieve
 *
 * Copyright 2020 Manos Pitsidianakis
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

use std::{
    str::FromStr,
    sync::{Arc, Mutex},
    time::SystemTime,
};

use nom::{
    branch::alt, bytes::complete::tag, combinator::map, multi::separated_list1,
    sequence::separated_pair,
};

use super::{ImapConnection, ImapProtocol, ImapServerConf, UIDStore};
use crate::{
    conf::AccountSettings,
    email::parser::IResult,
    error::{Error, Result},
    get_conf_val,
    imap::RequiredResponses,
};

pub struct ManageSieveConnection {
    pub inner: ImapConnection,
}

pub fn managesieve_capabilities(input: &[u8]) -> Result<Vec<(&[u8], &[u8])>> {
    let (_, ret) = separated_list1(
        tag(b"\r\n"),
        alt((
            separated_pair(quoted_raw, tag(b" "), quoted_raw),
            map(quoted_raw, |q| (q, &b""[..])),
        )),
    )(input)?;
    Ok(ret)
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ManageSieveResponse<'a> {
    Ok {
        code: Option<&'a [u8]>,
        message: Option<&'a [u8]>,
    },
    NoBye {
        code: Option<&'a [u8]>,
        message: Option<&'a [u8]>,
    },
}

mod parser {
    use nom::{
        bytes::complete::tag,
        character::complete::crlf,
        combinator::{iterator, map, opt},
    };
    pub use nom::{
        bytes::complete::{is_not, tag_no_case},
        sequence::{delimited, pair, preceded, terminated},
    };

    use super::*;

    pub fn sieve_name(input: &[u8]) -> IResult<&[u8], &[u8]> {
        crate::backends::imap::protocol_parser::string_token(input)
    }

    // *(sieve-name [SP "ACTIVE"] CRLF)
    // response-oknobye
    pub fn listscripts(input: &[u8]) -> IResult<&[u8], Vec<(&[u8], bool)>> {
        let mut it = iterator(
            input,
            alt((
                terminated(
                    map(terminated(sieve_name, tag_no_case(b" ACTIVE")), |r| {
                        (r, true)
                    }),
                    crlf,
                ),
                terminated(map(sieve_name, |r| (r, false)), crlf),
            )),
        );

        let parsed = (&mut it).collect::<Vec<(&[u8], bool)>>();
        let res: IResult<_, _> = it.finish();
        let (rest, _) = res?;
        Ok((rest, parsed))
    }

    // response-getscript    = (sieve-script CRLF response-ok) /
    //                      response-nobye
    pub fn getscript(input: &[u8]) -> IResult<&[u8], &[u8]> {
        sieve_name(input)
    }

    pub fn response_oknobye(input: &[u8]) -> IResult<&[u8], ManageSieveResponse> {
        alt((
            map(
                terminated(
                    pair(
                        preceded(
                            tag_no_case(b"ok"),
                            opt(preceded(
                                tag(b" "),
                                delimited(tag(b"("), is_not(")"), tag(b")")),
                            )),
                        ),
                        opt(preceded(tag(b" "), sieve_name)),
                    ),
                    crlf,
                ),
                |(code, message)| ManageSieveResponse::Ok { code, message },
            ),
            map(
                terminated(
                    pair(
                        preceded(
                            alt((tag_no_case(b"no"), tag_no_case(b"bye"))),
                            opt(preceded(
                                tag(b" "),
                                delimited(tag(b"("), is_not(")"), tag(b")")),
                            )),
                        ),
                        opt(preceded(tag(b" "), sieve_name)),
                    ),
                    crlf,
                ),
                |(code, message)| ManageSieveResponse::NoBye { code, message },
            ),
        ))(input)
    }

    #[test]
    fn test_managesieve_listscripts() {
        let input_1 = b"\"summer_script\"\r\n\"vacation_script\"\r\n{13}\r\nclever\"script\r\n\"main_script\" ACTIVE\r\nOK";
        assert_eq!(
            terminated(listscripts, tag_no_case(b"OK"))(input_1),
            Ok((
                &b""[..],
                vec![
                    (&b"summer_script"[..], false),
                    (&b"vacation_script"[..], false),
                    (&b"clever\"script"[..], false),
                    (&b"main_script"[..], true)
                ]
            ))
        );

        let input_2 = b"\"summer_script\"\r\n\"main_script\" active\r\nok";
        assert_eq!(
            terminated(listscripts, tag_no_case(b"OK"))(input_2),
            Ok((
                &b""[..],
                vec![(&b"summer_script"[..], false), (&b"main_script"[..], true)]
            ))
        );
        let input_3 = b"ok";
        assert_eq!(
            terminated(listscripts, tag_no_case(b"OK"))(input_3),
            Ok((&b""[..], vec![]))
        );
    }

    #[test]
    fn test_managesieve_general() {
        assert_eq!(managesieve_capabilities(b"\"IMPLEMENTATION\" \"Dovecot Pigeonhole\"\r\n\"SIEVE\" \"fileinto reject envelope encoded-character vacation subaddress comparator-i;ascii-numeric relational regex imap4flags copy include variables body enotify environment mailbox date index ihave duplicate mime foreverypart extracttext\"\r\n\"NOTIFY\" \"mailto\"\r\n\"SASL\" \"PLAIN\"\r\n\"STARTTLS\"\r\n\"VERSION\" \"1.0\"\r\n").unwrap(), vec![
            (&b"IMPLEMENTATION"[..],&b"Dovecot Pigeonhole"[..]),
            (&b"SIEVE"[..],&b"fileinto reject envelope encoded-character vacation subaddress comparator-i;ascii-numeric relational regex imap4flags copy include variables body enotify environment mailbox date index ihave duplicate mime foreverypart extracttext"[..]),
            (&b"NOTIFY"[..],&b"mailto"[..]),
            (&b"SASL"[..],&b"PLAIN"[..]),
            (&b"STARTTLS"[..], &b""[..]),
            (&b"VERSION"[..],&b"1.0"[..])]

        );

        let response_ok = b"OK (WARNINGS) \"line 8: server redirect action limit is 2, this redirect might be ignored\"\r\n";
        assert_eq!(
                response_oknobye(response_ok),
                Ok((
                    &b""[..],
                    ManageSieveResponse::Ok {
                        code: Some(&b"WARNINGS"[..]),
                        message: Some(&b"line 8: server redirect action limit is 2, this redirect might be ignored"[..]),
                    }
                ))
            );
        let response_ok = b"OK (WARNINGS)\r\n";
        assert_eq!(
            response_oknobye(response_ok),
            Ok((
                &b""[..],
                ManageSieveResponse::Ok {
                    code: Some(&b"WARNINGS"[..]),
                    message: None,
                }
            ))
        );
        let response_ok =
            b"OK \"line 8: server redirect action limit is 2, this redirect might be ignored\"\r\n";
        assert_eq!(
                response_oknobye(response_ok),
                Ok((
                    &b""[..],
                    ManageSieveResponse::Ok {
                        code: None,
                        message: Some(&b"line 8: server redirect action limit is 2, this redirect might be ignored"[..]),
                    }
                ))
            );
        let response_ok = b"Ok\r\n";
        assert_eq!(
            response_oknobye(response_ok),
            Ok((
                &b""[..],
                ManageSieveResponse::Ok {
                    code: None,
                    message: None,
                }
            ))
        );

        let response_nobye = b"No (NONEXISTENT) \"There is no script by that name\"\r\n";
        assert_eq!(
            response_oknobye(response_nobye),
            Ok((
                &b""[..],
                ManageSieveResponse::NoBye {
                    code: Some(&b"NONEXISTENT"[..]),
                    message: Some(&b"There is no script by that name"[..]),
                }
            ))
        );
        let response_nobye = b"No (NONEXISTENT) {31}\r\nThere is no script by that name\r\n";
        assert_eq!(
            response_oknobye(response_nobye),
            Ok((
                &b""[..],
                ManageSieveResponse::NoBye {
                    code: Some(&b"NONEXISTENT"[..]),
                    message: Some(&b"There is no script by that name"[..]),
                }
            ))
        );

        let response_nobye = b"No\r\n";
        assert_eq!(
            response_oknobye(response_nobye),
            Ok((
                &b""[..],
                ManageSieveResponse::NoBye {
                    code: None,
                    message: None,
                }
            ))
        );
    }
}

// Return a byte sequence surrounded by "s and decoded if necessary
pub fn quoted_raw(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() || input[0] != b'"' {
        return Err(nom::Err::Error((input, "empty").into()));
    }

    let mut i = 1;
    while i < input.len() {
        if input[i] == b'\"' && input[i - 1] != b'\\' {
            return Ok((&input[i + 1..], &input[1..i]));
        }
        i += 1;
    }

    Err(nom::Err::Error((input, "no quotes").into()))
}

impl ManageSieveConnection {
    pub fn new(
        account_hash: crate::backends::AccountHash,
        account_name: String,
        s: &AccountSettings,
        event_consumer: crate::backends::BackendEventConsumer,
    ) -> Result<Self> {
        let server_hostname = get_conf_val!(s["server_hostname"])?;
        let server_username = get_conf_val!(s["server_username"])?;
        let server_password = get_conf_val!(s["server_password"])?;
        let server_port = get_conf_val!(s["server_port"], 4190)?;
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        let timeout = get_conf_val!(s["timeout"], 16_u64)?;
        let timeout = if timeout == 0 {
            None
        } else {
            Some(std::time::Duration::from_secs(timeout))
        };
        let server_conf = ImapServerConf {
            server_hostname: server_hostname.to_string(),
            server_username: server_username.to_string(),
            server_password: server_password.to_string(),
            server_port,
            use_starttls: true,
            use_tls: true,
            danger_accept_invalid_certs,
            protocol: ImapProtocol::ManageSieve,
            timeout,
        };
        let uid_store = Arc::new(UIDStore {
            is_online: Arc::new(Mutex::new((
                SystemTime::now(),
                Err(Error::new("Account is uninitialised.")),
            ))),
            ..UIDStore::new(
                account_hash,
                Arc::new(account_name),
                event_consumer,
                server_conf.timeout,
            )
        });
        Ok(Self {
            inner: ImapConnection::new_connection(&server_conf, uid_store),
        })
    }

    pub async fn havespace(&mut self) -> Result<()> {
        Ok(())
    }

    pub async fn putscript(&mut self, script_name: &[u8], script: &[u8]) -> Result<()> {
        let mut ret = Vec::new();
        self.inner
            .send_literal(format!("Putscript {{{len}+}}\r\n", len = script_name.len()).as_bytes())
            .await?;
        self.inner.send_literal(script_name).await?;
        self.inner
            .send_literal(format!(" {{{len}+}}\r\n", len = script.len()).as_bytes())
            .await?;
        self.inner.send_literal(script).await?;
        self.inner
            .read_response(&mut ret, RequiredResponses::empty())
            .await?;
        let (_rest, response) = parser::response_oknobye(&ret)?;
        match response {
            ManageSieveResponse::Ok { .. } => Ok(()),
            ManageSieveResponse::NoBye { code, message } => Err(format!(
                "Could not upload script: {} {}",
                code.map(String::from_utf8_lossy).unwrap_or_default(),
                message.map(String::from_utf8_lossy).unwrap_or_default()
            )
            .into()),
        }
    }

    pub async fn listscripts(&mut self) -> Result<Vec<(Vec<u8>, bool)>> {
        let mut ret = Vec::new();
        self.inner.send_command(b"Listscripts").await?;
        self.inner
            .read_response(&mut ret, RequiredResponses::empty())
            .await?;
        let (_rest, scripts) =
            parser::terminated(parser::listscripts, parser::tag_no_case(b"OK"))(&ret)?;
        Ok(scripts
            .into_iter()
            .map(|(n, a)| (n.to_vec(), a))
            .collect::<Vec<(Vec<u8>, bool)>>())
    }

    pub async fn checkscript(&mut self, script: &[u8]) -> Result<()> {
        let mut ret = Vec::new();
        self.inner
            .send_literal(format!("Checkscript {{{len}+}}\r\n", len = script.len()).as_bytes())
            .await?;
        self.inner.send_literal(script).await?;
        self.inner
            .read_response(&mut ret, RequiredResponses::empty())
            .await?;
        let (_rest, response) = parser::response_oknobye(&ret)?;
        match response {
            ManageSieveResponse::Ok { .. } => Ok(()),
            ManageSieveResponse::NoBye { code, message } => Err(format!(
                "Checkscript reply: {} {}",
                code.map(String::from_utf8_lossy).unwrap_or_default(),
                message.map(String::from_utf8_lossy).unwrap_or_default()
            )
            .into()),
        }
    }

    pub async fn setactive(&mut self, script_name: &[u8]) -> Result<()> {
        let mut ret = Vec::new();
        self.inner
            .send_literal(format!("Setactive {{{len}+}}\r\n", len = script_name.len()).as_bytes())
            .await?;
        self.inner.send_literal(script_name).await?;
        self.inner
            .read_response(&mut ret, RequiredResponses::empty())
            .await?;
        let (_rest, response) = parser::response_oknobye(&ret)?;
        match response {
            ManageSieveResponse::Ok { .. } => Ok(()),
            ManageSieveResponse::NoBye { code, message } => Err(format!(
                "Could not set active script: {} {}",
                code.map(String::from_utf8_lossy).unwrap_or_default(),
                message.map(String::from_utf8_lossy).unwrap_or_default()
            )
            .into()),
        }
    }

    pub async fn getscript(&mut self, script_name: &[u8]) -> Result<Vec<u8>> {
        let mut ret = Vec::new();
        self.inner
            .send_literal(format!("Getscript {{{len}+}}\r\n", len = script_name.len()).as_bytes())
            .await?;
        self.inner.send_literal(script_name).await?;
        self.inner
            .read_response(&mut ret, RequiredResponses::empty())
            .await?;
        if let Ok((_, ManageSieveResponse::NoBye { code, message })) =
            parser::response_oknobye(&ret)
        {
            return Err(format!(
                "Could not set active script: {} {}",
                code.map(String::from_utf8_lossy).unwrap_or_default(),
                message.map(String::from_utf8_lossy).unwrap_or_default()
            )
            .into());
        }
        let (_rest, script) =
            parser::terminated(parser::getscript, parser::tag_no_case(b"OK"))(&ret)?;
        Ok(script.to_vec())
    }

    pub async fn deletescript(&mut self, script_name: &[u8]) -> Result<()> {
        let mut ret = Vec::new();
        self.inner
            .send_literal(
                format!("Deletescript {{{len}+}}\r\n", len = script_name.len()).as_bytes(),
            )
            .await?;
        self.inner.send_literal(script_name).await?;
        self.inner
            .read_response(&mut ret, RequiredResponses::empty())
            .await?;
        let (_rest, response) = parser::response_oknobye(&ret)?;
        match response {
            ManageSieveResponse::Ok { .. } => Ok(()),
            ManageSieveResponse::NoBye { code, message } => Err(format!(
                "Could not delete script: {} {}",
                code.map(String::from_utf8_lossy).unwrap_or_default(),
                message.map(String::from_utf8_lossy).unwrap_or_default()
            )
            .into()),
        }
    }

    pub async fn renamescript(&mut self) -> Result<()> {
        Ok(())
    }
}
