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

use super::{ImapConnection, ImapProtocol, ImapServerConf, UIDStore};
use crate::conf::AccountSettings;
use crate::error::{MeliError, Result};
use crate::get_conf_val;
use nom::{
    branch::alt, bytes::complete::tag, combinator::map, error::Error as NomError, error::ErrorKind,
    multi::separated_list1, sequence::separated_pair, IResult,
};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

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

#[test]
fn test_managesieve_capabilities() {
    assert_eq!(managesieve_capabilities(b"\"IMPLEMENTATION\" \"Dovecot Pigeonhole\"\r\n\"SIEVE\" \"fileinto reject envelope encoded-character vacation subaddress comparator-i;ascii-numeric relational regex imap4flags copy include variables body enotify environment mailbox date index ihave duplicate mime foreverypart extracttext\"\r\n\"NOTIFY\" \"mailto\"\r\n\"SASL\" \"PLAIN\"\r\n\"STARTTLS\"\r\n\"VERSION\" \"1.0\"\r\n").unwrap(), vec![
        (&b"IMPLEMENTATION"[..],&b"Dovecot Pigeonhole"[..]),
        (&b"SIEVE"[..],&b"fileinto reject envelope encoded-character vacation subaddress comparator-i;ascii-numeric relational regex imap4flags copy include variables body enotify environment mailbox date index ihave duplicate mime foreverypart extracttext"[..]),
        (&b"NOTIFY"[..],&b"mailto"[..]),
        (&b"SASL"[..],&b"PLAIN"[..]),
        (&b"STARTTLS"[..], &b""[..]),
        (&b"VERSION"[..],&b"1.0"[..])]

    );
}

// Return a byte sequence surrounded by "s and decoded if necessary
pub fn quoted_raw(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() || input[0] != b'"' {
        return Err(nom::Err::Error(NomError {
            input,
            code: ErrorKind::Tag,
        }));
    }

    let mut i = 1;
    while i < input.len() {
        if input[i] == b'\"' && input[i - 1] != b'\\' {
            return Ok((&input[i + 1..], &input[1..i]));
        }
        i += 1;
    }

    Err(nom::Err::Error(NomError {
        input,
        code: ErrorKind::Tag,
    }))
}

pub trait ManageSieve {
    fn havespace(&mut self) -> Result<()>;
    fn putscript(&mut self) -> Result<()>;

    fn listscripts(&mut self) -> Result<()>;
    fn setactive(&mut self) -> Result<()>;

    fn getscript(&mut self) -> Result<()>;

    fn deletescript(&mut self) -> Result<()>;
    fn renamescript(&mut self) -> Result<()>;
}

pub fn new_managesieve_connection(
    account_hash: crate::backends::AccountHash,
    account_name: String,
    s: &AccountSettings,
    event_consumer: crate::backends::BackendEventConsumer,
) -> Result<ImapConnection> {
    let server_hostname = get_conf_val!(s["server_hostname"])?;
    let server_username = get_conf_val!(s["server_username"])?;
    let server_password = get_conf_val!(s["server_password"])?;
    let server_port = get_conf_val!(s["server_port"], 4190)?;
    let danger_accept_invalid_certs: bool = get_conf_val!(s["danger_accept_invalid_certs"], false)?;
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
            Err(MeliError::new("Account is uninitialised.")),
        ))),
        ..UIDStore::new(
            account_hash,
            Arc::new(account_name),
            event_consumer,
            server_conf.timeout,
        )
    });
    Ok(ImapConnection::new_connection(&server_conf, uid_store))
}

impl ManageSieve for ImapConnection {
    fn havespace(&mut self) -> Result<()> {
        Ok(())
    }
    fn putscript(&mut self) -> Result<()> {
        Ok(())
    }

    fn listscripts(&mut self) -> Result<()> {
        Ok(())
    }
    fn setactive(&mut self) -> Result<()> {
        Ok(())
    }

    fn getscript(&mut self) -> Result<()> {
        Ok(())
    }

    fn deletescript(&mut self) -> Result<()> {
        Ok(())
    }
    fn renamescript(&mut self) -> Result<()> {
        Ok(())
    }
}
