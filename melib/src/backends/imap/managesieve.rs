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

use super::{ImapConnection, ImapProtocol, ImapServerConf};
use crate::conf::AccountSettings;
use crate::error::{MeliError, Result};
use crate::get_conf_val;
use nom::IResult;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::Instant;

named!(
    pub managesieve_capabilities<Vec<(&[u8], &[u8])>>,
        do_parse!(
            ret: separated_nonempty_list_complete!(tag!(b"\r\n"), alt_complete!(separated_pair!(quoted_raw, tag!(b" "), quoted_raw) | map!(quoted_raw, |q| (q, &b""[..]))))
        >> opt!(tag!("\r\n"))
        >> ({ ret })
        )
    );

#[test]
fn test_managesieve_capabilities() {
    assert_eq!(managesieve_capabilities(b"\"IMPLEMENTATION\" \"Dovecot Pigeonhole\"\r\n\"SIEVE\" \"fileinto reject envelope encoded-character vacation subaddress comparator-i;ascii-numeric relational regex imap4flags copy include variables body enotify environment mailbox date index ihave duplicate mime foreverypart extracttext\"\r\n\"NOTIFY\" \"mailto\"\r\n\"SASL\" \"PLAIN\"\r\n\"STARTTLS\"\r\n\"VERSION\" \"1.0\"\r\n").to_full_result(), Ok(vec![
(&b"IMPLEMENTATION"[..],&b"Dovecot Pigeonhole"[..]),
(&b"SIEVE"[..],&b"fileinto reject envelope encoded-character vacation subaddress comparator-i;ascii-numeric relational regex imap4flags copy include variables body enotify environment mailbox date index ihave duplicate mime foreverypart extracttext"[..]),
(&b"NOTIFY"[..],&b"mailto"[..]),
(&b"SASL"[..],&b"PLAIN"[..]),
(&b"STARTTLS"[..], &b""[..]),
(&b"VERSION"[..],&b"1.0"[..])])

);
}

// Return a byte sequence surrounded by "s and decoded if necessary
pub fn quoted_raw(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() || input[0] != b'"' {
        return IResult::Error(nom::ErrorKind::Custom(0));
    }

    let mut i = 1;
    while i < input.len() {
        if input[i] == b'\"' && input[i - 1] != b'\\' {
            return IResult::Done(&input[i + 1..], &input[1..i]);
        }
        i += 1;
    }

    return IResult::Error(nom::ErrorKind::Custom(0));
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

pub fn new_managesieve_connection(s: &AccountSettings) -> Result<ImapConnection> {
    let server_hostname = get_conf_val!(s["server_hostname"])?;
    let server_username = get_conf_val!(s["server_username"])?;
    let server_password = get_conf_val!(s["server_password"])?;
    let server_port = get_conf_val!(s["server_port"], 4190)?;
    let danger_accept_invalid_certs: bool = get_conf_val!(s["danger_accept_invalid_certs"], false)?;
    let server_conf = ImapServerConf {
        server_hostname: server_hostname.to_string(),
        server_username: server_username.to_string(),
        server_password: server_password.to_string(),
        server_port,
        use_starttls: true,
        danger_accept_invalid_certs,
        protocol: ImapProtocol::ManageSieve,
    };
    let online = Arc::new(Mutex::new((
        Instant::now(),
        Err(MeliError::new("Account is uninitialised.")),
    )));
    Ok(ImapConnection::new_connection(&server_conf, online))
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
