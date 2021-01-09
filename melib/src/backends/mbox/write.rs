/*
 * meli - mailbox module.
 *
 * Copyright 2021  Manos Pitsidianakis
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

impl MboxFormat {
    pub fn append(
        &self,
        writer: &mut dyn std::io::Write,
        input: &[u8],
        envelope_from: Option<&Address>,
        delivery_date: Option<crate::UnixTimestamp>,
        is_empty: bool,
        crlf: bool,
    ) -> Result<()> {
        let line_ending: &'static [u8] = if crlf { &b"\r\n"[..] } else { &b"\n"[..] };
        if !is_empty {
            writer.write_all(line_ending)?;
            writer.write_all(line_ending)?;
        }
        writer.write_all(&b"From "[..])?;
        if let Some(from) = envelope_from {
            writer.write_all(from.address_spec_raw())?;
        } else {
            writer.write_all(&b"MAILER-DAEMON"[..])?;
        }
        writer.write_all(&b" "[..])?;
        writer.write_all(
            crate::datetime::timestamp_to_string(
                delivery_date.unwrap_or_else(|| crate::datetime::now()),
                Some(crate::datetime::ASCTIME_FMT),
                true,
            )
            .trim()
            .as_bytes(),
        )?;
        writer.write_all(line_ending)?;
        let (mut headers, body) = parser::mail(input)?;
        match self {
            MboxFormat::MboxO | MboxFormat::MboxRd => Err(MeliError::new("Unimplemented.")),
            MboxFormat::MboxCl => {
                headers.retain(|(header_name, _)| {
                    !header_name.eq_ignore_ascii_case(b"Content-Length")
                });
                let len = (body.len()
                    + body
                        .windows(b"\nFrom ".len())
                        .filter(|w| w == b"\nFrom ")
                        .count()
                    + if body.starts_with(b"From ") { 1 } else { 0 })
                .to_string();
                for (h, v) in headers
                    .into_iter()
                    .chain(Some((&b"Content-Length"[..], len.as_bytes())))
                {
                    writer.write_all(h)?;
                    writer.write_all(&b": "[..])?;
                    writer.write_all(v)?;
                    writer.write_all(line_ending)?;
                }
                writer.write_all(line_ending)?;

                if body.starts_with(b"From ") {
                    writer.write_all(&[b'>'])?;
                }
                for i in 0..body.len() {
                    writer.write_all(&[body[i]])?;
                    if body[i..].starts_with(b"\nFrom ") {
                        writer.write_all(&[b'>'])?;
                    }
                }
                Ok(())
            }
            MboxFormat::MboxCl2 => {
                headers.retain(|(header_name, _)| {
                    !header_name.eq_ignore_ascii_case(b"Content-Length")
                });
                let len = body.len().to_string();
                for (h, v) in headers
                    .into_iter()
                    .chain(Some((&b"Content-Length"[..], len.as_bytes())))
                {
                    writer.write_all(h)?;
                    writer.write_all(&b": "[..])?;
                    writer.write_all(v)?;
                    writer.write_all(line_ending)?;
                }
                writer.write_all(line_ending)?;
                writer.write_all(body)?;
                Ok(())
            }
        }
    }
}
