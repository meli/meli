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
        (flags, tags): (Flag, Vec<&str>),
        metadata_format: MboxMetadata,
        is_empty: bool,
        crlf: bool,
    ) -> Result<()> {
        if tags.iter().any(|t| t.contains(' ')) {
            return Err(MeliError::new("mbox tags/keywords can't contain spaces"));
        }
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
                delivery_date.unwrap_or_else(crate::datetime::now),
                Some(crate::datetime::ASCTIME_FMT),
                true,
            )
            .trim()
            .as_bytes(),
        )?;
        writer.write_all(line_ending)?;
        let (mut headers, body) = parser::mail(input)?;
        headers.retain(|(header_name, _)| {
            !header_name.eq_ignore_ascii_case(b"Status")
                && !header_name.eq_ignore_ascii_case(b"X-Status")
                && !header_name.eq_ignore_ascii_case(b"X-Keywords")
                && !header_name.eq_ignore_ascii_case(b"Content-Length")
        });
        let write_header_val_fn = |writer: &mut dyn std::io::Write, bytes: &[u8]| {
            let mut i = 0;
            if crlf {
                while i < bytes.len() {
                    if bytes[i..].starts_with(b"\r\n") {
                        writer.write_all(&[b'\r', b'\n'])?;
                        i += 2;
                        continue;
                    } else if bytes[i] == b'\n' {
                        writer.write_all(&[b'\r', b'\n'])?;
                    } else {
                        writer.write_all(&[bytes[i]])?;
                    }
                    i += 1;
                }
            } else {
                while i < bytes.len() {
                    if bytes[i..].starts_with(b"\r\n") {
                        writer.write_all(&[b'\n'])?;
                        i += 2;
                    } else {
                        writer.write_all(&[bytes[i]])?;
                        i += 1;
                    }
                }
            }
            Ok::<(), MeliError>(())
        };
        let write_metadata_fn = |writer: &mut dyn std::io::Write| match metadata_format {
            MboxMetadata::CClient => {
                for (h, v) in {
                    if flags.is_seen() {
                        Some((&b"Status"[..], "R".into()))
                    } else {
                        None
                    }
                    .into_iter()
                    .chain(
                        if !flags.is_flagged()
                            && !flags.is_replied()
                            && !flags.is_draft()
                            && !flags.is_trashed()
                        {
                            None
                        } else {
                            Some((
                                &b"X-Status"[..],
                                format!(
                                    "{flagged}{replied}{draft}{trashed}",
                                    flagged = if flags.is_flagged() { "F" } else { "" },
                                    replied = if flags.is_replied() { "A" } else { "" },
                                    draft = if flags.is_draft() { "T" } else { "" },
                                    trashed = if flags.is_trashed() { "D" } else { "" }
                                ),
                            ))
                        },
                    )
                    .chain(if tags.is_empty() {
                        None
                    } else {
                        Some((&b"X-Keywords"[..], tags.as_slice().join(" ")))
                    })
                } {
                    writer.write_all(h)?;
                    writer.write_all(&b": "[..])?;
                    writer.write_all(v.as_bytes())?;
                    writer.write_all(line_ending)?;
                }
                Ok::<(), MeliError>(())
            }
            MboxMetadata::None => Ok(()),
        };

        let body_len = {
            let mut len = body.len();
            if crlf {
                let stray_lfs = body.iter().filter(|b| **b == b'\n').count()
                    - body.windows(b"\r\n".len()).filter(|w| w == b"\r\n").count();
                len += stray_lfs;
            } else {
                let crlfs = body.windows(b"\r\n".len()).filter(|w| w == b"\r\n").count();
                len -= crlfs;
            }
            len
        };

        match self {
            MboxFormat::MboxO | MboxFormat::MboxRd => Err(MeliError::new("Unimplemented.")),
            MboxFormat::MboxCl => {
                let len = (body_len
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
                    write_header_val_fn(writer, v)?;
                    writer.write_all(line_ending)?;
                }
                write_metadata_fn(writer)?;
                writer.write_all(line_ending)?;

                if body.starts_with(b"From ") {
                    writer.write_all(&[b'>'])?;
                }
                let mut i = 0;
                if crlf {
                    while i < body.len() {
                        if body[i..].starts_with(b"\r\n") {
                            writer.write_all(&[b'\r', b'\n'])?;
                            if body[i..].starts_with(b"\r\nFrom ") {
                                writer.write_all(&[b'>'])?;
                            }
                            i += 2;
                        } else if body[i] == b'\n' {
                            writer.write_all(&[b'\r', b'\n'])?;
                            if body[i..].starts_with(b"\nFrom ") {
                                writer.write_all(&[b'>'])?;
                            }
                            i += 1;
                        } else {
                            writer.write_all(&[body[i]])?;
                            i += 1;
                        }
                    }
                } else {
                    while i < body.len() {
                        if body[i..].starts_with(b"\r\n") {
                            writer.write_all(&[b'\n'])?;
                            if body[i..].starts_with(b"\r\nFrom ") {
                                writer.write_all(&[b'>'])?;
                            }
                            i += 2;
                        } else {
                            writer.write_all(&[body[i]])?;
                            if body[i..].starts_with(b"\nFrom ") {
                                writer.write_all(&[b'>'])?;
                            }
                            i += 1;
                        }
                    }
                }
                Ok(())
            }
            MboxFormat::MboxCl2 => {
                let len = body_len.to_string();
                for (h, v) in headers
                    .into_iter()
                    .chain(Some((&b"Content-Length"[..], len.as_bytes())))
                {
                    writer.write_all(h)?;
                    writer.write_all(&b": "[..])?;
                    write_header_val_fn(writer, v)?;
                    writer.write_all(line_ending)?;
                }
                write_metadata_fn(writer)?;
                writer.write_all(line_ending)?;
                let mut i = 0;
                if crlf {
                    while i < body.len() {
                        if body[i..].starts_with(b"\r\n") {
                            writer.write_all(&[b'\r', b'\n'])?;
                            i += 2;
                            continue;
                        } else if body[i] == b'\n' {
                            writer.write_all(&[b'\r', b'\n'])?;
                        } else {
                            writer.write_all(&[body[i]])?;
                        }
                        i += 1;
                    }
                } else {
                    while i < body.len() {
                        if body[i..].starts_with(b"\r\n") {
                            writer.write_all(&[b'\n'])?;
                            i += 2;
                        } else {
                            writer.write_all(&[body[i]])?;
                            i += 1;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}
