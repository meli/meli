/*
 * meli - email module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use crate::email::attachments::{Attachment, ContentType, MultipartType};
use crate::email::parser::BytesExt;
use crate::{MeliError, Result};

/// rfc3156
/// Upon receipt of a signed message, an application MUST:
///
///   (1)   Convert line endings to the canonical <CR><LF> sequence before
///         the signature can be verified.  This is necessary since the
///         local MTA may have converted to a local end of line convention.
///   (2)   Pass both the signed data and its associated content headers
///         along with the OpenPGP signature to the signature verification
///         service.
///
pub fn convert_attachment_to_rfc_spec(input: &[u8]) -> Vec<u8> {
    if input.is_empty() {
        return Vec::new();
    }
    let mut ret = Vec::with_capacity(input.len());

    if input[0] == b'\n' {
        /* This is an RFC violation but test for it anyway */
        ret.push(b'\r');
        ret.push(b'\n');
    } else {
        ret.push(input[0]);
    }

    let mut ctr = 1;

    while ctr < input.len() {
        if input[ctr] == b'\r' && ctr + 1 < input.len() && input[ctr + 1] == b'\n' {
            ret.push(b'\r');
            ret.push(b'\n');
            ctr += 2;
        } else if input[ctr] == b'\n' {
            ret.push(b'\r');
            ret.push(b'\n');
            ctr += 1;
        } else {
            ret.push(input[ctr]);
            ctr += 1;
        }
    }
    loop {
        match ret.iter().last() {
            None => {
                break;
            }
            Some(b'\n') | Some(b'\r') => {
                ret.pop();
            }
            _ => {
                break;
            }
        }
    }
    ret.push(0x0d);
    ret.push(0x0a);
    ret
}

pub fn verify_signature(a: &Attachment) -> Result<(Vec<u8>, &[u8])> {
    match a.content_type {
        ContentType::Multipart {
            kind: MultipartType::Signed,
            ref parts,
            boundary: _,
        } => {
            if parts.len() != 2 {
                return Err(MeliError::new(format!(
                    "Illegal number of parts in multipart/signed. Expected 2 got {}",
                    parts.len()
                )));
            }

            let part_boundaries = a.part_boundaries();

            let signed_part: Vec<u8> = if let Some(v) = parts
                .iter()
                .zip(part_boundaries.iter())
                .find(|(p, _)| p.content_type != ContentType::PGPSignature)
                .map(|(_, s)| convert_attachment_to_rfc_spec(s.display_bytes(a.body())))
            {
                v
            } else {
                return Err(MeliError::new(
                    "multipart/signed attachment without a signed part".to_string(),
                ));
            };
            let signature = if let Some(sig) = parts
                .iter()
                .find(|s| s.content_type == ContentType::PGPSignature)
                .map(|a| a.body())
            {
                sig.trim()
            } else {
                return Err(MeliError::new(
                    "multipart/signed attachment without a signature part".to_string(),
                ));
            };
            Ok((signed_part, signature))
        }
        _ => {
            unreachable!("Should not give non-signed attachments to this function");
        }
    }
}
