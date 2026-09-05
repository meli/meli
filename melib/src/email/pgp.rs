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

//! Verification of `OpenPGP` signatures.
use crate::{
    email::{
        attachment_types::{ContentType, MultipartType},
        attachments::Attachment,
        parser::BytesExt,
    },
    error::{Error, ErrorKind, Result},
};

/// Convert raw attachment to the form needed for signature verification ([RFC3156](https://tools.ietf.org/html/rfc3156))
///
/// ## RFC3156
///
/// ```text
/// Upon receipt of a signed message, an application MUST:
///
///   (1)   Convert line endings to the canonical <CR><LF> sequence before
///         the signature can be verified.  This is necessary since the
///         local MTA may have converted to a local end of line convention.
///   (2)   Pass both the signed data and its associated content headers
///         along with the OpenPGP signature to the signature verification
///         service.
/// ```
pub fn convert_attachment_to_rfc_spec(input: &[u8]) -> Vec<u8> {
    if input.is_empty() {
        return Vec::new();
    }
    let re = regex::bytes::Regex::new(r"[^\r]\n").unwrap();
    if re.find_iter(input).count() > 0 {
        return input.replace(b"\r\n", b"\n").replace(b"\n", b"\r\n");
    }
    input.to_vec()
}

// [ref:TODO]: add cleartext support
pub fn verify_signature(a: &Attachment) -> Result<(Vec<u8>, &Attachment)> {
    match a.content_type {
        ContentType::Multipart {
            kind: MultipartType::Signed,
            ref parts,
            boundary: _,
            ref parameters,
        } => {
            if parts.len() != 2 {
                return Err(Error::new(format!(
                    "Invalid number of parts in multipart/signed. Expected 2 got {}",
                    parts.len()
                ))
                .set_kind(ErrorKind::ValueError));
            }

            let Some((_, _micalg)) = parameters.iter().find(|(n, _)| n == b"micalg") else {
                return Err(Error::new("Content type does not have `micalg` set")
                    .set_kind(ErrorKind::ValueError));
            };
            let Some((_, protocol)) = parameters.iter().find(|(n, _)| n == b"protocol") else {
                return Err(Error::new("Content type does not have `protocol` set")
                    .set_kind(ErrorKind::ValueError));
            };
            if !(protocol == b"\"application/pgp-signature\""
                || protocol == b"application/pgp-signature")
            {
                let protocol = String::from_utf8_lossy(protocol);
                return Err(Error::new(format!(
                    "Content type has invalid `protocol` {protocol} (expected: \
                     application/pgp-signature)"
                ))
                .set_kind(ErrorKind::ValueError));
            }

            let signed_part: Vec<u8> = if let Some(v) = parts
                .iter()
                .find(|p| {
                    p.content_type != ContentType::PGPSignature
                        && p.content_type != ContentType::CMSSignature
                })
                .map(|a| convert_attachment_to_rfc_spec(a.raw()))
            {
                v
            } else {
                return Err(
                    Error::new("multipart/signed attachment without a signed part")
                        .set_kind(ErrorKind::ValueError),
                );
            };
            let signature = if let Some(sig) = parts.iter().find(|s| {
                s.content_type == ContentType::PGPSignature
                    || s.content_type == ContentType::CMSSignature
            }) {
                sig
            } else {
                return Err(
                    Error::new("multipart/signed attachment without a signature part")
                        .set_kind(ErrorKind::ValueError),
                );
            };
            Ok((signed_part, signature))
        }
        _ => Err(Error::new("Not a multipart/signed attachment").set_kind(ErrorKind::ValueError)),
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct DecryptionMetadata {
    pub recipients: Vec<Recipient>,
    pub file_name: Option<String>,
    pub session_key: Option<String>,
    pub is_mime: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Recipient {
    pub keyid: String,
    pub status: Result<()>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Signature {
    pub summary: Summary,
    pub cert: Recipient,
    pub validity: Validity,
    pub validity_reason: Option<String>,
}

impl From<Signature> for Result<()> {
    fn from(val: Signature) -> Self {
        val.cert.status
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SignaturesMetadata {
    pub signatures: Vec<Signature>,
}

bitflags::bitflags! {
    /// Signature summary
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Summary: u32 {
        /// The signature is fully valid.
        const VALID = 1;
        /// The signature is good but one might want to display some extra information. Check the
        /// other bits.
        const GREEN = 2;
        /// The signature is bad. It might be useful to check other bits and display more
        /// information, i.e., a revoked certificate might not render a signature invalid when the
        /// message was received
        /// prior to the cause for the revocation.
        const RED = 4;
        /// The key or at least one certificate has been revoked.
        const KEY_REVOKED = 16;
        /// The key or one of the certificates has expired. It is probably a good idea to display
        /// the date of the expiration.
        const KEY_EXPIRED = 32;
        /// The signature has expired.
        const SIG_EXPIRED = 64;
        /// Can't verify due to a missing key or certificate.
        const KEY_MISSING = 128;
        /// The CRL (or an equivalent mechanism) is not available.
        const CRL_MISSING = 256;
        /// Available CRL is too old.
        const CRL_TOO_OLD = 512;
        /// A policy requirement was not met.
        const BAD_POLICY = 1024;
        /// A system error occurred.
        const SYS_ERROR = 2048;
        /// A TOFU conflict was detected.
        const TOFU_CONFLICT = 4096;
    }
}

impl std::fmt::Display for Summary {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut iter = self.iter_names().peekable();
        while let Some((f, _)) = iter.next() {
            write!(fmt, "{f}")?;
            if iter.peek().is_some() {
                write!(fmt, ",")?;
            }
        }
        Ok(())
    }
}

/// Signature validity
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Validity {
    Unknown = 0,
    Undefined = 1,
    Never = 2,
    Marginal = 3,
    Full = 4,
    Ultimate = 5,
}

impl Validity {
    /// Get string representation as defined in gpg docs:
    ///
    /// ```text
    /// ‘GPGME_VALIDITY_UNKNOWN’
    ///          The user ID is of unknown validity.  The string representation
    ///          of this validity is “?”.
    ///     ‘GPGME_VALIDITY_UNDEFINED’
    ///          The validity of the user ID is undefined.  The string
    ///          representation of this validity is “q”.
    ///     ‘GPGME_VALIDITY_NEVER’
    ///          The user ID is never valid.  The string representation of this
    ///          validity is “n”.
    ///     ‘GPGME_VALIDITY_MARGINAL’
    ///          The user ID is marginally valid.  The string representation of
    ///          this validity is “m”.
    ///     ‘GPGME_VALIDITY_FULL’
    ///          The user ID is fully valid.  The string representation of this
    ///          validity is “f”.
    ///     ‘GPGME_VALIDITY_ULTIMATE’
    ///          The user ID is ultimately valid.  The string representation of
    ///          this validity is “u”.
    /// ```
    #[inline]
    pub fn string_representation(&'_ self) -> impl std::fmt::Display + '_ {
        ValidityStringRepresentation(self)
    }
}

impl std::fmt::Display for Validity {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(fmt, "UNKNOWN"),
            Self::Undefined => write!(fmt, "UNDEFINED"),
            Self::Never => write!(fmt, "NEVER"),
            Self::Marginal => write!(fmt, "MARGINAL"),
            Self::Full => write!(fmt, "FULL"),
            Self::Ultimate => write!(fmt, "ULTIMATE"),
        }
    }
}

struct ValidityStringRepresentation<'a>(&'a Validity);

impl<'a> std::fmt::Display for ValidityStringRepresentation<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Validity::Unknown => write!(fmt, "?"),
            Validity::Undefined => write!(fmt, "q"),
            Validity::Never => write!(fmt, "n"),
            Validity::Marginal => write!(fmt, "m"),
            Validity::Full => write!(fmt, "f"),
            Validity::Ultimate => write!(fmt, "u"),
        }
    }
}
