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

//! `OpenPGP` signatures and encryption.

use serde::{
    de::{self, Deserialize},
    Deserializer, Serialize, Serializer,
};

use crate::{
    email::{
        attachment_types::{ContentType, MultipartType, Text},
        attachments::Attachment,
        parser::BytesExt,
    },
    error::{Error, ErrorKind, Result},
};

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LocateKey: u8 {
        /// Locate a key using DNS CERT, as specified in RFC-4398.
        const CERT = 0b1;
        /// Locate a key using DNS PKA.
        const PKA  = 0b10;
        /// Locate a key using DANE, as specified in draft-ietf-dane-openpgpkey-05.txt.
        const DANE  = 0b100;
        /// Locate a key using the Web Key Directory protocol.
        const WKD  = 0b1000;
        /// Using DNS Service Discovery, check the domain in question for any LDAP keyservers to use. If this fails, attempt to locate the key using the PGP Universal method of checking ‘ldap://keys.(thedomain)’.
        const LDAP = 0b10000;
        /// Locate a key using a keyserver.
        const KEYSERVER  = 0b100000;
        /// In addition, a keyserver URL as used in the dirmngr configuration may be used here to query that particular keyserver.
        const KEYSERVER_URL = 0b1000000;
        /// Locate the key using the local keyrings. This mechanism allows the user to select the order a local key lookup is done. Thus using ‘--auto-key-locate local’ is identical to --no-auto-key-locate.
        const LOCAL = 0b10000000;
        /// This flag disables the standard local key lookup, done before any of the mechanisms defined by the --auto-key-locate are tried. The position of this mechanism in the list does not matter. It is not required if local is also used.
        const NODEFAULT = 0;
    }
}

impl<'de> Deserialize<'de> for LocateKey {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        <String>::deserialize(deserializer).map_or_else(
            |_| Err(de::Error::custom("LocateKey value must be a string.")),
            |s| Self::from_string_de::<'de, D, String>(s),
        )
    }
}

impl Serialize for LocateKey {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl LocateKey {
    pub fn from_string_de<'de, D, T: AsRef<str>>(s: T) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(match s.as_ref().trim() {
            s if s.eq_ignore_ascii_case("cert") => Self::CERT,
            s if s.eq_ignore_ascii_case("pka") => Self::PKA,
            s if s.eq_ignore_ascii_case("dane") => Self::DANE,
            s if s.eq_ignore_ascii_case("wkd") => Self::WKD,
            s if s.eq_ignore_ascii_case("ldap") => Self::LDAP,
            s if s.eq_ignore_ascii_case("keyserver") => Self::KEYSERVER,
            s if s.eq_ignore_ascii_case("keyserver-url") => Self::KEYSERVER_URL,
            s if s.eq_ignore_ascii_case("local") => Self::LOCAL,
            combination if combination.contains(',') => {
                let mut ret = Self::NODEFAULT;
                for c in combination.trim().split(',') {
                    ret |= Self::from_string_de::<'de, D, &str>(c.trim())?;
                }
                ret
            }
            _ => {
                return Err(de::Error::custom(
                    r#"Takes valid auto-key-locate GPG values: "cert", "pka", "dane", "wkd", "ldap", "keyserver", "keyserver-URL", "local", "nodefault""#,
                ))
            }
        })
    }
}

impl std::fmt::Display for LocateKey {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if *self == Self::NODEFAULT {
            write!(fmt, "clear,nodefault")
        } else {
            let mut accum = String::new();
            macro_rules! is_set {
                ($flag:expr, $string:literal) => {{
                    if self.intersects($flag) {
                        accum.push_str($string);
                        accum.push(',');
                    }
                }};
            }
            is_set!(Self::CERT, "cert");
            is_set!(Self::PKA, "pka");
            is_set!(Self::WKD, "wkd");
            is_set!(Self::LDAP, "ldap");
            is_set!(Self::KEYSERVER, "keyserver");
            is_set!(Self::KEYSERVER_URL, "keyserver-url");
            is_set!(Self::LOCAL, "local");
            accum.pop();
            write!(fmt, "{accum}")
        }
    }
}

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

pub enum UnverifiedSignature<'a> {
    Detached {
        signed_part: Vec<u8>,
        signature: &'a Attachment,
    },
    Cleartext {
        text: Vec<u8>,
    },
}

pub fn extract_unverified_signature(a: &'_ Attachment) -> Result<UnverifiedSignature<'_>> {
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
            Ok(UnverifiedSignature::Detached {
                signed_part,
                signature,
            })
        }
        ContentType::Text {
            charset: _,
            kind: Text::Plain,
            parameters: _,
        } => {
            let text = a.decode(Default::default());
            if text
                .strip_prefix(b"-----BEGIN PGP SIGNED MESSAGE-----")
                .and_then(|t| t.trim_end().strip_suffix(b"-----END PGP SIGNATURE-----"))
                .is_none()
            {
                return Err(Error::new("Not a signed attachment").set_kind(ErrorKind::ValueError));
            };
            Ok(UnverifiedSignature::Cleartext { text })
        }
        _ => Err(Error::new("Not a signed attachment").set_kind(ErrorKind::ValueError)),
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
    pub cleartext: bool,
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
