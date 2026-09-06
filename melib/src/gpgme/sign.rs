//
// meli
//
// Copyright 2026  Manos Pitsidianakis
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

// [ref:DEBT] segfaults on libgpgme code can crash the entire app; it should be
// handled.

use std::{ffi::CStr, ptr::NonNull, sync::Arc};

use crate::{
    email::pgp::{Recipient, Signature, Summary, Validity},
    error::{Error, ErrorKind},
    gpgme::bindings::*,
};

#[derive(Clone)]
pub struct SignResult {
    ptr: NonNull<_gpgme_op_sign_result>,
    lib: Arc<libloading::Library>,
}

impl SignResult {
    pub fn retrieve(ctx: &super::Context) -> Option<Self> {
        let ptr =
            // SAFETY: context pointer is valid
            NonNull::new(unsafe { call!(&ctx.inner.lib, gpgme_op_sign_result)(ctx.inner.ptr.as_ptr()) })?;
        Some(Self {
            ptr,
            lib: Arc::clone(&ctx.inner.lib),
        })
    }

    pub fn invalid_signers(&'_ self) -> super::key::InvalidKeysIter<'_> {
        // SAFETY: pointer is valid
        let ptr = unsafe { self.ptr.as_ref().invalid_signers };
        super::key::InvalidKeysIter::new(ptr, self.lib.clone())
    }

    pub fn signatures(&'_ self) -> NewSignaturesIter<'_> {
        // SAFETY: pointer is valid
        let ptr = unsafe { self.ptr.as_ref().signatures };
        NewSignaturesIter::new(ptr, self.lib.clone())
    }
}

#[derive(Clone, Debug)]
pub struct NewSignature {
    /// The type of the signature
    pub type_: gpgme_sig_mode_t,
    /// The public key algorithm used to create the signature
    pub pubkey_algo: gpgme_pubkey_algo_t,
    /// The hash algorithm used to create the signature
    pub hash_algo: std::result::Result<HashAlgorithm, (gpgme_hash_algo_t, String)>,
    /// Signature creation time.  */
    pub timestamp: ::core::ffi::c_long,
    /// The fingerprint of the signature
    pub fingerprint: String,
    pub class: ::core::ffi::c_uint,
    /// Crypto backend specific signature class
    pub sig_class: ::core::ffi::c_uint,
}

impl NewSignature {
    pub fn micalg(&self) -> String {
        let mut s = match self.hash_algo {
            Ok(ref h) => format!("pgp-{h}"),
            Err((_, ref s)) => format!("pgp-{s}"),
        };
        s.make_ascii_lowercase();
        s
    }
}

pub struct NewSignaturesIter<'a> {
    #[allow(unused)]
    lib: Arc<libloading::Library>,
    ptr: gpgme_new_signature_t,
    _ph: std::marker::PhantomData<&'a _gpgme_new_signature>,
}

impl NewSignaturesIter<'_> {
    pub fn new(ptr: gpgme_new_signature_t, lib: Arc<libloading::Library>) -> Self {
        Self {
            lib,
            ptr,
            _ph: std::marker::PhantomData,
        }
    }
}

impl Iterator for NewSignaturesIter<'_> {
    type Item = NewSignature;

    fn next(&mut self) -> Option<Self::Item> {
        let new_sig = NonNull::new(self.ptr)?;
        // SAFETY: pointer is valid
        let new_sig_ref = unsafe { new_sig.as_ref() };
        self.ptr = new_sig_ref.next;
        let hash_algo = new_sig_ref.hash_algo.try_into().map_err(|alg| {
            let algo_name_ptr = unsafe { call!(&self.lib, gpgme_hash_algo_name)(alg) };
            if algo_name_ptr.is_null() {
                // This should never happen because `alg` was given to us by libgpgme itself, but
                // whatever.
                return (alg, String::from("UNKNOWN"));
            }
            // SAFETY: gpgme guarantees it returns a valid statically-allocated string
            (
                alg,
                unsafe { CStr::from_ptr(algo_name_ptr) }
                    .to_string_lossy()
                    .to_string(),
            )
        });
        Some(NewSignature {
            type_: new_sig_ref.type_,
            pubkey_algo: new_sig_ref.pubkey_algo,
            hash_algo,
            timestamp: new_sig_ref.timestamp,
            class: new_sig_ref.class,
            sig_class: new_sig_ref.sig_class,
            // SAFETY: pointer is valid
            fingerprint: unsafe { CStr::from_ptr(new_sig_ref.fpr) }
                .to_string_lossy()
                .to_string(),
        })
    }
}

#[derive(Clone)]
pub struct VerifyResult {
    ptr: NonNull<_gpgme_op_verify_result>,
    lib: Arc<libloading::Library>,
}

impl VerifyResult {
    pub fn retrieve(lib: &Arc<libloading::Library>, ctx: &super::Context) -> Option<Self> {
        let ptr =
            // SAFETY: context pointer is valid
            NonNull::new(unsafe { call!(&lib, gpgme_op_verify_result)(ctx.inner.ptr.as_ptr()) })?;
        Some(Self {
            ptr,
            lib: Arc::clone(lib),
        })
    }

    pub fn signatures(&'_ self) -> SignaturesIter<'_> {
        // SAFETY: pointer is valid
        let ptr = unsafe { self.ptr.as_ref().signatures };
        SignaturesIter::new(ptr, self.lib.clone())
    }
}

pub struct SignaturesIter<'a> {
    #[allow(unused)]
    lib: Arc<libloading::Library>,
    ptr: gpgme_signature_t,
    _ph: std::marker::PhantomData<&'a _gpgme_signature>,
}

impl SignaturesIter<'_> {
    pub fn new(ptr: gpgme_signature_t, lib: Arc<libloading::Library>) -> Self {
        Self {
            lib,
            ptr,
            _ph: std::marker::PhantomData,
        }
    }
}

impl From<gpgme_sigsum_t> for Summary {
    #[inline]
    fn from(v: gpgme_sigsum_t) -> Self {
        Self::from_bits_truncate(v.0)
    }
}

impl From<gpgme_validity_t> for Validity {
    #[inline]
    fn from(v: gpgme_validity_t) -> Self {
        match v {
            gpgme_validity_t::GPGME_VALIDITY_UNKNOWN => Self::Unknown,
            gpgme_validity_t::GPGME_VALIDITY_UNDEFINED => Self::Undefined,
            gpgme_validity_t::GPGME_VALIDITY_NEVER => Self::Never,
            gpgme_validity_t::GPGME_VALIDITY_MARGINAL => Self::Marginal,
            gpgme_validity_t::GPGME_VALIDITY_FULL => Self::Full,
            gpgme_validity_t::GPGME_VALIDITY_ULTIMATE => Self::Ultimate,
        }
    }
}

impl Iterator for SignaturesIter<'_> {
    type Item = Signature;

    fn next(&mut self) -> Option<Self::Item> {
        let inner = NonNull::new(self.ptr)?;
        // SAFETY: pointer is valid, was obtained from libgpgme and lives as long as self is alive.
        let sig_ref = unsafe { inner.as_ref() };
        self.ptr = sig_ref.next;
        let summary = sig_ref.summary.into();
        let status = sig_ref.status;
        let keyid = unsafe { CStr::from_ptr(sig_ref.fpr) }
            .to_string_lossy()
            .to_string();
        let status = if status > 0 {
            Err(Error::new(format!("BAD signature from {keyid}"))
                .set_details(super::gpgme_error_to_string(&self.lib, status))
                .set_kind(ErrorKind::None))
        } else {
            Ok(())
        };
        let validity = sig_ref.validity.into();
        let validity_reason = if sig_ref.validity_reason > 0 {
            Some(super::gpgme_error_to_string(
                &self.lib,
                sig_ref.validity_reason,
            ))
        } else {
            None
        };

        Some(Signature {
            summary,
            cert: Recipient { keyid, status },
            validity,
            validity_reason,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum HashAlgorithm {
    None = 0,
    MD5 = 1,
    SHA1 = 2,
    RMD160 = 3,
    MD2 = 5,
    TIGER = 6,
    HAVAL = 7,
    SHA256 = 8,
    SHA384 = 9,
    SHA512 = 10,
    SHA224 = 11,
    MD4 = 301,
    CRC32 = 302,
    CRC32RFC1510 = 303,
    CRC24RFC2440 = 304,
}

impl TryFrom<gpgme_hash_algo_t> for HashAlgorithm {
    type Error = gpgme_hash_algo_t;

    #[inline]
    fn try_from(v: gpgme_hash_algo_t) -> std::result::Result<Self, Self::Error> {
        #[allow(unreachable_patterns)]
        let retval = match v {
            gpgme_hash_algo_t::GPGME_MD_NONE => Self::None,
            gpgme_hash_algo_t::GPGME_MD_MD5 => Self::MD5,
            gpgme_hash_algo_t::GPGME_MD_SHA1 => Self::SHA1,
            gpgme_hash_algo_t::GPGME_MD_RMD160 => Self::RMD160,
            gpgme_hash_algo_t::GPGME_MD_MD2 => Self::MD2,
            gpgme_hash_algo_t::GPGME_MD_TIGER => Self::TIGER,
            gpgme_hash_algo_t::GPGME_MD_HAVAL => Self::HAVAL,
            gpgme_hash_algo_t::GPGME_MD_SHA256 => Self::SHA256,
            gpgme_hash_algo_t::GPGME_MD_SHA384 => Self::SHA384,
            gpgme_hash_algo_t::GPGME_MD_SHA512 => Self::SHA512,
            gpgme_hash_algo_t::GPGME_MD_SHA224 => Self::SHA224,
            gpgme_hash_algo_t::GPGME_MD_MD4 => Self::MD4,
            gpgme_hash_algo_t::GPGME_MD_CRC32 => Self::CRC32,
            gpgme_hash_algo_t::GPGME_MD_CRC32_RFC1510 => Self::CRC32RFC1510,
            gpgme_hash_algo_t::GPGME_MD_CRC24_RFC2440 => Self::CRC24RFC2440,
            _ => return Err(v),
        };
        Ok(retval)
    }
}

/// Format [`HashAlgorithm`] according to RFCs
///
/// - <https://datatracker.ietf.org/doc/html/rfc2440#section-9.4>
/// - <https://datatracker.ietf.org/doc/html/rfc4880#section-9.4>
/// - <https://datatracker.ietf.org/doc/html/rfc9580#section-9.5>
/// - libgpgme source for the rest
impl std::fmt::Display for HashAlgorithm {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::None => write!(fmt, "NULL"),
            Self::MD5 => write!(fmt, "MD5"),
            Self::SHA1 => write!(fmt, "SHA1"),
            Self::RMD160 => write!(fmt, "RIPEMD160"),
            Self::MD2 => write!(fmt, "MD2"),
            Self::TIGER => write!(fmt, "TIGER192"),
            Self::HAVAL => write!(fmt, "HAVAL"),
            Self::SHA256 => write!(fmt, "SHA256"),
            Self::SHA384 => write!(fmt, "SHA384"),
            Self::SHA512 => write!(fmt, "SHA512"),
            Self::SHA224 => write!(fmt, "SHA224"),
            Self::MD4 => write!(fmt, "MD4"),
            Self::CRC32 => write!(fmt, "CRC32"),
            Self::CRC32RFC1510 => write!(fmt, "CRC32RFC1510"),
            Self::CRC24RFC2440 => write!(fmt, "CRC24RFC2440"),
        }
    }
}

impl std::str::FromStr for HashAlgorithm {
    type Err = ();

    fn from_str(mut v: &str) -> std::result::Result<Self, Self::Err> {
        if let Some(stripped) = v.strip_prefix("pgp") {
            v = stripped;
        }
        let retval = match v.trim() {
            v if v.eq_ignore_ascii_case("NULL") => Self::None,
            v if v.eq_ignore_ascii_case("MD5") => Self::MD5,
            v if v.eq_ignore_ascii_case("SHA1") => Self::SHA1,
            v if v.eq_ignore_ascii_case("RIPEMD160") => Self::RMD160,
            v if v.eq_ignore_ascii_case("MD2") => Self::MD2,
            v if v.eq_ignore_ascii_case("TIGER192") => Self::TIGER,
            v if v.eq_ignore_ascii_case("HAVAL") || v.eq_ignore_ascii_case("HAVAL-5-160") => {
                Self::HAVAL
            }
            v if v.eq_ignore_ascii_case("SHA256") => Self::SHA256,
            v if v.eq_ignore_ascii_case("SHA384") => Self::SHA384,
            v if v.eq_ignore_ascii_case("SHA512") => Self::SHA512,
            v if v.eq_ignore_ascii_case("SHA224") => Self::SHA224,
            v if v.eq_ignore_ascii_case("MD4") => Self::MD4,
            v if v.eq_ignore_ascii_case("CRC32") => Self::CRC32,
            v if v.eq_ignore_ascii_case("CRC32RFC1510") => Self::CRC32RFC1510,
            v if v.eq_ignore_ascii_case("CRC24RFC2440") => Self::CRC24RFC2440,
            _ => return Err(()),
        };
        Ok(retval)
    }
}
