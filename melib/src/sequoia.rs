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

use std::hash::{Hash, Hasher};

use futures::future::BoxFuture;
use openpgp::{
    parse::{PacketParser, PacketParserResult, Parse as _},
    policy::StandardPolicy,
    types::KeyFlags,
    Cert, Fingerprint, KeyHandle, Packet,
};
pub use sqz;
use sqz::{openpgp, openpgp::KeyID, prompt::Cancel, types::HashMode, Sequoia};

use crate::{
    email::pgp::{
        HashAlgorithm, Key, LocateKey, NewSignature, Recipient, Signature, SignaturesMetadata,
        Summary, Validity,
    },
    error::{Error, ErrorKind, Result},
};

enum Pattern {
    Email(crate::Address),
    KeyHandle(KeyHandle),
}

impl TryFrom<String> for Pattern {
    type Error = Error;

    fn try_from(pattern: String) -> Result<Self> {
        if let Ok(kh) = pattern.parse::<KeyHandle>() {
            if kh.is_invalid() {
                return Err(Error::new(format!("{pattern:?} is an invalid fingerprint"))
                    .set_kind(ErrorKind::ValueError));
            }
            Ok(Self::KeyHandle(kh))
        } else if let Ok(addr) = crate::Address::try_from(pattern.as_str()) {
            Ok(Self::Email(addr))
        } else {
            Err(Error::new(format!(
                "Could not interpret pattern {pattern:?} as e-mail address or fingerprint/keyid"
            ))
            .set_kind(ErrorKind::ValueError))
        }
    }
}

const POLICY: StandardPolicy = StandardPolicy::new();

pub struct Context {
    pub sq: Box<Sequoia>,
    locate_key: LocateKey,
}

impl Context {
    pub fn new() -> Result<Self> {
        Ok(Self {
            sq: Box::new(Sequoia::new(Cancel::new())?),
            locate_key: LocateKey::LOCAL,
        })
    }

    pub fn lookup_one(&self, secret: bool, pattern: String) -> Result<Cert> {
        match Pattern::try_from(pattern)? {
            Pattern::KeyHandle(kh) => {
                let cert = self.sq.lookup().lookup_one(KeyID::from(kh.clone()))?;
                if secret {
                    let key = Key::from(&cert);
                    if !(key.secret || key.can_sign || key.can_encrypt) {
                        return Err(Error::new(format!(
                            "Key {kh} found but it cannot sign or encrypt"
                        ))
                        .set_kind(ErrorKind::ValueError));
                    }
                }

                Ok(cert)
            }
            Pattern::Email(addr) => {
                let cert = self.sq.lookup_by().lookup_one_by_email(addr.get_email())?;
                if secret {
                    let key = Key::from(&cert);
                    if !(key.secret || key.can_sign || key.can_encrypt) {
                        return Err(Error::new(format!(
                            "Key for {email} with fingerprint{fpr} found but it cannot sign or \
                             encrypt",
                            email = addr.get_email(),
                            fpr = key.fingerprint
                        ))
                        .set_kind(ErrorKind::ValueError));
                    }
                }

                Ok(cert)
            }
        }
    }
}

type ResultFuture<T> = Result<BoxFuture<'static, Result<T>>>;

/// Network search sadly not supported because it requires tokio.
const LOCATE_KEY_NOT_SUPPORTED: LocateKey = LocateKey::CERT.union(
    LocateKey::PKA.union(
        LocateKey::DANE.union(
            LocateKey::LDAP
                .union(LocateKey::KEYSERVER.union(LocateKey::KEYSERVER_URL.union(LocateKey::WKD))),
        ),
    ),
);

impl Hash for Context {
    fn hash<H: Hasher>(&self, state: &mut H) {
        "sequoia".hash(state)
    }
}

impl crate::email::pgp::PGPBackend for Context {
    fn set_auto_key_locate(&mut self, new_val: LocateKey) -> Result<()> {
        if !new_val.intersection(LOCATE_KEY_NOT_SUPPORTED).is_empty() {
            return Err(crate::error::Error::new(format!(
                "Values {:?} not supported by this sequioa backend",
                new_val
                    .intersection(LOCATE_KEY_NOT_SUPPORTED)
                    .iter_names()
                    .collect::<Vec<_>>()
            )));
        }
        self.locate_key = new_val;
        Ok(())
    }

    fn get_auto_key_locate(&self) -> Result<LocateKey> {
        Ok(self.locate_key)
    }

    fn get_key(&self, secret: bool, pattern: String) -> ResultFuture<Key> {
        let key = self.lookup_one(secret, pattern)?;
        Ok(Box::pin(async move {
            Ok(Key {
                secret,
                ..key.into()
            })
        }))
    }

    fn verify(&mut self, signature: &[u8], text: &[u8]) -> ResultFuture<SignaturesMetadata> {
        let mut stream = vec![];
        self.sq
            .verify()
            .stream(&mut stream)
            .detached_signature(text, signature, std::io::empty())
            .inspect_err(|_| {
                log::trace!("Could not verify detached signature, event stream was: {stream:?}");
            })?;

        Ok(Box::pin(async move { stream.try_into() }))
    }

    fn verify_cleartext(&mut self, text: &[u8]) -> ResultFuture<SignaturesMetadata> {
        let mut stream = vec![];
        self.sq
            .verify()
            .stream(&mut stream)
            .cleartext_signature(text, std::io::empty())
            .inspect_err(|_| {
                log::trace!("Could not verify cleartext signature, event stream was: {stream:?}");
            })?;

        Ok(Box::pin(async move {
            let mut retval: SignaturesMetadata = stream.try_into()?;
            for sig in &mut retval.signatures {
                sig.cleartext = true;
            }
            Ok(retval)
        }))
    }

    fn keylist(&self, secret: bool, pattern: Option<String>) -> ResultFuture<Vec<Key>> {
        match pattern {
            Some(pattern) => match Pattern::try_from(pattern)? {
                Pattern::KeyHandle(kh) => {
                    let ret = self
                        .sq
                        .lookup()
                        .lookup_all(KeyID::from(kh))?
                        .into_iter()
                        .filter_map(|cert| {
                            let key: Key = cert.into();

                            if secret && !(key.secret || key.can_sign || key.can_encrypt) {
                                return None;
                            }
                            Some(key)
                        })
                        .collect::<Vec<_>>();

                    Ok(Box::pin(async move { Ok(ret) }))
                }
                Pattern::Email(addr) => {
                    let ret = self
                        .sq
                        .lookup_by()
                        .lookup_all_by_email(addr.get_email())?
                        .into_iter()
                        .filter_map(|cert| {
                            let key: Key = cert.into();

                            if secret && !(key.secret || key.can_sign || key.can_encrypt) {
                                return None;
                            }
                            Some(key)
                        })
                        .collect::<Vec<_>>();
                    Ok(Box::pin(async move { Ok(ret) }))
                }
            },
            None => {
                let ret = self
                    .sq
                    .lookup()
                    .lookup_all(KeyID::from(None))?
                    .into_iter()
                    .filter_map(|cert| {
                        let key: Key = cert.into();

                        if secret && !(key.secret || key.can_sign || key.can_encrypt) {
                            return None;
                        }
                        Some(key)
                    })
                    .collect::<Vec<_>>();

                Ok(Box::pin(async move { Ok(ret) }))
            }
        }
    }

    fn sign(
        &mut self,
        sign_keys: Vec<Key>,
        text: &[u8],
        is_binary: bool,
    ) -> ResultFuture<(NewSignature, Vec<u8>)> {
        let sign_keys = sign_keys
            .into_iter()
            .map(|key| {
                Ok(self
                    .sq
                    .lookup()
                    .lookup_one(key.fingerprint.parse::<Fingerprint>()?)?)
            })
            .collect::<Result<Vec<_>>>()?;
        let mut signature = vec![];
        let mut stream = vec![];

        self.sq
            .sign()
            .add_signers(sign_keys)
            .detached()
            .hash_mode(if is_binary {
                HashMode::Binary
            } else {
                HashMode::Text
            })
            .stream(&mut stream)
            .sign(text, &mut signature)
            .inspect_err(|_| {
                log::trace!("Could not sign, event stream was: {stream:?}");
            })?;

        let mut fingerprint = None;

        let hash_algorithm = {
            let mut ppr = PacketParser::from_bytes(&signature)?;
            let mut hash_algorithm = None;
            while let PacketParserResult::Some(pp) = ppr {
                let (packet, next_ppr) = pp.recurse()?;
                ppr = next_ppr;

                // Process the packet.
                if let Packet::Signature(sig) = packet {
                    hash_algorithm = Some(sig.hash_algo());
                    fingerprint = sig.issuer_fingerprints().next().map(|fpr| fpr.to_string());
                    break;
                }
            }
            hash_algorithm.unwrap().try_into()?
        };

        Ok(Box::pin(async move {
            let retval: NewSignature = NewSignature {
                fingerprint: fingerprint.unwrap_or_default(),
                hash_algorithm,
            };
            Ok((retval, signature))
        }))
    }

    fn encrypt(
        &mut self,
        encrypt_keys: Vec<Key>,
        sign_keys: Vec<Key>,
        plain: &[u8],
    ) -> ResultFuture<Vec<u8>> {
        let encrypt_keys = encrypt_keys
            .into_iter()
            .map(|key| {
                Ok(self
                    .sq
                    .lookup()
                    .lookup_one(key.fingerprint.parse::<Fingerprint>()?)?)
            })
            .collect::<Result<Vec<_>>>()?;
        let sign_keys = sign_keys
            .into_iter()
            .map(|key| {
                Ok(self
                    .sq
                    .lookup()
                    .lookup_one(key.fingerprint.parse::<Fingerprint>()?)?)
            })
            .collect::<Result<Vec<_>>>()?;
        let mut encrypted = vec![];
        let mut stream = vec![];
        self.sq
            .encrypt()
            .add_recipients(encrypt_keys)
            .add_signers(sign_keys)
            .stream(&mut stream)
            .encrypt(plain, &mut encrypted)
            .inspect_err(|_| {
                log::trace!("Could not encrypt, event stream was: {stream:?}");
            })?;

        Ok(Box::pin(async move { Ok(encrypted) }))
    }

    fn decrypt(&mut self, cipher: &[u8]) -> ResultFuture<(SignaturesMetadata, Vec<u8>)> {
        let mut decrypted = vec![];
        let mut stream = vec![];
        self.sq
            .decrypt()
            .stream(&mut stream)
            .signatures(0)
            .decrypt(cipher, &mut decrypted)
            .inspect_err(|_| {
                log::trace!("Could not decrypt, event stream was: {stream:?}");
            })?;

        let metadata = stream.try_into()?;
        Ok(Box::pin(async move { Ok((metadata, decrypted)) }))
    }
}

impl From<&Cert> for Key {
    fn from(sqk: &Cert) -> Self {
        let total = sqk.keys().with_policy(&POLICY, None).count();
        let expired = sqk
            .keys()
            .with_policy(&POLICY, None)
            .revoked(false)
            .alive()
            .count()
            == total;
        let can_encrypt = sqk
            .keys()
            .with_policy(&POLICY, None)
            .revoked(false)
            .key_flags(
                KeyFlags::empty()
                    .set_transport_encryption()
                    .set_storage_encryption(),
            )
            .alive()
            .count()
            > 0;
        let can_sign = sqk
            .keys()
            .with_policy(&POLICY, None)
            .revoked(false)
            .key_flags(KeyFlags::signing())
            .alive()
            .count()
            > 0;
        let revoked = sqk.keys().with_policy(&POLICY, None).revoked(true).count() == total;
        Self {
            fingerprint: sqk.fingerprint().to_string(),
            primary_uid: sqk
                .userids()
                .nth(0)
                .and_then(|u| u.userid().try_into().ok()),
            revoked,
            expired,
            disabled: false,
            invalid: false,
            can_encrypt,
            can_sign,
            secret: sqk.is_tsk(),
        }
    }
}

impl From<Cert> for Key {
    fn from(sqk: Cert) -> Self {
        Self::from(&sqk)
    }
}

impl TryFrom<&openpgp::packet::UserID> for crate::email::Address {
    type Error = ();

    fn try_from(userid: &openpgp::packet::UserID) -> std::result::Result<Self, Self::Error> {
        let name = userid.name().ok().flatten();
        let email = userid.email_normalized().ok().flatten().ok_or(())?;

        Ok(Self::new(name, email))
    }
}

impl TryFrom<sqz::verify::output::MessageStructure> for SignaturesMetadata {
    type Error = crate::error::Error;

    fn try_from(
        ms: sqz::verify::output::MessageStructure,
    ) -> std::result::Result<Self, Self::Error> {
        let mut signatures = vec![];
        for layer in ms.layers {
            let sqz::verify::output::MessageLayer::Signature(layer) = layer else {
                continue;
            };
            let sigs = layer.sigs;
            for sig in sigs {
                signatures.push(match sig.status {
                    sqz::verify::output::SignatureStatus::Verified(
                        sqz::verify::output::signature_status::Verified {
                            cert: _, key, wot, ..
                        },
                    ) => Signature {
                        cert: Recipient {
                            keyid: key.to_string(),
                            status: Ok(()),
                        },
                        cleartext: false,
                        validity_reason: None,
                        summary: Summary::VALID,
                        validity: wot
                            .map(|wot| match wot.authentication_level {
                                120.. => Validity::Full,
                                1..120 => Validity::Marginal,
                                0..1 => Validity::Undefined,
                            })
                            .unwrap_or(Validity::Unknown),
                    },
                    sqz::verify::output::SignatureStatus::MissingKey(
                        sqz::verify::output::signature_status::MissingKey { .. },
                    ) => Signature {
                        cert: Recipient {
                            keyid: String::new(),
                            status: Ok(()),
                        },
                        cleartext: false,
                        validity_reason: None,
                        summary: Summary::RED | Summary::KEY_MISSING,
                        validity: Validity::Unknown,
                    },
                    other => return Err(crate::error::Error::new(format!("{other:?}"))),
                });
            }
        }
        Ok(Self { signatures })
    }
}

impl TryFrom<Vec<sqz::verify::Output>> for SignaturesMetadata {
    type Error = crate::error::Error;

    fn try_from(stream: Vec<sqz::verify::Output>) -> std::result::Result<Self, Self::Error> {
        let mut signatures = vec![];
        for output in stream {
            if let sqz::verify::Output::MessageStructure(ms) = output {
                let m = Self::try_from(ms)?;
                signatures.extend(m.signatures);
            }
        }
        Ok(Self { signatures })
    }
}

impl TryFrom<Vec<sqz::decrypt::Output>> for SignaturesMetadata {
    type Error = crate::error::Error;

    fn try_from(stream: Vec<sqz::decrypt::Output>) -> std::result::Result<Self, Self::Error> {
        let mut signatures = vec![];
        for output in stream {
            if let sqz::decrypt::Output::MessageStructure(ms) = output {
                let m = Self::try_from(ms)?;
                signatures.extend(m.signatures);
            }
        }
        Ok(Self { signatures })
    }
}

impl TryFrom<openpgp::crypto::HashAlgorithm> for HashAlgorithm {
    type Error = crate::error::Error;

    fn try_from(hash_algo: openpgp::crypto::HashAlgorithm) -> Result<Self> {
        use openpgp::crypto::HashAlgorithm;
        Ok(match hash_algo {
            HashAlgorithm::MD5 => Self::MD5,
            HashAlgorithm::SHA1 => Self::SHA1,
            HashAlgorithm::RipeMD => Self::RMD160,
            HashAlgorithm::SHA256 => Self::SHA256,
            HashAlgorithm::SHA384 => Self::SHA384,
            HashAlgorithm::SHA512 => Self::SHA512,
            HashAlgorithm::SHA224 => Self::SHA224,
            HashAlgorithm::SHA3_256 => Self::SHA3_256,
            HashAlgorithm::SHA3_512 => Self::SHA3_512,
            other => {
                return Err(crate::error::Error::new(format!(
                    "Unsupported Sequoia hash algorithm {other:?}"
                )))
            }
        })
    }
}
