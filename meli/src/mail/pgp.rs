/*
 * meli
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

use std::{
    collections::{hash_map::DefaultHasher, BTreeMap},
    future::Future,
    hash::{Hash, Hasher},
    process::{Command, Stdio},
    sync::{Arc, Mutex},
};

use melib::{
    email::{
        attachment_types::{ContentDisposition, ContentType, MultipartType, Text},
        pgp::{
            self as melib_pgp, DecryptionMetadata, Key, LocateKey, NewSignature, PGPBackend,
            Recipient, ResultFuture, Signature, SignaturesMetadata, UnverifiedSignature,
        },
        Attachment, AttachmentBuilder,
    },
    error::*,
    parser::BytesExt,
};

use super::AttachmentBoxFuture;
use crate::{
    conf::pgp::{PGPBackendCLI, PGPBackendChoice},
    types::File,
};

/// Decrypts a `multipart/encrypted` or a cleartext encrypted message.
pub async fn decrypt(
    mut backend: impl PGPBackend,
    a: Attachment,
) -> Result<(DecryptionMetadata, Vec<u8>)> {
    let Attachment {
        content_type:
            ContentType::Multipart {
                kind: MultipartType::Encrypted,
                parts,
                ..
            },
        ..
    } = a
    else {
        if matches!(
            a.content_type,
            ContentType::Text {
                kind: Text::Plain,
                ..
            }
        ) {
            let content = a.text(Text::Plain);
            if content
                .trim_start()
                .starts_with("-----BEGIN PGP MESSAGE-----")
                && content.trim_end().ends_with("-----END PGP MESSAGE-----")
            {
                // Clear text
                let octet_stream =
                    melib::email::pgp::convert_attachment_to_rfc_spec(content.trim().as_bytes());
                return backend.decrypt(&octet_stream)?.await;
            }
        }
        return Err(Error::new("No encrypted payload found").set_kind(ErrorKind::ValueError));
    };
    let blob = parts
        .iter()
        .find(|p| p.content_type == "application/octet-stream")
        .ok_or_else(|| Error::new("No encrypted payload found").set_kind(ErrorKind::ValueError))?;
    let decoded_octet_stream = blob.decode(Default::default());
    backend.decrypt(&decoded_octet_stream)?.await
}

pub fn verify(
    mut backend: impl PGPBackend,
    a: Attachment,
) -> impl Future<Output = Result<SignaturesMetadata>> {
    thread_local! {
        static CACHE: Arc<Mutex<BTreeMap<u64, Result<SignaturesMetadata>>>> = Arc::new(Mutex::new(BTreeMap::new()));
    }

    let cache = CACHE.with(|cache| cache.clone());
    async move {
        let mut hasher = DefaultHasher::new();
        let unverified_signature = melib_pgp::extract_unverified_signature(&a)
            .chain_err_summary(|| "Could not verify signature.")?;
        match unverified_signature {
            UnverifiedSignature::Detached {
                signed_part,
                signature,
            } => {
                signed_part.hash(&mut hasher);
                signature.body().hash(&mut hasher);
                let attachment_hash: u64 = hasher.finish();

                {
                    let lck = cache.lock().unwrap();
                    let in_cache: bool = lck.contains_key(&attachment_hash);
                    if in_cache {
                        return lck[&attachment_hash].clone();
                    }
                }

                let result = backend.verify(signature.body().trim(), &signed_part)?.await;
                {
                    let mut lck = cache.lock().unwrap();
                    lck.insert(attachment_hash, result.clone());
                }
                result
            }
            UnverifiedSignature::Cleartext { text } => {
                text.hash(&mut hasher);
                let attachment_hash: u64 = hasher.finish();

                {
                    let lck = cache.lock().unwrap();
                    let in_cache: bool = lck.contains_key(&attachment_hash);
                    if in_cache {
                        return lck[&attachment_hash].clone();
                    }
                }

                let result = backend.verify_cleartext(&text)?.await;
                {
                    let mut lck = cache.lock().unwrap();
                    lck.insert(attachment_hash, result.clone());
                }
                result
            }
        }
    }
}

pub fn signatures_into_error(metadata: SignaturesMetadata) -> Result<Option<String>> {
    let mut comment = String::new();

    for sig in metadata.signatures.into_iter().rev() {
        let Signature {
            summary,
            cert:
                Recipient {
                    keyid: fingerprint,
                    status,
                },
            validity,
            validity_reason,
            cleartext,
        } = sig;
        if let Err(err) = status {
            return Err(Error::new(format!("BAD signature from {fingerprint}"))
                .set_source(Some(melib::src_err_arc_wrap! { err }))
                .set_kind(ErrorKind::ValueError));
        }
        if cleartext {
            comment = format!("{comment}[SAFETY WARNING: Cleartext signature!]");
        }
        if let Some(validity_reason) = validity_reason {
            comment =
                format!("{comment}good signature by {fingerprint}:{summary}{validity_reason}\n");
        } else {
            let validity = validity.string_representation();
            comment = format!(
                "{comment}good signature by {fingerprint}{colon}{summary}[{validity}]\n",
                colon = if summary.is_empty() { "" } else { ":" }
            );
        }
    }
    if comment.ends_with('\n') {
        comment.pop();
    }

    if comment.is_empty() {
        return Ok(None);
    }

    Ok(Some(comment))
}

pub fn sign_filter(
    choice: PGPBackendChoice,
    default_key: Option<String>,
    mut sign_keys: Vec<Key>,
) -> Result<impl FnOnce(AttachmentBuilder) -> AttachmentBoxFuture + Send> {
    Ok(move |a: AttachmentBuilder| -> AttachmentBoxFuture {
        Box::pin(async move {
            let mut backend = choice.instantiate()?;
            if let Some(default_key) = default_key {
                backend.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = backend.keylist(false, Some(default_key.clone()))?.await?;
                if keys.is_empty() {
                    return Err(Error::new(format!(
                        "Could not locate sign key with ID `{default_key}`"
                    )));
                }
                sign_keys.extend(keys);
            }
            if sign_keys.is_empty() {
                return Err(Error::new(
                    "No key was selected for signing; please select one.",
                ));
            }
            let a: Attachment = a.into();
            let signed_data = melib_pgp::convert_attachment_to_rfc_spec(a.into_raw().as_bytes());
            let (sig_metadata, sig_bytes) = backend.sign(sign_keys, &signed_data, false)?.await?;
            let sig_attachment =
                Attachment::new(ContentType::PGPSignature, Default::default(), sig_bytes);
            let a: AttachmentBuilder = a.into();
            let parts = vec![a, sig_attachment.into()];
            let boundary = ContentType::make_boundary(&parts);

            let micalg = sig_metadata.micalg().into_bytes();
            Ok(Attachment::new(
                ContentType::Multipart {
                    boundary: boundary.into_bytes(),
                    kind: MultipartType::Signed,
                    parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                    parameters: vec![
                        (b"micalg".into(), micalg),
                        (b"protocol".into(), b"\"application/pgp-signature\"".into()),
                    ],
                },
                Default::default(),
                vec![],
            )
            .into())
        })
    })
}

pub fn encrypt_filter(
    choice: PGPBackendChoice,
    encrypt_for_self: Option<melib::Address>,
    default_sign_key: Option<String>,
    mut sign_keys: Option<Vec<Key>>,
    default_encrypt_key: Option<String>,
    mut encrypt_keys: Vec<Key>,
) -> Result<impl FnOnce(AttachmentBuilder) -> AttachmentBoxFuture + Send> {
    Ok(move |a: AttachmentBuilder| -> AttachmentBoxFuture {
        Box::pin(async move {
            let mut backend = choice.instantiate()?;
            if let Some(default_key) = default_sign_key {
                backend.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = backend.keylist(true, Some(default_key.clone()))?.await?;
                if keys.is_empty() {
                    return Err(Error::new(format!(
                        "Could not locate sign key with ID `{default_key}`"
                    )));
                }
                if let Some(ref mut sign_keys) = sign_keys {
                    sign_keys.extend(keys);
                } else {
                    sign_keys = Some(keys);
                }
            }
            if let Some(ref sign_keys) = sign_keys {
                if sign_keys.is_empty() {
                    return Err(Error::new(
                        "No key was selected for signing; please select one.",
                    ));
                }
            }
            if let Some(default_key) = default_encrypt_key {
                backend.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = backend.keylist(false, Some(default_key.clone()))?.await?;
                if keys.is_empty() {
                    return Err(Error::new(format!(
                        "Could not locate encryption key with ID `{default_key}`"
                    )));
                }
                encrypt_keys.extend(keys);
            }
            if encrypt_keys.is_empty() {
                return Err(Error::new(
                    "No key was selected for encryption; please select one.",
                ));
            }
            if let Some(encrypt_for_self) = encrypt_for_self {
                backend.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = backend
                    .keylist(false, Some(encrypt_for_self.to_string()))?
                    .await?;
                if keys.is_empty() {
                    return Err(Error::new(format!(
                        "Could not locate personal encryption key for address `{encrypt_for_self}`"
                    )));
                }
                for key in keys {
                    if !encrypt_keys.contains(&key) {
                        encrypt_keys.push(key);
                    }
                }
            }
            let a: Attachment = if let Some(sign_keys) = sign_keys {
                let a: Attachment = a.into();
                let data = melib_pgp::convert_attachment_to_rfc_spec(a.into_raw().as_bytes());
                let (sig_metadata, sig_bytes) = backend.sign(sign_keys, &data, false)?.await?;
                let sig_attachment =
                    Attachment::new(ContentType::PGPSignature, Default::default(), sig_bytes);
                let a: AttachmentBuilder = a.into();
                let parts = vec![a, sig_attachment.into()];
                let boundary = ContentType::make_boundary(&parts);
                let micalg = sig_metadata.micalg().into_bytes();
                Attachment::new(
                    ContentType::Multipart {
                        boundary: boundary.into_bytes(),
                        kind: MultipartType::Signed,
                        parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                        parameters: vec![
                            (b"micalg".into(), micalg),
                            (b"protocol".into(), b"\"application/pgp-signature\"".into()),
                        ],
                    },
                    Default::default(),
                    vec![],
                )
            } else {
                a.into()
            };
            let data = a.into_raw().into_bytes();

            let enc_attachment = {
                let mut a = Attachment::new(
                    ContentType::OctetStream {
                        name: None,
                        parameters: vec![],
                    },
                    Default::default(),
                    backend.encrypt(encrypt_keys, &data)?.await?,
                );
                a.content_disposition =
                    ContentDisposition::from(br#"attachment; filename="msg.asc""#);
                a
            };
            let mut a: AttachmentBuilder = AttachmentBuilder::new(b"Version: 1\n");

            a.set_content_type_from_bytes(b"application/pgp-encrypted");
            a.set_content_disposition(ContentDisposition::from(b"attachment"));
            let parts = vec![a, enc_attachment.into()];
            let boundary = ContentType::make_boundary(&parts);
            Ok(Attachment::new(
                ContentType::Multipart {
                    boundary: boundary.into_bytes(),
                    kind: MultipartType::Encrypted,
                    parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                    parameters: vec![(b"protocol".into(), b"\"application/pgp-encrypted\"".into())],
                },
                Default::default(),
                vec![],
            )
            .into())
        })
    })
}

impl PGPBackendChoice {
    #[inline]
    pub fn instantiate(&'_ self) -> Result<PGPBackendInstance<'_>> {
        self.try_into()
    }
}

pub enum PGPBackendInstance<'a> {
    #[cfg(feature = "gpgme")]
    GpgME { ctx: melib::gpgme::Context },
    CLI {
        auto_key_locate: LocateKey,
        cli: &'a PGPBackendCLI,
    },
}

impl<'a> TryFrom<&'a PGPBackendChoice> for PGPBackendInstance<'a> {
    type Error = Error;

    fn try_from(choice: &'a PGPBackendChoice) -> Result<Self> {
        match choice {
            #[cfg(feature = "gpgme")]
            PGPBackendChoice::GpgME => Ok(Self::GpgME {
                ctx: melib::gpgme::Context::new()?,
            }),
            #[cfg(not(feature = "gpgme"))]
            PGPBackendChoice::GpgME => Err(Error::new(
                "Cannot instantiate GpgME backend: meli must be compiled with libgpgme. Try \
                 choosing another PGP backend.",
            )
            .set_kind(ErrorKind::Configuration)),
            PGPBackendChoice::CLI(ref cli) => Ok(Self::CLI {
                auto_key_locate: LocateKey::default(),
                cli,
            }),
        }
    }
}

impl<'a> PGPBackend for PGPBackendInstance<'a> {
    fn set_auto_key_locate(&mut self, val: LocateKey) -> Result<()> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => {
                ctx.set_auto_key_locate(val)?;
                Ok(())
            }
            Self::CLI {
                auto_key_locate, ..
            } => {
                *auto_key_locate = val;
                Ok(())
            }
        }
    }

    fn get_auto_key_locate(&self) -> Result<LocateKey> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => ctx.get_auto_key_locate(),
            Self::CLI {
                auto_key_locate, ..
            } => Ok(*auto_key_locate),
        }
    }

    fn get_key(&self, secret: bool, pattern: String) -> ResultFuture<Key> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::get_key(ctx, secret, pattern),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let get_key_command = cli.get_key_command.to_string();
                let auto_key_locate = *auto_key_locate;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let auto_key_locate = auto_key_locate.to_string();
                        let mut envs = vec![("AUTO_KEY_LOCATE", auto_key_locate.as_str())];
                        if secret {
                            envs.push(("SECRET", ""));
                        }
                        let output = Command::new(&get_key_command)
                            .envs(envs)
                            .arg(&pattern)
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {get_key_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{get_key_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        Ok(
                            serde_json::from_slice::<Key>(&output.stdout).map_err(|err| {
                                format!(
                                    "Could not deserialize key response from {get_key_command}: \
                                     {err}"
                                )
                            })?,
                        )
                    })
                    .await
                }))
            }
        }
    }

    fn verify(&mut self, signature: &[u8], text: &[u8]) -> ResultFuture<SignaturesMetadata> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::verify(ctx, signature, text),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let verify_command = cli.verify_command.to_string();
                let auto_key_locate = *auto_key_locate;
                let signature = File::create_temp_file(signature, None, None, None, true)?;
                let text = File::create_temp_file(text, None, None, None, true)?;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let output = Command::new(&verify_command)
                            .arg(signature.path())
                            .arg(text.path())
                            .env("AUTO_KEY_LOCATE", auto_key_locate.to_string())
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {verify_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{verify_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        let signatures: Vec<Signature> = serde_json::from_slice(&output.stdout)
                            .map_err(|err| {
                                format!(
                                    "Could not deserialize signature response from \
                                     {verify_command}: {err}"
                                )
                            })?;
                        Ok(SignaturesMetadata { signatures })
                    })
                    .await
                }))
            }
        }
    }

    fn verify_cleartext(&mut self, text: &[u8]) -> ResultFuture<SignaturesMetadata> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::verify_cleartext(ctx, text),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let verify_command = cli.verify_command.to_string();
                let auto_key_locate = *auto_key_locate;
                let text = File::create_temp_file(text, None, None, None, true)?;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let output = Command::new(&verify_command)
                            .arg(text.path())
                            .env("AUTO_KEY_LOCATE", auto_key_locate.to_string())
                            .env("CLEARTEXT", "")
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {verify_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{verify_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        let signatures: Vec<Signature> = serde_json::from_slice(&output.stdout)
                            .map_err(|err| {
                                format!(
                                    "Could not deserialize signature response from \
                                     {verify_command}: {err}"
                                )
                            })?;
                        Ok(SignaturesMetadata { signatures })
                    })
                    .await
                }))
            }
        }
    }

    fn keylist(&self, secret: bool, pattern: Option<String>) -> ResultFuture<Vec<Key>> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::keylist(ctx, secret, pattern),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let keylist_command = cli.keylist_command.to_string();
                let auto_key_locate = *auto_key_locate;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let auto_key_locate = auto_key_locate.to_string();
                        let mut envs = vec![("AUTO_KEY_LOCATE", auto_key_locate.as_str())];
                        if secret {
                            envs.push(("SECRET", ""));
                        }
                        let mut cmd = Command::new(&keylist_command);
                        if let Some(ref pattern) = pattern {
                            cmd.arg(pattern);
                        }
                        let output = cmd
                            .envs(envs)
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {keylist_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{keylist_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        Ok(
                            serde_json::from_slice::<Vec<Key>>(&output.stdout).map_err(|err| {
                                format!(
                                    "Could not deserialize keys response from {keylist_command}: \
                                     {err}"
                                )
                            })?,
                        )
                    })
                    .await
                }))
            }
        }
    }

    fn sign(
        &mut self,
        sign_keys: Vec<Key>,
        text: &[u8],
        is_binary: bool,
    ) -> ResultFuture<(NewSignature, Vec<u8>)> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::sign(ctx, sign_keys, text, is_binary),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let sign_command = cli.sign_command.to_string();
                let auto_key_locate = *auto_key_locate;
                let text = File::create_temp_file(text, None, None, None, true)?;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let mut cmd = Command::new(&sign_command);
                        cmd.env("AUTO_KEY_LOCATE", auto_key_locate.to_string())
                            .arg(text.path());
                        if is_binary {
                            cmd.env("IS_BINARY", "");
                        }
                        for key in sign_keys {
                            cmd.arg(&key.fingerprint);
                        }
                        let output = cmd
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {sign_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{sign_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        use serde::de::Deserialize;
                        Ok(
                            serde_json::from_slice::<[serde_json::Value; 2]>(&output.stdout)
                                .and_then(|[n, b]| {
                                    Ok((NewSignature::deserialize(n)?, <Vec<u8>>::deserialize(b)?))
                                })
                                .map_err(|err| {
                                    format!(
                                        "Could not deserialize new signature response from \
                                         {sign_command}: {err}"
                                    )
                                })?,
                        )
                    })
                    .await
                }))
            }
        }
    }

    fn encrypt(&mut self, encrypt_keys: Vec<Key>, plain: &[u8]) -> ResultFuture<Vec<u8>> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::encrypt(ctx, encrypt_keys, plain),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let encrypt_command = cli.encrypt_command.to_string();
                let auto_key_locate = *auto_key_locate;
                let plain = File::create_temp_file(plain, None, None, None, true)?;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let mut cmd = Command::new(&encrypt_command);
                        cmd.env("AUTO_KEY_LOCATE", auto_key_locate.to_string())
                            .arg(plain.path());
                        for key in encrypt_keys {
                            cmd.arg(&key.fingerprint);
                        }
                        let output = cmd
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {encrypt_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{encrypt_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        Ok(
                            serde_json::from_slice::<Vec<u8>>(&output.stdout).map_err(|err| {
                                format!(
                                    "Could not deserialize encryption bytes response from \
                                     {encrypt_command}: {err}"
                                )
                            })?,
                        )
                    })
                    .await
                }))
            }
        }
    }

    fn decrypt(&mut self, cipher: &[u8]) -> ResultFuture<(DecryptionMetadata, Vec<u8>)> {
        match self {
            #[cfg(feature = "gpgme")]
            Self::GpgME { ctx } => PGPBackend::decrypt(ctx, cipher),
            Self::CLI {
                ref auto_key_locate,
                cli,
                ..
            } => {
                let decrypt_command = cli.decrypt_command.to_string();
                let auto_key_locate = *auto_key_locate;
                let cipher = File::create_temp_file(cipher, None, None, None, true)?;
                Ok(Box::pin(async move {
                    melib::smol::unblock(move || {
                        let output = Command::new(&decrypt_command)
                            .env("AUTO_KEY_LOCATE", auto_key_locate.to_string())
                            .arg(cipher.path())
                            .stdin(Stdio::null())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped())
                            .output()
                            .chain_err_summary(|| format!("Could not launch {decrypt_command}"))?;
                        if !output.status.success() {
                            return Err(format!("{decrypt_command} exited with {output:?}").into());
                        }
                        if let Ok(err) = serde_json::from_slice::<String>(&output.stdout) {
                            return Err(err.into());
                        }
                        Ok((
                            DecryptionMetadata::default(),
                            serde_json::from_slice::<Vec<u8>>(&output.stdout).map_err(|err| {
                                format!(
                                    "Could not deserialize decryption response from \
                                     {decrypt_command}: {err}"
                                )
                            })?,
                        ))
                    })
                    .await
                }))
            }
        }
    }
}

#[cfg(all(feature = "gpgme", test))]
mod tests {
    // NOTE: debug stuff with `GPGME_DEBUG=9:/tmp/mygpgme.log` etc.

    use std::{
        borrow::Cow,
        ffi::CString,
        process::{Command, Stdio},
    };

    use melib::{
        gpgme::{EngineInfo, Protocol},
        log, smol,
        utils::logging::{LogLevel, Logger},
    };
    use rusty_fork::rusty_fork_test;

    use super::*;

    // Keys generated with <https://github.com/epilys/gen-rfc9500-gpg-keys>

    const PUBKEY: &[u8] = b"-----BEGIN PGP PUBLIC KEY BLOCK-----\r\n\r\nxsBNBAAAAAABCACw+egZQ6eumJKq3hfKfED4dE/tL4FI5sjqont9ABVI+1GSqyi1\r\nbFBgsRjM0THllIdMbKmJtWwnKW8J+5OgNN8y6Xxv8JmM/Y5vQt2lis0fqXmG8UTz\r\n0VTWdlAXXmhUs6lSADvAaIe4RVrCsZ97L3ZQTryY7JRVcbB4khUN3Gp0yg+801SX\r\nzoFTTa+UGIRLE66jH51aa5VXu99hnv1OiH8tQrjdi8mH6uG/icq4XuIeNWMF32wH\r\nqIOOPvQcWV3M5D2vxJEj702Ku6k9OQXkAo17qRSEonWW4HtLbtmS8He1JNPc/n3d\r\nVUm+fM6NoDXPoLP7j55G9zKyqGtGAWXAj1MTABEBAAHNEHVzZXJAZXhhbXBsZS5v\r\ncmfCwLsEEwEKAG8FggAAAAAJEMwuljyZl1FjRxQAAAAAAB4AIHNhbHRAbm90YXRp\r\nb25zLnNlcXVvaWEtcGdwLm9yZ56lfAkULy8QwPhEcrlasB0N4oBn0im6wT4mwiAT\r\nHZjBFiEErtwR+84tdGv4v3FmzC6WPJmXUWMAAI1tCACHuuzmgEqoIrk3QZaZwReK\r\nzNOs/einaVqItsI38AWLlyruwM+5IBYskBx7EjPk/yBMyWSR0X9WxiBpuXrxcpql\r\nqU8NUYXEEQeo57921ol9FnAWEp2Aqo11O5r26P7XDv+IDj0qX3+uAjSwmH0wJvrH\r\nloWCBooVuEaMX0VeMcuVXzqGZtHMp8DB1sWJMof1Znhrx3N/tAV+RnYdzuhBIgci\r\nUZRQ5MLqrt8ks9fyIAL3btRS2nsBGdyTbzFxVkoxc4yRx2ZiNiB8OlMzGk5YoiOf\r\ntkKM/mF6HTpfppF0CIhuo/q29lUCSpQDmfjksawPq3Z6LGaqw4vsj5fHEo7k47Nu\r\n=1oV4\r\n-----END PGP PUBLIC KEY BLOCK-----\r\n";

    const PRIVKEY: &[u8] = b"-----BEGIN PGP PRIVATE KEY BLOCK-----\r\n\r\nxcLYBAAAAAABCACw+egZQ6eumJKq3hfKfED4dE/tL4FI5sjqont9ABVI+1GSqyi1\r\nbFBgsRjM0THllIdMbKmJtWwnKW8J+5OgNN8y6Xxv8JmM/Y5vQt2lis0fqXmG8UTz\r\n0VTWdlAXXmhUs6lSADvAaIe4RVrCsZ97L3ZQTryY7JRVcbB4khUN3Gp0yg+801SX\r\nzoFTTa+UGIRLE66jH51aa5VXu99hnv1OiH8tQrjdi8mH6uG/icq4XuIeNWMF32wH\r\nqIOOPvQcWV3M5D2vxJEj702Ku6k9OQXkAo17qRSEonWW4HtLbtmS8He1JNPc/n3d\r\nVUm+fM6NoDXPoLP7j55G9zKyqGtGAWXAj1MTABEBAAEAB/9BGIsgz9vbws8f/nUt\r\ny6pyOQY1LiYV1J3OgFl/zwoFQDvvAPoGUYL3Lez7WW9LDOj/WXC68HqJpRnsyBay\r\n9P+sUGmvGwa/73v2vNeeToHIxaOn2RMNw8+62uX20oj5ruP2/5L64Pga9Ze+yWrp\r\n+rlALNX+QfcFvr20e7c20/5sWlHg4gcyqXteRsHL2ybXSFTGtmBK7UY3Nf+QdgRl\r\nV8r5Sb9EiJXCBDLB4JwBTqdWYENPGg874pS6vF1TDmoQIT9TtgN1/ISnVz8q8SFV\r\nhPW0vabU6PnhenjZfne4baShhGR1MYp6EKVhAU7/ojqB7Fbp5BCd74yz95ciP32N\r\nDUNRBADM8eW7kMjpeB6nW+vxC8JS4R6wI6AmDxiHVSpWhj9KZCHoxgC/Uj1ssbCt\r\nvdZb/uSoigN+PRpBXlu5VkjaWgyia1T0pjlIUiw9X4m5SnLv/5UTTVlAzkV1jzCJ\r\ngJCJVliO71dbPkvEw2jP6BPunCUsKwLg35HxqgGTjThoXWC6bwQA3RBXAjgvIys2\r\ngfU3keImF8e/TprLge1I2vbWmV2j6rZCg5r/AS0upii5CvJ5/T5vfJPNgPBy8B/y\r\nRDs+6PJO1GmnlhOkG9JAIPkv0RBZvR0PMBtbp6nTY3yo1lwamBVBfY6rc0sLTzos\r\nZh2aGoLzrHNMQFMGaauORzBFpY5lU50D/AqB2KYYMUqAOvYcBnEfLDmyZv9BTVNH\r\nbR2lKkMYqv5LlvDaBxVfilE02riO4p6BaAdvzXjKeRrGNEKoHNBpOSfYCOM16NjL\r\n8hIZB1CaV3WbT5oY+jp7Mzd57d56RZOE+ERK2uz/7JX9VSsM/LbH9pJibd4e8mik\r\nDS9ntciqOH/3QwrNEHVzZXJAZXhhbXBsZS5vcmfCwLsEEwEKAG8FggAAAAAJEMwu\r\nljyZl1FjRxQAAAAAAB4AIHNhbHRAbm90YXRpb25zLnNlcXVvaWEtcGdwLm9yZ56l\r\nfAkULy8QwPhEcrlasB0N4oBn0im6wT4mwiATHZjBFiEErtwR+84tdGv4v3FmzC6W\r\nPJmXUWMAAI1tCACHuuzmgEqoIrk3QZaZwReKzNOs/einaVqItsI38AWLlyruwM+5\r\nIBYskBx7EjPk/yBMyWSR0X9WxiBpuXrxcpqlqU8NUYXEEQeo57921ol9FnAWEp2A\r\nqo11O5r26P7XDv+IDj0qX3+uAjSwmH0wJvrHloWCBooVuEaMX0VeMcuVXzqGZtHM\r\np8DB1sWJMof1Znhrx3N/tAV+RnYdzuhBIgciUZRQ5MLqrt8ks9fyIAL3btRS2nsB\r\nGdyTbzFxVkoxc4yRx2ZiNiB8OlMzGk5YoiOftkKM/mF6HTpfppF0CIhuo/q29lUC\r\nSpQDmfjksawPq3Z6LGaqw4vsj5fHEo7k47Nu\r\n=tzjb\r\n-----END PGP PRIVATE KEY BLOCK-----\r\n";

    const CLEARTEXT_SIGNATURE: &[u8] = b"-----BEGIN PGP SIGNED MESSAGE-----\r\nHash: SHA512\r\n\r\nSample text for gpg signing\r\n\r\n-----BEGIN PGP SIGNATURE-----\r\n\r\nwsC7BAEBCgBvBYJqpsT9CRDMLpY8mZdRY0cUAAAAAAAeACBzYWx0QG5vdGF0aW9u\r\ncy5zZXF1b2lhLXBncC5vcme0O9QbQebgQFHUXDJ4BoXVa2gQocfWivPCQ1vnV0oE\r\nPxYhBK7cEfvOLXRr+L9xZswuljyZl1FjAABKcwf/U6P77F1e0JfG32SdlX8KRar/\r\nxxOBY4rewWFe0LX0iMICRXcoMuPDBa85V1IVY9zKxHfxXuk2Vyy7QK+UjKXPRK9X\r\nYJzN/h181kcAeuV/4FtGqbSa9cg0OWHvoA1trgppK+EaLtiQ/QOpZegOB0ACI+dv\r\nvm275yNfh3VZecUWLJ3qLxPqmB55/7/4EO56yc0Y9/dus9kACnvEI37k9AiVdniR\r\n/WXgZ9Vfr0FPLlWluwJEdRY+eNrfa4dlh+LbACDhtGE04bKyy9YzSMNVqky5gAxp\r\nOc8CN9JSZ1EJryg0qcAdFQYj4wpXQgl7Tn29eJeOGxjoozjjM6gwHUmBGQ6fSw==\r\n=XkqT\r\n-----END PGP SIGNATURE-----\r\n";

    rusty_fork_test! {
        #[test]
        /// Test that a generated signature is valid.
        fn test_gpg_signatures() {
            run_gpg_signatures();
        }

        #[test]
        /// Test that we can verify and decrypt cleartext messages
        fn test_gpg_cleartext() {
            run_gpg_cleartext();
        }

        #[test]
        /// Test that we can encrypt/decrypt
        fn test_gpg_encryption() {
            run_gpg_encryption();
        }

        #[test]
        fn test_gpg_cli() {
            run_gpg_cli();
        }
    }

    struct GpgTest {
        _logger: Logger,
        tempdir: tempfile::TempDir,
        gpgme_ctx: melib::gpgme::Context,
    }

    fn setup() -> Option<GpgTest> {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let tempdir = tempfile::tempdir().unwrap();
        {
            #[allow(unused_unsafe)]
            unsafe {
                std::env::set_var("GNUPGHOME", tempdir.path());
            }

            #[allow(unused_unsafe)]
            unsafe {
                std::env::set_var("GPG_AGENT_INFO", "");
            }
        }

        let mut gpgme_ctx = match melib::gpgme::Context::new() {
            Ok(v) => v,
            Err(err) if err.kind.is_not_found() => {
                log::info!("libgpgme could not be loaded, skipping this test.");
                return None;
            }
            err => err.unwrap(),
        };
        let current_engine_info = gpgme_ctx.engine_info().unwrap();
        let prev_len = current_engine_info.len();
        let Some(EngineInfo {
            file_name: Some(engine_file_name),
            ..
        }) = current_engine_info
            .iter()
            .find(|eng| eng.protocol == Protocol::OpenPGP)
        else {
            log::warn!(
                "WARN: No openpg protocol engine returned from gpgme. Returned protocols: \
                 {current_engine_info:?}"
            );
            return None;
        };
        gpgme_ctx
            .set_engine_info(
                Protocol::OpenPGP,
                Some(Cow::Owned(CString::new(engine_file_name.clone()).unwrap())),
                Some(Cow::Owned(
                    CString::new(tempdir.path().display().to_string()).unwrap(),
                )),
            )
            .unwrap();
        let new_engine_info = gpgme_ctx.engine_info().unwrap();
        // Sanity check:
        assert_eq!(
            new_engine_info.len(),
            prev_len,
            "new_engine_info was expected to have {} entry/ies but has {}: {:#?}",
            prev_len,
            new_engine_info.len(),
            new_engine_info
        );
        // Sanity check:
        assert_eq!(
            new_engine_info[0].home_dir,
            Some(tempdir.path().display().to_string()),
            "new_engine_info was expected to have temp dir as home_dir but has: {:#?}",
            new_engine_info[0].home_dir
        );
        Some(GpgTest {
            _logger,
            tempdir,
            gpgme_ctx,
        })
    }

    fn run_gpg_signatures() {
        let Some(GpgTest {
            _logger,
            tempdir,
            mut gpgme_ctx,
        }) = setup()
        else {
            return;
        };
        // Add public key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PUBKEY).unwrap())
            .unwrap();

        // Retrieve public key
        let pubkey: Key = smol::block_on(gpgme_ctx.keylist(false, None).unwrap())
            .unwrap()
            .into_iter()
            .find(|key| key.fingerprint() == "AEDC11FBCE2D746BF8BF7166CC2E963C99975163")
            .unwrap()
            .into();

        let mut draft = melib::Draft::default();
        draft.set_body("foobar\r\n\r\n".into());
        draft
            .try_set_header("From", "user@example.org".into())
            .unwrap();
        draft
            .try_set_header("To", "user@example.org".into())
            .unwrap();

        let body_attachment: AttachmentBuilder = Attachment::new(
            ContentType::default(),
            Default::default(),
            std::mem::take(&mut draft.body).into_bytes(),
        )
        .into();

        // Verify that we cannot use a keypair to sign if we don't have its secret key:
        let err = smol::block_on((sign_filter(
            PGPBackendChoice::GpgME,
            None,
            vec![pubkey.clone()],
        )
        .unwrap())(body_attachment.clone()))
        .unwrap_err();
        assert!(
            err.summary.starts_with(
                "libgpgpme: No secret key found with key id \
                 AEDC11FBCE2D746BF8BF7166CC2E963C99975163."
            ),
            "{err}"
        );

        // Add private key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PRIVKEY).unwrap())
            .unwrap();

        let body: AttachmentBuilder =
            smol::block_on((sign_filter(PGPBackendChoice::GpgME, None, vec![pubkey])
                .unwrap())(body_attachment))
            .unwrap();
        draft.attachments.insert(0, body);
        let raw_mail = draft.finalise().unwrap();
        //eprintln!("{raw_mail}");
        let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");

        let signatures = smol::block_on(verify(gpgme_ctx.clone(), mail.body())).unwrap();
        assert_eq!(
            &signatures_into_error(signatures).unwrap().unwrap(),
            "good signature by AEDC11FBCE2D746BF8BF7166CC2E963C99975163[?]"
        );

        let attachments = mail.body().attachments();

        let sig_bytes = attachments
            .iter()
            .find(|a| matches!(a.content_type, ContentType::PGPSignature))
            .unwrap()
            .raw();
        let signed_bytes = attachments
            .iter()
            .find(|a| matches!(a.content_type, ContentType::Text { .. }))
            .unwrap()
            .raw();

        let sig_file = tempdir.path().join("sig");
        let signed_file = tempdir.path().join("mime");

        let sig_bytes = sig_bytes.strip_prefix(b"Content-Transfer-Encoding: 8bit\r\nContent-Type: application/pgp-signature; charset=\"utf-8\"; name=\"signature.asc\"\r\nContent-Description: Digital signature\r\nContent-Disposition: inline\r\n\r\n").unwrap().to_vec();
        let signed_bytes = signed_bytes.to_vec();

        std::fs::write(&sig_file, &sig_bytes).unwrap();
        std::fs::write(&signed_file, signed_bytes).unwrap();

        if !matches!(Command::new("sh")
                .arg("-c")
                .arg("command -v gpg")
                .stdout(Stdio::null())
                .stdin(Stdio::null())
                .stderr(Stdio::null()).output(), Ok(out) if out.status.success())
        {
            log::info!("'gpg' binary not found in PATH, skipping verification.");
            return;
        }
        let output = Command::new("gpg")
            .arg("--verify")
            .arg(&sig_file)
            .arg(&signed_file)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "gpg --verify exited with {output:?}"
        );
        _ = tempdir.close();
    }

    fn run_gpg_encryption() {
        let Some(GpgTest {
            _logger,
            tempdir,
            mut gpgme_ctx,
        }) = setup()
        else {
            return;
        };

        // Add private key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PRIVKEY).unwrap())
            .unwrap();

        // Retrieve public key
        let pubkey: Key = smol::block_on(gpgme_ctx.keylist(false, None).unwrap())
            .unwrap()
            .into_iter()
            .find(|key| key.fingerprint() == "AEDC11FBCE2D746BF8BF7166CC2E963C99975163")
            .unwrap()
            .into();

        {
            let mut draft = melib::Draft::default();
            draft.set_body("foobar\r\n\r\n".into());
            draft
                .try_set_header("From", "user@example.org".into())
                .unwrap();
            draft
                .try_set_header("To", "user@example.org".into())
                .unwrap();

            let body_attachment: AttachmentBuilder = Attachment::new(
                ContentType::default(),
                Default::default(),
                std::mem::take(&mut draft.body).into_bytes(),
            )
            .into();

            let body: AttachmentBuilder =
                smol::block_on((encrypt_filter(
                    PGPBackendChoice::GpgME,
                    None,
                    None,
                    None,
                    None,
                    vec![pubkey.clone()],
                )
                .unwrap())(body_attachment.clone()))
                .unwrap();

            draft.attachments.insert(0, body);
            let raw_mail = draft.finalise().unwrap();
            let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");

            let (decrypted_metadata, decrypted) =
                smol::block_on(decrypt(gpgme_ctx.clone(), mail.body()))
                    .expect("Could not decrypt email");

            assert_eq!(
                decrypted_metadata,
                DecryptionMetadata {
                    recipients: vec![Recipient {
                        keyid: "CC2E963C99975163".into(),
                        status: Ok((),),
                    },],
                    file_name: None,
                    session_key: None,
                    is_mime: false,
                }
            );
            assert_eq!(
                body_attachment.build().into_raw(),
                String::from_utf8_lossy(&decrypted)
            );
        }
        // Do the same thing but this time also sign
        {
            let mut draft = melib::Draft::default();
            draft.set_body("foobar\r\n\r\n".into());
            draft
                .try_set_header("From", "user@example.org".into())
                .unwrap();
            draft
                .try_set_header("To", "user@example.org".into())
                .unwrap();
            let body_attachment: AttachmentBuilder = Attachment::new(
                ContentType::default(),
                Default::default(),
                std::mem::take(&mut draft.body).into_bytes(),
            )
            .into();
            let body: AttachmentBuilder =
                smol::block_on((encrypt_filter(
                    PGPBackendChoice::GpgME,
                    None,
                    None,
                    Some(vec![pubkey.clone()]),
                    None,
                    vec![pubkey],
                )
                .unwrap())(body_attachment.clone()))
                .unwrap();

            draft.attachments.insert(0, body);
            let raw_mail = draft.finalise().unwrap();
            let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");

            let (decrypted_metadata, decrypted) =
                smol::block_on(decrypt(gpgme_ctx.clone(), mail.body()))
                    .expect("Could not decrypt email");

            assert_eq!(
                decrypted_metadata,
                DecryptionMetadata {
                    recipients: vec![Recipient {
                        keyid: "CC2E963C99975163".into(),
                        status: Ok((),),
                    },],
                    file_name: None,
                    session_key: None,
                    is_mime: false,
                }
            );
            let decrypted = AttachmentBuilder::new(&decrypted).build();
            let signatures = smol::block_on(verify(gpgme_ctx.clone(), decrypted.clone())).unwrap();
            assert_eq!(
                &signatures_into_error(signatures).unwrap().unwrap(),
                "good signature by AEDC11FBCE2D746BF8BF7166CC2E963C99975163[?]"
            );

            let attachments = decrypted.attachments();

            let signed_bytes = attachments
                .iter()
                .find(|a| matches!(a.content_type, ContentType::Text { .. }))
                .unwrap()
                .raw();

            assert_eq!(
                String::from_utf8_lossy(&melib_pgp::convert_attachment_to_rfc_spec(
                    &body_attachment.build().into_raw().into_bytes()
                )),
                String::from_utf8_lossy(&melib_pgp::convert_attachment_to_rfc_spec(signed_bytes))
            );
        }
        _ = tempdir.close();
    }

    fn run_gpg_cleartext() {
        let Some(GpgTest {
            _logger,
            tempdir,
            mut gpgme_ctx,
        }) = setup()
        else {
            return;
        };
        // Add public key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PUBKEY).unwrap())
            .unwrap();

        let body: AttachmentBuilder = Attachment::new(
            ContentType::default(),
            Default::default(),
            CLEARTEXT_SIGNATURE.to_vec(),
        )
        .into();

        let mut draft = melib::Draft::default();
        draft.attachments.insert(0, body);
        let raw_mail = draft.finalise().unwrap();
        let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");

        let signatures = smol::block_on(verify(
            PGPBackendChoice::default().instantiate().unwrap(),
            mail.body(),
        ))
        .unwrap();
        assert_eq!(
            &signatures_into_error(signatures).unwrap().unwrap(),
            "[SAFETY WARNING: Cleartext signature!]good signature by \
             AEDC11FBCE2D746BF8BF7166CC2E963C99975163[?]"
        );

        _ = tempdir.close();
    }

    fn run_gpg_cli() {
        let Some(GpgTest {
            _logger,
            tempdir,
            mut gpgme_ctx,
        }) = setup()
        else {
            return;
        };

        let find_cmd = |cmd| {
            Command::new("sh")
                .arg("-c")
                .arg(format!("command -v {cmd}"))
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .stdin(Stdio::null())
                .spawn()
                .map_err(|err| err.to_string())
                .and_then(|find| find.wait_with_output().map_err(|err| err.to_string()))
                .and_then(|output| {
                    if output.status.success() {
                        Ok(output)
                    } else {
                        Err(format!("{output:?}"))
                    }
                })
        };

        if let Err(err) = find_cmd("gpg") {
            eprintln!("gpg binary not found, skipping test: {err:?}");
            return;
        }
        if let Err(err) = find_cmd("python3") {
            eprintln!("python3 binary not found, skipping test: {err:?}");
            return;
        }

        let gpg_verify = tempdir.path().join("gpg_verify.py");
        let gpg_sign = tempdir.path().join("gpg_sign.py");
        let gpg_encrypt = tempdir.path().join("gpg_encrypt.py");
        let gpg_decrypt = tempdir.path().join("gpg_decrypt.py");
        let gpg_get_key = tempdir.path().join("gpg_get_key.py");
        let gpg_keylist = tempdir.path().join("gpg_keylist.py");

        for (path, source) in &[
            (
                &gpg_verify,
                include_bytes!("../../../contrib/pgp-cli-backends/gpg/gpg_verify.py").as_slice(),
            ),
            (
                &gpg_sign,
                include_bytes!("../../../contrib/pgp-cli-backends/gpg/gpg_sign.py").as_slice(),
            ),
            (
                &gpg_encrypt,
                include_bytes!("../../../contrib/pgp-cli-backends/gpg/gpg_encrypt.py").as_slice(),
            ),
            (
                &gpg_decrypt,
                include_bytes!("../../../contrib/pgp-cli-backends/gpg/gpg_decrypt.py").as_slice(),
            ),
            (
                &gpg_get_key,
                include_bytes!("../../../contrib/pgp-cli-backends/gpg/gpg_get_key.py").as_slice(),
            ),
            (
                &gpg_keylist,
                include_bytes!("../../../contrib/pgp-cli-backends/gpg/gpg_keylist.py").as_slice(),
            ),
        ] {
            std::fs::write(path, source).unwrap();
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(path).unwrap().permissions();
            perms.set_mode(perms.mode() | 0o700);
            std::fs::set_permissions(path, perms).unwrap();
        }
        let cli = PGPBackendChoice::CLI(
            PGPBackendCLI {
                verify_command: gpg_verify.display().to_string(),
                sign_command: gpg_sign.display().to_string(),
                encrypt_command: gpg_encrypt.display().to_string(),
                decrypt_command: gpg_decrypt.display().to_string(),
                get_key_command: gpg_get_key.display().to_string(),
                keylist_command: gpg_keylist.display().to_string(),
            }
            .into(),
        );
        // Add public key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PUBKEY).unwrap())
            .unwrap();

        // Retrieve public key
        let pubkey: Key = smol::block_on(
            cli.instantiate()
                .unwrap()
                .get_key(false, "AEDC11FBCE2D746BF8BF7166CC2E963C99975163".into())
                .unwrap(),
        )
        .unwrap();
        assert_eq!(
            pubkey.fingerprint,
            "AEDC11FBCE2D746BF8BF7166CC2E963C99975163"
        );

        // Add private key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PRIVKEY).unwrap())
            .unwrap();

        // Sign and verify
        {
            let mut draft = melib::Draft::default();
            draft.set_body("foobar\r\n\r\n".into());
            draft
                .try_set_header("From", "user@example.org".into())
                .unwrap();
            draft
                .try_set_header("To", "user@example.org".into())
                .unwrap();

            let body_attachment: AttachmentBuilder = Attachment::new(
                ContentType::default(),
                Default::default(),
                std::mem::take(&mut draft.body).into_bytes(),
            )
            .into();

            let body: AttachmentBuilder =
                smol::block_on((sign_filter(cli.clone(), None, vec![pubkey.clone()])
                    .unwrap())(body_attachment))
                .unwrap();
            draft.attachments.insert(0, body);
            let raw_mail = draft.finalise().unwrap();
            //eprintln!("{raw_mail}");
            let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");
            let gpgme_signatures = smol::block_on(verify(gpgme_ctx.clone(), mail.body())).unwrap();
            let cli_signatures =
                smol::block_on(verify(cli.instantiate().unwrap(), mail.body())).unwrap();
            assert_eq!(
                &signatures_into_error(cli_signatures).unwrap().unwrap(),
                &signatures_into_error(gpgme_signatures).unwrap().unwrap(),
            );
        }

        // Encrypt and decrypt
        {
            let mut draft = melib::Draft::default();
            draft.set_body("foobar\r\n\r\n".into());
            draft
                .try_set_header("From", "user@example.org".into())
                .unwrap();
            draft
                .try_set_header("To", "user@example.org".into())
                .unwrap();

            let body_attachment: AttachmentBuilder = Attachment::new(
                ContentType::default(),
                Default::default(),
                std::mem::take(&mut draft.body).into_bytes(),
            )
            .into();
            let body: AttachmentBuilder = smol::block_on((encrypt_filter(
                cli.clone(),
                None,
                None,
                None,
                None,
                vec![pubkey],
            )
            .unwrap())(body_attachment))
            .unwrap();

            draft.attachments.insert(0, body);
            let raw_mail = draft.finalise().unwrap();
            let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");

            let (_, decrypted) = smol::block_on(decrypt(cli.instantiate().unwrap(), mail.body()))
                .expect("Could not decrypt email");
            assert_eq!(
                &String::from_utf8_lossy(&decrypted),
                "Content-Transfer-Encoding: 8bit\r\nContent-Type: text/plain; \
                 charset=\"utf-8\"\r\n\r\nfoobar\r\n\r\n"
            );
        }
        _ = tempdir.close();
    }
}
