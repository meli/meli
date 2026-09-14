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

#[cfg(feature = "gpgme")]
use std::{
    collections::{hash_map::DefaultHasher, BTreeMap},
    future::Future,
    hash::{Hash, Hasher},
    sync::{Arc, Mutex},
};

use melib::{
    email::pgp::{Recipient, Signature, SignaturesMetadata},
    error::*,
};
#[cfg(feature = "gpgme")]
use melib::{
    email::{
        attachment_types::{ContentDisposition, ContentType, MultipartType, Text},
        pgp::{self as melib_pgp, DecryptionMetadata, LocateKey, UnverifiedSignature},
        Attachment, AttachmentBuilder,
    },
    gpgme::*,
    parser::BytesExt,
};

#[cfg(feature = "gpgme")]
use super::AttachmentBoxFuture;

#[cfg(feature = "gpgme")]
/// Decrypts a `multipart/encrypted` or a cleartext encrypted message.
pub async fn decrypt(a: Attachment) -> Result<(DecryptionMetadata, Vec<u8>)> {
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
                let mut ctx = Context::new()?;
                let cipher = ctx.new_data_mem(&octet_stream)?;
                return ctx.decrypt(cipher)?.await;
            }
        }
        return Err(Error::new("No encrypted payload found").set_kind(ErrorKind::ValueError));
    };
    let blob = parts
        .iter()
        .find(|p| p.content_type == "application/octet-stream")
        .ok_or_else(|| Error::new("No encrypted payload found").set_kind(ErrorKind::ValueError))?;
    let decoded_octet_stream = blob.decode(Default::default());
    let mut ctx = Context::new()?;
    let cipher = ctx.new_data_mem(&decoded_octet_stream)?;
    ctx.decrypt(cipher)?.await
}

#[cfg(feature = "gpgme")]
pub fn verify(a: Attachment) -> impl Future<Output = Result<SignaturesMetadata>> {
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

                let mut ctx = Context::new()?;
                let signature = ctx.new_data_mem(signature.body().trim())?;
                let signed_part = ctx.new_data_mem(&signed_part)?;

                let result = ctx.verify(signature, signed_part)?.await;
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

                let mut ctx = Context::new()?;
                let text = ctx.new_data_mem(&text)?;

                let result = ctx.verify_cleartext(text)?.await.map(|(s, _)| s);
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

#[cfg(feature = "gpgme")]
pub fn sign_filter(
    default_key: Option<String>,
    mut sign_keys: Vec<Key>,
) -> Result<impl FnOnce(AttachmentBuilder) -> AttachmentBoxFuture + Send> {
    Ok(move |a: AttachmentBuilder| -> AttachmentBoxFuture {
        Box::pin(async move {
            if let Some(default_key) = default_key {
                let mut ctx = Context::new()?;
                ctx.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = ctx.keylist(false, Some(default_key.clone()))?.await?;
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
            let mut ctx = Context::new()?;
            let signed_data = melib_pgp::convert_attachment_to_rfc_spec(a.into_raw().as_bytes());
            let data = ctx.new_data_mem(&signed_data)?;
            let (sig_metadata, sig_bytes) = ctx.sign(sign_keys, data, false)?.await?;
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

#[cfg(feature = "gpgme")]
pub fn encrypt_filter(
    encrypt_for_self: Option<melib::Address>,
    default_sign_key: Option<String>,
    mut sign_keys: Option<Vec<Key>>,
    default_encrypt_key: Option<String>,
    mut encrypt_keys: Vec<Key>,
) -> Result<impl FnOnce(AttachmentBuilder) -> AttachmentBoxFuture + Send> {
    Ok(move |a: AttachmentBuilder| -> AttachmentBoxFuture {
        Box::pin(async move {
            if let Some(default_key) = default_sign_key {
                let mut ctx = Context::new()?;
                ctx.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = ctx.keylist(true, Some(default_key.clone()))?.await?;
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
                let mut ctx = Context::new()?;
                ctx.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = ctx.keylist(false, Some(default_key.clone()))?.await?;
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
                let mut ctx = Context::new()?;
                ctx.set_auto_key_locate(LocateKey::LOCAL)?;
                let keys = ctx
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
                let mut ctx = Context::new()?;
                let data = ctx.new_data_mem(&melib_pgp::convert_attachment_to_rfc_spec(
                    a.into_raw().as_bytes(),
                ))?;
                let (sig_metadata, sig_bytes) = ctx.sign(sign_keys, data, false)?.await?;
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
            let mut ctx = Context::new()?;
            let data = ctx.new_data_mem(a.into_raw().as_bytes())?;

            let enc_attachment = {
                let mut a = Attachment::new(
                    ContentType::OctetStream {
                        name: None,
                        parameters: vec![],
                    },
                    Default::default(),
                    ctx.encrypt(encrypt_keys, data)?.await?,
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
            .unwrap();

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
        let err = smol::block_on((sign_filter(None, vec![pubkey.clone()]).unwrap())(
            body_attachment.clone(),
        ))
        .unwrap_err();
        assert!(err.summary.starts_with("Unusable secret key"), "{err}");

        // Add private key
        gpgme_ctx
            .import_key(gpgme_ctx.new_data_mem(PRIVKEY).unwrap())
            .unwrap();

        let body: AttachmentBuilder =
            smol::block_on((sign_filter(None, vec![pubkey]).unwrap())(body_attachment)).unwrap();
        draft.attachments.insert(0, body);
        let raw_mail = draft.finalise().unwrap();
        //eprintln!("{raw_mail}");
        let mail = melib::Mail::new(raw_mail.into_bytes(), None).expect("Could not parse mail");

        let signatures = smol::block_on(verify(mail.body())).unwrap();
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
            .unwrap();

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
                smol::block_on(decrypt(mail.body())).expect("Could not decrypt email");

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
                smol::block_on(decrypt(mail.body())).expect("Could not decrypt email");

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
            let signatures = smol::block_on(verify(decrypted.clone())).unwrap();
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

        let signatures = smol::block_on(verify(mail.body())).unwrap();
        assert_eq!(
            &signatures_into_error(signatures).unwrap().unwrap(),
            "[SAFETY WARNING: Cleartext signature!]good signature by \
             AEDC11FBCE2D746BF8BF7166CC2E963C99975163[?]"
        );

        _ = tempdir.close();
    }
}
