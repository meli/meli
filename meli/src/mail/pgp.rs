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
    sync::{Arc, Mutex},
};

use melib::{
    email::{
        attachment_types::{ContentDisposition, ContentType, MultipartType},
        pgp as melib_pgp, Attachment, AttachmentBuilder,
    },
    error::*,
    gpgme::*,
    parser::BytesExt,
};

use super::AttachmentBoxFuture;

pub async fn decrypt(raw: Vec<u8>) -> Result<(melib_pgp::DecryptionMetadata, Vec<u8>)> {
    let mut ctx = Context::new()?;
    let cipher = ctx.new_data_mem(&raw)?;
    ctx.decrypt(cipher)?.await
}

pub fn verify(a: Attachment) -> impl Future<Output = Result<()>> {
    thread_local! {
        static CACHE: Arc<Mutex<BTreeMap<u64, Result<()>>>> = Arc::new(Mutex::new(BTreeMap::new()));
    }

    let hash_mtx = CACHE.with(|cache| cache.clone());
    verify_inner(a, hash_mtx)
}

async fn verify_inner(a: Attachment, cache: Arc<Mutex<BTreeMap<u64, Result<()>>>>) -> Result<()> {
    let mut hasher = DefaultHasher::new();
    a.hash(&mut hasher);
    let attachment_hash: u64 = hasher.finish();

    {
        let lck = cache.lock().unwrap();
        let in_cache: bool = lck.contains_key(&attachment_hash);
        if in_cache {
            return lck[&attachment_hash].clone();
        }
    }

    let (data, sig) =
        melib_pgp::verify_signature(&a).chain_err_summary(|| "Could not verify signature.")?;
    let mut ctx = Context::new()?;
    let sig = ctx.new_data_mem(sig.body().trim())?;
    let data = ctx.new_data_mem(&data)?;

    let result = ctx.verify(sig, data)?.await;
    {
        let mut lck = cache.lock().unwrap();
        lck.insert(attachment_hash, result.clone());
    }
    result
}

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
            let data = ctx.new_data_mem(&melib_pgp::convert_attachment_to_rfc_spec(
                a.into_raw().as_bytes(),
            ))?;
            let sig_attachment = Attachment::new(
                ContentType::PGPSignature,
                Default::default(),
                ctx.sign(sign_keys, data)?.await?,
            );
            let a: AttachmentBuilder = a.into();
            let parts = vec![a, sig_attachment.into()];
            let boundary = ContentType::make_boundary(&parts);
            Ok(Attachment::new(
                ContentType::Multipart {
                    boundary: boundary.into_bytes(),
                    kind: MultipartType::Signed,
                    parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                    parameters: vec![],
                },
                Default::default(),
                vec![],
            )
            .into())
        })
    })
}

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
                let sig_attachment = Attachment::new(
                    ContentType::PGPSignature,
                    Default::default(),
                    ctx.sign(sign_keys, data)?.await?,
                );
                let a: AttachmentBuilder = a.into();
                let parts = vec![a, sig_attachment.into()];
                let boundary = ContentType::make_boundary(&parts);
                Attachment::new(
                    ContentType::Multipart {
                        boundary: boundary.into_bytes(),
                        kind: MultipartType::Signed,
                        parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                        parameters: vec![],
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
                    parameters: vec![],
                },
                Default::default(),
                vec![],
            )
            .into())
        })
    })
}
