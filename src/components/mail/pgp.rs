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

use melib::email::{
    attachment_types::{ContentDisposition, ContentType, MultipartType},
    pgp as melib_pgp, Attachment, AttachmentBuilder,
};
use melib::error::*;
use melib::gpgme::*;
use std::future::Future;
use std::pin::Pin;

pub async fn decrypt(raw: Vec<u8>) -> Result<(melib_pgp::DecryptionMetadata, Vec<u8>)> {
    let mut ctx = Context::new()?;
    let cipher = ctx.new_data_mem(&raw)?;
    ctx.decrypt(cipher)?.await
}

pub async fn verify(a: Attachment) -> Result<()> {
    let (data, sig) =
        melib_pgp::verify_signature(&a).chain_err_summary(|| "Could not verify signature.")?;
    let mut ctx = Context::new()?;
    let sig = ctx.new_data_mem(&sig)?;
    let data = ctx.new_data_mem(&data)?;
    ctx.verify(sig, data)?.await
}

pub fn sign_filter(
    sign_keys: Vec<Key>,
) -> Result<
    impl FnOnce(AttachmentBuilder) -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>> + Send>>
        + Send,
> {
    Ok(
        move |a: AttachmentBuilder| -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>>+Send>> {
            Box::pin(async move {
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
                    },
                    Default::default(),
                    Vec::new(),
                )
                .into())
            })
        },
    )
}

pub fn encrypt_filter(
    sign_keys: Option<Vec<Key>>,
    encrypt_keys: Vec<Key>,
) -> Result<
    impl FnOnce(AttachmentBuilder) -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>> + Send>>
        + Send,
> {
    Ok(
        move |a: AttachmentBuilder| -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>>+Send>> {
            Box::pin(async move {
                let a: Attachment = a.into();
                debug!("main attachment is {:?}", &a);
                let mut ctx = Context::new()?;
                let data = ctx.new_data_mem(
                                a.into_raw().as_bytes()
                )?;

                let sig_attachment = {
                    let mut a = Attachment::new(
                            ContentType::OctetStream { name: None },
                            Default::default(),
                            ctx.encrypt(sign_keys, encrypt_keys, data)?.await?,
                        );
                        a.content_disposition = ContentDisposition::from(r#"attachment; filename="msg.asc""#.as_bytes());
                        a
                };
                let mut a: AttachmentBuilder = AttachmentBuilder::new("Version: 1".as_bytes());
                a.set_content_type_from_bytes("application/pgp-encrypted".as_bytes());
                a.set_content_disposition(ContentDisposition::from("attachment".as_bytes()));
                let parts = vec![a, sig_attachment.into()];
                let boundary = ContentType::make_boundary(&parts);
                Ok(Attachment::new(
                    ContentType::Multipart {
                        boundary: boundary.into_bytes(),
                        kind: MultipartType::Encrypted,
                        parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                    },
                    Default::default(),
                    Vec::new(),
                )
                .into())
            })
        },
    )
}
