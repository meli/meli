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

use super::*;
use melib::email::pgp as melib_pgp;
use std::future::Future;
use std::io::Write;
use std::pin::Pin;
use std::process::{Command, Stdio};

pub fn verify_signature(a: &Attachment, context: &mut Context) -> Result<Vec<u8>> {
    let (bytes, sig) =
        melib_pgp::verify_signature(a).chain_err_summary(|| "Could not verify signature.")?;
    let bytes_file = create_temp_file(&bytes, None, None, true);
    let signature_file = create_temp_file(sig, None, None, true);
    let binary = context
        .settings
        .pgp
        .gpg_binary
        .as_ref()
        .map(String::as_str)
        .unwrap_or("gpg2");
    Ok(Command::new(binary)
        .args(&[
            "--output",
            "-",
            "--verify",
            signature_file.path.to_str().unwrap(),
            bytes_file.path.to_str().unwrap(),
        ])
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .and_then(|gpg| gpg.wait_with_output())
        .map(|gpg| gpg.stderr)
        .chain_err_summary(|| {
            format!(
                "Failed to launch {} to verify PGP signature",
                context
                    .settings
                    .pgp
                    .gpg_binary
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or("gpg2"),
            )
        })?)
}

/// Returns multipart/signed
pub fn sign(
    a: AttachmentBuilder,
    gpg_binary: Option<&str>,
    pgp_key: Option<&str>,
) -> Result<AttachmentBuilder> {
    let binary = gpg_binary.unwrap_or("gpg2");
    let mut command = Command::new(binary);
    command.args(&[
        "--digest-algo",
        "sha512",
        "--output",
        "-",
        "--detach-sig",
        "--armor",
    ]);
    if let Some(key) = pgp_key {
        command.args(&["--local-user", key]);
    }
    let a: Attachment = a.into();

    let sig_attachment = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .and_then(|mut gpg| {
            gpg.stdin
                .as_mut()
                .expect("Could not get gpg stdin")
                .write_all(&melib_pgp::convert_attachment_to_rfc_spec(
                    a.into_raw().as_bytes(),
                ))?;
            let gpg = gpg.wait_with_output()?;
            Ok(Attachment::new(
                ContentType::PGPSignature,
                Default::default(),
                gpg.stdout,
            ))
        })
        .chain_err_summary(|| format!("Failed to launch {} to verify PGP signature", binary))?;

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
}

pub async fn decrypt(
    raw: Vec<u8>,
    gpg_binary: Option<String>,
    decrypt_key: Option<String>,
) -> Result<(melib_pgp::DecryptionMetadata, Vec<u8>)> {
    let bin = gpg_binary.as_ref().map(|s| s.as_str()).unwrap_or("gpg2");
    let mut command = Command::new(bin);
    command.args(&["--digest-algo", "sha512", "--output", "-"]);
    if let Some(ref key) = decrypt_key {
        command.args(&["--local-user", key]);
    }

    let stdout = command
        .args(&["--decrypt"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .and_then(|mut gpg| {
            gpg.stdin
                .as_mut()
                .expect("Could not get gpg stdin")
                .write_all(&raw)?;
            let gpg = gpg.wait_with_output()?;
            Ok(gpg.stdout)
        })
        .chain_err_summary(|| format!("Failed to launch {} to verify PGP signature", bin))?;
    Ok((melib_pgp::DecryptionMetadata::default(), stdout))
}

pub async fn verify(a: Attachment, gpg_binary: Option<String>) -> Result<Vec<u8>> {
    let (bytes, sig) =
        melib_pgp::verify_signature(&a).chain_err_summary(|| "Could not verify signature.")?;
    let bytes_file = create_temp_file(&bytes, None, None, true);
    let signature_file = create_temp_file(sig, None, None, true);
    Ok(
        Command::new(gpg_binary.as_ref().map(String::as_str).unwrap_or("gpg2"))
            .args(&[
                "--output",
                "-",
                "--verify",
                signature_file.path.to_str().unwrap(),
                bytes_file.path.to_str().unwrap(),
            ])
            .stdin(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .and_then(|gpg| gpg.wait_with_output())
            .map(|gpg| gpg.stderr)
            .chain_err_summary(|| {
                format!(
                    "Failed to launch {} to verify PGP signature",
                    gpg_binary.as_ref().map(String::as_str).unwrap_or("gpg2"),
                )
            })?,
    )
}

pub fn sign_filter(
    gpg_binary: Option<String>,
    pgp_key: Option<String>,
) -> Result<
    impl FnOnce(AttachmentBuilder) -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>> + Send>>
        + Send,
> {
    let binary = gpg_binary.unwrap_or("gpg2".to_string());
    let mut command = Command::new(&binary);
    command.args(&[
        "--digest-algo",
        "sha512",
        "--output",
        "-",
        "--detach-sig",
        "--armor",
    ]);
    if let Some(key) = pgp_key.as_ref() {
        command.args(&["--local-user", key]);
    }
    Ok(
        move |a: AttachmentBuilder| -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>>+Send>> {
            Box::pin(async move {
                let a: Attachment = a.into();

                let sig_attachment = command
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::null())
                    .spawn()
                    .and_then(|mut gpg| {
                        gpg.stdin
                            .as_mut()
                            .expect("Could not get gpg stdin")
                            .write_all(&melib_pgp::convert_attachment_to_rfc_spec(
                                a.into_raw().as_bytes(),
                            ))?;
                        let gpg = gpg.wait_with_output()?;
                        Ok(Attachment::new(
                            ContentType::PGPSignature,
                            Default::default(),
                            gpg.stdout,
                        ))
                    })
                    .chain_err_summary(|| {
                        format!("Failed to launch {} to verify PGP signature", binary)
                    })?;

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
    gpg_binary: Option<String>,
    my_public_key: Option<String>,
    recipients: Vec<String>,
) -> Result<
    impl FnOnce(AttachmentBuilder) -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>> + Send>>
        + Send,
> {
    let binary = gpg_binary.unwrap_or("gpg2".to_string());
    let mut command = Command::new(&binary);
    command.args(&[
        "--batch",
        "--no-tty",
        "--encrypt",
        "--armor",
        "--output",
        "-",
    ]);
    if let Some(key) = my_public_key.as_ref() {
        command.args(&["--recipient", key]);
    } else {
        command.arg("--default-recipient-self");
    }
    for r in &recipients {
        command.args(&["--recipient", r.as_str()]);
    }
    Ok(
        move |a: AttachmentBuilder| -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>>+Send>> {
            Box::pin(async move {
                let a: Attachment = a.into();

                let sig_attachment = command
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::null())
                    .spawn()
                    .and_then(|mut gpg| {
                        gpg.stdin
                            .as_mut()
                            .expect("Could not get gpg stdin")
                            .write_all(&melib_pgp::convert_attachment_to_rfc_spec(
                                a.into_raw().as_bytes(),
                            ))?;
                        let gpg = gpg.wait_with_output()?;
                        let mut a = Attachment::new(
                            ContentType::OctetStream { name: None },
                            Default::default(),
                            gpg.stdout,
                        );
                        a.content_disposition = ContentDisposition::from(r#"attachment; filename="msg.asc""#.as_bytes());
                        Ok(a)
                    })
                    .chain_err_summary(|| {
                        format!("Failed to launch {} to verify PGP signature", binary)
                    })?;

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
