/*
 * meli
 *
 * Copyright 2023 Manos Pitsidianakis
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

//! Pre-submission hooks for draft validation and/or transformations.
pub use std::borrow::Cow;

use super::*;

pub enum HookFn {
    /// Stateful hook.
    Closure(Box<dyn FnMut(&mut Context, &mut Draft) -> Result<()> + Send + Sync>),

    /// Static hook.
    Ptr(fn(&mut Context, &mut Draft) -> Result<()>),
}

impl std::fmt::Debug for HookFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct(stringify!(HookFn))
            .field(
                "kind",
                &match self {
                    Self::Closure(_) => "closure",
                    Self::Ptr(_) => "function ptr",
                },
            )
            .finish()
    }
}

impl std::ops::Deref for HookFn {
    type Target = dyn FnMut(&mut Context, &mut Draft) -> Result<()> + Send + Sync;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Ptr(ref v) => v,
            Self::Closure(ref v) => v,
        }
    }
}

impl std::ops::DerefMut for HookFn {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Ptr(ref mut v) => v,
            Self::Closure(ref mut v) => v,
        }
    }
}

#[derive(Debug)]
/// Pre-submission hook for draft validation and/or transformations.
pub struct Hook {
    /// Hook name for enabling/disabling it from configuration.
    ///
    /// See [`ComposingSettings::disabled_compose_hooks`].
    name: Cow<'static, str>,
    hook_fn: HookFn,
}

impl Hook {
    /// Hook name for enabling/disabling it from configuration.
    ///
    /// See [`ComposingSettings::disabled_compose_hooks`].
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn new_shell_command(name: Cow<'static, str>, command: String) -> Self {
        let name_ = name.clone();
        Self {
            name,
            hook_fn: HookFn::Closure(Box::new(move |_, draft| -> Result<()> {
                use std::thread;

                let mut child = Command::new("sh")
                    .arg("-c")
                    .arg(&command)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .spawn()
                    .map_err(|err| -> Error {
                        format!(
                            "could not execute `{command}`. Check if its binary is in PATH or if \
                             the command is valid. Original error: {err}"
                        )
                        .into()
                    })?;
                let mut stdin = child
                    .stdin
                    .take()
                    .ok_or_else(|| Error::new("failed to get stdin"))?;

                thread::scope(|s| {
                    s.spawn(move || {
                        stdin
                            .write_all(draft.body.as_bytes())
                            .expect("failed to write to stdin");
                    });
                });
                let output = child.wait_with_output().map_err(|err| -> Error {
                    format!("failed to wait on hook child {name_}: {err}").into()
                })?;
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                if !output.status.success() || !stdout.is_empty() || !stderr.is_empty() {
                    return Err(format!(
                        "{name_}\n  exit code: {:?}\n  stdout:\n{}\n  stderr:\n{}",
                        output.status.code(),
                        stdout,
                        stderr,
                    )
                    .into());
                }

                Ok(())
            })),
        }
    }
}

impl std::ops::Deref for Hook {
    type Target = dyn FnMut(&mut Context, &mut Draft) -> Result<()> + Send + Sync;

    fn deref(&self) -> &Self::Target {
        self.hook_fn.deref()
    }
}

impl std::ops::DerefMut for Hook {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.hook_fn.deref_mut()
    }
}

fn past_date_warn(_ctx: &mut Context, draft: &mut Draft) -> Result<()> {
    use melib::datetime::*;
    if let Some(v) = draft
        .headers
        .get("Date")
        .map(rfc822_to_timestamp)
        .and_then(Result::ok)
    {
        let now: UnixTimestamp = now();
        let diff = now.abs_diff(v);
        if diff >= 60 * 60 {
            return Err(format!(
                "Value of Date header is {} minutes in the {}.",
                diff / 60,
                if now > v { "past" } else { "future" }
            )
            .into());
        }
    }
    Ok(())
}

/// Warn if [`melib::Draft`] Date is far in the past/future.
pub const PASTDATEWARN: Hook = Hook {
    name: Cow::Borrowed("past-date-warn"),
    hook_fn: HookFn::Ptr(past_date_warn),
};

fn important_header_warn(_ctx: &mut Context, draft: &mut Draft) -> Result<()> {
    for hdr in ["From", "To"] {
        match draft.headers.get(hdr).map(melib::Address::list_try_from) {
            Some(Ok(_)) => {}
            Some(Err(err)) => return Err(format!("{hdr} header value is invalid ({err}).").into()),
            None => return Err(format!("{hdr} header is missing and should be present.").into()),
        }
    }

    {
        match draft
            .headers
            .get("Date")
            .map(melib::datetime::rfc822_to_timestamp)
        {
            Some(Err(err)) => return Err(format!("Date header value is invalid ({err}).").into()),
            Some(Ok(0)) => return Err("Date header value is invalid.".into()),
            _ => {}
        }
    }

    for hdr in ["Cc", "Bcc"] {
        if let Some(Err(err)) = draft
            .headers
            .get(hdr)
            .filter(|v| !v.trim().is_empty())
            .map(melib::Address::list_try_from)
        {
            return Err(format!("{hdr} header value is invalid ({err}).").into());
        }
    }
    Ok(())
}

/// Warn if important [`melib::Draft`] header is missing or invalid.
pub const HEADERWARN: Hook = Hook {
    name: Cow::Borrowed("important-header-warn"),
    hook_fn: HookFn::Ptr(important_header_warn),
};

fn missing_attachment_warn(_ctx: &mut Context, draft: &mut Draft) -> Result<()> {
    if draft
        .headers
        .get("Subject")
        .map(|s| s.to_lowercase().contains("attach"))
        .unwrap_or(false)
        && draft.attachments.is_empty()
    {
        return Err("Subject mentions attachments but attachments are empty.".into());
    }

    if draft.body.to_lowercase().contains("attach") && draft.attachments.is_empty() {
        return Err("Draft body mentions attachments but attachments are empty.".into());
    }

    Ok(())
}

/// Warn if Subject and/or draft body mentions attachments but they are missing.
pub const MISSINGATTACHMENTWARN: Hook = Hook {
    name: Cow::Borrowed("missing-attachment-warn"),
    hook_fn: HookFn::Ptr(missing_attachment_warn),
};

fn empty_draft_warn(_ctx: &mut Context, draft: &mut Draft) -> Result<()> {
    if draft
        .headers
        .get("Subject")
        .filter(|v| !v.trim().is_empty())
        .is_none()
        && draft.body.trim().is_empty()
    {
        return Err("Both Subject and body are empty.".into());
    }

    Ok(())
}

/// Warn if draft has no subject and no body.
pub const EMPTYDRAFTWARN: Hook = Hook {
    name: Cow::Borrowed("empty-draft-warn"),
    hook_fn: HookFn::Ptr(empty_draft_warn),
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_draft_hook_datewarn() {
        let tempdir = tempfile::tempdir().unwrap();
        let mut ctx = Context::new_mock(&tempdir);
        let mut draft = Draft::default();
        draft
            .set_body("αδφαφσαφασ".to_string())
            .set_header(HeaderName::SUBJECT, "test_update()".into())
            .set_header(HeaderName::DATE, "Sun, 16 Jun 2013 17:56:45 +0200".into());
        println!("Check that past Date header value produces a warning…");
        #[allow(const_item_mutation)]
        let err_msg = PASTDATEWARN(&mut ctx, &mut draft).unwrap_err().to_string();
        assert!(
            err_msg.starts_with("Value of Date header is "),
            "PASTDATEWARN should complain about Date value being in the past: {}",
            err_msg
        );
        assert!(
            err_msg.ends_with(" minutes in the past."),
            "PASTDATEWARN should complain about Date value being in the past: {}",
            err_msg
        );
    }

    #[test]
    fn test_draft_hook_headerwarn() {
        let tempdir = tempfile::tempdir().unwrap();
        let mut ctx = Context::new_mock(&tempdir);
        let mut draft = Draft::default();
        draft
            .set_body("αδφαφσαφασ".to_string())
            .set_header(HeaderName::SUBJECT, "test_update()".into())
            .set_header(HeaderName::DATE, "Sun sds16 Jun 2013 17:56:45 +0200".into());
        let mut hook = HEADERWARN;

        println!("Check for missing/empty From header value…");
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg,
            "From header value is invalid (Parsing error. In input: \"...\",\nError: Alternative, \
             Many1, Alternative, atom(): starts with whitespace or empty).",
            "HEADERWARN should complain about From value being empty: {}",
            err_msg
        );
        draft.set_header(HeaderName::FROM, "user <user@example.com>".into());

        println!("Check for missing/empty To header value…");
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg,
            "To header value is invalid (Parsing error. In input: \"...\",\nError: Alternative, \
             Many1, Alternative, atom(): starts with whitespace or empty).",
            "HEADERWARN should complain about To value being empty: {}",
            err_msg
        );
        draft.set_header(HeaderName::TO, "other user <user@example.com>".into());

        println!("Check for invalid Date header value…");
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg, "Date header value is invalid.",
            "HEADERWARN should complain about Date value being invalid: {}",
            err_msg
        );

        println!("Check that valid header values produces no errors…");
        draft = Draft::default();
        draft
            .set_body("αδφαφσαφασ".to_string())
            .set_header(HeaderName::FROM, "user <user@example.com>".into())
            .set_header(HeaderName::TO, "other user <user@example.com>".into())
            .set_header(HeaderName::SUBJECT, "test_update()".into());
        hook(&mut ctx, &mut draft).unwrap();
        draft.set_header(HeaderName::FROM, "user user@example.com>".into());

        println!("Check for invalid From header value…");
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg,
            "From header value is invalid (Parsing error. In input: \"user \
             user@example.com>...\",\nError: Alternative, Tag).",
            "HEADERWARN should complain about From value being invalid: {}",
            err_msg
        );
    }

    #[test]
    fn test_draft_hook_missingattachmentwarn() {
        let tempdir = tempfile::tempdir().unwrap();
        let mut ctx = Context::new_mock(&tempdir);
        let mut draft = Draft::default();
        draft
            .set_body("αδφαφσαφασ".to_string())
            .set_header(HeaderName::SUBJECT, "Attachments included".into())
            .set_header(HeaderName::DATE, "Sun, 16 Jun 2013 17:56:45 +0200".into());

        let mut hook = MISSINGATTACHMENTWARN;

        println!(
            "Check that mentioning attachments in Subject produces a warning if draft has no \
             attachments…"
        );
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg, "Subject mentions attachments but attachments are empty.",
            "MISSINGATTACHMENTWARN should complain about missing attachments: {}",
            err_msg
        );

        draft
            .set_header(HeaderName::SUBJECT, "Hello.".into())
            .set_body("Attachments included".to_string());
        println!(
            "Check that mentioning attachments in body produces a warning if draft has no \
             attachments…"
        );
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg, "Draft body mentions attachments but attachments are empty.",
            "MISSINGATTACHMENTWARN should complain about missing attachments: {}",
            err_msg
        );

        println!(
            "Check that mentioning attachments produces no warnings if draft has attachments…"
        );
        draft.set_header(HeaderName::SUBJECT, "Attachments included".into());
        let mut attachment = AttachmentBuilder::new(b"");
        attachment
            .set_raw(b"foobar".to_vec())
            .set_content_type(ContentType::Other {
                name: Some("info.txt".to_string()),
                tag: b"text/plain".to_vec(),
                parameters: vec![],
            })
            .set_content_transfer_encoding(ContentTransferEncoding::Base64);
        draft.attachments_mut().push(attachment);

        hook(&mut ctx, &mut draft).unwrap();
    }

    #[test]
    fn test_draft_hook_emptydraftwarn() {
        let tempdir = tempfile::tempdir().unwrap();
        let mut ctx = Context::new_mock(&tempdir);
        let mut draft = Draft::default();
        draft.set_header(HeaderName::DATE, "Sun, 16 Jun 2013 17:56:45 +0200".into());

        let mut hook = EMPTYDRAFTWARN;

        println!("Check that empty draft produces a warning…");
        let err_msg = hook(&mut ctx, &mut draft).unwrap_err().to_string();
        assert_eq!(
            err_msg, "Both Subject and body are empty.",
            "EMPTYDRAFTWARN should complain about empty draft: {}",
            err_msg
        );

        println!("Check that non-empty draft produces no warning…");
        draft.set_header(HeaderName::SUBJECT, "Ping".into());
        hook(&mut ctx, &mut draft).unwrap();
    }
}
