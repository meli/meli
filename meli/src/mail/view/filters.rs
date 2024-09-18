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

use std::{
    borrow::Cow,
    io::Write,
    process::{Command, Stdio},
    sync::Arc,
};

type ProcessEventFn = fn(&mut ViewFilter, &mut UIEvent, &mut Context) -> bool;

use melib::{
    attachment_types::{ContentType, MultipartType, Text},
    error::*,
    log,
    parser::BytesExt,
    text::Truncate,
    utils::xdg::query_default_app,
    Attachment, AttachmentBuilder, Result,
};
use smallvec::SmallVec;

use crate::{
    components::*,
    desktop_exec_to_command,
    jobs::{IsAsync, JobId, JoinHandle},
    mail::view::ViewSettings,
    terminal::{Area, CellBuffer},
    try_recv_timeout, Context, ErrorKind, File, StatusEvent, UIEvent,
};

type FilterResult = std::result::Result<(Attachment, Vec<u8>), (Error, Vec<u8>)>;
type OnSuccessNoticeCb = Arc<dyn (Fn() -> Cow<'static, str>) + Send + Sync>;

pub enum ViewFilterContent {
    Running {
        job_id: JobId,
        on_success_notice_cb: OnSuccessNoticeCb,
        job_handle: JoinHandle<FilterResult>,
        view_settings: ViewSettings,
    },
    Error {
        inner: Error,
    },
    Filtered {
        inner: String,
    },
    InlineAttachments {
        parts: Vec<ViewFilter>,
    },
}

impl std::fmt::Debug for ViewFilterContent {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ViewFilterContent::*;
        match self {
            Running {
                ref job_id,
                on_success_notice_cb: _,
                job_handle: _,
                view_settings: _,
            } => fmt
                .debug_struct(stringify!(ViewFilterContent::Running))
                .field("job_id", &job_id)
                .finish(),
            Error { ref inner } => fmt
                .debug_struct(stringify!(ViewFilterContent::Error))
                .field("error", inner)
                .finish(),
            Filtered { ref inner } => fmt
                .debug_struct(stringify!(ViewFilterContent::Filtered))
                .field("body_text", &inner.trim_at_boundary(18))
                .field("body_text_len", &inner.len())
                .finish(),
            InlineAttachments { ref parts } => fmt
                .debug_struct(stringify!(ViewFilterContent::InlineAttachments))
                .field("parts_no", &parts.len())
                .field("parts", &parts)
                .finish(),
        }
    }
}

pub struct ViewFilter {
    pub filter_invocation: String,
    pub content_type: ContentType,
    pub notice: Option<Cow<'static, str>>,
    pub body_text: ViewFilterContent,
    pub unfiltered: Vec<u8>,
    pub event_handler: Option<ProcessEventFn>,
    pub id: ComponentId,
}

impl std::fmt::Debug for ViewFilter {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(ViewFilter))
            .field("filter_invocation", &self.filter_invocation)
            .field("content_type", &self.content_type)
            .field("notice", &self.notice)
            .field("body_text", &self.body_text)
            .field("event_handler", &self.event_handler.is_some())
            .field("id", &self.id)
            .finish()
    }
}

impl std::fmt::Display for ViewFilter {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.filter_invocation.trim_at_boundary(5))
    }
}

impl ViewFilter {
    pub fn new_html(
        body: &Attachment,
        view_settings: &ViewSettings,
        context: &Context,
    ) -> Result<Self> {
        fn run(cmd: &str, args: &[&str], bytes: &[u8]) -> Result<String> {
            let mut html_filter = Command::new(cmd)
                .args(args)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()?;
            html_filter
                .stdin
                .as_mut()
                .ok_or("Failed to write to html filter stdin")?
                .write_all(bytes)
                .chain_err_summary(|| "Failed to write to html filter stdin")?;
            Ok(String::from_utf8_lossy(
                &html_filter
                    .wait_with_output()
                    .chain_err_summary(|| "Could not wait for process output")?
                    .stdout,
            )
            .into())
        }

        let mut att = body;
        let mut stack = vec![body];
        while let Some(a) = stack.pop() {
            match a.content_type {
                ContentType::Text {
                    kind: Text::Html, ..
                } => {
                    att = a;
                    break;
                }
                ContentType::Text { .. }
                | ContentType::PGPSignature
                | ContentType::CMSSignature => {
                    continue;
                }
                ContentType::Multipart {
                    kind: MultipartType::Related,
                    ref parts,
                    ref parameters,
                    ..
                } => {
                    if let Some(main_attachment) = parameters
                        .iter()
                        .find_map(|(k, v)| if k == b"type" { Some(v) } else { None })
                        .and_then(|t| parts.iter().find(|a| a.content_type == t.as_slice()))
                    {
                        stack.push(main_attachment);
                    } else {
                        for a in parts {
                            if let ContentType::Text {
                                kind: Text::Html, ..
                            } = a.content_type
                            {
                                att = a;
                                break;
                            }
                        }
                        stack.extend(parts);
                    }
                }
                ContentType::Multipart {
                    kind: MultipartType::Alternative,
                    ref parts,
                    ..
                } => {
                    for a in parts {
                        if let ContentType::Text {
                            kind: Text::Html, ..
                        } = a.content_type
                        {
                            att = a;
                            break;
                        }
                    }
                    stack.extend(parts);
                }
                ContentType::Multipart {
                    kind: _, ref parts, ..
                } => {
                    for a in parts {
                        if let ContentType::Text {
                            kind: Text::Html, ..
                        } = a.content_type
                        {
                            att = a;
                            break;
                        }
                    }
                    stack.extend(parts);
                }
                _ => {}
            }
        }
        let settings = &context.settings;
        let (filter_invocation, cmd, args): (
            Cow<'static, str>,
            &'static str,
            SmallVec<[Cow<'static, str>; 8]>,
        ) = if let Some(filter_invocation) = settings.pager.html_filter.as_ref() {
            (
                filter_invocation.to_string().into(),
                "sh",
                smallvec::smallvec!["-c".into(), filter_invocation.to_string().into()],
            )
        } else {
            (
                "w3m -I utf-8 -T text/html".into(),
                "w3m",
                smallvec::smallvec!["-I".into(), "utf-8".into(), "-T".into(), "text/html".into()],
            )
        };
        let bytes: Vec<u8> = att.decode(Default::default());

        let filter_invocation2 = filter_invocation.to_string();
        let bytes2 = bytes.clone();
        let job = async move {
            let filter_invocation = filter_invocation2;
            let bytes = bytes2;
            let borrowed_args = args
                .iter()
                .map(|a| a.as_ref())
                .collect::<SmallVec<[&str; 8]>>();
            match run(cmd, &borrowed_args, &bytes) {
                Err(err) => Err((
                    Error::new(format!(
                        "Failed to start html filter process `{}`",
                        filter_invocation,
                    ))
                    .set_source(Some(Arc::new(err)))
                    .set_kind(ErrorKind::External),
                    bytes,
                )),
                Ok(body_text) => {
                    let mut att = AttachmentBuilder::default();
                    att.set_raw(body_text.into_bytes()).set_body_to_raw();
                    Ok((att.build(), bytes))
                }
            }
        };
        let filter_invocation2 = filter_invocation.to_string();
        let open_html_shortcut = settings.shortcuts.envelope_view.open_html.clone();
        let on_success_notice_cb = Arc::new(move || {
            format!(
                "Text piped through `{}` Press `{}` to open in web browser.",
                filter_invocation2, open_html_shortcut
            )
            .into()
        });
        let mut job_handle = context.main_loop_handler.job_executor.spawn(
            filter_invocation.to_string().into(),
            job,
            IsAsync::Blocking,
        );
        let mut retval = Self {
            filter_invocation: filter_invocation.to_string(),
            content_type: att.content_type.clone(),
            notice: None,
            unfiltered: bytes,
            body_text: ViewFilterContent::Filtered {
                inner: String::new(),
            },
            event_handler: Some(Self::job_process_event),
            id: ComponentId::default(),
        };
        if let Ok(Some(job_result)) = try_recv_timeout!(&mut job_handle.chan) {
            retval.body_text = ViewFilterContent::Running {
                job_id: job_handle.job_id,
                on_success_notice_cb: on_success_notice_cb.clone(),
                job_handle,
                view_settings: view_settings.clone(),
            };
            retval.event_handler = Some(Self::html_process_event);
            Self::process_job_result(
                &mut retval,
                Ok(Some(job_result)),
                on_success_notice_cb,
                view_settings,
                context,
            );
            return Ok(retval);
        }
        Ok(Self {
            body_text: ViewFilterContent::Running {
                job_id: job_handle.job_id,
                on_success_notice_cb,
                job_handle,
                view_settings: view_settings.clone(),
            },
            ..retval
        })
    }

    pub fn new_attachment(
        att: &Attachment,
        view_settings: &ViewSettings,
        context: &Context,
    ) -> Result<Self> {
        if matches!(
            att.content_type,
            ContentType::Other { .. } | ContentType::OctetStream { .. }
        ) {
            return Err(Error::new(format!(
                "Cannot view {} attachment as text.",
                att.content_type,
            ))
            .set_kind(ErrorKind::ValueError));
        }
        if let ContentType::Multipart {
            kind: MultipartType::Alternative,
            ref parts,
            ..
        } = att.content_type
        {
            if let Some(Ok(v)) = parts
                .iter()
                .find(|p| p.is_text() && !p.body().trim().is_empty())
                .map(|p| Self::new_attachment(p, view_settings, context))
            {
                return Ok(v);
            }
        } else if let ContentType::Multipart {
            kind: MultipartType::Related | MultipartType::Mixed,
            ref parts,
            ..
        } = att.content_type
        {
            return Ok(Self {
                filter_invocation: String::new(),
                content_type: att.content_type.clone(),
                notice: None,
                body_text: ViewFilterContent::InlineAttachments {
                    parts: parts
                        .iter()
                        .filter_map(|p| Self::new_attachment(p, view_settings, context).ok())
                        .collect::<Vec<Self>>(),
                },
                unfiltered: att.decode(Default::default()),
                event_handler: None,
                id: ComponentId::default(),
            });
        }
        if att.is_html() {
            return Self::new_html(att, view_settings, context);
        }
        if matches!(
            att.content_type,
            ContentType::Multipart {
                kind: MultipartType::Digest,
                ..
            }
        ) {
            return Ok(Self {
                filter_invocation: String::new(),
                content_type: att.content_type.clone(),
                notice: None,
                body_text: ViewFilterContent::Filtered {
                    inner: String::new(),
                },
                unfiltered: vec![],
                event_handler: None,
                id: ComponentId::default(),
            });
        } else if let ContentType::Multipart {
            kind: MultipartType::Signed,
            ref parts,
            ref boundary,
            ref parameters,
        } = att.content_type
        {
            if !view_settings.auto_verify_signatures.is_true() {
                let att = Attachment {
                    content_type: ContentType::Multipart {
                        kind: MultipartType::Mixed,
                        parts: parts.clone(),
                        parameters: parameters.clone(),
                        boundary: boundary.clone(),
                    },
                    ..att.clone()
                };
                return Ok(Self {
                    notice: Some("Unverified signature.".into()),
                    ..Self::new_attachment(&att, view_settings, context)?
                });
            }
            #[cfg(not(feature = "gpgme"))]
            {
                let content = att.raw();
                let bytes = content.trim().to_vec();
                return Ok(Self {
                    filter_invocation: String::new(),
                    content_type: att.content_type.clone(),
                    notice: Some(
                        "Cannot verify signature: meli must be compiled with libgpgme support."
                            .into(),
                    ),
                    body_text: ViewFilterContent::InlineAttachments {
                        parts: parts
                            .iter()
                            .filter_map(|p| Self::new_attachment(p, view_settings, context).ok())
                            .collect::<Vec<Self>>(),
                    },
                    unfiltered: bytes,
                    event_handler: None,
                    id: ComponentId::default(),
                });
            }
            #[cfg(feature = "gpgme")]
            {
                for a in parts {
                    if a.content_type == "application/pgp-signature" {
                        let content = att.raw();
                        let bytes = content.trim().to_vec();
                        let verify_fut = {
                            let a = Attachment {
                                content_type: ContentType::Multipart {
                                    kind: MultipartType::Mixed,
                                    parts: parts.clone(),
                                    parameters: parameters.clone(),
                                    boundary: boundary.clone(),
                                },
                                ..att.clone()
                            };
                            let att = att.clone();
                            async move {
                                crate::mail::pgp::verify(att)
                                    .await
                                    .map_err(|err| (err, bytes.clone()))
                                    .map(|_| (a, bytes))
                            }
                        };
                        let mut job_handle = context.main_loop_handler.job_executor.spawn(
                            "gpg::verify".into(),
                            verify_fut,
                            IsAsync::Blocking,
                        );
                        let on_success_notice_cb = Arc::new(|| "Verified signature.".into());
                        let mut retval = Self {
                            filter_invocation: "gpg::verify".into(),
                            content_type: att.content_type.clone(),
                            notice: None,
                            body_text: ViewFilterContent::Filtered {
                                inner: String::new(),
                            },
                            unfiltered: att.raw().to_vec(),
                            event_handler: Some(Self::job_process_event),
                            id: ComponentId::default(),
                        };
                        if let Ok(Some(job_result)) = try_recv_timeout!(&mut job_handle.chan) {
                            retval.body_text = ViewFilterContent::Running {
                                job_id: job_handle.job_id,
                                on_success_notice_cb: on_success_notice_cb.clone(),
                                job_handle,
                                view_settings: view_settings.clone(),
                            };
                            retval.event_handler = None;
                            Self::process_job_result(
                                &mut retval,
                                Ok(Some(job_result)),
                                on_success_notice_cb,
                                view_settings,
                                context,
                            );
                            return Ok(retval);
                        }
                        return Ok(Self {
                            body_text: ViewFilterContent::Running {
                                job_id: job_handle.job_id,
                                on_success_notice_cb,
                                job_handle,
                                view_settings: view_settings.clone(),
                            },
                            ..retval
                        });
                    }
                }
            }
        } else if let ContentType::Multipart {
            kind: MultipartType::Encrypted,
            ref parts,
            ..
        } = att.content_type
        {
            #[cfg(not(feature = "gpgme"))]
            {
                let msg = "Cannot decrypt: meli must be compiled with libgpgme support.";
                if let Some(Ok(mut res)) =
                    parts.iter().find_map(|part| {
                        match Self::new_attachment(part, view_settings, context) {
                            v @ Ok(_) => Some(v),
                            Err(_) => None,
                        }
                    })
                {
                    match res.notice {
                        Some(ref mut notice) => {
                            let notice = std::mem::take(notice);
                            let mut notice = notice.into_owned();
                            notice.push_str("\n");
                            notice.push_str(msg);

                            res.notice = Some(notice.into());
                        }
                        None => {
                            res.notice = Some(msg.into());
                        }
                    }
                    return Ok(res);
                }
            }
            #[cfg(feature = "gpgme")]
            {
                for a in parts {
                    if a.content_type == "application/octet-stream" {
                        let content = a.raw();
                        let bytes = content.trim().to_vec();
                        let decrypt_fut = async {
                            let (_metadata, bytes) = crate::mail::pgp::decrypt(
                                melib::email::pgp::convert_attachment_to_rfc_spec(&bytes),
                            )
                            .await
                            .map_err(|err| (err, bytes))?;
                            Ok((AttachmentBuilder::new(&bytes).build(), bytes))
                        };
                        let mut job_handle = context.main_loop_handler.job_executor.spawn(
                            "gpg::decrypt".into(),
                            decrypt_fut,
                            IsAsync::Blocking,
                        );
                        let on_success_notice_cb = Arc::new(|| "Decrypted content.".into());
                        let mut retval = Self {
                            filter_invocation: "gpg::decrypt".into(),
                            content_type: att.content_type.clone(),
                            notice: None,
                            body_text: ViewFilterContent::Filtered {
                                inner: String::new(),
                            },
                            unfiltered: a.raw().to_vec(),
                            event_handler: Some(Self::job_process_event),
                            id: ComponentId::default(),
                        };
                        if let Ok(Some(job_result)) = try_recv_timeout!(&mut job_handle.chan) {
                            retval.body_text = ViewFilterContent::Running {
                                job_id: job_handle.job_id,
                                on_success_notice_cb: on_success_notice_cb.clone(),
                                job_handle,
                                view_settings: view_settings.clone(),
                            };
                            retval.event_handler = None;
                            Self::process_job_result(
                                &mut retval,
                                Ok(Some(job_result)),
                                on_success_notice_cb,
                                view_settings,
                                context,
                            );
                            return Ok(retval);
                        }
                        return Ok(Self {
                            body_text: ViewFilterContent::Running {
                                job_id: job_handle.job_id,
                                on_success_notice_cb,
                                job_handle,
                                view_settings: view_settings.clone(),
                            },
                            ..retval
                        });
                    }
                }
            }
        }
        #[cfg(feature = "gpgme")]
        if let ContentType::Text {
            kind: Text::Plain, ..
        } = att.content_type
        {
            let content = att.text(Text::Plain);
            if content
                .trim_start()
                .starts_with("-----BEGIN PGP MESSAGE-----")
                && content.trim_end().ends_with("-----END PGP MESSAGE-----")
            {
                let bytes = content.trim().to_string().into_bytes();
                let decrypt_fut = async {
                    let (_metadata, bytes) = crate::mail::pgp::decrypt(
                        melib::email::pgp::convert_attachment_to_rfc_spec(&bytes),
                    )
                    .await
                    .map_err(|err| (err, bytes))?;
                    Ok((AttachmentBuilder::new(&bytes).build(), bytes))
                };
                let mut job_handle = context.main_loop_handler.job_executor.spawn(
                    "gpg::decrypt".into(),
                    decrypt_fut,
                    IsAsync::Blocking,
                );
                let on_success_notice_cb = Arc::new(|| "Decrypted content.".into());
                let mut retval = Self {
                    filter_invocation: "gpg::decrypt".into(),
                    content_type: att.content_type.clone(),
                    notice: None,
                    body_text: ViewFilterContent::Filtered {
                        inner: String::new(),
                    },
                    unfiltered: content.into_bytes(),
                    event_handler: Some(Self::job_process_event),
                    id: ComponentId::default(),
                };
                if let Ok(Some(job_result)) = try_recv_timeout!(&mut job_handle.chan) {
                    retval.body_text = ViewFilterContent::Running {
                        job_id: job_handle.job_id,
                        on_success_notice_cb: on_success_notice_cb.clone(),
                        job_handle,
                        view_settings: view_settings.clone(),
                    };
                    retval.event_handler = None;
                    Self::process_job_result(
                        &mut retval,
                        Ok(Some(job_result)),
                        on_success_notice_cb,
                        view_settings,
                        context,
                    );
                    return Ok(retval);
                }
                return Ok(Self {
                    body_text: ViewFilterContent::Running {
                        job_id: job_handle.job_id,
                        on_success_notice_cb,
                        job_handle,
                        view_settings: view_settings.clone(),
                    },
                    ..retval
                });
            }
        }
        Ok(Self {
            filter_invocation: String::new(),
            content_type: att.content_type.clone(),
            notice: None,
            body_text: ViewFilterContent::Filtered {
                inner: att.text(Text::Plain),
            },
            unfiltered: att.decode(Default::default()),
            event_handler: None,
            id: ComponentId::default(),
        })
    }

    fn html_process_event(self_: &mut Self, event: &mut UIEvent, context: &mut Context) -> bool {
        if matches!(event, UIEvent::Input(key) if *key == context.settings.shortcuts.envelope_view.open_html)
        {
            let command = context
                .settings
                .pager
                .html_open
                .as_ref()
                .map(|s| s.to_string())
                .or_else(|| query_default_app("text/html").ok());
            let command = if cfg!(target_os = "macos") {
                command.or_else(|| Some("open".into()))
            } else if cfg!(target_os = "linux") {
                command.or_else(|| Some("xdg-open".into()))
            } else {
                command
            };
            if let Some(command) = command {
                let res = File::create_temp_file(&self_.unfiltered, None, None, Some("html"), true)
                    .and_then(|p| {
                        let exec_cmd = desktop_exec_to_command(
                            &command,
                            p.path().display().to_string(),
                            false,
                        );

                        Ok((
                            p,
                            Command::new("sh")
                                .args(["-c", &exec_cmd])
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .spawn()?,
                        ))
                    });
                match res {
                    Ok((p, child)) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::UpdateSubStatus(command.clone()),
                        ));
                        context.temp_files.push(p);
                        context
                            .children
                            .entry(command.into())
                            .or_default()
                            .push(child);
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Failed to start `{command}`: {err}",
                            )),
                        ));
                    }
                }
            } else {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        "Couldn't find a default application for html files.".to_string(),
                    )));
            }
            return true;
        }
        false
    }

    pub fn contains_job_id(&self, match_job_id: JobId) -> bool {
        if let ViewFilterContent::Running { ref job_id, .. } = self.body_text {
            return *job_id == match_job_id;
        }
        if let ViewFilterContent::InlineAttachments { ref parts, .. } = self.body_text {
            return parts.iter().any(|p| p.contains_job_id(match_job_id));
        }
        false
    }

    fn job_process_event(self_: &mut Self, event: &mut UIEvent, context: &mut Context) -> bool {
        log::trace!(
            "job_process_event: self_ = {:?}, event = {:?}",
            self_,
            event
        );
        if matches!(event, UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)) if self_.contains_job_id(*job_id))
        {
            if matches!(self_.body_text, ViewFilterContent::Running { .. }) {
                if let ViewFilterContent::Running {
                    job_id: _,
                    mut job_handle,
                    on_success_notice_cb,
                    view_settings,
                } = std::mem::replace(
                    &mut self_.body_text,
                    ViewFilterContent::Filtered {
                        inner: String::new(),
                    },
                ) {
                    log::trace!("job_process_event: inside if let ");
                    let job_result = job_handle.chan.try_recv();
                    Self::process_job_result(
                        self_,
                        job_result,
                        on_success_notice_cb,
                        &view_settings,
                        context,
                    );
                }
                return true;
            }
            if let ViewFilterContent::InlineAttachments { ref mut parts, .. } = self_.body_text {
                return parts
                    .iter_mut()
                    .any(|p| Self::job_process_event(p, event, context));
            }
        }
        false
    }

    fn process_job_result(
        self_: &mut Self,
        result: std::result::Result<Option<FilterResult>, ::futures::channel::oneshot::Canceled>,
        on_success_notice_cb: OnSuccessNoticeCb,
        view_settings: &ViewSettings,
        context: &Context,
    ) {
        match result {
            Err(err) => {
                self_.event_handler = None;
                /* Job was cancelled */
                self_.body_text = ViewFilterContent::Error {
                    inner: Error::new("Job was cancelled.").set_source(Some(Arc::new(err))),
                };
                self_.notice = Some(format!("{} cancelled", self_.filter_invocation).into());
            }
            Ok(None) => {
                self_.event_handler = None;
                // something happened, perhaps a worker thread panicked
                self_.body_text = ViewFilterContent::Error {
                    inner: Error::new(
                        "Unknown error. Maybe some process panicked in the background?",
                    ),
                };
                self_.notice = Some(format!("{} failed", self_.filter_invocation).into());
            }
            Ok(Some(Ok((att, bytes)))) => {
                self_.event_handler = None;
                log::trace!("job_process_event: OK ");
                match Self::new_attachment(&att, view_settings, context) {
                    Ok(mut new_self) => {
                        if self_.content_type.is_text_html() {
                            new_self.event_handler = Some(Self::html_process_event);
                        }
                        new_self.unfiltered = bytes;
                        new_self.notice = Some(on_success_notice_cb());
                        *self_ = new_self;
                    }
                    Err(err) => {
                        self_.body_text = ViewFilterContent::Error { inner: err };
                        self_.notice = Some(
                            format!("decoding result of {} failed", self_.filter_invocation).into(),
                        );
                    }
                }
            }
            Ok(Some(Err((error, bytes)))) => {
                self_.event_handler = None;
                self_.body_text = ViewFilterContent::Error { inner: error };
                self_.unfiltered = bytes;
                self_.notice = Some(format!("{} failed", self_.filter_invocation).into());
            }
        }
    }
}

impl Component for ViewFilter {
    fn draw(&mut self, _: &mut CellBuffer, _: Area, _: &mut Context) {}
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut f) = self.event_handler {
            return f(self, event, context);
        }
        if let ViewFilterContent::InlineAttachments { ref mut parts, .. } = self.body_text {
            return parts.iter_mut().any(|p| p.process_event(event, context));
        }
        false
    }

    fn is_dirty(&self) -> bool {
        false
    }

    fn set_dirty(&mut self, _: bool) {}

    fn id(&self) -> ComponentId {
        self.id
    }
}
