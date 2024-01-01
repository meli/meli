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
    parser::BytesExt,
    text::Truncate,
    utils::xdg::query_default_app,
    Attachment, Result,
};

use crate::{
    components::*,
    desktop_exec_to_command,
    terminal::{Area, CellBuffer},
    Context, ErrorKind, File, StatusEvent, UIEvent,
};

#[derive(Clone)]
pub struct ViewFilter {
    pub filter_invocation: String,
    pub content_type: ContentType,
    pub notice: Option<Cow<'static, str>>,
    pub body_text: String,
    pub unfiltered: Vec<u8>,
    pub event_handler: Option<ProcessEventFn>,
    pub id: ComponentId,
}

impl std::fmt::Debug for ViewFilter {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(stringify!(ViewFilter))
            .field("filter_invocation", &self.filter_invocation)
            .field("content_type", &self.content_type)
            .field("notice", &self.notice)
            .field("body_text", &self.body_text.trim_at_boundary(18))
            .field("body_text_len", &self.body_text.len())
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
    pub fn new_html(body: &Attachment, context: &Context) -> Result<Self> {
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
        let bytes: Vec<u8> = att.decode(Default::default());

        let settings = &context.settings;
        if let Some(filter_invocation) = settings.pager.html_filter.as_ref() {
            match run("sh", &["-c", filter_invocation], &bytes) {
                Err(err) => {
                    return Err(Error::new(format!(
                        "Failed to start html filter process `{}`",
                        filter_invocation,
                    ))
                    .set_source(Some(Arc::new(err)))
                    .set_kind(ErrorKind::External));
                }
                Ok(body_text) => {
                    let notice =
                        Some(format!("Text piped through `{}`.\n\n", filter_invocation).into());
                    return Ok(Self {
                        filter_invocation: filter_invocation.clone(),
                        content_type: att.content_type.clone(),
                        notice,
                        body_text,
                        unfiltered: bytes,
                        event_handler: Some(Self::html_process_event),
                        id: ComponentId::default(),
                    });
                }
            }
        }
        if let Ok(body_text) = run("w3m", &["-I", "utf-8", "-T", "text/html"], &bytes) {
            return Ok(Self {
                filter_invocation: "w3m -I utf-8 -T text/html".into(),
                content_type: att.content_type.clone(),
                notice: Some("Text piped through `w3m -I utf-8 -T text/html`.\n\n".into()),
                body_text,
                unfiltered: bytes,
                event_handler: Some(Self::html_process_event),
                id: ComponentId::default(),
            });
        }

        Err(
            Error::new("Failed to find any application to use as html filter")
                .set_kind(ErrorKind::Configuration),
        )
    }

    pub fn new_attachment(att: &Attachment, context: &mut Context) -> Result<Self> {
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
                .map(|p| Self::new_attachment(p, context))
            {
                return Ok(v);
            }
        } else if let ContentType::Multipart {
            kind: MultipartType::Related,
            ref parts,
            ..
        } = att.content_type
        {
            if let Some(v @ Ok(_)) = parts.iter().find_map(|p| {
                if let v @ Ok(_) = Self::new_attachment(p, context) {
                    Some(v)
                } else {
                    None
                }
            }) {
                return v;
            }
        }
        if att.is_html() {
            return Self::new_html(att, context);
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
                body_text: String::new(),
                unfiltered: vec![],
                event_handler: None,
                id: ComponentId::default(),
            });
        }
        if let ContentType::Multipart {
            kind: MultipartType::Mixed,
            ref parts,
            ..
        } = att.content_type
        {
            if let Some(Ok(res)) =
                parts
                    .iter()
                    .find_map(|part| match Self::new_attachment(part, context) {
                        v @ Ok(_) => Some(v),
                        Err(_) => None,
                    })
            {
                return Ok(res);
            }
        }
        let notice = Some("Viewing attachment.\n\n".into());
        Ok(Self {
            filter_invocation: String::new(),
            content_type: att.content_type.clone(),
            notice,
            body_text: att.text(),
            unfiltered: att.decode(Default::default()),
            event_handler: None,
            id: ComponentId::default(),
        })
    }

    fn html_process_event(_self: &mut Self, event: &mut UIEvent, context: &mut Context) -> bool {
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
                match File::create_temp_file(&_self.unfiltered, None, None, Some("html"), true)
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
                    }) {
                    Ok((p, child)) => {
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(command)));
                        context.temp_files.push(p);
                        context.children.push(child);
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
}

impl Component for ViewFilter {
    fn draw(&mut self, _: &mut CellBuffer, _: Area, _: &mut Context) {}
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut f) = self.event_handler {
            return f(self, event, context);
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
