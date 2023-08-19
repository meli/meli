/*
 * meli
 *
 * Copyright 2017 Manos Pitsidianakis
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

use std::fmt::Write as IoWrite;

use melib::{attachment_types::Charset, error::*, pgp::DecryptionMetadata, Attachment, Result};

use crate::{
    conf::shortcuts::EnvelopeViewShortcuts,
    jobs::{JobId, JoinHandle},
    ShortcutMap, ThemeAttribute, UIDialog,
};

#[derive(Debug, Clone)]
pub struct ViewSettings {
    pub pager_filter: Option<String>,
    pub html_filter: Option<String>,
    pub url_launcher: Option<String>,
    pub expand_headers: bool,
    pub theme_default: ThemeAttribute,
    pub env_view_shortcuts: ShortcutMap,
    /// `"mail.view.body"`
    pub body_theme: ThemeAttribute,
    pub auto_choose_multipart_alternative: bool,
    pub sticky_headers: bool,
    pub show_date_in_my_timezone: bool,
    pub show_extra_headers: Vec<String>,
    pub auto_verify_signatures: bool,
    pub auto_decrypt: bool,
}

impl Default for ViewSettings {
    fn default() -> Self {
        Self {
            theme_default: Default::default(),
            body_theme: Default::default(),
            pager_filter: None,
            html_filter: None,
            url_launcher: None,
            env_view_shortcuts: EnvelopeViewShortcuts::default().key_values(),
            auto_choose_multipart_alternative: true,
            expand_headers: false,
            sticky_headers: false,
            show_date_in_my_timezone: false,
            show_extra_headers: vec![],
            auto_verify_signatures: true,
            auto_decrypt: true,
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum LinkKind {
    Url,
    Email,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Link {
    pub start: usize,
    pub end: usize,
    pub kind: self::LinkKind,
}

#[derive(Debug, Default)]
pub enum ForceCharset {
    #[default]
    None,
    Dialog(Box<UIDialog<Option<Charset>>>),
    Forced(Charset),
}

impl From<&ForceCharset> for Option<Charset> {
    fn from(val: &ForceCharset) -> Self {
        match val {
            ForceCharset::Forced(val) => Some(*val),
            ForceCharset::None | ForceCharset::Dialog(_) => None,
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Source {
    Decoded,
    Raw,
}

bitflags::bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct ViewOptions: u8 {
        const DEFAULT           = 0;
        const URL               = 1;
        const SOURCE            = Self::URL.bits() << 1;
        const SOURCE_RAW        = Self::SOURCE.bits() << 1;
    }
}

impl Default for ViewOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl ViewOptions {
    pub fn convert(
        &self,
        links: &mut Vec<Link>,
        attachment: &melib::Attachment,
        text: &str,
    ) -> String {
        let mut text = if self.contains(Self::SOURCE) {
            if self.contains(Self::SOURCE_RAW) {
                String::from_utf8_lossy(attachment.raw()).into_owned()
            } else {
                /* Decode each header value */
                let mut ret = String::new();
                match melib::email::parser::headers::headers(attachment.raw()).map(|(_, v)| v) {
                    Ok(headers) => {
                        for (h, v) in headers {
                            _ = match melib::email::parser::encodings::phrase(v, true) {
                                Ok((_, v)) => ret.write_fmt(format_args!(
                                    "{h}: {}\n",
                                    String::from_utf8_lossy(&v)
                                )),
                                Err(err) => ret.write_fmt(format_args!("{h}: {err}\n")),
                            };
                        }
                    }
                    Err(err) => {
                        _ = write!(&mut ret, "{err}");
                    }
                }
                if !ret.ends_with("\n\n") {
                    ret.push_str("\n\n");
                }
                ret.push_str(text);
                ret
            }
        } else {
            text.to_string()
        };

        while text.ends_with("\n\n") {
            text.pop();
            text.pop();
        }

        if self.contains(Self::URL) {
            if links.is_empty() {
                let finder = linkify::LinkFinder::new();
                *links = finder
                    .links(&text)
                    .filter_map(|l| {
                        if *l.kind() == linkify::LinkKind::Url {
                            Some(Link {
                                start: l.start(),
                                end: l.end(),
                                kind: LinkKind::Url,
                            })
                        } else if *l.kind() == linkify::LinkKind::Email {
                            Some(Link {
                                start: l.start(),
                                end: l.end(),
                                kind: LinkKind::Email,
                            })
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<Link>>();
            }
            for (lidx, l) in links.iter().enumerate().rev() {
                text.insert_str(l.start, &format!("[{}]", lidx));
            }
        }

        text
    }
}

#[derive(Debug)]
pub enum AttachmentDisplay {
    Alternative {
        inner: Box<Attachment>,
        shown_display: usize,
        display: Vec<AttachmentDisplay>,
    },
    InlineText {
        inner: Box<Attachment>,
        comment: Option<String>,
        text: String,
    },
    InlineOther {
        inner: Box<Attachment>,
    },
    Attachment {
        inner: Box<Attachment>,
    },
    SignedPending {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
        handle: JoinHandle<Result<()>>,
        job_id: JobId,
    },
    SignedFailed {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
        error: Error,
    },
    SignedUnverified {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
    },
    SignedVerified {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
        description: String,
    },
    EncryptedPending {
        inner: Box<Attachment>,
        handle: JoinHandle<Result<(DecryptionMetadata, Vec<u8>)>>,
    },
    EncryptedFailed {
        inner: Box<Attachment>,
        error: Error,
    },
    EncryptedSuccess {
        inner: Box<Attachment>,
        plaintext: Box<Attachment>,
        plaintext_display: Vec<AttachmentDisplay>,
        description: String,
    },
}

pub use filters::*;
mod filters {
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
        text_processing::Truncate,
        utils::xdg::query_default_app,
        Attachment, Result,
    };

    use crate::{
        components::*,
        create_temp_file, desktop_exec_to_command,
        terminal::{Area, CellBuffer},
        Context, ErrorKind, StatusEvent, UIEvent,
    };

    #[derive(Clone)]
    pub struct ViewFilter {
        pub filter_invocation: String,
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
        pub fn new_html(body: &Attachment, context: &mut Context) -> Result<Self> {
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
                    notice: None,
                    body_text: String::new(),
                    unfiltered: vec![],
                    event_handler: None,
                    id: ComponentId::default(),
                });
            }
            let notice = Some("Viewing attachment.\n\n".into());
            Ok(Self {
                filter_invocation: String::new(),
                notice,
                body_text: att.text(),
                unfiltered: att.decode(Default::default()),
                event_handler: None,
                id: ComponentId::default(),
            })
        }

        fn html_process_event(
            _self: &mut ViewFilter,
            event: &mut UIEvent,
            context: &mut Context,
        ) -> bool {
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
                    let p = create_temp_file(&_self.unfiltered, None, None, Some("html"), true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(
                            command.to_string(),
                        )));
                    let exec_cmd =
                        desktop_exec_to_command(&command, p.path.display().to_string(), false);

                    match Command::new("sh")
                        .args(["-c", &exec_cmd])
                        .stdin(Stdio::piped())
                        .stdout(Stdio::piped())
                        .spawn()
                    {
                        Ok(child) => {
                            context.temp_files.push(p);
                            context.children.push(child);
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(format!(
                                    "Failed to start `{}`: {}",
                                    &exec_cmd, err
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
}
