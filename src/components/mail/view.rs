/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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
    collections::HashSet,
    convert::TryFrom,
    fmt::Write as _,
    io::Write,
    os::unix::fs::PermissionsExt,
    process::{Command, Stdio},
};

use melib::{email::attachment_types::ContentType, list_management, parser::BytesExt};
use smallvec::SmallVec;

use super::*;
use crate::{
    conf::accounts::JobRequest,
    jobs::{JobId, JoinHandle},
};

mod html;
pub use self::html::*;
mod thread;
pub use self::thread::*;

mod envelope;
use linkify::LinkFinder;
use xdg_utils::query_default_app;

pub use self::envelope::*;

#[derive(Debug, Default)]
enum ForceCharset {
    #[default]
    None,
    Dialog(Box<UIDialog<Option<Charset>>>),
    Forced(Charset),
}

impl Into<Option<Charset>> for &ForceCharset {
    fn into(self) -> Option<Charset> {
        match self {
            ForceCharset::Forced(val) => Some(*val),
            ForceCharset::None | ForceCharset::Dialog(_) => None,
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Source {
    Decoded,
    Raw,
}

#[derive(PartialEq, Debug, Default)]
enum ViewMode {
    #[default]
    Normal,
    Url,
    Attachment(usize),
    Source(Source),
    //Ansi(RawBuffer),
    Subview,
    ContactSelector(Box<UIDialog<Card>>),
}

impl ViewMode {
    /*
    fn is_ansi(&self) -> bool {
        match self {
            ViewMode::Ansi(_) => true,
            _ => false,
        }
    }
    */

    fn is_attachment(&self) -> bool {
        matches!(self, ViewMode::Attachment(_))
    }

    fn is_contact_selector(&self) -> bool {
        matches!(self, ViewMode::ContactSelector(_))
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
        handle: JoinHandle<Result<(melib::pgp::DecryptionMetadata, Vec<u8>)>>,
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

/// Contains an Envelope view, with sticky headers, a pager for the body, and
/// subviews for more menus
#[derive(Debug, Default)]
pub struct MailView {
    coordinates: (AccountHash, MailboxHash, EnvelopeHash),
    pager: Pager,
    subview: Option<Box<dyn Component>>,
    dirty: bool,
    initialised: bool,
    mode: ViewMode,
    expand_headers: bool,
    attachment_tree: String,
    attachment_paths: Vec<Vec<usize>>,
    headers_no: usize,
    headers_cursor: usize,
    force_draw_headers: bool,
    theme_default: ThemeAttribute,
    active_jobs: HashSet<JobId>,
    state: MailViewState,
    force_charset: ForceCharset,

    cmd_buf: String,
    id: ComponentId,
}

#[derive(Debug, Copy, Clone)]
pub enum PendingReplyAction {
    Reply,
    ReplyToAuthor,
    ReplyToAll,
    ForwardAttachment,
    ForwardInline,
}

#[derive(Debug)]
enum MailViewState {
    Init {
        pending_action: Option<PendingReplyAction>,
    },
    LoadingBody {
        handle: JoinHandle<Result<Vec<u8>>>,
        pending_action: Option<PendingReplyAction>,
    },
    Error {
        err: Error,
    },
    Loaded {
        bytes: Vec<u8>,
        env: Box<Envelope>,
        body: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
        body_text: String,
        links: Vec<Link>,
    },
}

impl MailViewState {
    fn load_bytes(self_: &mut MailView, bytes: Vec<u8>, context: &mut Context) {
        let account = &mut context.accounts[&self_.coordinates.0];
        if account
            .collection
            .get_env(self_.coordinates.2)
            .other_headers()
            .is_empty()
        {
            let _ = account
                .collection
                .get_env_mut(self_.coordinates.2)
                .populate_headers(&bytes);
        }
        let env = Box::new(account.collection.get_env(self_.coordinates.2).clone());
        let body = Box::new(AttachmentBuilder::new(&bytes).build());
        let display = MailView::attachment_to(
            &body,
            context,
            self_.coordinates,
            &mut self_.active_jobs,
            (&self_.force_charset).into(),
        );
        let (paths, attachment_tree_s) = self_.attachment_displays_to_tree(&display);
        self_.attachment_tree = attachment_tree_s;
        self_.attachment_paths = paths;
        let body_text = self_.attachment_displays_to_text(&display, context, true);
        self_.state = MailViewState::Loaded {
            display,
            env,
            body,
            bytes,
            body_text,
            links: vec![],
        };
    }

    fn redecode(self_: &mut MailView, context: &mut Context) {
        let (new_display, new_body_text) =
            if let MailViewState::Loaded { ref body, .. } = self_.state {
                let new_display = MailView::attachment_to(
                    body,
                    context,
                    self_.coordinates,
                    &mut self_.active_jobs,
                    (&self_.force_charset).into(),
                );
                let (paths, attachment_tree_s) = self_.attachment_displays_to_tree(&new_display);
                self_.attachment_tree = attachment_tree_s;
                self_.attachment_paths = paths;
                let body_text = self_.attachment_displays_to_text(&new_display, context, true);
                (new_display, body_text)
            } else {
                return;
            };

        if let MailViewState::Loaded {
            ref mut display,
            ref mut body_text,
            ..
        } = self_.state
        {
            *display = new_display;
            *body_text = new_body_text;
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum LinkKind {
    Url,
    Email,
}

#[derive(Debug, Copy, Clone)]
struct Link {
    start: usize,
    end: usize,
    kind: LinkKind,
}

impl Default for MailViewState {
    fn default() -> Self {
        MailViewState::Init {
            pending_action: None,
        }
    }
}

impl Clone for MailView {
    fn clone(&self) -> Self {
        MailView {
            subview: None,
            cmd_buf: String::with_capacity(4),
            pager: self.pager.clone(),
            mode: ViewMode::Normal,
            attachment_tree: self.attachment_tree.clone(),
            attachment_paths: self.attachment_paths.clone(),
            state: MailViewState::default(),
            active_jobs: self.active_jobs.clone(),
            force_charset: ForceCharset::None,
            ..*self
        }
    }
}

impl fmt::Display for MailView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "view mail")
    }
}

impl MailView {
    pub fn new(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        pager: Option<Pager>,
        subview: Option<Box<dyn Component>>,
        context: &mut Context,
    ) -> Self {
        let mut ret = MailView {
            coordinates,
            pager: pager.unwrap_or_default(),
            subview,
            dirty: true,
            initialised: false,
            mode: ViewMode::Normal,
            expand_headers: false,
            attachment_tree: String::new(),
            attachment_paths: vec![],

            headers_no: 5,
            headers_cursor: 0,
            force_draw_headers: false,

            theme_default: crate::conf::value(context, "mail.view.body"),
            active_jobs: Default::default(),
            state: MailViewState::default(),
            force_charset: ForceCharset::None,
            cmd_buf: String::with_capacity(4),
            id: ComponentId::new_v4(),
        };

        ret.init_futures(context);
        ret
    }

    fn init_futures(&mut self, context: &mut Context) {
        debug!("init_futures");
        self.theme_default = crate::conf::value(context, "mail.view.body");
        let mut pending_action = None;
        let account = &mut context.accounts[&self.coordinates.0];
        if debug!(account.contains_key(self.coordinates.2)) {
            {
                match account
                    .operation(self.coordinates.2)
                    .and_then(|mut op| op.as_bytes())
                {
                    Ok(fut) => {
                        let mut handle = account.job_executor.spawn_specialized(fut);
                        let job_id = handle.job_id;
                        pending_action = if let MailViewState::Init {
                            ref mut pending_action,
                        } = self.state
                        {
                            pending_action.take()
                        } else {
                            None
                        };
                        if let Ok(Some(bytes_result)) = try_recv_timeout!(&mut handle.chan) {
                            match bytes_result {
                                Ok(bytes) => {
                                    MailViewState::load_bytes(self, bytes, context);
                                }
                                Err(err) => {
                                    self.state = MailViewState::Error { err };
                                }
                            }
                        } else {
                            self.state = MailViewState::LoadingBody {
                                handle,
                                pending_action: pending_action.take(),
                            };
                            self.active_jobs.insert(job_id);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::NewJob(job_id)));
                        }
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Could not get message: {}", err)),
                        ));
                    }
                }
            }
            let account = &mut context.accounts[&self.coordinates.0];
            if !account.collection.get_env(self.coordinates.2).is_seen() {
                let job = account.backend.write().unwrap().set_flags(
                    self.coordinates.2.into(),
                    self.coordinates.1,
                    smallvec::smallvec![(Ok(Flag::SEEN), true)],
                );
                match job {
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account.insert_job(
                            handle.job_id,
                            JobRequest::SetFlags {
                                env_hashes: self.coordinates.2.into(),
                                handle,
                            },
                        );
                    }
                    Err(e) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Could not set message as seen: {}",
                                e
                            )),
                        ));
                    }
                };
            }
        }
        if let Some(p) = pending_action {
            self.perform_action(p, context);
        }
    }

    fn perform_action(&mut self, action: PendingReplyAction, context: &mut Context) {
        let (bytes, reply_body, env) = match self.state {
            MailViewState::Init {
                ref mut pending_action,
                ..
            }
            | MailViewState::LoadingBody {
                ref mut pending_action,
                ..
            } => {
                if pending_action.is_none() {
                    *pending_action = Some(action);
                }
                return;
            }
            MailViewState::Loaded {
                ref bytes,
                ref display,
                ref env,
                ..
            } => (
                bytes,
                self.attachment_displays_to_text(display, context, false),
                env,
            ),
            MailViewState::Error { .. } => {
                return;
            }
        };
        let composer = match action {
            PendingReplyAction::Reply => Box::new(Composer::reply_to_select(
                self.coordinates,
                reply_body,
                context,
            )),
            PendingReplyAction::ReplyToAuthor => Box::new(Composer::reply_to_author(
                self.coordinates,
                reply_body,
                context,
            )),
            PendingReplyAction::ReplyToAll => Box::new(Composer::reply_to_all(
                self.coordinates,
                reply_body,
                context,
            )),
            PendingReplyAction::ForwardAttachment => Box::new(Composer::forward(
                self.coordinates,
                bytes,
                env,
                true,
                context,
            )),
            PendingReplyAction::ForwardInline => Box::new(Composer::forward(
                self.coordinates,
                bytes,
                env,
                false,
                context,
            )),
        };

        context
            .replies
            .push_back(UIEvent::Action(Tab(New(Some(composer)))));
    }

    fn attachment_displays_to_text(
        &self,
        displays: &[AttachmentDisplay],
        context: &mut Context,
        show_comments: bool,
    ) -> String {
        let mut acc = String::new();
        for d in displays {
            use AttachmentDisplay::*;
            match d {
                Alternative {
                    inner: _,
                    shown_display,
                    display,
                } => {
                    acc.push_str(&self.attachment_displays_to_text(
                        &display[*shown_display..(*shown_display + 1)],
                        context,
                        show_comments,
                    ));
                }
                InlineText {
                    inner: _,
                    text,
                    comment: Some(comment),
                } if show_comments => {
                    acc.push_str(comment);
                    if !acc.ends_with("\n\n") {
                        acc.push_str("\n\n");
                    }
                    acc.push_str(text);
                }
                InlineText {
                    inner: _,
                    text,
                    comment: _,
                } => acc.push_str(text),
                InlineOther { inner } => {
                    if !acc.ends_with("\n\n") {
                        acc.push_str("\n\n");
                    }
                    acc.push_str(&inner.to_string());
                    if !acc.ends_with("\n\n") {
                        acc.push_str("\n\n");
                    }
                }
                Attachment { inner: _ } => {}
                SignedPending {
                    inner: _,
                    display,
                    handle: _,
                    job_id: _,
                } => {
                    if show_comments {
                        acc.push_str("Waiting for signature verification.\n\n");
                    }
                    acc.push_str(&self.attachment_displays_to_text(
                        display,
                        context,
                        show_comments,
                    ));
                }
                SignedUnverified { inner: _, display } => {
                    if show_comments {
                        acc.push_str("Unverified signature.\n\n");
                    }
                    acc.push_str(&self.attachment_displays_to_text(display, context, show_comments))
                }
                SignedFailed {
                    inner: _,
                    display,
                    error,
                } => {
                    if show_comments {
                        let _ = writeln!(acc, "Failed to verify signature: {}.\n", error);
                    }
                    acc.push_str(&self.attachment_displays_to_text(
                        display,
                        context,
                        show_comments,
                    ));
                }
                SignedVerified {
                    inner: _,
                    display,
                    description,
                } => {
                    if show_comments {
                        if description.is_empty() {
                            acc.push_str("Verified signature.\n\n");
                        } else {
                            acc.push_str(description);
                            acc.push_str("\n\n");
                        }
                    }
                    acc.push_str(&self.attachment_displays_to_text(
                        display,
                        context,
                        show_comments,
                    ));
                }
                EncryptedPending { .. } => acc.push_str("Waiting for decryption result."),
                EncryptedFailed { inner: _, error } => {
                    let _ = write!(acc, "Decryption failed: {}.", &error);
                }
                EncryptedSuccess {
                    inner: _,
                    plaintext: _,
                    plaintext_display,
                    description,
                } => {
                    if show_comments {
                        if description.is_empty() {
                            acc.push_str("Succesfully decrypted.\n\n");
                        } else {
                            acc.push_str(description);
                            acc.push_str("\n\n");
                        }
                    }
                    acc.push_str(&self.attachment_displays_to_text(
                        plaintext_display,
                        context,
                        show_comments,
                    ));
                }
            }
        }
        acc
    }

    fn attachment_displays_to_tree(
        &self,
        displays: &[AttachmentDisplay],
    ) -> (Vec<Vec<usize>>, String) {
        let mut acc = String::new();
        let mut branches = SmallVec::new();
        let mut paths = Vec::with_capacity(displays.len());
        let mut cur_path = vec![];
        let mut idx = 0;

        fn append_entry(
            (idx, (depth, att_display)): (&mut usize, (usize, &AttachmentDisplay)),
            branches: &mut SmallVec<[bool; 8]>,
            paths: &mut Vec<Vec<usize>>,
            cur_path: &mut Vec<usize>,
            has_sibling: bool,
            s: &mut String,
        ) {
            use AttachmentDisplay::*;
            let mut default_alternative: Option<usize> = None;
            let (att, sub_att_display_vec) = match att_display {
                Alternative {
                    inner,
                    shown_display,
                    display,
                } => {
                    default_alternative = Some(*shown_display);
                    (inner, display.as_slice())
                }
                InlineText {
                    inner,
                    text: _,
                    comment: _,
                }
                | InlineOther { inner }
                | Attachment { inner }
                | EncryptedPending { inner, handle: _ }
                | EncryptedFailed { inner, error: _ } => (inner, &[][..]),
                SignedPending {
                    inner,
                    display,
                    handle: _,
                    job_id: _,
                }
                | SignedUnverified { inner, display }
                | SignedFailed {
                    inner,
                    display,
                    error: _,
                }
                | SignedVerified {
                    inner,
                    display,
                    description: _,
                }
                | EncryptedSuccess {
                    inner: _,
                    plaintext: inner,
                    plaintext_display: display,
                    description: _,
                } => (inner, display.as_slice()),
            };
            s.extend(format!("\n[{}]", idx).chars());
            for &b in branches.iter() {
                if b {
                    s.push('|');
                } else {
                    s.push(' ');
                }
                s.push(' ');
            }
            if depth > 0 {
                if has_sibling {
                    s.push('|');
                } else {
                    s.push(' ');
                }
                s.push_str("\\_ ");
            } else {
                s.push(' ');
                s.push(' ');
            }

            s.push_str(&att.to_string());
            paths.push(cur_path.clone());
            if matches!(att.content_type, ContentType::Multipart { .. }) {
                let mut iter = (0..sub_att_display_vec.len()).peekable();
                if has_sibling {
                    branches.push(true);
                } else {
                    branches.push(false);
                }
                while let Some(i) = iter.next() {
                    *idx += 1;
                    cur_path.push(i);
                    append_entry(
                        (idx, (depth + 1, &sub_att_display_vec[i])),
                        branches,
                        paths,
                        cur_path,
                        iter.peek().is_some(),
                        s,
                    );
                    if Some(i) == default_alternative {
                        s.push_str(" (displayed by default)");
                    }
                    cur_path.pop();
                }
                branches.pop();
            }
        }

        for (i, d) in displays.iter().enumerate() {
            cur_path.push(i);
            append_entry(
                (&mut idx, (0, d)),
                &mut branches,
                &mut paths,
                &mut cur_path,
                i + 1 < displays.len(),
                &mut acc,
            );
            cur_path.pop();
            idx += 1;
        }
        (paths, acc)
    }

    fn attachment_to(
        body: &Attachment,
        context: &mut Context,
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        active_jobs: &mut HashSet<JobId>,
        force_charset: Option<Charset>,
    ) -> Vec<AttachmentDisplay> {
        let mut ret = vec![];
        fn rec(
            a: &Attachment,
            context: &mut Context,
            coordinates: (AccountHash, MailboxHash, EnvelopeHash),
            acc: &mut Vec<AttachmentDisplay>,
            active_jobs: &mut HashSet<JobId>,
            force_charset: Option<Charset>,
        ) {
            if a.content_disposition.kind.is_attachment() || a.content_type == "message/rfc822" {
                acc.push(AttachmentDisplay::Attachment {
                    inner: Box::new(a.clone()),
                });
            } else if a.content_type().is_text_html() {
                let bytes = a.decode(force_charset.into());
                let filter_invocation =
                    mailbox_settings!(context[coordinates.0][&coordinates.1].pager.html_filter)
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or("w3m -I utf-8 -T text/html");
                let command_obj = Command::new("sh")
                    .args(["-c", filter_invocation])
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn();
                match command_obj {
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some(format!(
                                "Failed to start html filter process: {}",
                                filter_invocation,
                            )),
                            err.to_string(),
                            Some(NotificationType::Error(melib::ErrorKind::External)),
                        ));
                        let comment = Some(format!(
                            "Failed to start html filter process: `{}`. Press `v` to open in web \
                             browser. \n\n",
                            filter_invocation
                        ));
                        let text = String::from_utf8_lossy(&bytes).to_string();
                        acc.push(AttachmentDisplay::InlineText {
                            inner: Box::new(a.clone()),
                            comment,
                            text,
                        });
                    }
                    Ok(mut html_filter) => {
                        html_filter
                            .stdin
                            .as_mut()
                            .unwrap()
                            .write_all(&bytes)
                            .expect("Failed to write to stdin");
                        let comment = Some(format!(
                            "Text piped through `{}`. Press `v` to open in web browser. \n\n",
                            filter_invocation
                        ));
                        let text = String::from_utf8_lossy(
                            &html_filter.wait_with_output().unwrap().stdout,
                        )
                        .to_string();
                        acc.push(AttachmentDisplay::InlineText {
                            inner: Box::new(a.clone()),
                            comment,
                            text,
                        });
                    }
                }
            } else if a.is_text() {
                let bytes = a.decode(force_charset.into());
                acc.push(AttachmentDisplay::InlineText {
                    inner: Box::new(a.clone()),
                    comment: None,
                    text: String::from_utf8_lossy(&bytes).to_string(),
                });
            } else if let ContentType::Multipart {
                ref kind,
                ref parts,
                ..
            } = a.content_type
            {
                match kind {
                    MultipartType::Alternative => {
                        if parts.is_empty() {
                            return;
                        }
                        let mut display = vec![];
                        let mut chosen_attachment_idx = 0;
                        if let Some(text_attachment_pos) =
                            parts.iter().position(|a| a.content_type == "text/plain")
                        {
                            let bytes = &parts[text_attachment_pos].decode(force_charset.into());
                            if bytes.trim().is_empty()
                                && mailbox_settings!(
                                    context[coordinates.0][&coordinates.1]
                                        .pager
                                        .auto_choose_multipart_alternative
                                )
                                .is_true()
                            {
                                if let Some(text_attachment_pos) =
                                    parts.iter().position(|a| a.content_type == "text/html")
                                {
                                    /* Select html alternative since text/plain is empty */
                                    chosen_attachment_idx = text_attachment_pos;
                                }
                            } else {
                                /* Select text/plain alternative */
                                chosen_attachment_idx = text_attachment_pos;
                            }
                        }
                        for a in parts {
                            rec(
                                a,
                                context,
                                coordinates,
                                &mut display,
                                active_jobs,
                                force_charset,
                            );
                        }
                        acc.push(AttachmentDisplay::Alternative {
                            inner: Box::new(a.clone()),
                            shown_display: chosen_attachment_idx,
                            display,
                        });
                    }
                    MultipartType::Signed => {
                        #[cfg(not(feature = "gpgme"))]
                        {
                            acc.push(AttachmentDisplay::SignedUnverified {
                                inner: Box::new(a.clone()),
                                display: {
                                    let mut v = vec![];
                                    rec(
                                        &parts[0],
                                        context,
                                        coordinates,
                                        &mut v,
                                        active_jobs,
                                        force_charset,
                                    );
                                    v
                                },
                            });
                        }
                        #[cfg(feature = "gpgme")]
                        {
                            if *mailbox_settings!(
                                context[coordinates.0][&coordinates.1]
                                    .pgp
                                    .auto_verify_signatures
                            ) {
                                let verify_fut = crate::components::mail::pgp::verify(a.clone());
                                let handle = context.job_executor.spawn_specialized(verify_fut);
                                active_jobs.insert(handle.job_id);
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::NewJob(handle.job_id),
                                ));
                                acc.push(AttachmentDisplay::SignedPending {
                                    inner: Box::new(a.clone()),
                                    job_id: handle.job_id,
                                    display: {
                                        let mut v = vec![];
                                        rec(
                                            &parts[0],
                                            context,
                                            coordinates,
                                            &mut v,
                                            active_jobs,
                                            force_charset,
                                        );
                                        v
                                    },
                                    handle,
                                });
                            } else {
                                acc.push(AttachmentDisplay::SignedUnverified {
                                    inner: Box::new(a.clone()),
                                    display: {
                                        let mut v = vec![];
                                        rec(
                                            &parts[0],
                                            context,
                                            coordinates,
                                            &mut v,
                                            active_jobs,
                                            force_charset,
                                        );
                                        v
                                    },
                                });
                            }
                        }
                    }
                    MultipartType::Encrypted => {
                        for a in parts {
                            if a.content_type == "application/octet-stream" {
                                #[cfg(not(feature = "gpgme"))]
                                {
                                    acc.push(AttachmentDisplay::EncryptedFailed {
                                        inner: Box::new(a.clone()),
                                        error: Error::new(
                                            "Cannot decrypt: meli must be compiled with libgpgme \
                                             support.",
                                        ),
                                    });
                                }
                                #[cfg(feature = "gpgme")]
                                {
                                    if *mailbox_settings!(
                                        context[coordinates.0][&coordinates.1].pgp.auto_decrypt
                                    ) {
                                        let decrypt_fut =
                                            crate::components::mail::pgp::decrypt(a.raw().to_vec());
                                        let handle =
                                            context.job_executor.spawn_specialized(decrypt_fut);
                                        active_jobs.insert(handle.job_id);
                                        context.replies.push_back(UIEvent::StatusEvent(
                                            StatusEvent::NewJob(handle.job_id),
                                        ));
                                        acc.push(AttachmentDisplay::EncryptedPending {
                                            inner: Box::new(a.clone()),
                                            handle,
                                        });
                                    } else {
                                        acc.push(AttachmentDisplay::EncryptedFailed {
                                            inner: Box::new(a.clone()),
                                            error: Error::new("Undecrypted."),
                                        });
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        for a in parts {
                            rec(a, context, coordinates, acc, active_jobs, force_charset);
                        }
                    }
                }
            }
        }
        rec(
            body,
            context,
            coordinates,
            &mut ret,
            active_jobs,
            force_charset,
        );
        ret
    }

    pub fn update(
        &mut self,
        new_coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        context: &mut Context,
    ) {
        if self.coordinates != new_coordinates {
            self.coordinates = new_coordinates;
            self.mode = ViewMode::Normal;
            self.initialised = false;
            self.init_futures(context);
            self.set_dirty(true);
        }
    }

    fn open_attachment(
        &'_ self,
        lidx: usize,
        context: &mut Context,
    ) -> Option<&'_ melib::Attachment> {
        let display = if let MailViewState::Loaded { ref display, .. } = self.state {
            display
        } else {
            return None;
        };
        if let Some(path) = self.attachment_paths.get(lidx).and_then(|path| {
            if !path.is_empty() {
                Some(path)
            } else {
                None
            }
        }) {
            let first = path[0];
            use AttachmentDisplay::*;
            let root_attachment = match &display[first] {
                Alternative {
                    inner,
                    shown_display: _,
                    display: _,
                }
                | InlineText {
                    inner,
                    text: _,
                    comment: _,
                }
                | InlineOther { inner }
                | Attachment { inner }
                | SignedPending {
                    inner,
                    display: _,
                    handle: _,
                    job_id: _,
                }
                | SignedFailed {
                    inner,
                    display: _,
                    error: _,
                }
                | SignedVerified {
                    inner,
                    display: _,
                    description: _,
                }
                | SignedUnverified { inner, display: _ }
                | EncryptedPending { inner, handle: _ }
                | EncryptedFailed { inner, error: _ }
                | EncryptedSuccess {
                    inner: _,
                    plaintext: inner,
                    plaintext_display: _,
                    description: _,
                } => inner,
            };
            fn find_attachment<'a>(
                a: &'a melib::Attachment,
                path: &[usize],
            ) -> Option<&'a melib::Attachment> {
                if path.is_empty() {
                    return Some(a);
                }
                if let ContentType::Multipart { ref parts, .. } = a.content_type {
                    let first = path[0];
                    if first < parts.len() {
                        return find_attachment(&parts[first], &path[1..]);
                    }
                }
                None
            }

            let ret = find_attachment(root_attachment, &path[1..]);
            if lidx == 0 {
                return ret.and_then(|a| {
                    if a.content_disposition.kind.is_attachment()
                        || a.content_type == "message/rfc822"
                    {
                        Some(a)
                    } else {
                        None
                    }
                });
            } else {
                return ret;
            }
        }
        context
            .replies
            .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                "Attachment `{}` not found.",
                lidx
            ))));
        None
    }

    fn start_contact_selector(&mut self, context: &mut Context) {
        let account = &context.accounts[&self.coordinates.0];
        if !account.contains_key(self.coordinates.2) {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                    "Email not found".into(),
                )));
            return;
        }
        let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);

        let mut entries = Vec::new();
        for addr in envelope.from().iter().chain(envelope.to().iter()) {
            let mut new_card: Card = Card::new();
            new_card.set_email(addr.get_email());
            if let Some(display_name) = addr.get_display_name() {
                new_card.set_name(display_name);
            }
            entries.push((new_card, format!("{}", addr)));
        }
        drop(envelope);
        self.mode = ViewMode::ContactSelector(Box::new(Selector::new(
            "select contacts to add",
            entries,
            false,
            Some(Box::new(move |id: ComponentId, results: &[Card]| {
                Some(UIEvent::FinishedUIDialog(id, Box::new(results.to_vec())))
            })),
            context,
        )));
        self.dirty = true;
        self.initialised = false;
    }
}

impl Component for MailView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() && !self.force_draw_headers {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let y: usize = {
            let account = &context.accounts[&self.coordinates.0];
            if !account.contains_key(self.coordinates.2) {
                /* The envelope has been renamed or removed, so wait for the appropriate
                 * event to arrive */
                return;
            }
            let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);

            let headers = crate::conf::value(context, "mail.view.headers");
            let headers_names = crate::conf::value(context, "mail.view.headers_names");
            let headers_area = crate::conf::value(context, "mail.view.headers_area");

            if let ViewMode::Source(_) = self.mode {
                clear_area(grid, area, self.theme_default);
                context.dirty_areas.push_back(area);
                get_y(upper_left)
            } else {
                let height_p = self.pager.size().1;

                let height = height!(area) - self.headers_no - 1;

                self.headers_no = 0;
                let mut skip_header_ctr = self.headers_cursor;
                let sticky = *mailbox_settings!(
                    context[self.coordinates.0][&self.coordinates.1]
                        .pager
                        .headers_sticky
                ) || height_p < height;
                let (_, mut y) = upper_left;
                macro_rules! print_header {
                    ($(($header:literal, $string:expr)),*$(,)?) => {
                        $({
                            if sticky || skip_header_ctr == 0 {
                                if y <= get_y(bottom_right) {
                                    let (_x, _y) = write_string_to_grid(
                                        $header,
                                        grid,
                                        headers_names.fg,
                                        headers_names.bg,
                                        headers_names.attrs,
                                        (set_y(upper_left, y), bottom_right),
                                        Some(get_x(upper_left)),
                                    );
                                    if let Some(cell) = grid.get_mut(_x, _y) {
                                        cell.set_ch(' ')
                                            .set_fg(headers_area.fg)
                                            .set_bg(headers_area.bg)
                                            .set_attrs(headers_area.attrs);
                                    }

                                    let (_x, _y) = write_string_to_grid(
                                        &$string,
                                        grid,
                                        headers.fg,
                                        headers.bg,
                                        headers.attrs,
                                        ((_x + 1, _y), bottom_right),
                                        Some(get_x(upper_left)),
                                    );
                                    clear_area(
                                        grid,
                                        (
                                            (std::cmp::min(_x, get_x(bottom_right)), _y),
                                            (get_x(bottom_right), _y),
                                        ),
                                        headers_area,
                                    );
                                    y = _y + 1;
                                }
                            } else {
                                skip_header_ctr -= 1;
                            }
                            self.headers_no += 1;
                        })+
                    };
                }
                let find_offset = |s: &str| -> (bool, (i64, i64)) {
                    let mut diff = (true, (0, 0));
                    if let Some(pos) = s.as_bytes().iter().position(|b| *b == b'+' || *b == b'-') {
                        let offset = &s[pos..];
                        diff.0 = offset.starts_with('+');
                        if let (Ok(hr_offset), Ok(min_offset)) =
                            (offset[1..3].parse::<i64>(), offset[3..5].parse::<i64>())
                        {
                            diff.1 .0 = hr_offset;
                            diff.1 .1 = min_offset;
                        }
                    }
                    diff
                };
                let orig_date = envelope.date_as_str();
                let date_str: std::borrow::Cow<str> = if mailbox_settings!(
                    context[self.coordinates.0][&self.coordinates.1]
                        .pager
                        .show_date_in_my_timezone
                )
                .is_true()
                {
                    let local_date = melib::datetime::timestamp_to_string(
                        envelope.timestamp,
                        Some(melib::datetime::RFC822_DATE),
                        false,
                    );
                    let orig_offset = find_offset(orig_date);
                    let local_offset = find_offset(&local_date);
                    if orig_offset == local_offset {
                        orig_date.into()
                    } else {
                        format!(
                            "{} [actual timezone: {}{:02}{:02}]",
                            local_date,
                            if orig_offset.0 { '+' } else { '-' },
                            orig_offset.1 .0,
                            orig_offset.1 .1
                        )
                        .into()
                    }
                } else {
                    orig_date.into()
                };
                print_header!(
                    ("Date:", date_str),
                    ("From:", envelope.field_from_to_string()),
                    ("To:", envelope.field_to_to_string()),
                );
                if envelope.other_headers().contains_key("Cc")
                    && !envelope.other_headers()["Cc"].is_empty()
                {
                    print_header!(("Cc:", envelope.field_cc_to_string()));
                }
                print_header!(
                    ("Subject:", envelope.subject()),
                    ("Message-ID:", format!("<{}>", envelope.message_id_raw()))
                );
                if self.expand_headers {
                    if let Some(val) = envelope.in_reply_to_display() {
                        print_header!(
                            ("In-Reply-To:", val),
                            (
                                "References:",
                                envelope
                                    .references()
                                    .iter()
                                    .map(std::string::ToString::to_string)
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            )
                        );
                    }
                }
                if let Some(list_management::ListActions {
                    ref id,
                    ref archive,
                    ref post,
                    ref unsubscribe,
                }) = list_management::ListActions::detect(&envelope)
                {
                    let mut x = get_x(upper_left);
                    if let Some(id) = id {
                        if sticky || skip_header_ctr == 0 {
                            clear_area(
                                grid,
                                (set_y(upper_left, y), set_y(bottom_right, y)),
                                headers_area,
                            );
                            let (_x, _) = write_string_to_grid(
                                "List-ID: ",
                                grid,
                                headers_names.fg,
                                headers_names.bg,
                                headers_names.attrs,
                                (set_y(upper_left, y), bottom_right),
                                None,
                            );
                            let (_x, _y) = write_string_to_grid(
                                id,
                                grid,
                                headers.fg,
                                headers.bg,
                                headers.attrs,
                                ((_x, y), bottom_right),
                                None,
                            );
                            x = _x;
                            if _y != y {
                                x = get_x(upper_left);
                            }
                            y = _y;
                        }
                        self.headers_no += 1;
                    }
                    if sticky || skip_header_ctr == 0 {
                        if archive.is_some() || post.is_some() || unsubscribe.is_some() {
                            let (_x, _y) = write_string_to_grid(
                                " Available actions: [ ",
                                grid,
                                headers_names.fg,
                                headers_names.bg,
                                headers_names.attrs,
                                ((x, y), bottom_right),
                                Some(get_x(upper_left)),
                            );
                            x = _x;
                            y = _y;
                        }
                        if archive.is_some() {
                            let (_x, _y) = write_string_to_grid(
                                "list-archive, ",
                                grid,
                                headers.fg,
                                headers.bg,
                                headers.attrs,
                                ((x, y), bottom_right),
                                Some(get_x(upper_left)),
                            );
                            x = _x;
                            y = _y;
                        }
                        if post.is_some() {
                            let (_x, _y) = write_string_to_grid(
                                "list-post, ",
                                grid,
                                headers.fg,
                                headers.bg,
                                headers.attrs,
                                ((x, y), bottom_right),
                                Some(get_x(upper_left)),
                            );
                            x = _x;
                            y = _y;
                        }
                        if unsubscribe.is_some() {
                            let (_x, _y) = write_string_to_grid(
                                "list-unsubscribe, ",
                                grid,
                                headers.fg,
                                headers.bg,
                                headers.attrs,
                                ((x, y), bottom_right),
                                Some(get_x(upper_left)),
                            );
                            x = _x;
                            y = _y;
                        }
                        if archive.is_some() || post.is_some() || unsubscribe.is_some() {
                            if x >= 2 {
                                grid[(x - 2, y)].set_ch(' ');
                            }
                            if x > 0 {
                                grid[(x - 1, y)]
                                    .set_ch(']')
                                    .set_fg(headers_names.fg)
                                    .set_bg(headers_names.bg)
                                    .set_attrs(headers_names.attrs);
                            }
                        }
                        for x in x..=get_x(bottom_right) {
                            grid[(x, y)]
                                .set_ch(' ')
                                .set_fg(headers_area.fg)
                                .set_bg(headers_area.bg);
                        }
                        y += 1;
                    }
                }

                self.force_draw_headers = false;
                clear_area(
                    grid,
                    (set_y(upper_left, y), set_y(bottom_right, y)),
                    headers_area,
                );
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 3)));
                if !*mailbox_settings!(
                    context[self.coordinates.0][&self.coordinates.1]
                        .pager
                        .headers_sticky
                ) {
                    let height_p = self.pager.size().1;

                    let height = height!(area).saturating_sub(y).saturating_sub(1);
                    if self.pager.cursor_pos() >= self.headers_no {
                        get_y(upper_left)
                    } else if (height_p > height && self.headers_cursor < self.headers_no + 1)
                        || self.headers_cursor == 0
                        || height_p < height
                    {
                        y + 1
                    } else {
                        get_y(upper_left)
                    }
                } else {
                    y + 1
                }
            }
        };

        if !self.initialised {
            let (body, body_text, bytes, links) = if let MailViewState::Loaded {
                ref body,
                ref body_text,
                ref bytes,
                ref mut links,
                ..
            } = self.state
            {
                (body, body_text, bytes, links)
            } else if let MailViewState::Error { ref err } = self.state {
                clear_area(
                    grid,
                    (set_y(upper_left, y), bottom_right),
                    self.theme_default,
                );
                context
                    .dirty_areas
                    .push_back((set_y(upper_left, y), bottom_right));
                context.replies.push_back(UIEvent::Notification(
                    Some("Failed to open e-mail".to_string()),
                    err.to_string(),
                    Some(NotificationType::Error(err.kind)),
                ));
                log::error!("Failed to open envelope: {err}");
                self.init_futures(context);
                return;
            } else {
                clear_area(
                    grid,
                    (set_y(upper_left, y), bottom_right),
                    self.theme_default,
                );
                context
                    .dirty_areas
                    .push_back((set_y(upper_left, y), bottom_right));
                return;
            };
            self.initialised = true;
            match self.mode {
                ViewMode::Attachment(aidx) => {
                    let mut text = "Viewing attachment. Press `r` to return \n".to_string();
                    if let Some(attachment) = self.open_attachment(aidx, context) {
                        if attachment.is_html() {
                            let mut subview = Box::new(HtmlView::new(attachment, context));
                            subview.set_coordinates(Some(self.coordinates));
                            self.subview = Some(subview);
                            self.mode = ViewMode::Subview;
                        } else {
                            text.push_str(&attachment.text());
                            let colors = crate::conf::value(context, "mail.view.body");
                            self.pager =
                                Pager::from_string(text, Some(context), Some(0), None, colors);
                            if let Some(ref filter) = mailbox_settings!(
                                context[self.coordinates.0][&self.coordinates.1]
                                    .pager
                                    .filter
                            ) {
                                self.pager.filter(filter);
                            }
                            self.subview = None;
                        }
                    } else {
                        text.push_str("Internal error. MailView::open_attachment failed.");
                        let colors = crate::conf::value(context, "mail.view.body");
                        self.pager = Pager::from_string(text, Some(context), Some(0), None, colors);
                        if let Some(ref filter) = mailbox_settings!(
                            context[self.coordinates.0][&self.coordinates.1]
                                .pager
                                .filter
                        ) {
                            self.pager.filter(filter);
                        }
                        self.subview = None;
                    }
                }
                ViewMode::Normal if body.is_html() => {
                    let mut subview = Box::new(HtmlView::new(body, context));
                    subview.set_coordinates(Some(self.coordinates));
                    self.subview = Some(subview);
                    self.mode = ViewMode::Subview;
                }
                ViewMode::Normal
                    if mailbox_settings!(
                        context[self.coordinates.0][&self.coordinates.1]
                            .pager
                            .auto_choose_multipart_alternative
                    )
                    .is_true()
                        && match body.content_type {
                            ContentType::Multipart {
                                kind: MultipartType::Alternative,
                                ref parts,
                                ..
                            } => parts.iter().all(|p| {
                                p.is_html() || (p.is_text() && p.body().trim().is_empty())
                            }),
                            _ => false,
                        } =>
                {
                    let mut subview = Box::new(HtmlView::new(
                        body.content_type
                            .parts()
                            .unwrap()
                            .iter()
                            .find(|a| a.is_html())
                            .unwrap_or(body),
                        context,
                    ));
                    subview.set_coordinates(Some(self.coordinates));
                    self.subview = Some(subview);
                    self.mode = ViewMode::Subview;
                    self.initialised = false;
                }
                ViewMode::Subview | ViewMode::ContactSelector(_) => {}
                ViewMode::Source(source) => {
                    let text = {
                        if source == Source::Raw {
                            String::from_utf8_lossy(bytes).into_owned()
                        } else {
                            /* Decode each header value */
                            let mut ret = melib::email::parser::headers::headers(bytes)
                                .map(|(_, v)| v)
                                .map_err(|err| err.into())
                                .and_then(|headers| {
                                    Ok(headers
                                        .into_iter()
                                        .map(|(h, v)| {
                                            melib::email::parser::encodings::phrase(v, true)
                                                .map(|(_, v)| {
                                                    let mut h = h.to_vec();
                                                    h.push(b':');
                                                    h.push(b' ');
                                                    h.extend(v.into_iter());
                                                    h
                                                })
                                                .map_err(|err| err.into())
                                        })
                                        .collect::<Result<Vec<Vec<u8>>>>()?
                                        .join(&b"\n"[..]))
                                })
                                .map(|v| String::from_utf8_lossy(&v).into_owned())
                                .unwrap_or_else(|err: Error| err.to_string());
                            if !ret.ends_with("\n\n") {
                                ret.push_str("\n\n");
                            }
                            ret.push_str(body_text);
                            if !ret.ends_with("\n\n") {
                                ret.push_str("\n\n");
                            }
                            ret.push_str(&self.attachment_tree);
                            ret
                        }
                    };
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager = Pager::from_string(text, Some(context), None, None, colors);
                    if let Some(ref filter) = mailbox_settings!(
                        context[self.coordinates.0][&self.coordinates.1]
                            .pager
                            .filter
                    ) {
                        self.pager.filter(filter);
                    }
                }
                ViewMode::Url => {
                    let mut text = body_text.clone();
                    if links.is_empty() {
                        let finder = LinkFinder::new();
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
                    if !text.ends_with("\n\n") {
                        text.push_str("\n\n");
                    }
                    text.push_str(&self.attachment_tree);

                    let cursor_pos = self.pager.cursor_pos();
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager =
                        Pager::from_string(text, Some(context), Some(cursor_pos), None, colors);
                    if let Some(ref filter) = mailbox_settings!(
                        context[self.coordinates.0][&self.coordinates.1]
                            .pager
                            .filter
                    ) {
                        self.pager.filter(filter);
                    }
                    self.subview = None;
                }
                _ => {
                    let mut text = body_text.clone();
                    if !text.ends_with("\n\n") {
                        text.push_str("\n\n");
                    }
                    text.push_str(&self.attachment_tree);
                    let cursor_pos = if self.mode.is_attachment() {
                        0
                    } else {
                        self.pager.cursor_pos()
                    };
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager =
                        Pager::from_string(text, Some(context), Some(cursor_pos), None, colors);
                    if let Some(ref filter) = mailbox_settings!(
                        context[self.coordinates.0][&self.coordinates.1]
                            .pager
                            .filter
                    ) {
                        self.pager.filter(filter);
                    }
                    self.subview = None;
                }
            };
        }
        match self.mode {
            ViewMode::Subview if self.subview.is_some() => {
                if let Some(s) = self.subview.as_mut() {
                    if !s.is_dirty() {
                        s.set_dirty(true);
                    }
                    s.draw(grid, (set_y(upper_left, y), bottom_right), context);
                }
            }
            _ => {
                self.pager
                    .draw(grid, (set_y(upper_left, y), bottom_right), context);
            }
        }
        if let ViewMode::ContactSelector(ref mut s) = self.mode {
            s.draw(grid, area, context);
        }

        if let ForceCharset::Dialog(ref mut s) = self.force_charset {
            s.draw(grid, area, context);
        }

        self.dirty = false;
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        if self.coordinates.0.is_null() || self.coordinates.1.is_null() {
            return false;
        }

        match (&mut self.force_charset, &event) {
            (ForceCharset::Dialog(selector), UIEvent::FinishedUIDialog(id, results))
                if *id == selector.id() =>
            {
                self.force_charset =
                    if let Some(results) = results.downcast_ref::<Vec<Option<Charset>>>() {
                        if results.len() != 1 {
                            ForceCharset::None
                        } else if let Some(charset) = results[0] {
                            ForceCharset::Forced(charset)
                        } else {
                            ForceCharset::None
                        }
                    } else {
                        ForceCharset::None
                    };
                MailViewState::redecode(self, context);
                self.initialised = false;
                self.set_dirty(true);
                return true;
            }
            (ForceCharset::Dialog(selector), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            _ => {}
        }

        let shortcuts = self.get_shortcuts(context);
        match (&mut self.mode, &mut event) {
            /*(ViewMode::Ansi(ref mut buf), _) => {
                if buf.process_event(event, context) {
                    return true;
                }
            }*/
            (ViewMode::Subview, _) => {
                if let Some(s) = self.subview.as_mut() {
                    if s.process_event(event, context) {
                        return true;
                    }
                }
            }
            (ViewMode::ContactSelector(ref s), UIEvent::FinishedUIDialog(id, results))
                if *id == s.id() =>
            {
                if let Some(results) = results.downcast_ref::<Vec<Card>>() {
                    let account = &mut context.accounts[&self.coordinates.0];
                    {
                        for card in results.iter() {
                            account.address_book.add_card(card.clone());
                        }
                    }
                }
                self.mode = ViewMode::Normal;
                self.initialised = false;
                self.set_dirty(true);
                return true;
            }
            (ViewMode::ContactSelector(ref mut s), _) => {
                if s.process_event(event, context) {
                    return true;
                }
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            _ => match event {
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::PAGER]["scroll_up"])
                        && !*mailbox_settings!(
                            context[self.coordinates.0][&self.coordinates.1]
                                .pager
                                .headers_sticky
                        )
                        && self.headers_cursor <= self.headers_no =>
                {
                    self.force_draw_headers = true;
                    if self.pager.cursor_pos() == 0 {
                        self.headers_cursor = self.headers_cursor.saturating_sub(1);
                    } else if self.pager.process_event(event, context) {
                        return true;
                    }
                    self.pager.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::PAGER]["scroll_down"])
                        && !*mailbox_settings!(
                            context[self.coordinates.0][&self.coordinates.1]
                                .pager
                                .headers_sticky
                        )
                        && self.headers_cursor < self.headers_no =>
                {
                    self.force_draw_headers = true;
                    self.headers_cursor += 1;
                    self.pager.set_dirty(true);
                    return true;
                }
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                    if self.active_jobs.contains(job_id) =>
                {
                    match self.state {
                        MailViewState::LoadingBody {
                            ref mut handle,
                            pending_action: _,
                        } if handle.job_id == *job_id => {
                            match handle.chan.try_recv() {
                                Err(_) => { /* Job was canceled */ }
                                Ok(None) => { /* something happened, perhaps a worker
                                      * thread panicked */
                                }
                                Ok(Some(Ok(bytes))) => {
                                    MailViewState::load_bytes(self, bytes, context);
                                }
                                Ok(Some(Err(err))) => {
                                    self.state = MailViewState::Error { err };
                                }
                            }
                        }
                        MailViewState::Init { .. } => {
                            self.init_futures(context);
                        }
                        MailViewState::Loaded {
                            ref mut display, ..
                        } => {
                            let mut caught = false;
                            for d in display.iter_mut() {
                                match d {
                                    AttachmentDisplay::SignedPending {
                                        inner,
                                        handle,
                                        display,
                                        job_id: our_job_id,
                                    } if *our_job_id == *job_id => {
                                        caught = true;
                                        self.initialised = false;
                                        match handle.chan.try_recv() {
                                            Err(_) => { /* Job was canceled */ }
                                            Ok(None) => { /* something happened,
                                                  * perhaps a worker thread
                                                  * panicked */
                                            }
                                            Ok(Some(Ok(()))) => {
                                                *d = AttachmentDisplay::SignedVerified {
                                                    inner: std::mem::replace(
                                                        inner,
                                                        Box::new(
                                                            AttachmentBuilder::new(&[]).build(),
                                                        ),
                                                    ),
                                                    display: std::mem::take(display),
                                                    description: String::new(),
                                                };
                                            }
                                            Ok(Some(Err(error))) => {
                                                *d = AttachmentDisplay::SignedFailed {
                                                    inner: std::mem::replace(
                                                        inner,
                                                        Box::new(
                                                            AttachmentBuilder::new(&[]).build(),
                                                        ),
                                                    ),
                                                    display: std::mem::take(display),
                                                    error,
                                                };
                                            }
                                        }
                                    }
                                    AttachmentDisplay::EncryptedPending { inner, handle }
                                        if handle.job_id == *job_id =>
                                    {
                                        caught = true;
                                        self.initialised = false;
                                        match handle.chan.try_recv() {
                                            Err(_) => { /* Job was canceled */ }
                                            Ok(None) => { /* something happened,
                                                  * perhaps a worker thread
                                                  * panicked */
                                            }
                                            Ok(Some(Ok((metadata, decrypted_bytes)))) => {
                                                let plaintext = Box::new(
                                                    AttachmentBuilder::new(&decrypted_bytes)
                                                        .build(),
                                                );
                                                let plaintext_display = Self::attachment_to(
                                                    &plaintext,
                                                    context,
                                                    self.coordinates,
                                                    &mut self.active_jobs,
                                                    (&self.force_charset).into(),
                                                );
                                                *d = AttachmentDisplay::EncryptedSuccess {
                                                    inner: std::mem::replace(
                                                        inner,
                                                        Box::new(
                                                            AttachmentBuilder::new(&[]).build(),
                                                        ),
                                                    ),
                                                    plaintext,
                                                    plaintext_display,
                                                    description: format!("{:?}", metadata),
                                                };
                                            }
                                            Ok(Some(Err(error))) => {
                                                *d = AttachmentDisplay::EncryptedFailed {
                                                    inner: std::mem::replace(
                                                        inner,
                                                        Box::new(
                                                            AttachmentBuilder::new(&[]).build(),
                                                        ),
                                                    ),
                                                    error,
                                                };
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            if caught {
                                let mut new_body_text = String::new();
                                if let MailViewState::Loaded { ref display, .. } = self.state {
                                    new_body_text =
                                        self.attachment_displays_to_text(display, context, true);
                                    let (paths, attachment_tree_s) =
                                        self.attachment_displays_to_tree(display);
                                    self.attachment_tree = attachment_tree_s;
                                    self.attachment_paths = paths;
                                }
                                if let MailViewState::Loaded {
                                    ref mut body_text,
                                    ref mut links,
                                    ..
                                } = self.state
                                {
                                    links.clear();
                                    *body_text = new_body_text;
                                }
                            }
                        }
                        _ => {}
                    }
                    self.active_jobs.remove(job_id);
                    self.set_dirty(true);
                }
                _ => {
                    if self.pager.process_event(event, context) {
                        return true;
                    }
                }
            },
        }

        let shortcuts = &self.get_shortcuts(context);
        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["reply"]) =>
            {
                self.perform_action(PendingReplyAction::Reply, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["reply_to_all"]) =>
            {
                self.perform_action(PendingReplyAction::ReplyToAll, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["reply_to_author"]) =>
            {
                self.perform_action(PendingReplyAction::ReplyToAuthor, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["forward"]) =>
            {
                match mailbox_settings!(
                    context[self.coordinates.0][&self.coordinates.1]
                        .composing
                        .forward_as_attachment
                ) {
                    f if f.is_ask() => {
                        let id = self.id;
                        context.replies.push_back(UIEvent::GlobalUIDialog(Box::new(
                            UIConfirmationDialog::new(
                                "How do you want the email to be forwarded?",
                                vec![
                                    (true, "inline".to_string()),
                                    (false, "as attachment".to_string()),
                                ],
                                true,
                                Some(Box::new(move |_: ComponentId, result: bool| {
                                    Some(UIEvent::FinishedUIDialog(
                                        id,
                                        Box::new(if result {
                                            PendingReplyAction::ForwardInline
                                        } else {
                                            PendingReplyAction::ForwardAttachment
                                        }),
                                    ))
                                })),
                                context,
                            ),
                        )));
                    }
                    f if f.is_true() => {
                        self.perform_action(PendingReplyAction::ForwardAttachment, context);
                    }
                    _ => {
                        self.perform_action(PendingReplyAction::ForwardInline, context);
                    }
                }
                return true;
            }
            UIEvent::FinishedUIDialog(id, ref result) if id == self.id() => {
                if let Some(result) = result.downcast_ref::<PendingReplyAction>() {
                    self.perform_action(*result, context);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["edit"]) =>
            {
                let account_hash = self.coordinates.0;
                let env_hash = self.coordinates.2;
                let (sender, mut receiver) = crate::jobs::oneshot::channel();
                let operation = context.accounts[&account_hash].operation(env_hash);
                let bytes_job = async move {
                    let _ = sender.send(operation?.as_bytes()?.await);
                    Ok(())
                };
                let handle = if context.accounts[&account_hash]
                    .backend_capabilities
                    .is_async
                {
                    context.accounts[&account_hash]
                        .job_executor
                        .spawn_specialized(bytes_job)
                } else {
                    context.accounts[&account_hash]
                        .job_executor
                        .spawn_blocking(bytes_job)
                };
                context.accounts[&account_hash].insert_job(
                    handle.job_id,
                    crate::conf::accounts::JobRequest::Generic {
                        name: "fetch envelope".into(),
                        handle,
                        on_finish: Some(CallbackFn(Box::new(move |context: &mut Context| {
                            match receiver.try_recv() {
                                Err(_) => { /* Job was canceled */ }
                                Ok(None) => { /* something happened, perhaps a worker
                                      * thread panicked */
                                }
                                Ok(Some(result)) => {
                                    match result.and_then(|bytes| {
                                        Composer::edit(account_hash, env_hash, &bytes, context)
                                    }) {
                                        Ok(composer) => {
                                            context.replies.push_back(UIEvent::Action(Tab(New(
                                                Some(Box::new(composer)),
                                            ))));
                                        }
                                        Err(err) => {
                                            let err_string = format!(
                                                "Failed to open envelope {}: {}",
                                                context.accounts[&account_hash]
                                                    .collection
                                                    .envelopes
                                                    .read()
                                                    .unwrap()
                                                    .get(&env_hash)
                                                    .map(|env| env.message_id_display())
                                                    .unwrap_or_else(|| "Not found".into()),
                                                err
                                            );
                                            log::error!("{err_string}");
                                            context.replies.push_back(UIEvent::Notification(
                                                Some("Failed to open e-mail".to_string()),
                                                err_string,
                                                Some(NotificationType::Error(err.kind)),
                                            ));
                                        }
                                    }
                                }
                            }
                        }))),
                        log_level: LogLevel::DEBUG,
                    },
                );
                return true;
            }
            UIEvent::Action(View(ViewAction::AddAddressesToContacts)) => {
                self.start_contact_selector(context);
                return true;
            }
            UIEvent::Input(ref key)
                if !self.mode.is_contact_selector()
                    && shortcut!(
                        key == shortcuts[Shortcuts::ENVELOPE_VIEW]["add_addresses_to_contacts"]
                    ) =>
            {
                self.start_contact_selector(context);
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt(''))
                if self.mode.is_contact_selector() =>
            {
                self.mode = ViewMode::Normal;
                self.set_dirty(true);
                self.initialised = false;
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c.is_ascii_digit() => {
                self.cmd_buf.push(c);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal
                    || self.mode == ViewMode::Subview
                    || self.mode == ViewMode::Source(Source::Decoded)
                    || self.mode == ViewMode::Source(Source::Raw))
                    && shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["view_raw_source"]) =>
            {
                self.mode = match self.mode {
                    ViewMode::Source(Source::Decoded) => ViewMode::Source(Source::Raw),
                    _ => ViewMode::Source(Source::Decoded),
                };
                self.set_dirty(true);
                self.initialised = false;
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode.is_attachment()
                    /*|| self.mode.is_ansi()*/
                    || self.mode == ViewMode::Subview
                    || self.mode == ViewMode::Url
                    || self.mode == ViewMode::Source(Source::Decoded)
                    || self.mode == ViewMode::Source(Source::Raw))
                    && shortcut!(
                        key == shortcuts[Shortcuts::ENVELOPE_VIEW]["return_to_normal_view"]
                    ) =>
            {
                self.mode = ViewMode::Normal;
                self.set_dirty(true);
                self.initialised = false;
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview)
                    && !self.cmd_buf.is_empty()
                    && shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["open_mailcap"]) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                match self.state {
                    MailViewState::Error { .. } | MailViewState::LoadingBody { .. } => {}
                    MailViewState::Loaded { .. } => {
                        if let Some(attachment) = self.open_attachment(lidx, context) {
                            if let Ok(()) =
                                crate::mailcap::MailcapEntry::execute(attachment, context)
                            {
                                self.set_dirty(true);
                            } else {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(format!(
                                        "no mailcap entry found for {}",
                                        attachment.content_type()
                                    )),
                                ));
                            }
                        }
                    }
                    MailViewState::Init { .. } => {
                        self.init_futures(context);
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["open_attachment"])
                    && !self.cmd_buf.is_empty()
                    && (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                match self.state {
                    MailViewState::Error { .. } | MailViewState::LoadingBody { .. } => {}
                    MailViewState::Loaded { .. } => {
                        if let Some(attachment) = self.open_attachment(lidx, context) {
                            match attachment.content_type() {
                                ContentType::MessageRfc822 => {
                                    match Mail::new(attachment.body().to_vec(), Some(Flag::SEEN)) {
                                        Ok(wrapper) => {
                                            context.replies.push_back(UIEvent::Action(Tab(New(
                                                Some(Box::new(EnvelopeView::new(
                                                    wrapper,
                                                    None,
                                                    None,
                                                    self.coordinates.0,
                                                ))),
                                            ))));
                                        }
                                        Err(e) => {
                                            context.replies.push_back(UIEvent::StatusEvent(
                                                StatusEvent::DisplayMessage(format!("{}", e)),
                                            ));
                                        }
                                    }
                                }

                                ContentType::Text { .. }
                                | ContentType::PGPSignature
                                | ContentType::CMSSignature => {
                                    self.mode = ViewMode::Attachment(lidx);
                                    self.initialised = false;
                                    self.dirty = true;
                                }
                                ContentType::Multipart { .. } => {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(
                                            "Multipart attachments are not supported yet."
                                                .to_string(),
                                        ),
                                    ));
                                }
                                ContentType::Other { .. } => {
                                    let attachment_type = attachment.mime_type();
                                    let filename = attachment.filename();
                                    if let Ok(command) = query_default_app(&attachment_type) {
                                        let p = create_temp_file(
                                            &attachment.decode(Default::default()),
                                            filename.as_deref(),
                                            None,
                                            true,
                                        );
                                        let exec_cmd = desktop_exec_to_command(
                                            &command,
                                            p.path.display().to_string(),
                                            false,
                                        );
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
                                        context.replies.push_back(UIEvent::StatusEvent(
                                            StatusEvent::DisplayMessage(
                                                if let Some(filename) = filename.as_ref() {
                                                    format!(
                                                        "Couldn't find a default application for \
                                                         file {} (type {})",
                                                        filename, attachment_type
                                                    )
                                                } else {
                                                    format!(
                                                        "Couldn't find a default application for \
                                                         type {}",
                                                        attachment_type
                                                    )
                                                },
                                            ),
                                        ));
                                    }
                                }
                                ContentType::OctetStream {
                                    ref name,
                                    parameters: _,
                                } => {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!(
                                            "Failed to open {}. application/octet-stream isn't \
                                             supported yet",
                                            name.as_ref().map(|n| n.as_str()).unwrap_or("file")
                                        )),
                                    ));
                                }
                            }
                        }
                    }
                    MailViewState::Init { .. } => {
                        self.init_futures(context);
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Url)
                    && shortcut!(
                        key == shortcuts[Shortcuts::ENVELOPE_VIEW]["toggle_expand_headers"]
                    ) =>
            {
                self.expand_headers = !self.expand_headers;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if !self.cmd_buf.is_empty()
                    && self.mode == ViewMode::Url
                    && shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["go_to_url"]) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                match self.state {
                    MailViewState::Init { .. } => {
                        self.init_futures(context);
                    }
                    MailViewState::Error { .. } | MailViewState::LoadingBody { .. } => {}
                    MailViewState::Loaded {
                        body: _,
                        bytes: _,
                        display: _,
                        env: _,
                        ref body_text,
                        ref links,
                    } => {
                        let (_kind, url) = {
                            if let Some(l) = links
                                .get(lidx)
                                .and_then(|l| Some((l.kind, body_text.get(l.start..l.end)?)))
                            {
                                l
                            } else {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(format!(
                                        "Link `{}` not found.",
                                        lidx
                                    )),
                                ));
                                return true;
                            }
                        };

                        let url_launcher = mailbox_settings!(
                            context[self.coordinates.0][&self.coordinates.1]
                                .pager
                                .url_launcher
                        )
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or(
                            #[cfg(target_os = "macos")]
                            {
                                "open"
                            },
                            #[cfg(not(target_os = "macos"))]
                            {
                                "xdg-open"
                            },
                        );
                        match Command::new(url_launcher)
                            .arg(url)
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()
                        {
                            Ok(child) => {
                                context.children.push(child);
                            }
                            Err(err) => {
                                context.replies.push_back(UIEvent::Notification(
                                    Some(format!("Failed to launch {:?}", url_launcher)),
                                    err.to_string(),
                                    Some(NotificationType::Error(melib::ErrorKind::External)),
                                ));
                            }
                        }
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Url)
                    && shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["toggle_url_mode"]) =>
            {
                match self.mode {
                    ViewMode::Normal => self.mode = ViewMode::Url,
                    ViewMode::Url => self.mode = ViewMode::Normal,
                    _ => {}
                }
                self.initialised = false;
                self.dirty = true;
                return true;
            }
            UIEvent::EnvelopeRename(old_hash, new_hash) if self.coordinates.2 == old_hash => {
                self.coordinates.2 = new_hash;
            }
            UIEvent::Action(View(ViewAction::ExportMail(ref path))) => {
                // Save entire message as eml
                let account = &context.accounts[&self.coordinates.0];
                if !account.contains_key(self.coordinates.2) {
                    /* The envelope has been renamed or removed, so wait for the appropriate
                     * event to arrive */
                    return true;
                }
                let bytes = if let MailViewState::Loaded { ref bytes, .. } = self.state {
                    bytes
                } else if let MailViewState::Error { ref err } = self.state {
                    context.replies.push_back(UIEvent::Notification(
                        Some("Failed to open e-mail".to_string()),
                        err.to_string(),
                        Some(NotificationType::Error(err.kind)),
                    ));
                    log::error!("Failed to open envelope: {err}");
                    self.init_futures(context);
                    return true;
                } else {
                    return true;
                };

                let mut path = std::path::Path::new(path).to_path_buf();

                if path.is_dir() {
                    let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                    path.push(format!("{}.eml", envelope.message_id_raw()));
                }
                match save_attachment(&path, bytes) {
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some(format!("Failed to create file at {}", path.display())),
                            err.to_string(),
                            Some(NotificationType::Error(melib::ErrorKind::External)),
                        ));
                        log::error!("Failed to create file at {}: {err}", path.display());
                        return true;
                    }
                    Ok(()) => {
                        context.replies.push_back(UIEvent::Notification(
                            None,
                            format!("Saved at {}", &path.display()),
                            Some(NotificationType::Info),
                        ));
                    }
                }

                return true;
            }
            UIEvent::Action(View(ViewAction::SaveAttachment(a_i, ref path))) => {
                {
                    let account = &context.accounts[&self.coordinates.0];
                    if !account.contains_key(self.coordinates.2) {
                        /* The envelope has been renamed or removed, so wait for the appropriate
                         * event to arrive */
                        return true;
                    }
                }
                let bytes = if let MailViewState::Loaded { ref bytes, .. } = self.state {
                    bytes
                } else if let MailViewState::Error { ref err } = self.state {
                    context.replies.push_back(UIEvent::Notification(
                        Some("Failed to open e-mail".to_string()),
                        err.to_string(),
                        Some(NotificationType::Error(err.kind)),
                    ));
                    log::error!("Failed to open envelope: {err}");
                    self.init_futures(context);
                    return true;
                } else {
                    return true;
                };

                let mut path = std::path::Path::new(path).to_path_buf();

                if let Some(u) = self.open_attachment(a_i, context) {
                    if path.is_dir() {
                        if let Some(filename) = u.filename() {
                            path.push(filename);
                        } else {
                            let u = melib::uuid::Uuid::new_v4();
                            path.push(u.as_hyphenated().to_string());
                        }
                    }
                    match save_attachment(&path, &u.decode(Default::default())) {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!("Failed to create file at {}", path.display())),
                                err.to_string(),
                                Some(NotificationType::Error(melib::ErrorKind::External)),
                            ));
                            log::error!("Failed to create file at {}: {err}", path.display());
                        }
                        Ok(()) => {
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                format!("Saved at {}", path.display()),
                                Some(NotificationType::Info),
                            ));
                        }
                    }
                } else if a_i == 0 {
                    let account = &context.accounts[&self.coordinates.0];
                    // Save entire message as eml
                    if path.is_dir() {
                        let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                        path.push(format!("{}.eml", envelope.message_id_raw()));
                    }
                    match save_attachment(&path, bytes) {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!("Failed to create file at {}", path.display())),
                                err.to_string(),
                                Some(NotificationType::Error(melib::ErrorKind::External)),
                            ));
                            log::error!("Failed to create file at {}: {err}", path.display());
                            return true;
                        }
                        Ok(()) => {
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                format!("Saved at {}", &path.display()),
                                Some(NotificationType::Info),
                            ));
                        }
                    }

                    return true;
                } else {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                            "Attachment `{}` not found.",
                            a_i
                        ))));
                }
                return true;
            }
            UIEvent::Action(MailingListAction(ref e)) => {
                let account = &context.accounts[&self.coordinates.0];
                if !account.contains_key(self.coordinates.2) {
                    /* The envelope has been renamed or removed, so wait for the appropriate
                     * event to arrive */
                    return true;
                }
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                let detect = list_management::ListActions::detect(&envelope);
                if let Some(ref actions) = detect {
                    match e {
                        MailingListAction::ListPost if actions.post.is_some() => {
                            /* open composer */
                            let mut failure = true;
                            if let list_management::ListAction::Email(list_post_addr) =
                                actions.post.as_ref().unwrap()[0]
                            {
                                if let Ok(mailto) = Mailto::try_from(list_post_addr) {
                                    let draft: Draft = mailto.into();
                                    let mut composer =
                                        Composer::with_account(self.coordinates.0, context);
                                    composer.set_draft(draft);
                                    context.replies.push_back(UIEvent::Action(Tab(New(Some(
                                        Box::new(composer),
                                    )))));
                                    failure = false;
                                }
                            }
                            if failure {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(String::from(
                                        "Couldn't parse List-Post header value",
                                    )),
                                ));
                            }
                            return true;
                        }
                        MailingListAction::ListUnsubscribe if actions.unsubscribe.is_some() => {
                            /* autosend or open unsubscribe option */
                            let unsubscribe = actions.unsubscribe.as_ref().unwrap();
                            for option in unsubscribe.iter() {
                                /* TODO: Ask for confirmation before proceding with an action */
                                match option {
                                    list_management::ListAction::Email(email) => {
                                        if let Ok(mailto) = Mailto::try_from(*email) {
                                            let mut draft: Draft = mailto.into();
                                            draft.set_header(
                                                HeaderName::FROM,
                                                context.accounts[&self.coordinates.0]
                                                    .settings
                                                    .account()
                                                    .make_display_name(),
                                            );
                                            /* Manually drop stuff because borrowck doesn't do it
                                             * on its own */
                                            drop(detect);
                                            drop(envelope);
                                            if let Err(err) = super::compose::send_draft(
                                                ToggleFlag::False,
                                                context,
                                                self.coordinates.0,
                                                draft,
                                                SpecialUsageMailbox::Sent,
                                                Flag::SEEN,
                                                true,
                                            ) {
                                                context.replies.push_back(UIEvent::StatusEvent(
                                                    StatusEvent::DisplayMessage(format!(
                                                        "Couldn't send unsubscribe e-mail: {}",
                                                        err
                                                    )),
                                                ));
                                            }
                                            return true;
                                        }
                                    }
                                    list_management::ListAction::Url(url) => {
                                        let url_launcher = mailbox_settings!(
                                            context[self.coordinates.0][&self.coordinates.1]
                                                .pager
                                                .url_launcher
                                        )
                                        .as_ref()
                                        .map(|s| s.as_str())
                                        .unwrap_or(
                                            #[cfg(target_os = "macos")]
                                            {
                                                "open"
                                            },
                                            #[cfg(not(target_os = "macos"))]
                                            {
                                                "xdg-open"
                                            },
                                        );
                                        match Command::new(url_launcher)
                                            .arg(String::from_utf8_lossy(url).into_owned())
                                            .stdin(Stdio::piped())
                                            .stdout(Stdio::piped())
                                            .spawn()
                                        {
                                            Ok(child) => {
                                                context.children.push(child);
                                            }
                                            Err(err) => {
                                                context.replies.push_back(UIEvent::StatusEvent(
                                                    StatusEvent::DisplayMessage(format!(
                                                        "Couldn't launch {:?}: {}",
                                                        url_launcher, err
                                                    )),
                                                ));
                                            }
                                        }
                                        return true;
                                    }
                                    list_management::ListAction::No => {}
                                }
                            }
                        }
                        MailingListAction::ListArchive if actions.archive.is_some() => {
                            /* open archive url with url_launcher */
                            let url_launcher = mailbox_settings!(
                                context[self.coordinates.0][&self.coordinates.1]
                                    .pager
                                    .url_launcher
                            )
                            .as_ref()
                            .map(|s| s.as_str())
                            .unwrap_or(
                                #[cfg(target_os = "macos")]
                                {
                                    "open"
                                },
                                #[cfg(not(target_os = "macos"))]
                                {
                                    "xdg-open"
                                },
                            );
                            match Command::new(url_launcher)
                                .arg(actions.archive.unwrap())
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .spawn()
                            {
                                Ok(child) => context.children.push(child),
                                Err(err) => {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!(
                                            "Couldn't launch {:?}: {}",
                                            url_launcher, err
                                        )),
                                    ));
                                }
                            }
                            return true;
                        }
                        _ => { /* error print message to user */ }
                    }
                };
            }
            UIEvent::Action(Listing(OpenInNewTab)) => {
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(self.clone()))))));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["change_charset"]) =>
            {
                let entries = vec![
                    (None, "default".to_string()),
                    (Some(Charset::Ascii), Charset::Ascii.to_string()),
                    (Some(Charset::UTF8), Charset::UTF8.to_string()),
                    (Some(Charset::UTF16), Charset::UTF16.to_string()),
                    (Some(Charset::ISO8859_1), Charset::ISO8859_1.to_string()),
                    (Some(Charset::ISO8859_2), Charset::ISO8859_2.to_string()),
                    (Some(Charset::ISO8859_3), Charset::ISO8859_3.to_string()),
                    (Some(Charset::ISO8859_4), Charset::ISO8859_4.to_string()),
                    (Some(Charset::ISO8859_5), Charset::ISO8859_5.to_string()),
                    (Some(Charset::ISO8859_6), Charset::ISO8859_6.to_string()),
                    (Some(Charset::ISO8859_7), Charset::ISO8859_7.to_string()),
                    (Some(Charset::ISO8859_8), Charset::ISO8859_8.to_string()),
                    (Some(Charset::ISO8859_10), Charset::ISO8859_10.to_string()),
                    (Some(Charset::ISO8859_13), Charset::ISO8859_13.to_string()),
                    (Some(Charset::ISO8859_14), Charset::ISO8859_14.to_string()),
                    (Some(Charset::ISO8859_15), Charset::ISO8859_15.to_string()),
                    (Some(Charset::ISO8859_16), Charset::ISO8859_16.to_string()),
                    (Some(Charset::Windows1250), Charset::Windows1250.to_string()),
                    (Some(Charset::Windows1251), Charset::Windows1251.to_string()),
                    (Some(Charset::Windows1252), Charset::Windows1252.to_string()),
                    (Some(Charset::Windows1253), Charset::Windows1253.to_string()),
                    (Some(Charset::GBK), Charset::GBK.to_string()),
                    (Some(Charset::GB2312), Charset::GB2312.to_string()),
                    (Some(Charset::GB18030), Charset::GB18030.to_string()),
                    (Some(Charset::BIG5), Charset::BIG5.to_string()),
                    (Some(Charset::ISO2022JP), Charset::ISO2022JP.to_string()),
                    (Some(Charset::EUCJP), Charset::EUCJP.to_string()),
                    (Some(Charset::KOI8R), Charset::KOI8R.to_string()),
                    (Some(Charset::KOI8U), Charset::KOI8U.to_string()),
                ];
                self.force_charset = ForceCharset::Dialog(Box::new(Selector::new(
                    "select charset to force",
                    entries,
                    true,
                    Some(Box::new(
                        move |id: ComponentId, results: &[Option<Charset>]| {
                            Some(UIEvent::FinishedUIDialog(id, Box::new(results.to_vec())))
                        },
                    )),
                    context,
                )));
                self.initialised = false;
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.pager.is_dirty()
            || self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || matches!(self.force_charset, ForceCharset::Dialog(ref s) if s.is_dirty())
            || matches!(self.mode, ViewMode::ContactSelector(ref s) if s.is_dirty())
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        match self.mode {
            ViewMode::Normal | ViewMode::Url | ViewMode::Source(_) | ViewMode::Attachment(_) => {
                self.pager.set_dirty(value);
            }
            ViewMode::ContactSelector(ref mut s) => {
                self.pager.set_dirty(value);
                s.set_dirty(value);
            }
            ViewMode::Subview => {
                if let Some(s) = self.subview.as_mut() {
                    s.set_dirty(value);
                }
            }
        }
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if let Some(ref sbv) = self.subview {
            sbv.get_shortcuts(context)
        } else {
            self.pager.get_shortcuts(context)
        };

        let mut our_map = context.settings.shortcuts.envelope_view.key_values();

        if !(self.mode.is_attachment()
            /*|| self.mode.is_ansi()*/
            || self.mode == ViewMode::Subview
            || self.mode == ViewMode::Source(Source::Decoded)
            || self.mode == ViewMode::Source(Source::Raw)
            || self.mode == ViewMode::Url)
        {
            our_map.remove("return_to_normal_view");
        }
        if self.mode != ViewMode::Url {
            our_map.remove("go_to_url");
        }
        if !(self.mode == ViewMode::Normal || self.mode == ViewMode::Url) {
            our_map.remove("toggle_url_mode");
        }
        map.insert(Shortcuts::ENVELOPE_VIEW, our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        if self.id == id {
            context
                .replies
                .push_back(UIEvent::Action(Tab(Kill(self.id))));
        }
    }
}

fn save_attachment(path: &std::path::Path, bytes: &[u8]) -> Result<()> {
    let mut f = std::fs::File::create(path)?;
    let mut permissions = f.metadata()?.permissions();
    permissions.set_mode(0o600); // Read/write for owner only.
    f.set_permissions(permissions)?;
    f.write_all(bytes)?;
    f.flush()?;
    Ok(())
}

fn desktop_exec_to_command(command: &str, path: String, is_url: bool) -> String {
    /* Purge unused field codes */
    let command = command
        .replace("%i", "")
        .replace("%c", "")
        .replace("%k", "");
    if command.contains("%f") {
        command.replacen("%f", &path.replace(' ', "\\ "), 1)
    } else if command.contains("%F") {
        command.replacen("%F", &path.replace(' ', "\\ "), 1)
    } else if command.contains("%u") || command.contains("%U") {
        let from_pattern = if command.contains("%u") { "%u" } else { "%U" };
        if is_url {
            command.replacen(from_pattern, &path, 1)
        } else {
            command.replacen(
                from_pattern,
                &format!("file://{}", path).replace(' ', "\\ "),
                1,
            )
        }
    } else if is_url {
        format!("{} {}", command, path)
    } else {
        format!("{} {}", command, path.replace(' ', "\\ "))
    }
}

#[test]
fn test_desktop_exec() {
    assert_eq!(
        "ristretto /tmp/file".to_string(),
        desktop_exec_to_command("ristretto %F", "/tmp/file".to_string(), false)
    );
    assert_eq!(
        "/usr/lib/firefox-esr/firefox-esr file:///tmp/file".to_string(),
        desktop_exec_to_command(
            "/usr/lib/firefox-esr/firefox-esr %u",
            "/tmp/file".to_string(),
            false
        )
    );
    assert_eq!(
        "/usr/lib/firefox-esr/firefox-esr www.example.com".to_string(),
        desktop_exec_to_command(
            "/usr/lib/firefox-esr/firefox-esr %u",
            "www.example.com".to_string(),
            true
        )
    );
    assert_eq!(
        "/usr/bin/vlc --started-from-file www.example.com".to_string(),
        desktop_exec_to_command(
            "/usr/bin/vlc --started-from-file %U",
            "www.example.com".to_string(),
            true
        )
    );
    assert_eq!(
        "zathura --fork file:///tmp/file".to_string(),
        desktop_exec_to_command("zathura --fork %U", "file:///tmp/file".to_string(), true)
    );
}
