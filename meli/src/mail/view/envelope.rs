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
    collections::VecDeque,
    process::{Command, Stdio},
};

use melib::utils::{shellexpand::ShellExpandTrait, xdg::query_default_app};

use super::*;
#[cfg(feature = "gpgme")]
use crate::jobs::IsAsync;
use crate::ThreadEvent;

/// Envelope view, with sticky headers, a pager for the body, and
/// subviews for more menus.
///
/// Doesn't have a concept of accounts, mailboxes or mail backends.
/// Therefore all settings it needs need to be provided through the
/// `view_settings` field of type [`ViewSettings`].
#[derive(Debug)]
pub struct EnvelopeView {
    pub pager: Pager,
    pub subview: Option<Box<EnvelopeView>>,
    pub dirty: bool,
    pub initialised: bool,
    pub force_draw_headers: bool,
    pub options: ViewOptions,
    pub mail: Mail,
    pub body: Box<Attachment>,
    pub display: Vec<AttachmentDisplay>,
    pub body_text: String,
    pub html_filter: Option<Result<ViewFilter>>,
    pub filters: Vec<ViewFilter>,
    pub links: Vec<Link>,
    pub attachment_tree: String,
    pub attachment_paths: Vec<Vec<usize>>,
    pub headers_no: usize,
    pub headers_cursor: usize,
    pub force_charset: ForceCharset,
    pub view_settings: ViewSettings,
    pub cmd_buf: String,
    pub active_jobs: HashSet<JobId>,
    pub main_loop_handler: MainLoopHandler,
    pub id: ComponentId,
}

impl Clone for EnvelopeView {
    fn clone(&self) -> Self {
        Self::new(
            self.mail.clone(),
            Some(self.pager.clone()),
            None,
            Some(self.view_settings.clone()),
            self.main_loop_handler.clone(),
        )
    }
}

impl std::fmt::Display for EnvelopeView {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "view mail")
    }
}

impl EnvelopeView {
    pub fn new(
        mail: Mail,
        pager: Option<Pager>,
        subview: Option<Box<Self>>,
        view_settings: Option<ViewSettings>,
        main_loop_handler: MainLoopHandler,
    ) -> Self {
        let view_settings = view_settings.unwrap_or_default();
        let body = Box::new(AttachmentBuilder::new(&mail.bytes).build());
        let mut ret = Self {
            pager: pager.unwrap_or_default(),
            subview,
            dirty: true,
            initialised: false,
            force_draw_headers: false,
            options: ViewOptions::default(),
            force_charset: ForceCharset::None,
            attachment_tree: String::new(),
            attachment_paths: vec![],
            body,
            display: vec![],
            links: vec![],
            body_text: String::new(),
            html_filter: None,
            filters: vec![],
            view_settings,
            headers_no: 5,
            headers_cursor: 0,
            mail,
            main_loop_handler,
            active_jobs: HashSet::default(),
            cmd_buf: String::with_capacity(4),
            id: ComponentId::default(),
        };

        ret.parse_attachments();

        ret
    }

    fn attachment_to_display_helper(
        a: &Attachment,
        main_loop_handler: &MainLoopHandler,
        active_jobs: &mut HashSet<JobId>,
        acc: &mut Vec<AttachmentDisplay>,
        view_settings: &ViewSettings,
        force_charset: Option<Charset>,
    ) {
        if a.content_disposition.kind.is_attachment() || a.content_type == "message/rfc822" {
            acc.push(AttachmentDisplay::Attachment {
                inner: Box::new(a.clone()),
            });
        } else if a.content_type().is_text_html() {
            let bytes = a.decode(force_charset.into());
            let filter_invocation = view_settings
                .html_filter
                .as_deref()
                .unwrap_or("w3m -I utf-8 -T text/html");
            let command_obj = Command::new("sh")
                .args(["-c", filter_invocation])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .and_then(|mut cmd| {
                    cmd.stdin.as_mut().unwrap().write_all(&bytes)?;
                    Ok(String::from_utf8_lossy(&cmd.wait_with_output()?.stdout).to_string())
                });
            match command_obj {
                Err(err) => {
                    main_loop_handler.send(ThreadEvent::UIEvent(UIEvent::Notification {
                        title: Some(
                            format!("Failed to start html filter process: {}", filter_invocation,)
                                .into(),
                        ),
                        body: err.to_string().into(),
                        source: Some(err.into()),
                        kind: Some(NotificationType::Error(melib::ErrorKind::External)),
                    }));
                    // [ref:FIXME]: add `v` configurable shortcut
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
                Ok(text) => {
                    // [ref:FIXME]: add `v` configurable shortcut
                    let comment = Some(format!(
                        "Text piped through `{}`. Press `v` to open in web browser. \n\n",
                        filter_invocation
                    ));
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
                            && view_settings.auto_choose_multipart_alternative
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
                        Self::attachment_to_display_helper(
                            a,
                            main_loop_handler,
                            active_jobs,
                            &mut display,
                            view_settings,
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
                                Self::attachment_to_display_helper(
                                    &parts[0],
                                    main_loop_handler,
                                    active_jobs,
                                    &mut v,
                                    view_settings,
                                    force_charset,
                                );
                                v
                            },
                        });
                    }
                    #[cfg(feature = "gpgme")]
                    {
                        if view_settings.auto_verify_signatures.is_true() {
                            let verify_fut = crate::mail::pgp::verify(a.clone());
                            let handle = main_loop_handler.job_executor.spawn(
                                "gpg::verify".into(),
                                verify_fut,
                                IsAsync::Blocking,
                            );
                            active_jobs.insert(handle.job_id);
                            main_loop_handler.send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                                StatusEvent::NewJob(handle.job_id),
                            )));
                            acc.push(AttachmentDisplay::SignedPending {
                                inner: Box::new(a.clone()),
                                job_id: handle.job_id,
                                display: {
                                    let mut v = vec![];
                                    Self::attachment_to_display_helper(
                                        &parts[0],
                                        main_loop_handler,
                                        active_jobs,
                                        &mut v,
                                        view_settings,
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
                                    Self::attachment_to_display_helper(
                                        &parts[0],
                                        main_loop_handler,
                                        active_jobs,
                                        &mut v,
                                        view_settings,
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
                                if view_settings.auto_decrypt.is_true() {
                                    let decrypt_fut = crate::mail::pgp::decrypt(a.raw().to_vec());
                                    let handle = main_loop_handler.job_executor.spawn(
                                        "gpg::decrypt".into(),
                                        decrypt_fut,
                                        IsAsync::Blocking,
                                    );
                                    active_jobs.insert(handle.job_id);
                                    main_loop_handler.send(ThreadEvent::UIEvent(
                                        UIEvent::StatusEvent(StatusEvent::NewJob(handle.job_id)),
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
                        Self::attachment_to_display_helper(
                            a,
                            main_loop_handler,
                            active_jobs,
                            acc,
                            view_settings,
                            force_charset,
                        );
                    }
                }
            }
        }
    }

    pub fn parse_attachments(&mut self) {
        let mut display = vec![];
        Self::attachment_to_display_helper(
            &self.body,
            &self.main_loop_handler,
            &mut self.active_jobs,
            &mut display,
            &self.view_settings,
            (&self.force_charset).into(),
        );
        let (attachment_paths, attachment_tree) = self.attachment_displays_to_tree(&display);
        self.display = display;
        self.attachment_tree = attachment_tree;
        self.attachment_paths = attachment_paths;
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

    fn open_attachment(
        &'_ self,
        lidx: usize,
        context: &mut Context,
    ) -> Option<&'_ melib::Attachment> {
        if let Some(path) = self.attachment_paths.get(lidx).and_then(|path| {
            if !path.is_empty() {
                Some(path)
            } else {
                None
            }
        }) {
            let first = path[0];
            use AttachmentDisplay::*;
            let root_attachment = match &self.display[first] {
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

    pub fn body_text(&self) -> &str {
        &self.body_text
    }
}

impl Component for EnvelopeView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        self.view_settings.theme_default = crate::conf::value(context, "theme_default");

        let hdr_theme = crate::conf::value(context, "mail.view.headers");
        let hdr_name_theme = crate::conf::value(context, "mail.view.headers_names");
        let hdr_area_theme = crate::conf::value(context, "mail.view.headers_area");

        let y: usize = {
            if self.options.contains(ViewOptions::SOURCE) {
                grid.clear_area(area, self.view_settings.theme_default);
                context.dirty_areas.push_back(area);
                0
            } else {
                let envelope = &self.mail;
                let height_p = self.pager.size().1;

                let height = area
                    .height()
                    .saturating_sub(self.headers_no)
                    .saturating_sub(1);

                self.headers_no = 0;
                let mut skip_header_ctr = self.headers_cursor;
                let sticky = self.view_settings.sticky_headers || height_p < height;
                let mut y = 0;
                macro_rules! print_header {
                    ($(($header:path, $string:expr)),*$(,)?) => {
                        $({
                            if sticky || skip_header_ctr == 0 {
                                if y <= area.height() {
                                    grid.clear_area(
                                        area.skip_rows(y).take_rows(1),
                                        hdr_area_theme,
                                    );
                                    let (_x, _y) =
                                        grid.write_string(
                                            &format!("{}:", $header),
                                            hdr_name_theme.fg,
                                            hdr_name_theme.bg,
                                            hdr_name_theme.attrs,
                                            area.skip_rows(y),
                                            None,
                                            Some(0)
                                        );
                                    let (__x, mut __y) =
                                        grid.write_string(
                                            &$string,
                                            hdr_theme.fg,
                                            hdr_theme.bg,
                                            hdr_theme.attrs,
                                            area.skip_rows(y + _y),
                                            Some(_x + 1),
                                            Some(2)
                                        );
                                    if __y > 0 {
                                        if __y > 3 && !self.view_settings.expand_headers {
                                            __y = 3;
                                        }
                                        grid.clear_area(
                                            area.skip_rows(y + _y + 1).take_rows(__y).take_cols(2),
                                            hdr_area_theme,
                                        );
                                    }
                                    y += _y +__y + 1;
                                }
                            } else {
                                skip_header_ctr = skip_header_ctr.saturating_sub(1);
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
                        if let (Some(hr_offset), Some(min_offset)) = (
                            offset.get(1..3).and_then(|slice| slice.parse::<i64>().ok()),
                            offset.get(3..5).and_then(|slice| slice.parse::<i64>().ok()),
                        ) {
                            diff.1 .0 = hr_offset;
                            diff.1 .1 = min_offset;
                        }
                    }
                    diff
                };
                let orig_date = envelope.date_as_str();
                let date_str: std::borrow::Cow<str> = if self.view_settings.show_date_in_my_timezone
                {
                    let local_date = datetime::timestamp_to_string(
                        envelope.timestamp,
                        Some(datetime::formats::RFC822_DATE),
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
                    (HeaderName::DATE, date_str),
                    (HeaderName::FROM, envelope.field_from_to_string()),
                    (HeaderName::TO, envelope.field_to_to_string()),
                );
                if envelope.other_headers().contains_key(HeaderName::CC)
                    && !envelope.other_headers()[HeaderName::CC].is_empty()
                {
                    print_header!((HeaderName::CC, envelope.field_cc_to_string()));
                }
                print_header!(
                    (HeaderName::SUBJECT, envelope.subject()),
                    (
                        HeaderName::MESSAGE_ID,
                        envelope.message_id().display_brackets().to_string()
                    )
                );
                if self.view_settings.expand_headers {
                    if let Some(val) = envelope.in_reply_to() {
                        print_header!(
                            (
                                HeaderName::IN_REPLY_TO,
                                melib::MessageID::display_slice(val.refs(), Some(" "))
                            ),
                            (
                                HeaderName::REFERENCES,
                                melib::MessageID::display_slice(envelope.references(), Some(" "))
                            )
                        );
                    }
                }
                for hdr in &self.view_settings.show_extra_headers {
                    if let Some((val, hdr)) = HeaderName::try_from(hdr)
                        .ok()
                        .and_then(|hdr| Some((envelope.other_headers().get(&hdr)?, hdr)))
                    {
                        print_header!((hdr, val));
                    }
                }
                if let Some(list_management::ListActions {
                    ref id,
                    ref archive,
                    ref post,
                    ref unsubscribe,
                }) = list_management::ListActions::detect(envelope)
                {
                    let mut x = 0;
                    if let Some(id) = id {
                        if sticky || skip_header_ctr == 0 {
                            grid.clear_area(area.nth_row(y), hdr_area_theme);
                            let (_x, _y) = grid.write_string(
                                "List-ID: ",
                                hdr_name_theme.fg,
                                hdr_name_theme.bg,
                                hdr_name_theme.attrs,
                                area.nth_row(y),
                                None,
                                Some(0),
                            );
                            if _y != 0 {
                                x = _x;
                            } else {
                                x += _x;
                            }
                            y += _y;
                            let (_x, _y) = grid.write_string(
                                id,
                                hdr_theme.fg,
                                hdr_theme.bg,
                                hdr_theme.attrs,
                                area.nth_row(y),
                                Some(x),
                                Some(0),
                            );
                            if _y != 0 {
                                x = _x;
                            } else {
                                x += _x;
                            }
                            y += _y;
                        }
                        self.headers_no += 1;
                    }
                    if sticky || skip_header_ctr == 0 {
                        if archive.is_some() || post.is_some() || unsubscribe.is_some() {
                            let (_x, _y) = grid.write_string(
                                " Available actions: [ ",
                                hdr_name_theme.fg,
                                hdr_name_theme.bg,
                                hdr_name_theme.attrs,
                                area.skip(0, y),
                                Some(x),
                                Some(0),
                            );
                            if _y != 0 {
                                x = _x;
                            } else {
                                x += _x;
                            }
                            y += _y;
                        }
                        if archive.is_some() {
                            let (_x, _y) = grid.write_string(
                                "list-archive, ",
                                hdr_theme.fg,
                                hdr_theme.bg,
                                hdr_theme.attrs,
                                area.skip(0, y),
                                Some(x),
                                Some(0),
                            );
                            if _y != 0 {
                                x = _x;
                            } else {
                                x += _x;
                            }
                            y += _y;
                        }
                        if post.is_some() {
                            let (_x, _y) = grid.write_string(
                                "list-post, ",
                                hdr_theme.fg,
                                hdr_theme.bg,
                                hdr_theme.attrs,
                                area.skip(0, y),
                                Some(x),
                                Some(0),
                            );
                            if _y != 0 {
                                x = _x;
                            } else {
                                x += _x;
                            }
                            y += _y;
                        }
                        if unsubscribe.is_some() {
                            let (_x, _y) = grid.write_string(
                                "list-unsubscribe, ",
                                hdr_theme.fg,
                                hdr_theme.bg,
                                hdr_theme.attrs,
                                area.skip(0, y),
                                Some(x),
                                Some(0),
                            );
                            if _y != 0 {
                                x = _x;
                            } else {
                                x += _x;
                            }
                            y += _y;
                        }
                        if archive.is_some() || post.is_some() || unsubscribe.is_some() {
                            if x >= 2 {
                                for c in grid.row_iter(area, (x - 2)..(x - 1), y) {
                                    grid[c].set_ch(' ');
                                }
                            }
                            if x > 0 {
                                for c in grid.row_iter(area, (x - 1)..x, y) {
                                    grid[c]
                                        .set_ch(']')
                                        .set_fg(hdr_name_theme.fg)
                                        .set_bg(hdr_name_theme.bg)
                                        .set_attrs(hdr_name_theme.attrs);
                                }
                            }
                        }
                        y += 1;
                    }
                }

                self.force_draw_headers = false;
                grid.clear_area(area.skip_rows(y), self.view_settings.theme_default);
                context.dirty_areas.push_back(area.take_rows(y + 3));
                if !self.view_settings.sticky_headers {
                    let height_p = self.pager.size().1;

                    let height = area.height().saturating_sub(y).saturating_sub(1);
                    if self.pager.cursor_pos() >= self.headers_no {
                        0
                    } else if (height_p > height && self.headers_cursor < self.headers_no + 1)
                        || self.headers_cursor == 0
                        || height_p < height
                    {
                        y + 1
                    } else {
                        0
                    }
                } else {
                    y + 1
                }
            }
        };

        if self.filters.is_empty() {
            let body = self.mail.body();
            if body.is_html() {
                let attachment = if let Some(sub) = match body.content_type {
                    ContentType::Multipart {
                        kind: MultipartType::Alternative,
                        ref parts,
                        ..
                    } => parts.iter().find(|p| p.is_html()),
                    _ => None,
                } {
                    sub
                } else {
                    &body
                };
                if let Ok(filter) = ViewFilter::new_html(attachment, &self.view_settings, context) {
                    self.filters.push(filter);
                }
            } else if self.view_settings.auto_choose_multipart_alternative
                && match body.content_type {
                    ContentType::Multipart {
                        kind: MultipartType::Alternative,
                        ref parts,
                        ..
                    } => parts
                        .iter()
                        .all(|p| p.is_html() || (p.is_text() && p.body().trim().is_empty())),
                    _ => false,
                }
            {
                if let Ok(filter) = ViewFilter::new_html(
                    body.content_type
                        .parts()
                        .unwrap()
                        .iter()
                        .find(|a| a.is_html())
                        .unwrap_or(&body),
                    &self.view_settings,
                    context,
                ) {
                    self.filters.push(filter);
                } else if let Ok(filter) =
                    ViewFilter::new_attachment(&body, &self.view_settings, context)
                {
                    self.filters.push(filter);
                }
            } else if let Ok(filter) =
                ViewFilter::new_attachment(&body, &self.view_settings, context)
            {
                self.filters.push(filter);
            }
        }
        if !self.initialised {
            self.initialised = true;
            let mut text = if !self.filters.is_empty() {
                let mut text = String::new();
                self.body_text.clear();
                if let Some(last) = self.filters.last() {
                    let mut stack = vec![last];
                    while let Some(ViewFilter {
                        filter_invocation,
                        body_text,
                        notice,
                        ..
                    }) = stack.pop()
                    {
                        text.push_str(
                            &notice
                                .as_ref()
                                .map(|s| s.to_string())
                                .or_else(|| {
                                    if filter_invocation.is_empty() {
                                        None
                                    } else {
                                        Some(format!("Text filtered by `{filter_invocation}`"))
                                    }
                                })
                                .unwrap_or_default(),
                        );
                        if !text.is_empty() {
                            text.push('\n');
                        }
                        if !self.body_text.is_empty() {
                            self.body_text.push('\n');
                        }
                        match body_text {
                            ViewFilterContent::Filtered { inner } => {
                                let payload =
                                    self.options.convert(&mut self.links, &self.body, inner);
                                text.push_str(&payload);
                                self.body_text.push_str(&payload);
                            }
                            ViewFilterContent::Error { inner } => text.push_str(&inner.to_string()),
                            ViewFilterContent::Running { .. } => {
                                text.push_str("Filter job running in background.")
                            }
                            ViewFilterContent::InlineAttachments { parts } => {
                                stack.extend(parts.iter().rev());
                            }
                        }
                    }
                }
                text
            } else {
                self.options
                    .convert(&mut self.links, &self.body, &self.body_text)
            };
            if !text.trim().is_empty() {
                text.push_str("\n\n");
            }
            text.push_str(&self.attachment_tree);
            while text.ends_with('\n') {
                text.pop();
            }
            let cursor_pos = self.pager.cursor_pos();
            self.view_settings.body_theme = crate::conf::value(context, "mail.view.body");
            self.pager = Pager::from_string(
                text,
                context,
                Some(cursor_pos),
                None,
                self.view_settings.body_theme,
            );
            if let Some(ref filter) = self.view_settings.pager_filter {
                self.pager.filter(filter, context);
            }
        }

        if let Some(s) = self.subview.as_mut() {
            if !s.is_dirty() {
                s.set_dirty(true);
            }
            s.draw(grid, area.skip_rows(y), context);
        } else {
            self.pager.draw(grid, area.skip_rows(y), context);
        }
        if let ForceCharset::Dialog(ref mut s) = self.force_charset {
            s.draw(grid, area, context);
        }

        // Draw number command buffer at the bottom right corner:

        let l = area.nth_row(area.height());
        if self.cmd_buf.is_empty() {
            grid.clear_area(l.skip_cols_from_end(8), self.view_settings.theme_default);
        } else {
            let s = self.cmd_buf.to_string();
            grid.write_string(
                &s,
                self.view_settings.theme_default.fg,
                self.view_settings.theme_default.bg,
                self.view_settings.theme_default.attrs,
                l.skip_cols_from_end(8),
                None,
                None,
            );
        }

        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if matches!(event, UIEvent::StatusEvent(StatusEvent::JobFinished(_))) {
            match *event {
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                    if self.active_jobs.contains(job_id) =>
                {
                    let mut caught = false;
                    for d in self.display.iter_mut() {
                        let succeeded: bool;
                        match d {
                            AttachmentDisplay::SignedPending {
                                ref mut inner,
                                handle,
                                display,
                                job_id: our_job_id,
                            } if *our_job_id == *job_id => {
                                caught = true;
                                match handle.chan.try_recv() {
                                    Err(_) => {
                                        /* Job was canceled */
                                        succeeded = false;
                                        log::warn!("Could not verify signature: Job was canceled",);
                                    }
                                    Ok(None) => {
                                        /* something happened,
                                         * perhaps a worker thread
                                         * panicked */
                                        succeeded = false;
                                        log::warn!(
                                            "Could not verify signature: check logs for any errors",
                                        );
                                    }
                                    Ok(Some(Ok(()))) => {
                                        succeeded = true;
                                        *d = AttachmentDisplay::SignedVerified {
                                            inner: std::mem::replace(
                                                inner,
                                                Box::new(AttachmentBuilder::new(&[]).build()),
                                            ),
                                            display: std::mem::take(display),
                                            description: String::new(),
                                        };
                                    }
                                    Ok(Some(Err(error))) => {
                                        succeeded = false;
                                        log::error!("Could not verify signature: {}", error);
                                        *d = AttachmentDisplay::SignedFailed {
                                            inner: std::mem::replace(
                                                inner,
                                                Box::new(AttachmentBuilder::new(&[]).build()),
                                            ),
                                            display: std::mem::take(display),
                                            error,
                                        };
                                    }
                                }
                            }
                            AttachmentDisplay::EncryptedPending {
                                ref mut inner,
                                handle,
                            } if handle.job_id == *job_id => {
                                caught = true;
                                match handle.chan.try_recv() {
                                    Err(_) => {
                                        /* Job was canceled */

                                        succeeded = false;
                                        log::warn!(
                                            "Could not decrypt encrypted message: Job was canceled",
                                        );
                                    }
                                    Ok(None) => {
                                        /* something happened,
                                         * perhaps a worker thread
                                         * panicked */
                                        succeeded = false;
                                        log::warn!(
                                            "Could not decrypt encrypted message: check logs for \
                                             any errors",
                                        );
                                    }
                                    Ok(Some(Ok((metadata, decrypted_bytes)))) => {
                                        succeeded = true;
                                        let plaintext = Box::new(
                                            AttachmentBuilder::new(&decrypted_bytes).build(),
                                        );
                                        let mut plaintext_display = vec![];
                                        Self::attachment_to_display_helper(
                                            &plaintext,
                                            &self.main_loop_handler,
                                            &mut self.active_jobs,
                                            &mut plaintext_display,
                                            &self.view_settings,
                                            (&self.force_charset).into(),
                                        );
                                        *d = AttachmentDisplay::EncryptedSuccess {
                                            inner: std::mem::replace(
                                                inner,
                                                Box::new(AttachmentBuilder::new(&[]).build()),
                                            ),
                                            plaintext,
                                            plaintext_display,
                                            description: format!("{:?}", metadata),
                                        };
                                    }
                                    Ok(Some(Err(error))) => {
                                        succeeded = false;
                                        log::error!(
                                            "Could not decrypt encrypted message: {}",
                                            error
                                        );
                                        *d = AttachmentDisplay::EncryptedFailed {
                                            inner: std::mem::replace(
                                                inner,
                                                Box::new(AttachmentBuilder::new(&[]).build()),
                                            ),
                                            error,
                                        };
                                    }
                                }
                            }
                            _ => continue,
                        }
                        context
                            .main_loop_handler
                            .job_executor
                            .set_job_success(*job_id, succeeded);
                    }
                    if caught {
                        self.links.clear();
                        self.initialised = false;
                        self.set_dirty(true);
                    }

                    self.active_jobs.remove(job_id);
                }
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                    if self.filters.iter().any(|f| f.contains_job_id(*job_id)) =>
                {
                    let mut stack = self.filters.iter_mut().collect::<VecDeque<&mut _>>();
                    while let Some(filter) = stack.pop_front() {
                        if let Some(cb) = filter.event_handler {
                            if cb(
                                filter,
                                &mut UIEvent::StatusEvent(StatusEvent::JobFinished(*job_id)),
                                context,
                            ) {
                                break;
                            }
                        }
                        if let ViewFilterContent::InlineAttachments { ref mut parts, .. } =
                            filter.body_text
                        {
                            stack.extend(parts.iter_mut());
                        }
                    }
                    self.links.clear();
                    self.initialised = false;
                    self.set_dirty(true);
                }
                _ => {}
            }
        }
        match (&mut self.force_charset, &event) {
            (ForceCharset::Dialog(selector), UIEvent::FinishedUIDialog(id, results))
                if *id == selector.id() =>
            {
                if let Some(results) = results.downcast_ref::<Vec<Option<Charset>>>() {
                    if results.len() != 1 {
                        self.force_charset = ForceCharset::None;
                        self.set_dirty(true);
                        return true;
                    }
                    if let Some(charset) = results[0] {
                        self.force_charset = ForceCharset::Forced(charset);
                    } else {
                        self.force_charset = ForceCharset::None;
                    }
                } else {
                    self.force_charset = ForceCharset::None;
                }
                self.set_dirty(true);
                return true;
            }
            (ForceCharset::Dialog(selector), UIEvent::ComponentUnrealize(id))
                if *id == selector.id() =>
            {
                self.force_charset = ForceCharset::None;
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

        let shortcuts = &self.shortcuts(context);

        if let Some(ref mut sub) = self.subview {
            if matches!(event, UIEvent::Input(ref key) if shortcut!(
                key == shortcuts[Shortcuts::ENVELOPE_VIEW]["return_to_normal_view"]
            )) {
                if sub.process_event(event, context) && !sub.filters.is_empty() {
                    return true;
                }
            } else if sub.process_event(event, context) {
                return true;
            }
        } else {
            if !self.view_settings.sticky_headers {
                let shortcuts = self.pager.shortcuts(context);
                match event {
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::PAGER]["scroll_up"])
                            && self.pager.cursor_pos() == 0
                            && self.headers_cursor > 0 =>
                    {
                        self.headers_cursor -= 1;
                        self.set_dirty(true);
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::PAGER]["scroll_down"])
                            && self.headers_cursor < self.headers_no =>
                    {
                        self.headers_cursor += 1;
                        self.set_dirty(true);
                        return true;
                    }

                    _ => {}
                }
            }

            if self.pager.process_event(event, context)
                || self
                    .filters
                    .last_mut()
                    .map(|f| f.process_event(event, context))
                    .unwrap_or(false)
            {
                return true;
            }
        }

        match *event {
            UIEvent::Resize | UIEvent::VisibilityChange(true) => {
                self.set_dirty(true);
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Char('\x1B'))
                if !self.cmd_buf.is_empty() =>
            {
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c.is_ascii_digit() => {
                if self.cmd_buf.len() < 9 {
                    self.cmd_buf.push(c);
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["view_raw_source"]) =>
            {
                if self.options.contains(ViewOptions::SOURCE) {
                    self.options.toggle(ViewOptions::SOURCE_RAW);
                } else {
                    self.options.toggle(ViewOptions::SOURCE);
                }
                self.set_dirty(true);
                self.initialised = false;
                return true;
            }
            UIEvent::Input(ref key)
                if self.options != ViewOptions::DEFAULT
                    && shortcut!(
                        key == shortcuts[Shortcuts::ENVELOPE_VIEW]["return_to_normal_view"]
                    ) =>
            {
                self.options.remove(ViewOptions::SOURCE | ViewOptions::URL);
                self.set_dirty(true);
                self.initialised = false;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(
                    key == shortcuts[Shortcuts::ENVELOPE_VIEW]["return_to_normal_view"]
                ) =>
            {
                if self.subview.take().is_some() {
                    self.initialised = false;
                } else if self.filters.is_empty() {
                    return false;
                } else {
                    self.filters.pop();
                    self.initialised = false;
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if !self.cmd_buf.is_empty()
                    && shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["open_mailcap"]) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                if let Some(attachment) = self.open_attachment(lidx, context) {
                    if crate::mailcap::MailcapEntry::execute(attachment, context).is_ok() {
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
                return true;
            }
            UIEvent::Action(View(ViewAction::ExportMail(ref path))) => {
                // Save entire message as eml
                let mut path = std::path::Path::new(path).to_path_buf().expand();

                if path.is_dir() {
                    path.push(format!("{}.eml", self.mail.message_id()));
                }
                if path.is_relative() {
                    path = context.current_dir().join(&path);
                }
                match save_attachment(&path, &self.mail.bytes) {
                    Err(err) => {
                        log::error!("Failed to create file at {}: {err}", path.display());
                        context.replies.push_back(UIEvent::Notification {
                            title: Some(
                                format!("Failed to create file at {}", path.display()).into(),
                            ),
                            body: err.to_string().into(),
                            source: Some(err),
                            kind: Some(NotificationType::Error(melib::ErrorKind::External)),
                        });
                        return true;
                    }
                    Ok(()) => {
                        context.replies.push_back(UIEvent::Notification {
                            title: None,
                            source: None,
                            body: format!("Saved at {}", &path.display()).into(),
                            kind: Some(NotificationType::Info),
                        });
                    }
                }

                return true;
            }
            UIEvent::Action(View(ViewAction::SaveAttachment(a_i, ref path))) => {
                let mut path = std::path::Path::new(path).to_path_buf().expand();

                if let Some(u) = self.open_attachment(a_i, context) {
                    if path.is_dir() {
                        if let Some(filename) = u.filename() {
                            path.push(filename);
                        } else {
                            path.push(format!(
                                "meli_attachment_{a_i}_{}",
                                Uuid::new_v4().as_simple()
                            ));
                        }
                    }
                    if path.is_relative() {
                        path = context.current_dir().join(&path);
                    }
                    match save_attachment(&path, &u.decode(Default::default())) {
                        Err(err) => {
                            log::error!("Failed to create file at {}: {err}", path.display());
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(
                                    format!("Failed to create file at {}", path.display()).into(),
                                ),
                                body: err.to_string().into(),
                                source: Some(err),
                                kind: Some(NotificationType::Error(melib::ErrorKind::External)),
                            });
                        }
                        Ok(()) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: None,
                                source: None,
                                body: format!("Saved at {}", path.display()).into(),
                                kind: Some(NotificationType::Info),
                            });
                        }
                    }
                } else if a_i == 0 {
                    // Save entire message as eml
                    if path.is_dir() {
                        path.push(format!("{}.eml", self.mail.message_id()));
                    }
                    if path.is_relative() {
                        path = context.current_dir().join(&path);
                    }
                    match save_attachment(&path, &self.mail.bytes) {
                        Err(err) => {
                            log::error!("Failed to create file at {}: {err}", path.display());
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(
                                    format!("Failed to create file at {}", path.display()).into(),
                                ),
                                body: err.to_string().into(),
                                source: Some(err),
                                kind: Some(NotificationType::Error(melib::ErrorKind::External)),
                            });
                            return true;
                        }
                        Ok(()) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: None,
                                source: None,
                                body: format!("Saved at {}", &path.display()).into(),
                                kind: Some(NotificationType::Info),
                            });
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
            UIEvent::Action(View(ViewAction::PipeAttachment(a_i, ref bin, ref args))) => {
                use std::borrow::Cow;

                let bytes =
                    if let Some(u) = self.open_attachment(a_i, context) {
                        Cow::Owned(u.decode(Default::default()))
                    } else if a_i == 0 {
                        Cow::Borrowed(&self.mail.bytes)
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Attachment `{}` not found.", a_i)),
                        ));
                        return true;
                    };
                // Kill input thread so that spawned command can be sole receiver of stdin
                {
                    context.input_kill();
                }
                let pipe_command = format!("{} {}", bin, args.as_slice().join(" "));
                log::trace!("Executing: {}", &pipe_command);
                match Command::new(bin)
                    .args(args)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::inherit())
                    .stderr(Stdio::inherit())
                    .spawn()
                    .map_err(Error::from)
                    .and_then(|mut child| {
                        let Some(mut stdin) = child.stdin.take() else {
                            let _ = child.wait();
                            return Err(Error::new(format!(
                                "Could not open standard input of {bin}"
                            ))
                            .set_kind(ErrorKind::External));
                        };
                        stdin.write_all(&bytes).chain_err_summary(|| {
                            format!("Could not write to standard input of {bin}")
                        })?;

                        Ok(child)
                    }) {
                    Ok(mut child) => {
                        let _ = child.wait();
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification {
                            title: Some(
                                format!("Failed to execute {}: {}", pipe_command, err).into(),
                            ),
                            source: None,
                            body: err.to_string().into(),
                            kind: Some(NotificationType::Error(melib::error::ErrorKind::External)),
                        });
                        context
                            .replies
                            .push_back(UIEvent::Fork(ForkedProcess::Finished));
                        context.restore_input();
                        self.set_dirty(true);
                        return true;
                    }
                }
                context
                    .replies
                    .push_back(UIEvent::Fork(ForkedProcess::Finished));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["open_attachment"])
                    && !self.cmd_buf.is_empty() =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                if let Some(attachment) = self.open_attachment(lidx, context) {
                    match attachment.content_type() {
                        ContentType::MessageRfc822 => {
                            match Mail::new(attachment.body().to_vec(), Some(Flag::SEEN)) {
                                Ok(wrapper) => {
                                    self.subview = Some(Box::new(Self::new(
                                        wrapper,
                                        None,
                                        None,
                                        Some(self.view_settings.clone()),
                                        context.main_loop_handler.clone(),
                                    )));
                                    self.set_dirty(true);
                                }
                                Err(e) => {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!("{}", e)),
                                    ));
                                }
                            }
                        }
                        ContentType::Multipart { .. }
                        | ContentType::Text { .. }
                        | ContentType::PGPSignature
                        | ContentType::CMSSignature => {
                            if let Ok(filter) =
                                ViewFilter::new_attachment(attachment, &self.view_settings, context)
                            {
                                self.filters.push(filter);
                            }
                            self.initialised = false;
                            self.set_dirty(true);
                        }
                        ContentType::Other { .. } => {
                            let attachment_type = attachment.mime_type();
                            let filename = attachment.filename();
                            if let Ok(command) = query_default_app(&attachment_type) {
                                let res = File::create_temp_file(
                                    &attachment.decode(Default::default()),
                                    filename.as_deref(),
                                    None,
                                    None,
                                    true,
                                )
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
                                                "Failed to execute command: {err}"
                                            )),
                                        ));
                                    }
                                }
                            } else {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        if let Some(filename) = filename.as_ref() {
                                            format!(
                                                "Couldn't find a default application for file {} \
                                                 (type {})",
                                                filename, attachment_type
                                            )
                                        } else {
                                            format!(
                                                "Couldn't find a default application for type {}",
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
                                    "Failed to open {}. application/octet-stream is a stream of \
                                     bytes of unknown type. Try saving it as a file and opening \
                                     it manually.",
                                    name.as_ref().map(|n| n.as_str()).unwrap_or("file")
                                )),
                            ));
                        }
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(
                    key == shortcuts[Shortcuts::ENVELOPE_VIEW]["toggle_expand_headers"]
                ) =>
            {
                self.view_settings.expand_headers = !self.view_settings.expand_headers;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if !self.cmd_buf.is_empty()
                    && self.options.contains(ViewOptions::URL)
                    && shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["go_to_url"]) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                let links = &self.links;
                let (_kind, url) = {
                    if let Some(l) = links.get(lidx).map(|l| (l.kind, l.value.as_str())) {
                        l
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Link `{}` not found.", lidx)),
                        ));
                        return true;
                    }
                };

                let url_launcher = self.view_settings.url_launcher.as_deref().unwrap_or(
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
                        context
                            .children
                            .entry(url_launcher.to_string().into())
                            .or_default()
                            .push(child);
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification {
                            title: Some(format!("Failed to launch {:?}", url_launcher).into()),
                            body: err.to_string().into(),
                            source: Some(err.into()),
                            kind: Some(NotificationType::Error(melib::ErrorKind::External)),
                        });
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["toggle_url_mode"]) =>
            {
                self.options.toggle(ViewOptions::URL);
                self.initialised = false;
                self.dirty = true;
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
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        false
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if let Some(ref sbv) = self.subview {
            sbv.shortcuts(context)
        } else {
            self.pager.shortcuts(context)
        };

        let mut our_map = self.view_settings.env_view_shortcuts.clone();

        //if !self
        //    .options
        //    .contains(ViewOptions::SOURCE | ViewOptions::URL)
        //    || self.filters.is_empty()
        //{
        //    our_map.remove("return_to_normal_view");
        //}
        if !self.options.contains(ViewOptions::URL) {
            our_map.shift_remove("go_to_url");
        }
        map.insert(Shortcuts::ENVELOPE_VIEW, our_map);

        map
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.pager.is_dirty()
            || self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || matches!(self.force_charset, ForceCharset::Dialog(ref s) if s.is_dirty())
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.pager.set_dirty(value);
        if let Some(ref mut s) = self.subview {
            s.set_dirty(value);
        }
        if let ForceCharset::Dialog(ref mut s) = self.force_charset {
            s.set_dirty(value);
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        debug_assert!(self.id == id);
        context
            .replies
            .push_back(UIEvent::Action(Tab(Kill(self.id))));
    }
}
