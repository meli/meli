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

use super::*;
use crate::conf::accounts::JobRequest;
use crate::jobs::{oneshot, JobId};
use melib::list_management;
use melib::parser::BytesExt;
use smallvec::SmallVec;
use std::collections::HashSet;
use std::io::Write;

use std::convert::TryFrom;
use std::process::{Command, Stdio};

mod html;
pub use self::html::*;
mod thread;
pub use self::thread::*;

mod envelope;
pub use self::envelope::*;

use linkify::{Link, LinkFinder};
use xdg_utils::query_default_app;

#[derive(PartialEq, Copy, Clone, Debug)]
enum Source {
    Decoded,
    Raw,
}

#[derive(PartialEq, Debug)]
enum ViewMode {
    Normal,
    Url,
    Attachment(usize),
    Source(Source),
    Ansi(RawBuffer),
    Subview,
    ContactSelector(UIDialog<Card>),
}

impl Default for ViewMode {
    fn default() -> Self {
        ViewMode::Normal
    }
}

impl ViewMode {
    fn is_ansi(&self) -> bool {
        match self {
            ViewMode::Ansi(_) => true,
            _ => false,
        }
    }
    fn is_attachment(&self) -> bool {
        match self {
            ViewMode::Attachment(_) => true,
            _ => false,
        }
    }
    fn is_contact_selector(&self) -> bool {
        match self {
            ViewMode::ContactSelector(_) => true,
            _ => false,
        }
    }
}

/// Contains an Envelope view, with sticky headers, a pager for the body, and subviews for more
/// menus
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
    headers_no: usize,
    headers_cursor: usize,
    force_draw_headers: bool,
    theme_default: ThemeAttribute,
    active_jobs: HashSet<JobId>,
    state: MailViewState,

    cmd_buf: String,
    id: ComponentId,
}

#[derive(Debug)]
pub enum PendingReplyAction {
    Reply,
    ReplyToAuthor,
    ReplyToAll,
}

#[derive(Debug)]
pub enum MailViewState {
    Init {
        pending_action: Option<PendingReplyAction>,
    },
    LoadingBody {
        job_id: JobId,
        chan: oneshot::Receiver<Result<Vec<u8>>>,
        pending_action: Option<PendingReplyAction>,
    },
    Loaded {
        body: Result<Vec<u8>>,
    },
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
            state: MailViewState::default(),
            active_jobs: self.active_jobs.clone(),
            ..*self
        }
    }
}

impl fmt::Display for MailView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", MailView::DESCRIPTION)
    }
}

impl MailView {
    const DESCRIPTION: &'static str = "view mail";
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

            headers_no: 5,
            headers_cursor: 0,
            force_draw_headers: false,

            theme_default: crate::conf::value(context, "mail.view.body"),
            active_jobs: Default::default(),
            state: MailViewState::default(),

            cmd_buf: String::with_capacity(4),
            id: ComponentId::new_v4(),
        };

        ret.init_futures(context);
        ret
    }

    fn init_futures(&mut self, context: &mut Context) {
        debug!("init_futures");
        let mut pending_action = None;
        let account = &mut context.accounts[&self.coordinates.0];
        if debug!(account.contains_key(self.coordinates.2)) {
            {
                match account
                    .operation(self.coordinates.2)
                    .and_then(|mut op| op.as_bytes())
                {
                    Ok(fut) => {
                        let (mut chan, handle, job_id) =
                            account.job_executor.spawn_specialized(fut);
                        pending_action = if let MailViewState::Init {
                            ref mut pending_action,
                        } = self.state
                        {
                            pending_action.take()
                        } else {
                            None
                        };
                        if let Ok(Some(bytes_result)) = try_recv_timeout!(&mut chan) {
                            self.state = MailViewState::Loaded { body: bytes_result };
                        } else {
                            self.state = MailViewState::LoadingBody {
                                job_id,
                                chan,
                                pending_action: pending_action.take(),
                            };
                            self.active_jobs.insert(job_id);
                            account.insert_job(job_id, JobRequest::AsBytes(handle));
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
            if !account.collection.get_env(self.coordinates.2).is_seen() {
                let job = account.backend.write().unwrap().set_flags(
                    self.coordinates.2.into(),
                    self.coordinates.1,
                    smallvec::smallvec![(Ok(Flag::SEEN), true)],
                );
                match job {
                    Ok(fut) => {
                        let (rcvr, handle, job_id) = account.job_executor.spawn_specialized(fut);
                        account.insert_job(
                            job_id,
                            JobRequest::SetFlags(self.coordinates.2.into(), handle, rcvr),
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
        let bytes = match self.state {
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
            MailViewState::Loaded { body: Ok(ref b) } => b,
            MailViewState::Loaded { body: Err(_) } => {
                return;
            }
        };
        let composer = match action {
            PendingReplyAction::Reply => {
                Box::new(Composer::reply_to_select(self.coordinates, bytes, context))
            }
            PendingReplyAction::ReplyToAuthor => {
                Box::new(Composer::reply_to_author(self.coordinates, bytes, context))
            }
            PendingReplyAction::ReplyToAll => {
                Box::new(Composer::reply_to_all(self.coordinates, bytes, context))
            }
        };

        context
            .replies
            .push_back(UIEvent::Action(Tab(New(Some(composer)))));
    }

    /// Returns the string to be displayed in the Viewer
    fn attachment_to_text<'closure, 's: 'closure, 'context: 's>(
        &'s mut self,
        body: &'context Attachment,
        context: &'context mut Context,
    ) -> String {
        let finder = LinkFinder::new();
        let coordinates = self.coordinates;
        let body_text = String::from_utf8_lossy(&decode_rec(
            body,
            Some(Box::new(move |a: &'closure Attachment, v: &mut Vec<u8>| {
                if a.content_type().is_text_html() {
                    /* FIXME: duplication with view/html.rs */
                    if let Some(filter_invocation) =
                        mailbox_settings!(context[coordinates.0][&coordinates.1].pager.html_filter)
                            .as_ref()
                    {
                        let command_obj = Command::new("sh")
                            .args(&["-c", filter_invocation])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn();
                        if command_obj.is_err() {
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!(
                                    "Failed to start html filter process: {}",
                                    filter_invocation,
                                )),
                                String::new(),
                                Some(NotificationType::ERROR),
                            ));
                            return;
                        }

                        let mut html_filter = command_obj.unwrap();
                        html_filter
                            .stdin
                            .as_mut()
                            .unwrap()
                            .write_all(&v)
                            .expect("Failed to write to stdin");
                        *v = format!(
                            "Text piped through `{}`. Press `v` to open in web browser. \n\n",
                            filter_invocation
                        )
                        .into_bytes();
                        v.extend(html_filter.wait_with_output().unwrap().stdout);
                    } else {
                        if let Ok(mut html_filter) = Command::new("w3m")
                            .args(&["-I", "utf-8", "-T", "text/html"])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()
                        {
                            html_filter
                                .stdin
                                .as_mut()
                                .unwrap()
                                .write_all(&v)
                                .expect("Failed to write to html filter stdin");
                            *v = String::from(
                                "Text piped through `w3m`. Press `v` to open in web browser. \n\n",
                            )
                            .into_bytes();
                            v.extend(html_filter.wait_with_output().unwrap().stdout);
                        } else {
                            context.replies.push_back(UIEvent::Notification(
                                Some(
                                    "Failed to find any application to use as html filter"
                                        .to_string(),
                                ),
                                String::new(),
                                Some(NotificationType::ERROR),
                            ));
                        }
                    }
                } else if a.is_signed() {
                    v.clear();
                    if context.settings.pgp.auto_verify_signatures {
                        v.extend(crate::mail::pgp::verify_signature(a, context).into_iter());
                    }
                }
            })),
        ))
        .into_owned();
        if body.count_attachments() > 1 {
            self.attachment_tree.clear();
            attachment_tree(
                (&mut 0, (0, &body)),
                &mut SmallVec::new(),
                false,
                &mut self.attachment_tree,
            );
        }
        match self.mode {
            ViewMode::Normal
            | ViewMode::Subview
            | ViewMode::ContactSelector(_)
            | ViewMode::Source(Source::Decoded) => {
                format!("{}\n\n{}", body_text, self.attachment_tree)
            }
            ViewMode::Source(Source::Raw) => String::from_utf8_lossy(body.body()).into_owned(),
            ViewMode::Url => {
                let mut t = body_text;
                for (lidx, l) in finder.links(&body.text()).enumerate() {
                    let offset = if lidx < 10 {
                        lidx * 3
                    } else if lidx < 100 {
                        26 + (lidx - 9) * 4
                    } else if lidx < 1000 {
                        385 + (lidx - 99) * 5
                    } else {
                        panic!("FIXME: Message body with more than 100 urls, fix this");
                    };
                    t.insert_str(l.start() + offset, &format!("[{}]", lidx));
                }
                t.push_str("\n\n");
                t.push_str(&self.attachment_tree);
                t
            }
            ViewMode::Attachment(aidx) => {
                let attachments = body.attachments();
                let mut ret = "Viewing attachment. Press `r` to return \n".to_string();
                ret.push_str(&attachments[aidx].text());
                ret
            }
            ViewMode::Ansi(_) => "Viewing attachment. Press `r` to return \n".to_string(),
        }
    }

    pub fn update(
        &mut self,
        new_coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        context: &mut Context,
    ) {
        self.coordinates = new_coordinates;
        self.mode = ViewMode::Normal;
        self.initialised = false;
        self.init_futures(context);
        self.set_dirty(true);
    }

    fn open_attachment(&mut self, lidx: usize, context: &mut Context, use_mailcap: bool) {
        let attachments = if let MailViewState::Loaded { ref body } = self.state {
            match body {
                Ok(body) => AttachmentBuilder::new(body).build().attachments(),
                Err(err) => {
                    context.replies.push_back(UIEvent::Notification(
                        Some("Failed to open e-mail".to_string()),
                        err.to_string(),
                        Some(NotificationType::ERROR),
                    ));
                    log(
                        format!("Failed to open envelope: {}", err.to_string()),
                        ERROR,
                    );
                    self.init_futures(context);
                    return;
                }
            }
        } else {
            return;
        };
        if let Some(u) = attachments.get(lidx) {
            if use_mailcap {
                if let Ok(()) = crate::mailcap::MailcapEntry::execute(u, context) {
                    self.set_dirty(true);
                } else {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                            "no mailcap entry found for {}",
                            u.content_type()
                        ))));
                }
            } else {
                match u.content_type() {
                    ContentType::MessageRfc822 => match EnvelopeWrapper::new(u.body().to_vec()) {
                        Ok(wrapper) => {
                            context
                                .replies
                                .push_back(UIEvent::Action(Tab(New(Some(Box::new(
                                    EnvelopeView::new(wrapper, None, None, self.coordinates.0),
                                ))))));
                        }
                        Err(e) => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(format!("{}", e)),
                            ));
                        }
                    },

                    ContentType::Text { .. } | ContentType::PGPSignature => {
                        self.mode = ViewMode::Attachment(lidx);
                        self.initialised = false;
                        self.dirty = true;
                    }
                    ContentType::Multipart { .. } => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(
                                "Multipart attachments are not supported yet.".to_string(),
                            ),
                        ));
                    }
                    ContentType::Other { ref name, .. } => {
                        let attachment_type = u.mime_type();
                        let binary = query_default_app(&attachment_type);
                        let mut name_opt = name.as_ref().and_then(|n| {
                            melib::email::parser::encodings::phrase(n.as_bytes(), false)
                                .map(|(_, v)| v)
                                .ok()
                                .and_then(|n| String::from_utf8(n).ok())
                        });
                        if name_opt.is_none() {
                            name_opt = name.as_ref().map(|n| n.clone());
                        }
                        if let Ok(binary) = binary {
                            let p = create_temp_file(
                                &decode(u, None),
                                name_opt.as_ref().map(String::as_str),
                                None,
                                true,
                            );
                            match debug!(context.plugin_manager.activate_hook(
                                "attachment-view",
                                p.path().display().to_string().into_bytes()
                            )) {
                                Ok(crate::plugins::FilterResult::Ansi(s)) => {
                                    if let Some(buf) = crate::terminal::ansi::ansi_to_cellbuffer(&s)
                                    {
                                        let raw_buf = RawBuffer::new(buf, name_opt);
                                        self.mode = ViewMode::Ansi(raw_buf);
                                        self.initialised = false;
                                        self.dirty = true;
                                        return;
                                    }
                                }
                                Ok(crate::plugins::FilterResult::UiMessage(s)) => {
                                    context.replies.push_back(UIEvent::Notification(
                                        None,
                                        s,
                                        Some(NotificationType::ERROR),
                                    ));
                                }
                                _ => {}
                            }
                            match Command::new(&binary)
                                .arg(p.path())
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
                                            "Failed to start {}: {}",
                                            binary.display(),
                                            err
                                        )),
                                    ));
                                }
                            }
                        } else {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(if name.is_some() {
                                    format!(
                                        "Couldn't find a default application for file {} (type {})",
                                        name.as_ref().unwrap(),
                                        attachment_type
                                    )
                                } else {
                                    format!(
                                        "Couldn't find a default application for type {}",
                                        attachment_type
                                    )
                                }),
                            ));
                        }
                    }
                    ContentType::OctetStream { ref name } => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Failed to open {}. application/octet-stream isn't supported yet",
                                name.as_ref().map(|n| n.as_str()).unwrap_or("file")
                            )),
                        ));
                    }
                }
            }
        } else {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                    "Attachment `{}` not found.",
                    lidx
                ))));
        }
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
                /* The envelope has been renamed or removed, so wait for the appropriate event to
                 * arrive */
                self.dirty = false;
                return;
            }
            let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);

            let headers = crate::conf::value(context, "mail.view.headers");

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
                    ($($string:expr)+) => {
                        $({
                            if sticky || skip_header_ctr == 0 {
                                let (_x, _y) = write_string_to_grid(
                                    &$string,
                                    grid,
                                    headers.fg,
                                    headers.bg,
                                    headers.attrs,
                                    (set_y(upper_left, y), bottom_right),
                                    Some(get_x(upper_left)),
                                );
                            clear_area(grid, ((_x, _y), (get_x(bottom_right), _y)), headers);
                            y = _y + 1;
                        } else {
                            skip_header_ctr -= 1;
                        }
                        self.headers_no += 1;
                        })+
                    };
                }
                print_header!(
                    format!("Date: {}", envelope.date_as_str())
                    format!("From: {}", envelope.field_from_to_string())
                    format!("To: {}", envelope.field_to_to_string())
                );
                if envelope.other_headers().contains_key("Cc")
                    && !envelope.other_headers()["Cc"].is_empty()
                {
                    print_header!(format!("Cc: {}", envelope.field_cc_to_string()));
                }
                print_header!(
                    format!("Subject: {}", envelope.subject())
                    format!("Message-ID: <{}>", envelope.message_id_raw())
                );
                if self.expand_headers {
                    if let Some(val) = envelope.in_reply_to_display() {
                        print_header!(
                            format!("In-Reply-To: {}", val)
                            format!(
                                "References: {}",
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
                                headers,
                            );
                            let (_x, _) = write_string_to_grid(
                                "List-ID: ",
                                grid,
                                headers.fg,
                                headers.bg,
                                headers.attrs,
                                (set_y(upper_left, y), bottom_right),
                                None,
                            );
                            let (_x, _y) = write_string_to_grid(
                                id,
                                grid,
                                Color::Default,
                                Color::Default,
                                Attr::DEFAULT,
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
                                headers.fg,
                                headers.bg,
                                headers.attrs,
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
                                Color::Default,
                                Color::Default,
                                Attr::DEFAULT,
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
                                Color::Default,
                                Color::Default,
                                Attr::DEFAULT,
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
                                Color::Default,
                                Color::Default,
                                Attr::DEFAULT,
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
                                grid[(x - 1, y)].set_fg(headers.fg);
                                grid[(x - 1, y)].set_bg(headers.bg);
                                grid[(x - 1, y)].set_attrs(headers.attrs);
                                grid[(x - 1, y)].set_ch(']');
                            }
                        }
                        for x in x..=get_x(bottom_right) {
                            grid[(x, y)].set_ch(' ');
                            grid[(x, y)].set_bg(Color::Default);
                            grid[(x, y)].set_fg(Color::Default);
                        }
                        y += 1;
                    }
                }

                self.force_draw_headers = false;
                clear_area(
                    grid,
                    (set_y(upper_left, y), set_y(bottom_right, y)),
                    headers,
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
            let bytes = if let MailViewState::Loaded { ref body } = self.state {
                match body {
                    Ok(body) => body,
                    Err(e) => {
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
                            e.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        log(format!("Failed to open envelope: {}", e.to_string()), ERROR);
                        self.init_futures(context);
                        return;
                    }
                }
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
            let body = AttachmentBuilder::new(bytes).build();
            match self.mode {
                ViewMode::Attachment(aidx) if body.attachments()[aidx].is_html() => {
                    let attachment = &body.attachments()[aidx];
                    self.subview = Some(Box::new(HtmlView::new(&attachment, context)));
                    self.mode = ViewMode::Subview;
                    self.initialised = false;
                }
                ViewMode::Normal if body.is_html() => {
                    self.subview = Some(Box::new(HtmlView::new(&body, context)));
                    self.mode = ViewMode::Subview;
                    self.initialised = false;
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
                    self.subview = Some(Box::new(HtmlView::new(
                        &body
                            .content_type
                            .parts()
                            .unwrap()
                            .into_iter()
                            .find(|a| a.is_html())
                            .unwrap_or(&body),
                        context,
                    )));
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
                                .unwrap_or_else(|err: MeliError| err.to_string());
                            ret.push_str("\n\n");
                            ret.extend(self.attachment_to_text(&body, context).chars());
                            ret
                        }
                    };
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager = Pager::from_string(text, Some(context), None, None, colors);
                }
                ViewMode::Ansi(ref buf) => {
                    write_string_to_grid(
                        &format!("Viewing `{}`. Press `r` to return", buf.title()),
                        grid,
                        Color::Default,
                        Color::Default,
                        Attr::DEFAULT,
                        (set_y(upper_left, y), bottom_right),
                        Some(get_x(upper_left)),
                    );
                }
                _ => {
                    let text = {
                        self.attachment_to_text(&body, context)
                        /*
                        // URL indexes must be colored (ugh..)
                        MailView::plain_text_to_buf(&text, self.mode == ViewMode::Url)
                        */
                    };
                    let cursor_pos = if self.mode.is_attachment() {
                        0
                    } else {
                        self.pager.cursor_pos()
                    };
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager =
                        Pager::from_string(text, Some(context), Some(cursor_pos), None, colors);
                    self.subview = None;
                }
            };
        }
        match self.mode {
            ViewMode::Subview if self.subview.is_some() => {
                if let Some(s) = self.subview.as_mut() {
                    s.draw(grid, (set_y(upper_left, y), bottom_right), context);
                }
            }
            ViewMode::Ansi(ref mut buf) => {
                buf.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
            }
            _ => {
                self.pager
                    .draw(grid, (set_y(upper_left, y), bottom_right), context);
            }
        }
        if let ViewMode::ContactSelector(ref mut s) = self.mode {
            s.draw(grid, center_area(area, s.content.size()), context);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);
        match (&mut self.mode, &mut event) {
            (ViewMode::Ansi(ref mut buf), _) => {
                if buf.process_event(event, context) {
                    return true;
                }
            }
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
                    if shortcut!(key == shortcuts[Pager::DESCRIPTION]["scroll_up"])
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
                    } else {
                        if self.pager.process_event(event, context) {
                            return true;
                        }
                    }
                    self.pager.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Pager::DESCRIPTION]["scroll_down"])
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
                            job_id: ref id,
                            ref mut chan,
                            pending_action: _,
                        } if job_id == id => {
                            let bytes_result = chan.try_recv().unwrap().unwrap();
                            debug!("bytes_result");
                            self.state = MailViewState::Loaded { body: bytes_result };
                        }
                        MailViewState::Init { .. } => {
                            self.init_futures(context);
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
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[MailView::DESCRIPTION]["reply"]) =>
            {
                self.perform_action(PendingReplyAction::Reply, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[MailView::DESCRIPTION]["reply_to_all"]) =>
            {
                self.perform_action(PendingReplyAction::ReplyToAll, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[MailView::DESCRIPTION]["reply_to_author"]) =>
            {
                self.perform_action(PendingReplyAction::ReplyToAuthor, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[MailView::DESCRIPTION]["edit"]) =>
            {
                let account_hash = self.coordinates.0;
                let env_hash = self.coordinates.2;
                let (sender, mut receiver) = crate::jobs::oneshot::channel();
                let operation = context.accounts[&account_hash].operation(env_hash);
                let bytes_job = async move {
                    let _ = sender.send(operation?.as_bytes()?.await);
                    Ok(())
                };
                let (channel, handle, job_id) = if context.accounts[&account_hash]
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
                    job_id,
                    crate::conf::accounts::JobRequest::Generic {
                        name: "fetch envelope".into(),
                        handle,
                        channel,
                        on_finish: Some(CallbackFn(Box::new(move |context: &mut Context| {
                            let result = receiver.try_recv().unwrap().unwrap();
                            match result.and_then(|bytes| {
                                Composer::edit(account_hash, env_hash, &bytes, context)
                            }) {
                                Ok(composer) => {
                                    context.replies.push_back(UIEvent::Action(Tab(New(Some(
                                        Box::new(composer),
                                    )))));
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
                                        err.to_string()
                                    );
                                    log(&err_string, ERROR);
                                    context.replies.push_back(UIEvent::Notification(
                                        Some("Failed to open e-mail".to_string()),
                                        err_string,
                                        Some(NotificationType::ERROR),
                                    ));
                                }
                            }
                        }))),
                    },
                );
                return true;
            }
            UIEvent::Input(ref key)
                if !self.mode.is_contact_selector()
                    && shortcut!(
                        key == shortcuts[MailView::DESCRIPTION]["add_addresses_to_contacts"]
                    ) =>
            {
                let account = &context.accounts[&self.coordinates.0];
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);

                let mut entries = Vec::new();
                for addr in envelope.from().iter().chain(envelope.to().iter()) {
                    let mut new_card: Card = Card::new();
                    new_card.set_email(addr.get_email());
                    new_card.set_name(addr.get_display_name());
                    entries.push((new_card, format!("{}", addr)));
                }
                drop(envelope);
                self.mode = ViewMode::ContactSelector(Selector::new(
                    "select contacts to add",
                    entries,
                    false,
                    Some(Box::new(move |id: ComponentId, results: &[Card]| {
                        Some(UIEvent::FinishedUIDialog(id, Box::new(results.to_vec())))
                    })),
                    context,
                ));
                self.dirty = true;
                self.initialised = false;
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
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
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
                    && shortcut!(key == shortcuts[MailView::DESCRIPTION]["view_raw_source"]) =>
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
                    || self.mode.is_ansi()
                    || self.mode == ViewMode::Subview
                    || self.mode == ViewMode::Url
                    || self.mode == ViewMode::Source(Source::Decoded)
                    || self.mode == ViewMode::Source(Source::Raw))
                    && shortcut!(
                        key == shortcuts[MailView::DESCRIPTION]["return_to_normal_view"]
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
                    && shortcut!(key == shortcuts[MailView::DESCRIPTION]["open_mailcap"]) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                match self.state {
                    MailViewState::LoadingBody { .. } => {}
                    MailViewState::Loaded { .. } => {
                        self.open_attachment(lidx, context, true);
                    }
                    MailViewState::Init { .. } => {
                        self.init_futures(context);
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[MailView::DESCRIPTION]["open_attachment"])
                    && !self.cmd_buf.is_empty()
                    && (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                match self.state {
                    MailViewState::LoadingBody { .. } => {}
                    MailViewState::Loaded { .. } => {
                        self.open_attachment(lidx, context, false);
                    }
                    MailViewState::Init { .. } => {
                        self.init_futures(context);
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[MailView::DESCRIPTION]["toggle_expand_headers"]) =>
            {
                self.expand_headers = !self.expand_headers;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if !self.cmd_buf.is_empty()
                    && self.mode == ViewMode::Url
                    && shortcut!(key == shortcuts[MailView::DESCRIPTION]["go_to_url"]) =>
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
                    MailViewState::LoadingBody { .. } => {}
                    MailViewState::Loaded { ref body } => {
                        let url = {
                            let finder = LinkFinder::new();
                            let t = match body {
                                Ok(body) => AttachmentBuilder::new(&body).build().text(),
                                Err(e) => {
                                    context.replies.push_back(UIEvent::Notification(
                                        Some("Failed to open e-mail".to_string()),
                                        e.to_string(),
                                        Some(NotificationType::ERROR),
                                    ));
                                    log(e.to_string(), ERROR);
                                    self.init_futures(context);
                                    return true;
                                }
                            };
                            let links: Vec<Link> = finder.links(&t).collect();
                            if let Some(u) = links.get(lidx) {
                                u.as_str().to_string()
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

                        match Command::new("xdg-open")
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
                                    Some("Failed to launch xdg-open".to_string()),
                                    err.to_string(),
                                    Some(NotificationType::ERROR),
                                ));
                            }
                        }
                    }
                }
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Url)
                    && shortcut!(key == shortcuts[MailView::DESCRIPTION]["toggle_url_mode"]) =>
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
            UIEvent::Action(View(ViewAction::SaveAttachment(a_i, ref path))) => {
                let account = &context.accounts[&self.coordinates.0];
                if !account.contains_key(self.coordinates.2) {
                    /* The envelope has been renamed or removed, so wait for the appropriate event to
                     * arrive */
                    return true;
                }
                let bytes = if let MailViewState::Loaded { ref body } = self.state {
                    match body {
                        Ok(body) => body,
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some("Failed to open e-mail".to_string()),
                                err.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                            log(
                                format!("Failed to open envelope: {}", err.to_string()),
                                ERROR,
                            );
                            self.init_futures(context);
                            return true;
                        }
                    }
                } else {
                    return true;
                };

                if a_i == 0 {
                    let mut path = std::path::Path::new(path).to_path_buf();
                    let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                    // Save entire message as eml
                    if path.is_dir() {
                        path.push(format!("{}.eml", envelope.message_id_display()));
                    }
                    let mut f = match std::fs::File::create(&path) {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!("Failed to create file at {}", path.display())),
                                err.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                            log(
                                format!(
                                    "Failed to create file at {}: {}",
                                    path.display(),
                                    err.to_string()
                                ),
                                ERROR,
                            );
                            return true;
                        }
                        Ok(f) => f,
                    };
                    use std::os::unix::fs::PermissionsExt;
                    let metadata = f.metadata().unwrap();
                    let mut permissions = metadata.permissions();

                    permissions.set_mode(0o600); // Read/write for owner only.
                    f.set_permissions(permissions).unwrap();

                    f.write_all(bytes).unwrap();
                    f.flush().unwrap();
                    context.replies.push_back(UIEvent::Notification(
                        None,
                        format!("Saved at {}", &path.display()),
                        Some(NotificationType::INFO),
                    ));

                    return true;
                }

                let attachments = AttachmentBuilder::new(bytes).build().attachments();
                if let Some(u) = attachments.get(a_i) {
                    match u.content_type() {
                        ContentType::MessageRfc822
                        | ContentType::Text { .. }
                        | ContentType::PGPSignature => {
                            debug!(path);
                            let mut f = match std::fs::File::create(path) {
                                Err(e) => {
                                    context.replies.push_back(UIEvent::Notification(
                                        Some(format!("Failed to create file at {}", path)),
                                        e.to_string(),
                                        Some(NotificationType::ERROR),
                                    ));
                                    log(
                                        format!(
                                            "Failed to create file at {}: {}",
                                            path,
                                            e.to_string()
                                        ),
                                        ERROR,
                                    );
                                    return true;
                                }
                                Ok(f) => f,
                            };
                            use std::os::unix::fs::PermissionsExt;
                            let metadata = f.metadata().unwrap();
                            let mut permissions = metadata.permissions();

                            permissions.set_mode(0o600); // Read/write for owner only.
                            f.set_permissions(permissions).unwrap();

                            f.write_all(&decode(u, None)).unwrap();
                            f.flush().unwrap();
                        }

                        ContentType::Multipart { .. } => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(
                                    "Multipart attachments are not supported yet.".to_string(),
                                ),
                            ));
                            return true;
                        }
                        ContentType::OctetStream { name: ref _name }
                        | ContentType::Other {
                            name: ref _name, ..
                        } => {
                            let mut f = match std::fs::File::create(path.trim()) {
                                Err(e) => {
                                    context.replies.push_back(UIEvent::Notification(
                                        Some(format!("Failed to create file at {}", path)),
                                        e.to_string(),
                                        Some(NotificationType::ERROR),
                                    ));
                                    log(
                                        format!(
                                            "Failed to create file at {}: {}",
                                            path,
                                            e.to_string()
                                        ),
                                        ERROR,
                                    );
                                    return true;
                                }
                                Ok(f) => f,
                            };

                            f.write_all(&decode(u, None)).unwrap();
                            f.flush().unwrap();
                        }
                    }
                    context.replies.push_back(UIEvent::Notification(
                        None,
                        format!("Saved at {}", &path),
                        Some(NotificationType::INFO),
                    ));
                } else {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                            "Attachment `{}` not found.",
                            a_i
                        ))));
                    return true;
                }
            }
            UIEvent::Action(MailingListAction(ref e)) => {
                let account = &context.accounts[&self.coordinates.0];
                if !account.contains_key(self.coordinates.2) {
                    /* The envelope has been renamed or removed, so wait for the appropriate event to
                     * arrive */
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
                                    let mut composer = Composer::new(self.coordinates.0, context);
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
                            /* autosend or open unsubscribe option*/
                            let unsubscribe = actions.unsubscribe.as_ref().unwrap();
                            for option in unsubscribe.iter() {
                                /* TODO: Ask for confirmation before proceding with an action */
                                match option {
                                    list_management::ListAction::Email(email) => {
                                        if let Ok(mailto) = Mailto::try_from(*email) {
                                            let mut draft: Draft = mailto.into();
                                            draft.set_header(
                                                "From",
                                                crate::components::mail::get_display_name(
                                                    context,
                                                    self.coordinates.0,
                                                ),
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
                                        match Command::new("xdg-open")
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
                                                        "Couldn't launch xdg-open: {}",
                                                        err
                                                    )),
                                                ));
                                            }
                                        }
                                        return true;
                                    }
                                }
                            }
                        }
                        MailingListAction::ListArchive if actions.archive.is_some() => {
                            /* open archive url with xdg-open */
                            match Command::new("xdg-open")
                                .arg(actions.archive.unwrap())
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .spawn()
                            {
                                Ok(child) => context.children.push(child),
                                Err(err) => {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!(
                                            "Couldn't launch xdg-open: {}",
                                            err
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
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.pager.is_dirty()
            || self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || if let ViewMode::ContactSelector(ref s) = self.mode {
                s.is_dirty()
            } else if let ViewMode::Ansi(ref r) = self.mode {
                r.is_dirty()
            } else {
                false
            }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        match self.mode {
            ViewMode::Normal => {
                self.pager.set_dirty(value);
            }
            ViewMode::Subview => {
                if let Some(s) = self.subview.as_mut() {
                    s.set_dirty(value);
                }
            }
            _ => {}
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
            || self.mode.is_ansi()
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
        map.insert(MailView::DESCRIPTION, our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        debug_assert!(self.id == id);
        context
            .replies
            .push_back(UIEvent::Action(Tab(Kill(self.id))));
    }
}

fn attachment_tree(
    (idx, (depth, att)): (&mut usize, (usize, &Attachment)),
    branches: &mut SmallVec<[bool; 8]>,
    has_sibling: bool,
    s: &mut String,
) {
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
        if has_sibling {
            s.push('|');
            s.push('\\');
        } else {
            s.push(' ');
        }
        s.push(' ');
    }

    s.extend(att.to_string().chars());
    match att.content_type {
        ContentType::Multipart {
            parts: ref sub_att_vec,
            ..
        } => {
            let mut iter = (0..sub_att_vec.len()).peekable();
            if has_sibling {
                branches.push(true);
            } else {
                branches.push(false);
            }
            while let Some(i) = iter.next() {
                *idx += 1;
                attachment_tree(
                    (idx, (depth + 1, &sub_att_vec[i])),
                    branches,
                    iter.peek() != None,
                    s,
                );
            }
            branches.pop();
        }
        _ => {}
    }
}
