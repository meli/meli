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
use melib::list_management;
use melib::Draft;

use crate::conf::accounts::JobRequest;
use crate::jobs::{JobChannel, JobId, JoinHandle};
use crate::terminal::embed::EmbedGrid;
use nix::sys::wait::WaitStatus;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use xdg_utils::query_mime_info;

#[derive(Debug, PartialEq)]
enum Cursor {
    Headers,
    Body,
    //Attachments,
}

#[derive(Debug)]
enum EmbedStatus {
    Stopped(Arc<Mutex<EmbedGrid>>, File),
    Running(Arc<Mutex<EmbedGrid>>, File),
}

impl std::ops::Deref for EmbedStatus {
    type Target = Arc<Mutex<EmbedGrid>>;
    fn deref(&self) -> &Arc<Mutex<EmbedGrid>> {
        use EmbedStatus::*;
        match self {
            Stopped(ref e, _) | Running(ref e, _) => e,
        }
    }
}

impl std::ops::DerefMut for EmbedStatus {
    fn deref_mut(&mut self) -> &mut Arc<Mutex<EmbedGrid>> {
        use EmbedStatus::*;
        match self {
            Stopped(ref mut e, _) | Running(ref mut e, _) => e,
        }
    }
}

#[derive(Debug)]
pub struct Composer {
    reply_context: Option<(MailboxHash, EnvelopeHash)>,
    reply_bytes_request: Option<(JobId, JobChannel<Vec<u8>>)>,
    account_hash: AccountHash,

    cursor: Cursor,

    pager: Pager,
    draft: Draft,
    form: FormWidget,

    mode: ViewMode,

    embed_area: Area,
    embed: Option<EmbedStatus>,
    sign_mail: ToggleFlag,
    dirty: bool,
    has_changes: bool,
    initialized: bool,
    id: ComponentId,
}

impl Default for Composer {
    fn default() -> Self {
        let mut pager = Pager::default();
        pager.set_reflow(text_processing::Reflow::FormatFlowed);
        Composer {
            reply_context: None,
            reply_bytes_request: None,
            account_hash: 0,

            cursor: Cursor::Headers,

            pager,
            draft: Draft::default(),
            form: FormWidget::default(),

            mode: ViewMode::Edit,
            sign_mail: ToggleFlag::Unset,
            dirty: true,
            has_changes: false,
            embed_area: ((0, 0), (0, 0)),
            embed: None,
            initialized: false,
            id: ComponentId::new_v4(),
        }
    }
}

#[derive(Debug)]
enum ViewMode {
    Discard(Uuid, UIDialog<char>),
    Edit,
    Embed,
    SelectRecipients(UIDialog<Address>),
    Send(UIConfirmationDialog),
    WaitingForSendResult(UIDialog<char>, JoinHandle, JobId, JobChannel<()>),
}

impl ViewMode {
    fn is_edit(&self) -> bool {
        if let ViewMode::Edit = self {
            true
        } else {
            false
        }
    }
}

impl fmt::Display for Composer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        if self.reply_context.is_some() {
            write!(f, "reply: {:8}", self.draft.headers()["Subject"])
        } else {
            write!(f, "composing")
        }
    }
}

impl Composer {
    const DESCRIPTION: &'static str = "composing";
    pub fn new(account_hash: AccountHash, context: &Context) -> Self {
        let mut ret = Composer {
            account_hash,
            id: ComponentId::new_v4(),
            ..Default::default()
        };
        for (h, v) in
            mailbox_acc_settings!(context[account_hash].composing.default_header_values).iter()
        {
            if v.is_empty() {
                continue;
            }
            if let Some(k) = ret
                .draft
                .headers()
                .keys()
                .find(|k| k.eq_ignore_ascii_case(h))
            {
                let _k = k.clone();
                ret.draft.headers_mut().insert(_k, v.into());
            } else {
                /* set_header() also updates draft's header_order field */
                ret.draft.set_header(h, v.into());
            }
        }
        if *mailbox_acc_settings!(context[account_hash].composing.insert_user_agent) {
            ret.draft.set_header(
                "User-Agent".into(),
                format!("meli {}", option_env!("CARGO_PKG_VERSION").unwrap_or("0.0")),
            );
        }
        ret.pager
            .set_colors(crate::conf::value(context, "theme_default"));
        ret
    }

    pub fn edit(new_account_hash: AccountHash, h: EnvelopeHash, context: &Context) -> Result<Self> {
        let mut ret = Composer::default();
        let op = context.accounts[&new_account_hash].operation(h)?;
        let envelope: EnvelopeRef = context.accounts[&new_account_hash].collection.get_env(h);

        ret.draft = Draft::edit(&envelope, op)?;

        ret.account_hash = new_account_hash;
        Ok(ret)
    }

    pub fn with_context(
        coordinates: (AccountHash, MailboxHash),
        msg: EnvelopeHash,
        context: &mut Context,
    ) -> Self {
        let account = &context.accounts[&coordinates.0];
        let mut ret = Composer::default();
        ret.pager
            .set_colors(crate::conf::value(context, "theme_default"));
        let parent_message = account.collection.get_env(msg);
        /* If message is from a mailing list and we detect a List-Post header, ask user if they
         * want to reply to the mailing list or the submitter of the message */
        if let Some(actions) = list_management::ListActions::detect(&parent_message) {
            if let Some(post) = actions.post {
                if let list_management::ListAction::Email(list_post_addr) = post[0] {
                    if let Ok(list_address) = melib::email::parser::generic::mailto(list_post_addr)
                        .map(|(_, m)| m.address)
                    {
                        let list_address_string = list_address.to_string();
                        ret.mode = ViewMode::SelectRecipients(UIDialog::new(
                            "select recipients",
                            vec![
                                (
                                    parent_message.from()[0].clone(),
                                    parent_message.field_from_to_string(),
                                ),
                                (list_address, list_address_string),
                            ],
                            false,
                            Some(Box::new(move |id: ComponentId, results: &[Address]| {
                                Some(UIEvent::FinishedUIDialog(
                                    id,
                                    Box::new(
                                        results
                                            .iter()
                                            .map(|a| a.to_string())
                                            .collect::<Vec<String>>()
                                            .join(", "),
                                    ),
                                ))
                            })),
                            context,
                        ));
                    }
                }
            }
        }
        let subject = parent_message.subject();
        ret.draft.headers_mut().insert(
            "Subject".into(),
            if !subject.starts_with("Re: ") {
                format!("Re: {}", subject)
            } else {
                subject.into()
            },
        );

        drop(parent_message);
        match context.accounts[&coordinates.0]
            .operation(msg)
            .and_then(|mut op| op.as_bytes())
        {
            Err(err) => {
                context.replies.push_back(UIEvent::Notification(
                    None,
                    err.to_string(),
                    Some(NotificationType::ERROR),
                ));
            }
            Ok(fut) => {
                let (mut rcvr, handle, job_id) = context.accounts[&coordinates.0]
                    .job_executor
                    .spawn_specialized(fut);
                context.accounts[&coordinates.0]
                    .active_jobs
                    .insert(job_id, JobRequest::AsBytes(handle));
                if let Ok(Some(parent_bytes)) = try_recv_timeout!(&mut rcvr) {
                    match parent_bytes {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                err.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                        }
                        Ok(parent_bytes) => {
                            let env_hash = msg;
                            let parent_message = context.accounts[&coordinates.0]
                                .collection
                                .get_env(env_hash);
                            let mut new_draft = Draft::new_reply(&parent_message, &parent_bytes);
                            new_draft
                                .headers_mut()
                                .extend(ret.draft.headers_mut().drain());
                            new_draft
                                .attachments_mut()
                                .extend(ret.draft.attachments_mut().drain(..));
                            ret.set_draft(new_draft);
                        }
                    }
                } else {
                    ret.reply_bytes_request = Some((job_id, rcvr));
                }
            }
        }
        ret.account_hash = coordinates.0;
        ret.reply_context = Some((coordinates.1, msg));
        ret
    }

    pub fn set_draft(&mut self, draft: Draft) {
        self.draft = draft;
        self.update_form();
    }

    fn update_draft(&mut self) {
        let header_values = self.form.values_mut();
        let draft_header_map = self.draft.headers_mut();
        for (k, v) in draft_header_map.iter_mut() {
            if let Some(ref vn) = header_values.get(k) {
                *v = vn.as_str().to_string();
            }
        }
    }

    fn update_form(&mut self) {
        let old_cursor = self.form.cursor();
        self.form = FormWidget::new("Save".into());
        self.form.hide_buttons();
        self.form.set_cursor(old_cursor);
        let headers = self.draft.headers();
        let account_hash = self.account_hash;
        for &k in &["Date", "From", "To", "Cc", "Bcc", "Subject"] {
            if k == "To" || k == "Cc" || k == "Bcc" {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, term| {
                        let book: &AddressBook = &c.accounts[&account_hash].address_book;
                        let results: Vec<String> = book.search(term);
                        results
                            .into_iter()
                            .map(|r| AutoCompleteEntry::from(r))
                            .collect::<Vec<AutoCompleteEntry>>()
                    }),
                ));
            } else {
                self.form.push((k.into(), headers[k].to_string()));
            }
        }
    }

    fn draw_attachments(&self, grid: &mut CellBuffer, area: Area, context: &Context) {
        let attachments_no = self.draft.attachments().len();
        let theme_default = crate::conf::value(context, "theme_default");
        clear_area(grid, area, theme_default);
        if self.sign_mail.is_true() {
            write_string_to_grid(
                &format!(
                    "☑ sign with {}",
                    mailbox_acc_settings!(context[self.account_hash].pgp.key)
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or("default key")
                ),
                grid,
                theme_default.fg,
                theme_default.bg,
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                None,
            );
        } else {
            write_string_to_grid(
                "☐ don't sign",
                grid,
                theme_default.fg,
                theme_default.bg,
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                None,
            );
        }
        if attachments_no == 0 {
            write_string_to_grid(
                "no attachments",
                grid,
                theme_default.fg,
                theme_default.bg,
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 2)), bottom_right!(area)),
                None,
            );
        } else {
            write_string_to_grid(
                &format!("{} attachments ", attachments_no),
                grid,
                theme_default.fg,
                theme_default.bg,
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 2)), bottom_right!(area)),
                None,
            );
            for (i, a) in self.draft.attachments().iter().enumerate() {
                if let Some(name) = a.content_type().name() {
                    write_string_to_grid(
                        &format!(
                            "[{}] \"{}\", {} {} bytes",
                            i,
                            name,
                            a.content_type(),
                            a.raw.len()
                        ),
                        grid,
                        theme_default.fg,
                        theme_default.bg,
                        theme_default.attrs,
                        (pos_inc(upper_left!(area), (0, 3 + i)), bottom_right!(area)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        &format!("[{}] {} {} bytes", i, a.content_type(), a.raw.len()),
                        grid,
                        theme_default.fg,
                        theme_default.bg,
                        theme_default.attrs,
                        (pos_inc(upper_left!(area), (0, 3 + i)), bottom_right!(area)),
                        None,
                    );
                }
            }
        }
    }
}

impl Component for Composer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let upper_left = set_y(upper_left, get_y(upper_left) + 1);

        if height!(area) < 4 {
            return;
        }

        let width = width!(area);

        if !self.initialized {
            if self.sign_mail.is_unset() {
                self.sign_mail = ToggleFlag::InternalVal(*mailbox_acc_settings!(
                    context[self.account_hash].pgp.auto_sign
                ));
            }
            if !self.draft.headers().contains_key("From") || self.draft.headers()["From"].is_empty()
            {
                self.draft.headers_mut().insert(
                    "From".into(),
                    crate::components::mail::get_display_name(context, self.account_hash),
                );
            }
            self.pager.update_from_str(self.draft.body(), Some(77));
            self.update_form();
            self.initialized = true;
        }
        let header_height = self.form.len();
        let theme_default = crate::conf::value(context, "theme_default");

        let mid = if width > 80 {
            let width = width - 80;
            let mid = width / 2;

            if self.dirty {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    //set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
                    grid[(mid, i)]
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                    //set_and_join_box(grid, (mid + 80, i), VERT_BOUNDARY);
                    grid[(mid + 80, i)]
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
            }
            mid
        } else {
            0
        };

        let header_area = (
            set_x(upper_left, mid + 1),
            (
                get_x(bottom_right).saturating_sub(mid),
                get_y(upper_left) + header_height,
            ),
        );
        let attachments_no = self.draft.attachments().len();
        let attachment_area = (
            (mid + 1, get_y(bottom_right) - 2 - attachments_no),
            pos_dec(bottom_right, (mid, 0)),
        );

        let body_area = (
            pos_inc(upper_left, (mid + 1, header_height + 1)),
            pos_dec(bottom_right, (mid, 3 + attachments_no)),
        );

        let (x, y) = write_string_to_grid(
            if self.reply_context.is_some() {
                "COMPOSING REPLY"
            } else {
                "COMPOSING MESSAGE"
            },
            grid,
            Color::Byte(189),
            Color::Byte(167),
            Attr::DEFAULT,
            (
                pos_dec(upper_left!(header_area), (0, 1)),
                bottom_right!(header_area),
            ),
            None,
        );
        clear_area(grid, ((x, y), (set_y(bottom_right, y))), theme_default);
        change_colors(
            grid,
            (
                set_x(pos_dec(upper_left!(header_area), (0, 1)), x),
                set_y(bottom_right!(header_area), y),
            ),
            Color::Byte(189),
            Color::Byte(167),
        );
        clear_area(
            grid,
            (
                pos_dec(upper_left, (0, 1)),
                set_x(bottom_right, get_x(upper_left) + mid),
            ),
            theme_default,
        );
        clear_area(
            grid,
            (
                (
                    get_x(bottom_right).saturating_sub(mid),
                    get_y(upper_left) - 1,
                ),
                bottom_right,
            ),
            theme_default,
        );

        /* Regardless of view mode, do the following */
        self.form.draw(grid, header_area, context);
        if let Some(ref mut embed_pty) = self.embed {
            let embed_area = (upper_left!(header_area), bottom_right!(body_area));
            match embed_pty {
                EmbedStatus::Running(_, _) => {
                    let mut guard = embed_pty.lock().unwrap();
                    clear_area(grid, embed_area, theme_default);
                    copy_area(
                        grid,
                        &guard.grid,
                        embed_area,
                        ((0, 0), pos_dec(guard.terminal_size, (1, 1))),
                    );
                    guard.set_terminal_size((width!(embed_area), height!(embed_area)));
                    context.dirty_areas.push_back(area);
                    self.dirty = false;
                    return;
                }
                EmbedStatus::Stopped(_, _) => {
                    clear_area(grid, body_area, theme_default);
                    write_string_to_grid(
                        "process has stopped, press 'e' to re-activate",
                        grid,
                        theme_default.fg,
                        theme_default.bg,
                        theme_default.attrs,
                        body_area,
                        None,
                    );
                    context.dirty_areas.push_back(area);
                }
            }
        } else {
            self.embed_area = (upper_left!(header_area), bottom_right!(body_area));
            self.pager.set_dirty(true);
            self.pager.draw(grid, body_area, context);
        }

        if self.cursor == Cursor::Body {
            change_colors(
                grid,
                (
                    set_y(upper_left!(body_area), get_y(bottom_right!(body_area))),
                    bottom_right!(body_area),
                ),
                theme_default.fg,
                Color::Byte(237),
            );
        } else {
            change_colors(
                grid,
                (
                    set_y(upper_left!(body_area), get_y(bottom_right!(body_area))),
                    bottom_right!(body_area),
                ),
                theme_default.fg,
                theme_default.bg,
            );
        }

        match self.mode {
            ViewMode::Edit | ViewMode::Embed => {}
            ViewMode::Send(ref mut s) => {
                s.draw(grid, center_area(area, s.content.size()), context);
            }
            ViewMode::SelectRecipients(ref mut s) => {
                s.draw(grid, center_area(area, s.content.size()), context);
            }
            ViewMode::Discard(_, ref mut s) => {
                /* Let user choose whether to quit with/without saving or cancel */
                s.draw(grid, center_area(area, s.content.size()), context);
            }
            ViewMode::WaitingForSendResult(ref mut s, _, _, _) => {
                /* Let user choose whether to wait for success or cancel */
                s.draw(grid, center_area(area, s.content.size()), context);
            }
        }
        self.dirty = false;
        self.draw_attachments(grid, attachment_area, context);
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);
        match (&mut self.mode, &mut event) {
            (_, UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)))
                if self
                    .reply_bytes_request
                    .as_ref()
                    .map(|(j, _)| j == job_id)
                    .unwrap_or(false) =>
            {
                let bytes = self
                    .reply_bytes_request
                    .take()
                    .unwrap()
                    .1
                    .try_recv()
                    .unwrap()
                    .unwrap();
                match bytes {
                    Ok(parent_bytes) => {
                        let env_hash = self.reply_context.unwrap().1;
                        let parent_message = context.accounts[&self.account_hash]
                            .collection
                            .get_env(env_hash);
                        let mut new_draft = Draft::new_reply(&parent_message, &parent_bytes);
                        new_draft
                            .headers_mut()
                            .extend(self.draft.headers_mut().drain());
                        new_draft
                            .attachments_mut()
                            .extend(self.draft.attachments_mut().drain(..));
                        self.set_draft(new_draft);
                        self.set_dirty(true);
                        self.initialized = false;
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some(format!("Failed to load parent envelope")),
                            err.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                    }
                }
                return true;
            }
            (ViewMode::Edit, _) => {
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::Send(ref selector), UIEvent::FinishedUIDialog(id, result))
                if selector.id() == *id =>
            {
                if let Some(true) = result.downcast_ref::<bool>() {
                    self.update_draft();
                    match send_draft(
                        self.sign_mail,
                        context,
                        self.account_hash,
                        self.draft.clone(),
                        SpecialUsageMailbox::Sent,
                        Flag::SEEN,
                        false,
                    ) {
                        Ok(Some((job_id, handle, chan))) => {
                            self.mode = ViewMode::WaitingForSendResult(
                                UIDialog::new(
                                    "Waiting for confirmation.. The tab will close automatically on successful submission.",
                                    vec![
                                    ('c', "force close tab".to_string()),
                                    ('n', "close this message and return to edit mode".to_string()),
                                    ],
                                    true,
                                    Some(Box::new(move |id: ComponentId, results: &[char]| {
                                        Some(UIEvent::FinishedUIDialog(
                                                id,
                                                Box::new(results.get(0).map(|c| *c).unwrap_or('c')),
                                        ))
                                    })),
                                    context,
                                ), handle, job_id, chan);
                        }
                        Ok(None) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some("Sent.".into()),
                                String::new(),
                                Some(NotificationType::INFO),
                            ));
                            context
                                .replies
                                .push_back(UIEvent::Action(Tab(Kill(self.id))));
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                err.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                            save_draft(
                                self.draft.clone().finalise().unwrap().as_bytes(),
                                context,
                                SpecialUsageMailbox::Drafts,
                                Flag::SEEN | Flag::DRAFT,
                                self.account_hash,
                            );
                            self.mode = ViewMode::Edit;
                        }
                    }
                }
                self.set_dirty(true);
                return true;
            }
            (ViewMode::Send(ref mut selector), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            (
                ViewMode::SelectRecipients(ref selector),
                UIEvent::FinishedUIDialog(id, ref mut result),
            ) if selector.id() == *id => {
                if let Some(to_val) = result.downcast_mut::<String>() {
                    self.draft
                        .headers_mut()
                        .insert("To".to_string(), std::mem::replace(to_val, String::new()));
                    self.update_form();
                }
                self.mode = ViewMode::Edit;
                return true;
            }
            (ViewMode::SelectRecipients(ref mut selector), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::Discard(u, ref selector), UIEvent::FinishedUIDialog(id, ref mut result))
                if selector.id() == *id =>
            {
                if let Some(key) = result.downcast_mut::<char>() {
                    match key {
                        'x' => {
                            context.replies.push_back(UIEvent::Action(Tab(Kill(*u))));
                            return true;
                        }
                        'n' => {}
                        'y' => {
                            save_draft(
                                self.draft.clone().finalise().unwrap().as_bytes(),
                                context,
                                SpecialUsageMailbox::Drafts,
                                Flag::SEEN | Flag::DRAFT,
                                self.account_hash,
                            );
                            context.replies.push_back(UIEvent::Action(Tab(Kill(*u))));
                            return true;
                        }
                        _ => {}
                    }
                }
                self.set_dirty(true);
                self.mode = ViewMode::Edit;
                return true;
            }
            (ViewMode::Discard(_, ref mut selector), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            (
                ViewMode::WaitingForSendResult(ref selector, _, _, _),
                UIEvent::FinishedUIDialog(id, result),
            ) if selector.id() == *id => {
                if let Some(key) = result.downcast_mut::<char>() {
                    match key {
                        'c' => {
                            context
                                .replies
                                .push_back(UIEvent::Action(Tab(Kill(self.id))));
                            return true;
                        }
                        'n' => {
                            self.set_dirty(true);
                            if let ViewMode::WaitingForSendResult(_, handle, job_id, chan) =
                                std::mem::replace(&mut self.mode, ViewMode::Edit)
                            {
                                context.accounts[&self.account_hash].active_jobs.insert(
                                    job_id,
                                    JobRequest::SendMessageBackground(handle, chan),
                                );
                            }
                        }
                        _ => {}
                    }
                }
                return true;
            }
            (
                ViewMode::WaitingForSendResult(_, _, ref our_job_id, ref mut chan),
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)),
            ) if *our_job_id == *job_id => {
                let result = chan.try_recv().unwrap();
                if let Some(Err(err)) = result {
                    self.mode = ViewMode::Edit;
                    context.replies.push_back(UIEvent::Notification(
                        None,
                        err.to_string(),
                        Some(NotificationType::ERROR),
                    ));
                    self.set_dirty(true);
                } else {
                    context
                        .replies
                        .push_back(UIEvent::Action(Tab(Kill(self.id))));
                }
                return true;
            }
            (ViewMode::WaitingForSendResult(ref mut selector, _, _, _), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            _ => {}
        }
        if self.cursor == Cursor::Headers
            && self.mode.is_edit()
            && self.form.process_event(event, context)
        {
            if let UIEvent::InsertInput(_) = event {
                self.has_changes = true;
            }
            return true;
        }

        match *event {
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            /*
            /* Switch e-mail From: field to the `left` configured account. */
            UIEvent::Input(Key::Left) if self.cursor == Cursor::From => {
            self.draft.headers_mut().insert(
            "From".into(),
            get_display_name(context, self.account_hash),
            );
            self.dirty = true;
            return true;
            }
            /* Switch e-mail From: field to the `right` configured account. */
            UIEvent::Input(Key::Right) if self.cursor == Cursor::From => {
            if self.account_cursor + 1 < context.accounts.len() {
            self.account_cursor += 1;
            self.draft.headers_mut().insert(
            "From".into(),
            get_display_name(context, self.account_cursor),
            );
            self.dirty = true;
            }
            return true;
            }*/
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_up"]) =>
            {
                self.cursor = Cursor::Headers;
                self.form.process_event(event, context);
                self.dirty = true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_down"]) =>
            {
                self.cursor = Cursor::Body;
                self.dirty = true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["send_mail"])
                    && self.mode.is_edit() =>
            {
                self.update_draft();
                self.mode = ViewMode::Send(UIConfirmationDialog::new(
                    "send mail?",
                    vec![(true, "yes".to_string()), (false, "no".to_string())],
                    /* only one choice */
                    true,
                    Some(Box::new(move |id: ComponentId, result: bool| {
                        Some(UIEvent::FinishedUIDialog(id, Box::new(result)))
                    })),
                    context,
                ));
                return true;
            }
            UIEvent::EmbedInput((Key::Ctrl('z'), _)) => {
                self.embed.as_ref().unwrap().lock().unwrap().stop();
                match self.embed.take() {
                    Some(EmbedStatus::Running(e, f)) | Some(EmbedStatus::Stopped(e, f)) => {
                        self.embed = Some(EmbedStatus::Stopped(e, f));
                    }
                    _ => {}
                }
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.dirty = true;
            }
            UIEvent::EmbedInput((ref k, ref b)) => {
                use std::io::Write;
                if let Some(ref mut embed) = self.embed {
                    let mut embed_guard = embed.lock().unwrap();
                    if embed_guard.stdin.write_all(b).is_err() {
                        match embed_guard.is_active() {
                            Ok(WaitStatus::Exited(_, exit_code)) => {
                                drop(embed_guard);
                                if exit_code != 0 {
                                    context.replies.push_back(UIEvent::Notification(
                                        None,
                                        format!(
                                            "Subprocess has exited with exit code {}",
                                            exit_code
                                        ),
                                        Some(NotificationType::ERROR),
                                    ));
                                } else if let EmbedStatus::Running(_, f) = embed {
                                    let result = f.read_to_string();
                                    match Draft::from_str(result.as_str()) {
                                        Ok(mut new_draft) => {
                                            std::mem::swap(
                                                self.draft.attachments_mut(),
                                                new_draft.attachments_mut(),
                                            );
                                            if self.draft != new_draft {
                                                self.has_changes = true;
                                            }
                                            self.draft = new_draft;
                                        }
                                        Err(_) => {
                                            context.replies.push_back(UIEvent::Notification(
                                                    None,
                                                    "Could not parse draft headers correctly. The invalid text has been set as the body of your draft".to_string(),
                                                    Some(NotificationType::ERROR),
                                                    ));
                                            self.draft.set_body(result);
                                            self.has_changes = true;
                                        }
                                    }
                                    self.initialized = false;
                                }
                                self.embed = None;
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                            Ok(WaitStatus::Stopped(_, _)) => {
                                drop(embed_guard);
                                match self.embed.take() {
                                    Some(EmbedStatus::Running(e, f))
                                    | Some(EmbedStatus::Stopped(e, f)) => {
                                        self.embed = Some(EmbedStatus::Stopped(e, f));
                                    }
                                    _ => {}
                                }
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                                self.dirty = true;
                                return true;
                            }
                            Ok(WaitStatus::Continued(_)) | Ok(WaitStatus::StillAlive) => {
                                context
                                    .replies
                                    .push_back(UIEvent::EmbedInput((k.clone(), b.to_vec())));
                                return true;
                            }
                            e => {
                                context.replies.push_back(UIEvent::Notification(
                                    None,
                                    format!("Subprocess has exited with reason {:?}", e),
                                    Some(NotificationType::ERROR),
                                ));
                                drop(embed_guard);
                                self.embed = None;
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                        }
                    }
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if self.embed.is_some()
                    && shortcut!(key == shortcuts[Self::DESCRIPTION]["edit_mail"]) =>
            {
                self.embed.as_ref().unwrap().lock().unwrap().wake_up();
                match self.embed.take() {
                    Some(EmbedStatus::Running(e, f)) | Some(EmbedStatus::Stopped(e, f)) => {
                        self.embed = Some(EmbedStatus::Running(e, f));
                    }
                    _ => {}
                }
                self.mode = ViewMode::Embed;
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Embed));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Self::DESCRIPTION]["edit_mail"]) =>
            {
                /* Edit draft in $EDITOR */
                let editor = if let Some(editor_command) =
                    mailbox_acc_settings!(context[self.account_hash].composing.editor_command)
                        .as_ref()
                {
                    editor_command.to_string()
                } else {
                    match std::env::var("EDITOR") {
                        Err(e) => {
                            context.replies.push_back(UIEvent::Notification(
                            Some(e.to_string()),
                            "$EDITOR is not set. You can change an envvar's value with setenv or set composing.editor_command setting in your configuration.".to_string(),
                            Some(NotificationType::ERROR),
                        ));
                            return true;
                        }
                        Ok(v) => v,
                    }
                };
                /* update Draft's headers based on form values */
                self.update_draft();
                let f = create_temp_file(
                    self.draft.to_string().unwrap().as_str().as_bytes(),
                    None,
                    None,
                    true,
                );

                if *mailbox_acc_settings!(context[self.account_hash].composing.embed) {
                    self.embed = Some(EmbedStatus::Running(
                        crate::terminal::embed::create_pty(
                            width!(self.embed_area),
                            height!(self.embed_area),
                            [editor, f.path().display().to_string()].join(" "),
                        )
                        .unwrap(),
                        f,
                    ));
                    self.dirty = true;
                    context
                        .replies
                        .push_back(UIEvent::ChangeMode(UIMode::Embed));
                    context.replies.push_back(UIEvent::Fork(ForkType::Embed(
                        self.embed.as_ref().unwrap().lock().unwrap().child_pid,
                    )));
                    self.mode = ViewMode::Embed;
                    return true;
                }
                use std::process::{Command, Stdio};
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    context.input_kill();
                }

                let editor_command = format!("{} {}", editor, f.path().display());
                log(
                    format!(
                        "Executing: sh -c \"{}\"",
                        editor_command.replace("\"", "\\\"")
                    ),
                    DEBUG,
                );
                match Command::new("sh")
                    .args(&["-c", &editor_command])
                    .stdin(Stdio::inherit())
                    .stdout(Stdio::inherit())
                    .spawn()
                {
                    Ok(mut child) => {
                        let _ = child.wait();
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some(format!("Failed to execute {}: {}", editor, err)),
                            err.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                        context.restore_input();
                        return true;
                    }
                }
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                let result = f.read_to_string();
                match Draft::from_str(result.as_str()) {
                    Ok(mut new_draft) => {
                        std::mem::swap(self.draft.attachments_mut(), new_draft.attachments_mut());
                        if self.draft != new_draft {
                            self.has_changes = true;
                        }
                        self.draft = new_draft;
                    }
                    Err(_) => {
                        context.replies.push_back(UIEvent::Notification(
                                                    None,
                                                    "Could not parse draft headers correctly. The invalid text has been set as the body of your draft".to_string(),
                                                    Some(NotificationType::ERROR),
                                                    ));
                        self.draft.set_body(result);
                        self.has_changes = true;
                    }
                }
                self.initialized = false;
                self.dirty = true;
                return true;
            }
            UIEvent::Action(ref a) => {
                match a {
                    Action::Compose(ComposeAction::AddAttachmentPipe(ref command)) => {
                        if command.is_empty() {
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                format!("pipe command value is invalid: {}", command),
                                Some(NotificationType::ERROR),
                            ));
                            return false;
                        }
                        let f = create_temp_file(&[], None, None, true);
                        match std::process::Command::new("sh")
                            .args(&["-c", command])
                            .stdin(std::process::Stdio::null())
                            .stdout(std::process::Stdio::from(f.file()))
                            .spawn()
                        {
                            Ok(child) => {
                                let _ = child
                                    .wait_with_output()
                                    .expect("failed to launch command")
                                    .stdout;
                                let mut attachment =
                                    match melib::email::attachment_from_file(f.path()) {
                                        Ok(a) => a,
                                        Err(e) => {
                                            context.replies.push_back(UIEvent::Notification(
                                                Some("could not add attachment".to_string()),
                                                e.to_string(),
                                                Some(NotificationType::ERROR),
                                            ));
                                            self.dirty = true;
                                            return true;
                                        }
                                    };
                                if let Ok(mime_type) = query_mime_info(f.path()) {
                                    match attachment.content_type {
                                        ContentType::Other { ref mut tag, .. } => {
                                            *tag = mime_type;
                                        }
                                        _ => {}
                                    }
                                }
                                self.draft.attachments_mut().push(attachment);
                                self.dirty = true;
                                return true;
                            }
                            Err(err) => {
                                context.replies.push_back(UIEvent::Notification(
                                    None,
                                    format!("could not execute pipe command {}: {}", command, err),
                                    Some(NotificationType::ERROR),
                                ));
                                return true;
                            }
                        }
                    }
                    Action::Compose(ComposeAction::AddAttachment(ref path)) => {
                        let mut attachment = match melib::email::attachment_from_file(path) {
                            Ok(a) => a,
                            Err(e) => {
                                context.replies.push_back(UIEvent::Notification(
                                    Some("could not add attachment".to_string()),
                                    e.to_string(),
                                    Some(NotificationType::ERROR),
                                ));
                                self.dirty = true;
                                return true;
                            }
                        };
                        if let Ok(mime_type) = query_mime_info(path) {
                            match attachment.content_type {
                                ContentType::Other { ref mut tag, .. } => {
                                    *tag = mime_type;
                                }
                                _ => {}
                            }
                        }
                        self.draft.attachments_mut().push(attachment);
                        self.dirty = true;
                        return true;
                    }
                    Action::Compose(ComposeAction::RemoveAttachment(idx)) => {
                        if *idx + 1 > self.draft.attachments().len() {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(
                                    "attachment with given index does not exist".to_string(),
                                ),
                            ));
                            self.dirty = true;
                            return true;
                        }
                        self.draft.attachments_mut().remove(*idx);
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage("attachment removed".to_string()),
                        ));
                        self.dirty = true;
                        return true;
                    }
                    Action::Compose(ComposeAction::SaveDraft) => {
                        save_draft(
                            self.draft.clone().finalise().unwrap().as_bytes(),
                            context,
                            SpecialUsageMailbox::Drafts,
                            Flag::SEEN | Flag::DRAFT,
                            self.account_hash,
                        );
                        return true;
                    }
                    Action::Compose(ComposeAction::ToggleSign) => {
                        let is_true = self.sign_mail.is_true();
                        self.sign_mail = ToggleFlag::from(!is_true);
                        self.dirty = true;
                        return true;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        match self.mode {
            ViewMode::Embed => true,
            _ => self.dirty || self.pager.is_dirty() || self.form.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.pager.set_dirty(value);
        self.form.set_dirty(value);
    }

    fn kill(&mut self, uuid: Uuid, context: &mut Context) {
        if self.id != uuid {
            return;
        }

        if !self.has_changes {
            context.replies.push_back(UIEvent::Action(Tab(Kill(uuid))));
            return;
        }

        self.mode = ViewMode::Discard(
            uuid,
            UIDialog::new(
                "this draft has unsaved changes",
                vec![
                    ('x', "quit without saving".to_string()),
                    ('y', "save draft and quit".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
                Some(Box::new(move |id: ComponentId, results: &[char]| {
                    Some(UIEvent::FinishedUIDialog(
                        id,
                        Box::new(results.get(0).map(|c| *c).unwrap_or('n')),
                    ))
                })),
                context,
            ),
        );
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.mode.is_edit() {
            self.pager.get_shortcuts(context)
        } else {
            Default::default()
        };

        let our_map: ShortcutMap =
            mailbox_acc_settings!(context[self.account_hash].shortcuts.composing).key_values();
        map.insert(Composer::DESCRIPTION, our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if !self.has_changes {
            return true;
        }

        let id = self.id;
        /* Play it safe and ask user for confirmation */
        self.mode = ViewMode::Discard(
            id,
            UIDialog::new(
                "this draft has unsaved changes",
                vec![
                    ('x', "quit without saving".to_string()),
                    ('y', "save draft and quit".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
                Some(Box::new(move |id: ComponentId, results: &[char]| {
                    Some(UIEvent::FinishedUIDialog(
                        id,
                        Box::new(results.get(0).map(|c| *c).unwrap_or('n')),
                    ))
                })),
                context,
            ),
        );
        self.set_dirty(true);
        false
    }
}

pub fn send_draft(
    sign_mail: ToggleFlag,
    context: &mut Context,
    account_hash: AccountHash,
    mut draft: Draft,
    mailbox_type: SpecialUsageMailbox,
    flags: Flag,
    complete_in_background: bool,
) -> Result<Option<(JobId, JoinHandle, JobChannel<()>)>> {
    let format_flowed = *mailbox_acc_settings!(context[account_hash].composing.format_flowed);
    if sign_mail.is_true() {
        let mut content_type = ContentType::default();
        if format_flowed {
            if let ContentType::Text {
                ref mut parameters, ..
            } = content_type
            {
                parameters.push((b"format".to_vec(), b"flowed".to_vec()));
            }
        }

        let mut body: AttachmentBuilder = Attachment::new(
            content_type,
            Default::default(),
            std::mem::replace(&mut draft.body, String::new()).into_bytes(),
        )
        .into();
        if !draft.attachments.is_empty() {
            let mut parts = std::mem::replace(&mut draft.attachments, Vec::new());
            parts.insert(0, body);
            let boundary = ContentType::make_boundary(&parts);
            body = Attachment::new(
                ContentType::Multipart {
                    boundary: boundary.into_bytes(),
                    kind: MultipartType::Mixed,
                    parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                },
                Default::default(),
                Vec::new(),
            )
            .into();
        }
        let output = crate::components::mail::pgp::sign(
            body.into(),
            mailbox_acc_settings!(context[account_hash].pgp.gpg_binary)
                .as_ref()
                .map(|s| s.as_str()),
            mailbox_acc_settings!(context[account_hash].pgp.key)
                .as_ref()
                .map(|s| s.as_str()),
        );
        match output {
            Err(err) => {
                debug!("{:?} could not sign draft msg", err);
                log(
                    format!(
                        "Could not sign draft in account `{}`: {}.",
                        context.accounts[&account_hash].name(),
                        err.to_string()
                    ),
                    ERROR,
                );
                context.replies.push_back(UIEvent::Notification(
                    Some(format!(
                        "Could not sign draft in account `{}`.",
                        context.accounts[&account_hash].name()
                    )),
                    err.to_string(),
                    Some(NotificationType::ERROR),
                ));
                return Err(err);
            }
            Ok(output) => {
                draft.attachments.push(output);
            }
        }
    } else {
        let mut content_type = ContentType::default();
        if format_flowed {
            if let ContentType::Text {
                ref mut parameters, ..
            } = content_type
            {
                parameters.push((b"format".to_vec(), b"flowed".to_vec()));
            }

            let body: AttachmentBuilder = Attachment::new(
                content_type,
                Default::default(),
                std::mem::replace(&mut draft.body, String::new()).into_bytes(),
            )
            .into();
            draft.attachments.insert(0, body);
        }
    }
    let bytes = draft.finalise().unwrap();
    let send_mail = mailbox_acc_settings!(context[account_hash].composing.send_mail).clone();
    let ret =
        context.accounts[&account_hash].send(bytes.clone(), send_mail, complete_in_background);
    save_draft(bytes.as_bytes(), context, mailbox_type, flags, account_hash);
    ret
}

pub fn save_draft(
    bytes: &[u8],
    context: &mut Context,
    mailbox_type: SpecialUsageMailbox,
    flags: Flag,
    account_hash: AccountHash,
) {
    match context.accounts[&account_hash].save_special(bytes, mailbox_type, flags) {
        Err(MeliError {
            summary, details, ..
        }) => {
            context.replies.push_back(UIEvent::Notification(
                summary.map(|s| s.into()),
                details.into(),
                Some(NotificationType::ERROR),
            ));
        }
        Ok(mailbox_hash) => {
            context.replies.push_back(UIEvent::Notification(
                Some("Message saved".into()),
                format!(
                    "Message saved in `{}`",
                    &context.accounts[&account_hash].mailbox_entries[&mailbox_hash].name
                ),
                Some(NotificationType::INFO),
            ));
        }
    }
}
