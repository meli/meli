/*
 * meli - ui crate
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

use melib::Draft;
use mime_apps::query_mime_info;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum Cursor {
    Headers,
    Body,
    //Attachments,
}

#[derive(Debug)]
pub struct Composer {
    reply_context: Option<((usize, usize), Box<ThreadView>)>, // (folder_index, thread_node_index)
    account_cursor: usize,

    cursor: Cursor,

    pager: Pager,
    draft: Draft,
    form: FormWidget,

    mode: ViewMode,
    sign_mail: ToggleFlag,
    dirty: bool,
    initialized: bool,
    id: ComponentId,
}

impl Default for Composer {
    fn default() -> Self {
        Composer {
            reply_context: None,
            account_cursor: 0,

            cursor: Cursor::Headers,

            pager: Pager::default(),
            draft: Draft::default(),
            form: FormWidget::default(),

            mode: ViewMode::Edit,
            sign_mail: ToggleFlag::Unset,
            dirty: true,
            initialized: false,
            id: ComponentId::new_v4(),
        }
    }
}

#[derive(Debug)]
enum ViewMode {
    Discard(Uuid, Selector<char>),
    Edit,
    //Selector(Selector),
    ThreadView,
}

impl ViewMode {
    fn is_edit(&self) -> bool {
        if let ViewMode::Edit = self {
            true
        } else {
            false
        }
    }

    fn is_threadview(&self) -> bool {
        if let ViewMode::ThreadView = self {
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
            write!(f, "compose")
        }
    }
}

impl Composer {
    const DESCRIPTION: &'static str = "compose";
    pub fn new(account_cursor: usize) -> Self {
        Composer {
            account_cursor,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    /*
     * coordinates: (account index, mailbox index, root set thread_node index)
     * msg: index of message we reply to in thread_nodes
     * context: current context
     */
    pub fn edit(account_pos: usize, h: EnvelopeHash, context: &Context) -> Result<Self> {
        let mut ret = Composer::default();
        let op = context.accounts[account_pos].operation(h);
        let envelope: &Envelope = context.accounts[account_pos].get_env(&h);

        ret.draft = Draft::edit(envelope, op)?;

        ret.account_cursor = account_pos;
        Ok(ret)
    }
    pub fn with_context(
        coordinates: (usize, usize, usize),
        msg: ThreadHash,
        context: &Context,
    ) -> Self {
        let account = &context.accounts[coordinates.0];
        let mailbox = &account[coordinates.1].unwrap();
        let threads = &account.collection.threads[&mailbox.folder.hash()];
        let thread_nodes = &threads.thread_nodes();
        let mut ret = Composer::default();
        let p = &thread_nodes[&msg];
        let parent_message = &account.collection[&p.message().unwrap()];
        let mut op = account.operation(parent_message.hash());
        let parent_bytes = op.as_bytes();

        ret.draft = Draft::new_reply(parent_message, parent_bytes.unwrap());
        ret.draft.headers_mut().insert(
            "Subject".into(),
            if p.show_subject() {
                format!(
                    "Re: {}",
                    account.get_env(&p.message().unwrap()).subject().clone()
                )
            } else {
                account.get_env(&p.message().unwrap()).subject().into()
            },
        );

        ret.account_cursor = coordinates.0;
        ret.reply_context = Some((
            (coordinates.1, coordinates.2),
            Box::new(ThreadView::new(coordinates, Some(msg), context)),
        ));
        ret
    }

    pub fn set_draft(&mut self, draft: Draft) {
        self.draft = draft;
        self.update_form();
    }

    fn update_draft(&mut self) {
        let header_values = self.form.values_mut();
        let draft_header_map = self.draft.headers_mut();
        /* avoid extra allocations by updating values instead of inserting */
        for (k, v) in draft_header_map.iter_mut() {
            if let Some(vn) = header_values.remove(k) {
                std::mem::swap(v, &mut vn.into_string());
            }
        }
    }

    fn update_form(&mut self) {
        let old_cursor = self.form.cursor();
        self.form = FormWidget::new("Save".into());
        self.form.hide_buttons();
        self.form.set_cursor(old_cursor);
        let headers = self.draft.headers();
        let account_cursor = self.account_cursor;
        for &k in &["Date", "From", "To", "Cc", "Bcc", "Subject"] {
            if k == "To" || k == "Cc" || k == "Bcc" {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, term| {
                        let book: &AddressBook = &c.accounts[account_cursor].address_book;
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
        if self.sign_mail.is_true() {
            write_string_to_grid(
                &format!(
                    "☑ sign with {}",
                    context
                        .settings
                        .pgp
                        .key
                        .as_ref()
                        .map(String::as_str)
                        .unwrap_or("default key")
                ),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                false,
            );
        } else {
            write_string_to_grid(
                "☐ don't sign",
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                false,
            );
        }
        if attachments_no == 0 {
            write_string_to_grid(
                "no attachments",
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left!(area), (0, 2)), bottom_right!(area)),
                false,
            );
        } else {
            write_string_to_grid(
                &format!("{} attachments ", attachments_no),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left!(area), (0, 2)), bottom_right!(area)),
                false,
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
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        (pos_inc(upper_left!(area), (0, 3 + i)), bottom_right!(area)),
                        false,
                    );
                } else {
                    write_string_to_grid(
                        &format!("[{}] {} {} bytes", i, a.content_type(), a.raw.len()),
                        grid,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        (pos_inc(upper_left!(area), (0, 3 + i)), bottom_right!(area)),
                        false,
                    );
                }
            }
        }
    }
}

impl Component for Composer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area);
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let upper_left = set_y(upper_left, get_y(upper_left) + 1);

        if height!(area) < 4 {
            return;
        }

        let width = if width!(area) > 80 && self.reply_context.is_some() {
            width!(area) / 2
        } else {
            width!(area)
        };

        if !self.initialized {
            if self.sign_mail.is_unset() {
                self.sign_mail = ToggleFlag::InternalVal(context.settings.pgp.auto_sign);
            }
            if !self.draft.headers().contains_key("From") || self.draft.headers()["From"].is_empty()
            {
                self.draft.headers_mut().insert(
                    "From".into(),
                    crate::components::mail::get_display_name(context, self.account_cursor),
                );
            }
            self.pager.update_from_str(self.draft.body(), Some(77));
            self.update_form();
            self.initialized = true;
        }
        let header_height = self.form.len();

        let mid = if width > 80 {
            let width = width - 80;
            let mid = if self.reply_context.is_some() {
                get_x(upper_left) + width!(area) / 2
            } else {
                width / 2
            };

            if self.reply_context.is_some() {
                for i in get_y(upper_left) - 1..=get_y(bottom_right) {
                    //set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
                //grid[set_x(bottom_right, mid)].set_ch(VERT_BOUNDARY); // Enforce full vert bar at the bottom
                grid[set_x(bottom_right, mid)].set_fg(Color::Byte(240));
            }

            if self.dirty {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    //set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                    //set_and_join_box(grid, (mid + 80, i), VERT_BOUNDARY);
                    grid[(mid + 80, i)].set_fg(Color::Default);
                    grid[(mid + 80, i)].set_bg(Color::Default);
                }
            }
            mid
        } else {
            0
        };

        if width > 80 && self.reply_context.is_some() {
            let area = (pos_dec(upper_left, (0, 1)), set_x(bottom_right, mid - 1));
            let view = &mut self.reply_context.as_mut().unwrap().1;
            view.set_dirty();
            view.draw(grid, area, context);
        }

        let header_area = if self.reply_context.is_some() {
            (
                set_x(upper_left, mid + 1),
                set_y(bottom_right, get_y(upper_left) + header_height),
            )
        } else {
            (
                set_x(upper_left, mid + 1),
                (
                    get_x(bottom_right).saturating_sub(mid),
                    get_y(upper_left) + header_height,
                ),
            )
        };
        let attachments_no = self.draft.attachments().len();
        let attachment_area = if self.reply_context.is_some() {
            (
                (mid + 1, get_y(bottom_right) - 2 - attachments_no),
                bottom_right,
            )
        } else {
            (
                (mid + 1, get_y(bottom_right) - 2 - attachments_no),
                pos_dec(bottom_right, (mid, 0)),
            )
        };

        let body_area = if self.reply_context.is_some() {
            (
                (mid + 1, get_y(upper_left) + header_height + 1),
                set_y(bottom_right, get_y(bottom_right) - 3 - attachments_no),
            )
        } else {
            (
                pos_inc(upper_left, (mid + 1, header_height + 1)),
                pos_dec(bottom_right, (mid, 3 + attachments_no)),
            )
        };

        let (x, y) = write_string_to_grid(
            if self.reply_context.is_some() {
                "COMPOSING REPLY"
            } else {
                "COMPOSING MESSAGE"
            },
            grid,
            Color::Byte(189),
            Color::Byte(167),
            Attr::Default,
            (
                pos_dec(upper_left!(header_area), (0, 1)),
                bottom_right!(header_area),
            ),
            false,
        );
        change_colors(
            grid,
            (
                set_x(pos_dec(upper_left!(header_area), (0, 1)), x),
                set_y(bottom_right!(header_area), y),
            ),
            Color::Byte(189),
            Color::Byte(167),
        );

        /* Regardless of view mode, do the following */
        self.form.draw(grid, header_area, context);

        match self.mode {
            ViewMode::ThreadView | ViewMode::Edit => {
                self.pager.set_dirty();
                self.pager.draw(grid, body_area, context);
            }
            ViewMode::Discard(_, ref mut s) => {
                self.pager.set_dirty();
                self.pager.draw(grid, body_area, context);
                /* Let user choose whether to quit with/without saving or cancel */
                s.draw(grid, center_area(area, s.content.size()), context);
            }
        }

        self.draw_attachments(grid, attachment_area, context);
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match (&mut self.mode, &mut self.reply_context, &event) {
            // don't pass Reply command to thread view in reply_context
            (_, _, UIEvent::Input(Key::Char('R'))) => {}
            (ViewMode::ThreadView, Some((_, ref mut view)), _) => {
                if view.process_event(event, context) {
                    self.dirty = true;
                    return true;
                }
                /* Cannot mutably borrow in pattern guard, pah! */
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::ThreadView, _, _) => {
                /* Cannot mutably borrow in pattern guard, pah! */
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::Discard(_, ref mut selector), _, _) => {
                if selector.process_event(event, context) {
                    if selector.is_done() {
                        let (u, s) = match std::mem::replace(&mut self.mode, ViewMode::ThreadView) {
                            ViewMode::Discard(u, s) => (u, s),
                            _ => unreachable!(),
                        };
                        let key = s.collect()[0] as char;
                        match key {
                            'x' => {
                                context.replies.push_back(UIEvent::Action(Tab(Kill(u))));
                                return true;
                            }
                            'n' => {}
                            'y' => {
                                let mut failure = true;
                                let draft = std::mem::replace(&mut self.draft, Draft::default());

                                let draft = draft.finalise().unwrap();
                                for folder in &[
                                    &context.accounts[self.account_cursor]
                                        .special_use_folder(SpecialUseMailbox::Drafts),
                                    &context.accounts[self.account_cursor]
                                        .special_use_folder(SpecialUseMailbox::Inbox),
                                    &context.accounts[self.account_cursor]
                                        .special_use_folder(SpecialUseMailbox::Normal),
                                ] {
                                    if folder.is_none() {
                                        continue;
                                    }
                                    let folder = folder.unwrap();
                                    if let Err(e) = context.accounts[self.account_cursor].save(
                                        draft.as_bytes(),
                                        folder,
                                        Some(Flag::SEEN | Flag::DRAFT),
                                    ) {
                                        debug!("{:?} could not save draft msg", e);
                                        log(
                                            format!(
                                                "Could not save draft in '{}' folder: {}.",
                                                folder,
                                                e.to_string()
                                            ),
                                            ERROR,
                                        );
                                        context.replies.push_back(UIEvent::Notification(
                                            Some(format!(
                                                "Could not save draft in '{}' folder.",
                                                folder
                                            )),
                                            e.into(),
                                            Some(NotificationType::ERROR),
                                        ));
                                    } else {
                                        failure = false;
                                        break;
                                    }
                                }

                                if failure {
                                    let file =
                                        create_temp_file(draft.as_bytes(), None, None, false);
                                    debug!("message saved in {}", file.path.display());
                                    log(
                                        format!(
                                    "Message was stored in {} so that you can restore it manually.",
                                    file.path.display()
                                ),
                                        INFO,
                                    );
                                    context.replies.push_back(UIEvent::Notification(
                                        Some("Could not save in any folder".into()),
                                        format!(
                                    "Message was stored in {} so that you can restore it manually.",
                                    file.path.display()
                                ),
                                        Some(NotificationType::INFO),
                                    ));
                                }
                                context.replies.push_back(UIEvent::Action(Tab(Kill(u))));
                                return true;
                            }
                            _ => {}
                        }
                        self.set_dirty();
                    }
                    return true;
                }
            }
            _ => {}
        }
        if self.form.process_event(event, context) {
            return true;
        }

        match *event {
            UIEvent::Resize => {
                self.set_dirty();
            }
            /*
            /* Switch e-mail From: field to the `left` configured account. */
            UIEvent::Input(Key::Left) if self.cursor == Cursor::From => {
            self.account_cursor = self.account_cursor.saturating_sub(1);
            self.draft.headers_mut().insert(
            "From".into(),
            get_display_name(context, self.account_cursor),
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
            UIEvent::Input(Key::Up) => {
                self.cursor = Cursor::Headers;
            }
            UIEvent::Input(Key::Down) => {
                self.cursor = Cursor::Body;
            }
            /* Switch to thread view mode if we're on Edit mode */
            UIEvent::Input(Key::Char('v')) if self.mode.is_edit() => {
                self.mode = ViewMode::ThreadView;
                self.set_dirty();
                return true;
            }
            /* Switch to Edit mode if we're on ThreadView mode */
            UIEvent::Input(Key::Char('o')) if self.mode.is_threadview() => {
                self.mode = ViewMode::Edit;
                self.set_dirty();
                return true;
            }
            UIEvent::Input(Key::Char('s')) => {
                self.update_draft();
                if send_draft(
                    self.sign_mail,
                    context,
                    self.account_cursor,
                    self.draft.clone(),
                ) {
                    context
                        .replies
                        .push_back(UIEvent::Action(Tab(Kill(self.id))));
                }
                return true;
            }
            UIEvent::Input(Key::Char('e')) => {
                /* Edit draft in $EDITOR */
                use std::process::{Command, Stdio};
                let settings = &context.settings;
                let editor = if let Some(editor_cmd) = settings.composing.editor_cmd.as_ref() {
                    editor_cmd.to_string()
                } else {
                    match std::env::var("EDITOR") {
                        Err(e) => {
                            context.replies.push_back(UIEvent::Notification(
                            Some(e.to_string()),
                            "$EDITOR is not set. You can change an envvar's value with setenv or set composing.editor_cmd setting in your configuration.".to_string(),
                            Some(NotificationType::ERROR),
                        ));
                            return true;
                        }
                        Ok(v) => v,
                    }
                };
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    context.input_kill();
                }
                /* update Draft's headers based on form values */
                self.update_draft();
                let f = create_temp_file(
                    self.draft.to_string().unwrap().as_str().as_bytes(),
                    None,
                    None,
                    true,
                );

                let parts = split_command!(editor);
                let (cmd, args) = (parts[0], &parts[1..]);
                if let Err(e) = Command::new(cmd)
                    .args(args)
                    .arg(&f.path())
                    .stdin(Stdio::inherit())
                    .stdout(Stdio::inherit())
                    .output()
                {
                    context.replies.push_back(UIEvent::Notification(
                        Some(format!("Failed to execute {}", editor)),
                        e.to_string(),
                        Some(NotificationType::ERROR),
                    ));
                    context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                    context.restore_input();
                    return true;
                }
                let result = f.read_to_string();
                let mut new_draft = Draft::from_str(result.as_str()).unwrap();
                std::mem::swap(self.draft.attachments_mut(), new_draft.attachments_mut());
                self.draft = new_draft;
                self.initialized = false;
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                context.restore_input();
                self.dirty = true;
                return true;
            }
            UIEvent::Action(ref a) => {
                match a {
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
        self.dirty
            || self.pager.is_dirty()
            || self
                .reply_context
                .as_ref()
                .map(|(_, p)| p.is_dirty())
                .unwrap_or(false)
            || self.form.is_dirty()
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
        self.pager.set_dirty();
        self.form.set_dirty();
        if let Some((_, ref mut view)) = self.reply_context {
            view.set_dirty();
        }
    }

    fn kill(&mut self, uuid: Uuid, _context: &mut Context) {
        self.mode = ViewMode::Discard(
            uuid,
            Selector::new(
                "this draft has unsaved changes",
                vec![
                    ('x', "quit without saving".to_string()),
                    ('y', "save draft and quit".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
            ),
        );
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.mode.is_threadview() {
            self.pager.get_shortcuts(context)
        } else {
            Default::default()
        };

        if let Some((_, ref view)) = self.reply_context {
            map.extend(view.get_shortcuts(context));
        }

        let mut our_map: ShortcutMap = Default::default();
        if self.mode.is_threadview() {
            our_map.insert("Switch to right panel (draft editing).", Key::Char('o'));
        }
        if self.mode.is_edit() && self.reply_context.is_some() {
            our_map.insert("Switch to left panel (thread view)", Key::Char('v'));
        }
        our_map.insert("Deliver draft to mailer.", Key::Char('s'));
        our_map.insert("Edit in $EDITOR", Key::Char('e'));
        map.insert(Composer::DESCRIPTION.to_string(), our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn can_quit_cleanly(&mut self) -> bool {
        /* Play it safe and ask user for confirmation */
        self.mode = ViewMode::Discard(
            self.id,
            Selector::new(
                "this draft has unsaved changes",
                vec![
                    ('x', "quit without saving".to_string()),
                    ('y', "save draft and quit".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
            ),
        );
        self.set_dirty();
        false
    }
}

pub fn send_draft(
    sign_mail: ToggleFlag,
    context: &mut Context,
    account_cursor: usize,
    mut draft: Draft,
) -> bool {
    use std::io::Write;
    use std::process::{Command, Stdio};
    let mut failure = true;
    let settings = &context.settings;
    let parts = split_command!(settings.composing.mailer_cmd);
    let (cmd, args) = (parts[0], &parts[1..]);
    let mut msmtp = Command::new(cmd)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to start mailer command");
    {
        let stdin = msmtp.stdin.as_mut().expect("failed to open stdin");
        if sign_mail.is_true() {
            let mut body: AttachmentBuilder = Attachment::new(
                Default::default(),
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
                context.settings.pgp.gpg_binary.as_ref().map(String::as_str),
                context.settings.pgp.key.as_ref().map(String::as_str),
            );
            if let Err(e) = &output {
                debug!("{:?} could not sign draft msg", e);
                log(
                    format!(
                        "Could not sign draft in account `{}`: {}.",
                        context.accounts[account_cursor].name(),
                        e.to_string()
                    ),
                    ERROR,
                );
                context.replies.push_back(UIEvent::Notification(
                    Some(format!(
                        "Could not sign draft in account `{}`.",
                        context.accounts[account_cursor].name()
                    )),
                    e.to_string(),
                    Some(NotificationType::ERROR),
                ));
                return false;
            }
            draft.attachments.push(output.unwrap());
        }
        let draft = draft.finalise().unwrap();
        stdin
            .write_all(draft.as_bytes())
            .expect("Failed to write to stdin");
        for folder in &[
            &context.accounts[account_cursor].special_use_folder(SpecialUseMailbox::Sent),
            &context.accounts[account_cursor].special_use_folder(SpecialUseMailbox::Inbox),
            &context.accounts[account_cursor].special_use_folder(SpecialUseMailbox::Normal),
        ] {
            if folder.is_none() {
                continue;
            }
            let folder = folder.unwrap();
            if let Err(e) =
                context.accounts[account_cursor].save(draft.as_bytes(), folder, Some(Flag::SEEN))
            {
                debug!("{:?} could not save sent msg", e);
                log(
                    format!("Could not save in '{}' folder: {}.", folder, e.to_string()),
                    ERROR,
                );
                context.replies.push_back(UIEvent::Notification(
                    Some(format!("Could not save in '{}' folder.", folder)),
                    e.into(),
                    Some(NotificationType::ERROR),
                ));
            } else {
                failure = false;
                break;
            }
        }

        if failure {
            let file = create_temp_file(draft.as_bytes(), None, None, false);
            debug!("message saved in {}", file.path.display());
            log(
                format!(
                    "Message was stored in {} so that you can restore it manually.",
                    file.path.display()
                ),
                INFO,
            );
            context.replies.push_back(UIEvent::Notification(
                Some("Could not save in any folder".into()),
                format!(
                    "Message was stored in {} so that you can restore it manually.",
                    file.path.display()
                ),
                Some(NotificationType::INFO),
            ));
        }
    }
    let output = msmtp.wait().expect("Failed to wait on mailer");
    if output.success() {
        context.replies.push_back(UIEvent::Notification(
            Some("Sent.".into()),
            String::new(),
            None,
        ));
    } else {
        if let Some(exit_code) = output.code() {
            log(
                format!(
                    "Could not send e-mail using `{}`: Process exited with {}",
                    cmd, exit_code
                ),
                ERROR,
            );
        } else {
            log(
                format!(
                    "Could not send e-mail using `{}`: Process was killed by signal",
                    cmd
                ),
                ERROR,
            );
        }
    }
    !failure
}
