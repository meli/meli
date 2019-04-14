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
            dirty: true,
            initialized: false,
            id: ComponentId::new_v4(),
        }
    }
}

#[derive(Debug)]
enum ViewMode {
    Discard(Uuid),
    Edit,
    //Selector(Selector),
    Overview,
}

impl ViewMode {
    fn is_discard(&self) -> bool {
        if let ViewMode::Discard(_) = self {
            true
        } else {
            false
        }
    }

    fn is_edit(&self) -> bool {
        if let ViewMode::Edit = self {
            true
        } else {
            false
        }
    }

    fn is_overview(&self) -> bool {
        if let ViewMode::Overview = self {
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
    pub fn edit(coordinates: (usize, usize, usize), msg: usize, context: &Context) -> Self {
        let mailbox = &context.accounts[coordinates.0][coordinates.1]
            .as_ref()
            .unwrap();
        let threads = &mailbox.collection.threads;
        let thread_nodes = &threads.thread_nodes();
        let mut ret = Composer::default();
        let message = &mailbox.collection[&thread_nodes[msg].message().unwrap()];
        let op = context.accounts[coordinates.0]
            .backend
            .operation(message.hash(), mailbox.folder.hash());

        ret.draft = Draft::edit(message, op);

        ret.account_cursor = coordinates.0;
        ret
    }
    pub fn with_context(coordinates: (usize, usize, usize), msg: usize, context: &Context) -> Self {
        let mailbox = &context.accounts[coordinates.0][coordinates.1]
            .as_ref()
            .unwrap();
        let threads = &mailbox.collection.threads;
        let thread_nodes = &threads.thread_nodes();
        let mut ret = Composer::default();
        let p = &thread_nodes[msg];
        let parent_message = &mailbox.collection[&p.message().unwrap()];
        let mut op = context.accounts[coordinates.0]
            .backend
            .operation(parent_message.hash(), mailbox.folder.hash());
        let parent_bytes = op.as_bytes();

        ret.draft = Draft::new_reply(parent_message, parent_bytes.unwrap());
        ret.draft.headers_mut().insert(
            "Subject".into(),
            if p.show_subject() {
                format!(
                    "Re: {}",
                    mailbox.collection[&p.message().unwrap()].subject().clone()
                )
            } else {
                mailbox.collection[&p.message().unwrap()].subject().into()
            },
        );

        ret.account_cursor = coordinates.0;
        ret.reply_context = Some((
            (coordinates.1, coordinates.2),
            Box::new(ThreadView::new(coordinates, Some(msg), context)),
        ));
        ret
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
                    }),
                ));
            } else {
                self.form.push((k.into(), headers[k].to_string()));
            }
        }
    }

    /*
    let (x, y) = if k == "From" {
        write_string_to_grid(
            "◀ ",
            grid,
            Color::Byte(251),
            Color::Default,
            ((x, y), set_y(bottom_right, y)),
            true,
        )
    } else {
        (x, y)
    };
    let (x, y) = write_string_to_grid(
        &headers[k],
        grid,
        Color::Default,
        bg_color,
        ((x, y), set_y(bottom_right, y)),
        true,
    );
    if k == "From" {
        write_string_to_grid(
            " ▶",
            grid,
            Color::Byte(251),
            Color::Default,
            ((x, y), set_y(bottom_right, y)),
            true,
        )
        */
}

impl Component for Composer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area);
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let upper_left = set_y(upper_left, get_y(upper_left) + 1);

        let width = if width!(area) > 80 && self.reply_context.is_some() {
            width!(area) / 2
        } else {
            width!(area)
        };

        if !self.initialized {
            if !self.draft.headers().contains_key("From") || self.draft.headers()["From"].is_empty()
            {
                self.draft.headers_mut().insert(
                    "From".into(),
                    get_display_name(context, self.account_cursor),
                );
            }
            self.pager.update_from_str(self.draft.body(), Some(77));
            self.update_form();
            self.initialized = true;
        }
        let header_height = self.form.len() + 1;

        let mid = if width > 80 {
            let width = width - 80;
            let mid = if self.reply_context.is_some() {
                get_x(upper_left) + width!(area) / 2
            } else {
                width / 2
            };

            if self.reply_context.is_some() {
                for i in get_y(upper_left) - 1..=get_y(bottom_right) {
                    set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
                grid[set_x(bottom_right, mid)].set_ch(VERT_BOUNDARY); // Enforce full vert bar at the bottom
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
                set_y(bottom_right, get_y(upper_left) + header_height + 1),
            )
        } else {
            (
                set_x(upper_left, mid + 1),
                (
                    get_x(bottom_right).saturating_sub(mid),
                    get_y(upper_left) + header_height + 1,
                ),
            )
        };
        let body_area = if self.reply_context.is_some() {
            (
                (mid + 1, get_y(upper_left) + header_height + 1),
                bottom_right,
            )
        } else {
            (
                pos_inc(upper_left, (mid + 1, header_height + 2)),
                pos_dec(bottom_right, (mid, 0)),
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
            ViewMode::Overview | ViewMode::Edit => {
                self.pager.set_dirty();
                self.pager.draw(grid, body_area, context);
            }
            ViewMode::Discard(_) => {
                /* Let user choose whether to quit with/without saving or cancel */
                let mid_x = { std::cmp::max(width!(area) / 2, width / 2) - width / 2 };
                let mid_y = { std::cmp::max(height!(area) / 2, 11) - 11 };

                let upper_left = upper_left!(body_area);
                let bottom_right = bottom_right!(body_area);
                let area = (
                    pos_inc(upper_left, (mid_x, mid_y)),
                    pos_dec(bottom_right, (mid_x, mid_y)),
                );
                create_box(grid, area);
                let area = (
                    pos_inc(upper_left, (mid_x + 2, mid_y + 2)),
                    pos_dec(
                        bottom_right,
                        (mid_x.saturating_sub(2), mid_y.saturating_sub(2)),
                    ),
                );

                let (_, y) = write_string_to_grid(
                    &format!("Draft \"{:10}\"", self.draft.headers()["Subject"]),
                    grid,
                    Color::Default,
                    Color::Default,
                    area,
                    true,
                );
                let (_, y) = write_string_to_grid(
                    "[x] quit without saving",
                    grid,
                    Color::Byte(124),
                    Color::Default,
                    (set_y(upper_left!(area), y + 2), bottom_right!(area)),
                    true,
                );
                let (_, y) = write_string_to_grid(
                    "[y] save draft and quit",
                    grid,
                    Color::Byte(124),
                    Color::Default,
                    (set_y(upper_left!(area), y + 1), bottom_right!(area)),
                    true,
                );
                write_string_to_grid(
                    "[n] cancel",
                    grid,
                    Color::Byte(124),
                    Color::Default,
                    (set_y(upper_left!(area), y + 1), bottom_right!(area)),
                    true,
                );
            }
        }

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match (&mut self.mode, &mut self.reply_context, &event) {
            // don't pass Reply command to thread view in reply_context
            (_, _, UIEvent::Input(Key::Char('R'))) => {}
            (ViewMode::Overview, Some((_, ref mut view)), _) => {
                if view.process_event(event, context) {
                    self.dirty = true;
                    return true;
                }
                /* Cannot mutably borrow in pattern guard, pah! */
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::Overview, _, _) => {
                /* Cannot mutably borrow in pattern guard, pah! */
                if self.pager.process_event(event, context) {
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
            UIEvent::Input(Key::Char(key)) if self.mode.is_discard() => {
                match (key, &self.mode) {
                    ('x', ViewMode::Discard(u)) => {
                        context.replies.push_back(UIEvent::Action(Tab(Kill(*u))));
                        return true;
                    }
                    ('n', _) => {}
                    ('y', ViewMode::Discard(u)) => {
                        let account = &context.accounts[self.account_cursor];
                        let draft = std::mem::replace(&mut self.draft, Draft::default());
                        if let Err(e) = account.save_draft(draft) {
                            if cfg!(debug_assertions) {
                                eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("{:?} could not save draft", e);
                            }
                            context.replies.push_back(UIEvent::Notification(
                                Some("Could not save draft.".into()),
                                e.into(),
                            ));
                        }
                        context.replies.push_back(UIEvent::Action(Tab(Kill(*u))));
                        return true;
                    }
                    _ => {
                        return false;
                    }
                }
                self.mode = ViewMode::Overview;
                self.set_dirty();
                return true;
            }
            /* Switch to Overview mode if we're on Edit mode */
            UIEvent::Input(Key::Char('v')) if self.mode.is_edit() => {
                self.mode = ViewMode::Overview;
                self.set_dirty();
                return true;
            }
            /* Switch to Edit mode if we're on Overview mode */
            UIEvent::Input(Key::Char('o')) if self.mode.is_overview() => {
                self.mode = ViewMode::Edit;
                self.set_dirty();
                return true;
            }
            UIEvent::Input(Key::Char('s')) if self.mode.is_overview() => {
                use std::io::Write;
                use std::process::{Command, Stdio};
                let settings = &context.settings;
                let parts = split_command!(settings.mailer.mailer_cmd);
                let (cmd, args) = (parts[0], &parts[1..]);
                let mut msmtp = Command::new(cmd)
                    .args(args)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("Failed to start mailer command");
                {
                    let mut stdin = msmtp.stdin.as_mut().expect("failed to open stdin");
                    let draft = self.draft.clone().finalise().unwrap();
                    stdin
                        .write_all(draft.as_bytes())
                        .expect("Failed to write to stdin");
                    if let Err(e) = context.accounts[self.account_cursor].save(
                        draft.as_bytes(),
                        &context.accounts[self.account_cursor]
                            .settings
                            .conf()
                            .sent_folder(),
                    ) {
                        if cfg!(debug_assertions) {
                            eprint!("{}:{}_{}:	", file!(), line!(), column!());
eprintln!("{:?} could not save sent msg", e);
                        }
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not save in 'Sent' folder.".into()),
                            e.into(),
                        ));
                    }
                }
                context.replies.push_back(UIEvent::Notification(
                    Some("Sent.".into()),
                    format!(
                        "Mailer output: {:#?}",
                        msmtp
                            .wait_with_output()
                            .expect("Failed to wait on filter")
                            .stdout
                    ),
                ));
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(Kill(self.id))));
                return true;
            }
            UIEvent::Input(Key::Char('e')) if self.cursor == Cursor::Body => {
                /* Edit draft in $EDITOR */
                use std::process::{Command, Stdio};
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    context.input_kill();
                }
                /* update Draft's headers based on form values */
                self.update_draft();
                let mut f =
                    create_temp_file(self.draft.to_string().unwrap().as_str().as_bytes(), None);
                //let mut f = Box::new(std::fs::File::create(&dir).unwrap());

                // TODO: check exit status
                Command::new("vim")
                    .arg("+/^$")
                    .arg(&f.path())
                    .stdin(Stdio::inherit())
                    .stdout(Stdio::inherit())
                    .output()
                    .expect("failed to execute process");
                let result = f.read_to_string();
                self.draft = Draft::from_str(result.as_str()).unwrap();
                self.initialized = false;
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                context.restore_input();
                /*

                Cursor::To | Cursor::Cc | Cursor::Bcc => {
                    let account = &context.accounts[self.account_cursor];
                    let mut entries = account.address_book.values().map(|v| (v.id().as_bytes().to_vec(), v.email().to_string())).collect();
                    self.mode = ViewMode::Selector(Selector::new(entries, true));
                },
                Cursor::Attachments => {
                    unimplemented!()
                },
                Cursor::From => {
                    return true;
                }
                */
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

    fn kill(&mut self, uuid: Uuid) {
        self.mode = ViewMode::Discard(uuid);
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMap {
        let mut map = if self.mode.is_overview() {
            self.pager.get_shortcuts(context)
        } else {
            Default::default()
        };

        if let Some((_, ref view)) = self.reply_context {
            map.extend(view.get_shortcuts(context));
            map.remove("reply");
        }

        if self.mode.is_overview() {
            map.insert("Switch to edit mode.", Key::Char('o'));
            map.insert("Deliver draft to mailer.", Key::Char('s'));
        }
        if self.mode.is_edit() {
            map.insert("Switch to overview", Key::Char('v'));
        }
        map.insert("Edit in $EDITOR", Key::Char('e'));

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

fn get_display_name(context: &Context, idx: usize) -> String {
    let settings = context.accounts[idx].runtime_settings.account();
    if let Some(d) = settings.display_name.as_ref() {
        format!("{} <{}>", d, settings.identity)
    } else {
        settings.identity.to_string()
    }
}
