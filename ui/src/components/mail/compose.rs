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

#[derive(Debug)]
pub struct Composer {
    reply_context: Option<((usize, usize), Box<ThreadView>)>, // (folder_index, thread_node_index)
    account_cursor: usize,

    pager: Pager,
    draft: Draft,

    mode: ViewMode,
    dirty: bool,
    initialized: bool,
}

impl Default for Composer {
    fn default() -> Self {
        Composer {
            reply_context: None,
            account_cursor: 0,

            pager: Pager::default(),
            draft: Draft::default(),

            mode: ViewMode::Overview,
            dirty: true,
            initialized: false,
        }
    }
}

#[derive(Debug)]
enum ViewMode {
    Discard(Uuid),
    Pager,
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

    fn is_overview(&self) -> bool {
        if let ViewMode::Overview = self {
            true
        } else {
            false
        }
    }

    fn is_pager(&self) -> bool {
        if let ViewMode::Pager = self {
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
    /*
     * coordinates: (account index, mailbox index, root set thread_node index)
     * msg: index of message we reply to in thread_nodes
     * context: current context
     */
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

    fn draw_header_table(&mut self, grid: &mut CellBuffer, area: Area) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let headers = self.draft.headers();
        {
            let (mut x, mut y) = upper_left;
            for k in &["Date", "From", "To", "Cc", "Bcc", "Subject"] {
                let update = {
                    let (x, y) = write_string_to_grid(
                        k,
                        grid,
                        Color::Default,
                        Color::Default,
                        ((x, y), set_y(bottom_right, y)),
                        true,
                    );
                    let (x, y) = write_string_to_grid(
                        ": ",
                        grid,
                        Color::Default,
                        Color::Default,
                        ((x, y), set_y(bottom_right, y)),
                        true,
                    );
                    let (x, y) = if k == &"From" {
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
                        &headers[*k],
                        grid,
                        Color::Default,
                        Color::Default,
                        ((x, y), set_y(bottom_right, y)),
                        true,
                    );
                    if k == &"From" {
                        write_string_to_grid(
                            " ▶",
                            grid,
                            Color::Byte(251),
                            Color::Default,
                            ((x, y), set_y(bottom_right, y)),
                            true,
                        )
                    } else {
                        (x, y)
                    }
                };
                x = get_x(upper_left);
                y = update.1 + 1;
            }
        }
    }
}

impl Component for Composer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized {
            clear_area(grid, area);
            self.initialized = true;
        }

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let upper_left = set_y(upper_left, get_y(upper_left) + 1);
        let header_height = 5;
        let width = if width!(area) > 80 && self.reply_context.is_some() {
            width!(area) / 2
        } else {
            width!(area)
        };

        let mid = if width > 80 {
            let width = width - 80;
            let mid = if self.reply_context.is_some() {
                width!(area) / 2 + width / 2
            } else {
                width / 2
            };

            if self.reply_context.is_some() {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
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
            let area = (upper_left, set_x(bottom_right, mid - 1));
            let view = &mut self.reply_context.as_mut().unwrap().1;
            view.draw(grid, area, context);
        }

        if self.dirty {
            for i in get_x(upper_left) + mid + 1..=get_x(upper_left) + mid + 79 {
                //set_and_join_box(grid, (i, header_height), HORZ_BOUNDARY);
                grid[(i, header_height)].set_fg(Color::Default);
                grid[(i, header_height)].set_bg(Color::Default);
            }
        }

        let header_area = (set_x(upper_left, mid + 1), (mid + 78, header_height + 1));
        let body_area = (
            (mid + 1, header_height + 2),
            (mid + 78, get_y(bottom_right)),
        );

        if self.dirty {
            self.draft.headers_mut().insert(
                "From".into(),
                get_display_name(context, self.account_cursor),
            );
            self.dirty = false;
        }

        /* Regardless of view mode, do the following */
        clear_area(grid, header_area);
        clear_area(grid, body_area);
        self.draw_header_table(grid, header_area);
        self.pager.draw(grid, body_area, context);

        /* Let user choose whether to quit with/without saving or cancel */
        if let ViewMode::Discard(_) = self.mode {
            let mid_x = width!(area) / 2;
            let mid_y = height!(area) / 2;
            for x in mid_x - 40..=mid_x + 40 {
                for y in mid_y - 11..=mid_y + 11 {
                    grid[(x, y)] = Cell::default();
                }
            }

            for i in mid_x - 40..=mid_x + 40 {
                set_and_join_box(grid, (i, mid_y - 11), HORZ_BOUNDARY);

                set_and_join_box(grid, (i, mid_y + 11), HORZ_BOUNDARY);
            }

            for i in mid_y - 11..=mid_y + 11 {
                set_and_join_box(grid, (mid_x - 40, i), VERT_BOUNDARY);

                set_and_join_box(grid, (mid_x + 40, i), VERT_BOUNDARY);
            }

            let area = ((mid_x - 20, mid_y - 7), (mid_x + 39, mid_y + 10));

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

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        match (&mut self.mode, &mut self.reply_context) {
            (ViewMode::Pager, _) => {
                /* Cannot mutably borrow in pattern guard, pah! */
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::Overview, Some((_, ref mut view))) => {
                if view.process_event(event, context) {
                    self.dirty = true;
                    return true;
                }
            }
            _ => {}
        }

        match event.event_type {
            UIEventType::Resize => {
                self.set_dirty();
            }
            /* Switch e-mail From: field to the `left` configured account. */
            UIEventType::Input(Key::Left) => {
                self.account_cursor = self.account_cursor.saturating_sub(1);
                self.draft.headers_mut().insert(
                    "From".into(),
                    get_display_name(context, self.account_cursor),
                );
                self.dirty = true;
                return true;
            }
            /* Switch e-mail From: field to the `right` configured account. */
            UIEventType::Input(Key::Right) => {
                if self.account_cursor + 1 < context.accounts.len() {
                    self.account_cursor += 1;
                    self.draft.headers_mut().insert(
                        "From".into(),
                        get_display_name(context, self.account_cursor),
                    );
                    self.dirty = true;
                }
                return true;
            }
            UIEventType::Input(Key::Char(key)) if self.mode.is_discard() => {
                match (key, &self.mode) {
                    ('x', ViewMode::Discard(u)) => {
                        context.replies.push_back(UIEvent {
                            id: 0,
                            event_type: UIEventType::Action(Tab(Kill(*u))),
                        });
                        return true;
                    }
                    ('n', _) => {},
                    ('y', ViewMode::Discard(u)) => {
                        let account = &context.accounts[self.account_cursor];
                        let draft = std::mem::replace(&mut self.draft, Draft::default());
                        eprintln!("{:?}", account.save_draft(draft));

                        //eprintln!("{:?}", self.draft.to_string());
                        context.replies.push_back(UIEvent {
                            id: 0,
                            event_type: UIEventType::Action(Tab(Kill(*u))),
                        });
                        return true;
                    },
                    _ => {
                        return false;
                    }
                }
                self.mode = ViewMode::Overview;
                self.set_dirty();
                return true;
            }
            /* Switch to Overview mode if we're on Pager mode */
            UIEventType::Input(Key::Char('o')) if self.mode.is_pager() => {
                self.mode = ViewMode::Overview;
                self.set_dirty();
                return true;
            }
            /* Switch to Pager mode if we're on Overview mode */
            UIEventType::Input(Key::Char('v')) if self.mode.is_overview() => {
                self.mode = ViewMode::Pager;
                self.set_dirty();
                return true;
            }
            /* Edit draft in $EDITOR */
            UIEventType::Input(Key::Char('e')) => {
                use std::process::{Command, Stdio};
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    context.input_kill();
                }
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
                self.pager.update_from_str(self.draft.body());
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::Fork(ForkType::Finished),
                });
                context.restore_input();
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.pager.is_dirty() || self
            .reply_context
            .as_ref()
            .map(|(_, p)| p.is_dirty())
            .unwrap_or(false)
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
        self.initialized = false;
        self.pager.set_dirty();
        if let Some((_, ref mut view)) = self.reply_context {
            view.set_dirty();
        }
    }

    fn kill(&mut self, uuid: Uuid) {
        self.mode = ViewMode::Discard(uuid);
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
