/*
 * meli - ui crate.
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
use std::cmp;

#[derive(Debug, Clone)]
struct ThreadEntry {
    index: (usize, usize, usize),
    /// (indentation, thread_node index, line number in listing)
    indentation: usize,
    msg_idx: EnvelopeHash,
}

#[derive(Debug, Default)]
pub struct ThreadView {
    new_cursor_pos: usize,
    cursor_pos: usize,
    expanded_pos: usize,
    new_expanded_pos: usize,
    dirty: bool,
    coordinates: (usize, usize, usize),
    mailview: MailView,
    show_mailview: bool,
    entries: Vec<ThreadEntry>,
    content: CellBuffer,
    initiated: bool,
}

impl ThreadView {
    /*
     * coordinates: (account index, mailbox index, root set thread_node index)
     * expanded_idx: optional position of expanded entry when we render the threadview. Default
     *  expanded message is the last one.
     * context: current context
     */
    pub fn new(
        coordinates: (usize, usize, usize),
        expanded_idx: Option<usize>,
        context: &Context,
    ) -> Self {
        /* stack to push thread messages in order in order to pop and print them later */
        let mailbox = &context.accounts[coordinates.0][coordinates.1]
            .as_ref()
            .unwrap();
        let threads = &mailbox.collection.threads;

        let thread_iter = threads.thread_iter(coordinates.2);
        let mut view = ThreadView {
            dirty: true,
            initiated: false,
            coordinates,
            mailview: MailView::default(),
            show_mailview: true,
            entries: Vec::new(),
            cursor_pos: 1,
            new_cursor_pos: 0,
            ..Default::default()
        };
        for (line, (ind, idx)) in thread_iter.enumerate() {
            let entry = if let Some(msg_idx) = threads.thread_nodes()[idx].message() {
                view.make_entry((ind, idx, line), msg_idx)
            } else {
                continue;
            };
            view.entries.push(entry);
            match expanded_idx {
                Some(expanded_idx) if expanded_idx == idx => {
                    view.new_expanded_pos = view.entries.len().saturating_sub(1);
                    view.expanded_pos = view.new_expanded_pos + 1;
                }
                _ => {}
            }
        }
        if expanded_idx.is_none() {
            view.new_expanded_pos = view.entries.len().saturating_sub(1);
            view.expanded_pos = view.new_expanded_pos + 1;
        }

        let height = 2 * view.entries.len() + 1;
        let mut width = 0;

        let mut strings: Vec<String> = Vec::with_capacity(view.entries.len());

        let mut highlight_reply_subjects: Vec<Option<usize>> =
            Vec::with_capacity(view.entries.len());
        for e in &view.entries {
            let envelope: &Envelope = &mailbox.collection[&e.msg_idx];
            let thread_node = &threads.thread_nodes()[e.index.1];
            let string = if thread_node.show_subject() {
                let subject = envelope.subject();
                highlight_reply_subjects.push(Some(subject.len()));
                format!(
                    " {}{} - {} {}",
                    " ".repeat(e.index.0 * 4),
                    envelope.date_as_str(),
                    envelope.field_from_to_string(),
                    envelope.subject(),
                )
            } else {
                highlight_reply_subjects.push(None);
                format!(
                    " {}{} - {}",
                    " ".repeat(e.index.0 * 4),
                    envelope.date_as_str(),
                    envelope.field_from_to_string(),
                )
            };
            strings.push(string);
            width = cmp::max(
                width,
                e.index.0 * 4 + strings.last().as_ref().unwrap().len() + 2,
            );
        }
        let mut content = CellBuffer::new(width, height, Cell::default());
        for (y, e) in view.entries.iter().enumerate() {
            /* Box character drawing stuff */
            if y > 0 && content.get_mut(e.index.0 * 4, 2 * y - 1).is_some() {
                let index = (e.index.0 * 4, 2 * y - 1);
                if content[index].ch() == ' ' {
                    let mut ctr = 1;
                    while content[(e.index.0 * 4 + ctr, 2 * y - 1)].ch() == ' ' {
                        set_and_join_box(
                            &mut content,
                            (e.index.0 * 4 + ctr, 2 * y - 1),
                            HORZ_BOUNDARY,
                        );
                        ctr += 1;
                    }
                    set_and_join_box(&mut content, index, HORZ_BOUNDARY);
                }
            }
            write_string_to_grid(
                &strings[y],
                &mut content,
                Color::Default,
                Color::Default,
                ((e.index.0 * 4 + 1, 2 * y), (width - 1, height - 1)),
                true,
            );
            if let Some(len) = highlight_reply_subjects[y] {
                let index = e.index.0 * 4 + 1 + strings[y].len() - len;
                let area = ((index, 2 * y), (width - 2, 2 * y));
                let fg_color = Color::Byte(33);
                let bg_color = Color::Default;
                change_colors(&mut content, area, fg_color, bg_color);
            }
            set_and_join_box(&mut content, (e.index.0 * 4, 2 * y), VERT_BOUNDARY);
            set_and_join_box(&mut content, (e.index.0 * 4, 2 * y + 1), VERT_BOUNDARY);
            for i in ((e.index.0 * 4) + 1)..width - 1 {
                set_and_join_box(&mut content, (i, 2 * y + 1), HORZ_BOUNDARY);
            }
            set_and_join_box(&mut content, (width - 1, 2 * y + 1), VERT_BOUNDARY);
        }

        for y in 0..height - 1 {
            set_and_join_box(&mut content, (width - 1, y), VERT_BOUNDARY);
        }
        view.content = content;
        view.new_cursor_pos = view.new_expanded_pos;
        view
    }

    fn make_entry(&mut self, i: (usize, usize, usize), msg_idx: EnvelopeHash) -> ThreadEntry {
        let (ind, _, _) = i;
        ThreadEntry {
            index: i,
            indentation: ind,
            msg_idx,
        }
    }

    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize) {
        if idx == self.cursor_pos {
            let fg_color = Color::Default;
            let bg_color = Color::Byte(246);
            change_colors(grid, area, fg_color, bg_color);
            return;
        }

        let (width, height) = self.content.size();
        copy_area(
            grid,
            &self.content,
            area,
            (
                (self.entries[idx].index.0 * 4 + 1, 2 * idx),
                (width - 1, height - 1),
            ),
        );
    }

    /// Draw the list
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let (width, height) = self.content.size();
        if height == 0 {
            clear_area(grid, area);
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 2;
        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);

        let top_idx = page_no * rows;

        /* This closure (written for code clarity, should be inlined by the compiler) returns the
         * **line** of an entry in the grid. */
        let entries = &self.entries;
        let get_entry_area = |idx: usize| {
            (
                (
                    entries[idx].index.0 * 4 + 1 + get_x(upper_left),
                    get_y(upper_left) + 2 * (idx % rows),
                ),
                (
                    cmp::min(get_x(upper_left) + width - 2, get_x(bottom_right)),
                    get_y(upper_left) + 2 * (idx % rows),
                ),
            )
        };

        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for &idx in &[old_cursor_pos, self.new_cursor_pos] {
                if idx >= self.entries.len() {
                    continue;
                }
                let new_area = get_entry_area(idx);
                self.highlight_line(grid, new_area, idx);
                context.dirty_areas.push_back(new_area);
            }
            return;
        }
        self.cursor_pos = self.new_cursor_pos;

        /* Page_no has changed, so draw new page */
        copy_area(
            grid,
            &self.content,
            area,
            ((0, 2 * top_idx), (width - 1, height - 1)),
        );
        self.highlight_line(grid, get_entry_area(self.cursor_pos), self.cursor_pos);
        context.dirty_areas.push_back(area);
    }

    fn draw_vert(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);

        let bottom_right = bottom_right!(area);
        let mid = get_x(upper_left) + self.content.size().0;

        if !self.dirty {
            let upper_left = (mid + 1, get_y(upper_left) + 1);
            if self.show_mailview {
                self.mailview
                    .draw(grid, (upper_left, bottom_right), context);
            }
            return;
        }

        self.dirty = false;

        /* First draw the thread subject on the first row */
        let y = {
            let mailbox = &mut context.accounts[self.coordinates.0][self.coordinates.1]
                .as_ref()
                .unwrap();
            let threads = &mailbox.collection.threads;
            let thread_node = &threads.thread_nodes()[threads.root_set(self.coordinates.2)];
            let i = if let Some(i) = thread_node.message() {
                i
            } else {
                threads.thread_nodes()[thread_node.children()[0]]
                    .message()
                    .unwrap()
            };
            let envelope: &Envelope = &mailbox.collection[&i];

            let (x, y) = write_string_to_grid(
                &envelope.subject(),
                grid,
                Color::Byte(33),
                Color::Default,
                area,
                true,
            );
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            //context.dirty_areas.push_back(((0,0), set_y(bottom_right, y)));
            y + 2
        };

        let (width, height) = self.content.size();
        if height == 0 || height == self.cursor_pos || width == 0 {
            return;
        }

        /* if this is the first ever draw, there is nothing on the grid to update so populate it
         * first */
        if !self.initiated {
            clear_area(grid, (set_y(upper_left, y - 1), bottom_right));
            let (width, height) = self.content.size();
            if self.show_mailview {
                let area = (set_y(upper_left, y), set_x(bottom_right, mid - 1));
                let upper_left = upper_left!(area);
                let bottom_right = bottom_right!(area);

                let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 2;
                let page_no = (self.new_cursor_pos).wrapping_div(rows);
                let top_idx = page_no * rows;

                copy_area(
                    grid,
                    &self.content,
                    area,
                    ((0, 2 * top_idx), (width - 1, height - 1)),
                );
                for y in get_y(upper_left)..=get_y(bottom_right) {
                    set_and_join_box(grid, (mid, y), VERT_BOUNDARY);
                }
            } else {
                let area = (set_y(upper_left, y), bottom_right);
                let upper_left = upper_left!(area);
                let bottom_right = bottom_right!(area);

                let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 2;
                let page_no = (self.new_cursor_pos).wrapping_div(rows);
                let top_idx = page_no * rows;
                copy_area(
                    grid,
                    &self.content,
                    area,
                    ((0, 2 * top_idx), (width - 1, height - 1)),
                );
            }
            context.dirty_areas.push_back(area);
            self.initiated = true;
        }

        if self.show_mailview {
            self.draw_list(
                grid,
                (set_y(upper_left, y), set_x(bottom_right, mid - 1)),
                context,
            );
            let upper_left = (mid + 1, get_y(upper_left) + 1);
            self.mailview
                .draw(grid, (upper_left, bottom_right), context);
        } else {
            self.draw_list(grid, (set_y(upper_left, y), bottom_right), context);
        }
    }
    fn draw_horz(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_rows = height!(area);

        let pager_ratio = context.runtime_settings.pager.pager_ratio;
        let bottom_entity_rows = (pager_ratio * total_rows) / 100;

        if bottom_entity_rows > total_rows {
            clear_area(grid, area);
            context.dirty_areas.push_back(area);
            return;
        }

        let mid = get_y(upper_left) + total_rows - bottom_entity_rows;

        if !self.dirty {
            if self.show_mailview {
                self.mailview
                    .draw(grid, (set_y(upper_left, mid + 1), bottom_right), context);
            }
            return;
        }

        self.dirty = false;

        /* First draw the thread subject on the first row */
        let y = {
            let mailbox = &mut context.accounts[self.coordinates.0][self.coordinates.1]
                .as_ref()
                .unwrap();
            let threads = &mailbox.collection.threads;
            let thread_node = &threads.thread_nodes()[threads.root_set(self.coordinates.2)];
            let i = if let Some(i) = thread_node.message() {
                i
            } else {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[iter_ptr].children()[0];
                }
                threads.thread_nodes()[iter_ptr].message().unwrap()
            };
            let envelope: &Envelope = &mailbox.collection[&i];

            let (x, y) = write_string_to_grid(
                &envelope.subject(),
                grid,
                Color::Byte(33),
                Color::Default,
                area,
                true,
            );
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            //context.dirty_areas.push_back(((0,0), set_y(bottom_right, y)));
            y + 2
        };

        let (width, height) = self.content.size();
        if height == 0 || height == self.cursor_pos || width == 0 {
            return;
        }

        /* if this is the first ever draw, there is nothing on the grid to update so populate it
         * first */
        if !self.initiated {
            clear_area(grid, (set_y(upper_left, y - 1), bottom_right));
            let (width, height) = self.content.size();

            if self.show_mailview {
                let area = (set_y(upper_left, y), set_y(bottom_right, mid - 1));
                let upper_left = upper_left!(area);
                let bottom_right = bottom_right!(area);

                let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 2;
                let page_no = (self.new_cursor_pos).wrapping_div(rows);
                let top_idx = page_no * rows;

                copy_area(
                    grid,
                    &self.content,
                    area,
                    ((0, 2 * top_idx), (width - 1, height - 1)),
                );
                for x in get_x(upper_left)..=get_x(bottom_right) {
                    set_and_join_box(grid, (x, mid), HORZ_BOUNDARY);
                }
            } else {
                let area = (set_y(upper_left, y), bottom_right);
                let upper_left = upper_left!(area);
                let bottom_right = bottom_right!(area);

                let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 2;
                let page_no = (self.new_cursor_pos).wrapping_div(rows);
                let top_idx = page_no * rows;
                copy_area(
                    grid,
                    &self.content,
                    area,
                    ((0, 2 * top_idx), (width - 1, height - 1)),
                );
            }
            context.dirty_areas.push_back(area);
            self.initiated = true;
        }
        if self.show_mailview {
            self.draw_list(
                grid,
                (set_y(upper_left, y), set_y(bottom_right, mid - 1)),
                context,
            );
            self.mailview
                .draw(grid, (set_y(upper_left, mid + 1), bottom_right), context);
        } else {
            self.draw_list(grid, (set_y(upper_left, y), bottom_right), context);
        }
    }
}

impl fmt::Display for ThreadView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "view thread")
    }
}

impl Component for ThreadView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let total_rows = height!(area);
        let total_cols = width!(area);
        if total_rows < 24 || total_cols < 80 {
            return;
        }

        /* If user has selected another mail to view, change to it */
        if self.new_expanded_pos != self.expanded_pos {
            self.expanded_pos = self.new_expanded_pos;
            self.mailview = MailView::new(
                (
                    self.coordinates.0,
                    self.coordinates.1,
                    self.entries[self.expanded_pos].msg_idx,
                ),
                None,
                None,
            );
        }

        if self.entries.len() == 1 {
            self.mailview.draw(grid, area, context);
            return;
        }

        if total_cols >= self.content.size().0 + 74 {
            self.draw_vert(grid, area, context);
        } else {
            self.draw_horz(grid, area, context);
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.mailview.process_event(event, context) {
            return true;
        }
        match event.event_type {
            UIEventType::Input(Key::Char('R')) => {
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::Action(Tab(Reply(
                        self.coordinates,
                        self.entries[self.expanded_pos].index.1,
                    ))),
                });
                return true;
            }
            UIEventType::Input(Key::Char('e')) => {
                {
                    let mailbox = &context.accounts[self.coordinates.0][self.coordinates.1]
                        .as_ref()
                        .unwrap();
                    let threads = &mailbox.collection.threads;
                    let thread_node = &threads.thread_nodes()[threads.root_set(self.coordinates.2)];
                    let i = if let Some(i) = thread_node.message() {
                        i
                    } else {
                        threads.thread_nodes()[thread_node.children()[0]]
                            .message()
                            .unwrap()
                    };
                    let envelope: &Envelope = &mailbox.collection[&i];
                    let op = context.accounts[self.coordinates.0]
                        .backend
                        .operation(envelope.hash(), mailbox.folder.hash());
                    eprintln!("sending action edit for {}, {}", envelope.message_id(), op.description());
                }
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::Action(Tab(Edit(
                        self.coordinates,
                        self.entries[self.expanded_pos].index.1,
                    ))),
                });
                return true;
            }
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos > 0 {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(1);
                    self.dirty = true;
                }
                return true;
            }
            UIEventType::Input(Key::Down) => {
                let height = self.entries.len();
                if height > 0 && self.cursor_pos + 1 < height {
                    self.new_cursor_pos += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEventType::Input(Key::Char('\n')) => {
                if self.entries.len() < 2 {
                    return true;
                }
                self.new_expanded_pos = self.cursor_pos;
                self.show_mailview = true;
                self.initiated = false;
                self.set_dirty();
                return true;
            }
            UIEventType::Input(Key::Char('p')) => {
                self.show_mailview = !self.show_mailview;
                self.initiated = false;
                self.set_dirty();
                return true;
            }
            UIEventType::Resize => {
                self.set_dirty();
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.mailview.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.initiated = false;
        self.dirty = true;
        self.mailview.set_dirty();
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMap {
        let mut map = self
            .mailview
            .get_shortcuts(context);

        map.insert(
            "reply", Key::Char('R')
        );
        map.insert(
            "toggle_mailview", Key::Char('p')
        );

        map
    }
}
