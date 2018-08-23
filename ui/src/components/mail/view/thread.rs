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

#[derive(Debug)]
struct ThreadEntry {
    index: (usize, usize, usize),
    indentation: usize,
    content: CellBuffer,

    msg_idx: usize,
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
    entries: Vec<ThreadEntry>,
    content: CellBuffer,
    initiated: bool,
}

impl ThreadView {
    pub fn new(coordinates: (usize, usize, usize), context: &Context) -> Self {
        let mut stack: Vec<(usize, usize)> = Vec::with_capacity(32);
        let mailbox = &context.accounts[coordinates.0][coordinates.1].as_ref().unwrap();
        let threads = &mailbox.threads;
        let container = &threads.containers()[threads.root_set()[coordinates.2]];

        if container.message().is_some() {
            stack.push((0, threads.root_set()[coordinates.2]));
        } else {
            stack.push((1, container.first_child().unwrap()));
        }
        let mut view = ThreadView {
            dirty: true,
            initiated: false,
            coordinates,
            mailview: MailView::default(),
            entries: Vec::new(),
            cursor_pos: 1,
            new_cursor_pos: 0,
            .. Default::default()
        };
        let mut line = 0;
        let mut max_ind = 0;
        while let Some((ind, idx)) = stack.pop() {
            max_ind = cmp::max(max_ind, ind);
            let entry = view.make_entry(context, (ind, idx, line));
            view.entries.push(entry);
            line += 1;
            let container = &threads.containers()[idx];
            if let Some(i) = container.next_sibling() {
                stack.push((ind, i));
            }

            if let Some(i) = container.first_child() {
                stack.push((ind + 1, i));
            }
        }
        view.new_expanded_pos = view.entries.len() - 1;
        view.expanded_pos = view.new_expanded_pos + 1;

        let height = 2*view.entries.len();
        let mut width = 0;

        let mut strings: Vec<String> = Vec::with_capacity(view.entries.len());


        for e in &view.entries {
            let envelope: &Envelope = &mailbox.collection[e.msg_idx];
            strings.push(format!(" {}{} - {}", " ".repeat(e.index.0 * 4), envelope.date_as_str(), envelope.from_to_string()));
            width = cmp::max(width, e.index.0 + strings.last().as_ref().unwrap().len() + 1);

        }
        let mut content = CellBuffer::new(width, height,  Cell::default());
        for (y, e) in view.entries.iter().enumerate() {
            if y > 0 && content.get_mut(e.index.0 * 4, 2*y - 1).is_some() {
                let index = (e.index.0 * 4, 2*y - 1);
                if content[index].ch() == ' ' {
                    let mut ctr = 1;
                    while content[(e.index.0 * 4 + ctr, 2 * y - 1)].ch() == ' ' {
                        content[(e.index.0 * 4 + ctr, 2 * y - 1)].set_ch(HORZ_BOUNDARY);
                        ctr += 1;
                    }
                    content[index].set_ch(_TOP_LEFT_CORNER);
                } else {
                    content[index].set_ch(LIGHT_DOWN_AND_HORIZONTAL);
                };
            }

            write_string_to_grid(
                &strings[y],
                &mut content,
                Color::Default,
                Color::Default,
                ((e.index.0 + 1, 2*y), (width - 1, height - 1)),
                true,
                );
            content[(e.index.0 * 4, 2*y)].set_ch(VERT_BOUNDARY);
            for i in (e.index.0 * 4)..width {
                content[(i, 2*y + 1)].set_ch(HORZ_BOUNDARY);
            }
            content[(e.index.0 *4, 2*y + 1)].set_ch(LIGHT_UP_AND_HORIZONTAL);
        }
        //view.mailview = MailView::new((view.coordinates.0, view.coordinates.1, view.entries[view.expanded_pos].msg_idx), None, None);
        view.content = content;
        view.new_cursor_pos = view.new_expanded_pos;
        view
    }

    fn make_entry(&mut self, context: &Context, i: (usize, usize, usize)) -> ThreadEntry {
        let (ind, idx, order) = i;
        let mailbox = &context.accounts[self.coordinates.0][self.coordinates.1]
            .as_ref()
            .unwrap();
        let container = &mailbox.threads.containers()[idx];
        let msg_idx = if let Some(i) = container.message() {
            i
        } else {
            mailbox.threads.containers()[
                container.first_child().unwrap()
            ].message().unwrap()
        };
        let envelope: &Envelope = &mailbox.collection[msg_idx];
        let op = context.accounts[self.coordinates.0].backend.operation(envelope.hash());
        let body = envelope.body(op);

        let mut body_text: String = " \n".repeat(6);
        body_text.push_str(&String::from_utf8_lossy(&decode_rec(&body, None)));
        let mut buf = CellBuffer::from(&body_text).split_newlines();


        let date = format!("Date: {}\n", envelope.date_as_str());
        let from = format!("From: {}\n", envelope.from_to_string());
        let message_id = &format!("Message-ID: <{}>\n\n", envelope.message_id_raw());
        let mut width = [date.len(), from.len(), message_id.len(), buf.size().0].iter().map(|&v| v).max().unwrap_or(1);
        let height = buf.size().1;
        if width > buf.size().0 {
            buf.resize(width, height, Cell::default());
            width -= 1;
        } else {
            width = buf.size().0 - 1;
        }

        write_string_to_grid(
            &date,
            &mut buf,
            Color::Byte(33),
            Color::Default,
            ((ind, 0), (width, height)),
            true,
            );
        write_string_to_grid(
            &from,
            &mut buf,
            Color::Byte(33),
            Color::Default,
            ((ind, 1), (width, height)),
            true,
            );
        write_string_to_grid(
            &message_id,
            &mut buf,
            Color::Byte(33),
            Color::Default,
            ((ind, 2), (width, height)),
            true,
            );

        ThreadEntry {
            index: (ind, idx, order),
            indentation: ind,
            content: buf,

            msg_idx,
        }
    }

    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize) {
        let fg_color = Color::Default;
        let bg_color = if self.cursor_pos == idx {
            Color::Byte(246)
        } else {
            Color::Default
        };
        change_colors(grid, area, fg_color, bg_color);
    }

    /// Draw the list
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let (width, height) = self.content.size();
        if height == 0 {
            clear_area(grid, area);
            copy_area(grid, &self.content, area, ((0, 0), (width - 1, 0)));
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 2;
        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);

        let top_idx = page_no * rows;

        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for &idx in &[old_cursor_pos, self.new_cursor_pos] {
                let new_area = (
                    set_y(upper_left, get_y(upper_left) + 2*(idx % rows)),
                    set_y(bottom_right, get_y(upper_left) + 2*(idx % rows)),
                );
                self.highlight_line(grid, new_area, idx);
                context.dirty_areas.push_back(new_area);
            }
            return;
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }

        /* Page_no has changed, so draw new page */
        copy_area(
            grid,
            &self.content,
            area,
            ((0, 2*top_idx), (width - 1, height - 1)),
        );
        self.highlight_line(
            grid,
            (
                set_y(upper_left, get_y(upper_left) + 2*(self.cursor_pos % rows)),
                set_y(bottom_right, get_y(upper_left) + 2*(self.cursor_pos % rows)),
            ),
            self.cursor_pos,
        );
        context.dirty_areas.push_back(area);
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
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let total_rows = get_y(bottom_right) - get_y(upper_left);
        let pager_ratio = context.runtime_settings.pager.pager_ratio;
        let bottom_entity_rows = (pager_ratio * total_rows) / 100;

        if bottom_entity_rows > total_rows {
            clear_area(grid, area);
            context.dirty_areas.push_back(area);
            return;
        }

        let mid = get_y(upper_left) + total_rows - bottom_entity_rows;

        if !self.dirty {
            self.mailview.draw(grid, (set_y(upper_left, mid + 1), bottom_right), context);
            return;
        }

        self.dirty = false;

        let y = {
            let mailbox = &mut context.accounts[self.coordinates.0][self.coordinates.1].as_ref().unwrap();
            let threads = &mailbox.threads;
            let container = &threads.containers()[threads.root_set()[self.coordinates.2]];
            let i = if let Some(i) = container.message() {
                i
            } else {
                threads.containers()[
                    container.first_child().unwrap()
                ].message().unwrap()
            };
            let envelope: &Envelope = &mailbox.collection[i];

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

        if !self.initiated {
            clear_area(grid, (set_y(upper_left, y - 1), bottom_right));
            let (width, height) = self.content.size();
            copy_area(
                grid,
                &self.content,
                (set_y(upper_left, y), set_y(bottom_right, mid - 1)),
                ((0, 0), (width - 1, height - 1)),
                );
            for x in get_x(upper_left)..=get_x(bottom_right) {
                grid[(x, mid)].set_ch(HORZ_BOUNDARY);
            }
            if let Some(cell) = grid.get_mut(get_x(upper_left).saturating_sub(1), mid) {
                if cell.ch() == VERT_BOUNDARY {
                    cell.set_ch(LIGHT_VERTICAL_AND_RIGHT);
                }
            }
            context.dirty_areas.push_back(area);
            self.initiated = true;
        }

        if self.new_expanded_pos != self.expanded_pos {
            self.expanded_pos = self.new_expanded_pos;
            self.mailview = MailView::new((self.coordinates.0, self.coordinates.1, self.entries[self.expanded_pos].msg_idx), None, None);
        }
        self.draw_list(grid, (set_y(upper_left, y), set_y(bottom_right, mid - 1)), context);
        self.mailview.draw(grid, (set_y(upper_left, mid + 1), bottom_right), context);
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        if self.mailview.process_event(event, context) {
            return true;
        }
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos > 0 {
                    self.new_cursor_pos -= 1;
                    self.dirty = true;
                }
                return true;
            },
            UIEventType::Input(Key::Down) => {
                let height = self.entries.len();
                if height > 0 && self.cursor_pos + 1 < height {
                    self.new_cursor_pos += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEventType::Input(Key::Char('\n')) => {
                self.new_expanded_pos = self.cursor_pos;
                self.dirty = true;
                return true;
            },
            UIEventType::Resize => {
                self.dirty = true;
            },
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.mailview.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        self.mailview.set_dirty();
    }
}
