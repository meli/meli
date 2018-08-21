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

#[derive(Debug)]
struct ThreadEntry {
    index: (usize, usize, usize),
    indentation: usize,
    content: CellBuffer,

    msg_idx: usize,
    expanded: bool,
}


#[derive(Debug)]
pub struct ThreadView {
    dirty: bool,
    coordinates: (usize, usize, usize),
    pager: Pager,
    entries: Vec<ThreadEntry>,
}

impl ThreadView {
    pub fn new(coordinates: (usize, usize, usize),
    ) -> Self {
        ThreadView {
            dirty: true,
            coordinates,
            pager: Pager::default(),
            entries: Vec::new(),
        }
    }

    fn make_entry(&mut self, context: &mut Context, i: (usize, usize, usize)) -> ThreadEntry {
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
            ((0, 0), (width, height)),
            true,
            );
        write_string_to_grid(
            &from,
            &mut buf,
            Color::Byte(33),
            Color::Default,
            ((0, 1), (width, height)),
            true,
            );
        write_string_to_grid(
            &message_id,
            &mut buf,
            Color::Byte(33),
            Color::Default,
            ((0, 2), (width, height)),
            true,
            );

        ThreadEntry {
            index: (ind, idx, order),
            indentation: ind,
            content: buf,

            msg_idx,
            expanded: false,
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
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        clear_area(grid, area);
        let mut stack: Vec<(usize, usize)> = Vec::with_capacity(32);

        let y =
        {
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
            if container.message().is_some() {
                stack.push((0, threads.root_set()[self.coordinates.2]));
            } else {
                stack.push((1, container.first_child().unwrap()));
            }
            y
        };

        if self.dirty {

            let mut line = 0;
            while let Some((ind, idx)) = stack.pop() {
                let entry = self.make_entry(context, (ind, idx, line));
                self.entries.push(entry);
                line += 1;
                let mailbox = &mut context.accounts[self.coordinates.0][self.coordinates.1].as_ref().unwrap();
                let threads = &mailbox.threads;
                let container = &threads.containers()[idx];
                if let Some(i) = container.next_sibling() {
                    stack.push((ind, i));
                }
                if let Some(i) = container.first_child() {
                    stack.push((ind + 1, i));
                }
            }
            let height = self.entries.iter().map(|x| x.content.size().1).sum();
            let width = self.entries.iter().map(|x| x.content.size().0).max().unwrap_or(0);
            let mut content = CellBuffer::new(width, height, Cell::default());

            let mut y = 0;

            let (cols_dest, rows_dest) = content.size();
            for e in &self.entries {
                let (cols_src, rows_src) = e.content.size();
                y = copy_area(&mut content, &e.content, ((0, y), (cols_dest - 1, rows_dest - 1)), ((0, 0), (cols_src - 1, rows_src - 1))).1;
                for i in 0..cols_dest {
                    content[(i, y)].set_ch(HORZ_BOUNDARY);
                }
            }
            self.pager = Pager::from_buf(content, None);
            self.dirty = false;
        }

        self.pager.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);

        context
            .dirty_areas
            .push_back(area);
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        self.pager.process_event(event, context);
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.pager.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}
