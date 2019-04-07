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
use std::ops::Index;

#[derive(Debug, Clone)]
struct ThreadEntry {
    index: (usize, usize, usize),
    /// (indentation, thread_node index, line number in listing)
    indentation: usize,
    msg_hash: EnvelopeHash,
    seen: bool,
    dirty: bool,
    hidden: bool,
    heading: String,
}

#[derive(Debug, Default)]
pub struct ThreadView {
    new_cursor_pos: usize,
    cursor_pos: usize,
    expanded_pos: usize,
    new_expanded_pos: usize,
    reversed: bool,
    coordinates: (usize, usize, usize),
    mailview: MailView,
    show_mailview: bool,
    entries: Vec<ThreadEntry>,
    visible_entries: Vec<Vec<usize>>,

    dirty: bool,
    content: CellBuffer,
    initiated: bool,
}

#[derive(Debug)]
struct StackVec {
    len: usize,
    array: [usize; 8],
    heap_vec: Vec<usize>,
}

impl StackVec {
    fn new() -> Self {
        StackVec {
            len: 0,
            array: [0, 0, 0, 0, 0, 0, 0, 0],
            heap_vec: Vec::new(),
        }
    }
    fn push(&mut self, ind: usize) {
        if self.len == self.array.len() {
            self.heap_vec.clear();
            self.heap_vec.reserve(16);
            self.heap_vec.copy_from_slice(&self.array);
            self.heap_vec.push(ind);
        } else if self.len > self.array.len() {
            self.heap_vec.push(ind);
        } else {
            self.array[self.len] = ind;
        }
        self.len += 1;
    }
    fn pop(&mut self) -> usize {
        if self.len >= self.array.len() {
            self.heap_vec.pop().unwrap()
        } else {
            let ret = self.array[self.len];
            self.len = self.len.saturating_sub(1);
            ret
        }
    }
    fn len(&self) -> usize {
        self.len
    }
    fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl Index<usize> for StackVec {
    type Output = usize;

    fn index(&self, idx: usize) -> &usize {
        if self.len >= self.array.len() {
            &self.heap_vec[idx]
        } else {
            &self.array[idx]
        }
    }
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
        let mut view = ThreadView {
            reversed: false,
            initiated: false,
            coordinates,
            mailview: MailView::default(),
            show_mailview: true,
            entries: Vec::new(),
            cursor_pos: 1,
            new_cursor_pos: 0,
            dirty: true,
            ..Default::default()
        };
        view.initiate(expanded_idx, context);
        view.new_cursor_pos = view.new_expanded_pos;
        view
    }
    pub fn update(&mut self, context: &Context) {
        if self.entries.is_empty() {
            return;
        }

        let old_entries = self.entries.clone();

        let old_focused_entry = if self.entries.len() > self.cursor_pos {
            Some(self.entries.remove(self.cursor_pos))
        } else {
            None
        };

        let old_expanded_entry = if self.entries.len() > self.expanded_pos {
            Some(self.entries.remove(self.expanded_pos))
        } else {
            None
        };

        let expanded_pos = self.expanded_pos;
        self.initiate(Some(expanded_pos), context);

        let mut old_cursor = 0;
        let mut new_cursor = 0;
        loop {
            if old_cursor >= old_entries.len() || new_cursor >= self.entries.len() {
                break;
            }
            if old_entries[old_cursor].msg_hash == self.entries[new_cursor].msg_hash
                || old_entries[old_cursor].index == self.entries[new_cursor].index
                || old_entries[old_cursor].heading == self.entries[new_cursor].heading
            {
                self.entries[new_cursor].hidden = old_entries[old_cursor].hidden;
                old_cursor += 1;
                new_cursor += 1;
            } else {
                new_cursor += 1;
            }
            self.recalc_visible_entries();
        }

        if let Some(old_focused_entry) = old_focused_entry {
            if let Some(new_entry_idx) = self.entries.iter().position(|e| {
                e.msg_hash == old_focused_entry.msg_hash
                    || (e.index.1 == old_focused_entry.index.1
                        && e.index.2 == old_focused_entry.index.2)
            }) {
                self.cursor_pos = new_entry_idx;
            }
        }
        if let Some(old_expanded_entry) = old_expanded_entry {
            if let Some(new_entry_idx) = self.entries.iter().position(|e| {
                e.msg_hash == old_expanded_entry.msg_hash
                    || (e.index.1 == old_expanded_entry.index.1
                        && e.index.2 == old_expanded_entry.index.2)
            }) {
                self.expanded_pos = new_entry_idx;
            }
        }
        self.set_dirty();
    }
    fn initiate(&mut self, expanded_idx: Option<usize>, context: &Context) {
        /* stack to push thread messages in order in order to pop and print them later */
        let mailbox = &context.accounts[self.coordinates.0][self.coordinates.1]
            .as_ref()
            .unwrap();
        let threads = &mailbox.collection.threads;

        let thread_iter = threads.thread_iter(self.coordinates.2);
        self.entries.clear();
        for (line, (ind, idx)) in thread_iter.enumerate() {
            let entry = if let Some(msg_hash) = threads.thread_nodes()[idx].message() {
                let seen: bool = mailbox.collection[&msg_hash].is_seen();
                self.make_entry((ind, idx, line), msg_hash, seen)
            } else {
                continue;
            };
            self.entries.push(entry);
            match expanded_idx {
                Some(expanded_idx) if expanded_idx == idx => {
                    self.new_expanded_pos = self.entries.len().saturating_sub(1);
                    self.expanded_pos = self.new_expanded_pos + 1;
                }
                _ => {}
            }
        }
        if expanded_idx.is_none() {
            self.new_expanded_pos = self.entries.len().saturating_sub(1);
            self.expanded_pos = self.new_expanded_pos + 1;
        }

        let height = 2 * self.entries.len() + 1;
        let mut width = 0;

        let mut highlight_reply_subjects: Vec<Option<usize>> =
            Vec::with_capacity(self.entries.len());
        for e in &mut self.entries {
            let envelope: &Envelope = &mailbox.collection[&e.msg_hash];
            let thread_node = &threads.thread_nodes()[e.index.1];
            let string = if thread_node.show_subject() {
                let subject = envelope.subject();
                highlight_reply_subjects.push(Some(subject.grapheme_width()));
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
            e.heading = string;
            width = cmp::max(width, e.index.0 * 4 + e.heading.grapheme_width() + 2);
        }
        let mut content = CellBuffer::new(width, height, Cell::default());
        if self.reversed {
            for (y, e) in self.entries.iter().rev().enumerate() {
                /* Box character drawing stuff */
                if y > 0 && content.get_mut(e.index.0 * 4, 2 * y - 1).is_some() {
                    let index = (e.index.0 * 4, 2 * y - 1);
                    if content[index].ch() == ' ' {
                        let mut ctr = 1;
                        while content.get(e.index.0 * 4 + ctr, 2 * y - 1).is_some() {
                            if content[(e.index.0 * 4 + ctr, 2 * y - 1)].ch() != ' ' {
                                break;
                            }
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
                    &e.heading,
                    &mut content,
                    if e.seen {
                        Color::Default
                    } else {
                        Color::Byte(0)
                    },
                    if e.seen {
                        Color::Default
                    } else {
                        Color::Byte(251)
                    },
                    (
                        (e.index.0 * 4 + 1, 2 * y),
                        (e.index.0 * 4 + e.heading.grapheme_width() + 1, height - 1),
                    ),
                    true,
                );
                if let Some(len) = highlight_reply_subjects[y] {
                    let index = e.index.0 * 4 + 1 + e.heading.grapheme_width() - len;
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
                set_and_join_box(&mut content, (width - 1, 2 * y), VERT_BOUNDARY);
                set_and_join_box(&mut content, (width - 1, 2 * y + 1), VERT_BOUNDARY);
            }
        } else {
            for (y, e) in self.entries.iter().enumerate() {
                /* Box character drawing stuff */
                if y > 0 && content.get_mut(e.index.0 * 4, 2 * y - 1).is_some() {
                    let index = (e.index.0 * 4, 2 * y - 1);
                    if content[index].ch() == ' ' {
                        let mut ctr = 1;
                        while content.get(e.index.0 * 4 + ctr, 2 * y - 1).is_some() {
                            if content[(e.index.0 * 4 + ctr, 2 * y - 1)].ch() != ' ' {
                                break;
                            }
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
                    &e.heading,
                    &mut content,
                    if e.seen {
                        Color::Default
                    } else {
                        Color::Byte(0)
                    },
                    if e.seen {
                        Color::Default
                    } else {
                        Color::Byte(251)
                    },
                    (
                        (e.index.0 * 4 + 1, 2 * y),
                        (e.index.0 * 4 + e.heading.grapheme_width() + 1, height - 1),
                    ),
                    false,
                );
                if let Some(len) = highlight_reply_subjects[y] {
                    let index = e.index.0 * 4 + 1 + e.heading.grapheme_width() - len;
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
                set_and_join_box(&mut content, (width - 1, 2 * y), VERT_BOUNDARY);
                set_and_join_box(&mut content, (width - 1, 2 * y + 1), VERT_BOUNDARY);
            }

            for y in 0..height - 1 {
                set_and_join_box(&mut content, (width - 1, y), VERT_BOUNDARY);
            }
        }
        self.content = content;
        self.visible_entries = vec![(0..self.entries.len()).collect()];
    }

    fn make_entry(
        &mut self,
        i: (usize, usize, usize),
        msg_hash: EnvelopeHash,
        seen: bool,
    ) -> ThreadEntry {
        let (ind, _, _) = i;
        ThreadEntry {
            index: i,
            indentation: ind,
            msg_hash,
            seen,
            dirty: true,
            hidden: false,
            heading: String::new(),
        }
    }

    fn highlight_line(&self, grid: &mut CellBuffer, dest_area: Area, src_area: Area, idx: usize) {
        let visibles: Vec<&usize> = self
            .visible_entries
            .iter()
            .flat_map(|ref v| v.iter())
            .collect();
        if idx == *visibles[self.cursor_pos] {
            let fg_color = Color::Default;
            let bg_color = Color::Byte(246);
            change_colors(grid, dest_area, fg_color, bg_color);
            return;
        }

        let (width, height) = self.content.size();
        copy_area(grid, &self.content, dest_area, src_area);
    }

    /// draw the list
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let (width, height) = self.content.size();
        if height == 0 {
            clear_area(grid, area);
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = (get_y(bottom_right) - get_y(upper_left)).wrapping_div(2);
        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);

        let top_idx = page_no * rows;

        /* This closure (written for code clarity, should be inlined by the compiler) returns the
         * **line** of an entry in the ThreadView grid. */
        let get_entry_area = |idx: usize, entries: &[ThreadEntry]| {
            let entries = &entries;
            let visual_indentation = entries[idx].index.0 * 4;
            (
                (visual_indentation, 2 * idx),
                (
                    visual_indentation + entries[idx].heading.grapheme_width() + 1,
                    2 * idx,
                ),
            )
        };

        if self.dirty || (page_no != prev_page_no) {
            if page_no != prev_page_no {
                clear_area(grid, area);
            }
            let visibles = self
                .visible_entries
                .iter()
                .flat_map(|v| v.iter())
                .skip(top_idx)
                .take(rows);
            let mut visible_entry_counter = 0;

            for v in visibles {
                if visible_entry_counter >= rows {
                    break;
                }
                let idx = v;
                copy_area(
                    grid,
                    &self.content,
                    (
                        pos_inc(upper_left, (0, 2 * visible_entry_counter)), // dest_area
                        bottom_right,
                    ),
                    (
                        (0, 2 * idx), //src_area
                        (width - 1, 2 * idx + 1),
                    ),
                );
                visible_entry_counter += 1;
            }
            /* If cursor position has changed, remove the highlight from the previous position and
             * apply it in the new one. */
            let visibles: Vec<&usize> = self
                .visible_entries
                .iter()
                .flat_map(|ref v| v.iter())
                .collect();
            self.cursor_pos = self.new_cursor_pos;
            let idx = *visibles[self.cursor_pos];
            let src_area = { get_entry_area(idx, &self.entries) };
            let visual_indentation = self.entries[idx].indentation * 4;
            let dest_area = (
                pos_inc(
                    upper_left,
                    (visual_indentation, 2 * (self.cursor_pos - top_idx)),
                ),
                (
                    get_x(upper_left)
                        + visual_indentation
                        + self.entries[idx].heading.grapheme_width()
                        + 1,
                    get_y(upper_left) + 2 * (self.cursor_pos - top_idx),
                ),
            );

            self.highlight_line(grid, dest_area, src_area, idx);
            self.dirty = false;
            context.dirty_areas.push_back(area);
        } else {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            /* If cursor position has changed, remove the highlight from the previous position and
             * apply it in the new one. */
            let visibles: Vec<&usize> = self
                .visible_entries
                .iter()
                .flat_map(|ref v| v.iter())
                .collect();
            for &idx in &[old_cursor_pos, self.cursor_pos] {
                let entry_idx = *visibles[idx];
                let src_area = { get_entry_area(entry_idx, &self.entries) };
                let visual_indentation = self.entries[entry_idx].indentation * 4;
                let dest_area = (
                    pos_inc(
                        upper_left,
                        (visual_indentation, 2 * (visibles[..idx].len() - top_idx)),
                    ),
                    (
                        get_x(upper_left)
                            + visual_indentation
                            + self.entries[entry_idx].heading.grapheme_width()
                            + 1,
                        get_y(upper_left) + 2 * (visibles[..idx].len() - top_idx),
                    ),
                );

                self.highlight_line(grid, dest_area, src_area, entry_idx);

                let (upper_left, bottom_right) = dest_area;
                context
                    .dirty_areas
                    .push_back((upper_left, (get_x(bottom_right), get_y(upper_left) + 1)));
            }
        }
    }

    fn draw_vert(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);

        let bottom_right = bottom_right!(area);
        let mid = get_x(upper_left) + self.content.size().0;

        /* First draw the thread subject on the first row */
        let y = if self.dirty {
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
                y + 2
            };
            clear_area(grid, (set_y(upper_left, y), set_x(bottom_right, mid)));
            y
        } else {
            get_y(upper_left) + 2
        };
        let (width, height) = self.content.size();
        if height == 0 || width == 0 {
            return;
        }
        if self.dirty {
            for x in get_x(upper_left)..=get_x(bottom_right) {
                set_and_join_box(grid, (x, y - 1), HORZ_BOUNDARY);
            }
        }

        self.draw_list(
            grid,
            (set_y(upper_left, y), set_x(bottom_right, mid - 1)),
            context,
        );
        {
            let upper_left = (mid + 1, get_y(upper_left) + y - 1);
            self.mailview
                .draw(grid, (upper_left, bottom_right), context);
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

        /* First draw the thread subject on the first row */
        let y = {
            let mailbox = &context.accounts[self.coordinates.0][self.coordinates.1]
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

        for x in get_x(upper_left)..=get_x(bottom_right) {
            set_and_join_box(grid, (x, y - 1), HORZ_BOUNDARY);
        }
    }

    fn visible_entries(&self) -> Vec<Vec<usize>> {
        self.visible_entries.clone()
    }
    fn recalc_visible_entries(&mut self) {
        if self
            .entries
            .iter_mut()
            .fold(false, |flag, e| e.dirty || flag)
        {
            self.visible_entries = self
                .entries
                .iter()
                .enumerate()
                .fold(
                    (vec![Vec::new()], StackVec::new(), false),
                    |(mut visies, mut stack, is_prev_hidden), (idx, e)| {
                        match (e.hidden, is_prev_hidden) {
                            (true, false) => {
                                visies.last_mut().unwrap().push(idx);
                                stack.push(e.indentation);
                                (visies, stack, e.hidden)
                            }
                            (true, true)
                                if !stack.is_empty() && stack[stack.len() - 1] == e.indentation =>
                            {
                                visies.push(vec![idx]);
                                (visies, stack, e.hidden)
                            }
                            (true, true) => (visies, stack, e.hidden),
                            (false, true)
                                if stack[stack.len() - 1] >= e.indentation
                                    && stack.len() > 1
                                    && stack[stack.len() - 2] >= e.indentation =>
                            {
                                //FIXME pop all until e.indentation
                                visies.push(vec![idx]);
                                stack.pop();
                                (visies, stack, e.hidden)
                            }
                            (false, true) if stack[stack.len() - 1] >= e.indentation => {
                                visies.push(vec![idx]);
                                stack.pop();
                                (visies, stack, e.hidden)
                            }
                            (false, true) => (visies, stack, is_prev_hidden),
                            (false, false) => {
                                visies.last_mut().unwrap().push(idx);
                                (visies, stack, e.hidden)
                            }
                        }
                    },
                )
                .0;
        }
    }

    /// Current position in self.entries (not in drawn entries which might exclude nonvisible ones)
    fn current_pos(&self) -> usize {
        let visibles: Vec<&usize> = self
            .visible_entries
            .iter()
            .flat_map(|ref v| v.iter())
            .collect();
        *visibles[self.new_cursor_pos]
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
                    self.entries[self.current_pos()].msg_hash,
                ),
                None,
                None,
                context,
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
                    if cfg!(feature = "debug_log") {
                        eprintln!(
                            "sending action edit for {}, {}",
                            envelope.message_id(),
                            op.description()
                        );
                    }
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
                }
                return true;
            }
            UIEventType::Input(Key::Down) => {
                let height = self.visible_entries.iter().flat_map(|v| v.iter()).count();
                if height > 0 && self.new_cursor_pos + 1 < height {
                    self.new_cursor_pos += 1;
                }
                return true;
            }
            UIEventType::Input(Key::Char('\n')) => {
                if self.entries.len() < 2 {
                    return true;
                }
                self.new_expanded_pos = self.current_pos();
                self.show_mailview = true;
                //self.initiated = false;
                self.set_dirty();
                return true;
            }
            UIEventType::Input(Key::Char('p')) => {
                self.show_mailview = !self.show_mailview;
                self.initiated = false;
                self.mailview.set_dirty();
                return true;
            }
            UIEventType::Input(Key::Ctrl('r')) => {
                self.reversed = !self.reversed;
                let expanded_pos = self.expanded_pos;
                self.initiate(Some(expanded_pos), context);
                self.initiated = false;
                self.dirty = true;
                return true;
            }
            UIEventType::Input(Key::Char('h')) => {
                let current_pos = self.current_pos();
                self.entries[current_pos].hidden = !self.entries[current_pos].hidden;
                self.entries[current_pos].dirty = true;
                {
                    let visible_entries: Vec<&usize> =
                        self.visible_entries.iter().flat_map(|v| v.iter()).collect();
                    let search_old_cursor_pos = |entries: Vec<&usize>, x: usize| {
                        let mut low = 0;
                        let mut high = entries.len() - 1;
                        while low <= high {
                            let mid = low + (high - low) / 2;
                            if *entries[mid] == x {
                                return mid;
                            }
                            if x > *entries[mid] {
                                low = mid + 1;
                            } else {
                                high = mid - 1;
                            }
                        }
                        return high + 1; //mid
                    };
                    self.new_cursor_pos = search_old_cursor_pos(visible_entries, self.cursor_pos);
                }
                self.cursor_pos = self.new_cursor_pos;
                self.recalc_visible_entries();
                self.dirty = true;
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
        (self.cursor_pos != self.new_cursor_pos)
            || self.dirty
            || (self.show_mailview && self.mailview.is_dirty())
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        self.mailview.set_dirty();
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMap {
        let mut map = self.mailview.get_shortcuts(context);

        map.insert("reply", Key::Char('R'));
        map.insert("reverse thread order", Key::Ctrl('r'));
        map.insert("toggle_mailview", Key::Char('p'));
        map.insert("toggle_subthread visibility", Key::Char('h'));

        map
    }
}
