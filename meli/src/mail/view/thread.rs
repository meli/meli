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

use std::cmp;

use melib::{
    utils::datetime::{timestamp_to_string, UnixTimestamp},
    Address,
};

use super::*;
use crate::components::PageMovement;

#[derive(Debug)]
struct ThreadEntry {
    index: (usize, ThreadNodeHash, usize),
    /// (indentation, thread_node index, line number in listing)
    indentation: usize,
    msg_hash: EnvelopeHash,
    seen: bool,
    dirty: bool,
    hidden: bool,
    heading: String,
    timestamp: UnixTimestamp,
    mailview: Box<MailView>,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ThreadViewFocus {
    #[default]
    None,
    Thread,
    MailView,
}

#[derive(Debug, Default)]
pub struct ThreadView {
    new_cursor_pos: usize,
    cursor_pos: usize,
    expanded_pos: usize,
    new_expanded_pos: usize,
    reversed: bool,
    coordinates: (AccountHash, MailboxHash, EnvelopeHash),
    thread_group: ThreadHash,
    focus: ThreadViewFocus,
    entries: Vec<ThreadEntry>,
    visible_entries: Vec<Vec<usize>>,
    //indentation_colors: [ThemeAttribute; 6],
    use_color: bool,
    horizontal: Option<bool>,
    movement: Option<PageMovement>,
    dirty: bool,
    content: Screen<Virtual>,
    id: ComponentId,
}

impl ThreadView {
    /// @coordinates: (account index, mailbox_hash, root set thread_node index)
    /// @expanded_hash: optional position of expanded entry when we render the
    ///                 ThreadView.
    ///                 default: expanded message is the last one.
    /// @context: current context
    pub fn new(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        thread_group: ThreadHash,
        expanded_hash: Option<EnvelopeHash>,
        go_to_first_unread: bool,
        focus: Option<ThreadViewFocus>,
        context: &mut Context,
    ) -> Self {
        let mut view = ThreadView {
            reversed: false,
            coordinates,
            thread_group,
            focus: focus.unwrap_or_default(),
            entries: Vec::new(),
            cursor_pos: 1,
            new_cursor_pos: 0,
            dirty: true,
            id: ComponentId::default(),
            //indentation_colors: [
            //    crate::conf::value(context, "mail.view.thread.indentation.a"),
            //    crate::conf::value(context, "mail.view.thread.indentation.b"),
            //    crate::conf::value(context, "mail.view.thread.indentation.c"),
            //    crate::conf::value(context, "mail.view.thread.indentation.d"),
            //    crate::conf::value(context, "mail.view.thread.indentation.e"),
            //    crate::conf::value(context, "mail.view.thread.indentation.f"),
            //],
            use_color: context.settings.terminal.use_color(),
            horizontal: None,
            ..Default::default()
        };
        view.initiate(expanded_hash, go_to_first_unread, context);
        view.new_cursor_pos = view.new_expanded_pos;
        view
    }

    pub fn update(&mut self, context: &mut Context) {
        if self.entries.is_empty() {
            return;
        }

        let old_entries = std::mem::take(&mut self.entries);

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

        let expanded_hash = old_expanded_entry.as_ref().map(|e| e.msg_hash);
        self.initiate(expanded_hash, false, context);

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
        self.set_dirty(true);
    }

    fn initiate(
        &mut self,
        expanded_hash: Option<EnvelopeHash>,
        go_to_first_unread: bool,
        context: &mut Context,
    ) {
        #[inline(always)]
        fn make_entry(
            i: (usize, ThreadNodeHash, usize),
            (account_hash, mailbox_hash, msg_hash): (AccountHash, MailboxHash, EnvelopeHash),
            seen: bool,
            initialize_now: bool,
            timestamp: UnixTimestamp,
            context: &mut Context,
        ) -> ThreadEntry {
            let (ind, _, _) = i;
            ThreadEntry {
                index: i,
                indentation: ind,
                mailview: Box::new(MailView::new(
                    Some((account_hash, mailbox_hash, msg_hash)),
                    initialize_now,
                    context,
                )),
                msg_hash,
                seen,
                dirty: true,
                hidden: false,
                heading: String::new(),
                timestamp,
            }
        }

        let collection = context.accounts[&self.coordinates.0].collection.clone();
        let threads = collection.get_threads(self.coordinates.1);

        if !threads.groups.contains_key(&self.thread_group) {
            return;
        }
        let (account_hash, mailbox_hash, _) = self.coordinates;

        // Find out how many entries there are going to be, and prioritize
        // initialization to the open entry and the most recent ones.
        //
        // This helps skip initializing the whole thread at once, which will make the UI
        // loading slower.
        //
        // This won't help at all if the latest entry is a reply to an older entry but
        // oh well.
        let mut total_entries = vec![];
        for (_, thread_node_hash) in threads.thread_iter(self.thread_group) {
            if let Some(msg_hash) = threads.thread_nodes()[&thread_node_hash].message() {
                if Some(msg_hash) == expanded_hash {
                    continue;
                }
                let env_ref = collection.get_env(msg_hash);
                total_entries.push((msg_hash, env_ref.timestamp));
            };
        }
        total_entries.sort_by_key(|e| cmp::Reverse(e.1));
        let tokens = f64::from(u32::try_from(total_entries.len()).unwrap_or(0)) * 0.29;
        let tokens = tokens.ceil() as usize;
        total_entries.truncate(tokens);

        // Now, only the expanded envelope plus the ones that remained in total_entries
        // (around 30% of the total messages in the thread) will be scheduled
        // for loading immediately. The others will be lazily loaded when the
        // user opens them for reading.

        let thread_iter = threads.thread_iter(self.thread_group);
        self.entries.clear();
        let mut earliest_unread = 0;
        let mut earliest_unread_entry = 0;
        for (line, (ind, thread_node_hash)) in thread_iter.enumerate() {
            let entry = if let Some(msg_hash) = threads.thread_nodes()[&thread_node_hash].message()
            {
                let (is_seen, timestamp) = {
                    let env_ref = collection.get_env(msg_hash);
                    if !env_ref.is_seen()
                        && (earliest_unread == 0 || env_ref.timestamp < earliest_unread)
                    {
                        earliest_unread = env_ref.timestamp;
                        earliest_unread_entry = self.entries.len();
                    }
                    (env_ref.is_seen(), env_ref.timestamp)
                };
                let initialize_now = if total_entries.is_empty() {
                    false
                } else {
                    // ExtractIf but it hasn't been stabilized yet.
                    // https://doc.rust-lang.org/std/vec/struct.Vec.html#method.extract_if
                    let mut i = 0;
                    let mut result = false;
                    while i < total_entries.len() {
                        if total_entries[i].0 == msg_hash {
                            total_entries.remove(i);
                            result = true;
                            break;
                        } else {
                            i += 1;
                        }
                    }
                    result
                };
                make_entry(
                    (ind, thread_node_hash, line),
                    (account_hash, mailbox_hash, msg_hash),
                    is_seen,
                    initialize_now || expanded_hash == Some(msg_hash),
                    timestamp,
                    context,
                )
            } else {
                continue;
            };
            match expanded_hash {
                Some(expanded_hash) if expanded_hash == entry.msg_hash => {
                    self.new_expanded_pos = self.entries.len();
                    self.expanded_pos = self.new_expanded_pos + 1;
                }
                _ => {}
            }
            self.entries.push(entry);
        }
        if expanded_hash.is_none() {
            self.new_expanded_pos = self
                .entries
                .iter()
                .enumerate()
                .reduce(|a, b| if a.1.timestamp > b.1.timestamp { a } else { b })
                .map(|el| el.0)
                .unwrap_or(0);
            self.expanded_pos = self.new_expanded_pos + 1;
        }
        if go_to_first_unread && earliest_unread > 0 {
            self.new_expanded_pos = earliest_unread_entry;
            self.expanded_pos = earliest_unread_entry + 1;
        }

        let height = self.entries.len();
        let mut width = 0;

        let mut highlight_reply_subjects: Vec<Option<usize>> =
            Vec::with_capacity(self.entries.len());
        for e in &mut self.entries {
            let envelope: EnvelopeRef = context.accounts[&self.coordinates.0]
                .collection
                .get_env(e.msg_hash);
            let thread_node = &threads.thread_nodes()[&e.index.1];
            let from = Address::display_name_slice(envelope.from());
            let date = timestamp_to_string(envelope.date(), Some("%Y-%m-%d %H:%M\0"), true);
            e.heading = if thread_node.show_subject() {
                let subject = envelope.subject();
                highlight_reply_subjects.push(Some(subject.grapheme_width()));
                format!(
                    "{date} {subject:`>indent$} {from}",
                    indent = 2 * e.index.0 + subject.grapheme_width(),
                )
            } else {
                highlight_reply_subjects.push(None);
                format!(
                    "{date} {from:`>indent$}",
                    indent = 2 * e.index.0 + from.grapheme_width()
                )
            };
            width = width.max(e.heading.grapheme_width() + 1);
        }
        if !self.content.resize_with_context(width, height, context) {
            return;
        }
        let theme_default = crate::conf::value(context, "theme_default");
        let highlight_theme = crate::conf::value(context, "highlight");
        if self.reversed {
            for (y, e) in self.entries.iter().rev().enumerate() {
                {
                    let area = self
                        .content
                        .area()
                        .skip_rows(y)
                        .take(e.heading.grapheme_width() + 1, height - 1);
                    self.content.grid_mut().write_string(
                        &e.heading,
                        if e.seen {
                            theme_default.fg
                        } else {
                            highlight_theme.fg
                        },
                        if e.seen {
                            theme_default.bg
                        } else {
                            highlight_theme.bg
                        },
                        theme_default.attrs,
                        area,
                        None,
                    );
                }
                if highlight_reply_subjects[y].is_some() {
                    let area = self.content.area().skip_rows(y).take_rows(1);
                    self.content.grid_mut().change_theme(area, highlight_theme);
                }
            }
        } else {
            for (y, e) in self.entries.iter().enumerate() {
                {
                    let area = self
                        .content
                        .area()
                        .skip_rows(y)
                        .take(e.heading.grapheme_width(), height - 1);
                    self.content.grid_mut().write_string(
                        &e.heading,
                        if e.seen {
                            theme_default.fg
                        } else {
                            highlight_theme.fg
                        },
                        if e.seen {
                            theme_default.bg
                        } else {
                            highlight_theme.bg
                        },
                        theme_default.attrs,
                        area,
                        None,
                    );
                }
                if highlight_reply_subjects[y].is_some() {
                    let area = self.content.area().skip_rows(y).take(width - 2, y);
                    self.content.grid_mut().change_theme(area, highlight_theme);
                }
            }
        }
        self.visible_entries = vec![(0..self.entries.len()).collect()];
    }

    fn highlight_line(
        &self,
        grid: &mut CellBuffer,
        dest_area: Area,
        src_area: Area,
        idx: usize,
        context: &Context,
    ) {
        let visibles: Vec<&usize> = self.visible_entries.iter().flat_map(|v| v.iter()).collect();
        if idx == *visibles[self.cursor_pos] {
            let theme_default = crate::conf::value(context, "theme_default");
            let bg_color = crate::conf::value(context, "highlight").bg;
            let attrs = if self.use_color {
                theme_default.attrs
            } else {
                Attr::REVERSE
            };

            grid.change_theme(
                dest_area,
                ThemeAttribute {
                    fg: theme_default.fg,
                    bg: bg_color,
                    attrs,
                },
            );
            return;
        }

        grid.copy_area(self.content.grid(), dest_area, src_area);
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            context.dirty_areas.push_back(area);
            return;
        }
        let height = self.content.area().height();
        if height == 0 {
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = area.height();
        if rows == 0 {
            context.dirty_areas.push_back(area);
            return;
        }
        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::Up(amount) => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(rows * multiplier);
                }
                PageMovement::Down(amount) => {
                    if self.new_cursor_pos + amount + 1 < height {
                        self.new_cursor_pos += amount;
                    } else if self.new_cursor_pos + amount > height {
                        self.new_cursor_pos = height - 1;
                    } else {
                        self.new_cursor_pos = (height / rows) * rows;
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    if self.new_cursor_pos + rows * multiplier + 1 < height {
                        self.new_cursor_pos += rows * multiplier;
                    } else {
                        self.new_cursor_pos = (height / rows) * rows;
                    }
                }
                PageMovement::Right(_) | PageMovement::Left(_) => {}
                PageMovement::Home => {
                    self.new_cursor_pos = 0;
                }
                PageMovement::End => {
                    self.new_cursor_pos = (height / rows) * rows;
                }
            }
        }
        if self.new_cursor_pos >= self.entries.len() {
            self.new_cursor_pos = self.entries.len().saturating_sub(1);
        }
        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);

        let top_idx = page_no * rows;
        // returns the **line** of an entry in the ThreadView grid.
        let get_entry_area = |idx: usize| self.content.area().skip_rows(idx).take_rows(1);

        if self.dirty || (page_no != prev_page_no) {
            grid.clear_area(area, crate::conf::value(context, "theme_default"));
            let visibles: Vec<&usize> =
                self.visible_entries.iter().flat_map(|v| v.iter()).collect();

            for (visible_entry_counter, v) in visibles.iter().skip(top_idx).take(rows).enumerate() {
                if visible_entry_counter >= rows {
                    break;
                }
                let idx = *v;

                grid.copy_area(
                    self.content.grid(),
                    area.skip_rows(visible_entry_counter).take_rows(1),
                    self.content.area().skip_rows(*idx).take_rows(1),
                );
            }
            // If cursor position has changed, remove the highlight from the previous
            // position and apply it in the new one.
            self.cursor_pos = self.new_cursor_pos;
            if self.cursor_pos + 1 > visibles.len() {
                self.cursor_pos = visibles.len().saturating_sub(1);
            }
            let idx = *visibles[self.cursor_pos];
            let src_area = get_entry_area(idx);
            let dest_area = area.skip_rows(self.cursor_pos - top_idx).take_rows(1);

            self.highlight_line(grid, dest_area, src_area, idx, context);
            if rows < visibles.len() {
                ScrollBar::default().set_show_arrows(true).draw(
                    grid,
                    area.nth_col(area.width().saturating_sub(1)),
                    context,
                    self.cursor_pos,
                    rows,
                    visibles.len(),
                );
            }
            if top_idx + rows > visibles.len() {
                grid.clear_area(
                    area.skip_rows(visibles.len() - top_idx),
                    crate::conf::value(context, "theme_default"),
                );
            }
        } else {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            // If cursor position has changed, remove the highlight from the previous
            // position and apply it in the new one.
            let visibles: Vec<&usize> =
                self.visible_entries.iter().flat_map(|v| v.iter()).collect();
            for &idx in &[old_cursor_pos, self.cursor_pos] {
                let entry_idx = *visibles[idx];
                let src_area = get_entry_area(entry_idx);
                let dest_area = area.skip_rows(visibles[..idx].len() - top_idx).take_rows(1);

                self.highlight_line(grid, dest_area, src_area, entry_idx, context);
                if rows < visibles.len() {
                    ScrollBar::default().set_show_arrows(true).draw(
                        grid,
                        area.nth_col(area.width().saturating_sub(1)),
                        context,
                        self.cursor_pos,
                        rows,
                        visibles.len(),
                    );
                }
            }
        }
        context.dirty_areas.push_back(area);
    }

    fn draw_vert(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            return;
        }
        let mid = self.content.area().width().min(area.width() / 2);

        let theme_default = crate::conf::value(context, "theme_default");
        // First draw the thread subject on the first row
        if self.dirty {
            grid.clear_area(area, theme_default);
            let account = &context.accounts[&self.coordinates.0];
            let threads = account.collection.get_threads(self.coordinates.1);
            let thread_root = threads.thread_iter(self.thread_group).next().unwrap().1;
            let thread_node = &threads.thread_nodes()[&thread_root];
            let i = thread_node.message().unwrap_or_else(|| {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[&iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                }
                threads.thread_nodes()[&iter_ptr].message().unwrap()
            });
            let envelope: EnvelopeRef = account.collection.get_env(i);

            let (_, y) = grid.write_string(
                &envelope.subject(),
                theme_default.fg,
                theme_default.bg,
                theme_default.attrs,
                area,
                Some(0),
            );
            context.dirty_areas.push_back(area);
            grid.clear_area(area.nth_col(mid), theme_default);
            grid.clear_area(area.skip(mid, y + 1), theme_default);
        };
        let area = area.skip_rows(2);
        let (width, height) = self.content.area().size();
        if height == 0 || width == 0 {
            return;
        }

        match self.focus {
            ThreadViewFocus::None => {
                self.draw_list(grid, area.take_cols(mid.saturating_sub(1)), context);
                self.entries[self.new_expanded_pos].mailview.draw(
                    grid,
                    area.skip_cols(mid + 1),
                    context,
                );
            }
            ThreadViewFocus::Thread => {
                grid.clear_area(area.skip_cols(mid + 1), theme_default);
                self.draw_list(grid, area, context);
            }
            ThreadViewFocus::MailView => {
                self.entries[self.new_expanded_pos]
                    .mailview
                    .draw(grid, area, context);
            }
        }
        context.dirty_areas.push_back(area);
    }

    //fn draw_horz(&mut self, grid: &mut CellBuffer, area: Area, context: &mut
    // Context) {    if self.entries.is_empty() {
    //        return;
    //    }
    //    let upper_left = area.upper_left();
    //    let bottom_right = area.bottom_right();
    //    let total_rows = area.height();

    //    let pager_ratio = *mailbox_settings!(
    //        context[self.coordinates.0][&self.coordinates.1]
    //            .pager
    //            .pager_ratio
    //    );
    //    let mut bottom_entity_rows = (pager_ratio * total_rows) / 100;

    //    if bottom_entity_rows > total_rows {
    //        bottom_entity_rows = total_rows.saturating_sub(1);
    //    }

    //    let mut mid = get_y(upper_left) + total_rows - bottom_entity_rows;
    //    if mid >= get_y(bottom_right) {
    //        mid = get_y(bottom_right) / 2;
    //    }
    //    let mid = mid;

    //    let theme_default = crate::conf::value(context, "theme_default");
    //    // First draw the thread subject on the first row
    //    let y = {
    //        grid.clear_area(area, theme_default);
    //        let account = &context.accounts[&self.coordinates.0];
    //        let threads = account.collection.get_threads(self.coordinates.1);
    //        let thread_root =
    // threads.thread_iter(self.thread_group).next().unwrap().1;        let
    // thread_node = &threads.thread_nodes()[&thread_root];        let i =
    // thread_node.message().unwrap_or_else(|| {            let mut iter_ptr =
    // thread_node.children()[0];            while
    // threads.thread_nodes()[&iter_ptr].message().is_none() {
    // iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];            }
    //            threads.thread_nodes()[&iter_ptr].message().unwrap()
    //        });
    //        let envelope: EnvelopeRef = account.collection.get_env(i);

    //        let (x, y) = grid.write_string(
    //            &envelope.subject(),
    //            theme_default.fg,
    //            theme_default.bg,
    //            theme_default.attrs,
    //            area,
    //            Some(get_x(upper_left)),
    //        );
    //        for x in x..=get_x(bottom_right) {
    //            grid[(x, y)]
    //                .set_ch(' ')
    //                .set_fg(theme_default.fg)
    //                .set_bg(theme_default.bg);
    //        }
    //        context.dirty_areas.push_back(area);
    //        y + 2
    //    };

    //    for x in get_x(upper_left)..=get_x(bottom_right) {
    //        set_and_join_box(grid, (x, y - 1), BoxBoundary::Horizontal);
    //        grid[(x, y - 1)]
    //            .set_fg(theme_default.fg)
    //            .set_bg(theme_default.bg);
    //    }

    //    let (width, height) = self.content.area().size();
    //    if height == 0 || height == self.cursor_pos || width == 0 {
    //        return;
    //    }

    //    grid.clear_area(area.skip_rows(y).take_rows(mid + 1), theme_default);

    //    match self.focus {
    //        ThreadViewFocus::None => {
    //            let area = area.skip_rows(y).take_rows(mid);
    //            let rows = area.height() / 2;
    //            if rows == 0 {
    //                return;
    //            }
    //            let page_no = (self.new_cursor_pos).wrapping_div(rows);
    //            let top_idx = page_no * rows;

    //            grid.copy_area(
    //                self.content.grid(),
    //                area,
    //                self.content.area().skip_rows(top_idx),
    //            );
    //            context.dirty_areas.push_back(area);
    //        }
    //        ThreadViewFocus::Thread => {
    //            let area = {
    //                let val = area.skip_rows(y);
    //                if val.height() < 20 {
    //                    area
    //                } else {
    //                    val
    //                }
    //            };
    //            let rows = area.height() / 2;
    //            if rows == 0 {
    //                return;
    //            }
    //            let page_no = (self.new_cursor_pos).wrapping_div(rows);
    //            let top_idx = page_no * rows;

    //            grid.copy_area(
    //                self.content.grid(),
    //                area,
    //                self.content.area().skip_rows(top_idx),
    //            );
    //            context.dirty_areas.push_back(area);
    //        }
    //        ThreadViewFocus::MailView => { /* show only envelope */ }
    //    }

    //    match self.focus {
    //        ThreadViewFocus::None => {
    //            {
    //                let area = {
    //                    let val = area.skip_rows(mid);
    //                    if val.height() < 20 {
    //                        area
    //                    } else {
    //                        val
    //                    }
    //                };
    //                context.dirty_areas.push_back(area);
    //                for x in get_x(area.upper_left())..=get_x(area.bottom_right())
    // {                    set_and_join_box(grid, (x, mid),
    // BoxBoundary::Horizontal);                    grid[(x, mid)]
    //                        .set_fg(theme_default.fg)
    //                        .set_bg(theme_default.bg);
    //                }
    //            }
    //            {
    //                let area = area.skip_rows(y).take_rows(mid - 1);
    //                self.draw_list(grid, area, context);
    //            }
    //            let area = area.take_rows(mid);
    //            self.entries[self.new_expanded_pos]
    //                .mailview
    //                .draw(grid, area, context);
    //        }
    //        ThreadViewFocus::Thread => {
    //            self.dirty = true;
    //            self.draw_list(grid, area.skip_rows(y), context);
    //        }
    //        ThreadViewFocus::MailView => {
    //            self.entries[self.new_expanded_pos]
    //                .mailview
    //                .draw(grid, area, context);
    //        }
    //    }
    //}

    fn recalc_visible_entries(&mut self) {
        if self.entries.is_empty() {
            return;
        }
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
                    (vec![Vec::new()], SmallVec::<[_; 8]>::new(), false),
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
                                // [ref:FIXME]: pop all until e.indentation
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
        if self.reversed {
            self.visible_entries.reverse()
        }
    }

    /// Current position in self.entries (not in drawn entries which might
    /// exclude nonvisible ones)
    fn current_pos(&self) -> Option<usize> {
        self.visible_entries
            .iter()
            .flat_map(|v| v.iter())
            .nth(self.new_cursor_pos)
            .copied()
    }
}

impl std::fmt::Display for ThreadView {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(e) = self.entries.get(0) {
            e.mailview.fmt(fmt)
        } else {
            write!(fmt, "view thread")
        }
    }
}

impl Component for ThreadView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            self.set_dirty(false);
        }
        if !self.is_dirty() {
            return;
        }

        // If user has selected another mail to view, change to it
        if self.new_expanded_pos != self.expanded_pos {
            self.expanded_pos = self.new_expanded_pos;
        }

        if self.entries.len() == 1 {
            self.entries[self.new_expanded_pos]
                .mailview
                .draw(grid, area, context);
        } else {
            self.draw_vert(grid, area, context);
        }
        self.set_dirty(false);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let (UIEvent::Action(Listing(OpenInNewTab)), false) = (&event, self.entries.is_empty()) {
            // Handle this before self.mailview does
            let mut new_tab = Self::new(
                self.coordinates,
                self.thread_group,
                Some(self.entries[self.expanded_pos].msg_hash),
                false,
                Some(self.focus),
                context,
            );
            new_tab.set_dirty(true);
            context
                .replies
                .push_back(UIEvent::Action(Tab(New(Some(Box::new(new_tab))))));
            return true;
        }

        if matches!(
            self.focus,
            ThreadViewFocus::None | ThreadViewFocus::MailView
        ) && !self.entries.is_empty()
            && self.entries[self.new_expanded_pos]
                .mailview
                .process_event(event, context)
        {
            return true;
        }

        let shortcuts = self.shortcuts(context);
        match *event {
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["toggle_layout"]) =>
            {
                if let Some(ref mut v) = self.horizontal {
                    *v = !*v;
                } else {
                    self.horizontal = Some(false);
                }
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["scroll_up"]) =>
            {
                if self.cursor_pos > 0 {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(1);
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["scroll_down"]) =>
            {
                let height = self.visible_entries.iter().flat_map(|v| v.iter()).count();
                if height > 0 && self.new_cursor_pos + 1 < height {
                    self.new_cursor_pos += 1;
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["prev_page"]) =>
            {
                self.movement = Some(PageMovement::PageUp(1));
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["next_page"]) =>
            {
                self.movement = Some(PageMovement::PageDown(1));
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref k) if shortcut!(k == shortcuts[Shortcuts::GENERAL]["home_page"]) => {
                self.movement = Some(PageMovement::Home);
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref k) if shortcut!(k == shortcuts[Shortcuts::GENERAL]["end_page"]) => {
                self.movement = Some(PageMovement::End);
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Shortcuts::GENERAL]["open_entry"]) =>
            {
                if self.entries.len() > 1 {
                    if let Some(new_expanded_pos) = self.current_pos() {
                        self.new_expanded_pos = new_expanded_pos;
                        self.expanded_pos = new_expanded_pos;
                        if matches!(self.focus, ThreadViewFocus::Thread) {
                            self.focus = ThreadViewFocus::None;
                        }
                        self.set_dirty(true);
                    }
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["toggle_mailview"]) =>
            {
                self.focus = match self.focus {
                    ThreadViewFocus::None | ThreadViewFocus::MailView => ThreadViewFocus::Thread,
                    ThreadViewFocus::Thread => ThreadViewFocus::None,
                };
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["toggle_threadview"]) =>
            {
                self.focus = match self.focus {
                    ThreadViewFocus::None | ThreadViewFocus::Thread => ThreadViewFocus::MailView,
                    ThreadViewFocus::MailView => ThreadViewFocus::None,
                };
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["reverse_thread_order"]) =>
            {
                if !self.entries.is_empty() {
                    self.reversed = !self.reversed;
                    let expanded_hash = self.entries[self.expanded_pos].msg_hash;
                    self.initiate(Some(expanded_hash), false, context);
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::THREAD_VIEW]["collapse_subtree"]) =>
            {
                if self.entries.is_empty() {
                    return true;
                }
                if let Some(current_pos) = self.current_pos() {
                    self.entries[current_pos].hidden = !self.entries[current_pos].hidden;
                    self.entries[current_pos].dirty = true;
                    {
                        let visible_entries: Vec<&usize> =
                            self.visible_entries.iter().flat_map(|v| v.iter()).collect();
                        // search_old_cursor_pos
                        self.new_cursor_pos =
                            (|entries: Vec<&usize>, x: usize| {
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
                                high + 1 //mid
                            })(visible_entries, self.cursor_pos);
                    }
                    self.cursor_pos = self.new_cursor_pos;
                    self.recalc_visible_entries();
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Resize | UIEvent::VisibilityChange(true) => {
                self.set_dirty(true);
                false
            }
            UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                let account = &context.accounts[&self.coordinates.0];
                for e in self.entries.iter_mut() {
                    if e.msg_hash == *old_hash {
                        e.msg_hash = *new_hash;
                        let seen: bool = account.collection.get_env(*new_hash).is_seen();
                        e.dirty = e.seen != seen;
                        e.seen = seen;
                        e.mailview.process_event(
                            &mut UIEvent::EnvelopeRename(*old_hash, *new_hash),
                            context,
                        );
                        self.set_dirty(true);
                        break;
                    }
                }
                false
            }
            UIEvent::EnvelopeUpdate(ref env_hash) => {
                let account = &context.accounts[&self.coordinates.0];
                for e in self.entries.iter_mut() {
                    if e.msg_hash == *env_hash {
                        let seen: bool = account.collection.get_env(*env_hash).is_seen();
                        e.dirty = e.seen != seen;
                        e.seen = seen;
                        e.mailview
                            .process_event(&mut UIEvent::EnvelopeUpdate(*env_hash), context);
                        self.set_dirty(true);
                        break;
                    }
                }
                false
            }
            _ => {
                if self
                    .entries
                    .iter_mut()
                    .any(|entry| entry.mailview.process_event(event, context))
                {
                    return true;
                }
                false
            }
        }
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || (!matches!(self.focus, ThreadViewFocus::Thread)
                && !self.entries.is_empty()
                && self.entries[self.new_expanded_pos].mailview.is_dirty())
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let Some(entry) = self.entries.get_mut(self.new_expanded_pos) {
            entry.mailview.set_dirty(value);
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if !self.entries.is_empty() {
            self.entries[self.new_expanded_pos]
                .mailview
                .shortcuts(context)
        } else {
            ShortcutMaps::default()
        };

        map.insert(
            Shortcuts::GENERAL,
            mailbox_settings!(
                context[self.coordinates.0][&self.coordinates.1]
                    .shortcuts
                    .general
            )
            .key_values(),
        );
        map.insert(
            Shortcuts::THREAD_VIEW,
            mailbox_settings!(
                context[self.coordinates.0][&self.coordinates.1]
                    .shortcuts
                    .thread_view
            )
            .key_values(),
        );

        map
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
