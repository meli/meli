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
use crate::components::utilities::PageMovement;
use std::cmp;
use std::ops::{Deref, DerefMut};

//use melib::mailbox::backends::BackendOp;

const MAX_COLS: usize = 500;

macro_rules! address_list {
    (($name:expr) as comma_sep_list) => {{
        let mut ret: String =
            $name
                .into_iter()
                .fold(String::new(), |mut s: String, n: &Address| {
                    s.extend(n.to_string().chars());
                    s.push_str(", ");
                    s
                });
        ret.pop();
        ret.pop();
        ret
    }};
}

macro_rules! column_str {
    (
        struct $name:ident(String)) => {
        pub struct $name(String);

        impl Deref for $name {
            type Target = String;
            fn deref(&self) -> &String {
                &self.0
            }
        }
        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut String {
                &mut self.0
            }
        }
    };
}

column_str!(struct IndexNoString(String));
column_str!(struct DateString(String));
column_str!(struct FromString(String));
column_str!(struct SubjectString(String));
column_str!(struct FlagString(String));

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `ThreadView`.
#[derive(Debug)]
pub struct CompactListing {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    order: FnvHashMap<EnvelopeHash, usize>,
    /// Cache current view.
    data_columns: DataColumns,

    filter_term: String,
    filtered_selection: Vec<EnvelopeHash>,
    filtered_order: FnvHashMap<EnvelopeHash, usize>,
    selection: FnvHashMap<EnvelopeHash, bool>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: ThreadView,
    row_updates: StackVec<EnvelopeHash>,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl ListingTrait for CompactListing {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>) {
        (self.cursor_pos.0, self.cursor_pos.1, None)
    }
    fn set_coordinates(&mut self, coordinates: (usize, usize, Option<EnvelopeHash>)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.unfocused = false;
    }
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        if self.length == 0 {
            return;
        }
        let i = self.get_envelope_under_cursor(idx, context);
        let account = &context.accounts[self.cursor_pos.0];
        let is_seen = {
            let root_envelope: &Envelope = &account.get_env(&i);
            root_envelope.is_seen()
        };

        let fg_color = if !is_seen {
            Color::Byte(0)
        } else {
            Color::Default
        };
        let bg_color = if self.cursor_pos.2 == idx && self.selection[&i] {
            Color::Byte(246 | 210)
        } else if self.cursor_pos.2 == idx {
            Color::Byte(246)
        } else if self.selection[&i] {
            Color::Byte(210)
        } else if !is_seen {
            Color::Byte(251)
        } else if idx % 2 == 0 {
            Color::Byte(236)
        } else {
            Color::Default
        };
        if !grid.is_empty() {
            change_colors(grid, area, fg_color, bg_color);
            return;
        }

        let (upper_left, bottom_right) = area;
        let (mut x, _y) = upper_left;
        for i in 0..self.data_columns.columns.len() {
            let (width, height) = self.data_columns.columns[i].size();
            if self.data_columns.widths[i] == 0 {
                continue;
            }
            copy_area(
                grid,
                &self.data_columns.columns[i],
                (
                    set_x(upper_left, x),
                    set_x(
                        bottom_right,
                        std::cmp::min(get_x(bottom_right), x + (self.data_columns.widths[i])),
                    ),
                ),
                ((0, idx), (width.saturating_sub(1), height - 1)),
            );
            if i != self.data_columns.columns.len() - 1 {
                change_colors(
                    grid,
                    (
                        set_x(
                            upper_left,
                            x + self.data_columns.widths[i].saturating_sub(1),
                        ),
                        set_x(bottom_right, x + self.data_columns.widths[i] + 1),
                    ),
                    fg_color,
                    bg_color,
                );
            } else {
                change_colors(
                    grid,
                    (
                        set_x(
                            upper_left,
                            std::cmp::min(get_x(bottom_right), x + (self.data_columns.widths[i])),
                        ),
                        bottom_right,
                    ),
                    fg_color,
                    bg_color,
                );
            }
            x += self.data_columns.widths[i] + 2; // + SEPARATOR
            if x > get_x(bottom_right) {
                break;
            }
        }
    }
    /// Draw the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0
        {
            self.refresh_mailbox(context);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area);
            copy_area(
                grid,
                &self.data_columns.columns[0],
                area,
                ((0, 0), pos_dec(self.data_columns.columns[0].size(), (1, 1))),
            );
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;

        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::PageUp => {
                    self.new_cursor_pos.2 = self.new_cursor_pos.2.saturating_sub(rows);
                }
                PageMovement::PageDown => {
                    if self.new_cursor_pos.2 + rows + 1 < self.length {
                        self.new_cursor_pos.2 += rows;
                    } else {
                        self.new_cursor_pos.2 = (self.length / rows) * rows;
                    }
                }
                PageMovement::Home => {
                    self.new_cursor_pos.2 = 0;
                }
                PageMovement::End => {
                    self.new_cursor_pos.2 = (self.length / rows) * rows;
                }
            }
        }

        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let top_idx = page_no * rows;

        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if self.cursor_pos.2 != self.new_cursor_pos.2 && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for idx in &[old_cursor_pos.2, self.new_cursor_pos.2] {
                if *idx >= self.length {
                    continue; //bounds check
                }
                let new_area = (
                    set_y(upper_left, get_y(upper_left) + (*idx % rows)),
                    set_y(bottom_right, get_y(upper_left) + (*idx % rows)),
                );
                self.highlight_line(grid, new_area, *idx, context);
                context.dirty_areas.push_back(new_area);
            }
            return;
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }
        if self.new_cursor_pos.2 >= self.length {
            self.new_cursor_pos.2 = self.length - 1;
            self.cursor_pos.2 = self.new_cursor_pos.2;
        }

        let width = width!(area);
        self.data_columns.widths = Default::default();
        self.data_columns.widths[0] = self.data_columns.columns[0].size().0;
        self.data_columns.widths[1] = self.data_columns.columns[1].size().0; /* date*/
        self.data_columns.widths[2] = self.data_columns.columns[2].size().0; /* from */
        self.data_columns.widths[3] = self.data_columns.columns[3].size().0; /* flags */
        self.data_columns.widths[4] = self.data_columns.columns[4].size().0; /* subject */

        let min_col_width = std::cmp::min(
            15,
            std::cmp::min(self.data_columns.widths[4], self.data_columns.widths[2]),
        );
        if self.data_columns.widths[0] + self.data_columns.widths[1] + 3 * min_col_width + 8 > width
        {
            let remainder = width
                .saturating_sub(self.data_columns.widths[0])
                .saturating_sub(self.data_columns.widths[1])
                - 4;
            self.data_columns.widths[2] = remainder / 6;
            self.data_columns.widths[4] = (2 * remainder) / 3 - self.data_columns.widths[3];
        } else {
            let remainder = width
                .saturating_sub(self.data_columns.widths[0])
                .saturating_sub(self.data_columns.widths[1])
                .saturating_sub(8);
            if min_col_width + self.data_columns.widths[4] > remainder {
                self.data_columns.widths[4] =
                    remainder - min_col_width - self.data_columns.widths[3];
                self.data_columns.widths[2] = min_col_width;
            }
        }
        clear_area(grid, area);
        /* Page_no has changed, so draw new page */
        let mut x = get_x(upper_left);
        let mut flag_x = 0;
        for i in 0..self.data_columns.columns.len() {
            let column_width = self.data_columns.columns[i].size().0;
            if i == 3 {
                flag_x = x;
            }
            if self.data_columns.widths[i] == 0 {
                continue;
            }
            copy_area(
                grid,
                &self.data_columns.columns[i],
                (
                    set_x(upper_left, x),
                    set_x(
                        bottom_right,
                        std::cmp::min(get_x(bottom_right), x + (self.data_columns.widths[i])),
                    ),
                ),
                (
                    (0, top_idx),
                    (column_width.saturating_sub(1), self.length - 1),
                ),
            );
            x += self.data_columns.widths[i] + 2; // + SEPARATOR
            if x > get_x(bottom_right) {
                break;
            }
        }
        for r in 0..cmp::min(self.length - top_idx, rows) {
            let (fg_color, bg_color) = {
                let c = &self.data_columns.columns[0][(0, r + top_idx)];
                (c.fg(), c.bg())
            };
            change_colors(
                grid,
                (
                    pos_inc(upper_left, (0, r)),
                    (flag_x - 1, get_y(upper_left) + r),
                ),
                fg_color,
                bg_color,
            );
            for x in flag_x..(flag_x + 2 + self.data_columns.widths[3]) {
                grid[(x, get_y(upper_left) + r)].set_bg(bg_color);
            }
            change_colors(
                grid,
                (
                    (
                        flag_x + 2 + self.data_columns.widths[3],
                        get_y(upper_left) + r,
                    ),
                    (get_x(bottom_right), get_y(upper_left) + r),
                ),
                fg_color,
                bg_color,
            );
        }

        /* TODO: highlight selected entries */
        self.highlight_line(
            grid,
            (
                set_y(upper_left, get_y(upper_left) + (self.cursor_pos.2 % rows)),
                set_y(bottom_right, get_y(upper_left) + (self.cursor_pos.2 % rows)),
            ),
            self.cursor_pos.2,
            context,
        );

        if top_idx + rows > self.length {
            clear_area(
                grid,
                (
                    pos_inc(upper_left, (0, self.length - top_idx)),
                    bottom_right,
                ),
            );
        }
        context.dirty_areas.push_back(area);
    }
    fn filter(&mut self, filter_term: &str, context: &Context) {
        self.filtered_order.clear();
        self.filtered_selection.clear();
        for v in self.selection.values_mut() {
            *v = false;
        }
        self.filter_term.clear();

        for (i, h) in self.order.keys().enumerate() {
            let account = &context.accounts[self.cursor_pos.0];
            let envelope = &account.collection[h];
            if envelope.subject().contains(&filter_term) {
                self.filtered_selection.push(*h);
                self.filtered_order.insert(*h, i);
                self.selection.insert(*h, false);
                continue;
            }
            if envelope.field_from_to_string().contains(&filter_term) {
                self.filtered_selection.push(*h);
                self.filtered_order.insert(*h, i);
                self.selection.insert(*h, false);
                continue;
            }
            let op = account.operation(*h);
            let body = envelope.body(op);
            let decoded = decode_rec(&body, None);
            let body_text = String::from_utf8_lossy(&decoded);
            if body_text.contains(&filter_term) {
                self.filtered_selection.push(*h);
                self.filtered_order.insert(*h, i);
                self.selection.insert(*h, false);
            }
        }
        if !self.filtered_selection.is_empty() {
            self.filter_term = filter_term.to_string();
            self.cursor_pos.2 = std::cmp::min(self.filtered_selection.len() - 1, self.cursor_pos.2);
            self.length = self.filtered_selection.len();
        } else {
            self.length = 0;
            let message = format!("No results for `{}`.", filter_term);
            self.data_columns.columns[0] =
                CellBuffer::new(message.len(), self.length + 1, Cell::with_char(' '));
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                Color::Default,
                Color::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                false,
            );
        }
    }
}

impl fmt::Display for CompactListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl Default for CompactListing {
    fn default() -> Self {
        CompactListing::new()
    }
}

impl CompactListing {
    const DESCRIPTION: &'static str = "compact listing";
    fn new() -> Self {
        CompactListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            order: FnvHashMap::default(),
            filter_term: String::new(),
            filtered_selection: Vec::new(),
            filtered_order: FnvHashMap::default(),
            selection: FnvHashMap::default(),
            row_updates: StackVec::new(),
            data_columns: DataColumns::default(),
            dirty: true,
            force_draw: true,
            unfocused: false,
            view: ThreadView::default(),

            movement: None,
            id: ComponentId::new_v4(),
        }
    }
    fn make_entry_string(
        e: &Envelope,
        len: usize,
        idx: usize,
        is_snoozed: bool,
    ) -> (
        IndexNoString,
        DateString,
        FromString,
        FlagString,
        SubjectString,
    ) {
        if len > 0 {
            (
                IndexNoString(idx.to_string()),
                DateString(CompactListing::format_date(e)),
                FromString(address_list!((e.from()) as comma_sep_list)),
                FlagString(format!(
                    "{}{}",
                    if e.has_attachments() { "📎" } else { "" },
                    if is_snoozed { "💤" } else { "" }
                )),
                SubjectString(format!("{} ({})", e.subject(), len,)),
            )
        } else {
            (
                IndexNoString(idx.to_string()),
                DateString(CompactListing::format_date(e)),
                FromString(address_list!((e.from()) as comma_sep_list)),
                FlagString(format!(
                    "{}{}",
                    if e.has_attachments() { "📎" } else { "" },
                    if is_snoozed { "💤" } else { "" }
                )),
                SubjectString(e.subject().to_string()),
            )
        }
    }

    /// Fill the `self.data_columns` `CellBuffers` with the contents of the account folder the user has
    /// chosen.
    fn refresh_mailbox(&mut self, context: &mut Context) {
        self.dirty = true;
        let old_cursor_pos = self.cursor_pos;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;
        let folder_hash = if let Some(h) = context.accounts[self.cursor_pos.0]
            .folders_order
            .get(self.cursor_pos.1)
        {
            *h
        } else {
            self.cursor_pos.1 = old_cursor_pos.1;
            self.dirty = false;
            return;
        };
        context
            .replies
            .push_back(UIEvent::RefreshMailbox((self.cursor_pos.0, folder_hash)));

        // Get mailbox as a reference.
        //
        match context.accounts[self.cursor_pos.0].status(folder_hash) {
            Ok(_) => {}
            Err(_) => {
                self.data_columns.columns[0] =
                    CellBuffer::new("Loading.".len(), 1, Cell::with_char(' '));
                self.length = 0;
                write_string_to_grid(
                    "Loading.",
                    &mut self.data_columns.columns[0],
                    Color::Default,
                    Color::Default,
                    ((0, 0), (MAX_COLS - 1, 0)),
                    false,
                );
                return;
            }
        }
        if old_cursor_pos == self.new_cursor_pos {
            self.view.update(context);
        } else if self.unfocused {
            self.view = ThreadView::new(self.new_cursor_pos, None, context);
        }

        let account = &context.accounts[self.cursor_pos.0];
        let mailbox = account[self.cursor_pos.1].as_ref().unwrap();

        let threads = &account.collection.threads[&mailbox.folder.hash()];
        self.order.clear();
        self.selection.clear();
        self.length = 0;
        let mut rows = Vec::with_capacity(1024);
        let mut min_width = (0, 0, 0, 0, 0);

        threads.sort_by(self.sort, self.subsort, &account.collection);
        for (idx, root_idx) in threads.root_iter().enumerate() {
            self.length += 1;
            let thread_node = &threads.thread_nodes()[&root_idx];
            let i = if let Some(i) = thread_node.message() {
                i
            } else {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[&iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                }
                threads.thread_nodes()[&iter_ptr].message().unwrap()
            };
            if !context.accounts[self.cursor_pos.0].contains_key(i) {
                debug!("key = {}", i);
                debug!(
                    "name = {} {}",
                    mailbox.name(),
                    context.accounts[self.cursor_pos.0].name()
                );
                debug!("{:#?}", context.accounts);

                panic!();
            }
            let root_envelope: &Envelope = &context.accounts[self.cursor_pos.0].get_env(&i);

            let strings = CompactListing::make_entry_string(
                root_envelope,
                thread_node.len(),
                idx,
                threads.is_snoozed(root_idx),
            );
            min_width.0 = cmp::max(min_width.0, strings.0.grapheme_width()); /* index */
            min_width.1 = cmp::max(min_width.1, strings.1.grapheme_width()); /* date */
            min_width.2 = cmp::max(min_width.2, strings.2.grapheme_width()); /* from */
            min_width.3 = cmp::max(min_width.3, strings.3.grapheme_width()); /* flags */
            min_width.4 = cmp::max(min_width.4, strings.4.grapheme_width()); /* subject */
            rows.push(strings);
            self.order.insert(i, idx);
            self.selection.insert(i, false);
        }
        let CompactListing {
            ref mut selection,
            ref order,
            ..
        } = self;
        selection.retain(|e, _| order.contains_key(e));

        /* index column */
        self.data_columns.columns[0] =
            CellBuffer::new(min_width.0, rows.len(), Cell::with_char(' '));
        /* date column */
        self.data_columns.columns[1] =
            CellBuffer::new(min_width.1, rows.len(), Cell::with_char(' '));
        /* from column */
        self.data_columns.columns[2] =
            CellBuffer::new(min_width.2, rows.len(), Cell::with_char(' '));
        /* flags column */
        self.data_columns.columns[3] =
            CellBuffer::new(min_width.3, rows.len(), Cell::with_char(' '));
        /* subject column */
        self.data_columns.columns[4] =
            CellBuffer::new(min_width.4, rows.len(), Cell::with_char(' '));

        for ((idx, root_idx), strings) in threads.root_iter().enumerate().zip(rows) {
            let thread_node = &threads.thread_nodes()[&root_idx];
            let i = if let Some(i) = thread_node.message() {
                i
            } else {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[&iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                }
                threads.thread_nodes()[&iter_ptr].message().unwrap()
            };
            if !context.accounts[self.cursor_pos.0].contains_key(i) {
                //debug!("key = {}", i);
                //debug!(
                //    "name = {} {}",
                //    mailbox.name(),
                //    context.accounts[self.cursor_pos.0].name()
                //);
                //debug!("{:#?}", context.accounts);

                panic!();
            }
            let fg_color = if thread_node.has_unseen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if thread_node.has_unseen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            };
            let (x, _) = write_string_to_grid(
                &strings.0,
                &mut self.data_columns.columns[0],
                fg_color,
                bg_color,
                ((0, idx), (min_width.0, idx)),
                false,
            );
            for x in x..min_width.0 {
                self.data_columns.columns[0][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.1,
                &mut self.data_columns.columns[1],
                fg_color,
                bg_color,
                ((0, idx), (min_width.1, idx)),
                false,
            );
            for x in x..min_width.1 {
                self.data_columns.columns[1][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.2,
                &mut self.data_columns.columns[2],
                fg_color,
                bg_color,
                ((0, idx), (min_width.2, idx)),
                false,
            );
            for x in x..min_width.2 {
                self.data_columns.columns[2][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.3,
                &mut self.data_columns.columns[3],
                fg_color,
                bg_color,
                ((0, idx), (min_width.3, idx)),
                false,
            );
            for x in x..min_width.3 {
                self.data_columns.columns[3][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.4,
                &mut self.data_columns.columns[4],
                fg_color,
                bg_color,
                ((0, idx), (min_width.4, idx)),
                false,
            );
            for x in x..min_width.4 {
                self.data_columns.columns[4][(x, idx)].set_bg(bg_color);
            }
            match (
                threads.is_snoozed(root_idx),
                &context.accounts[self.cursor_pos.0]
                    .get_env(&i)
                    .has_attachments(),
            ) {
                (true, true) => {
                    self.data_columns.columns[3][(0, idx)].set_fg(Color::Red);
                    self.data_columns.columns[3][(1, idx)].set_fg(Color::Byte(103));
                }
                (true, false) => {
                    self.data_columns.columns[3][(0, idx)].set_fg(Color::Red);
                }
                (false, true) => {
                    self.data_columns.columns[3][(0, idx)].set_fg(Color::Byte(103));
                }
                (false, false) => {}
            }
        }
        if self.length == 0 {
            let message = format!("Folder `{}` is empty.", mailbox.folder.name());
            self.data_columns.columns[0] =
                CellBuffer::new(message.len(), self.length + 1, Cell::with_char(' '));
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                Color::Default,
                Color::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                false,
            );
            return;
        }
    }

    fn format_date(envelope: &Envelope) -> String {
        let d = std::time::UNIX_EPOCH + std::time::Duration::from_secs(envelope.date());
        let now: std::time::Duration = std::time::SystemTime::now()
            .duration_since(d)
            .unwrap_or_else(|_| std::time::Duration::new(std::u64::MAX, 0));
        match now.as_secs() {
            n if n < 10 * 60 * 60 => format!("{} hours ago{}", n / (60 * 60), " ".repeat(8)),
            n if n < 24 * 60 * 60 => format!("{} hours ago{}", n / (60 * 60), " ".repeat(7)),
            n if n < 4 * 24 * 60 * 60 => {
                format!("{} days ago{}", n / (24 * 60 * 60), " ".repeat(9))
            }
            _ => envelope.datetime().format("%Y-%m-%d %H:%M:%S").to_string(),
        }
    }
    fn draw_filtered_selection(&mut self, context: &mut Context) {
        if self.filtered_selection.is_empty() {
            return;
        }
        let account = &context.accounts[self.cursor_pos.0];
        let mailbox = account[self.cursor_pos.1].as_ref().unwrap();

        let threads = &account.collection.threads[&mailbox.folder.hash()];
        self.length = 0;
        let mut rows = Vec::with_capacity(1024);
        let mut min_width = (0, 0, 0, 0, 0);

        for (idx, envelope_hash) in self.filtered_selection.iter().enumerate() {
            self.length += 1;
            let envelope: &Envelope = &context.accounts[self.cursor_pos.0].get_env(&envelope_hash);
            let t_idx = envelope.thread();
            let strings = CompactListing::make_entry_string(
                envelope,
                threads[&envelope.thread()].len(),
                idx,
                threads.is_snoozed(t_idx),
            );
            min_width.0 = cmp::max(min_width.0, strings.0.grapheme_width()); /* index */
            min_width.1 = cmp::max(min_width.1, strings.1.grapheme_width()); /* date */
            min_width.2 = cmp::max(min_width.2, strings.2.grapheme_width()); /* from */
            min_width.3 = cmp::max(min_width.3, strings.3.grapheme_width()); /* flags */
            min_width.4 = cmp::max(min_width.4, strings.4.grapheme_width()); /* subject */
            rows.push(strings);
        }

        /* index column */
        self.data_columns.columns[0] =
            CellBuffer::new(min_width.0, rows.len(), Cell::with_char(' '));
        /* date column */
        self.data_columns.columns[1] =
            CellBuffer::new(min_width.1, rows.len(), Cell::with_char(' '));
        /* from column */
        self.data_columns.columns[2] =
            CellBuffer::new(min_width.2, rows.len(), Cell::with_char(' '));
        /* flags column */
        self.data_columns.columns[3] =
            CellBuffer::new(min_width.3, rows.len(), Cell::with_char(' '));
        /* subject column */
        self.data_columns.columns[4] =
            CellBuffer::new(min_width.4, rows.len(), Cell::with_char(' '));

        for ((idx, envelope_hash), strings) in self.filtered_selection.iter().enumerate().zip(rows)
        {
            let envelope: &Envelope = &context.accounts[self.cursor_pos.0].get_env(&envelope_hash);
            let fg_color = if !envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if self.selection[&envelope_hash] {
                Color::Byte(210)
            } else if !envelope.is_seen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            };
            let (x, _) = write_string_to_grid(
                &strings.0,
                &mut self.data_columns.columns[0],
                fg_color,
                bg_color,
                ((0, idx), (min_width.0, idx)),
                false,
            );
            for x in x..min_width.0 {
                self.data_columns.columns[0][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.1,
                &mut self.data_columns.columns[1],
                fg_color,
                bg_color,
                ((0, idx), (min_width.1, idx)),
                false,
            );
            for x in x..min_width.1 {
                self.data_columns.columns[1][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.2,
                &mut self.data_columns.columns[2],
                fg_color,
                bg_color,
                ((0, idx), (min_width.2, idx)),
                false,
            );
            for x in x..min_width.2 {
                self.data_columns.columns[2][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.3,
                &mut self.data_columns.columns[3],
                fg_color,
                bg_color,
                ((0, idx), (min_width.3, idx)),
                false,
            );
            for x in x..min_width.3 {
                self.data_columns.columns[3][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.4,
                &mut self.data_columns.columns[4],
                fg_color,
                bg_color,
                ((0, idx), (min_width.4, idx)),
                false,
            );
            for x in x..min_width.4 {
                self.data_columns.columns[4][(x, idx)].set_bg(bg_color);
            }
            match (
                threads.is_snoozed(envelope.thread()),
                &context.accounts[self.cursor_pos.0]
                    .get_env(&envelope_hash)
                    .has_attachments(),
            ) {
                (true, true) => {
                    self.data_columns.columns[3][(0, idx)].set_fg(Color::Red);
                    self.data_columns.columns[3][(1, idx)].set_fg(Color::Byte(103));
                }
                (true, false) => {
                    self.data_columns.columns[3][(0, idx)].set_fg(Color::Red);
                }
                (false, true) => {
                    self.data_columns.columns[3][(0, idx)].set_fg(Color::Byte(103));
                }
                (false, false) => {}
            }
        }
    }
    fn get_envelope_under_cursor(&self, cursor: usize, context: &Context) -> EnvelopeHash {
        let account = &context.accounts[self.cursor_pos.0];
        let folder_hash = account[self.cursor_pos.1]
            .as_ref()
            .map(|m| m.folder.hash())
            .unwrap();
        let threads = &account.collection.threads[&folder_hash];
        if self.filtered_selection.is_empty() {
            let thread_node = threads.root_set(cursor);
            let thread_node = &threads.thread_nodes()[&thread_node];
            if let Some(i) = thread_node.message() {
                i
            } else {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[&iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                }
                threads.thread_nodes()[&iter_ptr].message().unwrap()
            }
        } else {
            self.filtered_selection[self.cursor_pos.2]
        }
    }
}

impl Component for CompactListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.unfocused {
            if !self.is_dirty() {
                return;
            }
            if !self.filtered_selection.is_empty() {
                self.draw_filtered_selection(context);
                let (upper_left, bottom_right) = area;
                let (x, y) = write_string_to_grid(
                    &format!("Filter (Press ESC to exit): {}", self.filter_term),
                    grid,
                    Color::Default,
                    Color::Default,
                    area,
                    true,
                );
                clear_area(grid, ((x, y), set_y(bottom_right, y)));
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 1)));

                self.draw_list(grid, (set_y(upper_left, y + 1), bottom_right), context);
                self.dirty = false;
                return;
            }
            if !self.row_updates.is_empty() {
                let (upper_left, bottom_right) = area;
                while let Some(row) = self.row_updates.pop() {
                    let row: usize = self.order[&row];
                    let rows = get_y(bottom_right) - get_y(upper_left) + 1;
                    let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

                    let top_idx = page_no * rows;
                    if row >= top_idx && row <= top_idx + rows {
                        self.highlight_line(
                            grid,
                            (
                                set_y(upper_left, get_y(upper_left) + (row % rows)),
                                set_y(bottom_right, get_y(upper_left) + (row % rows)),
                            ),
                            row,
                            context,
                        );
                    }
                }
                if self.force_draw {
                    /* Draw the entire list */
                    self.draw_list(grid, area, context);
                    self.force_draw = false;
                }
            } else {
                /* Draw the entire list */
                self.draw_list(grid, area, context);
            }
        } else {
            if self.length == 0 && self.dirty {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
                return;
            }

            self.view.draw(grid, area, context);
        }
        self.dirty = false;
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.unfocused && self.view.process_event(event, context) {
            return true;
        }

        let shortcuts = &self.get_shortcuts(context)[CompactListing::DESCRIPTION];
        match *event {
            UIEvent::Input(Key::Up) => {
                if self.cursor_pos.2 > 0 {
                    self.new_cursor_pos.2 = self.new_cursor_pos.2.saturating_sub(1);
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Input(Key::Down) => {
                if self.length > 0 && self.new_cursor_pos.2 < self.length - 1 {
                    self.new_cursor_pos.2 += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Input(ref k) if !self.unfocused && *k == shortcuts["open_thread"] => {
                if self.filtered_selection.is_empty() {
                    self.view = ThreadView::new(self.cursor_pos, None, context);
                } else {
                    let mut temp = self.cursor_pos;
                    let account = &mut context.accounts[self.cursor_pos.0];
                    let thread_hash = {
                        account
                            .get_env(&self.filtered_selection[self.cursor_pos.2])
                            .thread()
                            .clone()
                    };
                    let folder_hash = account[self.cursor_pos.1]
                        .as_ref()
                        .map(|m| m.folder.hash())
                        .unwrap();
                    let threads = &account.collection.threads[&folder_hash];
                    let root_thread_index = threads.root_iter().position(|t| t == thread_hash);
                    if let Some(pos) = root_thread_index {
                        temp.2 = pos;
                        self.view = ThreadView::new(temp, Some(thread_hash), context);
                    } else {
                        return true;
                    }
                }
                self.unfocused = true;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["prev_page"] => {
                self.movement = Some(PageMovement::PageUp);
                self.set_dirty();
            }
            UIEvent::Input(ref key) if *key == shortcuts["next_page"] => {
                self.movement = Some(PageMovement::PageDown);
                self.set_dirty();
            }
            UIEvent::Input(ref key) if *key == Key::Home => {
                self.movement = Some(PageMovement::Home);
                self.set_dirty();
            }
            UIEvent::Input(ref key) if *key == Key::End => {
                self.movement = Some(PageMovement::End);
                self.set_dirty();
            }
            UIEvent::Input(ref k) if self.unfocused && *k == shortcuts["exit_thread"] => {
                self.unfocused = false;
                self.dirty = true;
                /* If self.row_updates is not empty and we exit a thread, the row_update events
                 * will be performed but the list will not be drawn. So force a draw in any case.
                 * */
                self.force_draw = true;
                return true;
            }
            UIEvent::Input(ref key) if !self.unfocused && *key == shortcuts["select_entry"] => {
                let env_hash = self.get_envelope_under_cursor(self.cursor_pos.2, context);
                self.selection.entry(env_hash).and_modify(|e| *e = !*e);
            }
            UIEvent::MailboxUpdate((ref idxa, ref idxf))
                if (*idxa, *idxf)
                    == (
                        self.new_cursor_pos.0,
                        context.accounts[self.new_cursor_pos.0].folders_order
                            [self.new_cursor_pos.1],
                    ) =>
            {
                self.refresh_mailbox(context);
                self.set_dirty();
            }
            UIEvent::StartupCheck(ref f)
                if *f
                    == context.accounts[self.cursor_pos.0].folders_order[self.new_cursor_pos.1] =>
            {
                self.refresh_mailbox(context);
                self.set_dirty();
            }
            UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                if let Some(row) = self.order.remove(old_hash) {
                    self.order.insert(*new_hash, row);
                    let selection_status = self.selection.remove(old_hash).unwrap();
                    self.selection.insert(*new_hash, selection_status);
                    self.highlight_line(
                        &mut CellBuffer::default(),
                        ((0, row), (MAX_COLS - 1, row)),
                        row,
                        context,
                    );
                    for h in self.filtered_selection.iter_mut() {
                        if *h == *old_hash {
                            *h = *new_hash;
                            break;
                        }
                    }

                    self.row_updates.push(*new_hash);
                } else {
                    /* Listing has was updated in time before the event */
                }
                self.view
                    .process_event(&mut UIEvent::EnvelopeRename(*old_hash, *new_hash), context);
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Action(ref action) => match action {
                Action::ViewMailbox(idx) => {
                    if context.accounts[self.cursor_pos.0]
                        .folders_order
                        .get(self.cursor_pos.1)
                        .is_none()
                    {
                        return true;
                    }
                    self.filtered_selection.clear();
                    self.new_cursor_pos.1 = *idx;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::SubSort(field, order) => {
                    debug!("SubSort {:?} , {:?}", field, order);
                    self.subsort = (*field, *order);
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::Sort(field, order) => {
                    debug!("Sort {:?} , {:?}", field, order);
                    self.sort = (*field, *order);
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::ToggleThreadSnooze => {
                    let i = self.get_envelope_under_cursor(self.cursor_pos.2, context);
                    let account = &mut context.accounts[self.cursor_pos.0];
                    let thread_hash = account.get_env(&i).thread();
                    let folder_hash = account[self.cursor_pos.1]
                        .as_ref()
                        .map(|m| m.folder.hash())
                        .unwrap();
                    let threads = account.collection.threads.entry(folder_hash).or_default();
                    let thread_group = threads.thread_nodes()[&thread_hash].thread_group();
                    let thread_group = threads.find(thread_group);
                    /*let i = if let Some(i) = threads.thread_nodes[&thread_group].message() {
                        i
                    } else {
                        let mut iter_ptr = threads.thread_nodes[&thread_group].children()[0];
                        while threads.thread_nodes()[&iter_ptr].message().is_none() {
                            iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                        }
                        threads.thread_nodes()[&iter_ptr].message().unwrap()
                    };*/
                    let root_node = threads.thread_nodes.entry(thread_group).or_default();
                    let is_snoozed = root_node.snoozed();
                    root_node.set_snoozed(!is_snoozed);
                    //self.row_updates.push(i);
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::Listing(Filter(ref filter_term)) => {
                    self.filter(filter_term, context);
                    self.dirty = true;
                }
                Action::Listing(a @ SetRead)
                | Action::Listing(a @ SetUnread)
                | Action::Listing(a @ Delete) => {
                    /* Iterate over selection if exists, else only over the envelope under the
                     * cursor. Using two iterators allows chaining them which results into a Chain
                     * type. We can't conditonally select either a slice iterator or a Map iterator
                     * because of the type system */
                    let is_selection_empty =
                        self.selection.values().cloned().any(std::convert::identity);
                    let i = [self.get_envelope_under_cursor(self.cursor_pos.2, context)];
                    let cursor_iter;
                    let sel_iter = if is_selection_empty {
                        cursor_iter = None;
                        Some(self.selection.iter().filter(|(_, v)| **v).map(|(k, _)| k))
                    } else {
                        cursor_iter = Some(i.iter());
                        None
                    };
                    let iter = sel_iter
                        .into_iter()
                        .flatten()
                        .chain(cursor_iter.into_iter().flatten());
                    for &i in iter {
                        let account = &mut context.accounts[self.cursor_pos.0];
                        if !account.contains_key(i) {
                            /* The envelope has been renamed or removed, so wait for the appropriate event to
                             * arrive */
                            continue;
                        }
                        match a {
                            SetRead => {
                                let hash = account.get_env(&i).hash();
                                let op = account.operation(hash);
                                let envelope: &mut Envelope = &mut account.get_env_mut(&i);
                                envelope.set_seen(op).unwrap();
                                self.row_updates.push(i);
                            }
                            SetUnread => {
                                let hash = account.get_env(&i).hash();
                                let op = account.operation(hash);
                                let envelope: &mut Envelope = &mut account.get_env_mut(&i);
                                envelope.set_unseen(op).unwrap();
                                self.row_updates.push(i);
                            }
                            Delete => { /* do nothing */ }
                            _ => unreachable!(),
                        }
                    }
                    self.dirty = true;
                    for v in self.selection.values_mut() {
                        *v = false;
                    }
                    return true;
                }

                _ => {}
            },
            UIEvent::Input(Key::Esc) if !self.unfocused && !self.filtered_selection.is_empty() => {
                self.filter_term.clear();
                self.filtered_selection.clear();
                for v in self.selection.values_mut() {
                    *v = false;
                }
                self.filtered_order.clear();
                self.refresh_mailbox(context);
                return true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
            || if self.unfocused {
                self.view.is_dirty()
            } else {
                false
            }
    }
    fn set_dirty(&mut self) {
        if self.unfocused {
            self.view.set_dirty();
        }
        self.dirty = true;
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.unfocused {
            self.view.get_shortcuts(context)
        } else {
            ShortcutMaps::default()
        };

        let config_map = context.settings.shortcuts.compact_listing.key_values();
        map.insert(
            CompactListing::DESCRIPTION.to_string(),
            [
                (
                    "open_thread",
                    if let Some(key) = config_map.get("open_thread") {
                        (*key).clone()
                    } else {
                        Key::Char('\n')
                    },
                ),
                (
                    "prev_page",
                    if let Some(key) = config_map.get("prev_page") {
                        (*key).clone()
                    } else {
                        Key::PageUp
                    },
                ),
                (
                    "next_page",
                    if let Some(key) = config_map.get("next_page") {
                        (*key).clone()
                    } else {
                        Key::PageDown
                    },
                ),
                (
                    "exit_thread",
                    if let Some(key) = config_map.get("exit_thread") {
                        (*key).clone()
                    } else {
                        Key::Char('i')
                    },
                ),
                (
                    "select_entry",
                    if let Some(key) = config_map.get("select_entry") {
                        (*key).clone()
                    } else {
                        Key::Char('v')
                    },
                ),
            ]
            .iter()
            .cloned()
            .collect(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
