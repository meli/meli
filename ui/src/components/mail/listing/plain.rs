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

use super::EntryStrings;
use super::*;
use crate::components::utilities::PageMovement;
use std::cmp;
use std::iter::FromIterator;

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

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `MailView`.
#[derive(Debug)]
pub struct PlainListing {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    all_envelopes: fnv::FnvHashSet<EnvelopeHash>,
    order: FnvHashMap<EnvelopeHash, usize>,
    /// Cache current view.
    data_columns: DataColumns,

    filter_term: String,
    filtered_selection: Vec<EnvelopeHash>,
    filtered_order: FnvHashMap<EnvelopeHash, usize>,
    selection: FnvHashMap<EnvelopeHash, bool>,
    local_collection: Vec<EnvelopeHash>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: MailView,
    row_updates: StackVec<EnvelopeHash>,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl ListingTrait for PlainListing {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1, None)
    }

    fn set_coordinates(&mut self, coordinates: (usize, usize, Option<EnvelopeHash>)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.unfocused = false;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term.clear();
        self.row_updates.clear();
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        if self.length == 0 {
            return;
        }
        let i = self.get_env_under_cursor(idx, context);

        let account = &context.accounts[self.cursor_pos.0];
        let envelope: EnvelopeRef = account.collection.get_env(i);

        let fg_color = self.data_columns.columns[0][(0, idx)].fg();
        let bg_color = if context.settings.terminal.theme == "light" {
            if self.cursor_pos.2 == idx {
                Color::Byte(244)
            } else if self.selection[&i] {
                Color::Byte(210)
            } else if !envelope.is_seen() {
                Color::Byte(251)
            } else {
                self.data_columns.columns[0][(0, idx)].bg()
            }
        } else {
            if self.cursor_pos.2 == idx {
                Color::Byte(246)
            } else if self.selection[&i] {
                Color::Byte(210)
            } else if !envelope.is_seen() {
                Color::Byte(251)
            } else {
                self.data_columns.columns[0][(0, idx)].bg()
            }
        };

        let (upper_left, bottom_right) = area;
        change_colors(grid, area, fg_color, bg_color);
        let mut x = get_x(upper_left)
            + self.data_columns.widths[0]
            + self.data_columns.widths[1]
            + self.data_columns.widths[2]
            + 3 * 2;

        copy_area(
            grid,
            &self.data_columns.columns[3],
            (set_x(upper_left, x), bottom_right),
            (
                (0, idx),
                pos_dec(self.data_columns.columns[3].size(), (1, 1)),
            ),
        );
        for _ in 0..self.data_columns.widths[3] {
            grid[(x, get_y(upper_left))].set_bg(bg_color);
            x += 1;
        }
        return;
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
                PageMovement::Up(amount) => {
                    self.new_cursor_pos.2 = self.new_cursor_pos.2.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => {
                    self.new_cursor_pos.2 = self.new_cursor_pos.2.saturating_sub(rows * multiplier);
                }
                PageMovement::Down(amount) => {
                    if self.new_cursor_pos.2 + amount + 1 < self.length {
                        self.new_cursor_pos.2 += amount;
                    } else {
                        self.new_cursor_pos.2 = self.length - 1;
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    if self.new_cursor_pos.2 + rows * multiplier + 1 < self.length {
                        self.new_cursor_pos.2 += rows * multiplier;
                    } else if self.new_cursor_pos.2 + rows * multiplier > self.length {
                        self.new_cursor_pos.2 = self.length - 1;
                    } else {
                        self.new_cursor_pos.2 = (self.length / rows) * rows;
                    }
                }
                PageMovement::Right(_) | PageMovement::Left(_) => {}
                PageMovement::Home => {
                    self.new_cursor_pos.2 = 0;
                }
                PageMovement::End => {
                    if self.new_cursor_pos.2 + rows > self.length {
                        self.new_cursor_pos.2 = self.length - 1;
                    } else {
                        self.new_cursor_pos.2 = (self.length / rows) * rows;
                    }
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
                    (flag_x.saturating_sub(1), get_y(upper_left) + r),
                ),
                fg_color,
                bg_color,
            );
            for x in flag_x
                ..std::cmp::min(
                    get_x(bottom_right),
                    flag_x + 2 + self.data_columns.widths[3],
                )
            {
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
        if filter_term.is_empty() {
            return;
        }

        self.order.clear();
        self.selection.clear();
        self.length = 0;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term = filter_term.to_string();
        self.row_updates.clear();
        for v in self.selection.values_mut() {
            *v = false;
        }

        let account = &context.accounts[self.cursor_pos.0];
        let folder_hash = account[self.cursor_pos.1].unwrap().folder.hash();
        match account.search(&self.filter_term, self.sort, folder_hash) {
            Ok(results) => {
                for env_hash in results {
                    if !account.collection.contains_key(&env_hash) {
                        continue;
                    }
                    if self.filtered_order.contains_key(&env_hash) {
                        continue;
                    }
                    if self.all_envelopes.contains(&env_hash) {
                        self.filtered_selection.push(env_hash);
                        self.filtered_order
                            .insert(env_hash, self.filtered_selection.len() - 1);
                    }
                }
                if !self.filtered_selection.is_empty() {
                    self.new_cursor_pos.2 =
                        std::cmp::min(self.filtered_selection.len() - 1, self.cursor_pos.2);
                } else {
                    self.data_columns.columns[0] =
                        CellBuffer::new_with_context(0, 0, Cell::with_char(' '), context);
                }
                self.redraw_list(context);
            }
            Err(e) => {
                self.cursor_pos.2 = 0;
                self.new_cursor_pos.2 = 0;
                let message = format!(
                    "Encountered an error while searching for `{}`: {}.",
                    &self.filter_term, e
                );
                log(
                    format!("Failed to search for term {}: {}", &self.filter_term, e),
                    ERROR,
                );
                self.data_columns.columns[0] =
                    CellBuffer::new_with_context(message.len(), 1, Cell::with_char(' '), context);
                write_string_to_grid(
                    &message,
                    &mut self.data_columns.columns[0],
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((0, 0), (message.len() - 1, 0)),
                    None,
                );
            }
        }
    }

    fn set_movement(&mut self, mvm: PageMovement) {
        self.movement = Some(mvm);
        self.set_dirty();
    }
}

impl fmt::Display for PlainListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl Default for PlainListing {
    fn default() -> Self {
        PlainListing::new()
    }
}

impl PlainListing {
    const DESCRIPTION: &'static str = "plain listing";
    fn new() -> Self {
        PlainListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            all_envelopes: fnv::FnvHashSet::default(),
            local_collection: Vec::new(),
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
            view: MailView::default(),

            movement: None,
            id: ComponentId::new_v4(),
        }
    }
    fn make_entry_string(e: EnvelopeRef) -> EntryStrings {
        EntryStrings {
            date: DateString(PlainListing::format_date(&e)),
            subject: SubjectString(format!("{}", e.subject())),
            flag: FlagString(format!("{}", if e.has_attachments() { "📎" } else { "" },)),
            from: FromString(address_list!((e.from()) as comma_sep_list)),
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

        // Get mailbox as a reference.
        //
        match context.accounts[self.cursor_pos.0].status(folder_hash) {
            Ok(()) => {}
            Err(_) => {
                let message: String = context.accounts[self.cursor_pos.0][folder_hash].to_string();
                self.data_columns.columns[0] =
                    CellBuffer::new_with_context(message.len(), 1, Cell::with_char(' '), context);
                self.length = 0;
                write_string_to_grid(
                    message.as_str(),
                    &mut self.data_columns.columns[0],
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((0, 0), (MAX_COLS - 1, 0)),
                    None,
                );
                return;
            }
        }
        self.local_collection = context.accounts[self.cursor_pos.0][folder_hash]
            .unwrap()
            .envelopes
            .iter()
            .cloned()
            .collect();
        self.redraw_list(context);

        if self.length > 0 {
            let env_hash = self.get_env_under_cursor(self.cursor_pos.2, context);
            let temp = (self.new_cursor_pos.0, self.new_cursor_pos.1, env_hash);
            if old_cursor_pos == self.new_cursor_pos {
                self.view.update(temp);
            } else if self.unfocused {
                self.view = MailView::new(temp, None, None);
            }
        }
    }

    fn redraw_list(&mut self, context: &Context) {
        let account = &context.accounts[self.cursor_pos.0];
        let mailbox = &account[self.cursor_pos.1].unwrap();

        self.order.clear();
        self.selection.clear();
        self.length = 0;
        let mut rows = Vec::with_capacity(1024);
        let mut min_width = (0, 0, 0, 0, 0);

        let envelopes = account.collection.envelopes.read().unwrap();
        let sort = self.sort;
        self.local_collection.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let ma = &envelopes[a];
                let mb = &envelopes[b];
                mb.date().cmp(&ma.date())
            }
            (SortField::Date, SortOrder::Asc) => {
                let ma = &envelopes[a];
                let mb = &envelopes[b];
                ma.date().cmp(&mb.date())
            }
            (SortField::Subject, SortOrder::Desc) => {
                let ma = &envelopes[a];
                let mb = &envelopes[b];
                ma.subject().cmp(&mb.subject())
            }
            (SortField::Subject, SortOrder::Asc) => {
                let ma = &envelopes[a];
                let mb = &envelopes[b];
                mb.subject().cmp(&ma.subject())
            }
        });

        let mut refresh_mailbox = false;
        let iter = if self.filter_term.is_empty() {
            refresh_mailbox = true;
            Box::new(self.local_collection.iter().cloned())
                as Box<dyn Iterator<Item = EnvelopeHash>>
        } else {
            Box::new(self.filtered_selection.iter().map(|h| *h))
                as Box<dyn Iterator<Item = EnvelopeHash>>
        };
        for (idx, i) in iter.enumerate() {
            self.length += 1;
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
            let envelope: EnvelopeRef = context.accounts[self.cursor_pos.0].collection.get_env(i);

            let entry_strings = PlainListing::make_entry_string(envelope);
            min_width.1 = cmp::max(min_width.1, entry_strings.date.grapheme_width()); /* date */
            min_width.2 = cmp::max(min_width.2, entry_strings.from.grapheme_width()); /* from */
            min_width.3 = cmp::max(min_width.3, entry_strings.flag.grapheme_width()); /* flags */
            min_width.4 = cmp::max(min_width.4, entry_strings.subject.grapheme_width()); /* subject */
            rows.push(entry_strings);
            if refresh_mailbox {
                self.all_envelopes.insert(i);
            }

            self.order.insert(i, idx);
            self.selection.insert(i, false);
        }

        min_width.0 = self.length.saturating_sub(1).to_string().len();

        /* index column */
        self.data_columns.columns[0] =
            CellBuffer::new_with_context(min_width.0, rows.len(), Cell::with_char(' '), context);
        /* date column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(min_width.1, rows.len(), Cell::with_char(' '), context);
        /* from column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(min_width.2, rows.len(), Cell::with_char(' '), context);
        /* flags column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context(min_width.3, rows.len(), Cell::with_char(' '), context);
        /* subject column */
        self.data_columns.columns[4] =
            CellBuffer::new_with_context(min_width.4, rows.len(), Cell::with_char(' '), context);

        let iter = if self.filter_term.is_empty() {
            Box::new(self.local_collection.iter().cloned())
                as Box<dyn Iterator<Item = EnvelopeHash>>
        } else {
            Box::new(self.filtered_selection.iter().map(|h| *h))
                as Box<dyn Iterator<Item = EnvelopeHash>>
        };

        for ((idx, i), strings) in iter.enumerate().zip(rows) {
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

            let envelope: EnvelopeRef = context.accounts[self.cursor_pos.0].collection.get_env(i);
            let fg_color = if !envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if context.settings.terminal.theme == "light" {
                if !envelope.is_seen() {
                    Color::Byte(251)
                } else if idx % 2 == 0 {
                    Color::Byte(252)
                } else {
                    Color::Default
                }
            } else {
                if !envelope.is_seen() {
                    Color::Byte(253)
                } else if idx % 2 == 0 {
                    Color::Byte(236)
                } else {
                    Color::Default
                }
            };
            let (x, _) = write_string_to_grid(
                &idx.to_string(),
                &mut self.data_columns.columns[0],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.0, idx)),
                None,
            );
            for x in x..min_width.0 {
                self.data_columns.columns[0][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.date,
                &mut self.data_columns.columns[1],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.1, idx)),
                None,
            );
            for x in x..min_width.1 {
                self.data_columns.columns[1][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.from,
                &mut self.data_columns.columns[2],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.2, idx)),
                None,
            );
            for x in x..min_width.2 {
                self.data_columns.columns[2][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.flag,
                &mut self.data_columns.columns[3],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.3, idx)),
                None,
            );
            for x in x..min_width.3 {
                self.data_columns.columns[3][(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.subject,
                &mut self.data_columns.columns[4],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.4, idx)),
                None,
            );
            for x in x..min_width.4 {
                self.data_columns.columns[4][(x, idx)].set_bg(bg_color);
            }
            if context.accounts[self.cursor_pos.0]
                .collection
                .get_env(i)
                .has_attachments()
            {
                self.data_columns.columns[3][(0, idx)].set_fg(Color::Byte(103));
            }
        }
        if self.length == 0 && self.filter_term.is_empty() {
            let mailbox = &account[self.cursor_pos.1];
            let message = mailbox.to_string();
            self.data_columns.columns[0] = CellBuffer::new_with_context(
                message.len(),
                self.length + 1,
                Cell::with_char(' '),
                context,
            );
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                None,
            );
        }
    }

    fn get_env_under_cursor(&self, cursor: usize, _context: &Context) -> EnvelopeHash {
        if self.filter_term.is_empty() {
            self.local_collection[cursor]
        } else {
            self.filtered_selection[cursor]
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

    fn perform_action(&mut self, context: &mut Context, env_hash: EnvelopeHash, a: &ListingAction) {
        let account = &mut context.accounts[self.cursor_pos.0];
        let hash = account.collection.get_env(env_hash).hash();
        let op = account.operation(hash);
        let mut envelope: EnvelopeRefMut = account.collection.get_env_mut(env_hash);
        match a {
            ListingAction::SetSeen => {
                if let Err(e) = envelope.set_seen(op) {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                            e.to_string(),
                        )));
                }
            }
            ListingAction::SetUnseen => {
                if let Err(e) = envelope.set_unseen(op) {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                            e.to_string(),
                        )));
                }
            }
            ListingAction::Delete => { /* do nothing */ }
            _ => unreachable!(),
        }
        self.row_updates.push(env_hash);
    }
}

impl Component for PlainListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.unfocused {
            if !self.is_dirty() {
                return;
            }
            let mut area = area;
            if !self.filter_term.is_empty() {
                let (upper_left, bottom_right) = area;
                let (x, y) = write_string_to_grid(
                    &format!(
                        "{} results for `{}` (Press ESC to exit)",
                        self.filtered_selection.len(),
                        self.filter_term
                    ),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    area,
                    Some(get_x(upper_left)),
                );
                clear_area(grid, ((x, y), set_y(bottom_right, y)));
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 1)));

                area = (set_y(upper_left, y + 1), bottom_right);
            }

            if !self.row_updates.is_empty() {
                let (upper_left, bottom_right) = area;
                while let Some(row) = self.row_updates.pop() {
                    let row: usize = self.order[&row];
                    let rows = get_y(bottom_right) - get_y(upper_left) + 1;
                    let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

                    let top_idx = page_no * rows;
                    if row >= top_idx && row <= top_idx + rows {
                        let area = (
                            set_y(upper_left, get_y(upper_left) + (row % rows)),
                            set_y(bottom_right, get_y(upper_left) + (row % rows)),
                        );
                        self.highlight_line(grid, area, row, context);
                        context.dirty_areas.push_back(area);
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

        let shortcuts = self.get_shortcuts(context);
        if self.length > 0 {
            match *event {
                UIEvent::Input(ref k)
                    if !self.unfocused
                        && shortcut!(k == shortcuts[PlainListing::DESCRIPTION]["open_thread"]) =>
                {
                    let env_hash = self.get_env_under_cursor(self.cursor_pos.2, context);
                    let temp = (self.cursor_pos.0, self.cursor_pos.1, env_hash);
                    self.view = MailView::new(temp, None, None);
                    self.unfocused = true;
                    self.dirty = true;
                    return true;
                }
                UIEvent::Input(ref k)
                    if self.unfocused
                        && shortcut!(k == shortcuts[PlainListing::DESCRIPTION]["exit_thread"]) =>
                {
                    self.unfocused = false;
                    self.dirty = true;
                    /* If self.row_updates is not empty and we exit a thread, the row_update events
                     * will be performed but the list will not be drawn. So force a draw in any case.
                     * */
                    self.force_draw = true;
                    return true;
                }
                UIEvent::Input(ref key)
                    if !self.unfocused
                        && shortcut!(
                            key == shortcuts[PlainListing::DESCRIPTION]["select_entry"]
                        ) =>
                {
                    let env_hash = self.get_env_under_cursor(self.cursor_pos.2, context);
                    self.selection.entry(env_hash).and_modify(|e| *e = !*e);
                }
                UIEvent::Action(ref action) => match action {
                    Action::SubSort(field, order) if !self.unfocused => {
                        debug!("SubSort {:?} , {:?}", field, order);
                        self.subsort = (*field, *order);
                        //if !self.filtered_selection.is_empty() {
                        //    let threads = &account.collection.threads[&folder_hash];
                        //    threads.vec_inner_sort_by(&mut self.filtered_selection, self.sort, &account.collection);
                        //} else {
                        //    self.refresh_mailbox(context);
                        //}
                        return true;
                    }
                    Action::Sort(field, order) if !self.unfocused => {
                        debug!("Sort {:?} , {:?}", field, order);
                        self.sort = (*field, *order);
                        return true;
                    }
                    Action::Listing(a @ ListingAction::SetSeen)
                    | Action::Listing(a @ ListingAction::SetUnseen)
                    | Action::Listing(a @ ListingAction::Delete)
                        if !self.unfocused =>
                    {
                        let is_selection_empty =
                            self.selection.values().cloned().any(std::convert::identity);
                        let i = [self.get_env_under_cursor(self.cursor_pos.2, context)];
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
                            .chain(cursor_iter.into_iter().flatten())
                            .cloned();
                        let stack = StackVec::from_iter(iter.into_iter());
                        for i in stack {
                            self.perform_action(context, i, a);
                        }
                        self.dirty = true;
                        for v in self.selection.values_mut() {
                            *v = false;
                        }
                        return true;
                    }

                    _ => {}
                },
                _ => {}
            }
        }
        match *event {
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
                let account = &context.accounts[self.cursor_pos.0];
                let folder_hash = account[self.cursor_pos.1].unwrap().folder.hash();
                if !account.collection.contains_key(new_hash)
                    || !account[folder_hash].unwrap().envelopes.contains(new_hash)
                {
                    return false;
                }

                self.row_updates.push(*new_hash);
                if let Some(row) = self.order.remove(old_hash) {
                    self.order.insert(*new_hash, row);
                    let selection_status = self.selection.remove(old_hash).unwrap();
                    self.selection.insert(*new_hash, selection_status);
                    for h in self.filtered_selection.iter_mut() {
                        if *h == *old_hash {
                            *h = *new_hash;
                            break;
                        }
                    }
                }

                self.dirty = true;

                self.view
                    .process_event(&mut UIEvent::EnvelopeRename(*old_hash, *new_hash), context);
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Input(Key::Esc) if !self.unfocused && !self.filter_term.is_empty() => {
                self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1, None));
                self.set_dirty();
                self.refresh_mailbox(context);
                return true;
            }
            UIEvent::Action(ref action) => match action {
                Action::ViewMailbox(idx) => {
                    if context.accounts[self.cursor_pos.0]
                        .folders_order
                        .get(*idx)
                        .is_none()
                    {
                        return true;
                    }
                    self.filtered_selection.clear();
                    self.new_cursor_pos.1 = *idx;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::Listing(Filter(ref filter_term)) if !self.unfocused => {
                    self.filter(filter_term, context);
                    self.dirty = true;
                }
                _ => {}
            },
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
        map.insert(PlainListing::DESCRIPTION, config_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
