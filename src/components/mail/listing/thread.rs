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

use super::*;
use crate::components::utilities::PageMovement;
use std::cmp;
use std::convert::TryInto;

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `MailView`.
#[derive(Debug)]
pub struct ThreadListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (usize, MailboxHash, usize),
    new_cursor_pos: (usize, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    /// Cache current view.
    color_cache: ColorCache,

    data_columns: DataColumns,
    rows_drawn: SegmentTree,
    rows: Vec<((usize, bool, bool, EnvelopeHash), EntryStrings)>,
    row_updates: SmallVec<[ThreadHash; 8]>,
    selection: HashMap<ThreadHash, bool>,
    order: HashMap<EnvelopeHash, usize>,
    /// If we must redraw on next redraw event
    dirty: bool,
    /// If `self.view` is focused or not.
    unfocused: bool,
    initialised: bool,
    view: Option<MailView>,
    movement: Option<PageMovement>,
    id: ComponentId,
}

impl MailListingTrait for ThreadListing {
    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]> {
        &mut self.row_updates
    }

    fn selection(&mut self) -> &mut HashMap<ThreadHash, bool> {
        &mut self.selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]> {
        SmallVec::new()
    }

    /// Fill the `self.content` `CellBuffer` with the contents of the account mailbox the user has
    /// chosen.
    fn refresh_mailbox(&mut self, context: &mut Context, _force: bool) {
        self.dirty = true;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;

        self.color_cache = ColorCache {
            unseen: crate::conf::value(context, "mail.listing.plain.even_unseen"),
            highlighted: crate::conf::value(context, "mail.listing.plain.even_highlighted"),
            even: crate::conf::value(context, "mail.listing.plain.even"),
            odd: crate::conf::value(context, "mail.listing.plain.odd"),
            even_unseen: crate::conf::value(context, "mail.listing.plain.even_unseen"),
            odd_unseen: crate::conf::value(context, "mail.listing.plain.odd_unseen"),
            selected: crate::conf::value(context, "mail.listing.plain.even_selected"),
            attachment_flag: crate::conf::value(context, "mail.listing.attachment_flag"),
            thread_snooze_flag: crate::conf::value(context, "mail.listing.thread_snooze_flag"),
            ..self.color_cache
        };
        if !context.settings.terminal.use_color() {
            self.color_cache.highlighted.attrs |= Attr::REVERSE;
        }

        // Get mailbox as a reference.
        //
        match context.accounts[self.cursor_pos.0].load(self.cursor_pos.1) {
            Ok(_) => {}
            Err(_) => {
                let default_cell = {
                    let mut ret = Cell::with_char(' ');
                    ret.set_fg(self.color_cache.theme_default.fg)
                        .set_bg(self.color_cache.theme_default.bg)
                        .set_attrs(self.color_cache.theme_default.attrs);
                    ret
                };
                let message: String =
                    context.accounts[self.cursor_pos.0][&self.cursor_pos.1].status();
                self.data_columns.columns[0] =
                    CellBuffer::new_with_context(message.len(), 1, default_cell, context);
                self.length = 0;
                write_string_to_grid(
                    message.as_str(),
                    &mut self.data_columns.columns[0],
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    ((0, 0), (message.len().saturating_sub(1), 0)),
                    None,
                );
                return;
            }
        }
        let threads = context.accounts[self.cursor_pos.0]
            .collection
            .get_threads(self.cursor_pos.1);
        let mut roots = threads.roots();
        threads.group_inner_sort_by(
            &mut roots,
            self.sort,
            &context.accounts[self.cursor_pos.0].collection.envelopes,
        );

        self.redraw_threads_list(
            context,
            Box::new(roots.into_iter()) as Box<dyn Iterator<Item = ThreadHash>>,
        );
    }

    fn redraw_threads_list(
        &mut self,
        context: &Context,
        items: Box<dyn Iterator<Item = ThreadHash>>,
    ) {
        let account = &context.accounts[self.cursor_pos.0];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        self.length = 0;
        self.order.clear();
        let default_cell = {
            let mut ret = Cell::with_char(' ');
            ret.set_fg(self.color_cache.theme_default.fg)
                .set_bg(self.color_cache.theme_default.bg)
                .set_attrs(self.color_cache.theme_default.attrs);
            ret
        };
        if threads.len() == 0 {
            let message = format!("Mailbox `{}` is empty.", account[&self.cursor_pos.1].name());
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), 1, default_cell, context);
            write_string_to_grid(
                message.as_str(),
                &mut self.data_columns.columns[0],
                self.color_cache.theme_default.fg,
                self.color_cache.theme_default.bg,
                self.color_cache.theme_default.attrs,
                ((0, 0), (message.len() - 1, 0)),
                None,
            );
            return;
        }
        let mut rows = Vec::with_capacity(1024);
        let mut min_width = (0, 0, 0, 0, 0);
        let mut row_widths: (
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
        ) = (
            SmallVec::new(),
            SmallVec::new(),
            SmallVec::new(),
            SmallVec::new(),
            SmallVec::new(),
        );

        let mut indentations: Vec<bool> = Vec::with_capacity(6);
        let roots = items
            .filter_map(|r| threads.groups[&r].root().map(|r| r.root))
            .collect::<_>();
        let mut iter = threads.threads_group_iter(roots).peekable();
        let thread_nodes: &HashMap<ThreadNodeHash, ThreadNode> = &threads.thread_nodes();
        /* This is just a desugared for loop so that we can use .peek() */
        let mut idx = 0;
        let mut prev_group = ThreadHash::null();
        while let Some((indentation, thread_node_hash, has_sibling)) = iter.next() {
            let thread_node = &thread_nodes[&thread_node_hash];

            if thread_node.has_message() {
                let envelope: EnvelopeRef =
                    account.collection.get_env(thread_node.message().unwrap());
                self.order.insert(envelope.hash(), idx);
                use melib::search::QueryTrait;
                if let Some(filter_query) = mailbox_settings!(
                    context[self.cursor_pos.0][&self.cursor_pos.1]
                        .listing
                        .filter
                )
                .as_ref()
                {
                    if !envelope.is_match(filter_query) {
                        continue;
                    }
                }
                let is_root = threads.find_group(thread_node.group) != prev_group;
                prev_group = threads.find_group(thread_node.group);

                let mut entry_strings = self.make_entry_string(&envelope, context);
                entry_strings.subject = SubjectString(ThreadListing::make_thread_entry(
                    &envelope,
                    indentation,
                    thread_node_hash,
                    &threads,
                    &indentations,
                    has_sibling,
                    is_root,
                ));
                row_widths.1.push(
                    entry_strings
                        .date
                        .grapheme_width()
                        .try_into()
                        .unwrap_or(255),
                ); /* date */
                row_widths.2.push(
                    entry_strings
                        .from
                        .grapheme_width()
                        .try_into()
                        .unwrap_or(255),
                ); /* from */
                row_widths.3.push(
                    entry_strings
                        .flag
                        .grapheme_width()
                        .try_into()
                        .unwrap_or(255),
                ); /* flags */
                row_widths.4.push(
                    (entry_strings.subject.grapheme_width()
                        + 1
                        + entry_strings.tags.grapheme_width())
                    .try_into()
                    .unwrap_or(255),
                );
                min_width.1 = cmp::max(min_width.1, entry_strings.date.grapheme_width()); /* date */
                min_width.2 = cmp::max(min_width.2, entry_strings.from.grapheme_width()); /* from */
                min_width.3 = cmp::max(min_width.3, entry_strings.flag.grapheme_width()); /* flags */
                min_width.4 = cmp::max(
                    min_width.4,
                    entry_strings.subject.grapheme_width()
                        + 1
                        + entry_strings.tags.grapheme_width(),
                ); /* tags + subject */
                rows.push((
                    (
                        idx,
                        envelope.is_seen(),
                        envelope.has_attachments(),
                        envelope.hash(),
                    ),
                    entry_strings,
                ));
                idx += 1;
            } else {
                continue;
            }

            match iter.peek() {
                Some((x, _, _)) if *x > indentation => {
                    if has_sibling {
                        indentations.push(true);
                    } else {
                        indentations.push(false);
                    }
                }
                Some((x, _, _)) if *x < indentation => {
                    for _ in 0..(indentation - *x) {
                        indentations.pop();
                    }
                }
                _ => {}
            }
        }
        min_width.0 = idx.saturating_sub(1).to_string().len();
        /* index column */
        self.data_columns.columns[0] =
            CellBuffer::new_with_context(min_width.0, rows.len(), default_cell, context);

        /* date column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(min_width.1, rows.len(), default_cell, context);
        /* from column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(min_width.2, rows.len(), default_cell, context);
        self.data_columns.segment_tree[2] = row_widths.2.into();
        /* flags column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context(min_width.3, rows.len(), default_cell, context);
        /* subject column */
        self.data_columns.columns[4] =
            CellBuffer::new_with_context(min_width.4, rows.len(), default_cell, context);
        self.data_columns.segment_tree[4] = row_widths.4.into();

        self.rows = rows;
        self.rows_drawn = SegmentTree::from(
            std::iter::repeat(1)
                .take(self.rows.len())
                .collect::<SmallVec<_>>(),
        );
        debug_assert!(self.rows_drawn.array.len() == self.rows.len());
        self.draw_rows(
            context,
            0,
            std::cmp::min(80, self.rows.len().saturating_sub(1)),
        );
        self.length = self.order.len();
    }
}

impl ListingTrait for ThreadListing {
    fn coordinates(&self) -> (usize, MailboxHash) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1)
    }
    fn set_coordinates(&mut self, coordinates: (usize, MailboxHash)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.unfocused = false;
        self.view = None;
        self.order.clear();
        self.row_updates.clear();
        self.initialised = false;
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0
        {
            self.refresh_mailbox(context, false);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area, self.color_cache.theme_default);
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;
        if rows == 0 {
            return;
        }
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
                    self.new_cursor_pos.2 = self.length - 1;
                }
            }
        }

        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let top_idx = page_no * rows;
        self.draw_rows(
            context,
            top_idx,
            cmp::min(self.length.saturating_sub(1), top_idx + rows - 1),
        );

        /*
        if !self.initialised {
            self.initialised = false;
            copy_area(
                grid,
                &self.content,
                area,
                ((0, top_idx), (MAX_COLS - 1, self.length)),
            );
            self.highlight_line(
                grid,
                (
                    set_y(upper_left, get_y(upper_left) + (self.cursor_pos.2 % rows)),
                    set_y(bottom_right, get_y(upper_left) + (self.cursor_pos.2 % rows)),
                ),
                self.cursor_pos.2,
                context,
            );
            context.dirty_areas.push_back(area);
        }
        */
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

        /* Page_no has changed, so draw new page */
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
                .saturating_sub(4);
            self.data_columns.widths[2] = remainder / 6;
            self.data_columns.widths[4] =
                ((2 * remainder) / 3).saturating_sub(self.data_columns.widths[3]);
        } else {
            let remainder = width
                .saturating_sub(self.data_columns.widths[0])
                .saturating_sub(self.data_columns.widths[1])
                .saturating_sub(8);
            if min_col_width + self.data_columns.widths[4] > remainder {
                self.data_columns.widths[4] =
                    remainder.saturating_sub(min_col_width + self.data_columns.widths[3]);
                self.data_columns.widths[2] = min_col_width;
            }
        }
        for &i in &[2, 4] {
            /* Set From and Subject column widths to their maximum value width in the range
             * [top_idx, top_idx + rows]. By using a segment tree the query is O(logn), which is
             * great!
             */
            self.data_columns.widths[i] =
                self.data_columns.segment_tree[i].get_max(top_idx, top_idx + rows) as usize;
        }
        if self.data_columns.widths.iter().sum::<usize>() > width {
            let diff = self.data_columns.widths.iter().sum::<usize>() - width;
            if self.data_columns.widths[2] > 2 * diff {
                self.data_columns.widths[2] -= diff;
            } else {
                self.data_columns.widths[2] = std::cmp::max(
                    15,
                    self.data_columns.widths[2].saturating_sub((2 * diff) / 3),
                );
                self.data_columns.widths[4] = std::cmp::max(
                    15,
                    self.data_columns.widths[4].saturating_sub(diff / 3 + diff % 3),
                );
            }
        }
        clear_area(grid, area, self.color_cache.theme_default);
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
                /*
                let thread_hash = self.get_thread_under_cursor(r + top_idx);

                if self.selection[&thread_hash] {
                    (c.fg(), self.color_cache.selected.bg)
                } else {
                }
                */
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
                self.color_cache.theme_default,
            );
        }
        context.dirty_areas.push_back(area);
        /*
        copy_area(
            grid,
            &self.content,
            area,
            ((0, top_idx), (MAX_COLS - 1, self.length)),
        );
        self.highlight_line(
            grid,
            (
                set_y(upper_left, get_y(upper_left) + (self.cursor_pos.2 % rows)),
                set_y(bottom_right, get_y(upper_left) + (self.cursor_pos.2 % rows)),
            ),
            self.cursor_pos.2,
            context,
        );
        context.dirty_areas.push_back(area);
        */
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        if self.length == 0 {
            return;
        }

        let env_hash = self.get_env_under_cursor(idx, context);
        let envelope: EnvelopeRef = context.accounts[self.cursor_pos.0]
            .collection
            .get_env(env_hash);

        let fg_color = if !envelope.is_seen() {
            Color::Byte(0)
        } else {
            Color::Default
        };
        let bg_color = if self.cursor_pos.2 == idx {
            Color::Byte(246)
        } else if !envelope.is_seen() {
            Color::Byte(251)
        } else if idx % 2 == 0 {
            Color::Byte(236)
        } else {
            Color::Default
        };
        change_colors(grid, area, fg_color, bg_color);
    }

    fn set_movement(&mut self, mvm: PageMovement) {
        self.movement = Some(mvm);
        self.set_dirty(true);
    }
}

impl fmt::Display for ThreadListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl ThreadListing {
    pub fn new(coordinates: (usize, MailboxHash)) -> Self {
        ThreadListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (Default::default(), Default::default()),
            color_cache: ColorCache::default(),
            data_columns: DataColumns::default(),
            rows_drawn: SegmentTree::default(),
            rows: vec![],
            row_updates: SmallVec::new(),
            selection: HashMap::default(),
            order: HashMap::default(),
            dirty: true,
            unfocused: false,
            view: None,
            initialised: false,
            movement: None,
            id: ComponentId::new_v4(),
        }
    }

    fn highlight_line_self(&mut self, _idx: usize, _context: &Context) {
        /*
         * FIXME
        if self.length == 0 {
            return;
        }

        let env_hash = self.get_env_under_cursor(idx, context);
        let envelope: EnvelopeRef = context.accounts[self.cursor_pos.0]
            .collection
            .get_env(env_hash);

        */
    }

    fn make_thread_entry(
        envelope: &Envelope,
        indent: usize,
        node_idx: ThreadNodeHash,
        threads: &Threads,
        indentations: &[bool],
        has_sibling: bool,
        is_root: bool,
    ) -> String {
        let thread_node = &threads[&node_idx];
        let has_parent = thread_node.has_parent() && !is_root;
        let show_subject = thread_node.show_subject();

        let mut s = String::new(); //format!("{}{}{} ", idx, " ", ThreadListing::format_date(&envelope));
        for i in 0..indent {
            if indentations.len() > i && indentations[i] {
                s.push('│');
            } else if indentations.len() > i {
                s.push(' ');
            }
            if i > 0 {
                s.push(' ');
            }
        }
        if indent > 0 && (has_sibling || has_parent) {
            if has_sibling && has_parent {
                s.push('├');
            } else if has_sibling {
                s.push('┬');
            } else {
                s.push('└');
            }
            s.push('─');
            s.push('>');
        }

        /*
        s.push_str(if envelope.has_attachments() {
            "📎"
        } else {
            ""
        });
        */
        if show_subject {
            s.push_str(&format!("{:.85}", envelope.subject()));
        }
        s
    }

    fn get_env_under_cursor(&self, cursor: usize, _context: &Context) -> EnvelopeHash {
        *self
            .order
            .iter()
            .find(|(_, &r)| r == cursor)
            .unwrap_or_else(|| {
                debug!("self.order empty ? cursor={} {:#?}", cursor, &self.order);
                panic!();
            })
            .0
    }

    fn make_entry_string(&self, e: &Envelope, context: &Context) -> EntryStrings {
        let mut tags = String::new();
        let mut colors: SmallVec<[_; 8]> = SmallVec::new();
        let backend_lck = context.accounts[self.cursor_pos.0].backend.read().unwrap();
        if let Some(t) = backend_lck.tags() {
            let tags_lck = t.read().unwrap();
            for t in e.labels().iter() {
                if mailbox_settings!(
                    context[self.cursor_pos.0][&self.cursor_pos.1]
                        .tags
                        .ignore_tags
                )
                .contains(t)
                    || !tags_lck.contains_key(t)
                {
                    continue;
                }
                tags.push(' ');
                tags.push_str(tags_lck.get(t).as_ref().unwrap());
                tags.push(' ');
                colors.push(
                    mailbox_settings!(context[self.cursor_pos.0][&self.cursor_pos.1].tags.colors)
                        .get(t)
                        .map(|&c| c),
                );
            }
            if !tags.is_empty() {
                tags.pop();
            }
        }
        let mut subject = e.subject().to_string();
        subject.truncate_at_boundary(150);
        EntryStrings {
            date: DateString(ConversationsListing::format_date(context, e.date())),
            subject: SubjectString(subject),
            flag: FlagString(format!("{}", if e.has_attachments() { "📎" } else { "" },)),
            from: FromString(address_list!((e.from()) as comma_sep_list)),
            tags: TagString(tags, colors),
        }
    }

    fn draw_rows(&mut self, context: &Context, start: usize, end: usize) {
        if self.length == 0 {
            return;
        }
        debug_assert!(end >= start);
        if self.rows_drawn.get_max(start, end) == 0 {
            //debug!("not drawing {}-{}", start, end);
            return;
        }
        //debug!("drawing {}-{}", start, end);
        for i in start..=end {
            self.rows_drawn.update(i, 0);
        }
        let min_width = (
            self.data_columns.columns[0].size().0,
            self.data_columns.columns[1].size().0,
            self.data_columns.columns[2].size().0,
            self.data_columns.columns[3].size().0,
            self.data_columns.columns[4].size().0,
        );

        for ((idx, is_seen, has_attachments, env_hash), strings) in
            self.rows.iter().skip(start).take(end - start + 1)
        {
            let idx = *idx;
            if !context.accounts[self.cursor_pos.0].contains_key(*env_hash) {
                //debug!("key = {}", root_env_hash);
                //debug!(
                //    "name = {} {}",
                //    account[&self.cursor_pos.1].name(),
                //    context.accounts[self.cursor_pos.0].name()
                //);
                //debug!("{:#?}", context.accounts);

                panic!();
            }
            let row_attr = if *is_seen {
                if idx % 2 == 0 {
                    self.color_cache.even
                } else {
                    self.color_cache.odd
                }
            } else if idx % 2 == 0 {
                self.color_cache.even_unseen
            } else {
                self.color_cache.odd_unseen
            };
            let (x, _) = write_string_to_grid(
                &idx.to_string(),
                &mut self.data_columns.columns[0],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.0, idx)),
                None,
            );
            for x in x..min_width.0 {
                self.data_columns.columns[0][(x, idx)]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            let (x, _) = write_string_to_grid(
                &strings.date,
                &mut self.data_columns.columns[1],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.1, idx)),
                None,
            );
            for x in x..min_width.1 {
                self.data_columns.columns[1][(x, idx)]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            let (x, _) = write_string_to_grid(
                &strings.from,
                &mut self.data_columns.columns[2],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.2, idx)),
                None,
            );
            #[cfg(feature = "regexp")]
            {
                for text_formatter in crate::conf::text_format_regexps(context, "listing.from") {
                    let t = self.data_columns.columns[2].insert_tag(text_formatter.tag);
                    for (start, end) in text_formatter.regexp.find_iter(strings.from.as_str()) {
                        self.data_columns.columns[2].set_tag(t, (start, idx), (end, idx));
                    }
                }
            }
            for x in x..min_width.2 {
                self.data_columns.columns[2][(x, idx)]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            let (x, _) = write_string_to_grid(
                &strings.flag,
                &mut self.data_columns.columns[3],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.3, idx)),
                None,
            );
            for x in x..min_width.3 {
                self.data_columns.columns[3][(x, idx)]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            let (x, _) = write_string_to_grid(
                &strings.subject,
                &mut self.data_columns.columns[4],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.4, idx)),
                None,
            );
            #[cfg(feature = "regexp")]
            {
                for text_formatter in crate::conf::text_format_regexps(context, "listing.subject") {
                    let t = self.data_columns.columns[4].insert_tag(text_formatter.tag);
                    for (start, end) in text_formatter.regexp.find_iter(strings.subject.as_str()) {
                        self.data_columns.columns[4].set_tag(t, (start, idx), (end, idx));
                    }
                }
            }
            let x = {
                let mut x = x + 1;
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let color = color.unwrap_or(self.color_cache.tag_default.bg);
                    let (_x, _) = write_string_to_grid(
                        t,
                        &mut self.data_columns.columns[4],
                        self.color_cache.tag_default.fg,
                        color,
                        self.color_cache.tag_default.attrs,
                        ((x + 1, idx), (min_width.4, idx)),
                        None,
                    );
                    self.data_columns.columns[4][(x, idx)].set_bg(color);
                    if _x < min_width.4 {
                        self.data_columns.columns[4][(_x, idx)]
                            .set_bg(color)
                            .set_keep_bg(true);
                    }
                    for x in (x + 1).._x {
                        self.data_columns.columns[4][(x, idx)]
                            .set_keep_fg(true)
                            .set_keep_bg(true)
                            .set_keep_attrs(true);
                    }
                    self.data_columns.columns[4][(x, idx)].set_keep_bg(true);
                    x = _x + 1;
                }
                x
            };
            for x in x..min_width.4 {
                self.data_columns.columns[4][(x, idx)]
                    .set_ch(' ')
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            if *has_attachments {
                self.data_columns.columns[3][(0, idx)].set_fg(self.color_cache.attachment_flag.fg);
            }
        }
    }
}

impl Component for ThreadListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        /*
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
        }
        */
        if !self.unfocused {
            if !self.is_dirty() {
                return;
            }
            self.dirty = false;
            /* Draw the entire list */
            self.draw_list(grid, area, context);
        } else {
            self.cursor_pos = self.new_cursor_pos;
            let upper_left = upper_left!(area);
            let bottom_right = bottom_right!(area);
            if self.length == 0 && self.dirty {
                clear_area(grid, area, self.color_cache.theme_default);
                context.dirty_areas.push_back(area);
                return;
            }

            /* Render the mail body in a pager, basically copy what HSplit does */
            let total_rows = get_y(bottom_right) - get_y(upper_left);
            let pager_ratio = context.runtime_settings.pager.pager_ratio;
            let bottom_entity_rows = (pager_ratio * total_rows) / 100;

            if bottom_entity_rows > total_rows {
                clear_area(grid, area, self.color_cache.theme_default);
                context.dirty_areas.push_back(area);
                return;
            }

            let idx = self.cursor_pos.2;

            /* Mark message as read */
            let must_highlight = {
                if self.length == 0 {
                    false
                } else {
                    let account = &context.accounts[self.cursor_pos.0];
                    let envelope: EnvelopeRef = account
                        .collection
                        .get_env(self.get_env_under_cursor(idx, context));
                    envelope.is_seen()
                }
            };

            if must_highlight {
                self.highlight_line_self(idx, context);
            }

            let mid = get_y(upper_left) + total_rows - bottom_entity_rows;
            self.draw_list(
                grid,
                (
                    upper_left,
                    (get_x(bottom_right), get_y(upper_left) + mid - 1),
                ),
                context,
            );
            if self.length == 0 {
                self.dirty = false;
                return;
            }
            {
                /* TODO: Move the box drawing business in separate functions */
                if get_x(upper_left) > 0 && grid[(get_x(upper_left) - 1, mid)].ch() == VERT_BOUNDARY
                {
                    grid[(get_x(upper_left) - 1, mid)].set_ch(LIGHT_VERTICAL_AND_RIGHT);
                }

                for i in get_x(upper_left)..=get_x(bottom_right) {
                    grid[(i, mid)].set_ch(HORZ_BOUNDARY);
                }
                context
                    .dirty_areas
                    .push_back((set_y(upper_left, mid), set_y(bottom_right, mid)));
            }
            // TODO: Make headers view configurable

            if !self.dirty {
                if let Some(v) = self.view.as_mut() {
                    v.draw(grid, (set_y(upper_left, mid + 1), bottom_right), context);
                }
                return;
            }

            let coordinates = (
                self.cursor_pos.0,
                self.cursor_pos.1,
                self.get_env_under_cursor(self.cursor_pos.2, context),
            );

            if let Some(ref mut v) = self.view {
                v.update(coordinates, context);
            } else {
                self.view = Some(MailView::new(coordinates, None, None, context));
            }

            self.view.as_mut().unwrap().draw(
                grid,
                (set_y(upper_left, mid + 1), bottom_right),
                context,
            );

            self.dirty = false;
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }
        match *event {
            UIEvent::Input(Key::Char('\n')) if !self.unfocused => {
                self.unfocused = true;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('i')) if self.unfocused => {
                self.unfocused = false;
                self.dirty = true;
                self.view = None;
                return true;
            }
            UIEvent::MailboxUpdate((ref idxa, ref idxf))
                if (*idxa, *idxf) == (self.new_cursor_pos.0, self.cursor_pos.1) =>
            {
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
            }
            UIEvent::StartupCheck(ref f) if *f == self.cursor_pos.1 => {
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
            }
            UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                let account = &context.accounts[self.cursor_pos.0];
                if !account.collection.contains_key(&new_hash) {
                    return false;
                }
                if let Some(row) = self.order.remove(old_hash) {
                    self.order.insert(*new_hash, row);
                    (self.rows[row].0).3 = *new_hash;
                    //self.row_updates.push(old_hash);
                }

                self.dirty = true;

                self.view.as_mut().map(|c| {
                    c.process_event(&mut UIEvent::EnvelopeRename(*old_hash, *new_hash), context)
                });
            }
            UIEvent::EnvelopeRemove(ref env_hash, _) => {
                if self.order.contains_key(env_hash) {
                    self.refresh_mailbox(context, false);
                    self.set_dirty(true);
                }
            }
            UIEvent::EnvelopeUpdate(ref env_hash) => {
                let account = &context.accounts[self.cursor_pos.0];
                if !account.collection.contains_key(env_hash) {
                    return false;
                }
                if self.order.contains_key(env_hash) {
                    //self.row_updates.push(*env_hash);
                }

                self.dirty = true;

                self.view
                    .as_mut()
                    .map(|c| c.process_event(&mut UIEvent::EnvelopeUpdate(*env_hash), context));
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Action(ref action) => match action {
                Action::SubSort(field, order) => {
                    debug!("SubSort {:?} , {:?}", field, order);
                    self.subsort = (*field, *order);
                    self.dirty = true;
                    self.refresh_mailbox(context, false);
                    return true;
                }
                Action::Sort(field, order) => {
                    debug!("Sort {:?} , {:?}", field, order);
                    self.sort = (*field, *order);
                    self.dirty = true;
                    self.refresh_mailbox(context, false);
                    return true;
                }
                _ => {}
            },
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.view.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
    fn set_dirty(&mut self, value: bool) {
        if let Some(p) = self.view.as_mut() {
            p.set_dirty(value);
        };
        self.dirty = value;
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        self.view
            .as_ref()
            .map(|p| p.get_shortcuts(context))
            .unwrap_or_default()
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
