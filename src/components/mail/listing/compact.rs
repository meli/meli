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

use std::{cmp, collections::BTreeMap, convert::TryInto, iter::FromIterator};

use indexmap::IndexSet;

use super::*;
use crate::{components::PageMovement, jobs::JoinHandle};

macro_rules! digits_of_num {
    ($num:expr) => {{
        const GUESS: [usize; 65] = [
            1, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 8, 8,
            8, 9, 9, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15,
            15, 15, 16, 16, 16, 17, 17, 17, 18, 18, 18, 18, 19,
        ];
        const TENS: [usize; 20] = [
            1,
            10,
            100,
            1000,
            10000,
            100000,
            1000000,
            10000000,
            100000000,
            1000000000,
            10000000000,
            100000000000,
            1000000000000,
            10000000000000,
            100000000000000,
            1000000000000000,
            10000000000000000,
            100000000000000000,
            1000000000000000000,
            10000000000000000000,
        ];
        const SIZE_IN_BITS: usize = std::mem::size_of::<usize>() * 8;

        let leading_zeros = $num.leading_zeros() as usize;
        let base_two_digits: usize = SIZE_IN_BITS - leading_zeros;
        let x = GUESS[base_two_digits];
        x + if $num >= TENS[x] { 1 } else { 0 }
    }};
}

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

macro_rules! row_attr {
    ($color_cache:expr, $even: expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
        ThemeAttribute {
            fg: if $highlighted {
                if $even {
                    $color_cache.even_highlighted.fg
                } else {
                    $color_cache.odd_highlighted.fg
                }
            } else if $selected {
                if $even {
                    $color_cache.even_selected.fg
                } else {
                    $color_cache.odd_selected.fg
                }
            } else if $unseen {
                if $even {
                    $color_cache.even_unseen.fg
                } else {
                    $color_cache.odd_unseen.fg
                }
            } else if $even {
                $color_cache.even.fg
            } else {
                $color_cache.odd.fg
            },
            bg: if $highlighted {
                if $even {
                    $color_cache.even_highlighted.bg
                } else {
                    $color_cache.odd_highlighted.bg
                }
            } else if $selected {
                if $even {
                    $color_cache.even_selected.bg
                } else {
                    $color_cache.odd_selected.bg
                }
            } else if $unseen {
                if $even {
                    $color_cache.even_unseen.bg
                } else {
                    $color_cache.odd_unseen.bg
                }
            } else if $even {
                $color_cache.even.bg
            } else {
                $color_cache.odd.bg
            },
            attrs: if $highlighted {
                if $even {
                    $color_cache.even_highlighted.attrs
                } else {
                    $color_cache.odd_highlighted.attrs
                }
            } else if $selected {
                if $even {
                    $color_cache.even_selected.attrs
                } else {
                    $color_cache.odd_selected.attrs
                }
            } else if $unseen {
                if $even {
                    $color_cache.even_unseen.attrs
                } else {
                    $color_cache.odd_unseen.attrs
                }
            } else if $even {
                $color_cache.even.attrs
            } else {
                $color_cache.odd.attrs
            },
        }
    }};
}

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the
/// `Envelope` content in a `ThreadView`.
#[derive(Debug)]
pub struct CompactListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (AccountHash, MailboxHash, usize),
    new_cursor_pos: (AccountHash, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    sortcmd: bool,
    subsort: (SortField, SortOrder),
    /// Cache current view.
    data_columns: DataColumns<4>,
    rows_drawn: SegmentTree,
    rows: RowsState<(ThreadHash, EnvelopeHash)>,

    #[allow(clippy::type_complexity)]
    search_job: Option<(String, JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>)>,
    #[allow(clippy::type_complexity)]
    select_job: Option<(String, JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>)>,
    filter_term: String,
    filtered_selection: Vec<ThreadHash>,
    filtered_order: HashMap<ThreadHash, usize>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` exists or not.
    focus: Focus,
    view: Box<ThreadView>,
    color_cache: ColorCache,

    movement: Option<PageMovement>,
    modifier_active: bool,
    modifier_command: Option<Modifier>,
    id: ComponentId,
}

impl MailListingTrait for CompactListing {
    fn row_updates(&mut self) -> &mut SmallVec<[EnvelopeHash; 8]> {
        &mut self.rows.row_updates
    }

    fn selection(&mut self) -> &mut HashMap<EnvelopeHash, bool> {
        &mut self.rows.selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[EnvelopeHash; 8]> {
        let is_selection_empty = !self
            .rows
            .selection
            .values()
            .cloned()
            .any(std::convert::identity);
        let cursor_iter;
        let sel_iter = if !is_selection_empty {
            cursor_iter = None;
            Some(
                self.rows
                    .selection
                    .iter()
                    .filter(|(_, v)| **v)
                    .map(|(k, _)| *k),
            )
        } else {
            if let Some(env_hashes) = self
                .get_thread_under_cursor(self.cursor_pos.2)
                .and_then(|thread| self.rows.thread_to_env.get(&thread).cloned())
            {
                cursor_iter = Some(env_hashes.into_iter());
            } else {
                cursor_iter = None;
            }
            None
        };
        let iter = sel_iter
            .into_iter()
            .flatten()
            .chain(cursor_iter.into_iter().flatten());
        SmallVec::from_iter(iter)
    }

    /// Fill the `self.data_columns` `CellBuffers` with the contents of the
    /// account mailbox the user has chosen.
    fn refresh_mailbox(&mut self, context: &mut Context, force: bool) {
        self.set_dirty(true);
        self.rows.clear();
        let old_cursor_pos = self.cursor_pos;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;

        self.color_cache = ColorCache::new(context, IndexStyle::Compact);

        // Get mailbox as a reference.
        //
        match context.accounts[&self.cursor_pos.0].load(self.cursor_pos.1) {
            Ok(()) => {}
            Err(_) => {
                let message: String =
                    context.accounts[&self.cursor_pos.0][&self.cursor_pos.1].status();
                self.data_columns.columns[0] =
                    CellBuffer::new_with_context(message.len(), 1, None, context);
                self.length = 0;
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
        }

        let threads = context.accounts[&self.cursor_pos.0]
            .collection
            .get_threads(self.cursor_pos.1);
        let mut roots = threads.roots();
        threads.group_inner_sort_by(
            &mut roots,
            self.sort,
            &context.accounts[&self.cursor_pos.0].collection.envelopes,
        );

        self.redraw_threads_list(
            context,
            Box::new(roots.into_iter()) as Box<dyn Iterator<Item = ThreadHash>>,
        );

        if !force && old_cursor_pos == self.new_cursor_pos {
            self.view.update(context);
        } else if self.unfocused() {
            if let Some(thread) = self.get_thread_under_cursor(self.cursor_pos.2) {
                self.view = Box::new(ThreadView::new(self.new_cursor_pos, thread, None, context));
            }
        }
    }

    fn redraw_threads_list(
        &mut self,
        context: &Context,
        items: Box<dyn Iterator<Item = ThreadHash>>,
    ) {
        let account = &context.accounts[&self.cursor_pos.0];

        let threads = account.collection.get_threads(self.cursor_pos.1);
        self.rows.clear();
        // Use account settings only if no sortcmd has been used
        if !self.sortcmd {
            self.sort = context.accounts[&self.cursor_pos.0].settings.account.order
        }
        self.length = 0;
        let mut min_width = (0, 0, 0, 0);
        #[allow(clippy::type_complexity)]
        let mut row_widths: (
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
            SmallVec<[u8; 1024]>,
        ) = (
            SmallVec::new(),
            SmallVec::new(),
            SmallVec::new(),
            SmallVec::new(),
        );

        let tags_lck = account.collection.tag_index.read().unwrap();

        let mut other_subjects = IndexSet::new();
        let mut tags = IndexSet::new();
        let mut from_address_list = Vec::new();
        let mut from_address_set: std::collections::HashSet<Vec<u8>> =
            std::collections::HashSet::new();
        'items_for_loop: for thread in items {
            let thread_node = &threads.thread_nodes()[&threads.thread_ref(thread).root()];
            let root_env_hash = if let Some(h) = thread_node.message().or_else(|| {
                if thread_node.children().is_empty() {
                    return None;
                }
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[&iter_ptr].message().is_none() {
                    if threads.thread_nodes()[&iter_ptr].children().is_empty() {
                        return None;
                    }
                    iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                }
                threads.thread_nodes()[&iter_ptr].message()
            }) {
                h
            } else {
                continue 'items_for_loop;
            };
            if !context.accounts[&self.cursor_pos.0].contains_key(root_env_hash) {
                debug!("key = {}", root_env_hash);
                debug!(
                    "name = {} {}",
                    account[&self.cursor_pos.1].name(),
                    context.accounts[&self.cursor_pos.0].name()
                );
                debug!("{:#?}", context.accounts);

                panic!();
            }
            let root_envelope: EnvelopeRef = context.accounts[&self.cursor_pos.0]
                .collection
                .get_env(root_env_hash);
            use melib::search::QueryTrait;
            if let Some(filter_query) = mailbox_settings!(
                context[self.cursor_pos.0][&self.cursor_pos.1]
                    .listing
                    .filter
            )
            .as_ref()
            {
                if !root_envelope.is_match(filter_query) {
                    continue;
                }
            }
            other_subjects.clear();
            tags.clear();
            from_address_list.clear();
            from_address_set.clear();
            for (envelope, show_subject) in threads
                .thread_group_iter(thread)
                .filter_map(|(_, h)| {
                    Some((
                        threads.thread_nodes()[&h].message()?,
                        threads.thread_nodes()[&h].show_subject(),
                    ))
                })
                .map(|(env_hash, show_subject)| {
                    (
                        context.accounts[&self.cursor_pos.0]
                            .collection
                            .get_env(env_hash),
                        show_subject,
                    )
                })
            {
                if show_subject {
                    other_subjects.insert(envelope.subject().to_string());
                }
                if account.backend_capabilities.supports_tags {
                    for &t in envelope.tags().iter() {
                        tags.insert(t);
                    }
                }

                for addr in envelope.from().iter() {
                    if from_address_set.contains(addr.address_spec_raw()) {
                        continue;
                    }
                    from_address_set.insert(addr.address_spec_raw().to_vec());
                    from_address_list.push(addr.clone());
                }
            }

            let row_attr = row_attr!(
                self.color_cache,
                self.length % 2 == 0,
                threads.thread_ref(thread).unseen() > 0,
                false,
                false
            );
            self.rows.row_attr_cache.insert(self.length, row_attr);

            let entry_strings = self.make_entry_string(
                &root_envelope,
                context,
                &tags_lck,
                &from_address_list,
                &threads,
                &other_subjects,
                &tags,
                thread,
            );
            row_widths
                .0
                .push(digits_of_num!(self.length).try_into().unwrap_or(255));
            /* date */
            row_widths.1.push(
                entry_strings
                    .date
                    .grapheme_width()
                    .try_into()
                    .unwrap_or(255),
            );
            /* from */
            row_widths.2.push(
                entry_strings
                    .from
                    .grapheme_width()
                    .try_into()
                    .unwrap_or(255),
            );
            /* subject */
            row_widths.3.push(
                (entry_strings.flag.grapheme_width()
                    + 1
                    + entry_strings.subject.grapheme_width()
                    + 1
                    + entry_strings.tags.grapheme_width())
                .try_into()
                .unwrap_or(255),
            );
            min_width.1 = cmp::max(min_width.1, entry_strings.date.grapheme_width()); /* date */
            min_width.2 = cmp::max(min_width.2, entry_strings.from.grapheme_width()); /* from */
            min_width.3 = cmp::max(
                min_width.3,
                entry_strings.flag.grapheme_width()
                    + 1
                    + entry_strings.subject.grapheme_width()
                    + 1
                    + entry_strings.tags.grapheme_width(),
            ); /* subject */
            self.rows.insert_thread(
                thread,
                (thread, root_env_hash),
                threads
                    .thread_to_envelope
                    .get(&thread)
                    .cloned()
                    .unwrap_or_default()
                    .into(),
                entry_strings,
            );
            self.length += 1;
        }

        min_width.0 = self.length.saturating_sub(1).to_string().len();

        self.data_columns.elasticities[0].set_rigid();
        self.data_columns.elasticities[1].set_rigid();
        self.data_columns.elasticities[2].set_grow(5, Some(35));
        self.data_columns.elasticities[3].set_rigid();
        self.data_columns
            .cursor_config
            .set_handle(true)
            .set_even_odd_theme(
                self.color_cache.even_highlighted,
                self.color_cache.odd_highlighted,
            );
        self.data_columns
            .theme_config
            .set_even_odd_theme(self.color_cache.even, self.color_cache.odd);

        /* index column */
        self.data_columns.columns[0] =
            CellBuffer::new_with_context(min_width.0, self.rows.len(), None, context);
        self.data_columns.segment_tree[0] = row_widths.0.into();

        /* date column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(min_width.1, self.rows.len(), None, context);
        self.data_columns.segment_tree[1] = row_widths.1.into();
        /* from column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(min_width.2, self.rows.len(), None, context);
        self.data_columns.segment_tree[2] = row_widths.2.into();
        /* subject column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context(min_width.3, self.rows.len(), None, context);
        self.data_columns.segment_tree[3] = row_widths.3.into();

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
        if self.length == 0 && self.filter_term.is_empty() {
            let message: String = account[&self.cursor_pos.1].status();
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), self.length + 1, None, context);
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                self.color_cache.theme_default.fg,
                self.color_cache.theme_default.bg,
                self.color_cache.theme_default.attrs,
                ((0, 0), (message.len() - 1, 0)),
                None,
            );
        }
    }
}

impl ListingTrait for CompactListing {
    fn coordinates(&self) -> (AccountHash, MailboxHash) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1)
    }

    fn set_coordinates(&mut self, coordinates: (AccountHash, MailboxHash)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.focus = Focus::None;
        self.view = Box::<ThreadView>::default();
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term.clear();
        self.rows.row_updates.clear();
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let thread_hash = if let Some(h) = self.get_thread_under_cursor(idx) {
            h
        } else {
            return;
        };

        let account = &context.accounts[&self.cursor_pos.0];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        let thread = threads.thread_ref(thread_hash);

        let row_attr = row_attr!(
            self.color_cache,
            idx % 2 == 0,
            thread.unseen() > 0,
            self.cursor_pos.2 == idx,
            self.rows.is_thread_selected(thread_hash)
        );
        let (upper_left, bottom_right) = area;
        let x = get_x(upper_left)
            + self.data_columns.widths[0]
            + self.data_columns.widths[1]
            + self.data_columns.widths[2]
            + 3 * 2;

        for c in grid.row_iter(
            get_x(upper_left)..(get_x(bottom_right) + 1),
            get_y(upper_left),
        ) {
            grid[c]
                .set_fg(row_attr.fg)
                .set_bg(row_attr.bg)
                .set_attrs(row_attr.attrs);
        }

        copy_area(
            grid,
            &self.data_columns.columns[3],
            (set_x(upper_left, x), bottom_right),
            (
                (0, idx),
                pos_dec(
                    (
                        self.data_columns.widths[3],
                        self.data_columns.columns[3].size().1,
                    ),
                    (1, 1),
                ),
            ),
        );
        for c in grid.row_iter(x..(get_x(bottom_right) + 1), get_y(upper_left)) {
            grid[c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
        }
    }

    /// Draw the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0
        {
            self.refresh_mailbox(context, false);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area, self.color_cache.theme_default);
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
                        self.new_cursor_pos.2 = (self.length.saturating_sub(1) / rows) * rows;
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
        let end_idx = cmp::min(self.length.saturating_sub(1), top_idx + rows - 1);
        self.draw_rows(context, top_idx, end_idx);

        /* If cursor position has changed, remove the highlight from the previous
         * position and apply it in the new one. */
        if self.cursor_pos.2 != self.new_cursor_pos.2 && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for &(idx, highlight) in &[(old_cursor_pos.2, false), (self.new_cursor_pos.2, true)] {
                if idx >= self.length {
                    continue; //bounds check
                }
                let new_area = nth_row_area(area, idx % rows);
                self.data_columns
                    .draw(grid, idx, self.cursor_pos.2, grid.bounds_iter(new_area));
                if highlight {
                    let row_attr = row_attr!(self.color_cache, idx % 2 == 0, false, true, false);
                    change_colors(grid, new_area, row_attr.fg, row_attr.bg);
                } else if let Some(row_attr) = self.rows.row_attr_cache.get(&idx) {
                    change_colors(grid, new_area, row_attr.fg, row_attr.bg);
                }
                context.dirty_areas.push_back(new_area);
            }
            if !self.force_draw {
                return;
            }
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }
        if self.new_cursor_pos.2 >= self.length {
            self.new_cursor_pos.2 = self.length - 1;
            self.cursor_pos.2 = self.new_cursor_pos.2;
        }

        /* Page_no has changed, so draw new page */
        _ = self
            .data_columns
            .recalc_widths((width!(area), height!(area)), top_idx);
        clear_area(grid, area, self.color_cache.theme_default);
        /* copy table columns */
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos.2, grid.bounds_iter(area));
        /* apply each row colors separately */
        for i in top_idx..(top_idx + height!(area)) {
            if let Some(row_attr) = self.rows.row_attr_cache.get(&i) {
                change_colors(grid, nth_row_area(area, i % rows), row_attr.fg, row_attr.bg);
            }
        }

        /* highlight cursor */
        let row_attr = row_attr!(
            self.color_cache,
            self.cursor_pos.2 % 2 == 0,
            false,
            true,
            false
        );
        change_colors(
            grid,
            nth_row_area(area, self.cursor_pos.2 % rows),
            row_attr.fg,
            row_attr.bg,
        );

        /* clear gap if available height is more than count of entries */
        if top_idx + rows > self.length {
            clear_area(
                grid,
                (pos_inc(upper_left, (0, rows - 1)), bottom_right),
                self.color_cache.theme_default,
            );
        }
        context.dirty_areas.push_back(area);
    }

    fn filter(
        &mut self,
        filter_term: String,
        results: SmallVec<[EnvelopeHash; 512]>,
        context: &Context,
    ) {
        self.length = 0;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term = filter_term;

        let account = &context.accounts[&self.cursor_pos.0];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        for env_hash in results {
            if !account.collection.contains_key(&env_hash) {
                continue;
            }
            let env_thread_node_hash = account.collection.get_env(env_hash).thread();
            if !threads.thread_nodes.contains_key(&env_thread_node_hash) {
                continue;
            }
            let thread = threads.find_group(threads.thread_nodes[&env_thread_node_hash].group);
            if self.filtered_order.contains_key(&thread) {
                continue;
            }
            if self.rows.all_threads.contains(&thread) {
                self.filtered_selection.push(thread);
                self.filtered_order
                    .insert(thread, self.filtered_selection.len() - 1);
            }
        }
        if !self.filtered_selection.is_empty() {
            threads.group_inner_sort_by(
                &mut self.filtered_selection,
                self.sort,
                &context.accounts[&self.cursor_pos.0].collection.envelopes,
            );
            self.new_cursor_pos.2 =
                std::cmp::min(self.filtered_selection.len() - 1, self.cursor_pos.2);
        } else {
            self.data_columns.columns[0] = CellBuffer::new_with_context(0, 0, None, context);
        }
        self.redraw_threads_list(
            context,
            Box::new(self.filtered_selection.clone().into_iter())
                as Box<dyn Iterator<Item = ThreadHash>>,
        );
    }

    fn unfocused(&self) -> bool {
        !matches!(self.focus, Focus::None)
    }

    fn set_modifier_active(&mut self, new_val: bool) {
        self.modifier_active = new_val;
    }

    fn set_modifier_command(&mut self, new_val: Option<Modifier>) {
        self.modifier_command = new_val;
    }

    fn modifier_command(&self) -> Option<Modifier> {
        self.modifier_command
    }

    fn set_movement(&mut self, mvm: PageMovement) {
        self.movement = Some(mvm);
        self.set_dirty(true);
    }

    fn set_focus(&mut self, new_value: Focus, context: &mut Context) {
        match new_value {
            Focus::None => {
                self.view
                    .process_event(&mut UIEvent::VisibilityChange(false), context);
                self.dirty = true;
                /* If self.rows.row_updates is not empty and we exit a thread, the row_update
                 * events will be performed but the list will not be drawn.
                 * So force a draw in any case.
                 */
                self.force_draw = true;
            }
            Focus::Entry => {
                self.force_draw = true;
                self.dirty = true;
                self.view.set_dirty(true);
            }
            Focus::EntryFullscreen => {
                self.view.set_dirty(true);
            }
        }
        self.focus = new_value;
    }

    fn focus(&self) -> Focus {
        self.focus
    }
}

impl fmt::Display for CompactListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl CompactListing {
    pub const DESCRIPTION: &'static str = "compact listing";
    pub fn new(coordinates: (AccountHash, MailboxHash)) -> Box<Self> {
        Box::new(CompactListing {
            cursor_pos: (coordinates.0, MailboxHash::default(), 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            sortcmd: false,
            subsort: (SortField::Date, SortOrder::Desc),
            search_job: None,
            select_job: None,
            filter_term: String::new(),
            filtered_selection: Vec::new(),
            filtered_order: HashMap::default(),
            focus: Focus::None,
            data_columns: DataColumns::default(),
            rows_drawn: SegmentTree::default(),
            rows: RowsState::default(),
            dirty: true,
            force_draw: true,
            view: Box::<ThreadView>::default(),
            color_cache: ColorCache::default(),
            movement: None,
            modifier_active: false,
            modifier_command: None,
            id: ComponentId::new_v4(),
        })
    }

    fn make_entry_string(
        &self,
        root_envelope: &Envelope,
        context: &Context,
        tags_lck: &BTreeMap<TagHash, String>,
        from: &[Address],
        threads: &Threads,
        other_subjects: &IndexSet<String>,
        tags: &IndexSet<TagHash>,
        hash: ThreadHash,
    ) -> EntryStrings {
        let thread = threads.thread_ref(hash);
        let mut tags_string = String::new();
        let mut colors: SmallVec<[_; 8]> = SmallVec::new();
        let account = &context.accounts[&self.cursor_pos.0];
        if account.backend_capabilities.supports_tags {
            for t in tags {
                if mailbox_settings!(
                    context[self.cursor_pos.0][&self.cursor_pos.1]
                        .tags
                        .ignore_tags
                )
                .contains(t)
                    || account_settings!(context[self.cursor_pos.0].tags.ignore_tags).contains(t)
                    || context.settings.tags.ignore_tags.contains(t)
                    || !tags_lck.contains_key(t)
                {
                    continue;
                }
                tags_string.push(' ');
                tags_string.push_str(tags_lck.get(t).as_ref().unwrap());
                tags_string.push(' ');
                colors.push(
                    mailbox_settings!(context[self.cursor_pos.0][&self.cursor_pos.1].tags.colors)
                        .get(t)
                        .cloned()
                        .or_else(|| {
                            account_settings!(context[self.cursor_pos.0].tags.colors)
                                .get(t)
                                .cloned()
                                .or_else(|| context.settings.tags.colors.get(t).cloned())
                        }),
                );
            }
            if !tags_string.is_empty() {
                tags_string.pop();
            }
        }
        let mut subject = if *mailbox_settings!(
            context[self.cursor_pos.0][&self.cursor_pos.1]
                .listing
                .thread_subject_pack
        ) {
            other_subjects
                .into_iter()
                .fold(String::new(), |mut acc, s| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.push_str(s);
                    acc
                })
        } else {
            root_envelope.subject().to_string()
        };
        subject.truncate_at_boundary(150);
        EntryStrings {
            date: DateString(ConversationsListing::format_date(context, thread.date())),
            subject: if thread.len() > 1 {
                SubjectString(format!("{} ({})", subject, thread.len()))
            } else {
                SubjectString(subject)
            },
            flag: FlagString(format!(
                "{selected}{snoozed}{unseen}{attachments}{whitespace}",
                selected = if self
                    .rows
                    .selection
                    .get(&root_envelope.hash())
                    .cloned()
                    .unwrap_or(false)
                {
                    mailbox_settings!(
                        context[self.cursor_pos.0][&self.cursor_pos.1]
                            .listing
                            .selected_flag
                    )
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(super::DEFAULT_SELECTED_FLAG)
                } else {
                    ""
                },
                snoozed = if thread.snoozed() {
                    mailbox_settings!(
                        context[self.cursor_pos.0][&self.cursor_pos.1]
                            .listing
                            .thread_snoozed_flag
                    )
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(super::DEFAULT_SNOOZED_FLAG)
                } else {
                    ""
                },
                unseen = if thread.unseen() > 0 {
                    mailbox_settings!(
                        context[self.cursor_pos.0][&self.cursor_pos.1]
                            .listing
                            .unseen_flag
                    )
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(super::DEFAULT_UNSEEN_FLAG)
                } else {
                    ""
                },
                attachments = if thread.has_attachments() {
                    mailbox_settings!(
                        context[self.cursor_pos.0][&self.cursor_pos.1]
                            .listing
                            .attachment_flag
                    )
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(super::DEFAULT_ATTACHMENT_FLAG)
                } else {
                    ""
                },
                whitespace = if self
                    .rows
                    .selection
                    .get(&root_envelope.hash())
                    .cloned()
                    .unwrap_or(false)
                    || thread.unseen() > 0
                    || thread.snoozed()
                    || thread.has_attachments()
                {
                    " "
                } else {
                    ""
                },
            )),
            from: FromString(address_list!((from) as comma_sep_list)),
            tags: TagString(tags_string, colors),
        }
    }

    fn get_thread_under_cursor(&self, cursor: usize) -> Option<ThreadHash> {
        if self.filter_term.is_empty() {
            self.rows
                .thread_order
                .iter()
                .find(|(_, &r)| r == cursor)
                .map(|(h, _)| h)
                .cloned()
        } else {
            self.filtered_selection.get(cursor).cloned()
        }
    }

    fn update_line(&mut self, context: &Context, env_hash: EnvelopeHash) {
        let account = &context.accounts[&self.cursor_pos.0];

        if !account.contains_key(env_hash) {
            /* The envelope has been renamed or removed, so wait for the appropriate
             * event to arrive */
            return;
        }
        let tags_lck = account.collection.tag_index.read().unwrap();
        let envelope: EnvelopeRef = account.collection.get_env(env_hash);
        let thread_hash = self.rows.env_to_thread[&env_hash];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        let thread = threads.thread_ref(thread_hash);
        let idx = self.rows.thread_order[&thread_hash];
        let row_attr = row_attr!(
            self.color_cache,
            idx % 2 == 0,
            thread.unseen() > 0,
            false,
            self.rows.is_thread_selected(thread_hash)
        );
        self.rows.row_attr_cache.insert(idx, row_attr);

        let mut other_subjects = IndexSet::new();
        let mut tags = IndexSet::new();
        let mut from_address_list = Vec::new();
        let mut from_address_set: std::collections::HashSet<Vec<u8>> =
            std::collections::HashSet::new();
        for (envelope, show_subject) in threads
            .thread_group_iter(thread_hash)
            .filter_map(|(_, h)| {
                threads.thread_nodes()[&h]
                    .message()
                    .map(|env_hash| (env_hash, threads.thread_nodes()[&h].show_subject()))
            })
            .map(|(env_hash, show_subject)| {
                (
                    context.accounts[&self.cursor_pos.0]
                        .collection
                        .get_env(env_hash),
                    show_subject,
                )
            })
        {
            if show_subject {
                other_subjects.insert(envelope.subject().to_string());
            }
            if account.backend_capabilities.supports_tags {
                for &t in envelope.tags().iter() {
                    tags.insert(t);
                }
            }
            for addr in envelope.from().iter() {
                if from_address_set.contains(addr.address_spec_raw()) {
                    continue;
                }
                from_address_set.insert(addr.address_spec_raw().to_vec());
                from_address_list.push(addr.clone());
            }
        }

        let strings = self.make_entry_string(
            &envelope,
            context,
            &tags_lck,
            &from_address_list,
            &threads,
            &other_subjects,
            &tags,
            thread_hash,
        );
        drop(envelope);
        let columns = &mut self.data_columns.columns;
        let min_width = (
            columns[0].size().0,
            columns[1].size().0,
            columns[2].size().0,
            columns[3].size().0,
        );
        let (x, _) = write_string_to_grid(
            &idx.to_string(),
            &mut columns[0],
            row_attr.fg,
            row_attr.bg,
            row_attr.attrs,
            ((0, idx), (min_width.0, idx)),
            None,
        );
        for c in columns[0].row_iter(x..min_width.0, idx) {
            columns[0][c].set_bg(row_attr.bg).set_ch(' ');
        }
        let (x, _) = write_string_to_grid(
            &strings.date,
            &mut columns[1],
            row_attr.fg,
            row_attr.bg,
            row_attr.attrs,
            ((0, idx), (min_width.1.saturating_sub(1), idx)),
            None,
        );
        for c in columns[1].row_iter(x..min_width.1, idx) {
            columns[1][c].set_bg(row_attr.bg).set_ch(' ');
        }
        let (x, _) = write_string_to_grid(
            &strings.from,
            &mut columns[2],
            row_attr.fg,
            row_attr.bg,
            row_attr.attrs,
            ((0, idx), (min_width.2, idx)),
            None,
        );
        for c in columns[2].row_iter(x..min_width.2, idx) {
            columns[2][c].set_bg(row_attr.bg).set_ch(' ');
        }
        let (x, _) = write_string_to_grid(
            &strings.flag,
            &mut columns[3],
            row_attr.fg,
            row_attr.bg,
            row_attr.attrs,
            ((0, idx), (min_width.3, idx)),
            None,
        );
        let (x, _) = write_string_to_grid(
            &strings.subject,
            &mut columns[3],
            row_attr.fg,
            row_attr.bg,
            row_attr.attrs,
            ((x, idx), (min_width.3, idx)),
            None,
        );
        columns[3][(x, idx)].set_bg(row_attr.bg).set_ch(' ');
        let x = {
            let mut x = x + 1;
            for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                let color = color.unwrap_or(self.color_cache.tag_default.bg);
                let (_x, _) = write_string_to_grid(
                    t,
                    &mut columns[3],
                    self.color_cache.tag_default.fg,
                    color,
                    self.color_cache.tag_default.attrs,
                    ((x + 1, idx), (min_width.3, idx)),
                    None,
                );
                for c in columns[3].row_iter(x..(x + 1), idx) {
                    columns[3][c].set_bg(color);
                }
                for c in columns[3].row_iter(_x..(_x + 1), idx) {
                    columns[3][c].set_bg(color).set_keep_bg(true);
                }
                for c in columns[3].row_iter((x + 1)..(_x + 1), idx) {
                    columns[3][c]
                        .set_keep_fg(true)
                        .set_keep_bg(true)
                        .set_keep_attrs(true);
                }
                for c in columns[3].row_iter(x..(x + 1), idx) {
                    columns[3][c].set_keep_bg(true);
                }
                x = _x + 1;
                columns[3][(x, idx)].set_bg(row_attr.bg).set_ch(' ');
            }
            x
        };
        for c in columns[3].row_iter(x..min_width.3, idx) {
            columns[3][c].set_ch(' ').set_bg(row_attr.bg);
        }
        *self.rows.entries.get_mut(idx).unwrap() = ((thread_hash, env_hash), strings);
        self.rows_drawn.update(idx, 1);
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
        );

        for (idx, ((_thread_hash, root_env_hash), strings)) in self
            .rows
            .entries
            .iter()
            .enumerate()
            .skip(start)
            .take(end - start + 1)
        {
            if !context.accounts[&self.cursor_pos.0].contains_key(*root_env_hash) {
                //debug!("key = {}", root_env_hash);
                //debug!(
                //    "name = {} {}",
                //    account[&self.cursor_pos.1].name(),
                //    context.accounts[&self.cursor_pos.0].name()
                //);
                //debug!("{:#?}", context.accounts);

                panic!();
            }
            let row_attr = self.rows.row_attr_cache[&idx];
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
            let (x, _) = write_string_to_grid(
                &strings.subject,
                &mut self.data_columns.columns[3],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((x, idx), (min_width.3, idx)),
                None,
            );
            #[cfg(feature = "regexp")]
            {
                for text_formatter in crate::conf::text_format_regexps(context, "listing.subject") {
                    let t = self.data_columns.columns[3].insert_tag(text_formatter.tag);
                    for (start, end) in text_formatter.regexp.find_iter(strings.subject.as_str()) {
                        self.data_columns.columns[3].set_tag(t, (start, idx), (end, idx));
                    }
                }
            }
            let x = {
                let mut x = x + 1;
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let color = color.unwrap_or(self.color_cache.tag_default.bg);
                    let (_x, _) = write_string_to_grid(
                        t,
                        &mut self.data_columns.columns[3],
                        self.color_cache.tag_default.fg,
                        color,
                        self.color_cache.tag_default.attrs,
                        ((x + 1, idx), (min_width.3, idx)),
                        None,
                    );
                    self.data_columns.columns[3][(x, idx)].set_bg(color);
                    if _x < min_width.3 {
                        self.data_columns.columns[3][(_x, idx)]
                            .set_bg(color)
                            .set_keep_bg(true);
                    }
                    for x in (x + 1).._x {
                        self.data_columns.columns[3][(x, idx)]
                            .set_keep_fg(true)
                            .set_keep_bg(true)
                            .set_keep_attrs(true);
                    }
                    self.data_columns.columns[3][(x, idx)].set_keep_bg(true);
                    x = _x + 1;
                }
                x
            };
            for x in x..min_width.3 {
                self.data_columns.columns[3][(x, idx)]
                    .set_ch(' ')
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
        }
    }

    fn select(
        &mut self,
        search_term: &str,
        results: Result<SmallVec<[EnvelopeHash; 512]>>,
        context: &mut Context,
    ) {
        let account = &context.accounts[&self.cursor_pos.0];
        match results {
            Ok(results) => {
                let threads = account.collection.get_threads(self.cursor_pos.1);
                for env_hash in results {
                    if !account.collection.contains_key(&env_hash) {
                        continue;
                    }
                    debug!(account.collection.get_env(env_hash).subject());
                    let env_thread_node_hash = account.collection.get_env(env_hash).thread();
                    if !threads.thread_nodes.contains_key(&env_thread_node_hash) {
                        continue;
                    }
                    let thread =
                        threads.find_group(threads.thread_nodes[&env_thread_node_hash].group);
                    if self.rows.all_threads.contains(&thread) {
                        self.rows
                            .selection
                            .entry(env_hash)
                            .and_modify(|entry| *entry = true);
                    }
                }
            }
            Err(err) => {
                self.cursor_pos.2 = 0;
                self.new_cursor_pos.2 = 0;
                let message = format!(
                    "Encountered an error while searching for `{}`: {}.",
                    search_term, &err
                );
                log::error!("{}", message);
                context.replies.push_back(UIEvent::Notification(
                    Some("Could not perform search".to_string()),
                    message,
                    Some(crate::types::NotificationType::Error(err.kind)),
                ));
            }
        }
    }
}

impl Component for CompactListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }

        if matches!(self.focus, Focus::EntryFullscreen) {
            return self.view.draw(grid, area, context);
        }

        if !self.unfocused() {
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
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    area,
                    Some(get_x(upper_left)),
                );
                let default_cell = {
                    let mut ret = Cell::with_char(' ');
                    ret.set_fg(self.color_cache.theme_default.fg)
                        .set_bg(self.color_cache.theme_default.bg)
                        .set_attrs(self.color_cache.theme_default.attrs);
                    ret
                };
                for row in grid.bounds_iter(((x, y), set_y(bottom_right, y))) {
                    for c in row {
                        grid[c] = default_cell;
                    }
                }
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 1)));

                area = (set_y(upper_left, y + 1), bottom_right);
            }
            let (upper_left, bottom_right) = area;
            let rows = get_y(bottom_right) - get_y(upper_left) + 1;

            if let Some(modifier) = self.modifier_command.take() {
                if let Some(mvm) = self.movement.as_ref() {
                    match mvm {
                        PageMovement::Up(amount) => {
                            for c in self.cursor_pos.2.saturating_sub(*amount)..=self.cursor_pos.2 {
                                if let Some(thread) = self.get_thread_under_cursor(c) {
                                    self.rows.update_selection_with_thread(
                                        thread,
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                |e: &mut bool| *e = !*e
                                            }
                                            Modifier::Union => |e: &mut bool| *e = true,
                                            Modifier::Difference => |e: &mut bool| *e = false,
                                            Modifier::Intersection => |_: &mut bool| {},
                                        },
                                    );
                                }
                            }
                            if modifier == Modifier::Intersection {
                                for c in (0..self.cursor_pos.2.saturating_sub(*amount))
                                    .chain((self.cursor_pos.2 + 2)..self.length)
                                {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageUp(multiplier) => {
                            for c in self.cursor_pos.2.saturating_sub(rows * multiplier)
                                ..=self.cursor_pos.2
                            {
                                if let Some(thread) = self.get_thread_under_cursor(c) {
                                    self.rows.update_selection_with_thread(
                                        thread,
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                |e: &mut bool| *e = !*e
                                            }
                                            Modifier::Union => |e: &mut bool| *e = true,
                                            Modifier::Difference => |e: &mut bool| *e = false,
                                            Modifier::Intersection => |_: &mut bool| {},
                                        },
                                    );
                                }
                            }
                        }
                        PageMovement::Down(amount) => {
                            for c in self.cursor_pos.2
                                ..std::cmp::min(self.length, self.cursor_pos.2 + amount + 1)
                            {
                                if let Some(thread) = self.get_thread_under_cursor(c) {
                                    self.rows.update_selection_with_thread(
                                        thread,
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                |e: &mut bool| *e = !*e
                                            }
                                            Modifier::Union => |e: &mut bool| *e = true,
                                            Modifier::Difference => |e: &mut bool| *e = false,
                                            Modifier::Intersection => |_: &mut bool| {},
                                        },
                                    );
                                }
                            }
                            if modifier == Modifier::Intersection {
                                for c in (0..self.cursor_pos.2).chain(
                                    (std::cmp::min(self.length, self.cursor_pos.2 + amount) + 1)
                                        ..self.length,
                                ) {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageDown(multiplier) => {
                            for c in self.cursor_pos.2
                                ..std::cmp::min(
                                    self.cursor_pos.2 + rows * multiplier + 1,
                                    self.length,
                                )
                            {
                                if let Some(thread) = self.get_thread_under_cursor(c) {
                                    self.rows.update_selection_with_thread(
                                        thread,
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                |e: &mut bool| *e = !*e
                                            }
                                            Modifier::Union => |e: &mut bool| *e = true,
                                            Modifier::Difference => |e: &mut bool| *e = false,
                                            Modifier::Intersection => |_: &mut bool| {},
                                        },
                                    );
                                }
                            }
                            if modifier == Modifier::Intersection {
                                for c in (0..self.cursor_pos.2).chain(
                                    (std::cmp::min(
                                        self.cursor_pos.2 + rows * multiplier,
                                        self.length,
                                    ) + 1)..self.length,
                                ) {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::Right(_) | PageMovement::Left(_) => {}
                        PageMovement::Home => {
                            for c in 0..=self.cursor_pos.2 {
                                if let Some(thread) = self.get_thread_under_cursor(c) {
                                    self.rows.update_selection_with_thread(
                                        thread,
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                |e: &mut bool| *e = !*e
                                            }
                                            Modifier::Union => |e: &mut bool| *e = true,
                                            Modifier::Difference => |e: &mut bool| *e = false,
                                            Modifier::Intersection => |_: &mut bool| {},
                                        },
                                    );
                                }
                            }
                            if modifier == Modifier::Intersection {
                                for c in (self.cursor_pos.2)..self.length {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::End => {
                            for c in self.cursor_pos.2..self.length {
                                if let Some(thread) = self.get_thread_under_cursor(c) {
                                    self.rows.update_selection_with_thread(
                                        thread,
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                |e: &mut bool| *e = !*e
                                            }
                                            Modifier::Union => |e: &mut bool| *e = true,
                                            Modifier::Difference => |e: &mut bool| *e = false,
                                            Modifier::Intersection => |_: &mut bool| {},
                                        },
                                    );
                                }
                            }
                            if modifier == Modifier::Intersection {
                                for c in 0..self.cursor_pos.2 {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                    }
                }
                self.force_draw = true;
            }

            if !self.rows.row_updates.is_empty() {
                while let Some(env_hash) = self.rows.row_updates.pop() {
                    self.update_line(context, env_hash);
                    let row: usize = self.rows.env_order[&env_hash];
                    let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

                    let top_idx = page_no * rows;
                    self.force_draw |= row >= top_idx && row < top_idx + rows;
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
                clear_area(grid, area, self.color_cache.theme_default);
                context.dirty_areas.push_back(area);
                return;
            }

            self.view.draw(grid, area, context);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);

        match (&event, self.focus) {
            (UIEvent::Input(ref k), Focus::Entry)
                if shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"]) =>
            {
                self.set_focus(Focus::EntryFullscreen, context);
                return true;
            }
            (UIEvent::Input(ref k), Focus::EntryFullscreen)
                if shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_left"]) =>
            {
                self.set_focus(Focus::Entry, context);
                return true;
            }
            (UIEvent::Input(ref k), Focus::Entry)
                if shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_left"]) =>
            {
                self.set_focus(Focus::None, context);
                return true;
            }
            _ => {}
        }

        if self.unfocused() && self.view.process_event(event, context) {
            return true;
        }

        if self.length > 0 {
            match *event {
                UIEvent::Input(ref k)
                    if matches!(self.focus, Focus::None)
                        && (shortcut!(k == shortcuts[Shortcuts::LISTING]["open_entry"])
                            || shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"])) =>
                {
                    if let Some(thread) = self.get_thread_under_cursor(self.cursor_pos.2) {
                        self.view =
                            Box::new(ThreadView::new(self.cursor_pos, thread, None, context));
                        self.set_focus(Focus::Entry, context);
                    }
                    return true;
                }
                UIEvent::Input(ref k)
                    if matches!(self.focus, Focus::Entry)
                        && shortcut!(k == shortcuts[Shortcuts::LISTING]["exit_entry"]) =>
                {
                    self.set_focus(Focus::None, context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if matches!(self.focus, Focus::None)
                        && shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"]) =>
                {
                    self.set_focus(Focus::Entry, context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if !matches!(self.focus, Focus::None)
                        && shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_left"]) =>
                {
                    match self.focus {
                        Focus::Entry => {
                            self.set_focus(Focus::None, context);
                        }
                        Focus::EntryFullscreen => {
                            self.set_focus(Focus::Entry, context);
                        }
                        Focus::None => {
                            unreachable!();
                        }
                    }
                    return true;
                }
                UIEvent::Input(ref key)
                    if !self.unfocused()
                        && shortcut!(key == shortcuts[Shortcuts::LISTING]["select_entry"]) =>
                {
                    if self.modifier_active && self.modifier_command.is_none() {
                        self.modifier_command = Some(Modifier::default());
                    } else if let Some(thread_hash) =
                        self.get_thread_under_cursor(self.cursor_pos.2)
                    {
                        self.rows
                            .update_selection_with_thread(thread_hash, |e| *e = !*e);
                        self.set_dirty(true);
                    }
                    return true;
                }
                UIEvent::Action(ref action) => {
                    match action {
                        Action::Sort(field, order) if !self.unfocused() => {
                            debug!("Sort {:?} , {:?}", field, order);
                            self.sort = (*field, *order);
                            self.sortcmd = true;
                            if !self.filtered_selection.is_empty() {
                                // FIXME: perform sort
                                self.set_dirty(true);
                            } else {
                                self.refresh_mailbox(context, false);
                            }
                            return true;
                        }
                        Action::SubSort(field, order) if !self.unfocused() => {
                            debug!("SubSort {:?} , {:?}", field, order);
                            self.subsort = (*field, *order);
                            // FIXME: perform subsort.
                            return true;
                        }
                        Action::Listing(ToggleThreadSnooze) if !self.unfocused() => {
                            /*
                            let thread = self.get_thread_under_cursor(self.cursor_pos.2);
                            let account = &mut context.accounts[&self.cursor_pos.0];
                            account
                                .collection
                                .threads
                                .write()
                                .unwrap()
                                .entry(self.cursor_pos.1)
                                .and_modify(|threads| {
                                    let is_snoozed = threads.thread_ref(thread).snoozed();
                                    threads.thread_ref_mut(thread).set_snoozed(!is_snoozed);
                                });
                            self.rows.row_updates.push(thread);
                            self.refresh_mailbox(context, false);
                            */
                            return true;
                        }

                        _ => {}
                    }
                }
                _ => {}
            }
        }
        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.color_cache = ColorCache::new(context, IndexStyle::Compact);
                self.refresh_mailbox(context, true);
                self.set_dirty(true);
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
                let account = &context.accounts[&self.cursor_pos.0];
                let threads = account.collection.get_threads(self.cursor_pos.1);
                if !account.collection.contains_key(new_hash) {
                    return false;
                }
                let new_env_thread_node_hash = account.collection.get_env(*new_hash).thread();
                if !threads.thread_nodes.contains_key(&new_env_thread_node_hash) {
                    return false;
                }
                let thread: ThreadHash =
                    threads.find_group(threads.thread_nodes()[&new_env_thread_node_hash].group);
                drop(threads);
                if self.rows.contains_thread(thread) {
                    self.rows.row_update_add_thread(thread);
                }

                self.set_dirty(true);

                if self.unfocused() {
                    self.view
                        .process_event(&mut UIEvent::EnvelopeRename(*old_hash, *new_hash), context);
                }
            }
            UIEvent::EnvelopeRemove(ref _env_hash, ref thread_hash) => {
                if self.rows.thread_order.contains_key(thread_hash) {
                    self.refresh_mailbox(context, false);
                    self.set_dirty(true);
                }
            }
            UIEvent::EnvelopeUpdate(ref env_hash) => {
                let account = &context.accounts[&self.cursor_pos.0];
                let threads = account.collection.get_threads(self.cursor_pos.1);
                if !account.collection.contains_key(env_hash) {
                    return false;
                }
                let new_env_thread_node_hash = account.collection.get_env(*env_hash).thread();
                if !threads.thread_nodes.contains_key(&new_env_thread_node_hash) {
                    return false;
                }
                let thread: ThreadHash =
                    threads.find_group(threads.thread_nodes()[&new_env_thread_node_hash].group);
                drop(threads);
                if self.rows.contains_thread(thread) {
                    self.rows.row_update_add_thread(thread);
                }

                self.set_dirty(true);

                if self.unfocused() {
                    self.view
                        .process_event(&mut UIEvent::EnvelopeUpdate(*env_hash), context);
                }
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Input(Key::Esc)
                if !self.unfocused()
                    && self
                        .rows
                        .selection
                        .values()
                        .cloned()
                        .any(std::convert::identity) =>
            {
                for v in self.rows.selection.values_mut() {
                    *v = false;
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Esc) if !self.unfocused() && !self.filter_term.is_empty() => {
                self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1));
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Action(Action::Listing(Search(ref filter_term))) if !self.unfocused() => {
                match context.accounts[&self.cursor_pos.0].search(
                    filter_term,
                    self.sort,
                    self.cursor_pos.1,
                ) {
                    Ok(job) => {
                        let handle = context.accounts[&self.cursor_pos.0]
                            .job_executor
                            .spawn_specialized(job);
                        self.search_job = Some((filter_term.to_string(), handle));
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not perform search".to_string()),
                            err.to_string(),
                            Some(crate::types::NotificationType::Error(err.kind)),
                        ));
                    }
                };
                self.set_dirty(true);
            }
            UIEvent::Action(Action::Listing(Select(ref search_term))) if !self.unfocused() => {
                match context.accounts[&self.cursor_pos.0].search(
                    search_term,
                    self.sort,
                    self.cursor_pos.1,
                ) {
                    Ok(job) => {
                        let mut handle = context.accounts[&self.cursor_pos.0]
                            .job_executor
                            .spawn_specialized(job);
                        if let Ok(Some(search_result)) = try_recv_timeout!(&mut handle.chan) {
                            self.select(search_term, search_result, context);
                        } else {
                            self.select_job = Some((search_term.to_string(), handle));
                        }
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not perform search".to_string()),
                            err.to_string(),
                            Some(crate::types::NotificationType::Error(err.kind)),
                        ));
                    }
                };
                self.set_dirty(true);
            }
            UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                if self
                    .search_job
                    .as_ref()
                    .map(|(_, j)| j == job_id)
                    .unwrap_or(false) =>
            {
                let (filter_term, mut handle) = self.search_job.take().unwrap();
                match handle.chan.try_recv() {
                    Err(_) => { /* search was canceled */ }
                    Ok(None) => { /* something happened, perhaps a worker thread panicked */ }
                    Ok(Some(Ok(results))) => self.filter(filter_term, results, context),
                    Ok(Some(Err(err))) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not perform search".to_string()),
                            err.to_string(),
                            Some(crate::types::NotificationType::Error(err.kind)),
                        ));
                    }
                }
                self.set_dirty(true);
            }
            UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                if self
                    .select_job
                    .as_ref()
                    .map(|(_, j)| j == job_id)
                    .unwrap_or(false) =>
            {
                let (search_term, mut handle) = self.select_job.take().unwrap();
                match handle.chan.try_recv() {
                    Err(_) => { /* search was canceled */ }
                    Ok(None) => { /* something happened, perhaps a worker thread panicked */ }
                    Ok(Some(results)) => self.select(&search_term, results, context),
                }
                self.set_dirty(true);
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        match self.focus {
            Focus::None => self.dirty,
            Focus::Entry => self.dirty || self.view.is_dirty(),
            Focus::EntryFullscreen => self.view.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if self.unfocused() {
            self.view.set_dirty(value);
        }
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.unfocused() {
            self.view.get_shortcuts(context)
        } else {
            ShortcutMaps::default()
        };

        map.insert(
            Shortcuts::LISTING,
            context.settings.shortcuts.listing.key_values(),
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
