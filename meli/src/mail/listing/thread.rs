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

use std::{convert::TryInto, iter::FromIterator};

use melib::{Address, SortField, SortOrder, ThreadNode, Threads};

use super::*;
use crate::{components::PageMovement, jobs::JoinHandle, segment_tree::SegmentTree};

macro_rules! row_attr {
    ($color_cache:expr, even: $even:expr, unseen: $unseen:expr, highlighted: $highlighted:expr, selected: $selected:expr  $(,)*) => {{
        let color_cache = &$color_cache;
        let even = $even;
        let unseen = $unseen;
        let highlighted = $highlighted;
        let selected = $selected;
        ThemeAttribute {
            fg: if highlighted && selected {
                if even {
                    color_cache.even_highlighted_selected.fg
                } else {
                    color_cache.odd_highlighted_selected.fg
                }
            } else if highlighted {
                if even {
                    color_cache.even_highlighted.fg
                } else {
                    color_cache.odd_highlighted.fg
                }
            } else if selected {
                if even {
                    color_cache.even_selected.fg
                } else {
                    color_cache.odd_selected.fg
                }
            } else if unseen {
                if even {
                    color_cache.even_unseen.fg
                } else {
                    color_cache.odd_unseen.fg
                }
            } else if even {
                color_cache.even.fg
            } else {
                color_cache.odd.fg
            },
            bg: if highlighted && selected {
                if even {
                    color_cache.even_highlighted_selected.bg
                } else {
                    color_cache.odd_highlighted_selected.bg
                }
            } else if highlighted {
                if even {
                    color_cache.even_highlighted.bg
                } else {
                    color_cache.odd_highlighted.bg
                }
            } else if selected {
                if even {
                    color_cache.even_selected.bg
                } else {
                    color_cache.odd_selected.bg
                }
            } else if unseen {
                if even {
                    color_cache.even_unseen.bg
                } else {
                    color_cache.odd_unseen.bg
                }
            } else if even {
                color_cache.even.bg
            } else {
                color_cache.odd.bg
            },
            attrs: if highlighted && selected {
                if even {
                    color_cache.even_highlighted_selected.attrs
                } else {
                    color_cache.odd_highlighted_selected.attrs
                }
            } else if highlighted {
                if even {
                    color_cache.even_highlighted.attrs
                } else {
                    color_cache.odd_highlighted.attrs
                }
            } else if selected {
                if even {
                    color_cache.even_selected.attrs
                } else {
                    color_cache.odd_selected.attrs
                }
            } else if unseen {
                if even {
                    color_cache.even_unseen.attrs
                } else {
                    color_cache.odd_unseen.attrs
                }
            } else if even {
                color_cache.even.attrs
            } else {
                color_cache.odd.attrs
            },
        }
    }};
}

/// A list of all mail ([`Envelope`]s) in a `Mailbox`. On `\n`
/// it opens the [`Envelope`] content in a [`MailView`].
#[derive(Debug)]
pub struct ThreadListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (AccountHash, MailboxHash, usize),
    new_cursor_pos: (AccountHash, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    /// Cache current view.
    color_cache: ColorCache,

    #[allow(clippy::type_complexity)]
    search_job: Option<(String, JoinHandle<Result<Vec<EnvelopeHash>>>)>,
    #[allow(clippy::type_complexity)]
    select_job: Option<(String, JoinHandle<Result<Vec<EnvelopeHash>>>)>,
    filter_term: String,
    filtered_selection: Vec<ThreadHash>,
    filtered_order: HashMap<ThreadHash, usize>,
    data_columns: DataColumns<5>,
    rows_drawn: SegmentTree,
    rows: RowsState<(ThreadHash, EnvelopeHash)>,
    seen_cache: IndexMap<EnvelopeHash, bool>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` is visible or not.
    focus: Focus,
    initialized: bool,
    modifier_active: bool,
    modifier_command: Option<Modifier>,
    movement: Option<PageMovement>,
    view_area: Option<Area>,
    parent: ComponentId,
    id: ComponentId,
}

impl MailListingTrait for ThreadListing {
    fn row_updates(&mut self) -> &mut SmallVec<[EnvelopeHash; 8]> {
        &mut self.rows.row_updates
    }

    fn selection(&self) -> &HashMap<EnvelopeHash, bool> {
        &self.rows.selection
    }

    fn selection_mut(&mut self) -> &mut HashMap<EnvelopeHash, bool> {
        &mut self.rows.selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[EnvelopeHash; 8]> {
        let is_selection_empty: bool = !self
            .selection()
            .values()
            .cloned()
            .any(std::convert::identity);
        if is_selection_empty {
            return self
                .get_env_under_cursor(self.new_cursor_pos.2)
                .into_iter()
                .collect::<_>();
        }
        SmallVec::from_iter(self.selection().iter().filter(|(_, &v)| v).map(|(k, _)| *k))
    }

    /// Fill the `self.content` `CellBuffer` with the contents of the account
    /// mailbox the user has chosen.
    fn refresh_mailbox(&mut self, context: &mut Context, _force: bool) {
        self.set_dirty(true);
        self.initialized = true;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;

        self.color_cache = ColorCache::new(context, IndexStyle::Threaded);

        // Get mailbox as a reference.
        //
        match context.accounts[&self.cursor_pos.0].load(self.cursor_pos.1, true) {
            Ok(_) => {}
            Err(_) => {
                self.length = 0;
                let message: String =
                    context.accounts[&self.cursor_pos.0][&self.cursor_pos.1].status();
                _ = self.data_columns.columns[0].resize_with_context(message.len(), 1, context);
                let area = self.data_columns.columns[0].area();
                self.data_columns.columns[0].grid_mut().write_string(
                    message.as_str(),
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    area,
                    None,
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
    }

    fn redraw_threads_list(
        &mut self,
        context: &Context,
        items: Box<dyn Iterator<Item = ThreadHash>>,
    ) {
        let account = &context.accounts[&self.new_cursor_pos.0];
        let threads = account.collection.get_threads(self.new_cursor_pos.1);
        self.length = 0;
        self.rows.clear();
        if threads.len() == 0 {
            let message: String = account[&self.new_cursor_pos.1].status();
            _ = self.data_columns.columns[0].resize_with_context(message.len(), 1, context);
            let area = self.data_columns.columns[0].area();
            self.data_columns.columns[0].grid_mut().write_string(
                message.as_str(),
                self.color_cache.theme_default.fg,
                self.color_cache.theme_default.bg,
                self.color_cache.theme_default.attrs,
                area,
                None,
                None,
            );
            return;
        }
        let mut min_width = (0, 0, 0, 0, 0);
        #[allow(clippy::type_complexity)]
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

        let roots = items
            .filter_map(|r| threads.groups[&r].root().map(|r| r.root))
            .collect::<SmallVec<[ThreadNodeHash; 1024]>>();
        let mut indentations: Vec<bool> = Vec::with_capacity(6);
        let mut iter = threads.threads_iter(roots).peekable();
        let thread_nodes: &HashMap<ThreadNodeHash, ThreadNode> = threads.thread_nodes();
        /* This is just a desugared for loop so that we can use .peek() */
        let mut idx: usize = 0;
        let mut prev_group = ThreadHash::null();
        let mut hide_from: bool = false;
        let threaded_repeat_identical_from_values: bool = *mailbox_settings!(
            context[self.new_cursor_pos.0][&self.new_cursor_pos.1]
                .listing
                .threaded_repeat_identical_from_values
        );
        let should_highlight_self = mailbox_settings!(
            context[self.cursor_pos.0][&self.cursor_pos.1]
                .listing
                .highlight_self
        )
        .is_true();
        let my_address: Address = context.accounts[&self.cursor_pos.0]
            .settings
            .account
            .main_identity_address();
        let highlight_self_colwidth: usize = mailbox_settings!(
            context[self.cursor_pos.0][&self.cursor_pos.1]
                .listing
                .highlight_self_flag
        )
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or(super::DEFAULT_HIGHLIGHT_SELF_FLAG)
        .grapheme_width();
        while let Some((indentation, thread_node_hash, has_sibling)) = iter.next() {
            let thread_node = &thread_nodes[&thread_node_hash];

            if let Some(env_hash) = thread_node.message() {
                let envelope: EnvelopeRef = account.collection.get_env(env_hash);
                use melib::search::QueryTrait;
                if let Some(filter_query) = mailbox_settings!(
                    context[self.new_cursor_pos.0][&self.new_cursor_pos.1]
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
                entry_strings.highlight_self = should_highlight_self
                    && (envelope.recipient_any(&my_address) || envelope.sender_any(&my_address));
                entry_strings.subject = SubjectString(Self::make_thread_entry(
                    &envelope,
                    indentation,
                    thread_node_hash,
                    &threads,
                    &indentations,
                    has_sibling,
                    is_root,
                ));
                if hide_from {
                    entry_strings.from.clear();
                }
                hide_from = !threaded_repeat_identical_from_values
                    && matches!(
                        iter.peek(),
                        Some((_, tnh, _)) if thread_nodes[tnh].message().map(|next| account.collection.get_env(next).from() == envelope.from()
                                             && threads.find_group(thread_nodes[tnh].group) == prev_group).unwrap_or(false)
                    );
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
                    (entry_strings.flag.grapheme_width()
                        + usize::from(entry_strings.highlight_self) * highlight_self_colwidth)
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
                min_width.1 = min_width.1.max(entry_strings.date.grapheme_width()); /* date */
                min_width.2 = min_width.2.max(entry_strings.from.grapheme_width()); /* from */
                min_width.3 = min_width.3.max(
                    entry_strings.flag.grapheme_width()
                        + usize::from(entry_strings.highlight_self) * highlight_self_colwidth,
                ); /* flags */
                min_width.4 = min_width.4.max(
                    entry_strings.subject.grapheme_width()
                        + 1
                        + entry_strings.tags.grapheme_width(),
                ); /* tags + subject */
                self.rows.insert_thread(
                    threads.envelope_to_thread[&env_hash],
                    (threads.envelope_to_thread[&env_hash], env_hash),
                    smallvec::smallvec![env_hash],
                    entry_strings,
                );
                self.seen_cache.insert(env_hash, envelope.is_seen());
                idx += 1;
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

        self.data_columns.elasticities[0].set_rigid();
        self.data_columns.elasticities[1].set_rigid();
        self.data_columns.elasticities[2].set_grow(15, Some(35));
        self.data_columns.elasticities[3].set_rigid();
        self.data_columns.elasticities[4].set_rigid();
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

        // index column
        _ = self.data_columns.columns[0].resize_with_context(min_width.0, self.rows.len(), context);
        self.data_columns.segment_tree[0] = row_widths.0.into();
        // date column
        _ = self.data_columns.columns[1].resize_with_context(min_width.1, self.rows.len(), context);
        self.data_columns.segment_tree[1] = row_widths.1.into();
        // from column
        _ = self.data_columns.columns[2].resize_with_context(min_width.2, self.rows.len(), context);
        self.data_columns.segment_tree[2] = row_widths.2.into();
        // flags column
        _ = self.data_columns.columns[3].resize_with_context(min_width.3, self.rows.len(), context);
        self.data_columns.segment_tree[3] = row_widths.3.into();
        // subject column
        _ = self.data_columns.columns[4].resize_with_context(min_width.4, self.rows.len(), context);
        self.data_columns.segment_tree[4] = row_widths.4.into();

        self.rows_drawn = SegmentTree::from(
            std::iter::repeat(1)
                .take(self.rows.len())
                .collect::<SmallVec<_>>(),
        );
        debug_assert_eq!(self.rows_drawn.array.len(), self.rows.len());
        self.draw_rows(
            context,
            0,
            std::cmp::min(80, self.rows.len().saturating_sub(1)),
        );
        self.length = self.rows.len();
    }
}

impl ListingTrait for ThreadListing {
    fn coordinates(&self) -> (AccountHash, MailboxHash) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1)
    }

    fn next_entry(&mut self, context: &mut Context) {
        if self
            .get_env_under_cursor(self.new_cursor_pos.2 + 1)
            .is_some()
        {
            // [ref:TODO]: makes this less ugly.
            self.movement = Some(PageMovement::Down(1));
            self.perform_movement(None);
            self.force_draw = true;
            self.dirty = true;
            self.set_focus(Focus::Entry, context);
        }
    }

    fn prev_entry(&mut self, context: &mut Context) {
        if self.new_cursor_pos.2 == 0 {
            return;
        }
        if self
            .get_env_under_cursor(self.new_cursor_pos.2 - 1)
            .is_some()
        {
            // [ref:TODO]: makes this less ugly.
            self.movement = Some(PageMovement::Up(1));
            self.perform_movement(None);
            self.force_draw = true;
            self.dirty = true;
            self.set_focus(Focus::Entry, context);
        }
    }

    fn set_coordinates(&mut self, coordinates: (AccountHash, MailboxHash)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.focus = Focus::None;
        self.rows.clear();
        self.initialized = false;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term.clear();
        self.data_columns.clear();
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized
            || self.cursor_pos.1 != self.new_cursor_pos.1
            || self.cursor_pos.0 != self.new_cursor_pos.0
        {
            self.refresh_mailbox(context, false);
        }
        if self.length == 0 {
            grid.clear_area(area, self.color_cache.theme_default);
            grid.copy_area(
                self.data_columns.columns[0].grid(),
                area,
                self.data_columns.columns[0].area(),
            );
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = area.height();
        if rows == 0 {
            return;
        }

        self.perform_movement(Some(rows));

        if self.force_draw {
            grid.clear_area(area, self.color_cache.theme_default);
        }

        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let top_idx = page_no * rows;
        let end_idx = self.length.saturating_sub(1).min(top_idx + rows - 1);
        self.draw_rows(context, top_idx, end_idx);

        // If cursor position has changed, remove the highlight from the previous
        // position and apply it in the new one.
        if self.cursor_pos.2 != self.new_cursor_pos.2 && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for &(idx, highlight) in &[(old_cursor_pos.2, false), (self.new_cursor_pos.2, true)] {
                if idx >= self.length {
                    continue; //bounds check
                }
                let new_area = area.nth_row(idx % rows);
                self.data_columns
                    .draw(grid, idx, self.cursor_pos.2, grid.bounds_iter(new_area));
                if highlight {
                    let row_attr = row_attr!(self.color_cache, even: idx % 2 == 0, unseen: false, highlighted: true, selected: false);
                    grid.change_theme(new_area, row_attr);
                } else if let Some(row_attr) = self.rows.row_attr_cache.get(&idx) {
                    grid.change_theme(new_area, *row_attr);
                }
                context.dirty_areas.push_back(new_area);
            }
            if *account_settings!(context[self.cursor_pos.0].listing.relative_list_indices) {
                self.draw_relative_numbers(grid, area, top_idx);
                context.dirty_areas.push_back(area);
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

        if !self.force_draw {
            grid.clear_area(area, self.color_cache.theme_default);
        }

        // Page_no has changed, so draw new page
        _ = self.data_columns.recalc_widths(area.size(), top_idx);
        // copy table columns
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos.2, grid.bounds_iter(area));
        if *account_settings!(context[self.cursor_pos.0].listing.relative_list_indices) {
            self.draw_relative_numbers(grid, area, top_idx);
        }
        // apply each row colors separately
        for i in top_idx..(top_idx + area.height()) {
            if let Some(row_attr) = self.rows.row_attr_cache.get(&i) {
                grid.change_theme(area.nth_row(i % rows), *row_attr);
            }
        }

        // highlight cursor
        let row_attr = row_attr!(
            self.color_cache,
            even: self.cursor_pos.2 % 2 == 0,
            unseen: false,
            highlighted: true,
            selected: false
        );
        grid.change_theme(area.nth_row(self.cursor_pos.2 % rows), row_attr);

        // clear gap if available height is more than count of entries
        if top_idx + rows > self.length {
            grid.change_theme(
                area.skip_rows(self.length - top_idx),
                self.color_cache.theme_default,
            );
        }

        self.force_draw = false;
        context.dirty_areas.push_back(area);
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let Some(env_hash) = self.get_env_under_cursor(idx) else {
            return;
        };

        let envelope: EnvelopeRef = context.accounts[&self.cursor_pos.0]
            .collection
            .get_env(env_hash);

        let row_attr = row_attr!(
            self.color_cache,
            even: idx % 2 == 0,
            unseen: !envelope.is_seen(),
            highlighted: self.cursor_pos.2 == idx,
            selected: self.selection()[&env_hash],
        );

        let x = self.data_columns.widths[0]
            + self.data_columns.widths[1]
            + self.data_columns.widths[2]
            + 3 * 2;

        for c in grid.row_iter(area, 0..area.width(), 0) {
            grid[c]
                .set_fg(row_attr.fg)
                .set_bg(row_attr.bg)
                .set_attrs(row_attr.attrs);
        }

        grid.copy_area(
            self.data_columns.columns[3].grid(),
            area.skip_cols(x),
            self.data_columns.columns[3].area().nth_row(idx),
        );
        for c in grid.row_iter(area, x..area.width(), 0) {
            grid[c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
        }
    }

    fn filter(&mut self, filter_term: String, results: Vec<EnvelopeHash>, context: &Context) {
        if filter_term.is_empty() {
            return;
        }

        self.length = 0;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term = filter_term;
        self.rows.row_updates.clear();
        for v in self.selection_mut().values_mut() {
            *v = false;
        }

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
            self.new_cursor_pos.2 = self.cursor_pos.2.min(self.filtered_selection.len() - 1);
        } else {
            _ = self.data_columns.columns[0].resize_with_context(0, 0, context);
        }
        self.redraw_threads_list(
            context,
            Box::new(self.filtered_selection.clone().into_iter())
                as Box<dyn Iterator<Item = ThreadHash>>,
        );
    }

    fn view_area(&self) -> Option<Area> {
        self.view_area
    }

    fn unfocused(&self) -> bool {
        !matches!(self.focus, Focus::None)
    }

    fn modifier_active(&self) -> bool {
        self.modifier_active
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
                self.dirty = true;
                // If self.rows.row_updates is not empty and we exit a thread, the row_update
                // events will be performed but the list will not be drawn.
                // So force a draw in any case.
                self.force_draw = true;
            }
            Focus::Entry => {
                if let Some((thread_hash, env_hash)) = self
                    .get_env_under_cursor(self.new_cursor_pos.2)
                    .and_then(|env_hash| Some((*self.rows.env_to_thread.get(&env_hash)?, env_hash)))
                {
                    self.force_draw = true;
                    self.dirty = true;
                    self.kick_parent(
                        self.parent,
                        ListingMessage::OpenEntryUnderCursor {
                            thread_hash,
                            env_hash,
                            show_thread: false,
                            go_to_first_unread: false,
                        },
                        context,
                    );
                    self.cursor_pos.2 = self.new_cursor_pos.2;
                } else {
                    return;
                }
            }
            Focus::EntryFullscreen => {
                self.dirty = true;
            }
        }
        self.focus = new_value;
        self.kick_parent(
            self.parent,
            ListingMessage::FocusUpdate { new_value },
            context,
        );
    }

    fn focus(&self) -> Focus {
        self.focus
    }
}

impl std::fmt::Display for ThreadListing {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "mail")
    }
}

impl ThreadListing {
    pub fn new(
        parent: ComponentId,
        coordinates: (AccountHash, MailboxHash),
        context: &Context,
    ) -> Box<Self> {
        let color_cache = ColorCache::new(context, IndexStyle::Threaded);
        Box::new(Self {
            cursor_pos: (coordinates.0, MailboxHash::default(), 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (Default::default(), Default::default()),
            data_columns: DataColumns::new(color_cache.theme_default),
            color_cache,
            rows_drawn: SegmentTree::default(),
            rows: RowsState::default(),
            seen_cache: IndexMap::default(),
            filter_term: String::new(),
            search_job: None,
            select_job: None,
            filtered_selection: Vec::new(),
            filtered_order: HashMap::default(),
            dirty: true,
            force_draw: true,
            focus: Focus::None,
            initialized: false,
            movement: None,
            modifier_active: false,
            modifier_command: None,
            view_area: None,
            parent,
            id: ComponentId::default(),
        })
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
        let has_parent = thread_node.has_parent();
        let has_visible_parent = has_parent && !is_root;
        let show_subject = thread_node.show_subject();

        let mut subject = String::new();

        // Do not print any indentation if entry is a root but it has a parent that is
        // missing AND it has no siblings; therefore there's no point in
        // printing anything before the root's level in the thread tree. It
        // would just be empty space.
        if !(is_root && has_parent && !has_sibling) {
            for i in 0..indent {
                if indentations.len() > i && indentations[i] {
                    subject.push('│');
                } else if indentations.len() > i {
                    subject.push(' ');
                }
                if i > 0 {
                    subject.push(' ');
                }
            }
        }

        if indent > 0 && ((has_sibling || has_visible_parent) || is_root) {
            if has_sibling && has_visible_parent {
                subject.push('├');
            } else if has_sibling {
                subject.push('┬');
            } else if has_parent && is_root {
                subject.push('─');
            } else {
                subject.push('└');
            }
            subject.push('─');
            subject.push('>');
        }

        if show_subject {
            subject.push_str(&envelope.subject());
        }
        subject
    }

    fn get_env_under_cursor(&self, cursor: usize) -> Option<EnvelopeHash> {
        self.rows.entries.get(cursor).map(|v| (v.0).1)
    }

    fn make_entry_string(&self, e: &Envelope, context: &Context) -> EntryStrings {
        let mut tags = String::new();
        let mut colors = SmallVec::new();
        let account = &context.accounts[&self.cursor_pos.0];
        if account.backend_capabilities.supports_tags {
            let tags_lck = account.collection.tag_index.read().unwrap();
            for t in e.tags().iter() {
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
                tags.push(' ');
                tags.push_str(tags_lck.get(t).as_ref().unwrap());
                tags.push(' ');
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
            if !tags.is_empty() {
                tags.pop();
            }
        }
        let subject = e.subject().trim().to_string();
        EntryStrings {
            date: DateString(self.format_date(context, e.date())),
            subject: SubjectString(subject),
            flag: FlagString::new(
                e.flags(),
                self.selection().get(&e.hash()).cloned().unwrap_or(false),
                /* snoozed */ false,
                !e.is_seen(),
                e.has_attachments(),
                context,
                (self.cursor_pos.0, self.cursor_pos.1),
            ),
            from: FromString(Address::display_name_slice(e.from(), None)),
            tags: TagString(tags, colors),
            unseen: !e.is_seen(),
            highlight_self: false,
        }
    }

    fn draw_rows(&mut self, context: &Context, start: usize, end: usize) {
        if self.length == 0 {
            return;
        }
        debug_assert!(end >= start);
        if self.rows_drawn.get_max(start, end) == 0 {
            return;
        }
        for i in start..=end {
            self.rows_drawn.update(i, 0);
        }
        let min_width = (
            self.data_columns.columns[0].area().width(),
            self.data_columns.columns[1].area().width(),
            self.data_columns.columns[2].area().width(),
            self.data_columns.columns[3].area().width(),
            self.data_columns.columns[4].area().width(),
        );
        let columns = &mut self.data_columns.columns;
        let mut itoa_buffer = itoa::Buffer::new();
        for (idx, ((_thread_hash, env_hash), strings)) in self
            .rows
            .entries
            .iter()
            .enumerate()
            .skip(start)
            .take(end - start + 1)
        {
            if !context.accounts[&self.cursor_pos.0].contains_key(*env_hash) {
                continue;
            }
            let row_attr = row_attr!(
                self.color_cache,
                even: idx % 2 == 0,
                unseen: !self.seen_cache[env_hash],
                highlighted: false,
                selected: false,
            );
            self.rows.row_attr_cache.insert(idx, row_attr);
            {
                let mut area_col_0 = columns[0].area().nth_row(idx);
                if !*account_settings!(context[self.cursor_pos.0].listing.relative_list_indices) {
                    area_col_0 = area_col_0.skip_cols(columns[0].grid_mut().write_string(
                        itoa_buffer.format(idx),
                        row_attr.fg,
                        row_attr.bg,
                        row_attr.attrs,
                        area_col_0,
                        None,
                        None,
                    ));
                    for c in columns[0].grid().row_iter(area_col_0, 0..min_width.0, 0) {
                        columns[0].grid_mut()[c]
                            .set_bg(row_attr.bg)
                            .set_attrs(row_attr.attrs);
                    }
                }
            }
            {
                let mut area_col_1 = columns[1].area().nth_row(idx);
                area_col_1 = area_col_1.skip_cols(columns[1].grid_mut().write_string(
                    &strings.date,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area_col_1,
                    None,
                    None,
                ));
                for c in columns[1].grid().row_iter(area_col_1, 0..min_width.1, 0) {
                    columns[1].grid_mut()[c]
                        .set_bg(row_attr.bg)
                        .set_attrs(row_attr.attrs);
                }
            }
            {
                let area_col_2 = columns[2].area().nth_row(idx);
                let (skip_cols, _) = columns[2].grid_mut().write_string(
                    &strings.from,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area_col_2,
                    None,
                    None,
                );
                {
                    for text_formatter in crate::conf::text_format_regexps(context, "listing.from")
                    {
                        let t = columns[2].grid_mut().insert_tag(text_formatter.tag);
                        for (start, end) in text_formatter.regexp.find_iter(strings.from.as_str()) {
                            columns[2].grid_mut().set_tag(
                                t,
                                (start + skip_cols, idx),
                                (end + skip_cols, idx),
                            );
                        }
                    }
                }
                for c in columns[2]
                    .grid()
                    .row_iter(area_col_2, skip_cols..min_width.2, 0)
                {
                    columns[2].grid_mut()[c]
                        .set_bg(row_attr.bg)
                        .set_attrs(row_attr.attrs);
                }
            }
            {
                let mut area_col_3 = columns[3].area().nth_row(idx);
                area_col_3 = area_col_3.skip_cols(columns[3].grid_mut().write_string(
                    &strings.flag,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area_col_3,
                    None,
                    None,
                ));
                if strings.highlight_self {
                    let (x, _) = columns[3].grid_mut().write_string(
                        mailbox_settings!(
                            context[self.cursor_pos.0][&self.cursor_pos.1]
                                .listing
                                .highlight_self_flag
                        )
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or(super::DEFAULT_HIGHLIGHT_SELF_FLAG),
                        self.color_cache.highlight_self.fg,
                        row_attr.bg,
                        row_attr.attrs | Attr::FORCE_TEXT,
                        area_col_3,
                        None,
                        None,
                    );
                    for c in columns[3].grid().row_iter(area_col_3, 0..x, 0) {
                        columns[3].grid_mut()[c].set_keep_fg(true);
                    }
                    area_col_3 = area_col_3.skip_cols(x + 1);
                }
                for c in columns[3].grid().row_iter(area_col_3, 0..min_width.3, 0) {
                    columns[3].grid_mut()[c]
                        .set_bg(row_attr.bg)
                        .set_attrs(row_attr.attrs);
                }
            }
            {
                let mut area_col_4 = columns[4].area().nth_row(idx);
                area_col_4 = area_col_4.skip_cols(columns[4].grid_mut().write_string(
                    &strings.subject,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area_col_4,
                    None,
                    None,
                ));
                {
                    for text_formatter in
                        crate::conf::text_format_regexps(context, "listing.subject")
                    {
                        let t = columns[4].grid_mut().insert_tag(text_formatter.tag);
                        for (start, end) in
                            text_formatter.regexp.find_iter(strings.subject.as_str())
                        {
                            columns[4].grid_mut().set_tag(t, (start, idx), (end, idx));
                        }
                    }
                }
                area_col_4 = area_col_4.skip_cols(1);
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let color = color.unwrap_or(self.color_cache.tag_default.bg);
                    let (x, _) = columns[4].grid_mut().write_string(
                        t,
                        self.color_cache.tag_default.fg,
                        color,
                        self.color_cache.tag_default.attrs,
                        area_col_4.skip_cols(1),
                        None,
                        None,
                    );
                    for c in columns[4].grid().row_iter(area_col_4, 0..(x + 1), 0) {
                        columns[4].grid_mut()[c]
                            .set_bg(color)
                            .set_keep_fg(true)
                            .set_keep_bg(true)
                            .set_keep_attrs(true);
                    }
                    area_col_4 = area_col_4.skip_cols(x + 1);
                }
                for c in columns[4].grid().row_iter(area_col_4, 0..min_width.4, 0) {
                    columns[4].grid_mut()[c]
                        .set_ch(' ')
                        .set_bg(row_attr.bg)
                        .set_attrs(row_attr.attrs);
                }
            }
        }
    }

    fn update_line(&mut self, context: &Context, env_hash: EnvelopeHash) {
        let account = &context.accounts[&self.cursor_pos.0];

        if !account.contains_key(env_hash) {
            // The envelope has been renamed or removed, so wait for the appropriate
            // event to arrive
            return;
        }
        let envelope: EnvelopeRef = account.collection.get_env(env_hash);
        let thread_hash = self.rows.env_to_thread[&env_hash];
        let idx = self.rows.env_order[&env_hash];
        let row_attr = row_attr!(
            self.color_cache,
            even: idx % 2 == 0,
            unseen: !envelope.is_seen(),
            highlighted: false,
            selected: self.selection()[&env_hash]
        );
        self.seen_cache.insert(env_hash, envelope.is_seen());

        let should_highlight_self = mailbox_settings!(
            context[self.cursor_pos.0][&self.cursor_pos.1]
                .listing
                .highlight_self
        )
        .is_true();
        let mut entry_strings = self.make_entry_string(&envelope, context);
        entry_strings.highlight_self = should_highlight_self && {
            let my_address: Address = context.accounts[&self.cursor_pos.0]
                .settings
                .account
                .main_identity_address();
            envelope.recipient_any(&my_address) || envelope.sender_any(&my_address)
        };
        // [ref:FIXME]: generate new tree indentation for this new row subject
        // entry_strings.subject = SubjectString(Self::make_thread_entry(
        //     &envelope,
        //     indentation,
        //     thread_node_hash,
        //     &threads,
        //     &indentations,
        //     has_sibling,
        //     is_root,
        // ));
        drop(envelope);
        std::mem::swap(
            &mut self.rows.entries.get_mut(idx).unwrap().1.subject,
            &mut entry_strings.subject,
        );
        let columns = &mut self.data_columns.columns;
        for n in 0..=4 {
            let area = columns[n].area().nth_row(idx);
            columns[n].grid_mut().clear_area(area, row_attr);
        }
        self.rows_drawn.update(idx, 1);

        *self.rows.entries.get_mut(idx).unwrap() = ((thread_hash, env_hash), entry_strings);
    }

    fn select(
        &mut self,
        search_term: &str,
        results: Result<Vec<EnvelopeHash>>,
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
                    let env_thread_node_hash = account.collection.get_env(env_hash).thread();
                    if !threads.thread_nodes.contains_key(&env_thread_node_hash) {
                        continue;
                    }
                    let thread =
                        threads.find_group(threads.thread_nodes[&env_thread_node_hash].group);
                    if self.rows.all_threads.contains(&thread) {
                        self.selection_mut()
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
                context.replies.push_back(UIEvent::Notification {
                    title: Some("Could not perform search".into()),
                    source: None,
                    body: message.into(),
                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                });
            }
        }
    }

    fn draw_relative_numbers(&self, grid: &mut CellBuffer, area: Area, top_idx: usize) {
        let width = self.data_columns.columns[0].area().width();
        let area = area.take_cols(width);
        for i in 0..area.height() {
            if top_idx + i >= self.length {
                break;
            }
            let row_attr = if let Some(env_hash) = self.get_env_under_cursor(top_idx + i) {
                row_attr!(
                    self.color_cache,
                    even: (top_idx + i) % 2 == 0,
                    unseen: !self.seen_cache[&env_hash],
                    highlighted: self.cursor_pos.2 == (top_idx + i),
                    selected: self.selection()[&env_hash]
                )
            } else {
                row_attr!(self.color_cache, even: (top_idx + i) % 2 == 0, unseen: false, highlighted: true, selected: false)
            };

            grid.clear_area(area.nth_row(i), row_attr);
            grid.write_string(
                &if self.new_cursor_pos.2.saturating_sub(top_idx) == i {
                    self.new_cursor_pos.2.to_string()
                } else {
                    (i as isize - (self.new_cursor_pos.2 - top_idx) as isize)
                        .abs()
                        .to_string()
                },
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                area.nth_row(i),
                None,
                None,
            );
        }
    }

    fn perform_movement(&mut self, height: Option<usize>) {
        let rows = height.unwrap_or(1);
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
                PageMovement::Right(amount) => {
                    self.data_columns.x_offset += amount;
                    self.data_columns.x_offset = self.data_columns.x_offset.min(
                        self.data_columns
                            .widths
                            .iter()
                            .map(|w| w + 2)
                            .sum::<usize>()
                            .saturating_sub(2),
                    );
                }
                PageMovement::Left(amount) => {
                    self.data_columns.x_offset = self.data_columns.x_offset.saturating_sub(amount);
                }
                PageMovement::Home => {
                    self.new_cursor_pos.2 = 0;
                }
                PageMovement::End => {
                    self.new_cursor_pos.2 = self.length.saturating_sub(1);
                }
            }
        }
    }
}

impl Component for ThreadListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if matches!(self.focus, Focus::EntryFullscreen) {
            self.view_area = area.into();
            return;
        }

        if !self.is_dirty() {
            return;
        }

        if matches!(self.focus, Focus::None) {
            let mut area = area;
            if !self.filter_term.is_empty() {
                let (x, y) = grid.write_string(
                    &format!(
                        "{} results for `{}` (Press ESC to exit)",
                        self.filtered_selection.len(),
                        self.filter_term
                    ),
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    area,
                    None,
                    Some(0),
                );

                grid.clear_area(area.skip(x, y).nth_row(y), self.color_cache.theme_default);
                context.dirty_areas.push_back(area);

                area = area.skip_rows(y + 1);
            }

            let rows = area.height();

            if let Some(modifier) = self.modifier_command.take() {
                if let Some(mvm) = self.movement.as_ref() {
                    match mvm {
                        PageMovement::Up(amount) => {
                            for c in self.new_cursor_pos.2.saturating_sub(*amount)
                                ..=self.new_cursor_pos.2
                            {
                                if let Some(env_hash) = self.get_env_under_cursor(c) {
                                    self.rows.update_selection_with_env(
                                        env_hash,
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
                                for c in (0..self.new_cursor_pos.2.saturating_sub(*amount))
                                    .chain((self.new_cursor_pos.2 + 2)..self.length)
                                {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageUp(multiplier) => {
                            for c in self.new_cursor_pos.2.saturating_sub(rows * multiplier)
                                ..=self.new_cursor_pos.2
                            {
                                if let Some(env_hash) = self.get_env_under_cursor(c) {
                                    self.rows.update_selection_with_env(
                                        env_hash,
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
                            for c in self.new_cursor_pos.2
                                ..self.length.min(self.new_cursor_pos.2 + amount + 1)
                            {
                                if let Some(env_hash) = self.get_env_under_cursor(c) {
                                    self.rows.update_selection_with_env(
                                        env_hash,
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
                                for c in (0..self.new_cursor_pos.2).chain(
                                    self.length.min(self.new_cursor_pos.2 + amount) + 1
                                        ..self.length,
                                ) {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageDown(multiplier) => {
                            for c in self.new_cursor_pos.2
                                ..self.length.min(self.new_cursor_pos.2 + rows * multiplier)
                            {
                                if let Some(env_hash) = self.get_env_under_cursor(c) {
                                    self.rows.update_selection_with_env(
                                        env_hash,
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
                                for c in (0..self.new_cursor_pos.2).chain(
                                    self.length.min(self.new_cursor_pos.2 + rows * multiplier) + 1
                                        ..self.length,
                                ) {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::Right(_) | PageMovement::Left(_) => {}
                        PageMovement::Home => {
                            for c in 0..=self.new_cursor_pos.2 {
                                if let Some(env_hash) = self.get_env_under_cursor(c) {
                                    self.rows.update_selection_with_env(
                                        env_hash,
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
                                for c in (self.new_cursor_pos.2)..self.length {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::End => {
                            for c in self.new_cursor_pos.2..self.length {
                                if let Some(env_hash) = self.get_env_under_cursor(c) {
                                    self.rows.update_selection_with_env(
                                        env_hash,
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
                                for c in 0..self.new_cursor_pos.2 {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                    }
                }
                self.force_draw = true;
            }

            if !self.rows.row_updates.is_empty() {
                let page_no = (self.new_cursor_pos.2).wrapping_div(rows);
                let top_idx = page_no * rows;

                while let Some(env_hash) = self.rows.row_updates.pop() {
                    self.update_line(context, env_hash);
                    let row: usize = self.rows.env_order[&env_hash];
                    let envelope: EnvelopeRef = context.accounts[&self.new_cursor_pos.0]
                        .collection
                        .get_env(env_hash);
                    let row_attr = row_attr!(
                        self.color_cache,
                        even: row % 2 == 0,
                        unseen: !envelope.is_seen(),
                        highlighted: false,
                        selected: self.selection()[&env_hash]
                    );
                    self.rows.row_attr_cache.insert(row, row_attr);
                    self.force_draw |= row >= top_idx && row < top_idx + rows;
                }
                if self.force_draw {
                    /* Draw the entire list */
                    self.draw_list(grid, area, context);
                }
            } else {
                /* Draw the entire list */
                self.draw_list(grid, area, context);
            }
        } else {
            self.view_area = area.into();
            if self.length == 0 && self.dirty {
                grid.clear_area(area, self.color_cache.theme_default);
                context.dirty_areas.push_back(area);
            }
        }
        self.force_draw = false;
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.shortcuts(context);

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

        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.color_cache = ColorCache::new(context, IndexStyle::Threaded);
                self.set_dirty(true);
            }
            UIEvent::Input(ref k)
                if matches!(self.focus, Focus::None)
                    && (shortcut!(k == shortcuts[Shortcuts::LISTING]["open_entry"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"])) =>
            {
                self.set_focus(Focus::Entry, context);
                return true;
            }
            UIEvent::Input(ref k)
                if !matches!(self.focus, Focus::None)
                    && shortcut!(k == shortcuts[Shortcuts::LISTING]["exit_entry"]) =>
            {
                self.set_focus(Focus::None, context);
                return true;
            }
            UIEvent::Input(ref k)
                if !matches!(self.focus, Focus::Entry)
                    && shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"]) =>
            {
                self.set_focus(Focus::EntryFullscreen, context);
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
            UIEvent::MailboxUpdate((ref idxa, ref idxf))
                if (*idxa, *idxf) == (self.new_cursor_pos.0, self.new_cursor_pos.1) =>
            {
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
            }
            UIEvent::StartupCheck(ref f) if *f == self.new_cursor_pos.1 => {
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
            }
            UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                let account = &context.accounts[&self.new_cursor_pos.0];
                if !account.collection.contains_key(new_hash) {
                    return false;
                }
                self.rows.rename_env(*old_hash, *new_hash);
                self.seen_cache.shift_remove(old_hash);
                self.seen_cache
                    .insert(*new_hash, account.collection.get_env(*new_hash).is_seen());
                if let Some(&row) = self.rows.env_order.get(new_hash) {
                    (self.rows.entries[row].0).1 = *new_hash;
                }

                self.set_dirty(true);
            }
            UIEvent::EnvelopeRemove(ref env_hash, _) => {
                if self.rows.contains_env(*env_hash) {
                    self.refresh_mailbox(context, false);
                    self.seen_cache.shift_remove(env_hash);
                    self.set_dirty(true);
                }
            }
            UIEvent::EnvelopeUpdate(ref env_hash) => {
                let account = &context.accounts[&self.new_cursor_pos.0];
                if !account.collection.contains_key(env_hash) {
                    return false;
                }
                if self.rows.contains_env(*env_hash) {
                    self.rows.row_updates.push(*env_hash);
                    self.seen_cache
                        .insert(*env_hash, account.collection.get_env(*env_hash).is_seen());
                }

                self.set_dirty(true);
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Input(Key::Esc) if !self.unfocused() && !self.filter_term.is_empty() => {
                self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1));
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if !self.unfocused()
                    && shortcut!(key == shortcuts[Shortcuts::LISTING]["select_entry"]) =>
            {
                if let Some(env_hash) = self.get_env_under_cursor(self.new_cursor_pos.2) {
                    self.rows.update_selection_with_env(env_hash, |e| *e = !*e);
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if !self.unfocused()
                    && shortcut!(key == shortcuts[Shortcuts::LISTING]["select_motion"]) =>
            {
                if self.modifier_active && self.modifier_command.is_none() {
                    self.modifier_command = Some(Modifier::default());
                }
                return true;
            }
            UIEvent::Action(ref action) => match action {
                Action::SubSort(field, order) => {
                    self.subsort = (*field, *order);
                    self.set_dirty(true);
                    self.refresh_mailbox(context, false);
                    return true;
                }
                Action::Sort(field, order) => {
                    self.sort = (*field, *order);
                    self.set_dirty(true);
                    self.refresh_mailbox(context, false);
                    return true;
                }
                Action::Listing(Search(ref filter_term)) if !self.unfocused() => {
                    match context.accounts[&self.new_cursor_pos.0].search(
                        filter_term,
                        self.sort,
                        self.new_cursor_pos.1,
                    ) {
                        Ok(job) => {
                            let handle = context.accounts[&self.new_cursor_pos.0]
                                .main_loop_handler
                                .job_executor
                                .spawn(
                                    "search".into(),
                                    job,
                                    context.accounts[&self.new_cursor_pos.0].is_async(),
                                );
                            self.search_job = Some((filter_term.to_string(), handle));
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Could not perform search".into()),
                                body: err.to_string().into(),
                                kind: Some(crate::types::NotificationType::Error(err.kind)),
                                source: Some(err),
                            });
                        }
                    };
                    self.set_dirty(true);
                    return true;
                }
                Action::Listing(Select(ref search_term)) if !self.unfocused() => {
                    match context.accounts[&self.cursor_pos.0].search(
                        search_term,
                        self.sort,
                        self.cursor_pos.1,
                    ) {
                        Ok(job) => {
                            let mut handle = context.accounts[&self.cursor_pos.0]
                                .main_loop_handler
                                .job_executor
                                .spawn(
                                    "select-by-search".into(),
                                    job,
                                    context.accounts[&self.cursor_pos.0].is_async(),
                                );
                            if let Ok(Some(search_result)) = try_recv_timeout!(&mut handle.chan) {
                                self.select(search_term, search_result, context);
                            } else {
                                self.select_job = Some((search_term.to_string(), handle));
                            }
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Could not perform search".into()),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(crate::types::NotificationType::Error(err.kind)),
                            });
                        }
                    };
                    self.set_dirty(true);
                    return true;
                }
                _ => {}
            },
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
                        context.replies.push_back(UIEvent::Notification {
                            title: Some("Could not perform search".into()),
                            body: err.to_string().into(),
                            kind: Some(crate::types::NotificationType::Error(err.kind)),
                            source: Some(err),
                        });
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
            Focus::None | Focus::Entry => {
                self.dirty || self.force_draw || !self.rows.row_updates.is_empty()
            }
            Focus::EntryFullscreen => false,
        }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();

        map.insert(
            Shortcuts::LISTING,
            context.settings.shortcuts.listing.key_values(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
