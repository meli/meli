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

use std::{collections::BTreeMap, convert::TryInto, iter::FromIterator};

use indexmap::IndexSet;
use melib::{Address, SortField, SortOrder, TagHash, Threads};

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
    data_columns: DataColumns<5>,
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
    color_cache: ColorCache,

    movement: Option<PageMovement>,
    modifier_active: bool,
    modifier_command: Option<Modifier>,
    view_area: Option<Area>,
    parent: ComponentId,
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
                self.length = 0;
                let message: String =
                    context.accounts[&self.cursor_pos.0][&self.cursor_pos.1].status();
                if self.data_columns.columns[0].resize_with_context(message.len(), 1, context) {
                    let area_col_0 = self.data_columns.columns[0].area();
                    self.data_columns.columns[0].grid_mut().write_string(
                        message.as_str(),
                        self.color_cache.theme_default.fg,
                        self.color_cache.theme_default.bg,
                        self.color_cache.theme_default.attrs,
                        area_col_0,
                        None,
                        None,
                    );
                }
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
        drop(threads);

        self.redraw_threads_list(
            context,
            Box::new(roots.into_iter()) as Box<dyn Iterator<Item = ThreadHash>>,
        );

        if self
            .get_thread_under_cursor(self.cursor_pos.2)
            .and_then(|thread| {
                self.rows
                    .thread_to_env
                    .get(&thread)
                    .and_then(|e| Some((thread, e.first()?)))
            })
            .is_some()
        {
            if !force && old_cursor_pos == self.new_cursor_pos {
                self.kick_parent(self.parent, ListingMessage::UpdateView, context);
            } else if self.unfocused() {
                self.force_draw = true;
                self.dirty = true;
                self.set_focus(Focus::Entry, context);
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
        self.length = 0;
        // Use account settings only if no sortcmd has been used
        if !self.sortcmd {
            self.sort = context.accounts[&self.cursor_pos.0].settings.account.order
        }
        self.length = 0;
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

        let tags_lck = account.collection.tag_index.read().unwrap();

        let mut other_subjects = IndexSet::new();
        let mut tags = IndexSet::new();
        let mut from_address_list = Vec::new();
        let mut from_address_set: std::collections::HashSet<Vec<u8>> =
            std::collections::HashSet::new();
        let mut highlight_self: bool;
        let my_address: Address = context.accounts[&self.cursor_pos.0]
            .settings
            .account
            .main_identity_address();
        let should_highlight_self = mailbox_settings!(
            context[self.cursor_pos.0][&self.cursor_pos.1]
                .listing
                .highlight_self
        )
        .is_true();
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
                log::debug!("key = {}", root_env_hash);
                log::debug!(
                    "name = {} {}",
                    account[&self.cursor_pos.1].name(),
                    context.accounts[&self.cursor_pos.0].name()
                );
                log::debug!("{:#?}", context.accounts);

                continue;
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
            highlight_self = false;
            for (envelope, show_subject) in threads
                .thread_iter(thread)
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

                highlight_self |= should_highlight_self && envelope.recipient_any(&my_address);
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
                even: self.length % 2 == 0,
                unseen: threads.thread_ref(thread).unseen() > 0,
                highlighted: false,
                selected: false
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
                highlight_self,
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
            row_widths.3.push(
                entry_strings
                    .flag
                    .grapheme_width()
                    .try_into()
                    .unwrap_or(255),
            );
            row_widths.4.push(
                (entry_strings.subject.grapheme_width() + 1 + entry_strings.tags.grapheme_width())
                    .try_into()
                    .unwrap_or(255),
            );
            min_width.1 = min_width.1.max(entry_strings.date.grapheme_width()); /* date */
            min_width.2 = min_width.2.max(entry_strings.from.grapheme_width()); /* from */
            min_width.3 = min_width.3.max(
                entry_strings.flag.grapheme_width() + usize::from(entry_strings.highlight_self),
            );
            min_width.4 = min_width.4.max(
                entry_strings.subject.grapheme_width() + 1 + entry_strings.tags.grapheme_width(),
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

        min_width.0 = digits_of_num!(self.length.saturating_sub(1));

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

        /* index column */
        _ = self.data_columns.columns[0].resize_with_context(min_width.0, self.rows.len(), context);
        /* date column */
        _ = self.data_columns.columns[1].resize_with_context(min_width.1, self.rows.len(), context);
        /* from column */
        _ = self.data_columns.columns[2].resize_with_context(min_width.2, self.rows.len(), context);
        // flags column
        _ = self.data_columns.columns[3].resize_with_context(min_width.3, self.rows.len(), context);
        // subject column
        _ = self.data_columns.columns[4].resize_with_context(min_width.4, self.rows.len(), context);
        self.data_columns.segment_tree[0] = row_widths.0.into();
        self.data_columns.segment_tree[1] = row_widths.1.into();
        self.data_columns.segment_tree[2] = row_widths.2.into();
        self.data_columns.segment_tree[3] = row_widths.3.into();
        self.data_columns.segment_tree[4] = row_widths.4.into();

        self.rows_drawn = SegmentTree::from(
            std::iter::repeat(1)
                .take(self.rows.len())
                .collect::<SmallVec<_>>(),
        );
        debug_assert_eq!(self.rows_drawn.array.len(), self.rows.len());
        self.draw_rows(context, 0, 80.min(self.rows.len().saturating_sub(1)));
        if self.length == 0 && self.filter_term.is_empty() {
            let message: String = account[&self.cursor_pos.1].status();
            if self.data_columns.columns[0].resize_with_context(message.len(), 1, context) {
                let area_col_0 = self.data_columns.columns[0].area();
                self.data_columns.columns[0].grid_mut().write_string(
                    &message,
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    area_col_0,
                    None,
                    None,
                );
            }
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
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term.clear();
        self.rows.row_updates.clear();
    }

    fn next_entry(&mut self, context: &mut Context) {
        if self
            .get_thread_under_cursor(self.new_cursor_pos.2 + 1)
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
            .get_thread_under_cursor(self.new_cursor_pos.2 - 1)
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
            even: idx % 2 == 0,
            unseen: thread.unseen() > 0,
            highlighted: self.cursor_pos.2 == idx,
            selected: self.rows.is_thread_selected(thread_hash)
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

    /// Draw the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0
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

        /* If cursor position has changed, remove the highlight from the previous
         * position and apply it in the new one. */
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
                self.draw_relative_numbers(grid, area, top_idx, context);
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
        /* Page_no has changed, so draw new page */
        _ = self.data_columns.recalc_widths(area.size(), top_idx);
        /* copy table columns */
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos.2, grid.bounds_iter(area));
        if *account_settings!(context[self.cursor_pos.0].listing.relative_list_indices) {
            self.draw_relative_numbers(grid, area, top_idx, context);
        }
        /* apply each row colors separately */
        for i in top_idx..(top_idx + area.height()) {
            if let Some(row_attr) = self.rows.row_attr_cache.get(&i) {
                grid.change_theme(area.nth_row(i % rows), *row_attr);
            }
        }

        /* highlight cursor */
        let row_attr = row_attr!(
            self.color_cache,
            even: self.cursor_pos.2 % 2 == 0,
            unseen: false,
            highlighted: true,
            selected: false
        );
        grid.change_theme(area.nth_row(self.cursor_pos.2 % rows), row_attr);

        /* clear gap if available height is more than count of entries */
        if top_idx + rows > self.length {
            grid.change_theme(
                area.skip_rows(self.length - top_idx),
                self.color_cache.theme_default,
            );
        }

        self.force_draw = false;
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
                /* If self.rows.row_updates is not empty and we exit a thread, the row_update
                 * events will be performed but the list will not be drawn.
                 * So force a draw in any case.
                 */
                self.force_draw = true;
            }
            Focus::Entry => {
                if let Some((thread_hash, env_hash)) = self
                    .get_thread_under_cursor(self.new_cursor_pos.2)
                    .and_then(|thread| self.rows.thread_to_env.get(&thread).map(|e| (thread, e[0])))
                {
                    self.force_draw = true;
                    self.dirty = true;
                    self.kick_parent(
                        self.parent,
                        ListingMessage::OpenEntryUnderCursor {
                            thread_hash,
                            env_hash,
                            show_thread: true,
                            go_to_first_unread: true,
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

impl std::fmt::Display for CompactListing {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "mail")
    }
}

impl CompactListing {
    pub fn new(parent: ComponentId, coordinates: (AccountHash, MailboxHash)) -> Box<Self> {
        Box::new(Self {
            cursor_pos: (AccountHash::default(), MailboxHash::default(), 0),
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
            color_cache: ColorCache::default(),
            movement: None,
            modifier_active: false,
            modifier_command: None,
            view_area: None,
            parent,
            id: ComponentId::default(),
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn make_entry_string(
        &self,
        root_envelope: &Envelope,
        context: &Context,
        tags_lck: &BTreeMap<TagHash, String>,
        from: &[Address],
        threads: &Threads,
        other_subjects: &IndexSet<String>,
        tags: &IndexSet<TagHash>,
        highlight_self: bool,
        hash: ThreadHash,
    ) -> EntryStrings {
        let thread = threads.thread_ref(hash);
        let mut tags_string = String::new();
        let flags = root_envelope.flags();
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
        let subject = if *mailbox_settings!(
            context[self.cursor_pos.0][&self.cursor_pos.1]
                .listing
                .thread_subject_pack
        ) {
            other_subjects
                .into_iter()
                .fold(String::new(), |mut acc, s| {
                    if s.trim().is_empty() {
                        return acc;
                    }
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.push_str(s.trim());
                    acc
                })
        } else {
            root_envelope.subject().trim().to_string()
        };
        EntryStrings {
            date: DateString(self.format_date(context, thread.date())),
            subject: if thread.len() > 1 {
                SubjectString(format!("{} ({})", subject, thread.len()))
            } else {
                SubjectString(subject)
            },
            flag: FlagString::new(
                flags,
                self.rows
                    .selection
                    .get(&root_envelope.hash())
                    .cloned()
                    .unwrap_or(false),
                thread.snoozed(),
                thread.unseen() > 0,
                thread.has_attachments(),
                context,
                (self.cursor_pos.0, self.cursor_pos.1),
            ),
            from: FromString(Address::display_name_slice(from)),
            tags: TagString(tags_string, colors),
            unseen: thread.unseen() > 0,
            highlight_self,
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
            even: idx % 2 == 0,
            unseen: thread.unseen() > 0,
            highlighted: false,
            selected: self.rows.is_thread_selected(thread_hash)
        );
        self.rows.row_attr_cache.insert(idx, row_attr);

        let mut other_subjects = IndexSet::new();
        let mut tags = IndexSet::new();
        let mut from_address_list = Vec::new();
        let mut from_address_set: std::collections::HashSet<Vec<u8>> =
            std::collections::HashSet::new();
        let mut highlight_self: bool = false;
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
        for (envelope, show_subject) in threads
            .thread_iter(thread_hash)
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
            highlight_self |= should_highlight_self && envelope.recipient_any(&my_address);
            for addr in envelope.from().iter() {
                if from_address_set.contains(addr.address_spec_raw()) {
                    continue;
                }
                from_address_set.insert(addr.address_spec_raw().to_vec());
                from_address_list.push(addr.clone());
            }
        }

        let mut entry_strings = self.make_entry_string(
            &envelope,
            context,
            &tags_lck,
            &from_address_list,
            &threads,
            &other_subjects,
            &tags,
            highlight_self,
            thread_hash,
        );
        entry_strings.highlight_self = should_highlight_self && {
            let my_address: Address = context.accounts[&self.cursor_pos.0]
                .settings
                .account
                .main_identity_address();
            envelope.recipient_any(&my_address)
        };
        drop(envelope);
        let columns = &mut self.data_columns.columns;
        for n in 0..=4 {
            let area = columns[n].area().nth_row(idx);
            columns[n].grid_mut().clear_area(area, row_attr);
        }
        self.rows_drawn.update(idx, 1);

        *self.rows.entries.get_mut(idx).unwrap() = ((thread_hash, env_hash), entry_strings);
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
            self.data_columns.columns[3].area().width(),
        );

        let columns = &mut self.data_columns.columns;
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

                continue;
            }
            let row_attr = self.rows.row_attr_cache[&idx];
            let (x, _) = {
                let area = columns[0].area().nth_row(idx);
                columns[0].grid_mut().write_string(
                    &idx.to_string(),
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area,
                    None,
                    None,
                )
            };
            for c in {
                let area = columns[0].area();
                columns[0].grid_mut().row_iter(area, x..min_width.0, idx)
            } {
                columns[0].grid_mut()[c]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            let (x, _) = {
                let area = columns[1].area().nth_row(idx);
                columns[1].grid_mut().write_string(
                    &strings.date,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area,
                    None,
                    None,
                )
            };
            for c in {
                let area = columns[1].area();
                columns[1].grid_mut().row_iter(area, x..min_width.1, idx)
            } {
                columns[1].grid_mut()[c]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs);
            }
            let (x, _) = {
                let area = columns[2].area().nth_row(idx);
                columns[2].grid_mut().write_string(
                    &strings.from,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area,
                    None,
                    None,
                )
            };
            for c in {
                let area = columns[2].area();
                columns[2].grid_mut().row_iter(area, x..min_width.2, idx)
            } {
                columns[2].grid_mut()[c]
                    .set_bg(row_attr.bg)
                    .set_attrs(row_attr.attrs)
                    .set_ch(' ');
            }
            #[cfg(feature = "regexp")]
            {
                for text_formatter in crate::conf::text_format_regexps(context, "listing.from") {
                    let t = columns[2].grid_mut().insert_tag(text_formatter.tag);
                    for (start, end) in text_formatter.regexp.find_iter(strings.from.as_str()) {
                        columns[2].grid_mut().set_tag(t, (start, idx), (end, idx));
                    }
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
                #[cfg(feature = "regexp")]
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
                        area_col_4,
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
        if self.length == 0 && self.filter_term.is_empty() {
            let account = &context.accounts[&self.cursor_pos.0];
            let message: String = account[&self.cursor_pos.1].status();
            if self.data_columns.columns[0].resize_with_context(message.len(), 1, context) {
                let area_col_0 = self.data_columns.columns[0].area();
                self.data_columns.columns[0].grid_mut().write_string(
                    message.as_str(),
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    area_col_0,
                    None,
                    None,
                );
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
                context.replies.push_back(UIEvent::Notification {
                    title: Some("Could not perform search".into()),
                    source: None,
                    body: message.into(),
                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                });
            }
        }
    }

    fn draw_relative_numbers(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        top_idx: usize,
        context: &Context,
    ) {
        let width = self.data_columns.columns[0].area().width();
        let area = area.take_cols(width);
        let account = &context.accounts[&self.cursor_pos.0];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        for i in 0..area.height() {
            let idx = top_idx + i;
            if idx >= self.length {
                break;
            }
            let row_attr = if let Some(thread_hash) = self.get_thread_under_cursor(idx) {
                let thread = threads.thread_ref(thread_hash);
                row_attr!(
                    self.color_cache,
                    even: idx % 2 == 0,
                    unseen: thread.unseen() > 0,
                    highlighted: self.new_cursor_pos.2 == idx,
                    selected: self.rows.is_thread_selected(thread_hash)
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
                    self.new_cursor_pos.2 = self.length - 1;
                }
            }
        }
    }
}

impl Component for CompactListing {
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
                                for c in (0..self.new_cursor_pos.2.saturating_sub(*amount))
                                    .chain((self.new_cursor_pos.2 + 2)..self.length)
                                {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageUp(multiplier) => {
                            for c in self.new_cursor_pos.2.saturating_sub(rows * multiplier)
                                ..=self.new_cursor_pos.2
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
                            for c in self.new_cursor_pos.2
                                ..self.length.min(self.new_cursor_pos.2 + amount + 1)
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
                                for c in (0..self.new_cursor_pos.2).chain(
                                    self.length.min(self.new_cursor_pos.2 + amount) + 1
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
                            for c in self.new_cursor_pos.2
                                ..self
                                    .length
                                    .min(self.new_cursor_pos.2 + rows * multiplier + 1)
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
                                for c in (0..self.new_cursor_pos.2).chain(
                                    self.length.min(self.new_cursor_pos.2 + rows * multiplier) + 1
                                        ..self.length,
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
                            for c in 0..=self.new_cursor_pos.2 {
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
                                for c in (self.new_cursor_pos.2)..self.length {
                                    if let Some(thread) = self.get_thread_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_thread(thread, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::End => {
                            for c in self.new_cursor_pos.2..self.length {
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
                                for c in 0..self.new_cursor_pos.2 {
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
            self.view_area = area.into();
            if self.length == 0 && self.dirty {
                grid.clear_area(area, self.color_cache.theme_default);
                context.dirty_areas.push_back(area);
            }
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.shortcuts(context);

        match (&event, self.focus) {
            (UIEvent::VisibilityChange(true), _) => {
                self.force_draw = true;
                self.set_dirty(true);
                return true;
            }
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

        if self.length > 0 {
            match *event {
                UIEvent::Input(ref k)
                    if matches!(self.focus, Focus::None)
                        && (shortcut!(k == shortcuts[Shortcuts::LISTING]["open_entry"])
                            || shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"])) =>
                {
                    self.set_focus(Focus::Entry, context);

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
                            self.sort = (*field, *order);
                            self.sortcmd = true;
                            if !self.filtered_selection.is_empty() {
                                // [ref:FIXME]: perform sort
                                self.set_dirty(true);
                            } else {
                                self.refresh_mailbox(context, false);
                            }
                            return true;
                        }
                        Action::SubSort(field, order) if !self.unfocused() => {
                            self.subsort = (*field, *order);
                            // [ref:FIXME]: perform subsort.
                            return true;
                        }
                        Action::Listing(ToggleThreadSnooze) if !self.unfocused() => {
                            // [ref:FIXME]: Re-implement toggle thread snooze
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
            UIEvent::EnvelopeRename(_, ref new_hash) => {
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
            }
            UIEvent::EnvelopeRemove(_, ref thread_hash) => {
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
            UIEvent::Action(Action::Listing(Search(ref filter_term))) if !self.unfocused() => {
                match context.accounts[&self.cursor_pos.0].search(
                    filter_term,
                    self.sort,
                    self.cursor_pos.1,
                ) {
                    Ok(job) => {
                        let handle = context.accounts[&self.cursor_pos.0]
                            .main_loop_handler
                            .job_executor
                            .spawn_specialized("search".into(), job);
                        self.search_job = Some((filter_term.to_string(), handle));
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
            UIEvent::Action(Action::Listing(Select(ref search_term))) if !self.unfocused() => {
                match context.accounts[&self.cursor_pos.0].search(
                    search_term,
                    self.sort,
                    self.cursor_pos.1,
                ) {
                    Ok(job) => {
                        let mut handle = context.accounts[&self.cursor_pos.0]
                            .main_loop_handler
                            .job_executor
                            .spawn_specialized("select_by_search".into(), job);
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
                            source: None,
                            body: err.to_string().into(),
                            kind: Some(crate::types::NotificationType::Error(err.kind)),
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
