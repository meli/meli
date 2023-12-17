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

use std::iter::FromIterator;

use melib::{Address, SortField, SortOrder, ThreadNode};

use super::{EntryStrings, *};
use crate::{components::PageMovement, jobs::JoinHandle};

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
/// `Envelope` content in a `MailView`.
#[derive(Debug)]
pub struct PlainListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (AccountHash, MailboxHash, usize),
    new_cursor_pos: (AccountHash, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    rows: RowsState<(ThreadHash, EnvelopeHash)>,
    /// Cache current view.
    data_columns: DataColumns<4>,

    #[allow(clippy::type_complexity)]
    search_job: Option<(String, JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>)>,
    #[allow(clippy::type_complexity)]
    select_job: Option<(String, JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>)>,
    filter_term: String,
    filtered_selection: Vec<EnvelopeHash>,
    filtered_order: HashMap<EnvelopeHash, usize>,
    local_collection: Vec<EnvelopeHash>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If view is visible or not.
    focus: Focus,
    color_cache: ColorCache,
    movement: Option<PageMovement>,
    modifier_active: bool,
    modifier_command: Option<Modifier>,
    view_area: Option<Area>,
    parent: ComponentId,
    id: ComponentId,
}

impl MailListingTrait for PlainListing {
    fn row_updates(&mut self) -> &mut SmallVec<[EnvelopeHash; 8]> {
        &mut self.rows.row_updates
    }

    fn selection(&mut self) -> &mut HashMap<EnvelopeHash, bool> {
        &mut self.rows.selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[EnvelopeHash; 8]> {
        let is_selection_empty: bool = !self
            .rows
            .selection
            .values()
            .cloned()
            .any(std::convert::identity);
        if is_selection_empty {
            return self
                .get_env_under_cursor(self.cursor_pos.2)
                .into_iter()
                .collect::<_>();
        }
        SmallVec::from_iter(
            self.rows
                .selection
                .iter()
                .filter(|(_, &v)| v)
                .map(|(k, _)| *k),
        )
    }

    /// Fill the `self.data_columns` `CellBuffers` with the contents of the
    /// account mailbox the user has chosen.
    fn refresh_mailbox(&mut self, context: &mut Context, force: bool) {
        self.set_dirty(true);
        self.force_draw = true;
        let old_cursor_pos = self.cursor_pos;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;

        self.color_cache = ColorCache::new(context, IndexStyle::Plain);

        // Get mailbox as a reference.
        //
        match context.accounts[&self.cursor_pos.0].load(self.cursor_pos.1) {
            Ok(()) => {}
            Err(_) => {
                self.length = 0;
                let message: String =
                    context.accounts[&self.cursor_pos.0][&self.cursor_pos.1].status();
                if self.data_columns.columns[0].resize_with_context(message.len(), 1, context) {
                    let area = self.data_columns.columns[0].area();
                    self.data_columns.columns[0].grid_mut().write_string(
                        message.as_str(),
                        self.color_cache.theme_default.fg,
                        self.color_cache.theme_default.bg,
                        self.color_cache.theme_default.attrs,
                        area,
                        None,
                    );
                }
                return;
            }
        }
        self.local_collection = context.accounts[&self.cursor_pos.0]
            .collection
            .get_mailbox(self.cursor_pos.1)
            .iter()
            .cloned()
            .collect();
        let env_lck = context.accounts[&self.cursor_pos.0]
            .collection
            .envelopes
            .read()
            .unwrap();
        let sort = self.sort;
        self.local_collection.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let ma = &env_lck[a];
                let mb = &env_lck[b];
                mb.date().cmp(&ma.date())
            }
            (SortField::Date, SortOrder::Asc) => {
                let ma = &env_lck[a];
                let mb = &env_lck[b];
                ma.date().cmp(&mb.date())
            }
            (SortField::Subject, SortOrder::Desc) => {
                let ma = &env_lck[a];
                let mb = &env_lck[b];
                ma.subject().cmp(&mb.subject())
            }
            (SortField::Subject, SortOrder::Asc) => {
                let ma = &env_lck[a];
                let mb = &env_lck[b];
                mb.subject().cmp(&ma.subject())
            }
        });
        let items = Box::new(self.local_collection.clone().into_iter())
            as Box<dyn Iterator<Item = EnvelopeHash>>;

        self.redraw_list(context, items);
        drop(env_lck);

        if let Some(env_hash) = self.get_env_under_cursor(self.cursor_pos.2) {
            if !force && old_cursor_pos == self.new_cursor_pos {
                self.kick_parent(self.parent, ListingMessage::UpdateView, context);
            } else if self.unfocused() {
                let thread_hash = self.rows.env_to_thread[&env_hash];
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
        let roots = items
            .filter_map(|r| threads.groups[&r].root().map(|r| r.root))
            .collect::<_>();
        let thread_nodes: &HashMap<ThreadNodeHash, ThreadNode> = threads.thread_nodes();
        let env_hash_iter = Box::new(
            threads
                .threads_iter(roots)
                .filter_map(|(_, thread_node_hash, _)| thread_nodes[&thread_node_hash].message())
                .collect::<SmallVec<[EnvelopeHash; 2048]>>()
                .into_iter(),
        ) as Box<dyn Iterator<Item = EnvelopeHash>>;
        self.redraw_list(context, env_hash_iter);
    }
}

impl ListingTrait for PlainListing {
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
        self.data_columns.clear();
    }

    fn next_entry(&mut self, context: &mut Context) {
        if self.get_env_under_cursor(self.cursor_pos.2 + 1).is_some() {
            // [ref:TODO]: makes this less ugly.
            self.movement = Some(PageMovement::Down(1));
            self.force_draw = true;
            self.dirty = true;
            self.cursor_pos.2 += 1;
            self.new_cursor_pos.2 += 1;
            self.set_focus(Focus::Entry, context);
        }
    }

    fn prev_entry(&mut self, context: &mut Context) {
        if self.cursor_pos.2 == 0 {
            return;
        }
        if self.get_env_under_cursor(self.cursor_pos.2 - 1).is_some() {
            // [ref:TODO]: makes this less ugly.
            self.movement = Some(PageMovement::Up(1));
            self.force_draw = true;
            self.dirty = true;
            self.cursor_pos.2 -= 1;
            self.new_cursor_pos.2 -= 1;
            self.set_focus(Focus::Entry, context);
        }
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let Some(i) = self.get_env_under_cursor(idx) else {
            // self.length == 0
            return;
        };

        let account = &context.accounts[&self.cursor_pos.0];
        let envelope: EnvelopeRef = account.collection.get_env(i);

        let row_attr = row_attr!(
            self.color_cache,
            even: idx % 2 == 0,
            unseen: !envelope.is_seen(),
            highlighted: self.cursor_pos.2 == idx,
            selected: self.rows.selection[&i]
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

        if self.force_draw {
            grid.clear_area(area, self.color_cache.theme_default);
        }

        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let top_idx = page_no * rows;

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
        if filter_term.is_empty() {
            return;
        }

        self.length = 0;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term = filter_term;
        self.rows.row_updates.clear();
        for v in self.rows.selection.values_mut() {
            *v = false;
        }

        let account = &context.accounts[&self.cursor_pos.0];
        for env_hash in results {
            if !account.collection.contains_key(&env_hash) {
                continue;
            }
            if self.filtered_order.contains_key(&env_hash) {
                continue;
            }
            if self.rows.contains_env(env_hash) {
                self.filtered_selection.push(env_hash);
                self.filtered_order
                    .insert(env_hash, self.filtered_selection.len() - 1);
            }
        }
        if !self.filtered_selection.is_empty() {
            self.new_cursor_pos.2 = self.cursor_pos.2.min(self.filtered_selection.len() - 1);
        } else {
            _ = self.data_columns.columns[0].resize_with_context(0, 0, context);
        }
        self.redraw_list(
            context,
            Box::new(self.filtered_selection.clone().into_iter())
                as Box<dyn Iterator<Item = EnvelopeHash>>,
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
                // If self.rows.row_updates is not empty and we exit a thread, the row_update
                // events will be performed but the list will not be drawn.
                // So force a draw in any case.
                self.force_draw = true;
            }
            Focus::Entry => {
                if let Some((thread_hash, env_hash)) = self
                    .get_env_under_cursor(self.cursor_pos.2)
                    .map(|env_hash| (self.rows.env_to_thread[&env_hash], env_hash))
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

impl std::fmt::Display for PlainListing {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "mail")
    }
}

impl PlainListing {
    pub fn new(parent: ComponentId, coordinates: (AccountHash, MailboxHash)) -> Box<Self> {
        Box::new(PlainListing {
            cursor_pos: (AccountHash::default(), MailboxHash::default(), 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            rows: RowsState::default(),
            local_collection: Vec::new(),
            filter_term: String::new(),
            search_job: None,
            select_job: None,
            filtered_selection: Vec::new(),
            filtered_order: HashMap::default(),
            data_columns: DataColumns::default(),
            dirty: true,
            force_draw: true,
            focus: Focus::None,
            color_cache: ColorCache::default(),
            movement: None,
            modifier_active: false,
            modifier_command: None,
            view_area: None,
            parent,
            id: ComponentId::default(),
        })
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
            flag: FlagString(format!(
                "{selected}{unseen}{attachments}{whitespace}",
                selected = if self.rows.selection.get(&e.hash()).cloned().unwrap_or(false) {
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
                unseen = if !e.is_seen() {
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
                attachments = if e.has_attachments() {
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
                whitespace = if self.rows.selection.get(&e.hash()).cloned().unwrap_or(false)
                    || !e.is_seen()
                    || e.has_attachments()
                {
                    " "
                } else {
                    ""
                },
            )),
            from: FromString(Address::display_name_slice(e.from())),
            tags: TagString(tags, colors),
        }
    }

    fn redraw_list(&mut self, context: &Context, iter: Box<dyn Iterator<Item = EnvelopeHash>>) {
        let account = &context.accounts[&self.cursor_pos.0];
        let mailbox = &account[&self.cursor_pos.1];
        let threads = account.collection.get_threads(self.cursor_pos.1);

        self.rows.clear();
        self.length = 0;
        let mut min_width = (0, 0, 0, 0, 0);
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

        for i in iter {
            if !context.accounts[&self.cursor_pos.0].contains_key(i) {
                log::debug!("key = {}", i);
                log::debug!(
                    "name = {} {}",
                    mailbox.name(),
                    context.accounts[&self.cursor_pos.0].name()
                );
                log::debug!("{:#?}", context.accounts);

                continue;
            }
            let envelope: EnvelopeRef = context.accounts[&self.cursor_pos.0].collection.get_env(i);
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
            let row_attr = row_attr!(
                self.color_cache,
                even: self.length % 2 == 0,
                unseen: !envelope.is_seen(),
                highlighted: false,
                selected: false
            );
            self.rows.row_attr_cache.insert(self.length, row_attr);

            let entry_strings = self.make_entry_string(&envelope, context);
            row_widths
                .0
                .push(digits_of_num!(self.length).try_into().unwrap_or(255));
            row_widths.1.push(
                entry_strings
                    .date
                    .grapheme_width()
                    .try_into()
                    .unwrap_or(255),
            );
            row_widths.2.push(
                entry_strings
                    .from
                    .grapheme_width()
                    .try_into()
                    .unwrap_or(255),
            );
            row_widths.3.push(
                (entry_strings.flag.grapheme_width()
                    + entry_strings.subject.grapheme_width()
                    + 1
                    + entry_strings.tags.grapheme_width())
                .try_into()
                .unwrap_or(255),
            );
            min_width.1 = min_width.1.max(entry_strings.date.grapheme_width()); /* date */
            min_width.2 = min_width.2.max(entry_strings.from.grapheme_width()); /* from */
            min_width.3 = min_width.3.max(
                entry_strings.flag.grapheme_width()
                    + entry_strings.subject.grapheme_width()
                    + 1
                    + entry_strings.tags.grapheme_width(),
            ); /* tags + subject */
            self.rows.insert_thread(
                threads.envelope_to_thread[&i],
                (threads.envelope_to_thread[&i], i),
                smallvec::smallvec![i],
                entry_strings,
            );

            self.length += 1;
        }

        min_width.0 = self.length.saturating_sub(1).to_string().len();

        self.data_columns.elasticities[0].set_rigid();
        self.data_columns.elasticities[1].set_rigid();
        self.data_columns.elasticities[2].set_grow(15, Some(35));
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
        _ = self.data_columns.columns[0].resize_with_context(min_width.0, self.rows.len(), context);
        /* date column */
        _ = self.data_columns.columns[1].resize_with_context(min_width.1, self.rows.len(), context);
        /* from column */
        _ = self.data_columns.columns[2].resize_with_context(min_width.2, self.rows.len(), context);
        /* subject column */
        _ = self.data_columns.columns[3].resize_with_context(min_width.3, self.rows.len(), context);
        self.data_columns.segment_tree[0] = row_widths.0.into();
        self.data_columns.segment_tree[1] = row_widths.1.into();
        self.data_columns.segment_tree[2] = row_widths.2.into();
        self.data_columns.segment_tree[3] = row_widths.3.into();

        let iter = if self.filter_term.is_empty() {
            Box::new(self.local_collection.iter().cloned())
                as Box<dyn Iterator<Item = EnvelopeHash>>
        } else {
            Box::new(self.filtered_selection.iter().cloned())
                as Box<dyn Iterator<Item = EnvelopeHash>>
        };

        let columns = &mut self.data_columns.columns;
        for ((idx, i), (_, strings)) in iter.enumerate().zip(self.rows.entries.iter()) {
            if !context.accounts[&self.cursor_pos.0].contains_key(i) {
                //log::debug!("key = {}", i);
                //log::debug!(
                //    "name = {} {}",
                //    mailbox.name(),
                //    context.accounts[&self.cursor_pos.0].name()
                //);
                //log::debug!("{:#?}", context.accounts);

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
            let (x, _) = {
                let area = columns[3].area().nth_row(idx);
                columns[3].grid_mut().write_string(
                    &strings.flag,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    area,
                    None,
                )
            };
            let x = {
                let area = columns[3].area().nth_row(idx).skip_cols(x);
                columns[3]
                    .grid_mut()
                    .write_string(
                        &strings.subject,
                        row_attr.fg,
                        row_attr.bg,
                        row_attr.attrs,
                        area,
                        None,
                    )
                    .0
                    + x
            };
            let mut x = x + 1;
            for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                let color = color.unwrap_or(self.color_cache.tag_default.bg);
                let _x = {
                    let area = columns[3].area().nth_row(idx).skip_cols(x + 1);
                    columns[3]
                        .grid_mut()
                        .write_string(
                            t,
                            self.color_cache.tag_default.fg,
                            color,
                            self.color_cache.tag_default.attrs,
                            area,
                            None,
                        )
                        .0
                        + x
                        + 1
                };
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, x..(x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_bg(color);
                }
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, _x..(_x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_bg(color).set_keep_bg(true);
                }
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, (x + 1)..(_x + 1), idx)
                } {
                    columns[3].grid_mut()[c]
                        .set_keep_fg(true)
                        .set_keep_bg(true)
                        .set_keep_attrs(true);
                }
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, x..(x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_keep_bg(true);
                }
                x = _x + 2;
            }
        }
        if self.length == 0 && self.filter_term.is_empty() {
            let message: String = account[&self.cursor_pos.1].status();
            if self.data_columns.columns[0].resize_with_context(message.len(), 1, context) {
                let area = self.data_columns.columns[0].area();
                self.data_columns.columns[0].grid_mut().write_string(
                    message.as_str(),
                    self.color_cache.theme_default.fg,
                    self.color_cache.theme_default.bg,
                    self.color_cache.theme_default.attrs,
                    area,
                    None,
                );
            }
        }
    }

    fn get_env_under_cursor(&self, cursor: usize) -> Option<EnvelopeHash> {
        if self.filter_term.is_empty() {
            self.local_collection.get(cursor).cloned()
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
        let envelope: EnvelopeRef = account.collection.get_env(env_hash);
        let thread_hash = self.rows.env_to_thread[&env_hash];
        let idx = self.rows.env_order[&env_hash];
        let row_attr = row_attr!(
            self.color_cache,
            even: idx % 2 == 0,
            unseen: !envelope.is_seen(),
            highlighted: false,
            selected: self.rows.selection[&env_hash]
        );
        self.rows.row_attr_cache.insert(idx, row_attr);

        let strings = self.make_entry_string(&envelope, context);
        drop(envelope);
        let columns = &mut self.data_columns.columns;
        {
            let area = columns[0].area().nth_row(idx);
            columns[0].grid_mut().clear_area(area, row_attr)
        };
        {
            let area = columns[1].area().nth_row(idx);
            columns[1].grid_mut().clear_area(area, row_attr);
        }
        {
            let area = columns[2].area().nth_row(idx);
            columns[2].grid_mut().clear_area(area, row_attr);
        }
        {
            let area = columns[3].area().nth_row(idx);
            columns[3].grid_mut().clear_area(area, row_attr);
        }

        let (x, _) = {
            let area = columns[0].area().nth_row(idx);
            columns[0].grid_mut().write_string(
                &idx.to_string(),
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                area,
                None,
            )
        };
        for c in {
            let area = columns[0].area();
            columns[0].grid_mut().row_iter(area, x..area.width(), idx)
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
            )
        };
        for c in {
            let area = columns[1].area();
            columns[1].grid_mut().row_iter(area, x..area.width(), idx)
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
            )
        };
        for c in {
            let area = columns[2].area();
            columns[2].grid_mut().row_iter(area, x..area.width(), idx)
        } {
            columns[2].grid_mut()[c]
                .set_bg(row_attr.bg)
                .set_attrs(row_attr.attrs);
        }
        let (x, _) = {
            let area = columns[3].area().nth_row(idx);
            columns[3].grid_mut().write_string(
                &strings.flag,
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                area,
                None,
            )
        };
        let (x, _) = {
            let area = columns[3].area().nth_row(idx).skip_cols(x);
            columns[3].grid_mut().write_string(
                &strings.subject,
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                area,
                None,
            )
        };
        let x = {
            let mut x = x + 1;
            for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                let color = color.unwrap_or(self.color_cache.tag_default.bg);
                let (_x, _) = {
                    let area = columns[3].area().nth_row(idx).skip_cols(x + 1);
                    columns[3].grid_mut().write_string(
                        t,
                        self.color_cache.tag_default.fg,
                        color,
                        self.color_cache.tag_default.attrs,
                        area,
                        None,
                    )
                };
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, x..(x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_bg(color);
                }
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, _x..(_x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_bg(color).set_keep_bg(true);
                }
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, (x + 1)..(_x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_keep_fg(true).set_keep_bg(true);
                }
                for c in {
                    let area = columns[3].area();
                    columns[3].grid_mut().row_iter(area, x..(x + 1), idx)
                } {
                    columns[3].grid_mut()[c].set_keep_bg(true);
                }
                x = _x + 2;
            }
            x
        };
        for c in {
            let area = columns[3].area();
            columns[3].grid().row_iter(area, x..area.width(), idx)
        } {
            columns[3].grid_mut()[c]
                .set_bg(row_attr.bg)
                .set_attrs(row_attr.attrs);
        }
        *self.rows.entries.get_mut(idx).unwrap() = ((thread_hash, env_hash), strings);
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
}

impl Component for PlainListing {
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
                            for c in self.cursor_pos.2.saturating_sub(*amount)..=self.cursor_pos.2 {
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
                                for c in (0..self.cursor_pos.2.saturating_sub(*amount))
                                    .chain((self.cursor_pos.2 + 2)..self.length)
                                {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageUp(multiplier) => {
                            for c in self.cursor_pos.2.saturating_sub(rows * multiplier)
                                ..=self.cursor_pos.2
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
                            for c in
                                self.cursor_pos.2..self.length.min(self.cursor_pos.2 + amount + 1)
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
                                for c in (0..self.cursor_pos.2).chain(
                                    self.length.min(self.cursor_pos.2 + amount) + 1..self.length,
                                ) {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::PageDown(multiplier) => {
                            for c in self.cursor_pos.2
                                ..self.length.min(self.cursor_pos.2 + rows * multiplier)
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
                                for c in (0..self.cursor_pos.2).chain(
                                    self.length.min(self.cursor_pos.2 + rows * multiplier) + 1
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
                            for c in 0..=self.cursor_pos.2 {
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
                                for c in (self.cursor_pos.2)..self.length {
                                    if let Some(env_hash) = self.get_env_under_cursor(c) {
                                        self.rows
                                            .update_selection_with_env(env_hash, |e| *e = false);
                                    }
                                }
                            }
                        }
                        PageMovement::End => {
                            for c in self.cursor_pos.2..self.length {
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
                                for c in 0..self.cursor_pos.2 {
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
                while let Some(env_hash) = self.rows.row_updates.pop() {
                    self.update_line(context, env_hash);
                    let row: usize = self.rows.env_order[&env_hash];
                    let envelope: EnvelopeRef = context.accounts[&self.cursor_pos.0]
                        .collection
                        .get_env(env_hash);
                    let row_attr = row_attr!(
                        self.color_cache,
                        even: row % 2 == 0,
                        unseen: !envelope.is_seen(),
                        highlighted: false,
                        selected: self.rows.selection[&env_hash]
                    );
                    self.rows.row_attr_cache.insert(row, row_attr);
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
                    if !matches!(self.focus, Focus::None)
                        && shortcut!(k == shortcuts[Shortcuts::LISTING]["exit_entry"]) =>
                {
                    self.set_focus(Focus::None, context);
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
                    } else if let Some(env_hash) = self.get_env_under_cursor(self.cursor_pos.2) {
                        self.rows.update_selection_with_env(env_hash, |e| *e = !*e);
                        self.set_dirty(true);
                    }
                    return true;
                }
                UIEvent::Action(ref action) => match action {
                    Action::SubSort(field, order) if !self.unfocused() => {
                        self.subsort = (*field, *order);
                        //if !self.filtered_selection.is_empty() {
                        //    let threads = &account.collection.threads[&self.cursor_pos.1];
                        //    threads.vec_inner_sort_by(&mut self.filtered_selection, self.sort,
                        // &account.collection);
                        //} else {
                        //    self.refresh_mailbox(contex, falset);
                        //}
                        return true;
                    }
                    Action::Sort(field, order) if !self.unfocused() => {
                        self.sort = (*field, *order);
                        return true;
                    }

                    _ => {}
                },
                _ => {}
            }
        }
        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.color_cache = ColorCache::new(context, IndexStyle::Plain);

                self.refresh_mailbox(context, true);
                self.set_dirty(true);
            }
            UIEvent::MailboxUpdate((ref idxa, ref idxf))
                if (*idxa, *idxf) == (self.new_cursor_pos.0, self.cursor_pos.1) =>
            {
                self.refresh_mailbox(context, true);
                self.set_dirty(true);
            }
            UIEvent::StartupCheck(ref f) if *f == self.cursor_pos.1 => {
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
            }
            UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                let account = &context.accounts[&self.cursor_pos.0];
                if !account.collection.contains_key(new_hash)
                    || !account
                        .collection
                        .get_mailbox(self.cursor_pos.1)
                        .contains(new_hash)
                {
                    return false;
                }

                self.rows.rename_env(*old_hash, *new_hash);
                for h in self.filtered_selection.iter_mut() {
                    if *h == *old_hash {
                        *h = *new_hash;
                        break;
                    }
                }

                self.set_dirty(true);
            }
            UIEvent::EnvelopeRemove(ref env_hash, _) => {
                if self.rows.contains_env(*env_hash) {
                    self.refresh_mailbox(context, false);
                    self.set_dirty(true);
                }
            }
            UIEvent::EnvelopeUpdate(ref env_hash) => {
                let account = &context.accounts[&self.cursor_pos.0];
                if !account.collection.contains_key(env_hash)
                    || !account
                        .collection
                        .get_mailbox(self.cursor_pos.1)
                        .contains(env_hash)
                {
                    return false;
                }

                self.rows.row_updates.push(*env_hash);
                self.set_dirty(true);
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
                self.rows.clear_selection();
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Esc) if !self.unfocused() && !self.filter_term.is_empty() => {
                self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1));
                self.set_dirty(true);
                self.refresh_mailbox(context, false);
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
        self.force_draw
            || match self.focus {
                Focus::None | Focus::Entry => self.dirty,
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
