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

use super::EntryStrings;
use super::*;
use crate::components::utilities::PageMovement;
use std::cmp;
use std::convert::TryInto;
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
/// `ThreadView`.
#[derive(Debug)]
pub struct CompactListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (usize, MailboxHash, usize),
    new_cursor_pos: (usize, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    all_threads: fnv::FnvHashSet<ThreadHash>,
    order: FnvHashMap<ThreadHash, usize>,
    /// Cache current view.
    data_columns: DataColumns,

    filter_term: String,
    filtered_selection: Vec<ThreadHash>,
    filtered_order: FnvHashMap<ThreadHash, usize>,
    selection: FnvHashMap<ThreadHash, bool>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: ThreadView,
    row_updates: SmallVec<[ThreadHash; 8]>,
    color_cache: ColorCache,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl MailListingTrait for CompactListing {
    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]> {
        &mut self.row_updates
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]> {
        let is_selection_empty = self.selection.values().cloned().any(std::convert::identity);
        let i = [self.get_thread_under_cursor(self.cursor_pos.2)];
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
        SmallVec::from_iter(iter.into_iter())
    }

    /// Fill the `self.data_columns` `CellBuffers` with the contents of the account mailbox the user has
    /// chosen.
    fn refresh_mailbox(&mut self, context: &mut Context, force: bool) {
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

        self.color_cache = ColorCache {
            unseen: crate::conf::value(context, "mail.listing.compact.unseen"),
            highlighted: crate::conf::value(context, "mail.listing.compact.highlighted"),
            even: crate::conf::value(context, "mail.listing.compact.even"),
            odd: crate::conf::value(context, "mail.listing.compact.odd"),
            selected: crate::conf::value(context, "mail.listing.compact.selected"),
            attachment_flag: crate::conf::value(context, "mail.listing.attachment_flag"),
            thread_snooze_flag: crate::conf::value(context, "mail.listing.thread_snooze_flag"),
            theme_default: crate::conf::value(context, "theme_default"),
            ..self.color_cache
        };
        if !context.settings.terminal.use_color() {
            self.color_cache.highlighted.attrs |= Attr::Reverse;
        }

        // Get mailbox as a reference.
        //
        match context.accounts[self.cursor_pos.0].load(self.cursor_pos.1) {
            Ok(()) => {}
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
                    ((0, 0), (MAX_COLS - 1, 0)),
                    None,
                );
                return;
            }
        }

        let threads = &context.accounts[self.cursor_pos.0].collection.threads[&self.cursor_pos.1];
        self.all_threads.clear();
        let mut roots = threads.roots();
        threads.group_inner_sort_by(
            &mut roots,
            self.sort,
            &context.accounts[self.cursor_pos.0].collection.envelopes,
        );

        self.redraw_list(
            context,
            Box::new(roots.into_iter()) as Box<dyn Iterator<Item = ThreadHash>>,
        );

        if !force && old_cursor_pos == self.new_cursor_pos {
            self.view.update(context);
        } else if self.unfocused {
            let thread = self.get_thread_under_cursor(self.cursor_pos.2);

            self.view = ThreadView::new(self.new_cursor_pos, thread, None, context);
        }
    }
}

impl ListingTrait for CompactListing {
    fn coordinates(&self) -> (usize, MailboxHash) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1)
    }

    fn set_coordinates(&mut self, coordinates: (usize, MailboxHash)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.unfocused = false;
        self.view = ThreadView::default();
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term.clear();
        self.row_updates.clear();
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        if self.length == 0 {
            return;
        }
        let thread_hash = self.get_thread_under_cursor(idx);

        let account = &context.accounts[self.cursor_pos.0];
        let threads = &account.collection.threads[&self.cursor_pos.1];
        let thread = threads.thread_ref(thread_hash);

        let fg_color = if thread.unseen() > 0 {
            self.color_cache.unseen.fg
        } else if self.cursor_pos.2 == idx {
            self.color_cache.highlighted.fg
        } else if idx % 2 == 0 {
            self.color_cache.even.fg
        } else {
            self.color_cache.odd.fg
        };
        let bg_color = if self.cursor_pos.2 == idx {
            self.color_cache.highlighted.bg
        } else if self.selection[&thread_hash] {
            self.color_cache.selected.bg
        } else if thread.unseen() > 0 {
            self.color_cache.unseen.bg
        } else if idx % 2 == 0 {
            self.color_cache.even.bg
        } else {
            self.color_cache.odd.bg
        };
        let attrs = if self.cursor_pos.2 == idx {
            self.color_cache.highlighted.attrs
        } else if self.selection[&thread_hash] {
            self.color_cache.selected.attrs
        } else if thread.unseen() > 0 {
            self.color_cache.unseen.attrs
        } else if idx % 2 == 0 {
            self.color_cache.even.attrs
        } else {
            self.color_cache.odd.attrs
        };

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
            grid[c].set_fg(fg_color).set_bg(bg_color).set_attrs(attrs);
        }

        copy_area(
            grid,
            &self.data_columns.columns[3],
            (set_x(upper_left, x), bottom_right),
            (
                (0, idx),
                pos_dec(self.data_columns.columns[3].size(), (1, 1)),
            ),
        );
        for c in grid.row_iter(x..(self.data_columns.widths[3] + x), get_y(upper_left)) {
            grid[c].set_bg(bg_color).set_attrs(attrs);
        }
        return;
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
        if self.data_columns.widths.iter().fold(0, |acc, &w| acc + w) > width {
            let diff = self.data_columns.widths.iter().fold(0, |acc, &w| acc + w) - width;
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
                let thread_hash = self.get_thread_under_cursor(r + top_idx);

                let c = &self.data_columns.columns[0][(0, r + top_idx)];
                if self.selection[&thread_hash] {
                    (c.fg(), self.color_cache.selected.bg)
                } else {
                    (c.fg(), c.bg())
                }
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

        let account = &context.accounts[self.cursor_pos.0];
        match account.search(&self.filter_term, self.sort, self.cursor_pos.1) {
            Ok(results) => {
                let threads = &account.collection.threads[&self.cursor_pos.1];
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
                    if self.filtered_order.contains_key(&thread) {
                        continue;
                    }
                    if self.all_threads.contains(&thread) {
                        self.filtered_selection.push(thread);
                        self.filtered_order
                            .insert(thread, self.filtered_selection.len() - 1);
                    }
                }
                if !self.filtered_selection.is_empty() {
                    threads.group_inner_sort_by(
                        &mut self.filtered_selection,
                        self.sort,
                        &context.accounts[self.cursor_pos.0].collection.envelopes,
                    );
                    self.new_cursor_pos.2 =
                        std::cmp::min(self.filtered_selection.len() - 1, self.cursor_pos.2);
                } else {
                    let default_cell = {
                        let mut ret = Cell::with_char(' ');
                        ret.set_fg(self.color_cache.theme_default.fg)
                            .set_bg(self.color_cache.theme_default.bg)
                            .set_attrs(self.color_cache.theme_default.attrs);
                        ret
                    };
                    self.data_columns.columns[0] =
                        CellBuffer::new_with_context(0, 0, default_cell, context);
                }
                self.redraw_list(
                    context,
                    Box::new(self.filtered_selection.clone().into_iter())
                        as Box<dyn Iterator<Item = ThreadHash>>,
                );
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
                let default_cell = {
                    let mut ret = Cell::with_char(' ');
                    ret.set_fg(self.color_cache.theme_default.fg)
                        .set_bg(self.color_cache.theme_default.bg)
                        .set_attrs(self.color_cache.theme_default.attrs);
                    ret
                };
                self.data_columns.columns[0] =
                    CellBuffer::new_with_context(message.len(), 1, default_cell, context);
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

    fn set_movement(&mut self, mvm: PageMovement) {
        self.movement = Some(mvm);
        self.set_dirty(true);
    }
}

impl fmt::Display for CompactListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl CompactListing {
    const DESCRIPTION: &'static str = "compact listing";
    pub fn new(coordinates: (usize, MailboxHash)) -> Self {
        CompactListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            all_threads: fnv::FnvHashSet::default(),
            order: FnvHashMap::default(),
            filter_term: String::new(),
            filtered_selection: Vec::new(),
            filtered_order: FnvHashMap::default(),
            selection: FnvHashMap::default(),
            row_updates: SmallVec::new(),
            data_columns: DataColumns::default(),
            dirty: true,
            force_draw: true,
            unfocused: false,
            view: ThreadView::default(),
            color_cache: ColorCache::default(),
            movement: None,
            id: ComponentId::new_v4(),
        }
    }
    fn make_entry_string(
        &self,
        e: &Envelope,
        context: &Context,
        threads: &Threads,
        hash: ThreadHash,
    ) -> EntryStrings {
        let thread = threads.thread_ref(hash);
        let mailbox = &context.accounts[self.cursor_pos.0][&self.cursor_pos.1].conf;
        let mut tags = String::new();
        let mut colors: SmallVec<[_; 8]> = SmallVec::new();
        let backend_lck = context.accounts[self.cursor_pos.0].backend.read().unwrap();
        if let Some(t) = backend_lck.tags() {
            let tags_lck = t.read().unwrap();
            for t in e.labels().iter() {
                if mailbox
                    .conf_override
                    .tags
                    .as_ref()
                    .map(|s| s.ignore_tags.contains(t))
                    .unwrap_or(false)
                {
                    continue;
                }
                tags.push(' ');
                tags.push_str(tags_lck.get(t).as_ref().unwrap());
                tags.push(' ');
                if let Some(&c) = mailbox
                    .conf_override
                    .tags
                    .as_ref()
                    .map(|s| s.colors.get(t))
                    .unwrap_or(None)
                {
                    colors.push(c);
                } else {
                    colors.push(Color::Byte(8));
                }
            }
            if !tags.is_empty() {
                tags.pop();
            }
        }
        let mut subject = e.subject().to_string();
        subject.truncate_at_boundary(150);
        if thread.len() > 1 {
            EntryStrings {
                date: DateString(ConversationsListing::format_date(context, thread.date())),
                subject: SubjectString(format!("{} ({})", subject, thread.len(),)),
                flag: FlagString(format!(
                    "{}{}",
                    if thread.has_attachments() { "📎" } else { "" },
                    if thread.snoozed() { "💤" } else { "" }
                )),
                from: FromString(address_list!((e.from()) as comma_sep_list)),
                tags: TagString(tags, colors),
            }
        } else {
            EntryStrings {
                date: DateString(ConversationsListing::format_date(context, thread.date())),
                subject: SubjectString(subject),
                flag: FlagString(format!(
                    "{}{}",
                    if thread.has_attachments() { "📎" } else { "" },
                    if thread.snoozed() { "💤" } else { "" }
                )),
                from: FromString(address_list!((e.from()) as comma_sep_list)),
                tags: TagString(tags, colors),
            }
        }
    }

    fn redraw_list(&mut self, context: &Context, items: Box<dyn Iterator<Item = ThreadHash>>) {
        let account = &context.accounts[self.cursor_pos.0];

        let threads = &account.collection.threads[&self.cursor_pos.1];
        self.order.clear();
        self.selection.clear();
        self.length = 0;
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

        for thread in items {
            let thread_node = &threads.thread_nodes()[&threads.thread_ref(thread).root()];
            let root_env_hash = thread_node.message().unwrap_or_else(|| {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[&iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[&iter_ptr].children()[0];
                }
                threads.thread_nodes()[&iter_ptr].message().unwrap()
            });
            if !context.accounts[self.cursor_pos.0].contains_key(root_env_hash) {
                debug!("key = {}", root_env_hash);
                debug!(
                    "name = {} {}",
                    account[&self.cursor_pos.1].name(),
                    context.accounts[self.cursor_pos.0].name()
                );
                debug!("{:#?}", context.accounts);

                panic!();
            }
            let root_envelope: EnvelopeRef = context.accounts[self.cursor_pos.0]
                .collection
                .get_env(root_env_hash);
            use crate::cache::QueryTrait;
            if let Some(filter_query) =
                mailbox_settings!(context[self.cursor_pos.0][&self.cursor_pos.1].listing)
                    .filter
                    .as_ref()
            {
                if !root_envelope.is_match(filter_query) {
                    continue;
                }
            }

            let entry_strings = self.make_entry_string(&root_envelope, context, threads, thread);
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
                (entry_strings.subject.grapheme_width() + 1 + entry_strings.tags.grapheme_width())
                    .try_into()
                    .unwrap_or(255),
            );
            min_width.1 = cmp::max(min_width.1, entry_strings.date.grapheme_width()); /* date */
            min_width.2 = cmp::max(min_width.2, entry_strings.from.grapheme_width()); /* from */
            min_width.3 = cmp::max(min_width.3, entry_strings.flag.grapheme_width()); /* flags */
            min_width.4 = cmp::max(
                min_width.4,
                entry_strings.subject.grapheme_width() + 1 + entry_strings.tags.grapheme_width(),
            ); /* subject */
            rows.push(((self.length, (thread, root_env_hash)), entry_strings));
            self.all_threads.insert(thread);

            self.order.insert(thread, self.length);
            self.selection.insert(thread, false);
            self.length += 1;
        }

        min_width.0 = self.length.saturating_sub(1).to_string().len();

        let default_cell = {
            let mut ret = Cell::with_char(' ');
            ret.set_fg(self.color_cache.theme_default.fg)
                .set_bg(self.color_cache.theme_default.bg)
                .set_attrs(self.color_cache.theme_default.attrs);
            ret
        };
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

        for ((idx, (thread, root_env_hash)), strings) in rows {
            if !context.accounts[self.cursor_pos.0].contains_key(root_env_hash) {
                //debug!("key = {}", root_env_hash);
                //debug!(
                //    "name = {} {}",
                //    account[&self.cursor_pos.1].name(),
                //    context.accounts[self.cursor_pos.0].name()
                //);
                //debug!("{:#?}", context.accounts);

                panic!();
            }
            let thread = threads.thread_ref(thread);
            let row_attr = if thread.unseen() > 0 {
                self.color_cache.unseen
            } else if idx % 2 == 0 {
                self.color_cache.even
            } else {
                self.color_cache.odd
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
            let x = {
                let mut x = x + 1;
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let (_x, _) = write_string_to_grid(
                        t,
                        &mut self.data_columns.columns[4],
                        Color::White,
                        color,
                        Attr::Bold,
                        ((x + 1, idx), (min_width.4, idx)),
                        None,
                    );
                    self.data_columns.columns[4][(x, idx)].set_bg(color);
                    if _x < min_width.4 {
                        self.data_columns.columns[4][(_x, idx)].set_bg(color);
                        self.data_columns.columns[4][(_x, idx)].set_keep_bg(true);
                    }
                    for x in (x + 1).._x {
                        self.data_columns.columns[4][(x, idx)].set_keep_fg(true);
                        self.data_columns.columns[4][(x, idx)].set_keep_bg(true);
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
            match (thread.snoozed(), thread.has_attachments()) {
                (true, true) => {
                    self.data_columns.columns[3][(0, idx)]
                        .set_fg(self.color_cache.attachment_flag.fg);
                    self.data_columns.columns[3][(2, idx)]
                        .set_fg(self.color_cache.thread_snooze_flag.fg);
                }
                (true, false) => {
                    self.data_columns.columns[3][(0, idx)]
                        .set_fg(self.color_cache.thread_snooze_flag.fg);
                }
                (false, true) => {
                    self.data_columns.columns[3][(0, idx)]
                        .set_fg(self.color_cache.attachment_flag.fg);
                }
                (false, false) => {}
            }
        }
        if self.length == 0 && self.filter_term.is_empty() {
            let message = format!("{} is empty", account[&self.cursor_pos.1].name());
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), self.length + 1, default_cell, context);
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                self.color_cache.theme_default.fg,
                self.color_cache.theme_default.bg,
                self.color_cache.theme_default.attrs,
                ((0, 0), (MAX_COLS - 1, 0)),
                None,
            );
        }
    }

    fn get_thread_under_cursor(&self, cursor: usize) -> ThreadHash {
        if self.filter_term.is_empty() {
            *self
                .order
                .iter()
                .find(|(_, &r)| r == cursor)
                .unwrap_or_else(|| {
                    debug!("self.order empty ? cursor={} {:#?}", cursor, &self.order);
                    panic!();
                })
                .0
        } else {
            self.filtered_selection[cursor]
        }
    }

    fn update_line(&mut self, context: &Context, thread_hash: ThreadHash) {
        let account = &context.accounts[self.cursor_pos.0];
        let threads = &account.collection.threads[&self.cursor_pos.1];
        let thread = threads.thread_ref(thread_hash);
        let thread_node_hash = threads.thread_group_iter(thread_hash).next().unwrap().1;
        if let Some(env_hash) = threads.thread_nodes()[&thread_node_hash].message() {
            if !account.contains_key(env_hash) {
                /* The envelope has been renamed or removed, so wait for the appropriate event to
                 * arrive */
                return;
            }
            let idx = self.order[&thread_hash];
            let (fg_color, bg_color) = if thread.unseen() > 0 {
                (self.color_cache.unseen.fg, self.color_cache.unseen.bg)
            } else if idx % 2 == 0 {
                (self.color_cache.even.fg, self.color_cache.even.bg)
            } else {
                (self.color_cache.odd.fg, self.color_cache.odd.bg)
            };
            let envelope: EnvelopeRef = account.collection.get_env(env_hash);
            let strings = self.make_entry_string(&envelope, context, threads, thread_hash);
            drop(envelope);
            let columns = &mut self.data_columns.columns;
            let min_width = (
                columns[0].size().0,
                columns[1].size().0,
                columns[2].size().0,
                columns[3].size().0,
                columns[4].size().0,
            );
            let (x, _) = write_string_to_grid(
                &idx.to_string(),
                &mut columns[0],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.0, idx)),
                None,
            );
            for c in columns[0].row_iter(x..min_width.0, idx) {
                columns[0][c].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.date,
                &mut columns[1],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.1.saturating_sub(1), idx)),
                None,
            );
            for c in columns[1].row_iter(x..min_width.1, idx) {
                columns[1][c].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.from,
                &mut columns[2],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.2, idx)),
                None,
            );
            for c in columns[2].row_iter(x..min_width.2, idx) {
                columns[2][c].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.flag,
                &mut columns[3],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.3, idx)),
                None,
            );
            for c in columns[3].row_iter(x..min_width.3, idx) {
                columns[3][c].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &strings.subject,
                &mut columns[4],
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (min_width.4, idx)),
                None,
            );
            let x = {
                let mut x = x + 1;
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let (_x, _) = write_string_to_grid(
                        t,
                        &mut columns[4],
                        Color::White,
                        color,
                        Attr::Bold,
                        ((x + 1, idx), (min_width.4, idx)),
                        None,
                    );
                    for c in columns[4].row_iter(x..(x + 1), idx) {
                        columns[4][c].set_bg(color);
                    }
                    for c in columns[4].row_iter(_x..(_x + 1), idx) {
                        columns[4][c].set_bg(color);
                        columns[4][c].set_keep_bg(true);
                    }
                    for c in columns[4].row_iter((x + 1)..(_x + 1), idx) {
                        columns[4][c].set_keep_fg(true);
                        columns[4][c].set_keep_bg(true);
                    }
                    for c in columns[4].row_iter(x..(x + 1), idx) {
                        columns[4][c].set_keep_bg(true);
                    }
                    x = _x + 1;
                }
                x
            };
            for c in columns[4].row_iter(x..min_width.4, idx) {
                columns[4][c].set_ch(' ');
                columns[4][c].set_bg(bg_color);
            }
            match (thread.snoozed(), thread.has_attachments()) {
                (true, true) => {
                    columns[3][(0, idx)].set_fg(self.color_cache.attachment_flag.fg);
                    columns[3][(2, idx)].set_fg(self.color_cache.thread_snooze_flag.fg);
                }
                (true, false) => {
                    columns[3][(0, idx)].set_fg(self.color_cache.thread_snooze_flag.fg);
                }
                (false, true) => {
                    columns[3][(0, idx)].set_fg(self.color_cache.attachment_flag.fg);
                }
                (false, false) => {}
            }
        }
    }
}

impl Component for CompactListing {
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

            if !self.row_updates.is_empty() {
                let (upper_left, bottom_right) = area;
                while let Some(row) = self.row_updates.pop() {
                    self.update_line(context, row);
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
                clear_area(grid, area, self.color_cache.theme_default);
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
                        && shortcut!(
                            k == shortcuts[CompactListing::DESCRIPTION]["open_thread"]
                        ) =>
                {
                    let thread = self.get_thread_under_cursor(self.cursor_pos.2);
                    self.view = ThreadView::new(self.cursor_pos, thread, None, context);
                    self.unfocused = true;
                    self.dirty = true;
                    return true;
                }
                UIEvent::Input(ref k)
                    if self.unfocused
                        && shortcut!(
                            k == shortcuts[CompactListing::DESCRIPTION]["exit_thread"]
                        ) =>
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
                            key == shortcuts[CompactListing::DESCRIPTION]["select_entry"]
                        ) =>
                {
                    let thread_hash = self.get_thread_under_cursor(self.cursor_pos.2);
                    self.selection.entry(thread_hash).and_modify(|e| *e = !*e);
                }
                UIEvent::Action(ref action) => {
                    match action {
                        Action::Sort(field, order) if !self.unfocused => {
                            debug!("Sort {:?} , {:?}", field, order);
                            self.sort = (*field, *order);
                            if !self.filtered_selection.is_empty() {
                                // FIXME: perform sort
                                self.dirty = true;
                            } else {
                                self.refresh_mailbox(context, false);
                            }
                            return true;
                        }
                        Action::SubSort(field, order) if !self.unfocused => {
                            debug!("SubSort {:?} , {:?}", field, order);
                            self.subsort = (*field, *order);
                            // FIXME: perform subsort.
                            return true;
                        }
                        Action::ToggleThreadSnooze if !self.unfocused => {
                            let thread = self.get_thread_under_cursor(self.cursor_pos.2);
                            let account = &mut context.accounts[self.cursor_pos.0];
                            account
                                .collection
                                .threads
                                .entry(self.cursor_pos.1)
                                .and_modify(|threads| {
                                    let is_snoozed = threads.thread_ref(thread).snoozed();
                                    threads.thread_ref_mut(thread).set_snoozed(!is_snoozed);
                                });
                            self.row_updates.push(thread);
                            self.refresh_mailbox(context, false);
                            return true;
                        }

                        _ => {}
                    }
                }
                _ => {}
            }
        }
        match *event {
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
                let threads = &account.collection.threads[&self.cursor_pos.1];
                if !account.collection.contains_key(&new_hash) {
                    return false;
                }
                let new_env_thread_node_hash = account.collection.get_env(*new_hash).thread();
                if !threads.thread_nodes.contains_key(&new_env_thread_node_hash) {
                    return false;
                }
                let thread: ThreadHash =
                    threads.find_group(threads.thread_nodes()[&new_env_thread_node_hash].group);
                if self.order.contains_key(&thread) {
                    self.row_updates.push(thread);
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
            UIEvent::Input(Key::Esc)
                if !self.unfocused
                    && self.selection.values().cloned().any(std::convert::identity) =>
            {
                for v in self.selection.values_mut() {
                    *v = false;
                }
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Esc) if !self.unfocused && !self.filter_term.is_empty() => {
                self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1));
                self.refresh_mailbox(context, false);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Action(Action::Listing(Search(ref filter_term))) if !self.unfocused => {
                self.filter(filter_term, context);
                self.dirty = true;
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
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if self.unfocused {
            self.view.set_dirty(value);
        }
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.unfocused {
            self.view.get_shortcuts(context)
        } else {
            ShortcutMaps::default()
        };

        let config_map = context.settings.shortcuts.compact_listing.key_values();
        map.insert(CompactListing::DESCRIPTION, config_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
