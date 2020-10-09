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
use crate::jobs::{JobId, JoinHandle};
use std::cmp;
use std::iter::FromIterator;

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

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `MailView`.
#[derive(Debug)]
pub struct PlainListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (AccountHash, MailboxHash, usize),
    new_cursor_pos: (AccountHash, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    all_envelopes: HashSet<EnvelopeHash>,
    order: HashMap<EnvelopeHash, usize>,
    /// Cache current view.
    data_columns: DataColumns,

    search_job: Option<(String, JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>)>,
    filter_term: String,
    filtered_selection: Vec<EnvelopeHash>,
    filtered_order: HashMap<EnvelopeHash, usize>,
    selection: HashMap<EnvelopeHash, bool>,
    _selection: HashMap<ThreadHash, bool>,
    thread_node_hashes: HashMap<EnvelopeHash, ThreadNodeHash>,
    local_collection: Vec<EnvelopeHash>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: MailView,
    row_updates: SmallVec<[EnvelopeHash; 8]>,
    _row_updates: SmallVec<[ThreadHash; 8]>,
    color_cache: ColorCache,

    active_jobs: HashMap<JobId, JoinHandle<Result<()>>>,
    movement: Option<PageMovement>,
    id: ComponentId,
}

impl MailListingTrait for PlainListing {
    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]> {
        &mut self._row_updates
    }

    fn selection(&mut self) -> &mut HashMap<ThreadHash, bool> {
        &mut self._selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]> {
        SmallVec::new()
        /*
        let is_selection_empty = self.selection.values().cloned().any(std::convert::identity);
        if is_selection_empty {
            self.selection
                .iter()
                .filter(|(_, v)| **v)
                .map(|(k, _)| self.thread_node_hashes[k])
                .collect()
        } else {
            let mut ret = SmallVec::new();
            ret.push(self.get_thread_under_cursor(self.cursor_pos.2, context));
            ret
        }
        */
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
            even: crate::conf::value(context, "mail.listing.plain.even"),
            odd: crate::conf::value(context, "mail.listing.plain.odd"),
            even_unseen: crate::conf::value(context, "mail.listing.plain.even_unseen"),
            odd_unseen: crate::conf::value(context, "mail.listing.plain.odd_unseen"),
            even_highlighted: crate::conf::value(context, "mail.listing.plain.even_highlighted"),
            odd_highlighted: crate::conf::value(context, "mail.listing.plain.odd_highlighted"),
            even_selected: crate::conf::value(context, "mail.listing.plain.even_selected"),
            odd_selected: crate::conf::value(context, "mail.listing.plain.odd_selected"),
            attachment_flag: crate::conf::value(context, "mail.listing.attachment_flag"),
            thread_snooze_flag: crate::conf::value(context, "mail.listing.thread_snooze_flag"),
            tag_default: crate::conf::value(context, "mail.listing.tag_default"),
            ..self.color_cache
        };
        if !context.settings.terminal.use_color() {
            self.color_cache.highlighted.attrs |= Attr::REVERSE;
            self.color_cache.tag_default.attrs |= Attr::REVERSE;
            self.color_cache.even_highlighted.attrs |= Attr::REVERSE;
            self.color_cache.odd_highlighted.attrs |= Attr::REVERSE;
        }

        // Get mailbox as a reference.
        //
        match context.accounts[&self.cursor_pos.0].load(self.cursor_pos.1) {
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
                    context.accounts[&self.cursor_pos.0][&self.cursor_pos.1].status();
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
        self.thread_node_hashes = context.accounts[&self.cursor_pos.0]
            .collection
            .get_mailbox(self.cursor_pos.1)
            .iter()
            .map(|h| (*h, env_lck[h].thread()))
            .collect();
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
        for &env_hash in &self.local_collection {
            self.all_envelopes.insert(env_hash);
        }
        let items = Box::new(self.local_collection.clone().into_iter())
            as Box<dyn Iterator<Item = EnvelopeHash>>;

        self.redraw_list(context, items);
        drop(env_lck);

        if self.length > 0 {
            let env_hash = self.get_env_under_cursor(self.cursor_pos.2, context);
            let temp = (self.new_cursor_pos.0, self.new_cursor_pos.1, env_hash);
            if !force && old_cursor_pos == self.new_cursor_pos {
                self.view.update(temp, context);
            } else if self.unfocused {
                self.view = MailView::new(temp, None, None, context);
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
        let thread_nodes: &HashMap<ThreadNodeHash, ThreadNode> = &threads.thread_nodes();
        let env_hash_iter = Box::new(
            threads
                .threads_group_iter(roots)
                .filter_map(|(_, thread_node_hash, _)| {
                    let thread_node = &thread_nodes[&thread_node_hash];

                    thread_node.message()
                })
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
        self.unfocused = false;
        self.view = MailView::default();
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

        let account = &context.accounts[&self.cursor_pos.0];
        let envelope: EnvelopeRef = account.collection.get_env(i);

        let row_attr = row_attr!(
            self.color_cache,
            idx % 2 == 0,
            !envelope.is_seen(),
            self.cursor_pos.2 == idx,
            self.selection[&i]
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
                pos_dec(self.data_columns.columns[3].size(), (1, 1)),
            ),
        );
        for c in grid.row_iter(x..(x + self.data_columns.widths[3]), get_y(upper_left)) {
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
                self.data_columns.widths[4] = remainder
                    .saturating_sub(min_col_width)
                    .saturating_sub(self.data_columns.widths[3]);
                self.data_columns.widths[2] = min_col_width;
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
            for c in grid.row_iter(
                flag_x
                    ..std::cmp::min(
                        get_x(bottom_right),
                        flag_x + 2 + self.data_columns.widths[3],
                    ),
                get_y(upper_left) + r,
            ) {
                grid[c].set_bg(bg_color);
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

    fn filter(
        &mut self,
        filter_term: String,
        results: Result<SmallVec<[EnvelopeHash; 512]>>,
        context: &Context,
    ) {
        if filter_term.is_empty() {
            return;
        }

        self.order.clear();
        self.selection.clear();
        self.length = 0;
        self.filtered_selection.clear();
        self.filtered_order.clear();
        self.filter_term = filter_term;
        self.row_updates.clear();
        for v in self.selection.values_mut() {
            *v = false;
        }

        let account = &context.accounts[&self.cursor_pos.0];
        match results {
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
                        as Box<dyn Iterator<Item = EnvelopeHash>>,
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

impl fmt::Display for PlainListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl PlainListing {
    const DESCRIPTION: &'static str = "plain listing";
    pub fn new(coordinates: (AccountHash, MailboxHash)) -> Self {
        PlainListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            all_envelopes: HashSet::default(),
            local_collection: Vec::new(),
            thread_node_hashes: HashMap::default(),
            order: HashMap::default(),
            filter_term: String::new(),
            search_job: None,
            filtered_selection: Vec::new(),
            filtered_order: HashMap::default(),
            selection: HashMap::default(),
            _selection: HashMap::default(),
            row_updates: SmallVec::new(),
            _row_updates: SmallVec::new(),
            data_columns: DataColumns::default(),
            dirty: true,
            force_draw: true,
            unfocused: false,
            view: MailView::default(),
            color_cache: ColorCache::default(),
            active_jobs: HashMap::default(),

            movement: None,
            id: ComponentId::new_v4(),
        }
    }

    fn make_entry_string(&self, e: EnvelopeRef, context: &Context) -> EntryStrings {
        let mut tags = String::new();
        let mut colors = SmallVec::new();
        let backend_lck = context.accounts[&self.cursor_pos.0].backend.read().unwrap();
        if let Some(t) = backend_lck.tags() {
            let tags_lck = t.read().unwrap();
            for t in e.labels().iter() {
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
        let mut subject = e.subject().to_string();
        subject.truncate_at_boundary(150);
        EntryStrings {
            date: DateString(PlainListing::format_date(&e)),
            subject: SubjectString(subject),
            flag: FlagString(format!("{}", if e.has_attachments() { "📎" } else { "" },)),
            from: FromString(address_list!((e.from()) as comma_sep_list)),
            tags: TagString(tags, colors),
        }
    }

    fn redraw_list(&mut self, context: &Context, iter: Box<dyn Iterator<Item = EnvelopeHash>>) {
        let account = &context.accounts[&self.cursor_pos.0];
        let mailbox = &account[&self.cursor_pos.1];

        self.order.clear();
        self.selection.clear();
        self.length = 0;
        let mut rows = Vec::with_capacity(1024);
        let mut min_width = (0, 0, 0, 0, 0);

        for i in iter {
            if !context.accounts[&self.cursor_pos.0].contains_key(i) {
                debug!("key = {}", i);
                debug!(
                    "name = {} {}",
                    mailbox.name(),
                    context.accounts[&self.cursor_pos.0].name()
                );
                debug!("{:#?}", context.accounts);

                panic!();
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

            let entry_strings = self.make_entry_string(envelope, context);
            min_width.1 = cmp::max(min_width.1, entry_strings.date.grapheme_width()); /* date */
            min_width.2 = cmp::max(min_width.2, entry_strings.from.grapheme_width()); /* from */
            min_width.3 = cmp::max(min_width.3, entry_strings.flag.grapheme_width()); /* flags */
            min_width.4 = cmp::max(
                min_width.4,
                entry_strings.subject.grapheme_width() + 1 + entry_strings.tags.grapheme_width(),
            ); /* tags + subject */
            rows.push(entry_strings);

            self.order.insert(i, self.length);
            self.selection.insert(i, false);
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
        /* flags column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context(min_width.3, rows.len(), default_cell, context);
        /* subject column */
        self.data_columns.columns[4] =
            CellBuffer::new_with_context(min_width.4, rows.len(), default_cell, context);

        let iter = if self.filter_term.is_empty() {
            Box::new(self.local_collection.iter().cloned())
                as Box<dyn Iterator<Item = EnvelopeHash>>
        } else {
            Box::new(self.filtered_selection.iter().map(|h| *h))
                as Box<dyn Iterator<Item = EnvelopeHash>>
        };

        let columns = &mut self.data_columns.columns;
        for ((idx, i), strings) in iter.enumerate().zip(rows) {
            if !context.accounts[&self.cursor_pos.0].contains_key(i) {
                //debug!("key = {}", i);
                //debug!(
                //    "name = {} {}",
                //    mailbox.name(),
                //    context.accounts[&self.cursor_pos.0].name()
                //);
                //debug!("{:#?}", context.accounts);

                panic!();
            }

            let envelope: EnvelopeRef = context.accounts[&self.cursor_pos.0].collection.get_env(i);
            let row_attr = row_attr!(
                self.color_cache,
                idx % 2 == 0,
                !envelope.is_seen(),
                false,
                false
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
                columns[0][c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
            }
            let (x, _) = write_string_to_grid(
                &strings.date,
                &mut columns[1],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.1, idx)),
                None,
            );
            for c in columns[1].row_iter(x..min_width.1, idx) {
                columns[1][c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
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
                columns[2][c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
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
            for c in columns[3].row_iter(x..min_width.3, idx) {
                columns[3][c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
            }
            let (x, _) = write_string_to_grid(
                &strings.subject,
                &mut columns[4],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.4, idx)),
                None,
            );
            let x = {
                let mut x = x + 1;
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let color = color.unwrap_or(self.color_cache.tag_default.bg);
                    let (_x, _) = write_string_to_grid(
                        t,
                        &mut columns[4],
                        self.color_cache.tag_default.fg,
                        color,
                        self.color_cache.tag_default.attrs,
                        ((x + 1, idx), (min_width.4, idx)),
                        None,
                    );
                    for c in columns[4].row_iter(x..(x + 1), idx) {
                        columns[4][c].set_bg(color);
                    }
                    for c in columns[4].row_iter(_x..(_x + 1), idx) {
                        columns[4][c].set_bg(color).set_keep_bg(true);
                    }
                    for c in columns[4].row_iter((x + 1)..(_x + 1), idx) {
                        columns[4][c].set_keep_fg(true).set_keep_bg(true);
                    }
                    for c in columns[4].row_iter(x..(x + 1), idx) {
                        columns[4][c].set_keep_bg(true);
                    }
                    x = _x + 1;
                }
                x
            };
            for c in columns[4].row_iter(x..min_width.4, idx) {
                columns[4][c].set_bg(row_attr.bg).set_attrs(row_attr.attrs);
            }
            if context.accounts[&self.cursor_pos.0]
                .collection
                .get_env(i)
                .has_attachments()
            {
                columns[3][(0, idx)].set_fg(Color::Byte(103));
            }
        }
        if self.length == 0 && self.filter_term.is_empty() {
            let message: String = account[&self.cursor_pos.1].status();
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), self.length + 1, default_cell, context);
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
            _ => melib::datetime::timestamp_to_string(envelope.datetime(), None),
        }
    }

    fn perform_action(&mut self, context: &mut Context, env_hash: EnvelopeHash, a: &ListingAction) {
        let account = &mut context.accounts[&self.cursor_pos.0];
        match {
            match a {
                ListingAction::SetSeen => account.backend.write().unwrap().set_flags(
                    env_hash.into(),
                    self.cursor_pos.1,
                    smallvec::smallvec![(Ok(Flag::SEEN), true)],
                ),
                ListingAction::SetUnseen => account.backend.write().unwrap().set_flags(
                    env_hash.into(),
                    self.cursor_pos.1,
                    smallvec::smallvec![(Ok(Flag::SEEN), false)],
                ),
                ListingAction::Delete => {
                    /* do nothing */
                    Err(MeliError::new("Delete is unimplemented"))
                }
                _ => unreachable!(),
            }
        } {
            Err(e) => {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        e.to_string(),
                    )));
            }
            Ok(fut) => {
                let handle = account.job_executor.spawn_specialized(fut);
                self.active_jobs.insert(handle.job_id, handle);
            }
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
                    Attr::DEFAULT,
                    area,
                    Some(get_x(upper_left)),
                );
                clear_area(
                    grid,
                    ((x, y), set_y(bottom_right, y)),
                    self.color_cache.theme_default,
                );
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
                        && shortcut!(k == shortcuts[PlainListing::DESCRIPTION]["open_thread"]) =>
                {
                    let env_hash = self.get_env_under_cursor(self.cursor_pos.2, context);
                    let temp = (self.cursor_pos.0, self.cursor_pos.1, env_hash);
                    self.view = MailView::new(temp, None, None, context);
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
                        //    let threads = &account.collection.threads[&self.cursor_pos.1];
                        //    threads.vec_inner_sort_by(&mut self.filtered_selection, self.sort, &account.collection);
                        //} else {
                        //    self.refresh_mailbox(contex, falset);
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
                        let stack: SmallVec<[_; 8]> = SmallVec::from_iter(iter.into_iter());
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
                if !account.collection.contains_key(new_hash)
                    || !account
                        .collection
                        .get_mailbox(self.cursor_pos.1)
                        .contains(new_hash)
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

                if self.unfocused {
                    self.view
                        .process_event(&mut UIEvent::EnvelopeRename(*old_hash, *new_hash), context);
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

                self.row_updates.push(*env_hash);
                self.dirty = true;

                if self.unfocused {
                    self.view
                        .process_event(&mut UIEvent::EnvelopeUpdate(*env_hash), context);
                }
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
                self.set_dirty(true);
                self.refresh_mailbox(context, false);
                return true;
            }
            UIEvent::Action(Action::Listing(Search(ref filter_term))) if !self.unfocused => {
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
            UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                if self
                    .search_job
                    .as_ref()
                    .map(|(_, j)| j == job_id)
                    .unwrap_or(false) =>
            {
                let (filter_term, mut handle) = self.search_job.take().unwrap();
                let results = handle.chan.try_recv().unwrap().unwrap();
                self.filter(filter_term, results, context);
                self.set_dirty(true);
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
