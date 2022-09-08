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
use crate::components::PageMovement;
use crate::jobs::JoinHandle;
use indexmap::IndexSet;
use std::iter::FromIterator;

macro_rules! row_attr {
    ($field:ident, $color_cache:expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
        ThemeAttribute {
            fg: if $highlighted {
                $color_cache.highlighted.fg
            } else if $selected {
                $color_cache.selected.fg
            } else if $unseen {
                $color_cache.unseen.fg
            } else {
                $color_cache.$field.fg
            },
            bg: if $highlighted {
                $color_cache.highlighted.bg
            } else if $selected {
                $color_cache.selected.bg
            } else if $unseen {
                $color_cache.unseen.bg
            } else {
                $color_cache.$field.bg
            },
            attrs: if $highlighted {
                $color_cache.highlighted.attrs
            } else if $selected {
                $color_cache.selected.attrs
            } else if $unseen {
                $color_cache.unseen.attrs
            } else {
                $color_cache.$field.attrs
            },
        }
    }};
    ($color_cache:expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
        ThemeAttribute {
            fg: if $highlighted {
                $color_cache.highlighted.fg
            } else if $selected {
                $color_cache.selected.fg
            } else if $unseen {
                $color_cache.unseen.fg
            } else {
                $color_cache.theme_default.fg
            },
            bg: if $highlighted {
                $color_cache.highlighted.bg
            } else if $selected {
                $color_cache.selected.bg
            } else if $unseen {
                $color_cache.unseen.bg
            } else {
                $color_cache.theme_default.bg
            },
            attrs: if $highlighted {
                $color_cache.highlighted.attrs
            } else if $selected {
                $color_cache.selected.attrs
            } else if $unseen {
                $color_cache.unseen.attrs
            } else {
                $color_cache.theme_default.attrs
            },
        }
    }};
}

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `ThreadView`.
#[derive(Debug)]
pub struct ConversationsListing {
    /// (x, y, z): x is accounts, y is mailboxes, z is index inside a mailbox.
    cursor_pos: (AccountHash, MailboxHash, usize),
    new_cursor_pos: (AccountHash, MailboxHash, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    all_threads: HashSet<ThreadHash>,
    order: HashMap<ThreadHash, usize>,
    #[allow(clippy::type_complexity)]
    rows: std::result::Result<Vec<((usize, (ThreadHash, EnvelopeHash)), EntryStrings)>, String>,

    #[allow(clippy::type_complexity)]
    search_job: Option<(String, JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>)>,
    filter_term: String,
    filtered_selection: Vec<ThreadHash>,
    filtered_order: HashMap<ThreadHash, usize>,
    selection: HashMap<ThreadHash, bool>,
    /// If we must redraw on next redraw event
    dirty: bool,
    force_draw: bool,
    /// If `self.view` exists or not.
    focus: Focus,
    view: ThreadView,
    row_updates: SmallVec<[ThreadHash; 8]>,
    color_cache: ColorCache,

    movement: Option<PageMovement>,
    modifier_active: bool,
    modifier_command: Option<Modifier>,
    id: ComponentId,
}

impl MailListingTrait for ConversationsListing {
    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]> {
        &mut self.row_updates
    }

    fn selection(&mut self) -> &mut HashMap<ThreadHash, bool> {
        &mut self.selection
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
        SmallVec::from_iter(iter)
    }

    fn refresh_mailbox(&mut self, context: &mut Context, force: bool) {
        self.dirty = true;
        let old_mailbox_hash = self.cursor_pos.1;
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
            theme_default: crate::conf::value(context, "mail.listing.conversations"),
            subject: crate::conf::value(context, "mail.listing.conversations.subject"),
            from: crate::conf::value(context, "mail.listing.conversations.from"),
            date: crate::conf::value(context, "mail.listing.conversations.date"),
            selected: crate::conf::value(context, "mail.listing.conversations.selected"),
            unseen: crate::conf::value(context, "mail.listing.conversations.unseen"),
            highlighted: crate::conf::value(context, "mail.listing.conversations.highlighted"),
            attachment_flag: crate::conf::value(context, "mail.listing.attachment_flag"),
            thread_snooze_flag: crate::conf::value(context, "mail.listing.thread_snooze_flag"),
            tag_default: crate::conf::value(context, "mail.listing.tag_default"),
            ..self.color_cache
        };

        if !context.settings.terminal.use_color() {
            self.color_cache.highlighted.attrs |= Attr::REVERSE;
            self.color_cache.tag_default.attrs |= Attr::REVERSE;
        }
        // Get mailbox as a reference.
        //
        match context.accounts[&self.cursor_pos.0].load(self.cursor_pos.1) {
            Ok(()) => {}
            Err(_) => {
                let message: String =
                    context.accounts[&self.cursor_pos.0][&self.cursor_pos.1].status();
                self.rows = Err(message);
                return;
            }
        }

        let threads = context.accounts[&self.cursor_pos.0]
            .collection
            .get_threads(self.cursor_pos.1);
        self.all_threads.clear();
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

        if !force && old_cursor_pos == self.new_cursor_pos && old_mailbox_hash == self.cursor_pos.1
        {
            self.view.update(context);
        } else if self.unfocused() {
            let thread_group = self.get_thread_under_cursor(self.cursor_pos.2);

            self.view = ThreadView::new(self.new_cursor_pos, thread_group, None, context);
        }
    }

    fn redraw_threads_list(
        &mut self,
        context: &Context,
        items: Box<dyn Iterator<Item = ThreadHash>>,
    ) {
        let account = &context.accounts[&self.cursor_pos.0];

        let threads = account.collection.get_threads(self.cursor_pos.1);
        self.order.clear();
        self.selection.clear();
        self.length = 0;
        if self.rows.is_err() {
            self.rows = Ok(vec![]);
        } else {
            self.rows.as_mut().unwrap().clear();
        }
        let mut max_entry_columns = 0;

        let mut other_subjects = IndexSet::new();
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
            other_subjects.clear();
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

                for addr in envelope.from().iter() {
                    if from_address_set.contains(addr.address_spec_raw()) {
                        continue;
                    }
                    from_address_set.insert(addr.address_spec_raw().to_vec());
                    from_address_list.push(addr.clone());
                }
            }
            let root_envelope: &EnvelopeRef = &context.accounts[&self.cursor_pos.0]
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

            let strings = self.make_entry_string(
                root_envelope,
                context,
                &from_address_list,
                &threads,
                &other_subjects,
                thread,
            );
            max_entry_columns = std::cmp::max(
                max_entry_columns,
                strings.flag.len()
                    + 3
                    + strings.subject.grapheme_width()
                    + 1
                    + strings.tags.grapheme_width(),
            );
            max_entry_columns = std::cmp::max(
                max_entry_columns,
                strings.date.len() + 1 + strings.from.grapheme_width(),
            );
            self.rows
                .as_mut()
                .unwrap()
                .push(((self.length, (thread, root_env_hash)), strings));
            self.all_threads.insert(thread);

            self.order.insert(thread, self.length);
            self.selection.insert(thread, false);
            self.length += 1;
        }

        if self.length == 0 && self.filter_term.is_empty() {
            let message: String = account[&self.cursor_pos.1].status();
            self.rows = Err(message);
        }
    }
}

impl ListingTrait for ConversationsListing {
    fn coordinates(&self) -> (AccountHash, MailboxHash) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1)
    }

    fn set_coordinates(&mut self, coordinates: (AccountHash, MailboxHash)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.focus = Focus::None;
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
        self.draw_rows(grid, area, context, idx);
    }

    /// Draw the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0
        {
            self.refresh_mailbox(context, false);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if let Err(message) = self.rows.as_ref() {
            clear_area(grid, area, self.color_cache.theme_default);
            write_string_to_grid(
                message,
                grid,
                self.color_cache.theme_default.fg,
                self.color_cache.theme_default.bg,
                self.color_cache.theme_default.attrs,
                area,
                None,
            );
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 3;
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
                        self.new_cursor_pos.2 = self.length.saturating_sub(1);
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    if self.new_cursor_pos.2 + rows * multiplier + 1 < self.length {
                        self.new_cursor_pos.2 += rows * multiplier;
                    } else if self.new_cursor_pos.2 + rows * multiplier > self.length {
                        self.new_cursor_pos.2 = self.length.saturating_sub(1);
                    } else {
                        self.new_cursor_pos.2 = (self.length.saturating_sub(1) / rows) * rows;
                    }
                }
                PageMovement::Right(_) | PageMovement::Left(_) => {}
                PageMovement::Home => {
                    self.new_cursor_pos.2 = 0;
                }
                PageMovement::End => {
                    self.new_cursor_pos.2 = self.length.saturating_sub(1);
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
                    set_y(upper_left, get_y(upper_left) + 3 * (*idx % rows)),
                    set_y(bottom_right, get_y(upper_left) + 3 * (*idx % rows) + 2),
                );
                self.highlight_line(grid, new_area, *idx, context);
                context.dirty_areas.push_back(new_area);
            }
            return;
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }
        if self.new_cursor_pos.2 >= self.length {
            self.new_cursor_pos.2 = self.length.saturating_sub(1);
            self.cursor_pos.2 = self.new_cursor_pos.2;
        }

        clear_area(grid, area, self.color_cache.theme_default);
        /* Page_no has changed, so draw new page */
        self.draw_rows(grid, area, context, top_idx);

        self.highlight_line(
            grid,
            (
                pos_inc(upper_left, (0, 3 * (self.cursor_pos.2 % rows))),
                set_y(
                    bottom_right,
                    get_y(upper_left) + 3 * (self.cursor_pos.2 % rows) + 2,
                ),
            ),
            self.cursor_pos.2,
            context,
        );

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
            if self.all_threads.contains(&thread) {
                self.filtered_selection.push(thread);
                self.filtered_order
                    .insert(thread, self.filtered_selection.len().saturating_sub(1));
            }
        }
        if !self.filtered_selection.is_empty() {
            threads.group_inner_sort_by(
                &mut self.filtered_selection,
                self.sort,
                &context.accounts[&self.cursor_pos.0].collection.envelopes,
            );
            self.new_cursor_pos.2 = std::cmp::min(
                self.filtered_selection.len().saturating_sub(1),
                self.cursor_pos.2,
            );
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
                /* If self.row_updates is not empty and we exit a thread, the row_update events
                 * will be performed but the list will not be drawn. So force a draw in any case.
                 * */
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

impl fmt::Display for ConversationsListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl ConversationsListing {
    //const DESCRIPTION: &'static str = "conversations listing";
    //const PADDING_CHAR: char = ' '; //░';

    pub fn new(coordinates: (AccountHash, MailboxHash)) -> Box<Self> {
        Box::new(Self {
            cursor_pos: (coordinates.0, 1, 0),
            new_cursor_pos: (coordinates.0, coordinates.1, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            order: HashMap::default(),
            all_threads: HashSet::default(),
            search_job: None,
            filter_term: String::new(),
            filtered_selection: Vec::new(),
            filtered_order: HashMap::default(),
            selection: HashMap::default(),
            row_updates: SmallVec::new(),
            rows: Ok(Vec::with_capacity(1024)),
            dirty: true,
            force_draw: true,
            focus: Focus::None,
            view: ThreadView::default(),
            color_cache: ColorCache::default(),
            movement: None,
            modifier_active: false,
            modifier_command: None,
            id: ComponentId::new_v4(),
        })
    }

    pub(super) fn make_entry_string(
        &self,
        e: &Envelope,
        context: &Context,
        from: &[Address],
        threads: &Threads,
        other_subjects: &IndexSet<String>,
        hash: ThreadHash,
    ) -> EntryStrings {
        let thread = threads.thread_ref(hash);
        let mut tags = String::new();
        let mut colors = SmallVec::new();
        let account = &context.accounts[&self.cursor_pos.0];
        if account.backend_capabilities.supports_tags {
            let tags_lck = account.collection.tag_index.read().unwrap();
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
                    acc.push_str(&s);
                    acc
                })
        } else {
            e.subject().to_string()
        };
        subject.truncate_at_boundary(100);
        if thread.len() > 1 {
            EntryStrings {
                date: DateString(ConversationsListing::format_date(context, thread.date())),
                subject: SubjectString(format!("{} ({})", subject, thread.len())),
                flag: FlagString(format!(
                    "{}{}",
                    if thread.has_attachments() { "📎" } else { "" },
                    if thread.snoozed() { "💤" } else { "" }
                )),
                from: FromString(address_list!((from) as comma_sep_list)),
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
                from: FromString(address_list!((from) as comma_sep_list)),
                tags: TagString(tags, colors),
            }
        }
    }

    pub(super) fn format_date(context: &Context, epoch: UnixTimestamp) -> String {
        let d = std::time::UNIX_EPOCH + std::time::Duration::from_secs(epoch);
        let now: std::time::Duration = std::time::SystemTime::now()
            .duration_since(d)
            .unwrap_or_else(|_| std::time::Duration::new(std::u64::MAX, 0));
        match now.as_secs() {
            n if context.settings.listing.recent_dates && n < 60 * 60 => format!(
                "{} minute{} ago",
                n / (60),
                if n / 60 == 1 { "" } else { "s" }
            ),
            n if context.settings.listing.recent_dates && n < 24 * 60 * 60 => format!(
                "{} hour{} ago",
                n / (60 * 60),
                if n / (60 * 60) == 1 { "" } else { "s" }
            ),
            n if context.settings.listing.recent_dates && n < 7 * 24 * 60 * 60 => format!(
                "{} day{} ago",
                n / (24 * 60 * 60),
                if n / (24 * 60 * 60) == 1 { "" } else { "s" }
            ),
            _ => melib::datetime::timestamp_to_string(
                epoch,
                context
                    .settings
                    .listing
                    .datetime_fmt
                    .as_deref()
                    .or(Some("%Y-%m-%d %T")),
                false,
            ),
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
        let account = &context.accounts[&self.cursor_pos.0];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        let thread_node_hash = threads.thread_group_iter(thread_hash).next().unwrap().1;
        let idx: usize = self.order[&thread_hash];
        let env_hash = threads.thread_nodes()[&thread_node_hash].message().unwrap();

        let mut other_subjects = IndexSet::new();
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
            for addr in envelope.from().iter() {
                if from_address_set.contains(addr.address_spec_raw()) {
                    continue;
                }
                from_address_set.insert(addr.address_spec_raw().to_vec());
                from_address_list.push(addr.clone());
            }
        }
        let envelope: EnvelopeRef = account.collection.get_env(env_hash);
        let strings = self.make_entry_string(
            &envelope,
            context,
            &from_address_list,
            &threads,
            &other_subjects,
            thread_hash,
        );
        drop(envelope);
        if let Ok(rows) = self.rows.as_mut() {
            if let Some(row) = rows.get_mut(idx) {
                row.1 = strings;
            }
        }
    }

    fn draw_rows(&self, grid: &mut CellBuffer, area: Area, context: &Context, top_idx: usize) {
        let rows_ref = match self.rows.as_ref() {
            Ok(rows) => rows,
            Err(_) => return,
        };
        let account = &context.accounts[&self.cursor_pos.0];
        let threads = account.collection.get_threads(self.cursor_pos.1);
        let (mut upper_left, bottom_right) = area;
        for ((idx, (thread_hash, root_env_hash)), strings) in rows_ref.iter().skip(top_idx) {
            if !context.accounts[&self.cursor_pos.0].contains_key(*root_env_hash) {
                panic!();
            }
            let thread = threads.thread_ref(*thread_hash);

            let row_attr = row_attr!(
                self.color_cache,
                thread.unseen() > 0,
                self.cursor_pos.2 == *idx,
                self.selection[thread_hash]
            );
            /* draw flags */
            let (x, _) = write_string_to_grid(
                &strings.flag,
                grid,
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                (upper_left, bottom_right),
                None,
            );
            for x in x..(x + 3) {
                grid[set_x(upper_left, x)].set_bg(row_attr.bg);
            }
            let subject_attr = row_attr!(
                subject,
                self.color_cache,
                thread.unseen() > 0,
                self.cursor_pos.2 == *idx,
                self.selection[thread_hash]
            );
            /* draw subject */
            let (mut x, _) = write_string_to_grid(
                &strings.subject,
                grid,
                subject_attr.fg,
                subject_attr.bg,
                subject_attr.attrs,
                (set_x(upper_left, x), bottom_right),
                None,
            );
            for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                let color = color.unwrap_or(self.color_cache.tag_default.bg);
                let (_x, _) = write_string_to_grid(
                    t,
                    grid,
                    self.color_cache.tag_default.fg,
                    color,
                    self.color_cache.tag_default.attrs,
                    (set_x(upper_left, x + 1), bottom_right),
                    None,
                );
                grid[set_x(upper_left, x)].set_bg(color);
                if _x <= get_x(bottom_right) {
                    grid[set_x(upper_left, _x)].set_bg(color).set_keep_bg(true);
                }
                for x in (x + 1).._x {
                    grid[set_x(upper_left, x)]
                        .set_keep_fg(true)
                        .set_keep_bg(true);
                }
                grid[set_x(upper_left, x)].set_keep_bg(true);
                x = _x + 1;
            }
            for x in x..get_x(bottom_right) {
                grid[set_x(upper_left, x)]
                    .set_ch(' ')
                    .set_fg(row_attr.fg)
                    .set_bg(row_attr.bg);
            }
            let date_attr = row_attr!(
                date,
                self.color_cache,
                thread.unseen() > 0,
                self.cursor_pos.2 == *idx,
                self.selection[thread_hash]
            );
            upper_left.1 += 1;
            if upper_left.1 >= bottom_right.1 {
                return;
            }
            /* Next line, draw date */
            let (x, _) = write_string_to_grid(
                &strings.date,
                grid,
                date_attr.fg,
                date_attr.bg,
                date_attr.attrs,
                (upper_left, bottom_right),
                None,
            );
            for x in x..(x + 4) {
                grid[set_x(upper_left, x)]
                    .set_ch('▁')
                    .set_fg(row_attr.fg)
                    .set_bg(row_attr.bg);
            }
            let from_attr = row_attr!(
                from,
                self.color_cache,
                thread.unseen() > 0,
                self.cursor_pos.2 == *idx,
                self.selection[thread_hash]
            );
            /* draw from */
            let (x, _) = write_string_to_grid(
                &strings.from,
                grid,
                from_attr.fg,
                from_attr.bg,
                from_attr.attrs,
                (set_x(upper_left, x + 4), bottom_right),
                None,
            );

            for x in x..get_x(bottom_right) {
                grid[set_x(upper_left, x)]
                    .set_ch('▁')
                    .set_fg(row_attr.fg)
                    .set_bg(row_attr.bg);
            }
            upper_left.1 += 2;
            if upper_left.1 >= bottom_right.1 {
                return;
            }
        }
    }
}

impl Component for ConversationsListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }

        if matches!(self.focus, Focus::EntryFullscreen) {
            return self.view.draw(grid, area, context);
        }

        let (upper_left, bottom_right) = area;
        {
            let mut area = area;

            if !self.filter_term.is_empty() {
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
                for c in grid.row_iter(x..(get_x(bottom_right) + 1), y) {
                    grid[c] = Cell::default();
                }
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
            let (upper_left, bottom_right) = area;
            let rows = (get_y(bottom_right) - get_y(upper_left) + 1) / 3;
            if let Some(modifier) = self.modifier_command.take() {
                if let Some(mvm) = self.movement.as_ref() {
                    match mvm {
                        PageMovement::Up(amount) => {
                            for c in self.new_cursor_pos.2.saturating_sub(*amount)
                                ..=self.new_cursor_pos.2
                            {
                                let thread = self.get_thread_under_cursor(c);
                                match modifier {
                                    Modifier::SymmetricDifference => {
                                        self.selection.entry(thread).and_modify(|e| *e = !*e);
                                    }
                                    Modifier::Union => {
                                        self.selection.entry(thread).and_modify(|e| *e = true);
                                    }
                                    Modifier::Difference => {
                                        self.selection.entry(thread).and_modify(|e| *e = false);
                                    }
                                    Modifier::Intersection => {}
                                }
                                self.row_updates.push(thread);
                            }
                            if modifier == Modifier::Intersection {
                                for c in (0..self.new_cursor_pos.2.saturating_sub(*amount))
                                    .chain((self.new_cursor_pos.2 + 2)..self.length)
                                {
                                    let thread = self.get_thread_under_cursor(c);
                                    self.selection.entry(thread).and_modify(|e| *e = false);
                                    self.row_updates.push(thread);
                                }
                            }
                        }
                        PageMovement::PageUp(multiplier) => {
                            for c in self.new_cursor_pos.2.saturating_sub(rows * multiplier)
                                ..=self.new_cursor_pos.2
                            {
                                let thread = self.get_thread_under_cursor(c);
                                match modifier {
                                    Modifier::SymmetricDifference => {
                                        self.selection.entry(thread).and_modify(|e| *e = !*e);
                                    }
                                    Modifier::Union => {
                                        self.selection.entry(thread).and_modify(|e| *e = true);
                                    }
                                    Modifier::Difference => {
                                        self.selection.entry(thread).and_modify(|e| *e = false);
                                    }
                                    Modifier::Intersection => {}
                                }
                                self.row_updates.push(thread);
                            }
                        }
                        PageMovement::Down(amount) => {
                            for c in self.new_cursor_pos.2
                                ..std::cmp::min(self.length, self.new_cursor_pos.2 + amount + 1)
                            {
                                let thread = self.get_thread_under_cursor(c);
                                match modifier {
                                    Modifier::SymmetricDifference => {
                                        self.selection.entry(thread).and_modify(|e| *e = !*e);
                                    }
                                    Modifier::Union => {
                                        self.selection.entry(thread).and_modify(|e| *e = true);
                                    }
                                    Modifier::Difference => {
                                        self.selection.entry(thread).and_modify(|e| *e = false);
                                    }
                                    Modifier::Intersection => {}
                                }
                                self.row_updates.push(thread);
                            }
                            if modifier == Modifier::Intersection {
                                for c in (0..self.new_cursor_pos.2).chain(
                                    (std::cmp::min(self.length, self.new_cursor_pos.2 + amount + 1)
                                        + 1)..self.length,
                                ) {
                                    let thread = self.get_thread_under_cursor(c);
                                    self.selection.entry(thread).and_modify(|e| *e = false);
                                    self.row_updates.push(thread);
                                }
                            }
                        }
                        PageMovement::PageDown(multiplier) => {
                            for c in self.new_cursor_pos.2
                                ..std::cmp::min(
                                    self.new_cursor_pos.2 + rows * multiplier + 1,
                                    self.length,
                                )
                            {
                                let thread = self.get_thread_under_cursor(c);
                                match modifier {
                                    Modifier::SymmetricDifference => {
                                        self.selection.entry(thread).and_modify(|e| *e = !*e);
                                    }
                                    Modifier::Union => {
                                        self.selection.entry(thread).and_modify(|e| *e = true);
                                    }
                                    Modifier::Difference => {
                                        self.selection.entry(thread).and_modify(|e| *e = false);
                                    }
                                    Modifier::Intersection => {}
                                }
                                self.row_updates.push(thread);
                            }
                            if modifier == Modifier::Intersection {
                                for c in (0..self.new_cursor_pos.2).chain(
                                    (std::cmp::min(
                                        self.new_cursor_pos.2 + rows * multiplier + 1,
                                        self.length,
                                    ) + 1)..self.length,
                                ) {
                                    let thread = self.get_thread_under_cursor(c);
                                    self.selection.entry(thread).and_modify(|e| *e = false);
                                    self.row_updates.push(thread);
                                }
                            }
                        }
                        PageMovement::Right(_) | PageMovement::Left(_) => {}
                        PageMovement::Home => {
                            for c in 0..=self.new_cursor_pos.2 {
                                let thread = self.get_thread_under_cursor(c);
                                match modifier {
                                    Modifier::SymmetricDifference => {
                                        self.selection.entry(thread).and_modify(|e| *e = !*e);
                                    }
                                    Modifier::Union => {
                                        self.selection.entry(thread).and_modify(|e| *e = true);
                                    }
                                    Modifier::Difference => {
                                        self.selection.entry(thread).and_modify(|e| *e = false);
                                    }
                                    Modifier::Intersection => {}
                                }
                                self.row_updates.push(thread);
                            }
                            if modifier == Modifier::Intersection {
                                for c in (self.new_cursor_pos.2 + 1)..self.length {
                                    let thread = self.get_thread_under_cursor(c);
                                    self.selection.entry(thread).and_modify(|e| *e = false);
                                    self.row_updates.push(thread);
                                }
                            }
                        }
                        PageMovement::End => {
                            for c in self.new_cursor_pos.2..self.length {
                                let thread = self.get_thread_under_cursor(c);
                                match modifier {
                                    Modifier::SymmetricDifference => {
                                        self.selection.entry(thread).and_modify(|e| *e = !*e);
                                    }
                                    Modifier::Union => {
                                        self.selection.entry(thread).and_modify(|e| *e = true);
                                    }
                                    Modifier::Difference => {
                                        self.selection.entry(thread).and_modify(|e| *e = false);
                                    }
                                    Modifier::Intersection => {}
                                }
                                self.row_updates.push(thread);
                            }
                            if modifier == Modifier::Intersection {
                                for c in 0..self.new_cursor_pos.2 {
                                    let thread = self.get_thread_under_cursor(c);
                                    self.selection.entry(thread).and_modify(|e| *e = false);
                                    self.row_updates.push(thread);
                                }
                            }
                        }
                    }
                }
            }
            if !self.row_updates.is_empty() {
                /* certain rows need to be updated (eg an unseen message was just set seen)
                 * */
                while let Some(row) = self.row_updates.pop() {
                    self.update_line(context, row);
                    let row: usize = self.order[&row];

                    let page_no = (self.cursor_pos.2).wrapping_div(rows);

                    let top_idx = page_no * rows;
                    /* Update row only if it's currently visible */
                    if row >= top_idx && row < top_idx + rows {
                        let area = (
                            set_y(upper_left, get_y(upper_left) + (3 * (row % rows))),
                            set_y(bottom_right, get_y(upper_left) + (3 * (row % rows) + 2)),
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
        }
        if matches!(self.focus, Focus::Entry) {
            if self.length == 0 && self.dirty {
                clear_area(grid, area, self.color_cache.theme_default);
                context.dirty_areas.push_back(area);
                return;
            }

            let area = (
                set_x(upper_left, get_x(upper_left) + width!(area) / 3 + 2),
                bottom_right,
            );
            self.view.draw(grid, area, context);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);

        match (&event, self.focus) {
            (UIEvent::Input(ref k), Focus::Entry)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["focus_right"]) =>
            {
                self.set_focus(Focus::EntryFullscreen, context);
                return true;
            }
            (UIEvent::Input(ref k), Focus::EntryFullscreen)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["focus_left"]) =>
            {
                self.set_focus(Focus::Entry, context);
                return true;
            }
            (UIEvent::Input(ref k), Focus::Entry)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["focus_left"]) =>
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
                        && (shortcut!(k == shortcuts[Listing::DESCRIPTION]["open_entry"])
                            || shortcut!(k == shortcuts[Listing::DESCRIPTION]["focus_right"])) =>
                {
                    let thread = self.get_thread_under_cursor(self.cursor_pos.2);
                    self.view = ThreadView::new(self.cursor_pos, thread, None, context);
                    self.set_focus(Focus::Entry, context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if !matches!(self.focus, Focus::None)
                        && shortcut!(k == shortcuts[Listing::DESCRIPTION]["exit_entry"]) =>
                {
                    self.set_focus(Focus::None, context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if matches!(self.focus, Focus::Entry)
                        && shortcut!(k == shortcuts[Listing::DESCRIPTION]["focus_right"]) =>
                {
                    self.set_focus(Focus::EntryFullscreen, context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if !matches!(self.focus, Focus::None)
                        && shortcut!(k == shortcuts[Listing::DESCRIPTION]["focus_left"]) =>
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
                        && shortcut!(key == shortcuts[Listing::DESCRIPTION]["select_entry"]) =>
                {
                    if self.modifier_active && self.modifier_command.is_none() {
                        self.modifier_command = Some(Modifier::default());
                    } else {
                        let thread_hash = self.get_thread_under_cursor(self.cursor_pos.2);
                        self.selection.entry(thread_hash).and_modify(|e| *e = !*e);
                        self.row_updates.push(thread_hash);
                    }
                    return true;
                }
                UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                    let account = &context.accounts[&self.cursor_pos.0];
                    let threads = account.collection.get_threads(self.cursor_pos.1);
                    if !account.collection.contains_key(new_hash) {
                        return false;
                    }
                    let env_thread_node_hash = account.collection.get_env(*new_hash).thread();
                    if !threads.thread_nodes.contains_key(&env_thread_node_hash) {
                        return false;
                    }
                    let thread: ThreadHash =
                        threads.find_group(threads.thread_nodes()[&env_thread_node_hash].group);
                    drop(threads);
                    if self.order.contains_key(&thread) {
                        self.row_updates.push(thread);
                    }

                    self.dirty = true;

                    if self.unfocused() {
                        self.view.process_event(
                            &mut UIEvent::EnvelopeRename(*old_hash, *new_hash),
                            context,
                        );
                    }
                }
                UIEvent::EnvelopeRemove(ref _env_hash, ref thread_hash) => {
                    if self.order.contains_key(thread_hash) {
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
                    let env_thread_node_hash = account.collection.get_env(*env_hash).thread();
                    if !threads.thread_nodes.contains_key(&env_thread_node_hash) {
                        return false;
                    }
                    let thread: ThreadHash =
                        threads.find_group(threads.thread_nodes()[&env_thread_node_hash].group);
                    drop(threads);
                    if self.order.contains_key(&thread) {
                        self.row_updates.push(thread);
                    }

                    self.dirty = true;

                    if self.unfocused() {
                        self.view
                            .process_event(&mut UIEvent::EnvelopeUpdate(*env_hash), context);
                    }
                }
                UIEvent::Action(ref action) => match action {
                    Action::SubSort(field, order) if !self.unfocused() => {
                        debug!("SubSort {:?} , {:?}", field, order);
                        self.subsort = (*field, *order);
                        // FIXME subsort
                        //if !self.filtered_selection.is_empty() {
                        //    let threads = &account.collection.threads[&self.cursor_pos.1];
                        //    threads.vec_inner_sort_by(&mut self.filtered_selection, self.sort, &account.collection);
                        //} else {
                        //    self.refresh_mailbox(context, false);
                        //}
                        return true;
                    }
                    Action::Sort(field, order) if !self.unfocused() => {
                        debug!("Sort {:?} , {:?}", field, order);
                        // FIXME sort
                        /*
                        self.sort = (*field, *order);
                        if !self.filtered_selection.is_empty() {
                            let threads = &context.accounts[&self.cursor_pos.0].collection.threads
                                [&self.cursor_pos.1];
                            threads.vec_inner_sort_by(
                                &mut self.filtered_selection,
                                self.sort,
                                &context.accounts[&self.cursor_pos.0].collection.envelopes,
                            );
                            self.dirty = true;
                        } else {
                            self.refresh_mailbox(context, false);
                        }
                            */
                        return true;
                    }
                    Action::Listing(ToggleThreadSnooze) if !self.unfocused() => {
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
                        self.row_updates.push(thread);
                        self.refresh_mailbox(context, false);
                        return true;
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.color_cache = ColorCache {
                    theme_default: crate::conf::value(context, "mail.listing.conversations"),
                    subject: crate::conf::value(context, "mail.listing.conversations.subject"),
                    from: crate::conf::value(context, "mail.listing.conversations.from"),
                    date: crate::conf::value(context, "mail.listing.conversations.date"),
                    selected: crate::conf::value(context, "mail.listing.conversations.selected"),
                    unseen: crate::conf::value(context, "mail.listing.conversations.unseen"),
                    highlighted: crate::conf::value(
                        context,
                        "mail.listing.conversations.highlighted",
                    ),
                    attachment_flag: crate::conf::value(context, "mail.listing.attachment_flag"),
                    thread_snooze_flag: crate::conf::value(
                        context,
                        "mail.listing.thread_snooze_flag",
                    ),
                    tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                    ..self.color_cache
                };

                if !context.settings.terminal.use_color() {
                    self.color_cache.highlighted.attrs |= Attr::REVERSE;
                    self.color_cache.tag_default.attrs |= Attr::REVERSE;
                }
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
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Action(ref action) => match action {
                Action::Listing(Search(ref filter_term)) if !self.unfocused() => {
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
                    return true;
                }
                _ => {}
            },
            UIEvent::Input(Key::Esc)
                if !self.unfocused()
                    && self.selection.values().cloned().any(std::convert::identity) =>
            {
                for (k, v) in self.selection.iter_mut() {
                    if *v {
                        *v = false;
                        self.row_updates.push(*k);
                    }
                }
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Char(''))
                if !self.unfocused() && !&self.filter_term.is_empty() =>
            {
                self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1));
                self.refresh_mailbox(context, false);
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
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not perform search".to_string()),
                            err.to_string(),
                            Some(crate::types::NotificationType::Error(err.kind)),
                        ));
                    }
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
        if self.unfocused() {
            self.view.set_dirty(value);
        }
        self.dirty = value;
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.unfocused() {
            self.view.get_shortcuts(context)
        } else {
            ShortcutMaps::default()
        };

        let config_map = context.settings.shortcuts.listing.key_values();
        map.insert(Listing::DESCRIPTION, config_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
