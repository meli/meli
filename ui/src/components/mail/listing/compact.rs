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
use components::utilities::PageMovement;

//use melib::mailbox::backends::BackendOp;

const MAX_COLS: usize = 500;

#[derive(Debug)]
struct MailboxView {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    /// Cache current view.
    content: CellBuffer,
    /// If we must redraw on next redraw event
    dirty: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: ThreadView,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl fmt::Display for MailboxView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

impl MailboxView {
    /// Helper function to format entry strings for CompactListing */
    /* TODO: Make this configurable */
    fn make_entry_string(e: &Envelope, len: usize, idx: usize) -> String {
        if len > 0 {
            format!(
                "{}    {}    {:.85} ({})",
                idx,
                &MailboxView::format_date(e),
                e.subject(),
                len
            )
        } else {
            format!(
                "{}    {}    {:.85}",
                idx,
                &MailboxView::format_date(e),
                e.subject(),
            )
        }
    }

    fn new() -> Self {
        let content = CellBuffer::new(0, 0, Cell::with_char(' '));
        MailboxView {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            content,
            dirty: true,
            unfocused: false,
            view: ThreadView::default(),

            movement: None,
            id: ComponentId::new_v4(),
        }
    }
    /// Fill the `self.content` `CellBuffer` with the contents of the account folder the user has
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
        let folder_hash = context.accounts[self.cursor_pos.0].folders_order[self.cursor_pos.1];

        // Get mailbox as a reference.
        //
        match context.accounts[self.cursor_pos.0].status(folder_hash) {
            Ok(_) => {}
            Err(_) => {
                self.content = CellBuffer::new(MAX_COLS, 1, Cell::with_char(' '));
                self.length = 0;
                write_string_to_grid(
                    "Loading.",
                    &mut self.content,
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
        } else {
            self.view = ThreadView::new(self.new_cursor_pos, None, context);
        }

        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();

        self.length = mailbox.collection.threads.root_len();
        self.content = CellBuffer::new(MAX_COLS, self.length + 1, Cell::with_char(' '));
        if self.length == 0 {
            write_string_to_grid(
                &format!("Folder `{}` is empty.", mailbox.folder.name()),
                &mut self.content,
                Color::Default,
                Color::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                false,
            );
            return;
        }
        let threads = &mailbox.collection.threads;
        threads.sort_by(self.sort, self.subsort, &mailbox.collection);
        for (idx, root_idx) in threads.root_iter().enumerate() {
            let thread_node = &threads.thread_nodes()[root_idx];
            let i = if let Some(i) = thread_node.message() {
                i
            } else {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[iter_ptr].children()[0];
                }
                threads.thread_nodes()[iter_ptr].message().unwrap()
            };
            if !mailbox.collection.contains_key(&i) {
                if cfg!(debug_assertions) {
                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
                    eprintln!("key = {}", i);
                }
                eprint!("{}:{}_{}:	", file!(), line!(), column!());
                eprintln!(
                    "name = {} {}",
                    mailbox.name(),
                    context.accounts[self.cursor_pos.0].name()
                );
                if cfg!(debug_assertions) {
                    eprint!("{}:{}_{}:	", file!(), line!(), column!());
                    eprintln!("{:#?}", context.accounts);
                }

                panic!();
            }
            let root_envelope: &Envelope = &mailbox.collection[&i];
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
                &MailboxView::make_entry_string(root_envelope, thread_node.len(), idx),
                &mut self.content,
                fg_color,
                bg_color,
                ((0, idx), (MAX_COLS - 1, idx)),
                false,
            );

            for x in x..MAX_COLS {
                self.content[(x, idx)].set_ch(' ');
                self.content[(x, idx)].set_bg(bg_color);
            }
        }
    }

    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        if idx == self.cursor_pos.2 {
            let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
                .as_ref()
                .unwrap();
            if mailbox.is_empty() {
                return;
            }
            let threads = &mailbox.collection.threads;
            let thread_node = threads.root_set(idx);
            let thread_node = &threads.thread_nodes()[thread_node];
            let i = if let Some(i) = thread_node.message() {
                i
            } else {
                let mut iter_ptr = thread_node.children()[0];
                while threads.thread_nodes()[iter_ptr].message().is_none() {
                    iter_ptr = threads.thread_nodes()[iter_ptr].children()[0];
                }
                threads.thread_nodes()[iter_ptr].message().unwrap()
            };

            let root_envelope: &Envelope = &mailbox.collection[&i];
            let fg_color = if !root_envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if self.cursor_pos.2 == idx {
                Color::Byte(246)
            } else if !root_envelope.is_seen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            };
            change_colors(grid, area, fg_color, bg_color);
            return;
        }

        let (width, height) = self.content.size();
        copy_area(
            grid,
            &self.content,
            area,
            ((0, idx), (width - 1, height - 1)),
        );
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
                &self.content,
                area,
                ((0, 0), (MAX_COLS - 1, self.length)),
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
                    /* This might "overflow" beyond the max_cursor_pos boundary if it's not yet
                     * set. TODO: Rework the page up/down stuff
                     */
                    if self.new_cursor_pos.2 + 2 * rows + 1 < self.length {
                        self.new_cursor_pos.2 += rows;
                    } else {
                        self.new_cursor_pos.2 = self.length.saturating_sub(rows).saturating_sub(1);
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

        /* Page_no has changed, so draw new page */
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
}

impl Component for MailboxView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.unfocused {
            if !self.is_dirty() {
                return;
            }
            /* Draw the entire list */
            self.draw_list(grid, area, context);
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
                self.view = ThreadView::new(self.cursor_pos, None, context);
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
            UIEvent::Input(ref k) if self.unfocused && *k == shortcuts["exit_thread"] => {
                self.unfocused = false;
                self.dirty = true;
                return true;
            }
            UIEvent::RefreshMailbox(_) => {
                self.dirty = true;
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
                    == context.accounts[self.new_cursor_pos.0].folders_order
                        [self.new_cursor_pos.1] =>
            {
                self.refresh_mailbox(context);
                self.set_dirty();
            }
            UIEvent::EnvelopeRename(ref folder_hash, _, _) => {
                if *folder_hash
                    == context.accounts[self.cursor_pos.0].folders_order[self.new_cursor_pos.1]
                {
                    self.refresh_mailbox(context);
                    self.set_dirty();
                }
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Action(ref action) => match action {
                Action::ViewMailbox(idx) => {
                    self.new_cursor_pos.1 = *idx;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::SubSort(field, order) => {
                    if cfg!(debug_assertions) {
                        eprint!("{}:{}_{}:	", file!(), line!(), column!());
                        eprintln!("SubSort {:?} , {:?}", field, order);
                    }
                    self.subsort = (*field, *order);
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::Sort(field, order) => {
                    if cfg!(debug_assertions) {
                        eprint!("{}:{}_{}:	", file!(), line!(), column!());
                        eprintln!("Sort {:?} , {:?}", field, order);
                    }
                    self.sort = (*field, *order);
                    self.refresh_mailbox(context);
                    return true;
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

    fn get_shortcuts(&self, context: &Context) -> ShortcutMap {
        let mut map = if self.unfocused {
            self.view.get_shortcuts(context)
        } else {
            ShortcutMap::default()
        };

        let config_map = context.settings.shortcuts.compact_listing.key_values();
        map.insert(
            "open_thread",
            if let Some(key) = config_map.get("open_thread") {
                (*key).clone()
            } else {
                Key::Char('\n')
            },
        );
        map.insert(
            "prev_page",
            if let Some(key) = config_map.get("prev_page") {
                (*key).clone()
            } else {
                Key::PageUp
            },
        );
        map.insert(
            "next_page",
            if let Some(key) = config_map.get("next_page") {
                (*key).clone()
            } else {
                Key::PageDown
            },
        );
        map.insert(
            "exit_thread",
            if let Some(key) = config_map.get("exit_thread") {
                (*key).clone()
            } else {
                Key::Char('i')
            },
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

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `ThreadView`.
#[derive(Debug)]
pub struct CompactListing {
    views: Vec<MailboxView>,
    cursor: usize,
    dirty: bool,
    populated: bool,
    id: ComponentId,
}

impl ListingTrait for CompactListing {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>) {
        (self.cursor, self.views[self.cursor].cursor_pos.1, None)
    }
    fn set_coordinates(&mut self, coordinates: (usize, usize, Option<EnvelopeHash>)) {
        self.views[self.cursor].new_cursor_pos = (coordinates.0, coordinates.1, 0);
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
    pub fn new() -> Self {
        CompactListing {
            views: Vec::with_capacity(8),
            cursor: 0,
            dirty: true,
            populated: false,
            id: ComponentId::new_v4(),
        }
    }
}

impl Component for CompactListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.populated {
            eprint!("{}:{}_{}:	", file!(), line!(), column!());
            eprintln!("populating");
            for (idx, a) in context.accounts.iter().enumerate() {
                for (fidx, _) in a.iter_mailboxes().enumerate() {
                    let mut m = MailboxView::new();
                    m.new_cursor_pos = (idx, fidx, 0);
                    self.views.push(m);
                }
            }
            self.populated = true;
        }
        self.dirty = false;

        if self.views.is_empty() {
            return;
        }
        self.views[self.cursor].draw(grid, area, context);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.views.is_empty() {
            return false;
        }
        match *event {
            UIEvent::Resize
            | UIEvent::MailboxUpdate(_)
            | UIEvent::ComponentKill(_)
            | UIEvent::StartupCheck(_)
            | UIEvent::EnvelopeUpdate(_)
            | UIEvent::EnvelopeRename(_, _, _)
            | UIEvent::EnvelopeRemove(_) => {
                return self
                    .views
                    .iter_mut()
                    .fold(false, |acc, v| acc || v.process_event(event, context));
            }
            _ => return self.views[self.cursor].process_event(event, context),
        }
    }

    fn is_dirty(&self) -> bool {
        if self.views.is_empty() {
            return self.dirty;
        }
        self.dirty || self.views[self.cursor].is_dirty()
    }
    fn set_dirty(&mut self) {
        if self.views.is_empty() {
            return;
        }

        self.views[self.cursor].set_dirty();
        self.dirty = true;
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMap {
        if self.views.is_empty() {
            return Default::default();
        }
        self.views[self.cursor].get_shortcuts(context)
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
