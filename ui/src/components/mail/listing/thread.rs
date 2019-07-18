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
use crate::components::utilities::PageMovement;

const MAX_COLS: usize = 500;

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `MailView`.
#[derive(Debug)]
pub struct ThreadListing {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    /// Cache current view.
    content: CellBuffer,

    locations: Vec<EnvelopeHash>,
    /// If we must redraw on next redraw event
    dirty: bool,
    /// If `self.view` is focused or not.
    unfocused: bool,
    initialised: bool,
    view: Option<MailView>,
    movement: Option<PageMovement>,
    id: ComponentId,
}

impl ListingTrait for ThreadListing {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>) {
        (
            self.cursor_pos.0,
            self.cursor_pos.1,
            Some(self.locations[self.cursor_pos.2]),
        )
    }
    fn set_coordinates(&mut self, coordinates: (usize, usize, Option<EnvelopeHash>)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
    }
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0
        {
            self.refresh_mailbox(context);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area);
            copy_area(grid, &self.content, area, ((0, 0), (MAX_COLS - 1, 0)));
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;
        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let top_idx = page_no * rows;
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

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();
        if mailbox.is_empty() || mailbox.len() <= idx {
            return;
        }

        if self.locations[idx] != 0 {
            let envelope: &Envelope =
                &context.accounts[self.cursor_pos.0].get_env(&self.locations[idx]);

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
    }
}

impl Default for ThreadListing {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ThreadListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl ThreadListing {
    pub fn new() -> Self {
        let content = CellBuffer::new(0, 0, Cell::with_char(' '));
        ThreadListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            sort: (Default::default(), Default::default()),
            subsort: (Default::default(), Default::default()),
            content,
            locations: Vec::new(),
            dirty: true,
            unfocused: false,
            view: None,
            initialised: false,
            movement: None,
            id: ComponentId::new_v4(),
        }
    }
    /// Fill the `self.content` `CellBuffer` with the contents of the account folder the user has
    /// chosen.
    fn refresh_mailbox(&mut self, context: &mut Context) {
        self.dirty = true;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            //TODO: store cursor_pos in each folder
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;
        let folder_hash = context.accounts[self.cursor_pos.0].folders_order[self.cursor_pos.1];

        // Inform State that we changed the current folder view.
        context
            .replies
            .push_back(UIEvent::RefreshMailbox((self.cursor_pos.0, folder_hash)));
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
        let account = &context.accounts[self.cursor_pos.0];
        let mailbox = account[self.cursor_pos.1].as_ref().unwrap();

        let threads = &account.collection.threads[&mailbox.folder.hash()];
        self.length = threads.len();
        self.content = CellBuffer::new(MAX_COLS, self.length + 1, Cell::with_char(' '));
        self.locations.clear();
        if self.length == 0 {
            write_string_to_grid(
                &format!("Folder `{}` is empty.", mailbox.folder.name()),
                &mut self.content,
                Color::Default,
                Color::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                true,
            );
            return;
        }

        let mut indentations: Vec<bool> = Vec::with_capacity(6);
        let mut thread_idx = 0; // needed for alternate thread colors
                                /* Draw threaded view. */
        threads.sort_by(self.sort, self.subsort, &account.collection);
        let thread_nodes: &FnvHashMap<ThreadHash, ThreadNode> = &threads.thread_nodes();
        let mut iter = threads.threads_iter().peekable();
        /* This is just a desugared for loop so that we can use .peek() */
        let mut idx = 0;
        while let Some((indentation, thread_hash, has_sibling)) = iter.next() {
            let thread_node = &thread_nodes[&thread_hash];

            if indentation == 0 {
                thread_idx += 1;
            }
            if thread_node.has_message() {
                let envelope: &Envelope = &account.get_env(&thread_node.message().unwrap());
                self.locations.push(envelope.hash());
                let fg_color = if !envelope.is_seen() {
                    Color::Byte(0)
                } else {
                    Color::Default
                };
                let bg_color = if !envelope.is_seen() {
                    Color::Byte(251)
                } else if thread_idx % 2 == 0 {
                    Color::Byte(236)
                } else {
                    Color::Default
                };
                let (x, _) = write_string_to_grid(
                    &ThreadListing::make_thread_entry(
                        envelope,
                        idx,
                        indentation,
                        thread_hash,
                        threads,
                        &indentations,
                        has_sibling,
                    ),
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
    }

    fn highlight_line_self(&mut self, idx: usize, context: &Context) {
        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();
        if mailbox.is_empty() {
            return;
        }
        if self.locations[idx] != 0 {
            let envelope: &Envelope =
                &context.accounts[self.cursor_pos.0].get_env(&self.locations[idx]);

            let fg_color = if !envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if !envelope.is_seen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            };
            change_colors(
                &mut self.content,
                ((0, idx), (MAX_COLS - 1, idx)),
                fg_color,
                bg_color,
            );
        }
    }

    fn make_thread_entry(
        envelope: &Envelope,
        idx: usize,
        indent: usize,
        node_idx: ThreadHash,
        threads: &Threads,
        indentations: &[bool],
        has_sibling: bool,
        //op: Box<BackendOp>,
    ) -> String {
        let thread_node = &threads[&node_idx];
        let has_parent = thread_node.has_parent();
        let show_subject = thread_node.show_subject();

        let mut s = format!("{}{}{} ", idx, " ", ThreadListing::format_date(&envelope));
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

        if show_subject {
            s.push_str(&format!("{:.85}", envelope.subject()));
        }
        /* TODO Very slow since we have to build all attachments
        let attach_count = envelope.body(op).count_attachments();
        if attach_count > 1 {
            s.push_str(&format!(" {}âˆž ", attach_count - 1));
        }
         */
        s
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

impl Component for ThreadListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
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
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
            }

            /* Render the mail body in a pager, basically copy what HSplit does */
            let total_rows = get_y(bottom_right) - get_y(upper_left);
            let pager_ratio = context.runtime_settings.pager.pager_ratio;
            let bottom_entity_rows = (pager_ratio * total_rows) / 100;

            if bottom_entity_rows > total_rows {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
                return;
            }

            let idx = self.cursor_pos.2;

            let has_message: bool = self.locations[self.new_cursor_pos.2] > 0;
            if !has_message {
                self.dirty = false;
                /* Draw the entire list */
                return self.draw_list(grid, area, context);
            }

            /* Mark message as read */
            let must_highlight = {
                if self.length == 0 {
                    false
                } else {
                    let account = &mut context.accounts[self.cursor_pos.0];
                    let (hash, is_seen) = {
                        let envelope: &Envelope =
                            &account.get_env(&self.locations[self.cursor_pos.2]);
                        (envelope.hash(), envelope.is_seen())
                    };
                    is_seen
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
                self.locations[self.cursor_pos.2],
            );

            if let Some(ref mut v) = self.view {
                v.update(coordinates);
            } else {
                self.view = Some(MailView::new(coordinates, None, None));
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
            UIEvent::Input(Key::Up) => {
                if self.cursor_pos.2 > 0 {
                    self.new_cursor_pos.2 -= 1;
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
            UIEvent::Input(ref key) if *key == Key::PageUp => {
                self.movement = Some(PageMovement::PageUp);
                self.set_dirty();
            }
            UIEvent::Input(ref key) if *key == Key::PageDown => {
                self.movement = Some(PageMovement::PageDown);
                self.set_dirty();
            }
            UIEvent::Input(ref key) if *key == Key::Home => {
                self.movement = Some(PageMovement::Home);
                self.set_dirty();
            }
            UIEvent::Input(ref key) if *key == Key::End => {
                self.movement = Some(PageMovement::End);
                self.set_dirty();
            }
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
            UIEvent::RefreshMailbox(_) => {
                self.dirty = true;
                self.view = None;
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
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Action(ref action) => match action {
                Action::ViewMailbox(idx_m) => {
                    self.new_cursor_pos.1 = *idx_m;
                    self.dirty = true;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::SubSort(field, order) => {
                    debug!("SubSort {:?} , {:?}", field, order);
                    self.subsort = (*field, *order);
                    self.dirty = true;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::Sort(field, order) => {
                    debug!("Sort {:?} , {:?}", field, order);
                    self.sort = (*field, *order);
                    self.dirty = true;
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
        self.dirty || self.view.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
    fn set_dirty(&mut self) {
        if let Some(p) = self.view.as_mut() {
            p.set_dirty();
        };
        self.dirty = true;
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
