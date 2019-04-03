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

const MAX_COLS: usize = 500;

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `MailView`.
#[derive(Debug)]
pub struct PlainListing {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    local_collection: Vec<EnvelopeHash>,
    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),
    /// Cache current view.
    content: CellBuffer,
    /// If we must redraw on next redraw event
    dirty: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: Option<MailView>,
}

impl Default for PlainListing {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for PlainListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl PlainListing {
    /// Helper function to format entry strings for PlainListing */
    /* TODO: Make this configurable */
    fn make_entry_string(e: &Envelope, idx: usize) -> String {
        format!(
            "{}    {}    {}",
            idx,
            &e.datetime().format("%Y-%m-%d %H:%M:%S").to_string(),
            e.subject()
        )
    }

    pub fn new() -> Self {
        let content = CellBuffer::new(0, 0, Cell::with_char(' '));
        PlainListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            local_collection: Vec::new(),
            sort: (Default::default(), Default::default()),
            subsort: (Default::default(), Default::default()),
            content,
            dirty: true,
            unfocused: false,
            view: None,
        }
    }
    /// Fill the `self.content` `CellBuffer` with the contents of the account folder the user has
    /// chosen.
    fn refresh_mailbox(&mut self, context: &mut Context) {
        self.dirty = true;
        if !(self.cursor_pos.0 == self.new_cursor_pos.0
            && self.cursor_pos.1 == self.new_cursor_pos.1)
        {
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.0 = self.new_cursor_pos.0;
        self.cursor_pos.1 = self.new_cursor_pos.1;

        // Inform State that we changed the current folder view.
        context.replies.push_back(UIEvent {
            id: 0,
            event_type: UIEventType::RefreshMailbox((self.cursor_pos.0, self.cursor_pos.1)),
        });
        // Get mailbox as a reference.
        //
        // Get mailbox as a reference.
        //
        match context.accounts[self.cursor_pos.0].status(self.cursor_pos.1) {
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
        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();

        self.length = mailbox.len();
        self.content = CellBuffer::new(MAX_COLS, self.length + 1, Cell::with_char(' '));
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

        // Populate `CellBuffer` with every entry.
        let mut idx = 0;
        for y in 0..=self.length {
            if idx >= self.length {
                /* No more entries left, so fill the rest of the area with empty space */
                clear_area(&mut self.content, ((0, y), (MAX_COLS - 1, self.length)));
                break;
            }
            /* Write an entire line for each envelope entry. */
            self.local_collection = mailbox.collection.keys().cloned().collect();
            let sort = self.sort;
            self.local_collection.sort_by(|a, b| match sort {
                (SortField::Date, SortOrder::Desc) => {
                    let ma = &mailbox.collection[a];
                    let mb = &mailbox.collection[b];
                    mb.date().cmp(&ma.date())
                }
                (SortField::Date, SortOrder::Asc) => {
                    let ma = &mailbox.collection[a];
                    let mb = &mailbox.collection[b];
                    ma.date().cmp(&mb.date())
                }
                (SortField::Subject, SortOrder::Desc) => {
                    let ma = &mailbox.collection[a];
                    let mb = &mailbox.collection[b];
                    ma.subject().cmp(&mb.subject())
                }
                (SortField::Subject, SortOrder::Asc) => {
                    let ma = &mailbox.collection[a];
                    let mb = &mailbox.collection[b];
                    mb.subject().cmp(&ma.subject())
                }
            });
            let envelope: &Envelope = &mailbox.collection[&self.local_collection[idx]];

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
            let (x, y) = write_string_to_grid(
                &PlainListing::make_entry_string(envelope, idx),
                &mut self.content,
                fg_color,
                bg_color,
                ((0, y), (MAX_COLS - 1, y)),
                false,
            );

            for x in x..MAX_COLS {
                self.content[(x, y)].set_ch(' ');
                self.content[(x, y)].set_bg(bg_color);
            }

            idx += 1;
        }
    }

    fn highlight_line_self(&mut self, idx: usize, context: &Context) {
        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();
        let envelope: &Envelope = &mailbox.collection[&self.local_collection[idx]];

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

    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();
        let envelope: &Envelope = &mailbox.collection[&self.local_collection[idx]];

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

    /// Draw the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 || self.cursor_pos.0 != self.new_cursor_pos.0 {
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
}

impl Component for PlainListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.unfocused {
            if !self.is_dirty() {
                return;
            }
            self.dirty = false;
            /* Draw the entire list */
            self.draw_list(grid, area, context);
        } else {
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
            /* Mark message as read */
            let idx = self.cursor_pos.2;
            let must_highlight = {
                if self.length == 0 {
                    false
                } else {
                    let account = &mut context.accounts[self.cursor_pos.0];
                    let (hash, is_seen) = {
                        let mailbox = &mut account[self.cursor_pos.1].as_mut().unwrap();
                        let envelope: &mut Envelope = &mut mailbox
                            .collection
                            .entry(self.local_collection[idx])
                            .or_default();
                        (envelope.hash(), envelope.is_seen())
                    };
                    if !is_seen {
                        let folder_hash = {
                            let mailbox = &mut account[self.cursor_pos.1].as_mut().unwrap();
                            mailbox.folder.hash()
                        };
                        let op = {
                            let backend = &account.backend;
                            backend.operation(hash, folder_hash)
                        };
                        let mailbox = &mut account[self.cursor_pos.1].as_mut().unwrap();
                        let envelope: &mut Envelope = &mut mailbox
                            .collection
                            .entry(self.local_collection[idx])
                            .or_default();
                        envelope.set_seen(op).unwrap();
                        true
                    } else {
                        false
                    }
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
                    grid[(i, mid)].set_ch('─');
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
            {
                let coordinates = self.cursor_pos;
                let coordinates = (
                    coordinates.0,
                    coordinates.1,
                    self.local_collection[self.cursor_pos.2],
                );
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
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos.2 > 0 {
                    self.new_cursor_pos.2 -= 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEventType::Input(Key::Down) => {
                if self.length > 0 && self.new_cursor_pos.2 < self.length - 1 {
                    self.new_cursor_pos.2 += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEventType::Input(Key::Char('\n')) if !self.unfocused => {
                self.unfocused = true;
                self.dirty = true;
                return true;
            }
            UIEventType::Input(Key::Char('m')) if !self.unfocused => {
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::Action(Tab(NewDraft)),
                });
                return true;
            }
            UIEventType::Input(Key::Char('i')) if self.unfocused => {
                self.unfocused = false;
                self.dirty = true;
                self.view = None;
                return true;
            }
            UIEventType::Input(Key::Char(k @ 'J')) | UIEventType::Input(Key::Char(k @ 'K')) => {
                let folder_length = context.accounts[self.cursor_pos.0].len();
                let accounts_length = context.accounts.len();
                match k {
                    'J' if folder_length > 0 => {
                        if self.new_cursor_pos.1 < folder_length - 1 {
                            self.new_cursor_pos.1 = self.cursor_pos.1 + 1;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        } else if accounts_length > 0 && self.new_cursor_pos.0 < accounts_length - 1
                        {
                            self.new_cursor_pos.0 = self.cursor_pos.0 + 1;
                            self.new_cursor_pos.1 = 0;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        }
                    }
                    'K' => {
                        if self.cursor_pos.1 > 0 {
                            self.new_cursor_pos.1 = self.cursor_pos.1 - 1;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        } else if self.cursor_pos.0 > 0 {
                            self.new_cursor_pos.0 = self.cursor_pos.0 - 1;
                            self.new_cursor_pos.1 = 0;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        }
                    }
                    _ => {}
                }
                return true;
            }
            UIEventType::Input(Key::Char(k @ 'h')) | UIEventType::Input(Key::Char(k @ 'l')) => {
                let accounts_length = context.accounts.len();
                match k {
                    'h' if accounts_length > 0 && self.new_cursor_pos.0 < accounts_length - 1 => {
                        self.new_cursor_pos.0 = self.cursor_pos.0 + 1;
                        self.new_cursor_pos.1 = 0;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    }
                    'l' if self.cursor_pos.0 > 0 => {
                        self.new_cursor_pos.0 = self.cursor_pos.0 - 1;
                        self.new_cursor_pos.1 = 0;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    }
                    _ => {}
                }
                return true;
            }
            UIEventType::RefreshMailbox(_) => {
                self.dirty = true;
                self.view = None;
            }
            UIEventType::MailboxUpdate((ref idxa, ref idxf)) if *idxa == self.new_cursor_pos.0 && *idxf == self.new_cursor_pos.1 => {
                self.refresh_mailbox(context);
                self.set_dirty();
            }
            UIEventType::StartupCheck(ref f) if context.mailbox_hashes[f] == (self.new_cursor_pos.0, self.new_cursor_pos.1) => {
                self.refresh_mailbox(context);
                self.set_dirty();
            }
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEventType::Resize => {
                self.dirty = true;
            }
            UIEventType::Action(ref action) => match action {
                Action::ViewMailbox(idx) => {
                    self.new_cursor_pos.1 = *idx;
                    self.dirty = true;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::SubSort(field, order) => {
                    if cfg!(feature = "debug_log") {
                        eprintln!("SubSort {:?} , {:?}", field, order);
                    }
                    self.subsort = (*field, *order);
                    self.dirty = true;
                    self.refresh_mailbox(context);
                    return true;
                }
                Action::Sort(field, order) => {
                    if cfg!(feature = "debug_log") {
                        eprintln!("Sort {:?} , {:?}", field, order);
                    }
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
}
