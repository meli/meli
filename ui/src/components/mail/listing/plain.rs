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

use std::cmp;
use std::ops::{Deref, DerefMut};
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

macro_rules! column_str {
    (
        struct $name:ident(String)) => {
        pub struct $name(String);

        impl Deref for $name {
            type Target = String;
            fn deref(&self) -> &String {
                &self.0
            }
        }
        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut String {
                &mut self.0
            }
        }
    };
}

column_str!(struct IndexNoString(String));
column_str!(struct DateString(String));
column_str!(struct FromString(String));
column_str!(struct SubjectString(String));

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
    movement: Option<PageMovement>,
    id: ComponentId,
}

impl ListingTrait for PlainListing {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>) {
        (self.new_cursor_pos.0, self.new_cursor_pos.1, None)
    }
    fn set_coordinates(&mut self, coordinates: (usize, usize, Option<EnvelopeHash>)) {
        self.new_cursor_pos = (coordinates.0, coordinates.1, 0);
        self.unfocused = false;
        self.local_collection.clear();
    }
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let account = &context.accounts[self.cursor_pos.0];
        let envelope: &Envelope = &account.get_env(&self.local_collection[idx]);

        let fg_color = if !envelope.is_seen() {
            Color::Byte(0)
        } else {
            Color::Default
        };
        let bg_color = if context.settings.terminal.theme == "light" {
            if self.cursor_pos.2 == idx {
                Color::Byte(246)
            } else if !envelope.is_seen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(252)
            } else {
                Color::Default
            }
        } else {
            if self.cursor_pos.2 == idx {
                Color::Byte(246)
            } else if !envelope.is_seen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            }
        };
        change_colors(grid, area, fg_color, bg_color);
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
            copy_area(grid, &self.content, area, ((0, 0), (MAX_COLS - 1, 0)));
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
                    if self.new_cursor_pos.2 + rows + 1 < self.length {
                        self.new_cursor_pos.2 += rows;
                    } else if self.new_cursor_pos.2 + rows > self.length {
                        self.new_cursor_pos.2 = self.length - 1;
                    } else {
                        self.new_cursor_pos.2 = (self.length / rows) * rows;
                    }
                }
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
    fn make_entry_string(
        e: &Envelope,
        idx: usize,
    ) -> (IndexNoString, FromString, DateString, SubjectString) {
        (
            IndexNoString(idx.to_string()),
            FromString(address_list!((e.from()) as comma_sep_list)),
            DateString(PlainListing::format_date(e)),
            SubjectString(format!(
                "{}{}",
                e.subject(),
                if e.has_attachments() { " 📎" } else { "" },
            )),
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
            self.cursor_pos.2 = 0;
            self.new_cursor_pos.2 = 0;
        }
        self.cursor_pos.0 = self.new_cursor_pos.0;
        self.cursor_pos.1 = self.new_cursor_pos.1;
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
                let message: String = context.accounts[self.cursor_pos.0][folder_hash].to_string();
                self.content = CellBuffer::new(message.len(), 1, Cell::with_char(' '));
                self.length = 0;
                write_string_to_grid(
                    message.as_str(),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((0, 0), (MAX_COLS - 1, 0)),
                    false,
                );
                return;
            }
        }
        let account = &context.accounts[self.cursor_pos.0];
        let mailbox = &account[self.cursor_pos.1].unwrap();

        self.length = mailbox.len();
        self.content = CellBuffer::new(MAX_COLS, self.length + 1, Cell::with_char(' '));
        if self.length == 0 {
            write_string_to_grid(
                &format!("Folder `{}` is empty.", mailbox.folder.name()),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                true,
            );
            return;
        }
        self.local_collection = account.collection.keys().cloned().collect();
        let sort = self.sort;
        self.local_collection.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let ma = &account.get_env(a);
                let mb = &account.get_env(b);
                mb.date().cmp(&ma.date())
            }
            (SortField::Date, SortOrder::Asc) => {
                let ma = &account.get_env(a);
                let mb = &account.get_env(b);
                ma.date().cmp(&mb.date())
            }
            (SortField::Subject, SortOrder::Desc) => {
                let ma = &account.get_env(a);
                let mb = &account.get_env(b);
                ma.subject().cmp(&mb.subject())
            }
            (SortField::Subject, SortOrder::Asc) => {
                let ma = &account.get_env(a);
                let mb = &account.get_env(b);
                mb.subject().cmp(&ma.subject())
            }
        });

        let mut rows = Vec::with_capacity(1024);
        let mut min_width = (0, 0, 0);
        let widths: (usize, usize, usize);

        for idx in 0..self.local_collection.len() {
            let envelope: &Envelope = &account.get_env(&self.local_collection[idx]);

            let strings = PlainListing::make_entry_string(envelope, idx);
            min_width.0 = cmp::max(min_width.0, strings.0.len()); /* index */
            min_width.1 = cmp::max(min_width.1, strings.2.split_graphemes().len()); /* date */
            min_width.2 = cmp::max(min_width.2, strings.3.split_graphemes().len()); /* subject */
            rows.push(strings);
        }
        let column_sep: usize = if MAX_COLS >= min_width.0 + min_width.1 + min_width.2 {
            widths = min_width;
            2
        } else {
            let width = MAX_COLS - 3 - min_width.0;
            widths = (
                min_width.0,
                cmp::min(min_width.1, width / 3),
                cmp::min(min_width.2, width / 3),
            );
            1
        };
        // Populate `CellBuffer` with every entry.
        for (idx, y) in (0..=self.length).enumerate() {
            if idx >= self.length {
                /* No more entries left, so fill the rest of the area with empty space */
                clear_area(&mut self.content, ((0, y), (MAX_COLS - 1, self.length)));
                break;
            }
            /* Write an entire line for each envelope entry. */
            let envelope: &Envelope = &account.get_env(&self.local_collection[idx]);

            let fg_color = if !envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if context.settings.terminal.theme == "light" {
                if !envelope.is_seen() {
                    Color::Byte(251)
                } else if idx % 2 == 0 {
                    Color::Byte(252)
                } else {
                    Color::Default
                }
            } else {
                if !envelope.is_seen() {
                    Color::Byte(251)
                } else if idx % 2 == 0 {
                    Color::Byte(236)
                } else {
                    Color::Default
                }
            };
            let (x, _) = write_string_to_grid(
                &rows[idx].0,
                &mut self.content,
                fg_color,
                bg_color,
                Attr::Default,
                ((0, idx), (widths.0, idx)),
                false,
            );
            for x in x..=widths.0 + column_sep {
                self.content[(x, idx)].set_bg(bg_color);
            }
            let mut _x = widths.0 + column_sep;
            let (x, _) = write_string_to_grid(
                &rows[idx].2,
                &mut self.content,
                fg_color,
                bg_color,
                Attr::Default,
                ((_x, idx), (widths.1 + _x, idx)),
                false,
            );
            _x += widths.1 + column_sep + 1;
            for x in x.._x {
                self.content[(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &rows[idx].1,
                &mut self.content,
                fg_color,
                bg_color,
                Attr::Default,
                ((_x, idx), (widths.1 + _x, idx)),
                false,
            );
            _x += widths.1 + column_sep + 2;
            for x in x.._x {
                self.content[(x, idx)].set_bg(bg_color);
            }
            let (x, _) = write_string_to_grid(
                &rows[idx].3,
                &mut self.content,
                fg_color,
                bg_color,
                Attr::Default,
                ((_x, idx), (widths.2 + _x, idx)),
                false,
            );

            for x in x..MAX_COLS {
                self.content[(x, y)].set_ch(' ');
                self.content[(x, y)].set_bg(bg_color);
            }
        }
    }

    fn unhighlight_line(&mut self, idx: usize, context: &Context) {
        let fg_color = Color::Default;
        let bg_color = if context.settings.terminal.theme == "light" {
            if idx % 2 == 0 {
                Color::Byte(252)
            } else {
                Color::Default
            }
        } else {
            if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            }
        };
        change_colors(
            &mut self.content,
            ((0, idx), (MAX_COLS - 1, idx)),
            fg_color,
            bg_color,
        );
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
            let must_unhighlight = {
                if self.length == 0 {
                    false
                } else {
                    let account = &mut context.accounts[self.cursor_pos.0];
                    let envelope: &mut Envelope =
                        &mut account.get_env_mut(&self.local_collection[idx]);
                    !envelope.is_seen()
                }
            };
            if must_unhighlight {
                self.unhighlight_line(idx, context);
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
                Action::ViewMailbox(idx) => {
                    self.new_cursor_pos.1 = *idx;
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

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
