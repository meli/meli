/*
 * meli
 *
 * Copyright 2020 Manos Pitsidianakis
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

#[derive(Debug)]
pub struct OfflineListing {
    cursor_pos: (AccountHash, MailboxHash),
    _row_updates: SmallVec<[ThreadHash; 8]>,
    _selection: HashMap<ThreadHash, bool>,
    dirty: bool,
    id: ComponentId,
}

impl MailListingTrait for OfflineListing {
    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]> {
        &mut self._row_updates
    }

    fn selection(&mut self) -> &mut HashMap<ThreadHash, bool> {
        &mut self._selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]> {
        return SmallVec::new();
    }

    fn refresh_mailbox(&mut self, _context: &mut Context, _force: bool) {}
    fn redraw_threads_list(
        &mut self,
        _context: &Context,
        _items: Box<dyn Iterator<Item = ThreadHash>>,
    ) {
    }
    fn redraw_envelope_list(
        &mut self,
        _context: &Context,
        _items: Box<dyn Iterator<Item = EnvelopeHash>>,
    ) {
    }
}

impl ListingTrait for OfflineListing {
    fn coordinates(&self) -> (AccountHash, MailboxHash) {
        self.cursor_pos
    }

    fn set_coordinates(&mut self, coordinates: (AccountHash, MailboxHash)) {
        self.cursor_pos = coordinates;
    }

    fn highlight_line(
        &mut self,
        _grid: &mut CellBuffer,
        _area: Area,
        _idx: usize,
        _context: &Context,
    ) {
    }

    fn draw_list(&mut self, _: &mut CellBuffer, _: Area, _: &mut Context) {}

    fn set_movement(&mut self, _: PageMovement) {}
}

impl fmt::Display for OfflineListing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl OfflineListing {
    pub fn new(cursor_pos: (AccountHash, MailboxHash)) -> Self {
        OfflineListing {
            cursor_pos,
            _row_updates: SmallVec::new(),
            _selection: HashMap::default(),
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
}

impl Component for OfflineListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }
        self.dirty = false;
        let theme_default = conf::value(context, "theme_default");
        clear_area(grid, area, theme_default);
        if let Err(err) = context.is_online(self.cursor_pos.0) {
            let (x, _) = write_string_to_grid(
                "offline: ",
                grid,
                Color::Byte(243),
                theme_default.bg,
                theme_default.attrs,
                area,
                None,
            );
            write_string_to_grid(
                &err.to_string(),
                grid,
                Color::Red,
                theme_default.bg,
                theme_default.attrs,
                (set_x(upper_left!(area), x + 1), bottom_right!(area)),
                None,
            );
        } else {
            let (_, mut y) = write_string_to_grid(
                "loading...",
                grid,
                Color::Byte(243),
                theme_default.bg,
                theme_default.attrs,
                area,
                None,
            );
            let mut jobs: SmallVec<[_; 64]> = context.accounts[&self.cursor_pos.0]
                .active_jobs
                .iter()
                .collect();
            jobs.sort_by_key(|(j, _)| *j);
            for (job_id, j) in jobs {
                write_string_to_grid(
                    &format!("{}: {:?}", job_id, j),
                    grid,
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    (set_y(upper_left!(area), y + 1), bottom_right!(area)),
                    None,
                );
                y += 1;
            }

            context
                .replies
                .push_back(UIEvent::AccountStatusChange(self.cursor_pos.0));
        }
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match event {
            UIEvent::AccountStatusChange(account_hash) if *account_hash == self.cursor_pos.0 => {
                self.dirty = true
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, _value: bool) {
        self.dirty = true;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
