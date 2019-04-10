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

/*! Entities that handle Mail specific functions.
 */
use super::*;

pub mod index;
pub use self::index::*;

#[derive(Debug)]
struct MenuEntry {
    name: String,
    subentries: Vec<MenuEntry>,
    index: Index,
}

#[derive(Debug)]
pub struct Indexer {
    entries: Vec<MenuEntry>,
    dirty: bool,
    cursor: Vec<usize>,
    id: ComponentId,
}

impl fmt::Display for Indexer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "index")
    }
}

impl Default for Indexer {
    fn default() -> Self {
        Indexer {
            entries: Vec::with_capacity(8),
            dirty: true,
            cursor: Vec::with_capacity(8),
            id: ComponentId::new_v4(),
        }
    }
}

impl Indexer {
    fn draw_menu(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}
}

impl Component for Indexer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }

        clear_area(grid, area);
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let total_cols = get_x(bottom_right) - get_x(upper_left);
        let index_entity_width = (30 * total_cols) / 100;
        let mid = get_x(bottom_right) - index_entity_width;
        for i in get_y(upper_left)..=get_y(bottom_right) {
            set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
        }

        let left_menu_area = (upper_left, (set_x(bottom_right, mid - 1)));
        let right_index_area = (set_x(upper_left, mid + 1), bottom_right);

        self.draw_menu(grid, left_menu_area, context);
        self.entries[self.cursor[0]]
            .index
            .draw(grid, right_index_area, context);

        self.dirty = false;
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        if !self.entries[self.cursor[0]]
            .index
            .process_event(event, _context)
        {
            for i in 0..self.entries.len() {
                if i == self.cursor[0] {
                    continue;
                }
                self.entries[i].index.process_event(event, _context);
            }
        }

        match *event {
            UIEvent::RefreshMailbox(_) => {
                self.dirty = true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
