/*
 * meli - contacts module
 *
 * Copyright 2019 Manos Pitsidianakis
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

use melib::{AddressBook, Card};

#[derive(Debug)]
pub struct ContactManager {
    content: CellBuffer,
    dirty: bool,
    initialized: bool,
}

impl Default for ContactManager {
    fn default() -> Self {
        ContactManager {
            content: CellBuffer::default(),
            dirty: true,
            initialized: false,
        }
    }
}

impl fmt::Display for ContactManager {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "contacts")
    }
}

impl ContactManager {
}

impl Component for ContactManager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized {
            clear_area(grid, area);
            self.initialized = true;
        }
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
        self.initialized = false;
    }

    fn kill(&mut self, uuid: Uuid) {
    }
}
