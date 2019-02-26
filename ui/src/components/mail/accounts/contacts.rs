/*
 * meli - ui crate.
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

#[derive(Debug)]
pub struct ContactsPanel {
    content: CellBuffer,
    dirty: bool,
}

impl fmt::Display for ContactsPanel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "contacts")
    }
}


impl Component for ContactsPanel {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            self.dirty = false;
        }
        clear_area(grid, area);

        let (width, height) = self.content.size();
        copy_area(grid, &self.content, area, ((0, 0), (width - 1, height - 1)));
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}

impl ContactsPanel {
    pub fn new(context: &Context) -> ContactsPanel {
        let mut content = CellBuffer::new(120, 25 + context.accounts.len() * 20, Cell::default());
        write_string_to_grid(
            "Contacts",
            &mut content,
            Color::Default,
            Color::Default,
            ((2, 3), (120 - 1, 3)),
            true,
            );

        for (i, a) in context.accounts.iter().enumerate() {
            create_box(&mut content, ((2,5+i*10 ), (120-1, 15+i*10)));
            let (x, y) = write_string_to_grid(
                a.name(),
                &mut content,
                Color::Default,
                Color::Default,
                ((3, 5 + i*10), (120 - 2, 5 + i*10)),
                true,
                );
            write_string_to_grid(
                " ▒██▒ ",
                &mut content,
                Color::Byte(32),
                Color::Default,
                ((x, y), (120 - 2, 5 + i*10)),
                true,
                );
            write_string_to_grid(
                &a.runtime_settings.account().identity,
                &mut content,
                Color::Default,
                Color::Default,
                ((4, y + 2), (120 - 2, y + 2)),
                true,
                );

        }

        ContactsPanel {
            content,
            dirty: true,
        }
    }
}
