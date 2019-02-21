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

mod contact_list;

pub use self::contact_list::*;

macro_rules! write_field {
    ($title:expr, $value:expr, $target_grid:expr, $fg_color:expr, $bg_color:expr, $width:expr, $y:expr) => {{
        let (x, y) = write_string_to_grid(
            $title,
            &mut $target_grid,
            $fg_color,
            $bg_color,
            ((1, $y + 2), ($width - 1, $y + 2)),
            false,
            );
        write_string_to_grid(
            &$value,
            &mut $target_grid,
            Color::Default,
            Color::Default,
            ((x, y), ($width - 1, y)),
            false,
            );
        y
    }}
}

#[derive(Debug)]
enum ViewMode {
    ReadOnly,
    Read,
    Edit,
    New,
}

#[derive(Debug)]
pub struct ContactManager {
    id: Uuid,
    pub card: Card,
    mode: ViewMode,
    content: CellBuffer,
    dirty: bool,
    initialized: bool,
}

impl Default for ContactManager {
    fn default() -> Self {
        ContactManager {
            id: Uuid::nil(),
            card: Card::new(),
            mode: ViewMode::Read,
            content: CellBuffer::new(200, 100, Cell::with_char(' ')),
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
    fn initialize(&mut self) {
        let (width, height) = self.content.size();

        let (x, y) = write_string_to_grid(
            "Contact Name  ",
            &mut self.content,
            Color::Byte(33),
            Color::Default,
            ((0, 0), (width, 0)),
            false,
            );
        let (x, y) = write_string_to_grid(
            "Last edited: ",
            &mut self.content,
            Color::Byte(250),
            Color::Default,
            ((x, 0), (width, 0)),
            false,
            );
        let (x, y) = write_string_to_grid(
            &self.card.last_edited(),
            &mut self.content,
            Color::Byte(250),
            Color::Default,
            ((x, 0), (width, 0)),
            false,
            );
        for x in 0..width {
            set_and_join_box(&mut self.content, (x, 2), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 4), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 6), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 8), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 10), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 12), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 14), HORZ_BOUNDARY);
            set_and_join_box(&mut self.content, (x, 16), HORZ_BOUNDARY);
        }
        for y in 0..height {
            set_and_join_box(&mut self.content, (width - 1, y), VERT_BOUNDARY);
        }
        let mut y = write_field!("First Name: ", self.card.firstname(), self.content, Color::Byte(250), Color::Default, width, 1);
        y = write_field!("Last Name: ", self.card.lastname(), self.content, Color::Byte(250), Color::Default, width, y);
        y = write_field!("Additional Name: ", self.card.additionalname(), self.content, Color::Byte(250), Color::Default, width, y);
        y = write_field!("Name Prefix: ", self.card.name_prefix(), self.content, Color::Byte(250), Color::Default, width, y);
        y = write_field!("Name Suffix: ", self.card.name_suffix(), self.content, Color::Byte(250), Color::Default, width, y);
        y = write_field!("E-mail: ", self.card.email(), self.content, Color::Byte(250), Color::Default, width, y);
        y = write_field!("url: ", self.card.url(), self.content, Color::Byte(250), Color::Default, width, y);
        y = write_field!("key: ", self.card.key(), self.content, Color::Byte(250), Color::Default, width, y);
    }
}

impl Component for ContactManager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized {
            self.initialize();
            self.initialized = true;
        }
        clear_area(grid, area);
        let (width, height) = self.content.size();
        copy_area(grid, &self.content, area, ((0, 0), (width - 1, height -1)));
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        match event.event_type {
            UIEventType::Input(Key::Char('\n')) => {
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::EntityKill(self.id),
                });
                return true;
            },
            _ => {},
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
        self.initialized = false;
    }

    fn set_id(&mut self, uuid: Uuid) {
        self.id = uuid;
    }
}
