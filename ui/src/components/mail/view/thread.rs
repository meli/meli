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

pub struct ThreadView {
    dirty: bool,
    coordinates: (usize, usize, usize),

}

impl ThreadView {
    pub fn new(coordinates: (usize, usize, usize),
    ) -> Self {
        ThreadView {
            dirty: true,
            coordinates,
        }
    }
}

impl fmt::Display for ThreadView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "view thread")
    }
}

impl Component for ThreadView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let mailbox = &mut context.accounts[self.coordinates.0][self.coordinates.1].as_ref().unwrap();
        let threads = &mailbox.threads;
        let container = &threads.containers()[threads.root_set()[self.coordinates.2]];
        let i = if let Some(i) = container.message() {
            i
        } else {
            threads.containers()[
                container.first_child().unwrap()
            ].message().unwrap()
        };
        let envelope: &Envelope = &mailbox.collection[i];
        let (x, y) = write_string_to_grid(
            &format!("Date: {}", envelope.date_as_str()),
            grid,
            Color::Byte(33),
            Color::Default,
            area,
            true,
            );
        for x in x..=get_x(bottom_right) {
            grid[(x, y)].set_ch(' ');
            grid[(x, y)].set_bg(Color::Default);
            grid[(x, y)].set_fg(Color::Default);
        }
        let (x, y) = write_string_to_grid(
            &format!("From: {}", envelope.from_to_string()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y + 1), bottom_right),
            true,
            );
        for x in x..=get_x(bottom_right) {
            grid[(x, y)].set_ch(' ');
            grid[(x, y)].set_bg(Color::Default);
            grid[(x, y)].set_fg(Color::Default);
        }
        let (x, y) = write_string_to_grid(
            &format!("To: {}", envelope.to_to_string()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y + 1), bottom_right),
            true,
            );
        for x in x..=get_x(bottom_right) {
            grid[(x, y)].set_ch(' ');
            grid[(x, y)].set_bg(Color::Default);
            grid[(x, y)].set_fg(Color::Default);
        }
        let (x, y) = write_string_to_grid(
            &format!("Subject: {}", envelope.subject()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y + 1), bottom_right),
            true,
            );
        for x in x..=get_x(bottom_right) {
            grid[(x, y)].set_ch(' ');
            grid[(x, y)].set_bg(Color::Default);
            grid[(x, y)].set_fg(Color::Default);
        }
        let (x, y) = write_string_to_grid(
            &format!("Message-ID: <{}>", envelope.message_id_raw()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y + 1), bottom_right),
            true,
            );
        for x in x..=get_x(bottom_right) {
            grid[(x, y)].set_ch(' ');
            grid[(x, y)].set_bg(Color::Default);
            grid[(x, y)].set_fg(Color::Default);
        }
        clear_area(grid, (set_y(upper_left, y + 1), set_y(bottom_right, y + 2)));
        context
            .dirty_areas
            .push_back((upper_left, set_y(bottom_right, y + 1)));
    }
    fn process_event(&mut self, _event: &UIEvent, _context: &mut Context) {
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}
