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

/*!
  Notification handling components.
  */
use notify_rust::Notification as notify_Notification;

use super::*;

/// Passes notifications to the OS using the XDG specifications.
pub struct XDGNotifications {}

impl Component for XDGNotifications {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}
    fn process_event(&mut self, event: &UIEvent, _context: &mut Context) {
        if let UIEventType::Notification(ref t) = event.event_type {
            notify_Notification::new()
                .summary("Refresh Event")
                .body(t)
                .icon("dialog-information")
                .show()
                .unwrap();
        }
    }
}
