/*
 * meli - accounts module.
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
use std::fmt;

#[derive(Debug)]
pub struct AccountsPanel {
    cursor: usize,
    content: CellBuffer,
    dirty: bool,
    id: ComponentId,
}

impl fmt::Display for AccountsPanel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "accounts")
    }
}

impl Component for AccountsPanel {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            self.initialize(context);
            self.dirty = false;
        }
        clear_area(grid, area);

        let (width, height) = self.content.size();
        copy_area(grid, &self.content, area, ((0, 0), (width - 1, height - 1)));
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match *event {
            UIEvent::Input(Key::Up) => {
                self.cursor = self.cursor.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Down) => {
                if self.cursor + 1 < context.accounts.len() {
                    self.cursor += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Input(Key::Char('\n')) => {
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(TabOpen(Some(Box::new(
                        ContactList::for_account(self.cursor),
                    ))))));
                return true;
            }
            UIEvent::MailboxUpdate(_) => {
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

impl AccountsPanel {
    pub fn new(context: &Context) -> AccountsPanel {
        let content = CellBuffer::new(120, 25 + context.accounts.len() * 20, Cell::default());

        AccountsPanel {
            cursor: 0,
            content,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
    fn initialize(&mut self, context: &Context) {
        write_string_to_grid(
            "Accounts",
            &mut self.content,
            Color::Default,
            Color::Default,
            ((2, 3), (120 - 1, 3)),
            true,
        );

        for (i, a) in context.accounts.iter().enumerate() {
            for x in 2..(120 - 1) {
                set_and_join_box(&mut self.content, (x, 5 + i * 10), HORZ_BOUNDARY);
            }
            //create_box(&mut self.content, ((2, 5 + i * 10), (120 - 1, 15 + i * 10)));
            let (x, y) = write_string_to_grid(
                a.name(),
                &mut self.content,
                Color::Default,
                Color::Default,
                ((3, 5 + i * 10), (120 - 2, 5 + i * 10)),
                true,
            );
            write_string_to_grid(
                " ▒██▒ ",
                &mut self.content,
                Color::Byte(32),
                Color::Default,
                ((x, y), (120 - 2, 5 + i * 10)),
                true,
            );
            write_string_to_grid(
                &a.runtime_settings.account().identity,
                &mut self.content,
                Color::Default,
                Color::Default,
                ((4, y + 2), (120 - 2, y + 2)),
                true,
            );
            if i == self.cursor {
                for h in 1..8 {
                    self.content[(2, h + y + 1)].set_ch('*');
                }
            } else {
                for h in 1..8 {
                    self.content[(2, h + y + 1)].set_ch(' ');
                }
            }
            let (x, _) = write_string_to_grid(
                "- Settings",
                &mut self.content,
                Color::Default,
                Color::Default,
                ((5, y + 3), (120 - 2, y + 3)),
                true,
            );
            write_string_to_grid(
                &format!(
                    "total {}",
                    a.collection
                        .envelopes
                        .values()
                        .filter(|e| !e.is_seen())
                        .count()
                ),
                &mut self.content,
                Color::Default,
                Color::Default,
                ((10 + x, y + 3), (120 - 2, y + 3)),
                true,
            );
            write_string_to_grid(
                "- Contacts",
                &mut self.content,
                Color::Default,
                Color::Default,
                ((5, y + 4), (120 - 2, y + 4)),
                true,
            );
            write_string_to_grid(
                "- Mailing Lists",
                &mut self.content,
                Color::Default,
                Color::Default,
                ((5, y + 5), (120 - 2, y + 5)),
                true,
            );
        }
    }
}
