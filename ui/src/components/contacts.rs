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
    form: FormWidget,
    account_pos: usize,
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
            form: FormWidget::default(),
            account_pos: 0,
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
        self.form = FormWidget::new("Save".into());
        self.form.add_button(("Cancel".into(), false));
        self.form.push(("First Name".into(), self.card.firstname().to_string()));
        self.form.push(("Last Name".into(), self.card.lastname().to_string()));
        self.form.push(("Additional Name".into(), self.card.additionalname().to_string()));
        self.form.push(("Name Prefix".into(), self.card.name_prefix().to_string()));
        self.form.push(("Name Suffix".into(), self.card.name_suffix().to_string()));
        self.form.push(("E-mail".into(), self.card.email().to_string()));
        self.form.push(("url".into(), self.card.url().to_string()));
        self.form.push(("key".into(), self.card.key().to_string()));
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
        copy_area(grid, &self.content, area, ((0, 0), (width - 1, 0)));

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.form.draw(grid, (set_y(upper_left, get_y(upper_left) + 1), bottom_right), context);
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.form.process_event(event, context) {
            match self.form.buttons_result() {
                None => {},
                Some(true) => {
                    let mut new_card = Card::from(std::mem::replace(&mut self.form, FormWidget::default()).collect().unwrap());
                    new_card.set_id(*self.card.id());
                    context.accounts[self.account_pos].address_book.add_card(new_card);
                    context.replies.push_back(UIEvent {
                        id: 0,
                        event_type: UIEventType::StatusEvent(StatusEvent::DisplayMessage("Saved.".into())),
                    });
                    context.replies.push_back(UIEvent {
                        id: 0,
                        event_type: UIEventType::EntityKill(self.id),
                    });
                },
                Some(false) => {
                    context.replies.push_back(UIEvent {
                        id: 0,
                        event_type: UIEventType::EntityKill(self.id),
                    });

                },
            }
            return true;
        }
        /*
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
           */
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty | self.form.is_dirty()
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
        self.initialized = false;
        self.form.set_dirty();
    }

    fn set_id(&mut self, uuid: Uuid) {
        self.id = uuid;
    }
}
