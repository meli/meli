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
use std::collections::HashMap;

mod contact_list;

pub use self::contact_list::*;

#[derive(Debug)]
enum ViewMode {
    ReadOnly,
    Discard(UIDialog<char>),
    Edit,
    //New,
}

#[derive(Debug)]
pub struct ContactManager {
    id: ComponentId,
    parent_id: ComponentId,
    pub card: Card,
    mode: ViewMode,
    form: FormWidget<bool>,
    account_pos: usize,
    content: CellBuffer,
    theme_default: ThemeAttribute,
    dirty: bool,
    has_changes: bool,

    initialized: bool,
}

impl fmt::Display for ContactManager {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "contacts")
    }
}

impl ContactManager {
    fn new(context: &Context) -> Self {
        let theme_default: ThemeAttribute = crate::conf::value(context, "theme_default");
        let default_cell = {
            let mut ret = Cell::with_char(' ');
            ret.set_fg(theme_default.fg)
                .set_bg(theme_default.bg)
                .set_attrs(theme_default.attrs);
            ret
        };
        ContactManager {
            id: Uuid::nil(),
            parent_id: Uuid::nil(),
            card: Card::new(),
            mode: ViewMode::Edit,
            form: FormWidget::default(),
            account_pos: 0,
            content: CellBuffer::new(100, 1, default_cell),
            theme_default,
            dirty: true,
            has_changes: false,
            initialized: false,
        }
    }

    fn initialize(&mut self) {
        let (width, _) = self.content.size();

        let (x, _) = write_string_to_grid(
            "Last edited: ",
            &mut self.content,
            Color::Byte(250),
            self.theme_default.bg,
            self.theme_default.attrs,
            ((0, 0), (width - 1, 0)),
            None,
        );
        let (x, y) = write_string_to_grid(
            &self.card.last_edited(),
            &mut self.content,
            Color::Byte(250),
            self.theme_default.bg,
            self.theme_default.attrs,
            ((x, 0), (width - 1, 0)),
            None,
        );

        if self.card.external_resource() {
            self.mode = ViewMode::ReadOnly;
            let _ = self
                .content
                .resize(self.content.size().0, 2, Cell::default());
            write_string_to_grid(
                "This contact's origin is external and cannot be edited within meli.",
                &mut self.content,
                Color::Byte(250),
                self.theme_default.bg,
                self.theme_default.attrs,
                ((x, y), (width - 1, y)),
                None,
            );
        }

        self.form = FormWidget::new(("Save".into(), true));
        self.form.add_button(("Cancel(Esc)".into(), false));
        self.form
            .push(("NAME".into(), self.card.name().to_string().into()));
        self.form.push((
            "ADDITIONAL NAME".into(),
            self.card.additionalname().to_string().into(),
        ));
        self.form.push((
            "NAME PREFIX".into(),
            self.card.name_prefix().to_string().into(),
        ));
        self.form.push((
            "NAME SUFFIX".into(),
            self.card.name_suffix().to_string().into(),
        ));
        self.form
            .push(("E-MAIL".into(), self.card.email().to_string().into()));
        self.form
            .push(("URL".into(), self.card.url().to_string().into()));
        self.form
            .push(("KEY".into(), self.card.key().to_string().into()));
        for (k, v) in self.card.extra_properties() {
            self.form.push((k.to_string().into(), v.to_string().into()));
        }
    }

    pub fn set_parent_id(&mut self, new_val: ComponentId) {
        self.parent_id = new_val;
    }
}

impl Component for ContactManager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized {
            self.initialize();
            self.initialized = true;
        }

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        if self.dirty {
            let (width, _height) = self.content.size();
            clear_area(
                grid,
                (upper_left, set_y(bottom_right, get_y(upper_left) + 1)),
                self.theme_default,
            );
            copy_area_with_break(grid, &self.content, area, ((0, 0), (width - 1, 0)));
            self.dirty = false;
        }

        self.form.draw(
            grid,
            (set_y(upper_left, get_y(upper_left) + 2), bottom_right),
            context,
        );
        match self.mode {
            ViewMode::Discard(ref mut selector) => {
                /* Let user choose whether to quit with/without saving or cancel */
                selector.draw(grid, area, context);
            }
            _ => {}
        }

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match self.mode {
            ViewMode::Discard(ref mut selector) => {
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            ViewMode::Edit => {
                if let &mut UIEvent::Input(Key::Esc) = event {
                    if self.can_quit_cleanly(context) {
                        context
                            .replies
                            .push_back(UIEvent::Action(Tab(Kill(self.parent_id))));
                    }
                    return true;
                }
                if self.form.process_event(event, context) {
                    match self.form.buttons_result() {
                        None => {}
                        Some(true) => {
                            let fields = std::mem::replace(&mut self.form, FormWidget::default())
                                .collect()
                                .unwrap();
                            let fields: HashMap<String, String> = fields
                                .into_iter()
                                .map(|(s, v)| {
                                    (
                                        s.to_string(),
                                        match v {
                                            Field::Text(v, _) => v.as_str().to_string(),
                                            Field::Choice(mut v, c) => v.remove(c).to_string(),
                                        },
                                    )
                                })
                                .collect();
                            let mut new_card = Card::from(fields);
                            new_card.set_id(*self.card.id());
                            context.accounts[self.account_pos]
                                .address_book
                                .add_card(new_card);
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage("Saved.".into()),
                            ));
                            context.replies.push_back(UIEvent::ComponentKill(self.id));
                        }
                        Some(false) => {
                            context.replies.push_back(UIEvent::ComponentKill(self.id));
                        }
                    }
                    self.set_dirty(true);
                    if let UIEvent::InsertInput(_) = event {
                        self.has_changes = true;
                    }
                    return true;
                }
            }
            ViewMode::ReadOnly => {
                if let &mut UIEvent::Input(Key::Esc) = event {
                    if self.can_quit_cleanly(context) {
                        context.replies.push_back(UIEvent::ComponentKill(self.id));
                    }
                    return true;
                }
            }
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.form.is_dirty()
            || if let ViewMode::Discard(ref selector) = self.mode {
                selector.is_dirty()
            } else {
                false
            }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.form.set_dirty(value);
        if let ViewMode::Discard(ref mut selector) = self.mode {
            selector.set_dirty(value);
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if !self.has_changes {
            return true;
        }

        let parent_id = self.parent_id;
        /* Play it safe and ask user for confirmation */
        self.mode = ViewMode::Discard(UIDialog::new(
            "this contact has unsaved changes",
            vec![
                ('x', "quit without saving".to_string()),
                ('y', "save draft and quit".to_string()),
                ('n', "cancel".to_string()),
            ],
            true,
            Some(Box::new(move |_, results: &[char]| match results[0] {
                'x' => Some(UIEvent::Action(Tab(Kill(parent_id)))),
                'n' => None,
                'y' => None,
                _ => None,
            })),
            context,
        ));
        self.set_dirty(true);
        false
    }
}
