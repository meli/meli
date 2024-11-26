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

use std::borrow::Cow;

use indexmap::IndexMap;
use melib::Card;

use crate::{
    terminal::*,
    utilities::{FormButtonAction, FormWidget},
    CellBuffer, Component, ComponentId, Context, Field, Key, StatusEvent, ThemeAttribute, UIDialog,
    UIEvent,
};

#[derive(Debug)]
enum ViewMode {
    ReadOnly,
    Discard(Box<UIDialog<char>>),
    Edit,
}

#[derive(Debug)]
pub struct ContactManager {
    id: ComponentId,
    parent_id: Option<ComponentId>,
    pub card: Card,
    mode: ViewMode,
    form: FormWidget<FormButtonAction>,
    pub account_pos: usize,
    content: Screen<Virtual>,
    theme_default: ThemeAttribute,
    dirty: bool,
    has_changes: bool,
    initialized: bool,
}

impl std::fmt::Display for ContactManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.card)
    }
}

impl ContactManager {
    pub fn new(context: &Context) -> Self {
        let theme_default: ThemeAttribute = crate::conf::value(context, "theme_default");
        Self {
            id: ComponentId::default(),
            parent_id: None,
            card: Card::new(),
            mode: ViewMode::Edit,
            form: FormWidget::default(),
            account_pos: 0,
            content: Screen::<Virtual>::new(theme_default),
            theme_default,
            dirty: true,
            has_changes: false,
            initialized: false,
        }
    }

    fn initialize(&mut self, context: &Context) {
        if !self.content.resize_with_context(100, 1, context) {
            return;
        }
        let area = self.content.area();

        if self.card.external_resource() {
            self.mode = ViewMode::ReadOnly;
            self.content.grid_mut().write_string(
                "This contact's origin is external and cannot be edited within meli.",
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                area,
                None,
                None,
            );
        } else {
            let (x, _) = self.content.grid_mut().write_string(
                "Last edited: ",
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                area,
                None,
                None,
            );
            self.content.grid_mut().write_string(
                &self.card.last_edited(),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                area.skip_cols(x),
                None,
                None,
            );
        }

        self.form = FormWidget::new(
            if self.card.external_resource() {
                ("Cancel(Esc)".into(), FormButtonAction::Cancel)
            } else {
                ("Save".into(), FormButtonAction::Accept)
            },
            /* cursor_up_shortcut */ context.settings.shortcuts.general.scroll_up.clone(),
            /* cursor_down_shortcut */
            context.settings.shortcuts.general.scroll_down.clone(),
        );
        if !self.card.external_resource() {
            self.form
                .add_button(("Export".into(), FormButtonAction::Other("Export")));
            self.form
                .add_button(("Cancel(Esc)".into(), FormButtonAction::Cancel));
        }
        self.form
            .push(("NAME".into(), self.card.name().to_string()));
        self.form.push((
            "ADDITIONAL NAME".into(),
            self.card.additionalname().to_string(),
        ));
        self.form
            .push(("NAME PREFIX".into(), self.card.name_prefix().to_string()));
        self.form
            .push(("NAME SUFFIX".into(), self.card.name_suffix().to_string()));
        self.form
            .push(("E-MAIL".into(), self.card.email().to_string()));
        self.form.push(("URL".into(), self.card.url().to_string()));
        self.form.push(("KEY".into(), self.card.key().to_string()));
        for (k, v) in self.card.extra_properties() {
            self.form.push((k.to_string().into(), v.to_string()));
        }
    }

    pub fn set_parent_id(&mut self, new_val: ComponentId) {
        self.parent_id = Some(new_val);
    }
}

impl Component for ContactManager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized {
            self.initialize(context);
            self.initialized = true;
        }

        if self.is_dirty() {
            grid.clear_area(area, self.theme_default);
            grid.copy_area(self.content.grid(), area, self.content.area());
            self.dirty = false;
        }

        self.form
            .draw(grid, area.skip_rows(self.content.area().height()), context);
        if let ViewMode::Discard(ref mut selector) = self.mode {
            /* Let user choose whether to quit with/without saving or cancel */
            selector.draw(grid, area, context);
        }

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::value(context, "theme_default");
            self.content.grid_mut().empty();
            self.initialized = false;
            self.set_dirty(true);
        }
        match self.mode {
            ViewMode::Discard(ref mut selector) => {
                if matches!(event, UIEvent::ComponentUnrealize(ref id) if *id == selector.id()) {
                    selector.unrealize(context);
                    self.mode = ViewMode::Edit;
                    self.set_dirty(true);
                    return true;
                }
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            ViewMode::Edit => {
                if matches!(event, UIEvent::Input(Key::Esc)) {
                    if self.can_quit_cleanly(context) {
                        self.unrealize(context);
                    }
                    return true;
                }
                if self.form.process_event(event, context) {
                    match self.form.buttons_result() {
                        None => {}
                        Some(FormButtonAction::Accept) => {
                            let fields = std::mem::take(&mut self.form).collect();
                            let fields: IndexMap<String, String> = fields
                                .into_iter()
                                .map(|(s, v)| {
                                    (
                                        s.to_string(),
                                        match v {
                                            Field::Text(v) => v.as_str().to_string(),
                                            Field::Choice(mut v, c, _) => v.remove(c).to_string(),
                                        },
                                    )
                                })
                                .collect();
                            let mut new_card = Card::from(fields);
                            new_card.set_id(*self.card.id());
                            context.accounts[self.account_pos]
                                .contacts
                                .add_card(new_card);
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage("Saved.".into()),
                            ));
                            self.unrealize(context);
                        }
                        Some(FormButtonAction::Cancel) => {
                            self.unrealize(context);
                        }
                        Some(FormButtonAction::Other("Export")) => {
                            let card = if self.has_changes {
                                let fields = self.form.clone().collect();
                                let fields: IndexMap<String, String> = fields
                                    .into_iter()
                                    .map(|(s, v)| {
                                        (
                                            s.to_string(),
                                            match v {
                                                Field::Text(v) => v.as_str().to_string(),
                                                Field::Choice(mut v, c, _) => {
                                                    v.remove(c).to_string()
                                                }
                                            },
                                        )
                                    })
                                    .collect();
                                let mut card = Card::from(fields);
                                card.set_id(*self.card.id());
                                Cow::Owned(card)
                            } else {
                                Cow::Borrowed(&self.card)
                            };
                            super::export_to_vcard(&card, self.account_pos, context);
                            return true;
                        }
                        Some(FormButtonAction::Reset | FormButtonAction::Other(_)) => {}
                    }
                    self.set_dirty(true);
                    if matches!(event, UIEvent::InsertInput(_)) {
                        self.has_changes = true;
                    }
                    return true;
                }
            }
            ViewMode::ReadOnly => {
                if matches!(event, UIEvent::Input(Key::Esc)) {
                    if self.can_quit_cleanly(context) {
                        self.unrealize(context);
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
            || matches!(self.mode,  ViewMode::Discard(ref selector) if selector.is_dirty())
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

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if !self.has_changes {
            return true;
        }

        if matches!(self.mode, ViewMode::Discard(_)) {
            true
        } else {
            let Some(parent_id) = self.parent_id else {
                return true;
            };
            /* Play it safe and ask user for confirmation */
            self.mode = ViewMode::Discard(Box::new(UIDialog::new(
                "this contact has unsaved changes",
                vec![
                    ('y', "quit without saving".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
                Some(Box::new(move |id, results: &[char]| {
                    if matches!(results.first(), Some(&'y')) {
                        Some(UIEvent::ComponentUnrealize(parent_id))
                    } else {
                        Some(UIEvent::ComponentUnrealize(id))
                    }
                })),
                context,
            )));
            self.set_dirty(true);
            false
        }
    }
}
