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

use std::{borrow::Cow, sync::Arc};

use futures::channel::oneshot;
use indexmap::IndexMap;
use melib::{
    contacts::{AddressBook, AddressBookName, Card},
    error::Result,
    AccountHash, ContactBackendID, Error,
};

use crate::{
    conf::themes::ThemeAttribute,
    contacts::export::ContactExportDialog,
    jobs::JobId,
    terminal::*,
    types::NotificationType,
    utilities::form::{FormButtonAction, TextField},
    CellBuffer, Component, ComponentId, Context, Key, StatusEvent, UIDialog, UIEvent,
};

type FormWidget =
    crate::utilities::form::FormWidget<FormButtonAction, Cow<'static, str>, TextField>;

#[derive(Debug)]
enum ViewMode {
    ReadOnly,
    Discard {
        dialog: Box<UIDialog<char>>,
        create: bool,
    },
    WaitingForSaveResult {
        dialog: Box<UIDialog<char>>,
        receiver: oneshot::Receiver<Result<()>>,
        job_id: JobId,
        create: bool,
    },
    Edit {
        create: bool,
    },
}

#[derive(Debug)]
pub struct ContactEditor {
    id: ComponentId,
    card: Card,
    mode: ViewMode,
    form: FormWidget,
    account_hash: AccountHash,
    book_id: (Arc<ContactBackendID>, AddressBookName),
    content: Screen<Virtual>,
    theme_default: ThemeAttribute,
    dirty: bool,
    has_changes: bool,
    initialized: bool,
}

impl std::fmt::Display for ContactEditor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.card)
    }
}

impl ContactEditor {
    pub fn create_card(
        account_hash: AccountHash,
        book_id: (Arc<ContactBackendID>, AddressBookName),
        context: &Context,
    ) -> Self {
        let book: &AddressBook = &context.accounts[&account_hash].contacts.books[&book_id];
        let mode = if book.read_only {
            ViewMode::ReadOnly
        } else {
            ViewMode::Edit { create: true }
        };
        let theme_default: ThemeAttribute = crate::conf::theme_value(context, "theme_default");
        Self {
            id: ComponentId::default(),
            card: Card::new(),
            mode,
            form: FormWidget::default(),
            account_hash,
            book_id,
            content: Screen::<Virtual>::new(theme_default),
            theme_default,
            dirty: true,
            has_changes: false,
            initialized: false,
        }
    }

    pub fn edit_card(
        account_hash: AccountHash,
        book_id: (Arc<ContactBackendID>, AddressBookName),
        context: &Context,
        card: Card,
    ) -> Self {
        let book: &AddressBook = &context.accounts[&account_hash].contacts.books[&book_id];
        let mode = if book.read_only {
            ViewMode::ReadOnly
        } else {
            ViewMode::Edit { create: false }
        };
        let theme_default: ThemeAttribute = crate::conf::theme_value(context, "theme_default");
        Self {
            id: ComponentId::default(),
            card,
            mode,
            form: FormWidget::default(),
            account_hash,
            book_id,
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

        let is_create = matches!(
            self.mode,
            ViewMode::Discard { create: true, .. }
                | ViewMode::WaitingForSaveResult { create: true, .. }
                | ViewMode::Edit { create: true }
        );
        if matches!(self.mode, ViewMode::ReadOnly) {
            self.content.grid_mut().write_string(
                "This contact is in a read-only address book.",
                &self.theme_default,
                area,
                None,
                None,
            );
        } else if is_create {
            self.content.grid_mut().write_string(
                "New contact",
                &self.theme_default,
                area,
                None,
                None,
            );
        } else {
            let (x, _) = self.content.grid_mut().write_string(
                "Last edited: ",
                &self.theme_default,
                area,
                None,
                None,
            );
            self.content.grid_mut().write_string(
                &self.card.last_edited(),
                &self.theme_default,
                area.skip_cols(x),
                None,
                None,
            );
        }

        self.form = FormWidget::new(
            if self.card.external_resource() {
                ("Cancel(Esc)".into(), FormButtonAction::Cancel)
            } else {
                (
                    if is_create {
                        "Create".into()
                    } else {
                        "Save".into()
                    },
                    FormButtonAction::Accept,
                )
            },
            // cursor_up_shortcut
            context.settings.shortcuts.general.scroll_up.clone(),
            // cursor_down_shortcut
            context.settings.shortcuts.general.scroll_down.clone(),
            // cursor_right_shortcut
            context.settings.shortcuts.general.scroll_right.clone(),
            // cursor_left_shortcut
            context.settings.shortcuts.general.scroll_left.clone(),
        );
        if !self.card.external_resource() {
            if !is_create {
                self.form
                    .add_button(("Export".into(), FormButtonAction::Other("Export")));
            }
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
}

impl Component for ContactEditor {
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
        if let ViewMode::Discard { ref mut dialog, .. } = self.mode {
            // Let user choose whether to quit with/without saving or cancel
            dialog.draw(grid, area, context);
        }

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::theme_value(context, "theme_default");
            self.content.grid_mut().empty();
            self.initialized = false;
            self.set_dirty(true);
        }
        match self.mode {
            ViewMode::Discard {
                ref mut dialog,
                ref create,
            } => {
                if matches!(event, UIEvent::ComponentUnrealize(ref id) if *id == dialog.id()) {
                    let create = *create;
                    dialog.unrealize(context);
                    self.mode = ViewMode::Edit { create };
                    self.set_dirty(true);
                    return true;
                }
                if dialog.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            ViewMode::Edit { create } => {
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
                            let fields: IndexMap<String, String> = self
                                .form
                                .values()
                                .clone()
                                .into_iter()
                                .map(|(s, v)| (s.to_string(), v.as_str().to_owned()))
                                .collect();
                            let mut card = Card::from(fields);
                            card.set_id(*self.card.id());
                            let res = if create {
                                context.accounts[&self.account_hash]
                                    .add_card(self.book_id.clone(), card)
                            } else {
                                context.accounts[&self.account_hash]
                                    .edit_card(self.book_id.clone(), card)
                            };
                            match res {
                                Ok((job_id, receiver)) => {
                                    let dialog = UIDialog::new(
                                        "Waiting for confirmation. The tab will close \
                                         automatically on successful creation.",
                                        vec![
                                            ('c', "force close tab".to_string()),
                                            (
                                                'n',
                                                "close this message and return to edit mode"
                                                    .to_string(),
                                            ),
                                        ],
                                        true,
                                        Some(Box::new(
                                            move |id: ComponentId, results: &[char], context| {
                                                context.replies.push_back(
                                                    UIEvent::FinishedUIDialog(
                                                        id,
                                                        Box::new(
                                                            results.first().cloned().unwrap_or('c'),
                                                        ),
                                                    ),
                                                );
                                            },
                                        )),
                                        context,
                                    )
                                    .into();
                                    self.mode = ViewMode::WaitingForSaveResult {
                                        dialog,
                                        job_id,
                                        receiver,
                                        create,
                                    };
                                }
                                Err(err) => {
                                    self.mode = ViewMode::Edit { create };
                                    context.replies.push_back(UIEvent::Notification {
                                        title: None,
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                    });
                                    self.initialized = false;
                                    self.set_dirty(true);
                                }
                            }
                            self.set_dirty(true);
                        }
                        Some(FormButtonAction::Cancel) => {
                            self.unrealize(context);
                        }
                        Some(FormButtonAction::Other("Export")) => {
                            let card = if self.has_changes {
                                let fields = self.form.clone().collect();
                                let fields: IndexMap<String, String> = fields
                                    .into_iter()
                                    .map(|(s, v)| (s.to_string(), v.as_str().to_owned()))
                                    .collect();
                                let mut card = Card::from(fields);
                                card.set_id(*self.card.id());
                                card
                            } else {
                                self.card.clone()
                            };
                            context.replies.push_back(UIEvent::GlobalUIDialog {
                                value: Box::new(ContactExportDialog::new(card, context)),
                                parent: Some(self.id),
                            });
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
            ViewMode::WaitingForSaveResult {
                ref mut dialog,
                ref mut receiver,
                job_id,
                create,
            } => {
                match event {
                    UIEvent::FinishedUIDialog(id, result) if *id == dialog.id() => {
                        if let Some(key) = result.downcast_mut::<char>() {
                            match key {
                                'c' => {
                                    self.unrealize(context);
                                    return true;
                                }
                                'n' => {
                                    self.set_dirty(true);
                                    self.mode = ViewMode::Edit { create };
                                }
                                _ => {}
                            }
                        }
                    }
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref other_job_id))
                        if *other_job_id == job_id =>
                    {
                        match receiver.try_recv().map_err(
                            |_: futures::channel::oneshot::Canceled| Error::new("Job was canceled"),
                        ) {
                            Err(err) | Ok(Some(Err(err))) => {
                                self.mode = ViewMode::Edit { create };
                                context.replies.push_back(UIEvent::Notification {
                                    title: None,
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(NotificationType::Error(err.kind)),
                                });
                                self.set_dirty(true);
                            }
                            Ok(None) | Ok(Some(Ok(()))) => {
                                self.mode = ViewMode::Edit { create };
                                context.replies.push_back(UIEvent::Notification {
                                    title: Some("Contact saved.".into()),
                                    source: None,
                                    body: format!(
                                        "Card was {}.",
                                        if create { "created" } else { "saved" }
                                    )
                                    .into(),
                                    kind: Some(NotificationType::Info),
                                });
                                self.unrealize(context);
                            }
                        }
                    }
                    _ => {
                        if dialog.process_event(event, context) {
                            self.set_dirty(true);
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.form.is_dirty()
            || matches!(self.mode,  ViewMode::Discard {ref dialog, ..} if dialog.is_dirty())
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.form.set_dirty(value);
        if let ViewMode::Discard { ref mut dialog, .. } = self.mode {
            dialog.set_dirty(value);
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if !self.has_changes {
            return true;
        }

        match self.mode {
            ViewMode::Discard { .. } | ViewMode::ReadOnly => true,
            ViewMode::WaitingForSaveResult { .. } => false,
            ViewMode::Edit { create } => {
                let parent_id = self.id;
                // Ask user for confirmation
                self.mode = ViewMode::Discard {
                    dialog: Box::new(UIDialog::new(
                        "this contact has unsaved changes",
                        vec![
                            ('y', "quit without saving".to_string()),
                            ('n', "cancel".to_string()),
                        ],
                        true,
                        Some(Box::new(move |id, results: &[char], context| {
                            context
                                .replies
                                .push_back(if matches!(results.first(), Some(&'y')) {
                                    UIEvent::ComponentUnrealize(parent_id)
                                } else {
                                    UIEvent::ComponentUnrealize(id)
                                });
                        })),
                        context,
                    )),
                    create,
                };
                self.set_dirty(true);
                false
            }
        }
    }
}
