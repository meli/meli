//
// meli
//
// Copyright 2026  Manos Pitsidianakis
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{path::PathBuf, sync::Arc};

use futures::channel::oneshot;
use melib::{
    contacts::Card,
    error::{Error, ErrorKind, Result},
    utils::shellexpand::ShellExpandTrait as _,
    AddressBookName, ContactBackendID,
};

use crate::{
    components::prelude::*,
    terminal::*,
    types::{File, NotificationType},
    utilities::{
        autocomplete::generic_file_path_complete_fn,
        form::{generic_file_path_validate_fn, Field, FormButtonAction},
    },
};

type FormWidget = crate::utilities::form::FormWidget<FormButtonAction, Cow<'static, str>, Field>;

#[derive(Debug)]
pub struct ContactExportDialog {
    id: ComponentId,
    card: Card,
    form: FormWidget,
    theme_default: ThemeAttribute,
    cached_area: Area,
    dirty: bool,
}

impl std::fmt::Display for ContactExportDialog {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "export {}?", self.card)
    }
}

impl ContactExportDialog {
    pub fn new(card: Card, context: &Context) -> Self {
        let theme_default: ThemeAttribute = crate::conf::theme_value(context, "theme_default");
        let mut form = FormWidget::new(
            ("Cancel(Esc)".into(), FormButtonAction::Cancel),
            // cursor_up_shortcut
            context.settings.shortcuts.general.scroll_up.clone(),
            // cursor_down_shortcut
            context.settings.shortcuts.general.scroll_down.clone(),
            // cursor_right_shortcut
            context.settings.shortcuts.general.scroll_right.clone(),
            // cursor_left_shortcut
            context.settings.shortcuts.general.scroll_left.clone(),
        );
        form.push_cl((
            "Path or directory".into(),
            String::new(),
            generic_file_path_complete_fn(),
        ));
        form.push_choices((
            "Format".into(),
            vec!["VCard v4".into(), "JSContact v1".into()],
        ));
        form.add_button(("Export".into(), FormButtonAction::Accept));
        form.set_padding(1);
        Self {
            id: ComponentId::default(),
            card,
            form,
            cached_area: Area::new_empty(Default::default()),
            theme_default,
            dirty: true,
        }
    }
}

impl Component for ContactExportDialog {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let area = area.center_inside((60, self.form.height() + 8));
        self.cached_area = area;
        grid.clear_area(area, self.theme_default);

        let form_area = create_box(grid, area).skip_cols(1).skip_cols_from_end(1);
        let mut y =
            grid.write_string(
                &format!("Export {}?", self.card),
                &self.theme_default,
                form_area,
                None,
                Some(0),
            )
            .1 + 2;
        y +=
            grid.write_string(
                &format!(
                    "Navigate options with {} to go down, {} to go up, select with {}, cancel \
                     with {}",
                    self.form.cursor_down_shortcut,
                    self.form.cursor_up_shortcut,
                    Key::Char('\n'),
                    Key::Esc
                ),
                &self.theme_default,
                form_area.skip_rows(y),
                None,
                Some(0),
            )
            .1 + 2;

        self.form.draw(grid, form_area.skip_rows(y), context);
        self.set_dirty(false);
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::theme_value(context, "theme_default");
            self.set_dirty(true);
        }
        if self.form.process_event(event, context) {
            match self.form.buttons_result() {
                None => {}
                Some(FormButtonAction::Accept) => {
                    let output_dir: String = self.form.values()["Path or directory"]
                        .as_text_field()
                        .expect("path field in form")
                        .as_str()
                        .to_owned();
                    if output_dir.is_empty() {
                        return true;
                    }
                    let card = &self.card;
                    let is_vcf: bool = {
                        let field = self.form.values()["Format"]
                            .as_choice_field()
                            .expect("format field in form");
                        field.values[field.cursor] == "VCard v4"
                    };
                    let output_dir = std::path::Path::new(&output_dir).expand();
                    let filename = if output_dir.is_dir() {
                        Some(format!(
                            "{prefix}_{name}",
                            prefix = card.uid,
                            name = card.name().unwrap_or_default(),
                        ))
                    } else {
                        None
                    };
                    let extension_s = if is_vcf { "vcf" } else { "json" };
                    let extension = if output_dir.is_dir() {
                        Some(extension_s)
                    } else {
                        None
                    };

                    let output_string = if is_vcf {
                        card.to_vcard_string()
                    } else {
                        serde_json::to_string(&card).expect("serialize JSContact")
                    };

                    let mut output_dir = Some(output_dir);
                    let res = File::create_temp_file(
                        output_string.as_bytes(),
                        filename.as_deref(),
                        output_dir.as_mut(),
                        extension,
                        false,
                    );
                    match res {
                        Ok(f) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(
                                    if is_vcf {
                                        "Exported .vcf"
                                    } else {
                                        "Exported JSContact"
                                    }
                                    .into(),
                                ),
                                body: format!(
                                    "Exported contact to {fmt} file to\n{path}",
                                    fmt = extension_s,
                                    path = f.path().display()
                                )
                                .into(),
                                kind: Some(NotificationType::Info),
                                source: None,
                            });
                            self.unrealize(context);
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Could not export contact.".into()),
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                                source: Some(err),
                            });
                        }
                    }
                    self.set_dirty(true);
                }
                Some(FormButtonAction::Reset | FormButtonAction::Other(_))
                | Some(FormButtonAction::Cancel) => {
                    self.unrealize(context);
                }
            }
            self.set_dirty(true);
            return true;
        }
        if matches!(event, UIEvent::Input(Key::Esc)) {
            self.set_dirty(true);
            self.unrealize(context);
            return true;
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.form.is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.form.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        self.form.shortcuts(context)
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        self.form.can_quit_cleanly(context)
    }
}

impl OverlayComponent for ContactExportDialog {
    fn cached_area(&self) -> Area {
        self.cached_area
    }
}

#[derive(Debug)]
enum ContactImportState {
    Form(Box<FormWidget>),
    Waiting {
        jobs: Vec<(JobId, oneshot::Receiver<Result<()>>)>,
        job_results: Vec<Result<()>>,
        path: PathBuf,
    },
}

#[derive(Debug)]
pub struct ContactImportDialog {
    id: ComponentId,
    account_hash: AccountHash,
    state: ContactImportState,
    book_id: (Arc<ContactBackendID>, AddressBookName),
    cached_area: Area,
    theme_default: ThemeAttribute,
    dirty: bool,
}

impl std::fmt::Display for ContactImportDialog {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "import")
    }
}

impl ContactImportDialog {
    pub fn new(
        account_hash: AccountHash,
        book_id: (Arc<ContactBackendID>, AddressBookName),
        context: &Context,
    ) -> Self {
        let theme_default: ThemeAttribute = crate::conf::theme_value(context, "theme_default");
        let mut form = Box::new(FormWidget::new(
            ("Cancel(Esc)".into(), FormButtonAction::Cancel),
            // cursor_up_shortcut
            context.settings.shortcuts.general.scroll_up.clone(),
            // cursor_down_shortcut
            context.settings.shortcuts.general.scroll_down.clone(),
            // cursor_right_shortcut
            context.settings.shortcuts.general.scroll_right.clone(),
            // cursor_left_shortcut
            context.settings.shortcuts.general.scroll_left.clone(),
        ));
        form.push_cl((
            "Path or directory".into(),
            String::new(),
            generic_file_path_complete_fn(),
        ))
        .set_validate_fn(generic_file_path_validate_fn());
        form.add_button(("Import".into(), FormButtonAction::Accept));
        Self {
            id: ComponentId::default(),
            account_hash,
            book_id,
            state: ContactImportState::Form(form),
            cached_area: Area::new_empty(Default::default()),
            theme_default,
            dirty: true,
        }
    }
}

impl Component for ContactImportDialog {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let area = area.center_inside((48, 10));
        self.cached_area = area;
        grid.clear_area(area, self.theme_default);

        let form_area = create_box(grid, area);
        match &mut self.state {
            ContactImportState::Form(form) => {
                let (_, y) = grid.write_string(
                    &format!(
                        "Navigate options with {} to go down, {} to go up, select with {}, cancel \
                         with {}",
                        form.cursor_down_shortcut,
                        form.cursor_up_shortcut,
                        Key::Char('\n'),
                        Key::Esc
                    ),
                    &self.theme_default,
                    form_area,
                    Some(2),
                    Some(0),
                );
                form.draw(grid, form_area.skip_rows(y + 1), context);
            }
            ContactImportState::Waiting {
                jobs,
                job_results,
                path: _,
            } => {
                let count = jobs.len();
                let singular = jobs.len() == 1;
                let (_, y) = grid.write_string(
                    &format!(
                        "Importing {count}/{total} contact{s}",
                        s = if singular { "" } else { "s" },
                        total = count + job_results.len(),
                    ),
                    &self.theme_default,
                    form_area,
                    Some(2),
                    Some(0),
                );
                let failures = job_results.iter().filter(|r| r.is_err()).count();
                if failures > 0 {
                    grid.write_string(
                        &format!(
                            "{failures} failure{s}",
                            s = if failures == 1 { "" } else { "s" },
                        ),
                        &self.theme_default,
                        form_area.skip_rows(y + 1),
                        Some(2),
                        Some(0),
                    );
                }
            }
        }
        self.set_dirty(false);
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::theme_value(context, "theme_default");
            self.set_dirty(true);
        }
        match &mut self.state {
            ContactImportState::Form(form) => {
                if form.process_event(event, context) {
                    match form.buttons_result() {
                        None => {}
                        Some(FormButtonAction::Accept) => {
                            let input: String = form.values()["Path or directory"]
                                .as_text_field()
                                .expect("path field in form")
                                .as_str()
                                .to_owned();
                            if input.is_empty() {
                                return true;
                            }
                            let input = std::path::Path::new(&input).expand();
                            match melib::contacts::vcard::load_cards(&input).and_then(|cards| {
                                cards
                                    .into_iter()
                                    .map(|c| {
                                        context.accounts[&self.account_hash]
                                            .add_card(self.book_id.clone(), c)
                                    })
                                    .collect::<Result<Vec<_>>>()
                            }) {
                                Ok(jobs) => {
                                    self.state = ContactImportState::Waiting {
                                        jobs,
                                        job_results: vec![],
                                        path: input,
                                    };
                                }
                                Err(err) => {
                                    context.replies.push_back(UIEvent::Notification {
                                        title: Some("Could not import contact.".into()),
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                        source: Some(err),
                                    });
                                }
                            }
                            self.set_dirty(true);
                        }
                        Some(FormButtonAction::Reset | FormButtonAction::Other(_))
                        | Some(FormButtonAction::Cancel) => {
                            self.unrealize(context);
                        }
                    }
                    self.set_dirty(true);
                    return true;
                }
                let shortcuts = self.shortcuts(context);
                if matches!(event, UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"])
                        || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"])
                        || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"])
                        || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]))
                {
                    return true;
                }
            }
            ContactImportState::Waiting {
                jobs,
                job_results,
                path,
            } => {
                if let UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)) = event {
                    if let Some(pos) = jobs.iter().position(|(j, _)| j == job_id) {
                        let (_, mut job) = jobs.remove(pos);
                        match job.try_recv() {
                            Err(_) => {
                                job_results.push(Err(Error::new("Import job was canceled")));
                            }
                            Ok(None) => {
                                job_results.push(Err(Error::new(
                                    "No import result; perhaps a worker thread panicked",
                                )
                                .set_kind(ErrorKind::Bug)));
                            }
                            Ok(Some(res)) => job_results.push(res),
                        }
                        if jobs.is_empty() {
                            let imported = job_results.iter().filter(|r| r.is_ok()).count();
                            let failures = job_results.iter().filter(|r| r.is_err()).count();
                            let body = match (imported > 0, failures > 0) {
                                (true, false) => {
                                    format!(
                                        "Imported {count} contact{s} from {path}",
                                        count = imported,
                                        s = if imported == 1 { "" } else { "s" },
                                        path = path.display()
                                    )
                                }
                                (true, true) => {
                                    format!(
                                        "Imported {count} contact{s} from {path}, rejected \
                                         {failures} contacts: {errs:?}",
                                        count = imported,
                                        s = if imported == 1 { "" } else { "s" },
                                        path = path.display(),
                                        errs = job_results
                                            .iter()
                                            .filter(|r| r.is_err())
                                            .collect::<Vec<_>>(),
                                    )
                                }
                                (false, true) => {
                                    format!(
                                        "Could not import any contacts from {path}, encountered \
                                         {failures} error{s}: {errs:?}",
                                        s = if failures == 1 { "" } else { "s" },
                                        path = path.display(),
                                        errs = job_results
                                            .iter()
                                            .filter(|r| r.is_err())
                                            .collect::<Vec<_>>(),
                                    )
                                }
                                (false, false) => {
                                    format!(
                                        "Did not find any contacts in {path}, so nothing was \
                                         imported",
                                        path = path.display(),
                                    )
                                }
                            };
                            context.replies.push_back(UIEvent::Notification {
                                title: None,
                                body: body.into(),
                                kind: Some(NotificationType::Info),
                                source: None,
                            });
                            self.unrealize(context);
                        }
                    }
                }
            }
        }
        if matches!(event, UIEvent::Input(Key::Esc)) {
            self.set_dirty(true);
            self.unrealize(context);
            return true;
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || if let ContactImportState::Form(form) = &self.state {
                form.is_dirty()
            } else {
                false
            }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let ContactImportState::Form(form) = &mut self.state {
            form.set_dirty(value);
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        if let ContactImportState::Form(form) = &self.state {
            form.shortcuts(context)
        } else {
            ShortcutMaps::default()
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if let ContactImportState::Form(form) = &mut self.state {
            form.can_quit_cleanly(context)
        } else {
            true
        }
    }

    fn children(&self) -> IndexMap<ComponentId, &dyn Component> {
        if let ContactImportState::Form(form) = &self.state {
            std::iter::once((form.id(), form.as_ref() as &dyn Component)).collect()
        } else {
            IndexMap::default()
        }
    }

    fn children_mut(&mut self) -> IndexMap<ComponentId, &mut dyn Component> {
        if let ContactImportState::Form(form) = &mut self.state {
            std::iter::once((form.id(), form.as_mut() as &mut dyn Component)).collect()
        } else {
            IndexMap::default()
        }
    }
}

impl OverlayComponent for ContactImportDialog {
    fn cached_area(&self) -> Area {
        self.cached_area
    }
}
