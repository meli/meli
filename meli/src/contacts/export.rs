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

use melib::{contacts::Card, utils::shellexpand::ShellExpandTrait as _};

use crate::{
    conf::themes::ThemeAttribute,
    terminal::*,
    types::NotificationType,
    utilities::{FormButtonAction, FormWidget},
    AutoCompleteEntry, CellBuffer, Component, ComponentId, Context, Field, File, Key, UIEvent,
};

#[derive(Debug)]
pub struct ContactExportDialog {
    id: ComponentId,
    card: Card,
    form: FormWidget<FormButtonAction>,
    theme_default: ThemeAttribute,
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
            Box::new(move |_, term| {
                use crate::command::parser::Lexer;

                let mut lexer = Lexer::new(term);
                let Some(input) = lexer.next() else {
                    return vec![];
                };
                if lexer.next().is_some() {
                    return vec![];
                }
                match input {
                    Ok(input) => std::path::Path::new(input.value())
                        .complete(true, input.value().ends_with('/'))
                        .into_iter()
                        .take(100)
                        .map(|m| {
                            format!(
                                "{input}{path}",
                                input = term.strip_suffix(input.raw()).unwrap(),
                                path = input.append(&m)
                            )
                        })
                        .map(AutoCompleteEntry::from)
                        .collect::<Vec<AutoCompleteEntry>>(),
                    Err(lex_token_err) => {
                        if let (Some(raw), Some(value)) =
                            (lex_token_err.raw(), lex_token_err.value())
                        {
                            std::path::Path::new(value)
                                .complete(true, value.ends_with('/'))
                                .into_iter()
                                .take(100)
                                .map(|m| {
                                    if lex_token_err.is_quoted()
                                        || m.contains('"')
                                        || m.contains(' ')
                                    {
                                        format!(
                                            "{input}\"{value}{m}\"",
                                            input = term.strip_suffix(raw).unwrap(),
                                            value = value,
                                            m = m.replace('"', "\\\""),
                                        )
                                    } else {
                                        format!(
                                            "{input}{value}{m}",
                                            input = term.strip_suffix(raw).unwrap(),
                                            value = value,
                                        )
                                    }
                                })
                                .map(AutoCompleteEntry::from)
                                .collect::<Vec<AutoCompleteEntry>>()
                        } else {
                            vec![]
                        }
                    }
                }
            }),
        ));
        form.add_button(("Export".into(), FormButtonAction::Accept));
        Self {
            id: ComponentId::default(),
            card,
            form,
            theme_default,
            dirty: true,
        }
    }
}

impl Component for ContactExportDialog {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        // [ref: FIXME]: overlay components need to always be dirty otherwise they are not shown
        // when State::redraw finishes
        self.set_dirty(true);
        // if !self.is_dirty() {
        // //FIXME
        //     return;
        // }
        let area = area.center_inside((48, 10));
        grid.clear_area(area, self.theme_default);

        let form_area = create_box(grid, area);
        let (_, y) = grid.write_string(
            &format!(
                "Navigate options with {} to go down, {} to go up, select with {}, cancel with {}",
                self.form.cursor_down_shortcut,
                self.form.cursor_up_shortcut,
                Key::Char('\n'),
                Key::Esc
            ),
            &self.theme_default,
            form_area,
            Some(2),
            Some(0),
        );

        self.form.draw(grid, form_area.skip_rows(y + 1), context);
        // self.set_dirty(false);
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
                    let output_dir: String = self
                        .form
                        .values()
                        .first()
                        .map(|(_, v)| match v {
                            Field::Text(v) => v.as_str().to_string(),
                            Field::Choice(v, c, _) => v.clone().remove(*c).to_string(),
                        })
                        .expect("path field in form");
                    let card = &self.card;
                    let output_dir = std::path::Path::new(&output_dir).expand();
                    let filename = if output_dir.is_dir() {
                        Some(format!(
                            "{prefix}{name}{suffix}{space}{additionalname}",
                            prefix = card.name_prefix(),
                            name = card.name(),
                            suffix = card.name_suffix(),
                            space = if card.additionalname().trim().is_empty() {
                                ""
                            } else {
                                " "
                            },
                            additionalname = card.additionalname()
                        ))
                    } else {
                        None
                    };
                    let extension = if output_dir.is_dir() {
                        Some("vcf")
                    } else {
                        None
                    };

                    let mut output_dir = Some(output_dir);
                    let res = File::create_temp_file(
                        card.to_vcard_string().as_bytes(),
                        filename.as_deref(),
                        output_dir.as_mut(),
                        extension,
                        false,
                    );
                    match res {
                        Ok(f) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Exported .vcf".into()),
                                body: format!(
                                    "Exported contact to vcard file to\n{}",
                                    f.path().display()
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

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        self.form.can_quit_cleanly(context)
    }
}
