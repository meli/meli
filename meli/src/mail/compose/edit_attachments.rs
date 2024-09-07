/*
 * meli -
 *
 * Copyright  Manos Pitsidianakis
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

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum EditAttachmentCursor {
    AttachmentNo(usize),
    #[default]
    Buttons,
}

#[derive(Debug)]
pub enum EditAttachmentMode {
    Overview,
    Edit {
        inner: Box<FormWidget<FormButtonAction>>,
        no: usize,
    },
}

#[derive(Debug)]
pub struct EditAttachments {
    /// For shortcut setting retrieval.
    pub account_hash: Option<AccountHash>,
    pub mode: EditAttachmentMode,
    pub buttons: ButtonWidget<FormButtonAction>,
    pub cursor: EditAttachmentCursor,
    pub dirty: bool,
    pub id: ComponentId,
}

impl EditAttachments {
    pub fn new(account_hash: Option<AccountHash>) -> Self {
        //ButtonWidget::new(("Add".into(), FormButtonAction::Other("add")));
        let mut buttons = ButtonWidget::new(("Go Back".into(), FormButtonAction::Cancel));
        buttons.set_focus(true);
        buttons.set_cursor(1);
        Self {
            account_hash,
            mode: EditAttachmentMode::Overview,
            buttons,
            cursor: EditAttachmentCursor::Buttons,
            dirty: true,
            id: ComponentId::default(),
        }
    }
}

impl EditAttachmentsRefMut<'_, '_> {
    fn new_edit_widget(
        &self,
        no: usize,
        context: &Context,
    ) -> Option<Box<FormWidget<FormButtonAction>>> {
        if no >= self.draft.attachments().len() {
            return None;
        }
        let filename = self.draft.attachments()[no].content_type().name();
        let mime_type = self.draft.attachments()[no].content_type();
        let shortcuts = self.shortcuts(context);

        let mut ret = FormWidget::new(
            ("Save".into(), FormButtonAction::Accept),
            /* cursor_up_shortcut */
            shortcuts
                .get(Shortcuts::COMPOSING)
                .and_then(|c| c.get("scroll_up").cloned())
                .unwrap_or_else(|| context.settings.shortcuts.composing.scroll_up.clone()),
            /* cursor_down_shortcut */
            shortcuts
                .get(Shortcuts::COMPOSING)
                .and_then(|c| c.get("scroll_down").cloned())
                .unwrap_or_else(|| context.settings.shortcuts.composing.scroll_down.clone()),
        );

        ret.add_button(("Reset".into(), FormButtonAction::Reset));
        ret.add_button(("Cancel".into(), FormButtonAction::Cancel));
        ret.push(("Filename".into(), filename.unwrap_or_default().to_string()));
        ret.push(("Mime type".into(), mime_type.to_string()));
        Some(Box::new(ret))
    }
}

#[derive(Debug)]
pub struct EditAttachmentsRefMut<'a, 'b> {
    pub inner: &'a mut EditAttachments,
    pub draft: &'b mut Draft,
}

impl std::fmt::Display for EditAttachmentsRefMut<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "edit attachments")
    }
}

impl Component for EditAttachmentsRefMut<'_, '_> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let EditAttachmentMode::Edit {
            ref mut inner,
            no: _,
        } = self.inner.mode
        {
            inner.draw(grid, area, context);
        } else if self.is_dirty() {
            let attachments_no = self.draft.attachments().len();
            let theme_default = crate::conf::value(context, "theme_default");
            grid.clear_area(area, theme_default);
            if attachments_no == 0 {
                grid.write_string(
                    "no attachments",
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    area,
                    None,
                    None,
                );
            } else {
                grid.write_string(
                    &format!("{} attachments ", attachments_no),
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    area,
                    None,
                    None,
                );
                for (i, a) in self.draft.attachments().iter().enumerate() {
                    let bg = if let EditAttachmentCursor::AttachmentNo(u) = self.inner.cursor {
                        if u == i {
                            crate::conf::value(context, "highlight").bg
                        } else {
                            theme_default.bg
                        }
                    } else {
                        theme_default.bg
                    };
                    grid.write_string(
                        &if let Some(name) = a.content_type().name() {
                            format!(
                                "[{}] \"{}\", {} {}",
                                i,
                                name,
                                a.content_type(),
                                melib::BytesDisplay(a.raw.len())
                            )
                        } else {
                            format!(
                                "[{}] {} {}",
                                i,
                                a.content_type(),
                                melib::BytesDisplay(a.raw.len())
                            )
                        },
                        theme_default.fg,
                        bg,
                        theme_default.attrs,
                        area.skip(2, 2 + i),
                        None,
                        None,
                    );
                }
            }
            self.inner.buttons.draw(
                grid,
                area.skip_rows(3 + self.draft.attachments().len()),
                context,
            );
            self.set_dirty(false);
            context.dirty_areas.push_back(area);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let EditAttachmentMode::Edit {
            ref mut inner,
            ref no,
        } = self.inner.mode
        {
            if inner.process_event(event, context) {
                match inner.buttons_result() {
                    Some(FormButtonAction::Accept) | Some(FormButtonAction::Cancel) => {
                        self.inner.mode = EditAttachmentMode::Overview;
                    }
                    Some(FormButtonAction::Reset) => {
                        let no = *no;
                        if let Some(inner) = self.new_edit_widget(no, context) {
                            self.inner.mode = EditAttachmentMode::Edit { inner, no };
                        }
                    }
                    Some(_) | None => {}
                }
                return true;
            }
        } else {
            let shortcuts = self.shortcuts(context);

            match event {
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_up"]) =>
                {
                    self.set_dirty(true);
                    match self.inner.cursor {
                        EditAttachmentCursor::AttachmentNo(ref mut n) => {
                            if self.draft.attachments().is_empty() {
                                self.inner.cursor = EditAttachmentCursor::Buttons;
                                self.inner.buttons.set_focus(true);
                                self.inner.buttons.process_event(event, context);
                                return true;
                            }
                            *n = n.saturating_sub(1);
                        }
                        EditAttachmentCursor::Buttons => {
                            if !self.inner.buttons.process_event(event, context) {
                                self.inner.buttons.set_focus(false);
                                if self.draft.attachments().is_empty() {
                                    return true;
                                }
                                self.inner.cursor = EditAttachmentCursor::AttachmentNo(
                                    self.draft.attachments().len() - 1,
                                );
                            }
                        }
                    }
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_down"]) =>
                {
                    self.set_dirty(true);
                    match self.inner.cursor {
                        EditAttachmentCursor::AttachmentNo(ref mut n) => {
                            if *n + 1 == self.draft.attachments().len() {
                                self.inner.cursor = EditAttachmentCursor::Buttons;
                                self.inner.buttons.set_focus(true);
                                self.inner.buttons.process_event(event, context);
                                return true;
                            }
                            *n += 1;
                        }
                        EditAttachmentCursor::Buttons => {
                            self.inner.buttons.set_focus(true);
                            self.inner.buttons.process_event(event, context);
                        }
                    }
                    return true;
                }
                UIEvent::Input(Key::Char('\n')) => {
                    match self.inner.cursor {
                        EditAttachmentCursor::AttachmentNo(ref no) => {
                            if let Some(inner) = self.new_edit_widget(*no, context) {
                                self.inner.mode = EditAttachmentMode::Edit { inner, no: *no };
                            }
                            self.set_dirty(true);
                        }
                        EditAttachmentCursor::Buttons => {
                            self.inner.buttons.process_event(event, context);
                        }
                    }
                    return true;
                }
                _ => {
                    if self.inner.cursor == EditAttachmentCursor::Buttons
                        && self.inner.buttons.process_event(event, context)
                    {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.inner.dirty
            || self.inner.buttons.is_dirty()
            || if let EditAttachmentMode::Edit { ref inner, no: _ } = self.inner.mode {
                inner.is_dirty()
            } else {
                false
            }
    }

    fn set_dirty(&mut self, value: bool) {
        self.inner.dirty = value;
        self.inner.buttons.set_dirty(value);
        if let EditAttachmentMode::Edit {
            ref mut inner,
            no: _,
        } = self.inner.mode
        {
            inner.set_dirty(value);
        }
    }

    fn kill(&mut self, _uuid: ComponentId, _context: &mut Context) {}

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();

        let our_map: ShortcutMap = self
            .inner
            .account_hash
            .map(|acc| account_settings!(context[acc].shortcuts.composing).key_values())
            .unwrap_or_else(|| context.settings.shortcuts.composing.key_values());
        map.insert(Shortcuts::COMPOSING, our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.inner.id
    }
}
