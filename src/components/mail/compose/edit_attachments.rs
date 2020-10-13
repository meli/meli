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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum EditAttachmentCursor {
    AttachmentNo(usize),
    Buttons,
}

impl Default for EditAttachmentCursor {
    fn default() -> Self {
        EditAttachmentCursor::Buttons
    }
}

#[derive(Debug)]
pub enum EditAttachmentMode {
    Overview,
    Edit {
        inner: FormWidget<FormButtonActions>,
        no: usize,
    },
}

#[derive(Debug)]
pub struct EditAttachments {
    pub mode: EditAttachmentMode,
    pub buttons: ButtonWidget<FormButtonActions>,
    pub cursor: EditAttachmentCursor,
    pub dirty: bool,
    pub id: ComponentId,
}

impl EditAttachments {
    pub fn new() -> Self {
        let mut buttons = ButtonWidget::new(("Add".into(), FormButtonActions::Other("add")));
        buttons.push(("Go Back".into(), FormButtonActions::Cancel));
        buttons.set_focus(true);
        buttons.set_cursor(1);
        EditAttachments {
            mode: EditAttachmentMode::Overview,
            buttons,
            cursor: EditAttachmentCursor::Buttons,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
}

impl EditAttachmentsRefMut<'_, '_> {
    fn new_edit_widget(&self, no: usize) -> Option<FormWidget<FormButtonActions>> {
        if no >= self.draft.attachments().len() {
            return None;
        }
        let filename = self.draft.attachments()[no].content_type().name();
        let mime_type = self.draft.attachments()[no].content_type();
        let mut ret = FormWidget::new(("Save".into(), FormButtonActions::Accept));

        ret.add_button(("Reset".into(), FormButtonActions::Reset));
        ret.add_button(("Cancel".into(), FormButtonActions::Cancel));
        ret.push(("Filename".into(), filename.unwrap_or_default().to_string()));
        ret.push(("Mime type".into(), mime_type.to_string()));
        Some(ret)
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
            clear_area(grid, area, theme_default);
            if attachments_no == 0 {
                write_string_to_grid(
                    "no attachments",
                    grid,
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    area,
                    None,
                );
            } else {
                write_string_to_grid(
                    &format!("{} attachments ", attachments_no),
                    grid,
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    area,
                    None,
                );
                for (i, a) in self.draft.attachments().iter().enumerate() {
                    let bg = if let EditAttachmentCursor::AttachmentNo(u) = self.inner.cursor {
                        if u == i {
                            Color::Byte(237)
                        } else {
                            theme_default.bg
                        }
                    } else {
                        theme_default.bg
                    };
                    if let Some(name) = a.content_type().name() {
                        write_string_to_grid(
                            &format!(
                                "[{}] \"{}\", {} {}",
                                i,
                                name,
                                a.content_type(),
                                melib::Bytes(a.raw.len())
                            ),
                            grid,
                            theme_default.fg,
                            bg,
                            theme_default.attrs,
                            (pos_inc(upper_left!(area), (0, 1 + i)), bottom_right!(area)),
                            None,
                        );
                    } else {
                        write_string_to_grid(
                            &format!("[{}] {} {}", i, a.content_type(), melib::Bytes(a.raw.len())),
                            grid,
                            theme_default.fg,
                            bg,
                            theme_default.attrs,
                            (pos_inc(upper_left!(area), (0, 1 + i)), bottom_right!(area)),
                            None,
                        );
                    }
                }
            }
            self.inner.buttons.draw(
                grid,
                (
                    pos_inc(upper_left!(area), (0, 1 + self.draft.attachments().len())),
                    bottom_right!(area),
                ),
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
                    Some(FormButtonActions::Accept) | Some(FormButtonActions::Cancel) => {
                        self.inner.mode = EditAttachmentMode::Overview;
                    }
                    Some(FormButtonActions::Reset) => {
                        let no = *no;
                        if let Some(inner) = self.new_edit_widget(no) {
                            self.inner.mode = EditAttachmentMode::Edit { inner, no };
                        }
                    }
                    Some(_) | None => {}
                }
                return true;
            }
        } else {
            match event {
                UIEvent::Input(Key::Up) => {
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
                UIEvent::Input(Key::Down) => {
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
                            if let Some(inner) = self.new_edit_widget(*no) {
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

    fn kill(&mut self, _uuid: Uuid, _context: &mut Context) {}

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        ShortcutMaps::default()
    }

    fn id(&self) -> ComponentId {
        self.inner.id
    }

    fn set_id(&mut self, new_id: ComponentId) {
        self.inner.id = new_id;
    }
}
