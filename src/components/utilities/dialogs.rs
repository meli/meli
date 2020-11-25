/*
 * meli
 *
 * Copyright 2020 Manos Pitsidianakis
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

#[derive(Debug, Copy, PartialEq, Clone)]
enum SelectorCursor {
    /// Cursor is at an entry
    Entry(usize),
    /// Cursor is located on the Ok button
    Ok,
    /// Cursor is located on the Cancel button
    Cancel,
}

/// Shows a little window with options for user to select.
///
/// Instantiate with Selector::new(). Set single_only to true if user should only choose one of the
/// options. After passing input events to this component, check Selector::is_done to see if the
/// user has finalised their choices. Collect the choices by consuming the Selector with
/// Selector::collect()
pub struct Selector<T: 'static + PartialEq + Debug + Clone + Sync + Send, F: 'static + Sync + Send>
{
    /// allow only one selection
    single_only: bool,
    entries: Vec<(T, bool)>,
    pub content: CellBuffer,

    cursor: SelectorCursor,

    /// If true, user has finished their selection
    done: bool,
    done_fn: F,
    dirty: bool,
    id: ComponentId,
}

pub type UIConfirmationDialog = Selector<
    bool,
    Option<Box<dyn FnOnce(ComponentId, bool) -> Option<UIEvent> + 'static + Sync + Send>>,
>;

pub type UIDialog<T> = Selector<
    T,
    Option<Box<dyn FnOnce(ComponentId, &[T]) -> Option<UIEvent> + 'static + Sync + Send>>,
>;

impl<T: 'static + PartialEq + Debug + Clone + Sync + Send, F: 'static + Sync + Send> fmt::Debug
    for Selector<T, F>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("Selector", f)
    }
}

impl<T: 'static + PartialEq + Debug + Clone + Sync + Send, F: 'static + Sync + Send> fmt::Display
    for Selector<T, F>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("Selector", f)
    }
}

impl<T: 'static + PartialEq + Debug + Clone + Sync + Send, F: 'static + Sync + Send> PartialEq
    for Selector<T, F>
{
    fn eq(&self, other: &Selector<T, F>) -> bool {
        self.entries == other.entries
    }
}

impl<T: 'static + PartialEq + Debug + Clone + Sync + Send> Component for UIDialog<T> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let (width, height) = self.content.size();
        copy_area_with_break(grid, &self.content, area, ((0, 0), (width, height)));
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let (width, height) = self.content.size();
        let shortcuts = self.get_shortcuts(context);
        let mut highlighted_attrs = crate::conf::value(context, "widgets.options.highlighted");
        if !context.settings.terminal.use_color() {
            highlighted_attrs.attrs |= Attr::REVERSE;
        }
        match (event, self.cursor) {
            (UIEvent::Input(Key::Char('\n')), _) if self.single_only => {
                /* User can only select one entry, so Enter key finalises the selection */
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Entry(c)) if !self.single_only => {
                /* User can select multiple entries, so Enter key toggles the entry under the
                 * cursor */
                self.entries[c].1 = !self.entries[c].1;
                if self.entries[c].1 {
                    write_string_to_grid(
                        "x",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::DEFAULT,
                        ((3, c + 2), (width - 2, c + 2)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        " ",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::DEFAULT,
                        ((3, c + 2), (width - 2, c + 2)),
                        None,
                    );
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Ok) if !self.single_only => {
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Esc), _) => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Cancel) if !self.single_only => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Up), SelectorCursor::Entry(c)) if c > 0 => {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(2..(width - 2), c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..(width - 2), c + 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(2..4, c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..4, c + 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                }
                self.cursor = SelectorCursor::Entry(c - 1);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
            | (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts["general"]["scroll_up"]) =>
            {
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..(width - 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(Color::Default)
                        .set_bg(Color::Default)
                        .set_attrs(Attr::DEFAULT);
                }
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                let mut highlighted_attrs =
                    crate::conf::value(context, "widgets.options.highlighted");
                if !context.settings.terminal.use_color() {
                    highlighted_attrs.attrs |= Attr::REVERSE;
                }
                for c in self.content.row_iter(2..4, c + 2) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if c < self.entries.len().saturating_sub(1)
                    && shortcut!(key == shortcuts["general"]["scroll_down"]) =>
            {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(2..(width - 2), c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..(width - 2), c + 3) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(2..4, c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..4, c + 3) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                }
                self.cursor = SelectorCursor::Entry(c + 1);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if !self.single_only && shortcut!(key == shortcuts["general"]["scroll_down"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(2..4, c + 2) {
                    self.content[c]
                        .set_fg(Color::Default)
                        .set_bg(Color::Default)
                        .set_attrs(Attr::DEFAULT);
                }
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..((width - "OK    Cancel".len()) / 2 + 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
                if shortcut!(key == shortcuts["general"]["scroll_right"]) =>
            {
                self.cursor = SelectorCursor::Cancel;
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..((width - "OK    Cancel".len()) / 2 + 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(Color::Default)
                        .set_bg(Color::Default)
                        .set_attrs(Attr::DEFAULT);
                }
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2 + 6)
                        ..((width - "OK    Cancel".len()) / 2 + 11),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts["general"]["scroll_left"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..((width - "OK    Cancel".len()) / 2 + 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2 + 6, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 11, height - 3),
                    ),
                    Color::Default,
                    Color::Default,
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts["general"]["scroll_left"])
                    || shortcut!(key == shortcuts["general"]["scroll_right"])
                    || shortcut!(key == shortcuts["general"]["scroll_up"])
                    || shortcut!(key == shortcuts["general"]["scroll_down"]) =>
            {
                return true
            }
            _ => {}
        }

        false
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        map.insert("general", context.settings.shortcuts.general.key_values());
        map
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl Component for UIConfirmationDialog {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let (width, height) = self.content.size();
        copy_area_with_break(grid, &self.content, area, ((0, 0), (width, height)));
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let (width, height) = self.content.size();
        let shortcuts = self.get_shortcuts(context);
        let mut highlighted_attrs = crate::conf::value(context, "widgets.options.highlighted");
        if !context.settings.terminal.use_color() {
            highlighted_attrs.attrs |= Attr::REVERSE;
        }
        match (event, self.cursor) {
            (UIEvent::Input(Key::Char('\n')), _) if self.single_only => {
                /* User can only select one entry, so Enter key finalises the selection */
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Entry(c)) if !self.single_only => {
                /* User can select multiple entries, so Enter key toggles the entry under the
                 * cursor */
                self.entries[c].1 = !self.entries[c].1;
                if self.entries[c].1 {
                    write_string_to_grid(
                        "x",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::DEFAULT,
                        ((3, c + 2), (width - 2, c + 2)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        " ",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::DEFAULT,
                        ((3, c + 2), (width - 2, c + 2)),
                        None,
                    );
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Ok) if !self.single_only => {
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Esc), _) => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Cancel) if !self.single_only => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    context.replies.push_back(UIEvent::ComponentKill(self.id));
                }
                return true;
            }
            (UIEvent::Input(Key::Up), SelectorCursor::Entry(c)) if c > 0 => {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(2..(width - 2), c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..(width - 2), c + 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(2..4, c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..4, c + 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                }
                self.cursor = SelectorCursor::Entry(c - 1);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
            | (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts["general"]["scroll_up"]) =>
            {
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..(width - 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(Color::Default)
                        .set_bg(Color::Default)
                        .set_attrs(Attr::DEFAULT);
                }
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                let mut highlighted_attrs =
                    crate::conf::value(context, "widgets.options.highlighted");
                if !context.settings.terminal.use_color() {
                    highlighted_attrs.attrs |= Attr::REVERSE;
                }
                for c in self.content.row_iter(2..4, c + 2) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if c < self.entries.len().saturating_sub(1)
                    && shortcut!(key == shortcuts["general"]["scroll_down"]) =>
            {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(2..(width - 2), c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..(width - 2), c + 3) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(2..4, c + 2) {
                        self.content[c]
                            .set_fg(Color::Default)
                            .set_bg(Color::Default)
                            .set_attrs(Attr::DEFAULT);
                    }
                    for c in self.content.row_iter(2..4, c + 3) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                }
                self.cursor = SelectorCursor::Entry(c + 1);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if !self.single_only && shortcut!(key == shortcuts["general"]["scroll_down"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(2..4, c + 2) {
                    self.content[c]
                        .set_fg(Color::Default)
                        .set_bg(Color::Default)
                        .set_attrs(Attr::DEFAULT);
                }
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..((width - "OK    Cancel".len()) / 2 + 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
                if shortcut!(key == shortcuts["general"]["scroll_right"]) =>
            {
                self.cursor = SelectorCursor::Cancel;
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..((width - "OK    Cancel".len()) / 2 + 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(Color::Default)
                        .set_bg(Color::Default)
                        .set_attrs(Attr::DEFAULT);
                }
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2 + 6)
                        ..((width - "OK    Cancel".len()) / 2 + 11),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts["general"]["scroll_left"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(
                    ((width - "OK    Cancel".len()) / 2)..((width - "OK    Cancel".len()) / 2 + 1),
                    height - 3,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2 + 6, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 11, height - 3),
                    ),
                    Color::Default,
                    Color::Default,
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts["general"]["scroll_left"])
                    || shortcut!(key == shortcuts["general"]["scroll_right"])
                    || shortcut!(key == shortcuts["general"]["scroll_up"])
                    || shortcut!(key == shortcuts["general"]["scroll_down"]) =>
            {
                return true
            }
            _ => {}
        }

        false
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        map.insert("general", context.settings.shortcuts.general.key_values());
        map
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl<T: PartialEq + Debug + Clone + Sync + Send, F: 'static + Sync + Send> Selector<T, F> {
    pub fn new(
        title: &str,
        entries: Vec<(T, String)>,
        single_only: bool,
        done_fn: F,
        context: &Context,
    ) -> Selector<T, F> {
        let width = std::cmp::max(
            "OK    Cancel".len(),
            std::cmp::max(
                entries
                    .iter()
                    .max_by_key(|e| e.1.len())
                    .map(|v| v.1.len())
                    .unwrap_or(0),
                title.len(),
            ),
        ) + 7;
        let height = entries.len()
            + 4
            + if single_only {
                0
            } else {
                /* Extra room for buttons Okay/Cancel */
                3
            };
        let mut content =
            CellBuffer::new_with_context(width, height, Cell::with_char(' '), context);
        let ascii_drawing = context.settings.terminal.ascii_drawing;
        write_string_to_grid(
            if ascii_drawing { "+-" } else { "┏━" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::DEFAULT,
            ((0, 0), (width - 1, 0)),
            None,
        );
        let (x, _) = write_string_to_grid(
            title,
            &mut content,
            Color::Default,
            Color::Default,
            Attr::DEFAULT,
            ((2, 0), (width - 1, 0)),
            None,
        );
        for i in 1..(width - title.len() - 1) {
            write_string_to_grid(
                if ascii_drawing { "-" } else { "━" },
                &mut content,
                Color::Byte(8),
                Color::Default,
                Attr::DEFAULT,
                ((x + i, 0), (width - 1, 0)),
                None,
            );
        }
        write_string_to_grid(
            if ascii_drawing { "+" } else { "┓" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::DEFAULT,
            ((width - 1, 0), (width - 1, 0)),
            None,
        );
        write_string_to_grid(
            if ascii_drawing { "+" } else { "┗" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::DEFAULT,
            ((0, height - 1), (width - 1, height - 1)),
            None,
        );
        write_string_to_grid(
            &if ascii_drawing {
                "-".repeat(width - 2)
            } else {
                "━".repeat(width - 2)
            },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::DEFAULT,
            ((1, height - 1), (width - 2, height - 1)),
            None,
        );
        write_string_to_grid(
            if ascii_drawing { "+" } else { "┛" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::DEFAULT,
            ((width - 1, height - 1), (width - 1, height - 1)),
            None,
        );
        for i in 1..height - 1 {
            write_string_to_grid(
                if ascii_drawing { "|" } else { "┃" },
                &mut content,
                Color::Byte(8),
                Color::Default,
                Attr::DEFAULT,
                ((0, i), (width - 1, i)),
                None,
            );
            write_string_to_grid(
                if ascii_drawing { "|" } else { "┃" },
                &mut content,
                Color::Byte(8),
                Color::Default,
                Attr::DEFAULT,
                ((width - 1, i), (width - 1, i)),
                None,
            );
        }
        let mut highlighted_attrs = crate::conf::value(context, "widgets.options.highlighted");
        if !context.settings.terminal.use_color() {
            highlighted_attrs.attrs |= Attr::REVERSE;
        }
        if single_only {
            for (i, e) in entries.iter().enumerate() {
                write_string_to_grid(
                    &e.1,
                    &mut content,
                    Color::Default,
                    if i == 0 {
                        highlighted_attrs.bg
                    } else {
                        Color::Default
                    },
                    if i == 0 {
                        highlighted_attrs.attrs
                    } else {
                        Attr::DEFAULT
                    },
                    ((2, i + 2), (width - 1, i + 2)),
                    None,
                );
            }
        } else {
            for (i, e) in entries.iter().enumerate() {
                write_string_to_grid(
                    &format!("[ ] {}", e.1),
                    &mut content,
                    Color::Default,
                    Color::Default,
                    Attr::DEFAULT,
                    ((2, i + 2), (width - 1, i + 2)),
                    None,
                );
                if i == 0 {
                    content[(2, i + 2)]
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                    content[(3, i + 2)]
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                    content[(4, i + 2)]
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
            }
            write_string_to_grid(
                "OK    Cancel",
                &mut content,
                Color::Default,
                Color::Default,
                Attr::BOLD,
                (
                    ((width - "OK    Cancel".len()) / 2, height - 3),
                    (width - 1, height - 3),
                ),
                None,
            );
        }
        let mut identifiers: Vec<(T, bool)> =
            entries.into_iter().map(|(id, _)| (id, false)).collect();
        if single_only {
            /* set default option */
            identifiers[0].1 = true;
        }

        Selector {
            single_only,
            entries: identifiers,
            content,
            cursor: SelectorCursor::Entry(0),
            done: false,
            done_fn,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }

    pub fn is_done(&self) -> bool {
        self.done
    }

    pub fn collect(self) -> Vec<T> {
        self.entries
            .into_iter()
            .filter(|v| v.1)
            .map(|(id, _)| id)
            .collect()
    }
}

impl<T: 'static + PartialEq + Debug + Clone + Sync + Send> UIDialog<T> {
    fn done(&mut self) -> Option<UIEvent> {
        let Self {
            ref mut done_fn,
            ref mut entries,
            ref id,
            ..
        } = self;
        done_fn.take().and_then(|done_fn| {
            debug!(done_fn(
                *id,
                entries
                    .iter()
                    .filter(|v| v.1)
                    .map(|(id, _)| id)
                    .cloned()
                    .collect::<Vec<_>>()
                    .as_slice(),
            ))
        })
    }
}

impl UIConfirmationDialog {
    fn done(&mut self) -> Option<UIEvent> {
        let Self {
            ref mut done_fn,
            ref mut entries,
            ref id,
            ..
        } = self;
        done_fn.take().and_then(|done_fn| {
            done_fn(
                *id,
                entries
                    .iter()
                    .filter(|v| v.1)
                    .map(|(id, _)| id)
                    .cloned()
                    .any(core::convert::identity),
            )
        })
    }
}
