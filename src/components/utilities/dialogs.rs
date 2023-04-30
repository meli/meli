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

const OK_CANCEL: &str = "OK    Cancel";
const OK_OFFSET: usize = 0;
const OK_LENGTH: usize = "OK".len();
const CANCEL_OFFSET: usize = "OK    ".len();
const CANCEL_LENGTH: usize = "Cancel".len();

#[derive(Debug, Copy, PartialEq, Eq, Clone)]
enum SelectorCursor {
    Unfocused,
    /// Cursor is at an entry
    Entry(usize),
    /// Cursor is located on the Ok button
    Ok,
    /// Cursor is located on the Cancel button
    Cancel,
}

/// Shows a little window with options for user to select.
///
/// Instantiate with Selector::new(). Set single_only to true if user should
/// only choose one of the options. After passing input events to this
/// component, check Selector::is_done to see if the user has finalised their
/// choices. Collect the choices by consuming the Selector with
/// Selector::collect()
pub struct Selector<T: 'static + PartialEq + Debug + Clone + Sync + Send, F: 'static + Sync + Send>
{
    /// allow only one selection
    single_only: bool,
    entries: Vec<(T, bool)>,
    entry_titles: Vec<String>,
    pub content: CellBuffer,
    theme_default: ThemeAttribute,

    cursor: SelectorCursor,
    vertical_alignment: Alignment,
    horizontal_alignment: Alignment,
    title: String,

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
        Selector::draw(self, grid, area, context);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.initialise(context);
            self.set_dirty(true);
            return false;
        }

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
                        highlighted_attrs.fg,
                        highlighted_attrs.bg,
                        highlighted_attrs.attrs,
                        ((1, c), (width - 1, c)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        " ",
                        &mut self.content,
                        highlighted_attrs.fg,
                        highlighted_attrs.bg,
                        highlighted_attrs.attrs,
                        ((1, c), (width - 1, c)),
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
            (UIEvent::Input(ref key), SelectorCursor::Unfocused)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    for c in self.content.row_iter(0..(width - 1), 0) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[0].1 = true;
                } else {
                    for c in self.content.row_iter(0..3, 0) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                }
                self.cursor = SelectorCursor::Entry(0);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) && c > 0 =>
            {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(0..(width - 1), c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..(width - 1), c - 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(0..3, c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..3, c - 1) {
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
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) =>
            {
                for c in self
                    .content
                    .row_iter(((width - OK_CANCEL.len()) / 2)..(width - 1), height - 1)
                {
                    self.content[c]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                for c in self.content.row_iter(0..3, c) {
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
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(0..(width - 1), c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..(width - 1), c + 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(0..3, c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..3, c + 1) {
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
                if !self.single_only
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(0..3, c) {
                    self.content[c]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + OK_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + OK_OFFSET + OK_LENGTH),
                    height - 1,
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
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
            {
                self.cursor = SelectorCursor::Cancel;
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + OK_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + OK_OFFSET + OK_LENGTH),
                    height - 1,
                ) {
                    self.content[c]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET + CANCEL_LENGTH),
                    height - 1,
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
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + OK_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + OK_OFFSET + OK_LENGTH),
                    height - 1,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                change_colors(
                    &mut self.content,
                    (
                        ((width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET, height - 1),
                        (
                            (width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET + CANCEL_LENGTH,
                            height - 1,
                        ),
                    ),
                    self.theme_default.fg,
                    self.theme_default.bg,
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                return true
            }
            _ => {}
        }

        false
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        map.insert(
            Shortcuts::GENERAL,
            context.settings.shortcuts.general.key_values(),
        );
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
        Selector::draw(self, grid, area, context);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.initialise(context);
            self.set_dirty(true);
            return false;
        }

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
                        highlighted_attrs.fg,
                        highlighted_attrs.bg,
                        highlighted_attrs.attrs,
                        ((1, c), (width - 1, c)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        " ",
                        &mut self.content,
                        highlighted_attrs.fg,
                        highlighted_attrs.bg,
                        highlighted_attrs.attrs,
                        ((1, c), (width - 1, c)),
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
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) && c > 0 =>
            {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(0..(width - 1), c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..(width - 1), c - 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(0..3, c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..3, c - 1) {
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
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) =>
            {
                for c in self
                    .content
                    .row_iter(((width - OK_CANCEL.len()) / 2)..(width - 1), height - 1)
                {
                    self.content[c]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                for c in self.content.row_iter(0..3, c) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Unfocused)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    for c in self.content.row_iter(0..(width - 1), 0) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[0].1 = true;
                } else {
                    for c in self.content.row_iter(0..3, 0) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                }
                self.cursor = SelectorCursor::Entry(0);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if c < self.entries.len().saturating_sub(1)
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    // Redraw selection
                    for c in self.content.row_iter(0..(width - 1), c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..(width - 1), c + 1) {
                        self.content[c]
                            .set_fg(highlighted_attrs.fg)
                            .set_bg(highlighted_attrs.bg)
                            .set_attrs(highlighted_attrs.attrs);
                    }
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                } else {
                    // Redraw cursor
                    for c in self.content.row_iter(0..3, c) {
                        self.content[c]
                            .set_fg(self.theme_default.fg)
                            .set_bg(self.theme_default.bg)
                            .set_attrs(self.theme_default.attrs);
                    }
                    for c in self.content.row_iter(0..3, c + 1) {
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
                if !self.single_only
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(0..3, c) {
                    self.content[c]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + OK_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + OK_OFFSET + OK_LENGTH),
                    height - 1,
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
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
            {
                self.cursor = SelectorCursor::Cancel;
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + OK_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + OK_OFFSET + OK_LENGTH),
                    height - 1,
                ) {
                    self.content[c]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET + CANCEL_LENGTH),
                    height - 1,
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
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                for c in self.content.row_iter(
                    ((width - OK_CANCEL.len()) / 2 + OK_OFFSET)
                        ..((width - OK_CANCEL.len()) / 2 + OK_OFFSET + OK_LENGTH),
                    height - 1,
                ) {
                    self.content[c]
                        .set_fg(highlighted_attrs.fg)
                        .set_bg(highlighted_attrs.bg)
                        .set_attrs(highlighted_attrs.attrs);
                }
                change_colors(
                    &mut self.content,
                    (
                        ((width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET, height - 1),
                        (
                            (width - OK_CANCEL.len()) / 2 + CANCEL_OFFSET + CANCEL_LENGTH,
                            height - 1,
                        ),
                    ),
                    self.theme_default.fg,
                    self.theme_default.bg,
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                return true
            }
            _ => {}
        }

        false
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        map.insert(
            Shortcuts::GENERAL,
            context.settings.shortcuts.general.key_values(),
        );
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
        mut entries: Vec<(T, String)>,
        single_only: bool,
        done_fn: F,
        context: &Context,
    ) -> Selector<T, F> {
        let entry_titles = entries
            .iter_mut()
            .map(|(_id, ref mut title)| std::mem::take(title))
            .collect::<Vec<String>>();
        let mut identifiers: Vec<(T, bool)> =
            entries.into_iter().map(|(id, _)| (id, false)).collect();
        if single_only {
            /* set default option */
            identifiers[0].1 = true;
        }

        let mut ret = Selector {
            single_only,
            entries: identifiers,
            entry_titles,
            content: Default::default(),
            cursor: SelectorCursor::Unfocused,
            vertical_alignment: Alignment::Center,
            horizontal_alignment: Alignment::Center,
            title: title.to_string(),
            done: false,
            done_fn,
            dirty: true,
            theme_default: Default::default(),
            id: ComponentId::new_v4(),
        };
        ret.initialise(context);
        ret
    }

    fn initialise(&mut self, context: &Context) {
        self.theme_default = crate::conf::value(context, "theme_default");
        let width = std::cmp::max(
            OK_CANCEL.len(),
            std::cmp::max(
                self.entry_titles
                    .iter()
                    .max_by_key(|e| e.len())
                    .map(|v| v.len())
                    .unwrap_or(0),
                self.title.len(),
            ),
        ) + 5;
        let height = self.entries.len()
            + if self.single_only {
                0
            } else {
                /* Extra room for buttons Okay/Cancel */
                2
            };
        let mut content = CellBuffer::new_with_context(width, height, None, context);
        if self.single_only {
            for (i, e) in self.entry_titles.iter().enumerate() {
                write_string_to_grid(
                    e,
                    &mut content,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    ((0, i), (width - 1, i)),
                    None,
                );
            }
        } else {
            for (i, e) in self.entry_titles.iter().enumerate() {
                write_string_to_grid(
                    &format!("[ ] {}", e),
                    &mut content,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    ((0, i), (width - 1, i)),
                    None,
                );
            }
            write_string_to_grid(
                OK_CANCEL,
                &mut content,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs | Attr::BOLD,
                (
                    ((width - OK_CANCEL.len()) / 2, height - 1),
                    (width - 1, height - 1),
                ),
                None,
            );
        }
        self.content = content;
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

    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let shortcuts = context.settings.shortcuts.general.key_values();
        let navigate_help_string = format!(
            "Navigate options with {} to go down, {} to go up, select with {}",
            shortcuts["scroll_down"],
            shortcuts["scroll_up"],
            Key::Char('\n')
        );
        let width = std::cmp::max(
            self.content.size().0 + 1,
            std::cmp::max(self.title.len(), navigate_help_string.len()) + 3,
        ) + 3;
        let height = self.content.size().1 + {
            /* padding */
            3
        };
        let dialog_area = align_area(
            area,
            (width, height),
            self.vertical_alignment,
            self.horizontal_alignment,
        );
        let inner_area = create_box(grid, dialog_area);
        clear_area(grid, inner_area, self.theme_default);
        write_string_to_grid(
            &self.title,
            grid,
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs | Attr::BOLD,
            (
                pos_inc(upper_left!(dialog_area), (2, 0)),
                bottom_right!(dialog_area),
            ),
            None,
        );
        write_string_to_grid(
            &navigate_help_string,
            grid,
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs | Attr::ITALICS,
            (
                pos_inc(upper_left!(dialog_area), (2, height)),
                bottom_right!(dialog_area),
            ),
            None,
        );
        let inner_area = (
            pos_inc(upper_left!(inner_area), (1, 1)),
            bottom_right!(inner_area),
        );
        let (width, height) = self.content.size();
        copy_area(
            grid,
            &self.content,
            inner_area,
            ((0, 0), (width - 1, height - 1)),
        );
        context.dirty_areas.push_back(dialog_area);
        self.dirty = false;
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
