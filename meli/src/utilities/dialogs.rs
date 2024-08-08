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

const OK: &str = "OK";
const CANCEL: &str = "Cancel";
const CANCEL_OFFSET: usize = "OK    ".len();

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
/// Instantiate with `Selector::new()`. Set `single_only` to true if user should
/// only choose one of the options. After passing input events to this
/// component, check `Selector::is_done` to see if the user has finalised their
/// choices. Collect the choices by consuming the `Selector` with
/// `Selector::collect()`
pub struct Selector<
    T: 'static + PartialEq + std::fmt::Debug + Clone + Sync + Send,
    F: 'static + Sync + Send,
> {
    /// allow only one selection
    single_only: bool,
    entries: Vec<(T, bool)>,
    entry_titles: Vec<String>,
    theme_default: ThemeAttribute,

    cursor: SelectorCursor,
    scroll_x_cursor: usize,
    movement: Option<PageMovement>,
    vertical_alignment: Alignment,
    horizontal_alignment: Alignment,
    title: String,
    content: Screen<Virtual>,
    initialized: bool,
    /// If `true`, user has finished their selection
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

impl<T: 'static + PartialEq + std::fmt::Debug + Clone + Sync + Send, F: 'static + Sync + Send>
    std::fmt::Debug for Selector<T, F>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("Selector", f)
    }
}

impl<T: 'static + PartialEq + std::fmt::Debug + Clone + Sync + Send, F: 'static + Sync + Send>
    std::fmt::Display for Selector<T, F>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("Selector", f)
    }
}

impl<T: 'static + PartialEq + std::fmt::Debug + Clone + Sync + Send, F: 'static + Sync + Send>
    PartialEq for Selector<T, F>
{
    fn eq(&self, other: &Self) -> bool {
        self.entries == other.entries
    }
}

impl<T: 'static + PartialEq + std::fmt::Debug + Clone + Sync + Send> Component for UIDialog<T> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        Selector::draw(self, grid, area, context);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.initialise(context);
            self.set_dirty(true);
            return false;
        }

        let shortcuts = self.shortcuts(context);
        match (event, self.cursor) {
            (UIEvent::Input(Key::Char('\n')), _) if self.single_only => {
                /* User can only select one entry, so Enter key finalises the selection */
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    self.unrealize(context);
                }
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Entry(c)) if !self.single_only => {
                /* User can select multiple entries, so Enter key toggles the entry under the
                 * cursor */
                self.entries[c].1 = !self.entries[c].1;
                self.dirty = true;
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Ok) if !self.single_only => {
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    self.unrealize(context);
                }
                return true;
            }
            (UIEvent::Input(Key::Esc), _) => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                if !self.done {
                    self.unrealize(context);
                }
                self.done = true;
                _ = self.done();
                self.cancel(context);
                self.set_dirty(true);
                return false;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Cancel) if !self.single_only => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    self.unrealize(context);
                }
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Unfocused)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    self.entries[0].1 = true;
                }
                self.cursor = SelectorCursor::Entry(0);
                self.dirty = true;
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) && c > 0 =>
            {
                if self.single_only {
                    // Redraw selection
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                }
                self.cursor = SelectorCursor::Entry(c - 1);
                self.dirty = true;
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
            | (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) =>
            {
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                self.dirty = true;
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if c < self.entries.len().saturating_sub(1)
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    // Redraw selection
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                }
                self.cursor = SelectorCursor::Entry(c + 1);
                self.dirty = true;
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(_))
                if !self.single_only
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
            {
                self.cursor = SelectorCursor::Cancel;
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) =>
            {
                self.movement = Some(PageMovement::Left(1));
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
            {
                self.movement = Some(PageMovement::Right(1));
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["prev_page"]) =>
            {
                self.movement = Some(PageMovement::PageUp(1));
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["next_page"]) =>
            {
                self.movement = Some(PageMovement::PageDown(1));
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["home_page"]) =>
            {
                self.movement = Some(PageMovement::Home);
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["end_page"]) =>
            {
                self.movement = Some(PageMovement::End);
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), _)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"])
                    || shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                return true;
            }
            _ => {}
        }

        false
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
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
        self.initialized = false;
    }

    fn id(&self) -> ComponentId {
        self.id
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

        let shortcuts = self.shortcuts(context);
        match (event, self.cursor) {
            (UIEvent::Input(Key::Char('\n')), _) if self.single_only => {
                /* User can only select one entry, so Enter key finalises the selection */
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    self.unrealize(context);
                }
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Entry(c)) if !self.single_only => {
                /* User can select multiple entries, so Enter key toggles the entry under the
                 * cursor */
                self.entries[c].1 = !self.entries[c].1;
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Ok) if !self.single_only => {
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    self.unrealize(context);
                }
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(Key::Esc), _) => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                if !self.done {
                    self.unrealize(context);
                }
                self.done = true;
                _ = self.done();
                self.cancel(context);
                self.set_dirty(true);
                self.initialized = false;
                return false;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Cancel) if !self.single_only => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                if let Some(event) = self.done() {
                    context.replies.push_back(event);
                    self.unrealize(context);
                }
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) && c > 0 =>
            {
                if self.single_only {
                    // Redraw selection
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                }
                self.cursor = SelectorCursor::Entry(c - 1);
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
            | (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) =>
            {
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Unfocused)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    self.entries[0].1 = true;
                }
                self.cursor = SelectorCursor::Entry(0);
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(c))
                if c < self.entries.len().saturating_sub(1)
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                if self.single_only {
                    // Redraw selection
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                }
                self.cursor = SelectorCursor::Entry(c + 1);
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Entry(_))
                if !self.single_only
                    && shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Ok)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
            {
                self.cursor = SelectorCursor::Cancel;
                self.set_dirty(true);
                self.initialized = false;
                return true;
            }
            (UIEvent::Input(ref key), SelectorCursor::Cancel)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) =>
            {
                self.cursor = SelectorCursor::Ok;
                self.set_dirty(true);
                self.initialized = false;
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

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
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
}

impl<T: PartialEq + std::fmt::Debug + Clone + Sync + Send, F: 'static + Sync + Send>
    Selector<T, F>
{
    pub fn new(
        title: &str,
        mut entries: Vec<(T, String)>,
        single_only: bool,
        done_fn: F,
        context: &Context,
    ) -> Self {
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

        let theme_default = crate::conf::value(context, "theme_default");
        let mut ret = Self {
            single_only,
            entries: identifiers,
            entry_titles,
            cursor: SelectorCursor::Unfocused,
            scroll_x_cursor: 0,
            movement: None,
            vertical_alignment: Alignment::Center,
            horizontal_alignment: Alignment::Center,
            title: title.to_string(),
            content: Screen::<Virtual>::new(theme_default),
            initialized: false,
            done: false,
            done_fn,
            dirty: true,
            theme_default,
            id: ComponentId::default(),
        };
        ret.initialise(context);
        ret
    }

    fn initialise(&mut self, context: &Context) {
        self.theme_default = crate::conf::value(context, "theme_default");
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

    fn initialize(&mut self, context: &Context) {
        let mut highlighted_attrs = crate::conf::value(context, "widgets.options.highlighted");
        if !context.settings.terminal.use_color() {
            highlighted_attrs.attrs |= Attr::REVERSE;
        }
        let shortcuts = context.settings.shortcuts.general.key_values();
        let navigate_help_string = format!(
            "Navigate options with {} to go down, {} to go up, select with {}, cancel with {}",
            shortcuts["scroll_down"],
            shortcuts["scroll_up"],
            Key::Char('\n'),
            Key::Esc
        );
        let width = std::cmp::max(
            self.entry_titles.iter().map(|e| e.len()).max().unwrap_or(0) + 3,
            std::cmp::max(self.title.len(), navigate_help_string.len()) + 3,
        ) + 3;
        let height = self.entries.len()
            // padding
            + 3
            // buttons row
            + if self.single_only { 1 } else { 5 };
        if !self.content.resize_with_context(width, height, context) {
            self.dirty = false;
            return;
        }

        let inner_area = self.content.area();
        let (_, y) = self.content.grid_mut().write_string(
            &self.title,
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs | Attr::BOLD,
            inner_area.skip_cols(2),
            None,
            None,
        );

        let y = self
            .content
            .grid_mut()
            .write_string(
                &navigate_help_string,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs | Attr::ITALICS,
                inner_area.skip_cols(2).skip_rows(y + 2),
                None,
                None,
            )
            .1
            + y
            + 2;

        let inner_area = inner_area.skip_cols(1).skip_rows(y + 2);

        /* Extra room for buttons Okay/Cancel */
        if self.single_only {
            for (i, e) in self.entry_titles.iter().enumerate() {
                let attr = if matches!(self.cursor, SelectorCursor::Entry(e) if e == i) {
                    highlighted_attrs
                } else {
                    self.theme_default
                };
                self.content.grid_mut().write_string(
                    e,
                    attr.fg,
                    attr.bg,
                    attr.attrs,
                    inner_area.nth_row(i),
                    None,
                    None,
                );
            }
        } else {
            for (i, e) in self.entry_titles.iter().enumerate() {
                let attr = if matches!(self.cursor, SelectorCursor::Entry(e) if e == i) {
                    highlighted_attrs
                } else {
                    self.theme_default
                };
                self.content.grid_mut().write_string(
                    &format!("[{}] {}", if self.entries[i].1 { "x" } else { " " }, e),
                    attr.fg,
                    attr.bg,
                    attr.attrs,
                    inner_area.nth_row(i),
                    None,
                    None,
                );
            }
            let inner_area = inner_area.nth_row(self.entry_titles.len() + 2).skip_cols(2);
            let attr = if matches!(self.cursor, SelectorCursor::Ok) {
                highlighted_attrs
            } else {
                self.theme_default
            };
            let (x, y) = self.content.grid_mut().write_string(
                OK,
                attr.fg,
                attr.bg,
                attr.attrs | Attr::BOLD,
                inner_area,
                None,
                None,
            );
            let attr = if matches!(self.cursor, SelectorCursor::Cancel) {
                highlighted_attrs
            } else {
                self.theme_default
            };
            self.content.grid_mut().write_string(
                CANCEL,
                attr.fg,
                attr.bg,
                attr.attrs,
                inner_area.skip(CANCEL_OFFSET + x, y),
                None,
                None,
            );
        }
        self.initialized = true;
    }

    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let mut highlighted_attrs = crate::conf::value(context, "widgets.options.highlighted");
        if !context.settings.terminal.use_color() {
            highlighted_attrs.attrs |= Attr::REVERSE;
        }
        if !self.initialized {
            // [ref:FIXME]: don't re-initialize when the only change is highlight index.
            self.initialize(context);
        }
        let (width, height) = self.content.area().size();
        let dialog_area = area.align_inside(
            (width + 2, height + 2),
            self.horizontal_alignment,
            self.vertical_alignment,
        );
        let inner_area = create_box(grid, dialog_area);
        let rows = inner_area.height();
        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::Up(_) | PageMovement::Down(_) => {}
                PageMovement::Right(amount) => {
                    self.scroll_x_cursor = self.scroll_x_cursor.saturating_add(amount);
                }
                PageMovement::Left(amount) => {
                    self.scroll_x_cursor = self.scroll_x_cursor.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => match self.cursor {
                    SelectorCursor::Unfocused => {
                        self.cursor = SelectorCursor::Entry(0);
                        self.initialize(context);
                    }
                    SelectorCursor::Entry(c) => {
                        self.cursor = SelectorCursor::Entry(c.saturating_sub(multiplier * rows));
                        self.initialize(context);
                    }
                    SelectorCursor::Ok | SelectorCursor::Cancel
                        if !self.entry_titles.is_empty() =>
                    {
                        self.cursor = SelectorCursor::Entry(
                            self.entry_titles.len().saturating_sub(multiplier * rows),
                        );
                        self.initialize(context);
                    }
                    SelectorCursor::Ok | SelectorCursor::Cancel => {}
                },
                PageMovement::PageDown(multiplier) => match self.cursor {
                    SelectorCursor::Unfocused => {
                        self.cursor = SelectorCursor::Entry(
                            self.entry_titles
                                .len()
                                .saturating_sub(1)
                                .min(multiplier * rows),
                        );
                        self.initialize(context);
                    }
                    SelectorCursor::Entry(c)
                        if c.saturating_add(multiplier * rows) < self.entry_titles.len()
                            && !self.entry_titles.is_empty() =>
                    {
                        self.cursor = SelectorCursor::Entry(
                            self.entry_titles
                                .len()
                                .saturating_sub(1)
                                .min(c.saturating_add(multiplier * rows)),
                        );
                        self.initialize(context);
                    }
                    SelectorCursor::Entry(_) => {
                        self.cursor = SelectorCursor::Ok;
                        self.initialize(context);
                    }
                    SelectorCursor::Ok | SelectorCursor::Cancel => {}
                },
                PageMovement::Home if !self.entry_titles.is_empty() => {
                    self.cursor = SelectorCursor::Entry(0);
                    self.initialize(context);
                }
                PageMovement::End
                    if matches!(self.cursor, SelectorCursor::Ok | SelectorCursor::Cancel) => {}
                PageMovement::End
                    if !matches!(self.cursor, SelectorCursor::Entry(c) if c +1 == self.entry_titles.len())
                        && !self.entry_titles.is_empty() =>
                {
                    self.cursor = SelectorCursor::Entry(self.entry_titles.len().saturating_sub(1));
                    self.initialize(context);
                }
                PageMovement::Home | PageMovement::End => {}
            }
        }
        let skip_rows = match self.cursor {
            SelectorCursor::Unfocused => 0,
            SelectorCursor::Entry(e) if e >= rows => e.min(height.saturating_sub(rows)),
            SelectorCursor::Entry(_) => 0,
            SelectorCursor::Ok | SelectorCursor::Cancel => height.saturating_sub(rows),
        };

        self.scroll_x_cursor = self
            .scroll_x_cursor
            .min(width.saturating_sub(inner_area.width()));
        grid.copy_area(
            self.content.grid(),
            inner_area,
            self.content
                .area()
                .skip_cols(self.scroll_x_cursor)
                .skip_rows(skip_rows),
        );

        if height > dialog_area.height() {
            let inner_area = inner_area.skip_rows(1);
            ScrollBar::default().set_show_arrows(true).draw(
                grid,
                inner_area.nth_col(inner_area.width().saturating_sub(1)),
                context,
                // position
                skip_rows,
                // visible_rows
                inner_area.height(),
                // length
                height,
            );
        }
        if width > dialog_area.width() {
            let inner_area = inner_area.skip_cols(1);
            ScrollBar::default().set_show_arrows(true).draw_horizontal(
                grid,
                inner_area.nth_row(inner_area.height().saturating_sub(1)),
                context,
                // position
                self.scroll_x_cursor,
                // visible_cols
                inner_area.width(),
                // length
                width,
            );
        }
        context.dirty_areas.push_back(dialog_area);
        self.dirty = false;
    }
}

impl<T: 'static + PartialEq + std::fmt::Debug + Clone + Sync + Send> UIDialog<T> {
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
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
        })
    }

    fn cancel(&self, context: &mut Context) {
        context.unrealized.insert(self.id());
        context
            .replies
            .push_back(UIEvent::ComponentUnrealize(self.id()));
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
                    .any(std::convert::identity),
            )
        })
    }

    fn cancel(&self, context: &mut Context) {
        context.unrealized.insert(self.id());
        context
            .replies
            .push_back(UIEvent::ComponentUnrealize(self.id()));
    }
}
