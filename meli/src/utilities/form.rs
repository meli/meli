//
// meli
//
// Copyright 2017-2020, 2026 Manos Pitsidianakis
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

//! Forms and text input widgets.

use std::{borrow::Cow, sync::Arc};

use melib::{
    error::{Error, ErrorKind, Result},
    text::Truncate as _,
    utils::shellexpand::ShellExpandTrait as _,
};

use crate::{
    components::prelude::*,
    terminal::text_editing::UText,
    utilities::autocomplete::{AutoComplete, AutoCompleteFn},
};

trait GetIndex {
    type Value;

    fn get_indexed(&self, index: usize) -> Option<&Self::Value>;
    fn get_indexed_mut(&mut self, index: usize) -> Option<&mut Self::Value>;
}

impl<K, V, S> GetIndex for IndexMap<K, V, S> {
    type Value = V;

    #[inline(always)]
    fn get_indexed(&self, index: usize) -> Option<&V> {
        Some(self.get_index(index)?.1)
    }

    #[inline(always)]
    fn get_indexed_mut(&mut self, index: usize) -> Option<&mut V> {
        Some(self.get_index_mut(index)?.1)
    }
}

pub type ValidateFn = Arc<dyn Fn(&str) -> bool + Send + Sync>;

/// Text input widget.
#[derive(Clone)]
pub struct TextField {
    inner: UText,
    read_only: bool,
    autocomplete: Option<(Arc<AutoCompleteFn>, Box<AutoComplete>)>,
    validate_fn: Option<ValidateFn>,
    theme_attr: Option<ThemeAttribute>,
    read_only_attr: Option<ThemeAttribute>,
    highlight: Option<ThemeAttribute>,
    invalid_attr: Option<ThemeAttribute>,
    id: ComponentId,
}

impl std::fmt::Display for TextField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.as_str())
    }
}

impl std::fmt::Debug for TextField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(TextField))
            .field("id", &self.id)
            .field("inner", &self.inner)
            .field("read_only", &self.read_only)
            .field("has validate_fn", &self.validate_fn.is_some())
            .field("has AutoComplete", &self.autocomplete.is_some())
            .finish_non_exhaustive()
    }
}

impl Default for TextField {
    fn default() -> Self {
        Self {
            inner: UText::new(String::with_capacity(256)),
            read_only: false,
            autocomplete: None,
            validate_fn: None,
            theme_attr: None,
            read_only_attr: None,
            invalid_attr: None,
            highlight: None,
            id: ComponentId::default(),
        }
    }
}

impl TextField {
    pub fn new(inner: UText, autocomplete: Option<(AutoCompleteFn, Box<AutoComplete>)>) -> Self {
        Self {
            inner,
            autocomplete: autocomplete.map(|(a, b)| (Arc::new(a), b)),
            ..Self::default()
        }
    }

    #[inline]
    pub fn with_read_only(self, read_only: bool) -> Self {
        Self { read_only, ..self }
    }

    /// Text content as a string slice.
    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    /// Set [`Self::read_only`] value
    pub fn set_read_only(&mut self, new_val: bool) -> &mut Self {
        self.read_only = new_val;
        self
    }

    /// Reset with new content.
    pub fn set_content(&mut self, new_val: String) {
        self.inner.set_content(new_val);
    }

    /// Cursor position as seen by the user: the cursor is seen moving by
    /// jumping between graphemes boundaries.
    pub fn cursor(&self) -> usize {
        self.inner.grapheme_pos()
    }

    /// Byte cursor position meant to be used for accessing the inner string
    /// slice.
    pub fn byte_cursor(&self) -> usize {
        self.inner.cursor_pos()
    }

    /// Increase the cursor position to the next grapheme boundary.
    pub fn cursor_inc(&mut self) {
        self.inner.cursor_inc();
    }

    /// Decrease the cursor position to the previous grapheme boundary.
    pub fn cursor_dec(&mut self) {
        self.inner.cursor_dec();
    }

    /// Returns `true` if inner text is empty.
    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    /// Consume self and return the inner text as an owned string.
    pub fn into_string(self) -> String {
        self.inner.into_string()
    }

    /// Clear inner string content and set cursor to zero.
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    /// Draw cursor at given area along with autocomplete suggestions, if any.
    pub fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        let width = area.width();
        let pos = if width < self.inner.grapheme_pos() {
            width
        } else {
            self.inner.grapheme_pos()
        };

        grid.change_theme(
            area.skip_cols(pos).take_cols(1),
            *self
                .highlight
                .get_or_insert_with(|| crate::conf::theme_value(context, "highlight")),
        );
        if let Some((autocomplete_fn, autocomplete)) = self.autocomplete.as_mut() {
            let entries = autocomplete_fn(context, self.inner.as_str());
            autocomplete.set_suggestions(entries);
            autocomplete.draw(grid, secondary_area, context);
        }
    }

    #[inline]
    pub fn set_validate_fn(&mut self, validate_fn: Option<ValidateFn>) -> &mut Self {
        self.validate_fn = validate_fn;
        self
    }
}

impl Component for TextField {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let Self {
            inner,
            read_only,
            validate_fn,
            theme_attr,
            read_only_attr,
            invalid_attr,
            ..
        } = self;
        let width = area.width();
        /* Calculate which part of the str is visible
         * ##########################################
         *
         * Example:
         * For the string "The quick brown fox jumps over the lazy dog" with visible
         * width of field of 10 columns
         *
         *
         * Cursor <= width
         * =================
         * Cursor at:
         * ⇩
         * The quick brown fox jumps over the lazy dog
         *
         * cursor
         * ⇩
         * ┌──────────┐
         * │The quick │ brown fox jumps over the lazy dog
         * └──────────┘
         *
         * No skip.
         *
         * Cursor at the end
         * =================
         * Cursor at:
         *                                           ⇩
         * The quick brown fox jumps over the lazy dog
         *
         * remainder                                        cursor
         * ⇩⇩⇩⇩⇩                                              ⇩
         * +╌╌╌+╭┅┅┅┅┅┅┅┅┅┅╮╭┅┅┅┅┅┅┅┅┅┅╮╭┅┅┅┅┅┅┅┅┅┅╮┌──────────┐
         * |The|┊ quick bro┊┊wn fox jum┊┊ps over th┊│e lazy dog│
         * +╌╌╌+╰┅┅┅┅┅┅┅┅┅┅╯╰┅┅┅┅┅┅┅┅┅┅╯╰┅┅┅┅┅┅┅┅┅┅╯└──────────┘
         *  ⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧
         *              skip offset
         *
         * Intermediate cursor
         * ===================
         * Cursor at:
         *                               ⇩
         * The quick brown fox jumps over the lazy dog
         *
         * remainder                        cursor
         * ⇩                                  ⇩
         * +╭┅┅┅┅┅┅┅┅┅┅╮╭┅┅┅┅┅┅┅┅┅┅╮┌──────────┐
         * T|he quick b┊┊rown fox j┊│umps over │ the lazy dog
         * +╰┅┅┅┅┅┅┅┅┅┅╯╰┅┅┅┅┅┅┅┅┅┅╯└──────────┘
         * ⇧+⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧
         *              skip offset
         */
        let str = inner.as_str();
        grid.write_string(
            if width > 0 && width < inner.grapheme_pos() {
                str.trim_left_at_boundary(
                    width * inner.grapheme_pos().wrapping_div(width).saturating_sub(1)
                        + inner.grapheme_pos().wrapping_rem(width),
                )
            } else {
                str
            },
            if let Some(ref validate_fn) = validate_fn {
                if !validate_fn(str) {
                    &*invalid_attr
                        .get_or_insert_with(|| crate::conf::theme_value(context, "text.error"))
                } else if *read_only {
                    &*read_only_attr.get_or_insert_with(|| {
                        crate::conf::theme_value(context, "widgets.form.read_only")
                    })
                } else {
                    &*theme_attr.get_or_insert_with(|| {
                        crate::conf::theme_value(context, "widgets.form.field")
                    })
                }
            } else if *read_only {
                &*read_only_attr.get_or_insert_with(|| {
                    crate::conf::theme_value(context, "widgets.form.read_only")
                })
            } else {
                &*theme_attr
                    .get_or_insert_with(|| crate::conf::theme_value(context, "widgets.form.field"))
            },
            area,
            None,
            None,
        );
    }

    fn process_event(&mut self, event: &mut UIEvent, _: &mut Context) -> bool {
        if self.read_only {
            return false;
        }

        match *event {
            UIEvent::InsertInput(Key::Char('\t')) => {
                if let Some(suggestion) = self
                    .autocomplete
                    .as_mut()
                    .and_then(|a| a.1.get_suggestion())
                {
                    self.inner = UText::new(suggestion);
                    let len = self.inner.as_str().len();
                    self.inner.set_cursor(len);
                } else {
                    self.inner.insert_char(' ');
                }
            }
            UIEvent::InsertInput(Key::Char('\n')) => {
                if let Some(suggestion) = self
                    .autocomplete
                    .as_mut()
                    .and_then(|a| a.1.get_suggestion())
                {
                    self.inner = UText::new(suggestion);
                    let len = self.inner.as_str().len();
                    self.inner.set_cursor(len);
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Up) => {
                if let Some(ac) = self.autocomplete.as_mut() {
                    ac.1.dec_cursor();
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Down) => {
                if let Some(ac) = self.autocomplete.as_mut() {
                    ac.1.inc_cursor();
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Right) => {
                self.inner.cursor_inc();
            }
            UIEvent::InsertInput(Key::Left) => {
                self.inner.cursor_dec();
            }
            UIEvent::InsertInput(Key::Char(k)) => {
                self.inner.insert_char(k);
            }
            UIEvent::InsertInput(Key::Paste(ref p)) => {
                for c in p.chars() {
                    self.inner.insert_char(c);
                }
            }
            UIEvent::InsertInput(Key::Alt('b')) => {
                // Meta+B Backward one alphanumeric word

                let initial = self.inner.cursor_pos();
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .chars()
                    .last()
                    .map(|c| c.is_ascii_whitespace())
                    .unwrap_or_default()
                {
                    if !self.inner.cursor_dec() {
                        break;
                    }
                }
                if self.inner.cursor_pos() == initial {
                    // We didn't skip whitespace to reach the end of the previous word, so skip
                    // nonwhitespace instead.
                    while self.inner.as_str()[..self.inner.cursor_pos()]
                        .chars()
                        .last()
                        .map(|c| !c.is_ascii_whitespace())
                        .unwrap_or_default()
                    {
                        if !self.inner.cursor_dec() {
                            break;
                        }
                    }
                }
            }
            UIEvent::InsertInput(Key::Alt('f')) => {
                // Meta+F Forward one alphanumeric word (Move to its end)

                let initial = self.inner.cursor_pos();
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .chars()
                    .last()
                    .map(|c| c.is_ascii_whitespace())
                    .unwrap_or_default()
                {
                    if !self.inner.cursor_inc() {
                        break;
                    }
                }
                if self.inner.cursor_pos() == initial {
                    // We didn't skip whitespace to reach the start of the next word, so skip
                    // nonwhitespace instead.
                    while self.inner.as_str()[..self.inner.cursor_pos()]
                        .chars()
                        .last()
                        .map(|c| !c.is_ascii_whitespace())
                        .unwrap_or_default()
                    {
                        if !self.inner.cursor_inc() {
                            break;
                        }
                    }
                } else {
                    // self.inner.cursor_dec();
                }
            }
            UIEvent::InsertInput(Key::Backspace) | UIEvent::InsertInput(Key::Ctrl('h')) => {
                // Ctrl+H  Delete previous character
                if self.inner.backspace() {
                    if let Some(ac) = self.autocomplete.as_mut() {
                        ac.1.set_suggestions(Vec::new());
                    }
                }
            }
            UIEvent::InsertInput(Key::Delete) | UIEvent::InsertInput(Key::Ctrl('d')) => {
                // Delete one character
                if self.inner.cursor_inc() {
                    self.inner.backspace();
                }
            }
            UIEvent::InsertInput(Key::Ctrl('t')) => {
                // Ctrl+T Transpose characters
                self.inner.transpose();
            }
            UIEvent::InsertInput(Key::Ctrl('a')) | UIEvent::InsertInput(Key::Home) => {
                // Beginning of line
                self.inner.set_cursor(0);
            }
            UIEvent::InsertInput(Key::Ctrl('b')) => {
                // Backward one character
                self.inner.cursor_dec();
            }
            UIEvent::InsertInput(Key::Ctrl('f')) => {
                // Ctrl+F / → Forward one character
                self.inner.cursor_inc();
            }
            UIEvent::InsertInput(Key::Ctrl('w')) => {
                // Cut previous word
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .last_grapheme()
                    .map(|(_, graph)| !graph.is_empty() && graph.trim().is_empty())
                    .unwrap_or(false)
                {
                    if !self.inner.backspace() {
                        break;
                    }
                }
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .last_grapheme()
                    .map(|(_, graph)| !graph.is_empty() && !graph.trim().is_empty())
                    .unwrap_or(false)
                {
                    if !self.inner.backspace() {
                        break;
                    }
                }
            }
            UIEvent::InsertInput(Key::Ctrl('u')) => self.inner.cut_left(),
            UIEvent::InsertInput(Key::Ctrl('e')) | UIEvent::InsertInput(Key::End) => {
                // Ctrl+E End of line
                self.inner.set_cursor(self.inner.as_str().len());
            }
            _ => {
                return false;
            }
        }
        true
    }

    fn is_dirty(&self) -> bool {
        false
    }

    fn set_dirty(&mut self, _value: bool) {}

    fn shortcuts(&self, _: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        if !self.read_only {
            let mut our_map: ShortcutMap = Default::default();
            our_map.insert("toggle field editing", Key::Char('\n'));
            map.insert("text entry", our_map);
        }

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum FormFocus {
    #[default]
    Fields,
    Buttons,
    TextInput,
}

#[derive(Clone, Debug)]
pub struct ChoiceField {
    pub values: Vec<Cow<'static, str>>,
    pub cursor: usize,
    pub id: ComponentId,
}

impl std::fmt::Display for ChoiceField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.values[self.cursor], fmt)
    }
}

#[derive(Clone)]
pub struct DictField {
    pub values: Box<FormWidget<(), Cow<'static, str>, Field>>,
    /// If `true`, it implies the dict field represents a set of unique values instead of a
    /// dictionary, i.e. it hides the keys.
    pub is_a_set: bool,
    pub rename_entry: Option<Box<FormWidget<FormButtonAction, Cow<'static, str>, TextField>>>,
    pub new_entry_constructor: Arc<dyn Fn() -> Field + Send + Sync>,
}

impl std::fmt::Debug for DictField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.values, fmt)
    }
}

impl std::fmt::Display for DictField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.values, fmt)
    }
}

impl DictField {
    pub const SHORTCUTS_KEY: &str = "dict entry";
    pub const SHORTCUTS_ADD: &str = "add entry";
    pub const SHORTCUTS_REMOVE: &str = "remove entry";
    pub const SHORTCUTS_MOVE_UP: &str = "move entry up";
    pub const SHORTCUTS_MOVE_DOWN: &str = "move entry down";
    pub const SHORTCUTS_RENAME: &str = "rename entry";

    /// Draw cursor at given area along with autocomplete suggestions, if any.
    pub fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        let Self { values, .. } = self;
        let cursor = values.cursor;
        let Some((_, value)) = values.values_mut().get_index_mut(cursor) else {
            return;
        };
        value.draw_cursor(grid, area, secondary_area, context);
    }
}

impl Component for DictField {
    fn draw(&mut self, grid: &mut CellBuffer, mut area: Area, context: &mut Context) {
        if let Some(rename_entry) = &mut self.rename_entry {
            rename_entry.draw(grid, area, context);
            return;
        }

        if self.values.is_dirty() {
            self.values.hide_field_names = self.is_a_set;
            if self.values.focused {
                let shortcuts = self.shortcuts(context);
                let help_area = area.take_rows(1);
                let theme_attr = crate::conf::theme_value(context, "widgets.form.field");
                let help = format!(
                    "{add}/{remove} to add/remove, {move_up}/{move_down} to move, {rename} to \
                     rename",
                    add = shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_ADD],
                    remove = shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_REMOVE],
                    move_up = shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_MOVE_UP],
                    move_down = shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_MOVE_DOWN],
                    rename = shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_RENAME],
                );
                grid.write_string(&help, &theme_attr, help_area, None, None);
                context.dirty_areas.push_back(help_area);
                area = area.skip_rows(1);
            }
            self.values.draw(grid, area, context);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(rename_entry) = &mut self.rename_entry {
            let processed = rename_entry.process_event(event, context);
            if processed {
                match rename_entry.buttons_result() {
                    Some(FormButtonAction::Accept) => {
                        let (previous_id, new_id) =
                            rename_entry.values().get_index(0).expect("one entry");
                        let previous_id = previous_id.clone();
                        let new_id = new_id.as_str().to_string().into();
                        if let Err(err) = self.values.rename_field_label(&previous_id, new_id) {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Could not rename entry".into()),
                                body: err.to_string().into(),
                                kind: Some(crate::NotificationType::Error(err.kind)),
                                source: Some(err),
                            });
                        }
                        self.rename_entry = None;
                        self.set_dirty(true);
                    }
                    Some(FormButtonAction::Cancel) => {
                        self.rename_entry = None;
                        self.set_dirty(true);
                        return true;
                    }
                    _ => {}
                }
            }
            return processed;
        }

        if self.values.process_event(event, context) {
            return true;
        }

        let shortcuts = self.shortcuts(context);

        match event {
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_RENAME]) =>
            {
                if self.is_a_set {
                    self.values
                        .process_event(&mut UIEvent::Input(Key::Char('\n')), context);
                } else if let Some(field) = self
                    .values
                    .fields
                    .get_index(self.values.cursor)
                    .map(|(k, _)| k.clone())
                {
                    let mut rename_entry = FormWidget::<_, _, TextField>::new(
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
                    rename_entry.padding = 0;
                    rename_entry.add_button(("Save".into(), FormButtonAction::Accept));
                    let value = field.as_ref().to_owned();
                    rename_entry.push((field, value));
                    self.rename_entry = Some(Box::new(rename_entry));
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_ADD]) =>
            {
                let field = (self.new_entry_constructor)();
                let mut ctr = self.values.values().len();
                let mut new_key = format!("k{ctr}");
                while self
                    .values
                    .values()
                    .contains_key(&Cow::Borrowed(new_key.as_str()))
                {
                    ctr += 1;
                    new_key = format!("k{ctr}");
                }
                self.values.push_field((new_key.into(), field));
                self.set_dirty(true);
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_REMOVE]) =>
            {
                let cursor = self.values.cursor;
                if cursor < self.values.fields.len() {
                    self.values.fields.shift_remove_index(cursor);
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_MOVE_UP]) =>
            {
                let cursor = self.values.cursor;
                if cursor > 0 {
                    self.values.fields.swap_indices(cursor, cursor - 1);
                    self.values.cursor = cursor - 1;
                    self.set_dirty(true);
                }
                true
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::SHORTCUTS_KEY][Self::SHORTCUTS_MOVE_DOWN]) =>
            {
                let cursor = self.values.cursor;
                if cursor < self.values.fields.len().saturating_sub(1) {
                    self.values.fields.swap_indices(cursor, cursor + 1);
                    self.values.cursor = cursor + 1;
                    self.set_dirty(true);
                }
                true
            }
            _ => false,
        }
    }

    fn is_dirty(&self) -> bool {
        self.values.is_dirty()
            || self
                .rename_entry
                .as_ref()
                .map(|f| f.is_dirty())
                .unwrap_or(false)
    }

    fn set_dirty(&mut self, value: bool) {
        if let Some(rename_entry) = &mut self.rename_entry {
            rename_entry.set_dirty(value);
        }
        self.values.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        if let Some(rename_entry) = &self.rename_entry {
            return rename_entry.shortcuts(context);
        }
        let mut map = ShortcutMaps::default();
        let mut our_map: ShortcutMap = Default::default();
        our_map.insert(Self::SHORTCUTS_RENAME, Key::F(2));
        our_map.insert(Self::SHORTCUTS_ADD, Key::Char('+'));
        our_map.insert(Self::SHORTCUTS_REMOVE, Key::Char('-'));
        our_map.insert(Self::SHORTCUTS_MOVE_UP, Key::PageUp);
        our_map.insert(Self::SHORTCUTS_MOVE_DOWN, Key::PageDown);
        map.insert(Self::SHORTCUTS_KEY, our_map);

        map.extend(self.values.shortcuts(context));

        map
    }

    fn id(&self) -> ComponentId {
        self.values.id
    }
}

#[derive(Clone)]
pub enum Field {
    Text(TextField),
    Choice(ChoiceField),
    Dict(DictField),
}

impl std::fmt::Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Text(t) => std::fmt::Debug::fmt(t, f),
            Self::Choice(c) => std::fmt::Debug::fmt(c, f),
            Self::Dict(d) => std::fmt::Debug::fmt(d, f),
        }
    }
}

impl Default for Field {
    fn default() -> Self {
        Self::Text(TextField::default())
    }
}

impl Field {
    pub fn as_text_field(&self) -> Option<&TextField> {
        let Self::Text(field) = self else {
            return None;
        };
        Some(field)
    }

    pub fn as_choice_field(&self) -> Option<&ChoiceField> {
        let Self::Choice(field) = self else {
            return None;
        };
        Some(field)
    }

    pub fn as_dict_field(&self) -> Option<&DictField> {
        let Self::Dict(field) = self else {
            return None;
        };
        Some(field)
    }

    pub fn as_json_value(&self) -> serde_json::Value {
        match self {
            Self::Text(ref s) => serde_json::Value::String(s.as_str().to_owned()),
            Self::Choice(ChoiceField { values, cursor, .. }) => {
                serde_json::Value::String(if values.is_empty() {
                    String::new()
                } else {
                    values[*cursor].as_ref().to_owned()
                })
            }
            Self::Dict(DictField { values, .. }) => serde_json::Value::Object(
                values
                    .values()
                    .iter()
                    .map(|(k, f)| (k.as_ref().to_owned(), f.as_json_value()))
                    .collect(),
            ),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Text(ref s) => Some(s.as_str()),
            Self::Choice(ChoiceField {
                ref values, cursor, ..
            }) => Some(if values.is_empty() {
                ""
            } else {
                values[*cursor].as_ref()
            }),
            _ => None,
        }
    }

    pub fn cursor(&self) -> usize {
        match self {
            Self::Text(s) => s.cursor(),
            Self::Choice(ChoiceField { cursor, .. }) => *cursor,
            Self::Dict(DictField { values, .. }) => values.cursor(),
        }
    }

    pub fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        match self {
            Self::Text(text_field) => text_field.draw_cursor(grid, area, secondary_area, context),
            Self::Choice(_) => {}
            Self::Dict(dict_field) => dict_field.draw_cursor(grid, area, secondary_area, context),
        }
    }
}

impl Component for Field {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let theme_attr = crate::conf::theme_value(context, "widgets.form.field");
        match self {
            Self::Text(text_field) => {
                text_field.draw(grid, area, context);
            }
            Self::Choice(ChoiceField {
                ref values, cursor, ..
            }) => {
                let value = if values.is_empty() {
                    ""
                } else {
                    values[*cursor].as_ref()
                };
                grid.write_string(value, &theme_attr, area, None, None);
            }
            Self::Dict(dict_field) => {
                dict_field.draw(grid, area, context);
            }
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Self::Text(t) = self {
            if t.process_event(event, context) {
                return true;
            }
        }
        if let Self::Dict(d) = self {
            if d.process_event(event, context) {
                return true;
            }
        }

        match *event {
            UIEvent::InsertInput(Key::Up) => match self {
                Self::Text(_) | Self::Dict(_) => {}
                Self::Choice(ChoiceField { cursor, values, .. }) => {
                    if *cursor == 0 {
                        *cursor = values.len().saturating_sub(1);
                    } else {
                        *cursor -= 1;
                    }
                    self.set_dirty(true);
                    return true;
                }
            },
            UIEvent::InsertInput(Key::Down) => match self {
                Self::Text(_) | Self::Dict(_) => {}
                Self::Choice(ChoiceField { values, cursor, .. }) => {
                    *cursor = if *cursor == values.len().saturating_sub(1) {
                        0
                    } else {
                        *cursor + 1
                    };
                    self.set_dirty(true);
                    return true;
                }
            },
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        match self {
            Self::Text(t) => t.is_dirty(),
            Self::Choice(_) => false,
            Self::Dict(d) => d.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        match self {
            Self::Text(t) => t.set_dirty(value),
            Self::Choice(_) => {}
            Self::Dict(d) => d.set_dirty(value),
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        match self {
            Self::Text(t) => t.shortcuts(context),
            Self::Choice(_) => ShortcutMaps::default(),
            Self::Dict(d) => d.shortcuts(context),
        }
    }

    fn id(&self) -> ComponentId {
        match self {
            Self::Text(t) => t.id(),
            Self::Choice(c) => c.id,
            Self::Dict(d) => d.id(),
        }
    }
}

impl std::fmt::Display for Field {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Text(s) => std::fmt::Display::fmt(s, fmt),
            Self::Choice(c) => std::fmt::Display::fmt(c, fmt),
            Self::Dict(d) => std::fmt::Display::fmt(d, fmt),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum FormButtonAction {
    Accept,
    Reset,
    #[default]
    Cancel,
    Other(&'static str),
}

pub trait FormWidgetLabel:
    std::fmt::Debug
    + std::fmt::Display
    + Clone
    + Ord
    + std::hash::Hash
    + Eq
    + PartialOrd
    + Send
    + Sync
    + AsRef<str>
{
    fn display(&'_ self) -> Cow<'_, str> {
        Cow::Borrowed(self.as_ref())
    }
}

impl FormWidgetLabel for Cow<'static, str> {}
impl FormWidgetLabel for melib::HeaderName {
    fn display(&'_ self) -> Cow<'_, str> {
        Cow::Owned(self.to_string())
    }
}

pub trait FormWidgetField: std::fmt::Debug + Component + Clone + Send + Sync {
    fn read_only(&self) -> bool;

    fn set_focused(&mut self, focused: bool);

    /// Height (as in rows to render)
    fn height(&self) -> usize;

    fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    );
}

impl FormWidgetField for Field {
    fn read_only(&self) -> bool {
        match self {
            Self::Text(t) => t.read_only,
            Self::Choice(_) => false,
            Self::Dict(_) => false,
        }
    }

    fn set_focused(&mut self, focused: bool) {
        match self {
            Self::Text(_) | Self::Choice(_) => {}
            Self::Dict(d) => {
                if let Some(rename_entry) = &mut d.rename_entry {
                    rename_entry.set_focused(focused);
                } else {
                    d.values.set_focused(focused);
                }
            }
        }
    }

    fn height(&self) -> usize {
        match self {
            Self::Text(_) | Self::Choice(_) => 1,
            Self::Dict(d) => {
                if let Some(rename_entry) = &d.rename_entry {
                    rename_entry
                        .values()
                        .values()
                        .map(|f| f.height())
                        .sum::<usize>()
                        .max(1)
                        + usize::from(!rename_entry.hide_buttons) * (1 + rename_entry.padding)
                } else {
                    let entries = d
                        .values
                        .values()
                        .values()
                        .map(|f| f.height())
                        .sum::<usize>();

                    (entries + usize::from(d.values.focused)).max(1)
                }
            }
        }
    }

    fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        Self::draw_cursor(self, grid, area, secondary_area, context);
    }
}

impl FormWidgetField for TextField {
    fn read_only(&self) -> bool {
        self.read_only
    }

    fn set_focused(&mut self, _: bool) {}

    fn height(&self) -> usize {
        1
    }

    fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        Self::draw_cursor(self, grid, area, secondary_area, context);
    }
}

#[derive(Debug)]
pub struct FormWidget<T, L: FormWidgetLabel = Cow<'static, str>, F: FormWidgetField = Field>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    fields: IndexMap<L, F>,
    padding: usize,
    buttons: ButtonWidget<T>,

    field_name_max_length: usize,
    cursor: usize,
    focus: FormFocus,
    focused: bool,
    hide_buttons: bool,
    hide_field_names: bool,
    dirty: bool,
    pub cursor_up_shortcut: Key,
    pub cursor_down_shortcut: Key,
    pub cursor_right_shortcut: Key,
    pub cursor_left_shortcut: Key,
    id: ComponentId,
}

impl<
        T: 'static + std::fmt::Debug + Copy + Default + Clone + Send + Sync,
        L: FormWidgetLabel,
        F: FormWidgetField,
    > Clone for FormWidget<T, L, F>
{
    fn clone(&self) -> Self {
        Self {
            fields: self.fields.clone(),
            padding: self.padding,
            buttons: self.buttons.clone(),
            focus: self.focus,
            focused: self.focused,
            hide_buttons: self.hide_buttons,
            hide_field_names: self.hide_field_names,
            field_name_max_length: self.field_name_max_length,
            cursor: self.cursor,
            dirty: true,
            cursor_up_shortcut: self.cursor_up_shortcut.clone(),
            cursor_down_shortcut: self.cursor_down_shortcut.clone(),
            cursor_right_shortcut: self.cursor_right_shortcut.clone(),
            cursor_left_shortcut: self.cursor_left_shortcut.clone(),
            id: ComponentId::default(),
        }
    }
}

impl<
        T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
        L: FormWidgetLabel,
        F: FormWidgetField,
    > Default for FormWidget<T, L, F>
{
    fn default() -> Self {
        Self {
            fields: Default::default(),
            buttons: Default::default(),
            padding: 3,
            focus: FormFocus::Fields,
            hide_buttons: false,
            hide_field_names: false,
            field_name_max_length: 10,
            cursor: 0,
            focused: true,
            dirty: true,
            cursor_up_shortcut: Key::Char('k'),
            cursor_down_shortcut: Key::Char('j'),
            cursor_right_shortcut: Key::Char('l'),
            cursor_left_shortcut: Key::Char('h'),
            id: ComponentId::default(),
        }
    }
}

impl<
        T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
        L: FormWidgetLabel,
        F: FormWidgetField,
    > std::fmt::Display for FormWidget<T, L, F>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "form")
    }
}

impl<
        T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
        L: FormWidgetLabel,
        F: FormWidgetField,
    > FormWidget<T, L, F>
{
    pub fn new(
        action: (Cow<'static, str>, T),
        cursor_up_shortcut: Key,
        cursor_down_shortcut: Key,
        cursor_right_shortcut: Key,
        cursor_left_shortcut: Key,
    ) -> Self {
        Self {
            buttons: ButtonWidget::new(
                action,
                cursor_right_shortcut.clone(),
                cursor_left_shortcut.clone(),
            ),
            focus: FormFocus::Fields,
            hide_buttons: false,
            hide_field_names: false,
            cursor_up_shortcut,
            cursor_down_shortcut,
            cursor_right_shortcut,
            cursor_left_shortcut,
            id: ComponentId::default(),
            dirty: true,
            ..Default::default()
        }
    }

    pub fn height(&self) -> usize {
        self.values()
            .values()
            .map(|f| f.height())
            .sum::<usize>()
            .max(1)
            + usize::from(!self.hide_buttons) * (1 + self.padding)
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn set_cursor(&mut self, new_cursor: usize) {
        self.cursor = new_cursor;
    }

    pub fn set_focused(&mut self, focused: bool) {
        self.focused = focused;
        self.dirty = true;
    }

    pub fn hide_buttons(&mut self) {
        self.hide_buttons = true;
        self.buttons.set_dirty(false);
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    pub fn add_button(&mut self, val: (Cow<'static, str>, T)) {
        self.buttons.push(val);
    }

    pub fn values(&self) -> &IndexMap<L, F> {
        &self.fields
    }

    pub fn values_mut(&mut self) -> &mut IndexMap<L, F> {
        &mut self.fields
    }

    pub fn collect(self) -> IndexMap<L, F> {
        self.fields
    }

    pub fn buttons_result(&mut self) -> Option<T> {
        self.buttons.result.take()
    }

    pub fn rename_field_label(&mut self, previous_label: &L, new_label: L) -> Result<()> {
        let Some(pos) = self.fields.keys().position(|l| l == previous_label) else {
            return Err(Error::new(format!("label {previous_label} not found"))
                .set_kind(ErrorKind::NotFound));
        };
        self.fields
            .replace_index(pos, new_label)
            .map_err(|(other_index, new_label)| {
                Error::new(format!(
                    "label {new_label} conflicts with entry {other_index}"
                ))
                .set_kind(ErrorKind::ValueError)
            })?;
        Ok(())
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync, L: FormWidgetLabel>
    FormWidget<T, L, Field>
{
    pub fn push_choices(
        &mut self,
        (field_name, values): (L, Vec<Cow<'static, str>>),
    ) -> &mut ChoiceField {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        self.fields.insert(
            field_name.clone(),
            Field::Choice(ChoiceField {
                values,
                cursor: 0,
                id: ComponentId::default(),
            }),
        );
        let Field::Choice(field) = &mut self.fields[&field_name] else {
            unreachable!()
        };
        field
    }

    pub fn push_cl(
        &mut self,
        (field_name, value, auto): (L, String, AutoCompleteFn),
    ) -> &mut TextField {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        self.fields.insert(
            field_name.clone(),
            Field::Text(TextField::new(
                UText::new(value),
                Some((auto, AutoComplete::new(Vec::new()))),
            )),
        );
        let Field::Text(field) = &mut self.fields[&field_name] else {
            unreachable!()
        };
        field
    }

    pub fn push(&mut self, (field_name, value): (L, String)) -> &mut TextField {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        self.fields.insert(
            field_name.clone(),
            Field::Text(TextField::new(UText::new(value), None)),
        );
        let Field::Text(field) = &mut self.fields[&field_name] else {
            unreachable!()
        };
        field
    }

    pub fn push_field(&mut self, (field_name, value): (L, Field)) -> &mut Field {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        self.fields.insert(field_name.clone(), value);
        &mut self.fields[&field_name]
    }

    pub fn push_dict(
        &mut self,
        (field_name, values): (L, IndexMap<String, Field>),
        new_entry_constructor: Arc<dyn Fn() -> Field + Send + Sync>,
    ) -> &mut DictField {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        let mut dict: FormWidget<(), Cow<'static, str>, Field> = FormWidget {
            buttons: ButtonWidget::default(),
            focus: FormFocus::Fields,
            hide_buttons: true,
            cursor_up_shortcut: self.cursor_up_shortcut.clone(),
            cursor_down_shortcut: self.cursor_down_shortcut.clone(),
            cursor_right_shortcut: self.cursor_right_shortcut.clone(),
            cursor_left_shortcut: self.cursor_left_shortcut.clone(),
            dirty: true,
            focused: false,
            ..Default::default()
        };
        for (k, v) in values {
            dict.push_field((k.into(), v));
        }
        self.fields.insert(
            field_name.clone(),
            Field::Dict(DictField {
                values: Box::new(dict),
                rename_entry: None,
                new_entry_constructor,
                is_a_set: false,
            }),
        );
        let Field::Dict(field) = &mut self.fields[&field_name] else {
            unreachable!()
        };
        field
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync, L: FormWidgetLabel>
    FormWidget<T, L, TextField>
{
    pub fn push_cl(
        &mut self,
        (field_name, value, auto): (L, String, AutoCompleteFn),
    ) -> &mut TextField {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        self.fields.insert(
            field_name.clone(),
            TextField::new(
                UText::new(value),
                Some((auto, AutoComplete::new(Vec::new()))),
            ),
        );
        &mut self.fields[&field_name]
    }

    pub fn push(&mut self, (field_name, value): (L, String)) -> &mut TextField {
        self.field_name_max_length = self.field_name_max_length.max(field_name.as_ref().len());
        self.fields
            .insert(field_name.clone(), TextField::new(UText::new(value), None));
        &mut self.fields[&field_name]
    }
}

impl<
        T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
        L: FormWidgetLabel,
        F: FormWidgetField,
    > Component for FormWidget<T, L, F>
{
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.is_dirty() {
            let theme_default = crate::conf::theme_value(context, "theme_default");

            grid.clear_area(area, theme_default);
            let label_attrs = crate::conf::theme_value(context, "widgets.form.label");
            let mut highlighted = crate::conf::theme_value(context, "highlight");
            if !context.settings.terminal.use_color() {
                highlighted.attrs |= Attr::REVERSE;
            }

            let heights = self
                .fields
                .values()
                .map(|f| f.height())
                .collect::<Vec<usize>>();

            let mut y_offset = 0;
            let mut highlight_offset = None;
            for (i, ((k, f), height)) in self.fields.iter_mut().zip(heights).enumerate() {
                let field_area = area.skip_rows(i + y_offset).take_rows(height);
                let theme_attr =
                    if self.focused && i == self.cursor && self.focus == FormFocus::Fields {
                        grid.change_theme(field_area, highlighted);
                        highlighted
                    } else {
                        label_attrs
                    };
                if self.hide_field_names {
                    f.draw(grid, field_area, context);
                    // Highlight if necessary
                    if self.focused && i == self.cursor && self.focus == FormFocus::TextInput {
                        highlight_offset =
                            Some((i, (field_area, area.skip_rows(i + y_offset + 1))));
                    }
                } else {
                    // Write field label
                    grid.write_string(&k.display(), &theme_attr, field_area, None, None);
                    // draw field
                    f.draw(
                        grid,
                        field_area.skip_cols(self.field_name_max_length + 2),
                        context,
                    );

                    // Highlight if necessary
                    if self.focused && i == self.cursor && self.focus == FormFocus::TextInput {
                        highlight_offset = Some((
                            i,
                            (
                                field_area.skip_cols(self.field_name_max_length + 2),
                                area.skip_rows(i + y_offset + 1)
                                    .skip_cols(self.field_name_max_length + 2),
                            ),
                        ));
                    }
                }
                y_offset += height.saturating_sub(1);
            }
            if let Some((i, (field_area, secondary_area))) = highlight_offset {
                self.fields[i].draw_cursor(grid, field_area, secondary_area, context);
            }

            let length = self.fields.len() + y_offset;

            if !self.hide_buttons {
                self.buttons
                    .draw(grid, area.skip_rows(length + self.padding), context);
            }
            if length + self.padding + 1 < area.height() {
                grid.clear_area(area.skip_rows(length + self.padding + 1), theme_default);
            }
            self.set_dirty(false);
            context.dirty_areas.push_back(area);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match self.focus {
            FormFocus::Buttons => {
                if !self.hide_buttons && self.buttons.process_event(event, context) {
                    return true;
                }
                if matches!(event, UIEvent::Input(ref k) if *k == self.cursor_up_shortcut) {
                    self.focus = FormFocus::Fields;
                    self.buttons.set_focus(false);
                    self.set_dirty(true);
                    return true;
                }
            }
            FormFocus::TextInput => match event {
                UIEvent::InsertInput(Key::Up) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        field.process_event(event, context);
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::InsertInput(Key::Down) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        field.process_event(event, context);
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::InsertInput(Key::Char('\t')) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        field.process_event(event, context);
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::InsertInput(Key::Char('\n')) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        if field.process_event(event, context) {
                            return true;
                        }
                        self.focus = FormFocus::Fields;
                        context
                            .replies
                            .push_back(UIEvent::ChangeMode(UIMode::Normal));
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::InsertInput(Key::Right) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        field.process_event(event, context);
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::InsertInput(Key::Left) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        if !field.process_event(event, context) {
                            self.focus = FormFocus::Fields;
                            context
                                .replies
                                .push_back(UIEvent::ChangeMode(UIMode::Normal));
                        }
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::ChangeMode(UIMode::Normal) => {
                    self.focus = FormFocus::Fields;
                    return false;
                }
                UIEvent::InsertInput(Key::Backspace) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        field.process_event(event, context);
                        self.set_dirty(true);
                        return true;
                    }
                }
                UIEvent::InsertInput(_) => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        field.process_event(event, context);
                        self.set_dirty(true);
                        return true;
                    }
                }
                _ => {
                    if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                        if field.process_event(event, context) {
                            return true;
                        }
                    }
                }
            },
            FormFocus::Fields => {
                if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                    if field.process_event(event, context) {
                        return true;
                    }
                }
                match event {
                    UIEvent::Input(ref k) if *k == self.cursor_up_shortcut => {
                        if self.cursor > 0 {
                            if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                                field.set_focused(false);
                            }
                            self.cursor -= 1;
                            if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                                field.set_focused(true);
                            }
                            self.set_dirty(true);
                            return true;
                        }
                    }
                    UIEvent::Input(ref k) if *k == self.cursor_down_shortcut => {
                        if self.cursor < self.fields.len().saturating_sub(1) {
                            if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                                field.set_focused(false);
                            }
                            self.cursor += 1;
                            if let Some(field) = self.fields.get_indexed_mut(self.cursor) {
                                field.set_focused(true);
                            }
                        } else if !self.hide_buttons {
                            self.focus = FormFocus::Buttons;
                            self.buttons.set_focus(true);
                        } else {
                            return false;
                        }
                        self.set_dirty(true);
                        return true;
                    }
                    UIEvent::Input(Key::Char('\n')) => {
                        if let Some(field) = self.fields.get_indexed(self.cursor) {
                            if !field.read_only() {
                                self.focus = FormFocus::TextInput;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Insert));
                                self.set_dirty(true);
                            }
                            return true;
                        }
                    }
                    _ => {}
                }
            }
        }

        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.fields.values().any(Component::is_dirty) || self.buttons.is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        for field in self.fields.values_mut() {
            field.set_dirty(value);
        }
        self.buttons.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if !self.hide_buttons && self.focus == FormFocus::Buttons {
            self.buttons.shortcuts(context)
        } else {
            Default::default()
        };
        let mut our_map: ShortcutMap = Default::default();
        our_map.insert("up", self.cursor_up_shortcut.clone());
        our_map.insert("down", self.cursor_down_shortcut.clone());

        if let Some(field) = self.fields.get_indexed(self.cursor) {
            map.extend(field.shortcuts(context));
        }

        map.insert("fields input", our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

#[derive(Debug)]
pub struct ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    buttons: IndexMap<Cow<'static, str>, T>,
    result: Option<T>,
    cursor: usize,
    /// Is the button widget focused, i.e do we need to draw the highlighting?
    focus: bool,
    dirty: bool,
    cursor_right_shortcut: Key,
    cursor_left_shortcut: Key,
    id: ComponentId,
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> Default for ButtonWidget<T> {
    fn default() -> Self {
        Self {
            buttons: Default::default(),
            result: None,
            cursor: 0,
            focus: false,
            dirty: true,
            cursor_right_shortcut: Key::Char('l'),
            cursor_left_shortcut: Key::Char('h'),
            id: ComponentId::default(),
        }
    }
}

impl<T> std::fmt::Display for ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("", f)
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Clone + Send + Sync> Clone
    for ButtonWidget<T>
{
    fn clone(&self) -> Self {
        Self {
            buttons: self.buttons.clone(),
            result: self.result,
            cursor: self.cursor,
            focus: self.focus,
            cursor_right_shortcut: self.cursor_right_shortcut.clone(),
            cursor_left_shortcut: self.cursor_left_shortcut.clone(),
            dirty: true,
            id: ComponentId::default(),
        }
    }
}

impl<T> ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    pub fn new(
        init_val: (Cow<'static, str>, T),
        cursor_right_shortcut: Key,
        cursor_left_shortcut: Key,
    ) -> Self {
        Self {
            buttons: vec![init_val].into_iter().collect(),
            result: None,
            cursor: 0,
            focus: false,
            dirty: true,
            cursor_right_shortcut,
            cursor_left_shortcut,
            id: ComponentId::default(),
        }
    }

    pub fn push(&mut self, value: (Cow<'static, str>, T)) {
        self.buttons.insert(value.0, value.1);
    }

    pub fn is_resolved(&self) -> bool {
        self.result.is_some()
    }

    pub fn result(&mut self) -> Option<T> {
        self.result.take()
    }

    pub fn set_focus(&mut self, new_val: bool) {
        self.focus = new_val;
    }

    pub fn set_cursor(&mut self, new_val: usize) {
        if self.buttons.is_empty() {
            return;
        }
        self.cursor = new_val % self.buttons.len();
    }
}

impl<T> Component for ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            let theme_default = crate::conf::theme_value(context, "theme_default");
            grid.clear_area(area, theme_default);

            let mut len = 0;
            for (i, k) in self.buttons.keys().enumerate() {
                let cur_len = k.len();
                let theme_attr = if i == self.cursor && self.focus {
                    crate::conf::theme_value(context, "highlight")
                } else {
                    theme_default
                };
                grid.write_string(
                    k.as_ref(),
                    &(theme_attr | Attr::BOLD),
                    area.skip_cols(len),
                    None,
                    None,
                );
                len += cur_len + 3;
            }
            self.dirty = false;
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match *event {
            UIEvent::Input(Key::Char('\n')) => {
                self.result = Some(
                    self.buttons
                        .get_indexed(self.cursor)
                        .cloned()
                        .unwrap_or_default(),
                );
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref k) if *k == self.cursor_left_shortcut => {
                if self.cursor > 0 {
                    self.cursor -= 1;
                    self.set_dirty(true);
                    return true;
                }
            }
            UIEvent::Input(ref k)
                if *k == self.cursor_right_shortcut
                    && self.cursor < self.buttons.len().saturating_sub(1) =>
            {
                self.cursor += 1;
                self.set_dirty(true);
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn shortcuts(&self, _: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();

        let mut our_map: ShortcutMap = Default::default();
        our_map.insert("right", self.cursor_right_shortcut.clone());
        our_map.insert("left", self.cursor_left_shortcut.clone());
        our_map.insert("select", Key::Char('\n'));
        map.insert("buttons", our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

#[inline]
pub fn generic_file_path_validate_fn() -> Option<ValidateFn> {
    Some(Arc::new(|d| -> bool {
        use crate::command::parser::Lexer;

        let mut lexer = Lexer::new(d);
        let Some(input) = lexer.next() else {
            return false;
        };
        if lexer.next().is_some() {
            return false;
        }
        matches!(input, Ok(input) if matches!(std::path::Path::new(input.value()).expand().try_exists(), Ok(true)))
    }))
}
