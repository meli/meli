/*
 * meli
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

use std::{borrow::Cow, time::Duration};

use super::*;
use crate::melib::text::TextProcessing;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum FormFocus {
    #[default]
    Fields,
    Buttons,
    TextInput,
}

type Cursor = usize;

#[derive(Clone)]
pub enum Field {
    Text(TextField),
    Choice(Vec<Cow<'static, str>>, Cursor, ComponentId),
}

impl std::fmt::Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Text(ref t) => std::fmt::Debug::fmt(t, f),
            k => std::fmt::Debug::fmt(k, f),
        }
    }
}

impl Default for Field {
    fn default() -> Self {
        Self::Text(TextField::default())
    }
}

impl Field {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Text(ref s) => s.as_str(),
            Self::Choice(ref v, cursor, _) => {
                if v.is_empty() {
                    ""
                } else {
                    v[*cursor].as_ref()
                }
            }
        }
    }

    pub fn cursor(&self) -> usize {
        match self {
            Self::Text(ref s) => s.cursor(),
            Self::Choice(_, ref cursor, _) => *cursor,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    pub fn into_string(self) -> String {
        match self {
            Self::Text(s) => s.into_string(),
            Self::Choice(mut v, cursor, _) => v.remove(cursor).to_string(),
        }
    }

    pub fn clear(&mut self) {
        match self {
            Self::Text(s) => s.clear(),
            Self::Choice(_, _, _) => {}
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
            Self::Text(ref mut text_field) => {
                text_field.draw_cursor(grid, area, secondary_area, context);
            }
            Self::Choice(_, _cursor, _) => {}
        }
    }
}

impl Component for Field {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let theme_attr = crate::conf::value(context, "widgets.form.field");
        let str = self.as_str();
        match self {
            Self::Text(ref mut text_field) => {
                text_field.draw(grid, area, context);
            }
            Self::Choice(_, _, _) => {
                grid.write_string(
                    str,
                    theme_attr.fg,
                    theme_attr.bg,
                    theme_attr.attrs,
                    area,
                    None,
                    None,
                );
            }
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Self::Text(ref mut t) = self {
            return t.process_event(event, context);
        }

        match *event {
            UIEvent::InsertInput(Key::Right) => match self {
                Self::Text(_) => {
                    return false;
                }
                Self::Choice(ref vec, ref mut cursor, _) => {
                    *cursor = if *cursor == vec.len().saturating_sub(1) {
                        0
                    } else {
                        *cursor + 1
                    };
                }
            },
            UIEvent::InsertInput(Key::Left) => match self {
                Self::Text(_) => {
                    return false;
                }
                Self::Choice(_, ref mut cursor, _) => {
                    if *cursor == 0 {
                        return false;
                    } else {
                        *cursor -= 1;
                    }
                }
            },
            _ => {
                return false;
            }
        }
        self.set_dirty(true);
        true
    }

    fn is_dirty(&self) -> bool {
        false
    }

    fn set_dirty(&mut self, _value: bool) {}

    fn id(&self) -> ComponentId {
        match self {
            Self::Text(i) => i.id(),
            Self::Choice(_, _, i) => *i,
        }
    }
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Text(ref s) => s.as_str(),
                Self::Choice(ref v, ref cursor, _) => v[*cursor].as_ref(),
            }
        )
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

#[derive(Debug)]
pub struct FormWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    fields: IndexMap<Cow<'static, str>, Field>,
    layout: Vec<Cow<'static, str>>,
    buttons: ButtonWidget<T>,

    field_name_max_length: usize,
    cursor: usize,
    focus: FormFocus,
    hide_buttons: bool,
    dirty: bool,
    cursor_up_shortcut: Key,
    cursor_down_shortcut: Key,
    cursor_right_shortcut: Key,
    cursor_left_shortcut: Key,
    id: ComponentId,
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Clone + Send + Sync> Clone for FormWidget<T> {
    fn clone(&self) -> Self {
        Self {
            fields: self.fields.clone(),
            layout: self.layout.clone(),
            buttons: self.buttons.clone(),
            focus: self.focus,
            hide_buttons: self.hide_buttons,
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

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> Default for FormWidget<T> {
    fn default() -> Self {
        Self {
            fields: Default::default(),
            layout: Default::default(),
            buttons: Default::default(),
            focus: FormFocus::Fields,
            hide_buttons: false,
            field_name_max_length: 10,
            cursor: 0,
            dirty: true,
            cursor_up_shortcut: Key::Char('k'),
            cursor_down_shortcut: Key::Char('j'),
            cursor_right_shortcut: Key::Char('l'),
            cursor_left_shortcut: Key::Char('h'),
            id: ComponentId::default(),
        }
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> std::fmt::Display
    for FormWidget<T>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "form")
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> FormWidget<T> {
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
            cursor_up_shortcut,
            cursor_down_shortcut,
            cursor_right_shortcut,
            cursor_left_shortcut,
            id: ComponentId::default(),
            dirty: true,
            ..Default::default()
        }
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn set_cursor(&mut self, new_cursor: usize) {
        self.cursor = new_cursor;
    }

    pub fn hide_buttons(&mut self) {
        self.hide_buttons = true;
        self.buttons.set_dirty(false);
    }

    pub fn len(&self) -> usize {
        self.layout.len()
    }

    pub fn is_empty(&self) -> bool {
        self.layout.len() == 0
    }

    pub fn add_button(&mut self, val: (Cow<'static, str>, T)) {
        self.buttons.push(val);
    }

    pub fn push_choices(&mut self, value: (Cow<'static, str>, Vec<Cow<'static, str>>)) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields
            .insert(value.0, Field::Choice(value.1, 0, ComponentId::default()));
    }

    pub fn push_cl(&mut self, value: (Cow<'static, str>, String, AutoCompleteFn)) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields.insert(
            value.0,
            Field::Text(TextField::new(
                UText::new(value.1),
                Some((value.2, AutoComplete::new(Vec::new()))),
            )),
        );
    }

    pub fn push(&mut self, value: (Cow<'static, str>, String)) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields.insert(
            value.0,
            Field::Text(TextField::new(UText::new(value.1), None)),
        );
    }

    pub fn insert(&mut self, index: usize, value: (Cow<'static, str>, Field)) {
        self.layout.insert(index, value.0.clone());
        self.fields.insert(value.0, value.1);
    }

    pub fn values(&self) -> &IndexMap<Cow<'static, str>, Field> {
        &self.fields
    }

    pub fn values_mut(&mut self) -> &mut IndexMap<Cow<'static, str>, Field> {
        &mut self.fields
    }

    pub fn collect(self) -> IndexMap<Cow<'static, str>, Field> {
        self.fields
    }

    pub fn buttons_result(&mut self) -> Option<T> {
        self.buttons.result.take()
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> Component for FormWidget<T> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.is_dirty() {
            let theme_default = crate::conf::value(context, "theme_default");

            grid.clear_area(area, theme_default);
            let label_attrs = crate::conf::value(context, "widgets.form.label");
            let mut highlighted = crate::conf::value(context, "highlight");
            if !context.settings.terminal.use_color() {
                highlighted.attrs |= Attr::REVERSE;
            }

            for (i, k) in self.layout.iter().enumerate().rev() {
                let theme_attr = if i == self.cursor && self.focus == FormFocus::Fields {
                    grid.change_theme(area.nth_row(i), highlighted);
                    highlighted
                } else {
                    label_attrs
                };
                let v = self.fields.get_mut(k).unwrap();
                /* Write field label */
                grid.write_string(
                    k.as_ref(),
                    theme_attr.fg,
                    theme_attr.bg,
                    theme_attr.attrs,
                    area.nth_row(i).skip_cols(1),
                    None,
                    None,
                );
                /* draw field */
                v.draw(
                    grid,
                    area.nth_row(i).skip_cols(self.field_name_max_length + 3),
                    context,
                );
                grid.change_theme(area.nth_row(i), theme_attr);

                /* Highlight if necessary */
                if i == self.cursor && self.focus == FormFocus::TextInput {
                    v.draw_cursor(
                        grid,
                        area.nth_row(i).skip_cols(self.field_name_max_length + 3),
                        area.nth_row(i + 1)
                            .skip_cols(self.field_name_max_length + 3),
                        context,
                    );
                }
            }

            let length = self.layout.len();

            if !self.hide_buttons {
                self.buttons
                    .draw(grid, area.skip_rows(length + 3).skip_cols(1), context);
            }
            if length + 4 < area.height() {
                grid.clear_area(area.skip_rows(length + 3 + 1), theme_default);
            }
            self.set_dirty(false);
            context.dirty_areas.push_back(area);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if !self.hide_buttons
            && self.focus == FormFocus::Buttons
            && self.buttons.process_event(event, context)
        {
            return true;
        }

        match *event {
            UIEvent::Input(ref k)
                if *k == self.cursor_up_shortcut && self.focus == FormFocus::Buttons =>
            {
                self.focus = FormFocus::Fields;
                self.buttons.set_focus(false);
                self.set_dirty(true);
                return true;
            }
            UIEvent::InsertInput(Key::Up) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref k) if *k == self.cursor_up_shortcut => {
                self.cursor = self.cursor.saturating_sub(1);
                self.set_dirty(true);
                return true;
            }
            UIEvent::InsertInput(Key::Down) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref k)
                if *k == self.cursor_down_shortcut
                    && self.cursor < self.layout.len().saturating_sub(1) =>
            {
                self.cursor += 1;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref k)
                if *k == self.cursor_down_shortcut && self.focus == FormFocus::Fields =>
            {
                self.focus = FormFocus::Buttons;
                self.buttons.set_focus(true);
                self.set_dirty(true);
                if self.hide_buttons {
                    return false;
                }
                return true;
            }
            UIEvent::InsertInput(Key::Char('\t')) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Char('\n')) if self.focus == FormFocus::Fields => {
                self.focus = FormFocus::TextInput;
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Insert));
                self.set_dirty(true);
                return true;
            }
            UIEvent::InsertInput(Key::Right) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
                self.set_dirty(true);
                return true;
            }
            UIEvent::InsertInput(Key::Left) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                if !field.process_event(event, context) {
                    self.focus = FormFocus::Fields;
                    context
                        .replies
                        .push_back(UIEvent::ChangeMode(UIMode::Normal));
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::ChangeMode(UIMode::Normal) if self.focus == FormFocus::TextInput => {
                self.focus = FormFocus::Fields;
                return false;
            }
            UIEvent::InsertInput(Key::Backspace) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
                self.set_dirty(true);
                return true;
            }
            UIEvent::InsertInput(_) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
                self.set_dirty(true);
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.buttons.is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
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
        our_map.insert("toggle field editing", Key::Char('\n'));
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
    layout: Vec<Cow<'static, str>>,

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
            layout: Default::default(),
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
            layout: self.layout.clone(),
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
            layout: vec![init_val.0.clone()],
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
        self.layout.push(value.0.clone());
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
            let theme_default = crate::conf::value(context, "theme_default");
            grid.clear_area(area, theme_default);

            let mut len = 0;
            for (i, k) in self.layout.iter().enumerate() {
                let cur_len = k.len();
                grid.write_string(
                    k.as_ref(),
                    theme_default.fg,
                    if i == self.cursor && self.focus {
                        crate::conf::value(context, "highlight").bg
                    } else {
                        theme_default.bg
                    },
                    Attr::BOLD,
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
                        .get(&self.layout[self.cursor])
                        .cloned()
                        .unwrap_or_default(),
                );
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref k) if *k == self.cursor_left_shortcut => {
                self.cursor = self.cursor.saturating_sub(1);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref k)
                if *k == self.cursor_right_shortcut
                    && self.cursor < self.layout.len().saturating_sub(1) =>
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AutoCompleteEntry {
    pub entry: String,
    pub description: String,
}

impl AutoCompleteEntry {
    pub fn as_str(&self) -> &str {
        self.entry.as_str()
    }
}

impl From<String> for AutoCompleteEntry {
    fn from(val: String) -> Self {
        Self {
            entry: val,
            description: String::new(),
        }
    }
}

impl From<&(&str, &str, TokenStream)> for AutoCompleteEntry {
    fn from(val: &(&str, &str, TokenStream)) -> Self {
        let (a, b, _) = val;
        Self {
            entry: a.to_string(),
            description: b.to_string(),
        }
    }
}

impl From<(String, String)> for AutoCompleteEntry {
    fn from(val: (String, String)) -> Self {
        let (a, b) = val;
        Self {
            entry: a,
            description: b,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AutoComplete {
    entries: Vec<AutoCompleteEntry>,
    cursor: usize,

    dirty: bool,
    id: ComponentId,
}

impl std::fmt::Display for AutoComplete {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("AutoComplete", f)
    }
}

impl Component for AutoComplete {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            return;
        };
        self.dirty = false;

        let rows = area.height();
        if rows == 0 {
            return;
        }
        let page_no = (self.cursor.saturating_sub(1)).wrapping_div(rows);
        let top_idx = page_no * rows;
        let x_offset = usize::from(rows < self.entries.len());

        grid.clear_area(area, crate::conf::value(context, "theme_default"));
        let width = self
            .entries
            .iter()
            .map(|a| a.entry.grapheme_len() + a.description.grapheme_len() + 2)
            .max()
            .unwrap_or(0)
            + 1;
        // [ref:hardcoded_color_value]
        let theme_attr = ThemeAttribute {
            fg: Color::Byte(23),
            bg: Color::Byte(7),
            attrs: Attr::DEFAULT,
        };
        grid.change_theme(area, theme_attr);
        for (i, e) in self.entries.iter().skip(top_idx).enumerate() {
            let (x, _) = grid.write_string(
                &e.entry,
                Color::Byte(23),
                Color::Byte(7),
                Attr::DEFAULT,
                area.nth_row(i).take_cols(width),
                None,
                None,
            );
            grid.write_string(
                &e.description,
                Color::Byte(23),
                Color::Byte(7),
                Attr::ITALICS,
                area.nth_row(i).skip_cols(x + 2).take_cols(width),
                None,
                None,
            );
            grid.write_string(
                "▒",
                Color::Byte(23),
                Color::Byte(7),
                Attr::DEFAULT,
                area.nth_row(i).skip_cols(width - 1),
                None,
                None,
            );
        }

        /* Highlight cursor */
        if self.cursor > 0 {
            let highlight = crate::conf::value(context, "highlight");

            grid.change_theme(
                area.nth_row((self.cursor - 1) % rows)
                    .skip_cols(width.saturating_sub(1 + x_offset)),
                highlight,
            );
        }
        if rows < self.entries.len() {
            ScrollBar { show_arrows: false }.draw(
                grid,
                area.take_rows(x_offset),
                context,
                self.cursor.saturating_sub(1),
                rows,
                self.entries.len(),
            );
        }
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
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

impl AutoComplete {
    pub fn new(entries: Vec<AutoCompleteEntry>) -> Box<Self> {
        let mut ret = Self {
            entries: Vec::new(),
            cursor: 0,
            dirty: true,
            id: ComponentId::default(),
        };
        ret.set_suggestions(entries);
        Box::new(ret)
    }

    pub fn set_suggestions(&mut self, entries: Vec<AutoCompleteEntry>) -> bool {
        if entries.len() == self.entries.len() && entries == self.entries {
            return false;
        }

        self.entries = entries;
        self.cursor = 0;
        true
    }

    pub fn inc_cursor(&mut self) {
        if self.cursor < self.entries.len() {
            self.cursor += 1;
            self.set_dirty(true);
        }
    }
    pub fn dec_cursor(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
        self.set_dirty(true);
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn set_cursor(&mut self, val: usize) {
        debug_assert!(val <= self.entries.len());
        self.cursor = val;
    }

    pub fn get_suggestion(&mut self) -> Option<String> {
        if self.entries.is_empty() || self.cursor == 0 {
            return None;
        }
        let ret = self.entries.remove(self.cursor - 1);
        self.entries.clear();
        self.cursor = 0;
        Some(ret.entry)
    }

    pub fn suggestions(&self) -> &Vec<AutoCompleteEntry> {
        &self.entries
    }
}

/// A widget that draws a scrollbar.
///
/// # Example
///
/// ```rust,no_run
/// # use meli::{Area, Component, CellBuffer, Context, utilities::ScrollBar};
/// // Mock `Component::draw` impl
/// fn draw(grid: &mut CellBuffer, area: Area, context: &mut Context) {
///     let position = 0;
///     let visible_rows = area.height();
///     let scrollbar_area = area.nth_col(area.width());
///     let total_rows = 100;
///     ScrollBar::default().set_show_arrows(true).draw(
///         grid,
///         scrollbar_area,
///         context,
///         position,
///         visible_rows,
///         total_rows,
///     );
/// }
/// ```
#[derive(Clone, Copy, Default)]
pub struct ScrollBar {
    pub show_arrows: bool,
}

impl ScrollBar {
    /// Update `self.show_arrows` field.
    pub fn set_show_arrows(&mut self, new_val: bool) -> &mut Self {
        self.show_arrows = new_val;
        self
    }

    /// Draw `self` vertically.
    pub fn draw(
        &self,
        grid: &mut CellBuffer,
        area: Area,
        context: &Context,
        pos: usize,
        visible_rows: usize,
        length: usize,
    ) {
        if length == 0 {
            return;
        }
        let height = area.height();
        if height < 3 {
            return;
        }
        let theme_default = crate::conf::value(context, "theme_default");
        grid.clear_area(area, theme_default);

        let visible_rows = std::cmp::min(visible_rows, length);
        let ascii_drawing = grid.ascii_drawing;
        let ratio: f64 = (height as f64) / (length as f64);
        let scrollbar_height = std::cmp::max((ratio * (visible_rows as f64)) as usize, 1);
        let scrollbar_offset = (ratio * (pos as f64)) as usize;
        let mut area2 = area;

        if self.show_arrows {
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '^' } else { '▀' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg);
            area2 = area2.skip_rows(1);
        }

        area2 = area2.skip_rows(scrollbar_offset);
        for _ in 0..scrollbar_height {
            if area2.is_empty() {
                break;
            }
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '#' } else { '█' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_attrs(if !context.settings.terminal.use_color() {
                    theme_default.attrs | Attr::REVERSE
                } else {
                    theme_default.attrs
                });
            area2 = area2.skip_rows(1);
        }
        if self.show_arrows {
            grid[area2.bottom_right()]
                .set_ch(if ascii_drawing { 'v' } else { '▄' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_bg(crate::conf::value(context, "theme_default").bg);
        }
    }

    /// Draw `self` horizontally.
    pub fn draw_horizontal(
        &self,
        grid: &mut CellBuffer,
        area: Area,
        context: &Context,
        pos: usize,
        visible_cols: usize,
        length: usize,
    ) {
        if length == 0 {
            return;
        }
        let width = area.width();
        if width < 3 {
            return;
        }
        let theme_default = crate::conf::value(context, "theme_default");
        grid.clear_area(area, theme_default);

        let visible_cols = std::cmp::min(visible_cols, length);
        let ascii_drawing = grid.ascii_drawing;
        let ratio: f64 = (width as f64) / (length as f64);
        let scrollbar_width = std::cmp::min((ratio * (visible_cols as f64)) as usize, 1);
        let scrollbar_offset = (ratio * (pos as f64)) as usize;
        let mut area2 = area;

        if self.show_arrows {
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '<' } else { '▐' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg);
            area2 = area2.skip_cols(1);
        }

        area2 = area2.skip_cols(scrollbar_offset);
        for _ in 0..scrollbar_width {
            if area2.is_empty() {
                break;
            }
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '#' } else { '█' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_attrs(if !context.settings.terminal.use_color() {
                    theme_default.attrs | Attr::REVERSE
                } else {
                    theme_default.attrs
                });
            area2 = area2.skip_cols(1);
        }
        if self.show_arrows {
            grid[area2.bottom_right()]
                .set_ch(if ascii_drawing { '>' } else { '▌' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_bg(crate::conf::value(context, "theme_default").bg);
        }
    }
}

/// A widget that displays a customizable progress spinner.
///
/// It uses a [`Timer`](crate::jobs::Timer) and each time its timer fires, it
/// cycles to the next stage of its `kind` sequence.
///
/// `kind` is an array of strings/string slices and an
/// [`Duration` interval](std::time::Duration), and each item represents a stage
/// or frame of the progress spinner. For example, a
/// `(Duration::from_millis(130), &["-", "\\", "|", "/"])` value would cycle
/// through the sequence `-`, `\`, `|`, `/`, `-`, `\`, `|` and so on roughly
/// every 130 milliseconds.
///
/// # Example
///
/// ```rust,no_run
/// use std::collections::HashSet;
///
/// use meli::{jobs::JobId, utilities::ProgressSpinner, Component, Context, StatusEvent, UIEvent};
///
/// struct JobMonitoringWidget {
///     progress_spinner: ProgressSpinner,
///     in_progress_jobs: HashSet<JobId>,
/// }
///
/// impl JobMonitoringWidget {
///     fn new(context: &Context, container: Box<dyn Component>) -> Self {
///         let mut progress_spinner = ProgressSpinner::new(20, context);
///         match context.settings.terminal.progress_spinner_sequence.as_ref() {
///             Some(meli::conf::terminal::ProgressSpinnerSequence::Integer(k)) => {
///                 progress_spinner.set_kind(*k);
///             }
///             Some(meli::conf::terminal::ProgressSpinnerSequence::Custom {
///                 ref frames,
///                 ref interval_ms,
///             }) => {
///                 progress_spinner.set_custom_kind(frames.clone(), *interval_ms);
///             }
///             None => {}
///         }
///         Self {
///             progress_spinner,
///             in_progress_jobs: Default::default(),
///         }
///     }
///
///     // Mock `Component::process_event` impl.
///     fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
///         match event {
///             UIEvent::StatusEvent(StatusEvent::JobCanceled(ref job_id))
///             | UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)) => {
///                 self.in_progress_jobs.remove(job_id);
///                 if self.in_progress_jobs.is_empty() {
///                     self.progress_spinner.stop();
///                 }
///                 self.progress_spinner.set_dirty(true);
///                 false
///             }
///             UIEvent::StatusEvent(StatusEvent::NewJob(ref job_id)) => {
///                 if self.in_progress_jobs.is_empty() {
///                     self.progress_spinner.start();
///                 }
///                 self.progress_spinner.set_dirty(true);
///                 self.in_progress_jobs.insert(*job_id);
///                 false
///             }
///             UIEvent::Timer(_) => {
///                 if self.progress_spinner.process_event(event, context) {
///                     return true;
///                 }
///                 false
///             }
///             _ => false,
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct ProgressSpinner {
    timer: crate::jobs::Timer,
    stage: usize,
    pub kind: std::result::Result<usize, Vec<String>>,
    pub width: usize,
    theme_attr: ThemeAttribute,
    active: bool,
    dirty: bool,
    id: ComponentId,
}

impl ProgressSpinner {
    pub const KINDS: [(Duration, &'static [&'static str]); 37] = [
        (Duration::from_millis(130), &["-", "\\", "|", "/"]),
        (Self::INTERVAL, &["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"]),
        (Self::INTERVAL, &["⣀", "⣄", "⣤", "⣦", "⣶", "⣷", "⣿"]),
        (Self::INTERVAL, &["⣀", "⣄", "⣆", "⣇", "⣧", "⣷", "⣿"]),
        (Self::INTERVAL, &["○", "◔", "◐", "◕", "⬤"]),
        (Self::INTERVAL, &["□", "◱", "◧", "▣", "■"]),
        (Self::INTERVAL, &["□", "◱", "▨", "▩", "■"]),
        (Self::INTERVAL, &["□", "◱", "▥", "▦", "■"]),
        (Self::INTERVAL, &["░", "▒", "▓", "█"]),
        (Self::INTERVAL, &["░", "█"]),
        (Self::INTERVAL, &["⬜", "⬛"]),
        (Self::INTERVAL, &["▱", "▰"]),
        (Self::INTERVAL, &["▭", "◼"]),
        (Self::INTERVAL, &["▯", "▮"]),
        (Self::INTERVAL, &["◯", "⬤"]),
        (Self::INTERVAL, &["⚪", "⚫"]),
        (
            Self::INTERVAL,
            &["▖", "▗", "▘", "▝", "▞", "▚", "▙", "▟", "▜", "▛"],
        ),
        (Self::INTERVAL, &["|", "/", "-", "\\"]),
        (Self::INTERVAL, &[".", "o", "O", "@", "*"]),
        (Self::INTERVAL, &["◡◡", "⊙⊙", "◠◠", "⊙⊙"]),
        (Self::INTERVAL, &["◜ ", " ◝", " ◞", "◟ "]),
        (Self::INTERVAL, &["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]),
        (
            Self::INTERVAL,
            &["▁", "▃", "▄", "▅", "▆", "▇", "█", "▇", "▆", "▅", "▄", "▃"],
        ),
        (
            Self::INTERVAL,
            &[
                "▉", "▊", "▋", "▌", "▍", "▎", "▏", "▎", "▍", "▌", "▋", "▊", "▉",
            ],
        ),
        (Self::INTERVAL, &["▖", "▘", "▝", "▗"]),
        (Self::INTERVAL, &["▌", "▀", "▐", "▄"]),
        (Self::INTERVAL, &["┤", "┘", "┴", "└", "├", "┌", "┬", "┐"]),
        (Self::INTERVAL, &["◢", "◣", "◤", "◥"]),
        (Self::INTERVAL, &["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]),
        (
            Self::INTERVAL,
            &["⢎⡰", "⢎⡡", "⢎⡑", "⢎⠱", "⠎⡱", "⢊⡱", "⢌⡱", "⢆⡱"],
        ),
        (Self::INTERVAL, &[".", "o", "O", "°", "O", "o", "."]),
        (Duration::from_millis(100), &["㊂", "㊀", "㊁"]),
        (
            Duration::from_millis(100),
            &["💛 ", "💙 ", "💜 ", "💚 ", "❤️ "],
        ),
        (
            Duration::from_millis(100),
            &[
                "🕛 ", "🕐 ", "🕑 ", "🕒 ", "🕓 ", "🕔 ", "🕕 ", "🕖 ", "🕗 ", "🕘 ", "🕙 ", "🕚 ",
            ],
        ),
        (Duration::from_millis(100), &["🌍 ", "🌎 ", "🌏 "]),
        (
            Duration::from_millis(80),
            &[
                "[    ]", "[=   ]", "[==  ]", "[=== ]", "[ ===]", "[  ==]", "[   =]", "[    ]",
                "[   =]", "[  ==]", "[ ===]", "[====]", "[=== ]", "[==  ]", "[=   ]",
            ],
        ),
        (
            Duration::from_millis(80),
            &["🌑 ", "🌒 ", "🌓 ", "🌔 ", "🌕 ", "🌖 ", "🌗 ", "🌘 "],
        ),
    ];

    pub const INTERVAL_MS: u64 = 50;
    const INTERVAL: std::time::Duration = std::time::Duration::from_millis(Self::INTERVAL_MS);

    /// See source code of [`Self::KINDS`].
    pub fn new(kind: usize, context: &Context) -> Self {
        let kind = kind % Self::KINDS.len();
        let width = Self::KINDS[kind]
            .1
            .iter()
            .map(|f| f.grapheme_len())
            .max()
            .unwrap_or(0);
        let interval = Self::KINDS[kind].0;
        let timer = context
            .main_loop_handler
            .job_executor
            .clone()
            .create_timer(interval, interval);
        let mut theme_attr = crate::conf::value(context, "status.bar");
        if !context.settings.terminal.use_color() {
            theme_attr.attrs |= Attr::REVERSE;
        }
        theme_attr.attrs |= Attr::BOLD;
        Self {
            timer,
            stage: 0,
            kind: Ok(kind),
            width,
            theme_attr,
            dirty: true,
            active: false,
            id: ComponentId::default(),
        }
    }

    #[inline]
    pub fn is_active(&self) -> bool {
        self.active
    }

    /// See source code of [`Self::KINDS`].
    pub fn set_kind(&mut self, kind: usize) {
        self.stage = 0;
        self.width = Self::KINDS[kind % Self::KINDS.len()]
            .1
            .iter()
            .map(|f| f.grapheme_len())
            .max()
            .unwrap_or(0);
        self.kind = Ok(kind % Self::KINDS.len());
        let interval = Self::KINDS[kind % Self::KINDS.len()].0;
        self.timer.set_interval(interval);
        self.dirty = true;
    }

    pub fn set_custom_kind(&mut self, frames: Vec<String>, interval: u64) {
        self.stage = 0;
        self.width = frames.iter().map(|f| f.grapheme_len()).max().unwrap_or(0);
        if self.width == 0 {
            self.stop();
        }
        self.kind = Err(frames);
        self.timer.set_interval(Duration::from_millis(interval));
        self.dirty = true;
    }

    pub fn start(&mut self) {
        if self.width == 0 {
            return;
        }
        self.active = true;
        self.timer.rearm();
    }

    pub fn stop(&mut self) {
        self.active = false;
        self.stage = 0;
        self.timer.disable();
    }
}

impl std::fmt::Display for ProgressSpinner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "progress spinner")
    }
}

impl Component for ProgressSpinner {
    /// Draw current stage, if `self` is dirty.
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            grid.clear_area(area, self.theme_attr);
            if self.active {
                grid.write_string(
                    match self.kind.as_ref() {
                        Ok(kind) => (Self::KINDS[*kind].1)[self.stage],
                        Err(custom) => custom[self.stage].as_ref(),
                    },
                    self.theme_attr.fg,
                    self.theme_attr.bg,
                    self.theme_attr.attrs,
                    area,
                    None,
                    None,
                );
            }
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }
    }

    /// If the `event` is our timer firing, proceed to next stage.
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match event {
            UIEvent::Timer(id) if *id == self.timer.id() => {
                match self.kind.as_ref() {
                    Ok(kind) => {
                        self.stage = (self.stage + 1).wrapping_rem(Self::KINDS[*kind].1.len());
                    }
                    Err(custom) => {
                        self.stage = (self.stage + 1).wrapping_rem(custom.len());
                    }
                }
                self.dirty = true;
                true
            }
            _ => false,
        }
    }

    fn set_dirty(&mut self, new_val: bool) {
        self.dirty = new_val;
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
