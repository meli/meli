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

use std::{borrow::Cow, collections::HashMap, time::Duration};

use super::*;

#[derive(Debug, PartialEq, Eq, Default)]
enum FormFocus {
    #[default]
    Fields,
    Buttons,
    TextInput,
}

type Cursor = usize;

pub enum Field {
    Text(TextField),
    Choice(Vec<Cow<'static, str>>, Cursor, ComponentId),
}

impl Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Text(ref t) => fmt::Debug::fmt(t, f),
            k => fmt::Debug::fmt(k, f),
        }
    }
}

impl Default for Field {
    fn default() -> Field {
        Field::Text(TextField::default())
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
                write_string_to_grid(
                    str,
                    grid,
                    theme_attr.fg,
                    theme_attr.bg,
                    theme_attr.attrs,
                    area,
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

    fn set_id(&mut self, id: ComponentId) {
        match self {
            Self::Text(ref mut i) => i.set_id(id),
            Self::Choice(_, _, i) => *i = id,
        }
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum FormButtonActions {
    Accept,
    Reset,
    #[default]
    Cancel,
    Other(&'static str),
}

#[derive(Debug, Default)]
pub struct FormWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    fields: HashMap<Cow<'static, str>, Field>,
    layout: Vec<Cow<'static, str>>,
    buttons: ButtonWidget<T>,

    field_name_max_length: usize,
    cursor: usize,
    focus: FormFocus,
    hide_buttons: bool,
    dirty: bool,
    id: ComponentId,
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> fmt::Display for FormWidget<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("", f)
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> FormWidget<T> {
    pub fn new(action: (Cow<'static, str>, T)) -> FormWidget<T> {
        FormWidget {
            buttons: ButtonWidget::new(action),
            focus: FormFocus::Fields,
            hide_buttons: false,
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

    pub fn values(&self) -> &HashMap<Cow<'static, str>, Field> {
        &self.fields
    }

    pub fn values_mut(&mut self) -> &mut HashMap<Cow<'static, str>, Field> {
        &mut self.fields
    }

    pub fn collect(self) -> Option<HashMap<Cow<'static, str>, Field>> {
        if self.buttons_result().is_some() {
            Some(self.fields)
        } else {
            None
        }
    }
    pub fn buttons_result(&self) -> Option<T> {
        self.buttons.result
    }
}

impl<T: 'static + std::fmt::Debug + Copy + Default + Send + Sync> Component for FormWidget<T> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        if self.is_dirty() {
            let theme_default = crate::conf::value(context, "theme_default");
            clear_area(
                grid,
                (
                    upper_left,
                    set_y(bottom_right, get_y(upper_left) + self.layout.len()),
                ),
                theme_default,
            );
            let label_attrs = crate::conf::value(context, "widgets.form.label");

            for (i, k) in self.layout.iter().enumerate().rev() {
                let v = self.fields.get_mut(k).unwrap();
                /* Write field label */
                write_string_to_grid(
                    k.as_ref(),
                    grid,
                    label_attrs.fg,
                    label_attrs.bg,
                    label_attrs.attrs,
                    (
                        pos_inc(upper_left, (1, i)),
                        set_y(bottom_right, i + get_y(upper_left)),
                    ),
                    None,
                );
                /* draw field */
                v.draw(
                    grid,
                    (
                        pos_inc(upper_left, (self.field_name_max_length + 3, i)),
                        set_y(bottom_right, i + get_y(upper_left)),
                    ),
                    context,
                );

                /* Highlight if necessary */
                if i == self.cursor {
                    if self.focus == FormFocus::Fields {
                        let mut field_attrs =
                            crate::conf::value(context, "widgets.form.highlighted");
                        if !context.settings.terminal.use_color() {
                            field_attrs.attrs |= Attr::REVERSE;
                        }
                        for row in grid.bounds_iter((
                            pos_inc(upper_left, (0, i)),
                            (get_x(bottom_right).saturating_sub(1), i + get_y(upper_left)),
                        )) {
                            for c in row {
                                grid[c]
                                    .set_fg(field_attrs.fg)
                                    .set_bg(field_attrs.bg)
                                    .set_attrs(field_attrs.attrs);
                            }
                        }
                    }
                    if self.focus == FormFocus::TextInput {
                        v.draw_cursor(
                            grid,
                            (
                                pos_inc(upper_left, (self.field_name_max_length + 3, i)),
                                (get_x(bottom_right), i + get_y(upper_left)),
                            ),
                            (
                                pos_inc(upper_left, (self.field_name_max_length + 3, i + 1)),
                                bottom_right,
                            ),
                            context,
                        );
                    }
                }
            }

            let length = self.layout.len();
            clear_area(
                grid,
                (
                    pos_inc(upper_left, (0, length)),
                    set_y(bottom_right, length + 2 + get_y(upper_left)),
                ),
                theme_default,
            );
            if !self.hide_buttons {
                self.buttons.draw(
                    grid,
                    (
                        pos_inc(upper_left, (1, length + 3)),
                        set_y(bottom_right, length + 3 + get_y(upper_left)),
                    ),
                    context,
                );
            }
            if length + 4 < height!(area) {
                clear_area(
                    grid,
                    (pos_inc(upper_left, (0, length + 4)), bottom_right),
                    theme_default,
                );
            }
            self.set_dirty(false);
            context.dirty_areas.push_back(area);
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.focus == FormFocus::Buttons && self.buttons.process_event(event, context) {
            return true;
        }

        match *event {
            UIEvent::Input(Key::Up) if self.focus == FormFocus::Buttons => {
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
            UIEvent::Input(Key::Up) => {
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
            UIEvent::Input(Key::Down) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Down) if self.focus == FormFocus::Fields => {
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

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

#[derive(Debug, Default)]
pub struct ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    buttons: HashMap<Cow<'static, str>, T>,
    layout: Vec<Cow<'static, str>>,

    result: Option<T>,
    cursor: usize,
    /// Is the button widget focused, i.e do we need to draw the highlighting?
    focus: bool,
    dirty: bool,
    id: ComponentId,
}

impl<T> fmt::Display for ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("", f)
    }
}

impl<T> ButtonWidget<T>
where
    T: 'static + std::fmt::Debug + Copy + Default + Send + Sync,
{
    pub fn new(init_val: (Cow<'static, str>, T)) -> ButtonWidget<T> {
        ButtonWidget {
            layout: vec![init_val.0.clone()],
            buttons: vec![init_val].into_iter().collect(),
            result: None,
            cursor: 0,
            focus: false,
            dirty: true,
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

    pub fn result(&self) -> Option<T> {
        self.result
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
            clear_area(grid, area, theme_default);
            let upper_left = upper_left!(area);

            let mut len = 0;
            for (i, k) in self.layout.iter().enumerate() {
                let cur_len = k.len();
                write_string_to_grid(
                    k.as_ref(),
                    grid,
                    theme_default.fg,
                    if i == self.cursor && self.focus {
                        crate::conf::value(context, "highlight").bg
                    } else {
                        theme_default.bg
                    },
                    Attr::BOLD,
                    (
                        pos_inc(upper_left, (len, 0)),
                        pos_inc(upper_left, (cur_len + len, 0)),
                    ),
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
                        .remove(&self.layout[self.cursor])
                        .unwrap_or_default(),
                );
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Left) => {
                self.cursor = self.cursor.saturating_sub(1);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Right) if self.cursor < self.layout.len().saturating_sub(1) => {
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

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
        AutoCompleteEntry {
            entry: val,
            description: String::new(),
        }
    }
}

impl From<&(&str, &str, TokenStream)> for AutoCompleteEntry {
    fn from(val: &(&str, &str, TokenStream)) -> Self {
        let (a, b, _) = val;
        AutoCompleteEntry {
            entry: a.to_string(),
            description: b.to_string(),
        }
    }
}

impl From<(String, String)> for AutoCompleteEntry {
    fn from(val: (String, String)) -> Self {
        let (a, b) = val;
        AutoCompleteEntry {
            entry: a,
            description: b,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AutoComplete {
    entries: Vec<AutoCompleteEntry>,
    content: CellBuffer,
    cursor: usize,

    dirty: bool,
    id: ComponentId,
}

impl fmt::Display for AutoComplete {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("AutoComplete", f)
    }
}

impl Component for AutoComplete {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            return;
        };
        self.dirty = false;

        let (upper_left, bottom_right) = area;
        let rows = get_y(bottom_right) - get_y(upper_left);
        if rows == 0 {
            return;
        }
        let page_no = (self.cursor.saturating_sub(1)).wrapping_div(rows);
        let top_idx = page_no * rows;
        let x_offset = if rows < self.entries.len() { 1 } else { 0 };

        let (width, height) = self.content.size();
        clear_area(grid, area, crate::conf::value(context, "theme_default"));
        copy_area(
            grid,
            &self.content,
            (upper_left, pos_dec(bottom_right, (x_offset, 0))),
            (
                (0, top_idx),
                (width.saturating_sub(1), height.saturating_sub(1)),
            ),
        );
        /* Highlight cursor */
        if self.cursor > 0 {
            change_colors(
                grid,
                (
                    pos_inc(upper_left, (0, (self.cursor - 1) % rows)),
                    (
                        std::cmp::min(
                            get_x(upper_left) + width.saturating_sub(1),
                            get_x(bottom_right),
                        )
                        .saturating_sub(x_offset),
                        get_y(pos_inc(upper_left, (0, (self.cursor - 1) % rows))),
                    ),
                ),
                crate::conf::value(context, "highlight").fg,
                crate::conf::value(context, "highlight").bg,
            );
        }
        if rows < self.entries.len() {
            ScrollBar { show_arrows: false }.draw(
                grid,
                (
                    set_y(pos_dec(bottom_right, (x_offset, 0)), get_y(upper_left)),
                    pos_dec(bottom_right, (x_offset, 0)),
                ),
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
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl AutoComplete {
    pub fn new(entries: Vec<AutoCompleteEntry>) -> Box<Self> {
        let mut ret = AutoComplete {
            entries: Vec::new(),
            content: CellBuffer::default(),
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

        // FIXME: remove hardcoded color values
        let mut content = CellBuffer::new(
            entries
                .iter()
                .map(|a| a.entry.grapheme_len() + a.description.grapheme_len() + 2)
                .max()
                .unwrap_or(0)
                + 1,
            entries.len(),
            Cell::with_style(Color::Byte(23), Color::Byte(7), Attr::DEFAULT),
        );
        let width = content.cols();
        for (i, e) in entries.iter().enumerate() {
            let (x, _) = write_string_to_grid(
                &e.entry,
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                Attr::DEFAULT,
                ((0, i), (width - 1, i)),
                None,
            );
            write_string_to_grid(
                &e.description,
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                Attr::ITALICS,
                ((x + 2, i), (width - 1, i)),
                None,
            );
            write_string_to_grid(
                "▒",
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                Attr::DEFAULT,
                ((width - 1, i), (width - 1, i)),
                None,
            );
        }
        self.content = content;
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
        self.content.empty();
        Some(ret.entry)
    }

    pub fn suggestions(&self) -> &Vec<AutoCompleteEntry> {
        &self.entries
    }
}

#[derive(Default, Copy, Clone)]
pub struct ScrollBar {
    pub show_arrows: bool,
}

impl ScrollBar {
    pub fn set_show_arrows(&mut self, new_val: bool) -> &mut Self {
        self.show_arrows = new_val;
        self
    }

    pub fn draw(
        &mut self,
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
        let height = height!(area);
        if height < 3 {
            return;
        }
        let theme_default = crate::conf::value(context, "theme_default");
        clear_area(grid, area, theme_default);

        let visible_rows = std::cmp::min(visible_rows, length);
        let ascii_drawing = grid.ascii_drawing;
        let ratio: f64 = (height as f64) / (length as f64);
        let scrollbar_height = std::cmp::max((ratio * (visible_rows as f64)) as usize, 1);
        let scrollbar_offset = (ratio * (pos as f64)) as usize;
        let (mut upper_left, bottom_right) = area;

        if self.show_arrows {
            grid[upper_left]
                .set_ch(if ascii_drawing { '^' } else { '▀' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg);
            upper_left = pos_inc(upper_left, (0, 1));
        }

        upper_left = pos_inc(upper_left, (0, scrollbar_offset));
        for _ in 0..scrollbar_height {
            if get_y(upper_left) >= get_y(bottom_right) {
                break;
            }
            grid[upper_left]
                .set_ch(if ascii_drawing { '#' } else { '█' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_attrs(if !context.settings.terminal.use_color() {
                    theme_default.attrs | Attr::REVERSE
                } else {
                    theme_default.attrs
                });
            upper_left = pos_inc(upper_left, (0, 1));
        }
        if self.show_arrows {
            grid[bottom_right]
                .set_ch(if ascii_drawing { 'v' } else { '▄' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_bg(crate::conf::value(context, "theme_default").bg);
        }
    }

    pub fn draw_horizontal(
        &mut self,
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
        let width = width!(area);
        if width < 3 {
            return;
        }
        let theme_default = crate::conf::value(context, "theme_default");
        clear_area(grid, area, theme_default);

        let visible_cols = std::cmp::min(visible_cols, length);
        let ascii_drawing = grid.ascii_drawing;
        let ratio: f64 = (width as f64) / (length as f64);
        let scrollbar_width = std::cmp::min((ratio * (visible_cols as f64)) as usize, 1);
        let scrollbar_offset = (ratio * (pos as f64)) as usize;
        let (mut upper_left, bottom_right) = area;

        if self.show_arrows {
            grid[upper_left]
                .set_ch(if ascii_drawing { '<' } else { '▐' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg);
            upper_left = pos_inc(upper_left, (1, 0));
        }

        upper_left = pos_inc(upper_left, (scrollbar_offset, 0));
        for _ in 0..scrollbar_width {
            if get_x(upper_left) >= get_x(bottom_right) {
                break;
            }
            grid[upper_left]
                .set_ch(if ascii_drawing { '#' } else { '█' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_attrs(if !context.settings.terminal.use_color() {
                    theme_default.attrs | Attr::REVERSE
                } else {
                    theme_default.attrs
                });
            upper_left = pos_inc(upper_left, (1, 0));
        }
        if self.show_arrows {
            grid[bottom_right]
                .set_ch(if ascii_drawing { '>' } else { '▌' })
                .set_fg(crate::conf::value(context, "widgets.options.highlighted").bg)
                .set_bg(crate::conf::value(context, "theme_default").bg);
        }
    }
}

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
            .job_executor
            .clone()
            .create_timer(interval, interval);
        let mut theme_attr = crate::conf::value(context, "status.bar");
        if !context.settings.terminal.use_color() {
            theme_attr.attrs |= Attr::REVERSE;
        }
        theme_attr.attrs |= Attr::BOLD;
        ProgressSpinner {
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

    pub fn is_active(&self) -> bool {
        self.active
    }

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

impl fmt::Display for ProgressSpinner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "progress bar")
    }
}

impl Component for ProgressSpinner {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            clear_area(grid, area, self.theme_attr);
            if self.active {
                write_string_to_grid(
                    match self.kind.as_ref() {
                        Ok(kind) => (Self::KINDS[*kind].1)[self.stage],
                        Err(custom) => custom[self.stage].as_ref(),
                    },
                    grid,
                    self.theme_attr.fg,
                    self.theme_attr.bg,
                    self.theme_attr.attrs,
                    area,
                    None,
                );
            }
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }
    }

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

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
