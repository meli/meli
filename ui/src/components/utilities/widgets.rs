use super::*;
use fnv::FnvHashMap;

#[derive(Debug, PartialEq)]
enum FormFocus {
    Fields,
    Buttons,
    TextInput,
}

type Cursor = usize;

impl Default for FormFocus {
    fn default() -> FormFocus {
        FormFocus::Fields
    }
}

pub enum Field {
    Text(
        UText,
        Option<(Box<Fn(&Context, &str) -> Vec<String> + Send>, AutoComplete)>,
    ),
    Choice(Vec<String>, Cursor),
}

impl Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Text(s, _) => fmt::Debug::fmt(s, f),
            k => fmt::Debug::fmt(k, f),
        }
    }
}

use Field::*;

impl Default for Field {
    fn default() -> Field {
        Field::Text(UText::new(String::new()), None)
    }
}

impl Field {
    fn as_str(&self) -> &str {
        match self {
            Text(ref s, _) => s.as_str(),
            Choice(ref v, cursor) => {
                if v.is_empty() {
                    ""
                } else {
                    v[*cursor].as_str()
                }
            }
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Text(s, _) => s.into_string(),
            Choice(mut v, cursor) => v.remove(cursor),
        }
    }

    fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        let upper_left = upper_left!(area);
        match self {
            Text(ref term, auto_complete_fn) => {
                change_colors(
                    grid,
                    (
                        pos_inc(upper_left, (term.grapheme_pos(), 0)),
                        (pos_inc(upper_left, (term.grapheme_pos(), 0))),
                    ),
                    Color::Default,
                    Color::Byte(248),
                );
                if term.grapheme_len() <= 2 {
                    return;
                }
                if let Some((auto_complete_fn, auto_complete)) = auto_complete_fn {
                    let entries = auto_complete_fn(context, term.as_str());
                    auto_complete.set_suggestions(entries);
                    auto_complete.draw(grid, secondary_area, context);
                }
            }
            Choice(_, _cursor) => {}
        }
    }
}

impl Component for Field {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, _context: &mut Context) {
        write_string_to_grid(
            self.as_str(),
            grid,
            Color::Default,
            Color::Default,
            area,
            true,
        );
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        if let Text(ref mut s, Some((_, auto_complete))) = self {
            if let UIEventType::InsertInput(Key::Char('\t')) = event.event_type {
                if let Some(suggestion) = auto_complete.get_suggestion() {
                    *s = UText::new(suggestion);
                    let len = s.as_str().len().saturating_sub(1);
                    s.set_cursor(len);
                    return true;
                }
            }
        }

        match event.event_type {
            UIEventType::InsertInput(Key::Up) => {
                if let Text(_, Some((_, auto_complete))) = self {
                    auto_complete.dec_cursor();
                } else {
                    return false;
                }
            }
            UIEventType::InsertInput(Key::Down) => {
                if let Text(_, Some((_, auto_complete))) = self {
                    auto_complete.inc_cursor();
                } else {
                    return false;
                }
            }
            UIEventType::InsertInput(Key::Right) => match self {
                Text(ref mut s, _) => {
                    s.cursor_inc();
                }
                Choice(ref vec, ref mut cursor) => {
                    *cursor = if *cursor == vec.len().saturating_sub(1) {
                        0
                    } else {
                        *cursor + 1
                    };
                }
            },
            UIEventType::InsertInput(Key::Left) => match self {
                Text(ref mut s, _) => {
                    s.cursor_dec();
                }
                Choice(_, ref mut cursor) => {
                    if *cursor == 0 {
                        return false;
                    } else {
                        *cursor -= 1;
                    }
                }
            },
            UIEventType::InsertInput(Key::Char(k)) => {
                if let Text(ref mut s, _) = self {
                    s.insert_char(k);
                }
            }
            UIEventType::InsertInput(Key::Backspace) => match self {
                Text(ref mut s, auto_complete) => {
                    s.backspace();
                    if let Some(ac) = auto_complete.as_mut() {
                        ac.1.set_suggestions(Vec::new());
                    }
                }
                _ => {}
            },
            _ => {
                return false;
            }
        }
        self.set_dirty();
        true
    }
    fn is_dirty(&self) -> bool {
        true
    }
    fn set_dirty(&mut self) {}
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("field", f)
    }
}

#[derive(Debug, Default)]
pub struct FormWidget {
    fields: FnvHashMap<String, Field>,
    layout: Vec<String>,
    buttons: ButtonWidget<bool>,

    field_name_max_length: usize,
    cursor: usize,
    focus: FormFocus,
    hide_buttons: bool,
    dirty: bool,
}

impl fmt::Display for FormWidget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("", f)
    }
}

impl FormWidget {
    pub fn new(action: String) -> FormWidget {
        FormWidget {
            buttons: ButtonWidget::new((action, true)),
            focus: FormFocus::Fields,
            hide_buttons: false,
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
    }

    pub fn len(&self) -> usize {
        self.layout.len()
    }

    pub fn add_button(&mut self, val: (String, bool)) {
        self.buttons.push(val);
    }

    pub fn push_choices(&mut self, value: (String, Vec<String>)) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields.insert(value.0, Choice(value.1, 0));
    }
    pub fn push_cl(
        &mut self,
        value: (
            String,
            String,
            Box<Fn(&Context, &str) -> Vec<String> + Send>,
        ),
    ) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields.insert(
            value.0,
            Text(
                UText::new(value.1),
                Some((value.2, AutoComplete::new(Vec::new()))),
            ),
        );
    }
    pub fn push(&mut self, value: (String, String)) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields.insert(value.0, Text(UText::new(value.1), None));
    }

    pub fn insert(&mut self, index: usize, value: (String, Field)) {
        self.layout.insert(index, value.0.clone());
        self.fields.insert(value.0, value.1);
    }

    pub fn values_mut(&mut self) -> &mut FnvHashMap<String, Field> {
        &mut self.fields
    }

    pub fn collect(self) -> Option<FnvHashMap<String, Field>> {
        if let Some(true) = self.buttons_result() {
            Some(self.fields)
        } else {
            None
        }
    }
    pub fn buttons_result(&self) -> Option<bool> {
        self.buttons.result
    }
}

impl Component for FormWidget {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        for (i, k) in self.layout.iter().enumerate() {
            let v = self.fields.get_mut(k).unwrap();
            /* Write field label */
            write_string_to_grid(
                k.as_str(),
                grid,
                Color::Default,
                Color::Default,
                (
                    pos_inc(upper_left, (1, i)),
                    set_y(bottom_right, i + get_y(upper_left)),
                ),
                false,
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
                    change_colors(
                        grid,
                        (
                            pos_inc(upper_left, (0, i)),
                            set_y(bottom_right, i + get_y(upper_left)),
                        ),
                        Color::Default,
                        Color::Byte(246),
                    );
                }
                if self.focus == FormFocus::TextInput {
                    v.draw_cursor(
                        grid,
                        (
                            pos_inc(upper_left, (self.field_name_max_length + 3, i)),
                            (
                                get_x(upper_left) + self.field_name_max_length + 3,
                                i + get_y(upper_left),
                            ),
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
        if !self.hide_buttons {
            let length = self.layout.len();
            self.buttons.draw(
                grid,
                (
                    pos_inc(upper_left, (1, length * 2 + 3)),
                    set_y(bottom_right, length * 2 + 3 + get_y(upper_left)),
                ),
                context,
            );
        }
        self.dirty = false;
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.focus == FormFocus::Buttons && self.buttons.process_event(event, context) {
            return true;
        }

        match event.event_type {
            UIEventType::Input(Key::Up) if self.focus == FormFocus::Buttons => {
                self.focus = FormFocus::Fields;
            }
            UIEventType::InsertInput(Key::Up) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEventType::Input(Key::Up) => {
                self.cursor = self.cursor.saturating_sub(1);
            }
            UIEventType::InsertInput(Key::Down) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEventType::Input(Key::Down) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
            }
            UIEventType::Input(Key::Down) if self.focus == FormFocus::Fields => {
                self.focus = FormFocus::Buttons;
                if self.hide_buttons {
                    self.set_dirty();
                    return false;
                }
            }
            UIEventType::InsertInput(Key::Char('\t')) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEventType::Input(Key::Char('\n')) if self.focus == FormFocus::Fields => {
                self.focus = FormFocus::TextInput;
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::ChangeMode(UIMode::Insert),
                });
            }
            UIEventType::InsertInput(Key::Right) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEventType::InsertInput(Key::Left) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                if !field.process_event(event, context) {
                    self.focus = FormFocus::Fields;
                    context.replies.push_back(UIEvent {
                        id: 0,
                        event_type: UIEventType::ChangeMode(UIMode::Normal),
                    });
                }
            }
            UIEventType::ChangeMode(UIMode::Normal) if self.focus == FormFocus::TextInput => {
                self.focus = FormFocus::Fields;
            }
            UIEventType::InsertInput(Key::Char(_)) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEventType::InsertInput(Key::Backspace) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            _ => {
                return false;
            }
        }
        self.set_dirty();
        true
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}

#[derive(Debug, Default)]
pub struct ButtonWidget<T>
where
    T: std::fmt::Debug + Default + Send,
{
    buttons: FnvHashMap<String, T>,
    layout: Vec<String>,

    result: Option<T>,
    cursor: usize,
}

impl<T> fmt::Display for ButtonWidget<T>
where
    T: std::fmt::Debug + Default + Send,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("", f)
    }
}

impl<T> ButtonWidget<T>
where
    T: std::fmt::Debug + Default + Send,
{
    pub fn new(init_val: (String, T)) -> ButtonWidget<T> {
        ButtonWidget {
            layout: vec![init_val.0.clone()],
            buttons: vec![init_val].into_iter().collect(),
            result: None,
            cursor: 0,
        }
    }

    pub fn push(&mut self, value: (String, T)) {
        self.layout.push(value.0.clone());
        self.buttons.insert(value.0, value.1);
    }

    pub fn is_resolved(&self) -> bool {
        self.result.is_some()
    }
}

impl<T> Component for ButtonWidget<T>
where
    T: std::fmt::Debug + Default + Send,
{
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, _context: &mut Context) {
        let upper_left = upper_left!(area);

        let mut len = 0;
        for (i, k) in self.layout.iter().enumerate() {
            let cur_len = k.len();
            write_string_to_grid(
                k.as_str(),
                grid,
                Color::Default,
                if i == self.cursor {
                    Color::Byte(246)
                } else {
                    Color::Default
                },
                (
                    pos_inc(upper_left, (len, 0)),
                    pos_inc(upper_left, (cur_len + len, 0)),
                ),
                false,
            );
            len += cur_len + 3;
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match event.event_type {
            UIEventType::Input(Key::Char('\n')) => {
                self.result = Some(
                    self.buttons
                        .remove(&self.layout[self.cursor])
                        .unwrap_or_default(),
                );
                return true;
            }
            UIEventType::Input(Key::Left) => {
                self.cursor = self.cursor.saturating_sub(1);
                return true;
            }
            UIEventType::Input(Key::Right) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
                return true;
            }
            _ => {}
        }

        false
    }
    fn is_dirty(&self) -> bool {
        true
    }
    fn set_dirty(&mut self) {}
}

#[derive(Debug, PartialEq)]
pub struct AutoComplete {
    entries: Vec<String>,
    content: CellBuffer,
    cursor: usize,

    dirty: bool,
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

        let upper_left = upper_left!(area);
        self.dirty = false;
        let (width, height) = self.content.size();
        copy_area(
            grid,
            &self.content,
            area,
            ((0, 0), (width.saturating_sub(1), height.saturating_sub(1))),
        );
        /* Highlight cursor */
        change_colors(
            grid,
            (
                pos_inc(upper_left, (0, self.cursor)),
                pos_inc(upper_left, (width.saturating_sub(1), self.cursor)),
            ),
            Color::Default,
            Color::Byte(246),
        );
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}

impl AutoComplete {
    pub fn new(entries: Vec<String>) -> Self {
        let mut ret = AutoComplete {
            entries: Vec::new(),
            content: CellBuffer::default(),
            cursor: 0,
            dirty: true,
        };
        ret.set_suggestions(entries);
        ret
    }

    pub fn set_suggestions(&mut self, entries: Vec<String>) {
        if entries.len() == self.entries.len() && entries == self.entries {
            return;
        }

        let mut content = CellBuffer::new(
            entries.iter().map(|e| e.len()).max().unwrap_or(0) + 1,
            entries.len(),
            Cell::with_style(Color::Byte(23), Color::Byte(7), Attr::Default),
        );
        let width = content.cols();
        for (i, e) in entries.iter().enumerate() {
            write_string_to_grid(
                e,
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                ((0, i), (width - 1, i)),
                false,
            );
            write_string_to_grid(
                "▒",
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                ((width - 1, i), (width - 1, i)),
                false,
            );
        }
        self.content = content;
        self.entries = entries;
        self.cursor = 0;
    }

    pub fn inc_cursor(&mut self) {
        if self.cursor < self.entries.len().saturating_sub(1) {
            self.cursor += 1;
            self.set_dirty();
        }
    }
    pub fn dec_cursor(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
        self.set_dirty();
    }

    pub fn get_suggestion(&mut self) -> Option<String> {
        if self.entries.is_empty() {
            return None;
        }
        let ret = self.entries.remove(self.cursor);
        self.entries.clear();
        self.cursor = 0;
        self.content.empty();
        Some(ret)
    }
}
