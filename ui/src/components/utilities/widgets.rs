use super::*;
use fnv::FnvHashMap;

type AutoCompleteFn = Box<dyn Fn(&Context, &str) -> Vec<AutoCompleteEntry> + Send>;

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
    Text(UText, Option<(AutoCompleteFn, AutoComplete)>),
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

use crate::Field::*;

impl Default for Field {
    fn default() -> Field {
        Field::Text(UText::new(String::with_capacity(256)), None)
    }
}

impl Field {
    pub fn as_str(&self) -> &str {
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
    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    pub fn to_string(&self) -> String {
        match self {
            Text(ref s, _) => s.as_str().to_string(),
            Choice(ref v, ref cursor) => v[*cursor].clone(),
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Text(s, _) => s.into_string(),
            Choice(mut v, cursor) => v.remove(cursor),
        }
    }

    pub fn clear(&mut self) {
        match self {
            Text(s, _) => s.clear(),
            Choice(_, _) => {}
        }
    }

    pub fn draw_cursor(
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
            Attr::Default,
            area,
            true,
        );
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        if let Text(ref mut s, Some((_, auto_complete))) = self {
            if let UIEvent::InsertInput(Key::Char('\t')) = event {
                if let Some(suggestion) = auto_complete.get_suggestion() {
                    *s = UText::new(suggestion);
                    let len = s.as_str().len().saturating_sub(1);
                    s.set_cursor(len);
                    return true;
                }
            }
        }

        match *event {
            UIEvent::InsertInput(Key::Up) => {
                if let Text(_, Some((_, auto_complete))) = self {
                    auto_complete.dec_cursor();
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Down) => {
                if let Text(_, Some((_, auto_complete))) = self {
                    auto_complete.inc_cursor();
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Right) => match self {
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
            UIEvent::InsertInput(Key::Left) => match self {
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
            UIEvent::InsertInput(Key::Char(k)) => {
                if let Text(ref mut s, _) = self {
                    s.insert_char(k);
                }
            }
            UIEvent::InsertInput(Key::Paste(ref p)) => {
                if let Text(ref mut s, _) = self {
                    for c in p.chars() {
                        s.insert_char(c);
                    }
                }
            }
            UIEvent::InsertInput(Key::Backspace) | UIEvent::InsertInput(Key::Ctrl('h')) => {
                if let Text(ref mut s, auto_complete) = self {
                    s.backspace();
                    if let Some(ac) = auto_complete.as_mut() {
                        ac.1.set_suggestions(Vec::new());
                    }
                }
            }
            UIEvent::InsertInput(Key::Ctrl('a')) => {
                if let Text(ref mut s, _) = self {
                    s.set_cursor(0);
                }
            }
            UIEvent::InsertInput(Key::Ctrl('e')) => {
                if let Text(ref mut s, _) = self {
                    s.set_cursor(s.as_str().len());
                }
            }
            /* TODO: add rest of readline shortcuts */
            _ => {
                return false;
            }
        }
        self.set_dirty();
        true
    }
    fn is_dirty(&self) -> bool {
        false
    }
    fn set_dirty(&mut self) {}

    fn id(&self) -> ComponentId {
        ComponentId::nil()
    }
    fn set_id(&mut self, _id: ComponentId) {}
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
    id: ComponentId,
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
            id: ComponentId::new_v4(),
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
    pub fn push_cl(&mut self, value: (String, String, AutoCompleteFn)) {
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

        if self.dirty {
            clear_area(
                grid,
                (
                    upper_left,
                    set_y(bottom_right, get_y(upper_left) + self.layout.len()),
                ),
            );

            for (i, k) in self.layout.iter().enumerate() {
                let v = self.fields.get_mut(k).unwrap();
                /* Write field label */
                write_string_to_grid(
                    k.as_str(),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
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

            let length = self.layout.len();
            clear_area(
                grid,
                (
                    pos_inc(upper_left, (0, length)),
                    set_y(bottom_right, length + 2 + get_y(upper_left)),
                ),
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
            clear_area(
                grid,
                (
                    set_y(upper_left, length + 4 + get_y(upper_left)),
                    bottom_right,
                ),
            );
            self.dirty = false;
        }
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.focus == FormFocus::Buttons && self.buttons.process_event(event, context) {
            return true;
        }

        match *event {
            UIEvent::Input(Key::Up) if self.focus == FormFocus::Buttons => {
                self.focus = FormFocus::Fields;
                self.buttons.set_focus(false);
            }
            UIEvent::InsertInput(Key::Up) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEvent::Input(Key::Up) => {
                self.cursor = self.cursor.saturating_sub(1);
            }
            UIEvent::InsertInput(Key::Down) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEvent::Input(Key::Down) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
            }
            UIEvent::Input(Key::Down) if self.focus == FormFocus::Fields => {
                self.focus = FormFocus::Buttons;
                self.buttons.set_focus(true);
                if self.hide_buttons {
                    self.set_dirty();
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Char('\t')) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEvent::Input(Key::Char('\n')) if self.focus == FormFocus::Fields => {
                self.focus = FormFocus::TextInput;
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Insert));
            }
            UIEvent::InsertInput(Key::Right) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEvent::InsertInput(Key::Left) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                if !field.process_event(event, context) {
                    self.focus = FormFocus::Fields;
                    context
                        .replies
                        .push_back(UIEvent::ChangeMode(UIMode::Normal));
                }
            }
            UIEvent::ChangeMode(UIMode::Normal) if self.focus == FormFocus::TextInput => {
                self.focus = FormFocus::Fields;
            }
            UIEvent::InsertInput(Key::Backspace) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.process_event(event, context);
            }
            UIEvent::InsertInput(_) if self.focus == FormFocus::TextInput => {
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
        self.dirty || self.buttons.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        self.buttons.set_dirty();
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
    T: std::fmt::Debug + Default + Send,
{
    buttons: FnvHashMap<String, T>,
    layout: Vec<String>,

    result: Option<T>,
    cursor: usize,
    /// Is the button widget focused, i.e do we need to draw the highlighting?
    focus: bool,
    dirty: bool,
    id: ComponentId,
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
            focus: false,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }

    pub fn push(&mut self, value: (String, T)) {
        self.layout.push(value.0.clone());
        self.buttons.insert(value.0, value.1);
    }

    pub fn is_resolved(&self) -> bool {
        self.result.is_some()
    }

    pub fn set_focus(&mut self, new_val: bool) {
        self.focus = new_val;
    }
}

impl<T> Component for ButtonWidget<T>
where
    T: std::fmt::Debug + Default + Send,
{
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, _context: &mut Context) {
        if self.dirty {
            clear_area(grid, area);
            let upper_left = upper_left!(area);

            let mut len = 0;
            for (i, k) in self.layout.iter().enumerate() {
                let cur_len = k.len();
                write_string_to_grid(
                    k.as_str(),
                    grid,
                    Color::Default,
                    if i == self.cursor && self.focus {
                        Color::Byte(246)
                    } else {
                        Color::Default
                    },
                    Attr::Bold,
                    (
                        pos_inc(upper_left, (len, 0)),
                        pos_inc(upper_left, (cur_len + len, 0)),
                    ),
                    false,
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
            }
            UIEvent::Input(Key::Left) => {
                self.cursor = self.cursor.saturating_sub(1);
            }
            UIEvent::Input(Key::Right) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
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

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

#[derive(Debug, PartialEq, Clone)]
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

impl From<&(&str, &str)> for AutoCompleteEntry {
    fn from(val: &(&str, &str)) -> Self {
        let (a, b) = val;
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

#[derive(Debug, PartialEq, Clone)]
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

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl AutoComplete {
    pub fn new(entries: Vec<AutoCompleteEntry>) -> Self {
        let mut ret = AutoComplete {
            entries: Vec::new(),
            content: CellBuffer::default(),
            cursor: 0,
            dirty: true,
            id: ComponentId::new_v4(),
        };
        ret.set_suggestions(entries);
        ret
    }

    pub fn set_suggestions(&mut self, entries: Vec<AutoCompleteEntry>) -> bool {
        if entries.len() == self.entries.len() && entries == self.entries {
            return false;
        }

        let mut content = CellBuffer::new(
            entries
                .iter()
                .map(|a| a.entry.grapheme_len() + a.description.grapheme_len() + 2)
                .max()
                .unwrap_or(0)
                + 1,
            entries.len(),
            Cell::with_style(Color::Byte(23), Color::Byte(7), Attr::Default),
        );
        let width = content.cols();
        for (i, e) in entries.iter().enumerate() {
            let (x, _) = write_string_to_grid(
                &e.entry,
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                Attr::Default,
                ((0, i), (width - 1, i)),
                false,
            );
            write_string_to_grid(
                &e.description,
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                Attr::Default,
                ((x + 2, i), (width - 1, i)),
                false,
            );
            write_string_to_grid(
                "▒",
                &mut content,
                Color::Byte(23),
                Color::Byte(7),
                Attr::Default,
                ((width - 1, i), (width - 1, i)),
                false,
            );
        }
        self.content = content;
        self.entries = entries;
        self.cursor = 0;
        true
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

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn set_cursor(&mut self, val: usize) {
        debug_assert!(val < self.entries.len());
        self.cursor = val;
    }

    pub fn get_suggestion(&mut self) -> Option<String> {
        if self.entries.is_empty() {
            return None;
        }
        let ret = self.entries.remove(self.cursor);
        self.entries.clear();
        self.cursor = 0;
        self.content.empty();
        Some(ret.entry)
    }

    pub fn suggestions(&self) -> &Vec<AutoCompleteEntry> {
        &self.entries
    }
}

#[derive(Default)]
pub struct ScrollBar {
    show_arrows: bool,
    block_character: Option<char>,
}

impl ScrollBar {
    pub fn set_show_arrows(&mut self, flag: bool) {
        self.show_arrows = flag;
    }
    pub fn set_block_character(&mut self, val: Option<char>) {
        self.block_character = val;
    }
    pub fn draw(
        self,
        grid: &mut CellBuffer,
        area: Area,
        pos: usize,
        visible_rows: usize,
        length: usize,
    ) {
        if length == 0 {
            return;
        }
        let mut height = height!(area);
        if height < 3 {
            return;
        }
        if self.show_arrows {
            height -= height;
        }
        clear_area(grid, area);

        let visible_ratio: f32 = (std::cmp::min(visible_rows, length) as f32) / (length as f32);
        let scrollbar_height = std::cmp::max((visible_ratio * (height as f32)) as usize, 1);
        let scrollbar_offset = {
            let temp = (((pos as f32) / (length as f32)) * (height as f32)) as usize;
            if temp + scrollbar_height >= height {
                height - scrollbar_height
            } else {
                temp
            }
        };
        let (mut upper_left, bottom_right) = area;

        if self.show_arrows {
            grid[upper_left].set_ch('▴');
            upper_left = (upper_left.0, upper_left.1 + 1);
        }

        for y in get_y(upper_left)..(get_y(upper_left) + scrollbar_offset) {
            grid[set_y(upper_left, y)].set_ch(' ');
        }
        for y in (get_y(upper_left) + scrollbar_offset)
            ..=(get_y(upper_left) + scrollbar_offset + scrollbar_height)
        {
            grid[set_y(upper_left, y)].set_ch(self.block_character.unwrap_or('█'));
        }
        for y in (get_y(upper_left) + scrollbar_offset + scrollbar_height + 1)..get_y(bottom_right)
        {
            grid[set_y(upper_left, y)].set_ch(' ');
        }
        if self.show_arrows {
            grid[set_x(bottom_right, get_x(upper_left))].set_ch('▾');
        }
    }
}
