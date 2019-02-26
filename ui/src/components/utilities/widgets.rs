use super::*;
use fnv::FnvHashMap;

#[derive(Debug, PartialEq)]
enum FormFocus {
    Fields,
    Buttons,
    TextInput,
}

/*
enum Field {
    Text(String),
    Choice(Vec<String>),
}
*/

impl Default for FormFocus {
    fn default() -> FormFocus {
        FormFocus::Fields
    }
}

#[derive(Debug, Default)]
pub struct FormWidget {
    fields: FnvHashMap<String, String>,
    cursors: Vec<usize>,
    layout: Vec<String>,
    buttons: ButtonWidget<bool>,

    field_name_max_length: usize,
    cursor: usize,
    focus: FormFocus,
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
            ..Default::default()
        }
    }

    pub fn add_button(&mut self, val: (String, bool)) {
        self.buttons.push(val);
    }

    pub fn push(&mut self, value: (String, String)) {
        self.field_name_max_length = std::cmp::max(self.field_name_max_length, value.0.len());
        self.layout.push(value.0.clone());
        self.fields.insert(value.0, value.1);
        self.cursors.push(0);
    }

    pub fn insert(&mut self, index: usize, value: (String, String)) {
        self.layout.insert(index, value.0.clone());
        self.fields.insert(value.0, value.1);
        self.cursors.insert(index, 0);
    }

    pub fn collect(self) -> Option<FnvHashMap<String, String>> {
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
            let v = &self.fields[k];
            write_string_to_grid(
                k.as_str(),
                grid,
                Color::Default,
                Color::Default,
                (pos_inc(upper_left, (1, i * 2)), set_y(bottom_right, i * 2 + get_y(upper_left))),
                false,
                );
            write_string_to_grid(
                v.as_str(),
                grid,
                Color::Default,
                Color::Default,
                (pos_inc(upper_left, (self.field_name_max_length + 3, i * 2)), set_y(bottom_right, i * 2 + get_y(upper_left))),
                false,
                );
            if i == self.cursor  {
                if self.focus == FormFocus::Fields {
                change_colors(grid, (pos_inc(upper_left, (0, i * 2)), set_y(bottom_right, i * 2 + get_y(upper_left))), Color::Default, Color::Byte(246));
                }
                if self.focus == FormFocus::TextInput {
                    change_colors(grid,
                                  (pos_inc(upper_left, (self.field_name_max_length + 3 + self.cursors[i], i * 2)),
                                  (get_x(upper_left) + self.field_name_max_length + 3 + self.cursors[i], i * 2 + get_y(upper_left))),
                                  Color::Default, Color::Byte(248));
                }
            }
        }
        let length = self.layout.len();
        self.buttons.draw(grid, 
                (pos_inc(upper_left, (1, length * 2 + 3)), set_y(bottom_right, length * 2 + 3 + get_y(upper_left))),
                context);
        self.dirty = false;
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        if self.focus == FormFocus::Buttons {
            if self.buttons.process_event(event, context) {
                return true;
            }
        }

        match event.event_type {
            UIEventType::Input(Key::Up) if self.focus == FormFocus::Buttons => {
                self.focus = FormFocus::Fields;
            },
            UIEventType::Input(Key::Up) => {
                self.cursor = self.cursor.saturating_sub(1);
            },
            UIEventType::Input(Key::Down) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
            },
            UIEventType::Input(Key::Down) => {
                self.focus = FormFocus::Buttons;
            },
            UIEventType::Input(Key::Char('\n')) if self.focus == FormFocus::Fields => {
                self.focus = FormFocus::TextInput;
                context.replies.push_back(UIEvent {
                   id: 0,
                   event_type: UIEventType::ChangeMode(UIMode::Insert),
                   });
            },
            UIEventType::InsertInput(Key::Right) if self.focus == FormFocus::TextInput => {
                if self.cursors[self.cursor] < self.fields[&self.layout[self.cursor]].len().saturating_sub(1) {
                    self.cursors[self.cursor] += 1;
                }
            },
            UIEventType::InsertInput(Key::Left) if self.focus == FormFocus::TextInput => {
                if self.cursors[self.cursor] == 0 {
                    self.focus = FormFocus::Fields;
                    context.replies.push_back(UIEvent {
                        id: 0,
                        event_type: UIEventType::ChangeMode(UIMode::Normal),
                    });
                } else {
                    self.cursors[self.cursor] = self.cursors[self.cursor] - 1;
                }
            },
            UIEventType::ChangeMode(UIMode::Normal) if self.focus == FormFocus::TextInput => {
                self.focus = FormFocus::Fields;
            },
            UIEventType::InsertInput(Key::Char(k)) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                field.insert(self.cursors[self.cursor], k);
                self.cursors[self.cursor] += 1;
            },
            UIEventType::InsertInput(Key::Backspace) if self.focus == FormFocus::TextInput => {
                let field = self.fields.get_mut(&self.layout[self.cursor]).unwrap();
                match self.cursors[self.cursor] {
                    i if i == 0  => {},
                    i if i == field.len() => {
                        field.pop();
                        self.cursors[self.cursor] -= 1;
                    },
                    _ => {
                        field.remove(self.cursors[self.cursor]);
                        self.cursors[self.cursor] -= 1;
                    }
                }
            },
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
pub struct ButtonWidget<T> where T: std::fmt::Debug + Default + Send{
    buttons: FnvHashMap<String, T>,
    layout: Vec<String>,

    result: Option<T>,
    cursor: usize,
}

impl<T> fmt::Display for ButtonWidget<T> where T: std::fmt::Debug + Default + Send {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("", f)
    }
}

impl<T> ButtonWidget<T> where T: std::fmt::Debug + Default + Send {
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


impl<T> Component for ButtonWidget<T> where T: std::fmt::Debug + Default + Send {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

            let mut len = 0;
        for (i, k) in self.layout.iter().enumerate() {
            let cur_len = k.len();
            let (x, y) = write_string_to_grid(
                k.as_str(),
                grid,
                Color::Default,
                if i == self.cursor { Color::Byte(246) } else { Color::Default },
                (pos_inc(upper_left, (len, 0)), pos_inc(upper_left, (cur_len + len, 0))),
                false,
                );
            len += cur_len + 3;
        }
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        match event.event_type {
            UIEventType::Input(Key::Char('\n')) => {
                self.result = Some(self.buttons.remove(&self.layout[self.cursor]).unwrap_or_default());
                return true;
            },
            UIEventType::Input(Key::Left) => {
                self.cursor = self.cursor.saturating_sub(1);
                return true;
            },
            UIEventType::Input(Key::Right) if self.cursor < self.layout.len().saturating_sub(1) => {
                self.cursor += 1;
                return true;
            },
            _ => {}
        }

        false
    }
    fn is_dirty(&self) -> bool {
        true
    }
    fn set_dirty(&mut self) {}
}


