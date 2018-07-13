use ui::components::*;
use ui::cells::*;
///A simple box with borders and no content.
pub struct BoxPanel {
}

impl Component for BoxPanel {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        grid[upper_left].set_ch('u');
        grid[bottom_right].set_ch('b');
        let width = get_x(bottom_right) - get_x(upper_left);
        let height = get_y(bottom_right) - get_y(upper_left);

        grid[upper_left].set_ch('┌');
        grid[(get_x(upper_left), get_y(bottom_right))].set_ch(BOTTOM_LEFT_CORNER);
        grid[(get_x(bottom_right), get_y(upper_left))].set_ch('┐');
        grid[bottom_right].set_ch('┘');
        for i in get_y(upper_left) + 1..get_y(bottom_right) {
            grid[(get_x(upper_left), i)].set_ch('│');
            grid[(get_x(upper_left) + width, i)].set_ch('│');
        }
        for i in get_x(upper_left)+1..get_x(bottom_right) {
            grid[(i, get_y(upper_left))].set_ch('─');
            grid[(i, get_y(upper_left) + height)].set_ch('─');
        }
    }
    fn process_event(&mut self, _event: &UIEvent, _queue: &mut VecDeque<UIEvent>) {
        return;
    }
}

/// A horizontally split in half container.
pub struct HSplit {
    top: Entity,
    bottom: Entity,
    ratio: usize, // bottom/whole height * 100
}

impl HSplit {
    pub fn new(top: Entity, bottom: Entity, ratio: usize) -> Self {
        HSplit {
            top: top,
            bottom: bottom,
            ratio: ratio,
        }
    }
}


impl Component for HSplit {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        let total_rows = get_y(bottom_right) - get_y(upper_left);
        let bottom_entity_height = (self.ratio*total_rows )/100;
        let mid = get_y(upper_left) + total_rows - bottom_entity_height;

        for i in get_x(upper_left)..=get_x(bottom_right) {
            grid[(i, mid)].set_ch('─');
        }
        let _ = self.top.component.draw(grid, upper_left, (get_x(bottom_right), get_y(upper_left) + mid-1));
        let _ = self.bottom.component.draw(grid, (get_x(upper_left), get_y(upper_left) + mid), bottom_right);
        grid[bottom_right].set_ch('b');
    }
    fn process_event(&mut self, event: &UIEvent, queue: &mut VecDeque<UIEvent>) {
        self.top.rcv_event(event, queue);
        self.bottom.rcv_event(event, queue);
    }
}

/// A vertically split in half container.
pub struct VSplit {
    left: Entity,
    right: Entity,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
}

impl VSplit {
    pub fn new(left: Entity, right: Entity, ratio: usize) -> Self {
        VSplit {
            left: left,
            right: right,
            ratio: ratio,
        }
    }
}


impl Component for VSplit {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        let total_cols = get_x(bottom_right) - get_x(upper_left);
        let right_entity_width = (self.ratio*total_cols )/100;
        let mid = get_x(bottom_right) - right_entity_width;

        if get_y(upper_left)> 1 {
            let c = grid.get(mid, get_y(upper_left)-1).map(|a| a.ch()).unwrap_or_else(|| ' ');
            match c {
                HORZ_BOUNDARY => {
                    grid[(mid, get_y(upper_left)-1)].set_ch(LIGHT_DOWN_AND_HORIZONTAL);
                },
                _ => {},
            }
        }

        for i in get_y(upper_left)..get_y(bottom_right) {
            grid[(mid, i)].set_ch(VERT_BOUNDARY);
        }
        if get_y(bottom_right)> 1 {
            let c = grid.get(mid, get_y(bottom_right)-1).map(|a| a.ch()).unwrap_or_else(|| ' ');
            match c {
                HORZ_BOUNDARY => {
                    grid[(mid, get_y(bottom_right)+1)].set_ch(LIGHT_UP_AND_HORIZONTAL);
                },
                _ => {},
            }
        }
        let _ = self.left.component.draw(grid, upper_left, (mid-1, get_y(bottom_right)));
        let _ = self.right.component.draw(grid, (mid+1, get_y(upper_left)), bottom_right);
    }
    fn process_event(&mut self, event: &UIEvent, queue: &mut VecDeque<UIEvent>) {
        self.left.rcv_event(event, queue);
        self.right.rcv_event(event, queue);
    }
}

/// A box with a text content.
pub struct TextBox {
    content: String,
}

impl TextBox {
    pub fn new(s: String) -> Self {
        TextBox {
            content: s,
        }
    }
}

impl Component for TextBox {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        let mut x = get_x(upper_left);
        let y = get_y(upper_left);
        for c in self.content.chars() {
            grid[(x,y)].set_ch(c);
            x += 1;
            if x == get_x(bottom_right) + 1 {
                x = get_x(upper_left);
            }

            if y == get_y(bottom_right) {
                break;
            }
        }
    }
    fn process_event(&mut self, _event: &UIEvent, _queue: &mut VecDeque<UIEvent>) {
        return;
    }
}

/// A pager for text.
/// `Pager` holds its own content in its own `CellBuffer` and when `draw` is called, it draws the
/// current view of the text. It is responsible for scrolling etc.
pub struct Pager {
    cursor_pos: usize,
    rows: usize,
    cols: usize,
    height: usize,
    pub dirty: bool,
    content: CellBuffer,
}

impl Pager {
    pub fn new(mail: &Envelope, height: usize, width: usize) -> Self {
        let text = mail.get_body().get_text();
        let lines: Vec<&str> = text.trim().split('\n').collect();
        let rows = lines.len();
        let mut content = CellBuffer::new(width, rows, Cell::with_char(' '));
        for (i, l) in lines.iter().enumerate() {
            write_string_to_grid(l, &mut content, Color::Default, Color::Default, (0,i), (width-1, rows-1));
        }
        Pager {
            cursor_pos: 0,
            rows: rows,
            height: height,
            cols: width,
            dirty: true,
            content: content,
        }
    }
}

impl Component for Pager {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        if !self.dirty {
            return;
        }
        clear_area(grid, (get_x(upper_left), get_y(upper_left)-1), bottom_right);
        self.dirty = false;
        let mut inner_x = 0;
        let mut inner_y = self.cursor_pos;

        //if self.cursor_pos < self.rows && self.rows - self.cursor_pos > self.height {
            for y in get_y(upper_left)..get_y(bottom_right) {
                'for_x: for x in get_x(upper_left)..get_x(bottom_right) {
                    if inner_x == self.cols {
                        break 'for_x;
                    }
                    grid[(x,y)] = self.content[(inner_x, inner_y)];
                    inner_x += 1;
                }
                inner_y += 1;
                inner_x = 0;
                if inner_y == self.rows {
                    break;
                }
            }
    }
    fn process_event(&mut self, event: &UIEvent, _queue: &mut VecDeque<UIEvent>) {
        match event.event_type {
            UIEventType::Input(Key::Char('k')) => {
                if self.cursor_pos > 0 {
                    self.cursor_pos -= 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Char('j')) => {
                if self.rows > 0 && self.cursor_pos < self.rows - 1 {
                    self.cursor_pos += 1;
                    self.dirty = true;
                }
            },
            _ => {
            },
        }
    }
}

