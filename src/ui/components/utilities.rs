use ui::components::*;
use ui::cells::*;

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
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_rows = get_y(bottom_right) - get_y(upper_left);
        let bottom_entity_height = (self.ratio*total_rows )/100;
        let mid = get_y(upper_left) + total_rows - bottom_entity_height;

        for i in get_x(upper_left)..=get_x(bottom_right) {
            grid[(i, mid)].set_ch('─');
        }
        let _ = self.top.component.draw(grid,
                                       (upper_left, (get_x(bottom_right), get_y(upper_left) + mid-1)),
                                       context);
        let _ = self.bottom.component.draw(grid,
                                           ((get_x(upper_left), get_y(upper_left) + mid), bottom_right),
                                           context);
        grid[bottom_right].set_ch('b');
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        self.top.rcv_event(event, context);
        self.bottom.rcv_event(event, context);
    }
    fn is_dirty(&self) -> bool {
       self.top.component.is_dirty() || self.bottom.component.is_dirty()
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
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
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

        for i in get_y(upper_left)..=get_y(bottom_right) {
            grid[(mid, i)].set_ch(VERT_BOUNDARY);
            grid[(mid, i)].set_fg(Color::Default);
            grid[(mid, i)].set_bg(Color::Default);
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
        let _ = self.left.component.draw(grid,
                                         (upper_left, (mid-1, get_y(bottom_right))),
                                         context);
        let _ = self.right.component.draw(grid,
                                          ((mid+1, get_y(upper_left)), bottom_right),
                                          context);
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        self.left.rcv_event(event, context);
        self.right.rcv_event(event, context);
    }
    fn is_dirty(&self) -> bool {
       self.left.component.is_dirty() || self.right.component.is_dirty()
    }
}

/// A pager for text.
/// `Pager` holds its own content in its own `CellBuffer` and when `draw` is called, it draws the
/// current view of the text. It is responsible for scrolling etc.
pub struct Pager {
    cursor_pos: usize,
    height: usize,
    width: usize,
    dirty: bool,
    content: CellBuffer,
}

impl Pager {
    pub fn new(mail: &Envelope) -> Self {
        let text = mail.get_body().get_text();
        let lines: Vec<&str> = text.trim().split('\n').collect();
        let height = lines.len();
        let width = lines.iter().map(|l| l.len()).max().unwrap_or(0);
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));
        if width > 0 {
            for (i, l) in lines.iter().enumerate() {
                write_string_to_grid(l,
                                     &mut content,
                                     Color::Default,
                                     Color::Default,
                                     ((0, i), (width -1, i)));
            }
        }
        Pager {
            cursor_pos: 0,
            height: height,
            width: width,
            dirty: true,
            content: content,
        }
    }
}

impl Component for Pager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        self.dirty = false;
        if self.height == 0 || self.height == self.cursor_pos {
            return;
        }
        clear_area(grid,
                   (upper_left, bottom_right));
        context.dirty_areas.push_back((upper_left, bottom_right));

        //let pager_context: usize = context.settings.pager.pager_context;
        //let pager_stop: bool = context.settings.pager.pager_stop;
        //let rows = get_y(bottom_right) - get_y(upper_left);
        //let page_length = rows / self.height;
        let mut inner_x = 0;
        let mut inner_y = self.cursor_pos;

        for y in get_y(upper_left)..=get_y(bottom_right) {
            'for_x: for x in get_x(upper_left)..=get_x(bottom_right) {
                if inner_x == self.width {
                    break 'for_x;
                }
                grid[(x,y)] = self.content[(inner_x, inner_y)];
                inner_x += 1;
            }
            inner_y += 1;
            inner_x = 0;
            if inner_y == self.height {
                break;
            }
        }
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &UIEvent, _context: &mut Context) {
        match event.event_type {
            UIEventType::Input(Key::Char('k')) => {
                if self.cursor_pos > 0 {
                    self.cursor_pos -= 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Char('j')) => {
                if self.height > 0 && self.cursor_pos < self.height - 1 {
                    self.cursor_pos += 1;
                    eprintln!("new pager cursor is {}", self.cursor_pos);
                    self.dirty = true;
                }
            },
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            },
            _ => {
            },
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
}

/// Status bar.
pub struct StatusBar {
    container: Entity,
    status: String,
    ex_buffer: String,
    mode: UIMode,
    height: usize,
    dirty: bool,
}

impl StatusBar {
    pub fn new(container: Entity) -> Self {
        StatusBar {
            container: container,
            status: String::with_capacity(256),
            ex_buffer: String::with_capacity(256),
            dirty: true,
            mode: UIMode::Normal,
            height: 1,
        }
    }
    fn draw_status_bar(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area);
        write_string_to_grid(&self.status,
                             grid,
                             Color::Byte(36),
                             Color::Default,
                             area);
        context.dirty_areas.push_back(area);
    }
    fn draw_execute_bar(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area);
        write_string_to_grid(&self.ex_buffer,
                             grid,
                             Color::Byte(124),
                             Color::Default,
                             area);
        context.dirty_areas.push_back(area);
    }
}


impl Component for StatusBar {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let total_rows = get_y(bottom_right) - get_y(upper_left);
        if total_rows <= self.height {
            return;
        }
        let height = self.height;

        let _ = self.container.component.draw(grid,
                                              (upper_left, (get_x(bottom_right), get_y(bottom_right) - height)),
                                              context);
        
        if !self.is_dirty() {
            return;
        }
        self.dirty = false;
        self.draw_status_bar(grid, (set_y(upper_left, get_y(bottom_right)), bottom_right), context);
        match self.mode {
            UIMode::Normal => {
            },
            UIMode::Execute => {
                self.draw_execute_bar(grid,
                                      (set_y(upper_left, get_y(bottom_right) - height + 1), set_y(bottom_right, get_y(bottom_right) - height+1)),
                                      context);
            },

        }
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        self.container.rcv_event(event, context);
        match event.event_type {
            UIEventType::RefreshMailbox(ref m) => {
                self.status = format!("{} |Mailbox: {}, Messages: {}, New: {}", self.mode,  m.folder.get_name(), m.collection.len(), m.collection.iter().filter(|e| !e.is_seen()).count());
                self.dirty = true;

            },
            UIEventType::ChangeMode(m) => {
                let offset = self.status.find('|').unwrap_or(self.status.len());
                self.status.replace_range(..offset, &format!("{} ", m));
                self.dirty = true;
                self.mode = m;
                match m {
                    UIMode::Normal => {
                        self.height = 1;
                        self.ex_buffer.clear()
                    },
                    UIMode::Execute => {
                        self.height = 2;
                    },
                };
            },
            UIEventType::ExInput(Key::Char(c)) => {
                self.dirty = true;
                self.ex_buffer.push(c);
            },
            _ => {},
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.container.component.is_dirty()
    }
}
