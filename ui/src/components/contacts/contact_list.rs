use super::*;

const MAX_COLS: usize = 500;

#[derive(Debug, PartialEq)]
enum ViewMode {
    List,
    View(EntityId),
}

#[derive(Debug)]
pub struct ContactList {
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    length: usize,
    content: CellBuffer,

    id_positions: Vec<EntityId>,
    
    mode: ViewMode,
    dirty: bool,
    view: Option<Entity>,
}

impl Default for ContactList {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ContactList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "contacts")
    }
}

impl ContactList {
    pub fn new() -> Self {
        let content = CellBuffer::new(0, 0, Cell::with_char(' '));
        ContactList {
            cursor_pos: 0,
            new_cursor_pos: 0,
            length: 0,
            account_pos: 0,
            id_positions: Vec::new(),
            mode: ViewMode::List,
            content,
            dirty: true,
            view: None,
        }
    }

    fn initialize(&mut self, context: &mut Context) {
        let account = &mut context.accounts[self.account_pos];
        let book = &mut account.address_book;
        self.length = book.len();
        self.content.resize(MAX_COLS, book.len(), Cell::with_char(' '));

        self.id_positions.clear();
        if self.id_positions.capacity() < book.len() {
            self.id_positions.reserve(book.len()); 
        }

        for (i, c) in book.values().enumerate() {
            self.id_positions.push(*c.id());
            
            write_string_to_grid(
                c.email(),
                &mut self.content,
                Color::Default,
                Color::Default,
                ((0, i), (MAX_COLS - 1, book.len() - 1)),
                false
                );
        }
    }
}

impl Component for ContactList {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let Some(mgr) = self.view.as_mut() {
            mgr.draw(grid, area, context);
            return;
        }

        if self.dirty {
            self.initialize(context);
            clear_area(grid, area);
            copy_area(grid, &self.content, area, ((0, 0), (MAX_COLS - 1, self.content.size().1.saturating_sub(1))));
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        /* Reset previously highlighted line */
        let fg_color = Color::Default;
        let bg_color = Color::Default;
        change_colors(grid, (pos_inc(upper_left, (0, self.cursor_pos)), set_y(bottom_right, get_y(upper_left) + self.cursor_pos)), fg_color, bg_color);

        /* Highlight current line */
        let bg_color = Color::Byte(246);
        change_colors(grid, (pos_inc(upper_left, (0, self.new_cursor_pos)), set_y(bottom_right, get_y(upper_left) + self.new_cursor_pos)), fg_color, bg_color);
        self.cursor_pos = self.new_cursor_pos;
    }

    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }
        match event.event_type {
            UIEventType::Input(Key::Char('e')) if self.length > 0 => {
                let account = &mut context.accounts[self.account_pos];
                let book = &mut account.address_book;
                let card = book[&self.id_positions[self.cursor_pos]].clone();
                let mut manager = ContactManager::default();
                manager.card = card;
                manager.account_pos = self.account_pos;
                let entity = Entity::from(Box::new(manager));

                self.mode = ViewMode::View(*entity.id());
                self.view = Some(entity);
                
                return true;
            },
            UIEventType::Input(Key::Char('n')) => {
                let card = Card::new();
                let mut manager = ContactManager::default();
                manager.card = card;
                manager.account_pos = self.account_pos;
                let entity = Entity::from(Box::new(manager));
                self.mode = ViewMode::View(*entity.id());
                self.view = Some(entity);
                
                return true;
            },
            UIEventType::Input(Key::Up) => {
                self.set_dirty();
                self.new_cursor_pos = self.cursor_pos.saturating_sub(1);
                return true;
            },
            UIEventType::Input(Key::Down) if self.cursor_pos < self.length.saturating_sub(1) => {
                self.set_dirty();
                self.new_cursor_pos += 1;
                return true;
            },
            UIEventType::EntityKill(ref kill_id) if self.mode == ViewMode::View(*kill_id) => {
                self.mode = ViewMode::List;
                self.view.take();
                self.set_dirty();
                return true;

            },
            _ => {},
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.view.as_ref().map(|v| v.is_dirty()).unwrap_or(false)
    }

    fn set_dirty(&mut self) {
        if let Some(p) = self.view.as_mut() {
            p.set_dirty();
        };
        self.dirty = true;
    }
}
