use super::*;

const MAX_COLS: usize = 500;

#[derive(Debug, PartialEq)]
enum ViewMode {
    List,
    View(Uuid),
}

#[derive(Debug)]
pub struct ContactList {
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    length: usize,
    content: CellBuffer,

    uuid_positions: Vec<Uuid>,
    
    mode: ViewMode,
    initialized: bool,
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
            uuid_positions: Vec::new(),
            mode: ViewMode::List,
            content,
            initialized: false,
            dirty: true,
            view: None,
        }
    }

    fn initialize(&mut self, context: &mut Context) {
        let account = &mut context.accounts[self.account_pos];
        let book = &mut account.address_book;
        self.content.resize(MAX_COLS, book.len(), Cell::with_char(' '));
        eprintln!("{:?}", book);

        self.uuid_positions.clear();
        if self.uuid_positions.capacity() < book.len() {
            self.uuid_positions.reserve(book.len()); 
        }

        for (i, c) in book.values().enumerate() {
            self.uuid_positions.push(*c.uuid());
            
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
        if !self.initialized {
            self.initialize(context);
            self.initialized = true;
        }

        if let Some(mgr) = self.view.as_mut() {
            mgr.draw(grid, area, context);
            self.dirty = false;
            return;
        }

        if self.dirty {
            clear_area(grid, area);
            copy_area(grid, &self.content, area, ((0, 0), (MAX_COLS - 1, self.content.size().1 - 1)));
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }
        match event.event_type {
            UIEventType::Input(Key::Char('e')) => {
                let account = &mut context.accounts[self.account_pos];
                let book = &mut account.address_book;
                let card = book[&self.uuid_positions[self.cursor_pos]].clone();
                let mut manager = ContactManager::default();
                manager.card = card;



                let entity = Entity::from(Box::new(manager));

                self.mode = ViewMode::View(*entity.uuid());
                self.view = Some(entity);
                self.set_dirty();
                
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
        self.dirty
    }
    fn set_dirty(&mut self) {
        if let Some(p) = self.view.as_mut() {
            p.set_dirty();
        };
        self.dirty = true;
    }
}
