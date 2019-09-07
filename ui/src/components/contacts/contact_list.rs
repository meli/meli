use super::*;

use melib::CardId;

const MAX_COLS: usize = 500;

#[derive(Debug, PartialEq)]
enum ViewMode {
    List,
    View(CardId),
}

#[derive(Debug)]
pub struct ContactList {
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    length: usize,
    content: CellBuffer,

    id_positions: Vec<CardId>,

    mode: ViewMode,
    dirty: bool,
    view: Option<Box<Component>>,
    id: ComponentId,
}

impl Default for ContactList {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ContactList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ContactList::DESCRIPTION)
    }
}

impl ContactList {
    const DESCRIPTION: &'static str = "contact list";
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
            id: ComponentId::new_v4(),
        }
    }

    pub fn for_account(pos: usize) -> Self {
        ContactList {
            account_pos: pos,
            ..Self::new()
        }
    }

    fn initialize(&mut self, context: &mut Context) {
        let account = &mut context.accounts[self.account_pos];
        let book = &mut account.address_book;
        self.length = book.len();
        self.content
            .resize(MAX_COLS, book.len() + 1, Cell::with_char(' '));

        clear_area(&mut self.content, ((0, 0), (MAX_COLS - 1, self.length)));

        self.id_positions.clear();
        if self.id_positions.capacity() < book.len() {
            self.id_positions.reserve(book.len());
        }
        let mut maxima = ("Name".len(), "E-mail".len());

        for c in book.values() {
            self.id_positions.push(*c.id());
            maxima.0 = std::cmp::max(maxima.0, c.name().split_graphemes().len());
            maxima.1 = std::cmp::max(maxima.1, c.email().split_graphemes().len());
        }
        maxima.0 += 5;
        maxima.1 += maxima.0 + 5;
        let (x, _) = write_string_to_grid(
            "NAME",
            &mut self.content,
            Color::Black,
            Color::White,
            Attr::Default,
            ((0, 0), (MAX_COLS - 1, self.length)),
            false,
        );
        for x in x..maxima.0 {
            self.content[(x, 0)].set_bg(Color::White);
        }
        write_string_to_grid(
            "E-MAIL",
            &mut self.content,
            Color::Black,
            Color::White,
            Attr::Default,
            ((maxima.0, 0), (MAX_COLS - 1, self.length)),
            false,
        );
        for x in x..maxima.1 {
            self.content[(x, 0)].set_bg(Color::White);
        }
        write_string_to_grid(
            "URL",
            &mut self.content,
            Color::Black,
            Color::White,
            Attr::Default,
            ((maxima.1, 0), (MAX_COLS - 1, self.length)),
            false,
        );
        for x in x..(MAX_COLS - 1) {
            self.content[(x, 0)].set_bg(Color::White);
        }
        for (i, c) in book.values().enumerate() {
            self.id_positions.push(*c.id());

            write_string_to_grid(
                c.name(),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, i + 1), (MAX_COLS - 1, self.length)),
                false,
            );
            write_string_to_grid(
                c.email(),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((maxima.0, i + 1), (MAX_COLS - 1, self.length)),
                false,
            );
            write_string_to_grid(
                c.url(),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((maxima.1, i + 1), (MAX_COLS - 1, self.length)),
                false,
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
            copy_area(
                grid,
                &self.content,
                area,
                (
                    (0, 0),
                    (MAX_COLS - 1, self.content.size().1.saturating_sub(1)),
                ),
            );
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        /* Reset previously highlighted line */
        let fg_color = Color::Default;
        let bg_color = Color::Default;
        change_colors(
            grid,
            (
                pos_inc(upper_left, (0, self.cursor_pos + 1)),
                set_y(bottom_right, get_y(upper_left) + self.cursor_pos + 1),
            ),
            fg_color,
            bg_color,
        );

        /* Highlight current line */
        let bg_color = Color::Byte(246);
        change_colors(
            grid,
            (
                pos_inc(upper_left, (0, self.new_cursor_pos + 1)),
                set_y(bottom_right, get_y(upper_left) + self.new_cursor_pos + 1),
            ),
            fg_color,
            bg_color,
        );
        self.cursor_pos = self.new_cursor_pos;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }
        let shortcuts = &self.get_shortcuts(context)[Self::DESCRIPTION];
        match *event {
            UIEvent::Input(ref key) if *key == shortcuts["create_contact"] => {
                let mut manager = ContactManager::default();
                manager.account_pos = self.account_pos;
                let component = Box::new(manager);

                self.mode = ViewMode::View(component.id());
                self.view = Some(component);

                return true;
            }

            UIEvent::Input(ref key) if *key == shortcuts["edit_contact"] && self.length > 0 => {
                let account = &mut context.accounts[self.account_pos];
                let book = &mut account.address_book;
                let card = book[&self.id_positions[self.cursor_pos]].clone();
                let mut manager = ContactManager::default();
                manager.card = card;
                manager.account_pos = self.account_pos;
                let component = Box::new(manager);

                self.mode = ViewMode::View(component.id());
                self.view = Some(component);

                return true;
            }
            UIEvent::Input(Key::Char('n')) => {
                let card = Card::new();
                let mut manager = ContactManager::default();
                manager.card = card;
                manager.account_pos = self.account_pos;
                let component = Box::new(manager);
                self.mode = ViewMode::View(component.id());
                self.view = Some(component);

                return true;
            }
            UIEvent::Input(Key::Up) => {
                self.set_dirty();
                self.new_cursor_pos = self.cursor_pos.saturating_sub(1);
                return true;
            }
            UIEvent::Input(Key::Down) if self.cursor_pos < self.length.saturating_sub(1) => {
                self.set_dirty();
                self.new_cursor_pos += 1;
                return true;
            }
            UIEvent::ComponentKill(ref kill_id) if self.mode == ViewMode::View(*kill_id) => {
                self.mode = ViewMode::List;
                self.view.take();
                self.set_dirty();
                return true;
            }
            _ => {}
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

    fn kill(&mut self, uuid: Uuid, context: &mut Context) {
        debug_assert!(uuid == self.id);
        context.replies.push_back(UIEvent::Action(Tab(Kill(uuid))));
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = self
            .view
            .as_ref()
            .map(|p| p.get_shortcuts(context))
            .unwrap_or_default();

        let config_map = context.settings.shortcuts.contact_list.key_values();
        map.insert(
            self.to_string(),
            [
                ("create_contact", (*config_map["create_contact"]).clone()),
                ("edit_contact", (*config_map["edit_contact"]).clone()),
            ]
            .iter()
            .cloned()
            .collect(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
