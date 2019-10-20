use super::*;

use melib::CardId;
use std::cmp;

const MAX_COLS: usize = 500;

#[derive(Debug, PartialEq)]
enum ViewMode {
    List,
    View(ComponentId),
}

#[derive(Debug)]
pub struct ContactList {
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    length: usize,
    data_columns: DataColumns,
    initialized: bool,

    id_positions: Vec<CardId>,

    mode: ViewMode,
    dirty: bool,
    movement: Option<PageMovement>,
    view: Option<ContactManager>,
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
        ContactList {
            cursor_pos: 0,
            new_cursor_pos: 0,
            length: 0,
            account_pos: 0,
            id_positions: Vec::new(),
            mode: ViewMode::List,
            data_columns: DataColumns::default(),
            initialized: false,
            dirty: true,
            movement: None,
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

        self.id_positions.clear();
        if self.id_positions.capacity() < book.len() {
            self.id_positions.reserve(book.len());
        }
        self.dirty = true;
        let mut min_width = ("Name".len(), "E-mail".len(), 0, 0, 0);

        for c in book.values() {
            self.id_positions.push(*c.id());
            min_width.0 = cmp::max(min_width.0, c.name().split_graphemes().len()); /* name */
            min_width.1 = cmp::max(min_width.1, c.email().split_graphemes().len()); /* email */
            min_width.2 = cmp::max(min_width.2, c.url().split_graphemes().len());
            /* url */
        }

        /* name column */
        self.data_columns.columns[0] = CellBuffer::new_with_context(
            min_width.0,
            self.length + 1,
            Cell::with_char(' '),
            context,
        );
        /* email column */
        self.data_columns.columns[1] = CellBuffer::new_with_context(
            min_width.1,
            self.length + 1,
            Cell::with_char(' '),
            context,
        );
        /* url column */
        self.data_columns.columns[2] = CellBuffer::new_with_context(
            min_width.2,
            self.length + 1,
            Cell::with_char(' '),
            context,
        );
        let (x, _) = write_string_to_grid(
            "NAME",
            &mut self.data_columns.columns[0],
            Color::Black,
            Color::White,
            Attr::Bold,
            ((0, 0), (MAX_COLS - 1, self.length)),
            false,
        );
        write_string_to_grid(
            "E-MAIL",
            &mut self.data_columns.columns[1],
            Color::Black,
            Color::White,
            Attr::Bold,
            ((0, 0), (MAX_COLS - 1, self.length)),
            false,
        );

        write_string_to_grid(
            "URL",
            &mut self.data_columns.columns[2],
            Color::Black,
            Color::White,
            Attr::Bold,
            ((0, 0), (MAX_COLS - 1, self.length)),
            false,
        );

        let account = &mut context.accounts[self.account_pos];
        let book = &mut account.address_book;
        for (idx, c) in book.values().enumerate() {
            self.id_positions.push(*c.id());

            let (x, _) = write_string_to_grid(
                c.name(),
                &mut self.data_columns.columns[0],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx + 1), (min_width.0, idx + 1)),
                false,
            );

            let (x, _) = write_string_to_grid(
                c.email(),
                &mut self.data_columns.columns[1],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx + 1), (min_width.1, idx + 1)),
                false,
            );

            let (x, _) = write_string_to_grid(
                c.url(),
                &mut self.data_columns.columns[2],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx + 1), (min_width.2, idx + 1)),
                false,
            );
        }

        if self.length == 0 {
            let message = "Address book is empty.".to_string();
            self.data_columns.columns[0] = CellBuffer::new_with_context(
                message.len(),
                self.length + 1,
                Cell::with_char(' '),
                context,
            );
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                false,
            );
            return;
        }
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        /* Reset previously highlighted line */
        let fg_color = Color::Default;
        let bg_color = if idx == self.new_cursor_pos {
            Color::Byte(246)
        } else {
            Color::Default
        };
        change_colors(grid, area, fg_color, bg_color);
    }
}

impl Component for ContactList {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let Some(mgr) = self.view.as_mut() {
            mgr.draw(grid, area, context);
            return;
        }

        if !self.dirty {
            return;
        }
        self.dirty = false;
        if !self.initialized {
            self.initialize(context);
        }

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        if self.length == 0 {
            clear_area(grid, area);
            copy_area(
                grid,
                &self.data_columns.columns[0],
                area,
                ((0, 0), pos_dec(self.data_columns.columns[0].size(), (1, 1))),
            );
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left);

        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::PageUp => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(rows);
                }
                PageMovement::PageDown => {
                    if self.new_cursor_pos + rows + 1 < self.length {
                        self.new_cursor_pos += rows;
                    } else if self.new_cursor_pos + rows > self.length {
                        self.new_cursor_pos = self.length - 1;
                    } else {
                        self.new_cursor_pos = (self.length / rows) * rows;
                    }
                }
                PageMovement::Home => {
                    self.new_cursor_pos = 0;
                }
                PageMovement::End => {
                    if self.new_cursor_pos + rows > self.length {
                        self.new_cursor_pos = self.length - 1;
                    } else {
                        self.new_cursor_pos = (self.length / rows) * rows;
                    }
                }
            }
        }

        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);

        let top_idx = page_no * rows;

        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if self.cursor_pos != self.new_cursor_pos && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for idx in &[old_cursor_pos, self.new_cursor_pos] {
                if *idx >= self.length {
                    continue; //bounds check
                }
                let new_area = (
                    set_y(upper_left, get_y(upper_left) + (*idx % rows) + 1),
                    set_y(bottom_right, get_y(upper_left) + (*idx % rows) + 1),
                );
                self.highlight_line(grid, new_area, *idx);
                context.dirty_areas.push_back(new_area);
            }
            return;
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }
        if self.new_cursor_pos >= self.length {
            self.new_cursor_pos = self.length - 1;
            self.cursor_pos = self.new_cursor_pos;
        }

        let width = width!(area);
        self.data_columns.widths = Default::default();
        self.data_columns.widths[0] = self.data_columns.columns[0].size().0; /* name */
        self.data_columns.widths[1] = self.data_columns.columns[1].size().0; /* email*/
        self.data_columns.widths[2] = self.data_columns.columns[2].size().0; /* url */

        let min_col_width = std::cmp::min(
            15,
            std::cmp::min(self.data_columns.widths[0], self.data_columns.widths[1]),
        );
        if self.data_columns.widths[0] + self.data_columns.widths[1] + 3 * min_col_width + 8 > width
        {
            let remainder =
                width.saturating_sub(self.data_columns.widths[0] + self.data_columns.widths[1] + 4);
            self.data_columns.widths[2] = remainder / 6;
        }
        clear_area(grid, area);
        /* Page_no has changed, so draw new page */
        let mut x = get_x(upper_left);
        for i in 0..self.data_columns.columns.len() {
            let (column_width, column_height) = self.data_columns.columns[i].size();
            if self.data_columns.widths[i] == 0 {
                continue;
            }
            copy_area(
                grid,
                &self.data_columns.columns[i],
                (
                    set_x(upper_left, x),
                    set_x(
                        bottom_right,
                        std::cmp::min(get_x(bottom_right), x + (self.data_columns.widths[i])),
                    ),
                ),
                (
                    (0, top_idx),
                    (
                        column_width.saturating_sub(1),
                        column_height.saturating_sub(1),
                    ),
                ),
            );
            x += self.data_columns.widths[i] + 2; // + SEPARATOR
            if x > get_x(bottom_right) {
                break;
            }
        }

        change_colors(
            grid,
            (upper_left, set_y(bottom_right, get_y(upper_left))),
            Color::Black,
            Color::White,
        );

        if top_idx + rows + 1 > self.length {
            clear_area(
                grid,
                (
                    pos_inc(upper_left, (0, self.length - top_idx + 2)),
                    bottom_right,
                ),
            );
        }
        self.highlight_line(
            grid,
            (
                set_y(upper_left, get_y(upper_left) + (self.cursor_pos % rows) + 1),
                set_y(
                    bottom_right,
                    get_y(upper_left) + (self.cursor_pos % rows) + 1,
                ),
            ),
            self.cursor_pos,
        );
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }
        let shortcuts = &self.get_shortcuts(context)[Self::DESCRIPTION];
        match *event {
            UIEvent::Input(ref key)
                if *key == shortcuts["create_contact"] && self.view.is_none() =>
            {
                let mut manager = ContactManager::default();
                manager.set_parent_id(self.id);
                manager.account_pos = self.account_pos;

                self.mode = ViewMode::View(manager.id());
                self.view = Some(manager);

                return true;
            }

            UIEvent::Input(ref key)
                if *key == shortcuts["edit_contact"] && self.length > 0 && self.view.is_none() =>
            {
                let account = &mut context.accounts[self.account_pos];
                let book = &mut account.address_book;
                let card = book[&self.id_positions[self.cursor_pos]].clone();
                let mut manager = ContactManager::default();
                manager.set_parent_id(self.id);
                manager.card = card;
                manager.account_pos = self.account_pos;

                self.mode = ViewMode::View(manager.id());
                self.view = Some(manager);

                return true;
            }
            UIEvent::Input(Key::Char('n')) if self.view.is_none() => {
                let card = Card::new();
                let mut manager = ContactManager::default();
                manager.set_parent_id(self.id);
                manager.card = card;
                manager.account_pos = self.account_pos;

                self.mode = ViewMode::View(manager.id());
                self.view = Some(manager);

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
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.set_dirty();
            }
            UIEvent::Resize => {
                self.set_dirty();
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
            config_map
                .into_iter()
                .map(|(k, v)| (k, v.clone()))
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

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        self.view
            .as_mut()
            .map(|p| p.can_quit_cleanly(context))
            .unwrap_or(true)
    }
}
