/*
 * meli
 *
 * Copyright 2019 Manos Pitsidianakis
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
struct AccountMenuEntry {
    name: String,
    // Index in the config account vector.
    index: usize,
}

#[derive(Debug)]
pub struct ContactList {
    accounts: Vec<AccountMenuEntry>,
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    length: usize,
    data_columns: DataColumns,
    initialized: bool,

    id_positions: Vec<CardId>,

    mode: ViewMode,
    dirty: bool,
    show_divider: bool,
    menu_visibility: bool,
    movement: Option<PageMovement>,
    cmd_buf: String,
    view: Option<ContactManager>,
    ratio: usize, // right/(container width) * 100
    id: ComponentId,
}

impl fmt::Display for ContactList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ContactList::DESCRIPTION)
    }
}

impl ContactList {
    const DESCRIPTION: &'static str = "contact list";
    pub fn new(context: &Context) -> Self {
        let accounts = context
            .accounts
            .iter()
            .enumerate()
            .map(|(i, a)| AccountMenuEntry {
                name: a.name().to_string(),
                index: i,
            })
            .collect();
        ContactList {
            accounts,
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
            cmd_buf: String::with_capacity(8),
            view: None,
            ratio: 90,
            show_divider: false,
            menu_visibility: true,
            id: ComponentId::new_v4(),
        }
    }

    pub fn for_account(pos: usize, context: &Context) -> Self {
        ContactList {
            account_pos: pos,
            ..Self::new(context)
        }
    }

    fn initialize(&mut self, context: &mut Context) {
        let account = &context.accounts[self.account_pos];
        let book = &account.address_book;
        self.length = book.len();

        self.id_positions.clear();
        if self.id_positions.capacity() < book.len() {
            self.id_positions.reserve(book.len());
        }
        self.dirty = true;
        let mut min_width = ("Name".len(), "E-mail".len(), 0, "external".len(), 0, 0);

        for c in book.values() {
            /* name */
            min_width.0 = cmp::max(min_width.0, c.name().split_graphemes().len());
            /* email */
            min_width.1 = cmp::max(min_width.1, c.email().split_graphemes().len());
            /* url */
            min_width.2 = cmp::max(min_width.2, c.url().split_graphemes().len());
        }

        /* name column */
        self.data_columns.columns[0] =
            CellBuffer::new_with_context(min_width.0, self.length, Cell::with_char(' '), context);
        /* email column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(min_width.1, self.length, Cell::with_char(' '), context);
        /* url column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(min_width.2, self.length, Cell::with_char(' '), context);
        /* source column */
        self.data_columns.columns[3] = CellBuffer::new_with_context(
            "external".len(),
            self.length,
            Cell::with_char(' '),
            context,
        );

        let account = &context.accounts[self.account_pos];
        let book = &account.address_book;
        let mut book_values = book.values().collect::<Vec<&Card>>();
        book_values.sort_unstable_by_key(|c| c.name());
        for (idx, c) in book_values.iter().enumerate() {
            self.id_positions.push(*c.id());

            write_string_to_grid(
                c.name(),
                &mut self.data_columns.columns[0],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx), (min_width.0, idx)),
                None,
            );

            write_string_to_grid(
                c.email(),
                &mut self.data_columns.columns[1],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx), (min_width.1, idx)),
                None,
            );

            write_string_to_grid(
                c.url(),
                &mut self.data_columns.columns[2],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx), (min_width.2, idx)),
                None,
            );

            write_string_to_grid(
                if c.external_resource() {
                    "external"
                } else {
                    "local"
                },
                &mut self.data_columns.columns[3],
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, idx), (min_width.3, idx)),
                None,
            );
        }

        if self.length == 0 {
            let message = "Address book is empty.".to_string();
            self.data_columns.columns[0] = CellBuffer::new_with_context(
                message.len(),
                self.length,
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
                None,
            );
            return;
        }
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize) {
        /* Reset previously highlighted line */
        let fg_color = Color::Default;
        let bg_color = if idx == self.new_cursor_pos {
            Color::Byte(246)
        } else {
            Color::Default
        };
        change_colors(grid, area, fg_color, bg_color);
    }

    fn draw_menu(&mut self, grid: &mut CellBuffer, mut area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        clear_area(grid, area);
        /* visually divide menu and listing */
        area = (area.0, pos_dec(area.1, (1, 0)));
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut y = get_y(upper_left);
        for a in &self.accounts {
            self.print_account(grid, (set_y(upper_left, y), bottom_right), &a, context);
            y += 1;
        }

        context.dirty_areas.push_back(area);
    }
    /*
     * Print a single account in the menu area.
     */
    fn print_account(
        &self,
        grid: &mut CellBuffer,
        area: Area,
        a: &AccountMenuEntry,
        context: &mut Context,
    ) {
        if !is_valid_area!(area) {
            debug!("BUG: invalid area in print_account");
        }

        let width = width!(area);
        let must_highlight_account: bool = self.account_pos == a.index;
        let (fg_color, bg_color) = if must_highlight_account {
            if self.account_pos == a.index {
                (Color::Byte(233), Color::Byte(15))
            } else {
                (Color::Byte(15), Color::Byte(233))
            }
        } else {
            (Color::Default, Color::Default)
        };

        let s = format!(" [{}]", context.accounts[a.index].address_book.len());

        if a.name.grapheme_len() + s.len() > width + 1 {
            /* Print account name */
            let (x, y) =
                write_string_to_grid(&a.name, grid, fg_color, bg_color, Attr::Bold, area, None);
            write_string_to_grid(
                &s,
                grid,
                fg_color,
                bg_color,
                Attr::Bold,
                (
                    pos_dec(
                        (get_x(bottom_right!(area)), get_y(upper_left!(area))),
                        (s.len() - 1, 0),
                    ),
                    bottom_right!(area),
                ),
                None,
            );
            write_string_to_grid(
                "…",
                grid,
                fg_color,
                bg_color,
                Attr::Bold,
                (
                    pos_dec(
                        (get_x(bottom_right!(area)), get_y(upper_left!(area))),
                        (s.len() - 1, 0),
                    ),
                    bottom_right!(area),
                ),
                None,
            );

            for x in x..=get_x(bottom_right!(area)) {
                grid[(x, y)].set_fg(fg_color);
                grid[(x, y)].set_bg(bg_color);
            }
        } else {
            /* Print account name */

            let (x, y) =
                write_string_to_grid(&a.name, grid, fg_color, bg_color, Attr::Bold, area, None);
            write_string_to_grid(
                &s,
                grid,
                fg_color,
                bg_color,
                Attr::Bold,
                (
                    pos_dec(
                        (get_x(bottom_right!(area)), get_y(upper_left!(area))),
                        (s.len() - 1, 0),
                    ),
                    bottom_right!(area),
                ),
                None,
            );
            for x in x..=get_x(bottom_right!(area)) {
                grid[(x, y)].set_fg(fg_color);
                grid[(x, y)].set_bg(bg_color);
            }
        }
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        /* reserve top row for column headers */
        let upper_left = pos_inc(upper_left!(area), (0, 1));
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
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;

        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::Up(amount) => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(rows * multiplier);
                }
                PageMovement::Down(amount) => {
                    if self.new_cursor_pos + amount < self.length {
                        self.new_cursor_pos += amount;
                    } else {
                        self.new_cursor_pos = self.length - 1;
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    if self.new_cursor_pos + rows * multiplier < self.length {
                        self.new_cursor_pos += rows * multiplier;
                    } else if self.new_cursor_pos + rows * multiplier > self.length {
                        self.new_cursor_pos = self.length - 1;
                    } else {
                        self.new_cursor_pos = (self.length / rows) * rows;
                    }
                }
                PageMovement::Right(_) | PageMovement::Left(_) => {}
                PageMovement::Home => {
                    self.new_cursor_pos = 0;
                }
                PageMovement::End => {
                    self.new_cursor_pos = self.length - 1;
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
                    set_y(upper_left, get_y(upper_left) + (*idx % rows)),
                    set_y(bottom_right, get_y(upper_left) + (*idx % rows)),
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
        self.data_columns.widths[3] = self.data_columns.columns[3].size().0; /* source */

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
            if self.data_columns.widths[i] == 0 {
                continue;
            }
            let (column_width, column_height) = self.data_columns.columns[i].size();
            write_string_to_grid(
                match i {
                    0 => "NAME",
                    1 => "E-MAIL",
                    2 => "URL",
                    3 => "SOURCE",
                    _ => "",
                },
                grid,
                Color::Black,
                Color::White,
                Attr::Bold,
                (
                    set_x(upper_left!(area), x),
                    (
                        std::cmp::min(get_x(bottom_right), x + (self.data_columns.widths[i])),
                        get_y(upper_left!(area)),
                    ),
                ),
                None,
            );
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
            (
                upper_left!(area),
                set_y(bottom_right, get_y(upper_left!(area))),
            ),
            Color::Black,
            Color::White,
        );

        if top_idx + rows > self.length {
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
                set_y(upper_left, get_y(upper_left) + (self.cursor_pos % rows)),
                set_y(bottom_right, get_y(upper_left) + (self.cursor_pos % rows)),
            ),
            self.cursor_pos,
        );
        context.dirty_areas.push_back(area);
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
        if !self.initialized {
            self.initialize(context);
        }

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_cols = get_x(bottom_right) - get_x(upper_left);

        let right_component_width = if self.menu_visibility {
            (self.ratio * total_cols) / 100
        } else {
            total_cols
        };
        let mid = get_x(bottom_right) - right_component_width;
        if self.dirty && mid != get_x(upper_left) {
            if self.show_divider {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    grid[(mid, i)].set_ch(VERT_BOUNDARY);
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
            } else {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
            }
            context
                .dirty_areas
                .push_back(((mid, get_y(upper_left)), (mid, get_y(bottom_right))));
        }

        if right_component_width == total_cols {
            self.draw_list(grid, area, context);
        } else if right_component_width == 0 {
            self.draw_menu(grid, area, context);
        } else {
            self.draw_menu(grid, (upper_left, (mid, get_y(bottom_right))), context);
            self.draw_list(grid, (set_x(upper_left, mid + 1), bottom_right), context);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }
        let shortcuts = &self.get_shortcuts(context)[Self::DESCRIPTION];
        if self.view.is_none() {
            match *event {
                UIEvent::Input(ref key) if key == shortcuts["create_contact"] => {
                    let mut manager = ContactManager::default();
                    manager.set_parent_id(self.id);
                    manager.account_pos = self.account_pos;

                    self.mode = ViewMode::View(manager.id());
                    self.view = Some(manager);

                    return true;
                }

                UIEvent::Input(ref key) if key == shortcuts["edit_contact"] && self.length > 0 => {
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
                UIEvent::Input(ref key) if key == shortcuts["mail_contact"] && self.length > 0 => {
                    let account = &context.accounts[self.account_pos];
                    let book = &account.address_book;
                    let card = &book[&self.id_positions[self.cursor_pos]];
                    let mut draft: Draft = Draft::default();
                    *draft.headers_mut().get_mut("To").unwrap() =
                        format!("{} <{}>", &card.name(), &card.email());
                    context.replies.push_back(UIEvent::Action(Tab(NewDraft(
                        self.account_pos,
                        Some(draft),
                    ))));

                    return true;
                }
                UIEvent::Input(ref key) if key == shortcuts["next_account"] => {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    if self.accounts.is_empty() {
                        return true;
                    }
                    if self.account_pos + amount < self.accounts.len() {
                        self.account_pos += amount;
                        self.set_dirty();
                        self.initialized = false;
                        self.cursor_pos = 0;
                        self.new_cursor_pos = 0;
                        self.length = 0;
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                                self.get_status(context).unwrap(),
                            )));
                    }

                    return true;
                }
                UIEvent::Input(ref key) if key == shortcuts["prev_account"] => {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    if self.accounts.is_empty() {
                        return true;
                    }
                    if self.account_pos >= amount {
                        self.account_pos -= amount;
                        self.set_dirty();
                        self.cursor_pos = 0;
                        self.new_cursor_pos = 0;
                        self.length = 0;
                        self.initialized = false;
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                                self.get_status(context).unwrap(),
                            )));
                    }
                    return true;
                }
                UIEvent::Input(ref k) if k == shortcuts["toggle_menu_visibility"] => {
                    self.menu_visibility = !self.menu_visibility;
                    self.set_dirty();
                }
                UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) => {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                }
                UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                    self.cmd_buf.push(c);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                            self.cmd_buf.clone(),
                        )));
                    return true;
                }
                UIEvent::Input(Key::Up) => {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    self.movement = Some(PageMovement::Up(amount));
                    self.set_dirty();
                    return true;
                }
                UIEvent::Input(Key::Down) if self.cursor_pos < self.length.saturating_sub(1) => {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    self.set_dirty();
                    self.movement = Some(PageMovement::Down(amount));
                    return true;
                }
                UIEvent::Input(Key::PageUp) => {
                    let mult = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        mult
                    } else {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    self.set_dirty();
                    self.movement = Some(PageMovement::PageUp(mult));
                    return true;
                }
                UIEvent::Input(Key::PageDown) => {
                    let mult = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        mult
                    } else {
                        self.cmd_buf.clear();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    self.set_dirty();
                    self.movement = Some(PageMovement::PageDown(mult));
                    return true;
                }
                UIEvent::Input(ref key) if *key == Key::Home => {
                    self.set_dirty();
                    self.movement = Some(PageMovement::Home);
                    return true;
                }
                UIEvent::Input(ref key) if *key == Key::End => {
                    self.set_dirty();
                    self.movement = Some(PageMovement::End);
                    return true;
                }
                _ => {}
            }
        } else {
            match event {
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
        map.insert(Self::DESCRIPTION, config_map);

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

    fn get_status(&self, context: &Context) -> Option<String> {
        Some(format!(
            "{} entries",
            context.accounts[self.account_pos].address_book.len()
        ))
    }
}
