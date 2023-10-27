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

use std::cmp;

use melib::{backends::AccountHash, log, text_processing::TextProcessing, Card, CardId, Draft};

use crate::{
    conf, contacts::editor::ContactManager, shortcut, terminal::*, Action::Tab, Component,
    ComponentId, Composer, Context, DataColumns, PageMovement, ScrollContext, ScrollUpdate,
    ShortcutMaps, Shortcuts, StatusEvent, TabAction, ThemeAttribute, UIEvent, UIMode,
};

#[derive(Debug, PartialEq, Eq)]
enum ViewMode {
    List,
    View(ComponentId),
}

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    _hash: AccountHash,
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
    data_columns: DataColumns<4>,
    initialized: bool,
    theme_default: ThemeAttribute,
    highlight_theme: ThemeAttribute,

    id_positions: Vec<CardId>,

    mode: ViewMode,
    dirty: bool,

    sidebar_divider: char,
    sidebar_divider_theme: ThemeAttribute,

    menu_visibility: bool,
    movement: Option<PageMovement>,
    cmd_buf: String,
    view: Option<ContactManager>,
    ratio: usize, // right/(container width) * 100
    id: ComponentId,
}

impl std::fmt::Display for ContactList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "contacts")
    }
}

impl ContactList {
    pub fn new(context: &Context) -> Self {
        let accounts = context
            .accounts
            .iter()
            .enumerate()
            .map(|(i, (h, a))| AccountMenuEntry {
                name: a.name().to_string(),
                _hash: *h,
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
            theme_default: crate::conf::value(context, "theme_default"),
            highlight_theme: crate::conf::value(context, "highlight"),
            initialized: false,
            dirty: true,
            movement: None,
            cmd_buf: String::with_capacity(8),
            view: None,
            ratio: 90,
            sidebar_divider: context.settings.listing.sidebar_divider,
            sidebar_divider_theme: conf::value(context, "mail.sidebar_divider"),
            menu_visibility: true,
            id: ComponentId::default(),
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
            CellBuffer::new_with_context(min_width.0, self.length, None, context);
        /* email column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(min_width.1, self.length, None, context);
        /* url column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(min_width.2, self.length, None, context);
        /* source column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context("external".len(), self.length, None, context);

        let account = &context.accounts[self.account_pos];
        let book = &account.address_book;
        let mut book_values = book.values().collect::<Vec<&Card>>();
        book_values.sort_unstable_by_key(|c| c.name());
        for (idx, c) in book_values.iter().enumerate() {
            self.id_positions.push(*c.id());

            self.data_columns.columns[0].write_string(
                c.name(),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.0, idx)),
                None,
            );

            self.data_columns.columns[1].write_string(
                c.email(),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.1, idx)),
                None,
            );

            self.data_columns.columns[2].write_string(
                c.url(),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.2, idx)),
                None,
            );

            self.data_columns.columns[3].write_string(
                if c.external_resource() {
                    "external"
                } else {
                    "local"
                },
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.3, idx)),
                None,
            );
        }

        if self.length == 0 {
            let message = "Address book is empty.".to_string();
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), self.length, None, context);
            self.data_columns.columns[0].write_string(
                &message,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, 0), (message.len() - 1, 0)),
                None,
            );
        }
    }

    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize) {
        /* Reset previously highlighted line */
        let mut theme = if idx == self.new_cursor_pos {
            self.highlight_theme
        } else {
            self.theme_default
        };
        theme.fg = self.theme_default.fg;
        if !grid.use_color {
            theme.attrs |= Attr::REVERSE;
        }
        grid.change_theme(area, theme);
    }

    fn draw_menu(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        grid.clear_area(area, self.theme_default);
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut y = get_y(upper_left);
        for a in &self.accounts {
            self.print_account(grid, (set_y(upper_left, y), bottom_right), a, context);
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
            log::debug!("BUG: invalid area in print_account");
        }

        let width = width!(area);
        let must_highlight_account: bool = self.account_pos == a.index;
        let account_attrs = if must_highlight_account {
            let mut v = crate::conf::value(context, "mail.sidebar_highlighted");
            if !context.settings.terminal.use_color() {
                v.attrs |= Attr::REVERSE;
            }
            v
        } else {
            crate::conf::value(context, "mail.sidebar_account_name")
        };

        let s = format!(" [{}]", context.accounts[a.index].address_book.len());

        if a.name.grapheme_len() + s.len() > width + 1 {
            /* Print account name */
            let (x, y) = grid.write_string(
                &a.name,
                account_attrs.fg,
                account_attrs.bg,
                account_attrs.attrs,
                area,
                None,
            );
            grid.write_string(
                &s,
                account_attrs.fg,
                account_attrs.bg,
                account_attrs.attrs,
                (
                    pos_dec(
                        (get_x(bottom_right!(area)), get_y(upper_left!(area))),
                        (s.len() - 1, 0),
                    ),
                    bottom_right!(area),
                ),
                None,
            );
            grid.write_string(
                "…",
                account_attrs.fg,
                account_attrs.bg,
                account_attrs.attrs,
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
                grid[(x, y)]
                    .set_fg(account_attrs.fg)
                    .set_bg(account_attrs.bg)
                    .set_attrs(account_attrs.attrs);
            }
        } else {
            /* Print account name */

            let (x, y) = grid.write_string(
                &a.name,
                account_attrs.fg,
                account_attrs.bg,
                account_attrs.attrs,
                area,
                None,
            );
            grid.write_string(
                &s,
                account_attrs.fg,
                account_attrs.bg,
                account_attrs.attrs,
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
                grid[(x, y)]
                    .set_fg(account_attrs.fg)
                    .set_bg(account_attrs.bg)
                    .set_attrs(account_attrs.attrs);
            }
        }
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        /* reserve top row for column headers */
        let upper_left = pos_inc(upper_left!(area), (0, 1));
        let bottom_right = bottom_right!(area);

        if self.length == 0 {
            grid.clear_area(area, self.theme_default);

            grid.copy_area(
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
                    #[allow(clippy::comparison_chain)]
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

        if self.length >= rows {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                    ScrollUpdate::Update {
                        id: self.id,
                        context: ScrollContext {
                            shown_lines: top_idx + rows,
                            total_lines: self.length,
                            has_more_lines: false,
                        },
                    },
                )));
        } else {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                    ScrollUpdate::End(self.id),
                )));
        }

        /* If cursor position has changed, remove the highlight from the previous
         * position and apply it in the new one. */
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
        self.data_columns.widths[1] = self.data_columns.columns[1].size().0; /* email */
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
        grid.clear_area(area, self.theme_default);
        /* Page_no has changed, so draw new page */

        let header_attrs = crate::conf::value(context, "widgets.list.header");
        let mut x = get_x(upper_left);
        for i in 0..self.data_columns.columns.len() {
            if self.data_columns.widths[i] == 0 {
                continue;
            }
            let (column_width, column_height) = self.data_columns.columns[i].size();
            grid.write_string(
                match i {
                    0 => "NAME",
                    1 => "E-MAIL",
                    2 => "URL",
                    3 => "SOURCE",
                    _ => "",
                },
                header_attrs.fg,
                header_attrs.bg,
                header_attrs.attrs,
                (
                    set_x(upper_left!(area), x),
                    (
                        std::cmp::min(get_x(bottom_right), x + (self.data_columns.widths[i])),
                        get_y(upper_left!(area)),
                    ),
                ),
                None,
            );

            grid.copy_area(
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

        grid.change_theme(
            (
                upper_left!(area),
                set_y(bottom_right, get_y(upper_left!(area))),
            ),
            header_attrs,
        );

        if top_idx + rows > self.length {
            grid.clear_area(
                (
                    pos_inc(upper_left, (0, self.length - top_idx + 2)),
                    bottom_right,
                ),
                self.theme_default,
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
            for i in get_y(upper_left)..=get_y(bottom_right) {
                grid[(mid, i)]
                    .set_ch(self.sidebar_divider)
                    .set_fg(self.sidebar_divider_theme.fg)
                    .set_bg(self.sidebar_divider_theme.bg)
                    .set_attrs(self.sidebar_divider_theme.attrs);
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
            self.draw_menu(
                grid,
                (upper_left, (mid.saturating_sub(1), get_y(bottom_right))),
                context,
            );
            self.draw_list(grid, (set_x(upper_left, mid + 1), bottom_right), context);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                self.initialized = false;
                self.sidebar_divider = context.settings.listing.sidebar_divider;
                self.sidebar_divider_theme = conf::value(context, "mail.sidebar_divider");
                self.set_dirty(true);
            }
            UIEvent::AccountStatusChange(_, _) => {
                self.initialized = false;
                self.set_dirty(true);
            }
            UIEvent::ComponentUnrealize(ref kill_id) if self.mode == ViewMode::View(*kill_id) => {
                self.mode = ViewMode::List;
                self.view.take();
                self.set_dirty(true);
                return true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            _ => {}
        }

        if let Some(ref mut v) = self.view {
            if v.process_event(event, context) {
                return true;
            }
        }

        let shortcuts = self.shortcuts(context);
        if self.view.is_none() {
            match *event {
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["create_contact"]) =>
                {
                    let mut manager = ContactManager::new(context);
                    manager.set_parent_id(self.id);
                    manager.account_pos = self.account_pos;

                    self.mode = ViewMode::View(manager.id());
                    self.view = Some(manager);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));

                    return true;
                }

                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["edit_contact"]) =>
                {
                    if self.length == 0 {
                        return true;
                    }
                    let account = &mut context.accounts[self.account_pos];
                    let book = &mut account.address_book;
                    let card = book[&self.id_positions[self.cursor_pos]].clone();
                    let mut manager = ContactManager::new(context);
                    manager.set_parent_id(self.id);
                    manager.card = card;
                    manager.account_pos = self.account_pos;

                    self.mode = ViewMode::View(manager.id());
                    self.view = Some(manager);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));

                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["mail_contact"]) =>
                {
                    if self.length == 0 {
                        return true;
                    }
                    let account = &context.accounts[self.account_pos];
                    let account_hash = account.hash();
                    let book = &account.address_book;
                    let card = &book[&self.id_positions[self.cursor_pos]];
                    let mut draft: Draft = Draft::default();
                    *draft.headers_mut().get_mut("To").unwrap() =
                        format!("{} <{}>", &card.name(), &card.email());
                    let mut composer = Composer::with_account(account_hash, context);
                    composer.set_draft(draft, context);
                    context
                        .replies
                        .push_back(UIEvent::Action(Tab(TabAction::New(Some(Box::new(
                            composer,
                        ))))));

                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["delete_contact"]) =>
                {
                    if self.length == 0 {
                        return true;
                    }
                    // [ref:TODO]: add a confirmation dialog?
                    context.accounts[self.account_pos]
                        .address_book
                        .remove_card(self.id_positions[self.cursor_pos]);
                    self.initialized = false;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["next_account"]) =>
                {
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
                    if self.account_pos + amount < self.accounts.len() {
                        self.account_pos += amount;
                        self.set_dirty(true);
                        self.initialized = false;
                        self.cursor_pos = 0;
                        self.new_cursor_pos = 0;
                        self.length = 0;
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                                self.status(context),
                            )));
                    }

                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["prev_account"]) =>
                {
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
                        self.set_dirty(true);
                        self.cursor_pos = 0;
                        self.new_cursor_pos = 0;
                        self.length = 0;
                        self.initialized = false;
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                                self.status(context),
                            )));
                    }
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(
                        k == shortcuts[Shortcuts::CONTACT_LIST]["toggle_menu_visibility"]
                    ) =>
                {
                    self.menu_visibility = !self.menu_visibility;
                    self.set_dirty(true);
                }
                UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt(''))
                    if !self.cmd_buf.is_empty() =>
                {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                }
                UIEvent::Input(Key::Char(c)) if c.is_ascii_digit() => {
                    self.cmd_buf.push(c);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                            self.cmd_buf.clone(),
                        )));
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["scroll_up"]) =>
                {
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
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["scroll_down"]) =>
                {
                    if self.cursor_pos >= self.length.saturating_sub(1) {
                        return true;
                    }
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
                    self.set_dirty(true);
                    self.movement = Some(PageMovement::Down(amount));
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["prev_page"]) =>
                {
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
                    self.set_dirty(true);
                    self.movement = Some(PageMovement::PageUp(mult));
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["next_page"]) =>
                {
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
                    self.set_dirty(true);
                    self.movement = Some(PageMovement::PageDown(mult));
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["home_page"]) =>
                {
                    self.set_dirty(true);
                    self.movement = Some(PageMovement::Home);
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["end_page"]) =>
                {
                    self.set_dirty(true);
                    self.movement = Some(PageMovement::End);
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.view.as_ref().map(|v| v.is_dirty()).unwrap_or(false)
    }

    fn set_dirty(&mut self, value: bool) {
        if let Some(p) = self.view.as_mut() {
            p.set_dirty(value);
        };
        self.dirty = value;
    }

    fn kill(&mut self, uuid: ComponentId, context: &mut Context) {
        debug_assert!(uuid == self.id);
        context
            .replies
            .push_back(UIEvent::Action(Tab(TabAction::Kill(uuid))));
    }
    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = self
            .view
            .as_ref()
            .map(|p| p.shortcuts(context))
            .unwrap_or_default();

        map.insert(
            Shortcuts::CONTACT_LIST,
            context.settings.shortcuts.contact_list.key_values(),
        );
        map.insert(
            Shortcuts::GENERAL,
            context.settings.shortcuts.general.key_values(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        self.view
            .as_mut()
            .map(|p| p.can_quit_cleanly(context))
            .unwrap_or(true)
    }

    fn status(&self, context: &Context) -> String {
        format!(
            "{} entries",
            context.accounts[self.account_pos].address_book.len()
        )
    }
}
