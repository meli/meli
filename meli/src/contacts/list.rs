//
// meli
//
// Copyright 2019, 2025, 2026 Manos Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

//! Contacts and address book components
//!
//! This module is split into three main structures:
//!
//! - [`AddressBookList`]: a [`Component`] that
//!   displays an [`AddressBook`] as a list using
//!   [`DataColumns`]
//! - [`AccountContacts`]: a [`Component`] that holds all [`AddressBookList`] of
//!   an account and shows one at a time, using a cursor as an index.
//! - [`ContactList`]: a [`Component`] that holds one or more accounts as
//!   [`AccountContacts`] and shows one at a time using a cursor as an index.
//!   Furthmore, it has a sidebar showing all entries.

use std::sync::Arc;

use indexmap::IndexMap;
use melib::{
    contacts::{AddressBook, AddressBookName, Card, CardId},
    email::compose::Draft,
    error::Result,
    text::TextProcessing,
    AccountHash, ContactBackendID,
};

use crate::{
    account_settings,
    components::prelude::*,
    contacts::editor::ContactManager,
    mail::compose::Composer,
    utilities::listings::{AccountEntryTrait, List},
};

#[derive(Debug)]
pub struct AddressBookList {
    account_hash: AccountHash,
    backend_id: (Arc<ContactBackendID>, AddressBookName),
    read_only: bool,
    editor: Option<Box<ContactManager>>,
    cursor_pos: usize,
    new_cursor_pos: usize,
    id_positions: Vec<CardId>,
    data_columns: DataColumns<3>,
    initialized: bool,
    movement: Option<PageMovement>,
    theme_default: ThemeAttribute,
    highlight_theme: ThemeAttribute,
    dirty: bool,
    id: ComponentId,
}

impl AddressBookList {
    fn new(
        backend_id: (Arc<ContactBackendID>, AddressBookName),
        book: &AddressBook,
        account_hash: AccountHash,
        context: &Context,
    ) -> Self {
        let theme_default = crate::conf::value(context, "theme_default");
        let highlight_theme = crate::conf::value(context, "highlight");
        let data_columns = DataColumns::new(theme_default);

        Self {
            account_hash,
            backend_id,
            read_only: book.read_only,
            editor: None,
            cursor_pos: 0,
            new_cursor_pos: 0,
            id_positions: book.cards.keys().cloned().collect(),
            data_columns,
            initialized: false,
            movement: None,
            theme_default,
            highlight_theme,
            dirty: true,
            id: ComponentId::default(),
        }
    }

    fn initialize(&mut self, context: &Context) {
        self.data_columns.clear();
        let account = &context.accounts[&self.account_hash];
        let contacts: &AddressBook = &account.contacts.books[&self.backend_id];
        if contacts.cards.is_empty() {
            let message = "Address book is empty.".to_string();
            if self.data_columns.columns[0].resize_with_context(message.len(), 1, context) {
                let area = self.data_columns.columns[0].area();
                self.data_columns.columns[0].grid_mut().write_string(
                    &message,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                );
            }
            return;
        }

        self.id_positions.clear();
        if self.id_positions.capacity() < contacts.len() {
            self.id_positions.reserve(contacts.len());
        }
        self.dirty = true;
        let mut min_width = ("Name".len(), "E-mail".len(), 0);

        for c in contacts.values() {
            // name
            let name = c.name().grapheme_len();
            if name > 0 {
                min_width.0 = min_width.0.max(name + 1);
            }
            // email
            let email = c.email().grapheme_len();
            if email > 0 {
                min_width.1 = min_width.1.max(email + 1);
            }
            // url
            let url = c.url().grapheme_len();
            if url > 0 {
                min_width.2 = min_width.2.max(url + 1);
            }
        }

        // name column
        _ = self.data_columns.columns[0].resize_with_context(min_width.0, contacts.len(), context);
        // email column
        _ = self.data_columns.columns[1].resize_with_context(min_width.1, contacts.len(), context);
        // url column
        _ = self.data_columns.columns[2].resize_with_context(min_width.2, contacts.len(), context);

        let mut book_values = contacts.values().collect::<Vec<&Card>>();
        book_values.sort_unstable_by_key(|c| c.name());
        for (idx, c) in book_values.iter().enumerate() {
            self.id_positions.push(*c.id());

            {
                let area = self.data_columns.columns[0].area().nth_row(idx);
                self.data_columns.columns[0].grid_mut().write_string(
                    c.name(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                )
            };

            {
                let area = self.data_columns.columns[1].area().nth_row(idx);
                self.data_columns.columns[1].grid_mut().write_string(
                    c.email(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                )
            };

            {
                let area = self.data_columns.columns[2].area().nth_row(idx);
                self.data_columns.columns[2].grid_mut().write_string(
                    c.url(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                )
            };
        }
    }

    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize) {
        // Reset previously highlighted line
        let mut theme = if idx == self.new_cursor_pos {
            self.highlight_theme
        } else {
            self.theme_default
        };
        if !grid.use_color {
            theme.attrs |= Attr::REVERSE;
        }
        grid.change_theme(area, theme);
    }
}

impl std::fmt::Display for AddressBookList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "contacts")
    }
}

impl Component for AddressBookList {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        if !self.initialized {
            self.initialized = true;
            self.initialize(context);
        }
        if let Some(ref mut editor) = self.editor {
            return editor.draw(grid, area, context);
        }

        let total_area = area;
        // reserve top row for address book info
        let info_area = area.nth_row(0);
        // reserve top table row for column headers
        let header_area = area.nth_row(1);
        let area = area.skip_rows(2);
        let rows = area.height();

        grid.clear_area(info_area, self.theme_default);
        grid.clear_area(header_area, self.theme_default);
        grid.write_string(
            &format!(
                "{name} [{backend} {format}]",
                name = self.backend_id.1,
                backend = self.backend_id.0.name,
                format = self.backend_id.0.format
            ),
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs,
            info_area,
            None,
            None,
        );
        context.dirty_areas.push_back(info_area);
        if self.id_positions.is_empty() {
            grid.clear_area(area, self.theme_default);

            grid.copy_area(
                self.data_columns.columns[0].grid(),
                area,
                self.data_columns.columns[0].area(),
            );
            context.dirty_areas.push_back(total_area);
            self.set_dirty(false);
            return;
        }
        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::Up(amount) => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(rows * multiplier);
                }
                PageMovement::Down(amount) => {
                    if self.new_cursor_pos + amount < self.id_positions.len() {
                        self.new_cursor_pos += amount;
                    } else {
                        self.new_cursor_pos = self.id_positions.len() - 1;
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    #[allow(clippy::comparison_chain)]
                    if self.new_cursor_pos + rows * multiplier < self.id_positions.len() {
                        self.new_cursor_pos += rows * multiplier;
                    } else if self.new_cursor_pos + rows * multiplier > self.id_positions.len() {
                        self.new_cursor_pos = self.id_positions.len() - 1;
                    } else {
                        self.new_cursor_pos = (self.id_positions.len() / rows) * rows;
                    }
                }
                PageMovement::Right(_) | PageMovement::Left(_) => {}
                PageMovement::Home => {
                    self.new_cursor_pos = 0;
                }
                PageMovement::End => {
                    self.new_cursor_pos = self.id_positions.len() - 1;
                }
            }
        }

        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);

        let top_idx = page_no * rows;

        if self.id_positions.len() >= rows {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                    ScrollUpdate::Update {
                        id: self.id,
                        context: ScrollContext {
                            shown_lines: (top_idx + rows).min(self.id_positions.len() - top_idx),
                            total_lines: self.id_positions.len(),
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

        // If cursor position has changed, remove the highlight from the previous
        // position and apply it in the new one.
        if self.cursor_pos != self.new_cursor_pos && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for idx in &[old_cursor_pos, self.new_cursor_pos] {
                if *idx >= self.id_positions.len() {
                    continue;
                }
                let new_area = area.nth_row(*idx % rows);
                self.highlight_line(grid, new_area, *idx);
                context.dirty_areas.push_back(new_area);
            }
            return;
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }
        if self.new_cursor_pos >= self.id_positions.len() {
            self.new_cursor_pos = self.id_positions.len().saturating_sub(1);
            self.cursor_pos = self.new_cursor_pos;
        }

        // Page_no has changed, so draw new page
        grid.clear_area(area, self.theme_default);
        _ = self.data_columns.recalc_widths(area.size(), top_idx);
        // copy table columns
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos, grid.bounds_iter(area));

        let header_attrs = crate::conf::value(context, "widgets.list.header");
        let mut x = 0;
        for i in 0..self.data_columns.columns.len() {
            if self.data_columns.widths[i] == 0 {
                continue;
            }
            grid.write_string(
                match i {
                    0 => "NAME",
                    1 => "E-MAIL",
                    2 => "URL",
                    _ => "",
                },
                header_attrs.fg,
                header_attrs.bg,
                header_attrs.attrs,
                header_area
                    .skip_cols(x)
                    .take_cols(x + (self.data_columns.widths[i])),
                None,
                None,
            );

            x += self.data_columns.widths[i] + 2; // + SEPARATOR
            if x > header_area.width() {
                break;
            }
        }

        grid.change_theme(header_area, header_attrs);
        context.dirty_areas.push_back(header_area);

        if top_idx + rows > self.id_positions.len() {
            grid.clear_area(
                area.skip_rows(top_idx + rows - self.id_positions.len().saturating_sub(1)),
                self.theme_default,
            );
        }
        self.highlight_line(grid, area.nth_row(self.cursor_pos % rows), self.cursor_pos);
        context.dirty_areas.push_back(total_area);

        self.set_dirty(false);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut editor) = self.editor {
            if matches!(event, UIEvent::ComponentUnrealize(id) if *id == editor.id()) {
                editor.unrealize(context);
                self.initialized = false;
                self.editor = None;
                self.set_dirty(true);
                return true;
            }
            if editor.process_event(event, context) {
                return true;
            }
        }

        match event {
            UIEvent::ChangeMode(UIMode::Normal)
            | UIEvent::Resize
            | UIEvent::ConfigReload { old_settings: _ }
            | UIEvent::VisibilityChange(_) => {
                self.set_dirty(true);
                return false;
            }
            _ => {}
        }
        let shortcuts = self.shortcuts(context);
        match *event {
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["refresh"]) =>
            {
                self.initialized = false;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["create_contact"]) =>
            {
                if self.read_only {
                    return true;
                }
                let mut editor = Box::new(ContactManager::new(
                    self.account_hash,
                    self.backend_id.clone(),
                    context,
                ));
                editor.set_parent_id(self.id);

                self.editor = Some(editor);
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
                if self.id_positions.is_empty() {
                    return true;
                }

                let mut editor = Box::new(ContactManager::new(
                    self.account_hash,
                    self.backend_id.clone(),
                    context,
                ));
                let card = context.accounts[&self.account_hash].contacts.books[&self.backend_id]
                    [&self.id_positions[self.cursor_pos]]
                    .clone();
                editor.set_card(card);
                editor.set_parent_id(self.id);

                self.editor = Some(editor);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::End(self.id),
                    )));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["export_contact"]) =>
            {
                if self.id_positions.is_empty() {
                    return true;
                }
                let card = context.accounts[&self.account_hash].contacts.books[&self.backend_id]
                    [&self.id_positions[self.cursor_pos]]
                    .clone();
                super::export_to_vcard(&card, self.account_hash, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["mail_contact"]) =>
            {
                if self.id_positions.is_empty() {
                    return true;
                }
                let card = &context.accounts[&self.account_hash].contacts.books[&self.backend_id]
                    [&self.id_positions[self.cursor_pos]];
                let mut draft: Draft = Draft::default();
                *draft.headers_mut().get_mut("To").unwrap() =
                    format!("{} <{}>", card.name(), card.email());
                let mut composer = Composer::with_account(self.account_hash, context);
                composer.set_draft(draft, context);
                context
                    .replies
                    .push_back(UIEvent::Action(Action::Tab(TabAction::New(Some(
                        Box::new(composer),
                    )))));

                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::CONTACT_LIST]["delete_contact"]) =>
            {
                if self.id_positions.is_empty() {
                    return true;
                }
                // [ref:TODO]: add a confirmation dialog?
                context.accounts[&self.account_hash].contacts.books[&self.backend_id]
                    .remove_card(self.id_positions[self.cursor_pos]);
                self.initialized = false;
                self.set_dirty(true);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["scroll_up"]) =>
            {
                if self.new_cursor_pos == 0 {
                    return true;
                }
                self.movement = Some(PageMovement::Up(1));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["scroll_down"]) =>
            {
                if self.cursor_pos >= self.id_positions.len().saturating_sub(1) {
                    return true;
                }
                self.movement = Some(PageMovement::Down(1));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["prev_page"]) =>
            {
                self.movement = Some(PageMovement::PageUp(1));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["next_page"]) =>
            {
                self.movement = Some(PageMovement::PageDown(1));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["home_page"]) =>
            {
                self.movement = Some(PageMovement::Home);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["end_page"]) =>
            {
                self.movement = Some(PageMovement::End);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if context
                    .settings
                    .shortcuts
                    .contact_list
                    .commands
                    .iter()
                    .any(|cmd| {
                        if cmd.shortcut == *key {
                            for cmd in &cmd.command {
                                context.replies.push_back(UIEvent::Command(cmd.to_string()));
                            }
                            return true;
                        }
                        false
                    }) =>
            {
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self
                .editor
                .as_deref()
                .map(Component::is_dirty)
                .unwrap_or(false)
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let Some(ref mut editor) = self.editor {
            editor.set_dirty(value);
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if let Some(ref editor) = self.editor {
            editor.shortcuts(context)
        } else {
            ShortcutMaps::default()
        };
        map.insert(
            Shortcuts::CONTACT_LIST,
            account_settings!(context[&self.account_hash].shortcuts.contact_list).key_values(),
        );
        map.insert(
            Shortcuts::LISTING,
            account_settings!(context[&self.account_hash].shortcuts.listing).key_values(),
        );
        map.insert(
            Shortcuts::GENERAL,
            account_settings!(context[&self.account_hash].shortcuts.general).key_values(),
        );

        map
    }

    fn status(&self, context: &Context) -> String {
        if let Some(ref editor) = self.editor {
            return editor.status(context);
        }

        match self.id_positions.len() {
            1 => "1 entry".into(),
            no => format!("{no} entries"),
        }
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if let Some(ref mut editor) = self.editor {
            return editor.can_quit_cleanly(context);
        }
        true
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

#[derive(Debug)]
pub struct AccountContacts {
    account_hash: AccountHash,
    book_pos: usize,
    books: IndexMap<(Arc<ContactBackendID>, AddressBookName), Box<AddressBookList>>,
    dirty: bool,
    theme_default: ThemeAttribute,
    highlight_theme: ThemeAttribute,
    id: ComponentId,
}

impl AccountEntryTrait for AccountContacts {
    const DESCRIPTION: &str = "contacts";
    type Entry = AddressBookList;

    fn account_hash(&self) -> &AccountHash {
        &self.account_hash
    }

    fn new(
        _parent: ComponentId,
        id: ComponentId,
        account_hash: AccountHash,
        context: &mut Context,
    ) -> Result<Self> {
        let theme_default = crate::conf::value(context, "theme_default");
        let highlight_theme = crate::conf::value(context, "highlight");
        Ok(Self {
            account_hash,
            book_pos: 0,
            dirty: true,
            theme_default,
            highlight_theme,
            books: context.accounts[&account_hash]
                .contacts
                .books
                .iter()
                .map(|(book_id, book)| {
                    (
                        book_id.clone(),
                        Box::new(AddressBookList::new(
                            book_id.clone(),
                            book,
                            account_hash,
                            context,
                        )),
                    )
                })
                .collect(),
            id,
        })
    }

    fn no_of_entries(&self) -> usize {
        self.books.len()
    }

    fn draw_menu_entry(
        &self,
        i: usize,
        must_highlight_account: bool,
        grid: &mut CellBuffer,
        area: Area,
        _: &mut Context,
    ) {
        let book = &self.books[i];
        let book_attr = if must_highlight_account && self.book_pos == i {
            self.highlight_theme
        } else {
            self.theme_default
        };
        grid.change_theme(area, book_attr);
        let (x, y) = grid.write_string(
            &book.backend_id.1,
            book_attr.fg,
            book_attr.bg,
            book_attr.attrs,
            area,
            None,
            None,
        );
        grid.write_string(
            &format!(" [{}]", book.id_positions.len()),
            book_attr.fg,
            book_attr.bg,
            book_attr.attrs,
            area.skip(x, y),
            None,
            None,
        );
    }
}

impl std::fmt::Display for AccountContacts {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "contacts")
    }
}

impl Component for AccountContacts {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        if self.books.is_empty() {
            grid.clear_area(area, self.theme_default);
            context.dirty_areas.push_back(area);
        } else {
            self.books[self.book_pos].draw(grid, area, context);
        }
        self.set_dirty(false);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if !self.books.is_empty() && self.books[self.book_pos].process_event(event, context) {
            return true;
        }
        match event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                self.highlight_theme = crate::conf::value(context, "highlight");
            }
            UIEvent::ChangeMode(UIMode::Normal)
            | UIEvent::Resize
            | UIEvent::VisibilityChange(_) => {
                self.set_dirty(true);
            }
            _ => {}
        }
        let shortcuts = self.shortcuts(context);
        match *event {
            UIEvent::AccountStatusChange(ref account_hash, _)
                if account_hash == self.account_hash() =>
            {
                if context.accounts[account_hash].is_online(false).is_ok() {
                    self.books = context.accounts[account_hash]
                        .contacts
                        .books
                        .iter()
                        .map(|(book_id, book)| {
                            (
                                book_id.clone(),
                                Box::new(AddressBookList::new(
                                    book_id.clone(),
                                    book,
                                    self.account_hash,
                                    context,
                                )),
                            )
                        })
                        .collect();
                    self.set_dirty(true);
                }
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["next_mailbox"]) =>
            {
                let amount = 1;
                if self.book_pos + amount < self.books.len() {
                    self.book_pos += amount;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.status(context),
                        )));
                }

                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["prev_mailbox"]) =>
            {
                if self.books.is_empty() {
                    return true;
                }
                let amount = 1;
                if self.book_pos >= amount {
                    self.book_pos -= amount;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.status(context),
                        )));
                }
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        if !self.books.is_empty() {
            self.dirty || self.books[self.book_pos].is_dirty()
        } else {
            self.dirty
        }
    }

    fn set_dirty(&mut self, value: bool) {
        if !self.books.is_empty() {
            self.books[self.book_pos].set_dirty(value)
        }
        self.dirty = value;
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        if !self.books.is_empty() {
            self.books[self.book_pos].shortcuts(context)
        } else {
            let mut map = ShortcutMaps::default();
            if !map.contains_key(Shortcuts::CONTACT_LIST) {
                map.insert(
                    Shortcuts::CONTACT_LIST,
                    context.settings.shortcuts.contact_list.key_values(),
                );
            }
            if !map.contains_key(Shortcuts::LISTING) {
                map.insert(
                    Shortcuts::LISTING,
                    context.settings.shortcuts.listing.key_values(),
                );
            }
            if !map.contains_key(Shortcuts::GENERAL) {
                map.insert(
                    Shortcuts::GENERAL,
                    context.settings.shortcuts.general.key_values(),
                );
            }
            map
        }
    }

    fn status(&self, context: &Context) -> String {
        if !self.books.is_empty() {
            self.books[self.book_pos].status(context)
        } else {
            "No address books".into()
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

/// Display all account address books and a sidebar.
pub type ContactList = List<AccountContacts>;
