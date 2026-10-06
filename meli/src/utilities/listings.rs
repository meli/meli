//
// meli
//
// Copyright 2026  Manos Pitsidianakis
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

use melib::error::{Error, Result};

use crate::{account_settings, components::prelude::*, conf::themes::ThemeAttribute};

pub trait AccountEntryTrait: Component + Sized {
    const DESCRIPTION: &str;
    type Entry;

    fn account_hash(&self) -> &AccountHash;
    fn new(
        parent: ComponentId,
        id: ComponentId,
        account_hash: AccountHash,
        context: &mut Context,
    ) -> Result<Self>;
    fn no_of_entries(&self) -> usize;
    fn draw_menu_entry(
        &self,
        entry: usize,
        must_highlight_account: bool,
        grid: &mut CellBuffer,
        area: Area,
        context: &mut Context,
    );
}

#[derive(Debug)]
enum AccountEntry<E: AccountEntryTrait> {
    Offline {
        account_hash: AccountHash,
        err: Error,
        messages: Vec<Cow<'static, str>>,
        dirty: bool,
        id: ComponentId,
    },
    Loaded(E),
}

impl<E: AccountEntryTrait> AccountEntry<E> {
    #[inline]
    fn account_hash(&self) -> &AccountHash {
        match self {
            Self::Offline {
                ref account_hash, ..
            } => account_hash,
            Self::Loaded(ref inner) => inner.account_hash(),
        }
    }
}

impl<E: AccountEntryTrait> std::fmt::Display for AccountEntry<E> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", E::DESCRIPTION)
    }
}

impl<E: AccountEntryTrait> Component for AccountEntry<E> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        match self {
            Self::Offline {
                ref account_hash,
                ref err,
                ref messages,
                ..
            } => {
                let theme_default = crate::conf::theme_value(context, "theme_default");
                let text_unfocused = crate::conf::theme_value(context, "text.unfocused");
                let error_message = crate::conf::theme_value(context, "error_message");
                grid.clear_area(area, theme_default);
                if context.is_online(*account_hash).is_err() {
                    let (x, _) = grid.write_string(
                        "offline: ",
                        error_message.fg,
                        error_message.bg,
                        error_message.attrs,
                        area,
                        None,
                        None,
                    );

                    let (_, mut y_offset) = grid.write_string(
                        &err.to_string(),
                        error_message.fg,
                        error_message.bg,
                        error_message.attrs,
                        area,
                        Some(x + 1),
                        Some(0),
                    );
                    y_offset += 1;
                    if let Some(msg) = messages.last() {
                        grid.write_string(
                            msg,
                            text_unfocused.fg,
                            text_unfocused.bg,
                            Attr::BOLD,
                            area.skip_rows(y_offset),
                            None,
                            None,
                        );
                    }
                    y_offset += 1;
                    for (i, msg) in messages.iter().rev().skip(1).enumerate() {
                        grid.write_string(
                            msg,
                            text_unfocused.fg,
                            text_unfocused.bg,
                            text_unfocused.attrs,
                            area.skip_rows(y_offset + i),
                            None,
                            None,
                        );
                    }
                } else {
                    grid.write_string(
                        "loading...",
                        crate::conf::theme_value(context, "highlight").fg,
                        crate::conf::theme_value(context, "highlight").bg,
                        crate::conf::theme_value(context, "highlight").attrs,
                        area,
                        None,
                        None,
                    );
                    let mut jobs: Vec<_> =
                        context.accounts[account_hash].active_jobs.iter().collect();
                    jobs.sort_by_key(|(j, _)| *j);
                    for (i, (job_id, j)) in jobs.into_iter().enumerate() {
                        grid.write_string(
                            &format!("{}: {:?}", job_id, j),
                            text_unfocused.fg,
                            text_unfocused.bg,
                            text_unfocused.attrs,
                            area.skip_rows(i + 1),
                            None,
                            None,
                        );
                    }

                    context
                        .replies
                        .push_back(UIEvent::AccountStatusChange(*account_hash, None));
                }
                context.dirty_areas.push_back(area);
            }
            Self::Loaded(ref mut inner) => inner.draw(grid, area, context),
        }
        self.set_dirty(false);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Self::Loaded(ref mut inner) = self {
            if inner.process_event(event, context) {
                return true;
            }
        }

        match event {
            UIEvent::AccountStatusChange(account_hash, msg)
                if account_hash == self.account_hash() =>
            {
                match self {
                    Self::Offline {
                        ref account_hash,
                        ref mut messages,
                        ref id,
                        ..
                    } => {
                        if context.accounts[account_hash].is_online(false).is_ok() {
                            let parent_id = self.id();
                            let id = *id;
                            let entry = E::new(parent_id, id, *account_hash, context).unwrap();
                            *self = Self::Loaded(entry);
                        } else {
                            if let Some(ref msg) = msg {
                                if !matches!(messages.last(), Some(last_msg) if last_msg == msg) {
                                    messages.push(msg.clone());
                                }
                            }
                            self.set_dirty(true);
                        }
                    }
                    Self::Loaded(_) => {
                        let id = self.id();
                        if let Err(err) = context.accounts[&*account_hash].is_online(false) {
                            *self = Self::Offline {
                                account_hash: *account_hash,
                                dirty: true,
                                id,
                                err,
                                messages: if let Some(ref msg) = msg {
                                    vec![msg.clone()]
                                } else {
                                    vec![]
                                },
                            };
                        }
                    }
                }
            }
            UIEvent::ChangeMode(UIMode::Normal)
            | UIEvent::Resize
            | UIEvent::ConfigReload { old_settings: _ }
            | UIEvent::VisibilityChange(_) => {
                self.set_dirty(true);
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        match self {
            Self::Offline { dirty, .. } => *dirty,
            Self::Loaded(ref inner) => inner.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        match self {
            Self::Offline { ref mut dirty, .. } => *dirty = value,
            Self::Loaded(ref mut inner) => inner.set_dirty(value),
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        match self {
            Self::Offline { .. } => ShortcutMaps::default(),
            Self::Loaded(ref inner) => inner.shortcuts(context),
        }
    }

    fn status(&self, context: &Context) -> String {
        match self {
            Self::Offline { .. } => "Offline".into(),
            Self::Loaded(ref inner) => inner.status(context),
        }
    }

    fn id(&self) -> ComponentId {
        match self {
            Self::Offline { id, .. } => *id,
            Self::Loaded(ref inner) => inner.id(),
        }
    }
}

/// Display all account address books and a sidebar.
#[derive(Debug)]
pub struct List<E: AccountEntryTrait> {
    accounts: IndexMap<AccountHash, Box<AccountEntry<E>>>,
    account_pos: usize,
    menu_visibility: bool,
    theme_default: ThemeAttribute,
    dirty: bool,
    id: ComponentId,
}

impl<E: AccountEntryTrait> std::fmt::Display for List<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", E::DESCRIPTION)
    }
}

impl<E: AccountEntryTrait> List<E> {
    /// Create a [`List`].
    pub fn new(context: &mut Context) -> Self {
        let id = ComponentId::default();
        // Calling Account::is_online requires a mutable reference, so initialize
        // entries in a second pass.
        let accounts: IndexMap<AccountHash, Result<()>> = context
            .accounts
            .iter_mut()
            .map(|(h, a)| (*h, a.is_online(false)))
            .collect();
        let accounts: IndexMap<AccountHash, Box<AccountEntry<E>>> = accounts
            .into_iter()
            .map(|(account_hash, is_online)| {
                (account_hash, {
                    let entry_id = ComponentId::default();
                    match is_online {
                        Ok(()) => {
                            let entry = E::new(id, entry_id, account_hash, context).unwrap();
                            Box::new(AccountEntry::Loaded(entry))
                        }
                        Err(err) => Box::new(AccountEntry::Offline {
                            account_hash,
                            dirty: true,
                            err,
                            messages: vec![],
                            id: entry_id,
                        }),
                    }
                })
            })
            .collect();
        Self {
            accounts,
            account_pos: 0,
            menu_visibility: true,
            theme_default: crate::conf::theme_value(context, "theme_default"),
            dirty: true,
            id,
        }
    }

    pub fn draw_menu(&self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let mut y_offset = 0;
        let account_name_attr = crate::conf::theme_value(context, "mail.sidebar_account_name");
        let highlight_attr = {
            let mut v = crate::conf::theme_value(context, "mail.sidebar_highlighted");
            if !context.settings.terminal.use_color() {
                v.attrs |= Attr::REVERSE;
            }
            v
        };
        for (account_entry_index, (_, a)) in self.accounts.iter().enumerate() {
            let must_highlight_account: bool = self.account_pos == account_entry_index;
            let entries_no = match **a {
                AccountEntry::Offline { .. } => 1,
                AccountEntry::Loaded(ref inner) => 1 + inner.no_of_entries(),
            };

            let area = area.skip_rows(y_offset).take_rows(entries_no);
            let account_attr = if must_highlight_account {
                highlight_attr
            } else {
                account_name_attr
            };

            grid.change_theme(area, account_attr);
            let (x, y) = grid.write_string(
                &context.accounts[a.account_hash()].name,
                account_attr.fg,
                account_attr.bg,
                account_attr.attrs,
                area,
                None,
                None,
            );

            if let AccountEntry::Loaded(ref inner) = **a {
                let no_of_entries = inner.no_of_entries();
                if no_of_entries > 0 {
                    grid.write_string(
                        &format!(" [{}]", no_of_entries),
                        account_attr.fg,
                        account_attr.bg,
                        account_attr.attrs,
                        area.skip(x, y),
                        None,
                        None,
                    );
                }

                for i in 0..no_of_entries {
                    inner.draw_menu_entry(
                        i,
                        must_highlight_account,
                        grid,
                        area.skip_rows(1 + i).take_rows(1),
                        context,
                    );
                }
            } else {
                grid.write_string(
                    " offline",
                    account_attr.fg,
                    account_attr.bg,
                    account_attr.attrs | Attr::ITALICS,
                    area.skip(x, y),
                    None,
                    None,
                );
            }
            y_offset += entries_no;
        }

        context.dirty_areas.push_back(area);
    }
}

impl<E: AccountEntryTrait> Component for List<E> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let list_area = if self.menu_visibility {
            let menu_width = (10 * area.width()) / 100;
            context
                .dirty_areas
                .push_back(area.take_cols(menu_width + 1));
            grid.clear_area(area.take_cols(menu_width + 1), self.theme_default);
            let menu_area = area.take_cols(menu_width);
            self.draw_menu(grid, menu_area, context);

            area.skip_cols(menu_width + 1)
        } else {
            area
        };
        self.accounts[self.account_pos].draw(grid, list_area, context);
        self.set_dirty(false);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.accounts[self.account_pos].process_event(event, context) {
            return true;
        }

        match event {
            UIEvent::AccountStatusChange(account_hash, _msg) => {
                if self.accounts[&*account_hash].process_event(event, context) {
                    return true;
                }
            }
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::theme_value(context, "theme_default");
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
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["next_account"]) =>
            {
                let amount = 1;
                if self.account_pos + amount < self.accounts.len() {
                    self.account_pos += amount;
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
                if shortcut!(key == shortcuts[Shortcuts::LISTING]["prev_account"]) =>
            {
                if self.accounts.is_empty() {
                    return true;
                }
                let amount = 1;
                if self.account_pos >= amount {
                    self.account_pos -= amount;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.status(context),
                        )));
                }
                return true;
            }
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Shortcuts::LISTING]["toggle_menu_visibility"]) =>
            {
                self.menu_visibility = !self.menu_visibility;
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if account_settings!(
                    context[self.accounts[self.account_pos].account_hash()]
                        .shortcuts
                        .listing
                )
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
        self.dirty || self.accounts[self.account_pos].is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.accounts[self.account_pos].set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = self.accounts[self.account_pos].shortcuts(context);
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

    fn status(&self, context: &Context) -> String {
        self.accounts[self.account_pos].status(context)
    }

    fn kill(&mut self, uuid: ComponentId, context: &mut Context) {
        debug_assert!(uuid == self.id);
        context
            .replies
            .push_back(UIEvent::Action(Action::Tab(TabAction::Kill(uuid))));
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
