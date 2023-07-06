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

use melib::backends::AccountHash;

use super::*;
use crate::{conf::accounts::MailboxEntry, melib::text_processing::TextProcessing};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MailboxAction {
    Rename,
    Move,
    Subscribe,
    Unsubscribe,
}

#[derive(Debug, Default, PartialEq)]
enum ViewMode {
    #[default]
    List,
    Action(UIDialog<MailboxAction>),
}

#[derive(Debug)]
pub struct MailboxManager {
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    account_hash: AccountHash,
    length: usize,
    data_columns: DataColumns<5>,
    entries: IndexMap<MailboxHash, MailboxEntry>,
    mode: ViewMode,

    initialized: bool,
    theme_default: ThemeAttribute,
    highlight_theme: ThemeAttribute,

    dirty: bool,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl fmt::Display for MailboxManager {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mailboxes")
    }
}

impl MailboxManager {
    pub fn new(context: &Context, account_pos: usize) -> Self {
        let account_hash = context.accounts[account_pos].hash();
        let theme_default = crate::conf::value(context, "theme_default");
        let mut data_columns = DataColumns::default();
        data_columns.theme_config.set_single_theme(theme_default);
        MailboxManager {
            cursor_pos: 0,
            new_cursor_pos: 0,
            account_hash,
            mode: ViewMode::default(),
            entries: IndexMap::default(),
            length: 0,
            account_pos,
            data_columns,
            theme_default,
            highlight_theme: crate::conf::value(context, "highlight"),
            initialized: false,
            dirty: true,
            movement: None,
            id: ComponentId::default(),
        }
    }

    fn initialize(&mut self, context: &mut Context) {
        let account = &context.accounts[self.account_pos];
        self.length = account.mailbox_entries.len();
        self.entries = account.mailbox_entries.clone();
        self.entries
            .sort_by(|_, a, _, b| a.ref_mailbox.path().cmp(b.ref_mailbox.path()));

        self.set_dirty(true);
        let mut min_width = (
            "name".len(),
            "path".len(),
            "size".len(),
            "subscribed".len(),
            0,
            0,
        );

        for c in self.entries.values() {
            /* title */
            min_width.0 = cmp::max(min_width.0, c.name().split_graphemes().len());
            /* path */
            min_width.1 = cmp::max(min_width.1, c.ref_mailbox.path().len());
        }

        /* name column */
        self.data_columns.columns[0] =
            CellBuffer::new_with_context(min_width.0, self.length, None, context);
        /* path column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(min_width.1, self.length, None, context);
        /* size column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(min_width.2, self.length, None, context);
        /* subscribed column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context(min_width.3, self.length, None, context);

        for (idx, e) in self.entries.values().enumerate() {
            write_string_to_grid(
                e.name(),
                &mut self.data_columns.columns[0],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.0, idx)),
                None,
            );

            write_string_to_grid(
                e.ref_mailbox.path(),
                &mut self.data_columns.columns[1],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.1, idx)),
                None,
            );

            let (_unseen, total) = e.ref_mailbox.count().ok().unwrap_or((0, 0));
            write_string_to_grid(
                &total.to_string(),
                &mut self.data_columns.columns[2],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.2, idx)),
                None,
            );

            write_string_to_grid(
                if e.ref_mailbox.is_subscribed() {
                    "yes"
                } else {
                    "no"
                },
                &mut self.data_columns.columns[3],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.3, idx)),
                None,
            );
        }

        if self.length == 0 {
            let message = "No mailboxes.".to_string();
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), self.length, None, context);
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, 0), (message.len() - 1, 0)),
                None,
            );
        }
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let (upper_left, bottom_right) = area;

        if self.length == 0 {
            clear_area(grid, area, self.theme_default);
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
            for &(idx, highlight) in &[(old_cursor_pos, false), (self.new_cursor_pos, true)] {
                if idx >= self.length {
                    continue; //bounds check
                }
                let new_area = nth_row_area(area, idx % rows);
                self.data_columns
                    .draw(grid, idx, self.cursor_pos, grid.bounds_iter(new_area));
                let row_attr = if highlight {
                    self.highlight_theme
                } else {
                    self.theme_default
                };
                change_colors(grid, new_area, row_attr.fg, row_attr.bg);
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
        /* Page_no has changed, so draw new page */
        _ = self
            .data_columns
            .recalc_widths((width!(area), height!(area)), top_idx);
        clear_area(grid, area, self.theme_default);
        /* copy table columns */
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos, grid.bounds_iter(area));

        /* highlight cursor */
        change_colors(
            grid,
            nth_row_area(area, self.cursor_pos % rows),
            self.highlight_theme.fg,
            self.highlight_theme.bg,
        );

        /* clear gap if available height is more than count of entries */
        if top_idx + rows > self.length {
            clear_area(
                grid,
                (
                    pos_inc(upper_left, (0, self.length - top_idx)),
                    bottom_right,
                ),
                self.theme_default,
            );
        }
        context.dirty_areas.push_back(area);
    }
}

impl Component for MailboxManager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        if !self.initialized {
            self.initialize(context);
        }

        self.draw_list(grid, area, context);
        if let ViewMode::Action(ref mut s) = self.mode {
            s.draw(grid, area, context);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::value(context, "theme_default");
            self.initialized = false;
            self.set_dirty(true);
        }
        if let ViewMode::Action(ref mut s) = self.mode {
            match &event {
                UIEvent::FinishedUIDialog(id, result) if s.id() == *id => {
                    self.set_dirty(true);
                    self.mode = ViewMode::List;
                    if let Some(actions) = result.downcast_ref::<Vec<MailboxAction>>() {
                        if actions.len() == 1 {
                            use crate::actions::MailboxOperation;
                            match actions[0] {
                                MailboxAction::Move | MailboxAction::Rename => {
                                    context.replies.push_back(UIEvent::CmdInput(Key::Paste(
                                        format!(
                                            "rename-mailbox \"{account_name}\" \
                                             \"{mailbox_path_src}\" ",
                                            account_name =
                                                context.accounts[&self.account_hash].name(),
                                            mailbox_path_src =
                                                self.entries[self.cursor_pos].ref_mailbox.path()
                                        ),
                                    )));
                                    context
                                        .replies
                                        .push_back(UIEvent::ChangeMode(UIMode::Command));
                                }
                                MailboxAction::Subscribe => {
                                    if let Err(err) = context.accounts[&self.account_hash]
                                        .mailbox_operation(MailboxOperation::Subscribe(
                                            self.entries[self.cursor_pos]
                                                .ref_mailbox
                                                .path()
                                                .to_string(),
                                        ))
                                    {
                                        context.replies.push_back(UIEvent::Notification(
                                            None,
                                            err.to_string(),
                                            Some(crate::types::NotificationType::Error(err.kind)),
                                        ));
                                    }
                                }
                                MailboxAction::Unsubscribe => {
                                    if let Err(err) = context.accounts[&self.account_hash]
                                        .mailbox_operation(MailboxOperation::Unsubscribe(
                                            self.entries[self.cursor_pos]
                                                .ref_mailbox
                                                .path()
                                                .to_string(),
                                        ))
                                    {
                                        context.replies.push_back(UIEvent::Notification(
                                            None,
                                            err.to_string(),
                                            Some(crate::types::NotificationType::Error(err.kind)),
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    return true;
                }
                _ => {}
            }
            return s.process_event(event, context);
        }

        let shortcuts = self.shortcuts(context);
        match event {
            UIEvent::AccountStatusChange(account_hash, msg)
                if *account_hash == self.account_hash =>
            {
                self.initialize(context);

                self.set_dirty(true);
                //self.menu_content.empty();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(match msg {
                        Some(msg) => format!("{} {}", self.status(context), msg),
                        None => self.status(context),
                    })));
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) =>
            {
                let amount = 1;
                self.movement = Some(PageMovement::Up(amount));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"])
                    && self.cursor_pos < self.length.saturating_sub(1) =>
            {
                let amount = 1;
                self.set_dirty(true);
                self.movement = Some(PageMovement::Down(amount));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["prev_page"]) =>
            {
                let mult = 1;
                self.set_dirty(true);
                self.movement = Some(PageMovement::PageUp(mult));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["next_page"]) =>
            {
                let mult = 1;
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
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["open_entry"]) =>
            {
                self.set_dirty(true);
                self.mode = ViewMode::Action(UIDialog::new(
                    "select action",
                    vec![
                        (MailboxAction::Rename, "rename".into()),
                        (MailboxAction::Move, "move".into()),
                        (MailboxAction::Subscribe, "subscribe".into()),
                        (MailboxAction::Unsubscribe, "unsubscribe".into()),
                    ],
                    true,
                    Some(Box::new(
                        move |id: ComponentId, results: &[MailboxAction]| {
                            Some(UIEvent::FinishedUIDialog(id, Box::new(results.to_vec())))
                        },
                    )),
                    context,
                ));
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || if let ViewMode::Action(ref s) = self.mode {
                s.is_dirty()
            } else {
                false
            }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let ViewMode::Action(ref mut s) = self.mode {
            s.set_dirty(value);
        }
    }

    fn kill(&mut self, uuid: ComponentId, context: &mut Context) {
        debug_assert!(uuid == self.id);
        context.replies.push_back(UIEvent::Action(Tab(Kill(uuid))));
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();

        map.insert(
            Shortcuts::GENERAL,
            context.settings.shortcuts.general.key_values(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, _context: &Context) -> bool {
        true
    }

    fn status(&self, _context: &Context) -> String {
        format!("{} entries", self.entries.len())
    }
}
