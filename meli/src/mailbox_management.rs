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

use indexmap::IndexMap;
use melib::{backends::AccountHash, SortOrder};

use super::*;
use crate::{accounts::MailboxEntry, melib::text::TextProcessing};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
enum Column {
    _0 = 0,
    _1,
    _2,
    _3,
}

const fn _assert_len() {
    if MailboxManager::HEADERS.len() != Column::_3 as usize + 1 {
        panic!("MailboxManager::HEADERS length changed, please update Column enum accordingly.");
    }
}

const _: () = _assert_len();

#[derive(Debug)]
pub struct MailboxManager {
    cursor_pos: usize,
    new_cursor_pos: usize,
    account_pos: usize,
    account_hash: AccountHash,
    length: usize,
    data_columns: DataColumns<4>,
    min_width: [usize; 4],
    sort_col: Column,
    sort_order: SortOrder,
    entries: IndexMap<MailboxHash, MailboxEntry>,
    mode: ViewMode,

    initialized: bool,
    theme_default: ThemeAttribute,
    highlight_theme: ThemeAttribute,

    dirty: bool,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl std::fmt::Display for MailboxManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "mailboxes")
    }
}

impl MailboxManager {
    const HEADERS: [&'static str; 4] = ["name", "path", "size", "subscribed"];

    pub fn new(context: &Context, account_pos: usize) -> Self {
        let account_hash = context.accounts[account_pos].hash();
        let theme_default = crate::conf::value(context, "theme_default");
        let mut data_columns = DataColumns::new(theme_default);
        data_columns.theme_config.set_single_theme(theme_default);
        Self {
            cursor_pos: 0,
            new_cursor_pos: 0,
            account_hash,
            mode: ViewMode::default(),
            entries: IndexMap::default(),
            length: 0,
            account_pos,
            data_columns,
            sort_col: Column::_1,
            sort_order: SortOrder::Asc,
            min_width: [0; 4],
            theme_default,
            highlight_theme: crate::conf::value(context, "highlight"),
            initialized: false,
            dirty: true,
            movement: None,
            id: ComponentId::default(),
        }
    }

    fn initialize(&mut self, context: &Context) {
        let account = &context.accounts[self.account_pos];
        self.length = account.mailbox_entries.len();
        let mut entries = account.mailbox_entries.clone();
        entries.sort_by(|_, a, _, b| match (self.sort_col, self.sort_order) {
            (Column::_0, SortOrder::Asc) => a.ref_mailbox.name().cmp(b.ref_mailbox.name()),
            (Column::_0, SortOrder::Desc) => b.ref_mailbox.name().cmp(a.ref_mailbox.name()),
            (Column::_1, SortOrder::Asc) => a.ref_mailbox.path().cmp(b.ref_mailbox.path()),
            (Column::_1, SortOrder::Desc) => b.ref_mailbox.path().cmp(a.ref_mailbox.path()),
            (Column::_2, SortOrder::Asc) => {
                let (_, a) = a.ref_mailbox.count().ok().unwrap_or((0, 0));
                let (_, b) = b.ref_mailbox.count().ok().unwrap_or((0, 0));
                a.cmp(&b)
            }
            (Column::_2, SortOrder::Desc) => {
                let (_, a) = a.ref_mailbox.count().ok().unwrap_or((0, 0));
                let (_, b) = b.ref_mailbox.count().ok().unwrap_or((0, 0));
                b.cmp(&a)
            }
            (Column::_3, SortOrder::Asc)
                if a.ref_mailbox.is_subscribed() && b.ref_mailbox.is_subscribed() =>
            {
                std::cmp::Ordering::Equal
            }
            (Column::_3, SortOrder::Asc) if a.ref_mailbox.is_subscribed() => {
                std::cmp::Ordering::Greater
            }
            (Column::_3, SortOrder::Desc) if a.ref_mailbox.is_subscribed() => {
                std::cmp::Ordering::Less
            }
            (Column::_3, SortOrder::Asc) => std::cmp::Ordering::Less,
            (Column::_3, SortOrder::Desc) => std::cmp::Ordering::Greater,
        });
        self.entries = entries;
        macro_rules! hdr {
            ($idx:literal) => {{
                Self::HEADERS[$idx].len() + if self.sort_col as u8 == $idx { 1 } else { 0 }
            }};
        }

        self.set_dirty(true);
        let mut min_width = [hdr!(0), hdr!(1), hdr!(2), hdr!(3)];

        for c in self.entries.values() {
            // title
            min_width[0] = cmp::max(min_width[0], c.name().split_graphemes().len());
            // path
            min_width[1] = cmp::max(min_width[1], c.ref_mailbox.path().len());
        }

        // name column
        _ = self.data_columns.columns[0].resize_with_context(min_width[0], self.length, context);
        self.data_columns.columns[0].grid_mut().clear(None);
        // path column
        _ = self.data_columns.columns[1].resize_with_context(min_width[1], self.length, context);
        self.data_columns.columns[1].grid_mut().clear(None);
        // size column
        _ = self.data_columns.columns[2].resize_with_context(min_width[2], self.length, context);
        self.data_columns.columns[2].grid_mut().clear(None);
        // subscribed column
        _ = self.data_columns.columns[3].resize_with_context(min_width[3], self.length, context);
        self.data_columns.columns[3].grid_mut().clear(None);

        for (idx, e) in self.entries.values().enumerate() {
            {
                let area = self.data_columns.columns[0].area().nth_row(idx);
                self.data_columns.columns[0].grid_mut().write_string(
                    e.name(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                );
            }

            {
                let area = self.data_columns.columns[1].area().nth_row(idx);
                self.data_columns.columns[1].grid_mut().write_string(
                    e.ref_mailbox.path(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                );
            }

            {
                let area = self.data_columns.columns[2].area().nth_row(idx);
                let (_unseen, total) = e.ref_mailbox.count().ok().unwrap_or((0, 0));
                self.data_columns.columns[2].grid_mut().write_string(
                    &total.to_string(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                );
            }

            {
                let area = self.data_columns.columns[3].area().nth_row(idx);
                self.data_columns.columns[3].grid_mut().write_string(
                    if e.ref_mailbox.is_subscribed() {
                        "yes"
                    } else {
                        "no"
                    },
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                    None,
                );
            }
        }

        if self.length == 0 {
            let message = "No mailboxes.".to_string();
            if self.data_columns.columns[0].resize_with_context(message.len(), self.length, context)
            {
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
        }

        self.min_width = min_width;
    }

    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let rows = area.height();
        if rows < 2 {
            return;
        }

        if self.length == 0 {
            grid.clear_area(area, self.theme_default);

            grid.copy_area(
                self.data_columns.columns[0].grid(),
                area,
                self.data_columns.columns[0].area(),
            );
            context.dirty_areas.push_back(area);
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
                PageMovement::Right(amount) => {
                    self.data_columns.x_offset += amount;
                    self.data_columns.x_offset = self.data_columns.x_offset.min(
                        self.data_columns
                            .widths
                            .iter()
                            .map(|w| w + 2)
                            .sum::<usize>()
                            .saturating_sub(2),
                    );
                }
                PageMovement::Left(amount) => {
                    self.data_columns.x_offset = self.data_columns.x_offset.saturating_sub(amount);
                }
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

        // If cursor position has changed, remove the highlight from the previous
        // position and apply it in the new one.
        if self.cursor_pos != self.new_cursor_pos && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for &(idx, highlight) in &[(old_cursor_pos, false), (self.new_cursor_pos, true)] {
                if idx >= self.length {
                    continue; //bounds check
                }
                let new_area = area.nth_row(idx % rows);
                self.data_columns
                    .draw(grid, idx, self.cursor_pos, grid.bounds_iter(new_area));
                let row_attr = if highlight {
                    self.highlight_theme
                } else {
                    self.theme_default
                };
                grid.change_theme(new_area, row_attr);
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
        // Page_no has changed, so draw new page
        _ = self
            .data_columns
            .recalc_widths((area.width(), area.height()), top_idx);
        grid.clear_area(area, self.theme_default);
        // copy table columns
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos, grid.bounds_iter(area));

        // highlight cursor
        grid.change_theme(area.nth_row(self.cursor_pos % rows), self.highlight_theme);

        // clear gap if available height is more than count of entries
        if top_idx + rows > self.length {
            grid.change_theme(area.skip_rows(self.length - top_idx), self.theme_default);
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
        if self.dirty {
            let area = area.nth_row(0);
            // Draw column headers.
            grid.clear_area(area, self.theme_default);
            let mut x_offset = 0;
            for (i, (h, w)) in Self::HEADERS.iter().zip(self.min_width).enumerate() {
                grid.write_string(
                    h,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs | Attr::BOLD,
                    area.skip_cols(x_offset),
                    None,
                    None,
                );
                if self.sort_col as usize == i {
                    use SortOrder::*;
                    let arrow = match (grid.ascii_drawing, self.sort_order) {
                        (true, Asc) => DataColumns::<4>::ARROW_UP_ASCII,
                        (true, Desc) => DataColumns::<4>::ARROW_DOWN_ASCII,
                        (false, Asc) => DataColumns::<4>::ARROW_UP,
                        (false, Desc) => DataColumns::<4>::ARROW_DOWN,
                    };
                    grid.write_string(
                        arrow,
                        self.theme_default.fg,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        area.skip_cols(x_offset + h.len()),
                        None,
                        None,
                    );
                }
                x_offset += w + 2;
            }
            context.dirty_areas.push_back(area);
        }
        let area = area.skip_rows(1);
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
                                        context.replies.push_back(UIEvent::Notification {
                                            title: None,
                                            source: None,
                                            body: err.to_string().into(),
                                            kind: Some(crate::types::NotificationType::Error(
                                                err.kind,
                                            )),
                                        });
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
                                        context.replies.push_back(UIEvent::Notification {
                                            title: None,
                                            source: None,
                                            body: err.to_string().into(),
                                            kind: Some(crate::types::NotificationType::Error(
                                                err.kind,
                                            )),
                                        });
                                    }
                                }
                            }
                        }
                    }
                    return true;
                }
                UIEvent::ComponentUnrealize(id) if s.id() == *id => {
                    self.mode = ViewMode::List;
                    self.set_dirty(true);
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
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(match msg {
                        Some(msg) => format!("{} {}", self.status(context), msg),
                        None => self.status(context),
                    })));
            }
            UIEvent::Action(Action::SortColumn(column, order)) => {
                let column = match *column {
                    0 => Column::_0,
                    1 => Column::_1,
                    2 => Column::_2,
                    3 => Column::_3,
                    other => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Invalid column index `{}`: there are {} columns.",
                                other,
                                Self::HEADERS.len()
                            )),
                        ));

                        return true;
                    }
                };
                if (self.sort_col, self.sort_order) != (column, *order) {
                    self.sort_col = column;
                    self.sort_order = *order;
                    self.initialized = false;
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::Input(Key::Char(ref c)) if c.is_ascii_digit() => {
                let n = *c as u8 - b'0'; // safe cast because of is_ascii_digit() check;
                let column = match n {
                    1 => Column::_0,
                    2 => Column::_1,
                    3 => Column::_2,
                    4 => Column::_3,
                    _ => {
                        return false;
                    }
                };
                if self.sort_col == column {
                    self.sort_order = !self.sort_order;
                } else {
                    self.sort_col = column;
                    self.sort_order = SortOrder::default();
                }
                self.initialized = false;
                self.set_dirty(true);
                return true;
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

    fn status(&self, context: &Context) -> String {
        format!(
            "{} {} entries",
            context.accounts[&self.account_hash].name(),
            self.entries.len()
        )
    }
}
