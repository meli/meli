/*
 * meli - status tab module.
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
use std::fmt;

#[derive(Debug)]
pub struct StatusPanel {
    cursor: (usize, usize),
    account_cursor: usize,
    status: Option<AccountStatus>,
    content: CellBuffer,
    dirty: bool,
    id: ComponentId,
}

impl fmt::Display for StatusPanel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "status")
    }
}

impl Component for StatusPanel {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let Some(ref mut status) = self.status {
            status.draw(grid, area, context);
            return;
        }
        self.draw_accounts(context);
        let (width, height) = self.content.size();
        {
            let (_, y) = write_string_to_grid(
                "Worker threads",
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Bold,
                ((1, 1), (width - 1, height - 1)),
                Some(1),
            );
            let mut y = y + 1;
            let work_controller = context.work_controller().threads.lock().unwrap();
            let mut workers: Vec<&Worker> = work_controller.values().collect::<Vec<&Worker>>();
            let mut max_name = 0;
            workers.sort_by_key(|w| {
                max_name = std::cmp::max(max_name, w.name.len());
                w.name.as_str()
            });
            for worker in workers {
                let (x, y_off) = write_string_to_grid(
                    &format!(
                        "- {:<max_name$} {}",
                        worker.name.as_str(),
                        worker.status.as_str(),
                        max_name = max_name
                    ),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((1, y), (width - 1, height - 1)),
                    Some(1),
                );
                for x in x..(width - 1) {
                    self.content[(x, y)].set_ch(' ');
                }

                y = y_off + 1;
            }
            write_string_to_grid(
                "Static threads",
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Bold,
                ((1, y + 1), (width - 1, height - 1)),
                Some(1),
            );
            y += 2;

            let work_controller = context.work_controller().static_threads.lock().unwrap();
            let mut workers: Vec<&Worker> = work_controller.values().collect::<Vec<&Worker>>();
            max_name = 0;
            workers.retain(|w| w.name != "WorkController-thread");
            workers.sort_by_key(|w| {
                max_name = std::cmp::max(max_name, w.name.len());
                w.name.as_str()
            });
            for worker in workers {
                let (x, y_off) = write_string_to_grid(
                    &format!(
                        "- {:<max_name$} {}",
                        worker.name.as_str(),
                        worker.status.as_str(),
                        max_name = max_name
                    ),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((1, y), (width - 1, height - 1)),
                    Some(1),
                );
                for x in x..(width - 1) {
                    self.content[(x, y)].set_ch(' ');
                }

                y = y_off + 1;
            }
        }
        let (cols, rows) = (width!(area), height!(area));
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        clear_area(grid, area);
        copy_area(
            grid,
            &self.content,
            area,
            (
                (
                    std::cmp::min((width - 1).saturating_sub(cols), self.cursor.0),
                    std::cmp::min((height - 1).saturating_sub(rows), self.cursor.1),
                ),
                (
                    std::cmp::min(self.cursor.0 + cols, width - 1),
                    std::cmp::min(self.cursor.1 + rows, height - 1),
                ),
            ),
        );
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut status) = self.status {
            if status.process_event(event, context) {
                return true;
            }
        }

        match *event {
            UIEvent::Input(Key::Char('k')) => {
                self.account_cursor = self.account_cursor.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('j')) => {
                if self.account_cursor + 1 < context.accounts.len() {
                    self.account_cursor += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Input(Key::Char('\n')) if self.status.is_none() => {
                self.status = Some(AccountStatus::new(self.account_cursor));
                return true;
            }
            UIEvent::Input(Key::Esc) if self.status.is_some() => {
                self.status = None;
                return true;
            }
            UIEvent::Input(Key::Left) => {
                self.cursor.0 = self.cursor.0.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Right) => {
                self.cursor.0 = self.cursor.0 + 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Up) => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Down) => {
                self.cursor.1 = self.cursor.1 + 1;
                self.dirty = true;
                return true;
            }
            UIEvent::MailboxUpdate(_) => {
                self.dirty = true;
            }
            _ => {}
        }

        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.status.as_ref().map(|s| s.is_dirty()).unwrap_or(false)
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        if let Some(ref mut status) = self.status {
            status.set_dirty();
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl StatusPanel {
    pub fn new() -> StatusPanel {
        let mut content = CellBuffer::new(120, 40, Cell::default());
        content.set_growable(true);

        StatusPanel {
            cursor: (0, 0),
            account_cursor: 0,
            content,
            status: None,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
    fn draw_accounts(&mut self, context: &Context) {
        self.content
            .resize(120, 40 + context.accounts.len() * 20, Cell::default());
        write_string_to_grid(
            "Accounts",
            &mut self.content,
            Color::Default,
            Color::Default,
            Attr::Default,
            ((2, 10), (120 - 1, 10)),
            Some(2),
        );

        for (i, a) in context.accounts.iter().enumerate() {
            for x in 2..(120 - 1) {
                set_and_join_box(&mut self.content, (x, 12 + i * 10), HORZ_BOUNDARY);
            }
            //create_box(&mut self.content, ((2, 5 + i * 10), (120 - 1, 15 + i * 10)));
            let (x, y) = write_string_to_grid(
                a.name(),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Bold,
                ((3, 12 + i * 10), (120 - 2, 12 + i * 10)),
                Some(3),
            );
            write_string_to_grid(
                " ▒██▒ ",
                &mut self.content,
                Color::Byte(32),
                Color::Default,
                Attr::Default,
                ((x, y), (120 - 2, 12 + i * 10)),
                None,
            );
            write_string_to_grid(
                &a.runtime_settings.account().identity,
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((4, y + 2), (120 - 2, y + 2)),
                None,
            );
            if i == self.account_cursor {
                for h in 1..8 {
                    self.content[(2, h + y + 1)].set_ch('*');
                }
            } else {
                for h in 1..8 {
                    self.content[(2, h + y + 1)].set_ch(' ');
                }
            }
            let (mut column_width, _) = write_string_to_grid(
                &format!(
                    "Messages total {}, unseen {}",
                    a.collection.len(),
                    a.collection
                        .envelopes
                        .read()
                        .unwrap()
                        .values()
                        .filter(|e| !e.is_seen())
                        .count()
                ),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((5, y + 3), (120 - 2, y + 3)),
                None,
            );
            column_width = std::cmp::max(
                column_width,
                write_string_to_grid(
                    &format!("Contacts total {}", a.address_book.len()),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((5, y + 4), (120 - 2, y + 4)),
                    None,
                )
                .0,
            );
            column_width = std::cmp::max(
                column_width,
                write_string_to_grid(
                    &format!("Backend {}", a.settings.account().format()),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((5, y + 5), (120 - 2, y + 5)),
                    None,
                )
                .0,
            );
            /* next column */
            write_string_to_grid(
                &format!(
                    "Messages total {}, unseen {}",
                    a.collection.len(),
                    a.collection
                        .envelopes
                        .read()
                        .unwrap()
                        .values()
                        .filter(|e| !e.is_seen())
                        .count()
                ),
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((5 + column_width, y + 2), (120 - 2, y + 2)),
                None,
            );
        }
    }
}

impl Component for AccountStatus {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }
        self.dirty = false;
        let (width, height) = self.content.size();
        let a = &context.accounts[self.account_pos];
        let backend_lck = a.backend.read().unwrap();
        let (_x, _y) = write_string_to_grid(
            "(Press Esc to return)",
            &mut self.content,
            Color::Default,
            Color::Default,
            Attr::Default,
            ((1, 0), (width - 1, height - 1)),
            None,
        );
        let mut line = 2;

        let (_x, _y) = write_string_to_grid(
            "Tag support: ",
            &mut self.content,
            Color::Default,
            Color::Default,
            Attr::Bold,
            ((1, line), (width - 1, height - 1)),
            None,
        );
        write_string_to_grid(
            if backend_lck.tags().is_some() {
                "yes"
            } else {
                "no"
            },
            &mut self.content,
            Color::Default,
            Color::Default,
            Attr::Default,
            ((_x, _y), (width - 1, height - 1)),
            None,
        );
        line += 1;
        let (_x, _y) = write_string_to_grid(
            "Cache backend: ",
            &mut self.content,
            Color::Default,
            Color::Default,
            Attr::Bold,
            ((1, line), (width - 1, height - 1)),
            None,
        );
        write_string_to_grid(
            &if a.settings.account().format() == "imap"
                && *a.settings.conf.cache_type() == CacheType::None
            {
                "server-side search".to_string()
            } else if a.settings.account().format() == "notmuch"
                && *a.settings.conf.cache_type() == CacheType::None
            {
                "notmuch database".to_string()
            } else {
                #[cfg(feature = "sqlite3")]
                {
                    if *a.settings.conf.cache_type() == CacheType::Sqlite3 {
                        if let Ok(path) = crate::sqlite3::db_path() {
                            format!("sqlite3 database {}", path.display())
                        } else {
                            "sqlite3 database".to_string()
                        }
                    } else {
                        "none (search will be slow)".to_string()
                    }
                }
                #[cfg(not(feature = "sqlite3"))]
                {
                    "none (search will be slow)".to_string()
                }
            },
            &mut self.content,
            Color::Default,
            Color::Default,
            Attr::Default,
            ((_x, _y), (width - 1, height - 1)),
            None,
        );

        line += 1;
        if a.settings.account().format() == "imap" {
            let b = (*backend_lck).as_any();
            if let Some(imap_backend) = b.downcast_ref::<melib::backends::ImapType>() {
                write_string_to_grid(
                    "Server Capabilities:",
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    ((1, line), (width - 1, height - 1)),
                    None,
                );
                let mut capabilities = imap_backend.capabilities();
                let max_name_width = std::cmp::max(
                    "Server Capabilities:".len(),
                    capabilities.iter().map(String::len).max().unwrap_or(0),
                );
                write_string_to_grid(
                    "meli support:",
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((max_name_width + 6, line), (width - 1, height - 1)),
                    None,
                );
                capabilities.sort();
                line += 1;
                for (i, cap) in capabilities.into_iter().enumerate() {
                    let (width, height) = self.content.size();
                    write_string_to_grid(
                        &cap,
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        ((1, line + i), (width - 1, height - 1)),
                        None,
                    );

                    let (width, height) = self.content.size();
                    if melib::backends::imap::SUPPORTED_CAPABILITIES
                        .iter()
                        .any(|c| cap.eq_ignore_ascii_case(c))
                    {
                        write_string_to_grid(
                            "supported",
                            &mut self.content,
                            Color::Green,
                            Color::Default,
                            Attr::Default,
                            ((max_name_width + 6, line + i), (width - 1, height - 1)),
                            None,
                        );
                    } else {
                        write_string_to_grid(
                            "not supported",
                            &mut self.content,
                            Color::Red,
                            Color::Default,
                            Attr::Default,
                            ((max_name_width + 6, line + i), (width - 1, height - 1)),
                            None,
                        );
                    }
                }
            }
        }

        /* self.content may have been resized with write_string_to_grid() calls above since it has
         * growable set */
        let (width, height) = self.content.size();
        let (cols, rows) = (width!(area), height!(area));
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        clear_area(grid, area);
        copy_area(
            grid,
            &self.content,
            area,
            (
                (
                    std::cmp::min((width - 1).saturating_sub(cols), self.cursor.0),
                    std::cmp::min((height - 1).saturating_sub(rows), self.cursor.1),
                ),
                (
                    std::cmp::min(self.cursor.0 + cols, width - 1),
                    std::cmp::min(self.cursor.1 + rows, height - 1),
                ),
            ),
        );
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match *event {
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Input(Key::Left) => {
                self.cursor.0 = self.cursor.0.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Right) => {
                self.cursor.0 = self.cursor.0 + 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Up) => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Down) => {
                self.cursor.1 = self.cursor.1 + 1;
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl AccountStatus {
    pub fn new(account_pos: usize) -> AccountStatus {
        let mut content = CellBuffer::new(120, 5, Cell::default());
        content.set_growable(true);

        AccountStatus {
            cursor: (0, 0),
            account_pos,
            content,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
}

#[derive(Debug)]
struct AccountStatus {
    cursor: (usize, usize),
    account_pos: usize,
    content: CellBuffer,
    dirty: bool,
    id: ComponentId,
}

impl fmt::Display for AccountStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "status")
    }
}
