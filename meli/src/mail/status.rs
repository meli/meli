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

use melib::{MailBackendExtensionStatus, SpecialUsageMailbox};

use super::*;

#[derive(Debug)]
pub struct AccountStatus {
    cursor: (usize, usize),
    account_pos: usize,
    content: CellBuffer,
    dirty: bool,
    theme_default: ThemeAttribute,
    id: ComponentId,
}

impl std::fmt::Display for AccountStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "status")
    }
}

impl AccountStatus {
    pub fn new(account_pos: usize, theme_default: ThemeAttribute) -> AccountStatus {
        let default_cell = {
            let mut ret = Cell::with_char(' ');
            ret.set_fg(theme_default.fg)
                .set_bg(theme_default.bg)
                .set_attrs(theme_default.attrs);
            ret
        };
        let mut content = CellBuffer::new(120, 5, default_cell);
        content.set_growable(true);

        AccountStatus {
            cursor: (0, 0),
            account_pos,
            content,
            dirty: true,
            theme_default,
            id: ComponentId::default(),
        }
    }
}

impl Component for AccountStatus {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }
        self.dirty = false;
        let (mut width, _) = self.content.size();
        let a = &context.accounts[self.account_pos];
        let (_x, _y) = write_string_to_grid(
            "Account ",
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs | Attr::UNDERLINE,
            ((1, 0), (width - 1, 0)),
            None,
        );
        let (_x, _y) = write_string_to_grid(
            a.name(),
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD | Attr::UNDERLINE,
            ((_x, _y), (width - 1, _y)),
            None,
        );
        width = self.content.size().0;
        let mut line = 2;

        write_string_to_grid(
            "In-progress jobs:",
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            ((1, line), (width - 1, line)),
            None,
        );
        line += 2;

        for (job_id, req) in a.active_jobs.iter() {
            width = self.content.size().0;
            use crate::conf::accounts::JobRequest;
            let (x, y) = write_string_to_grid(
                &format!("{} {}", req, job_id),
                &mut self.content,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((1, line), (width - 1, line)),
                None,
            );
            if let JobRequest::DeleteMailbox { mailbox_hash, .. }
            | JobRequest::SetMailboxPermissions { mailbox_hash, .. }
            | JobRequest::SetMailboxSubscription { mailbox_hash, .. }
            | JobRequest::Refresh { mailbox_hash, .. }
            | JobRequest::Fetch { mailbox_hash, .. } = req
            {
                write_string_to_grid(
                    a.mailbox_entries[mailbox_hash].name(),
                    &mut self.content,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    ((x + 1, y), (width - 1, y)),
                    None,
                );
            }

            line += 1;
        }

        line += 2;
        width = self.content.size().0;

        let (_x, _y) = write_string_to_grid(
            "Tag support: ",
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            ((1, line), (width - 1, line)),
            None,
        );
        width = self.content.size().0;
        write_string_to_grid(
            if a.backend_capabilities.supports_tags {
                "yes"
            } else {
                "no"
            },
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs,
            ((_x, _y), (width - 1, line)),
            None,
        );
        width = self.content.size().0;
        line += 1;
        let (_x, _y) = write_string_to_grid(
            "Search backend: ",
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            ((1, line), (width - 1, line)),
            None,
        );
        width = self.content.size().0;
        write_string_to_grid(
            &match (
                a.settings.conf.search_backend(),
                a.backend_capabilities.supports_search,
            ) {
                (SearchBackend::Auto, true) | (SearchBackend::None, true) => {
                    "backend-side search".to_string()
                }
                (SearchBackend::Auto, false) | (SearchBackend::None, false) => {
                    "none (search will be slow)".to_string()
                }
                #[cfg(feature = "sqlite3")]
                (SearchBackend::Sqlite3, _) => {
                    if let Ok(path) = crate::sqlite3::db_path() {
                        format!("sqlite3 database {}", path.display())
                    } else {
                        "sqlite3 database".to_string()
                    }
                }
            },
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs,
            ((_x, _y), (width - 1, _y)),
            None,
        );
        width = self.content.size().0;
        line += 1;

        write_string_to_grid(
            "Special Mailboxes:",
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            ((1, line), (width - 1, line)),
            None,
        );
        for f in a
            .mailbox_entries
            .values()
            .map(|entry| &entry.ref_mailbox)
            .filter(|f| f.special_usage() != SpecialUsageMailbox::Normal)
        {
            width = self.content.size().0;
            line += 1;
            write_string_to_grid(
                &format!("{}: {}", f.path(), f.special_usage()),
                &mut self.content,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((1, line), (width - 1, line)),
                None,
            );
        }
        line += 2;
        width = self.content.size().0;
        write_string_to_grid(
            "Subscribed mailboxes:",
            &mut self.content,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            ((1, line), (width - 1, line)),
            None,
        );
        line += 2;
        for mailbox_node in a.list_mailboxes() {
            width = self.content.size().0;
            let f: &Mailbox = &a[&mailbox_node.hash].ref_mailbox;
            if f.is_subscribed() {
                write_string_to_grid(
                    f.path(),
                    &mut self.content,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    ((1, line), (width - 1, line)),
                    None,
                );
                line += 1;
            }
        }

        line += 1;
        width = self.content.size().0;
        if let Some(ref extensions) = a.backend_capabilities.extensions {
            write_string_to_grid(
                "Server Extensions:",
                &mut self.content,
                self.theme_default.fg,
                self.theme_default.bg,
                Attr::BOLD,
                ((1, line), (width - 1, line)),
                None,
            );
            let max_name_width = std::cmp::max(
                "Server Extensions:".len(),
                extensions
                    .iter()
                    .map(|(n, _)| std::cmp::min(30, n.len()))
                    .max()
                    .unwrap_or(0),
            );
            width = self.content.size().0;
            write_string_to_grid(
                "meli support:",
                &mut self.content,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((max_name_width + 6, line), (width - 1, line)),
                None,
            );
            line += 1;
            for (name, status) in extensions.iter() {
                width = self.content.size().0;
                write_string_to_grid(
                    name.trim_at_boundary(30),
                    &mut self.content,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    ((1, line), (width - 1, line)),
                    None,
                );

                width = self.content.size().0;
                let (x, y) = match status {
                    MailBackendExtensionStatus::Unsupported { comment: _ } => write_string_to_grid(
                        "not supported",
                        &mut self.content,
                        Color::Red,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        ((max_name_width + 6, line), (width - 1, line)),
                        None,
                    ),
                    MailBackendExtensionStatus::Supported { comment: _ } => write_string_to_grid(
                        "supported",
                        &mut self.content,
                        Color::Green,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        ((max_name_width + 6, line), (width - 1, line)),
                        None,
                    ),
                    MailBackendExtensionStatus::Enabled { comment: _ } => write_string_to_grid(
                        "enabled",
                        &mut self.content,
                        Color::Green,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        ((max_name_width + 6, line), (width - 1, line)),
                        None,
                    ),
                };
                match status {
                    MailBackendExtensionStatus::Unsupported { comment }
                    | MailBackendExtensionStatus::Supported { comment }
                    | MailBackendExtensionStatus::Enabled { comment } => {
                        if let Some(s) = comment {
                            let (x, y) = write_string_to_grid(
                                " (",
                                &mut self.content,
                                self.theme_default.fg,
                                self.theme_default.bg,
                                self.theme_default.attrs,
                                ((x, y), (width - 1, y)),
                                None,
                            );
                            let (x, y) = write_string_to_grid(
                                s,
                                &mut self.content,
                                self.theme_default.fg,
                                self.theme_default.bg,
                                self.theme_default.attrs,
                                ((x, y), (width - 1, y)),
                                None,
                            );
                            write_string_to_grid(
                                ")",
                                &mut self.content,
                                self.theme_default.fg,
                                self.theme_default.bg,
                                self.theme_default.attrs,
                                ((x, y), (width - 1, y)),
                                None,
                            );
                        }
                    }
                };
                line += 1;
            }
        }

        /* self.content may have been resized with write_string_to_grid() calls above
         * since it has growable set */
        let (width, height) = self.content.size();
        let (cols, rows) = (width!(area), height!(area));
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        clear_area(grid, area, self.theme_default);
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
        let shortcuts = self.shortcuts(context);
        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"])
                    && self.cursor.0 != 0 =>
            {
                self.cursor.0 -= 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
            {
                self.cursor.0 += 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) =>
            {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) =>
            {
                self.cursor.1 += 1;
                self.dirty = true;
                return true;
            }
            UIEvent::MailboxUpdate(_)
            | UIEvent::StatusEvent(StatusEvent::NewJob(_))
            | UIEvent::StatusEvent(StatusEvent::JobFinished(_))
            | UIEvent::StatusEvent(StatusEvent::JobCanceled(_)) => {
                self.set_dirty(true);
            }
            _ => {}
        }
        false
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut ret: ShortcutMaps = ShortcutMaps::default();
        ret.insert(
            Shortcuts::GENERAL,
            context.settings.shortcuts.general.key_values(),
        );
        ret
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
