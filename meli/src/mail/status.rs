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

use std::borrow::Cow;

use melib::{MailBackendExtensionStatus, SpecialUsageMailbox};

use super::*;
use crate::accounts::JobRequest;

#[derive(Debug)]
pub struct AccountStatus {
    cursor: (usize, usize),
    account_pos: usize,
    content: Screen<Virtual>,
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
    pub fn new(account_pos: usize, theme_default: ThemeAttribute) -> Self {
        let default_cell = {
            let mut ret = Cell::with_char(' ');
            ret.set_fg(theme_default.fg)
                .set_bg(theme_default.bg)
                .set_attrs(theme_default.attrs);
            ret
        };
        let mut content = Screen::<Virtual>::new();
        content.grid_mut().default_cell = default_cell;
        content.grid_mut().set_growable(true);
        _ = content.resize(80, 20);

        Self {
            cursor: (0, 0),
            account_pos,
            content,
            dirty: true,
            theme_default,
            id: ComponentId::default(),
        }
    }

    fn update_content(&mut self, (width, height): (usize, usize), context: &Context) {
        if !self.content.resize_with_context(width, height, context) {
            return;
        }
        let a = &context.accounts[self.account_pos];
        let area = self.content.area().skip_cols(1);
        let (_x, _y) = self.content.grid_mut().write_string(
            "Account ",
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs | Attr::UNDERLINE,
            area,
            None,
        );
        let area = self.content.area().skip(_x + 1, _y);
        let (_x, _y) = self.content.grid_mut().write_string(
            a.name(),
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD | Attr::UNDERLINE,
            area,
            None,
        );
        let mut line = 2;

        let area = self.content.area().skip(1, line);
        self.content.grid_mut().write_string(
            "In-progress jobs:",
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            area,
            None,
        );
        line += 2;

        for (job_id, req) in a.active_jobs.iter() {
            let area = self.content.area().skip(1, line);
            let (x, y) = self.content.grid_mut().write_string(
                &format!("{} {}", req, job_id),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                area,
                None,
            );
            if let JobRequest::DeleteMailbox { mailbox_hash, .. }
            | JobRequest::SetMailboxPermissions { mailbox_hash, .. }
            | JobRequest::SetMailboxSubscription { mailbox_hash, .. }
            | JobRequest::Refresh { mailbox_hash, .. }
            | JobRequest::Fetch { mailbox_hash, .. } = req
            {
                let area = self.content.area().skip(x + 1, y + line);
                self.content.grid_mut().write_string(
                    a.mailbox_entries[mailbox_hash].name(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                );
            }

            line += 1;
        }

        line += 2;

        let area = self.content.area().skip(1, line);
        let (_x, _y) = self.content.grid_mut().write_string(
            "Tag support: ",
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            area,
            None,
        );
        let area = self.content.area().skip(_x + 1, _y + line);
        self.content.grid_mut().write_string(
            if a.backend_capabilities.supports_tags {
                "yes"
            } else {
                "no"
            },
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs,
            area,
            None,
        );
        line += 1;
        let area = self.content.area().skip(1, line);
        let (_x, _y) = self.content.grid_mut().write_string(
            "Search backend: ",
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            area,
            None,
        );
        let area = self.content.area().skip(_x + 1, _y + line);
        self.content.grid_mut().write_string(
            &match (
                a.settings.conf.search_backend(),
                a.backend_capabilities.supports_search,
            ) {
                (SearchBackend::Auto, true) | (SearchBackend::None, true) => {
                    Cow::Borrowed("backend-side search")
                }
                (SearchBackend::Auto, false) | (SearchBackend::None, false) => {
                    Cow::Borrowed("none (search will be slow)")
                }
                #[cfg(feature = "sqlite3")]
                (SearchBackend::Sqlite3, _) => {
                    match crate::sqlite3::AccountCache::db_path(&a.name) {
                        Ok(Some(path)) => {
                            Cow::Owned(format!("sqlite3 database: {}", path.display()))
                        }
                        Ok(None) => Cow::Borrowed("sqlite3 database: uninitialized"),
                        Err(err) => Cow::Owned(format!("sqlite3 error: {err}")),
                    }
                }
            },
            self.theme_default.fg,
            self.theme_default.bg,
            self.theme_default.attrs,
            area,
            None,
        );
        line += 1;

        let area = self.content.area().skip(1, line);
        self.content.grid_mut().write_string(
            "Special Mailboxes:",
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            area,
            None,
        );
        for f in a
            .mailbox_entries
            .values()
            .map(|entry| &entry.ref_mailbox)
            .filter(|f| f.special_usage() != SpecialUsageMailbox::Normal)
        {
            line += 1;
            let area = self.content.area().skip(1, line);
            self.content.grid_mut().write_string(
                &format!("{}: {}", f.path(), f.special_usage()),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                area,
                None,
            );
        }
        line += 2;
        let area = self.content.area().skip(1, line);
        self.content.grid_mut().write_string(
            "Subscribed mailboxes:",
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::BOLD,
            area,
            None,
        );
        line += 2;
        for mailbox_node in a.list_mailboxes() {
            let f: &Mailbox = &a[&mailbox_node.hash].ref_mailbox;
            if f.is_subscribed() {
                let area = self.content.area().skip(1, line);
                self.content.grid_mut().write_string(
                    f.path(),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                );
                line += 1;
            }
        }

        line += 1;
        if let Some(ref extensions) = a.backend_capabilities.extensions {
            let area = self.content.area().skip(1, line);
            self.content.grid_mut().write_string(
                "Server Extensions:",
                self.theme_default.fg,
                self.theme_default.bg,
                Attr::BOLD,
                area,
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
            let area = self.content.area().skip(max_name_width + 6, line);
            self.content.grid_mut().write_string(
                "meli support:",
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                area,
                None,
            );
            line += 1;
            for (name, status) in extensions.iter() {
                let area = self.content.area().skip(1, line);
                self.content.grid_mut().write_string(
                    name.trim_at_boundary(30),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    area,
                    None,
                );

                let (x, y) = {
                    let (status, color) = match status {
                        MailBackendExtensionStatus::Unsupported { comment: _ } => {
                            ("not supported", Color::Red)
                        }
                        MailBackendExtensionStatus::Supported { comment: _ } => {
                            ("supported", Color::Green)
                        }
                        MailBackendExtensionStatus::Enabled { comment: _ } => {
                            ("enabled", Color::Green)
                        }
                    };
                    let area = self.content.area().skip(max_name_width + 6, line);
                    self.content.grid_mut().write_string(
                        status,
                        color,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        area,
                        None,
                    )
                };
                match status {
                    MailBackendExtensionStatus::Unsupported { comment }
                    | MailBackendExtensionStatus::Supported { comment }
                    | MailBackendExtensionStatus::Enabled { comment } => {
                        if let Some(s) = comment {
                            let area = self
                                .content
                                .area()
                                .skip(max_name_width + 6, line)
                                .skip(x, y);
                            let (_x, _y) = self.content.grid_mut().write_string(
                                " (",
                                self.theme_default.fg,
                                self.theme_default.bg,
                                self.theme_default.attrs,
                                area,
                                None,
                            );
                            let area = self
                                .content
                                .area()
                                .skip(max_name_width + 6, line)
                                .skip(x, y)
                                .skip(_x, _y);
                            let (__x, __y) = self.content.grid_mut().write_string(
                                s,
                                self.theme_default.fg,
                                self.theme_default.bg,
                                self.theme_default.attrs,
                                area,
                                None,
                            );
                            let area = self
                                .content
                                .area()
                                .skip(max_name_width + 6, line)
                                .skip(x + _x + __x, y + _y + __y);
                            self.content.grid_mut().write_string(
                                ")",
                                self.theme_default.fg,
                                self.theme_default.bg,
                                self.theme_default.attrs,
                                area,
                                None,
                            );
                        }
                    }
                };
                line += 1;
            }
        }
    }
}

impl Component for AccountStatus {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }
        self.dirty = false;
        self.update_content(area.size(), context);

        /* self.content may have been resized with write_string() calls above
         * since it has growable set */
        let (width, height) = self.content.area().size();
        let (cols, rows) = area.size();
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        grid.clear_area(area, self.theme_default);

        grid.copy_area(
            self.content.grid(),
            area,
            self.content
                .area()
                .skip(self.cursor.0, self.cursor.1)
                .take(cols, rows),
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
