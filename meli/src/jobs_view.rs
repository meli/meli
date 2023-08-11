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

use std::{borrow::Cow, cmp};

use indexmap::IndexMap;

use super::*;
use crate::{
    jobs::{JobId, JobMetadata},
    melib::utils::datetime::{self, formats::RFC3339_DATETIME_AND_SPACE},
};

#[derive(Debug)]
pub struct JobManager {
    cursor_pos: usize,
    new_cursor_pos: usize,
    length: usize,
    data_columns: DataColumns<5>,
    entries: IndexMap<JobId, JobMetadata>,

    initialized: bool,
    theme_default: ThemeAttribute,
    highlight_theme: ThemeAttribute,

    dirty: bool,

    movement: Option<PageMovement>,
    id: ComponentId,
}

impl std::fmt::Display for JobManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "jobs")
    }
}

impl JobManager {
    pub fn new(context: &Context) -> Self {
        let theme_default = crate::conf::value(context, "theme_default");
        let mut data_columns = DataColumns::default();
        data_columns.theme_config.set_single_theme(theme_default);
        Self {
            cursor_pos: 0,
            new_cursor_pos: 0,
            entries: IndexMap::default(),
            length: 0,
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
        self.entries = (*context.main_loop_handler.job_executor.jobs.lock().unwrap()).clone();
        self.length = self.entries.len();
        self.entries.sort_by(|_, a, _, b| a.started.cmp(&b.started));

        self.set_dirty(true);
        let mut min_width = (
            "id".len(),
            "desc".len(),
            "started".len(),
            "finished".len(),
            "succeeded".len(),
            0,
        );

        for c in self.entries.values() {
            /* title */
            min_width.0 = cmp::max(min_width.0, c.id.to_string().len());
            /* desc */
            min_width.1 = cmp::max(min_width.1, c.desc.len());
        }
        min_width.2 = "1970-01-01 00:00:00".len();
        min_width.3 = min_width.2;

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
        self.data_columns.columns[4] =
            CellBuffer::new_with_context(min_width.4, self.length, None, context);

        for (idx, e) in self.entries.values().enumerate() {
            write_string_to_grid(
                &e.id.to_string(),
                &mut self.data_columns.columns[0],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.0, idx)),
                None,
            );

            write_string_to_grid(
                &e.desc,
                &mut self.data_columns.columns[1],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.1, idx)),
                None,
            );

            write_string_to_grid(
                &datetime::timestamp_to_string(e.started, Some(RFC3339_DATETIME_AND_SPACE), true),
                &mut self.data_columns.columns[2],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.2, idx)),
                None,
            );

            write_string_to_grid(
                &if let Some(t) = e.finished {
                    Cow::Owned(datetime::timestamp_to_string(
                        t,
                        Some(RFC3339_DATETIME_AND_SPACE),
                        true,
                    ))
                } else {
                    Cow::Borrowed("null")
                },
                &mut self.data_columns.columns[3],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.3, idx)),
                None,
            );

            write_string_to_grid(
                &if e.finished.is_some() {
                    Cow::Owned(format!("{:?}", e.succeeded))
                } else {
                    Cow::Borrowed("-")
                },
                &mut self.data_columns.columns[4],
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (min_width.4, idx)),
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

impl Component for JobManager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        if !self.initialized {
            self.initialize(context);
        }

        self.draw_list(grid, area, context);
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::value(context, "theme_default");
            self.initialized = false;
            self.set_dirty(true);
        }

        let shortcuts = self.shortcuts(context);
        match event {
            UIEvent::StatusEvent(
                StatusEvent::JobFinished(_) | StatusEvent::JobCanceled(_) | StatusEvent::NewJob(_),
            ) => {
                self.initialized = false;
                self.set_dirty(true);
                return false;
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
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
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
