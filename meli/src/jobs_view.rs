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
    melib::{
        utils::datetime::{self, formats::RFC3339_DATETIME_AND_SPACE},
        SortOrder,
    },
};

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
enum Column {
    _0 = 0,
    _1,
    _2,
    _3,
    _4,
}

const fn _assert_len() {
    if JobManager::HEADERS.len() != Column::_4 as usize + 1 {
        panic!("JobManager::HEADERS length changed, please update Column enum accordingly.");
    }
}

const _: () = _assert_len();

#[derive(Debug)]
pub struct JobManager {
    cursor_pos: usize,
    new_cursor_pos: usize,
    length: usize,
    data_columns: DataColumns<5>,
    min_width: [usize; 5],
    sort_col: Column,
    sort_order: SortOrder,
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
    const HEADERS: [&'static str; 5] = ["id", "desc", "started", "finished", "succeeded"];

    pub fn new(context: &Context) -> Self {
        let theme_default = crate::conf::value(context, "theme_default");
        let highlight_theme = if context.settings.terminal.use_color() {
            crate::conf::value(context, "highlight")
        } else {
            ThemeAttribute {
                attrs: Attr::REVERSE,
                ..ThemeAttribute::default()
            }
        };
        let mut data_columns = DataColumns::default();
        data_columns.theme_config.set_single_theme(theme_default);
        Self {
            cursor_pos: 0,
            new_cursor_pos: 0,
            entries: IndexMap::default(),
            length: 0,
            data_columns,
            min_width: [0; 5],
            sort_col: Column::_2,
            sort_order: SortOrder::Desc,
            theme_default,
            highlight_theme,
            initialized: false,
            dirty: true,
            movement: None,
            id: ComponentId::default(),
        }
    }

    fn initialize(&mut self, context: &mut Context) {
        self.set_dirty(true);

        let mut entries = (*context.main_loop_handler.job_executor.jobs.lock().unwrap()).clone();

        self.length = entries.len();
        entries.sort_by(|_, a, _, b| match (self.sort_col, self.sort_order) {
            (Column::_0, SortOrder::Asc) => a.id.cmp(&b.id),
            (Column::_0, SortOrder::Desc) => b.id.cmp(&b.id),
            (Column::_1, SortOrder::Asc) => a.desc.cmp(&b.desc),
            (Column::_1, SortOrder::Desc) => b.desc.cmp(&a.desc),
            (Column::_2, SortOrder::Asc) => a.started.cmp(&b.started),
            (Column::_2, SortOrder::Desc) => b.started.cmp(&a.started),
            (Column::_3, SortOrder::Asc) => a.finished.cmp(&b.finished),
            (Column::_3, SortOrder::Desc) => b.finished.cmp(&a.finished),
            (Column::_4, SortOrder::Asc) if a.finished.is_some() && b.finished.is_some() => {
                a.succeeded.cmp(&b.succeeded)
            }
            (Column::_4, SortOrder::Desc) if a.finished.is_some() && b.finished.is_some() => {
                b.succeeded.cmp(&a.succeeded)
            }
            (Column::_4, SortOrder::Asc) if a.finished.is_none() => std::cmp::Ordering::Less,
            (Column::_4, SortOrder::Asc) => std::cmp::Ordering::Greater,
            (Column::_4, SortOrder::Desc) if a.finished.is_none() => std::cmp::Ordering::Greater,
            (Column::_4, SortOrder::Desc) => std::cmp::Ordering::Less,
        });
        self.entries = entries;

        macro_rules! hdr {
            ($idx:literal) => {{
                Self::HEADERS[$idx].len() + if self.sort_col as u8 == $idx { 1 } else { 0 }
            }};
        }
        self.min_width = [hdr!(0), hdr!(1), hdr!(2), hdr!(3), hdr!(4)];

        for c in self.entries.values() {
            /* title */
            self.min_width[0] = cmp::max(self.min_width[0], c.id.to_string().len());
            /* desc */
            self.min_width[1] = cmp::max(self.min_width[1], c.desc.len());
        }
        self.min_width[2] = "1970-01-01 00:00:00".len();
        self.min_width[3] = self.min_width[2];

        /* name column */
        self.data_columns.columns[0] =
            CellBuffer::new_with_context(self.min_width[0], self.length, None, context);
        /* path column */
        self.data_columns.columns[1] =
            CellBuffer::new_with_context(self.min_width[1], self.length, None, context);
        /* size column */
        self.data_columns.columns[2] =
            CellBuffer::new_with_context(self.min_width[2], self.length, None, context);
        /* subscribed column */
        self.data_columns.columns[3] =
            CellBuffer::new_with_context(self.min_width[3], self.length, None, context);
        self.data_columns.columns[4] =
            CellBuffer::new_with_context(self.min_width[4], self.length, None, context);

        for (idx, e) in self.entries.values().enumerate() {
            self.data_columns.columns[0].write_string_to_grid(
                &e.id.to_string(),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (self.min_width[0], idx)),
                None,
            );

            self.data_columns.columns[1].write_string_to_grid(
                &e.desc,
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (self.min_width[1], idx)),
                None,
            );

            self.data_columns.columns[2].write_string_to_grid(
                &datetime::timestamp_to_string(e.started, Some(RFC3339_DATETIME_AND_SPACE), true),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (self.min_width[2], idx)),
                None,
            );

            self.data_columns.columns[3].write_string_to_grid(
                &if let Some(t) = e.finished {
                    Cow::Owned(datetime::timestamp_to_string(
                        t,
                        Some(RFC3339_DATETIME_AND_SPACE),
                        true,
                    ))
                } else {
                    Cow::Borrowed("null")
                },
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (self.min_width[3], idx)),
                None,
            );

            self.data_columns.columns[4].write_string_to_grid(
                &if e.finished.is_some() {
                    Cow::Owned(format!("{:?}", e.succeeded))
                } else {
                    Cow::Borrowed("-")
                },
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                ((0, idx), (self.min_width[4], idx)),
                None,
            );
        }

        if self.length == 0 {
            let message = "No jobs.".to_string();
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), self.length, None, context);
            self.data_columns.columns[0].write_string_to_grid(
                &message,
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
        /* Page_no has changed, so draw new page */
        _ = self
            .data_columns
            .recalc_widths((width!(area), height!(area)), top_idx);
        grid.clear_area(area, self.theme_default);
        /* copy table columns */
        self.data_columns
            .draw(grid, top_idx, self.cursor_pos, grid.bounds_iter(area));

        /* highlight cursor */

        grid.change_theme(
            nth_row_area(area, self.cursor_pos % rows),
            self.highlight_theme,
        );

        /* clear gap if available height is more than count of entries */
        if top_idx + rows > self.length {
            grid.clear_area(
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
        {
            // Draw column headers.
            let area = nth_row_area(area, 0);
            grid.clear_area(area, self.theme_default);
            let mut x_offset = 0;
            let (upper_left, bottom_right) = area;
            for (i, (h, w)) in Self::HEADERS.iter().zip(self.min_width).enumerate() {
                grid.write_string_to_grid(
                    h,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs | Attr::BOLD,
                    (pos_inc(upper_left, (x_offset, 0)), bottom_right),
                    None,
                );
                if self.sort_col as usize == i {
                    use SortOrder::*;
                    let arrow = match (grid.ascii_drawing, self.sort_order) {
                        (true, Asc) => DataColumns::<5>::ARROW_UP_ASCII,
                        (true, Desc) => DataColumns::<5>::ARROW_DOWN_ASCII,
                        (false, Asc) => DataColumns::<5>::ARROW_UP,
                        (false, Desc) => DataColumns::<5>::ARROW_DOWN,
                    };
                    grid.write_string_to_grid(
                        arrow,
                        self.theme_default.fg,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        (pos_inc(upper_left, (x_offset + h.len(), 0)), bottom_right),
                        None,
                    );
                }
                x_offset += w + 2;
            }
            context.dirty_areas.push_back(area);
        }

        // Draw entry rows.
        if let Some(area) = skip_rows(area, 1) {
            self.draw_list(grid, area, context);
        }
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
            UIEvent::Action(Action::SortColumn(column, order)) => {
                let column = match *column {
                    0 => Column::_0,
                    1 => Column::_1,
                    2 => Column::_2,
                    3 => Column::_3,
                    4 => Column::_4,
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
                    5 => Column::_4,
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
            UIEvent::Resize => {
                self.set_dirty(true);
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
        map[Shortcuts::GENERAL].insert("sort by 1st column", Key::Char('1'));
        map[Shortcuts::GENERAL].insert("sort by 2nd column", Key::Char('2'));
        map[Shortcuts::GENERAL].insert("sort by 3rd column", Key::Char('3'));
        map[Shortcuts::GENERAL].insert("sort by 4th column", Key::Char('4'));
        map[Shortcuts::GENERAL].insert("sort by 5th column", Key::Char('5'));

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, _context: &Context) -> bool {
        true
    }

    fn status(&self, _context: &Context) -> String {
        format!(
            "{} entries. Use `sort <n> [asc/desc]` command or press column index number key \
             (twice to toggle asc/desc) to sort",
            self.entries.len()
        )
    }
}
