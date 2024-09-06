/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

//! Various useful utilities.

use std::collections::HashSet;

use indexmap::IndexMap;
use melib::{text::Reflow, ShellExpandTrait};

use super::*;
use crate::{components::ExtendShortcutsMaps, jobs::JobId, melib::text::TextProcessing};

mod pager;
pub use self::pager::*;

mod text;
pub use self::text::*;

mod widgets;
pub use self::widgets::*;

mod dialogs;
pub use self::dialogs::*;

mod tables;
pub use self::tables::*;

#[cfg(test)]
mod tests;

pub type AutoCompleteFn = Box<dyn Fn(&Context, &str) -> Vec<AutoCompleteEntry> + Send + Sync>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum SearchMovement {
    Previous,
    #[default]
    Next,
    First,
    Last,
}

#[derive(Clone, Debug, Default)]
pub struct SearchPattern {
    pattern: String,
    positions: Vec<(usize, usize)>,
    cursor: usize,
    movement: Option<SearchMovement>,
}

/// Status bar.
#[derive(Debug)]
pub struct StatusBar {
    container: Box<dyn Component>,
    status: String,
    status_message: String,
    substatus_message: String,
    ex_buffer: TextField,
    ex_buffer_cmd_history_pos: Option<usize>,
    display_buffer: String,
    mode: UIMode,
    mouse: bool,
    status_bar_height: usize,
    dirty: bool,
    id: ComponentId,
    progress_spinner: ProgressSpinner,
    in_progress_jobs: HashSet<JobId>,
    done_jobs: HashSet<JobId>,
    scroll_contexts: IndexMap<ComponentId, ScrollContext>,

    auto_complete: Box<AutoComplete>,
    cmd_history: Vec<String>,
}

impl std::fmt::Display for StatusBar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "status bar")
    }
}

impl StatusBar {
    pub fn new(context: &Context, container: Box<dyn Component>) -> Self {
        let mut progress_spinner = ProgressSpinner::new(20, context);
        match context.settings.terminal.progress_spinner_sequence.as_ref() {
            Some(conf::terminal::ProgressSpinnerSequence::Integer(k)) => {
                progress_spinner.set_kind(*k);
            }
            Some(conf::terminal::ProgressSpinnerSequence::Custom {
                ref frames,
                ref interval_ms,
            }) => {
                progress_spinner.set_custom_kind(frames.clone(), *interval_ms);
            }
            None => {}
        }

        Self {
            container,
            status: String::with_capacity(256),
            status_message: String::with_capacity(256),
            substatus_message: String::with_capacity(256),
            ex_buffer: TextField::new(UText::new(String::with_capacity(256)), None),
            ex_buffer_cmd_history_pos: None,
            display_buffer: String::with_capacity(8),
            dirty: true,
            mode: UIMode::Normal,
            mouse: context.settings.terminal.use_mouse.is_true(),
            status_bar_height: 1,
            id: ComponentId::default(),
            auto_complete: AutoComplete::new(Vec::new()),
            progress_spinner,
            in_progress_jobs: HashSet::default(),
            done_jobs: HashSet::default(),
            scroll_contexts: IndexMap::default(),
            cmd_history: crate::command::history::old_cmd_history(),
        }
    }

    fn draw_status_bar(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let mut attribute = crate::conf::value(context, "status.bar");
        if !context.settings.terminal.use_color() {
            attribute.attrs |= Attr::REVERSE;
        }
        grid.clear_area(area, attribute);
        let (x, _) = grid.write_string(
            &self.status,
            attribute.fg,
            attribute.bg,
            attribute.attrs,
            area,
            None,
            None,
        );
        let offset = self.status.find('|').unwrap_or(self.status.len());
        for c in grid.row_iter(area, offset..(area.width()), 0) {
            grid[c].set_attrs(attribute.attrs | Attr::BOLD);
        }
        if let Some((
            _,
            ScrollContext {
                shown_lines,
                total_lines,
                has_more_lines,
            },
        )) = self.scroll_contexts.last()
        {
            let s = format!(
                "| {shown_percentage}% {line_desc}{shown_lines}/{total_lines}{has_more_lines}",
                line_desc = if grid.ascii_drawing { "lines:" } else { "☰ " },
                shown_percentage = (*shown_lines as f32 / (*total_lines as f32) * 100.0) as usize,
                shown_lines = *shown_lines,
                total_lines = *total_lines,
                has_more_lines = if *has_more_lines { "(+)" } else { "" }
            );
            grid.write_string(
                &s,
                attribute.fg,
                attribute.bg,
                attribute.attrs,
                area.skip_cols(x + 1),
                None,
                None,
            );
        }

        if self.progress_spinner.is_dirty() {
            self.progress_spinner.draw(
                grid,
                area.skip_cols(area.width().saturating_sub(self.progress_spinner.width)),
                context,
            );
        }
        let skip = area
            .width()
            .saturating_sub(self.progress_spinner.width + self.display_buffer.len() + 1);
        grid.write_string(
            &self.display_buffer,
            attribute.fg,
            attribute.bg,
            attribute.attrs,
            area.skip_cols(skip),
            None,
            None,
        );

        context.dirty_areas.push_back(area);
    }

    fn update_status(&mut self, context: &Context) {
        self.status = format!(
            "{} {}| {}{}{}",
            self.mode,
            if self.mouse {
                context
                    .settings
                    .terminal
                    .mouse_flag
                    .as_deref()
                    .unwrap_or("🖱️ ")
            } else {
                ""
            },
            &self.status_message,
            if !self.substatus_message.is_empty() {
                " | "
            } else {
                ""
            },
            &self.substatus_message,
        );
    }

    fn draw_command_bar(&self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        grid.clear_area(area, crate::conf::value(context, "theme_default"));
        let command_bar = crate::conf::value(context, "status.command_bar");
        let (_, y) = grid.write_string(
            self.ex_buffer.as_str(),
            command_bar.fg,
            command_bar.bg,
            command_bar.attrs,
            area,
            None,
            None,
        );
        grid.change_theme(area, command_bar);
        if let Some(c) = grid
            .row_iter(area, self.ex_buffer.cursor()..area.width(), y)
            .next()
        {
            grid[c].set_attrs(command_bar.attrs | Attr::UNDERLINE);
        }
        context.dirty_areas.push_back(area);
    }
}

impl Component for StatusBar {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let total_rows = area.height();
        if total_rows <= self.status_bar_height {
            return;
        }

        self.container.draw(
            grid,
            area.take_rows(total_rows.saturating_sub(self.status_bar_height)),
            context,
        );

        self.dirty = false;
        self.draw_status_bar(grid, area.skip_rows(total_rows.saturating_sub(1)), context);

        if self.mode != UIMode::Command && !self.is_dirty() {
            return;
        }
        match self.mode {
            UIMode::Normal => {}
            UIMode::Command => {
                let area = area.nth_row(total_rows.saturating_sub(self.status_bar_height));
                self.draw_command_bar(grid, area, context);
                /* don't autocomplete for less than 3 characters */
                if self.ex_buffer.as_str().split_graphemes().len() <= 2 {
                    return;
                }

                let mut unique_suggestions: HashSet<&str> = HashSet::default();
                let mut suggestions: Vec<AutoCompleteEntry> = self
                    .cmd_history
                    .iter()
                    .rev()
                    .filter_map(|h| {
                        let sug = self.ex_buffer.as_str();
                        if h.starts_with(sug) && !unique_suggestions.contains(sug) {
                            unique_suggestions.insert(sug);
                            Some(h.clone().into())
                        } else {
                            None
                        }
                    })
                    .collect();
                let command_completion_suggestions =
                    crate::command::command_completion_suggestions(self.ex_buffer.as_str());

                suggestions.extend(command_completion_suggestions.iter().filter_map(|e| {
                    if !unique_suggestions.contains(e.as_str()) {
                        unique_suggestions.insert(e.as_str());
                        Some(e.clone().into())
                    } else {
                        None
                    }
                }));
                /*
                suggestions.extend(crate::command::COMMAND_COMPLETION.iter().filter_map(|e| {
                    if e.0.starts_with(self.ex_buffer.as_str()) {
                        Some(e.into())
                    } else {
                        None
                    }
                }));
                */
                if let Some(p) = self.ex_buffer.as_str().split_whitespace().last() {
                    let path = std::path::Path::new(p);
                    suggestions.extend(
                        path.complete(true, p.ends_with('/'))
                            .into_iter()
                            .map(|m| format!("{}{}", self.ex_buffer.as_str(), m).into()),
                    );
                }
                if suggestions.is_empty() && !self.auto_complete.suggestions().is_empty() {
                    self.auto_complete.set_suggestions(suggestions);
                    /* redraw self.container because we have got ridden of an autocomplete
                     * box, and it must be drawn over */
                    self.container.set_dirty(true);
                    return;
                }
                /* redraw self.container because we have less suggestions than before */
                if suggestions.len() < self.auto_complete.suggestions().len() {
                    self.container.set_dirty(true);
                }

                suggestions.sort_by(|a, b| a.entry.cmp(&b.entry));
                suggestions.dedup_by(|a, b| a.entry == b.entry);
                if self.auto_complete.set_suggestions(suggestions) {
                    let len = self.auto_complete.suggestions().len() - 1;
                    self.auto_complete.set_cursor(len);

                    self.container.set_dirty(true);
                }
                /*
                let hist_height = std::cmp::min(15, self.auto_complete.suggestions().len());
                    let hist_area = if status_bar_height < self.auto_complete.suggestions().len() {
                        let hist_area = (
                            (
                                get_x(upper_left),
                                std::cmp::min(
                                    get_y(bottom_right) - status_bar_height - hist_height + 1,
                                    get_y(pos_dec(bottom_right, (0, status_bar_height))),
                                ),
                            ),
                            pos_dec(bottom_right, (0, status_bar_height)),
                        );
                        ScrollBar::default().set_show_arrows(false).draw(
                            grid,
                            hist_area,
                            context,
                            self.auto_complete.cursor(),
                            hist_height,
                            self.auto_complete.suggestions().len(),
                        );
                        grid.change_theme(hist_area, crate::conf::value(context, "status.history"));
                        context.dirty_areas.push_back(hist_area);
                        hist_area
                    } else {
                        (
                            get_x(upper_left),
                            std::cmp::min(
                                get_y(bottom_right) - status_bar_height - hist_height + 1,
                                get_y(pos_dec(bottom_right, (0, status_bar_height))),
                            ),
                        ),
                        pos_dec(bottom_right, (0, status_bar_height)),
                    )
                };
                let offset = if hist_height
                    > (self.auto_complete.suggestions().len() - self.auto_complete.cursor())
                {
                    self.auto_complete.suggestions().len() - hist_height
                } else {
                    self.auto_complete.cursor()
                };

                grid.clear_area(hist_area, crate::conf::value(context, "theme_default"));
                let history_hints = crate::conf::value(context, "status.history.hints");
                if hist_height > 0 {
                    grid.change_theme(hist_area, history_hints);
                }
                for (y_offset, s) in self
                    .auto_complete
                    .suggestions()
                    .iter()
                    .skip(offset)
                    .take(hist_height)
                    .enumerate()
                {
                    let (x, y) = grid.write_string(
                        s.as_str(),
                        history_hints.fg,
                        history_hints.bg,
                        history_hints.attrs,
                        (
                            set_y(
                                upper_left!(hist_area),
                                get_y(bottom_right!(hist_area)) - hist_height + y_offset + 1,
                            ),
                            bottom_right!(hist_area),
                        ),
                        Some(get_x(upper_left!(hist_area))),
                    );
                    grid.write_string(
                        &s.description,
                        history_hints.fg,
                        history_hints.bg,
                        history_hints.attrs,
                        ((x + 2, y), bottom_right!(hist_area)),
                        None,
                    );
                    if y_offset + offset == self.auto_complete.cursor() {
                        grid.change_theme(
                            (
                                get_x(upper_left),
                                std::cmp::min(
                                    get_y(bottom_right) - status_bar_height - hist_height + 1,
                                    get_y(pos_dec(bottom_right, (0, status_bar_height))),
                                ),
                            ),
                            pos_dec(bottom_right, (0, status_bar_height)),
                        )
                    };
                    let offset = if hist_height
                        > (self.auto_complete.suggestions().len() - self.auto_complete.cursor())
                    {
                        self.auto_complete.suggestions().len() - hist_height
                    } else {
                        self.auto_complete.cursor()
                    };
                    grid.clear_area(hist_area, crate::conf::value(context, "theme_default"));
                    let history_hints = crate::conf::value(context, "status.history.hints");
                    if hist_height > 0 {
                        grid.change_theme(hist_area, history_hints);
                    }
                    for (y_offset, s) in self
                        .auto_complete
                        .suggestions()
                        .iter()
                        .skip(offset)
                        .take(hist_height)
                        .enumerate()
                    {
                        let (x, y) = grid.write_string(
                            s.as_str(),
                            grid,
                            history_hints.fg,
                            history_hints.bg,
                            history_hints.attrs,
                            (
                                set_y(
                                    hist_area.upper_left(),
                                    get_y(hist_area.bottom_right()) - hist_height + y_offset + 1,
                                ),
                                hist_area.bottom_right(),
                            ),
                            Some(get_x(hist_area.upper_left())),
                        );
                        grid.write_string(
                            &s.description,
                            grid,
                            history_hints.fg,
                            history_hints.bg,
                            history_hints.attrs,
                            ((x + 2, y), hist_area.bottom_right()),
                            None,
                        );
                        if y_offset + offset == self.auto_complete.cursor() {
                            grid.change_theme(
                                (
                                    set_y(
                                        hist_area.upper_left(),
                                        get_y(hist_area.bottom_right()) - hist_height
                                            + y_offset
                                            + 1,
                                    ),
                                    set_y(
                                        hist_area.bottom_right(),
                                        get_y(hist_area.bottom_right()) - hist_height
                                            + y_offset
                                            + 1,
                                    ),
                                ),
                                history_hints,
                            );
                            grid.write_string(
                                &s.as_str()[self.ex_buffer.as_str().len()..],
                                history_hints.fg,
                                history_hints.bg,
                                history_hints.attrs,
                                (
                                    (
                                        get_x(upper_left)
                                            + self.ex_buffer.as_str().split_graphemes().len(),
                                        get_y(bottom_right) - status_bar_height + 1,
                                    ),
                                    set_y(bottom_right, get_y(bottom_right) - status_bar_height + 1),
                                ),
                                None,
                            );
                        }
                    }
                    context.dirty_areas.push_back(hist_area);
                    */
            }
            _ => {}
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.container.process_event(event, context) {
            return true;
        }

        match event {
            UIEvent::ConfigReload { old_settings: _ } => {
                let mut progress_spinner = ProgressSpinner::new(20, context);
                match context.settings.terminal.progress_spinner_sequence.as_ref() {
                    Some(conf::terminal::ProgressSpinnerSequence::Integer(k)) => {
                        progress_spinner.set_kind(*k);
                    }
                    Some(conf::terminal::ProgressSpinnerSequence::Custom {
                        ref frames,
                        ref interval_ms,
                    }) => {
                        progress_spinner.set_custom_kind(frames.clone(), *interval_ms);
                    }
                    None => {}
                }
                if self.progress_spinner.is_active() {
                    progress_spinner.start();
                }
                self.progress_spinner = progress_spinner;
                self.mouse = context.settings.terminal.use_mouse.is_true();
                self.set_dirty(true);
                self.container.set_dirty(true);
            }
            UIEvent::ChangeMode(m) => {
                let offset = self.status.find('|').unwrap_or(self.status.len());
                self.status.replace_range(
                    ..offset,
                    &format!(
                        "{} {}",
                        m,
                        if self.mouse {
                            context
                                .settings
                                .terminal
                                .mouse_flag
                                .as_deref()
                                .unwrap_or("🖱️ ")
                        } else {
                            ""
                        },
                    ),
                );
                self.set_dirty(true);
                self.container.set_dirty(true);
                self.mode = *m;
                match m {
                    UIMode::Normal => {
                        self.status_bar_height = 1;
                        if !self.ex_buffer.is_empty() {
                            context
                                .replies
                                .push_back(UIEvent::Command(self.ex_buffer.as_str().to_string()));
                        }
                        if parse_command(self.ex_buffer.as_str().as_bytes()).is_ok()
                            && self.cmd_history.last().map(String::as_str)
                                != Some(self.ex_buffer.as_str())
                        {
                            crate::command::history::log_cmd(self.ex_buffer.as_str().to_string());
                            self.cmd_history.push(self.ex_buffer.as_str().to_string());
                        }
                        self.ex_buffer.clear();
                        self.ex_buffer_cmd_history_pos.take();
                    }
                    UIMode::Command => {
                        self.status_bar_height = 2;
                    }
                    _ => {
                        self.status_bar_height = 1;
                    }
                };
            }
            UIEvent::CmdInput(Key::Char('\t')) => {
                if let Some(suggestion) = self.auto_complete.get_suggestion().or_else(|| {
                    if self.auto_complete.cursor() == 0 {
                        self.auto_complete
                            .suggestions()
                            .last()
                            .map(|e| e.entry.clone())
                    } else {
                        None
                    }
                }) {
                    let mut utext = UText::new(suggestion);
                    let len = utext.as_str().len();
                    utext.set_cursor(len);
                    self.container.set_dirty(true);
                    self.set_dirty(true);
                    self.ex_buffer = TextField::new(utext, None);
                }
            }
            UIEvent::CmdInput(Key::Char(c)) => {
                self.dirty = true;
                self.ex_buffer
                    .process_event(&mut UIEvent::InsertInput(Key::Char(*c)), context);
                return true;
            }
            UIEvent::CmdInput(Key::Paste(s)) => {
                self.dirty = true;
                self.ex_buffer
                    .process_event(&mut UIEvent::InsertInput(Key::Paste(s.clone())), context);
                return true;
            }
            UIEvent::CmdInput(Key::Ctrl('u')) => {
                self.dirty = true;
                self.ex_buffer.clear();
                self.ex_buffer_cmd_history_pos.take();
                return true;
            }
            UIEvent::CmdInput(Key::Up) => {
                self.auto_complete.dec_cursor();
                self.dirty = true;
            }
            UIEvent::CmdInput(Key::Down) => {
                self.auto_complete.inc_cursor();
                self.set_dirty(true);
            }
            UIEvent::CmdInput(Key::Left) => {
                self.ex_buffer.cursor_dec();
                self.set_dirty(true);
            }
            UIEvent::CmdInput(Key::Right) => {
                self.ex_buffer.cursor_inc();
                self.set_dirty(true);
            }
            UIEvent::CmdInput(Key::Ctrl('p')) => {
                if self.cmd_history.is_empty() {
                    return true;
                }
                let pos = self.ex_buffer_cmd_history_pos.map(|p| p + 1).unwrap_or(0);
                let pos = std::cmp::min(pos, self.cmd_history.len().saturating_sub(1));
                if Some(pos) != self.ex_buffer_cmd_history_pos {
                    let mut utext = UText::new(
                        self.cmd_history[self.cmd_history.len().saturating_sub(1) - pos].clone(),
                    );
                    let len = utext.as_str().len();
                    utext.set_cursor(len);
                    self.container.set_dirty(true);
                    self.set_dirty(true);
                    self.ex_buffer = TextField::new(utext, None);
                    self.ex_buffer_cmd_history_pos = Some(pos);
                    self.dirty = true;
                }

                return true;
            }
            UIEvent::CmdInput(Key::Ctrl('n')) => {
                if self.cmd_history.is_empty() {
                    return true;
                }
                if Some(0) == self.ex_buffer_cmd_history_pos {
                    self.ex_buffer_cmd_history_pos = None;
                    self.ex_buffer.clear();
                    self.dirty = true;
                } else if let Some(pos) = self.ex_buffer_cmd_history_pos.map(|p| p - 1) {
                    let mut utext = UText::new(
                        self.cmd_history[self.cmd_history.len().saturating_sub(1) - pos].clone(),
                    );
                    let len = utext.as_str().len();
                    utext.set_cursor(len);
                    self.container.set_dirty(true);
                    self.set_dirty(true);
                    self.ex_buffer = TextField::new(utext, None);
                    self.ex_buffer_cmd_history_pos = Some(pos);
                    self.dirty = true;
                }

                return true;
            }
            UIEvent::CmdInput(k @ Key::Backspace) | UIEvent::CmdInput(k @ Key::Ctrl(_)) => {
                self.dirty = true;
                self.ex_buffer
                    .process_event(&mut UIEvent::InsertInput(k.clone()), context);
                return true;
            }
            UIEvent::CmdInput(Key::Esc) => {
                self.ex_buffer.clear();
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.dirty = true;
                return true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::BufClear) => {
                self.display_buffer.clear();
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::BufSet(s)) => {
                self.display_buffer.clone_from(s);
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::UpdateStatus(ref mut s)) => {
                self.status_message.clear();
                self.status_message.push_str(s.as_str());
                self.substatus_message.clear();
                self.update_status(context);
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(ref mut s)) => {
                self.substatus_message.clear();
                self.substatus_message.push_str(s.as_str());
                self.update_status(context);
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::SetMouse(val)) => {
                self.mouse = *val;
                self.update_status(context);
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::JobCanceled(ref job_id))
            | UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)) => {
                self.done_jobs.insert(*job_id);
                self.in_progress_jobs.remove(job_id);
                if self.in_progress_jobs.is_empty() {
                    self.progress_spinner.stop();
                }
                self.progress_spinner.set_dirty(true);
            }
            UIEvent::StatusEvent(StatusEvent::NewJob(ref job_id))
                if !self.done_jobs.contains(job_id) =>
            {
                if self.in_progress_jobs.is_empty() {
                    self.progress_spinner.start();
                }
                self.progress_spinner.set_dirty(true);
                self.in_progress_jobs.insert(*job_id);
            }
            UIEvent::StatusEvent(StatusEvent::ScrollUpdate(ScrollUpdate::End(component_id))) => {
                if self.scroll_contexts.shift_remove(component_id).is_some() {
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::StatusEvent(StatusEvent::ScrollUpdate(ScrollUpdate::Update {
                id,
                context,
            })) => {
                if self.scroll_contexts.insert(*id, *context) != Some(*context) {
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Timer(_) => {
                if self.progress_spinner.process_event(event, context) {
                    return true;
                }
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.container.is_dirty()
            || self.ex_buffer.is_dirty()
            || self.progress_spinner.is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.ex_buffer.set_dirty(value);
        self.progress_spinner.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        self.container.shortcuts(context)
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        self.container.can_quit_cleanly(context)
    }

    fn attributes(&self) -> &'static ComponentAttr {
        &ComponentAttr::CONTAINER
    }

    fn children(&self) -> IndexMap<ComponentId, &dyn Component> {
        let mut ret = IndexMap::default();
        ret.insert(self.container.id(), &self.container as &dyn Component);
        ret.insert(self.ex_buffer.id(), &self.ex_buffer as &dyn Component);
        ret.insert(
            self.progress_spinner.id(),
            &self.progress_spinner as &dyn Component,
        );
        ret
    }

    fn children_mut(&mut self) -> IndexMap<ComponentId, &mut dyn Component> {
        IndexMap::default()
    }

    fn realize(&self, parent: Option<ComponentId>, context: &mut Context) {
        context.realized.insert(self.id(), parent);
        self.container.realize(self.id().into(), context);
        self.progress_spinner.realize(self.id().into(), context);
        self.ex_buffer.realize(self.id().into(), context);
    }

    fn unrealize(&self, context: &mut Context) {
        context.unrealized.insert(self.id());
        self.container.unrealize(context);
        self.progress_spinner.unrealize(context);
        self.ex_buffer.unrealize(context);
    }
}

#[derive(Debug)]
struct HelpView {
    content: Screen<Virtual>,
    cursor: (usize, usize),
    curr_views: ShortcutMaps,
    search: Option<SearchPattern>,
}

#[derive(Debug)]
pub struct Tabbed {
    pinned: usize,
    children: Vec<Box<dyn Component>>,
    cursor_pos: usize,

    show_shortcuts: bool,
    help_view: HelpView,
    theme_default: ThemeAttribute,

    dirty: bool,
    id: ComponentId,
}

impl Tabbed {
    pub fn new(children: Vec<Box<dyn Component>>, context: &Context) -> Self {
        let pinned = children.len();
        let theme_default = crate::conf::value(context, "theme_default");
        let mut ret = Self {
            help_view: HelpView {
                content: Screen::<Virtual>::new(theme_default),
                curr_views: children
                    .first()
                    .map(|c| c.shortcuts(context))
                    .unwrap_or_default(),
                cursor: (0, 0),
                search: None,
            },
            theme_default,
            pinned,
            children,
            cursor_pos: 0,
            show_shortcuts: false,
            dirty: true,
            id: ComponentId::default(),
        };
        ret.help_view
            .curr_views
            .extend_shortcuts(ret.shortcuts(context));
        ret
    }

    fn draw_tabs(&self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let tab_bar_attribute = crate::conf::value(context, "tab.bar");
        grid.clear_area(area, tab_bar_attribute);
        if self.children.is_empty() {
            return;
        }
        let tab_unfocused_attribute = crate::conf::value(context, "tab.unfocused");
        let mut tab_focused_attribute = crate::conf::value(context, "tab.focused");
        if !context.settings.terminal.use_color() {
            tab_focused_attribute.attrs |= Attr::REVERSE;
        }

        let mut x = 0;
        for (idx, c) in self.children.iter().enumerate() {
            let ThemeAttribute { fg, bg, attrs } = if idx == self.cursor_pos {
                tab_focused_attribute
            } else {
                tab_unfocused_attribute
            };
            let name = format!(" {c} ");
            grid.write_string(&name, fg, bg, attrs, area.skip_cols(x), None, None);
            x += name.len() + 1;
            if idx == self.pinned.saturating_sub(1) {
                x += 2;
            }
            if x > area.width() {
                break;
            }
        }
        context.dirty_areas.push_back(area);
    }

    pub fn add_component(&mut self, new: Box<dyn Component>, context: &mut Context) {
        new.realize(self.id().into(), context);
        self.children.push(new);
    }

    fn update_help_curr_views(&mut self, context: &Context) {
        let mut children_maps = self.children[self.cursor_pos].shortcuts(context);
        children_maps.extend_shortcuts(self.shortcuts(context));
        if let Some(i) = children_maps
            .get_index_of(Shortcuts::GENERAL)
            .filter(|i| i + 1 != children_maps.len())
        {
            children_maps.move_index(i, children_maps.len().saturating_sub(1));
        }
        self.help_view.curr_views = children_maps;
    }
}

impl std::fmt::Display for Tabbed {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "tabs")
    }
}

impl Component for Tabbed {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            let first_row = area.nth_row(0);
            grid.clear_area(first_row, crate::conf::value(context, "tab.bar"));
            context.dirty_areas.push_back(first_row);
        }

        /* If children are dirty but self isn't and the shortcuts panel is visible,
         * it will get overwritten. */
        let must_redraw_shortcuts: bool = self.show_shortcuts && !self.dirty && self.is_dirty();

        /* children should be drawn after the shortcuts/help panel lest they
         * overwrite the panel on the grid. the drawing order is determined
         * by the dirty_areas queue which is LIFO */
        if self.children.len() > 1 {
            self.draw_tabs(grid, area.nth_row(0), context);
            self.children[self.cursor_pos].draw(grid, area.skip_rows(1), context);
        } else {
            self.children[self.cursor_pos].draw(grid, area, context);
        }
        let area = area.skip_rows(1);

        if (self.show_shortcuts && self.dirty) || must_redraw_shortcuts {
            let mut children_maps = self.children[self.cursor_pos].shortcuts(context);
            children_maps.extend_shortcuts(self.shortcuts(context));
            if children_maps.is_empty() {
                return;
            }
            if let Some(i) = children_maps
                .get_index_of(Shortcuts::GENERAL)
                .filter(|i| i + 1 != children_maps.len())
            {
                children_maps.move_index(i, children_maps.len().saturating_sub(1));
            }
            if (children_maps == self.help_view.curr_views) && must_redraw_shortcuts {
                let dialog_area = area.align_inside(
                    // add box perimeter padding
                    {
                        let (w, h) = self.help_view.content.area().size();
                        (w + 1, h + 1)
                    },
                    // horizontal
                    Alignment::Center,
                    // vertical
                    Alignment::Center,
                );
                context.dirty_areas.push_back(dialog_area);
                grid.clear_area(dialog_area, self.theme_default);
                let inner_area = create_box(grid, dialog_area);
                let (x, y) = grid.write_string(
                    "shortcuts",
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs | Attr::BOLD,
                    inner_area.skip_cols(2),
                    None,
                    None,
                );
                grid.write_string(
                    &format!(
                        "Press {} to close",
                        children_maps[Shortcuts::GENERAL]["toggle_help"]
                    ),
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs | Attr::ITALICS,
                    inner_area.skip(4 + x, y),
                    None,
                    None,
                );
                let inner_area = inner_area.skip_rows(y + 1).skip_rows_from_end(1);
                let (width, height) = self.help_view.content.grid().size();
                let (cols, rows) = inner_area.size();

                grid.copy_area(
                    self.help_view.content.grid(),
                    inner_area,
                    self.help_view
                        .content
                        .area()
                        .skip(
                            std::cmp::min(
                                (width - 1).saturating_sub(cols),
                                self.help_view.cursor.0,
                            ),
                            std::cmp::min(
                                (height - 1).saturating_sub(rows),
                                self.help_view.cursor.1,
                            ),
                        )
                        .take_rows(rows),
                );
                if height.wrapping_div(rows + 1) > 0 || width.wrapping_div(cols + 1) > 0 {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::Update {
                                id: self.id,
                                context: ScrollContext {
                                    shown_lines: std::cmp::min(
                                        (height).saturating_sub(rows + 1),
                                        self.help_view.cursor.1,
                                    ) + rows,
                                    total_lines: height,
                                    has_more_lines: false,
                                },
                            },
                        )));
                    ScrollBar::default().set_show_arrows(true).draw(
                        grid,
                        inner_area.nth_col(inner_area.width().saturating_sub(1)),
                        context,
                        /* position */
                        std::cmp::min((height).saturating_sub(rows + 1), self.help_view.cursor.1),
                        /* visible_rows */
                        rows,
                        /* length */
                        height,
                    );
                } else {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));
                }
                self.dirty = false;
                return;
            }
            let mut max_length = 6;
            let mut max_width =
                "Press XXXX to close, use COMMAND \"search\" to find shortcuts".len() + 3;

            let mut max_first_column_width = 3;

            for (desc, shortcuts) in children_maps.iter() {
                max_length += shortcuts.len() + 3;
                max_width = std::cmp::max(
                    max_width,
                    std::cmp::max(
                        desc.len(),
                        shortcuts
                            .values()
                            .map(|v| v.to_string().len() + 5)
                            .max()
                            .unwrap_or(0),
                    ),
                );
                max_first_column_width = std::cmp::max(
                    max_first_column_width,
                    shortcuts
                        .values()
                        .map(|v| v.to_string().len() + 5)
                        .max()
                        .unwrap_or(0),
                );
            }
            if !self
                .help_view
                .content
                .resize_with_context(max_width, max_length + 2, context)
            {
                self.dirty = false;
                return;
            }
            self.help_view.content.grid_mut().set_growable(true);
            let help_area = self.help_view.content.area();
            self.help_view.content.grid_mut().write_string(
                "use COMMAND \"search\" to find shortcuts",
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs,
                help_area.skip(2, 1),
                None,
                None,
            );
            let mut idx = 2;
            for (desc, shortcuts) in children_maps.iter() {
                let help_area = self.help_view.content.area();
                self.help_view.content.grid_mut().write_string(
                    desc,
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    help_area.skip(2, 2 + idx),
                    None,
                    None,
                );
                idx += 2;
                for (k, v) in shortcuts {
                    let help_area = self.help_view.content.area();
                    let (x, _) = self.help_view.content.grid_mut().write_string(
                        &format!(
                            "{: >width$}",
                            format!("{}", v),
                            width = max_first_column_width
                        ),
                        self.theme_default.fg,
                        self.theme_default.bg,
                        self.theme_default.attrs | Attr::BOLD,
                        help_area.skip(2, 2 + idx),
                        None,
                        None,
                    );
                    let help_area = self.help_view.content.area();
                    self.help_view.content.grid_mut().write_string(
                        k,
                        self.theme_default.fg,
                        self.theme_default.bg,
                        self.theme_default.attrs,
                        help_area.skip(x + 4, 2 + idx),
                        None,
                        None,
                    );
                    idx += 1;
                }
                idx += 1;
            }
            self.help_view.curr_views = children_maps;
            let dialog_area = area.align_inside(
                // add box perimeter padding
                {
                    let (w, h) = self.help_view.content.area().size();
                    (w + 1, h + 1)
                },
                // horizontal
                Alignment::Center,
                // vertical
                Alignment::Center,
            );
            context.dirty_areas.push_back(dialog_area);
            grid.clear_area(dialog_area, self.theme_default);
            let inner_area = create_box(grid, dialog_area);
            let (x, y) = grid.write_string(
                "shortcuts",
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs | Attr::BOLD,
                inner_area.skip_cols(2),
                None,
                None,
            );
            grid.write_string(
                &format!(
                    "Press {} to close",
                    self.help_view.curr_views[Shortcuts::GENERAL]["toggle_help"]
                ),
                self.theme_default.fg,
                self.theme_default.bg,
                self.theme_default.attrs | Attr::ITALICS,
                inner_area.skip(4 + x, y),
                None,
                None,
            );
            let inner_area = inner_area.skip_rows(y + 1).skip_rows_from_end(1);
            let (width, height) = self.help_view.content.area().size();
            let (cols, rows) = inner_area.size();
            if let Some(ref mut search) = self.help_view.search {
                use crate::melib::text::search::KMP;
                search.positions = self
                    .help_view
                    .content
                    .grid()
                    .kmp_search(&search.pattern)
                    .into_iter()
                    .map(|offset| (offset / width, offset % width))
                    .collect::<Vec<(usize, usize)>>();
                let results_attr = crate::conf::value(context, "pager.highlight_search");
                let results_current_attr =
                    crate::conf::value(context, "pager.highlight_search_current");
                search.cursor =
                    std::cmp::min(search.positions.len().saturating_sub(1), search.cursor);
                for (i, (y, x)) in search.positions.iter().enumerate() {
                    let area = self.help_view.content.area();
                    for c in self.help_view.content.grid().row_iter(
                        area,
                        *x..*x + search.pattern.grapheme_len(),
                        *y,
                    ) {
                        if i == search.cursor {
                            self.help_view.content.grid_mut()[c]
                                .set_fg(results_current_attr.fg)
                                .set_bg(results_current_attr.bg)
                                .set_attrs(results_current_attr.attrs);
                        } else {
                            self.help_view.content.grid_mut()[c]
                                .set_fg(results_attr.fg)
                                .set_bg(results_attr.bg)
                                .set_attrs(results_attr.attrs);
                        }
                    }
                }
                if !search.positions.is_empty() {
                    if let Some(mvm) = search.movement.take() {
                        match mvm {
                            SearchMovement::First => {
                                if self.help_view.cursor.1 > search.positions[search.cursor].0 {
                                    self.help_view.cursor.1 = search.positions[search.cursor].0;
                                }
                                if self.help_view.cursor.1 + rows
                                    < search.positions[search.cursor].0
                                {
                                    self.help_view.cursor.1 = search.positions[search.cursor].0;
                                }
                            }
                            SearchMovement::Previous => {
                                if self.help_view.cursor.1 > search.positions[search.cursor].0 {
                                    self.help_view.cursor.1 = search.positions[search.cursor].0;
                                }
                            }
                            SearchMovement::Next => {
                                if self.help_view.cursor.1 + rows
                                    < search.positions[search.cursor].0
                                {
                                    self.help_view.cursor.1 = search.positions[search.cursor].0;
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            /* trim cursor if it's bigger than the help screen */
            self.help_view.cursor = (
                std::cmp::min((width).saturating_sub(cols), self.help_view.cursor.0),
                std::cmp::min((height).saturating_sub(rows), self.help_view.cursor.1),
            );
            if cols == 0 || rows == 0 {
                return;
            }

            /* In this case we will be scrolling, so show the user how to do it */
            if height.wrapping_div(rows + 1) > 0 || width.wrapping_div(cols + 1) > 0 {
                let help_area = self.help_view.content.area();
                self.help_view.content.grid_mut().write_string(
                    "Use Up, Down, Left, Right to scroll.",
                    self.theme_default.fg,
                    self.theme_default.bg,
                    self.theme_default.attrs | Attr::ITALICS,
                    help_area.skip(2, 2),
                    None,
                    None,
                );
            }

            grid.copy_area(
                self.help_view.content.grid(),
                inner_area,
                self.help_view
                    .content
                    .area()
                    .skip(
                        std::cmp::min((width - 1).saturating_sub(cols), self.help_view.cursor.0),
                        std::cmp::min((height - 1).saturating_sub(rows), self.help_view.cursor.1),
                    )
                    .take_rows(std::cmp::min(rows, height - 1)),
            );
            if height.wrapping_div(rows + 1) > 0 || width.wrapping_div(cols + 1) > 0 {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::Update {
                            id: self.id,
                            context: ScrollContext {
                                shown_lines: std::cmp::min(
                                    (height).saturating_sub(rows),
                                    self.help_view.cursor.1,
                                ) + rows,
                                total_lines: height,
                                has_more_lines: false,
                            },
                        },
                    )));
                ScrollBar::default().set_show_arrows(true).draw(
                    grid,
                    inner_area.nth_col(inner_area.width().saturating_sub(1)),
                    context,
                    /* position */
                    std::cmp::min((height).saturating_sub(rows), self.help_view.cursor.1),
                    /* visible_rows */
                    rows,
                    /* length */
                    height,
                );
            } else {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::End(self.id),
                    )));
            }
        }
        self.dirty = false;
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = &self.help_view.curr_views;
        match &mut event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                self.set_dirty(true);
            }
            UIEvent::Input(Key::Alt(no)) if *no >= '1' && *no <= '9' => {
                let no = *no as usize - '1' as usize;
                if no < self.children.len() && self.cursor_pos != no % self.children.len() {
                    self.children[self.cursor_pos]
                        .process_event(&mut UIEvent::VisibilityChange(false), context);
                    self.cursor_pos = no % self.children.len();
                    self.update_help_curr_views(context);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.children[self.cursor_pos].status(context),
                        )));
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["next_tab"]) =>
            {
                self.children[self.cursor_pos]
                    .process_event(&mut UIEvent::VisibilityChange(false), context);
                self.cursor_pos = (self.cursor_pos + 1) % self.children.len();
                self.update_help_curr_views(context);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.children[self.cursor_pos].status(context),
                    )));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["toggle_help"])
                    || (self.show_shortcuts && key == Key::Esc) =>
            {
                if self.show_shortcuts {
                    // Children below the shortcut overlay must be redrawn.
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));
                }
                self.show_shortcuts = !self.show_shortcuts;
                self.dirty = true;
                return true;
            }
            UIEvent::Action(Tab(New(ref mut e @ Some(_)))) => {
                self.add_component(e.take().unwrap(), context);
                self.children[self.cursor_pos]
                    .process_event(&mut UIEvent::VisibilityChange(false), context);
                self.cursor_pos = self.children.len() - 1;
                self.children[self.cursor_pos].set_dirty(true);
                self.update_help_curr_views(context);
                return true;
            }
            UIEvent::Action(Tab(Close)) => {
                if self.pinned > self.cursor_pos {
                    return true;
                }
                let id = self.children[self.cursor_pos].id();
                self.children[self.cursor_pos].kill(id, context);
                self.update_help_curr_views(context);
                self.set_dirty(true);
                return true;
            }
            UIEvent::Action(Tab(Kill(id))) => {
                if self.pinned > self.cursor_pos {
                    return true;
                }
                if let Some(c_idx) = self.children.iter().position(|x| x.id() == *id) {
                    self.children[c_idx]
                        .process_event(&mut UIEvent::VisibilityChange(false), context);
                    self.children[c_idx].unrealize(context);
                    self.children.remove(c_idx);
                    self.cursor_pos = 0;
                    self.set_dirty(true);
                    self.update_help_curr_views(context);
                    return true;
                } else {
                    log::debug!(
                        "Child component with id {:?} not found.\nList: {:?}",
                        id,
                        self.children
                    );
                }
            }
            UIEvent::Action(Action::Listing(ListingAction::Search(pattern)))
                if self.show_shortcuts =>
            {
                self.help_view.search = Some(SearchPattern {
                    pattern: pattern.to_string(),
                    positions: vec![],
                    cursor: 0,
                    movement: Some(SearchMovement::First),
                });
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["next_search_result"])
                    && self.show_shortcuts
                    && self.help_view.search.is_some() =>
            {
                if let Some(ref mut search) = self.help_view.search {
                    search.movement = Some(SearchMovement::Next);
                    search.cursor += 1;
                } else {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::GENERAL]["previous_search_result"])
                    && self.show_shortcuts
                    && self.help_view.search.is_some() =>
            {
                if let Some(ref mut search) = self.help_view.search {
                    search.movement = Some(SearchMovement::Previous);
                    search.cursor = search.cursor.saturating_sub(1);
                } else {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Esc) if self.show_shortcuts && self.help_view.search.is_some() => {
                self.help_view.search = None;
                self.dirty = true;
                return true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Input(ref key)
                if self.show_shortcuts
                    && shortcut!(key == shortcuts[Shortcuts::LISTING]["search"]) =>
            {
                context
                    .replies
                    .push_back(UIEvent::CmdInput(Key::Paste("search ".to_string())));
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Command));
                return true;
            }
            UIEvent::Input(ref key) if self.show_shortcuts => {
                match key {
                    _ if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_up"]) => {
                        self.help_view.cursor.1 = self.help_view.cursor.1.saturating_sub(1);
                    }
                    _ if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_down"]) => {
                        self.help_view.cursor.1 += 1;
                    }
                    _ if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) => {
                        self.help_view.cursor.0 = self.help_view.cursor.0.saturating_sub(1);
                    }
                    _ if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) => {
                        self.help_view.cursor.0 += 1;
                    }
                    _ => {
                        /* ignore, don't pass to components below the shortcut panel */
                        return false;
                    }
                }
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        let c = self.cursor_pos;
        if let UIEvent::Input(_) | UIEvent::CmdInput(_) | UIEvent::EmbeddedInput(_) = event {
            self.children[c].process_event(event, context)
        } else {
            self.children[c].process_event(event, context)
                || self.children.iter_mut().enumerate().any(|(idx, child)| {
                    if idx == c {
                        return false;
                    }
                    child.process_event(event, context)
                })
        }
    }

    fn is_dirty(&self) -> bool {
        self.dirty || self.children[self.cursor_pos].is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.children[self.cursor_pos].set_dirty(value);
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        map.insert(
            Shortcuts::GENERAL,
            context.settings.shortcuts.general.key_values(),
        );
        map
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        for (i, c) in self.children.iter_mut().enumerate() {
            if !c.can_quit_cleanly(context) {
                self.cursor_pos = i;
                self.set_dirty(true);
                return false;
            }
        }
        true
    }

    fn attributes(&self) -> &'static ComponentAttr {
        &ComponentAttr::CONTAINER
    }

    fn children(&self) -> IndexMap<ComponentId, &dyn Component> {
        let mut ret = IndexMap::default();
        for c in &self.children {
            ret.insert(c.id(), c as &dyn Component);
        }
        ret
    }

    fn children_mut(&mut self) -> IndexMap<ComponentId, &mut dyn Component> {
        IndexMap::default()
    }

    fn realize(&self, parent: Option<ComponentId>, context: &mut Context) {
        context.realized.insert(self.id(), parent);
        for c in &self.children {
            c.realize(self.id().into(), context);
        }
    }

    fn unrealize(&self, context: &mut Context) {
        context
            .replies
            .push_back(UIEvent::ComponentUnrealize(self.id()));
        for c in &self.children {
            c.unrealize(context);
        }
    }
}

/*
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawBuffer {
    pub buf: CellBuffer,
    title: Option<String>,
    cursor: (usize, usize),
    id: ComponentId,
    dirty: bool,
}

impl std::fmt::Display for RawBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("Raw buffer", f)
    }
}

impl Component for RawBuffer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            let (width, height) = self.buf.size();
            let (cols, rows) = (area.width(), area.height());
            self.cursor = (
                std::cmp::min(width.saturating_sub(cols), self.cursor.0),
                std::cmp::min(height.saturating_sub(rows), self.cursor.1),
            );
            grid.clear_area(area, crate::conf::value(context, "theme_default"));

            grid.copy_area(
                &self.buf,
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
            self.dirty = false;
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match *event {
            UIEvent::Input(Key::Left) => {
                self.cursor.0 = self.cursor.0.saturating_sub(1);
                self.dirty = true;
                true
            }
            UIEvent::Input(Key::Right) => {
                self.cursor.0 += 1;
                self.dirty = true;
                true
            }
            UIEvent::Input(Key::Up) => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                true
            }
            UIEvent::Input(Key::Down) => {
                self.cursor.1 += 1;
                self.dirty = true;
                true
            }
            _ => false,
        }
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

impl RawBuffer {
    pub fn new(buf: CellBuffer, title: Option<String>) -> Self {
        RawBuffer {
            buf,
            title,
            cursor: (0, 0),
            dirty: true,
            id: ComponentId::default(),
        }
    }
    pub fn title(&self) -> &str {
        self.title.as_deref().unwrap_or("untitled")
    }
}
*/
