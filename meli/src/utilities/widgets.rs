/*
 * meli
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

use std::{borrow::Cow, time::Duration};

use super::*;
use crate::conf::themes::ThemeAttribute;

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct AutoCompleteEntry {
    pub entry: String,
    pub description: Cow<'static, str>,
}

impl AutoCompleteEntry {
    pub fn as_str(&self) -> &str {
        self.entry.as_str()
    }
}

impl From<String> for AutoCompleteEntry {
    fn from(val: String) -> Self {
        Self {
            entry: val,
            description: Cow::Borrowed(""),
        }
    }
}

impl From<(String, &'static str)> for AutoCompleteEntry {
    fn from((entry, b): (String, &'static str)) -> Self {
        Self {
            entry,
            description: Cow::Borrowed(b),
        }
    }
}

impl From<(String, String)> for AutoCompleteEntry {
    fn from((entry, b): (String, String)) -> Self {
        Self {
            entry,
            description: Cow::Owned(b),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AutoComplete {
    entries: Vec<AutoCompleteEntry>,
    cursor: Option<usize>,
    reversed: bool,
    dirty: bool,
    id: ComponentId,
}

impl std::fmt::Display for AutoComplete {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("AutoComplete", f)
    }
}

impl Component for AutoComplete {
    fn draw(&mut self, grid: &mut CellBuffer, mut area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            return;
        };
        self.dirty = false;

        let rows = area.height();
        if rows == 0 {
            return;
        }
        context.dirty_areas.push_back(area);
        let top_idx = self
            .cursor
            .map(|c| c.wrapping_div(rows))
            .map(|page_no| page_no * rows)
            .unwrap_or_default();

        if top_idx + rows > self.entries.len() {
            if self.reversed {
                area = area.skip_rows(rows - (self.entries.len() - top_idx));
            } else {
                area = area.skip_rows_from_end(rows - (self.entries.len() - top_idx));
            }
        }
        let rows = area.height();

        let width = self
            .entries
            .iter()
            .skip(top_idx)
            .take(rows)
            .map(|a| a.entry.grapheme_len() + a.description.as_ref().grapheme_len() + 2)
            .max()
            .unwrap_or(0)
            + 1;
        let area = area.take_cols(width);
        let theme_attr = crate::conf::theme_value(context, "widgets.autocomplete");
        grid.clear_area(area, crate::conf::theme_value(context, "theme_default"));
        grid.change_theme(area, theme_attr);
        let mut rev_iter = self
            .entries
            .iter()
            .enumerate()
            .skip(top_idx)
            .take(rows)
            .rev();
        let mut normal_iter = self.entries.iter().enumerate().skip(top_idx).take(rows);
        let iter: &mut dyn Iterator<Item = (usize, &AutoCompleteEntry)> = if self.reversed {
            &mut rev_iter
        } else {
            &mut normal_iter
        };
        let highlight = crate::conf::theme_value(context, "highlight");
        for (row, (i, e)) in iter.enumerate() {
            let theme_attr = if Some(i) == self.cursor {
                grid.change_theme(area.nth_row(row), highlight);
                highlight
            } else {
                theme_attr
            };
            let (x, _) = grid.write_string(&e.entry, &theme_attr, area.nth_row(row), None, None);
            grid.write_string(
                e.description.as_ref(),
                &(theme_attr | Attr::ITALICS),
                area.nth_row(row).skip_cols(x + 2),
                None,
                None,
            );
        }

        if rows < self.entries.len() {
            let pos = if self.reversed {
                self.entries.len().saturating_sub(top_idx)
            } else {
                top_idx
            };
            ScrollBar { show_arrows: false }.draw(
                grid,
                area.skip_cols(area.width().saturating_sub(1)),
                context,
                if pos <= rows { 0 } else { pos },
                rows,
                self.entries.len(),
            );
        }
    }

    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
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

impl AutoComplete {
    pub fn new(entries: Vec<AutoCompleteEntry>) -> Box<Self> {
        let mut ret = Self {
            entries: Vec::new(),
            cursor: None,
            dirty: true,
            reversed: false,
            id: ComponentId::default(),
        };
        ret.set_suggestions(entries);
        Box::new(ret)
    }

    pub fn set_reversed(&mut self, reversed: bool) {
        self.reversed = reversed;
    }

    pub fn set_suggestions(&mut self, entries: Vec<AutoCompleteEntry>) -> bool {
        if entries.len() == self.entries.len() && entries == self.entries {
            return false;
        }

        self.entries = entries;
        self.cursor = None;
        true
    }

    pub fn inc_cursor(&mut self) {
        if let Some(ref mut cursor) = self.cursor {
            if *cursor + 1 < self.entries.len() {
                *cursor += 1;
                self.set_dirty(true);
            }
        } else if self.entries.is_empty() {
            self.cursor = None;
        } else {
            self.cursor = Some(0);
        }
    }
    pub fn dec_cursor(&mut self) {
        if let Some(ref mut cursor) = self.cursor {
            if *cursor == 0 {
                self.cursor = None;
            } else {
                *cursor -= 1;
            }
            self.set_dirty(true);
        }
    }

    pub fn cursor(&self) -> Option<usize> {
        let val = self.cursor?;
        if self.reversed {
            Some(self.entries.len().saturating_sub(val))
        } else {
            Some(val)
        }
    }

    pub fn get_suggestion(&mut self) -> Option<String> {
        let cursor = self.cursor.take()?;
        if self.entries.is_empty() {
            return None;
        }
        let ret = self.entries.remove(cursor);
        self.entries.clear();
        Some(ret.entry)
    }

    /// Generate a completion to the common prefix of all suggestions.
    pub fn tab(&self) -> Option<String> {
        // https://users.rust-lang.org/t/how-to-find-common-prefix-of-two-byte-slices-effectively/25815/4

        fn mismatch(xs: &[u8], ys: &[u8]) -> usize {
            mismatch_chunks::<128>(xs, ys)
        }

        fn mismatch_chunks<const N: usize>(xs: &[u8], ys: &[u8]) -> usize {
            let off = std::iter::zip(xs.as_chunks::<N>().0, ys.as_chunks::<N>().0)
                .take_while(|(x, y)| x == y)
                .count()
                * N;
            off + std::iter::zip(&xs[off..], &ys[off..])
                .take_while(|(x, y)| x == y)
                .count()
        }

        if self.entries.is_empty() {
            return None;
        }
        if self.entries.len() == 1 {
            return Some(self.entries[0].entry.clone());
        }
        let mut entries = self.entries.clone();
        entries.sort_by(|a, b| a.entry.cmp(&b.entry));
        let mismatch = mismatch(
            entries[0].entry.as_bytes(),
            entries[entries.len() - 1].entry.as_bytes(),
        );

        Some(entries[0].entry[..mismatch].to_string())
    }

    pub fn suggestions(&self) -> &Vec<AutoCompleteEntry> {
        &self.entries
    }
}

/// A widget that draws a scrollbar.
///
/// # Example
///
/// ```rust,no_run
/// # use meli::{Area, Component, CellBuffer, Context, utilities::ScrollBar};
/// // Mock `Component::draw` impl
/// fn draw(grid: &mut CellBuffer, area: Area, context: &mut Context) {
///     let position = 0;
///     let visible_rows = area.height();
///     let scrollbar_area = area.nth_col(area.width());
///     let total_rows = 100;
///     ScrollBar::default().set_show_arrows(true).draw(
///         grid,
///         scrollbar_area,
///         context,
///         position,
///         visible_rows,
///         total_rows,
///     );
/// }
/// ```
#[derive(Clone, Copy, Default)]
pub struct ScrollBar {
    pub show_arrows: bool,
}

impl ScrollBar {
    /// Update `self.show_arrows` field.
    pub fn set_show_arrows(&mut self, new_val: bool) -> &mut Self {
        self.show_arrows = new_val;
        self
    }

    /// Draw `self` vertically.
    pub fn draw(
        &self,
        grid: &mut CellBuffer,
        area: Area,
        context: &Context,
        pos: usize,
        visible_rows: usize,
        length: usize,
    ) {
        if length == 0 {
            return;
        }
        let height = area.height();
        if height < 3 {
            return;
        }
        let theme_default = crate::conf::theme_value(context, "theme_default");
        grid.clear_area(area, theme_default);

        let visible_rows = std::cmp::min(visible_rows, length);
        let ascii_drawing = grid.ascii_drawing;
        let ratio: f64 = (height as f64) / (length as f64);
        let scrollbar_height = (1.max((ratio * (visible_rows as f64)) as usize)).min(height);
        let scrollbar_offset = (height - scrollbar_height).min((ratio * (pos as f64)) as usize);
        let mut area2 = area;

        if self.show_arrows {
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '^' } else { '▀' })
                .set_fg(crate::conf::theme_value(context, "widgets.options.highlighted").bg);
            area2 = area2.skip_rows(1);
        }

        area2 = area2.skip_rows(scrollbar_offset);
        for _ in 0..scrollbar_height {
            if area2.is_empty() {
                break;
            }
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '#' } else { '█' })
                .set_fg(crate::conf::theme_value(context, "widgets.options.highlighted").bg)
                .set_attrs(if !context.settings.terminal.use_color() {
                    theme_default.attrs | Attr::REVERSE
                } else {
                    theme_default.attrs
                });
            area2 = area2.skip_rows(1);
        }
        if self.show_arrows {
            grid[area2.bottom_right()]
                .set_ch(if ascii_drawing { 'v' } else { '▄' })
                .set_fg(crate::conf::theme_value(context, "widgets.options.highlighted").bg)
                .set_bg(crate::conf::theme_value(context, "theme_default").bg);
        }
    }

    /// Draw `self` horizontally.
    pub fn draw_horizontal(
        &self,
        grid: &mut CellBuffer,
        area: Area,
        context: &Context,
        pos: usize,
        visible_cols: usize,
        length: usize,
    ) {
        if length == 0 {
            return;
        }
        let width = area.width();
        if width < 3 {
            return;
        }
        let theme_default = crate::conf::theme_value(context, "theme_default");
        grid.clear_area(area, theme_default);

        let visible_cols = std::cmp::min(visible_cols, length);
        let ascii_drawing = grid.ascii_drawing;
        let ratio: f64 = (width as f64) / (length as f64);
        let scrollbar_width = std::cmp::min((ratio * (visible_cols as f64)) as usize, 1);
        let scrollbar_offset = (ratio * (pos as f64)) as usize;
        let mut area2 = area;

        if self.show_arrows {
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '<' } else { '▐' })
                .set_fg(crate::conf::theme_value(context, "widgets.options.highlighted").bg);
            area2 = area2.skip_cols(1);
        }

        area2 = area2.skip_cols(scrollbar_offset);
        for _ in 0..scrollbar_width {
            if area2.is_empty() {
                break;
            }
            grid[area2.upper_left()]
                .set_ch(if ascii_drawing { '#' } else { '█' })
                .set_fg(crate::conf::theme_value(context, "widgets.options.highlighted").bg)
                .set_attrs(if !context.settings.terminal.use_color() {
                    theme_default.attrs | Attr::REVERSE
                } else {
                    theme_default.attrs
                });
            area2 = area2.skip_cols(1);
        }
        if self.show_arrows {
            grid[area2.bottom_right()]
                .set_ch(if ascii_drawing { '>' } else { '▌' })
                .set_fg(crate::conf::theme_value(context, "widgets.options.highlighted").bg)
                .set_bg(crate::conf::theme_value(context, "theme_default").bg);
        }
    }
}

/// A widget that displays a customizable progress spinner.
///
/// It uses a [`Timer`](crate::jobs::Timer) and each time its timer fires, it
/// cycles to the next stage of its `kind` sequence.
///
/// `kind` is an array of strings/string slices and an
/// [`Duration` interval](std::time::Duration), and each item represents a stage
/// or frame of the progress spinner. For example, a
/// `(Duration::from_millis(130), &["-", "\\", "|", "/"])` value would cycle
/// through the sequence `-`, `\`, `|`, `/`, `-`, `\`, `|` and so on roughly
/// every 130 milliseconds.
///
/// # Example
///
/// ```rust,no_run
/// use std::collections::HashSet;
///
/// use meli::{jobs::JobId, utilities::ProgressSpinner, Component, Context, StatusEvent, UIEvent};
///
/// struct JobMonitoringWidget {
///     progress_spinner: ProgressSpinner,
///     in_progress_jobs: HashSet<JobId>,
/// }
///
/// impl JobMonitoringWidget {
///     fn new(context: &Context, container: Box<dyn Component>) -> Self {
///         let mut progress_spinner = ProgressSpinner::new(20, context);
///         match context.settings.terminal.progress_spinner_sequence.as_ref() {
///             Some(meli::conf::terminal::ProgressSpinnerSequence::Integer(k)) => {
///                 progress_spinner.set_kind(*k);
///             }
///             Some(meli::conf::terminal::ProgressSpinnerSequence::Custom {
///                 ref frames,
///                 ref interval_ms,
///             }) => {
///                 progress_spinner.set_custom_kind(frames.clone(), *interval_ms);
///             }
///             None => {}
///         }
///         Self {
///             progress_spinner,
///             in_progress_jobs: Default::default(),
///         }
///     }
///
///     // Mock `Component::process_event` impl.
///     fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
///         match event {
///             UIEvent::StatusEvent(StatusEvent::JobCanceled(ref job_id))
///             | UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)) => {
///                 self.in_progress_jobs.remove(job_id);
///                 if self.in_progress_jobs.is_empty() {
///                     self.progress_spinner.stop();
///                 }
///                 self.progress_spinner.set_dirty(true);
///                 false
///             }
///             UIEvent::StatusEvent(StatusEvent::NewJob(ref job_id)) => {
///                 if self.in_progress_jobs.is_empty() {
///                     self.progress_spinner.start();
///                 }
///                 self.progress_spinner.set_dirty(true);
///                 self.in_progress_jobs.insert(*job_id);
///                 false
///             }
///             UIEvent::Timer(_) => {
///                 if self.progress_spinner.process_event(event, context) {
///                     return true;
///                 }
///                 false
///             }
///             _ => false,
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct ProgressSpinner {
    timer: crate::jobs::Timer,
    stage: usize,
    pub kind: std::result::Result<usize, Vec<String>>,
    pub width: usize,
    theme_attr: ThemeAttribute,
    active: bool,
    dirty: bool,
    id: ComponentId,
}

impl ProgressSpinner {
    pub const KINDS: [(Duration, &'static [&'static str]); 37] = [
        (Duration::from_millis(130), &["-", "\\", "|", "/"]),
        (Self::INTERVAL, &["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"]),
        (Self::INTERVAL, &["⣀", "⣄", "⣤", "⣦", "⣶", "⣷", "⣿"]),
        (Self::INTERVAL, &["⣀", "⣄", "⣆", "⣇", "⣧", "⣷", "⣿"]),
        (Self::INTERVAL, &["○", "◔", "◐", "◕", "⬤"]),
        (Self::INTERVAL, &["□", "◱", "◧", "▣", "■"]),
        (Self::INTERVAL, &["□", "◱", "▨", "▩", "■"]),
        (Self::INTERVAL, &["□", "◱", "▥", "▦", "■"]),
        (Self::INTERVAL, &["░", "▒", "▓", "█"]),
        (Self::INTERVAL, &["░", "█"]),
        (Self::INTERVAL, &["⬜", "⬛"]),
        (Self::INTERVAL, &["▱", "▰"]),
        (Self::INTERVAL, &["▭", "◼"]),
        (Self::INTERVAL, &["▯", "▮"]),
        (Self::INTERVAL, &["◯", "⬤"]),
        (Self::INTERVAL, &["⚪", "⚫"]),
        (
            Self::INTERVAL,
            &["▖", "▗", "▘", "▝", "▞", "▚", "▙", "▟", "▜", "▛"],
        ),
        (Self::INTERVAL, &["|", "/", "-", "\\"]),
        (Self::INTERVAL, &[".", "o", "O", "@", "*"]),
        (Self::INTERVAL, &["◡◡", "⊙⊙", "◠◠", "⊙⊙"]),
        (Self::INTERVAL, &["◜ ", " ◝", " ◞", "◟ "]),
        (Self::INTERVAL, &["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]),
        (
            Self::INTERVAL,
            &["▁", "▃", "▄", "▅", "▆", "▇", "█", "▇", "▆", "▅", "▄", "▃"],
        ),
        (
            Self::INTERVAL,
            &[
                "▉", "▊", "▋", "▌", "▍", "▎", "▏", "▎", "▍", "▌", "▋", "▊", "▉",
            ],
        ),
        (Self::INTERVAL, &["▖", "▘", "▝", "▗"]),
        (Self::INTERVAL, &["▌", "▀", "▐", "▄"]),
        (Self::INTERVAL, &["┤", "┘", "┴", "└", "├", "┌", "┬", "┐"]),
        (Self::INTERVAL, &["◢", "◣", "◤", "◥"]),
        (Self::INTERVAL, &["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]),
        (
            Self::INTERVAL,
            &["⢎⡰", "⢎⡡", "⢎⡑", "⢎⠱", "⠎⡱", "⢊⡱", "⢌⡱", "⢆⡱"],
        ),
        (Self::INTERVAL, &[".", "o", "O", "°", "O", "o", "."]),
        (Duration::from_millis(100), &["㊂", "㊀", "㊁"]),
        (
            Duration::from_millis(100),
            &["💛 ", "💙 ", "💜 ", "💚 ", "❤️ "],
        ),
        (
            Duration::from_millis(100),
            &[
                "🕛 ", "🕐 ", "🕑 ", "🕒 ", "🕓 ", "🕔 ", "🕕 ", "🕖 ", "🕗 ", "🕘 ", "🕙 ", "🕚 ",
            ],
        ),
        (Duration::from_millis(100), &["🌍 ", "🌎 ", "🌏 "]),
        (
            Duration::from_millis(80),
            &[
                "[    ]", "[=   ]", "[==  ]", "[=== ]", "[ ===]", "[  ==]", "[   =]", "[    ]",
                "[   =]", "[  ==]", "[ ===]", "[====]", "[=== ]", "[==  ]", "[=   ]",
            ],
        ),
        (
            Duration::from_millis(80),
            &["🌑 ", "🌒 ", "🌓 ", "🌔 ", "🌕 ", "🌖 ", "🌗 ", "🌘 "],
        ),
    ];

    pub const INTERVAL_MS: u64 = 50;
    const INTERVAL: std::time::Duration = std::time::Duration::from_millis(Self::INTERVAL_MS);

    /// See source code of [`Self::KINDS`].
    pub fn new(kind: usize, context: &Context) -> Self {
        let kind = kind % Self::KINDS.len();
        let width = Self::KINDS[kind]
            .1
            .iter()
            .map(|f| f.grapheme_len())
            .max()
            .unwrap_or(0);
        let interval = Self::KINDS[kind].0;
        let timer = context
            .main_loop_handler
            .job_executor
            .clone()
            .create_timer(interval, interval);
        let mut theme_attr = crate::conf::theme_value(context, "status.bar");
        if !context.settings.terminal.use_color() {
            theme_attr.attrs |= Attr::REVERSE;
        }
        theme_attr.attrs |= Attr::BOLD;
        Self {
            timer,
            stage: 0,
            kind: Ok(kind),
            width,
            theme_attr,
            dirty: true,
            active: false,
            id: ComponentId::default(),
        }
    }

    #[inline]
    pub fn is_active(&self) -> bool {
        self.active
    }

    /// See source code of [`Self::KINDS`].
    pub fn set_kind(&mut self, kind: usize) {
        self.stage = 0;
        self.width = Self::KINDS[kind % Self::KINDS.len()]
            .1
            .iter()
            .map(|f| f.grapheme_len())
            .max()
            .unwrap_or(0);
        self.kind = Ok(kind % Self::KINDS.len());
        let interval = Self::KINDS[kind % Self::KINDS.len()].0;
        self.timer.set_interval(interval);
        self.dirty = true;
    }

    pub fn set_custom_kind(&mut self, frames: Vec<String>, interval: u64) {
        self.stage = 0;
        self.width = frames.iter().map(|f| f.grapheme_len()).max().unwrap_or(0);
        if self.width == 0 {
            self.stop();
        }
        self.kind = Err(frames);
        self.timer.set_interval(Duration::from_millis(interval));
        self.dirty = true;
    }

    pub fn start(&mut self) {
        if self.width == 0 {
            return;
        }
        self.active = true;
        self.timer.rearm();
    }

    pub fn stop(&mut self) {
        self.active = false;
        self.stage = 0;
        self.timer.disable();
    }
}

impl std::fmt::Display for ProgressSpinner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "progress spinner")
    }
}

impl Component for ProgressSpinner {
    /// Draw current stage, if `self` is dirty.
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            grid.clear_area(area, self.theme_attr);
            if self.active {
                grid.write_string(
                    match self.kind.as_ref() {
                        Ok(kind) => (Self::KINDS[*kind].1)[self.stage],
                        Err(custom) => custom[self.stage].as_ref(),
                    },
                    &self.theme_attr,
                    area,
                    None,
                    None,
                );
            }
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }
    }

    /// If the `event` is our timer firing, proceed to next stage.
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match event {
            UIEvent::Timer(id) if *id == self.timer.id() => {
                match self.kind.as_ref() {
                    Ok(kind) => {
                        self.stage = (self.stage + 1).wrapping_rem(Self::KINDS[*kind].1.len());
                    }
                    Err(custom) => {
                        self.stage = (self.stage + 1).wrapping_rem(custom.len());
                    }
                }
                self.dirty = true;
                true
            }
            _ => false,
        }
    }

    fn set_dirty(&mut self, new_val: bool) {
        self.dirty = new_val;
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
