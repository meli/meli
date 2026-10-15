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

use std::time::Duration;

use crate::components::prelude::*;

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
