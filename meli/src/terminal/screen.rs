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

//! Terminal grid cells, keys, colors, etc.
use std::io::{BufWriter, Write};

use melib::{log, uuid};
use termion::{clear, cursor, raw::IntoRawMode, screen::AlternateScreen};

use crate::{
    terminal::{
        cells::CellBuffer, Alignment, BracketModeEnd, BracketModeStart, Cell, Color, DisableMouse,
        DisableSGRMouse, EnableMouse, EnableSGRMouse, Pos, RestoreWindowTitleIconFromStack,
        SaveWindowTitleIconToStack,
    },
    Attr, Context,
};

pub type StateStdout = termion::screen::AlternateScreen<
    termion::raw::RawTerminal<BufWriter<Box<dyn Write + 'static>>>,
>;

type DrawHorizontalSegmentFn = fn(&mut CellBuffer, &mut StateStdout, usize, usize, usize) -> ();

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct ScreenGeneration((u64, u64));

impl ScreenGeneration {
    pub const NIL: Self = Self((0, 0));

    #[inline]
    pub fn next(self) -> Self {
        Self(uuid::Uuid::new_v4().as_u64_pair())
    }
}

impl Default for ScreenGeneration {
    fn default() -> Self {
        Self::NIL
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Virtual;

pub struct Tty {
    stdout: Option<StateStdout>,
    mouse: bool,
    draw_horizontal_segment_fn: DrawHorizontalSegmentFn,
}

impl Tty {
    #[inline]
    pub fn stdout_mut(&mut self) -> Option<&mut StateStdout> {
        self.stdout.as_mut()
    }

    #[inline]
    pub fn draw_fn(&self) -> DrawHorizontalSegmentFn {
        self.draw_horizontal_segment_fn
    }

    #[inline]
    pub fn mouse(&self) -> bool {
        self.mouse
    }

    #[inline]
    pub fn set_mouse(&mut self, mouse: bool) -> &mut Self {
        self.mouse = mouse;
        self
    }

    #[inline]
    pub fn set_draw_fn(
        &mut self,
        draw_horizontal_segment_fn: DrawHorizontalSegmentFn,
    ) -> &mut Self {
        self.draw_horizontal_segment_fn = draw_horizontal_segment_fn;
        self
    }
}

mod private {
    pub trait Sealed {}
}
impl private::Sealed for Virtual {}
impl private::Sealed for Tty {}

pub struct Screen<Display: private::Sealed> {
    cols: usize,
    rows: usize,
    grid: CellBuffer,
    overlay_grid: CellBuffer,
    display: Display,
    generation: ScreenGeneration,
}

impl<D: private::Sealed> std::fmt::Debug for Screen<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct(stringify!(Screen))
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("grid", &self.grid)
            .field("overlay_grid", &self.overlay_grid)
            .field("generation", &self.generation)
            .finish()
    }
}

impl<D: private::Sealed> Screen<D> {
    #[inline]
    pub fn init(display: D) -> Self {
        let area = Area {
            offset: (0, 0),
            upper_left: (0, 0),
            bottom_right: (0, 0),
            empty: true,
            canvas_cols: 0,
            canvas_rows: 0,
            generation: ScreenGeneration::NIL,
        };
        Self {
            cols: 0,
            rows: 0,
            grid: CellBuffer::nil(area),
            overlay_grid: CellBuffer::nil(area),
            display,
            generation: ScreenGeneration::NIL,
        }
    }

    #[inline]
    pub fn with_cols_and_rows(mut self, cols: usize, rows: usize) -> Self {
        self.generation = self.generation.next();
        self.cols = cols;
        self.rows = rows;
        Self {
            cols,
            rows,
            grid: CellBuffer::new(Cell::with_char(' '), self.area()),
            overlay_grid: CellBuffer::new(Cell::with_char(' '), self.area()),
            ..self
        }
    }

    pub const fn area(&self) -> Area {
        let upper_left = (0, 0);
        let bottom_right = (self.cols.saturating_sub(1), self.rows.saturating_sub(1));
        Area {
            offset: upper_left,
            upper_left,
            bottom_right,
            empty: matches!((self.cols, self.rows), (0, 0)),
            canvas_cols: self.cols,
            canvas_rows: self.rows,
            generation: self.generation,
        }
    }

    #[inline]
    pub fn grid(&self) -> &CellBuffer {
        &self.grid
    }

    #[inline]
    pub fn grid_mut(&mut self) -> &mut CellBuffer {
        &mut self.grid
    }

    #[inline]
    pub fn overlay_grid(&self) -> &CellBuffer {
        &self.overlay_grid
    }

    #[inline]
    pub fn overlay_grid_mut(&mut self) -> &mut CellBuffer {
        &mut self.overlay_grid
    }

    #[inline]
    pub fn grid_and_overlay_grid_mut(&mut self) -> (&mut CellBuffer, &mut CellBuffer) {
        (&mut self.grid, &mut self.overlay_grid)
    }

    #[inline(always)]
    pub const fn generation(&self) -> ScreenGeneration {
        self.generation
    }

    #[inline]
    pub const fn cols(&self) -> usize {
        self.cols
    }

    #[inline]
    pub const fn rows(&self) -> usize {
        self.rows
    }
}

impl Clone for Screen<Virtual> {
    fn clone(&self) -> Self {
        Self {
            grid: self.grid.clone(),
            overlay_grid: self.overlay_grid.clone(),
            ..*self
        }
    }
}

impl Screen<Tty> {
    #[inline]
    pub fn new() -> Self {
        Self::init(Tty {
            stdout: None,
            mouse: false,
            draw_horizontal_segment_fn: Self::draw_horizontal_segment,
        })
    }

    #[inline]
    pub fn with_tty(self, display: Tty) -> Self {
        Self { display, ..self }
    }

    #[inline]
    pub fn tty(&self) -> &Tty {
        &self.display
    }

    #[inline]
    pub fn tty_mut(&mut self) -> &mut Tty {
        &mut self.display
    }

    #[inline]
    pub fn draw(&mut self, x_start: usize, x_end: usize, y: usize) {
        let Some(stdout) = self.display.stdout.as_mut() else {
            return;
        };
        (self.display.draw_horizontal_segment_fn)(&mut self.grid, stdout, x_start, x_end, y);
    }

    #[inline]
    pub fn draw_overlay(&mut self, x_start: usize, x_end: usize, y: usize) {
        let Some(stdout) = self.display.stdout.as_mut() else {
            return;
        };
        (self.display.draw_horizontal_segment_fn)(
            &mut self.overlay_grid,
            stdout,
            x_start,
            x_end,
            y,
        );
    }

    /// On `SIGWNICH` the `State` redraws itself according to the new
    /// terminal size.
    pub fn update_size(&mut self) {
        let termsize = termion::terminal_size().ok();
        let termcols = termsize.map(|(w, _)| w);
        let termrows = termsize.map(|(_, h)| h);
        if termcols.unwrap_or(72) as usize != self.cols
            || termrows.unwrap_or(120) as usize != self.rows
        {
            log::trace!(
                "Size updated, from ({}, {}) -> ({:?}, {:?})",
                self.cols,
                self.rows,
                termcols,
                termrows
            );
        }
        let cols = termcols.unwrap_or(72) as usize;
        let rows = termrows.unwrap_or(120) as usize;
        if self.grid.resize(cols, rows, None) && self.overlay_grid.resize(cols, rows, None) {
            self.generation = self.generation.next();
            self.cols = cols;
            self.rows = rows;
            self.grid.area = self.area();
            self.overlay_grid.area = self.area();
        } else {
            log::warn!("Terminal size too big: ({} cols, {} rows)", cols, rows);
        }
    }

    /// Switch back to the terminal's main screen (The command line the user
    /// sees before opening the application)
    pub fn switch_to_main_screen(&mut self) {
        let Some(stdout) = self.display.stdout.as_mut() else {
            return;
        };
        let mouse = self.display.mouse;
        write!(
            stdout,
            "{}{}{}{}{disable_sgr_mouse}{disable_mouse}",
            termion::screen::ToMainScreen,
            cursor::Show,
            RestoreWindowTitleIconFromStack,
            BracketModeEnd,
            disable_sgr_mouse = if mouse { DisableSGRMouse.as_ref() } else { "" },
            disable_mouse = if mouse { DisableMouse.as_ref() } else { "" },
        )
        .unwrap();
        self.flush();
        self.display.stdout = None;
    }

    pub fn switch_to_alternate_screen(&mut self, context: &crate::Context) {
        let mut stdout = BufWriter::with_capacity(
            240 * 80,
            Box::new(std::io::stdout()) as Box<dyn std::io::Write>,
        );

        write!(
            &mut stdout,
            "{save_title_to_stack}{}{}{}{window_title}{}{}{enable_mouse}{enable_sgr_mouse}",
            termion::screen::ToAlternateScreen,
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1),
            BracketModeStart,
            save_title_to_stack = SaveWindowTitleIconToStack,
            window_title = if let Some(ref title) = context.settings.terminal.window_title {
                format!("\x1b]2;{}\x07", title)
            } else {
                String::new()
            },
            enable_mouse = if self.display.mouse {
                EnableMouse.as_ref()
            } else {
                ""
            },
            enable_sgr_mouse = if self.display.mouse {
                EnableSGRMouse.as_ref()
            } else {
                ""
            },
        )
        .unwrap();

        self.display.stdout = Some(AlternateScreen::from(stdout.into_raw_mode().unwrap()));
        self.flush();
    }

    #[inline]
    pub fn flush(&mut self) {
        if let Some(stdout) = self.display.stdout.as_mut() {
            stdout.flush().unwrap();
        }
    }

    /// Draw only a specific `area` on the screen.
    pub fn draw_horizontal_segment(
        grid: &mut CellBuffer,
        stdout: &mut StateStdout,
        x_start: usize,
        x_end: usize,
        y: usize,
    ) {
        write!(
            stdout,
            "{}",
            cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
        )
        .unwrap();
        let mut current_fg = Color::Default;
        let mut current_bg = Color::Default;
        let mut current_attrs = Attr::DEFAULT;
        write!(stdout, "\x1B[m").unwrap();
        for x in x_start..=x_end {
            let c = &grid[(x, y)];
            if c.attrs() != current_attrs {
                c.attrs().write(current_attrs, stdout).unwrap();
                current_attrs = c.attrs();
            }
            if c.bg() != current_bg {
                c.bg().write_bg(stdout).unwrap();
                current_bg = c.bg();
            }
            if c.fg() != current_fg {
                c.fg().write_fg(stdout).unwrap();
                current_fg = c.fg();
            }
            if !c.empty() {
                write!(stdout, "{}", c.ch()).unwrap();
                if c.attrs().intersects(Attr::FORCE_TEXT) {
                    _ = write!(stdout, "\u{FE0E}");
                }
            }
        }
    }

    pub fn draw_horizontal_segment_no_color(
        grid: &mut CellBuffer,
        stdout: &mut StateStdout,
        x_start: usize,
        x_end: usize,
        y: usize,
    ) {
        write!(
            stdout,
            "{}",
            cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
        )
        .unwrap();
        let mut current_attrs = Attr::DEFAULT;
        write!(stdout, "\x1B[m").unwrap();
        for x in x_start..=x_end {
            let c = &grid[(x, y)];
            if c.attrs() != current_attrs {
                c.attrs().write(current_attrs, stdout).unwrap();
                current_attrs = c.attrs();
            }
            if !c.empty() {
                write!(stdout, "{}", c.ch()).unwrap();
                if c.attrs().intersects(Attr::FORCE_TEXT) {
                    _ = write!(stdout, "\u{FE0E}");
                }
            }
        }
    }
}

impl Default for Screen<Virtual> {
    fn default() -> Self {
        Self::new()
    }
}

impl Screen<Virtual> {
    #[inline]
    pub fn new() -> Self {
        Self::init(Virtual)
    }

    #[must_use]
    pub fn resize(&mut self, cols: usize, rows: usize) -> bool {
        if self.grid.resize(cols, rows, None) && self.overlay_grid.resize(cols, rows, None) {
            self.generation = self.generation.next();
            self.cols = cols;
            self.rows = rows;
            self.grid.area = self.area();
            self.overlay_grid.area = self.area();
            return true;
        }

        false
    }

    #[must_use]
    pub fn resize_with_context(&mut self, cols: usize, rows: usize, context: &Context) -> bool {
        if self.grid.resize_with_context(cols, rows, context)
            && self.overlay_grid.resize_with_context(cols, rows, context)
        {
            self.generation = self.generation.next();
            self.cols = cols;
            self.rows = rows;
            self.grid.area = self.area();
            self.overlay_grid.area = self.area();
            return true;
        }

        false
    }
}

/// An `Area` consists of two points: the upper left and bottom right corners.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Area {
    offset: Pos,
    upper_left: Pos,
    bottom_right: Pos,
    empty: bool,
    canvas_cols: usize,
    canvas_rows: usize,
    generation: ScreenGeneration,
}

impl std::fmt::Debug for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct(stringify!(Area))
            .field("width", &self.width())
            .field("height", &self.height())
            .field("offset", &self.offset)
            .field("upper_left", &self.upper_left)
            .field("bottom_right", &self.bottom_right)
            .field("empty flag", &self.empty)
            .field("is_empty", &self.is_empty())
            .field("canvas_cols", &self.canvas_cols)
            .field("canvas_rows", &self.canvas_rows)
            .field("generation", &self.generation)
            .finish()
    }
}

impl<D: private::Sealed> From<&Screen<D>> for Area {
    fn from(sc: &Screen<D>) -> Self {
        sc.area()
    }
}

impl Area {
    #[inline]
    pub fn height(&self) -> usize {
        if self.is_empty() {
            return 0;
        }
        get_y(self.bottom_right).saturating_sub(get_y(self.upper_left)) + 1
    }

    #[inline]
    pub fn width(&self) -> usize {
        if self.is_empty() {
            return 0;
        }
        get_x(self.bottom_right).saturating_sub(get_x(self.upper_left)) + 1
    }

    #[inline]
    pub fn size(&self) -> (usize, usize) {
        (self.width(), self.height())
    }

    /// Get `n`th row of `area` or its last one.
    #[inline]
    pub fn nth_row(&self, n: usize) -> Self {
        let Self {
            offset,
            upper_left,
            bottom_right,
            empty,
            canvas_cols,
            canvas_rows,
            generation,
        } = *self;
        let (_, max_y) = bottom_right;
        let n = std::cmp::min(n, self.height());
        if self.is_empty() || max_y < (get_y(upper_left) + n) {
            return self.into_empty();
        }
        let y = std::cmp::min(max_y, get_y(upper_left) + n);
        Self {
            offset: pos_inc(offset, (0, n)),
            upper_left: set_y(upper_left, y),
            bottom_right: set_y(bottom_right, y),
            empty,
            canvas_cols,
            canvas_rows,
            generation,
        }
    }

    /// Get `n`th col of `area` or its last one.
    #[inline]
    pub fn nth_col(&self, n: usize) -> Self {
        let Self {
            offset,
            upper_left,
            bottom_right,
            empty,
            canvas_cols,
            canvas_rows,
            generation,
        } = *self;
        let (max_x, _) = bottom_right;
        let n = std::cmp::min(n, self.width());
        if self.is_empty() || max_x < (get_x(upper_left) + n) {
            return self.into_empty();
        }
        let x = std::cmp::min(max_x, get_x(upper_left) + n);
        Self {
            offset: pos_inc(offset, (x, 0)),
            upper_left: set_x(upper_left, x),
            bottom_right: set_x(bottom_right, x),
            empty,
            canvas_cols,
            canvas_rows,
            generation,
        }
    }

    /// Place box given by `(width, height)` in corner of `area`
    pub fn place_inside(&self, (width, height): (usize, usize), upper: bool, left: bool) -> Self {
        if self.is_empty() || width < 3 || height < 3 {
            return *self;
        }
        let (upper_x, upper_y) = self.upper_left;
        let (max_x, max_y) = self.bottom_right;
        let x = if upper {
            upper_x + 2
        } else {
            max_x.saturating_sub(2).saturating_sub(width)
        };

        let y = if left {
            upper_y + 2
        } else {
            max_y.saturating_sub(2).saturating_sub(height)
        };
        let upper_left = (std::cmp::min(x, max_x), std::cmp::min(y, max_y));
        let bottom_right = (
            std::cmp::min(x + width, max_x),
            std::cmp::min(y + height, max_y),
        );

        Self {
            offset: pos_inc(
                self.offset,
                (
                    (get_x(upper_left) - get_x(self.upper_left)),
                    (get_y(upper_left) - get_y(self.upper_left)),
                ),
            ),
            upper_left,
            bottom_right,
            empty: self.empty,
            canvas_cols: self.canvas_cols,
            canvas_rows: self.canvas_rows,
            generation: self.generation,
        }
    }

    /// Place given area of dimensions `(width, height)` inside `area` according
    /// to given alignment
    pub fn align_inside(
        &self,
        (width, height): (usize, usize),
        horizontal_alignment: Alignment,
        vertical_alignment: Alignment,
    ) -> Self {
        if self.is_empty() || width == 0 || height == 0 {
            return *self;
        }
        let (top_x, width) = match horizontal_alignment {
            Alignment::Center => (
                { std::cmp::max(self.width() / 2, width / 2) - width / 2 },
                width,
            ),
            Alignment::Start => (0, self.width().min(width)),
            Alignment::End => (self.width().saturating_sub(width), self.width().min(width)),
            Alignment::Fill => (0, self.width()),
        };
        let (top_y, height) = match vertical_alignment {
            Alignment::Center => (
                { std::cmp::max(self.height() / 2, height / 2) - height / 2 },
                self.height().min(height),
            ),
            Alignment::Start => (0, self.height().min(height)),
            Alignment::End => (self.height().saturating_sub(height), self.height()),
            Alignment::Fill => (0, self.height()),
        };

        self.skip(top_x, top_y).take(width, height)
    }

    /// Place box given by `dimensions` in center of `area`
    #[inline]
    pub fn center_inside(&self, dimensions: (usize, usize)) -> Self {
        self.align_inside(dimensions, Alignment::Center, Alignment::Center)
    }

    #[inline]
    pub fn contains(&self, other: Self) -> bool {
        debug_assert_eq!(self.generation, other.generation);
        if self.is_empty() {
            return false;
        } else if other.is_empty() {
            return true;
        }
        get_y(other.bottom_right) <= get_y(self.bottom_right)
            && get_x(other.upper_left) >= get_x(self.upper_left)
            && get_y(other.upper_left) >= get_y(self.upper_left)
            && get_x(other.bottom_right) <= get_x(self.bottom_right)
    }

    /// Skip `n` rows and return the remaining area.
    /// Return value will be an empty area if `n` is more than the height.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use meli::terminal::{Screen, Virtual, Area};
    /// # let mut screen = Screen::<Virtual>::new();
    /// # assert!(screen.resize(120, 20));
    /// # let area = screen.area();
    /// assert_eq!(area.width(), 120);
    /// assert_eq!(area.height(), 20);
    /// // Skip first two rows:
    /// let body = area.skip_rows(2);
    /// assert_eq!(body.height(), 18);
    /// ```
    #[inline]
    pub fn skip_rows(&self, n: usize) -> Self {
        let n = std::cmp::min(n, self.height());
        if self.is_empty() || self.upper_left.1 + n > self.bottom_right.1 {
            return self.into_empty();
        }

        Self {
            offset: pos_inc(self.offset, (0, n)),
            upper_left: pos_inc(self.upper_left, (0, n)),
            ..*self
        }
    }

    /// Skip the last `n` rows and return the remaining area.
    /// Return value will be an empty area if `n` is more than the height.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use meli::terminal::{Screen, Virtual, Area};
    /// # let mut screen = Screen::<Virtual>::new();
    /// # assert!(screen.resize(120, 20));
    /// # let area = screen.area();
    /// assert_eq!(area.width(), 120);
    /// assert_eq!(area.height(), 20);
    /// // Take only first two rows (equivalent to area.take_rows(2))
    /// let header = area.skip_rows_from_end(18);
    /// assert_eq!(header.height(), 2);
    /// assert_eq!(header, area.take_rows(2));
    /// ```
    #[inline]
    pub fn skip_rows_from_end(&self, n: usize) -> Self {
        let n = std::cmp::min(n, self.height());
        if self.is_empty() || self.bottom_right.1 < n {
            return self.into_empty();
        }

        Self {
            bottom_right: (self.bottom_right.0, self.bottom_right.1 - n),
            ..*self
        }
    }

    /// Skip the first `n` rows and return the remaining area.
    /// Return value will be an empty area if `n` is more than the width.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use meli::terminal::{Screen, Virtual, Area};
    /// # let mut screen = Screen::<Virtual>::new();
    /// # assert!(screen.resize(120, 20));
    /// # let area = screen.area();
    /// assert_eq!(area.width(), 120);
    /// assert_eq!(area.height(), 20);
    /// // Skip first two columns
    /// let indent = area.skip_cols(2);
    /// assert_eq!(indent.width(), 118);
    /// ```
    #[inline]
    pub fn skip_cols(&self, n: usize) -> Self {
        let n = std::cmp::min(n, self.width());
        if self.is_empty() || self.bottom_right.0 < self.upper_left.0 + n {
            return self.into_empty();
        }

        Self {
            offset: pos_inc(self.offset, (n, 0)),
            upper_left: pos_inc(self.upper_left, (n, 0)),
            ..*self
        }
    }

    /// Skip the last `n` rows and return the remaining area.
    /// Return value will be an empty area if `n` is more than the width.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use meli::terminal::{Screen, Virtual, Area};
    /// # let mut screen = Screen::<Virtual>::new();
    /// # assert!(screen.resize(120, 20));
    /// # let area = screen.area();
    /// assert_eq!(area.width(), 120);
    /// assert_eq!(area.height(), 20);
    /// // Skip last two columns
    /// let indent = area.skip_cols_from_end(2);
    /// assert_eq!(indent.width(), 118);
    /// assert_eq!(indent, area.take_cols(118));
    /// ```
    #[inline]
    pub fn skip_cols_from_end(&self, n: usize) -> Self {
        let n = std::cmp::min(n, self.width());
        if self.is_empty() || self.bottom_right.0 < n {
            return self.into_empty();
        }
        Self {
            bottom_right: (self.bottom_right.0 - n, self.bottom_right.1),
            ..*self
        }
    }

    /// Shortcut for using `Area::skip_cols` and `Area::skip_rows` together.
    #[inline]
    pub fn skip(&self, n_cols: usize, n_rows: usize) -> Self {
        self.skip_cols(n_cols).skip_rows(n_rows)
    }

    /// Take the first `n` rows and return the remaining area.
    /// Return value will be an empty area if `n` is more than the height.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use meli::terminal::{Screen, Virtual, Area};
    /// # let mut screen = Screen::<Virtual>::new();
    /// # assert!(screen.resize(120, 20));
    /// # let area = screen.area();
    /// assert_eq!(area.width(), 120);
    /// assert_eq!(area.height(), 20);
    /// // Take only first two rows
    /// let header = area.take_rows(2);
    /// assert_eq!(header.height(), 2);
    /// ```
    #[inline]
    pub fn take_rows(&self, n: usize) -> Self {
        let n = std::cmp::min(n, self.height());
        if self.is_empty() || self.bottom_right.1 < (self.height() - n) {
            return self.into_empty();
        }

        Self {
            bottom_right: (
                self.bottom_right.0,
                self.bottom_right.1 - (self.height() - n),
            ),
            ..*self
        }
    }

    /// Take the first `n` columns and return the remaining area.
    /// Return value will be an empty area if `n` is more than the width.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use meli::terminal::{Screen, Virtual, Area};
    /// # let mut screen = Screen::<Virtual>::new();
    /// # assert!(screen.resize(120, 20));
    /// # let area = screen.area();
    /// assert_eq!(area.width(), 120);
    /// assert_eq!(area.height(), 20);
    /// // Take only first two columns
    /// let header = area.take_cols(2);
    /// assert_eq!(header.width(), 2);
    /// ```
    #[inline]
    pub fn take_cols(&self, n: usize) -> Self {
        let n = std::cmp::min(n, self.width());
        if self.is_empty() || self.bottom_right.0 < (self.width() - n) {
            return self.into_empty();
        }

        Self {
            bottom_right: (
                self.bottom_right.0 - (self.width() - n),
                self.bottom_right.1,
            ),
            ..*self
        }
    }

    /// Shortcut for using `Area::take_cols` and `Area::take_rows` together.
    #[inline]
    pub fn take(&self, n_cols: usize, n_rows: usize) -> Self {
        self.take_cols(n_cols).take_rows(n_rows)
    }

    #[inline]
    pub const fn upper_left(&self) -> Pos {
        self.upper_left
    }

    #[inline]
    pub const fn bottom_right(&self) -> Pos {
        self.bottom_right
    }

    #[inline]
    pub const fn upper_right(&self) -> Pos {
        set_x(self.upper_left, get_x(self.bottom_right))
    }

    #[inline]
    pub const fn bottom_left(&self) -> Pos {
        set_y(self.upper_left, get_y(self.bottom_right))
    }

    #[inline]
    pub const fn offset(&self) -> Pos {
        self.offset
    }

    #[inline]
    pub const fn generation(&self) -> ScreenGeneration {
        self.generation
    }

    #[inline]
    pub const fn new_empty(generation: ScreenGeneration) -> Self {
        Self {
            offset: (0, 0),
            upper_left: (0, 0),
            bottom_right: (0, 0),
            canvas_rows: 0,
            canvas_cols: 0,
            empty: true,
            generation,
        }
    }

    #[inline]
    pub const fn into_empty(self) -> Self {
        Self {
            offset: (0, 0),
            upper_left: (0, 0),
            bottom_right: (0, 0),
            empty: true,
            ..self
        }
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.empty
            || (self.upper_left.0 > self.bottom_right.0 || self.upper_left.1 > self.bottom_right.1)
    }
}

#[inline(always)]
const fn pos_inc(p: Pos, inc: (usize, usize)) -> Pos {
    (p.0 + inc.0, p.1 + inc.1)
}

#[inline(always)]
const fn get_x(p: Pos) -> usize {
    p.0
}

#[inline(always)]
const fn get_y(p: Pos) -> usize {
    p.1
}

#[inline(always)]
const fn set_x(p: Pos, new_x: usize) -> Pos {
    (new_x, p.1)
}

#[inline(always)]
const fn set_y(p: Pos, new_y: usize) -> Pos {
    (p.0, new_y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_rows() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 1..=area.height() {
            assert_eq!(area.skip_rows(i).height(), area.height() - i);
            assert!(area.contains(area.skip_rows(i)));
            if i < area.height() {
                assert!(!area.take_rows(i).contains(area.skip_rows(i)));
            } else {
                assert!(area.take_rows(i).contains(area.skip_rows(i)));
            }
        }

        assert!(area.skip_rows(area.height()).is_empty());
        assert_eq!(area.skip_rows(0), area);
    }

    #[test]
    fn test_skip_rows_from_end() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 1..=area.height() {
            assert_eq!(area.skip_rows_from_end(i).height(), area.height() - i);
            assert!(area.contains(area.skip_rows_from_end(i)));
        }

        assert!(area.skip_rows_from_end(area.height()).is_empty());
        assert_eq!(area.skip_rows_from_end(0), area);
    }

    #[test]
    fn test_skip_cols() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 1..=area.width() {
            assert_eq!(area.skip_cols(i).width(), area.width() - i);
            assert!(area.contains(area.skip_cols(i)));
            if i < area.width() {
                assert!(!area.take_cols(i).contains(area.skip_cols(i)));
            } else {
                assert!(area.take_cols(i).contains(area.skip_cols(i)));
            }
        }

        assert!(area.skip_cols(area.width()).is_empty());
        assert_eq!(area.skip_cols(0), area);
    }

    #[test]
    fn test_skip_cols_from_end() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 1..=area.width() {
            assert_eq!(area.skip_cols_from_end(i).width(), area.width() - i);
            assert!(area.contains(area.skip_cols_from_end(i)));
        }

        assert!(area.skip_cols_from_end(area.width()).is_empty());
        assert_eq!(area.skip_cols_from_end(0), area);
    }

    #[test]
    fn test_take_rows() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 1..=area.height() {
            assert_eq!(area.take_rows(i).height(), i);
            assert!(area.contains(area.take_rows(i)));
        }

        assert!(area.take_rows(0).is_empty());
        assert_eq!(area.take_rows(area.height()), area);
    }

    #[test]
    fn test_take_cols() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 1..=area.width() {
            assert_eq!(area.take_cols(i).width(), i);
            assert!(area.contains(area.take_cols(i)));
        }

        assert!(area.take_cols(0).is_empty());
        assert_eq!(area.take_cols(area.width()), area);
    }

    #[test]
    fn test_nth_area() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for i in 0..area.width() {
            assert_eq!(area.nth_col(i).width(), 1);
            assert!(area.contains(area.nth_col(i)));
            if i + 1 == area.width() {
                assert!(area.nth_col(i).contains(area.nth_col(i + 1)));
            } else {
                assert!(!area.nth_col(i).contains(area.nth_col(i + 1)));
            }
        }

        for i in 0..area.height() {
            assert_eq!(area.nth_row(i).height(), 1);
            assert!(area.contains(area.nth_row(i)));
            if i + 1 == area.height() {
                assert!(area.nth_row(i).contains(area.nth_row(i + 1)));
            } else {
                assert!(!area.nth_row(i).contains(area.nth_row(i + 1)));
            }
        }
    }

    #[test]
    fn test_place_inside_area() {
        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        for width in 0..area.width() {
            for height in 0..area.height() {
                for upper in [true, false] {
                    for left in [true, false] {
                        let inner = area.place_inside((width, height), upper, left);
                        assert!(area.contains(inner));
                        if (3..area.height() - 2).contains(&height)
                            && (3..area.width() - 2).contains(&width)
                        {
                            assert_ne!(area, inner);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_align_inside_area() {
        use Alignment::{Center, End, Fill, Start};

        const ALIGNMENTS: [Alignment; 4] = [Fill, Start, End, Center];

        let mut screen = Screen::<Virtual>::new();
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        // Ask for all subsets that area has:
        for width in 1..=area.width() {
            for height in 1..=area.height() {
                for horz in ALIGNMENTS {
                    for vert in ALIGNMENTS {
                        let inner = area.align_inside((width, height), horz, vert);
                        assert!(area.contains(inner));
                        assert!(!inner.is_empty());
                        match (horz, vert) {
                            (Fill, Fill) => {
                                assert_eq!(area, inner);
                            }
                            (Fill, _) => {
                                assert_eq!(inner.width(), area.width());
                                assert_eq!(inner.height(), height);
                            }
                            (_, Fill) => {
                                assert_eq!(inner.height(), area.height());
                                assert_eq!(inner.width(), width);
                            }
                            _ => {
                                assert_eq!((width, height), inner.size());
                                if (width, height) != area.size() {
                                    assert_ne!(area, inner);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Ask for more width/height than area has:
        for width in 1..=(2 * area.width()) {
            for height in 1..=(2 * area.height()) {
                for horz in ALIGNMENTS {
                    for vert in ALIGNMENTS {
                        let inner = area.align_inside((width, height), horz, vert);
                        assert!(area.contains(inner));
                        assert!(!inner.is_empty());
                        match (horz, vert) {
                            (Fill, Fill) => {
                                assert_eq!(area, inner);
                            }
                            (Fill, _) => {
                                assert_eq!(inner.width(), area.width());
                                assert!(
                                    height >= inner.height() && area.height() >= inner.height()
                                );
                            }
                            (_, Fill) => {
                                assert_eq!(inner.height(), area.height());
                                assert!(width >= inner.width() && area.width() >= inner.width());
                            }
                            _ => {
                                assert!(
                                    height >= inner.height() && area.height() >= inner.height()
                                );
                                assert!(width >= inner.width() && area.width() >= inner.width());
                            }
                        }
                    }
                }
            }
        }
    }
}
