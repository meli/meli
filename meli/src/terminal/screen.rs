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

use melib::log;
use termion::{clear, cursor, raw::IntoRawMode, screen::AlternateScreen};

use crate::{
    cells::CellBuffer,
    terminal::{
        BracketModeEnd, BracketModeStart, Color, DisableMouse, DisableSGRMouse, EnableMouse,
        EnableSGRMouse, RestoreWindowTitleIconFromStack, SaveWindowTitleIconToStack,
    },
    Attr,
};

pub type StateStdout =
    termion::screen::AlternateScreen<termion::raw::RawTerminal<BufWriter<std::io::Stdout>>>;

type DrawHorizontalSegmentFn = fn(&mut CellBuffer, &mut StateStdout, usize, usize, usize) -> ();

pub struct Screen {
    pub cols: usize,
    pub rows: usize,
    pub grid: CellBuffer,
    pub overlay_grid: CellBuffer,
    pub stdout: Option<StateStdout>,
    pub mouse: bool,
    pub draw_horizontal_segment_fn: DrawHorizontalSegmentFn,
}

impl Screen {
    /// Switch back to the terminal's main screen (The command line the user
    /// sees before opening the application)
    pub fn switch_to_main_screen(&mut self) {
        let mouse = self.mouse;
        write!(
            self.stdout.as_mut().unwrap(),
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
        self.stdout = None;
    }

    pub fn switch_to_alternate_screen(&mut self, context: &crate::Context) {
        let s = std::io::stdout();
        let s = BufWriter::with_capacity(240 * 80, s);

        let mut stdout = AlternateScreen::from(s.into_raw_mode().unwrap());

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
            enable_mouse = if self.mouse { EnableMouse.as_ref() } else { "" },
            enable_sgr_mouse = if self.mouse {
                EnableSGRMouse.as_ref()
            } else {
                ""
            },
        )
        .unwrap();

        self.stdout = Some(stdout);
        self.flush();
    }

    pub fn flush(&mut self) {
        if let Some(s) = self.stdout.as_mut() {
            s.flush().unwrap();
        }
    }

    pub fn set_mouse(&mut self, value: bool) {
        if let Some(stdout) = self.stdout.as_mut() {
            write!(
                stdout,
                "{mouse}{sgr_mouse}",
                mouse = if value {
                    AsRef::<str>::as_ref(&EnableMouse)
                } else {
                    AsRef::<str>::as_ref(&DisableMouse)
                },
                sgr_mouse = if value {
                    AsRef::<str>::as_ref(&EnableSGRMouse)
                } else {
                    AsRef::<str>::as_ref(&DisableSGRMouse)
                },
            )
            .unwrap();
        }
        self.flush();
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
        self.cols = termcols.unwrap_or(72) as usize;
        self.rows = termrows.unwrap_or(120) as usize;
        if !self.grid.resize(self.cols, self.rows, None) {
            panic!(
                "Terminal size too big: ({} cols, {} rows)",
                self.cols, self.rows
            );
        }
        let _ = self.overlay_grid.resize(self.cols, self.rows, None);
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
            }
        }
    }
}
