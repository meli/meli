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

//! An `xterm`-compliant terminal, for running terminal applications inside
//! *meli*.
//!
//! ## API
//!
//! This module provides:
//!
//! - [`Terminal`]:
//!   * a struct containing the PID of the child process that talks to this
//!     pseudoterminal.
//!   * a [`std::fs::File`] handle to the child process's standard input stream.
//!   * an [`EmbeddedGrid`] which is a wrapper over [`CellBuffer`] along with
//!     the properties needed to maintain a proper state machine that keeps
//!     track of ongoing escape code operations.
//!
//! ## Creation
//!
//! To create a [`Terminal`], see [`create_pty`].

use std::{fs::File, io::PipeReader, mem::ManuallyDrop};

use melib::text::wcwidth;
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};

use super::*;
use crate::{
    conf::themes::ThemeAttribute,
    terminal::{
        cells::*,
        color::{Color, ColorContrast, RgbColor},
        embedded::escape_codes::{Blink, EscCode, Intensity, Sgr, SgrIter, State, Underline},
        Area, CursorStyle, EscapeSequenceParse as _, Screen, Virtual,
    },
};

#[derive(Clone, Copy, Debug, Default)]
#[repr(u8)]
pub enum ScreenBuffer {
    #[default]
    Normal = 0,
    Alternate,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CodepointBuf {
    None,
    TwoCodepoints(u8),
    ThreeCodepoints(u8, Option<u8>),
    FourCodepoints(u8, Option<u8>, Option<u8>),
}

#[derive(Debug)]
pub struct Terminal {
    pub grid: EmbeddedGrid,
    stdin: ManuallyDrop<File>,
    pub stderr: Option<PipeReader>,
    /// Pid of the embedded process
    pub child_pid: nix::unistd::Pid,
}

impl std::io::Write for Terminal {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        //  Key            Normal     Application
        //  -------------+----------+-------------
        //  Cursor Up    | CSI A    | SS3 A
        //  Cursor Down  | CSI B    | SS3 B
        //  Cursor Right | CSI C    | SS3 C
        //  Cursor Left  | CSI D    | SS3 D
        //  Home         | CSI H    | SS3 H
        //  End          | CSI F    | SS3 F
        //  -------------+----------+-------------
        if self.grid.cursor_key_mode {
            match buf {
                &[0x1b, 0x5b, b'A']
                | &[0x1b, 0x5b, b'B']
                | &[0x1b, 0x5b, b'C']
                | &[0x1b, 0x5b, b'D']
                | &[0x1b, 0x5b, b'H']
                | &[0x1b, 0x5b, b'F'] => {
                    self.stdin.write_all(&[0x1b, 0x4f, buf[2]])?;
                }
                _ => {
                    self.stdin.write_all(buf)?;
                }
            }
        } else {
            self.stdin.write_all(buf)?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdin.flush()
    }
}

impl Terminal {
    pub fn new(stdin: ManuallyDrop<File>, stderr: PipeReader, child_pid: nix::unistd::Pid) -> Self {
        Self {
            grid: EmbeddedGrid::new(),
            stdin,
            stderr: Some(stderr),
            child_pid,
        }
    }

    pub fn set_terminal_size(&mut self, cols: usize, rows: usize) {
        if !self.grid.set_terminal_size(cols, rows) {
            return;
        }
        let winsize = Winsize {
            ws_row: <u16>::try_from(rows).unwrap_or(u16::MAX),
            ws_col: <u16>::try_from(cols).unwrap_or(u16::MAX),
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        let frontend_fd = self.stdin.as_raw_fd();
        // SAFETY: frontend_fd is a valid pty file descriptor, and winsize is a correctly
        // initialized Winsize value
        let _ = unsafe { set_window_size(frontend_fd, &raw const winsize) };
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGWINCH);
    }

    pub fn wake_up(&self) {
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGCONT);
    }

    pub fn stop(&self) {
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGSTOP);
    }

    pub fn terminate(&self) {
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGTERM);
        std::thread::sleep(std::time::Duration::from_millis(150));
        let _ = waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG));
    }

    pub fn is_active(&self) -> std::result::Result<WaitStatus, nix::errno::Errno> {
        waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG))
    }

    pub fn process_byte(&mut self, byte: u8) {
        let Self {
            ref mut grid,
            ref mut stdin,
            stderr: _,
            child_pid: _,
        } = self;
        grid.process_byte(stdin, byte);
    }

    #[inline]
    /// Helper function to get every byte written to `pty_fd` and process it in
    /// the terminal state machine.
    pub fn forward_pty_translate_escape_codes(pty: Arc<Mutex<Self>>, pty_fd: std::fs::File) {
        let mut bytes_iter = std::io::BufReader::new(pty_fd).bytes();
        //log::trace!("waiting for bytes");
        while let Some(Ok(byte)) = bytes_iter.next() {
            // log::trace!("got a byte? {byte} = {:?}", byte as char);
            /* Drink deep, and descend. */
            pty.lock().unwrap().process_byte(byte);
        }
    }
}

/// `EmbeddedGrid` manages the terminal grid state of the embedded process.
///
/// The embedded process sends bytes to the frontend end (see super mod) and
/// interprets them in a state machine stored in `State`. Escape codes are
/// translated as changes to the grid, eg changes in a cell's colors.
///
/// The main process copies the grid whenever the actual terminal is redrawn.
#[derive(Clone, Debug)]
pub struct EmbeddedGrid {
    color_scheme: Option<(ColorContrast, RgbColor, RgbColor)>,
    cursor_color: Option<RgbColor>,
    /// `[top;bottom]`
    scroll_region: ScrollRegion,
    pub alternate_screen: Box<Screen<Virtual>>,
    pub state: State,
    /// (width, height)
    terminal_size: (usize, usize),
    initialized: bool,
    fg_color: Color,
    bg_color: Color,
    underline_color: Color,
    attrs: Attr,
    hyperlink: Option<u64>,
    cursor_key_mode: bool, // (DECCKM)
    origin_mode: bool,
    auto_wrap_mode: bool,
    reverse: bool,
    /// If next grapheme should be placed in the next line
    /// This should be reset whenever the cursor value changes
    wrap_next: bool,
    /// Store state in case a multi-byte character is encountered
    codepoints: CodepointBuf,
    pub normal_screen: Box<Screen<Virtual>>,
    screen_buffer: ScreenBuffer,
    tab_width: u8,
    dirty: bool,
}

impl Default for EmbeddedGrid {
    fn default() -> Self {
        Self::new()
    }
}

impl EmbeddedGrid {
    #[inline]
    pub fn new() -> Self {
        Self {
            scroll_region: ScrollRegion {
                top: 0,
                bottom: 0,
                left: 0,
                ..Default::default()
            },
            color_scheme: None,
            cursor_color: None,
            terminal_size: (0, 0),
            initialized: false,
            alternate_screen: Box::new(Screen::<Virtual>::new(Default::default())),
            state: State::Normal,
            fg_color: Color::Default,
            bg_color: Color::Default,
            underline_color: Color::Default,
            attrs: Attr::DEFAULT,
            hyperlink: None,
            auto_wrap_mode: true,
            reverse: false,
            wrap_next: false,
            cursor_key_mode: false,
            origin_mode: false,
            codepoints: CodepointBuf::None,
            normal_screen: Box::new(Screen::<Virtual>::new(Default::default())),
            screen_buffer: ScreenBuffer::Normal,
            tab_width: 4,
            dirty: true,
        }
    }

    /*
    pub fn to_string_debug(&self) -> String {
        let mut out = String::with_capacity(4096);
        out.push_str(&format!("screen_buffer: {:?}\n", self.screen_buffer));
        let grid = self.buffer();
        for y in 0..self.terminal_size().1 {
            for x in 0..self.terminal_size().0 {
                out.push(grid[(x, y)].ch());
            }
            out.push('\n');
        }
        out
    }
    */

    #[inline]
    pub fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.buffer_mut().set_dirty(true);
    }

    #[inline]
    pub const fn is_dirty(&self) -> bool {
        self.dirty
    }

    pub fn dirty_areas(&mut self) -> SmallVec<[(usize, (usize, usize)); 2]> {
        let mut retval = SmallVec::new();
        let (cols, rows) = self.terminal_size;
        let map = self.buffer().dirty_bitmap();
        for y in 0..rows {
            let mut segment = None;
            for x in 0..cols {
                if map.is_set(x, y) {
                    segment.get_or_insert((x, 0)).1 += 1;
                } else if let Some((start, width)) = segment.take() {
                    retval.push((y, (start, width)));
                }
            }
            if let Some((start, width)) = segment {
                retval.push((y, (start, width)));
            }
        }
        retval
    }

    #[inline]
    pub fn buffer(&self) -> &CellBuffer {
        match self.screen_buffer {
            ScreenBuffer::Normal => self.normal_screen.grid(),
            ScreenBuffer::Alternate => self.alternate_screen.grid(),
        }
    }

    #[inline]
    pub fn buffer_mut(&mut self) -> &mut CellBuffer {
        match self.screen_buffer {
            ScreenBuffer::Normal => self.normal_screen.grid_mut(),
            ScreenBuffer::Alternate => self.alternate_screen.grid_mut(),
        }
    }

    pub fn set_terminal_size(&mut self, cols: usize, rows: usize) -> bool {
        if (cols, rows) == self.terminal_size && self.initialized {
            return false;
        }
        if !self.alternate_screen.resize(cols, rows) {
            return false;
        }
        _ = self.normal_screen.resize(cols, rows);
        self.initialized = true;
        self.scroll_region.top = 0;
        self.scroll_region.bottom = rows.saturating_sub(1);

        self.terminal_size = (cols, rows);

        for screen in [&mut self.normal_screen, &mut self.alternate_screen] {
            screen.grid_mut().cursor.x = 0;
            screen.grid_mut().cursor.y = 0;
        }
        self.wrap_next = false;
        true
    }

    pub fn set_tab_width(&mut self, new_val: u8) {
        self.tab_width = new_val;
    }

    pub fn set_color_scheme(
        &mut self,
        color_scheme: Option<(ColorContrast, RgbColor, RgbColor)>,
    ) -> &mut Self {
        if let Some((_, fg, bg)) = color_scheme {
            if self.fg_color == Color::Default {
                self.fg_color = fg.into();
            }
            if self.bg_color == Color::Default {
                self.bg_color = bg.into();
            }
            if self.underline_color == Color::Default {
                self.underline_color = fg.into();
            }
        }
        self.color_scheme = color_scheme;
        self
    }

    pub fn set_cursor_color(&mut self, cursor_color: Option<RgbColor>) -> &mut Self {
        self.cursor_color = cursor_color;
        self
    }

    #[inline]
    pub const fn terminal_size(&self) -> (usize, usize) {
        self.terminal_size
    }

    #[inline]
    pub const fn area(&self) -> Area {
        match self.screen_buffer {
            ScreenBuffer::Normal => self.normal_screen.area(),
            ScreenBuffer::Alternate => self.alternate_screen.area(),
        }
    }

    pub fn process_byte(&mut self, stdin: &mut std::fs::File, byte: u8) {
        let Self {
            ref mut color_scheme,
            ref cursor_color,
            ref mut scroll_region,
            ref mut terminal_size,
            ref mut alternate_screen,
            state: _,
            ref mut fg_color,
            ref mut bg_color,
            ref mut underline_color,
            ref mut attrs,
            ref mut hyperlink,
            ref mut codepoints,
            ref mut auto_wrap_mode,
            ref mut reverse,
            ref mut wrap_next,
            ref mut cursor_key_mode,
            ref mut origin_mode,
            ref mut screen_buffer,
            ref mut normal_screen,
            initialized: _,
            ref mut dirty,
            ref tab_width,
        } = self;
        let mut screen = normal_screen;

        let is_alternate = match *screen_buffer {
            ScreenBuffer::Normal => false,
            _ => {
                screen = alternate_screen;
                true
            }
        };
        macro_rules! fg {
            () => {
                if *reverse {
                    *bg_color
                } else {
                    *fg_color
                }
            };
        }
        macro_rules! bg {
            () => {
                if *reverse {
                    *fg_color
                } else {
                    *bg_color
                }
            };
        }
        macro_rules! cursor {
            () => {
                screen.grid().cursor
            };
            (mut) => {{
                screen.grid_mut().cursor.dirty = true;
                &mut screen.grid_mut().cursor
            }};
        }
        macro_rules! increase_cursor_y {
            () => {
                cursor!(mut).y += 1;
                if !is_alternate {
                    cursor!(mut).x = 0;
                    if cursor!().y >= terminal_size.1 {
                        if !screen.resize(
                            std::cmp::max(1, screen.grid().cols()),
                            screen.grid().rows() + 2,
                        ) {
                            return;
                        }
                        scroll_region.bottom += 1;
                        terminal_size.1 += 1;
                    }
                }
            };
        }

        macro_rules! increase_cursor_x {
            () => {
                if cursor!().x + 1 < terminal_size.0 {
                    cursor!(mut).x += 1;
                } else if is_alternate && *auto_wrap_mode {
                    *wrap_next = true;
                } else if !is_alternate {
                    cursor!(mut).x = 0;
                    increase_cursor_y!();
                }
            };
        }

        macro_rules! cursor_val {
            () => {{
                let x = if cursor!().x > terminal_size.0 {
                    terminal_size.0.saturating_sub(1)
                } else {
                    cursor!().x
                };
                let y = if is_alternate {
                    std::cmp::min(
                        cursor!().y + scroll_region.top,
                        terminal_size.1.saturating_sub(1),
                    )
                } else {
                    cursor!().y
                };
                (x, y)
            }};
        }

        macro_rules! set_dirty_cell {
            () => {{
                let (x, y) = cursor_val!();
                screen.grid_mut().dirty_bitmap_mut().set(x, y);
                screen.grid_mut()[(x, y)].set_hyperlink(*hyperlink);
                *dirty = true;
            }};
            ($area:expr) => {{
                for row in screen.grid().bounds_iter($area) {
                    for (x, y) in row {
                        screen.grid_mut().dirty_bitmap_mut().set(x, y);
                        screen.grid_mut()[(x, y)].set_hyperlink(*hyperlink);
                    }
                }
                *dirty = true;
            }};
        }

        macro_rules! sgr {
            ($sgr:expr) => {{
                let sgr: Sgr = $sgr;
                match sgr {
                    Sgr::Reset => {
                        *attrs = Attr::DEFAULT;
                        *reverse = false;
                        *fg_color = color_scheme
                            .map(|(_, fg, _)| fg.into())
                            .unwrap_or(Color::Default);
                        *bg_color = color_scheme
                            .map(|(_, _, bg)| bg.into())
                            .unwrap_or(Color::Default);
                        *underline_color = color_scheme
                            .map(|(_, fg, _)| fg.into())
                            .unwrap_or(Color::Default);
                    }
                    Sgr::Intensity(Intensity::Bold) => {
                        /* bold */
                        *attrs |= Attr::BOLD;
                    }
                    Sgr::Intensity(Intensity::Dim) => {
                        /* faint, dim */
                        *attrs |= Attr::DIM;
                    }
                    Sgr::Intensity(Intensity::Normal) => {
                        /* Normal (neither bold nor faint), ECMA-48 3rd. */
                        *attrs &= !(Attr::BOLD | Attr::DIM);
                    }
                    Sgr::Underline(Underline::Single) => {
                        /* underlined */
                        *attrs |= Attr::UNDERLINE;
                    }
                    Sgr::Underline(Underline::Dashed)
                    | Sgr::Underline(Underline::Curly)
                    | Sgr::Underline(Underline::Dotted) => {
                        *attrs |= Attr::UNDERLINE;
                        *attrs |= Attr::UNDERCURL;
                    }
                    Sgr::Underline(Underline::Double) => {
                        /* Doubly-underlined */
                        // [ref:TODO]
                    }
                    Sgr::Underline(Underline::None) => {
                        /* Not underlined, ECMA-48 3rd. */
                        *attrs &= !Attr::UNDERLINE;
                        *attrs &= !Attr::UNDERCURL;
                    }
                    Sgr::Blink(Blink::Slow | Blink::Rapid) => {
                        /* blink */
                        *attrs |= Attr::BLINK;
                    }
                    Sgr::Blink(Blink::None) => {
                        /* Steady (not blinking), ECMA-48 3rd. */
                        *attrs &= !Attr::BLINK;
                    }
                    Sgr::Italic(true) => {
                        /* italicized */
                        *attrs |= Attr::ITALICS;
                    }
                    Sgr::Italic(false) => {
                        /* Not italicized, ECMA-48 3rd */
                        *attrs &= !Attr::ITALICS;
                    }
                    Sgr::Reverse(true) => {
                        /* Inverse */
                        *reverse = true;
                    }
                    Sgr::Reverse(false) => {
                        /* Positive (not inverse), ECMA-48 3rd. */
                        *reverse = false;
                    }
                    Sgr::Invisible(true) => {
                        /* invisible */
                        *attrs |= Attr::HIDDEN;
                    }
                    Sgr::Invisible(false) => {
                        /* Visible, i.e., not hidden, ECMA-48 3rd, VT300. */
                        *attrs &= !Attr::HIDDEN;
                    }
                    Sgr::StrikeThrough(true) => {
                        /* crossed out */
                        // [ref:TODO]
                    }
                    Sgr::StrikeThrough(false) => { /* Not crossed-out, ECMA-48 3rd. */ }
                    Sgr::Overline(true) => {
                        /* overline */
                        // [ref:TODO]
                    }
                    Sgr::Overline(false) => {
                        /* No overline, ECMA-48 3rd. */
                        // [ref:TODO]
                    }
                    Sgr::Font(_) => {
                        // font, ignore
                        // [ref:TODO]
                    }
                    Sgr::VerticalAlign(_) => {
                        // vertical align, ignore
                        // [ref:TODO]
                    }
                    Sgr::ForegroundReset => {
                        *fg_color = color_scheme
                            .map(|(_, fg, _)| fg.into())
                            .unwrap_or(Color::Default);
                    }
                    Sgr::Foreground(color) => *fg_color = color,
                    Sgr::BackgroundReset => {
                        *bg_color = color_scheme
                            .map(|(_, _, bg)| bg.into())
                            .unwrap_or(Color::Default)
                    }
                    Sgr::Background(color) => *bg_color = color,
                    Sgr::UnderlineColorReset => {
                        *underline_color = color_scheme
                            .map(|(_, fg, _)| fg.into())
                            .unwrap_or(Color::Default)
                    }
                    Sgr::UnderlineColor(color) => *underline_color = color,
                }
            }};
        }
        macro_rules! osc {
            ($osc1:expr, $osc2:expr) => {{
                let code = $osc1.as_slice();
                let value = $osc2.as_slice();
                // 10	Foreground color
                // 11	Background color
                // 12	Cursor color
                // 13	Pointer foreground color
                // 14	Pointer background color
                // 15	Tektronix foreground color
                // 16	Tektronix background color
                // 17	Highlight background color
                // 18	Tektronix cursor color
                // 19	Highlight foreground color
                if code == b"11" && value == b"?" {
                    // background query
                    if let Some((_, _, bg)) = *color_scheme {
                        stdin.write_all(b"\x1b]").unwrap();
                        stdin.write_all(&code).unwrap();
                        stdin.write_all(b";").unwrap();
                        write!(stdin, "{}", bg.as_x11_colorspec()).unwrap();
                        stdin.write_all(b"\x1b\\").unwrap();
                        stdin.flush().unwrap();
                    }
                } else if code == b"10" && value == b"?" {
                    // foreground query
                    if let Some((_, fg, _)) = *color_scheme {
                        stdin.write_all(b"\x1b]").unwrap();
                        stdin.write_all(&code).unwrap();
                        stdin.write_all(b";").unwrap();
                        write!(stdin, "{}", fg.as_x11_colorspec()).unwrap();
                        stdin.write_all(b"\x1b\\").unwrap();
                        stdin.flush().unwrap();
                    }
                } else if code == b"12" {
                    if value == b"?" {
                        // cursor color query
                        if let Some(color) = cursor!().color.or_else(|| *cursor_color) {
                            stdin.write_all(b"\x1b]").unwrap();
                            stdin.write_all(&code).unwrap();
                            stdin.write_all(b";").unwrap();
                            write!(stdin, "{}", color.as_x11_colorspec()).unwrap();
                            stdin.write_all(b"\x1b\\").unwrap();
                            stdin.flush().unwrap();
                        }
                    } else {
                        if let Some(color) = RgbColor::parse(&value) {
                            cursor!(mut).color = color.into();
                            set_dirty_cell!();
                        }
                    }
                } else {
                    log::trace!(
                        "osc code ={} value = {}",
                        &String::from_utf8_lossy(code),
                        String::from_utf8_lossy(value)
                    );
                }
            }};
            ($osc1:expr, $osc2:expr, $osc3:expr) => {{
                let code = $osc1.as_slice();
                let value1 = $osc2.as_slice();
                let value2 = $osc3.as_slice();
                if code == b"8" {
                    if !value2.is_empty() {
                        let params = String::from_utf8_lossy(value1);
                        let uri = String::from_utf8_lossy(value2);
                        let uri = if let Some(id) =
                            params.split(":").find_map(|p| p.strip_prefix("id="))
                        {
                            screen.grid_mut().insert_uri_with_id(&uri, id)
                        } else {
                            screen.grid_mut().insert_uri(&uri)
                        };
                        *hyperlink = Some(uri);
                    } else {
                        *hyperlink = None;
                    }
                } else {
                    log::trace!(
                        "osc code ={} value1 = {} value2 = {}",
                        &String::from_utf8_lossy(code),
                        String::from_utf8_lossy(value1),
                        String::from_utf8_lossy(value2),
                    );
                }
            }};
        }

        macro_rules! parse_rgb {
            ($r_buf:expr, $g_buf:expr, $b_buf:expr) => {{
                let r_buf = &$r_buf;
                let g_buf = &$g_buf;
                let b_buf = &$b_buf;

                std::str::from_utf8(r_buf)
                    .ok()
                    .and_then(|r| r.parse::<u8>().ok())
                    .and_then(|r| {
                        Some((
                            r,
                            std::str::from_utf8(g_buf)
                                .ok()
                                .and_then(|g| g.parse::<u8>().ok())?,
                        ))
                    })
                    .and_then(|(r, g)| {
                        Some((
                            r,
                            g,
                            std::str::from_utf8(b_buf)
                                .ok()
                                .and_then(|b| b.parse::<u8>().ok())?,
                        ))
                    })
                    .map(|(r, g, b)| Color::Rgb(RgbColor { r, g, b }))
            }};
        }

        let mut state = &mut self.state;
        match (byte, &mut state) {
            (b'\x1b', State::Normal) => {
                *state = State::ExpectingControlChar;
            }
            (b']', State::ExpectingControlChar) => {
                let buf1 = SmallVec::new();
                *state = State::Osc1([buf1]);
            }
            (b'[', State::ExpectingControlChar) => {
                *state = State::Csi;
            }
            (b'(', State::ExpectingControlChar) => {
                *state = State::G0;
            }
            (b'D', State::ExpectingControlChar) => {
                // ESCD Linefeed
                if cursor!().y == scroll_region.bottom {
                    screen
                        .grid_mut()
                        .scroll_up(scroll_region, scroll_region.top, 1);
                } else {
                    set_dirty_cell!();
                    increase_cursor_y!();
                }
                set_dirty_cell!();
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'J', State::ExpectingControlChar) => {
                // ESCJ Erase from the cursor to the end of the screen
                for y in cursor!().y..terminal_size.1 {
                    for x in cursor!().x..terminal_size.0 {
                        screen.grid_mut()[(x, y)] = Cell::default();
                        screen.grid_mut()[(x, y)]
                            .set_fg(fg!())
                            .set_bg(bg!())
                            .set_underline(*underline_color)
                            .set_attrs(*attrs);
                        screen.grid_mut().dirty_bitmap_mut().set(x, y);
                    }
                }
                *dirty = true;
                *state = State::Normal;
            }
            (b'K', State::ExpectingControlChar) => {
                // ESCK Erase from the cursor to the end of the line
                let y = cursor!().y;
                for x in cursor!().x..terminal_size.0 {
                    screen.grid_mut()[(x, y)] = Cell::default();
                    screen.grid_mut()[(x, y)]
                        .set_fg(fg!())
                        .set_bg(bg!())
                        .set_underline(*underline_color)
                        .set_attrs(*attrs);
                    screen.grid_mut().dirty_bitmap_mut().set(x, y);
                }
                set_dirty_cell!();
                *state = State::Normal;
            }
            (_, State::ExpectingControlChar) => {
                //log::trace!(
                //    "unrecognised: byte is {} and state is {:?}",
                //    byte as char, state
                //);
                *state = State::Normal;
            }
            (b'?', State::Csi) => {
                let buf1 = SmallVec::new();
                *state = State::CsiQ(buf1);
            }
            /* OSC stuff */
            (c, State::Osc1([ref mut buf])) if c.is_ascii_digit() || c == b'?' => {
                buf.push(c);
            }
            (b';', State::Osc1([ref mut buf1_p])) => {
                let buf1 = std::mem::take(buf1_p);
                let buf2 = SmallVec::new();
                *state = State::Osc2([buf1, buf2]);
            }
            (c, State::Osc2([_, ref mut buf])) if c.is_ascii_digit() || c == b'?' => {
                buf.push(c);
            }
            (b';', State::Osc2([ref mut buf1, ref mut buf2])) => {
                let buf1 = std::mem::take(buf1);
                let buf2 = std::mem::take(buf2);
                *state = State::Osc3([buf1, buf2, SmallVec::new()]);
            }
            /* Normal */
            (b'\r', State::Normal) => {
                set_dirty_cell!();
                cursor!(mut).x = 0;
                set_dirty_cell!();
                *wrap_next = false;
            }
            (b'\n', State::Normal) => {
                let y = cursor!().y;
                if y + 1 < terminal_size.1 || !is_alternate {
                    if y == scroll_region.bottom && is_alternate {
                        screen.grid_mut().scroll_up(scroll_region, y, 1);
                    } else {
                        increase_cursor_y!();
                        set_dirty_cell!();
                    }
                }
                *dirty = true;
                *wrap_next = false;
            }
            (b'', State::Normal) => {
                //log::trace!("Visual bell ^G, ignoring {:?}", cursor);
            }
            (0x08, State::Normal) => {
                /* Backspace */
                if cursor!().x > 0 {
                    set_dirty_cell!();
                    cursor!(mut).x -= 1;
                    set_dirty_cell!();
                }
            }
            (c, State::Normal) => {
                /* Character to be printed. */
                let c = if *codepoints == CodepointBuf::None && c & 0x80 == 0 {
                    /* This is a one byte char */
                    c as char
                } else {
                    match codepoints {
                        CodepointBuf::None if c & 0b1110_0000 == 0b1100_0000 => {
                            *codepoints = CodepointBuf::TwoCodepoints(c);
                            return;
                        }
                        CodepointBuf::None if c & 0b1111_0000 == 0b1110_0000 => {
                            *codepoints = CodepointBuf::ThreeCodepoints(c, None);
                            return;
                        }
                        CodepointBuf::None if c & 0b1111_1000 == 0b1111_0000 => {
                            *codepoints = CodepointBuf::FourCodepoints(c, None, None);
                            return;
                        }
                        CodepointBuf::TwoCodepoints(b) => std::str::from_utf8(&[*b, c])
                            .ok()
                            .and_then(|c| c.chars().next())
                            .unwrap_or(' '),
                        CodepointBuf::ThreeCodepoints(b, Some(b1)) => {
                            std::str::from_utf8(&[*b, *b1, c])
                                .ok()
                                .and_then(|c| c.chars().next())
                                .unwrap_or(' ')
                        }
                        CodepointBuf::ThreeCodepoints(_, ref mut b @ None) => {
                            *b = Some(c);
                            return;
                        }
                        CodepointBuf::FourCodepoints(b, Some(b1), Some(b2)) => {
                            std::str::from_utf8(&[*b, *b1, *b2, c])
                                .ok()
                                .and_then(|c| c.chars().next())
                                .unwrap_or(' ')
                        }
                        CodepointBuf::FourCodepoints(_, ref mut b1 @ None, None) => {
                            *b1 = Some(c);
                            return;
                        }
                        CodepointBuf::FourCodepoints(_, _, ref mut b2 @ None) => {
                            *b2 = Some(c);
                            return;
                        }
                        _ => {
                            //log::trace!(
                            //    "invalid utf8 sequence: codepoints = {:?} and c={}",
                            //    codepoints, c
                            //);
                            *codepoints = CodepointBuf::None;
                            return;
                        }
                    }
                };

                *codepoints = CodepointBuf::None;
                if *auto_wrap_mode && *wrap_next {
                    set_dirty_cell!();
                    *wrap_next = false;
                    if cursor!().y == scroll_region.bottom {
                        screen
                            .grid_mut()
                            .scroll_up(scroll_region, scroll_region.top, 1);
                    } else {
                        cursor!(mut).y += 1;
                    }
                    set_dirty_cell!();
                    cursor!(mut).x = 0;
                    set_dirty_cell!();
                }

                if c == '\t' {
                    for _ in 0..*tab_width {
                        set_dirty_cell!();
                        let coords = cursor_val!();
                        screen.grid_mut()[coords]
                            .set_ch(' ')
                            .set_fg(fg!())
                            .set_bg(bg!())
                            .set_underline(*underline_color)
                            .set_attrs(*attrs);
                        increase_cursor_x!();
                        set_dirty_cell!();
                    }
                } else {
                    set_dirty_cell!();
                    let coords = cursor_val!();
                    screen.grid_mut()[coords]
                        .set_ch(c)
                        .set_fg(fg!())
                        .set_bg(bg!())
                        .set_underline(*underline_color)
                        .set_attrs(*attrs);
                    match wcwidth(c) {
                        Some(0) | None => {
                            // Skip drawing zero width characters
                            screen.grid_mut()[coords].set_empty(true);
                        }
                        Some(1) => {}
                        Some(n) => {
                            // Grapheme takes more than one column, so the next cell will be
                            // drawn over. Set it as empty to skip drawing it.
                            for _ in 1..n {
                                increase_cursor_x!();
                                set_dirty_cell!();
                                let coords = cursor_val!();
                                screen.grid_mut()[coords]
                                    .set_empty(true)
                                    .set_fg(fg!())
                                    .set_bg(bg!())
                                    .set_underline(*underline_color)
                                    .set_attrs(*attrs);
                            }
                        }
                    }
                }
                set_dirty_cell!();
                increase_cursor_x!();
                set_dirty_cell!();
            }
            (b'u', State::Csi) => {
                /* restore cursor */
                cursor!(mut).show = true;
                set_dirty_cell!();
                *state = State::Normal;
            }
            (b'm', State::Csi) => {
                /* Reset character Attributes (SGR).  Ps = 0  -> Normal (default), VT100 */
                *fg_color = color_scheme
                    .map(|(_, fg, _)| fg.into())
                    .unwrap_or(Color::Default);
                *bg_color = color_scheme
                    .map(|(_, _, bg)| bg.into())
                    .unwrap_or(Color::Default);
                *underline_color = color_scheme
                    .map(|(_, fg, _)| fg.into())
                    .unwrap_or(Color::Default);
                *attrs = Attr::DEFAULT;
                *reverse = false;
                *state = State::Normal;
            }
            (b'C', State::Csi) => {
                // ESC[C    CSI Cursor Forward one Time
                cursor!(mut).x = std::cmp::min(cursor!().x + 1, terminal_size.0.saturating_sub(1));
                set_dirty_cell!();
                *wrap_next = false;
                *state = State::Normal;
            }
            /* CSI ? stuff */
            (c, State::CsiQ(ref mut buf)) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (b'h', State::CsiQ(ref buf)) => {
                match buf.as_slice() {
                    b"1" => {
                        *cursor_key_mode = true;
                    }
                    b"6" => {
                        *origin_mode = true;
                    }
                    b"7" => {
                        *auto_wrap_mode = true;
                    }
                    b"25" => {
                        cursor!(mut).show = true;
                        set_dirty_cell!();
                    }
                    b"1047" | b"1049" => {
                        set_dirty_cell!(screen.grid().area());
                        *screen_buffer = ScreenBuffer::Alternate;
                    }
                    b"1002" => {
                        // Button-event tracking, ignore.
                    }
                    b"1004" => {
                        // Set focus event mouse, ignore.
                    }
                    b"1005" => {
                        // Set ext mode mouse, ignore.
                    }
                    b"1006" => {
                        // Set sgr ext mode mouse, ignore.
                    }
                    b"2004" => {
                        // Set bracketed pasting, ignore.
                    }
                    _ => {
                        log::trace!("unknown csi? {:?}", String::from_utf8_lossy(buf.as_slice()));
                    }
                }

                *state = State::Normal;
            }
            (b'l', State::CsiQ(ref mut buf)) => {
                match buf.as_slice() {
                    b"1" => {
                        *cursor_key_mode = false;
                    }
                    b"6" => {
                        *origin_mode = false;
                    }
                    b"7" => {
                        *auto_wrap_mode = false;
                    }
                    b"12" => {
                        // Stop blinking cursor, ignore
                    }
                    b"25" => {
                        cursor!(mut).show = false;
                        set_dirty_cell!();
                    }
                    b"1047" | b"1049" => {
                        set_dirty_cell!(screen.grid().area());
                        *screen_buffer = ScreenBuffer::Normal;
                    }
                    b"1002" => {
                        // Button-event tracking, ignore.
                    }
                    b"1004" => {
                        // Set focus event mouse, ignore.
                    }
                    b"1005" => {
                        // Set ext mode mouse, ignore.
                    }
                    b"1006" => {
                        // Set sgr ext mode mouse, ignore.
                    }
                    b"2004" => {
                        // Set bracketed pasting, ignore.
                    }
                    _ => {
                        log::trace!(
                            "unknown csi? `l` {:?}",
                            String::from_utf8_lossy(buf.as_slice())
                        );
                    }
                }
                *state = State::Normal;
            }
            (b'$', State::CsiQ(ref mut buf)) => {
                let buf = std::mem::take(buf);
                *state = State::DECRQMDEC(buf);
            }
            (b'p', State::DECRQMDEC(ref mut buf)) => {
                // https://vt100.net/docs/vt510-rm/DECRPM.html
                // CSI ? Pd ; Ps $ y
                let buf = std::mem::take(buf);
                stdin.write_all(b"\x1b[?").unwrap();
                stdin.write_all(&buf).unwrap();
                stdin.write_all(b";0$y").unwrap();
                stdin.flush().unwrap();
                *state = State::Normal;
            }
            /* END OF CSI ? stuff */
            (c, State::Csi) if c.is_ascii_digit() => {
                let mut buf1 = SmallVec::new();
                buf1.push(c);
                *state = State::Csi1([buf1]);
            }
            (b'J', State::Csi) => {
                /* Erase in Display (ED), VT100. */
                /* Erase Below (default). */

                let area = screen.area();
                let area = area.skip_rows(std::cmp::min(
                    cursor!().y + 1 + scroll_region.top,
                    terminal_size.1.saturating_sub(1),
                ));
                screen.grid_mut().clear_area(
                    area,
                    ThemeAttribute {
                        fg: fg!(),
                        bg: bg!(),
                        underline: *underline_color,
                        attrs: *attrs,
                    },
                );
                set_dirty_cell!(area);
                *state = State::Normal;
            }
            (b'K', State::Csi) => {
                /* Erase in Line (ED), VT100. */
                /* Erase to right (Default) */
                let y = cursor!().y;
                for x in cursor!().x..terminal_size.0 {
                    screen.grid_mut()[(x, y)] = Cell::default();
                    screen.grid_mut()[(x, y)]
                        .set_fg(fg!())
                        .set_bg(bg!())
                        .set_underline(*underline_color)
                        .set_attrs(*attrs);
                    screen.grid_mut().dirty_bitmap_mut().set(x, y);
                }
                *dirty = true;
                *state = State::Normal;
            }
            (b'L', State::Csi) | (b'L', State::Csi1(_)) => {
                /* Insert n blank lines (default 1) */
                let n = if let State::Csi1([ref buf1]) = state {
                    std::str::from_utf8(buf1)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                        .unwrap_or(0)
                } else {
                    1
                };

                let y = cursor!().y;
                screen.grid_mut().scroll_down(scroll_region, y, n);

                *dirty = true;
                *state = State::Normal;
            }
            (b'M', State::Csi) | (b'M', State::Csi1(_)) => {
                /* Delete n lines (default 1) */
                let n = if let State::Csi1([ref buf1]) = state {
                    std::str::from_utf8(buf1)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                        .unwrap_or(0)
                } else {
                    1
                };

                let y = cursor!().y;
                screen.grid_mut().scroll_up(scroll_region, y, n);

                *dirty = true;
                *state = State::Normal;
            }
            (b'A', State::Csi) => {
                // Move cursor up 1 line
                if cursor!().y > 0 {
                    cursor!(mut).y -= 1;
                }
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'K', State::Csi1([buf])) if buf.as_ref() == b"0" => {
                /* Erase in Line (ED), VT100. */
                /* Erase to right (Default) */
                let y = cursor!().y;
                for x in cursor!().x..terminal_size.0 {
                    screen.grid_mut()[(x, y)] = Cell::default();
                    screen.grid_mut()[(x, y)]
                        .set_fg(fg!())
                        .set_bg(bg!())
                        .set_underline(*underline_color)
                        .set_attrs(*attrs);
                    screen.grid_mut().dirty_bitmap_mut().set(x, y);
                }
                *dirty = true;
                *state = State::Normal;
            }
            (b'K', State::Csi1([buf])) if buf.as_ref() == b"1" => {
                /* Erase in Line (ED), VT100. */
                /* Erase to left (Default) */
                let y = cursor!().y;
                for x in 0..=cursor!().x {
                    screen.grid_mut()[(x, y)] = Cell::default();
                    screen.grid_mut()[(x, y)]
                        .set_fg(fg!())
                        .set_bg(bg!())
                        .set_underline(*underline_color)
                        .set_attrs(*attrs);
                    screen.grid_mut().dirty_bitmap_mut().set(x, y);
                }
                *dirty = true;
                *state = State::Normal;
            }
            (b'K', State::Csi1([buf])) if buf.as_ref() == b"2" => {
                /* Erase in Line (ED), VT100. */
                /* Erase all */
                for y in 0..terminal_size.1 {
                    for x in 0..terminal_size.0 {
                        screen.grid_mut()[(x, y)] = Cell::default();
                        screen.grid_mut()[(x, y)]
                            .set_fg(fg!())
                            .set_bg(bg!())
                            .set_underline(*underline_color)
                            .set_attrs(*attrs);
                        screen.grid_mut().dirty_bitmap_mut().set(x, y);
                    }
                }

                let area = screen.area();
                let area = area.take_cols(terminal_size.0).take_rows(terminal_size.1);
                screen.grid_mut().clear_area(
                    area,
                    ThemeAttribute {
                        fg: fg!(),
                        bg: bg!(),
                        underline: *underline_color,
                        attrs: *attrs,
                    },
                );
                set_dirty_cell!(area);
                *state = State::Normal;
            }
            (b'J', State::Csi1([ref buf])) if buf.as_ref() == b"0" => {
                /* Erase in Display (ED), VT100. */
                /* Erase Below (default). */

                let area = screen.area();
                let area = area.skip_rows(std::cmp::min(
                    cursor!().y + 1 + scroll_region.top,
                    terminal_size.1.saturating_sub(1),
                ));
                screen.grid_mut().clear_area(
                    area,
                    ThemeAttribute {
                        fg: fg!(),
                        bg: bg!(),
                        underline: *underline_color,
                        attrs: *attrs,
                    },
                );
                set_dirty_cell!(area);
                *state = State::Normal;
            }
            (b'J', State::Csi1([ref buf])) if buf.as_ref() == b"1" => {
                /* Erase in Display (ED), VT100. */
                /* Erase Above */

                let area = screen.area();
                let area = area.take_rows(cursor!().y.saturating_sub(1) + scroll_region.top);
                screen.grid_mut().clear_area(
                    area,
                    ThemeAttribute {
                        fg: fg!(),
                        bg: bg!(),
                        underline: *underline_color,
                        attrs: *attrs,
                    },
                );
                set_dirty_cell!(area);
                *state = State::Normal;
            }
            (b'J', State::Csi1([ref buf])) if buf.as_ref() == b"2" => {
                /* Erase in Display (ED), VT100. */
                /* Erase All */

                let area = screen.area();
                screen.grid_mut().clear_area(
                    area,
                    ThemeAttribute {
                        fg: fg!(),
                        bg: bg!(),
                        underline: *underline_color,
                        attrs: *attrs,
                    },
                );
                set_dirty_cell!(area);
                *state = State::Normal;
            }
            (b'X', State::Csi1(_) | State::Csi) => {
                /* Erase Ps Character(s) (default = 1) (ECH).. */
                let ps = if let State::Csi1([ref buf]) = state {
                    std::str::from_utf8(buf)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                        .unwrap_or(0)
                } else {
                    1
                };

                let mut ctr = 0;
                let (mut cur_x, mut cur_y) = cursor_val!();
                while ctr < ps {
                    if cur_x >= terminal_size.0 {
                        cur_y += 1;
                        cur_x = 0;
                        if cur_y >= terminal_size.1 {
                            break;
                        }
                    }
                    screen.grid_mut()[(cur_x, cur_y)] = Cell::default();
                    screen.grid_mut()[(cur_x, cur_y)]
                        .set_fg(fg!())
                        .set_bg(bg!())
                        .set_underline(*underline_color)
                        .set_attrs(*attrs);
                    screen.grid_mut().dirty_bitmap_mut().set(cur_x, cur_y);
                    cur_x += 1;
                    ctr += 1;
                }
                *dirty = true;
                *state = State::Normal;
            }
            (b't', State::Csi1([buf])) => {
                /* Window manipulation */
                if buf.as_ref() == b"18" || buf.as_ref() == b"19" {
                    // Ps = 18 → Report the size of the text area in characters as CSI 8 ; height ;
                    // width t
                    stdin.write_all(b"\x1b[8;").unwrap();
                    stdin
                        .write_all((terminal_size.1).to_string().as_bytes())
                        .unwrap();
                    stdin.write_all(b";").unwrap();
                    stdin
                        .write_all((terminal_size.0).to_string().as_bytes())
                        .unwrap();
                    stdin.write_all(b"t").unwrap();
                    stdin.flush().unwrap();
                } else {
                    //log::trace!("ignoring unknown code {}",
                    // EscCode::from((&(*state), byte)));
                }
                *state = State::Normal;
            }
            (b'n', State::Csi1([buf])) => {
                if buf.as_slice() == b"6" {
                    // Ps = 6  ⇒  Report Cursor Position (CPR) [row;column].
                    // Result is CSI r ; c R
                    stdin.write_all(b"\x1b[").unwrap();
                    stdin
                        .write_all((cursor!().y + 1).to_string().as_bytes())
                        .unwrap();
                    stdin.write_all(b";").unwrap();
                    stdin
                        .write_all((cursor!().x + 1).to_string().as_bytes())
                        .unwrap();
                    stdin.write_all(b"R").unwrap();
                    stdin.flush().unwrap();
                } else if buf.as_slice() == b"5" {
                    // operating status
                    stdin.write_all(b"\x1b[0n").unwrap();
                    stdin.flush().unwrap();
                }
                *state = State::Normal;
            }
            (b'A', State::Csi1([buf])) => {
                // Move cursor up n lines
                let offset = std::str::from_utf8(buf)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(0);
                if cursor!().y >= offset {
                    cursor!(mut).y -= offset;
                }
                *state = State::Normal;
            }
            (b'B', State::Csi1([buf])) => {
                // ESC[{buf}B   CSI Cursor Down {buf} Times
                let offset = std::str::from_utf8(buf)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(0);
                if cursor!().y == scroll_region.bottom {
                    /* scroll down */
                    for y in scroll_region.top..scroll_region.bottom {
                        for x in 0..terminal_size.1 {
                            screen.grid_mut()[(x, y)] = screen.grid()[(x, y + 1)];
                            screen.grid_mut().dirty_bitmap_mut().set(x, y);
                        }
                    }
                    for x in 0..terminal_size.1 {
                        screen.grid_mut()[(x, scroll_region.bottom)] = Cell::default();
                        screen
                            .grid_mut()
                            .dirty_bitmap_mut()
                            .set(x, scroll_region.bottom);
                    }
                } else if offset + cursor!().y < terminal_size.1 {
                    cursor!(mut).y += offset;
                }
                if scroll_region.top + cursor!().y >= terminal_size.1 {
                    cursor!(mut).y = terminal_size.1.saturating_sub(1);
                }
                set_dirty_cell!();
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'D', State::Csi1([buf])) => {
                // ESC[{buf}D   CSI Cursor Backward {buf} Times
                let offset = std::str::from_utf8(buf)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(0);
                if cursor!().x >= offset {
                    cursor!(mut).x -= offset;
                }
                set_dirty_cell!();
                *state = State::Normal;
            }
            (b'E', State::Csi1([buf])) => {
                // ESC[{buf}E   CSI Cursor Next Line {buf} Times
                let offset = std::str::from_utf8(buf)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(0);
                if offset + cursor!().y < terminal_size.1 {
                    cursor!(mut).y += offset;
                }
                if scroll_region.top + cursor!().y >= terminal_size.1 {
                    cursor!(mut).y = terminal_size.1.saturating_sub(1);
                }
                cursor!(mut).x = 0;
                set_dirty_cell!();
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'F', State::Csi1([buf])) => {
                // ESC[{buf}F   CSI Cursor Previous Line {buf} Times
                let offset = std::str::from_utf8(buf)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(0);
                cursor!(mut).y = cursor!().y.saturating_sub(offset);
                cursor!(mut).x = 0;
                *wrap_next = false;
                set_dirty_cell!();
                *state = State::Normal;
            }
            (b'G', State::Csi1(_)) | (b'G', State::Csi) => {
                // ESC[{buf}G   Cursor Character Absolute  [column={buf}] (default = [row,1])
                let new_col = if let State::Csi1([buf]) = state {
                    std::str::from_utf8(buf)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                } else {
                    Some(1)
                };
                if let Some(new_col) = new_col {
                    if new_col < terminal_size.0 {
                        cursor!(mut).x = new_col.saturating_sub(1);
                    }
                }
                *wrap_next = false;
                set_dirty_cell!();
                *state = State::Normal;
            }
            (b'C', State::Csi1([buf])) => {
                // ESC[{buf}C   CSI Cursor Forward {buf} Times
                let offset = std::str::from_utf8(buf)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(0);
                if cursor!().x + offset < terminal_size.0 {
                    cursor!(mut).x += offset;
                }
                set_dirty_cell!();
                *state = State::Normal;
            }
            (b'P', State::Csi1(_)) | (b'P', State::Csi) => {
                // ESC[{buf}P   CSI Delete {buf} characters, default = 1
                let offset = if let State::Csi1([buf]) = state {
                    std::str::from_utf8(buf)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                } else {
                    Some(1)
                };

                if let Some(offset) = offset {
                    let x = cursor!().x;
                    let y = cursor!().y;
                    for i in 0..(terminal_size.0 - x - offset) {
                        screen.grid_mut()[(x + i, y)] = screen.grid()[(x + i + offset, y)];
                        screen.grid_mut().dirty_bitmap_mut().set(x + i, y);
                    }
                    for x in (terminal_size.0 - offset)..terminal_size.0 {
                        screen.grid_mut()[(x, y)]
                            .set_ch(' ')
                            .set_fg(fg!())
                            .set_bg(bg!())
                            .set_underline(*underline_color)
                            .set_attrs(*attrs);
                        screen.grid_mut().dirty_bitmap_mut().set(x, y);
                    }
                }
                *dirty = true;
                *state = State::Normal;
            }
            (b'd', State::Csi1(_)) | (b'd', State::Csi) => {
                /* CSI Pm d Line Position Absolute [row] (default = [1,column]) (VPA). */
                let row = if let State::Csi1([buf]) = state {
                    std::str::from_utf8(buf)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                } else {
                    Some(1)
                };
                if let Some(row) = row {
                    cursor!(mut).y = row.saturating_sub(1);
                    if scroll_region.top + cursor!().y >= terminal_size.1 {
                        cursor!(mut).y = terminal_size.1.saturating_sub(1);
                    }
                }
                *wrap_next = false;
                set_dirty_cell!();
                *state = State::Normal;
            }
            (b';', State::Csi1([ref mut buf1_p])) => {
                if buf1_p.is_empty() {
                    buf1_p.push(b'0');
                }
                let buf1 = std::mem::take(buf1_p);
                let buf2 = SmallVec::new();
                *state = State::Csi2([buf1, buf2]);
            }
            (b':', State::Csi1([ref buf1_p])) if buf1_p.as_slice() == b"58".as_slice() => {
                *state = State::Csi58;
            }
            (b':', State::Csi1([ref mut buf1_p])) if buf1_p.as_slice() == b"4".as_slice() => {
                let buf1 = std::mem::take(buf1_p);
                let buf2 = SmallVec::new();
                *state = State::Csi2([buf1, buf2]);
            }
            (b'm', State::Csi1(ref buf1)) => {
                // Character Attributes.
                if let Some(sgr) = SgrIter::new(buf1).next() {
                    sgr!(sgr);
                }
                *state = State::Normal;
            }
            (b'm', State::Csi2([ref buf1, ref buf2]))
                if buf1.as_slice() == b"4".as_slice() && buf2.as_slice() == b"0".as_slice() =>
            {
                *attrs &= !Attr::UNDERCURL;
                *state = State::Normal;
            }
            (b'm', State::Csi2([ref buf1, ref buf2]))
                if buf1.as_slice() == b"4".as_slice()
                    && ([&b"1"[..], &b"2"[..], &b"3"[..], &b"4"[..], &b"5"[..]]
                        .contains(&buf2.as_slice())) =>
            {
                if buf2.as_slice() == b"1".as_slice() {
                    *attrs |= Attr::UNDERLINE;
                } else {
                    // <ESC>[4:2m  # double underline
                    // <ESC>[4:3m  # curly underline
                    // <ESC>[4:4m  # dotted underline
                    // <ESC>[4:5m  # dashed underline
                    *attrs |= Attr::UNDERCURL;
                }
                *state = State::Normal;
            }
            (b'm', State::Csi2(ref buf)) => {
                let sgrs = SgrIter::new(buf);
                for sgr in sgrs {
                    sgr!(sgr);
                }
                *state = State::Normal;
            }
            (c, State::Csi1([ref mut buf])) if c.is_ascii_digit() || c == b' ' => {
                buf.push(c);
            }
            (b';', State::Csi2([ref mut buf1_p, ref mut buf2_p])) => {
                let buf1 = std::mem::take(buf1_p);
                if buf2_p.is_empty() {
                    buf2_p.push(b'0');
                }
                let buf2 = std::mem::take(buf2_p);
                let buf3 = SmallVec::new();
                *state = State::Csi3([buf1, buf2, buf3]);
            }
            (b't', State::Csi2(_)) => {
                //log::trace!("ignoring {}", EscCode::from((&(*state), byte)));
                // Window manipulation, skip it
                *state = State::Normal;
            }
            (b'H', State::Csi) => {
                //Cursor Position [row;column] (default = [1,1]) (CUP).
                let (orig_x, mut orig_y) = (1, 1);

                let (min_y, max_y) = if *origin_mode {
                    orig_y += scroll_region.top;
                    (scroll_region.top, scroll_region.bottom)
                } else {
                    (0, terminal_size.1.saturating_sub(1))
                };

                set_dirty_cell!();
                cursor!(mut).x = std::cmp::min(orig_x - 1, terminal_size.0.saturating_sub(1));
                cursor!(mut).y = std::cmp::max(min_y, std::cmp::min(max_y, orig_y - 1));
                set_dirty_cell!();
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'c', State::Csi) => {
                // CSI Ps c  Send Device Attributes (Primary DA).
                //             Ps = 0  or omitted
                stdin.write_all(b"\x1b[?c").unwrap();
                *state = State::Normal;
            }
            (b'c', State::Csi1(..)) => {
                // CSI Ps c  Send Device Attributes (Primary DA).
                stdin.write_all(b"\x1b[?c").unwrap();
                *state = State::Normal;
            }
            (b'H', State::Csi1([ref y])) => {
                //Cursor Position [row;1] (default = [1,1]) (CUP).
                let orig = std::str::from_utf8(y)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .map(|y| (0, y.saturating_sub(1)));

                if let Some((orig_x, mut orig_y)) = orig {
                    let (min_y, max_y) = if *origin_mode {
                        orig_y += scroll_region.top;
                        (scroll_region.top, scroll_region.bottom)
                    } else {
                        (0, terminal_size.1.saturating_sub(1))
                    };

                    set_dirty_cell!();
                    cursor!(mut).x = std::cmp::min(orig_x, terminal_size.0.saturating_sub(1));
                    cursor!(mut).y = std::cmp::max(min_y, std::cmp::min(max_y, orig_y));
                    set_dirty_cell!();
                }
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'H', State::Csi2([ref y, ref x])) => {
                //Cursor Position [row;column] (default = [1,1]) (CUP).
                let orig = std::str::from_utf8(x)
                    .ok()
                    .and_then(|n| n.parse::<usize>().ok())
                    .and_then(|x| {
                        Some((
                            x.saturating_sub(1),
                            std::str::from_utf8(y)
                                .ok()
                                .and_then(|n| n.parse::<usize>().ok())?
                                .saturating_sub(1),
                        ))
                    });

                if let Some((orig_x, mut orig_y)) = orig {
                    let (min_y, max_y) = if *origin_mode {
                        orig_y += scroll_region.top;
                        (scroll_region.top, scroll_region.bottom)
                    } else {
                        (0, terminal_size.1.saturating_sub(1))
                    };

                    set_dirty_cell!();
                    cursor!(mut).x = std::cmp::min(orig_x, terminal_size.0.saturating_sub(1));
                    cursor!(mut).y = std::cmp::max(min_y, std::cmp::min(max_y, orig_y));
                    set_dirty_cell!();
                }
                *wrap_next = false;

                *state = State::Normal;
            }
            (c, State::Csi2([_, ref mut buf])) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (b'r', State::Csi2(_)) | (b'r', State::Csi) => {
                /* CSI Ps ; Ps r Set Scrolling Region [top;bottom] (default = full size of
                 * window) (DECSTBM). */
                let region = if let State::Csi2([ref top, ref bottom]) = state {
                    std::str::from_utf8(top)
                        .ok()
                        .and_then(|n| n.parse::<usize>().ok())
                        .and_then(|top| {
                            Some((
                                top,
                                std::str::from_utf8(bottom).ok()?.parse::<usize>().ok()?,
                            ))
                        })
                } else {
                    Some((1, terminal_size.1))
                };

                if let Some((top, bottom)) = region {
                    if bottom > top {
                        scroll_region.top = top - 1;
                        scroll_region.bottom = bottom - 1;
                        cursor!(mut).x = 0;
                        cursor!(mut).y = 0;
                        *wrap_next = false;
                    }
                    set_dirty_cell!();
                }
                *state = State::Normal;
            }
            (b't', State::Csi3(_)) => {
                //log::trace!("ignoring {}", EscCode::from((&(*state), byte)));
                // Window manipulation, skip it
                *state = State::Normal;
            }

            (c, State::Csi3([_, _, ref mut buf])) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (b'm', State::Csi3([ref buf1, ref buf2, ref buf3]))
                if buf1.as_ref() == b"38" && buf2.as_ref() == b"5" =>
            {
                /* Set character attributes | foreground color */
                *fg_color = if let Some(byte) = std::str::from_utf8(buf3)
                    .ok()
                    .and_then(|n| n.parse::<u8>().ok())
                {
                    Color::Byte(byte)
                } else {
                    color_scheme
                        .map(|(_, fg, _)| fg.into())
                        .unwrap_or(Color::Default)
                };
                *state = State::Normal;
            }
            (b'm', State::Csi3([ref buf1, ref buf2, ref buf3]))
                if buf1.as_ref() == b"48" && buf2.as_ref() == b"5" =>
            {
                /* Set character attributes | background color */
                *bg_color = if let Some(byte) = std::str::from_utf8(buf3)
                    .ok()
                    .and_then(|n| n.parse::<u8>().ok())
                {
                    Color::Byte(byte)
                } else {
                    color_scheme
                        .map(|(_, _, bg)| bg.into())
                        .unwrap_or(Color::Default)
                };
                *state = State::Normal;
            }
            (b'm', State::Csi3([ref buf1, ref buf2, ref buf3]))
                if buf1.as_ref() == b"58" && buf2.as_ref() == b"5" =>
            {
                // Set underline color
                *underline_color = if let Some(byte) = std::str::from_utf8(buf3)
                    .ok()
                    .and_then(|n| n.parse::<u8>().ok())
                {
                    Color::Byte(byte)
                } else {
                    color_scheme
                        .map(|(_, fg, _)| fg.into())
                        .unwrap_or(Color::Default)
                };
                *state = State::Normal;
            }
            (c, State::Csi3([_, _, ref mut buf])) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (b';', State::Csi3([ref mut buf1_p, ref mut buf2_p, ref mut buf3_p])) => {
                let buf1 = std::mem::take(buf1_p);
                let buf2 = std::mem::take(buf2_p);
                if buf3_p.is_empty() {
                    buf3_p.push(b'0');
                }
                let buf3 = std::mem::take(buf3_p);
                let buf4 = SmallVec::new();
                *state = State::Csi4([buf1, buf2, buf3, buf4]);
            }
            (c, State::Csi4([_, _, _, ref mut buf])) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (
                b';',
                State::Csi4([ref mut buf1_p, ref mut buf2_p, ref mut buf3_p, ref mut buf4_p]),
            ) => {
                let buf1 = std::mem::take(buf1_p);
                let buf2 = std::mem::take(buf2_p);
                let buf3 = std::mem::take(buf3_p);
                if buf4_p.is_empty() {
                    buf4_p.push(b'0');
                }
                let buf4 = std::mem::take(buf4_p);
                let buf5 = SmallVec::new();
                *state = State::Csi5([buf1, buf2, buf3, buf4, buf5]);
            }
            (c, State::Csi5([_, _, _, _, ref mut buf])) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (
                b';',
                State::Csi5(
                    [ref mut buf1_p, ref mut buf2_p, ref mut buf3_p, ref mut buf4_p, ref mut buf5_p],
                ),
            ) => {
                let buf1 = std::mem::take(buf1_p);
                let buf2 = std::mem::take(buf2_p);
                let buf3 = std::mem::take(buf3_p);
                let buf4 = std::mem::take(buf4_p);
                if buf5_p.is_empty() {
                    buf5_p.push(b'0');
                }
                let buf5 = std::mem::take(buf5_p);
                let buf6 = SmallVec::new();
                *state = State::Csi6([buf1, buf2, buf3, buf4, buf5, buf6]);
            }
            (c, State::Csi6([_, _, _, _, _, ref mut buf])) if c.is_ascii_digit() => {
                buf.push(c);
            }
            (
                b'm',
                State::Csi6(
                    [ref mut buf1, ref mut buf2, ref mut _color_space_buf, ref mut r_buf, ref mut g_buf, ref mut b_buf],
                ),
            ) if buf1.as_ref() == b"38" && buf2.as_ref() == b"2" => {
                /* Set true foreground color */
                *fg_color = parse_rgb!(r_buf, g_buf, b_buf)
                    .or_else(|| color_scheme.map(|(_, fg, _)| fg.into()))
                    .unwrap_or(Color::Default);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi6(
                    [ref mut buf1, ref mut buf2, ref mut _color_space_buf, ref mut r_buf, ref mut g_buf, ref mut b_buf],
                ),
            ) if buf1.as_ref() == b"48" && buf2.as_ref() == b"2" => {
                /* Set true background color */
                *bg_color = parse_rgb!(r_buf, g_buf, b_buf)
                    .or_else(|| color_scheme.map(|(_, _, bg)| bg.into()))
                    .unwrap_or(Color::Default);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi6(
                    [ref mut buf1, ref mut buf2, ref mut _color_space_buf, ref mut r_buf, ref mut g_buf, ref mut b_buf],
                ),
            ) if buf1.as_ref() == b"58" && buf2.as_ref() == b"2" => {
                /* Set underline color */
                *underline_color = parse_rgb!(r_buf, g_buf, b_buf)
                    .or_else(|| color_scheme.map(|(_, fg, _)| fg.into()))
                    .unwrap_or(Color::Default);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi5(
                    [ref mut buf1, ref mut buf2, ref mut r_buf, ref mut g_buf, ref mut b_buf],
                ),
            ) if buf1.as_ref() == b"38" && buf2.as_ref() == b"2" => {
                /* Set true foreground color */
                *fg_color = parse_rgb!(r_buf, g_buf, b_buf)
                    .or_else(|| color_scheme.map(|(_, fg, _)| fg.into()))
                    .unwrap_or(Color::Default);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi5(
                    [ref mut buf1, ref mut buf2, ref mut r_buf, ref mut g_buf, ref mut b_buf],
                ),
            ) if buf1.as_ref() == b"48" && buf2.as_ref() == b"2" => {
                /* Set true background color */
                *bg_color = parse_rgb!(r_buf, g_buf, b_buf)
                    .or_else(|| color_scheme.map(|(_, _, bg)| bg.into()))
                    .unwrap_or(Color::Default);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi5(
                    [ref mut buf1, ref mut buf2, ref mut r_buf, ref mut g_buf, ref mut b_buf],
                ),
            ) if buf1.as_ref() == b"58" && buf2.as_ref() == b"2" => {
                /* Set underline color */
                *underline_color = parse_rgb!(r_buf, g_buf, b_buf)
                    .or_else(|| color_scheme.map(|(_, fg, _)| fg.into()))
                    .unwrap_or(Color::Default);
                *state = State::Normal;
            }
            (b'q', State::Csi1([buf]))
                if buf.len() == 2 && buf[1] == b' ' && (b'0'..=b'6').contains(&buf[0]) =>
            {
                /*
                  CSI Ps SP q
                  Set cursor style (DECSCUSR), VT520.
                  Ps = 0  ⇒  terminal default.
                  Ps = 1  ⇒  blinking block.
                  Ps = 2  ⇒  steady block.
                  Ps = 3  ⇒  blinking underline.
                  Ps = 4  ⇒  steady underline.
                  Ps = 5  ⇒  blinking bar, xterm.
                  Ps = 6  ⇒  steady bar, xterm.
                */
                let style = match &buf[0] {
                    b'0' => Some(CursorStyle::TerminalDefault),
                    b'1' => Some(CursorStyle::BlinkingBlock),
                    b'2' => Some(CursorStyle::SteadyBlock),
                    b'3' => Some(CursorStyle::BlinkingUnderline),
                    b'4' => Some(CursorStyle::SteadyUnderline),
                    b'5' => Some(CursorStyle::BlinkingBar),
                    b'6' => Some(CursorStyle::SteadyBar),
                    _ => None,
                };
                if let Some(style) = style {
                    cursor!(mut).style = style;
                    set_dirty_cell!();
                }
                *state = State::Normal;
            }
            (b'$', State::Csi1([ref mut buf])) => {
                let buf = std::mem::take(buf);
                *state = State::DECRQMANSI(buf);
            }
            (b'p', State::DECRQMANSI(ref mut buf)) => {
                // https://vt100.net/docs/vt510-rm/DECRPM.html
                // CSI Pa ; Ps $ y
                let buf = std::mem::take(buf);
                stdin.write_all(b"\x1b[").unwrap();
                stdin.write_all(&buf).unwrap();
                stdin.write_all(b";0$y").unwrap();
                stdin.flush().unwrap();
                *state = State::Normal;
            }
            (b'2', State::Csi58) => {
                *state = State::Csi58_2;
            }
            (b'5', State::Csi58) => {
                *state = State::Csi58_5;
            }
            (b':', State::Csi58_2) => {
                *state = State::Csi58_2_1 {
                    ps_1: SmallVec::new(),
                };
            }
            (b':', State::Csi58_5) => {
                *state = State::Csi58_5_ {
                    ps: SmallVec::new(),
                };
            }
            (c, State::Csi58_5_ { ref mut ps }) if c.is_ascii_digit() => {
                ps.push(c);
            }
            (b'm', State::Csi58_5_ { ps: _ }) => {
                *state = State::Normal;
            }
            (c, State::Csi58_2_1 { ref mut ps_1 }) if c.is_ascii_digit() => {
                ps_1.push(c);
            }
            (b':', State::Csi58_2_1 { ref mut ps_1 }) => {
                *state = State::Csi58_2_2 {
                    ps_1: std::mem::take(ps_1),
                    ps_2: SmallVec::new(),
                };
            }
            (
                c,
                State::Csi58_2_2 {
                    ps_1: _,
                    ref mut ps_2,
                },
            ) if c.is_ascii_digit() => {
                ps_2.push(c);
            }
            (
                b':',
                State::Csi58_2_2 {
                    ref mut ps_1,
                    ref mut ps_2,
                },
            ) => {
                *state = State::Csi58_2_3 {
                    ps_1: std::mem::take(ps_1),
                    ps_2: std::mem::take(ps_2),
                    ps_3: SmallVec::new(),
                };
            }
            (
                c,
                State::Csi58_2_3 {
                    ps_1: _,
                    ps_2: _,
                    ref mut ps_3,
                },
            ) if c.is_ascii_digit() => {
                ps_3.push(c);
            }
            (
                b':',
                State::Csi58_2_3 {
                    ref mut ps_1,
                    ref mut ps_2,
                    ref mut ps_3,
                },
            ) if ps_1.is_empty() => {
                std::mem::swap(ps_1, ps_2);
                std::mem::swap(ps_2, ps_3);
            }
            (
                b'm',
                State::Csi58_2_3 {
                    ps_1: _,
                    ps_2: _,
                    ps_3: _,
                },
            ) => {
                *state = State::Normal;
            }
            (b'm', State::Csi6(ref bufs)) => {
                // https://vt100.net/docs/vt510-rm/SGR.html
                // SGR—Select Graphic Rendition
                //
                // This control function selects one or more character attributes at the same
                // time.
                let sgrs = SgrIter::new(bufs);
                for sgr in sgrs {
                    sgr!(sgr);
                }
                *state = State::Normal;
            }
            (
                b';',
                State::Csi6([ref mut a, ref mut b, ref mut c, ref mut d, ref mut e, ref mut f]),
            ) => {
                let a = std::mem::take(a);
                let b = std::mem::take(b);
                let c = std::mem::take(c);
                let d = std::mem::take(d);
                let e = std::mem::take(e);
                let f = std::mem::take(f);
                *state = State::CsiLarge(vec![a, b, c, d, e, f, SmallVec::new()]);
            }
            (b';', State::CsiLarge(ref mut bufs)) => {
                bufs.push(SmallVec::new());
            }
            (b'm', State::CsiLarge(bufs)) => {
                let sgrs = SgrIter::new(bufs);
                for sgr in sgrs {
                    sgr!(sgr);
                }
                *state = State::Normal;
            }
            (other, State::CsiLarge(ref mut bufs)) => {
                bufs.last_mut().unwrap().push(other);
            }
            (b'\x07', State::Osc2([ref mut code, ref mut value])) => {
                osc!(code, value);
                *state = State::Normal;
            }
            (b'\x07', State::Osc3([ref mut code, ref mut value1, ref mut value2])) => {
                osc!(code, value1, value2);
                *state = State::Normal;
            }
            (b'\x1b', State::Osc2([ref mut code, ref mut value])) => {
                let code = std::mem::take(code);
                let value = std::mem::take(value);
                *state = State::Osc2WaitingForSt([code, value]);
            }
            (b'\\', State::Osc2WaitingForSt([ref mut code, ref mut value])) => {
                osc!(code, value);
                *state = State::Normal;
            }
            (b'\x1b', State::Osc3([ref mut code, ref mut value1, ref mut value2])) => {
                let code = std::mem::take(code);
                let value1 = std::mem::take(value1);
                let value2 = std::mem::take(value2);
                *state = State::Osc3WaitingForSt([code, value1, value2]);
            }
            (b'\\', State::Osc3WaitingForSt([ref mut code, ref mut value1, ref mut value2])) => {
                osc!(code, value1, value2);
                *state = State::Normal;
            }
            (other, State::Osc2([_, ref mut value])) => {
                value.push(other);
            }
            (other, State::Osc3([_, _, ref mut value])) => {
                value.push(other);
            }
            (c @ b'>', State::Csi) => {
                let mut buf1 = SmallVec::new();
                buf1.push(c);
                *state = State::Csi1([buf1]);
            }
            (
                _,
                State::Csi
                | State::Csi1(_)
                | State::Csi2(_)
                | State::Csi3(_)
                | State::Csi4(_)
                | State::Csi5(_)
                | State::Csi6(_)
                | State::Osc1(_)
                | State::Osc2WaitingForSt(_)
                | State::Osc3WaitingForSt(_)
                | State::DECRQMDEC(_)
                | State::DECRQMANSI(_)
                | State::CsiQ(_)
                | State::Csi58
                | State::Csi58_2
                | State::Csi58_5
                | State::Csi58_2_1 { ps_1: _ }
                | State::Csi58_2_2 { ps_1: _, ps_2: _ }
                | State::Csi58_2_3 {
                    ps_1: _,
                    ps_2: _,
                    ps_3: _,
                }
                | State::Csi58_5_ { ps: _ },
            ) => {
                log::trace!(
                    "state: {state:?} ignoring unknown code {} byte {byte}",
                    EscCode::from((&(*state), byte))
                );
                *state = State::Normal;
            }
            /* other stuff */
            (_, State::G0) => {
                // log::trace!("ignoring {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
        }
    }
}
