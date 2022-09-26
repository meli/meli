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

use super::*;
use crate::terminal::{cells::*, Color};
use melib::error::{MeliError, Result};
use melib::text_processing::wcwidth;
use nix::sys::wait::WaitStatus;
use nix::sys::wait::{waitpid, WaitPidFlag};

#[derive(Debug)]
enum ScreenBuffer {
    Normal,
    Alternate,
}

/// `EmbedGrid` manages the terminal grid state of the embed process.
///
/// The embed process sends bytes to the master end (see super mod) and interprets them in a state
/// machine stored in `State`. Escape codes are translated as changes to the grid, eg changes in a
/// cell's colors.
///
/// The main process copies the grid whenever the actual terminal is redrawn.
#[derive(Debug)]
pub struct EmbedGrid {
    cursor: (usize, usize),
    /// `[top;bottom]`
    scroll_region: ScrollRegion,
    pub alternate_screen: CellBuffer,
    pub state: State,
    /// (width, height)
    pub terminal_size: (usize, usize),
    initialized: bool,
    fg_color: Color,
    bg_color: Color,
    attrs: Attr,
    /// Store the fg/bg color when highlighting the cell where the cursor is so that it can be
    /// restored afterwards
    prev_fg_color: Option<Color>,
    prev_bg_color: Option<Color>,
    prev_attrs: Option<Attr>,

    cursor_key_mode: bool, // (DECCKM)
    show_cursor: bool,
    origin_mode: bool,
    auto_wrap_mode: bool,
    /// If next grapheme should be placed in the next line
    /// This should be reset whenever the cursor value changes
    wrap_next: bool,
    /// Store state in case a multi-byte character is encountered
    codepoints: CodepointBuf,
    pub normal_screen: CellBuffer,
    screen_buffer: ScreenBuffer,
}

#[derive(Debug)]
pub struct EmbedTerminal {
    pub grid: EmbedGrid,
    stdin: std::fs::File,
    /// Pid of the embed process
    pub child_pid: nix::unistd::Pid,
}

impl std::io::Write for EmbedTerminal {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        /*

                 Key            Normal     Application
                 -------------+----------+-------------
                 Cursor Up    | CSI A    | SS3 A
                 Cursor Down  | CSI B    | SS3 B
                 Cursor Right | CSI C    | SS3 C
                 Cursor Left  | CSI D    | SS3 D
                 -------------+----------+-------------

                   Key        Normal     Application
                   ---------+----------+-------------
                   Home     | CSI H    | SS3 H
                   End      | CSI F    | SS3 F
                   ---------+----------+-------------

        */
        if self.grid.cursor_key_mode {
            match buf {
                &[0x1b, 0x5b, b'A']
                | &[0x1b, 0x5b, b'B']
                | &[0x1b, 0x5b, b'C']
                | &[0x1b, 0x5b, b'D']
                | &[0x1b, 0x5b, b'H']
                | &[0x1b, 0x5b, b'F'] => {
                    self.stdin.write_all(&[0x1b, 0x4f, buf[2]])?;
                    Ok(buf.len())
                }
                _ => {
                    self.stdin.write_all(buf)?;
                    Ok(buf.len())
                }
            }
        } else {
            self.stdin.write_all(buf)?;
            Ok(buf.len())
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdin.flush()
    }
}

impl EmbedTerminal {
    pub fn new(stdin: std::fs::File, child_pid: nix::unistd::Pid) -> Self {
        EmbedTerminal {
            grid: EmbedGrid::new(),
            stdin,
            child_pid,
        }
    }

    pub fn set_terminal_size(&mut self, new_val: (usize, usize)) {
        self.grid.set_terminal_size(new_val);
        let winsize = Winsize {
            ws_row: <u16>::try_from(new_val.1).unwrap(),
            ws_col: <u16>::try_from(new_val.0).unwrap(),
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        let master_fd = self.stdin.as_raw_fd();
        let _ = unsafe { set_window_size(master_fd, &winsize) };
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGWINCH);
    }

    pub fn wake_up(&self) {
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGCONT);
    }

    pub fn stop(&self) {
        //debug!("stopping");
        let _ = nix::sys::signal::kill(debug!(self.child_pid), nix::sys::signal::SIGSTOP);
    }

    pub fn terminate(&self) {
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGTERM);
        std::thread::sleep(std::time::Duration::from_millis(150));
        let _ = waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG));
    }

    pub fn is_active(&self) -> Result<WaitStatus> {
        debug!(waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG),))
            .map_err(|e| MeliError::new(e.to_string()))
    }

    pub fn process_byte(&mut self, byte: u8) {
        let Self {
            ref mut grid,
            ref mut stdin,
            child_pid: _,
        } = self;
        grid.process_byte(stdin, byte);
    }
}

#[derive(Debug, PartialEq, Eq)]
enum CodepointBuf {
    None,
    TwoCodepoints(u8),
    ThreeCodepoints(u8, Option<u8>),
    FourCodepoints(u8, Option<u8>, Option<u8>),
}

impl EmbedGrid {
    pub fn new() -> Self {
        let mut normal_screen = CellBuffer::default();
        normal_screen.set_growable(true);
        EmbedGrid {
            cursor: (0, 0),
            scroll_region: ScrollRegion {
                top: 0,
                bottom: 0,
                left: 0,
                ..Default::default()
            },
            terminal_size: (0, 0),
            initialized: false,
            alternate_screen: CellBuffer::default(),
            state: State::Normal,
            fg_color: Color::Default,
            bg_color: Color::Default,
            attrs: Attr::DEFAULT,
            prev_fg_color: None,
            prev_bg_color: None,
            prev_attrs: None,
            show_cursor: true,
            auto_wrap_mode: true,
            wrap_next: false,
            cursor_key_mode: false,
            origin_mode: false,
            codepoints: CodepointBuf::None,
            normal_screen,
            screen_buffer: ScreenBuffer::Normal,
        }
    }

    pub fn buffer(&self) -> &CellBuffer {
        match self.screen_buffer {
            ScreenBuffer::Normal => &self.normal_screen,
            ScreenBuffer::Alternate => &self.alternate_screen,
        }
    }

    pub fn buffer_mut(&mut self) -> &mut CellBuffer {
        match self.screen_buffer {
            ScreenBuffer::Normal => &mut self.normal_screen,
            ScreenBuffer::Alternate => &mut self.alternate_screen,
        }
    }

    pub fn set_terminal_size(&mut self, new_val: (usize, usize)) {
        if new_val == self.terminal_size && self.initialized {
            return;
        }
        self.initialized = true;
        //debug!("resizing to {:?}", new_val);
        self.scroll_region.top = 0;
        self.scroll_region.bottom = new_val.1.saturating_sub(1);

        self.terminal_size = new_val;
        if !self.alternate_screen.resize(new_val.0, new_val.1, None) {
            panic!(
                "Terminal size too big: ({} cols, {} rows)",
                new_val.0, new_val.1
            );
        }
        self.alternate_screen.clear(Some(Cell::default()));
        if !self.normal_screen.resize(new_val.0, new_val.1, None) {
            panic!(
                "Terminal size too big: ({} cols, {} rows)",
                new_val.0, new_val.1
            );
        }
        self.normal_screen.clear(Some(Cell::default()));
        self.cursor = (0, 0);
        self.wrap_next = false;
    }

    pub fn process_byte(&mut self, stdin: &mut std::fs::File, byte: u8) {
        let EmbedGrid {
            ref mut cursor,
            ref mut scroll_region,
            ref mut terminal_size,
            ref mut alternate_screen,
            ref mut state,
            ref mut fg_color,
            ref mut bg_color,
            ref mut attrs,
            ref mut prev_fg_color,
            ref mut prev_bg_color,
            ref mut prev_attrs,
            ref mut codepoints,
            ref mut show_cursor,
            ref mut auto_wrap_mode,
            ref mut wrap_next,
            ref mut cursor_key_mode,
            ref mut origin_mode,
            ref mut screen_buffer,
            ref mut normal_screen,
            initialized: _,
        } = self;
        let mut grid = normal_screen;

        let is_alternate = match *screen_buffer {
            ScreenBuffer::Normal => false,
            _ => {
                grid = alternate_screen;
                true
            }
        };

        macro_rules! increase_cursor_y {
            () => {
                cursor.1 += 1;
                if !is_alternate {
                    cursor.0 = 0;
                    if cursor.1 >= terminal_size.1 {
                        if !grid.resize(std::cmp::max(1, grid.cols()), grid.rows() + 2, None) {
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
                if cursor.0 + 1 < terminal_size.0 {
                    cursor.0 += 1;
                } else if is_alternate && *auto_wrap_mode {
                    *wrap_next = true;
                } else if !is_alternate {
                    cursor.0 = 0;
                    increase_cursor_y!();
                }
            };
        }

        macro_rules! cursor_x {
            () => {{
                if cursor.0 >= terminal_size.0 {
                    cursor.0 = terminal_size.0.saturating_sub(1);
                }
                cursor.0
            }};
        }
        macro_rules! cursor_y {
            () => {
                if is_alternate {
                    std::cmp::min(
                        cursor.1 + scroll_region.top,
                        terminal_size.1.saturating_sub(1),
                    )
                } else {
                    cursor.1
                }
            };
        }
        macro_rules! cursor_val {
            () => {
                (cursor_x!(), cursor_y!())
            };
        }

        let mut state = state;
        match (byte, &mut state) {
            (b'\x1b', State::Normal) => {
                *state = State::ExpectingControlChar;
            }
            (b']', State::ExpectingControlChar) => {
                let buf1 = SmallVec::new();
                *state = State::Osc1(buf1);
            }
            (b'[', State::ExpectingControlChar) => {
                *state = State::Csi;
            }
            (b'(', State::ExpectingControlChar) => {
                *state = State::G0;
            }
            (b'D', State::ExpectingControlChar) => {
                // ESCD Linefeed
                //debug!("{}", EscCode::from((&(*state), byte)));
                if cursor.1 == scroll_region.bottom {
                    grid.scroll_up(scroll_region, scroll_region.top, 1);
                } else {
                    cursor.1 += 1;
                }
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'J', State::ExpectingControlChar) => {
                // ESCJ Erase from the cursor to the end of the screen
                //debug!("sending {}", EscCode::from((&(*state), byte)));
                //debug!("erasing from {:?} to {:?}", cursor, terminal_size);
                for y in cursor.1..terminal_size.1 {
                    for x in cursor.0..terminal_size.0 {
                        grid[(x, y)] = Cell::default();
                    }
                }
                *state = State::Normal;
            }
            (b'K', State::ExpectingControlChar) => {
                // ESCK Erase from the cursor to the end of the line
                //debug!("sending {}", EscCode::from((&(*state), byte)));
                for x in cursor.0..terminal_size.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                *state = State::Normal;
            }
            (_, State::ExpectingControlChar) => {
                //debug!(
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
            (c, State::Osc1(ref mut buf)) if (b'0'..=b'9').contains(&c) || c == b'?' => {
                buf.push(c);
            }
            (b';', State::Osc1(ref mut buf1_p)) => {
                let buf1 = std::mem::replace(buf1_p, SmallVec::new());
                let buf2 = SmallVec::new();
                *state = State::Osc2(buf1, buf2);
            }
            (c, State::Osc2(_, ref mut buf)) if (b'0'..=b'9').contains(&c) || c == b'?' => {
                buf.push(c);
            }
            /* Normal */
            (b'\r', State::Normal) => {
                //debug!("carriage return x-> 0, cursor was: {:?}", cursor);
                cursor.0 = 0;
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
            }
            (b'\n', State::Normal) => {
                //debug!("setting cell {:?} char '{}'", cursor, c as char);
                //debug!("newline y-> y+1, cursor was: {:?}", cursor);

                if cursor.1 + 1 < terminal_size.1 || !is_alternate {
                    if cursor.1 == scroll_region.bottom && is_alternate {
                        grid.scroll_up(scroll_region, cursor.1, 1);
                    } else {
                        increase_cursor_y!();
                    }
                }
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
            }
            (b'', State::Normal) => {
                //debug!("Visual bell ^G, ignoring {:?}", cursor);
            }
            (0x08, State::Normal) => {
                /* Backspace */
                //debug!("backspace x-> x-1, cursor was: {:?}", cursor);
                if cursor.0 > 0 {
                    cursor.0 -= 1;
                }
                //debug!("cursor became: {:?}", cursor);
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
                        CodepointBuf::TwoCodepoints(b) => {
                            //debug!("two byte char = ");
                            unsafe { std::str::from_utf8_unchecked(&[*b, c]) }
                                .chars()
                                .next()
                                .unwrap()
                        }
                        CodepointBuf::ThreeCodepoints(b, Some(b1)) => {
                            //debug!("three byte char = ",);
                            unsafe { std::str::from_utf8_unchecked(&[*b, *b1, c]) }
                                .chars()
                                .next()
                                .unwrap()
                        }
                        CodepointBuf::ThreeCodepoints(_, ref mut b @ None) => {
                            *b = Some(c);
                            return;
                        }
                        CodepointBuf::FourCodepoints(b, Some(b1), Some(b2)) => {
                            //debug!("four byte char = ",);
                            unsafe { std::str::from_utf8_unchecked(&[*b, *b1, *b2, c]) }
                                .chars()
                                .next()
                                .unwrap()
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
                            //debug!(
                            //    "invalid utf8 sequence: codepoints = {:?} and c={}",
                            //    codepoints, c
                            //);
                            *codepoints = CodepointBuf::None;
                            return;
                        }
                    }
                };
                //debug!("c = {:?}\tcursor={:?}", c, cursor);
                *codepoints = CodepointBuf::None;
                if *auto_wrap_mode && *wrap_next {
                    *wrap_next = false;
                    if cursor.1 == scroll_region.bottom {
                        grid.scroll_up(scroll_region, scroll_region.top, 1);
                    } else {
                        cursor.1 += 1;
                    }
                    cursor.0 = 0;
                }

                //if c == '↪' {
                //debug!("↪ cursor is {:?}", cursor_val!());
                //}
                grid[cursor_val!()].set_ch(c);
                grid[cursor_val!()].set_fg(*fg_color);
                grid[cursor_val!()].set_bg(*bg_color);
                grid[cursor_val!()].set_attrs(*attrs);
                match wcwidth(u32::from(c)) {
                    Some(0) | None => {
                        /* Skip drawing zero width characters */
                        grid[cursor_val!()].set_empty(true);
                    }
                    Some(1) => {}
                    Some(n) => {
                        /* Grapheme takes more than one column, so the next cell will be
                         * drawn over. Set it as empty to skip drawing it. */
                        for _ in 1..n {
                            increase_cursor_x!();
                            grid[cursor_val!()].set_empty(true);
                            grid[cursor_val!()].set_fg(*fg_color);
                            grid[cursor_val!()].set_bg(*bg_color);
                            grid[cursor_val!()].set_attrs(*attrs);
                        }
                    }
                }
                increase_cursor_x!();
            }
            (b'u', State::Csi) => {
                /* restore cursor */
                //debug!("restore cursor {}", EscCode::from((&(*state), byte)));
                *show_cursor = true;
                *state = State::Normal;
            }
            (b'm', State::Csi) => {
                /* Reset character Attributes (SGR).  Ps = 0  -> Normal (default), VT100 */
                //debug!("{}", EscCode::from((&(*state), byte)));
                *fg_color = Color::Default;
                *bg_color = Color::Default;
                *attrs = Attr::DEFAULT;
                grid[cursor_val!()].set_fg(Color::Default);
                grid[cursor_val!()].set_bg(Color::Default);
                grid[cursor_val!()].set_attrs(Attr::DEFAULT);
                *state = State::Normal;
            }
            (b'C', State::Csi) => {
                // ESC[C    CSI Cursor Forward one Time
                //debug!("cursor forward one time, cursor was: {:?}", cursor);
                cursor.0 = std::cmp::min(cursor.0 + 1, terminal_size.0.saturating_sub(1));
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            /* CSI ? stuff */
            (c, State::CsiQ(ref mut buf)) if (b'0'..=b'9').contains(&c) => {
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
                        *show_cursor = true;
                        *prev_fg_color = Some(grid[cursor_val!()].fg());
                        *prev_bg_color = Some(grid[cursor_val!()].bg());
                        *prev_attrs = Some(grid[cursor_val!()].attrs());
                        grid[cursor_val!()].set_fg(Color::Black);
                        grid[cursor_val!()].set_bg(Color::White);
                        grid[cursor_val!()].set_attrs(Attr::DEFAULT);
                    }
                    b"1047" | b"1049" => {
                        *screen_buffer = ScreenBuffer::Alternate;
                    }
                    _ => {
                        debug!("unknown csi? {:?}", String::from_utf8_lossy(buf.as_slice()));
                    }
                }

                //debug!("{}", EscCode::from((&(*state), byte)));
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
                    b"25" => {
                        *show_cursor = false;
                        if let Some(fg_color) = prev_fg_color.take() {
                            grid[cursor_val!()].set_fg(fg_color);
                        } else {
                            grid[cursor_val!()].set_fg(*fg_color);
                        }
                        if let Some(bg_color) = prev_bg_color.take() {
                            grid[cursor_val!()].set_bg(bg_color);
                        } else {
                            grid[cursor_val!()].set_bg(*bg_color);
                        }
                        if let Some(attrs) = prev_attrs.take() {
                            grid[cursor_val!()].set_attrs(attrs);
                        } else {
                            grid[cursor_val!()].set_attrs(*attrs);
                        }
                    }
                    b"1047" | b"1049" => {
                        *screen_buffer = ScreenBuffer::Normal;
                    }
                    _ => {
                        debug!(
                            "unknown csi? `l` {:?}",
                            String::from_utf8_lossy(buf.as_slice())
                        );
                    }
                }
                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            /* END OF CSI ? stuff */
            (c, State::Csi) if (b'0'..=b'9').contains(&c) => {
                let mut buf1 = SmallVec::new();
                buf1.push(c);
                *state = State::Csi1(buf1);
            }
            (b'J', State::Csi) => {
                /* Erase in Display (ED), VT100.*/
                /* Erase Below (default). */
                clear_area(
                    grid,
                    (
                        (
                            0,
                            std::cmp::min(
                                cursor.1 + 1 + scroll_region.top,
                                terminal_size.1.saturating_sub(1),
                            ),
                        ),
                        (
                            terminal_size.0.saturating_sub(1),
                            terminal_size.1.saturating_sub(1),
                        ),
                    ),
                    Default::default(),
                );
                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'K', State::Csi) => {
                /* Erase in Line (ED), VT100.*/
                /* Erase to right (Default) */
                //debug!("{}", EscCode::from((&(*state), byte)));
                for x in cursor.0..terminal_size.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                *state = State::Normal;
            }
            (b'L', State::Csi) | (b'L', State::Csi1(_)) => {
                /* Insert n blank lines (default 1) */
                let n = if let State::Csi1(ref buf1) = state {
                    unsafe { std::str::from_utf8_unchecked(buf1) }
                        .parse::<usize>()
                        .unwrap()
                } else {
                    1
                };

                grid.scroll_down(scroll_region, cursor.1, n);

                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'M', State::Csi) | (b'M', State::Csi1(_)) => {
                /* Delete n lines (default 1) */
                let n = if let State::Csi1(ref buf1) = state {
                    unsafe { std::str::from_utf8_unchecked(buf1) }
                        .parse::<usize>()
                        .unwrap()
                } else {
                    1
                };

                grid.scroll_up(scroll_region, cursor.1, n);

                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'A', State::Csi) => {
                // Move cursor up 1 line
                //debug!("cursor up 1 times, cursor was: {:?}", cursor);
                if cursor.1 > 0 {
                    cursor.1 -= 1;
                } else {
                    //debug!("cursor.1 == 0");
                }
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'K', State::Csi1(buf)) if buf.as_ref() == b"0" => {
                /* Erase in Line (ED), VT100.*/
                /* Erase to right (Default) */
                //debug!("{}", EscCode::from((&(*state), byte)));
                for x in cursor.0..terminal_size.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                *state = State::Normal;
            }
            (b'K', State::Csi1(buf)) if buf.as_ref() == b"1" => {
                /* Erase in Line (ED), VT100.*/
                /* Erase to left (Default) */
                for x in 0..=cursor.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'K', State::Csi1(buf)) if buf.as_ref() == b"2" => {
                /* Erase in Line (ED), VT100.*/
                /* Erase all */
                for y in 0..terminal_size.1 {
                    for x in 0..terminal_size.0 {
                        grid[(x, y)] = Cell::default();
                    }
                }
                //debug!("{}", EscCode::from((&(*state), byte)));
                clear_area(
                    grid,
                    ((0, 0), pos_dec(*terminal_size, (1, 1))),
                    Default::default(),
                );
                *state = State::Normal;
            }
            (b'J', State::Csi1(ref buf)) if buf.as_ref() == b"0" => {
                /* Erase in Display (ED), VT100.*/
                /* Erase Below (default). */
                clear_area(
                    grid,
                    (
                        (
                            0,
                            std::cmp::min(
                                cursor.1 + 1 + scroll_region.top,
                                terminal_size.1.saturating_sub(1),
                            ),
                        ),
                        (
                            terminal_size.0.saturating_sub(1),
                            terminal_size.1.saturating_sub(1),
                        ),
                    ),
                    Default::default(),
                );
                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'J', State::Csi1(ref buf)) if buf.as_ref() == b"1" => {
                /* Erase in Display (ED), VT100.*/
                /* Erase Above */
                clear_area(
                    grid,
                    (
                        (0, 0),
                        (
                            terminal_size.0.saturating_sub(1),
                            cursor.1.saturating_sub(1) + scroll_region.top,
                        ),
                    ),
                    Default::default(),
                );
                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'J', State::Csi1(ref buf)) if buf.as_ref() == b"2" => {
                /* Erase in Display (ED), VT100.*/
                /* Erase All */
                clear_area(
                    grid,
                    ((0, 0), pos_dec(*terminal_size, (1, 1))),
                    Default::default(),
                );
                //debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'X', State::Csi1(ref buf)) => {
                /* Erase Ps Character(s) (default = 1) (ECH)..*/
                let ps = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();

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
                    grid[(cur_x, cur_y)] = Cell::default();
                    cur_x += 1;
                    ctr += 1;
                }
                //debug!("Erased {} Character(s)", ps);
                *state = State::Normal;
            }
            (b't', State::Csi1(buf)) => {
                /* Window manipulation */
                if buf.as_ref() == b"18" || buf.as_ref() == b"19" {
                    // Ps = 18 → Report the size of the text area in characters as CSI 8 ; height ; width t
                    //debug!("report size of the text area");
                    //debug!("got {}", EscCode::from((&(*state), byte)));
                    stdin.write_all(b"\x1b[8;").unwrap();
                    stdin
                        .write_all((terminal_size.1).to_string().as_bytes())
                        .unwrap();
                    stdin.write_all(&[b';']).unwrap();
                    stdin
                        .write_all((terminal_size.0).to_string().as_bytes())
                        .unwrap();
                    stdin.write_all(&[b't']).unwrap();
                    stdin.flush().unwrap();
                } else {
                    //debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                }
                *state = State::Normal;
            }
            (b'n', State::Csi1(_)) => {
                // Ps = 6  ⇒  Report Cursor Position (CPR) [row;column].
                // Result is CSI r ; c R
                //debug!("report cursor position");
                //debug!("got {}", EscCode::from((&(*state), byte)));
                stdin.write_all(&[b'\x1b', b'[']).unwrap();
                stdin
                    .write_all((cursor.1 + 1).to_string().as_bytes())
                    .unwrap();
                stdin.write_all(&[b';']).unwrap();
                stdin
                    .write_all((cursor.0 + 1).to_string().as_bytes())
                    .unwrap();
                stdin.write_all(&[b'R']).unwrap();
                stdin.flush().unwrap();
                *state = State::Normal;
            }
            (b'A', State::Csi1(buf)) => {
                // Move cursor up n lines
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                //debug!("cursor up {} times, cursor was: {:?}", offset, cursor);
                if cursor.1 >= offset {
                    cursor.1 -= offset;
                } else {
                    //debug!("offset > cursor.1");
                }
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'B', State::Csi1(buf)) => {
                // ESC[{buf}B   CSI Cursor Down {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                //debug!("cursor down {} times, cursor was: {:?}", offset, cursor);
                if cursor.1 == scroll_region.bottom {
                    /* scroll down */
                    for y in scroll_region.top..scroll_region.bottom {
                        for x in 0..terminal_size.1 {
                            grid[(x, y)] = grid[(x, y + 1)];
                        }
                    }
                    for x in 0..terminal_size.1 {
                        grid[(x, scroll_region.bottom)] = Cell::default();
                    }
                } else if offset + cursor.1 < terminal_size.1 {
                    cursor.1 += offset;
                }
                if scroll_region.top + cursor.1 >= terminal_size.1 {
                    cursor.1 = terminal_size.1.saturating_sub(1);
                }
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'D', State::Csi1(buf)) => {
                // ESC[{buf}D   CSI Cursor Backward {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                if cursor.0 >= offset {
                    cursor.0 -= offset;
                }
                //debug!(
                //   "ESC[ {} D cursor backwards cursor became: {:?}",
                //    offset, cursor
                // );
                *state = State::Normal;
            }
            (b'E', State::Csi1(buf)) => {
                // ESC[{buf}E   CSI Cursor Next Line {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                //debug!(
                //    "cursor next line {} times, cursor was: {:?}",
                //    offset, cursor
                //);
                if offset + cursor.1 < terminal_size.1 {
                    cursor.1 += offset;
                }
                if scroll_region.top + cursor.1 >= terminal_size.1 {
                    cursor.1 = terminal_size.1.saturating_sub(1);
                }
                cursor.0 = 0;
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'F', State::Csi1(buf)) => {
                // ESC[{buf}F   CSI Cursor Previous Line {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                //debug!(
                //    "cursor previous line {} times, cursor was: {:?}",
                //    offset, cursor
                //);
                cursor.1 = cursor.1.saturating_sub(offset);
                cursor.0 = 0;
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'G', State::Csi1(_)) | (b'G', State::Csi) => {
                // ESC[{buf}G   Cursor Character Absolute  [column={buf}] (default = [row,1])
                let new_col = if let State::Csi1(buf) = state {
                    unsafe { std::str::from_utf8_unchecked(buf) }
                        .parse::<usize>()
                        .unwrap()
                } else {
                    1
                };
                //debug!("cursor absolute {}, cursor was: {:?}", new_col, cursor);
                if new_col < terminal_size.0 {
                    cursor.0 = new_col.saturating_sub(1);
                } else {
                    //debug!(
                    //    "error: new_cal = {} > terminal.size.0 = {}\nterminal_size = {:?}",
                    //    new_col, terminal_size.0, terminal_size
                    //);
                }
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'C', State::Csi1(buf)) => {
                // ESC[{buf}C   CSI Cursor Forward {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                //debug!("cursor forward {} times, cursor was: {:?}", offset, cursor);
                if cursor.0 + offset < terminal_size.0 {
                    cursor.0 += offset;
                }
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'P', State::Csi1(_)) | (b'P', State::Csi) => {
                // ESC[{buf}P   CSI Delete {buf} characters, default = 1
                let offset = if let State::Csi1(buf) = state {
                    unsafe { std::str::from_utf8_unchecked(buf) }
                        .parse::<usize>()
                        .unwrap()
                } else {
                    1
                };

                for i in 0..(terminal_size.0 - cursor.0 - offset) {
                    grid[(cursor.0 + i, cursor.1)] = grid[(cursor.0 + i + offset, cursor.1)];
                }
                for x in (terminal_size.0 - offset)..terminal_size.0 {
                    grid[(x, cursor.1)].set_ch(' ');
                }
                //debug!(
                //    "Delete {} Character(s) with cursor at {:?}  ",
                //    offset, cursor
                //);
                *state = State::Normal;
            }
            (b'd', State::Csi1(_)) | (b'd', State::Csi) => {
                /* CSI Pm d Line Position Absolute [row] (default = [1,column]) (VPA). */
                let row = if let State::Csi1(buf) = state {
                    unsafe { std::str::from_utf8_unchecked(buf) }
                        .parse::<usize>()
                        .unwrap()
                } else {
                    1
                };
                //debug!(
                //    "Line position absolute row {} with cursor at {:?}",
                //    row, cursor
                //);
                cursor.1 = row.saturating_sub(1);
                if scroll_region.top + cursor.1 >= terminal_size.1 {
                    cursor.1 = terminal_size.1.saturating_sub(1);
                }
                *wrap_next = false;
                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b';', State::Csi1(ref mut buf1_p)) => {
                let buf1 = std::mem::replace(buf1_p, SmallVec::new());
                let buf2 = SmallVec::new();
                *state = State::Csi2(buf1, buf2);
            }
            (b'm', State::Csi1(ref buf1)) => {
                // Character Attributes.
                match buf1.as_slice() {
                    b"0" => {
                        *attrs = Attr::DEFAULT;
                    }
                    b"1" => {
                        /* bold */
                        *attrs |= Attr::BOLD;
                    }
                    b"2" => {
                        /* faint, dim */
                        *attrs |= Attr::DIM;
                    }
                    b"3" => {
                        /* italicized */
                        *attrs |= Attr::ITALICS;
                    }
                    b"4" => {
                        /* underlined */
                        *attrs |= Attr::UNDERLINE;
                    }
                    b"5" => {
                        /* blink */
                        *attrs |= Attr::BLINK;
                    }
                    b"7" => {
                        /* Inverse */
                        *attrs |= Attr::REVERSE;
                    }
                    b"8" => {
                        /* invisible */
                        *attrs |= Attr::HIDDEN;
                    }
                    b"9" => { /* crossed out */ }
                    b"21" => { /* Doubly-underlined */ }
                    b"22" => {
                        /* Normal (neither bold nor faint), ECMA-48 3rd. */
                        *attrs &= !(Attr::BOLD | Attr::DIM);
                    }
                    b"23" => {
                        /* Not italicized, ECMA-48 3rd */
                        *attrs &= !Attr::ITALICS;
                    }
                    b"24" => {
                        /* Not underlined, ECMA-48 3rd. */
                        *attrs &= !Attr::UNDERLINE;
                    }
                    b"25" => {
                        /* Steady (not blinking), ECMA-48 3rd. */
                        *attrs &= !Attr::BLINK;
                    }
                    b"27" => {
                        /* Positive (not inverse), ECMA-48 3rd. */
                        *attrs &= !Attr::REVERSE;
                    }
                    b"28" => {
                        /* Visible, i.e., not hidden, ECMA-48 3rd, VT300. */
                        *attrs &= !Attr::HIDDEN;
                    }
                    b"29" => { /* Not crossed-out, ECMA-48 3rd. */ }
                    b"30" => *fg_color = Color::Black,
                    b"31" => *fg_color = Color::Red,
                    b"32" => *fg_color = Color::Green,
                    b"33" => *fg_color = Color::Yellow,
                    b"34" => *fg_color = Color::Blue,
                    b"35" => *fg_color = Color::Magenta,
                    b"36" => *fg_color = Color::Cyan,
                    b"37" => *fg_color = Color::White,
                    b"39" => *fg_color = Color::Default,

                    b"40" => *bg_color = Color::Black,
                    b"41" => *bg_color = Color::Red,
                    b"42" => *bg_color = Color::Green,
                    b"43" => *bg_color = Color::Yellow,
                    b"44" => *bg_color = Color::Blue,
                    b"45" => *bg_color = Color::Magenta,
                    b"46" => *bg_color = Color::Cyan,
                    b"47" => *bg_color = Color::White,
                    b"49" => *bg_color = Color::Default,

                    b"90" => *fg_color = Color::Black,
                    b"91" => *fg_color = Color::Red,
                    b"92" => *fg_color = Color::Green,
                    b"93" => *fg_color = Color::Yellow,
                    b"94" => *fg_color = Color::Blue,
                    b"95" => *fg_color = Color::Magenta,
                    b"96" => *fg_color = Color::Cyan,
                    b"97" => *fg_color = Color::White,

                    b"100" => *bg_color = Color::Black,
                    b"101" => *bg_color = Color::Red,
                    b"102" => *bg_color = Color::Green,
                    b"103" => *bg_color = Color::Yellow,
                    b"104" => *bg_color = Color::Blue,
                    b"105" => *bg_color = Color::Magenta,
                    b"106" => *bg_color = Color::Cyan,
                    b"107" => *bg_color = Color::White,
                    _ => {
                        debug!(
                            "unknown attribute Csi1 {:?} m",
                            String::from_utf8_lossy(buf1.as_slice())
                        );
                    }
                }
                grid[cursor_val!()].set_fg(*fg_color);
                grid[cursor_val!()].set_bg(*bg_color);
                grid[cursor_val!()].set_attrs(*attrs);
                *state = State::Normal;
            }
            (b'm', State::Csi2(ref buf1, ref buf2)) => {
                for b in &[buf1, buf2] {
                    match b.as_slice() {
                        b"0" => {
                            *attrs = Attr::DEFAULT;
                        }
                        b"1" => {
                            /* bold */
                            *attrs |= Attr::BOLD;
                        }
                        b"2" => {
                            /* faint, dim */
                            *attrs |= Attr::DIM;
                        }
                        b"3" => {
                            /* italicized */
                            *attrs |= Attr::ITALICS;
                        }
                        b"4" => {
                            /* underlined */
                            *attrs |= Attr::UNDERLINE;
                        }
                        b"5" => {
                            /* blink */
                            *attrs |= Attr::BLINK;
                        }
                        b"7" => {
                            /* Inverse */
                            *attrs |= Attr::REVERSE;
                        }
                        b"8" => {
                            /* invisible */
                            *attrs |= Attr::HIDDEN;
                        }
                        b"9" => { /* crossed out */ }
                        b"21" => { /* Doubly-underlined */ }
                        b"22" => {
                            /* Normal (neither bold nor faint), ECMA-48 3rd. */
                            *attrs &= !(Attr::BOLD | Attr::DIM);
                        }
                        b"23" => {
                            /* Not italicized, ECMA-48 3rd */
                            *attrs &= !Attr::ITALICS;
                        }
                        b"24" => {
                            /* Not underlined, ECMA-48 3rd. */
                            *attrs &= !Attr::UNDERLINE;
                        }
                        b"25" => {
                            /* Steady (not blinking), ECMA-48 3rd. */
                            *attrs &= !Attr::BLINK;
                        }
                        b"27" => {
                            /* Positive (not inverse), ECMA-48 3rd. */
                            *attrs &= !Attr::REVERSE;
                        }
                        b"28" => {
                            /* Visible, i.e., not hidden, ECMA-48 3rd, VT300. */
                            *attrs &= !Attr::HIDDEN;
                        }
                        b"29" => { /* Not crossed-out, ECMA-48 3rd. */ }
                        b"30" => *fg_color = Color::Black,
                        b"31" => *fg_color = Color::Red,
                        b"32" => *fg_color = Color::Green,
                        b"33" => *fg_color = Color::Yellow,
                        b"34" => *fg_color = Color::Blue,
                        b"35" => *fg_color = Color::Magenta,
                        b"36" => *fg_color = Color::Cyan,
                        b"37" => *fg_color = Color::White,
                        b"39" => *fg_color = Color::Default,

                        b"40" => *bg_color = Color::Black,
                        b"41" => *bg_color = Color::Red,
                        b"42" => *bg_color = Color::Green,
                        b"43" => *bg_color = Color::Yellow,
                        b"44" => *bg_color = Color::Blue,
                        b"45" => *bg_color = Color::Magenta,
                        b"46" => *bg_color = Color::Cyan,
                        b"47" => *bg_color = Color::White,
                        b"49" => *bg_color = Color::Default,

                        b"90" => *fg_color = Color::Black,
                        b"91" => *fg_color = Color::Red,
                        b"92" => *fg_color = Color::Green,
                        b"93" => *fg_color = Color::Yellow,
                        b"94" => *fg_color = Color::Blue,
                        b"95" => *fg_color = Color::Magenta,
                        b"96" => *fg_color = Color::Cyan,
                        b"97" => *fg_color = Color::White,

                        b"100" => *bg_color = Color::Black,
                        b"101" => *bg_color = Color::Red,
                        b"102" => *bg_color = Color::Green,
                        b"103" => *bg_color = Color::Yellow,
                        b"104" => *bg_color = Color::Blue,
                        b"105" => *bg_color = Color::Magenta,
                        b"106" => *bg_color = Color::Cyan,
                        b"107" => *bg_color = Color::White,
                        _ => {
                            debug!(
                                "unknown attribute Csi1 {:?} m",
                                String::from_utf8_lossy(buf1.as_slice())
                            );
                        }
                    }
                }
                grid[cursor_val!()].set_fg(*fg_color);
                grid[cursor_val!()].set_bg(*bg_color);
                grid[cursor_val!()].set_attrs(*attrs);
                *state = State::Normal;
            }
            (c, State::Csi1(ref mut buf)) if (b'0'..=b'9').contains(&c) || c == b' ' => {
                buf.push(c);
            }
            (b';', State::Csi2(ref mut buf1_p, ref mut buf2_p)) => {
                let buf1 = std::mem::replace(buf1_p, SmallVec::new());
                let buf2 = std::mem::replace(buf2_p, SmallVec::new());
                let buf3 = SmallVec::new();
                *state = State::Csi3(buf1, buf2, buf3);
            }
            (b't', State::Csi2(_, _)) => {
                //debug!("ignoring {}", EscCode::from((&(*state), byte)));
                // Window manipulation, skip it
                *state = State::Normal;
            }
            (b'H', State::Csi2(_, _)) | (b'H', State::Csi) => {
                //Cursor Position [row;column] (default = [1,1]) (CUP).
                let (orig_x, mut orig_y) = if let State::Csi2(ref y, ref x) = state {
                    (
                        unsafe { std::str::from_utf8_unchecked(x) }
                            .parse::<usize>()
                            .unwrap_or(1),
                        unsafe { std::str::from_utf8_unchecked(y) }
                            .parse::<usize>()
                            .unwrap_or(1),
                    )
                } else {
                    (1, 1)
                };

                let (min_y, max_y) = if *origin_mode {
                    //debug!(*origin_mode);
                    orig_y += scroll_region.top;
                    (scroll_region.top, scroll_region.bottom)
                } else {
                    (0, terminal_size.1.saturating_sub(1))
                };

                cursor.0 = std::cmp::min(orig_x - 1, terminal_size.0.saturating_sub(1));
                cursor.1 = std::cmp::max(min_y, std::cmp::min(max_y, orig_y - 1));
                *wrap_next = false;

                //debug!("{}", EscCode::from((&(*state), byte)),);
                //debug!(
                //    "cursor set to ({},{}), cursor was: {:?}",
                //    orig_x, orig_y, cursor
                //);

                //debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (c, State::Csi2(_, ref mut buf)) if (b'0'..=b'9').contains(&c) => {
                buf.push(c);
            }
            (b'r', State::Csi2(_, _)) | (b'r', State::Csi) => {
                /* CSI Ps ; Ps r Set Scrolling Region [top;bottom] (default = full size of window) (DECSTBM). */
                let (top, bottom) = if let State::Csi2(ref top, ref bottom) = state {
                    (
                        unsafe { std::str::from_utf8_unchecked(top) }
                            .parse::<usize>()
                            .unwrap_or(1),
                        unsafe { std::str::from_utf8_unchecked(bottom) }
                            .parse::<usize>()
                            .unwrap_or(1),
                    )
                } else {
                    (1, terminal_size.1)
                };

                if bottom > top {
                    scroll_region.top = top - 1;
                    scroll_region.bottom = bottom - 1;
                    *cursor = (0, 0);
                    *wrap_next = false;
                }
                //debug!("set scrolling region to {:?}", scroll_region);
                *state = State::Normal;
            }
            (b't', State::Csi3(_, _, _)) => {
                //debug!("ignoring {}", EscCode::from((&(*state), byte)));
                // Window manipulation, skip it
                *state = State::Normal;
            }

            (c, State::Csi3(_, _, ref mut buf)) if (b'0'..=b'9').contains(&c) => {
                buf.push(c);
            }
            (b'm', State::Csi3(ref buf1, ref buf2, ref buf3))
                if buf1.as_ref() == b"38" && buf2.as_ref() == b"5" =>
            {
                /* Set character attributes | foreground color */
                *fg_color = if let Ok(byte) =
                    unsafe { std::str::from_utf8_unchecked(buf3) }.parse::<u8>()
                {
                    //debug!("parsed buf as {}", byte);
                    Color::Byte(byte)
                } else {
                    Color::Default
                };
                grid[cursor_val!()].set_fg(*fg_color);
                *state = State::Normal;
            }
            (b'm', State::Csi3(ref buf1, ref buf2, ref buf3))
                if buf1.as_ref() == b"48" && buf2.as_ref() == b"5" =>
            {
                /* Set character attributes | background color */
                *bg_color = if let Ok(byte) =
                    unsafe { std::str::from_utf8_unchecked(buf3) }.parse::<u8>()
                {
                    //debug!("parsed buf as {}", byte);
                    Color::Byte(byte)
                } else {
                    Color::Default
                };
                grid[cursor_val!()].set_bg(*bg_color);
                *state = State::Normal;
            }
            (c, State::Csi3(_, _, ref mut buf)) if (b'0'..=b'9').contains(&c) => {
                buf.push(c);
            }
            (b';', State::Csi3(ref mut buf1_p, ref mut buf2_p, ref mut buf3_p)) => {
                let buf1 = std::mem::replace(buf1_p, SmallVec::new());
                let buf2 = std::mem::replace(buf2_p, SmallVec::new());
                let buf3 = std::mem::replace(buf3_p, SmallVec::new());
                let buf4 = SmallVec::new();
                *state = State::Csi4(buf1, buf2, buf3, buf4);
            }
            (c, State::Csi4(_, _, _, ref mut buf)) if (b'0'..=b'9').contains(&c) => {
                buf.push(c);
            }
            (b';', State::Csi4(ref mut buf1_p, ref mut buf2_p, ref mut buf3_p, ref mut buf4_p)) => {
                let buf1 = std::mem::replace(buf1_p, SmallVec::new());
                let buf2 = std::mem::replace(buf2_p, SmallVec::new());
                let buf3 = std::mem::replace(buf3_p, SmallVec::new());
                let buf4 = std::mem::replace(buf4_p, SmallVec::new());
                let buf5 = SmallVec::new();
                *state = State::Csi5(buf1, buf2, buf3, buf4, buf5);
            }
            (c, State::Csi5(_, _, _, _, ref mut buf)) if (b'0'..=b'9').contains(&c) => {
                buf.push(c);
            }
            (
                b';',
                State::Csi5(
                    ref mut buf1_p,
                    ref mut buf2_p,
                    ref mut buf3_p,
                    ref mut buf4_p,
                    ref mut buf5_p,
                ),
            ) => {
                let buf1 = std::mem::replace(buf1_p, SmallVec::new());
                let buf2 = std::mem::replace(buf2_p, SmallVec::new());
                let buf3 = std::mem::replace(buf3_p, SmallVec::new());
                let buf4 = std::mem::replace(buf4_p, SmallVec::new());
                let buf5 = std::mem::replace(buf5_p, SmallVec::new());
                let buf6 = SmallVec::new();
                *state = State::Csi6(buf1, buf2, buf3, buf4, buf5, buf6);
            }
            (c, State::Csi6(_, _, _, _, _, ref mut buf)) if (b'0'..=b'9').contains(&c) => {
                buf.push(c);
            }
            (
                b'm',
                State::Csi6(
                    ref mut buf1,
                    ref mut buf2,
                    ref mut _color_space_buf,
                    ref mut r_buf,
                    ref mut g_buf,
                    ref mut b_buf,
                ),
            ) if buf1.as_ref() == b"38" && buf2.as_ref() == b"2" => {
                /* Set true foreground color */
                *fg_color = match (
                    unsafe { std::str::from_utf8_unchecked(r_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(g_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(b_buf) }.parse::<u8>(),
                ) {
                    (Ok(r), Ok(g), Ok(b)) => Color::Rgb(r, g, b),
                    _ => Color::Default,
                };
                grid[cursor_val!()].set_fg(*fg_color);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi6(
                    ref mut buf1,
                    ref mut buf2,
                    ref mut _color_space_buf,
                    ref mut r_buf,
                    ref mut g_buf,
                    ref mut b_buf,
                ),
            ) if buf1.as_ref() == b"48" && buf2.as_ref() == b"2" => {
                /* Set true background color */
                *bg_color = match (
                    unsafe { std::str::from_utf8_unchecked(r_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(g_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(b_buf) }.parse::<u8>(),
                ) {
                    (Ok(r), Ok(g), Ok(b)) => Color::Rgb(r, g, b),
                    _ => Color::Default,
                };
                grid[cursor_val!()].set_bg(*bg_color);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi5(
                    ref mut buf1,
                    ref mut buf2,
                    ref mut r_buf,
                    ref mut g_buf,
                    ref mut b_buf,
                ),
            ) if buf1.as_ref() == b"38" && buf2.as_ref() == b"2" => {
                /* Set true foreground color */
                *fg_color = match (
                    unsafe { std::str::from_utf8_unchecked(r_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(g_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(b_buf) }.parse::<u8>(),
                ) {
                    (Ok(r), Ok(g), Ok(b)) => Color::Rgb(r, g, b),
                    _ => Color::Default,
                };
                grid[cursor_val!()].set_fg(*fg_color);
                *state = State::Normal;
            }
            (
                b'm',
                State::Csi5(
                    ref mut buf1,
                    ref mut buf2,
                    ref mut r_buf,
                    ref mut g_buf,
                    ref mut b_buf,
                ),
            ) if buf1.as_ref() == b"48" && buf2.as_ref() == b"2" => {
                /* Set true background color */
                *bg_color = match (
                    unsafe { std::str::from_utf8_unchecked(r_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(g_buf) }.parse::<u8>(),
                    unsafe { std::str::from_utf8_unchecked(b_buf) }.parse::<u8>(),
                ) {
                    (Ok(r), Ok(g), Ok(b)) => Color::Rgb(r, g, b),
                    _ => Color::Default,
                };
                grid[cursor_val!()].set_bg(*bg_color);
                *state = State::Normal;
            }
            (b'q', State::Csi1(buf))
                if buf.len() == 2 && buf[1] == b' ' && (b'0'..=b'6').contains(&buf[0]) =>
            {
                /*
                  CSI Ps SP q
                  Set cursor style (DECSCUSR), VT520.
                  Ps = 0  ⇒  blinking block.
                  Ps = 1  ⇒  blinking block (default).
                  Ps = 2  ⇒  steady block.
                  Ps = 3  ⇒  blinking underline.
                  Ps = 4  ⇒  steady underline.
                  Ps = 5  ⇒  blinking bar, xterm.
                  Ps = 6  ⇒  steady bar, xterm.
                */
                *state = State::Normal;
            }
            (_, State::Csi) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Csi1(_)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Csi2(_, _)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Csi3(_, _, _)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Csi4(_, _, _, _)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Csi5(_, _, _, _, _)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Csi6(_, _, _, _, _, _)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Osc1(_)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::Osc2(_, _)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            (_, State::CsiQ(_)) => {
                debug!(
                    "state: {:?} ignoring unknown code {} byte {}",
                    &state,
                    EscCode::from((&(*state), byte)),
                    byte
                );
                *state = State::Normal;
            }
            /* other stuff */
            (_, State::G0) => {
                debug!("ignoring {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
        }
    }
}

impl Default for EmbedGrid {
    fn default() -> Self {
        Self::new()
    }
}
