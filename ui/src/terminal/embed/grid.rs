use super::*;
use crate::terminal::cells::*;
use melib::error::{MeliError, Result};
use nix::sys::wait::WaitStatus;
use nix::sys::wait::{waitpid, WaitPidFlag};
use text_processing::wcwidth;
/**
 * `EmbedGrid` manages the terminal grid state of the embed process.
 *
 * The embed process sends bytes to the master end (see super mod) and interprets them in a state
 * machine stored in `State`. Escape codes are translated as changes to the grid, eg changes in a
 * cell's colors.
 *
 * The main process copies the grid whenever the actual terminal is redrawn.
 **/

/// In a scroll region up and down cursor movements shift the region vertically. The new lines are
/// empty.
#[derive(Debug)]
struct ScrollRegion {
    top: usize,
    bottom: usize,
}

#[derive(Debug)]
pub struct EmbedGrid {
    cursor: (usize, usize),
    /// [top;bottom]
    scroll_region: ScrollRegion,
    pub grid: CellBuffer,
    pub state: State,
    pub stdin: std::fs::File,
    /// Pid of the embed process
    pub child_pid: nix::unistd::Pid,
    /// (width, height)
    pub terminal_size: (usize, usize),
    fg_color: Color,
    bg_color: Color,
    /// Store the fg/bg color when highlighting the cell where the cursor is so that it can be
    /// restored afterwards
    prev_fg_color: Option<Color>,
    prev_bg_color: Option<Color>,

    show_cursor: bool,
    origin_mode: bool,
    auto_wrap_mode: bool,
    /// If next grapheme should be placed in the next line
    /// This should be reset whenever the cursor value changes
    wrap_next: bool,
    /// Store state in case a multi-byte character is encountered
    codepoints: CodepointBuf,
}

#[derive(Debug, PartialEq)]
enum CodepointBuf {
    None,
    TwoCodepoints(Vec<u8>),
    ThreeCodepoints(Vec<u8>),
    FourCodepoints(Vec<u8>),
}

impl EmbedGrid {
    pub fn new(stdin: std::fs::File, child_pid: nix::unistd::Pid) -> Self {
        EmbedGrid {
            cursor: (0, 0),
            scroll_region: ScrollRegion { top: 0, bottom: 0 },
            terminal_size: (0, 0),
            grid: CellBuffer::default(),
            state: State::Normal,
            stdin,
            child_pid,
            fg_color: Color::Default,
            bg_color: Color::Default,
            prev_fg_color: None,
            prev_bg_color: None,
            show_cursor: true,
            auto_wrap_mode: true,
            wrap_next: false,
            origin_mode: false,
            codepoints: CodepointBuf::None,
        }
    }

    pub fn set_terminal_size(&mut self, new_val: (usize, usize)) {
        if new_val == self.terminal_size {
            return;
        }
        debug!("resizing to {:?}", new_val);
        self.scroll_region.top = 0;
        self.scroll_region.bottom = new_val.1.saturating_sub(1);

        self.terminal_size = new_val;
        self.grid.resize(new_val.0, new_val.1, Cell::default());
        self.grid.clear(Cell::default());
        self.cursor = (0, 0);
        self.wrap_next = false;
        let winsize = Winsize {
            ws_row: <u16>::try_from(new_val.1).unwrap(),
            ws_col: <u16>::try_from(new_val.0).unwrap(),
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        let master_fd = self.stdin.as_raw_fd();
        unsafe { set_window_size(master_fd, &winsize).unwrap() };
        nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGWINCH).unwrap();
    }

    pub fn wake_up(&self) {
        nix::sys::signal::kill(self.child_pid, nix::sys::signal::SIGCONT).unwrap();
    }

    pub fn stop(&self) {
        debug!("stopping");
        nix::sys::signal::kill(debug!(self.child_pid), nix::sys::signal::SIGSTOP).unwrap();
    }

    pub fn is_active(&self) -> Result<WaitStatus> {
        debug!(waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG),))
            .map_err(|e| MeliError::new(e.to_string()))
    }

    pub fn process_byte(&mut self, byte: u8) {
        let EmbedGrid {
            ref mut cursor,
            ref mut scroll_region,
            ref terminal_size,
            ref mut grid,
            ref mut state,
            ref mut stdin,
            ref mut fg_color,
            ref mut bg_color,
            ref mut prev_fg_color,
            ref mut prev_bg_color,
            ref mut codepoints,
            ref mut show_cursor,
            ref mut auto_wrap_mode,
            ref mut wrap_next,
            ref mut origin_mode,
            child_pid: _,
        } = self;

        macro_rules! increase_cursor_x {
            () => {
                if cursor.0 + 1 < terminal_size.0 {
                    cursor.0 += 1;
                } else if *auto_wrap_mode {
                    *wrap_next = true;
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
                std::cmp::min(
                    cursor.1 + scroll_region.top,
                    terminal_size.1.saturating_sub(1),
                )
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
                let buf1 = Vec::new();
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
                debug!("{}", EscCode::from((&(*state), byte)));
                if cursor.1 == scroll_region.bottom {
                    scroll_up(grid, scroll_region, scroll_region.top, 1);
                } else {
                    cursor.1 += 1;
                }
                *wrap_next = false;
                *state = State::Normal;
            }
            (b'J', State::ExpectingControlChar) => {
                // ESCJ Erase from the cursor to the end of the screen
                debug!("sending {}", EscCode::from((&(*state), byte)));
                debug!("erasing from {:?} to {:?}", cursor, terminal_size);
                for y in cursor.1..terminal_size.1 {
                    for x in cursor.0..terminal_size.0 {
                        grid[(x, y)] = Cell::default();
                    }
                }
                *state = State::Normal;
            }
            (b'K', State::ExpectingControlChar) => {
                // ESCK Erase from the cursor to the end of the line
                debug!("sending {}", EscCode::from((&(*state), byte)));
                for x in cursor.0..terminal_size.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                *state = State::Normal;
            }
            (_, State::ExpectingControlChar) => {
                debug!(
                    "unrecognised: byte is {} and state is {:?}",
                    byte as char, state
                );
                *state = State::Normal;
            }
            (b'?', State::Csi) => {
                let buf1 = Vec::new();
                *state = State::CsiQ(buf1);
            }
            /* OSC stuff */
            (c, State::Osc1(ref mut buf)) if (c >= b'0' && c <= b'9') || c == b'?' => {
                buf.push(c);
            }
            (b';', State::Osc1(ref mut buf1_p)) => {
                let buf1 = std::mem::replace(buf1_p, Vec::new());
                let buf2 = Vec::new();
                *state = State::Osc2(buf1, buf2);
            }
            (c, State::Osc2(_, ref mut buf)) if (c >= b'0' && c <= b'9') || c == b'?' => {
                buf.push(c);
            }
            /* Normal */
            (b'\r', State::Normal) => {
                debug!("carriage return x-> 0, cursor was: {:?}", cursor);
                cursor.0 = 0;
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
            }
            (b'\n', State::Normal) => {
                //debug!("setting cell {:?} char '{}'", cursor, c as char);
                debug!("newline y-> y+1, cursor was: {:?}", cursor);

                if cursor.1 + 1 < terminal_size.1 {
                    if cursor.1 == scroll_region.bottom {
                        scroll_up(grid, scroll_region, cursor.1, 1);
                    } else {
                        cursor.1 += 1;
                    }
                }
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
            }
            (b'', State::Normal) => {
                debug!("Visual bell ^G, ignoring {:?}", cursor);
            }
            (0x08, State::Normal) => {
                /* Backspace */
                debug!("backspace x-> x-1, cursor was: {:?}", cursor);
                if cursor.0 > 0 {
                    cursor.0 -= 1;
                }
                debug!("cursor became: {:?}", cursor);
            }
            (c, State::Normal) => {
                /* Character to be printed. */
                let c = if *codepoints == CodepointBuf::None && c & 0x80 == 0 {
                    /* This is a one byte char */
                    c as char
                } else {
                    match codepoints {
                        CodepointBuf::None if c & 0b1110_0000 == 0b1100_0000 => {
                            *codepoints = CodepointBuf::TwoCodepoints(vec![c]);
                            return;
                        }
                        CodepointBuf::None if c & 0b1111_0000 == 0b1110_0000 => {
                            *codepoints = CodepointBuf::ThreeCodepoints(vec![c]);
                            return;
                        }
                        CodepointBuf::None if c & 0b1111_1000 == 0b1111_0000 => {
                            *codepoints = CodepointBuf::FourCodepoints(vec![c]);
                            return;
                        }
                        CodepointBuf::TwoCodepoints(buf) => {
                            debug!("two byte char = ");
                            unsafe { std::str::from_utf8_unchecked(&[buf[0], c]) }
                                .chars()
                                .next()
                                .unwrap()
                        }
                        CodepointBuf::ThreeCodepoints(buf) if buf.len() == 2 => {
                            debug!("three byte char = ",);
                            unsafe { std::str::from_utf8_unchecked(&[buf[0], buf[1], c]) }
                                .chars()
                                .next()
                                .unwrap()
                        }
                        CodepointBuf::ThreeCodepoints(buf) => {
                            buf.push(c);
                            return;
                        }
                        CodepointBuf::FourCodepoints(buf) if buf.len() == 3 => {
                            debug!("four byte char = ",);
                            unsafe { std::str::from_utf8_unchecked(&[buf[0], buf[1], buf[2], c]) }
                                .chars()
                                .next()
                                .unwrap()
                        }
                        CodepointBuf::FourCodepoints(buf) => {
                            buf.push(c);
                            return;
                        }
                        _ => {
                            debug!(
                                "invalid utf8 sequence: codepoints = {:?} and c={}",
                                codepoints, c
                            );
                            *codepoints = CodepointBuf::None;
                            return;
                        }
                    }
                };
                debug!("c = {:?}\tcursor={:?}", c, cursor);
                *codepoints = CodepointBuf::None;
                if *auto_wrap_mode && *wrap_next {
                    *wrap_next = false;
                    if cursor.1 == scroll_region.bottom {
                        scroll_up(grid, scroll_region, scroll_region.top, 1);
                    } else {
                        cursor.1 += 1;
                    }
                    cursor.0 = 0;
                }

                if c == '↪' {
                    debug!("↪ cursor is {:?}", cursor_val!());
                }
                grid[cursor_val!()].set_ch(c);
                grid[cursor_val!()].set_fg(*fg_color);
                grid[cursor_val!()].set_bg(*bg_color);
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
                        }
                    }
                }
                increase_cursor_x!();
            }
            (b'u', State::Csi) => {
                /* restore cursor */
                debug!("restore cursor {}", EscCode::from((&(*state), byte)));
                *show_cursor = true;
                *state = State::Normal;
            }
            (b'm', State::Csi) => {
                /* Reset character Attributes (SGR).  Ps = 0  -> Normal (default), VT100 */
                debug!("{}", EscCode::from((&(*state), byte)));
                *fg_color = Color::Default;
                *bg_color = Color::Default;
                grid[cursor_val!()].set_fg(Color::Default);
                grid[cursor_val!()].set_bg(Color::Default);
                *state = State::Normal;
            }
            (b'C', State::Csi) => {
                // ESC[C    CSI Cursor Forward one Time
                debug!("cursor forward one time, cursor was: {:?}", cursor);
                cursor.0 = std::cmp::min(cursor.0 + 1, terminal_size.0.saturating_sub(1));
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            /* CSI ? stuff */
            (c, State::CsiQ(ref mut buf)) if c >= b'0' && c <= b'9' => {
                buf.push(c);
            }
            (b'h', State::CsiQ(ref buf)) => {
                match buf.as_slice() {
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
                        grid[cursor_val!()].set_fg(Color::Black);
                        grid[cursor_val!()].set_bg(Color::White);
                    }
                    _ => {}
                }

                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'l', State::CsiQ(ref mut buf)) => {
                match buf.as_slice() {
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
                    }
                    _ => {}
                }
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            /* END OF CSI ? stuff */
            (c, State::Csi) if c >= b'0' && c <= b'9' => {
                let mut buf1 = Vec::new();
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
                );
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'K', State::Csi) => {
                /* Erase in Line (ED), VT100.*/
                /* Erase to right (Default) */
                debug!("{}", EscCode::from((&(*state), byte)));
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

                scroll_down(grid, scroll_region, cursor.1, n);

                debug!("{}", EscCode::from((&(*state), byte)));
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

                scroll_up(grid, scroll_region, cursor.1, n);

                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'A', State::Csi) => {
                // Move cursor up 1 line
                debug!("cursor up 1 times, cursor was: {:?}", cursor);
                if cursor.1 > 0 {
                    cursor.1 -= 1;
                } else {
                    debug!("cursor.1 == 0");
                }
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'K', State::Csi1(buf)) if buf == b"0" => {
                /* Erase in Line (ED), VT100.*/
                /* Erase to right (Default) */
                debug!("{}", EscCode::from((&(*state), byte)));
                for x in cursor.0..terminal_size.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                *state = State::Normal;
            }
            (b'K', State::Csi1(buf)) if buf == b"1" => {
                /* Erase in Line (ED), VT100.*/
                /* Erase to left (Default) */
                for x in 0..=cursor.0 {
                    grid[(x, cursor.1)] = Cell::default();
                }
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'K', State::Csi1(buf)) if buf == b"2" => {
                /* Erase in Line (ED), VT100.*/
                /* Erase all */
                for y in 0..terminal_size.1 {
                    for x in 0..terminal_size.0 {
                        grid[(x, y)] = Cell::default();
                    }
                }
                debug!("{}", EscCode::from((&(*state), byte)));
                clear_area(grid, ((0, 0), pos_dec(*terminal_size, (1, 1))));
                *state = State::Normal;
            }
            (b'J', State::Csi1(ref buf)) if buf == b"0" => {
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
                );
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'J', State::Csi1(ref buf)) if buf == b"1" => {
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
                );
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'J', State::Csi1(ref buf)) if buf == b"2" => {
                /* Erase in Display (ED), VT100.*/
                /* Erase All */
                clear_area(grid, ((0, 0), pos_dec(*terminal_size, (1, 1))));
                debug!("{}", EscCode::from((&(*state), byte)));
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
                debug!("Erased {} Character(s)", ps);
                *state = State::Normal;
            }
            (b't', State::Csi1(buf)) => {
                /* Window manipulation */
                if buf == b"18" || buf == b"19" {
                    // Ps = 18 → Report the size of the text area in characters as CSI 8 ; height ; width t
                    debug!("report size of the text area");
                    debug!("got {}", EscCode::from((&(*state), byte)));
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
                    debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                }
                *state = State::Normal;
            }
            (b'n', State::Csi1(_)) => {
                // Ps = 6  ⇒  Report Cursor Position (CPR) [row;column].
                // Result is CSI r ; c R
                debug!("report cursor position");
                debug!("got {}", EscCode::from((&(*state), byte)));
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
                debug!("cursor up {} times, cursor was: {:?}", offset, cursor);
                if cursor.1 >= offset {
                    cursor.1 -= offset;
                } else {
                    debug!("offset > cursor.1");
                }
                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'B', State::Csi1(buf)) => {
                // ESC[{buf}B   CSI Cursor Down {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                debug!("cursor down {} times, cursor was: {:?}", offset, cursor);
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
                debug!("cursor became: {:?}", cursor);
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
                debug!(
                    "ESC[ {} D cursor backwards cursor became: {:?}",
                    offset, cursor
                );
                *state = State::Normal;
            }
            (b'E', State::Csi1(buf)) => {
                // ESC[{buf}E   CSI Cursor Next Line {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                debug!(
                    "cursor next line {} times, cursor was: {:?}",
                    offset, cursor
                );
                if offset + cursor.1 < terminal_size.1 {
                    cursor.1 += offset;
                }
                if scroll_region.top + cursor.1 >= terminal_size.1 {
                    cursor.1 = terminal_size.1.saturating_sub(1);
                }
                cursor.0 = 0;
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'F', State::Csi1(buf)) => {
                // ESC[{buf}F   CSI Cursor Previous Line {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                debug!(
                    "cursor previous line {} times, cursor was: {:?}",
                    offset, cursor
                );
                cursor.1 = cursor.1.saturating_sub(offset);
                cursor.0 = 0;
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
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
                debug!("cursor absolute {}, cursor was: {:?}", new_col, cursor);
                if new_col < terminal_size.0 {
                    cursor.0 = new_col.saturating_sub(1);
                } else {
                    debug!(
                        "error: new_cal = {} > terminal.size.0 = {}\nterminal_size = {:?}",
                        new_col, terminal_size.0, terminal_size
                    );
                }
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b'C', State::Csi1(buf)) => {
                // ESC[{buf}C   CSI Cursor Forward {buf} Times
                let offset = unsafe { std::str::from_utf8_unchecked(buf) }
                    .parse::<usize>()
                    .unwrap();
                debug!("cursor forward {} times, cursor was: {:?}", offset, cursor);
                if cursor.0 + offset < terminal_size.0 {
                    cursor.0 += offset;
                }
                debug!("cursor became: {:?}", cursor);
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
                debug!(
                    "Delete {} Character(s) with cursor at {:?}  ",
                    offset, cursor
                );
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
                debug!(
                    "Line position absolute row {} with cursor at {:?}",
                    row, cursor
                );
                cursor.1 = row.saturating_sub(1);
                if scroll_region.top + cursor.1 >= terminal_size.1 {
                    cursor.1 = terminal_size.1.saturating_sub(1);
                }
                *wrap_next = false;
                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (b';', State::Csi1(ref mut buf1_p)) => {
                let buf1 = std::mem::replace(buf1_p, Vec::new());
                let buf2 = Vec::new();
                *state = State::Csi2(buf1, buf2);
            }
            (b'm', State::Csi1(ref buf1)) => {
                // Character Attributes.
                match buf1.as_slice() {
                    b"0" => {
                        *fg_color = Color::Default;
                        *bg_color = Color::Default;
                    }
                    b"1" => { /* bold */ }
                    b"7" => {
                        /* Inverse */
                        let temp = *fg_color;
                        *fg_color = *bg_color;
                        *bg_color = temp;
                    }
                    b"27" => {
                        /* Inverse off */
                        let temp = *fg_color;
                        *fg_color = *bg_color;
                        *bg_color = temp;
                    }
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
                    _ => {}
                }
                grid[cursor_val!()].set_fg(*fg_color);
                grid[cursor_val!()].set_bg(*bg_color);
                *state = State::Normal;
            }
            (b'm', State::Csi2(ref buf1, ref buf2)) => {
                for b in &[buf1, buf2] {
                    match b.as_slice() {
                        b"0" => {
                            *fg_color = Color::Default;
                            *bg_color = Color::Default;
                        }
                        b"1" => { /* bold */ }
                        b"7" => {
                            /* Inverse */
                            let temp = *fg_color;
                            *fg_color = *bg_color;
                            *bg_color = temp;
                        }
                        b"27" => {
                            /* Inverse off */
                            let temp = *fg_color;
                            *fg_color = *bg_color;
                            *bg_color = temp;
                        }
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
                        _ => {}
                    }
                }
                grid[cursor_val!()].set_fg(*fg_color);
                grid[cursor_val!()].set_bg(*bg_color);
                *state = State::Normal;
            }
            (c, State::Csi1(ref mut buf)) if (c >= b'0' && c <= b'9') || c == b' ' => {
                buf.push(c);
            }
            (b';', State::Csi2(ref mut buf1_p, ref mut buf2_p)) => {
                let buf1 = std::mem::replace(buf1_p, Vec::new());
                let buf2 = std::mem::replace(buf2_p, Vec::new());
                let buf3 = Vec::new();
                *state = State::Csi3(buf1, buf2, buf3);
            }
            (b't', State::Csi2(_, _)) => {
                debug!("ignoring {}", EscCode::from((&(*state), byte)));
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
                    debug!(*origin_mode);
                    orig_y += scroll_region.top;
                    (scroll_region.top, scroll_region.bottom)
                } else {
                    (0, terminal_size.1.saturating_sub(1))
                };

                cursor.0 = std::cmp::min(orig_x - 1, terminal_size.0.saturating_sub(1));
                cursor.1 = std::cmp::max(min_y, std::cmp::min(max_y, orig_y - 1));
                *wrap_next = false;

                debug!("{}", EscCode::from((&(*state), byte)),);
                debug!(
                    "cursor set to ({},{}), cursor was: {:?}",
                    orig_x, orig_y, cursor
                );

                debug!("cursor became: {:?}", cursor);
                *state = State::Normal;
            }
            (c, State::Csi2(_, ref mut buf)) if c >= b'0' && c <= b'9' => {
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
                debug!("set scrolling region to {:?}", scroll_region);
                *state = State::Normal;
            }
            (b't', State::Csi3(_, _, _)) => {
                debug!("ignoring {}", EscCode::from((&(*state), byte)));
                // Window manipulation, skip it
                *state = State::Normal;
            }

            (c, State::Csi3(_, _, ref mut buf)) if c >= b'0' && c <= b'9' => {
                buf.push(c);
            }
            (b'm', State::Csi3(ref buf1, ref buf2, ref buf3)) if buf1 == b"38" && buf2 == b"5" => {
                /* Set character attributes | foreground color */
                *fg_color = if let Ok(byte) =
                    u8::from_str_radix(unsafe { std::str::from_utf8_unchecked(buf3) }, 10)
                {
                    debug!("parsed buf as {}", byte);
                    Color::Byte(byte)
                } else {
                    Color::Default
                };
                grid[cursor_val!()].set_fg(*fg_color);
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (b'm', State::Csi3(ref buf1, ref buf2, ref buf3)) if buf1 == b"48" && buf2 == b"5" => {
                /* Set character attributes | background color */
                *bg_color = if let Ok(byte) =
                    u8::from_str_radix(unsafe { std::str::from_utf8_unchecked(buf3) }, 10)
                {
                    debug!("parsed buf as {}", byte);
                    Color::Byte(byte)
                } else {
                    Color::Default
                };
                grid[cursor_val!()].set_bg(*bg_color);
                debug!("{}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::Csi) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::Csi1(_)) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::Csi2(_, _)) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::Csi3(_, _, _)) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::Osc1(_)) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::Osc2(_, _)) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
                *state = State::Normal;
            }
            (_, State::CsiQ(_)) => {
                debug!("ignoring unknown code {}", EscCode::from((&(*state), byte)));
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

#[inline(always)]
/// Performs the normal scroll up motion:
///
/// First clear offset number of lines:
///
/// For offset = 1, top = 1:
///
///  | 111111111111 |            |              |
///  | 222222222222 |            | 222222222222 |
///  | 333333333333 |            | 333333333333 |
///  | 444444444444 |    -->     | 444444444444 |
///  | 555555555555 |            | 555555555555 |
///  | 666666666666 |            | 666666666666 |
///
///  In each step, swap the current line with the next by offset:
///
///  |              |            | 222222222222 |
///  | 222222222222 |            |              |
///  | 333333333333 |            | 333333333333 |
///  | 444444444444 |    -->     | 444444444444 |
///  | 555555555555 |            | 555555555555 |
///  | 666666666666 |            | 666666666666 |
///
///  Result:
///    Before                      After
///  | 111111111111 |            | 222222222222 |
///  | 222222222222 |            | 333333333333 |
///  | 333333333333 |            | 444444444444 |
///  | 444444444444 |            | 555555555555 |
///  | 555555555555 |            | 666666666666 |
///  | 666666666666 |            |              |
///
fn scroll_up(grid: &mut CellBuffer, scroll_region: &ScrollRegion, top: usize, offset: usize) {
    //debug!(
    //    "scroll_up scroll_region {:?}, top: {} offset {}",
    //    scroll_region, top, offset
    //);
    for y in top..=(top + offset - 1) {
        for x in 0..grid.size().0 {
            grid[(x, y)] = Cell::default();
        }
    }
    for y in top..=(scroll_region.bottom - offset) {
        for x in 0..grid.size().0 {
            let temp = grid[(x, y)];
            grid[(x, y)] = grid[(x, y + offset)];
            grid[(x, y + offset)] = temp;
        }
    }
}

#[inline(always)]
/// Performs the normal scroll down motion:
///
/// First clear offset number of lines:
///
/// For offset = 1, top = 1:
///
///  | 111111111111 |            | 111111111111 |
///  | 222222222222 |            | 222222222222 |
///  | 333333333333 |            | 333333333333 |
///  | 444444444444 |    -->     | 444444444444 |
///  | 555555555555 |            | 555555555555 |
///  | 666666666666 |            |              |
///
///  In each step, swap the current line with the prev by offset:
///
///  | 111111111111 |            | 111111111111 |
///  | 222222222222 |            | 222222222222 |
///  | 333333333333 |            | 333333333333 |
///  | 444444444444 |    -->     | 444444444444 |
///  | 555555555555 |            |              |
///  |              |            | 555555555555 |
///
///  Result:
///    Before                      After
///  | 111111111111 |            |              |
///  | 222222222222 |            | 111111111111 |
///  | 333333333333 |            | 222222222222 |
///  | 444444444444 |            | 333333333333 |
///  | 555555555555 |            | 444444444444 |
///  | 666666666666 |            | 555555555555 |
///
fn scroll_down(grid: &mut CellBuffer, scroll_region: &ScrollRegion, top: usize, offset: usize) {
    //debug!(
    //    "scroll_down scroll_region {:?}, top: {} offset {}",
    //    scroll_region, top, offset
    //);
    for y in (scroll_region.bottom - offset + 1)..=scroll_region.bottom {
        for x in 0..grid.size().0 {
            grid[(x, y)] = Cell::default();
        }
    }

    for y in ((top + offset)..=scroll_region.bottom).rev() {
        for x in 0..grid.size().0 {
            let temp = grid[(x, y)];
            grid[(x, y)] = grid[(x, y - offset)];
            grid[(x, y - offset)] = temp;
        }
    }
}
