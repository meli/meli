//
// meli
//
// Copyright 2017 - Manos Pitsidianakis
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

/// Setting mode value in ANSI or DEC report sequences.
///
/// See <https://vt100.net/docs/vt510-rm/DECRPM.html>.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[repr(u8)]
pub enum ANSIDECModeSetting {
    #[default]
    ModeNotRecognized = 0,
    Set = 1,
    Reset = 2,
    PermanentlySet = 3,
    PermanentlyReset = 4,
}

impl ANSIDECModeSetting {
    #[inline]
    pub fn is_set(&self) -> bool {
        matches!(self, Self::Set | Self::PermanentlySet)
    }

    #[inline]
    pub fn is_reset(&self) -> bool {
        matches!(self, Self::Reset)
    }
}

/// Report Mode, Terminal to Host.
///
/// See <https://vt100.net/docs/vt510-rm/DECRPM.html>.
///
/// Format is:
///
/// ```text
/// CSI ? Pd ; Ps $ y
/// ```
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DECRPMReport {
    WaitingForSemicolon {
        mode: u16,
    },
    Semicolon {
        mode: u16,
    },
    WaitingForDollar {
        mode: u16,
        state: ANSIDECModeSetting,
    },
    WaitingForEnd {
        mode: u16,
        state: ANSIDECModeSetting,
    },
}

#[derive(Debug, Default)]
/// Main process sends commands to the input thread.
pub enum InputCommand {
    #[default]
    /// Exit thread
    Kill,
}

#[cfg(unix)]
pub use unix::get_events;

#[cfg(unix)]
pub mod unix {
    use std::{
        collections::VecDeque,
        num::NonZeroU16,
        os::fd::{AsFd as _, AsRawFd as _, OwnedFd},
    };

    use crossbeam::{
        channel::{Receiver, Sender},
        select,
    };
    use nix::poll::{poll, PollFd, PollFlags, PollTimeout};

    use crate::{
        terminal::{
            color::RgbColor,
            input::{ANSIDECModeSetting, DECRPMReport, InputCommand},
            keys::{Key, MouseButton, MouseEvent},
            osc, EscapeSequenceQuery as _,
        },
        types::{TerminalEvent, ThreadEvent},
    };

    /// An event reported by the terminal.
    #[derive(Debug, Clone, PartialEq, Eq)]
    enum Event {
        /// A key press.
        Key(Key),
        /// A mouse button press, release or wheel use at specific coordinates.
        Mouse(MouseEvent),
        Unsupported,
        /// Start of DECRPM
        ReportModeStart,
        BracketPasteStart,
        BracketPasteEnd,
        FocusIn,
        FocusOut,
        CursorPosition {
            column: NonZeroU16,
            row: NonZeroU16,
        },
    }

    #[derive(Debug, Eq, PartialEq)]
    /// Keep track of whether we're accepting normal user input or a pasted string.
    enum InputMode {
        Normal,
        EscapeSequence(Vec<u8>),
        #[allow(clippy::upper_case_acronyms)]
        /// Report Mode, Terminal to Host.
        DECRPM(DECRPMReport),
        Paste(Vec<u8>),
    }

    /// The thread function that listens for user input and forwards it to the main
    /// event loop.
    ///
    /// If we fork (for example start `$EDITOR`) we want the `input-thread` to stop
    /// reading from stdin. The best way I came up with right now is to send a
    /// signal to the thread that is read in the first input in stdin after the
    /// fork, and then the thread kills itself. The parent process spawns a new
    /// input-thread when the child returns.
    ///
    /// The main loop uses [`crate::state::State::try_wait_on_children`] to check if
    /// child has exited.
    pub fn get_events(
        state_tx: Sender<ThreadEvent>,
        rx: &Receiver<InputCommand>,
        new_command_fd: &OwnedFd,
        working: std::sync::Arc<()>,
    ) {
        let stdin = std::io::stdin();
        let stdin2 = std::io::stdin();
        let mut input_mode = InputMode::Normal;
        let mut esc_seq_buf = vec![];
        let mut palette = (None, None);
        let mut paste_buf = String::with_capacity(256);
        let mut stdin_iter = EventsAndRaw::new(stdin);
        'poll_while: loop {
            let mut poll_fds = [
                PollFd::new(stdin2.as_fd(), PollFlags::POLLIN),
                PollFd::new(new_command_fd.as_fd(), PollFlags::POLLIN),
            ];
            let Ok(_n_raw) = poll(&mut poll_fds, PollTimeout::NONE) else {
                break 'poll_while;
            };
            select! {
                default => {
                    if poll_fds[0].revents().is_some() {
                        'stdin_while: for c in stdin_iter.by_ref() {
                            match (c, &mut input_mode) {
                                (Ok((Event::Key(Key::Alt(']')), _)), InputMode::Normal)=> {
                                    esc_seq_buf.clear();
                                    esc_seq_buf.extend_from_slice(b"\x1b]");
                                    input_mode = InputMode::EscapeSequence(std::mem::take(&mut esc_seq_buf));

                                    continue 'stdin_while;
                                }
                                (Ok((Event::Key(Key::Alt('\\')), _)), InputMode::EscapeSequence(ref mut buf)) => {
                                    esc_seq_buf = std::mem::take(buf);
                                    input_mode = InputMode::Normal;
                                    log::trace!("EscapeSequence is {esc_seq_buf:?} == {:?}", String::from_utf8_lossy(&esc_seq_buf));
                                    if let Some(bg) = osc::QueryBackground::parse(&esc_seq_buf) {
                                        log::trace!("EscapeSequence parsed bg {bg:?}");
                                        palette.1 = Some(bg);
                                    } else if let Some(fg) = osc::QueryForeground::parse(&esc_seq_buf) {
                                        log::trace!("EscapeSequence parsed fg {fg:?}");
                                        palette.0 = Some(fg);
                                    } else if let Some(cursor) = osc::QueryCursorColor::parse(&esc_seq_buf) {
                                        log::trace!("EscapeSequence parsed cursor color {cursor:?}");
                                        state_tx.send(TerminalEvent::CursorColor { cursor }.into()).unwrap();
                                    } else {
                                        log::trace!("EscapeSequence unknown");
                                    }
                                    if let (Some(fg), Some(bg)) = palette {
                                        log::trace!("compute_scheme_contrast(fg {fg:?}, bg {bg:?}) = {:?}", RgbColor::compute_scheme_contrast(fg, bg));
                                        palette.0.take();
                                        palette.1.take();
                                        state_tx.send(TerminalEvent::ColorScheme { fg, bg }.into()).unwrap();
                                    }
                                    continue 'stdin_while;
                                }
                                (Ok((Event::Key(_), ref bytes)), InputMode::EscapeSequence(ref mut buf)) => {
                                    buf.extend(bytes);
                                    continue 'stdin_while;
                                }
                                (Ok((Event::Key(k), bytes)), InputMode::Normal) => {
                                    state_tx.send(ThreadEvent::Input((k, bytes))).unwrap();
                                    continue 'poll_while;
                                }
                                (
                                    Ok((Event::Key(Key::Char(k)), ref mut bytes)), InputMode::Paste(ref mut buf),
                                ) => {
                                    paste_buf.push(k);
                                    let bytes = std::mem::take(bytes);
                                    buf.extend(bytes.into_iter());
                                    continue 'stdin_while;
                                }
                                (Ok((Event::BracketPasteStart, _)), _) => {
                                    input_mode = InputMode::Paste(Vec::new());
                                    continue 'stdin_while;
                                }
                                (Ok((Event::BracketPasteEnd, _)), InputMode::Paste(ref mut buf)) => {
                                    let buf = std::mem::take(buf);
                                    input_mode = InputMode::Normal;
                                    let ret = Key::from(&paste_buf);
                                    paste_buf.clear();
                                    state_tx.send(ThreadEvent::Input((ret, buf))).unwrap();
                                    continue 'poll_while;
                                }
                                (Ok((Event::Mouse(mev), bytes)), InputMode::Normal) => {
                                    state_tx.send(ThreadEvent::Input((Key::Mouse(mev), bytes))).unwrap();
                                    continue 'poll_while;
                                    }
                                (Ok((Event::ReportModeStart, _)), InputMode::Normal)  => {
                                    // DECRPM - Report Mode - Terminal To Host
                                    esc_seq_buf.clear();
                                    input_mode = InputMode::DECRPM(DECRPMReport::WaitingForSemicolon { mode: 0});
                                }
                                (Ok((Event::Key(Key::Char(k)), _)), InputMode::DECRPM(ref report_state)) => {
                                    // CSI ? Pd ; Ps $ y
                                    match (k, report_state) {
                                        (d, DECRPMReport::WaitingForSemicolon { mode }) if d.is_ascii_digit() => {
                                            if let Some(mut mode) = mode.checked_mul(10) {
                                                // SAFETY: we performed an char::is_ascii_digit() check in
                                                // the guard above.
                                                mode += (d as u8 - b'0') as u16;
                                                input_mode = InputMode::DECRPM(DECRPMReport::WaitingForSemicolon { mode });
                                            }
                                        },
                                        (';', DECRPMReport::WaitingForSemicolon { mode }) => {
                                            input_mode = InputMode::DECRPM(DECRPMReport::Semicolon { mode: *mode });
                                        },
                                        (other, DECRPMReport::WaitingForSemicolon { mode }) => {
                                            log::trace!("Received invalid DECRPM response: Was waiting for an ASCII digit or `;` after `Pd` argument (mode, whose value was currently {mode:?} but instead got character {other:?}");
                                            // Revert to normal input mode, to prevent locking
                                            // up the user's terminal input
                                            input_mode = InputMode::Normal;
                                        }
                                        (d, DECRPMReport::Semicolon { mode }) if d.is_ascii_digit() => {
                                            let state = match d {
                                                '0' => ANSIDECModeSetting::ModeNotRecognized,
                                                '1' => ANSIDECModeSetting::Set,
                                                '2' => ANSIDECModeSetting::Reset,
                                                '3' => ANSIDECModeSetting::PermanentlySet,
                                                '4' => ANSIDECModeSetting::PermanentlyReset,
                                                other => {
                                                    log::trace!("Received invalid DECRPM setting value: {other:?}: expected one of {{0, 1, 2, 3, 4}}");
                                                    ANSIDECModeSetting::default()
                                                }
                                            };
                                            input_mode = InputMode::DECRPM(DECRPMReport::WaitingForDollar { mode: *mode, state });
                                        },
                                        (other, DECRPMReport::Semicolon { ref mode }) => {
                                            log::trace!("Received invalid DECRPM response: Was waiting for an ASCII digit reporting setting value (`Ps` argument), for mode {mode:?} but instead got character {other:?}");
                                            // Revert to normal input mode, to prevent locking
                                            // up the user's terminal input
                                            input_mode = InputMode::Normal;
                                        }
                                        ('$', DECRPMReport::WaitingForDollar { mode, state }) => {
                                            input_mode = InputMode::DECRPM(DECRPMReport::WaitingForEnd { mode: *mode, state: *state });
                                        },
                                        (other, DECRPMReport::WaitingForDollar { mode, state }) => {
                                            log::trace!("Received invalid DECRPM response: Was waiting for an ASCII `$` character (`Pm` argument was {mode:?} and `Ps` argument was {state:?}) but instead got character {other:?}");
                                            // Revert to normal input mode, to prevent locking
                                            // up the user's terminal input
                                            input_mode = InputMode::Normal;
                                        }
                                        (c, DECRPMReport::WaitingForEnd { mode, state }) => {
                                            if c != 'y' {
                                                log::trace!("Received invalid DECRPM response: Was waiting for an ASCII `y` character (`Pm` argument was {mode:?} and `Ps` argument was {state:?}) but instead got character {c:?}");
                                            } else {
                                                log::trace!("Got an DECRPM Terminal mode report: Mode {mode:?} is set to {state:?}");
                                                state_tx.send(TerminalEvent::DECModeReport { mode: *mode, state: *state }.into()).unwrap();
                                            }
                                            // end of report sequence.
                                            input_mode = InputMode::Normal;
                                        },

                                    }
                                }
                                other => {
                                    log::trace!("get_events other = {:?}", other);
                                    continue 'poll_while;
                                } // Mouse events or errors.
                            }
                        }
                        if let InputMode::EscapeSequence(ref mut buf) = input_mode {
                            esc_seq_buf = std::mem::take(buf);
                            input_mode = InputMode::Normal;
                            log::trace!("EscapeSequence is {esc_seq_buf:?} == {:?}", String::from_utf8_lossy(&esc_seq_buf));
                            log::trace!("EscapeSequence parsed {:?}", osc::QueryBackground::parse(&esc_seq_buf));
                        }

                    }
                },
                recv(rx) -> cmd => {
                    use nix::sys::time::TimeValLike as _;
                    let mut buf = [0;2];
                    let mut read_fd_set = nix::sys::select::FdSet::new();
                    read_fd_set.insert(new_command_fd.as_fd());
                    let mut error_fd_set = nix::sys::select::FdSet::new();
                    error_fd_set.insert(new_command_fd.as_fd());
                    let timeval:  nix::sys::time::TimeSpec = nix::sys::time::TimeSpec::seconds(2);
                    let pselect_result = nix::sys::select::pselect(None, Some(&mut read_fd_set), None, Some(&mut error_fd_set), Some(&timeval), None);
                    if pselect_result.is_err() || error_fd_set.highest().map(|bfd| bfd.as_raw_fd()) == Some(new_command_fd.as_raw_fd()) || read_fd_set.highest().map(|bfd| bfd.as_raw_fd()) != Some(new_command_fd.as_raw_fd()) {
                        continue 'poll_while;
                    };
                    let _ = nix::unistd::read(new_command_fd, buf.as_mut());
                    match cmd.unwrap_or_default() {
                        InputCommand::Kill => return,
                    }
                }
            };
        }
        drop(working);
    }

    struct EventsAndRaw<R> {
        source: R,
        buffer: VecDeque<u8>,
        read_buffer: Vec<u8>,
    }

    impl<R: std::io::Read> Iterator for EventsAndRaw<R> {
        type Item = Result<(Event, Vec<u8>), std::io::Error>;

        fn next(&mut self) -> Option<Result<(Event, Vec<u8>), std::io::Error>> {
            if let Some(c) = self.buffer.pop_front() {
                self.read_buffer.push(c);
                let res = self.parse_event(c);
                if res.is_err() {
                    self.read_buffer.clear();
                }
                return Some(res);
            }

            // Here we read two bytes at a time. We need to distinguish between single ESC key
            // presses, and escape sequences (which start with ESC or a x1B byte). The idea is that
            // if this is an escape sequence, we will read multiple bytes (the first byte being
            // ESC) but if this is a single ESC keypress, we will only read a single byte.
            let mut buf = [0u8; 2];
            let res = match self.source.read(&mut buf) {
                Ok(0) => return None,
                Ok(1) => match buf[0] {
                    b'\x1B' => Ok((Event::Key(Key::Esc), vec![b'\x1B'])),
                    c => {
                        self.read_buffer.push(c);
                        self.parse_event(c)
                    }
                },
                Ok(_) => {
                    self.read_buffer.push(buf[0]);
                    self.buffer.push_back(buf[1]);
                    self.parse_event(buf[0])
                }
                Err(err) => Err(err),
            };
            if res.is_err() {
                self.read_buffer.clear();
            }

            Some(res)
        }
    }

    impl<R: std::io::Read> EventsAndRaw<R> {
        #[inline]
        const fn new(source: R) -> Self {
            Self {
                source,
                buffer: VecDeque::new(),
                read_buffer: vec![],
            }
        }

        fn next_byte(&mut self) -> Result<u8, std::io::Error> {
            let mut buf = [0; 1];
            if let Some(c) = self.buffer.pop_front() {
                self.read_buffer.push(c);
                return Ok(c);
            }
            match self.source.read(&mut buf) {
                Ok(0) => Err(std::io::Error::other("Could not parse input: read 0 bytes")),
                Err(err) => Err(err),
                Ok(_) => {
                    self.read_buffer.push(buf[0]);
                    Ok(buf[0])
                }
            }
        }

        fn parse_event(&mut self, item: u8) -> Result<(Event, Vec<u8>), std::io::Error> {
            let result = {
                match item {
                    b'\x1B' => {
                        match self.next_byte()? {
                            b'[' => self.parse_csi().ok_or_else(|| {
                                std::io::Error::other(format!(
                                    "Could not parse event: {:?}",
                                    self.read_buffer
                                ))
                            })?,
                            b'O' => {
                                match self.next_byte()? {
                                    // F1-F4
                                    val @ b'P'..=b'S' => Event::Key(Key::F(1 + val - b'P')),
                                    _ => {
                                        return Err(std::io::Error::other(format!(
                                            "Could not parse event: {:?}",
                                            self.read_buffer
                                        )))
                                    }
                                }
                            }
                            c => {
                                let ch = self.parse_utf8_char(c)?;
                                Event::Key(Key::Alt(ch))
                            }
                        }
                    }
                    b'\n' | b'\r' => Event::Key(Key::Char('\n')),
                    b'\t' => Event::Key(Key::Char('\t')),
                    b'\x7F' => Event::Key(Key::Backspace),
                    c @ b'\x01'..=b'\x1A' => Event::Key(Key::Ctrl((c - 0x1 + b'a') as char)),
                    c @ b'\x1C'..=b'\x1F' => Event::Key(Key::Ctrl((c - 0x1C + b'4') as char)),
                    b'\0' => Event::Key(Key::Null),
                    c => {
                        let ch = self.parse_utf8_char(c)?;
                        Event::Key(Key::Char(ch))
                    }
                }
            };
            Ok((result, std::mem::take(&mut self.read_buffer)))
        }

        fn parse_csi(&mut self) -> Option<Event> {
            Some(match self.next_byte().ok()? {
                b'[' => match self.next_byte().ok()? {
                    val @ b'A'..=b'E' => Event::Key(Key::F(1 + val - b'A')),
                    _ => return None,
                },
                b'D' => Event::Key(Key::Left),
                b'C' => Event::Key(Key::Right),
                b'A' => Event::Key(Key::Up),
                b'B' => Event::Key(Key::Down),
                b'H' => Event::Key(Key::Home),
                b'F' => Event::Key(Key::End),
                b'Z' => Event::Key(Key::Backtab),
                b'M' => {
                    // X10 emulation mouse encoding: ESC [ CB Cx Cy (6 characters only).
                    let cb = self.next_byte().ok()? as i8 - 32;
                    // (1, 1) are the coords for upper left.
                    let cx = self.next_byte().ok()?.saturating_sub(32) as u16;
                    let cy = self.next_byte().ok()?.saturating_sub(32) as u16;
                    Event::Mouse(match cb & 0b11 {
                        0 => {
                            if cb & 0x40 != 0 {
                                MouseEvent::Press(MouseButton::WheelUp, cx, cy)
                            } else {
                                MouseEvent::Press(MouseButton::Left, cx, cy)
                            }
                        }
                        1 => {
                            if cb & 0x40 != 0 {
                                MouseEvent::Press(MouseButton::WheelDown, cx, cy)
                            } else {
                                MouseEvent::Press(MouseButton::Middle, cx, cy)
                            }
                        }
                        2 => MouseEvent::Press(MouseButton::Right, cx, cy),
                        3 => MouseEvent::Release(cx, cy),
                        _ => return None,
                    })
                }
                b'<' => {
                    // xterm mouse encoding:
                    // ESC [ < Cb ; Cx ; Cy (;) (M or m)
                    let mut buf = Vec::new();
                    let mut c = self.next_byte().ok()?;
                    while !matches!(c, b'm' | b'M') {
                        buf.push(c);
                        c = self.next_byte().ok()?;
                    }
                    let str_buf = String::from_utf8(buf).ok()?;
                    let nums = str_buf
                        .split(';')
                        .map(|n| n.parse())
                        .collect::<Result<Vec<u16>, _>>()
                        .ok()?;

                    let cb = *nums.first()?;
                    let cx = *nums.get(1)?;
                    let cy = *nums.get(2)?;

                    let event = match cb {
                        0..=2 | 64..=65 => {
                            let button = match cb {
                                0 => MouseButton::Left,
                                1 => MouseButton::Middle,
                                2 => MouseButton::Right,
                                64 => MouseButton::WheelUp,
                                65 => MouseButton::WheelDown,
                                _ => unreachable!(),
                            };
                            match c {
                                b'M' => MouseEvent::Press(button, cx, cy),
                                b'm' => MouseEvent::Release(cx, cy),
                                _ => return None,
                            }
                        }
                        32 => MouseEvent::Hold(cx, cy),
                        3 => MouseEvent::Release(cx, cy),
                        _ => return None,
                    };

                    Event::Mouse(event)
                }
                b'I' => Event::FocusIn,
                b'O' => Event::FocusOut,
                b'P' => Event::Key(Key::F(1)),
                b'Q' => Event::Key(Key::F(2)),
                b'S' => Event::Key(Key::F(4)),
                c @ b'0'..=b'9' => {
                    let mut buf = Vec::new();
                    buf.push(c);
                    let mut c = self.next_byte().ok()?;
                    while !(64..=126).contains(&c) {
                        buf.push(c);
                        c = self.next_byte().ok()?;
                    }

                    match c {
                        // rxvt mouse encoding:
                        // ESC [ Cb ; Cx ; Cy ; M
                        b'M' => {
                            let str_buf = String::from_utf8(buf).ok()?;

                            let nums: Vec<u16> = str_buf
                                .split(';')
                                .map(|n| n.parse())
                                .collect::<Result<Vec<u16>, _>>()
                                .ok()?;

                            let cb = nums[0];
                            let cx = nums[1];
                            let cy = nums[2];

                            let event = match cb {
                                32 => MouseEvent::Press(MouseButton::Left, cx, cy),
                                33 => MouseEvent::Press(MouseButton::Middle, cx, cy),
                                34 => MouseEvent::Press(MouseButton::Right, cx, cy),
                                35 => MouseEvent::Release(cx, cy),
                                64 => MouseEvent::Hold(cx, cy),
                                96 | 97 => MouseEvent::Press(MouseButton::WheelUp, cx, cy),
                                _ => return None,
                            };

                            Event::Mouse(event)
                        }
                        // Special key code.
                        b'~' => {
                            let str_buf = String::from_utf8(buf).ok()?;

                            // This CSI sequence can be a list of semicolon-separated
                            // numbers.
                            let nums: Vec<u8> = str_buf
                                .split(';')
                                .map(|n| n.parse())
                                .collect::<Result<Vec<u8>, _>>()
                                .ok()?;

                            if nums.is_empty() {
                                return None;
                            }

                            if nums.len() == 1 {
                                match nums[0] {
                                    // https://vt100.net/docs/vt510-rm/chapter8.html#S8.4.7
                                    1 | 7 => Event::Key(Key::Home),
                                    2 => Event::Key(Key::Insert),
                                    3 => Event::Key(Key::Delete),
                                    4 | 8 => Event::Key(Key::End),
                                    5 => Event::Key(Key::PageUp),
                                    6 => Event::Key(Key::PageDown),
                                    v @ 11..=15 => Event::Key(Key::F(v - 10)),
                                    v @ 17..=21 => Event::Key(Key::F(v - 11)),
                                    v @ 23..=24 => Event::Key(Key::F(v - 12)),
                                    200 => Event::BracketPasteStart,
                                    201 => Event::BracketPasteEnd,
                                    _ => Event::Unsupported,
                                }
                            } else if nums.len() == 2 {
                                // key with a modifier https://vt100.net/docs/vt510-rm/DECFNK.html
                                Event::Unsupported
                            } else {
                                Event::Unsupported
                            }
                        }
                        b'u' => {
                            // CSI ... u
                            // - https://www.leonerd.org.uk/hacks/fixterms/
                            // - or kitty
                            return None;
                        }
                        // CSI Cy ; Cx R
                        // Cy - cursor row number (starting from 1)
                        // Cx - cursor column number (starting from 1)
                        b'R' => {
                            let str_buf = String::from_utf8(buf).ok()?;

                            let nums: Vec<u16> = str_buf
                                .split(';')
                                .map(|n| n.parse())
                                .collect::<Result<Vec<u16>, _>>()
                                .ok()?;

                            if nums.len() != 2 {
                                return None;
                            }
                            let row = NonZeroU16::new(nums[0])?;
                            let column = NonZeroU16::new(nums[1])?;
                            Event::CursorPosition { column, row }
                        }
                        // this is probably a key with a modifier (e.g. shift + arrow key?)
                        _ => return None,
                    }
                }
                b'?' => Event::ReportModeStart,
                _ => return None,
            })
        }

        fn parse_utf8_char(&mut self, c: u8) -> Result<char, std::io::Error> {
            if c.is_ascii() {
                Ok(c as char)
            } else {
                let bytes = &mut Vec::new();
                bytes.push(c);

                loop {
                    let next = self.next_byte()?;
                    bytes.push(next);
                    if let Ok(st) = str::from_utf8(bytes) {
                        return Ok(st.chars().next().unwrap());
                    }
                    if bytes.len() >= 4 {
                        return Err(std::io::Error::other(format!(
                            "Input character is not valid UTF-8: {bytes:?}"
                        )));
                    }
                }
            }
        }
    }

    #[test]
    fn events_and_raw() {
        use std::io::Cursor;
        fn char_to_bytes(c: char) -> Vec<u8> {
            let mut b = [0; 4];
            c.encode_utf8(&mut b).as_bytes().to_vec()
        }

        let mut scenarios = vec![
            (vec![], vec![]),
            (vec![b'a'], vec![(Event::Key(Key::Char('a')), vec![97])]),
            (
                char_to_bytes('ß'),
                vec![(Event::Key(Key::Char('ß')), vec![195, 159])],
            ),
            (
                char_to_bytes('β'),
                vec![(Event::Key(Key::Char('β')), vec![206, 178])],
            ),
            (vec![b'\0'], vec![(Event::Key(Key::Null), vec![0])]),
            (vec![b'\x1B'], vec![(Event::Key(Key::Esc), vec![27])]),
            (vec![b'\x7F'], vec![(Event::Key(Key::Backspace), vec![127])]),
            (
                vec![b'\x1B', b'[', b'<', b'2', b';', b'5', b';', b'6', b'M'],
                vec![(
                    Event::Mouse(MouseEvent::Press(MouseButton::Right, 5, 6)),
                    vec![27, 91, 60, 50, 59, 53, 59, 54, 77],
                )],
            ),
            (
                vec![b'\x1B', b'[', b'?', b'2', b'0', b'0', b'2', b'$', b'y'],
                vec![
                    (Event::ReportModeStart, vec![27, 91, 63]),
                    (Event::Key(Key::Char('2')), vec![50]),
                    (Event::Key(Key::Char('0')), vec![48]),
                    (Event::Key(Key::Char('0')), vec![48]),
                    (Event::Key(Key::Char('2')), vec![50]),
                    (Event::Key(Key::Char('$')), vec![36]),
                    (Event::Key(Key::Char('y')), vec![121]),
                ],
            ),
        ];

        for (i, val) in (b'P'..=b'S').into_iter().enumerate() {
            scenarios.push((
                vec![b'\x1B', b'O', val],
                vec![(Event::Key(Key::F(1 + i as u8)), vec![b'\x1B', b'O', val])],
            ));
        }
        for (val, c) in (b'\x01'..=b'\x1A').into_iter().zip('a'..) {
            scenarios.push((
                vec![val],
                if val == b'\t' {
                    vec![(Event::Key(Key::Char('\t')), vec![val])]
                } else if val == b'\n' || val == b'\r' {
                    vec![(Event::Key(Key::Char('\n')), vec![val])]
                } else {
                    vec![(Event::Key(Key::Ctrl(c)), vec![val])]
                },
            ));
        }
        for (val, c) in (b'\x1C'..=b'\x1F').into_iter().zip('4'..) {
            scenarios.push((vec![val], vec![(Event::Key(Key::Ctrl(c)), vec![val])]));
        }
        for (input, expected) in scenarios {
            let cursor = Cursor::new(input);
            let mut iter = EventsAndRaw::new(cursor);
            assert_eq!(
                iter.by_ref()
                    .collect::<Result<Vec<(Event, Vec<u8>)>, std::io::Error>>()
                    .unwrap(),
                expected
            );
            assert!(iter.read_buffer.is_empty());
            assert!(iter.buffer.is_empty());
        }
    }
}
