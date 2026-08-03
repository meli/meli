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

use std::os::fd::{AsFd, AsRawFd, OwnedFd};

use crossbeam::{channel::Receiver, select};
use melib::log;
use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
use termion::{
    event::{
        Event as TermionEvent, Key as TermionKey, MouseButton as TermionMouseButton,
        MouseEvent as TermionMouseEvent,
    },
    input::TermReadEventsAndRaw,
};

use crate::terminal::{
    color::Color,
    keys::{Key, MouseButton, MouseEvent},
    EscapeSequenceQuery, QueryBackground, QueryForeground,
};

/*
 * CSI events we use
 */

pub const BRACKET_PASTE_START: &[u8] = b"\x1B[200~";
pub const BRACKET_PASTE_END: &[u8] = b"\x1B[201~";

impl From<TermionMouseEvent> for MouseEvent {
    fn from(val: TermionMouseEvent) -> Self {
        use TermionMouseEvent::*;
        match val {
            Press(btn, a, b) => Self::Press(btn.into(), a, b),
            Release(a, b) => Self::Release(a, b),
            Hold(a, b) => Self::Hold(a, b),
        }
    }
}

impl From<TermionMouseButton> for MouseButton {
    fn from(val: TermionMouseButton) -> Self {
        use TermionMouseButton::*;
        match val {
            Left => Self::Left,
            Right => Self::Right,
            Middle => Self::Middle,
            WheelUp => Self::WheelUp,
            WheelDown => Self::WheelDown,
        }
    }
}

impl From<TermionKey> for Key {
    fn from(k: TermionKey) -> Self {
        match k {
            TermionKey::Backspace => Self::Backspace,
            TermionKey::Left => Self::Left,
            TermionKey::Right => Self::Right,
            TermionKey::Up => Self::Up,
            TermionKey::Down => Self::Down,
            TermionKey::Home => Self::Home,
            TermionKey::End => Self::End,
            TermionKey::PageUp => Self::PageUp,
            TermionKey::PageDown => Self::PageDown,
            TermionKey::Delete => Self::Delete,
            TermionKey::Insert => Self::Insert,
            TermionKey::F(u) => Self::F(u),
            TermionKey::Char(c) => Self::Char(c),
            TermionKey::Alt(c) => Self::Alt(c),
            TermionKey::Ctrl(c) => Self::Ctrl(c),
            TermionKey::Null => Self::Null,
            TermionKey::Esc => Self::Esc,
            _ => Self::Char(' '),
        }
    }
}

/// Setting mode value in ANSI or DEC report sequences.
///
/// See <https://vt100.net/docs/vt510-rm/DECRPM.html>.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[repr(u8)]
enum ANSIDECModeSetting {
    #[default]
    ModeNotRecognized = 0,
    Set = 1,
    Reset = 2,
    PermanentlySet = 3,
    PermanentlyReset = 4,
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
        setting: ANSIDECModeSetting,
    },
    WaitingForEnd {
        mode: u16,
        setting: ANSIDECModeSetting,
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

#[derive(Debug, Default)]
/// Main process sends commands to the input thread.
pub enum InputCommand {
    #[default]
    /// Exit thread
    Kill,
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
    mut closure: impl FnMut((Key, Vec<u8>)),
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
    let mut stdin_iter = stdin.events_and_raw();
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
                            (Ok((TermionEvent::Key(TermionKey::Alt(']')), _)), InputMode::Normal)=> {
                                esc_seq_buf.clear();
                                esc_seq_buf.extend_from_slice(b"\x1b]");
                                input_mode = InputMode::EscapeSequence(std::mem::take(&mut esc_seq_buf));

                                continue 'stdin_while;
                            }
                            (Ok((TermionEvent::Key(TermionKey::Alt('\\')), _)), InputMode::EscapeSequence(ref mut buf)) => {
                                esc_seq_buf = std::mem::take(buf);
                                input_mode = InputMode::Normal;
                                log::trace!("EscapeSequence is {esc_seq_buf:?} == {:?}", String::from_utf8_lossy(&esc_seq_buf));
                                if let Some(bg) = QueryBackground::parse(&String::from_utf8_lossy(&esc_seq_buf)) {
                                    log::trace!("EscapeSequence parsed bg {bg:?}");
                                    palette.1 = Some(bg);
                                } else if let Some(fg) = QueryForeground::parse(&String::from_utf8_lossy(&esc_seq_buf)) {
                                    log::trace!("EscapeSequence parsed fg {fg:?}");
                                    palette.0 = Some(fg);
                                } else {
                                    log::trace!("EscapeSequence unknown");
                                }
                                if let (Some(fg), Some(bg)) = palette {
                                    log::trace!("compute_scheme_contrast(fg {fg:?}, bg {bg:?}) = {:?}", Color::compute_scheme_contrast(fg, bg));
                                    palette.0.take();
                                    palette.1.take();
                                }
                                continue 'stdin_while;
                            }
                            (Ok((TermionEvent::Key(_), ref bytes)), InputMode::EscapeSequence(ref mut buf)) => {
                                buf.extend(bytes);
                                continue 'stdin_while;
                            }
                            (Ok((TermionEvent::Key(k), bytes)), InputMode::Normal) => {
                                closure((Key::from(k), bytes));
                                continue 'poll_while;
                            }
                            (
                                Ok((TermionEvent::Key(TermionKey::Char(k)), ref mut bytes)), InputMode::Paste(ref mut buf),
                            ) => {
                                paste_buf.push(k);
                                let bytes = std::mem::take(bytes);
                                buf.extend(bytes.into_iter());
                                continue 'stdin_while;
                            }
                            (Ok((TermionEvent::Unsupported(ref k), _)), _) if k.as_slice() == BRACKET_PASTE_START => {
                                input_mode = InputMode::Paste(Vec::new());
                                continue 'stdin_while;
                            }
                            (Ok((TermionEvent::Unsupported(ref k), _)), InputMode::Paste(ref mut buf))
                                if k.as_slice() == BRACKET_PASTE_END =>
                                {
                                    let buf = std::mem::take(buf);
                                    input_mode = InputMode::Normal;
                                    let ret = Key::from(&paste_buf);
                                    paste_buf.clear();
                                    closure((ret, buf));
                                    continue 'poll_while;
                                }
                            (Ok((TermionEvent::Mouse(mev), bytes)), InputMode::Normal) => {
                                closure((Key::Mouse(mev.into()), bytes));
                                continue 'poll_while;
                                }
                            (Ok((TermionEvent::Unsupported(ref k,), _)), InputMode::Normal) if k.as_slice() == [27, 91, 63] => {
                                // DECRPM - Report Mode - Terminal To Host
                                esc_seq_buf.clear();
                                input_mode = InputMode::DECRPM(DECRPMReport::WaitingForSemicolon { mode: 0});
                            }
                            (Ok((TermionEvent::Key(TermionKey::Char(k)), _)), InputMode::DECRPM(ref report_state)) => {
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
                                        let setting = match d {
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
                                        input_mode = InputMode::DECRPM(DECRPMReport::WaitingForDollar { mode: *mode, setting });
                                    },
                                    (other, DECRPMReport::Semicolon { ref mode }) => {
                                        log::trace!("Received invalid DECRPM response: Was waiting for an ASCII digit reporting setting value (`Ps` argument), for mode {mode:?} but instead got character {other:?}");
                                        // Revert to normal input mode, to prevent locking
                                        // up the user's terminal input
                                        input_mode = InputMode::Normal;
                                    }
                                    ('$', DECRPMReport::WaitingForDollar { mode, setting }) => {
                                        input_mode = InputMode::DECRPM(DECRPMReport::WaitingForEnd { mode: *mode, setting: *setting });
                                    },
                                    (other, DECRPMReport::WaitingForDollar { mode, setting }) => {
                                        log::trace!("Received invalid DECRPM response: Was waiting for an ASCII `$` character (`Pm` argument was {mode:?} and `Ps` argument was {setting:?}) but instead got character {other:?}");
                                        // Revert to normal input mode, to prevent locking
                                        // up the user's terminal input
                                        input_mode = InputMode::Normal;
                                    }
                                    (c, DECRPMReport::WaitingForEnd { mode, setting }) => {
                                        if c != 'y' {
                                            log::trace!("Received invalid DECRPM response: Was waiting for an ASCII `y` character (`Pm` argument was {mode:?} and `Ps` argument was {setting:?}) but instead got character {c:?}");
                                        } else {
                                            log::trace!("Got an DECRPM Terminal mode report: Mode {mode:?} is set to {setting:?}");
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
                        log::trace!("EscapeSequence parsed {:?}", QueryBackground::parse(&String::from_utf8_lossy(&esc_seq_buf)));
                    }

                }
            },
            recv(rx) -> cmd => {
                use nix::sys::time::TimeValLike;
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
