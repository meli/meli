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

use std::os::fd::{AsFd, AsRawFd, OwnedFd};

use crossbeam::{channel::Receiver, select};
use melib::log;
use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
use serde::{Serialize, Serializer};
use termion::{
    event::{
        Event as TEvent, Key as TKey, MouseButton as TermionMouseButton,
        MouseEvent as TermionMouseEvent,
    },
    input::TermReadEventsAndRaw,
};

use super::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Key {
    /// Backspace.
    Backspace,
    /// Left arrow.
    Left,
    /// Right arrow.
    Right,
    /// Up arrow.
    Up,
    /// Down arrow.
    Down,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page Up key.
    PageUp,
    /// Page Down key.
    PageDown,
    /// Delete key.
    Delete,
    /// Insert key.
    Insert,
    /// Function keys.
    ///
    /// Only function keys 1 through 12 are supported.
    F(u8),
    /// Normal character.
    Char(char),
    /// Alt modified character.
    Alt(char),
    /// Ctrl modified character.
    ///
    /// Note that certain keys may not be modifiable with `ctrl`, due to
    /// limitations of terminals.
    Ctrl(char),
    /// Null byte.
    Null,
    /// Esc key.
    Esc,
    Mouse(MouseEvent),
    Paste(String),
}

/// A mouse related event.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(untagged)]
pub enum MouseEvent {
    /// A mouse button was pressed.
    ///
    /// The coordinates are one-based.
    Press(MouseButton, u16, u16),
    /// A mouse button was released.
    ///
    /// The coordinates are one-based.
    Release(u16, u16),
    /// A mouse button is held over the given coordinates.
    ///
    /// The coordinates are one-based.
    Hold(u16, u16),
}

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

/// A mouse button.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(untagged)]
pub enum MouseButton {
    /// The left mouse button.
    Left,
    /// The right mouse button.
    Right,
    /// The middle mouse button.
    Middle,
    /// Mouse wheel is going up.
    ///
    /// This event is typically only used with [`MouseEvent::Press`].
    WheelUp,
    /// Mouse wheel is going down.
    ///
    /// This event is typically only used with [`MouseEvent::Press`].
    WheelDown,
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

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::Key::*;
        match self {
            F(n) => write!(f, "F{n}"),
            Char(' ') => write!(f, "Space"),
            Char('\t') => write!(f, "Tab"),
            Char('\n') => write!(f, "Enter"),
            Char(c) => write!(f, "{c}"),
            Alt(c) => write!(f, "M-{c}"),
            Ctrl(c) => write!(f, "C-{c}"),
            Paste(_) => write!(f, "Pasted buf"),
            Null => write!(f, "Null byte"),
            Esc => write!(f, "Esc"),
            Backspace => write!(f, "Backspace"),
            Left => write!(f, "Left"),
            Right => write!(f, "Right"),
            Up => write!(f, "Up"),
            Down => write!(f, "Down"),
            Home => write!(f, "Home"),
            End => write!(f, "End"),
            PageUp => write!(f, "PageUp"),
            PageDown => write!(f, "PageDown"),
            Delete => write!(f, "Delete"),
            Insert => write!(f, "Insert"),
            Mouse(_) => write!(f, "Mouse"),
        }
    }
}

impl<'a> From<&'a String> for Key {
    fn from(v: &'a String) -> Self {
        Self::Paste(v.to_string())
    }
}

impl From<TKey> for Key {
    fn from(k: TKey) -> Self {
        match k {
            TKey::Backspace => Self::Backspace,
            TKey::Left => Self::Left,
            TKey::Right => Self::Right,
            TKey::Up => Self::Up,
            TKey::Down => Self::Down,
            TKey::Home => Self::Home,
            TKey::End => Self::End,
            TKey::PageUp => Self::PageUp,
            TKey::PageDown => Self::PageDown,
            TKey::Delete => Self::Delete,
            TKey::Insert => Self::Insert,
            TKey::F(u) => Self::F(u),
            TKey::Char(c) => Self::Char(c),
            TKey::Alt(c) => Self::Alt(c),
            TKey::Ctrl(c) => Self::Ctrl(c),
            TKey::Null => Self::Null,
            TKey::Esc => Self::Esc,
            _ => Self::Char(' '),
        }
    }
}

impl PartialEq<Key> for &Key {
    fn eq(&self, other: &Key) -> bool {
        **self == *other
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
    let stdin_fd = PollFd::new(stdin2.as_fd(), PollFlags::POLLIN);
    let new_command_pollfd = PollFd::new(new_command_fd.as_fd(), PollFlags::POLLIN);
    let mut input_mode = InputMode::Normal;
    let mut esc_seq_buf = vec![];
    let mut palette = (None, None);
    let mut paste_buf = String::with_capacity(256);
    let mut stdin_iter = stdin.events_and_raw();
    'poll_while: while let Ok(_n_raw) = poll(&mut [new_command_pollfd, stdin_fd], PollTimeout::NONE)
    {
        select! {
            default => {
                if stdin_fd.revents().is_some() {
                    'stdin_while: for c in stdin_iter.by_ref() {
                        match (c, &mut input_mode) {
                            (Ok((TEvent::Key(TKey::Alt(']')), _)), InputMode::Normal)=> {
                                esc_seq_buf.clear();
                                esc_seq_buf.extend_from_slice(b"\x1b]");
                                input_mode = InputMode::EscapeSequence(std::mem::take(&mut esc_seq_buf));

                                continue 'stdin_while;
                            }
                            (Ok((TEvent::Key(TKey::Alt('\\')), _)), InputMode::EscapeSequence(ref mut buf)) => {
                                esc_seq_buf = std::mem::take(buf);
                                input_mode = InputMode::Normal;
                                log::trace!("EscapeSequence is {:?} == {:?}", &esc_seq_buf, String::from_utf8_lossy(&esc_seq_buf));
                                if let Some(bg) = QueryBackground::parse(&String::from_utf8_lossy(&esc_seq_buf)) {
                                    log::trace!("EscapeSequence parsed bg {:?}", bg);
                                    palette.1 = Some(bg);
                                } else if let Some(fg) = QueryForeground::parse(&String::from_utf8_lossy(&esc_seq_buf)) {
                                    log::trace!("EscapeSequence parsed fg {:?}", fg);
                                    palette.0 = Some(fg);
                                } else {
                                    log::trace!("EscapeSequence unknown");
                                }
                                if let (Some(fg), Some(bg)) = palette {
                                    log::trace!("compute_scheme_contrast(fg {:?}, bg {:?}) = {:?}", fg, bg, Color::compute_scheme_contrast(fg, bg));
                                    palette.0.take();
                                    palette.1.take();
                                }
                                continue 'stdin_while;
                            }
                            (Ok((TEvent::Key(_), ref bytes)), InputMode::EscapeSequence(ref mut buf)) => {
                                buf.extend(bytes);
                                continue 'stdin_while;
                            }
                            (Ok((TEvent::Key(k), bytes)), InputMode::Normal) => {
                                closure((Key::from(k), bytes));
                                continue 'poll_while;
                            }
                            (
                                Ok((TEvent::Key(TKey::Char(k)), ref mut bytes)), InputMode::Paste(ref mut buf),
                            ) => {
                                paste_buf.push(k);
                                let bytes = std::mem::take(bytes);
                                buf.extend(bytes.into_iter());
                                continue 'stdin_while;
                            }
                            (Ok((TEvent::Unsupported(ref k), _)), _) if k.as_slice() == BRACKET_PASTE_START => {
                                input_mode = InputMode::Paste(Vec::new());
                                continue 'stdin_while;
                            }
                            (Ok((TEvent::Unsupported(ref k), _)), InputMode::Paste(ref mut buf))
                                if k.as_slice() == BRACKET_PASTE_END =>
                                {
                                    let buf = std::mem::take(buf);
                                    input_mode = InputMode::Normal;
                                    let ret = Key::from(&paste_buf);
                                    paste_buf.clear();
                                    closure((ret, buf));
                                    continue 'poll_while;
                                }
                            (Ok((TEvent::Mouse(mev), bytes)), InputMode::Normal) => {
                                closure((Key::Mouse(mev.into()), bytes));
                                continue 'poll_while;
                                }
                            (Ok((TEvent::Unsupported(ref k,), _)), InputMode::Normal) if k.as_slice() == [27, 91, 63] => {
                                // DECRPM - Report Mode - Terminal To Host
                                esc_seq_buf.clear();
                                input_mode = InputMode::DECRPM(DECRPMReport::WaitingForSemicolon { mode: 0});
                            }
                            (Ok((TEvent::Key(TKey::Char(k)), _)), InputMode::DECRPM(ref report_state)) => {
                                // CSI ? Pd ; Ps $ y
                                match (k, report_state) {
                                    (d, DECRPMReport::WaitingForSemicolon { mode }) if d.is_ascii_digit() => {
                                        let mut mode = *mode;
                                        mode *= 10;
                                        // SAFETY: we performed an char::is_ascii_digit() check in
                                        // the guard above.
                                        mode += (d as u8 - b'0') as u16;
                                        input_mode = InputMode::DECRPM(DECRPMReport::WaitingForSemicolon { mode });
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
                                                log::trace!("Received invalid DECRPM setting value: {:?}: expected one of {{0, 1, 2, 3, 4}}", other);
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
                        log::trace!("EscapeSequence is {:?} == {:?}", &esc_seq_buf, String::from_utf8_lossy(&esc_seq_buf));
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
                let _ = nix::unistd::read(new_command_fd.as_raw_fd(), buf.as_mut());
                match cmd.unwrap_or_default() {
                    InputCommand::Kill => return,
                }
            }
        };
    }
    drop(working);
}

impl<'de> Deserialize<'de> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct KeyVisitor;

        impl Visitor<'_> for KeyVisitor {
            type Value = Key;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter
                    .write_str("a valid key value. Please consult the manual for valid key inputs.")
            }

            fn visit_str<E>(self, value: &str) -> Result<Key, E>
            where
                E: de::Error,
            {
                match value {
                    "Backspace" | "backspace" => Ok(Key::Backspace),
                    "Left" | "left" => Ok(Key::Left),
                    "Right" | "right" => Ok(Key::Right),
                    "Up" | "up" => Ok(Key::Up),
                    "Down" | "down" => Ok(Key::Down),
                    "Home" | "home" => Ok(Key::Home),
                    "End" | "end" => Ok(Key::End),
                    "PageUp" | "pageup" => Ok(Key::PageUp),
                    "PageDown" | "pagedown" => Ok(Key::PageDown),
                    "Delete" | "delete" => Ok(Key::Delete),
                    "Insert" | "insert" => Ok(Key::Insert),
                    "Enter" | "enter" => Ok(Key::Char('\n')),
                    "Tab" | "tab" => Ok(Key::Char('\t')),
                    "Esc" | "esc" => Ok(Key::Esc),
                    s if s.len() == 1 => Ok(Key::Char(s.chars().next().unwrap())),
                    s if s.starts_with('F') && (s.len() == 2 || s.len() == 3) => {
                        use std::str::FromStr;

                        if let Ok(n) = u8::from_str(&s[1..]) {
                            if (1..=12).contains(&n) {
                                return Ok(Key::F(n));
                            }
                        }
                        Err(de::Error::custom(format!(
                            "`{}` should be a number 1 <= n <= 12 instead.",
                            &s[1..]
                        )))
                    }
                    s if s.starts_with("M-") && s.len() == 3 => {
                        let c = s.as_bytes()[2] as char;

                        if c.is_lowercase() || c.is_numeric() {
                            return Ok(Key::Alt(c));
                        }

                        Err(de::Error::custom(format!(
                            "`{}` should be a lowercase and alphanumeric character instead.",
                            &s[2..]
                        )))
                    }
                    s if s.starts_with("C-") && s.len() == 3 => {
                        let c = s.as_bytes()[2] as char;

                        if c.is_lowercase() || c.is_numeric() {
                            return Ok(Key::Ctrl(c));
                        }
                        Err(de::Error::custom(format!(
                            "`{}` should be a lowercase and alphanumeric character instead.",
                            &s[2..]
                        )))
                    }
                    _ => Err(de::Error::custom(format!(
                        "Cannot derive shortcut from `{value}`. Please consult the manual for \
                         valid key inputs."
                    ))),
                }
            }
        }

        deserializer.deserialize_identifier(KeyVisitor)
    }
}

impl Serialize for Key {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Backspace => serializer.serialize_str("Backspace"),
            Self::Left => serializer.serialize_str("Left"),
            Self::Right => serializer.serialize_str("Right"),
            Self::Up => serializer.serialize_str("Up"),
            Self::Down => serializer.serialize_str("Down"),
            Self::Home => serializer.serialize_str("Home"),
            Self::End => serializer.serialize_str("End"),
            Self::PageUp => serializer.serialize_str("PageUp"),
            Self::PageDown => serializer.serialize_str("PageDown"),
            Self::Delete => serializer.serialize_str("Delete"),
            Self::Insert => serializer.serialize_str("Insert"),
            Self::Esc => serializer.serialize_str("Esc"),
            Self::Char('\n') => serializer.serialize_str("Enter"),
            Self::Char('\t') => serializer.serialize_str("Tab"),
            Self::Char(c) => serializer.serialize_char(*c),
            Self::F(n) => serializer.serialize_str(&format!("F{n}")),
            Self::Alt(c) => serializer.serialize_str(&format!("M-{c}")),
            Self::Ctrl(c) => serializer.serialize_str(&format!("C-{c}")),
            Self::Null => serializer.serialize_str("Null"),
            Self::Mouse(mev) => mev.serialize(serializer),
            Self::Paste(s) => serializer.serialize_str(s),
        }
    }
}

#[test]
fn test_key_serde() {
    #[derive(Debug, Deserialize, Eq, PartialEq)]
    struct V {
        k: Key,
    }

    macro_rules! test_key {
        ($s:literal, ok $v:expr) => {
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\"")),
                Ok(V { k: $v })
            );
        };
        ($s:literal, err $v:literal) => {
            test_key!($s, err $v, "^")
        };
        ($s:literal, err $v:literal, $extra:literal) => {
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\""))
                    .unwrap_err()
                    .to_string(),
                std::concat!(
                    "TOML parse error at line 1, column 5\n  |\n1 | k = \"",
                    $s,
                    "\"\n  |     ",
                    $extra,
                    "^^^^\n",
                    $v,
                    '\n',
                )
                .to_string()
            );
        };
    }
    test_key!("Backspace", ok Key::Backspace);
    test_key!("Left", ok  Key::Left );
    test_key!("Right", ok  Key::Right);
    test_key!("Up", ok  Key::Up );
    test_key!("Down", ok  Key::Down );
    test_key!("Home", ok  Key::Home );
    test_key!("End", ok  Key::End );
    test_key!("PageUp", ok  Key::PageUp );
    test_key!("PageDown", ok  Key::PageDown );
    test_key!("Delete", ok  Key::Delete );
    test_key!("Insert", ok  Key::Insert );
    test_key!("Enter", ok  Key::Char('\n') );
    test_key!("Tab", ok  Key::Char('\t') );
    test_key!("k", ok  Key::Char('k') );
    test_key!("1", ok  Key::Char('1') );
    test_key!("Esc", ok  Key::Esc );
    test_key!("C-a", ok  Key::Ctrl('a') );
    test_key!("C-1", ok  Key::Ctrl('1') );
    test_key!("M-a", ok  Key::Alt('a') );
    test_key!("F1", ok  Key::F(1) );
    test_key!("F12", ok  Key::F(12) );
    test_key!("C-V", err "`V` should be a lowercase and alphanumeric character instead.");
    test_key!("M-V", err "`V` should be a lowercase and alphanumeric character instead.");
    test_key!("F13", err "`13` should be a number 1 <= n <= 12 instead.");
    test_key!("Fc", err "`c` should be a number 1 <= n <= 12 instead.", "");
    test_key!("adsfsf", err "Cannot derive shortcut from `adsfsf`. Please consult the manual for valid key inputs.", "^^^^");
}
