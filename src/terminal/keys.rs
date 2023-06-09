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

use std::os::unix::io::{AsRawFd, RawFd};

use crossbeam::{channel::Receiver, select};
use nix::poll::{poll, PollFd, PollFlags};
use serde::{Serialize, Serializer};
use termion::{
    event::{
        Event as TermionEvent, Key as TermionKey, MouseButton as TermionMouseButton,
        MouseEvent as TermionMouseEvent,
    },
    input::TermReadEventsAndRaw,
};

use super::*;

#[derive(Debug, PartialEq, Eq, Clone)]
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    /// This event is typically only used with Mouse::Press.
    WheelUp,
    /// Mouse wheel is going down.
    ///
    /// This event is typically only used with Mouse::Press.
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

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Key::*;
        match self {
            F(n) => write!(f, "F{}", n),
            Char(' ') => write!(f, "Space"),
            Char('\t') => write!(f, "Tab"),
            Char('\n') => write!(f, "Enter"),
            Char(c) => write!(f, "{}", c),
            Alt(c) => write!(f, "M-{}", c),
            Ctrl(c) => write!(f, "C-{}", c),
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
        Key::Paste(v.to_string())
    }
}

impl From<TermionKey> for Key {
    fn from(k: TermionKey) -> Self {
        match k {
            TermionKey::Backspace => Key::Backspace,
            TermionKey::Left => Key::Left,
            TermionKey::Right => Key::Right,
            TermionKey::Up => Key::Up,
            TermionKey::Down => Key::Down,
            TermionKey::Home => Key::Home,
            TermionKey::End => Key::End,
            TermionKey::PageUp => Key::PageUp,
            TermionKey::PageDown => Key::PageDown,
            TermionKey::Delete => Key::Delete,
            TermionKey::Insert => Key::Insert,
            TermionKey::F(u) => Key::F(u),
            TermionKey::Char(c) => Key::Char(c),
            TermionKey::Alt(c) => Key::Alt(c),
            TermionKey::Ctrl(c) => Key::Ctrl(c),
            TermionKey::Null => Key::Null,
            TermionKey::Esc => Key::Esc,
            _ => Key::Char(' '),
        }
    }
}

impl PartialEq<Key> for &Key {
    fn eq(&self, other: &Key) -> bool {
        **self == *other
    }
}

#[derive(PartialEq, Eq)]
/// Keep track of whether we're accepting normal user input or a pasted string.
enum InputMode {
    Normal,
    Paste(Vec<u8>),
}

#[derive(Debug)]
/// Main process sends commands to the input thread.
pub enum InputCommand {
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
/// The main loop uses [`crate::state::State::try_wait_on_child`] to check if
/// child has exited.
pub fn get_events(
    mut closure: impl FnMut((Key, Vec<u8>)),
    rx: &Receiver<InputCommand>,
    new_command_fd: RawFd,
    working: std::sync::Arc<()>,
) {
    let stdin = std::io::stdin();
    let stdin_fd = PollFd::new(std::io::stdin().as_raw_fd(), PollFlags::POLLIN);
    let new_command_pollfd = nix::poll::PollFd::new(new_command_fd, nix::poll::PollFlags::POLLIN);
    let mut input_mode = InputMode::Normal;
    let mut paste_buf = String::with_capacity(256);
    let mut stdin_iter = stdin.events_and_raw();
    'poll_while: while let Ok(_n_raw) = poll(&mut [new_command_pollfd, stdin_fd], -1) {
        //debug!(_n_raw);
        select! {
            default => {
                if stdin_fd.revents().is_some() {
                    'stdin_while: for c in stdin_iter.by_ref() {
                        match (c, &mut input_mode) {
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
                            _ => {
                                continue 'poll_while;
                            } // Mouse events or errors.
                        }
                    }
                }
            },
            recv(rx) -> cmd => {
                use nix::sys::time::TimeValLike;
                let mut buf = [0;2];
                //debug!("get_events_raw will nix::unistd::read");
                let mut read_fd_set = nix::sys::select::FdSet::new();
                read_fd_set.insert(new_command_fd);
                let mut error_fd_set = nix::sys::select::FdSet::new();
                error_fd_set.insert(new_command_fd);
                let timeval:  nix::sys::time::TimeSpec = nix::sys::time::TimeSpec::seconds(2);
                if nix::sys::select::pselect(None, Some(&mut read_fd_set), None, Some(&mut error_fd_set), Some(&timeval), None).is_err() || error_fd_set.highest() == Some(new_command_fd) || read_fd_set.highest() != Some(new_command_fd) {
                    continue 'poll_while;
                };
                let _ = nix::unistd::read(new_command_fd, buf.as_mut());
                match cmd.unwrap() {
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

        impl<'de> Visitor<'de> for KeyVisitor {
            type Value = Key;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
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
                        "Cannot derive shortcut from `{}`. Please consult the manual for valid \
                         key inputs.",
                        value
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
            Key::Backspace => serializer.serialize_str("Backspace"),
            Key::Left => serializer.serialize_str("Left"),
            Key::Right => serializer.serialize_str("Right"),
            Key::Up => serializer.serialize_str("Up"),
            Key::Down => serializer.serialize_str("Down"),
            Key::Home => serializer.serialize_str("Home"),
            Key::End => serializer.serialize_str("End"),
            Key::PageUp => serializer.serialize_str("PageUp"),
            Key::PageDown => serializer.serialize_str("PageDown"),
            Key::Delete => serializer.serialize_str("Delete"),
            Key::Insert => serializer.serialize_str("Insert"),
            Key::Esc => serializer.serialize_str("Esc"),
            Key::Char('\n') => serializer.serialize_str("Enter"),
            Key::Char('\t') => serializer.serialize_str("Tab"),
            Key::Char(c) => serializer.serialize_char(*c),
            Key::F(n) => serializer.serialize_str(&format!("F{}", n)),
            Key::Alt(c) => serializer.serialize_str(&format!("M-{}", c)),
            Key::Ctrl(c) => serializer.serialize_str(&format!("C-{}", c)),
            Key::Null => serializer.serialize_str("Null"),
            Key::Mouse(mev) => mev.serialize(serializer),
            Key::Paste(s) => serializer.serialize_str(s),
        }
    }
}

#[test]
fn test_key_serde() {
    #[derive(Debug, Deserialize, PartialEq, Eq)]
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
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\""))
                    .unwrap_err()
                    .to_string(),
                $v.to_string()
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
    test_key!("C-V", err "`V` should be a lowercase and alphanumeric character instead. for key `k` at line 1 column 5");
    test_key!("M-V", err "`V` should be a lowercase and alphanumeric character instead. for key `k` at line 1 column 5");
    test_key!("F13", err "`13` should be a number 1 <= n <= 12 instead. for key `k` at line 1 column 5");
    test_key!("Fc", err "`c` should be a number 1 <= n <= 12 instead. for key `k` at line 1 column 5");
    test_key!("adsfsf", err "Cannot derive shortcut from `adsfsf`. Please consult the manual for valid key inputs. for key `k` at line 1 column 5");
}
