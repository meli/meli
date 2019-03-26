/*
 * meli - ui crate.
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

use super::*;
use chan;
use std::fmt;
use std::io;
use termion::event::Event as TermionEvent;
use termion::event::Key as TermionKey;
use termion::input::TermRead;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
    /// Note that certain keys may not be modifiable with `ctrl`, due to limitations of terminals.
    Ctrl(char),
    /// Null byte.
    Null,
    /// Esc key.
    Esc,
    Paste(String),
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

#[derive(PartialEq)]
enum InputMode {
    Normal,
    Paste,
}

/*
 * If we fork (for example start $EDITOR) we want the input-thread to stop reading from stdin. The
 * best way I came up with right now is to send a signal to the thread that is read in the first
 * input in stdin after the fork, and then the thread kills itself. The parent process spawns a new
 * input-thread when the child returns.
 *
 * The main loop uses try_wait_on_child() to check if child has exited.
 */
pub fn get_events(
    stdin: io::Stdin,
    mut closure: impl FnMut(Key),
    mut exit: impl FnMut(),
    rx: &chan::Receiver<bool>,
) {
    let mut input_mode = InputMode::Normal;
    let mut paste_buf = String::with_capacity(256);
    for c in stdin.events() {
        chan_select! {
            default => {},
            rx.recv() -> val => {
                if let Some(true) = val {
                    exit();
                    return;
                } else if let Some(false) = val {
                    return;
                }
            }


        };
        match c {
            Ok(TermionEvent::Key(k)) if input_mode == InputMode::Normal => {
                closure(Key::from(k));
            }
            Ok(TermionEvent::Key(TermionKey::Char(k))) if input_mode == InputMode::Paste => {
                paste_buf.push(k);
            }
            Ok(TermionEvent::Unsupported(ref k)) if k.as_slice() == BRACKET_PASTE_START => {
                input_mode = InputMode::Paste;
            }
            Ok(TermionEvent::Unsupported(ref k)) if k.as_slice() == BRACKET_PASTE_END => {
                input_mode = InputMode::Normal;
                let ret = Key::from(&paste_buf);
                paste_buf.clear();
                closure(ret);
            }
            _ => {} // Mouse events or errors.
        }
    }
}

/*
 * CSI events we use
 */

// Some macros taken from termion:
/// Create a CSI-introduced sequence.
macro_rules! csi {
    ($( $l:expr ),*) => { concat!("\x1B[", $( $l ),*) };
}

/// Derive a CSI sequence struct.
macro_rules! derive_csi_sequence {
    ($(#[$outer:meta])*
    ($name:ident, $value:expr)) => {
        $(#[$outer])*
        #[derive(Copy, Clone)]
        pub struct $name;

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, csi!($value))
            }
        }

        impl AsRef<[u8]> for $name {
            fn as_ref(&self) -> &'static [u8] {
                csi!($value).as_bytes()
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &'static str {
                csi!($value)
            }
        }
    };
}

derive_csi_sequence!(
    #[doc = "Empty struct with a Display implementation that returns the byte sequence to start [Bracketed Paste Mode](http://www.xfree86.org/current/ctlseqs.html#Bracketed%20Paste%20Mode)"]
    (BracketModeStart, "?2004h")
);

derive_csi_sequence!(
    #[doc = "Empty struct with a Display implementation that returns the byte sequence to end [Bracketed Paste Mode](http://www.xfree86.org/current/ctlseqs.html#Bracketed%20Paste%20Mode)"]
    (BracketModeEnd, "?2003l")
);

pub const BRACKET_PASTE_START: &[u8] = b"\x1B[200~";
pub const BRACKET_PASTE_END: &[u8] = b"\x1B[201~";

const FIELDS: &[&str] = &[];

impl<'de> Deserialize<'de> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct KeyVisitor;

        impl<'de> Visitor<'de> for KeyVisitor {
            type Value = Key;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("`secs` or `nanos`")
            }

            fn visit_str<E>(self, value: &str) -> Result<Key, E>
            where
                E: de::Error,
            {
                match value {
                    "Backspace" => Ok(Key::Backspace),
                    "Left" => Ok(Key::Left),
                    "Right" => Ok(Key::Right),
                    "Up" => Ok(Key::Up),
                    "Down" => Ok(Key::Down),
                    "Home" => Ok(Key::Home),
                    "End" => Ok(Key::End),
                    "PageUp" => Ok(Key::PageUp),
                    "PageDown" => Ok(Key::PageDown),
                    "Delete" => Ok(Key::Delete),
                    "Insert" => Ok(Key::Insert),
                    "Esc" => Ok(Key::Esc),
                    ref s if s.len() == 1 => Ok(Key::Char(s.chars().nth(0).unwrap())),
                    _ => Err(de::Error::unknown_field(value, FIELDS)),
                }
            }
        }

        deserializer.deserialize_identifier(KeyVisitor)
    }
}
