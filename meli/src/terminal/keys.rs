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

use serde::{de, de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

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

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::F(n) => write!(f, "F{n}"),
            Self::Char(' ') => write!(f, "Space"),
            Self::Char('\t') => write!(f, "Tab"),
            Self::Char('\n') => write!(f, "Enter"),
            Self::Char(c) => write!(f, "{c}"),
            Self::Alt(c) => write!(f, "M-{c}"),
            Self::Ctrl(c) => write!(f, "C-{c}"),
            Self::Paste(_) => write!(f, "Pasted buf"),
            Self::Null => write!(f, "Null byte"),
            Self::Esc => write!(f, "Esc"),
            Self::Backspace => write!(f, "Backspace"),
            Self::Left => write!(f, "Left"),
            Self::Right => write!(f, "Right"),
            Self::Up => write!(f, "Up"),
            Self::Down => write!(f, "Down"),
            Self::Home => write!(f, "Home"),
            Self::End => write!(f, "End"),
            Self::PageUp => write!(f, "PageUp"),
            Self::PageDown => write!(f, "PageDown"),
            Self::Delete => write!(f, "Delete"),
            Self::Insert => write!(f, "Insert"),
            Self::Mouse(_) => write!(f, "Mouse"),
        }
    }
}

impl<'a> From<&'a String> for Key {
    fn from(v: &'a String) -> Self {
        Self::Paste(v.to_string())
    }
}

impl PartialEq<Key> for &Key {
    fn eq(&self, other: &Key) -> bool {
        **self == *other
    }
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
