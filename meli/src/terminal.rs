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

use serde::{de, de::Visitor, Deserialize, Deserializer};

mod braille;
mod color;
mod screen;
pub use color::*;
#[macro_use]
pub mod cells;
#[macro_use]
pub mod keys;
pub mod embedded;
#[cfg(test)]
mod tests;
pub mod text_editing;

use std::{
    borrow::Cow,
    io::{BufRead, Write},
};

pub use braille::BraillePixelIter;
pub use screen::{Area, Screen, ScreenGeneration, StateStdout, Tty, Virtual};

pub use self::{cells::*, keys::*, text_editing::*};

/// A type alias for a `(x, y)` position on screen.
pub type Pos = (usize, usize);

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Alignment {
    /// Stretch to fill all space if possible, center if no meaningful way to
    /// stretch.
    Fill,
    /// Snap to left or top side, leaving space on right or bottom.
    Start,
    /// Snap to right or bottom side, leaving space on left or top.
    End,
    /// Center natural width of widget inside the allocation.
    #[default]
    Center,
}

#[macro_export]
macro_rules! emoji_text_presentation_selector {
    () => {
        '\u{FE0E}'
    };
}

/*
 * CSI events we use
 */

pub const BRACKET_PASTE_START: &[u8] = b"\x1B[200~";
pub const BRACKET_PASTE_END: &[u8] = b"\x1B[201~";

// Some macros taken from termion:
/// Create a CSI-introduced sequence.
macro_rules! csi {
    ($( $l:expr ),*) => { concat!("\x1b[", $( $l ),*) };
}

/// Derive a CSI sequence struct.
macro_rules! derive_csi_sequence {
    ($(#[$outer:meta])*
    ($name:ident, $value:expr)) => {
        $(#[$outer])*
        #[derive(Copy, Clone)]
        pub struct $name;

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, csi!($value))
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
    ///Ps = 1 0 0 2  ⇒  Don't use Cell Motion Mouse Tracking, xterm
    (DisableMouse, "?1002l")
);

derive_csi_sequence!(
    ///Ps = 1 0 0 2  ⇒  Use Cell Motion Mouse Tracking, xterm
    (EnableMouse, "?1002h")
);

derive_csi_sequence!(
    ///Ps = 1 0 0 6  Enable SGR Mouse Mode, xterm.
    (EnableSGRMouse, "?1006h")
);

derive_csi_sequence!(
    ///Ps = 1 0 0 6  Disable SGR Mouse Mode, xterm.
    (DisableSGRMouse, "?1006l")
);

derive_csi_sequence!(
    ///Ps = 1 0 0 7  Enable Alternate Scroll Mode, xterm.
    (EnableAlternateScrollMode, "?1007h")
);

derive_csi_sequence!(
    ///Ps = 1 0 0 7  Disable Alternate Scroll Mode, xterm.
    (DisableAlternateScrollMode, "?1007l")
);

derive_csi_sequence!(
    #[doc = "`CSI Ps ; Ps ; Ps t`, where `Ps = 2 2 ; 0`  -> Save xterm icon and window title on \
             stack."]
    (SaveWindowTitleIconToStack, "22;0t")
);

derive_csi_sequence!(
    #[doc = "Restore window title and icon from terminal's title stack. `CSI Ps ; Ps ; Ps t`, \
             where `Ps = 2 3 ; 0`  -> Restore xterm icon and window title from stack."]
    (RestoreWindowTitleIconFromStack, "23;0t")
);

derive_csi_sequence!(
    #[doc = "Empty struct with a Display implementation that returns the byte sequence to start [Bracketed Paste Mode](http://www.xfree86.org/current/ctlseqs.html#Bracketed%20Paste%20Mode)"]
    (BracketModeStart, "?2004h")
);

derive_csi_sequence!(
    #[doc = "Empty struct with a Display implementation that returns the byte sequence to end [Bracketed Paste Mode](http://www.xfree86.org/current/ctlseqs.html#Bracketed%20Paste%20Mode)"]
    (BracketModeEnd, "?2004l")
);

pub struct Ask {
    pub message: String,
}

impl Ask {
    pub fn run(self) -> bool {
        let mut buffer = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();

        print!("{} [Y/n] ", &self.message);
        let _ = std::io::stdout().flush();
        loop {
            buffer.clear();
            handle
                .read_line(&mut buffer)
                .expect("Could not read from stdin.");

            match buffer.trim() {
                "" | "Y" | "y" | "yes" | "YES" | "Yes" => {
                    return true;
                }
                "n" | "N" | "no" | "No" | "NO" => {
                    return false;
                }
                _ => {
                    print!("\n{} [Y/n] ", &self.message);
                    let _ = std::io::stdout().flush();
                }
            }
        }
    }
}

pub trait TextPresentation {
    /// Return `input` string while trying to use text presentations of
    /// symbols and emoji as much as possible. Might not work on all
    /// non-text symbols and is experimental.
    fn text_pr(&self) -> Cow<str>;
}

impl TextPresentation for str {
    fn text_pr(&self) -> Cow<str> {
        use std::str::FromStr;

        use melib::text::grapheme_clusters::TextProcessing;
        // [ref:FIXME]: add all relevant Unicode range/blocks to TextPresentation::text_pr()

        // [ref:VERIFY]: Check whether our existing unicode tables can be used for TextPresentation::text_pr()

        // [ref:DEBT]: TextPresentation::text_pr() is not tied to text submodule which can be updated for
        // each Unicode release

        let get_base_char = |grapheme: &Self| -> Option<char> {
            char::from_str(grapheme.get(0..4).or_else(|| {
                grapheme
                    .get(0..3)
                    .or_else(|| grapheme.get(0..2).or_else(|| grapheme.get(0..1)))
            })?)
            .ok()
        };
        let is_emoji = |base_char: char| -> bool {
            [
                0x2600..0x26FF,   // Miscellaneous Symbols
                0x2B00..0x2BFF,   // Miscellaneous Symbols and Arrows
                0x1F300..0x1F5FF, // Miscellaneous Symbols and Pictographs
                0x1F600..0x1F64F, // Emoticons
                0x1F680..0x1F6FF, // Transport and Map
                0x2600..0x26FF,   // Misc symbols
                0x2700..0x27BF,   // Dingbats
                0xFE00..0xFE0F,   // Variation Selectors
                0x1F900..0x1F9FF, // Supplemental Symbols and Pictographs
                0x1F1E6..0x1F1FF, // Flags
            ]
            .iter()
            .any(|range| range.contains(&(base_char as u32)))
        };

        let graphemes = self.split_graphemes();
        for g in &graphemes {
            let Some(base_char) = get_base_char(g) else {
                // Bail out
                return Cow::from(self);
            };
            if is_emoji(base_char) {
                let mut ret = String::with_capacity(self.len() + 1);
                for g in &graphemes {
                    ret.push_str(g);
                    let Some(base_char) = get_base_char(g) else {
                        // Bail out
                        return Cow::from(self);
                    };
                    if is_emoji(base_char) {
                        ret.push(emoji_text_presentation_selector!());
                    }
                }
                return Cow::from(ret);
            }
        }

        Cow::from(self)
    }
}

/// Returns `true` if standard output corresponds to an interactive TTY session.
pub fn is_tty() -> bool {
    unsafe { libc::isatty(libc::STDOUT_FILENO) == 1 }
}
