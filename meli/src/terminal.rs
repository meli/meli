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
pub mod position;
#[macro_use]
pub mod cells;
#[macro_use]
pub mod keys;
pub mod embed;
pub mod text_editing;

pub use braille::BraillePixelIter;
pub use screen::{Screen, StateStdout};

pub use self::{cells::*, keys::*, position::*, text_editing::*};

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
