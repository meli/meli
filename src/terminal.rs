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

/*! Terminal grid cells, keys, colors, etc.
 */
extern crate serde;
use self::serde::de::Visitor;
use self::serde::{de, Deserialize, Deserializer};
extern crate unicode_segmentation;

mod color;
pub use self::color::*;
#[macro_use]
mod position;
#[macro_use]
mod cells;
#[macro_use]
mod keys;
pub mod embed;
mod text_editing;
pub use self::cells::*;
pub use self::keys::*;
pub use self::position::*;
pub use self::text_editing::*;

use std::fmt;
/*
 * CSI events we use
 */

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
    #[doc = "`CSI Ps ; Ps ; Ps t`, where `Ps = 2 2 ; 0`  -> Save xterm icon and window title on stack."]
    (SaveWindowTitleIconToStack, "22;0t")
);

derive_csi_sequence!(
    #[doc = "Restore window title and icon from terminal's title stack. `CSI Ps ; Ps ; Ps t`, where `Ps = 2 3 ; 0`  -> Restore xterm icon and window title from stack."]
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

pub const BRACKET_PASTE_START: &[u8] = b"\x1B[200~";
pub const BRACKET_PASTE_END: &[u8] = b"\x1B[201~";

pub use braille::BraillePixelIter;
mod braille {
    mod tests {
        const _X_QRCODE: [u16; 3 * 48] = [
            0xff3f, 0x3cf3, 0xff03, 0xff3f, 0x3cf3, 0xff03, 0x0330, 0x0333, 0x0003, 0x0330, 0x0333,
            0x0003, 0xf333, 0xf030, 0x3f03, 0xf333, 0xf030, 0x3f03, 0xf333, 0xfc33, 0x3f03, 0xf333,
            0xfc33, 0x3f03, 0xf333, 0x0f33, 0x3f03, 0xf333, 0x0f33, 0x3f03, 0x0330, 0x3033, 0x0003,
            0x0330, 0x3033, 0x0003, 0xff3f, 0x33f3, 0xff03, 0xff3f, 0x33f3, 0xff03, 0x0000, 0xc003,
            0x0000, 0x0000, 0xc003, 0x0000, 0x3333, 0xfc00, 0xc300, 0x3333, 0xfc00, 0xc300, 0xc3c0,
            0x3f30, 0x0c00, 0xc3c0, 0x3f30, 0x0c00, 0xcff0, 0x3f03, 0xcf00, 0xcff0, 0x3f03, 0xcf00,
            0x0ccf, 0x0f30, 0xcc00, 0x0ccf, 0x0f30, 0xcc00, 0x0033, 0x3033, 0xf300, 0x0033, 0x3033,
            0xf300, 0x0000, 0xffcc, 0x0c00, 0x0000, 0xffcc, 0x0c00, 0xff3f, 0xccfc, 0x3000, 0xff3f,
            0xccfc, 0x3000, 0x0330, 0xf0cf, 0x0f00, 0x0330, 0xf0cf, 0x0f00, 0xf333, 0xcffc, 0x3003,
            0xf333, 0xcffc, 0x3003, 0xf333, 0x0030, 0xf000, 0xf333, 0x0030, 0xf000, 0xf333, 0x3f03,
            0x0303, 0xf333, 0x3f03, 0x0303, 0x0330, 0x3030, 0xf003, 0x0330, 0x3030, 0xf003, 0xff3f,
            0x0333, 0x3303, 0xff3f, 0x0333, 0x3303, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000,
        ];

        const _XTTHOMAS: [u16; 3 * 48] = [
            0xFFFF, 0xFF0F, 0x0000, 0xFFFF, 0xFF0F, 0x0000, 0x1FFC, 0xC10F, 0x0000, 0x0FFC, 0x810F,
            0x0000, 0x07FC, 0x010F, 0x0000, 0x07FC, 0x010F, 0x0000, 0x03FC, 0x010E, 0x0000, 0x03FC,
            0x010E, 0x0000, 0x01FC, 0x010C, 0x0000, 0x00FC, 0x0100, 0x0000, 0x00FC, 0x0100, 0x0000,
            0x00FC, 0x0100, 0x0000, 0x00FC, 0x0100, 0x0000, 0x00FC, 0x0100, 0x0000, 0x00FC, 0x0100,
            0x0000, 0x00FC, 0x0100, 0x0000, 0x00FC, 0x0100, 0x0000, 0x00FC, 0xFDFF, 0xFF7F, 0x00FC,
            0xFDFF, 0xFF7F, 0x00FC, 0xFDE0, 0x0F7E, 0x00FC, 0x7DE0, 0x0F7C, 0x00FC, 0x3DE0, 0x0F78,
            0x00FC, 0x3DE0, 0x0F78, 0x00FC, 0x1DE0, 0x0F70, 0x00FC, 0x1DE0, 0x0F70, 0x00FC, 0x0DE0,
            0x0F60, 0x00FC, 0x01E0, 0x0F00, 0x00FE, 0x07E0, 0x0F00, 0xC0FF, 0x1FE0, 0x0F00, 0xC0FF,
            0x1FE0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00,
            0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0,
            0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000,
            0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00, 0x0000, 0x00E0, 0x0F00,
            0x0000, 0x00F0, 0x3F00, 0x0000, 0x00FE, 0xFF00, 0x0000, 0x00FE, 0xFF00, 0x0000, 0x0000,
            0x0000,
        ];

        const _FBIRD_SCALED_DOWN: [u16; 3 * 48] = [
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x00e0, 0x0100, 0x0000, 0x0000, 0x0f00, 0x0000, 0x0000, 0x3c00, 0x0000, 0x00e0,
            0x790c, 0x0000, 0x00fc, 0xff3f, 0x0000, 0x0043, 0xffff, 0x0300, 0x0000, 0xfc0f, 0x0000,
            0x0000, 0xf00f, 0x0000, 0x0000, 0xf00f, 0x0000, 0x0000, 0xf00f, 0x0000, 0x0000, 0xfc07,
            0x0000, 0x0000, 0xfc03, 0x0000, 0x0000, 0xfc00, 0x0000, 0x0000, 0x0e00, 0x0000, 0x0000,
            0x0700, 0x0000, 0x0000, 0x0700, 0x0000, 0x0040, 0x0300, 0x0000, 0x0000, 0x0200, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000,
        ];

        const _FBIRD: [u16; 3 * 48] = [
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x007E, 0x0000, 0x0000, 0x00E0, 0x0700, 0x0000, 0x0080, 0x1F00, 0x0000,
            0x0000, 0x7F00, 0x0000, 0x007E, 0xFCE0, 0x0000, 0xC0FF, 0xFFF9, 0x0300, 0xF0FF, 0xFFFF,
            0x0700, 0x0CE8, 0xFFFF, 0xFF00, 0x0080, 0xFFFF, 0x0103, 0x0000, 0xFEFF, 0x0000, 0x0000,
            0xF8FF, 0x0000, 0x0000, 0xF07F, 0x0000, 0x0000, 0xF07F, 0x0000, 0x0000, 0xF87F, 0x0000,
            0x0000, 0xFC3F, 0x0000, 0x0000, 0xFE3F, 0x0000, 0x0000, 0xFE1F, 0x0000, 0x0000, 0xFF07,
            0x0000, 0x0000, 0xFF01, 0x0000, 0x0080, 0x0F00, 0x0000, 0x0080, 0x0700, 0x0000, 0x00C0,
            0x0300, 0x0000, 0x00E0, 0x0300, 0x0000, 0x00F0, 0x0300, 0x0000, 0x00E8, 0x0100, 0x0000,
            0x0080, 0x0100, 0x0000, 0x0000, 0x0100, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000,
        ];

        const _XFISH: [u16; 3 * 48] = [
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x007E, 0x0000, 0x0000, 0x00E0, 0x0700, 0x0000, 0x0080, 0x1F00, 0x0000,
            0x0000, 0x7F00, 0x0000, 0x007E, 0xFCE0, 0x0000, 0xC0FF, 0xFFF9, 0x0300, 0xF0FF, 0xFFFF,
            0x0700, 0x0CE8, 0xFFFF, 0xFF00, 0x0080, 0xFFFF, 0x0103, 0x0000, 0xFEFF, 0x0000, 0x0000,
            0xF8FF, 0x0000, 0x0000, 0xF07F, 0x0000, 0x0000, 0xF07F, 0x0000, 0x0000, 0xF87F, 0x0000,
            0x0000, 0xFC3F, 0x0000, 0x0000, 0xFE3F, 0x0000, 0x0000, 0xFE1F, 0x0000, 0x0000, 0xFF07,
            0x0000, 0x0000, 0xFF01, 0x0000, 0x0080, 0x0F00, 0x0000, 0x0080, 0x0700, 0x0000, 0x00C0,
            0x0300, 0x0000, 0x00E0, 0x0300, 0x0000, 0x00F0, 0x0300, 0x0000, 0x00E8, 0x0100, 0x0000,
            0x0080, 0x0100, 0x0000, 0x0000, 0x0100, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000,
        ];

        const _XFACE: [u16; 3 * 48] = [
            0xAAAA, 0xAAAA, 0xAAAA, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAA8, 0x0000, 0x0000,
            0x0000, 0xA222, 0x2222, 0x222A, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAA2, 0x0000,
            0x0000, 0x0000, 0xA222, 0x2222, 0x2228, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAAA,
            0x0000, 0x0000, 0x0000, 0xA222, 0x2222, 0x2222, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA,
            0xAAA8, 0x0000, 0x0000, 0x0000, 0xA222, 0x2222, 0x222A, 0x0000, 0x0000, 0x0000, 0xAAAA,
            0xAAAA, 0xAAA2, 0x0000, 0x0000, 0x0000, 0xA222, 0x2222, 0x2228, 0x0000, 0x0000, 0x0000,
            0xAAAA, 0xAAAA, 0xAAAA, 0x0000, 0x0000, 0x0000, 0xA222, 0x2222, 0x2222, 0x0000, 0x0000,
            0x0000, 0xAAAA, 0xAAAA, 0xAAA8, 0x0000, 0x0000, 0x0000, 0xA222, 0x2222, 0x222A, 0x0000,
            0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAA2, 0x0000, 0x0000, 0x0000, 0xA222, 0x2222, 0x2228,
            0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAAA, 0x0000, 0x0000, 0x0000, 0xA222, 0x2222,
            0x2222, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAA8, 0x0000, 0x0000, 0x0000, 0xA222,
            0x2222, 0x222A, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAA2, 0x0000, 0x0000, 0x0000,
            0xA222, 0x2222, 0x2228, 0x0000, 0x0000, 0x0000, 0xAAAA, 0xAAAA, 0xAAAA, 0x0000, 0x0000,
            0x0000,
        ];

        #[test]
        fn test_braille() {
            /* lines has 12 bitmaps, with 3 bitmap making each line that is 4 lines.
             * lines = [
             *        a_0, a_1, a_2,
             *        b_0, b_1, b_2,
             *        c_0, c_1, c_2,
             *        d_0, d_1, d_2,
             * ];
             */
            println!("Thomas: ");
            for lines in _XTTHOMAS.chunks(12) {
                let iter = super::BraillePixelIter::from(lines);
                for b in iter {
                    print!("{}", b);
                }
                println!();
            }

            println!("fbird: ");
            for lines in _FBIRD.chunks(12) {
                let iter = super::BraillePixelIter::from(lines);
                for b in iter {
                    print!("{}", b);
                }
                println!();
            }
            println!("ABC QR code: ");
            for lines in _X_QRCODE.chunks(12) {
                let iter = super::BraillePixelIter::from(lines);
                for b in iter {
                    print!("{}", b);
                }
                println!();
            }
        }
    }
    struct Braille16bitColumn {
        // each u16 in the tuple is one line ( first_line, second_line, third_line, fourth line) */
        bitmaps: (u16, u16, u16, u16),
        // reverse 1-indexing, so column: 1 means the left-most column in 16bit word */
        bitcolumn: u32,
    }

    /*
    impl std::fmt::Debug for Braille16bitColumn {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(
                f,
                "Braille16bitColumn {{\n bitmaps: \n{:x}\n{:x}\n{:x}\n{:x},\n bitcolumn: {}\n\n{}\n{:016b}\n{:016b}\n{:016b}\n{:016b},\n
                }}",self.bitmaps.0, self.bitmaps.1, self.bitmaps.2, self.bitmaps.3, self.bitcolumn, format!("{:016b}", 0x0001_u16.rotate_left(self.bitcolumn)).replace("0"," ").replace("1", "v"), self.bitmaps.0, self.bitmaps.1, self.bitmaps.2, self.bitmaps.3,
            )
        }
    }
    */

    /// Iterate on 2x4 pixel blocks from a bitmap and return a unicode braille character for each
    /// block. The iterator holds four lines of bitmaps encoded as `u16` numbers in swapped bit
    /// order, like the `xbm` graphics format. The bitmap is split to `u16` columns.
    ///
    /// ## Usage
    /// ```no_run
    /// /* BEE is the contents of a 48x48 xbm file. xbm is a C-like array of 8bit values, and
    ///  * each pair was manually (macro-ually?) condensed into a single 16bit value. Each 3 items
    ///  * represent one pixel row.
    ///  */
    /// const BEE: [u16; 3 * 48] = [
    ///     0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    ///     0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    ///     0x0000, 0x0002, 0x0000, 0x0000, 0xe003, 0x0000, 0x0000, 0xfc00, 0x0000, 0x0000, 0x3f00,
    ///     0x0000, 0x00e0, 0x0f00, 0x0000, 0x00f8, 0x0300, 0x0000, 0x00fe, 0x0000, 0x0080, 0x8f0d,
    ///     0x0000, 0x00e0, 0xff7f, 0x0000, 0x00f8, 0xffff, 0x0300, 0x00fc, 0xffff, 0x0f00, 0x00fe,
    ///     0xffff, 0x3f00, 0x00ff, 0xffff, 0xff00, 0xc0ff, 0xffff, 0xff01, 0xc0ff, 0xff77, 0xff07,
    ///     0xf0f9, 0xffff, 0xff07, 0xf0f0, 0xffef, 0xfd0f, 0xf0e0, 0xffff, 0xfb1f, 0xf0e1, 0xffc1,
    ///     0xfb0f, 0xe0f3, 0xffc3, 0xf307, 0xc0f7, 0xffc0, 0xe100, 0xc0ff, 0xd9e0, 0x3f00, 0x803e,
    ///     0xc1f8, 0x5f00, 0x8076, 0x43f4, 0xbf18, 0x806c, 0x43fc, 0xf325, 0x0009, 0xc3df, 0x4326,
    ///     0x001a, 0xcf3f, 0x622d, 0x0034, 0xff01, 0x2224, 0x00f0, 0xff00, 0x8312, 0x00a0, 0x5700,
    ///     0x0309, 0x00f8, 0x1b00, 0x8f06, 0x0048, 0x6000, 0xcd03, 0x0018, 0x6624, 0xdf00, 0x0030,
    ///     0x820f, 0x3f00, 0x00c0, 0xf0ff, 0x3f00, 0x0080, 0x03fe, 0x7f00, 0x0000, 0x7ce0, 0x0f00,
    ///     0x0000, 0x809f, 0x1c00, 0x0000, 0x0000, 0x3800, 0x0000, 0x0000, 0x7000, 0x0000, 0x0000,
    ///     0xe000,
    /// ];
    ///
    /// for lines in BEE.chunks(12) {
    ///     let iter = ui::BraillePixelIter::from(lines);
    ///     for b in iter {
    ///         print!("{}", b);
    ///     }
    ///     println!("");
    /// }
    /// ```
    ///
    /// Output:
    ///
    /// ```text
    /// ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⠀
    /// ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣤⣶⠾⠛⠉⠀⠀⠀
    /// ⠀⠀⠀⠀⠀⠀⢀⣠⣤⣤⣀⣠⣔⣾⣛⡛⠉⠀⠀⠀⠀⠀⠀⠀
    /// ⠀⠀⠀⠀⣠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣤⣀⠀⠀⠀⠀
    /// ⠀⠀⣤⣿⠟⠻⣿⣿⣿⣿⣿⣿⣿⣯⢿⣯⡿⣿⣿⣿⣷⣆⠀⠀
    /// ⠀⠀⠻⣿⣦⡀⣼⣿⣿⣿⣿⣿⠯⠉⠉⣿⡿⠘⢿⣿⠿⠟⠁⠀
    /// ⠀⠀⠀⢹⠹⣟⢿⡍⣧⠈⠁⡟⠀⣔⣾⣿⣿⠿⣯⣢⡀⡠⢄⠀
    /// ⠀⠀⠀⠀⠑⠜⣦⣀⣿⣶⣤⣿⠟⠛⠓⠉⣹⠀⠰⢃⢊⠗⡸⠀
    /// ⠀⠀⠀⠀⠀⢰⡚⠞⢛⡑⢣⡅⠀⡀⢀⠀⣟⣶⡀⣴⠵⠊⠀⠀
    /// ⠀⠀⠀⠀⠀⠀⠉⠲⠬⣀⣒⡚⠻⠿⢶⣶⣿⣿⠿⠄⠀⠀⠀⠀
    /// ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠁⠈⠀⠙⢷⣄⠀⠀⠀
    /// ```
    ///
    ///
    /// ## Explanation:
    /// For the following bitmap:
    /// ```text
    ///   ◻◼◻◻◼◻◻◼◻◻◻◼◼◼◼◼
    ///   ◼◼◼◼◼◼◻◻◼◻◼◻◻◼◼◼
    ///   ◻◼◼◼◼◼◼◼◻◻◻◻◼◼◻◻
    ///   ◼◻◼◼◻◼◼◻◼◼◼◻◻◻◻◻
    /// ```
    ///
    /// Iteration on each step examines two columns:
    /// ```text
    ///   ⇩⇩
    ///   ◻◼┆◻◻┆◼◻┆◻◼┆◻◻┆◻◼┆◼◼┆◼◼
    ///   ◼◼┆◼◼┆◼◼┆◻◻┆◼◻┆◼◻┆◻◼┆◼◼
    ///   ◻◼┆◼◼┆◼◼┆◼◼┆◻◻┆◻◻┆◼◼┆◻◻
    ///   ◼◻┆◼◼┆◻◼┆◼◻┆◼◼┆◼◻┆◻◻┆◻◻
    /// ```
    ///
    ///  The first two columns are encoded as:
    ///
    /// ```text
    /// ┏━━━━━━┳━━━━┓
    /// ┃pixels┃bits┃
    /// ┡━━━━━━╇━━━━┩
    /// │  ◻◼  │ 14 │
    /// │  ◼◼  │ 25 │
    /// │  ◻◻  │ 36 │
    /// │  ◼◻  │ 78 │
    /// └──────┴────┘
    ///   =
    /// braille bitmap is
    /// ◻◼◻◼◼◻◼◻ = 0b01011010 = 0x5A
    /// 12345678
    /// ```
    /// and braille character is bitmap + 0x2800 (Braille block's position in Unicode)
    /// ```text
    /// 0x5A + 0x2800 = 0x285A = '⡚'
    /// ```
    ///
    /// Why three columns? I originally wrote this for X-Face bitmaps, which are 48x48 pixels.
    pub struct BraillePixelIter {
        columns: [Braille16bitColumn; 3],
        column_ptr: usize,
    }

    impl From<&[u16]> for BraillePixelIter {
        fn from(from: &[u16]) -> Self {
            BraillePixelIter {
                columns: [
                    Braille16bitColumn {
                        bitmaps: (
                            from[0].swap_bytes().reverse_bits(),
                            from[3].swap_bytes().reverse_bits(),
                            from[6].swap_bytes().reverse_bits(),
                            from[9].swap_bytes().reverse_bits(),
                        ),
                        bitcolumn: 1,
                    },
                    Braille16bitColumn {
                        bitmaps: (
                            from[1].swap_bytes().reverse_bits(),
                            from[4].swap_bytes().reverse_bits(),
                            from[7].swap_bytes().reverse_bits(),
                            from[10].swap_bytes().reverse_bits(),
                        ),
                        bitcolumn: 1,
                    },
                    Braille16bitColumn {
                        bitmaps: (
                            from[2].swap_bytes().reverse_bits(),
                            from[5].swap_bytes().reverse_bits(),
                            from[8].swap_bytes().reverse_bits(),
                            from[11].swap_bytes().reverse_bits(),
                        ),
                        bitcolumn: 1,
                    },
                ],
                column_ptr: 0,
            }
        }
    }

    impl Iterator for BraillePixelIter {
        type Item = char;
        fn next(&mut self) -> Option<char> {
            if self.columns[self.column_ptr].bitcolumn == 17 {
                if self.column_ptr == 2 {
                    return None;
                }
                self.column_ptr += 1;
            }
            let Braille16bitColumn {
                ref bitmaps,
                ref mut bitcolumn,
            } = &mut self.columns[self.column_ptr];
            /* First bitcolumn out of two (braille is 2x4) */
            let mut bits: u16 = 0x1 & (bitmaps.0.rotate_left(*bitcolumn)); // * 0x1;
            bits += (0x1 & (bitmaps.1.rotate_left(*bitcolumn))) * 0x2;
            bits += (0x1 & (bitmaps.2.rotate_left(*bitcolumn))) * 0x4;
            bits += (0x1 & (bitmaps.3.rotate_left(*bitcolumn))) * 0x40;
            /* Second bitcolumn */
            *bitcolumn += 1;
            bits += (0x1 & (bitmaps.0.rotate_left(*bitcolumn))) * 0x8;
            bits += (0x1 & (bitmaps.1.rotate_left(*bitcolumn))) * 0x10;
            bits += (0x1 & (bitmaps.2.rotate_left(*bitcolumn))) * 0x20;
            bits += (0x1 & (bitmaps.3.rotate_left(*bitcolumn))) * 0x80;
            *bitcolumn += 1;

            /* The Braille Patterns block spans the entire [U+2800, U+28FF] range and bits is a
             * 16bit integer ∈ [0x00, 0xFF] so this is guaranteed to be a Unicode char */
            Some(unsafe { std::char::from_u32_unchecked(bits as u32 + 0x2800) })
        }
    }
}

pub use screen::StateStdout;
pub mod screen {
    use super::*;
    use cells::CellBuffer;
    use std::io::BufWriter;
    use std::io::Write;
    use termion::raw::IntoRawMode;
    use termion::screen::AlternateScreen;
    use termion::{clear, cursor};
    pub type StateStdout =
        termion::screen::AlternateScreen<termion::raw::RawTerminal<BufWriter<std::io::Stdout>>>;
    pub struct Screen {
        pub cols: usize,
        pub rows: usize,
        pub grid: CellBuffer,
        pub overlay_grid: CellBuffer,
        pub stdout: Option<StateStdout>,
        pub mouse: bool,
        pub draw_horizontal_segment_fn:
            fn(&mut CellBuffer, &mut StateStdout, usize, usize, usize) -> (),
    }

    impl Screen {
        /// Switch back to the terminal's main screen (The command line the user sees before opening
        /// the application)
        pub fn switch_to_main_screen(&mut self) {
            let mouse = self.mouse;
            write!(
                self.stdout.as_mut().unwrap(),
                "{}{}{}{}{disable_sgr_mouse}{disable_mouse}",
                termion::screen::ToMainScreen,
                cursor::Show,
                RestoreWindowTitleIconFromStack,
                BracketModeEnd,
                disable_sgr_mouse = if mouse { DisableSGRMouse.as_ref() } else { "" },
                disable_mouse = if mouse { DisableMouse.as_ref() } else { "" },
            )
            .unwrap();
            self.flush();
            self.stdout = None;
        }

        pub fn switch_to_alternate_screen(&mut self, context: &crate::Context) {
            let s = std::io::stdout();
            let s = BufWriter::with_capacity(240 * 80, s);

            let mut stdout = AlternateScreen::from(s.into_raw_mode().unwrap());

            write!(
                &mut stdout,
                "{save_title_to_stack}{}{}{}{window_title}{}{}{enable_mouse}{enable_sgr_mouse}",
                termion::screen::ToAlternateScreen,
                cursor::Hide,
                clear::All,
                cursor::Goto(1, 1),
                BracketModeStart,
                save_title_to_stack = SaveWindowTitleIconToStack,
                window_title = if let Some(ref title) = context.settings.terminal.window_title {
                    format!("\x1b]2;{}\x07", title)
                } else {
                    String::new()
                },
                enable_mouse = if self.mouse { EnableMouse.as_ref() } else { "" },
                enable_sgr_mouse = if self.mouse {
                    EnableSGRMouse.as_ref()
                } else {
                    ""
                },
            )
            .unwrap();

            self.stdout = Some(stdout);
            self.flush();
        }

        pub fn flush(&mut self) {
            if let Some(s) = self.stdout.as_mut() {
                s.flush().unwrap();
            }
        }

        pub fn set_mouse(&mut self, value: bool) {
            if let Some(stdout) = self.stdout.as_mut() {
                write!(
                    stdout,
                    "{mouse}{sgr_mouse}",
                    mouse = if value {
                        AsRef::<str>::as_ref(&EnableMouse)
                    } else {
                        AsRef::<str>::as_ref(&DisableMouse)
                    },
                    sgr_mouse = if value {
                        AsRef::<str>::as_ref(&EnableSGRMouse)
                    } else {
                        AsRef::<str>::as_ref(&DisableSGRMouse)
                    },
                )
                .unwrap();
            }
            self.flush();
        }
        /// On `SIGWNICH` the `State` redraws itself according to the new terminal size.
        pub fn update_size(&mut self) {
            let termsize = termion::terminal_size().ok();
            let termcols = termsize.map(|(w, _)| w);
            let termrows = termsize.map(|(_, h)| h);
            if termcols.unwrap_or(72) as usize != self.cols
                || termrows.unwrap_or(120) as usize != self.rows
            {
                debug!(
                    "Size updated, from ({}, {}) -> ({:?}, {:?})",
                    self.cols, self.rows, termcols, termrows
                );
            }
            self.cols = termcols.unwrap_or(72) as usize;
            self.rows = termrows.unwrap_or(120) as usize;
            if !self.grid.resize(self.cols, self.rows, None) {
                panic!(
                    "Terminal size too big: ({} cols, {} rows)",
                    self.cols, self.rows
                );
            }
            let _ = self.overlay_grid.resize(self.cols, self.rows, None);
        }

        /// Draw only a specific `area` on the screen.
        pub fn draw_horizontal_segment(
            grid: &mut CellBuffer,
            stdout: &mut StateStdout,
            x_start: usize,
            x_end: usize,
            y: usize,
        ) {
            write!(
                stdout,
                "{}",
                cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
            )
            .unwrap();
            let mut current_fg = Color::Default;
            let mut current_bg = Color::Default;
            let mut current_attrs = Attr::DEFAULT;
            write!(stdout, "\x1B[m").unwrap();
            for x in x_start..=x_end {
                let c = &grid[(x, y)];
                if c.attrs() != current_attrs {
                    c.attrs().write(current_attrs, stdout).unwrap();
                    current_attrs = c.attrs();
                }
                if c.bg() != current_bg {
                    c.bg().write_bg(stdout).unwrap();
                    current_bg = c.bg();
                }
                if c.fg() != current_fg {
                    c.fg().write_fg(stdout).unwrap();
                    current_fg = c.fg();
                }
                if !c.empty() {
                    write!(stdout, "{}", c.ch()).unwrap();
                }
            }
        }

        pub fn draw_horizontal_segment_no_color(
            grid: &mut CellBuffer,
            stdout: &mut StateStdout,
            x_start: usize,
            x_end: usize,
            y: usize,
        ) {
            write!(
                stdout,
                "{}",
                cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
            )
            .unwrap();
            let mut current_attrs = Attr::DEFAULT;
            write!(stdout, "\x1B[m").unwrap();
            for x in x_start..=x_end {
                let c = &grid[(x, y)];
                if c.attrs() != current_attrs {
                    c.attrs().write(current_attrs, stdout).unwrap();
                    current_attrs = c.attrs();
                }
                if !c.empty() {
                    write!(stdout, "{}", c.ch()).unwrap();
                }
            }
        }
    }
}
