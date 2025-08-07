/*
 * meli
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

use smallvec::SmallVec;

#[derive(Clone, Debug, Default)]
pub enum State {
    ExpectingControlChar,
    G0,                      // Designate G0 Character Set
    Osc1(SmallVec<[u8; 8]>), //ESC ] Operating System Command (OSC  is 0x9d).
    Osc2(SmallVec<[u8; 8]>, SmallVec<[u8; 8]>),
    Csi, // ESC [ Control Sequence Introducer (CSI  is 0x9b).
    Csi1(SmallVec<[u8; 8]>),
    Csi2(SmallVec<[u8; 8]>, SmallVec<[u8; 8]>),
    Csi3(SmallVec<[u8; 8]>, SmallVec<[u8; 8]>, SmallVec<[u8; 8]>),
    Csi4(
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
    ),
    Csi5(
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
    ),
    Csi6(
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
        SmallVec<[u8; 8]>,
    ),
    // `CSI 58 : 2 : Ps : Ps : Ps m`
    // `CSI 58 : 5 : Ps m`
    Csi58,
    Csi58_2,
    Csi58_5,
    Csi58_2_1 {
        ps_1: SmallVec<[u8; 8]>,
    },
    Csi58_2_2 {
        ps_1: SmallVec<[u8; 8]>,
        ps_2: SmallVec<[u8; 8]>,
    },
    Csi58_2_3 {
        ps_1: SmallVec<[u8; 8]>,
        ps_2: SmallVec<[u8; 8]>,
        ps_3: SmallVec<[u8; 8]>,
    },
    Csi58_5_ {
        ps: SmallVec<[u8; 8]>,
    },
    CsiQ(SmallVec<[u8; 8]>),
    #[default]
    Normal,
}

/// Used for debugging escape codes.
pub struct EscCode<'a>(pub &'a State, pub u8);

impl<'a> From<(&'a mut State, u8)> for EscCode<'a> {
    fn from(val: (&mut State, u8)) -> EscCode<'_> {
        let (s, b) = val;
        EscCode(s, b)
    }
}

impl<'a> From<(&'a State, u8)> for EscCode<'a> {
    fn from(val: (&State, u8)) -> EscCode<'_> {
        let (s, b) = val;
        EscCode(s, b)
    }
}

impl std::fmt::Display for EscCode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use State::*;
        macro_rules! unsafestr {
            ($buf:ident) => {
                unsafe { std::str::from_utf8_unchecked($buf) }
            };
        }
        match self {
            EscCode(G0, b'B') => write!(f, "ESC(B\t\tG0 USASCII charset set"),
            EscCode(G0, c) => write!(f, "ESC({}\t\tG0 charset set", *c as char),
            EscCode(Osc1(ref buf), ref c) => {
                write!(f, "ESC]{}{}\t\tOSC", unsafestr!(buf), *c as char)
            }
            EscCode(Osc2(ref buf1, ref buf2), c) => write!(
                f,
                "ESC]{};{}{}\t\tOSC [UNKNOWN]",
                unsafestr!(buf1),
                unsafestr!(buf2),
                *c as char
            ),
            EscCode(ExpectingControlChar, b'D') => write!(f, "ESC D Linefeed"),
            EscCode(Csi, b'm') => write!(
                f,
                "ESC[m\t\tCSI Character Attributes | Set Attr and Color to Normal (default)"
            ),
            EscCode(Csi, b'K') => write!(
                f,
                "ESC[K\t\tCSI Erase from the cursor to the end of the line"
            ),
            EscCode(Csi, b'L') => write!(f, "ESC[L\t\tCSI Insert one blank line"),
            EscCode(Csi, b'M') => write!(f, "ESC[M\t\tCSI delete line"),
            EscCode(Csi, b'J') => write!(
                f,
                "ESC[J\t\tCSI Erase from the cursor to the end of the screen"
            ),
            EscCode(Csi, b'H') => write!(f, "ESC[H\t\tCSI Move the cursor to home position."),
            EscCode(Csi, c) => write!(f, "ESC[{}\t\tCSI [UNKNOWN]", *c as char),
            EscCode(Csi1(ref buf), b'L') => write!(
                f,
                "ESC[{}L\t\tCSI Insert {} blank lines",
                unsafestr!(buf),
                unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'm') => write!(
                f,
                "ESC[{}m\t\tCSI Character Attributes | Set fg, bg color",
                unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'n') => write!(
                f,
                "ESC[{}n\t\tCSI Device Status Report (DSR)| Report Cursor Position",
                unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b't') if buf.as_ref() == b"18" => write!(
                f,
                "ESC[18t\t\tReport the size of the text area in characters",
            ),
            EscCode(Csi1(ref buf), b't') => write!(
                f,
                "ESC[{buf}t\t\tWindow manipulation, skipped",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'B') => write!(
                f,
                "ESC[{buf}B\t\tCSI Cursor Down {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'C') => write!(
                f,
                "ESC[{buf}C\t\tCSI Cursor Forward {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'D') => write!(
                f,
                "ESC[{buf}D\t\tCSI Cursor Backward {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'E') => write!(
                f,
                "ESC[{buf}E\t\tCSI Cursor Next Line {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'F') => write!(
                f,
                "ESC[{buf}F\t\tCSI Cursor Preceding Line {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'G') => write!(
                f,
                "ESC[{buf}G\t\tCursor Character Absolute  [column={buf}] (default = [row,1])",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'M') => write!(
                f,
                "ESC[{buf}M\t\tDelete P s Lines(s) (default = 1) (DCH).  ",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'P') => write!(
                f,
                "ESC[{buf}P\t\tDelete P s Character(s) (default = 1) (DCH).  ",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'S') => write!(
                f,
                "ESC[{buf}S\t\tCSI P s S Scroll up P s lines (default = 1) (SU), VT420, EC",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'J') => {
                write!(f, "Erase in display {buf}", buf = unsafestr!(buf))
            }
            EscCode(Csi1(ref buf), c) => {
                write!(f, "ESC[{}{}\t\tCSI [UNKNOWN]", unsafestr!(buf), *c as char)
            }
            EscCode(Csi2(ref buf1, ref buf2), b'r') => write!(
                f,
                "ESC[{};{}r\t\tCSI Set Scrolling Region [top;bottom] (default = full size of \
                 window) (DECSTBM), VT100.",
                unsafestr!(buf1),
                unsafestr!(buf2),
            ),
            EscCode(Csi2(ref buf1, ref buf2), c) => write!(
                f,
                "ESC[{};{}{}\t\tCSI",
                unsafestr!(buf1),
                unsafestr!(buf2),
                *c as char
            ),
            EscCode(Csi3(ref buf1, ref buf2, ref buf3), b'm') => write!(
                f,
                "ESC[{};{};{}m\t\tCSI Character Attributes | Set fg, bg color",
                unsafestr!(buf1),
                unsafestr!(buf2),
                unsafestr!(buf3),
            ),
            EscCode(Csi3(ref buf1, ref buf2, ref buf3), c) => write!(
                f,
                "ESC[{};{};{}{}\t\tCSI [UNKNOWN]",
                unsafestr!(buf1),
                unsafestr!(buf2),
                unsafestr!(buf3),
                *c as char
            ),
            EscCode(CsiQ(ref buf), b's') => write!(
                f,
                "ESC[?{}r\t\tCSI Save DEC Private Mode Values",
                unsafestr!(buf)
            ),
            EscCode(CsiQ(ref buf), b'r') => write!(
                f,
                "ESC[?{}r\t\tCSI Restore DEC Private Mode Values",
                unsafestr!(buf)
            ),
            EscCode(CsiQ(ref buf), b'h') if buf.as_ref() == b"25" => write!(
                f,
                "ESC[?25h\t\tCSI DEC Private Mode Set (DECSET) show cursor",
            ),
            EscCode(CsiQ(ref buf), b'h') if buf.as_ref() == b"12" => write!(
                f,
                "ESC[?12h\t\tCSI DEC Private Mode Set (DECSET) Start Blinking Cursor.",
            ),
            EscCode(CsiQ(ref buf), b'h') => write!(
                f,
                "ESC[?{}h\t\tCSI DEC Private Mode Set (DECSET). [UNKNOWN]",
                unsafestr!(buf)
            ),
            EscCode(CsiQ(ref buf), b'l') if buf.as_ref() == b"12" => write!(
                f,
                "ESC[?12l\t\tCSI DEC Private Mode Set (DECSET) Stop Blinking Cursor",
            ),
            EscCode(CsiQ(ref buf), b'l') if buf.as_ref() == b"25" => write!(
                f,
                "ESC[?25l\t\tCSI DEC Private Mode Set (DECSET) hide cursor",
            ),
            EscCode(CsiQ(ref buf), c) => {
                write!(f, "ESC[?{}{}\t\tCSI [UNKNOWN]", unsafestr!(buf), *c as char)
            }
            EscCode(Normal, c) => {
                write!(f, "{} as char: {} Normal", c, *c as char)
            }
            EscCode(unknown, c) => {
                write!(f, "{unknown:?}{c} [UNKNOWN]")
            }
        }
    }
}
