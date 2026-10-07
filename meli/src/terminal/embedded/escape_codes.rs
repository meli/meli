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

use crate::terminal::Color;

#[derive(Clone, Debug, Default)]
pub enum State {
    #[default]
    Normal,
    ExpectingControlChar,
    G0,                           // Designate G0 Character Set
    Osc1([SmallVec<[u8; 8]>; 1]), //ESC ] Operating System Command (OSC  is 0x9d).
    Osc2([SmallVec<[u8; 8]>; 2]),
    Csi, // ESC [ Control Sequence Introducer (CSI  is 0x9b).
    Csi1([SmallVec<[u8; 8]>; 1]),
    Csi2([SmallVec<[u8; 8]>; 2]),
    Csi3([SmallVec<[u8; 8]>; 3]),
    Csi4([SmallVec<[u8; 8]>; 4]),
    Csi5([SmallVec<[u8; 8]>; 5]),
    Csi6([SmallVec<[u8; 8]>; 6]),
    CsiLarge(Vec<SmallVec<[u8; 8]>>),
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
            EscCode(Osc1([ref buf]), ref c) => {
                write!(f, "ESC]{}{}\t\tOSC", unsafestr!(buf), *c as char)
            }
            EscCode(Osc2([ref buf1, ref buf2]), c) => write!(
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
            EscCode(Csi1([ref buf]), b'L') => write!(
                f,
                "ESC[{}L\t\tCSI Insert {} blank lines",
                unsafestr!(buf),
                unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'm') => write!(
                f,
                "ESC[{}m\t\tCSI Character Attributes | Set fg, bg color",
                unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'n') => write!(
                f,
                "ESC[{}n\t\tCSI Device Status Report (DSR)| Report Cursor Position",
                unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b't') if buf.as_ref() == b"18" => write!(
                f,
                "ESC[18t\t\tReport the size of the text area in characters",
            ),
            EscCode(Csi1([ref buf]), b't') => write!(
                f,
                "ESC[{buf}t\t\tWindow manipulation, skipped",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'B') => write!(
                f,
                "ESC[{buf}B\t\tCSI Cursor Down {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'C') => write!(
                f,
                "ESC[{buf}C\t\tCSI Cursor Forward {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'D') => write!(
                f,
                "ESC[{buf}D\t\tCSI Cursor Backward {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'E') => write!(
                f,
                "ESC[{buf}E\t\tCSI Cursor Next Line {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'F') => write!(
                f,
                "ESC[{buf}F\t\tCSI Cursor Preceding Line {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'G') => write!(
                f,
                "ESC[{buf}G\t\tCursor Character Absolute  [column={buf}] (default = [row,1])",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'M') => write!(
                f,
                "ESC[{buf}M\t\tDelete P s Lines(s) (default = 1) (DCH).  ",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'P') => write!(
                f,
                "ESC[{buf}P\t\tDelete P s Character(s) (default = 1) (DCH).  ",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'S') => write!(
                f,
                "ESC[{buf}S\t\tCSI P s S Scroll up P s lines (default = 1) (SU), VT420, EC",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1([ref buf]), b'J') => {
                write!(f, "Erase in display {buf}", buf = unsafestr!(buf))
            }
            EscCode(Csi1([ref buf]), c) => {
                write!(f, "ESC[{}{}\t\tCSI [UNKNOWN]", unsafestr!(buf), *c as char)
            }
            EscCode(Csi2([ref buf1, ref buf2]), b'r') => write!(
                f,
                "ESC[{};{}r\t\tCSI Set Scrolling Region [top;bottom] (default = full size of \
                 window) (DECSTBM), VT100.",
                unsafestr!(buf1),
                unsafestr!(buf2),
            ),
            EscCode(Csi2([ref buf1, ref buf2]), c) => write!(
                f,
                "ESC[{};{}{}\t\tCSI",
                unsafestr!(buf1),
                unsafestr!(buf2),
                *c as char
            ),
            EscCode(Csi3([ref buf1, ref buf2, ref buf3]), b'm') => write!(
                f,
                "ESC[{};{};{}m\t\tCSI Character Attributes | Set fg, bg color",
                unsafestr!(buf1),
                unsafestr!(buf2),
                unsafestr!(buf3),
            ),
            EscCode(Csi3([ref buf1, ref buf2, ref buf3]), c) => write!(
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Intensity {
    /// SGR 22: normal text intensity.
    #[default]
    Normal,
    /// SGR 1: bold text intensity.
    Bold,
    /// SGR 2: dim text intensity.
    Dim,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Blink {
    /// SGR 25: disable blinking text.
    #[default]
    None,
    /// SGR 5: slow blinking text.
    Slow,
    /// SGR 6: rapid blinking text.
    Rapid,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Font {
    /// SGR 10: use the default font.
    #[default]
    Default,

    /// SGR 11-19: select an alternate font.
    ///
    /// Valid values are 1-9, corresponding to SGR 11 through SGR 19.
    Alternate(u8),
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum VerticalAlign {
    /// SGR 75: baseline text alignment.
    #[default]
    BaseLine = 0,
    /// SGR 73: superscript text alignment.
    SuperScript = 1,
    /// SGR 74: subscript text alignment.
    SubScript = 2,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Underline {
    /// No underline
    #[default]
    None = 0,

    /// Straight underline
    Single = 1,

    /// Two underlines stacked on top of one another
    Double = 2,

    /// Curly / "squiggly" / "wavy" underline
    Curly = 3,

    /// Dotted underline
    Dotted = 4,

    /// Dashed underline
    Dashed = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sgr {
    /// SGR 0: reset all graphic rendition attributes to terminal defaults.
    Reset,
    /// Set text intensity
    Intensity(Intensity),

    /// Set underline style described by [`Underline`].
    ///
    /// This includes Kitty's styled underline extension when terminals support it.
    Underline(Underline),

    /// Set blink behavior described by [`Blink`].
    Blink(Blink),

    /// Enable SGR 3 italic text or disable it with SGR 23.
    Italic(bool),

    /// Enable SGR 7 reverse video or disable it with SGR 27.
    Reverse(bool),

    /// Enable SGR 8 invisible text or disable it with SGR 28.
    Invisible(bool),

    /// Enable SGR 9 strikethrough text or disable it with SGR 29.
    StrikeThrough(bool),

    /// Enable SGR 53 overline text or disable it with SGR 55.
    Overline(bool),

    /// Select the active font described by [`Font`].
    Font(Font),

    /// Set vertical alignment described by [`VerticalAlign`].
    VerticalAlign(VerticalAlign),

    /// Set the foreground color.
    Foreground(Color),

    /// Set the background color.
    Background(Color),

    /// Set the underline color.
    ///
    /// This uses the SGR 58 underline-color extension.
    UnderlineColor(Color),
}

pub struct SgrIter<'a> {
    pub inner: std::slice::Iter<'a, SmallVec<[u8; 8]>>,
}

impl<'a> SgrIter<'a> {
    pub fn new(buf: &'a [SmallVec<[u8; 8]>]) -> Self {
        Self { inner: buf.iter() }
    }
}

impl<'a> Iterator for SgrIter<'a> {
    type Item = Sgr;

    fn next(&mut self) -> Option<Self::Item> {
        let buf = self.inner.next()?;
        let sgr = match buf.as_slice() {
            b"0" => Sgr::Reset,
            b"22" => Sgr::Intensity(Intensity::Normal),
            b"1" => Sgr::Intensity(Intensity::Bold),
            b"2" => Sgr::Intensity(Intensity::Dim),
            b"24" => Sgr::Underline(Underline::None),
            b"4" => Sgr::Underline(Underline::Single),
            b"21" => Sgr::Underline(Underline::Double),
            b"4:3" => Sgr::Underline(Underline::Curly),
            b"4:4" => Sgr::Underline(Underline::Dotted),
            b"4:5" => Sgr::Underline(Underline::Dashed),
            b"25" => Sgr::Blink(Blink::None),
            b"5" => Sgr::Blink(Blink::Slow),
            b"6" => Sgr::Blink(Blink::Rapid),
            b"3" => Sgr::Italic(true),
            b"23" => Sgr::Italic(false),
            b"7" => Sgr::Reverse(true),
            b"27" => Sgr::Reverse(false),
            b"8" => Sgr::Invisible(true),
            b"28" => Sgr::Invisible(false),
            b"9" => Sgr::StrikeThrough(true),
            b"29" => Sgr::StrikeThrough(false),
            b"53" => Sgr::Overline(true),
            b"55" => Sgr::Overline(false),
            b"10" => Sgr::Font(Font::Default),
            b"11" => Sgr::Font(Font::Alternate(1)),
            b"12" => Sgr::Font(Font::Alternate(2)),
            b"13" => Sgr::Font(Font::Alternate(3)),
            b"14" => Sgr::Font(Font::Alternate(4)),
            b"15" => Sgr::Font(Font::Alternate(5)),
            b"16" => Sgr::Font(Font::Alternate(6)),
            b"17" => Sgr::Font(Font::Alternate(7)),
            b"18" => Sgr::Font(Font::Alternate(8)),
            b"19" => Sgr::Font(Font::Alternate(9)),
            b"75" => Sgr::VerticalAlign(VerticalAlign::BaseLine),
            b"73" => Sgr::VerticalAlign(VerticalAlign::SuperScript),
            b"74" => Sgr::VerticalAlign(VerticalAlign::SubScript),
            b"39" => Sgr::Foreground(Color::Default),
            b"30" => Sgr::Foreground(Color::Black),
            b"31" => Sgr::Foreground(Color::Red),
            b"32" => Sgr::Foreground(Color::Green),
            b"33" => Sgr::Foreground(Color::Yellow),
            b"34" => Sgr::Foreground(Color::Blue),
            b"35" => Sgr::Foreground(Color::Magenta),
            b"36" => Sgr::Foreground(Color::Cyan),
            b"37" => Sgr::Foreground(Color::White),
            b"90" => Sgr::Foreground(Color::BRIGHT_BLACK),
            b"91" => Sgr::Foreground(Color::BRIGHT_RED),
            b"92" => Sgr::Foreground(Color::BRIGHT_GREEN),
            b"93" => Sgr::Foreground(Color::BRIGHT_YELLOW),
            b"94" => Sgr::Foreground(Color::BRIGHT_BLUE),
            b"95" => Sgr::Foreground(Color::BRIGHT_MAGENTA),
            b"96" => Sgr::Foreground(Color::BRIGHT_CYAN),
            b"97" => Sgr::Foreground(Color::BRIGHT_WHITE),
            b"49" => Sgr::Background(Color::Default),
            b"40" => Sgr::Background(Color::Black),
            b"41" => Sgr::Background(Color::Red),
            b"42" => Sgr::Background(Color::Green),
            b"43" => Sgr::Background(Color::Yellow),
            b"44" => Sgr::Background(Color::Blue),
            b"45" => Sgr::Background(Color::Magenta),
            b"46" => Sgr::Background(Color::Cyan),
            b"47" => Sgr::Background(Color::White),
            b"100" => Sgr::Background(Color::BRIGHT_BLACK),
            b"101" => Sgr::Background(Color::BRIGHT_RED),
            b"102" => Sgr::Background(Color::BRIGHT_GREEN),
            b"103" => Sgr::Background(Color::BRIGHT_YELLOW),
            b"104" => Sgr::Background(Color::BRIGHT_BLUE),
            b"105" => Sgr::Background(Color::BRIGHT_MAGENTA),
            b"106" => Sgr::Background(Color::BRIGHT_CYAN),
            b"107" => Sgr::Background(Color::BRIGHT_WHITE),
            b"59" => Sgr::UnderlineColor(Color::Default),
            k @ b"38" | k @ b"48" | k @ b"58" => {
                macro_rules! next_u8 {
                    () => {{
                        let buf = self.inner.next()?;
                        std::str::from_utf8(&buf).ok()?.parse::<u8>().ok()?
                    }};
                }
                let color = match self.inner.next()?.as_slice() {
                    b"2" => {
                        let r = next_u8!();
                        let g = next_u8!();
                        let b = next_u8!();
                        Color::Rgb(r, g, b)
                    }
                    b"5" => Color::Byte(next_u8!()),
                    b"6" => {
                        let r = next_u8!();
                        let g = next_u8!();
                        let b = next_u8!();
                        let _alpha = next_u8!();
                        Color::Rgb(r, g, b)
                    }
                    _ => {
                        return self.next();
                    }
                };
                match k {
                    b"38" => Sgr::Foreground(color),
                    b"48" => Sgr::Background(color),
                    b"58" => Sgr::UnderlineColor(color),
                    _ => {
                        return self.next();
                    }
                }
            }
            _ => {
                return self.next();
            }
        };
        Some(sgr)
    }
}
