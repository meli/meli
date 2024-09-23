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

/// `Display` utility to print text as a clickable hyperlink with the [`OSC8`]
/// format.
///
/// [`OSC8`]: <https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda>
///
/// # Examples
///
/// ```rust,no_run
/// # use std::path::Path;
/// # use meli::terminal::Hyperlink;
/// fn print_path(path: &Path) {
///     if let Some(hostname) = nix::unistd::gethostname()
///         .ok()
///         .and_then(|s| s.into_string().ok())
///     {
///         println!(
///             "{}",
///             Hyperlink::new(
///                 &path.display(),
///                 &format_args!("file://{hostname}{}", path.display())
///             )
///         );
///     } else {
///         println!("{}", path.display());
///     }
/// }
/// ```
#[derive(Clone, Copy)]
pub struct Hyperlink<
    'a,
    'b,
    'i,
    T: std::fmt::Display + ?Sized,
    U: std::fmt::Display + ?Sized,
    I: std::fmt::Display + ?Sized,
> {
    pub id: Option<&'i I>,
    pub text: &'a T,
    pub url: &'b U,
}

impl<
        'a,
        'b,
        'i,
        T: std::fmt::Display + ?Sized,
        U: std::fmt::Display + ?Sized,
        I: std::fmt::Display + ?Sized,
    > std::fmt::Display for Hyperlink<'a, 'b, 'i, T, U, I>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let id: &dyn std::fmt::Display = if let Some(ref id) = self.id { id } else { &"" };
        write!(
            f,
            "\x1b]8;{ideq}{id};{url}\x07{text}\x1b]8;;\x07",
            url = self.url,
            text = self.text,
            ideq = if self.id.is_some() { "id=" } else { "" },
            id = id
        )
    }
}

impl<'a, 'b, T: std::fmt::Display + ?Sized, U: std::fmt::Display + ?Sized>
    Hyperlink<'a, 'b, 'static, T, U, str>
{
    pub const fn new(text: &'a T, url: &'b U) -> Self {
        Self {
            id: None,
            text,
            url,
        }
    }
}

impl<
        'a,
        'b,
        'i,
        T: std::fmt::Display + ?Sized,
        U: std::fmt::Display + ?Sized,
        I: std::fmt::Display + ?Sized,
    > Hyperlink<'a, 'b, 'i, T, U, I>
{
    pub const fn with_id(id: &'i I, text: &'a T, url: &'b U) -> Self {
        Self {
            id: Some(id),
            text,
            url,
        }
    }
}

/// Create an OSC-introduced sequence.
macro_rules! osc {
    ($( $l:expr ),*) => { concat!("\x1b]", $( $l ),*) };
}

/// Derive an OSC sequence struct.
macro_rules! derive_osc_sequence {
    ($(#[$outer:meta])*
    ($name:ident, $value:expr)) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Eq, PartialEq, Debug)]
        pub struct $name;

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, osc!($value))
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &'static str {
                osc!($value)
            }
        }
    };
}

pub trait EscapeSequenceParse: Sized {
    fn parse(bytes: &str) -> Option<Self>;
}

pub trait EscapeSequenceQuery: Sized {
    type Reply: EscapeSequenceParse;
    const PREFIX: &'static str;

    fn parse(bytes: &str) -> Option<Self::Reply> {
        if !bytes.starts_with(Self::PREFIX) {
            return None;
        }
        <Self::Reply as EscapeSequenceParse>::parse(&bytes[Self::PREFIX.len()..])
    }
}

impl EscapeSequenceQuery for QueryBackground {
    type Reply = Color;
    const PREFIX: &'static str = "\x1b]11;";
}

impl EscapeSequenceQuery for QueryForeground {
    type Reply = Color;
    const PREFIX: &'static str = "\x1b]10;";
}

impl EscapeSequenceParse for Color {
    fn parse(mut bytes: &str) -> Option<Self> {
        // OSC Escape sequence.
        // Terminal background report:
        melib::log::trace!("osc10/11 background query: {:?}", bytes,);

        // meli::terminal::keys: EscapeSequence is [27, 93, 49, 49, 59, 114, 103, 98,
        // 58, 49, 99, 49, 99, 47, 49, 98, 49, 98, 47, 49, 57, 49, 57] ==
        // "\u{1b}]11;rgb:1c1c/1b1b/1919" meli::terminal: osc11 background
        // query: [114, 103, 98, 58, 49, 99, 49, 99, 47, 49, 98, 49, 98, 47, 49, 57, 49,
        // 57] == "rgb:1c1c/1b1b/1919" meli::terminal::keys: EscapeSequence
        // parsed None
        if bytes.starts_with("rgba:") {
            bytes = &bytes["rgba:".len()..];
            // Formats:
            // rgba:RRRR/GGGG/BBBB/AAAA
            if bytes.len() == "RRRR/GGGG/BBBB/AAAA".len() {
                return parse_rgba(bytes);
            }

            return None;
        } else if bytes.starts_with("rgb:") {
            bytes = &bytes["rgb:".len()..];
            // Formats:
            // rgb:R/G/B
            // rgb:RR/GG/BB
            // rgb:RRRR/GGGG/BBBB
            if bytes.len() == "R/G/B".len() {
                return parse_rgb(bytes);
            }
            if bytes.len() == "RR/GG/BB".len() {
                return parse_rgb(bytes);
            }
            if bytes.len() == "RRRR/GGGG/BBBB".len() {
                return parse_rgb(bytes);
            }

            return None;
        }
        None
    }
}

/// From the `xparsecolor` man page:
/// > An RGB Device specification is identified by the prefix `rgb:` and
/// > conforms to the following syntax:
/// > ```text
/// > rgb:<red>/<green>/<blue>
/// >
/// > <red>, <green>, <blue> := h | hh | hhh | hhhh
/// > h := single hexadecimal digits (case insignificant)
/// > ```
/// > Note that *h* indicates the value scaled in 4 bits,
/// > *hh* the value scaled in 8 bits, *hhh* the value scaled in 12 bits,
/// > and *hhhh* the value scaled in 16 bits, respectively.
fn parse_rgb(input: &str) -> Option<Color> {
    let mut parts = input.split('/');
    let r = parse_channel_scaled(parts.next()?)?;
    let g = parse_channel_scaled(parts.next()?)?;
    let b = parse_channel_scaled(parts.next()?)?;
    if parts.next().is_none() {
        Some(Color::Rgb(r as u8, g as u8, b as u8))
    } else {
        None
    }
}

/// Some terminals such as urxvt (rxvt-unicode) optionally support
/// an alpha channel and sometimes return colors in the format
/// `rgba:<red>/<green>/<blue>/<alpha>`.
///
/// Dropping the alpha channel is a best-effort thing as
/// the effective color (when combined with a background color)
/// could have a completely different perceived lightness value.
///
/// Test with `urxvt -depth 32 -fg grey90 -bg rgba:0000/0000/4444/cccc`
fn parse_rgba(input: &str) -> Option<Color> {
    let mut parts = input.split('/');
    let r = parse_channel_scaled(parts.next()?)?;
    let g = parse_channel_scaled(parts.next()?)?;
    let b = parse_channel_scaled(parts.next()?)?;
    let _a = parse_channel_scaled(parts.next()?)?;
    if parts.next().is_none() {
        Some(Color::Rgb(r as u8, g as u8, b as u8))
    } else {
        None
    }
}

fn parse_channel_scaled(input: &str) -> Option<u16> {
    let len = input.len();
    if (1..=4).contains(&len) {
        let max = u32::pow(16, len as u32) - 1;
        let value = u32::from_str_radix(input, 16).ok()?;
        Some((u16::MAX as u32 * value / max) as u16)
    } else {
        None
    }
}
// const ST: &[u8] = b"\x1b\\";
// const QUERY_FG: &[u8] = b"\x1b]10;?";
// const FG_RESPONSE_PREFIX: &[u8] = b"\x1b]10;";
// const QUERY_BG: &[u8] = b"\x1b]11;?";
// const BG_RESPONSE_PREFIX: &[u8] = b"\x1b]11;";
derive_osc_sequence!(
    /// Query background color.
    (QueryBackground, "11;?\x1b\\")
);

derive_osc_sequence!(
    /// Query foreground color.
    (QueryForeground, "10;?\x1b\\")
);

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

derive_csi_sequence!(
    #[doc = "Query Synchronized Output support. `CSI ? 2026 $ p`"]
    (QuerySynchronizedOutputSupport, "?2026$p")
);

pub struct Ask<'m> {
    message: Cow<'m, str>,
    default: Option<bool>,
}

impl<'m> Ask<'m> {
    pub fn new<M>(m: M) -> Self
    where
        M: Into<Cow<'m, str>>,
    {
        let message = m.into();
        Self {
            message,
            default: Some(true),
        }
    }

    pub fn yes_by_default(self, default: bool) -> Self {
        Self {
            default: Some(default),
            ..self
        }
    }

    pub fn without_default(self) -> Self {
        Self {
            default: None,
            ..self
        }
    }

    pub fn run(self) -> bool {
        let mut buffer = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();

        let default = match self.default {
            None => "y/n",
            Some(true) => "Y/n",
            Some(false) => "y/N",
        };

        print!("{} [{default}] ", self.message.as_ref());
        let _ = std::io::stdout().flush();
        loop {
            buffer.clear();
            handle
                .read_line(&mut buffer)
                .expect("Could not read from stdin.");

            match (buffer.trim(), self.default) {
                ("", Some(val)) => return val,
                ("Y" | "y" | "yes" | "YES" | "Yes", _) => {
                    return true;
                }
                ("n" | "N" | "no" | "No" | "NO", _) => {
                    return false;
                }
                _ => {
                    print!("\n{} [{default}] ", self.message.as_ref());
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
