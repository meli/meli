/*
 * meli
 *
 * Copyright 2018 Manos Pitsidianakis
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

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use termion::color::{AnsiValue, Rgb as TermionRgb};

/// The color of a `Cell`.
///
/// `Color::Default` represents the default color of the underlying terminal.
///
/// The eight basic colors may be used directly and correspond to 0x00..0x07 in
/// the 8-bit (256) color range; in addition, the eight basic colors coupled
/// with `Attr::BOLD` correspond to 0x08..0x0f in the 8-bit color range.
///
/// `Color::Byte(..)` may be used to specify a color in the 8-bit range.
///
/// # Examples
///
/// ```no_run
/// use meli::Color;
///
/// // The default color.
/// let default = Color::Default;
///
/// // A basic color.
/// let red = Color::Red;
///
/// // An 8-bit color.
/// let fancy = Color::Byte(0x01);
///
/// // Basic colors are also 8-bit colors (but not vice-versa).
/// assert_eq!(red.as_byte(), fancy.as_byte())
/// ```
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Byte(u8),
    Rgb(u8, u8, u8),
    /// Terminal default.
    #[default]
    Default,
}

impl Color {
    /// Returns the `u8` representation of the `Color`.
    pub fn as_byte(self) -> Option<u8> {
        Some(match self {
            Self::Black => 0x00,
            Self::Red => 0x01,
            Self::Green => 0x02,
            Self::Yellow => 0x03,
            Self::Blue => 0x04,
            Self::Magenta => 0x05,
            Self::Cyan => 0x06,
            Self::White => 0x07,
            Self::Byte(b) => b,
            Self::Rgb(_, _, _) => return None,
            Self::Default => 0x00,
        })
    }

    pub fn from_byte(val: u8) -> Self {
        match val {
            0x00 => Self::Black,
            0x01 => Self::Red,
            0x02 => Self::Green,
            0x03 => Self::Yellow,
            0x04 => Self::Blue,
            0x05 => Self::Magenta,
            0x06 => Self::Cyan,
            0x07 => Self::White,
            _ => Self::Default,
        }
    }

    pub fn write_fg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Self::Default => write!(stdout, "{}", termion::color::Fg(termion::color::Reset)),
            Self::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Fg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Fg(self.as_termion())),
        }
    }

    pub fn write_bg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Self::Default => write!(stdout, "{}", termion::color::Bg(termion::color::Reset)),
            Self::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Bg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Bg(self.as_termion())),
        }
    }

    pub fn as_termion(self) -> AnsiValue {
        match self {
            b @ Self::Black
            | b @ Self::Red
            | b @ Self::Green
            | b @ Self::Yellow
            | b @ Self::Blue
            | b @ Self::Magenta
            | b @ Self::Cyan
            | b @ Self::White
            | b @ Self::Default => AnsiValue(b.as_byte().unwrap_or_default()),
            Self::Byte(b) => AnsiValue(b),
            Self::Rgb(_, _, _) => AnsiValue(0),
        }
    }

    pub fn from_string_de<'de, D>(s: String) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let byte = match s.as_str() {
            "Aqua" => 14,
            "Aquamarine1" => 122,
            "Aquamarine2" => 86,
            "Aquamarine3" => 79,
            "Black" => 0,
            "Blue" => 12,
            "Blue1" => 21,
            "Blue2" => 19,
            "Blue3" => 20,
            "BlueViolet" => 57,
            "CadetBlue" => 72,
            "CadetBlue1" => 73,
            "Chartreuse1" => 118,
            "Chartreuse2" => 112,
            "Chartreuse3" => 82,
            "Chartreuse4" => 70,
            "Chartreuse5" => 76,
            "Chartreuse6" => 64,
            "CornflowerBlue" => 69,
            "Cornsilk1" => 230,
            "Cyan1" => 51,
            "Cyan2" => 50,
            "Cyan3" => 43,
            "DarkBlue" => 18,
            "DarkCyan" => 36,
            "DarkGoldenrod" => 136,
            "DarkGreen" => 22,
            "DarkKhaki" => 143,
            "DarkMagenta" => 90,
            "DarkMagenta1" => 91,
            "DarkOliveGreen1" => 192,
            "DarkOliveGreen2" => 155,
            "DarkOliveGreen3" => 191,
            "DarkOliveGreen4" => 107,
            "DarkOliveGreen5" => 113,
            "DarkOliveGreen6" => 149,
            "DarkOrange" => 208,
            "DarkOrange2" => 130,
            "DarkOrange3" => 166,
            "DarkRed" => 52,
            "DarkRed2" => 88,
            "DarkSeaGreen" => 108,
            "DarkSeaGreen1" => 158,
            "DarkSeaGreen2" => 193,
            "DarkSeaGreen3" => 151,
            "DarkSeaGreen4" => 157,
            "DarkSeaGreen5" => 115,
            "DarkSeaGreen6" => 150,
            "DarkSeaGreen7" => 65,
            "DarkSeaGreen8" => 71,
            "DarkSlateGray1" => 123,
            "DarkSlateGray2" => 87,
            "DarkSlateGray3" => 116,
            "DarkTurquoise" => 44,
            "DarkViolet" => 128,
            "DarkViolet1" => 92,
            "DeepPink1" => 199,
            "DeepPink2" => 197,
            "DeepPink3" => 198,
            "DeepPink4" => 125,
            "DeepPink6" => 162,
            "DeepPink7" => 89,
            "DeepPink8" => 53,
            "DeepPink9" => 161,
            "DeepSkyBlue1" => 39,
            "DeepSkyBlue2" => 38,
            "DeepSkyBlue3" => 31,
            "DeepSkyBlue4" => 32,
            "DeepSkyBlue5" => 23,
            "DeepSkyBlue6" => 24,
            "DeepSkyBlue7" => 25,
            "DodgerBlue1" => 33,
            "DodgerBlue2" => 27,
            "DodgerBlue3" => 26,
            "Fuchsia" => 13,
            "Gold1" => 220,
            "Gold2" => 142,
            "Gold3" => 178,
            "Green" => 2,
            "Green1" => 46,
            "Green2" => 34,
            "Green3" => 40,
            "Green4" => 28,
            "GreenYellow" => 154,
            "Grey" => 8,
            "Grey0" => 16,
            "Grey100" => 231,
            "Grey11" => 234,
            "Grey15" => 235,
            "Grey19" => 236,
            "Grey23" => 237,
            "Grey27" => 238,
            "Grey3" => 232,
            "Grey30" => 239,
            "Grey35" => 240,
            "Grey37" => 59,
            "Grey39" => 241,
            "Grey42" => 242,
            "Grey46" => 243,
            "Grey50" => 244,
            "Grey53" => 102,
            "Grey54" => 245,
            "Grey58" => 246,
            "Grey62" => 247,
            "Grey63" => 139,
            "Grey66" => 248,
            "Grey69" => 145,
            "Grey7" => 233,
            "Grey70" => 249,
            "Grey74" => 250,
            "Grey78" => 251,
            "Grey82" => 252,
            "Grey84" => 188,
            "Grey85" => 253,
            "Grey89" => 254,
            "Grey93" => 255,
            "Honeydew2" => 194,
            "HotPink" => 205,
            "HotPink1" => 206,
            "HotPink2" => 169,
            "HotPink3" => 132,
            "HotPink4" => 168,
            "IndianRed" => 131,
            "IndianRed1" => 167,
            "IndianRed2" => 204,
            "IndianRed3" => 203,
            "Khaki1" => 228,
            "Khaki3" => 185,
            "LightCoral" => 210,
            "LightCyan2" => 195,
            "LightCyan3" => 152,
            "LightGoldenrod1" => 227,
            "LightGoldenrod2" => 222,
            "LightGoldenrod3" => 179,
            "LightGoldenrod4" => 221,
            "LightGoldenrod5" => 186,
            "LightGreen" => 119,
            "LightGreen1" => 120,
            "LightPink1" => 217,
            "LightPink2" => 174,
            "LightPink3" => 95,
            "LightSalmon1" => 216,
            "LightSalmon2" => 137,
            "LightSalmon3" => 173,
            "LightSeaGreen" => 37,
            "LightSkyBlue1" => 153,
            "LightSkyBlue2" => 109,
            "LightSkyBlue3" => 110,
            "LightSlateBlue" => 105,
            "LightSlateGrey" => 103,
            "LightSteelBlue" => 147,
            "LightSteelBlue1" => 189,
            "LightSteelBlue3" => 146,
            "LightYellow3" => 187,
            "Lime" => 10,
            "Magenta1" => 201,
            "Magenta2" => 165,
            "Magenta3" => 200,
            "Magenta4" => 127,
            "Magenta5" => 163,
            "Magenta6" => 164,
            "Maroon" => 1,
            "MediumOrchid" => 134,
            "MediumOrchid1" => 171,
            "MediumOrchid2" => 207,
            "MediumOrchid3" => 133,
            "MediumPurple" => 104,
            "MediumPurple1" => 141,
            "MediumPurple2" => 135,
            "MediumPurple3" => 140,
            "MediumPurple4" => 97,
            "MediumPurple5" => 98,
            "MediumPurple6" => 60,
            "MediumSpringGreen" => 49,
            "MediumTurquoise" => 80,
            "MediumVioletRed" => 126,
            "MistyRose1" => 224,
            "MistyRose3" => 181,
            "NavajoWhite1" => 223,
            "NavajoWhite3" => 144,
            "Navy" => 4,
            "NavyBlue" => 17,
            "Olive" => 3,
            "Orange1" => 214,
            "Orange2" => 172,
            "Orange3" => 58,
            "Orange4" => 94,
            "OrangeRed1" => 202,
            "Orchid" => 170,
            "Orchid1" => 213,
            "Orchid2" => 212,
            "PaleGreen1" => 121,
            "PaleGreen2" => 156,
            "PaleGreen3" => 114,
            "PaleGreen4" => 77,
            "PaleTurquoise1" => 159,
            "PaleTurquoise4" => 66,
            "PaleVioletRed1" => 211,
            "Pink1" => 218,
            "Pink3" => 175,
            "Plum1" => 219,
            "Plum2" => 183,
            "Plum3" => 176,
            "Plum4" => 96,
            "Purple" => 129,
            "Purple1" => 5,
            "Purple2" => 93,
            "Purple3" => 56,
            "Purple4" => 54,
            "Purple5" => 55,
            "Red" => 9,
            "Red1" => 196,
            "Red2" => 124,
            "Red3" => 160,
            "RosyBrown" => 138,
            "RoyalBlue1" => 63,
            "Salmon1" => 209,
            "SandyBrown" => 215,
            "SeaGreen1" => 84,
            "SeaGreen2" => 85,
            "SeaGreen3" => 83,
            "SeaGreen4" => 78,
            "Silver" => 7,
            "SkyBlue1" => 117,
            "SkyBlue2" => 111,
            "SkyBlue3" => 74,
            "SlateBlue1" => 99,
            "SlateBlue2" => 61,
            "SlateBlue3" => 62,
            "SpringGreen1" => 48,
            "SpringGreen2" => 42,
            "SpringGreen3" => 47,
            "SpringGreen4" => 35,
            "SpringGreen5" => 41,
            "SpringGreen6" => 29,
            "SteelBlue" => 67,
            "SteelBlue1" => 75,
            "SteelBlue2" => 81,
            "SteelBlue3" => 68,
            "Tan" => 180,
            "Teal" => 6,
            "Thistle1" => 225,
            "Thistle3" => 182,
            "Turquoise2" => 45,
            "Turquoise4" => 30,
            "Violet" => 177,
            "Wheat1" => 229,
            "Wheat4" => 101,
            "White" => 15,
            "Yellow" => 11,
            "Yellow1" => 226,
            "Yellow2" => 190,
            "Yellow3" => 184,
            "Yellow4" => 100,
            "Yellow5" => 106,
            "Yellow6" => 148,
            "Default" => return Ok(Self::Default),
            s if s.starts_with('#')
                && s.len() == 7
                && s.as_bytes()[1..].iter().all(|&b| {
                    b.is_ascii_digit() || (b'a'..=b'f').contains(&b) || (b'A'..=b'F').contains(&b)
                }) =>
            {
                return Ok(Self::Rgb(
                    u8::from_str_radix(&s[1..3], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    u8::from_str_radix(&s[3..5], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    u8::from_str_radix(&s[5..7], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                ))
            }
            s if s.starts_with('#')
                && s.len() == 4
                && s.as_bytes()[1..].iter().all(|&b| {
                    b.is_ascii_digit() || (b'a'..=b'f').contains(&b) || (b'A'..=b'F').contains(&b)
                }) =>
            {
                return Ok(Self::Rgb(
                    17 * u8::from_str_radix(&s[1..2], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    17 * u8::from_str_radix(&s[2..3], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    17 * u8::from_str_radix(&s[3..4], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                ))
            }
            _ => s
                .parse::<u8>()
                .map_err(|_| de::Error::custom("invalid `color` value"))?,
        };
        Ok(Self::Byte(byte))
    }
}

impl<'de> Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            Self::from_string_de::<'de, D>(s)
        } else {
            Err(de::Error::custom("invalid `color` value"))
        }
    }
}

#[test]
fn test_color_de() {
    #[derive(Debug, Deserialize, Eq, PartialEq)]
    struct V {
        k: Color,
    }

    macro_rules! test_color {
        ($s:literal, ok $v:expr) => {
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\"")),
                Ok(V { k: $v })
            );
        };
        ($s:literal, err $v:literal) => {
            test_color!($s, err $v, "")
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
                    "^^^^^^\n",
                    $v,
                    '\n',
                )
                .to_string()
            );
        };
    }
    test_color!("#Ff6600", ok Color::Rgb(255, 102, 0));
    test_color!("#2E3440", ok Color::Rgb(46, 52, 64));
    test_color!("#f60", ok Color::Rgb(255, 102, 0));
    test_color!("#gb0", err "invalid `color` value");
    test_color!("Olive", ok Color::Byte(3));
    test_color!("Oafahifdave", err "invalid `color` value", "^^^^^^^");
}

impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Black | Self::Byte(0) => serializer.serialize_str("Black"),
            Self::Byte(1) => serializer.serialize_str("Maroon"),
            Self::Green | Self::Byte(2) => serializer.serialize_str("Green"),
            Self::Byte(3) => serializer.serialize_str("Olive"),
            Self::Byte(4) => serializer.serialize_str("Navy"),
            Self::Byte(5) | Self::Magenta => serializer.serialize_str("Purple"),
            Self::Byte(6) | Self::Cyan => serializer.serialize_str("Teal"),
            Self::Byte(7) => serializer.serialize_str("Silver"),
            Self::Byte(8) => serializer.serialize_str("Grey"),
            Self::Red | Self::Byte(9) => serializer.serialize_str("Red"),
            Self::Byte(10) => serializer.serialize_str("Lime"),
            Self::Yellow | Self::Byte(11) => serializer.serialize_str("Yellow"),
            Self::Blue | Self::Byte(12) => serializer.serialize_str("Blue"),
            Self::Byte(13) => serializer.serialize_str("Fuchsia"),
            Self::Byte(14) => serializer.serialize_str("Aqua"),
            Self::White | Self::Byte(15) => serializer.serialize_str("White"),
            Self::Byte(16) => serializer.serialize_str("Grey0"),
            Self::Byte(17) => serializer.serialize_str("NavyBlue"),
            Self::Byte(18) => serializer.serialize_str("DarkBlue"),
            Self::Byte(19) => serializer.serialize_str("Blue3"),
            Self::Byte(20) => serializer.serialize_str("Blue3"),
            Self::Byte(21) => serializer.serialize_str("Blue1"),
            Self::Byte(22) => serializer.serialize_str("DarkGreen"),
            Self::Byte(23) => serializer.serialize_str("DeepSkyBlue4"),
            Self::Byte(24) => serializer.serialize_str("DeepSkyBlue4"),
            Self::Byte(25) => serializer.serialize_str("DeepSkyBlue4"),
            Self::Byte(26) => serializer.serialize_str("DodgerBlue3"),
            Self::Byte(27) => serializer.serialize_str("DodgerBlue2"),
            Self::Byte(28) => serializer.serialize_str("Green4"),
            Self::Byte(29) => serializer.serialize_str("SpringGreen4"),
            Self::Byte(30) => serializer.serialize_str("Turquoise4"),
            Self::Byte(31) => serializer.serialize_str("DeepSkyBlue3"),
            Self::Byte(32) => serializer.serialize_str("DeepSkyBlue3"),
            Self::Byte(33) => serializer.serialize_str("DodgerBlue1"),
            Self::Byte(34) => serializer.serialize_str("Green3"),
            Self::Byte(35) => serializer.serialize_str("SpringGreen3"),
            Self::Byte(36) => serializer.serialize_str("DarkCyan"),
            Self::Byte(37) => serializer.serialize_str("LightSeaGreen"),
            Self::Byte(38) => serializer.serialize_str("DeepSkyBlue2"),
            Self::Byte(39) => serializer.serialize_str("DeepSkyBlue1"),
            Self::Byte(40) => serializer.serialize_str("Green3"),
            Self::Byte(41) => serializer.serialize_str("SpringGreen3"),
            Self::Byte(42) => serializer.serialize_str("SpringGreen2"),
            Self::Byte(43) => serializer.serialize_str("Cyan3"),
            Self::Byte(44) => serializer.serialize_str("DarkTurquoise"),
            Self::Byte(45) => serializer.serialize_str("Turquoise2"),
            Self::Byte(46) => serializer.serialize_str("Green1"),
            Self::Byte(47) => serializer.serialize_str("SpringGreen2"),
            Self::Byte(48) => serializer.serialize_str("SpringGreen1"),
            Self::Byte(49) => serializer.serialize_str("MediumSpringGreen"),
            Self::Byte(50) => serializer.serialize_str("Cyan2"),
            Self::Byte(51) => serializer.serialize_str("Cyan1"),
            Self::Byte(52) => serializer.serialize_str("DarkRed"),
            Self::Byte(53) => serializer.serialize_str("DeepPink4"),
            Self::Byte(54) => serializer.serialize_str("Purple4"),
            Self::Byte(55) => serializer.serialize_str("Purple4"),
            Self::Byte(56) => serializer.serialize_str("Purple3"),
            Self::Byte(57) => serializer.serialize_str("BlueViolet"),
            Self::Byte(58) => serializer.serialize_str("Orange4"),
            Self::Byte(59) => serializer.serialize_str("Grey37"),
            Self::Byte(60) => serializer.serialize_str("MediumPurple4"),
            Self::Byte(61) => serializer.serialize_str("SlateBlue3"),
            Self::Byte(62) => serializer.serialize_str("SlateBlue3"),
            Self::Byte(63) => serializer.serialize_str("RoyalBlue1"),
            Self::Byte(64) => serializer.serialize_str("Chartreuse4"),
            Self::Byte(65) => serializer.serialize_str("DarkSeaGreen4"),
            Self::Byte(66) => serializer.serialize_str("PaleTurquoise4"),
            Self::Byte(67) => serializer.serialize_str("SteelBlue"),
            Self::Byte(68) => serializer.serialize_str("SteelBlue3"),
            Self::Byte(69) => serializer.serialize_str("CornflowerBlue"),
            Self::Byte(70) => serializer.serialize_str("Chartreuse3"),
            Self::Byte(71) => serializer.serialize_str("DarkSeaGreen4"),
            Self::Byte(72) => serializer.serialize_str("CadetBlue"),
            Self::Byte(73) => serializer.serialize_str("CadetBlue"),
            Self::Byte(74) => serializer.serialize_str("SkyBlue3"),
            Self::Byte(75) => serializer.serialize_str("SteelBlue1"),
            Self::Byte(76) => serializer.serialize_str("Chartreuse3"),
            Self::Byte(77) => serializer.serialize_str("PaleGreen3"),
            Self::Byte(78) => serializer.serialize_str("SeaGreen3"),
            Self::Byte(79) => serializer.serialize_str("Aquamarine3"),
            Self::Byte(80) => serializer.serialize_str("MediumTurquoise"),
            Self::Byte(81) => serializer.serialize_str("SteelBlue1"),
            Self::Byte(82) => serializer.serialize_str("Chartreuse2"),
            Self::Byte(83) => serializer.serialize_str("SeaGreen2"),
            Self::Byte(84) => serializer.serialize_str("SeaGreen1"),
            Self::Byte(85) => serializer.serialize_str("SeaGreen1"),
            Self::Byte(86) => serializer.serialize_str("Aquamarine1"),
            Self::Byte(87) => serializer.serialize_str("DarkSlateGray2"),
            Self::Byte(88) => serializer.serialize_str("DarkRed"),
            Self::Byte(89) => serializer.serialize_str("DeepPink4"),
            Self::Byte(90) => serializer.serialize_str("DarkMagenta"),
            Self::Byte(91) => serializer.serialize_str("DarkMagenta"),
            Self::Byte(92) => serializer.serialize_str("DarkViolet"),
            Self::Byte(93) => serializer.serialize_str("Purple"),
            Self::Byte(94) => serializer.serialize_str("Orange4"),
            Self::Byte(95) => serializer.serialize_str("LightPink4"),
            Self::Byte(96) => serializer.serialize_str("Plum4"),
            Self::Byte(97) => serializer.serialize_str("MediumPurple3"),
            Self::Byte(98) => serializer.serialize_str("MediumPurple3"),
            Self::Byte(99) => serializer.serialize_str("SlateBlue1"),
            Self::Byte(100) => serializer.serialize_str("Yellow4"),
            Self::Byte(101) => serializer.serialize_str("Wheat4"),
            Self::Byte(102) => serializer.serialize_str("Grey53"),
            Self::Byte(103) => serializer.serialize_str("LightSlateGrey"),
            Self::Byte(104) => serializer.serialize_str("MediumPurple"),
            Self::Byte(105) => serializer.serialize_str("LightSlateBlue"),
            Self::Byte(106) => serializer.serialize_str("Yellow4"),
            Self::Byte(107) => serializer.serialize_str("DarkOliveGreen3"),
            Self::Byte(108) => serializer.serialize_str("DarkSeaGreen"),
            Self::Byte(109) => serializer.serialize_str("LightSkyBlue3"),
            Self::Byte(110) => serializer.serialize_str("LightSkyBlue3"),
            Self::Byte(111) => serializer.serialize_str("SkyBlue2"),
            Self::Byte(112) => serializer.serialize_str("Chartreuse2"),
            Self::Byte(113) => serializer.serialize_str("DarkOliveGreen3"),
            Self::Byte(114) => serializer.serialize_str("PaleGreen3"),
            Self::Byte(115) => serializer.serialize_str("DarkSeaGreen3"),
            Self::Byte(116) => serializer.serialize_str("DarkSlateGray3"),
            Self::Byte(117) => serializer.serialize_str("SkyBlue1"),
            Self::Byte(118) => serializer.serialize_str("Chartreuse1"),
            Self::Byte(119) => serializer.serialize_str("LightGreen"),
            Self::Byte(120) => serializer.serialize_str("LightGreen"),
            Self::Byte(121) => serializer.serialize_str("PaleGreen1"),
            Self::Byte(122) => serializer.serialize_str("Aquamarine1"),
            Self::Byte(123) => serializer.serialize_str("DarkSlateGray1"),
            Self::Byte(124) => serializer.serialize_str("Red3"),
            Self::Byte(125) => serializer.serialize_str("DeepPink4"),
            Self::Byte(126) => serializer.serialize_str("MediumVioletRed"),
            Self::Byte(127) => serializer.serialize_str("Magenta3"),
            Self::Byte(128) => serializer.serialize_str("DarkViolet"),
            Self::Byte(129) => serializer.serialize_str("Purple"),
            Self::Byte(130) => serializer.serialize_str("DarkOrange3"),
            Self::Byte(131) => serializer.serialize_str("IndianRed"),
            Self::Byte(132) => serializer.serialize_str("HotPink3"),
            Self::Byte(133) => serializer.serialize_str("MediumOrchid3"),
            Self::Byte(134) => serializer.serialize_str("MediumOrchid"),
            Self::Byte(135) => serializer.serialize_str("MediumPurple2"),
            Self::Byte(136) => serializer.serialize_str("DarkGoldenrod"),
            Self::Byte(137) => serializer.serialize_str("LightSalmon3"),
            Self::Byte(138) => serializer.serialize_str("RosyBrown"),
            Self::Byte(139) => serializer.serialize_str("Grey63"),
            Self::Byte(140) => serializer.serialize_str("MediumPurple2"),
            Self::Byte(141) => serializer.serialize_str("MediumPurple1"),
            Self::Byte(142) => serializer.serialize_str("Gold3"),
            Self::Byte(143) => serializer.serialize_str("DarkKhaki"),
            Self::Byte(144) => serializer.serialize_str("NavajoWhite3"),
            Self::Byte(145) => serializer.serialize_str("Grey69"),
            Self::Byte(146) => serializer.serialize_str("LightSteelBlue3"),
            Self::Byte(147) => serializer.serialize_str("LightSteelBlue"),
            Self::Byte(148) => serializer.serialize_str("Yellow3"),
            Self::Byte(149) => serializer.serialize_str("DarkOliveGreen3"),
            Self::Byte(150) => serializer.serialize_str("DarkSeaGreen3"),
            Self::Byte(151) => serializer.serialize_str("DarkSeaGreen2"),
            Self::Byte(152) => serializer.serialize_str("LightCyan3"),
            Self::Byte(153) => serializer.serialize_str("LightSkyBlue1"),
            Self::Byte(154) => serializer.serialize_str("GreenYellow"),
            Self::Byte(155) => serializer.serialize_str("DarkOliveGreen2"),
            Self::Byte(156) => serializer.serialize_str("PaleGreen1"),
            Self::Byte(157) => serializer.serialize_str("DarkSeaGreen2"),
            Self::Byte(158) => serializer.serialize_str("DarkSeaGreen1"),
            Self::Byte(159) => serializer.serialize_str("PaleTurquoise1"),
            Self::Byte(160) => serializer.serialize_str("Red3"),
            Self::Byte(161) => serializer.serialize_str("DeepPink3"),
            Self::Byte(162) => serializer.serialize_str("DeepPink3"),
            Self::Byte(163) => serializer.serialize_str("Magenta3"),
            Self::Byte(164) => serializer.serialize_str("Magenta3"),
            Self::Byte(165) => serializer.serialize_str("Magenta2"),
            Self::Byte(166) => serializer.serialize_str("DarkOrange3"),
            Self::Byte(167) => serializer.serialize_str("IndianRed"),
            Self::Byte(168) => serializer.serialize_str("HotPink3"),
            Self::Byte(169) => serializer.serialize_str("HotPink2"),
            Self::Byte(170) => serializer.serialize_str("Orchid"),
            Self::Byte(171) => serializer.serialize_str("MediumOrchid1"),
            Self::Byte(172) => serializer.serialize_str("Orange3"),
            Self::Byte(173) => serializer.serialize_str("LightSalmon3"),
            Self::Byte(174) => serializer.serialize_str("LightPink3"),
            Self::Byte(175) => serializer.serialize_str("Pink3"),
            Self::Byte(176) => serializer.serialize_str("Plum3"),
            Self::Byte(177) => serializer.serialize_str("Violet"),
            Self::Byte(178) => serializer.serialize_str("Gold3"),
            Self::Byte(179) => serializer.serialize_str("LightGoldenrod3"),
            Self::Byte(180) => serializer.serialize_str("Tan"),
            Self::Byte(181) => serializer.serialize_str("MistyRose3"),
            Self::Byte(182) => serializer.serialize_str("Thistle3"),
            Self::Byte(183) => serializer.serialize_str("Plum2"),
            Self::Byte(184) => serializer.serialize_str("Yellow3"),
            Self::Byte(185) => serializer.serialize_str("Khaki3"),
            Self::Byte(186) => serializer.serialize_str("LightGoldenrod2"),
            Self::Byte(187) => serializer.serialize_str("LightYellow3"),
            Self::Byte(188) => serializer.serialize_str("Grey84"),
            Self::Byte(189) => serializer.serialize_str("LightSteelBlue1"),
            Self::Byte(190) => serializer.serialize_str("Yellow2"),
            Self::Byte(191) => serializer.serialize_str("DarkOliveGreen1"),
            Self::Byte(192) => serializer.serialize_str("DarkOliveGreen1"),
            Self::Byte(193) => serializer.serialize_str("DarkSeaGreen1"),
            Self::Byte(194) => serializer.serialize_str("Honeydew2"),
            Self::Byte(195) => serializer.serialize_str("LightCyan1"),
            Self::Byte(196) => serializer.serialize_str("Red1"),
            Self::Byte(197) => serializer.serialize_str("DeepPink2"),
            Self::Byte(198) => serializer.serialize_str("DeepPink1"),
            Self::Byte(199) => serializer.serialize_str("DeepPink1"),
            Self::Byte(200) => serializer.serialize_str("Magenta2"),
            Self::Byte(201) => serializer.serialize_str("Magenta1"),
            Self::Byte(202) => serializer.serialize_str("OrangeRed1"),
            Self::Byte(203) => serializer.serialize_str("IndianRed1"),
            Self::Byte(204) => serializer.serialize_str("IndianRed1"),
            Self::Byte(205) => serializer.serialize_str("HotPink"),
            Self::Byte(206) => serializer.serialize_str("HotPink"),
            Self::Byte(207) => serializer.serialize_str("MediumOrchid1"),
            Self::Byte(208) => serializer.serialize_str("DarkOrange"),
            Self::Byte(209) => serializer.serialize_str("Salmon1"),
            Self::Byte(210) => serializer.serialize_str("LightCoral"),
            Self::Byte(211) => serializer.serialize_str("PaleVioletRed1"),
            Self::Byte(212) => serializer.serialize_str("Orchid2"),
            Self::Byte(213) => serializer.serialize_str("Orchid1"),
            Self::Byte(214) => serializer.serialize_str("Orange1"),
            Self::Byte(215) => serializer.serialize_str("SandyBrown"),
            Self::Byte(216) => serializer.serialize_str("LightSalmon1"),
            Self::Byte(217) => serializer.serialize_str("LightPink1"),
            Self::Byte(218) => serializer.serialize_str("Pink1"),
            Self::Byte(219) => serializer.serialize_str("Plum1"),
            Self::Byte(220) => serializer.serialize_str("Gold1"),
            Self::Byte(221) => serializer.serialize_str("LightGoldenrod2"),
            Self::Byte(222) => serializer.serialize_str("LightGoldenrod2"),
            Self::Byte(223) => serializer.serialize_str("NavajoWhite1"),
            Self::Byte(224) => serializer.serialize_str("MistyRose1"),
            Self::Byte(225) => serializer.serialize_str("Thistle1"),
            Self::Byte(226) => serializer.serialize_str("Yellow1"),
            Self::Byte(227) => serializer.serialize_str("LightGoldenrod1"),
            Self::Byte(228) => serializer.serialize_str("Khaki1"),
            Self::Byte(229) => serializer.serialize_str("Wheat1"),
            Self::Byte(230) => serializer.serialize_str("Cornsilk1"),
            Self::Byte(231) => serializer.serialize_str("Grey100"),
            Self::Byte(232) => serializer.serialize_str("Grey3"),
            Self::Byte(233) => serializer.serialize_str("Grey7"),
            Self::Byte(234) => serializer.serialize_str("Grey11"),
            Self::Byte(235) => serializer.serialize_str("Grey15"),
            Self::Byte(236) => serializer.serialize_str("Grey19"),
            Self::Byte(237) => serializer.serialize_str("Grey23"),
            Self::Byte(238) => serializer.serialize_str("Grey27"),
            Self::Byte(239) => serializer.serialize_str("Grey30"),
            Self::Byte(240) => serializer.serialize_str("Grey35"),
            Self::Byte(241) => serializer.serialize_str("Grey39"),
            Self::Byte(242) => serializer.serialize_str("Grey42"),
            Self::Byte(243) => serializer.serialize_str("Grey46"),
            Self::Byte(244) => serializer.serialize_str("Grey50"),
            Self::Byte(245) => serializer.serialize_str("Grey54"),
            Self::Byte(246) => serializer.serialize_str("Grey58"),
            Self::Byte(247) => serializer.serialize_str("Grey62"),
            Self::Byte(248) => serializer.serialize_str("Grey66"),
            Self::Byte(249) => serializer.serialize_str("Grey70"),
            Self::Byte(250) => serializer.serialize_str("Grey74"),
            Self::Byte(251) => serializer.serialize_str("Grey78"),
            Self::Byte(252) => serializer.serialize_str("Grey82"),
            Self::Byte(253) => serializer.serialize_str("Grey85"),
            Self::Byte(254) => serializer.serialize_str("Grey89"),
            Self::Byte(255) => serializer.serialize_str("Grey93"),
            Self::Rgb(r, g, b) => {
                serializer.serialize_str(&format!("#{:02x}{:02x}{:02x}", r, g, b))
            }
            Self::Default => serializer.serialize_str("Default"),
        }
    }
}

pub mod aliases {
    use super::Color;

    impl Color {
        pub const BLACK: Self = Self::Black;
        pub const MAROON: Self = Self::Byte(1);
        pub const GREEN: Self = Self::Green;
        pub const OLIVE: Self = Self::Byte(3);
        pub const NAVY: Self = Self::Byte(4);
        pub const PURPLE: Self = Self::Magenta;
        pub const TEAL: Self = Self::Cyan;
        pub const SILVER: Self = Self::Byte(7);
        pub const GREY: Self = Self::Byte(8);
        pub const RED: Self = Self::Byte(9);
        pub const LIME: Self = Self::Byte(10);
        pub const YELLOW: Self = Self::Byte(11);
        pub const BLUE: Self = Self::Byte(12);
        pub const FUCHSIA: Self = Self::Byte(13);
        pub const AQUA: Self = Self::Byte(14);
        pub const WHITE: Self = Self::Byte(15);
        pub const GREY0: Self = Self::Byte(16);
        pub const NAVYBLUE: Self = Self::Byte(17);
        pub const DARKBLUE: Self = Self::Byte(18);
        pub const BLUE3: Self = Self::Byte(19);
        pub const BLUE3_: Self = Self::Byte(20);
        pub const BLUE1: Self = Self::Byte(21);
        pub const DARKGREEN: Self = Self::Byte(22);
        pub const DEEPSKYBLUE4: Self = Self::Byte(23);
        pub const DEEPSKYBLUE4_: Self = Self::Byte(24);
        pub const DEEPSKYBLUE4__: Self = Self::Byte(25);
        pub const DODGERBLUE3: Self = Self::Byte(26);
        pub const DODGERBLUE2: Self = Self::Byte(27);
        pub const GREEN4: Self = Self::Byte(28);
        pub const SPRINGGREEN4: Self = Self::Byte(29);
        pub const TURQUOISE4: Self = Self::Byte(30);
        pub const DEEPSKYBLUE3: Self = Self::Byte(31);
        pub const DEEPSKYBLUE3_: Self = Self::Byte(32);
        pub const DODGERBLUE1: Self = Self::Byte(33);
        pub const GREEN3: Self = Self::Byte(34);
        pub const SPRINGGREEN3: Self = Self::Byte(35);
        pub const DARKCYAN: Self = Self::Byte(36);
        pub const LIGHTSEAGREEN: Self = Self::Byte(37);
        pub const DEEPSKYBLUE2: Self = Self::Byte(38);
        pub const DEEPSKYBLUE1: Self = Self::Byte(39);
        pub const GREEN3_: Self = Self::Byte(40);
        pub const SPRINGGREEN3_: Self = Self::Byte(41);
        pub const SPRINGGREEN2: Self = Self::Byte(42);
        pub const CYAN3: Self = Self::Byte(43);
        pub const DARKTURQUOISE: Self = Self::Byte(44);
        pub const TURQUOISE2: Self = Self::Byte(45);
        pub const GREEN1: Self = Self::Byte(46);
        pub const SPRINGGREEN2_: Self = Self::Byte(47);
        pub const SPRINGGREEN1: Self = Self::Byte(48);
        pub const MEDIUMSPRINGGREEN: Self = Self::Byte(49);
        pub const CYAN2: Self = Self::Byte(50);
        pub const CYAN1: Self = Self::Byte(51);
        pub const DARKRED: Self = Self::Byte(52);
        pub const DEEPPINK4: Self = Self::Byte(53);
        pub const PURPLE4: Self = Self::Byte(54);
        pub const PURPLE4_: Self = Self::Byte(55);
        pub const PURPLE3: Self = Self::Byte(56);
        pub const BLUEVIOLET: Self = Self::Byte(57);
        pub const ORANGE4: Self = Self::Byte(58);
        pub const GREY37: Self = Self::Byte(59);
        pub const MEDIUMPURPLE4: Self = Self::Byte(60);
        pub const SLATEBLUE3: Self = Self::Byte(61);
        pub const SLATEBLUE3_: Self = Self::Byte(62);
        pub const ROYALBLUE1: Self = Self::Byte(63);
        pub const CHARTREUSE4: Self = Self::Byte(64);
        pub const DARKSEAGREEN4: Self = Self::Byte(65);
        pub const PALETURQUOISE4: Self = Self::Byte(66);
        pub const STEELBLUE: Self = Self::Byte(67);
        pub const STEELBLUE3: Self = Self::Byte(68);
        pub const CORNFLOWERBLUE: Self = Self::Byte(69);
        pub const CHARTREUSE3: Self = Self::Byte(70);
        pub const DARKSEAGREEN4_: Self = Self::Byte(71);
        pub const CADETBLUE: Self = Self::Byte(72);
        pub const CADETBLUE_: Self = Self::Byte(73);
        pub const SKYBLUE3: Self = Self::Byte(74);
        pub const STEELBLUE1: Self = Self::Byte(75);
        pub const CHARTREUSE3_: Self = Self::Byte(76);
        pub const PALEGREEN3: Self = Self::Byte(77);
        pub const SEAGREEN3: Self = Self::Byte(78);
        pub const AQUAMARINE3: Self = Self::Byte(79);
        pub const MEDIUMTURQUOISE: Self = Self::Byte(80);
        pub const STEELBLUE1_: Self = Self::Byte(81);
        pub const CHARTREUSE2: Self = Self::Byte(82);
        pub const SEAGREEN2: Self = Self::Byte(83);
        pub const SEAGREEN1: Self = Self::Byte(84);
        pub const SEAGREEN1_: Self = Self::Byte(85);
        pub const AQUAMARINE1: Self = Self::Byte(86);
        pub const DARKSLATEGRAY2: Self = Self::Byte(87);
        pub const DARKRED_: Self = Self::Byte(88);
        pub const DEEPPINK4_: Self = Self::Byte(89);
        pub const DARKMAGENTA: Self = Self::Byte(90);
        pub const DARKMAGENTA_: Self = Self::Byte(91);
        pub const DARKVIOLET: Self = Self::Byte(92);
        pub const PURPLE_: Self = Self::Byte(93);
        pub const ORANGE4_: Self = Self::Byte(94);
        pub const LIGHTPINK4: Self = Self::Byte(95);
        pub const PLUM4: Self = Self::Byte(96);
        pub const MEDIUMPURPLE3: Self = Self::Byte(97);
        pub const MEDIUMPURPLE3_: Self = Self::Byte(98);
        pub const SLATEBLUE1: Self = Self::Byte(99);
        pub const YELLOW4: Self = Self::Byte(100);
        pub const WHEAT4: Self = Self::Byte(101);
        pub const GREY53: Self = Self::Byte(102);
        pub const LIGHTSLATEGREY: Self = Self::Byte(103);
        pub const MEDIUMPURPLE: Self = Self::Byte(104);
        pub const LIGHTSLATEBLUE: Self = Self::Byte(105);
        pub const YELLOW4_: Self = Self::Byte(106);
        pub const DARKOLIVEGREEN3: Self = Self::Byte(107);
        pub const DARKSEAGREEN: Self = Self::Byte(108);
        pub const LIGHTSKYBLUE3: Self = Self::Byte(109);
        pub const LIGHTSKYBLUE3_: Self = Self::Byte(110);
        pub const SKYBLUE2: Self = Self::Byte(111);
        pub const CHARTREUSE2_: Self = Self::Byte(112);
        pub const DARKOLIVEGREEN3_: Self = Self::Byte(113);
        pub const PALEGREEN3_: Self = Self::Byte(114);
        pub const DARKSEAGREEN3: Self = Self::Byte(115);
        pub const DARKSLATEGRAY3: Self = Self::Byte(116);
        pub const SKYBLUE1: Self = Self::Byte(117);
        pub const CHARTREUSE1: Self = Self::Byte(118);
        pub const LIGHTGREEN: Self = Self::Byte(119);
        pub const LIGHTGREEN_: Self = Self::Byte(120);
        pub const PALEGREEN1: Self = Self::Byte(121);
        pub const AQUAMARINE1_: Self = Self::Byte(122);
        pub const DARKSLATEGRAY1: Self = Self::Byte(123);
        pub const RED3: Self = Self::Byte(124);
        pub const DEEPPINK4__: Self = Self::Byte(125);
        pub const MEDIUMVIOLETRED: Self = Self::Byte(126);
        pub const MAGENTA3: Self = Self::Byte(127);
        pub const DARKVIOLET_: Self = Self::Byte(128);
        pub const PURPLE__: Self = Self::Byte(129);
        pub const DARKORANGE3: Self = Self::Byte(130);
        pub const INDIANRED: Self = Self::Byte(131);
        pub const HOTPINK3: Self = Self::Byte(132);
        pub const MEDIUMORCHID3: Self = Self::Byte(133);
        pub const MEDIUMORCHID: Self = Self::Byte(134);
        pub const MEDIUMPURPLE2: Self = Self::Byte(135);
        pub const DARKGOLDENROD: Self = Self::Byte(136);
        pub const LIGHTSALMON3: Self = Self::Byte(137);
        pub const ROSYBROWN: Self = Self::Byte(138);
        pub const GREY63: Self = Self::Byte(139);
        pub const MEDIUMPURPLE2_: Self = Self::Byte(140);
        pub const MEDIUMPURPLE1: Self = Self::Byte(141);
        pub const GOLD3: Self = Self::Byte(142);
        pub const DARKKHAKI: Self = Self::Byte(143);
        pub const NAVAJOWHITE3: Self = Self::Byte(144);
        pub const GREY69: Self = Self::Byte(145);
        pub const LIGHTSTEELBLUE3: Self = Self::Byte(146);
        pub const LIGHTSTEELBLUE: Self = Self::Byte(147);
        pub const YELLOW3: Self = Self::Byte(148);
        pub const DARKOLIVEGREEN3__: Self = Self::Byte(149);
        pub const DARKSEAGREEN3_: Self = Self::Byte(150);
        pub const DARKSEAGREEN2: Self = Self::Byte(151);
        pub const LIGHTCYAN3: Self = Self::Byte(152);
        pub const LIGHTSKYBLUE1: Self = Self::Byte(153);
        pub const GREENYELLOW: Self = Self::Byte(154);
        pub const DARKOLIVEGREEN2: Self = Self::Byte(155);
        pub const PALEGREEN1_: Self = Self::Byte(156);
        pub const DARKSEAGREEN2_: Self = Self::Byte(157);
        pub const DARKSEAGREEN1: Self = Self::Byte(158);
        pub const PALETURQUOISE1: Self = Self::Byte(159);
        pub const RED3_: Self = Self::Byte(160);
        pub const DEEPPINK3: Self = Self::Byte(161);
        pub const DEEPPINK3_: Self = Self::Byte(162);
        pub const MAGENTA3_: Self = Self::Byte(163);
        pub const MAGENTA3__: Self = Self::Byte(164);
        pub const MAGENTA2: Self = Self::Byte(165);
        pub const DARKORANGE3_: Self = Self::Byte(166);
        pub const INDIANRED_: Self = Self::Byte(167);
        pub const HOTPINK3_: Self = Self::Byte(168);
        pub const HOTPINK2: Self = Self::Byte(169);
        pub const ORCHID: Self = Self::Byte(170);
        pub const MEDIUMORCHID1: Self = Self::Byte(171);
        pub const ORANGE3: Self = Self::Byte(172);
        pub const LIGHTSALMON3_: Self = Self::Byte(173);
        pub const LIGHTPINK3: Self = Self::Byte(174);
        pub const PINK3: Self = Self::Byte(175);
        pub const PLUM3: Self = Self::Byte(176);
        pub const VIOLET: Self = Self::Byte(177);
        pub const GOLD3_: Self = Self::Byte(178);
        pub const LIGHTGOLDENROD3: Self = Self::Byte(179);
        pub const TAN: Self = Self::Byte(180);
        pub const MISTYROSE3: Self = Self::Byte(181);
        pub const THISTLE3: Self = Self::Byte(182);
        pub const PLUM2: Self = Self::Byte(183);
        pub const YELLOW3_: Self = Self::Byte(184);
        pub const KHAKI3: Self = Self::Byte(185);
        pub const LIGHTGOLDENROD2: Self = Self::Byte(186);
        pub const LIGHTYELLOW3: Self = Self::Byte(187);
        pub const GREY84: Self = Self::Byte(188);
        pub const LIGHTSTEELBLUE1: Self = Self::Byte(189);
        pub const YELLOW2: Self = Self::Byte(190);
        pub const DARKOLIVEGREEN1: Self = Self::Byte(191);
        pub const DARKOLIVEGREEN1_: Self = Self::Byte(192);
        pub const DARKSEAGREEN1_: Self = Self::Byte(193);
        pub const HONEYDEW2: Self = Self::Byte(194);
        pub const LIGHTCYAN1: Self = Self::Byte(195);
        pub const RED1: Self = Self::Byte(196);
        pub const DEEPPINK2: Self = Self::Byte(197);
        pub const DEEPPINK1: Self = Self::Byte(198);
        pub const DEEPPINK1_: Self = Self::Byte(199);
        pub const MAGENTA2_: Self = Self::Byte(200);
        pub const MAGENTA1: Self = Self::Byte(201);
        pub const ORANGERED1: Self = Self::Byte(202);
        pub const INDIANRED1: Self = Self::Byte(203);
        pub const INDIANRED1_: Self = Self::Byte(204);
        pub const HOTPINK: Self = Self::Byte(205);
        pub const HOTPINK_: Self = Self::Byte(206);
        pub const MEDIUMORCHID1_: Self = Self::Byte(207);
        pub const DARKORANGE: Self = Self::Byte(208);
        pub const SALMON1: Self = Self::Byte(209);
        pub const LIGHTCORAL: Self = Self::Byte(210);
        pub const PALEVIOLETRED1: Self = Self::Byte(211);
        pub const ORCHID2: Self = Self::Byte(212);
        pub const ORCHID1: Self = Self::Byte(213);
        pub const ORANGE1: Self = Self::Byte(214);
        pub const SANDYBROWN: Self = Self::Byte(215);
        pub const LIGHTSALMON1: Self = Self::Byte(216);
        pub const LIGHTPINK1: Self = Self::Byte(217);
        pub const PINK1: Self = Self::Byte(218);
        pub const PLUM1: Self = Self::Byte(219);
        pub const GOLD1: Self = Self::Byte(220);
        pub const LIGHTGOLDENROD2_: Self = Self::Byte(221);
        pub const LIGHTGOLDENROD2__: Self = Self::Byte(222);
        pub const NAVAJOWHITE1: Self = Self::Byte(223);
        pub const MISTYROSE1: Self = Self::Byte(224);
        pub const THISTLE1: Self = Self::Byte(225);
        pub const YELLOW1: Self = Self::Byte(226);
        pub const LIGHTGOLDENROD1: Self = Self::Byte(227);
        pub const KHAKI1: Self = Self::Byte(228);
        pub const WHEAT1: Self = Self::Byte(229);
        pub const CORNSILK1: Self = Self::Byte(230);
        pub const GREY100: Self = Self::Byte(231);
        pub const GREY3: Self = Self::Byte(232);
        pub const GREY7: Self = Self::Byte(233);
        pub const GREY11: Self = Self::Byte(234);
        pub const GREY15: Self = Self::Byte(235);
        pub const GREY19: Self = Self::Byte(236);
        pub const GREY23: Self = Self::Byte(237);
        pub const GREY27: Self = Self::Byte(238);
        pub const GREY30: Self = Self::Byte(239);
        pub const GREY35: Self = Self::Byte(240);
        pub const GREY39: Self = Self::Byte(241);
        pub const GREY42: Self = Self::Byte(242);
        pub const GREY46: Self = Self::Byte(243);
        pub const GREY50: Self = Self::Byte(244);
        pub const GREY54: Self = Self::Byte(245);
        pub const GREY58: Self = Self::Byte(246);
        pub const GREY62: Self = Self::Byte(247);
        pub const GREY66: Self = Self::Byte(248);
        pub const GREY70: Self = Self::Byte(249);
        pub const GREY74: Self = Self::Byte(250);
        pub const GREY78: Self = Self::Byte(251);
        pub const GREY82: Self = Self::Byte(252);
        pub const GREY85: Self = Self::Byte(253);
        pub const GREY89: Self = Self::Byte(254);
        pub const GREY93: Self = Self::Byte(255);
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum ColorContrast {
    #[default]
    Dark,
    Light,
    Other,
}

impl Color {
    pub fn compute_scheme_contrast(fg: Self, bg: Self) -> ColorContrast {
        const PERCEPTUAL_MIDDLE_GRAY: u8 = 50;
        let fg = fg.perceived_lightness();
        let bg = bg.perceived_lightness();
        if bg < fg {
            ColorContrast::Dark
        } else if bg > fg || bg > PERCEPTUAL_MIDDLE_GRAY {
            ColorContrast::Light
        } else {
            ColorContrast::Dark
        }
    }

    /// The perceived lightness of the color
    /// as a value between `0` (black) and `100` (white)
    /// where `50` is the perceptual "middle grey".
    ///
    /// ```
    /// # use meli::terminal::Color;
    /// # let color = Color::default();
    /// let is_dark = color.perceived_lightness() <= 50;
    /// ```
    pub fn perceived_lightness(&self) -> u8 {
        luminance_to_perceived_lightness(luminance(self))
    }

    pub fn into_rgb(&self) -> (u8, u8, u8) {
        match self {
            Self::Black => (0, 0, 0),
            Self::Red => (255, 0, 0),
            Self::Green => (0, 255, 0),
            Self::Yellow => todo!(),
            Self::Blue => (0, 0, 255),
            Self::Magenta => todo!(),
            Self::Cyan => todo!(),
            Self::White => (255, 255, 255),
            Self::Byte(_b) => todo!(),
            Self::Rgb(r, g, b) => (*r, *g, *b),
            Self::Default => (0, 0, 0),
        }
    }
}

// Implementation of determining the perceived lightness
// follows this excellent answer: https://stackoverflow.com/a/56678483

fn srgb_to_lin(channel: f64) -> f64 {
    if channel < 0.04045 {
        channel / 12.92
    } else {
        ((channel + 0.055) / 1.055).powf(2.4)
    }
}

fn luminance(color: &Color) -> f64 {
    let (r, g, b) = color.into_rgb();
    let r = f64::from(r) / f64::from(u8::MAX);
    let g = f64::from(g) / f64::from(u8::MAX);
    let b = f64::from(b) / f64::from(u8::MAX);
    0.0722f64.mul_add(
        srgb_to_lin(b),
        0.2126f64.mul_add(srgb_to_lin(r), 0.7152 * srgb_to_lin(g)),
    )
}

// Perceptual lightness (L*)
fn luminance_to_perceived_lightness(luminance: f64) -> u8 {
    if luminance < 216. / 24389. {
        (luminance * (24389. / 27.)) as u8
    } else {
        luminance.cbrt().mul_add(116., -16.) as u8
    }
}
