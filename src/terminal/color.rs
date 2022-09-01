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
/// The eight basic colors may be used directly and correspond to 0x00..0x07 in the 8-bit (256)
/// color range; in addition, the eight basic colors coupled with `Attr::BOLD` correspond to
/// 0x08..0x0f in the 8-bit color range.
///
/// `Color::Byte(..)` may be used to specify a color in the 8-bit range.
///
/// # Examples
///
/// ```
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
#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
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
    Default,
}

impl Color {
    /// Returns the `u8` representation of the `Color`.
    pub fn as_byte(self) -> u8 {
        match self {
            Color::Black => 0x00,
            Color::Red => 0x01,
            Color::Green => 0x02,
            Color::Yellow => 0x03,
            Color::Blue => 0x04,
            Color::Magenta => 0x05,
            Color::Cyan => 0x06,
            Color::White => 0x07,
            Color::Byte(b) => b,
            Color::Rgb(_, _, _) => unreachable!(),
            Color::Default => 0x00,
        }
    }

    pub fn from_byte(val: u8) -> Self {
        match val {
            0x00 => Color::Black,
            0x01 => Color::Red,
            0x02 => Color::Green,
            0x03 => Color::Yellow,
            0x04 => Color::Blue,
            0x05 => Color::Magenta,
            0x06 => Color::Cyan,
            0x07 => Color::White,
            _ => Color::Default,
        }
    }

    pub fn write_fg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Color::Default => write!(stdout, "{}", termion::color::Fg(termion::color::Reset)),
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Fg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Fg(self.as_termion())),
        }
    }

    pub fn write_bg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Color::Default => write!(stdout, "{}", termion::color::Bg(termion::color::Reset)),
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Bg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Bg(self.as_termion())),
        }
    }

    pub fn as_termion(self) -> AnsiValue {
        match self {
            b @ Color::Black
            | b @ Color::Red
            | b @ Color::Green
            | b @ Color::Yellow
            | b @ Color::Blue
            | b @ Color::Magenta
            | b @ Color::Cyan
            | b @ Color::White
            | b @ Color::Default => AnsiValue(b.as_byte()),
            Color::Byte(b) => AnsiValue(b),
            Color::Rgb(_, _, _) => AnsiValue(0),
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
            "Default" => return Ok(Color::Default),
            s if s.starts_with('#')
                && s.len() == 7
                && s[1..].as_bytes().iter().all(|&b| {
                    (b'0'..=b'9').contains(&b)
                        || (b'a'..=b'f').contains(&b)
                        || (b'A'..=b'F').contains(&b)
                }) =>
            {
                return Ok(Color::Rgb(
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
                && s[1..].as_bytes().iter().all(|&b| {
                    (b'0'..=b'9').contains(&b)
                        || (b'a'..=b'f').contains(&b)
                        || (b'A'..=b'F').contains(&b)
                }) =>
            {
                return Ok(Color::Rgb(
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
        Ok(Color::Byte(byte))
    }
}

impl Default for Color {
    fn default() -> Self {
        Color::Default
    }
}

impl<'de> Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            Color::from_string_de::<'de, D>(s)
        } else {
            Err(de::Error::custom("invalid `color` value"))
        }
    }
}

#[test]
fn test_color_de() {
    #[derive(Debug, Deserialize, PartialEq)]
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
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\""))
                    .unwrap_err()
                    .to_string(),
                $v.to_string()
            );
        };
    }
    test_color!("#Ff6600", ok Color::Rgb(255, 102, 0));
    test_color!("#2E3440", ok Color::Rgb(46, 52, 64));
    test_color!("#f60", ok Color::Rgb(255, 102, 0));
    test_color!("#gb0", err "invalid `color` value for key `k` at line 1 column 1");
    test_color!("Olive", ok Color::Byte(3));
    test_color!("Oafahifdave", err "invalid `color` value for key `k` at line 1 column 1");
}

impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Color::Black | Color::Byte(0) => serializer.serialize_str("Black"),
            Color::Byte(1) => serializer.serialize_str("Maroon"),
            Color::Green | Color::Byte(2) => serializer.serialize_str("Green"),
            Color::Byte(3) => serializer.serialize_str("Olive"),
            Color::Byte(4) => serializer.serialize_str("Navy"),
            Color::Byte(5) | Color::Magenta => serializer.serialize_str("Purple"),
            Color::Byte(6) | Color::Cyan => serializer.serialize_str("Teal"),
            Color::Byte(7) => serializer.serialize_str("Silver"),
            Color::Byte(8) => serializer.serialize_str("Grey"),
            Color::Red | Color::Byte(9) => serializer.serialize_str("Red"),
            Color::Byte(10) => serializer.serialize_str("Lime"),
            Color::Yellow | Color::Byte(11) => serializer.serialize_str("Yellow"),
            Color::Blue | Color::Byte(12) => serializer.serialize_str("Blue"),
            Color::Byte(13) => serializer.serialize_str("Fuchsia"),
            Color::Byte(14) => serializer.serialize_str("Aqua"),
            Color::White | Color::Byte(15) => serializer.serialize_str("White"),
            Color::Byte(16) => serializer.serialize_str("Grey0"),
            Color::Byte(17) => serializer.serialize_str("NavyBlue"),
            Color::Byte(18) => serializer.serialize_str("DarkBlue"),
            Color::Byte(19) => serializer.serialize_str("Blue3"),
            Color::Byte(20) => serializer.serialize_str("Blue3"),
            Color::Byte(21) => serializer.serialize_str("Blue1"),
            Color::Byte(22) => serializer.serialize_str("DarkGreen"),
            Color::Byte(23) => serializer.serialize_str("DeepSkyBlue4"),
            Color::Byte(24) => serializer.serialize_str("DeepSkyBlue4"),
            Color::Byte(25) => serializer.serialize_str("DeepSkyBlue4"),
            Color::Byte(26) => serializer.serialize_str("DodgerBlue3"),
            Color::Byte(27) => serializer.serialize_str("DodgerBlue2"),
            Color::Byte(28) => serializer.serialize_str("Green4"),
            Color::Byte(29) => serializer.serialize_str("SpringGreen4"),
            Color::Byte(30) => serializer.serialize_str("Turquoise4"),
            Color::Byte(31) => serializer.serialize_str("DeepSkyBlue3"),
            Color::Byte(32) => serializer.serialize_str("DeepSkyBlue3"),
            Color::Byte(33) => serializer.serialize_str("DodgerBlue1"),
            Color::Byte(34) => serializer.serialize_str("Green3"),
            Color::Byte(35) => serializer.serialize_str("SpringGreen3"),
            Color::Byte(36) => serializer.serialize_str("DarkCyan"),
            Color::Byte(37) => serializer.serialize_str("LightSeaGreen"),
            Color::Byte(38) => serializer.serialize_str("DeepSkyBlue2"),
            Color::Byte(39) => serializer.serialize_str("DeepSkyBlue1"),
            Color::Byte(40) => serializer.serialize_str("Green3"),
            Color::Byte(41) => serializer.serialize_str("SpringGreen3"),
            Color::Byte(42) => serializer.serialize_str("SpringGreen2"),
            Color::Byte(43) => serializer.serialize_str("Cyan3"),
            Color::Byte(44) => serializer.serialize_str("DarkTurquoise"),
            Color::Byte(45) => serializer.serialize_str("Turquoise2"),
            Color::Byte(46) => serializer.serialize_str("Green1"),
            Color::Byte(47) => serializer.serialize_str("SpringGreen2"),
            Color::Byte(48) => serializer.serialize_str("SpringGreen1"),
            Color::Byte(49) => serializer.serialize_str("MediumSpringGreen"),
            Color::Byte(50) => serializer.serialize_str("Cyan2"),
            Color::Byte(51) => serializer.serialize_str("Cyan1"),
            Color::Byte(52) => serializer.serialize_str("DarkRed"),
            Color::Byte(53) => serializer.serialize_str("DeepPink4"),
            Color::Byte(54) => serializer.serialize_str("Purple4"),
            Color::Byte(55) => serializer.serialize_str("Purple4"),
            Color::Byte(56) => serializer.serialize_str("Purple3"),
            Color::Byte(57) => serializer.serialize_str("BlueViolet"),
            Color::Byte(58) => serializer.serialize_str("Orange4"),
            Color::Byte(59) => serializer.serialize_str("Grey37"),
            Color::Byte(60) => serializer.serialize_str("MediumPurple4"),
            Color::Byte(61) => serializer.serialize_str("SlateBlue3"),
            Color::Byte(62) => serializer.serialize_str("SlateBlue3"),
            Color::Byte(63) => serializer.serialize_str("RoyalBlue1"),
            Color::Byte(64) => serializer.serialize_str("Chartreuse4"),
            Color::Byte(65) => serializer.serialize_str("DarkSeaGreen4"),
            Color::Byte(66) => serializer.serialize_str("PaleTurquoise4"),
            Color::Byte(67) => serializer.serialize_str("SteelBlue"),
            Color::Byte(68) => serializer.serialize_str("SteelBlue3"),
            Color::Byte(69) => serializer.serialize_str("CornflowerBlue"),
            Color::Byte(70) => serializer.serialize_str("Chartreuse3"),
            Color::Byte(71) => serializer.serialize_str("DarkSeaGreen4"),
            Color::Byte(72) => serializer.serialize_str("CadetBlue"),
            Color::Byte(73) => serializer.serialize_str("CadetBlue"),
            Color::Byte(74) => serializer.serialize_str("SkyBlue3"),
            Color::Byte(75) => serializer.serialize_str("SteelBlue1"),
            Color::Byte(76) => serializer.serialize_str("Chartreuse3"),
            Color::Byte(77) => serializer.serialize_str("PaleGreen3"),
            Color::Byte(78) => serializer.serialize_str("SeaGreen3"),
            Color::Byte(79) => serializer.serialize_str("Aquamarine3"),
            Color::Byte(80) => serializer.serialize_str("MediumTurquoise"),
            Color::Byte(81) => serializer.serialize_str("SteelBlue1"),
            Color::Byte(82) => serializer.serialize_str("Chartreuse2"),
            Color::Byte(83) => serializer.serialize_str("SeaGreen2"),
            Color::Byte(84) => serializer.serialize_str("SeaGreen1"),
            Color::Byte(85) => serializer.serialize_str("SeaGreen1"),
            Color::Byte(86) => serializer.serialize_str("Aquamarine1"),
            Color::Byte(87) => serializer.serialize_str("DarkSlateGray2"),
            Color::Byte(88) => serializer.serialize_str("DarkRed"),
            Color::Byte(89) => serializer.serialize_str("DeepPink4"),
            Color::Byte(90) => serializer.serialize_str("DarkMagenta"),
            Color::Byte(91) => serializer.serialize_str("DarkMagenta"),
            Color::Byte(92) => serializer.serialize_str("DarkViolet"),
            Color::Byte(93) => serializer.serialize_str("Purple"),
            Color::Byte(94) => serializer.serialize_str("Orange4"),
            Color::Byte(95) => serializer.serialize_str("LightPink4"),
            Color::Byte(96) => serializer.serialize_str("Plum4"),
            Color::Byte(97) => serializer.serialize_str("MediumPurple3"),
            Color::Byte(98) => serializer.serialize_str("MediumPurple3"),
            Color::Byte(99) => serializer.serialize_str("SlateBlue1"),
            Color::Byte(100) => serializer.serialize_str("Yellow4"),
            Color::Byte(101) => serializer.serialize_str("Wheat4"),
            Color::Byte(102) => serializer.serialize_str("Grey53"),
            Color::Byte(103) => serializer.serialize_str("LightSlateGrey"),
            Color::Byte(104) => serializer.serialize_str("MediumPurple"),
            Color::Byte(105) => serializer.serialize_str("LightSlateBlue"),
            Color::Byte(106) => serializer.serialize_str("Yellow4"),
            Color::Byte(107) => serializer.serialize_str("DarkOliveGreen3"),
            Color::Byte(108) => serializer.serialize_str("DarkSeaGreen"),
            Color::Byte(109) => serializer.serialize_str("LightSkyBlue3"),
            Color::Byte(110) => serializer.serialize_str("LightSkyBlue3"),
            Color::Byte(111) => serializer.serialize_str("SkyBlue2"),
            Color::Byte(112) => serializer.serialize_str("Chartreuse2"),
            Color::Byte(113) => serializer.serialize_str("DarkOliveGreen3"),
            Color::Byte(114) => serializer.serialize_str("PaleGreen3"),
            Color::Byte(115) => serializer.serialize_str("DarkSeaGreen3"),
            Color::Byte(116) => serializer.serialize_str("DarkSlateGray3"),
            Color::Byte(117) => serializer.serialize_str("SkyBlue1"),
            Color::Byte(118) => serializer.serialize_str("Chartreuse1"),
            Color::Byte(119) => serializer.serialize_str("LightGreen"),
            Color::Byte(120) => serializer.serialize_str("LightGreen"),
            Color::Byte(121) => serializer.serialize_str("PaleGreen1"),
            Color::Byte(122) => serializer.serialize_str("Aquamarine1"),
            Color::Byte(123) => serializer.serialize_str("DarkSlateGray1"),
            Color::Byte(124) => serializer.serialize_str("Red3"),
            Color::Byte(125) => serializer.serialize_str("DeepPink4"),
            Color::Byte(126) => serializer.serialize_str("MediumVioletRed"),
            Color::Byte(127) => serializer.serialize_str("Magenta3"),
            Color::Byte(128) => serializer.serialize_str("DarkViolet"),
            Color::Byte(129) => serializer.serialize_str("Purple"),
            Color::Byte(130) => serializer.serialize_str("DarkOrange3"),
            Color::Byte(131) => serializer.serialize_str("IndianRed"),
            Color::Byte(132) => serializer.serialize_str("HotPink3"),
            Color::Byte(133) => serializer.serialize_str("MediumOrchid3"),
            Color::Byte(134) => serializer.serialize_str("MediumOrchid"),
            Color::Byte(135) => serializer.serialize_str("MediumPurple2"),
            Color::Byte(136) => serializer.serialize_str("DarkGoldenrod"),
            Color::Byte(137) => serializer.serialize_str("LightSalmon3"),
            Color::Byte(138) => serializer.serialize_str("RosyBrown"),
            Color::Byte(139) => serializer.serialize_str("Grey63"),
            Color::Byte(140) => serializer.serialize_str("MediumPurple2"),
            Color::Byte(141) => serializer.serialize_str("MediumPurple1"),
            Color::Byte(142) => serializer.serialize_str("Gold3"),
            Color::Byte(143) => serializer.serialize_str("DarkKhaki"),
            Color::Byte(144) => serializer.serialize_str("NavajoWhite3"),
            Color::Byte(145) => serializer.serialize_str("Grey69"),
            Color::Byte(146) => serializer.serialize_str("LightSteelBlue3"),
            Color::Byte(147) => serializer.serialize_str("LightSteelBlue"),
            Color::Byte(148) => serializer.serialize_str("Yellow3"),
            Color::Byte(149) => serializer.serialize_str("DarkOliveGreen3"),
            Color::Byte(150) => serializer.serialize_str("DarkSeaGreen3"),
            Color::Byte(151) => serializer.serialize_str("DarkSeaGreen2"),
            Color::Byte(152) => serializer.serialize_str("LightCyan3"),
            Color::Byte(153) => serializer.serialize_str("LightSkyBlue1"),
            Color::Byte(154) => serializer.serialize_str("GreenYellow"),
            Color::Byte(155) => serializer.serialize_str("DarkOliveGreen2"),
            Color::Byte(156) => serializer.serialize_str("PaleGreen1"),
            Color::Byte(157) => serializer.serialize_str("DarkSeaGreen2"),
            Color::Byte(158) => serializer.serialize_str("DarkSeaGreen1"),
            Color::Byte(159) => serializer.serialize_str("PaleTurquoise1"),
            Color::Byte(160) => serializer.serialize_str("Red3"),
            Color::Byte(161) => serializer.serialize_str("DeepPink3"),
            Color::Byte(162) => serializer.serialize_str("DeepPink3"),
            Color::Byte(163) => serializer.serialize_str("Magenta3"),
            Color::Byte(164) => serializer.serialize_str("Magenta3"),
            Color::Byte(165) => serializer.serialize_str("Magenta2"),
            Color::Byte(166) => serializer.serialize_str("DarkOrange3"),
            Color::Byte(167) => serializer.serialize_str("IndianRed"),
            Color::Byte(168) => serializer.serialize_str("HotPink3"),
            Color::Byte(169) => serializer.serialize_str("HotPink2"),
            Color::Byte(170) => serializer.serialize_str("Orchid"),
            Color::Byte(171) => serializer.serialize_str("MediumOrchid1"),
            Color::Byte(172) => serializer.serialize_str("Orange3"),
            Color::Byte(173) => serializer.serialize_str("LightSalmon3"),
            Color::Byte(174) => serializer.serialize_str("LightPink3"),
            Color::Byte(175) => serializer.serialize_str("Pink3"),
            Color::Byte(176) => serializer.serialize_str("Plum3"),
            Color::Byte(177) => serializer.serialize_str("Violet"),
            Color::Byte(178) => serializer.serialize_str("Gold3"),
            Color::Byte(179) => serializer.serialize_str("LightGoldenrod3"),
            Color::Byte(180) => serializer.serialize_str("Tan"),
            Color::Byte(181) => serializer.serialize_str("MistyRose3"),
            Color::Byte(182) => serializer.serialize_str("Thistle3"),
            Color::Byte(183) => serializer.serialize_str("Plum2"),
            Color::Byte(184) => serializer.serialize_str("Yellow3"),
            Color::Byte(185) => serializer.serialize_str("Khaki3"),
            Color::Byte(186) => serializer.serialize_str("LightGoldenrod2"),
            Color::Byte(187) => serializer.serialize_str("LightYellow3"),
            Color::Byte(188) => serializer.serialize_str("Grey84"),
            Color::Byte(189) => serializer.serialize_str("LightSteelBlue1"),
            Color::Byte(190) => serializer.serialize_str("Yellow2"),
            Color::Byte(191) => serializer.serialize_str("DarkOliveGreen1"),
            Color::Byte(192) => serializer.serialize_str("DarkOliveGreen1"),
            Color::Byte(193) => serializer.serialize_str("DarkSeaGreen1"),
            Color::Byte(194) => serializer.serialize_str("Honeydew2"),
            Color::Byte(195) => serializer.serialize_str("LightCyan1"),
            Color::Byte(196) => serializer.serialize_str("Red1"),
            Color::Byte(197) => serializer.serialize_str("DeepPink2"),
            Color::Byte(198) => serializer.serialize_str("DeepPink1"),
            Color::Byte(199) => serializer.serialize_str("DeepPink1"),
            Color::Byte(200) => serializer.serialize_str("Magenta2"),
            Color::Byte(201) => serializer.serialize_str("Magenta1"),
            Color::Byte(202) => serializer.serialize_str("OrangeRed1"),
            Color::Byte(203) => serializer.serialize_str("IndianRed1"),
            Color::Byte(204) => serializer.serialize_str("IndianRed1"),
            Color::Byte(205) => serializer.serialize_str("HotPink"),
            Color::Byte(206) => serializer.serialize_str("HotPink"),
            Color::Byte(207) => serializer.serialize_str("MediumOrchid1"),
            Color::Byte(208) => serializer.serialize_str("DarkOrange"),
            Color::Byte(209) => serializer.serialize_str("Salmon1"),
            Color::Byte(210) => serializer.serialize_str("LightCoral"),
            Color::Byte(211) => serializer.serialize_str("PaleVioletRed1"),
            Color::Byte(212) => serializer.serialize_str("Orchid2"),
            Color::Byte(213) => serializer.serialize_str("Orchid1"),
            Color::Byte(214) => serializer.serialize_str("Orange1"),
            Color::Byte(215) => serializer.serialize_str("SandyBrown"),
            Color::Byte(216) => serializer.serialize_str("LightSalmon1"),
            Color::Byte(217) => serializer.serialize_str("LightPink1"),
            Color::Byte(218) => serializer.serialize_str("Pink1"),
            Color::Byte(219) => serializer.serialize_str("Plum1"),
            Color::Byte(220) => serializer.serialize_str("Gold1"),
            Color::Byte(221) => serializer.serialize_str("LightGoldenrod2"),
            Color::Byte(222) => serializer.serialize_str("LightGoldenrod2"),
            Color::Byte(223) => serializer.serialize_str("NavajoWhite1"),
            Color::Byte(224) => serializer.serialize_str("MistyRose1"),
            Color::Byte(225) => serializer.serialize_str("Thistle1"),
            Color::Byte(226) => serializer.serialize_str("Yellow1"),
            Color::Byte(227) => serializer.serialize_str("LightGoldenrod1"),
            Color::Byte(228) => serializer.serialize_str("Khaki1"),
            Color::Byte(229) => serializer.serialize_str("Wheat1"),
            Color::Byte(230) => serializer.serialize_str("Cornsilk1"),
            Color::Byte(231) => serializer.serialize_str("Grey100"),
            Color::Byte(232) => serializer.serialize_str("Grey3"),
            Color::Byte(233) => serializer.serialize_str("Grey7"),
            Color::Byte(234) => serializer.serialize_str("Grey11"),
            Color::Byte(235) => serializer.serialize_str("Grey15"),
            Color::Byte(236) => serializer.serialize_str("Grey19"),
            Color::Byte(237) => serializer.serialize_str("Grey23"),
            Color::Byte(238) => serializer.serialize_str("Grey27"),
            Color::Byte(239) => serializer.serialize_str("Grey30"),
            Color::Byte(240) => serializer.serialize_str("Grey35"),
            Color::Byte(241) => serializer.serialize_str("Grey39"),
            Color::Byte(242) => serializer.serialize_str("Grey42"),
            Color::Byte(243) => serializer.serialize_str("Grey46"),
            Color::Byte(244) => serializer.serialize_str("Grey50"),
            Color::Byte(245) => serializer.serialize_str("Grey54"),
            Color::Byte(246) => serializer.serialize_str("Grey58"),
            Color::Byte(247) => serializer.serialize_str("Grey62"),
            Color::Byte(248) => serializer.serialize_str("Grey66"),
            Color::Byte(249) => serializer.serialize_str("Grey70"),
            Color::Byte(250) => serializer.serialize_str("Grey74"),
            Color::Byte(251) => serializer.serialize_str("Grey78"),
            Color::Byte(252) => serializer.serialize_str("Grey82"),
            Color::Byte(253) => serializer.serialize_str("Grey85"),
            Color::Byte(254) => serializer.serialize_str("Grey89"),
            Color::Byte(255) => serializer.serialize_str("Grey93"),
            Color::Rgb(r, g, b) => {
                serializer.serialize_str(&format!("#{:02x}{:02x}{:02x}", r, g, b))
            }
            Color::Default => serializer.serialize_str("Default"),
        }
    }
}
