/*
 * meli - melib crate.
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

#[cfg(feature = "unicode_algorithms")]
include!("src/text_processing/types.rs");

fn main() -> Result<(), std::io::Error> {
    #[cfg(feature = "unicode_algorithms")]
    {
        const MOD_PATH: &str = "src/text_processing/tables.rs";
        println!("cargo:rerun-if-changed=build.rs");
        println!("cargo:rerun-if-changed={}", MOD_PATH);
        /* Line break tables */
        use std::fs::File;
        use std::io::prelude::*;
        use std::io::BufReader;
        use std::path::Path;
        use std::process::{Command, Stdio};
        const LINE_BREAK_TABLE_URL: &str =
            "http://www.unicode.org/Public/UCD/latest/ucd/LineBreak.txt";
        /* Grapheme width tables */
        const UNICODE_DATA_URL: &str =
            "http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt";
        const EAW_URL: &str = "http://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt";
        const EMOJI_DATA_URL: &str =
            "https://www.unicode.org/Public/UCD/latest/ucd/emoji/emoji-data.txt";

        let mod_path = Path::new(MOD_PATH);
        if mod_path.exists() {
            eprintln!(
                "{} already exists, delete it if you want to replace it.",
                mod_path.display()
            );
            std::process::exit(0);
        }
        let mut child = Command::new("curl")
            .args(&["-o", "-", LINE_BREAK_TABLE_URL])
            .stdout(Stdio::piped())
            .stdin(Stdio::null())
            .stderr(Stdio::inherit())
            .spawn()?;

        let buf_reader = BufReader::new(child.stdout.take().unwrap());

        let mut line_break_table: Vec<(u32, u32, LineBreakClass)> = Vec::with_capacity(3800);
        for line in buf_reader.lines() {
            let line = line.unwrap();
            if line.starts_with('#') || line.starts_with(' ') || line.is_empty() {
                continue;
            }
            let tokens: &str = line.split_whitespace().next().unwrap();

            let semicolon_idx: usize = tokens.chars().position(|c| c == ';').unwrap();
            /* LineBreak.txt list is ascii encoded so we can assume each char takes one byte: */
            let chars_str: &str = &tokens[..semicolon_idx];

            let mut codepoint_iter = chars_str.split("..");

            let first_codepoint: u32 =
                u32::from_str_radix(codepoint_iter.next().unwrap(), 16).unwrap();

            let sec_codepoint: u32 = codepoint_iter
                .next()
                .map(|v| u32::from_str_radix(v, 16).unwrap())
                .unwrap_or(first_codepoint);
            let class = &tokens[semicolon_idx + 1..semicolon_idx + 1 + 2];
            line_break_table.push((first_codepoint, sec_codepoint, LineBreakClass::from(class)));
        }
        child.wait()?;

        let child = Command::new("curl")
            .args(&["-o", "-", UNICODE_DATA_URL])
            .stdout(Stdio::piped())
            .output()?;

        let unicode_data = String::from_utf8_lossy(&child.stdout);

        let child = Command::new("curl")
            .args(&["-o", "-", EAW_URL])
            .stdout(Stdio::piped())
            .output()?;

        let eaw_data = String::from_utf8_lossy(&child.stdout);

        let child = Command::new("curl")
            .args(&["-o", "-", EMOJI_DATA_URL])
            .stdout(Stdio::piped())
            .output()?;

        let emoji_data = String::from_utf8_lossy(&child.stdout);

        const MAX_CODEPOINT: usize = 0x110000;
        // See https://www.unicode.org/L2/L1999/UnicodeData.html
        const FIELD_CODEPOINT: usize = 0;
        const FIELD_CATEGORY: usize = 2;
        // Ambiguous East Asian characters
        const WIDTH_AMBIGUOUS_EASTASIAN: isize = -3;

        // Width changed from 1 to 2 in Unicode 9.0
        const WIDTH_WIDENED_IN_9: isize = -6;
        // Category for unassigned codepoints.
        const CAT_UNASSIGNED: &str = "Cn";

        // Category for private use codepoints.
        const CAT_PRIVATE_USE: &str = "Co";

        // Category for surrogates.
        const CAT_SURROGATE: &str = "Cs";

        struct Codepoint<'cat> {
            raw: u32,
            width: Option<isize>,
            category: &'cat str,
        }

        let mut codepoints: Vec<Codepoint> = Vec::with_capacity(MAX_CODEPOINT + 1);
        for i in 0..=MAX_CODEPOINT {
            codepoints.push(Codepoint {
                raw: i as u32,
                width: None,
                category: CAT_UNASSIGNED,
            });
        }

        set_general_categories(&mut codepoints, &unicode_data);
        set_eaw_widths(&mut codepoints, &eaw_data);
        set_emoji_widths(&mut codepoints, &emoji_data);
        set_hardcoded_ranges(&mut codepoints);
        fn hexrange_to_range(hexrange: &str) -> std::ops::Range<usize> {
            /* Given a string like 1F300..1F320 representing an inclusive range,
            return the range of codepoints.
            If the string is like 1F321, return a range of just that element.
            */
            let hexrange = hexrange.trim();
            let fields = hexrange
                .split("..")
                .map(|h| usize::from_str_radix(h.trim(), 16).unwrap())
                .collect::<Vec<usize>>();
            if fields.len() == 1 {
                fields[0]..(fields[0] + 1)
            } else {
                fields[0]..(fields[1] + 1)
            }
        }

        fn set_general_categories<'u>(codepoints: &mut Vec<Codepoint<'u>>, unicode_data: &'u str) {
            for line in unicode_data.lines() {
                let fields = line.trim().split(';').collect::<Vec<_>>();
                if fields.len() > FIELD_CATEGORY {
                    for idx in hexrange_to_range(fields[FIELD_CODEPOINT]) {
                        codepoints[idx].category = fields[FIELD_CATEGORY];
                    }
                }
            }
        }

        fn set_eaw_widths(codepoints: &mut Vec<Codepoint<'_>>, eaw_data_lines: &str) {
            //  Read from EastAsianWidth.txt, set width values on the codepoints
            for line in eaw_data_lines.lines() {
                let line = line.trim().split('#').next().unwrap_or(line);
                let fields = line.trim().split(';').collect::<Vec<_>>();
                if fields.len() != 2 {
                    continue;
                }
                let hexrange = fields[0];
                let width_type = fields[1];
                // width_types:
                //  A: ambiguous, F: fullwidth, H: halfwidth,
                // . N: neutral, Na: east-asian Narrow
                let width: isize = if width_type == "A" {
                    WIDTH_AMBIGUOUS_EASTASIAN
                } else if width_type == "F" || width_type == "W" {
                    2
                } else {
                    1
                };
                for cp in hexrange_to_range(hexrange) {
                    codepoints[cp].width = Some(width);
                }
            }
            // Apply the following special cases:
            //  - The unassigned code points in the following blocks default to "W":
            //         CJK Unified Ideographs Extension A: U+3400..U+4DBF
            //         CJK Unified Ideographs:             U+4E00..U+9FFF
            //         CJK Compatibility Ideographs:       U+F900..U+FAFF
            //  - All undesignated code points in Planes 2 and 3, whether inside or
            //      outside of allocated blocks, default to "W":
            //         Plane 2:                            U+20000..U+2FFFD
            //         Plane 3:                            U+30000..U+3FFFD
            const WIDE_RANGES: [(usize, usize); 5] = [
                (0x3400, 0x4DBF),
                (0x4E00, 0x9FFF),
                (0xF900, 0xFAFF),
                (0x20000, 0x2FFFD),
                (0x30000, 0x3FFFD),
            ];
            for &wr in WIDE_RANGES.iter() {
                for cp in wr.0..(wr.1 + 1) {
                    if codepoints[cp].width.is_none() {
                        codepoints[cp].width = Some(2);
                    }
                }
            }
        }
        fn set_emoji_widths(codepoints: &mut Vec<Codepoint<'_>>, emoji_data_lines: &str) {
            // Read from emoji-data.txt, set codepoint widths
            for line in emoji_data_lines.lines() {
                if !line.contains('#') || line.trim().starts_with('#') {
                    continue;
                }
                let mut fields = line.trim().split('#').collect::<Vec<_>>();
                if fields.len() != 2 {
                    continue;
                }
                let comment = fields.pop().unwrap();
                let fields = fields.pop().unwrap();

                let hexrange = fields.split(';').next().unwrap();

                // In later versions of emoji-data.txt there are some "reserved"
                // entries that have "NA" instead of a Unicode version number
                // of first use, they will now return a zero version instead of
                // crashing the script
                if comment.trim().starts_with("NA") {
                    continue;
                }

                use std::str::FromStr;
                let mut v = comment.trim().split_whitespace().next().unwrap();
                if v.starts_with('E') {
                    v = &v[1..];
                }
                if v.as_bytes()
                    .get(0)
                    .map(|c| !c.is_ascii_digit())
                    .unwrap_or(true)
                {
                    continue;
                }
                let mut idx = 1;
                while v
                    .as_bytes()
                    .get(idx)
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
                {
                    idx += 1;
                }
                if v.as_bytes().get(idx).map(|&c| c != b'.').unwrap_or(true) {
                    continue;
                }
                idx += 1;
                while v
                    .as_bytes()
                    .get(idx)
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
                {
                    idx += 1;
                }
                v = &v[0..idx];

                let version = f32::from_str(v).unwrap();
                for cp in hexrange_to_range(hexrange) {
                    // Don't consider <=1F000 values as emoji. These can only be made
                    // emoji through the variation selector which interacts terribly
                    // with wcwidth().
                    if cp < 0x1F000 {
                        continue;
                    }
                    // Skip codepoints that are explicitly not wide.
                    // For example U+1F336 ("Hot Pepper") renders like any emoji but is
                    // marked as neutral in EAW so has width 1 for some reason.
                    //if codepoints[cp].width == Some(1) {
                    //    continue;
                    //}

                    // If this emoji was introduced before Unicode 9, then it was widened in 9.
                    codepoints[cp].width = if version >= 9.0 {
                        Some(2)
                    } else {
                        Some(WIDTH_WIDENED_IN_9)
                    };
                }
            }
        }
        fn set_hardcoded_ranges(codepoints: &mut Vec<Codepoint<'_>>) {
            // Mark private use and surrogate codepoints
            // Private use can be determined awkwardly from UnicodeData.txt,
            // but we just hard-code them.
            // We do not treat "private use high surrogate" as private use
            // so as to match wcwidth9().
            const PRIVATE_RANGES: [(usize, usize); 3] =
                [(0xE000, 0xF8FF), (0xF0000, 0xFFFFD), (0x100000, 0x10FFFD)];
            for &(first, last) in PRIVATE_RANGES.iter() {
                for idx in first..=last {
                    codepoints[idx].category = CAT_PRIVATE_USE;
                }
            }

            const SURROGATE_RANGES: [(usize, usize); 2] = [(0xD800, 0xDBFF), (0xDC00, 0xDFFF)];
            for &(first, last) in SURROGATE_RANGES.iter() {
                for idx in first..=last {
                    codepoints[idx].category = CAT_SURROGATE;
                }
            }
        }

        let mut file = File::create(&mod_path)?;
        file.write_all(
            br#"/*
 * meli - text_processing crate.
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

use super::types::LineBreakClass::{self, *};

pub const LINE_BREAK_RULES: &[(u32, u32, LineBreakClass)] = &[
"#,
        )
        .unwrap();
        for l in &line_break_table {
            file.write_all(format!("    (0x{:X}, 0x{:X}, {:?}),\n", l.0, l.1, l.2).as_bytes())
                .unwrap();
        }
        file.write_all(b"];\n").unwrap();

        for (name, filter) in [
            (
                "ASCII",
                Box::new(|c: &&Codepoint| c.raw < 0x7f && c.raw >= 0x20)
                    as Box<dyn Fn(&&Codepoint) -> bool>,
            ),
            (
                "PRIVATE",
                Box::new(|c: &&Codepoint| c.category == CAT_PRIVATE_USE),
            ),
            (
                "NONPRINT",
                Box::new(|c: &&Codepoint| {
                    ["Cc", "Cf", "Zl", "Zp", CAT_SURROGATE].contains(&c.category)
                }),
            ),
            (
                "COMBINING",
                Box::new(|c: &&Codepoint| ["Mn", "Mc", "Me"].contains(&c.category)),
            ),
            ("DOUBLEWIDE", Box::new(|c: &&Codepoint| c.width == Some(2))),
            (
                "UNASSIGNED",
                Box::new(|c: &&Codepoint| c.category == CAT_UNASSIGNED),
            ),
            (
                "AMBIGUOUS",
                Box::new(|c: &&Codepoint| c.width == Some(WIDTH_AMBIGUOUS_EASTASIAN)),
            ),
            (
                "WIDENEDIN9",
                Box::new(|c: &&Codepoint| c.width == Some(WIDTH_WIDENED_IN_9)),
            ),
        ]
        .iter()
        {
            file.write_all(
                format!(
                    r#"
pub const {}: &[(u32, u32)] = &[
"#,
                    name
                )
                .as_bytes(),
            )
            .unwrap();
            let mut iter = codepoints.iter().filter(filter);
            let mut prev = iter.next().unwrap().raw;
            let mut a = prev;
            for cp in iter {
                if prev + 1 != cp.raw {
                    file.write_all(format!("    (0x{:X}, 0x{:X}),\n", a, prev).as_bytes())
                        .unwrap();
                    a = cp.raw;
                }
                prev = cp.raw;
            }
            file.write_all(format!("    (0x{:X}, 0x{:X}),\n", a, prev).as_bytes())
                .unwrap();
            file.write_all(b"];\n").unwrap();
        }
    }
    Ok(())
}
