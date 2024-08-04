/*
 * meli - text mod.
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

/*
 * This is an implementation of wcwidth() and wcswidth() as defined in
 * "The Single UNIX Specification, Version 2, The Open Group, 1997"
 * <http://www.UNIX-systems.org/online.html>
 *
 * Markus Kuhn -- 2001-09-08 -- public domain
 */

// Update to Unicode 12

#[macro_export]
macro_rules! big_if_true {
    ($a:expr) => {
        if $a {
            1
        } else {
            0
        }
    };
}

type WChar = u32;
type Interval = (WChar, WChar);

pub struct CodePointsIterator<'a> {
    rest: std::str::Chars<'a>,
}

/*
 * UTF-8 uses a system of binary prefixes, in which the high bits of each
 * byte mark whether it’s a single byte, the beginning of a multi-byte
 * sequence, or a continuation byte; the remaining bits, concatenated, give
 * the code point index. This table shows how it works:
 *
 * ```text
 * UTF-8 (binary)                      |Code point (binary)    |Range
 * ------------------------------------+-----------------------+-------
 * 0xxxxxxx                            |xxxxxxx                |U+0000–U+007F
 * 110xxxxx 10yyyyyy                   |xxxxxyyyyyy            |U+0080–U+07FF
 * 1110xxxx 10yyyyyy 10zzzzzz          |xxxxyyyyyyzzzzzz       |U+0800–U+FFFF
 * 11110xxx 10yyyyyy 10zzzzzz 10wwwwww |xxxyyyyyyzzzzzzwwwwww  |U+10000–U+10FFFF
 * ```
 *
 */
impl<'a> Iterator for CodePointsIterator<'a> {
    type Item = WChar;

    fn next(&mut self) -> Option<WChar> {
        self.rest.next().map(|c| c as WChar)
    }
}
pub trait CodePointsIter {
    fn code_points(&self) -> CodePointsIterator;
}

impl CodePointsIter for str {
    fn code_points(&self) -> CodePointsIterator {
        CodePointsIterator { rest: self.chars() }
    }
}
impl CodePointsIter for &str {
    fn code_points(&self) -> CodePointsIterator {
        CodePointsIterator { rest: self.chars() }
    }
}

/* auxiliary function for binary search in Interval table */
fn bisearch(ucs: WChar, table: &'static [Interval]) -> bool {
    let mut min = 0;
    let mut mid;

    let mut max = table.len() - 1;

    if ucs < table[0].0 || ucs > table[max].1 {
        return false;
    }
    while max >= min {
        mid = (min + max) / 2;
        if ucs > table[mid].1 {
            min = mid + 1;
        } else if ucs < table[mid].0 {
            max = mid - 1;
        } else {
            return true;
        }
    }

    false
}

pub fn wcwidth(ucs: WChar) -> Option<usize> {
    if bisearch(ucs, super::tables::ASCII) {
        Some(1)
    } else if bisearch(ucs, super::tables::PRIVATE)
        || bisearch(ucs, super::tables::NONPRINT)
        || bisearch(ucs, super::tables::COMBINING)
    {
        None
    } else if bisearch(ucs, super::tables::DOUBLEWIDE) {
        Some(2)
    } else if bisearch(ucs, super::tables::AMBIGUOUS) {
        Some(1)
    } else if bisearch(ucs, super::tables::UNASSIGNED) || bisearch(ucs, super::tables::WIDENEDIN9) {
        Some(2)
    } else {
        Some(1)
    }
}

pub fn wcswidth(mut pwcs: WChar, mut n: usize) -> Option<usize> {
    let mut width = 0;

    while pwcs > 0 && n > 0 {
        if let Some(w) = wcwidth(pwcs) {
            width += w;
        } else {
            return None;
        }

        pwcs += 1;
        n -= 1;
    }

    Some(width)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text::grapheme_clusters::TextProcessing;

    #[test]
    fn test_wcwidth() {
        assert_eq!(
            &"abc\0".code_points().collect::<Vec<_>>(),
            &[0x61, 0x62, 0x63, 0x0]
        );
        assert_eq!(&"●".code_points().collect::<Vec<_>>(), &[0x25cf]);
        assert_eq!(&"📎".code_points().collect::<Vec<_>>(), &[0x1f4ce]);
        assert_eq!(
            &"𐼹𐼺𐼻𐼼𐼽".code_points().collect::<Vec<_>>(),
            &[0x10F39, 0x10F3A, 0x10F3B, 0x10F3C, 0x10F3D]
        ); // Sogdian alphabet
        assert_eq!(
            &"𐼹a𐼽b".code_points().collect::<Vec<_>>(),
            &[0x10F39, 0x61, 0x10F3D, 0x62]
        ); // Sogdian alphabet
        assert_eq!(
            &"📎\u{FE0E}".code_points().collect::<Vec<_>>(),
            &[0x1f4ce, 0xfe0e]
        );
        assert_eq!("●".grapheme_width(), 1);
        assert_eq!("●📎".grapheme_width(), 3);
        assert_eq!("●📎︎".grapheme_width(), 3);
        assert_eq!("●\u{FE0E}📎\u{FE0E}".grapheme_width(), 3);
        assert_eq!("🎃".grapheme_width(), 2);
        assert_eq!("👻".grapheme_width(), 2);

        assert_eq!("こんにちわ世界".grapheme_width(), 14);
        assert_eq!("こ★ん■に●ち▲わ☆世◆界".grapheme_width(), 20);
    }
}
