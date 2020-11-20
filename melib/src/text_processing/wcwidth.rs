/*
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
    rest: &'a [u8],
}

/*
 * UTF-8 uses a system of binary prefixes, in which the high bits of each byte mark whether it’s a single byte, the beginning of a multi-byte sequence, or a continuation byte; the remaining bits, concatenated, give the code point index. This table shows how it works:
 *
 * UTF-8 (binary) 	                Code point (binary) 	Range
 * 0xxxxxxx                     	xxxxxxx 	        U+0000–U+007F
 * 110xxxxx 10yyyyyy 	                xxxxxyyyyyy 	        U+0080–U+07FF
 * 1110xxxx 10yyyyyy 10zzzzzz 	        xxxxyyyyyyzzzzzz 	U+0800–U+FFFF
 * 11110xxx 10yyyyyy 10zzzzzz 10wwwwww 	xxxyyyyyyzzzzzzwwwwww 	U+10000–U+10FFFF
 *
 */
impl<'a> Iterator for CodePointsIterator<'a> {
    type Item = WChar;

    fn next(&mut self) -> Option<WChar> {
        if self.rest.is_empty() {
            return None;
        }
        /* Input is UTF-8 valid strings, guaranteed by Rust's std */
        if self.rest[0] & 0b1000_0000 == 0x0 {
            let ret: WChar = WChar::from(self.rest[0]);
            self.rest = &self.rest[1..];
            return Some(ret);
        }
        if self.rest[0] & 0b1110_0000 == 0b1100_0000 {
            let ret: WChar = (WChar::from(self.rest[0]) & 0b0001_1111).rotate_left(6)
                + (WChar::from(self.rest[1]) & 0b0111_1111);
            self.rest = &self.rest[2..];
            return Some(ret);
        }

        if self.rest[0] & 0b1111_0000 == 0b1110_0000 {
            let ret: WChar = (WChar::from(self.rest[0]) & 0b0000_0111).rotate_left(12)
                + (WChar::from(self.rest[1]) & 0b0011_1111).rotate_left(6)
                + (WChar::from(self.rest[2]) & 0b0011_1111);
            self.rest = &self.rest[3..];
            return Some(ret);
        }

        let ret: WChar = (WChar::from(self.rest[0]) & 0b0000_0111).rotate_left(18)
            + (WChar::from(self.rest[1]) & 0b0011_1111).rotate_left(12)
            + (WChar::from(self.rest[2]) & 0b0011_1111).rotate_left(6)
            + (WChar::from(self.rest[3]) & 0b0011_1111);
        self.rest = &self.rest[4..];
        Some(ret)
    }
}
pub trait CodePointsIter {
    fn code_points(&self) -> CodePointsIterator;
}

impl CodePointsIter for str {
    fn code_points(&self) -> CodePointsIterator {
        CodePointsIterator {
            rest: self.as_bytes(),
        }
    }
}
impl CodePointsIter for &str {
    fn code_points(&self) -> CodePointsIterator {
        CodePointsIterator {
            rest: self.as_bytes(),
        }
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
    } else if bisearch(ucs, super::tables::PRIVATE) {
        None
    } else if bisearch(ucs, super::tables::NONPRINT) {
        None
    } else if bisearch(ucs, super::tables::COMBINING) {
        None
    } else if bisearch(ucs, super::tables::DOUBLEWIDE) {
        Some(2)
    } else if bisearch(ucs, super::tables::AMBIGUOUS) {
        Some(1)
    } else if bisearch(ucs, super::tables::UNASSIGNED) {
        Some(2)
    } else if bisearch(ucs, super::tables::WIDENEDIN9) {
        Some(2)
    } else {
        Some(1)
    }
}

#[test]
fn test_wcwidth() {
    use crate::text_processing::grapheme_clusters::TextProcessing;
    assert_eq!("●".grapheme_width(), 1);
    assert_eq!("●📎".grapheme_width(), 3);
    assert_eq!("●\u{FE0E}📎\u{FE0E}".grapheme_width(), 3);
    assert_eq!("🎃".grapheme_width(), 2);
    assert_eq!("👻".grapheme_width(), 2);
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
