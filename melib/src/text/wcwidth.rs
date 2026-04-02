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

/// Auxiliary function for binary search in interval table
const fn bisearch(ucs: u32, table: &'static [(u32, u32)]) -> bool {
    let mut min = 0;
    let mut mid;

    if table.is_empty() {
        return false;
    }
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

/// Determine columns needed for a character.
pub const fn wcwidth(ucs: char) -> Option<usize> {
    let ucs = ucs as u32;
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

/// Determine columns needed for a fixed-size character array.
pub const fn wcswidth(pwcs: &[char]) -> Option<usize> {
    let mut width = 0;
    let mut n = pwcs.len();

    while n > 0 {
        n -= 1;
        let c = pwcs[n];
        if let Some(w) = wcwidth(c) {
            width += w;
        }
    }

    if width > 0 {
        Some(width)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wcwidth() {
        assert_eq!(wcswidth(&"abc\0".chars().collect::<Vec<_>>()), Some(3));
        assert_eq!(wcswidth(&"●".chars().collect::<Vec<_>>()), Some(1));
        assert_eq!(wcswidth(&"📎".chars().collect::<Vec<_>>()), Some(2));
        // Sogdian alphabet
        assert_eq!(wcswidth(&"𐼹𐼺𐼻𐼼𐼽".chars().collect::<Vec<_>>()), Some(5));
        // Sogdian alphabet
        assert_eq!(wcswidth(&"𐼹a𐼽b".chars().collect::<Vec<_>>()), Some(4));
    }
}
