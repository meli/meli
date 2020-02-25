/*
 * meli - text_processing mod.
 *
 * Copyright 2020 Manos Pitsidianakis
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

use super::TextProcessing;

use smallvec::SmallVec;

pub trait KMP: TextProcessing {
    fn kmp_search(&self, pattern: &str) -> SmallVec<[usize; 256]> {
        let w = pattern.split_graphemes();
        let t = kmp_table(&w);
        let mut j = 0; // (the position of the current character in text)
        let mut k = 0; // (the position of the current character in pattern)
        let mut ret = SmallVec::new();

        let s = self.graphemes_indices();
        while j < s.len() && k < w.len() as i32 {
            if w[k as usize] == s[j].1 {
                j += 1;
                k += 1;
                if k as usize == w.len() {
                    ret.push(s[j - (k as usize)].0);
                    k = t[k as usize];
                }
            } else {
                k = t[k as usize];
                if k < 0 {
                    j += 1;
                    k += 1;
                }
            }
        }
        ret
    }
}

impl KMP for str {}

fn kmp_table(graphemes: &[&str]) -> SmallVec<[i32; 256]> {
    let mut ret: SmallVec<_> = SmallVec::with_capacity(graphemes.len() + 1);
    if graphemes.is_empty() {
        return ret;
    }
    ret.push(-1);
    for _ in 0..graphemes.len() {
        ret.push(0);
    }
    let mut pos: usize = 1;
    let mut cnd: i32 = 0;
    while pos < graphemes.len() {
        if graphemes[pos] == graphemes[cnd as usize] {
            ret[pos] = ret[cnd as usize];
        } else {
            ret[pos] = cnd;
            cnd = ret[cnd as usize];
            while cnd >= 0 && graphemes[pos] != graphemes[cnd as usize] {
                cnd = ret[cnd as usize];
            }
        }
        pos += 1;
        cnd += 1;
    }
    ret[pos] = cnd;
    ret
}

#[test]
fn test_search() {
    use super::_ALICE_CHAPTER_1;
    for ind in _ALICE_CHAPTER_1.kmp_search("Alice") {
        println!(
            "{:#?}",
            &_ALICE_CHAPTER_1
                [ind.saturating_sub(0)..std::cmp::min(_ALICE_CHAPTER_1.len(), ind + 25)]
        );
    }
}
