/*
 * meli - text_processing mod.
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

pub mod grapheme_clusters;
pub mod line_break;
mod tables;
mod types;
pub use types::Reflow;
pub mod wcwidth;
pub use grapheme_clusters::*;
pub use line_break::*;
pub use wcwidth::*;

pub trait Truncate {
    fn truncate_at_boundary(self, new_len: usize);
}

impl Truncate for &mut String {
    fn truncate_at_boundary(self, new_len: usize) {
        if new_len >= self.len() {
            return;
        }

        extern crate unicode_segmentation;
        use unicode_segmentation::UnicodeSegmentation;
        if let Some((last, _)) = UnicodeSegmentation::grapheme_indices(self.as_str(), true)
            .take(new_len)
            .last()
        {
            String::truncate(self, last);
        }
    }
}

pub trait GlobMatch {
    fn matches_glob(&self, s: &str) -> bool;
    fn is_glob(&self) -> bool;
}

impl GlobMatch for str {
    fn matches_glob(&self, _pattern: &str) -> bool {
        macro_rules! strip_slash {
            ($v:expr) => {
                if $v.ends_with("/") {
                    &$v[..$v.len() - 1]
                } else {
                    $v
                }
            };
        }
        let pattern: Vec<&str> = strip_slash!(_pattern).split_graphemes();
        let s: Vec<&str> = strip_slash!(self).split_graphemes();

        // Taken from https://research.swtch.com/glob

        let mut px = 0;
        let mut sx = 0;
        let mut next_px = 0;
        let mut next_sx = 0;
        while px < pattern.len() || sx < s.len() {
            if px < pattern.len() {
                match pattern[px] {
                    "?" => {
                        if sx < s.len() {
                            px += 1;
                            sx += 1;
                            continue;
                        }
                    }
                    "*" => {
                        // Try to match at sx.
                        // If that doesn't work out,
                        // restart at sx+1 next.
                        next_px = px;
                        next_sx = sx + 1;
                        px += 1;
                        continue;
                    }
                    p => {
                        if sx < s.len() && s[sx] == p {
                            px += 1;
                            sx += 1;
                            continue;
                        }
                    }
                }
            }
            // Mismatch. Maybe restart.
            if 0 < next_sx && next_sx <= s.len() {
                px = next_px;
                sx = next_sx;
                continue;
            }
            return false;
        }
        true
    }

    fn is_glob(&self) -> bool {
        self.contains('*')
    }
}

#[test]
fn test_globmatch() {
    assert!("INBOX".matches_glob("INBOX"));
    assert!("INBOX/".matches_glob("INBOX"));
    assert!("INBOX".matches_glob("INBO?"));

    assert!("INBOX/Sent".matches_glob("INBOX/*"));
    assert!(!"INBOX/Sent".matches_glob("INBOX"));
    assert!(!"INBOX/Sent".matches_glob("*/Drafts"));
    assert!("INBOX/Sent".matches_glob("*/Sent"));

    assert!("INBOX/Archives/2047".matches_glob("*"));
    assert!("INBOX/Archives/2047".matches_glob("INBOX/*/2047"));
    assert!("INBOX/Archives/2047".matches_glob("INBOX/Archives/2*047"));
    assert!("INBOX/Archives/2047".matches_glob("INBOX/Archives/204?"));

    assert!(!"INBOX/Lists/".matches_glob("INBOX/Lists/*"));
}
