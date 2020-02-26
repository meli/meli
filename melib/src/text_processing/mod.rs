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
pub mod search;
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

pub const _ALICE_CHAPTER_1: &'static str = r#"CHAPTER I. Down the Rabbit-Hole

Alice was beginning to get very tired of sitting by her sister on the 
bank, and of having nothing to do: once or twice she had peeped into the 
book her sister was reading, but it had no pictures or conversations in 
it, ‘and what is the use of a book,’ thought Alice ‘without pictures or 
conversations?’

So she was considering in her own mind (as well as she could, for the 
hot day made her feel very sleepy and stupid), whether the pleasure 
of making a daisy-chain would be worth the trouble of getting up and 
picking the daisies, when suddenly a White Rabbit with pink eyes ran 
close by her.

>>There was nothing so VERY remarkable in that; nor did Alice think it so 
>>VERY much out of the way to hear the Rabbit say to itself, ‘Oh dear! 
>> Oh dear! I shall be late!’ (when she thought it over afterwards, it 
>>occurred to her that she ought to have wondered at this, but at the time 
>>it all seemed quite natural); but when the Rabbit actually TOOK A WATCH 
OUT OF ITS WAISTCOAT-POCKET, and looked at it, and then hurried on, 
>>Alice started to her feet, for it flashed across her mind that she had 
>>never before seen a rabbit with either a waistcoat-pocket, or a watch 
>>to take out of it, and burning with curiosity, she ran across the field
after it, and fortunately was just in time to see it pop down a large 
rabbit-hole under the hedge.

In another moment down went Alice after it, never once considering how 
in the world she was to get out again.

The rabbit-hole went straight on like a tunnel for some way, and then 
dipped suddenly down, so suddenly that Alice had not a moment to think 
about stopping herself before she found herself falling down a very deep 
well.

Either the well was very deep, or she fell very slowly, for she had 
plenty of time as she went down to look about her and to wonder what was 
going to happen next. First, she tried to look down and make out what 
she was coming to, but it was too dark to see anything; then she 
looked at the sides of the well, and noticed that they were filled with 
cupboards and book-shelves; here and there she saw maps and pictures 
hung upon pegs. She took down a jar from one of the shelves as 
she passed; it was labelled ‘ORANGE MARMALADE’, but to her great 
disappointment it was empty: she did not like to drop the jar for fear 
of killing somebody, so managed to put it into one of the cupboards as 
she fell past it.

‘Well!’ thought Alice to herself, ‘after such a fall as this, I shall 
think nothing of tumbling down stairs! How brave they’ll all think me at 
home! Why, I wouldn’t say anything about it, even if I fell off the top 
of the house!’ (Which was very likely true.)"#;
