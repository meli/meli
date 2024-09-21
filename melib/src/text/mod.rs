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

use unicode_segmentation::UnicodeSegmentation;

pub mod grapheme_clusters;
pub mod line_break;
pub mod search;
pub mod tables;
pub mod types;
pub use types::Reflow;
pub mod wcwidth;
pub use grapheme_clusters::*;
pub use line_break::*;
pub use wcwidth::*;

pub trait Truncate {
    fn truncate_at_boundary(&mut self, new_len: usize);
    fn trim_at_boundary(&self, new_len: usize) -> &str;
    fn trim_left_at_boundary(&self, new_len: usize) -> &str;
    fn truncate_left_at_boundary(&mut self, new_len: usize);
}

impl Truncate for &str {
    fn truncate_at_boundary(&mut self, new_len: usize) {
        if new_len >= self.len() {
            return;
        }

        if let Some((last, _)) = UnicodeSegmentation::grapheme_indices(*self, true)
            .take(new_len)
            .last()
        {
            *self = &self[..last];
        }
    }

    fn trim_at_boundary(&self, new_len: usize) -> &str {
        if new_len >= self.len() {
            return self;
        }

        if let Some((last, _)) = UnicodeSegmentation::grapheme_indices(*self, true)
            .take(new_len)
            .last()
        {
            &self[..last]
        } else {
            self
        }
    }

    fn trim_left_at_boundary(&self, skip_len: usize) -> &str {
        if skip_len >= self.len() {
            return "";
        }

        if let Some((first, _)) = UnicodeSegmentation::grapheme_indices(*self, true).nth(skip_len) {
            &self[first..]
        } else {
            self
        }
    }

    fn truncate_left_at_boundary(&mut self, skip_len: usize) {
        if skip_len >= self.len() {
            *self = "";
            return;
        }

        if let Some((first, _)) = UnicodeSegmentation::grapheme_indices(*self, true).nth(skip_len) {
            *self = &self[first..];
        }
    }
}

impl Truncate for String {
    fn truncate_at_boundary(&mut self, new_len: usize) {
        if new_len >= self.len() {
            return;
        }

        if let Some((last, _)) = UnicodeSegmentation::grapheme_indices(self.as_str(), true)
            .take(new_len)
            .last()
        {
            Self::truncate(self, last);
        }
    }

    fn trim_at_boundary(&self, new_len: usize) -> &str {
        if new_len >= self.len() {
            return self;
        }

        if let Some((last, _)) = UnicodeSegmentation::grapheme_indices(self.as_str(), true)
            .take(new_len)
            .last()
        {
            &self[..last]
        } else {
            self.as_str()
        }
    }

    fn trim_left_at_boundary(&self, skip_len: usize) -> &str {
        if skip_len >= self.len() {
            return "";
        }

        if let Some((first, _)) =
            UnicodeSegmentation::grapheme_indices(self.as_str(), true).nth(skip_len)
        {
            &self[first..]
        } else {
            self.as_str()
        }
    }

    fn truncate_left_at_boundary(&mut self, skip_len: usize) {
        if skip_len >= self.len() {
            self.clear();
            return;
        }

        if let Some((first, _)) =
            UnicodeSegmentation::grapheme_indices(self.as_str(), true).nth(skip_len)
        {
            *self = self[first..].to_string();
        }
    }
}

pub mod hex {
    use std::fmt::Write;

    use crate::error::Result;

    pub fn bytes_to_hex(bytes: &[u8]) -> Result<String> {
        let mut retval = String::with_capacity(bytes.len() / 2 + bytes.len() / 4);
        for (i, c) in bytes.chunks(2).enumerate() {
            if i % 16 == 0 {
                writeln!(&mut retval)?;
            } else if i % 4 == 0 {
                write!(&mut retval, " ")?;
            }
            if c.len() == 2 {
                write!(&mut retval, "{:02x}{:02x}", c[0], c[1])?;
            } else {
                write!(&mut retval, "{:02x}", c[0])?;
            }
        }
        Ok(retval)
    }
}

#[cfg(test)]
pub const _ALICE_CHAPTER_1: &str = r#"CHAPTER I. Down the Rabbit-Hole

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
