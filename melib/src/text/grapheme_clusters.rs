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

//! Breaks a string into individual user-perceived "characters".
//!
//! Unicode UAX-29 standard, version 10.0.0

use unicode_segmentation::UnicodeSegmentation;

use super::{types::Reflow, wcwidth::wcwidth};

pub trait TextProcessing: UnicodeSegmentation + AsRef<str> {
    /// Returns a vector containg each grapheme as a slice.
    fn split_graphemes(&self) -> Vec<&str> {
        UnicodeSegmentation::graphemes(self, true).collect::<Vec<&str>>()
    }

    /// Returns a vector containg each grapheme and the index it starts at as a
    /// slice.
    fn graphemes_indices(&self) -> Vec<(usize, &str)> {
        UnicodeSegmentation::grapheme_indices(self, true).collect::<Vec<(usize, &str)>>()
    }

    /// Returns the first grapheme and the zero index.
    fn next_grapheme(&self) -> Option<(usize, &str)> {
        UnicodeSegmentation::grapheme_indices(self, true).next()
    }

    /// Returns the last grapheme and its starting index.
    fn last_grapheme(&self) -> Option<(usize, &str)> {
        UnicodeSegmentation::grapheme_indices(self, true).next_back()
    }

    /// Returns the total width of all graphemes using [`wcwidth`] for each
    /// code-point.
    fn grapheme_width(&self) -> usize {
        let mut count = 0;
        let s: &str = self.as_ref();
        for c in s.chars() {
            count += wcwidth(c).unwrap_or(0);
        }

        count
    }

    /// Returns the amount of graphemes.
    fn grapheme_len(&self) -> usize {
        self.split_graphemes().len()
    }

    /// Splits lines at word boundaries without breaking any words, for given
    /// line width.
    fn split_lines(&self, width: usize) -> Vec<String>;

    /// Splits lines at word boundaries without breaking any words, for given
    /// line width, using a reflow algorithm to balance line width
    /// variation.
    fn split_lines_reflow(&self, reflow: Reflow, width: Option<usize>) -> Vec<String>;
}

impl TextProcessing for str {
    fn split_lines(&self, width: usize) -> Vec<String> {
        if width == 0 {
            return vec![];
        }
        super::line_break::linear(self, width)
    }

    fn split_lines_reflow(&self, reflow: Reflow, width: Option<usize>) -> Vec<String> {
        if width == Some(0) {
            return vec![];
        }
        super::line_break::split_lines_reflow(self, reflow, width)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text::TextPresentation;

    #[test]
    fn test_grapheme_width() {
        assert_eq!("●".grapheme_width(), 1);
        assert_eq!("●📎".grapheme_width(), 3);
        assert_eq!("●📎︎".grapheme_width(), 3);
        assert_eq!("●\u{FE0E}📎\u{FE0E}".grapheme_width(), 3);
        assert_eq!("🎃".grapheme_width(), 2);
        assert_eq!("👻".grapheme_width(), 2);
        assert_eq!("🛡︎".grapheme_width(), 2);
        assert_eq!("🛡︎".text_pr().grapheme_width(), 2);

        assert_eq!("こんにちわ世界".grapheme_width(), 14);
        assert_eq!("こ★ん■に●ち▲わ☆世◆界".grapheme_width(), 20);
    }
}
