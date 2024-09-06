/*
 * meli
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

//! A string buffer that supports input operations, meant to be used in text
//! input widgets.

use melib::text::TextProcessing;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct UText {
    content: String,
    cursor_pos: usize,
    grapheme_cursor_pos: usize,
}

impl UText {
    /// Creates a new [`UText`] with the cursor initialized at the end.
    pub fn new(content: String) -> Self {
        Self {
            cursor_pos: content.len(),
            grapheme_cursor_pos: content.split_graphemes().len(),
            content,
        }
    }

    /// Set cursor to position. If new value is out of bounds, do nothing.
    pub fn set_cursor(&mut self, cursor_pos: usize) {
        if cursor_pos > self.content.len() {
            return;
        }

        let (first, _) = self.content.split_at(cursor_pos);
        self.grapheme_cursor_pos = first.split_graphemes().len();
        self.cursor_pos = cursor_pos;
    }

    /// Inner content as a string slice.
    pub fn as_str(&self) -> &str {
        self.content.as_str()
    }

    /// Clear string content and set cursor to zero.
    pub fn clear(&mut self) {
        self.content.clear();
        self.cursor_pos = 0;
        self.grapheme_cursor_pos = 0;
    }

    /// Consume self and return owned inner string value.
    pub fn into_string(self) -> String {
        self.content
    }

    /// Reset with new content.
    pub fn set_content(&mut self, new_val: String) {
        *self = Self::new(new_val);
    }

    pub fn grapheme_len(&self) -> usize {
        self.content.split_graphemes().len()
    }

    /// Increase cursor.
    ///
    /// Returns `false` if cursor is at boundary and did not move.
    pub fn cursor_inc(&mut self) -> bool {
        if self.cursor_pos >= self.content.len() {
            return false;
        }

        let (_, right) = self.content.split_at(self.cursor_pos);
        if let Some((_, graph)) = right.next_grapheme() {
            self.cursor_pos += graph.len();
            self.grapheme_cursor_pos += 1;
        }

        true
    }

    /// Decrease cursor.
    ///
    /// Returns `false` if cursor is at boundary and did not move.
    pub fn cursor_dec(&mut self) -> bool {
        if self.cursor_pos == 0 {
            return false;
        }
        let (left, _) = self.content.split_at(self.cursor_pos);
        if let Some((_, graph)) = left.last_grapheme() {
            self.cursor_pos -= graph.len();
            self.grapheme_cursor_pos -= 1;
        }
        true
    }

    /// Return the *byte* position of the cursor.
    pub fn cursor_pos(&self) -> usize {
        self.cursor_pos
    }

    /// Return the position of the cursor.
    pub fn grapheme_pos(&self) -> usize {
        self.grapheme_cursor_pos
    }

    /// Insert code point `k` in position `self.cursor_pos`
    ///
    /// Returns `false` if nothing was inserted.
    ///
    /// Before:
    ///
    /// ```text
    /// self.content = xxxxxx....xxxxxxx;
    ///                             ^
    ///                      self.cursor_pos
    /// ```
    ///
    /// After:
    ///
    /// ```text
    /// self.content = xxxxxx....xxxxkxxx;
    ///                              ^
    ///                       self.cursor_pos
    /// ```
    pub fn insert_char(&mut self, k: char) -> bool {
        if k.is_control() {
            return false;
        }

        self.content.insert(self.cursor_pos, k);
        self.cursor_pos += k.len_utf8();
        self.grapheme_cursor_pos += 1;
        true
    }

    /// Remove grapheme cluster that ends on `self.cursor_pos`.
    ///
    /// Before:
    ///
    /// ```text
    /// self.content = xxxxxx....xxggxxx;
    ///                             ^
    ///                      self.cursor_pos
    /// ```
    ///
    /// After:
    ///
    /// ```text
    /// self.content = xxxxxx....xxxxxx;
    ///                           ^
    ///                    self.cursor_pos
    /// ```
    pub fn backspace(&mut self) -> bool {
        if self.content.is_empty() {
            return false;
        }
        let (offset, graph_len) = {
            // Split string at cursor_pos:
            let (left, _) = self.content.split_at(self.cursor_pos);
            // left = xxxxxx....xxgg;
            // right = xxx;
            if let Some((offset, graph)) = left.last_grapheme() {
                (offset, graph.len())
            } else {
                return false;
            }
        };
        self.cursor_dec();

        self.content.drain(offset..offset + graph_len).count();
        true
    }

    pub fn cut_left(&mut self) {
        if self.content.is_empty() {
            return;
        }
        let offset = {
            let (left, _) = self.content.split_at(self.cursor_pos);
            left.last_grapheme()
                .map(|(offset, graph)| offset + graph.len())
                .unwrap_or(0)
        };
        self.cursor_pos = 0;
        self.grapheme_cursor_pos = 0;
        self.content.drain(..offset).count();
    }

    /// Transpose the two graphemes around the cursor.
    ///
    /// When given at the end of the text content, it transposes the last two
    /// graphemes on the line.
    pub fn transpose(&mut self) {
        if self.content.is_empty() {
            return;
        }
        if self.cursor_pos >= self.content.len() {
            // Last two graphemes.
            if let Some((i, _)) = self.content.last_grapheme() {
                if let Some((j, penult)) = self.content[..i].last_grapheme() {
                    let penult = penult.to_string();
                    self.content.drain(j..i);
                    self.content.push_str(&penult);
                }
            }
            return;
        }
        if let Some((i, next)) = self.content[..self.cursor_pos].last_grapheme() {
            let next = next.len();
            if let Some((j, prev)) = self.content[..self.cursor_pos][..i].last_grapheme() {
                let prev = prev.to_string();
                self.content.drain(j..i);
                self.content.insert_str(j + next, &prev);
                self.cursor_inc();
            }
        }
    }
}
