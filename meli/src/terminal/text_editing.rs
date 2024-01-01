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

use melib::text::TextProcessing;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct UText {
    content: String,
    cursor_pos: usize,
    grapheme_cursor_pos: usize,
}

impl UText {
    pub fn new(content: String) -> Self {
        Self {
            cursor_pos: content.len(),
            grapheme_cursor_pos: content.split_graphemes().len(),
            content,
        }
    }

    pub fn set_cursor(&mut self, cursor_pos: usize) {
        if cursor_pos > self.content.len() {
            return;
        }

        let (first, _) = self.content.split_at(cursor_pos);
        self.grapheme_cursor_pos = first.split_graphemes().len();
        self.cursor_pos = cursor_pos;
    }

    pub fn as_str(&self) -> &str {
        self.content.as_str()
    }

    pub fn clear(&mut self) {
        self.content.clear();
        self.cursor_pos = 0;
        self.grapheme_cursor_pos = 0;
    }

    pub fn into_string(self) -> String {
        self.content
    }
    pub fn grapheme_len(&self) -> usize {
        self.content.split_graphemes().len()
    }

    pub fn cursor_inc(&mut self) {
        if self.cursor_pos >= self.content.len() {
            return;
        }

        let (_, right) = self.content.split_at(self.cursor_pos);
        if let Some((_, graph)) = right.next_grapheme() {
            self.cursor_pos += graph.len();
            self.grapheme_cursor_pos += 1;
        }
    }
    pub fn cursor_dec(&mut self) {
        if self.cursor_pos == 0 {
            return;
        }
        let (left, _) = self.content.split_at(self.cursor_pos);
        if let Some((_, graph)) = left.last_grapheme() {
            self.cursor_pos -= graph.len();
            self.grapheme_cursor_pos -= 1;
        }
    }

    pub fn cursor_pos(&self) -> usize {
        self.cursor_pos
    }

    pub fn grapheme_pos(&self) -> usize {
        self.grapheme_cursor_pos
    }

    /*
     * Insert code point `k` in position `self.cursor_pos`:
     *
     * before:
     *
     * self.content = xxxxxx....xxxxxxx;
     *                             ^
     *                      self.cursor_pos
     *
     * after:
     *
     * self.content = xxxxxx....xxxxkxxx;
     *                              ^
     *                       self.cursor_pos
     */
    pub fn insert_char(&mut self, k: char) {
        if k.is_control() {
            return;
        }

        self.content.insert(self.cursor_pos, k);
        self.cursor_pos += k.len_utf8();
        self.grapheme_cursor_pos += 1;
    }

    /*
     * remove grapheme cluster that ends on `self.cursor_pos`:
     *
     * before:
     *
     * self.content = xxxxxx....xxggxxx;
     *                             ^
     *                      self.cursor_pos
     *
     * after:
     *
     * self.content = xxxxxx....xxxxxx;
     *                           ^
     *                    self.cursor_pos
     */
    pub fn backspace(&mut self) {
        if self.content.is_empty() {
            return;
        }
        let (offset, graph_len) = {
            /*
             * Split string at cursor_pos:
             */
            let (left, _) = self.content.split_at(self.cursor_pos);
            /*
             * left = xxxxxx....xxgg;
             * right = xxx;
             */
            if let Some((offset, graph)) = left.last_grapheme() {
                (offset, graph.len())
            } else {
                return;
            }
        };
        self.cursor_dec();

        self.content.drain(offset..offset + graph_len).count();
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
}
