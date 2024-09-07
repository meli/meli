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

//! Text input widgets.

use std::sync::Arc;

use super::*;
use crate::melib::text::Truncate;

/// Text input widget.
#[derive(Clone)]
pub struct TextField {
    inner: UText,
    autocomplete: Option<(Arc<AutoCompleteFn>, Box<AutoComplete>)>,
    theme_attr: Option<ThemeAttribute>,
    highlight: Option<ThemeAttribute>,
    id: ComponentId,
}

impl std::fmt::Debug for TextField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(TextField))
            .field("id", &self.id)
            .field("inner", &self.inner)
            .field("has AutoComplete", &self.autocomplete.is_some())
            .finish_non_exhaustive()
    }
}

impl Default for TextField {
    fn default() -> Self {
        Self {
            inner: UText::new(String::with_capacity(256)),
            autocomplete: None,
            theme_attr: None,
            highlight: None,
            id: ComponentId::default(),
        }
    }
}

impl TextField {
    pub fn new(inner: UText, autocomplete: Option<(AutoCompleteFn, Box<AutoComplete>)>) -> Self {
        Self {
            inner,
            autocomplete: autocomplete.map(|(a, b)| (Arc::new(a), b)),
            theme_attr: None,
            highlight: None,
            id: ComponentId::default(),
        }
    }

    /// Text content as a string slice.
    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    /// Reset with new content.
    pub fn set_content(&mut self, new_val: String) {
        self.inner.set_content(new_val);
    }

    /// Cursor position as seen by the user: the cursor is seen moving by
    /// jumping between graphemes boundaries.
    pub fn cursor(&self) -> usize {
        self.inner.grapheme_pos()
    }

    /// Byte cursor position meant to be used for accessing the inner string
    /// slice.
    pub fn byte_cursor(&self) -> usize {
        self.inner.cursor_pos()
    }

    /// Increase the cursor position to the next grapheme boundary.
    pub fn cursor_inc(&mut self) {
        self.inner.cursor_inc();
    }

    /// Decrease the cursor position to the previous grapheme boundary.
    pub fn cursor_dec(&mut self) {
        self.inner.cursor_dec();
    }

    /// Returns `true` if inner text is empty.
    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    /// Consume self and return the inner text as an owned string.
    pub fn into_string(self) -> String {
        self.inner.into_string()
    }

    /// Clear inner string content and set cursor to zero.
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    /// Draw cursor at given area along with autocomplete suggestions, if any.
    pub fn draw_cursor(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        secondary_area: Area,
        context: &mut Context,
    ) {
        let width = area.width();
        let pos = if width < self.inner.grapheme_pos() {
            width
        } else {
            self.inner.grapheme_pos()
        };

        let highlight_attr = self.highlight.unwrap_or_else(|| {
            let val = crate::conf::value(context, "highlight");
            self.highlight = Some(val);
            val
        });
        grid.change_colors(
            area.skip_cols(pos).take_cols(1),
            highlight_attr.fg,
            highlight_attr.bg,
        );
        if self.inner.grapheme_len() <= 2 {
            return;
        }
        if let Some((autocomplete_fn, autocomplete)) = self.autocomplete.as_mut() {
            let entries = autocomplete_fn(context, self.inner.as_str());
            autocomplete.set_suggestions(entries);
            autocomplete.draw(grid, secondary_area, context);
        }
    }
}

impl Component for TextField {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let theme_attr = self.theme_attr.unwrap_or_else(|| {
            let val = crate::conf::value(context, "widgets.form.field");
            self.theme_attr = Some(val);
            val
        });
        let width = area.width();
        let str = self.as_str();
        /* Calculate which part of the str is visible
         * ##########################################
         *
         * Example:
         * For the string "The quick brown fox jumps over the lazy dog" with visible
         * width of field of 10 columns
         *
         *
         * Cursor <= width
         * =================
         * Cursor at:
         * ⇩
         * The quick brown fox jumps over the lazy dog
         *
         * cursor
         * ⇩
         * ┌──────────┐
         * │The quick │ brown fox jumps over the lazy dog
         * └──────────┘
         *
         * No skip.
         *
         * Cursor at the end
         * =================
         * Cursor at:
         *                                           ⇩
         * The quick brown fox jumps over the lazy dog
         *
         * remainder                                        cursor
         * ⇩⇩⇩⇩⇩                                              ⇩
         * +╌╌╌+╭┅┅┅┅┅┅┅┅┅┅╮╭┅┅┅┅┅┅┅┅┅┅╮╭┅┅┅┅┅┅┅┅┅┅╮┌──────────┐
         * |The|┊ quick bro┊┊wn fox jum┊┊ps over th┊│e lazy dog│
         * +╌╌╌+╰┅┅┅┅┅┅┅┅┅┅╯╰┅┅┅┅┅┅┅┅┅┅╯╰┅┅┅┅┅┅┅┅┅┅╯└──────────┘
         *  ⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧
         *              skip offset
         *
         * Intermediate cursor
         * ===================
         * Cursor at:
         *                               ⇩
         * The quick brown fox jumps over the lazy dog
         *
         * remainder                        cursor
         * ⇩                                  ⇩
         * +╭┅┅┅┅┅┅┅┅┅┅╮╭┅┅┅┅┅┅┅┅┅┅╮┌──────────┐
         * T|he quick b┊┊rown fox j┊│umps over │ the lazy dog
         * +╰┅┅┅┅┅┅┅┅┅┅╯╰┅┅┅┅┅┅┅┅┅┅╯└──────────┘
         * ⇧+⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧++⇧⇧⇧⇧⇧⇧⇧⇧⇧⇧
         *              skip offset
         */
        grid.write_string(
            if width < self.inner.grapheme_pos() {
                str.trim_left_at_boundary(
                    width
                        * self
                            .inner
                            .grapheme_pos()
                            .wrapping_div(width)
                            .saturating_sub(1)
                        + self.inner.grapheme_pos().wrapping_rem(width),
                )
            } else {
                str
            },
            theme_attr.fg,
            theme_attr.bg,
            theme_attr.attrs,
            area,
            None,
            None,
        );
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match *event {
            UIEvent::InsertInput(Key::Char('\t')) => {
                if let Some(suggestion) = self
                    .autocomplete
                    .as_mut()
                    .and_then(|a| a.1.get_suggestion())
                {
                    self.inner = UText::new(suggestion);
                    let len = self.inner.as_str().len();
                    self.inner.set_cursor(len);
                } else {
                    self.inner.insert_char(' ');
                }
            }
            UIEvent::InsertInput(Key::Char('\n')) => {
                if let Some(suggestion) = self
                    .autocomplete
                    .as_mut()
                    .and_then(|a| a.1.get_suggestion())
                {
                    self.inner = UText::new(suggestion);
                    let len = self.inner.as_str().len();
                    self.inner.set_cursor(len);
                }
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
            }
            UIEvent::InsertInput(Key::Up) => {
                if let Some(ac) = self.autocomplete.as_mut() {
                    ac.1.dec_cursor();
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Down) => {
                if let Some(ac) = self.autocomplete.as_mut() {
                    ac.1.inc_cursor();
                } else {
                    return false;
                }
            }
            UIEvent::InsertInput(Key::Right) => {
                self.inner.cursor_inc();
            }
            UIEvent::InsertInput(Key::Left) => {
                self.inner.cursor_dec();
            }
            UIEvent::InsertInput(Key::Char(k)) => {
                self.inner.insert_char(k);
            }
            UIEvent::InsertInput(Key::Paste(ref p)) => {
                for c in p.chars() {
                    self.inner.insert_char(c);
                }
            }
            UIEvent::InsertInput(Key::Alt('b')) => {
                // Meta+B Backward one alphanumeric word

                let initial = self.inner.cursor_pos();
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .chars()
                    .last()
                    .map(|c| c.is_ascii_whitespace())
                    .unwrap_or_default()
                {
                    if !self.inner.cursor_dec() {
                        break;
                    }
                }
                if self.inner.cursor_pos() == initial {
                    // We didn't skip whitespace to reach the end of the previous word, so skip
                    // nonwhitespace instead.
                    while self.inner.as_str()[..self.inner.cursor_pos()]
                        .chars()
                        .last()
                        .map(|c| !c.is_ascii_whitespace())
                        .unwrap_or_default()
                    {
                        if !self.inner.cursor_dec() {
                            break;
                        }
                    }
                }
            }
            UIEvent::InsertInput(Key::Alt('f')) => {
                // Meta+F Forward one alphanumeric word (Move to its end)

                let initial = self.inner.cursor_pos();
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .chars()
                    .last()
                    .map(|c| c.is_ascii_whitespace())
                    .unwrap_or_default()
                {
                    if !self.inner.cursor_inc() {
                        break;
                    }
                }
                if self.inner.cursor_pos() == initial {
                    // We didn't skip whitespace to reach the start of the next word, so skip
                    // nonwhitespace instead.
                    while self.inner.as_str()[..self.inner.cursor_pos()]
                        .chars()
                        .last()
                        .map(|c| !c.is_ascii_whitespace())
                        .unwrap_or_default()
                    {
                        if !self.inner.cursor_inc() {
                            break;
                        }
                    }
                } else {
                    // self.inner.cursor_dec();
                }
            }
            UIEvent::InsertInput(Key::Backspace) | UIEvent::InsertInput(Key::Ctrl('h')) => {
                // Ctrl+H  Delete previous character
                if self.inner.backspace() {
                    if let Some(ac) = self.autocomplete.as_mut() {
                        ac.1.set_suggestions(Vec::new());
                    }
                }
            }
            UIEvent::InsertInput(Key::Delete) | UIEvent::InsertInput(Key::Ctrl('d')) => {
                // Delete one character
                if self.inner.cursor_inc() {
                    self.inner.backspace();
                }
            }
            UIEvent::InsertInput(Key::Ctrl('t')) => {
                // Ctrl+T Transpose characters
                self.inner.transpose();
            }
            UIEvent::InsertInput(Key::Ctrl('a')) | UIEvent::InsertInput(Key::Home) => {
                // Beginning of line
                self.inner.set_cursor(0);
            }
            UIEvent::InsertInput(Key::Ctrl('b')) => {
                // Backward one character
                self.inner.cursor_dec();
            }
            UIEvent::InsertInput(Key::Ctrl('f')) => {
                // Ctrl+F / → Forward one character
                self.inner.cursor_inc();
            }
            UIEvent::InsertInput(Key::Ctrl('w')) => {
                // Cut previous word
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .last_grapheme()
                    .map(|(_, graph)| !graph.is_empty() && graph.trim().is_empty())
                    .unwrap_or(false)
                {
                    if !self.inner.backspace() {
                        break;
                    }
                }
                while self.inner.as_str()[..self.inner.cursor_pos()]
                    .last_grapheme()
                    .map(|(_, graph)| !graph.is_empty() && !graph.trim().is_empty())
                    .unwrap_or(false)
                {
                    if !self.inner.backspace() {
                        break;
                    }
                }
            }
            UIEvent::InsertInput(Key::Ctrl('u')) => self.inner.cut_left(),
            UIEvent::InsertInput(Key::Ctrl('e')) | UIEvent::InsertInput(Key::End) => {
                // Ctrl+E End of line
                self.inner.set_cursor(self.inner.as_str().len());
            }
            _ => {
                return false;
            }
        }
        self.set_dirty(true);
        true
    }

    fn is_dirty(&self) -> bool {
        false
    }

    fn set_dirty(&mut self, _value: bool) {}

    fn id(&self) -> ComponentId {
        self.id
    }
}

impl std::fmt::Display for TextField {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.as_str())
    }
}
