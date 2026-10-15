//
// meli
//
// Copyright 2017-2020, 2026 Manos Pitsidianakis
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::borrow::Cow;

use melib::utils::shellexpand::ShellExpandTrait as _;

use crate::{components::prelude::*, utilities::widgets::ScrollBar};

pub type AutoCompleteFn = Box<dyn Fn(&Context, &str) -> Vec<AutoCompleteEntry> + Send + Sync>;

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct AutoCompleteEntry {
    pub entry: String,
    pub description: Cow<'static, str>,
}

impl AutoCompleteEntry {
    pub fn as_str(&self) -> &str {
        self.entry.as_str()
    }
}

impl From<String> for AutoCompleteEntry {
    fn from(val: String) -> Self {
        Self {
            entry: val,
            description: Cow::Borrowed(""),
        }
    }
}

impl From<(String, &'static str)> for AutoCompleteEntry {
    fn from((entry, b): (String, &'static str)) -> Self {
        Self {
            entry,
            description: Cow::Borrowed(b),
        }
    }
}

impl From<(String, String)> for AutoCompleteEntry {
    fn from((entry, b): (String, String)) -> Self {
        Self {
            entry,
            description: Cow::Owned(b),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AutoComplete {
    entries: Vec<AutoCompleteEntry>,
    cursor: Option<usize>,
    reversed: bool,
    dirty: bool,
    id: ComponentId,
}

impl std::fmt::Display for AutoComplete {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt("AutoComplete", f)
    }
}

impl Component for AutoComplete {
    fn draw(&mut self, grid: &mut CellBuffer, mut area: Area, context: &mut Context) {
        if self.entries.is_empty() {
            return;
        };
        self.dirty = false;

        let rows = area.height();
        if rows == 0 {
            return;
        }
        context.dirty_areas.push_back(area);
        let top_idx = self
            .cursor
            .map(|c| c.wrapping_div(rows))
            .map(|page_no| page_no * rows)
            .unwrap_or_default();

        if top_idx + rows > self.entries.len() {
            if self.reversed {
                area = area.skip_rows(rows - (self.entries.len() - top_idx));
            } else {
                area = area.skip_rows_from_end(rows - (self.entries.len() - top_idx));
            }
        }
        let rows = area.height();

        let width = self
            .entries
            .iter()
            .skip(top_idx)
            .take(rows)
            .map(|a| a.entry.grapheme_len() + a.description.as_ref().grapheme_len() + 2)
            .max()
            .unwrap_or(0)
            + 1;
        let area = area.take_cols(width);
        let theme_attr = crate::conf::theme_value(context, "widgets.autocomplete");
        grid.clear_area(area, crate::conf::theme_value(context, "theme_default"));
        grid.change_theme(area, theme_attr);
        let mut rev_iter = self
            .entries
            .iter()
            .enumerate()
            .skip(top_idx)
            .take(rows)
            .rev();
        let mut normal_iter = self.entries.iter().enumerate().skip(top_idx).take(rows);
        let iter: &mut dyn Iterator<Item = (usize, &AutoCompleteEntry)> = if self.reversed {
            &mut rev_iter
        } else {
            &mut normal_iter
        };
        let highlight = crate::conf::theme_value(context, "highlight");
        for (row, (i, e)) in iter.enumerate() {
            let theme_attr = if Some(i) == self.cursor {
                grid.change_theme(area.nth_row(row), highlight);
                highlight
            } else {
                theme_attr
            };
            let (x, _) = grid.write_string(&e.entry, &theme_attr, area.nth_row(row), None, None);
            grid.write_string(
                e.description.as_ref(),
                &(theme_attr | Attr::ITALICS),
                area.nth_row(row).skip_cols(x + 2),
                None,
                None,
            );
        }

        if rows < self.entries.len() {
            let pos = if self.reversed {
                self.entries.len().saturating_sub(top_idx)
            } else {
                top_idx
            };
            ScrollBar { show_arrows: false }.draw(
                grid,
                area.skip_cols(area.width().saturating_sub(1)),
                context,
                if pos <= rows { 0 } else { pos },
                rows,
                self.entries.len(),
            );
        }
    }

    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

impl AutoComplete {
    pub fn new(entries: Vec<AutoCompleteEntry>) -> Box<Self> {
        let mut ret = Self {
            entries: Vec::new(),
            cursor: None,
            dirty: true,
            reversed: false,
            id: ComponentId::default(),
        };
        ret.set_suggestions(entries);
        Box::new(ret)
    }

    pub fn set_reversed(&mut self, reversed: bool) {
        self.reversed = reversed;
    }

    pub fn set_suggestions(&mut self, entries: Vec<AutoCompleteEntry>) -> bool {
        if entries.len() == self.entries.len() && entries == self.entries {
            return false;
        }

        self.entries = entries;
        self.cursor = None;
        true
    }

    pub fn inc_cursor(&mut self) {
        if let Some(ref mut cursor) = self.cursor {
            if *cursor + 1 < self.entries.len() {
                *cursor += 1;
                self.set_dirty(true);
            }
        } else if self.entries.is_empty() {
            self.cursor = None;
        } else {
            self.cursor = Some(0);
        }
    }
    pub fn dec_cursor(&mut self) {
        if let Some(ref mut cursor) = self.cursor {
            if *cursor == 0 {
                self.cursor = None;
            } else {
                *cursor -= 1;
            }
            self.set_dirty(true);
        }
    }

    pub fn cursor(&self) -> Option<usize> {
        let val = self.cursor?;
        if self.reversed {
            Some(self.entries.len().saturating_sub(val))
        } else {
            Some(val)
        }
    }

    pub fn get_suggestion(&mut self) -> Option<String> {
        let cursor = self.cursor.take()?;
        if self.entries.is_empty() {
            return None;
        }
        let ret = self.entries.remove(cursor);
        self.entries.clear();
        Some(ret.entry)
    }

    /// Generate a completion to the common prefix of all suggestions.
    pub fn tab(&self) -> Option<String> {
        // https://users.rust-lang.org/t/how-to-find-common-prefix-of-two-byte-slices-effectively/25815/4

        fn mismatch(xs: &[u8], ys: &[u8]) -> usize {
            mismatch_chunks::<128>(xs, ys)
        }

        fn mismatch_chunks<const N: usize>(xs: &[u8], ys: &[u8]) -> usize {
            let off = std::iter::zip(xs.as_chunks::<N>().0, ys.as_chunks::<N>().0)
                .take_while(|(x, y)| x == y)
                .count()
                * N;
            off + std::iter::zip(&xs[off..], &ys[off..])
                .take_while(|(x, y)| x == y)
                .count()
        }

        if self.entries.is_empty() {
            return None;
        }
        if self.entries.len() == 1 {
            return Some(self.entries[0].entry.clone());
        }
        let mut entries = self.entries.clone();
        entries.sort_by(|a, b| a.entry.cmp(&b.entry));
        let mismatch = mismatch(
            entries[0].entry.as_bytes(),
            entries[entries.len() - 1].entry.as_bytes(),
        );

        Some(entries[0].entry[..mismatch].to_string())
    }

    pub fn suggestions(&self) -> &Vec<AutoCompleteEntry> {
        &self.entries
    }
}

#[inline]
pub fn generic_file_path_complete_fn() -> AutoCompleteFn {
    Box::new(move |_, term| {
        use crate::command::parser::Lexer;

        let mut lexer = Lexer::new(term);
        let Some(input) = lexer.next() else {
            return vec![];
        };
        if lexer.next().is_some() {
            return vec![];
        }
        match input {
            Ok(input) => std::path::Path::new(input.value())
                .complete(true, input.value().ends_with('/'))
                .into_iter()
                .take(100)
                .map(|m| {
                    format!(
                        "{input}{path}",
                        input = term.strip_suffix(input.raw()).unwrap(),
                        path = input.append(&m)
                    )
                })
                .map(AutoCompleteEntry::from)
                .collect::<Vec<AutoCompleteEntry>>(),
            Err(lex_token_err) => {
                if let (Some(raw), Some(value)) = (lex_token_err.raw(), lex_token_err.value()) {
                    std::path::Path::new(value)
                        .complete(true, value.ends_with('/'))
                        .into_iter()
                        .take(100)
                        .map(|m| {
                            if lex_token_err.is_quoted() || m.contains('"') || m.contains(' ') {
                                format!(
                                    "{input}\"{value}{m}\"",
                                    input = term.strip_suffix(raw).unwrap(),
                                    value = value,
                                    m = m.replace('"', "\\\""),
                                )
                            } else {
                                format!(
                                    "{input}{value}{m}",
                                    input = term.strip_suffix(raw).unwrap(),
                                    value = value,
                                )
                            }
                        })
                        .map(AutoCompleteEntry::from)
                        .collect::<Vec<AutoCompleteEntry>>()
                } else {
                    vec![]
                }
            }
        }
    })
}
