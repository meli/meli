/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

//! Define a `(x, y)` point in the terminal display as a holder of a character,
//! foreground/background colors and attributes.

use std::{
    collections::HashMap,
    convert::From,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use melib::{
    log,
    text::{search::KMP, wcwidth, TextPresentation},
};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use super::{Area, Color, Pos, ScreenGeneration};
use crate::{state::Context, ThemeAttribute};

/// In a scroll region up and down cursor movements shift the region vertically.
/// The new lines are empty.
///
/// See `CellBuffer::scroll_up` and `CellBuffer::scroll_down` for an explanation
/// of how `xterm` scrolling works.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ScrollRegion {
    pub top: usize,
    pub bottom: usize,
    pub left: usize,
    pub right: usize,
}

/// An array of `Cell`s that represents a terminal display.
///
/// A `CellBuffer` is a two-dimensional array of `Cell`s, each pair of indices
/// correspond to a single point on the underlying terminal.
///
/// The first index, `Cellbuffer[y]`, corresponds to a row, and thus the y-axis.
/// The second index, `Cellbuffer[y][x]`, corresponds to a column within a row
/// and thus the x-axis.
#[derive(Clone, Eq, PartialEq)]
pub struct CellBuffer {
    pub cols: usize,
    pub rows: usize,
    pub buf: Vec<Cell>,
    pub default_cell: Cell,
    /// ASCII-only flag.
    pub ascii_drawing: bool,
    /// Force text presentation for emojis.
    pub force_text_presentation: bool,
    /// Use color.
    pub use_color: bool,
    pub tab_width: u8,
    /// If printing to this buffer and we run out of space, expand it.
    growable: bool,
    tag_table: HashMap<u64, FormatTag>,
    tag_associations: SmallVec<[(u64, (usize, usize)); 128]>,
    pub(super) area: Area,
}

impl std::fmt::Debug for CellBuffer {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(CellBuffer))
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("buf cells", &self.buf.len())
            .field("default_cell", &self.default_cell)
            .field("ascii_drawing", &self.ascii_drawing)
            .field("force_text_presentation", &self.force_text_presentation)
            .field("use_color", &self.use_color)
            .field("tab_width", &self.tab_width)
            .field("growable", &self.growable)
            .field("tag_table", &self.tag_table)
            .field("tag_associations", &self.tag_associations)
            .field("area", &self.area)
            .field("generation", &self.area.generation())
            .finish()
    }
}

impl CellBuffer {
    pub const MAX_SIZE: usize = 100_000_000;

    pub fn nil(area: Area) -> Self {
        Self {
            cols: 0,
            rows: 0,
            buf: vec![],
            default_cell: Cell::new_default(),
            growable: false,
            ascii_drawing: false,
            force_text_presentation: false,
            use_color: false,
            tab_width: 4,
            tag_table: Default::default(),
            tag_associations: SmallVec::new(),
            area,
        }
    }

    pub fn set_cols(&mut self, new_cols: usize) {
        self.cols = new_cols;
    }

    /// Constructs a new `CellBuffer` with the given number of columns and rows,
    /// using the given `cell` as a blank.
    pub fn new(default_cell: Cell, area: Area) -> Self {
        let cols = area.width();
        let rows = area.height();
        Self {
            cols,
            rows,
            buf: vec![default_cell; cols * rows],
            default_cell,
            growable: false,
            ascii_drawing: false,
            force_text_presentation: false,
            use_color: true,
            tab_width: 4,
            tag_table: Default::default(),
            tag_associations: SmallVec::new(),
            area,
        }
    }

    pub fn new_with_context(default_cell: Option<Cell>, area: Area, context: &Context) -> Self {
        let default_cell = default_cell.unwrap_or_else(|| {
            let mut ret = Cell::default();
            let theme_default = crate::conf::value(context, "theme_default");
            ret.set_fg(theme_default.fg)
                .set_bg(theme_default.bg)
                .set_attrs(theme_default.attrs);
            ret
        });
        Self {
            ascii_drawing: context.settings.terminal.ascii_drawing,
            force_text_presentation: context.settings.terminal.use_text_presentation(),
            use_color: context.settings.terminal.use_color(),
            ..Self::new(default_cell, area)
        }
    }

    pub fn set_force_text_presentation(&mut self, new_val: bool) -> &mut Self {
        self.force_text_presentation = new_val;
        self
    }

    pub fn set_ascii_drawing(&mut self, new_val: bool) {
        self.ascii_drawing = new_val;
    }

    pub fn set_use_color(&mut self, new_val: bool) {
        self.use_color = new_val;
    }

    pub fn set_tab_width(&mut self, new_val: u8) {
        self.tab_width = new_val;
    }

    pub fn set_growable(&mut self, new_val: bool) {
        self.growable = new_val;
    }

    /// Resizes `CellBuffer` to the given number of rows and columns, using the
    /// given `Cell` as a blank.
    #[must_use]
    pub(super) fn resize_with_context(
        &mut self,
        newcols: usize,
        newrows: usize,
        context: &Context,
    ) -> bool {
        self.default_cell = {
            let mut ret = Cell::default();
            let theme_default = crate::conf::value(context, "theme_default");
            ret.set_fg(theme_default.fg)
                .set_bg(theme_default.bg)
                .set_attrs(theme_default.attrs);
            ret
        };
        self.ascii_drawing = context.settings.terminal.ascii_drawing;
        self.use_color = context.settings.terminal.use_color();
        self.force_text_presentation = context.settings.terminal.use_text_presentation();

        let newlen = newcols * newrows;
        if (self.cols, self.rows) == (newcols, newrows) || newlen >= Self::MAX_SIZE {
            return newlen < Self::MAX_SIZE;
        }

        self.buf = vec![self.default_cell; newlen];
        self.cols = newcols;
        self.rows = newrows;
        true
    }

    /// Resizes `CellBuffer` to the given number of rows and columns, using the
    /// given `Cell` as a blank.
    #[must_use]
    pub(super) fn resize(&mut self, newcols: usize, newrows: usize, blank: Option<Cell>) -> bool {
        let newlen = newcols * newrows;
        if (self.cols, self.rows) == (newcols, newrows) || newlen >= Self::MAX_SIZE {
            return newlen < Self::MAX_SIZE;
        }

        let blank = blank.unwrap_or(self.default_cell);
        let oldbuf = std::mem::replace(&mut self.buf, vec![blank; newlen]);
        let (oldcols, oldrows) = (self.cols, self.rows);
        for y in 0..oldrows.min(newrows) {
            let row_length = oldcols.min(newcols);
            self.buf[y * newcols..(y * newcols + row_length)]
                .copy_from_slice(&oldbuf[y * oldcols..(y * oldcols + row_length)]);
        }
        self.cols = newcols;
        self.rows = newrows;
        true
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn empty(&mut self) {
        self.buf.clear();
        self.cols = 0;
        self.rows = 0;
    }

    /// Clears `self`, using the given `Cell` as a blank.
    pub fn clear(&mut self, blank: Option<Cell>) {
        let blank = blank.unwrap_or(self.default_cell);
        for cell in self.cellvec_mut().iter_mut() {
            *cell = blank;
        }
    }

    pub fn pos_to_index(&self, x: usize, y: usize) -> Option<usize> {
        let (cols, rows) = self.size();
        if x < cols && y < rows {
            Some((cols * y) + x)
        } else {
            None
        }
    }

    /// Returns a reference to the `Cell` at the given coordinates, or `None` if
    /// the index is out of bounds.
    pub fn get(&self, x: usize, y: usize) -> Option<&Cell> {
        match self.pos_to_index(x, y) {
            Some(i) => self.cellvec().get(i),
            None => None,
        }
    }

    /// Returns a mutable reference to the `Cell` at the given coordinates, or
    /// `None` if the index is out of bounds.
    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut Cell> {
        match self.pos_to_index(x, y) {
            Some(i) => self.cellvec_mut().get_mut(i),
            None => None,
        }
    }

    pub fn size(&self) -> (usize, usize) {
        (self.cols, self.rows)
    }

    pub fn cellvec(&self) -> &Vec<Cell> {
        &self.buf
    }

    pub fn cellvec_mut(&mut self) -> &mut Vec<Cell> {
        &mut self.buf
    }

    pub fn cols(&self) -> usize {
        self.size().0
    }

    pub fn rows(&self) -> usize {
        self.size().1
    }

    #[inline(always)]
    /// Performs the normal scroll up motion:
    ///
    /// First clear offset number of lines:
    ///
    /// For offset = 1, top = 1:
    ///
    ///  ```text
    ///  | 111111111111 |            |              |
    ///  | 222222222222 |            | 222222222222 |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            | 555555555555 |
    ///  | 666666666666 |            | 666666666666 |
    ///  ```
    ///
    ///  In each step, swap the current line with the next by offset:
    ///
    ///  ```text
    ///  |              |            | 222222222222 |
    ///  | 222222222222 |            |              |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            | 555555555555 |
    ///  | 666666666666 |            | 666666666666 |
    ///  ```
    ///
    ///  Result:
    ///  ```text
    ///    Before                      After
    ///  | 111111111111 |            | 222222222222 |
    ///  | 222222222222 |            | 333333333333 |
    ///  | 333333333333 |            | 444444444444 |
    ///  | 444444444444 |            | 555555555555 |
    ///  | 555555555555 |            | 666666666666 |
    ///  | 666666666666 |            |              |
    ///  ```
    pub fn scroll_up(&mut self, scroll_region: &ScrollRegion, top: usize, offset: usize) {
        let l = scroll_region.left;
        let r = if scroll_region.right == 0 {
            self.size().0
        } else {
            scroll_region.right
        };
        for y in top..top + offset {
            for x in l..r {
                self[(x, y)] = Cell::default();
            }
        }
        for y in top..=(scroll_region.bottom - offset) {
            for x in l..r {
                let temp = self[(x, y)];
                self[(x, y)] = self[(x, y + offset)];
                self[(x, y + offset)] = temp;
            }
        }
    }

    #[inline(always)]
    /// Performs the normal scroll down motion:
    ///
    /// First clear offset number of lines:
    ///
    /// For offset = 1, top = 1:
    ///
    ///  ```text
    ///  | 111111111111 |            | 111111111111 |
    ///  | 222222222222 |            | 222222222222 |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            | 555555555555 |
    ///  | 666666666666 |            |              |
    ///  ```
    ///
    ///  In each step, swap the current line with the prev by offset:
    ///
    ///  ```text
    ///  | 111111111111 |            | 111111111111 |
    ///  | 222222222222 |            | 222222222222 |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            |              |
    ///  |              |            | 555555555555 |
    ///  ```
    ///
    ///  Result:
    ///  ```text
    ///    Before                      After
    ///  | 111111111111 |            |              |
    ///  | 222222222222 |            | 111111111111 |
    ///  | 333333333333 |            | 222222222222 |
    ///  | 444444444444 |            | 333333333333 |
    ///  | 555555555555 |            | 444444444444 |
    ///  | 666666666666 |            | 555555555555 |
    ///  ```
    pub fn scroll_down(&mut self, scroll_region: &ScrollRegion, top: usize, offset: usize) {
        for y in (scroll_region.bottom - offset + 1)..=scroll_region.bottom {
            for x in 0..self.size().0 {
                self[(x, y)] = Cell::default();
            }
        }

        for y in ((top + offset)..=scroll_region.bottom).rev() {
            for x in 0..self.size().0 {
                let temp = self[(x, y)];
                self[(x, y)] = self[(x, y - offset)];
                self[(x, y - offset)] = temp;
            }
        }
    }

    /// See `BoundsIterator` documentation.
    pub fn bounds_iter(&self, area: Area) -> BoundsIterator {
        debug_assert_eq!(self.generation(), area.generation());
        if area.is_empty() {
            return BoundsIterator::empty(self.generation());
        }

        #[inline(always)]
        fn get_x(p: Pos) -> usize {
            p.0
        }

        #[inline(always)]
        fn get_y(p: Pos) -> usize {
            p.1
        }

        BoundsIterator {
            width: area.width(),
            height: area.height(),
            rows: std::cmp::min(self.rows.saturating_sub(1), get_y(area.upper_left()))
                ..(std::cmp::min(self.rows, get_y(area.bottom_right()) + 1)),
            cols: (
                std::cmp::min(self.cols.saturating_sub(1), get_x(area.upper_left())),
                std::cmp::min(self.cols, get_x(area.bottom_right()) + 1),
            ),
            area,
        }
    }

    /// See `RowIterator` documentation.
    pub fn row_iter(
        &self,
        area: Area,
        bounds: std::ops::Range<usize>,
        relative_row: usize,
    ) -> RowIterator {
        debug_assert_eq!(self.generation(), area.generation());
        if self.generation() != area.generation() {
            return RowIterator::empty(self.generation());
        }
        let row = area.offset().1 + relative_row;

        if row < self.rows && !area.is_empty() {
            let col = std::cmp::min(self.cols.saturating_sub(1), area.offset().0 + bounds.start)
                ..(std::cmp::min(self.cols, area.offset().0 + bounds.end));
            let area = area
                .nth_row(relative_row)
                .skip_cols(bounds.start)
                .take_cols(bounds.len());
            RowIterator { row, col, area }
        } else {
            RowIterator::empty(self.generation())
        }
    }

    pub fn tag_associations(&self) -> SmallVec<[(usize, u64, bool); 128]> {
        let mut ret: SmallVec<[(usize, u64, bool); 128]> = self.tag_associations.iter().fold(
            SmallVec::new(),
            |mut acc, (tag_hash, (start, end))| {
                acc.push((*start, *tag_hash, true));
                acc.push((*end, *tag_hash, false));
                acc
            },
        );
        ret.sort_by_key(|el| el.0);
        ret
    }

    pub fn tag_table(&self) -> &HashMap<u64, FormatTag> {
        &self.tag_table
    }

    pub fn tag_table_mut(&mut self) -> &mut HashMap<u64, FormatTag> {
        &mut self.tag_table
    }

    pub fn insert_tag(&mut self, tag: FormatTag) -> u64 {
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash, Hasher},
        };

        let mut hasher = DefaultHasher::new();
        tag.hash(&mut hasher);
        let hash = hasher.finish();
        self.tag_table.insert(hash, tag);
        hash
    }

    pub fn set_tag(&mut self, tag: u64, start: (usize, usize), end: (usize, usize)) {
        let start = self
            .pos_to_index(start.0, start.1)
            .unwrap_or_else(|| self.buf.len().saturating_sub(1));
        let end = self
            .pos_to_index(end.0, end.1)
            .unwrap_or_else(|| self.buf.len().saturating_sub(1));
        if start != end {
            self.tag_associations.push((tag, (start, end)));
        }
    }

    #[inline(always)]
    pub fn generation(&self) -> ScreenGeneration {
        self.area.generation()
    }

    /// Completely clear an `Area` with an empty char and the terminal's default
    /// colors.
    pub fn clear_area(&mut self, area: Area, attributes: ThemeAttribute) {
        for row in self.bounds_iter(area) {
            for c in row {
                self[c] = Cell::default();
                self[c]
                    .set_fg(attributes.fg)
                    .set_bg(attributes.bg)
                    .set_attrs(attributes.attrs);
            }
        }
    }

    /// Change foreground and background colors in an `Area`
    pub fn change_colors(&mut self, area: Area, fg_color: Color, bg_color: Color) {
        for row in self.bounds_iter(area) {
            for c in row {
                self[c].set_fg(fg_color).set_bg(bg_color);
            }
        }
    }

    /// Change [`ThemeAttribute`] in an `Area`
    pub fn change_theme(&mut self, area: Area, theme: ThemeAttribute) {
        for row in self.bounds_iter(area) {
            for c in row {
                self[c]
                    .set_fg(theme.fg)
                    .set_bg(theme.bg)
                    .set_attrs(theme.attrs);
            }
        }
    }

    /// Copy a source `Area` to a destination.
    pub fn copy_area(&mut self, grid_src: &Self, dest: Area, src: Area) -> Pos {
        debug_assert_eq!(self.generation(), dest.generation());
        debug_assert_eq!(grid_src.generation(), src.generation());
        if self.generation() != dest.generation() || grid_src.generation() != src.generation() {
            log::debug!(
                "BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}",
                src,
                dest
            );
            return dest.upper_left();
        }

        if grid_src.is_empty() || self.is_empty() || dest.is_empty() || src.is_empty() {
            return dest.upper_left();
        }

        #[inline(always)]
        fn get_x(p: Pos) -> usize {
            p.0
        }

        #[inline(always)]
        fn get_y(p: Pos) -> usize {
            p.1
        }

        let mut ret = dest.bottom_right();
        let mut src_x = get_x(src.upper_left());
        let mut src_y = get_y(src.upper_left());
        let (cols, rows) = grid_src.size();
        if src_x >= cols || src_y >= rows {
            log::debug!("BUG: src area outside of grid_src in copy_area",);
            return dest.upper_left();
        }

        let tag_associations = grid_src.tag_associations();
        let start_idx = grid_src.pos_to_index(src_x, src_y).unwrap();
        let mut tag_offset: usize = tag_associations
            .binary_search_by(|probe| probe.0.cmp(&start_idx))
            .unwrap_or_else(|i| i);
        let mut stack: std::collections::BTreeSet<&FormatTag> =
            std::collections::BTreeSet::default();
        for y in get_y(dest.upper_left())..=get_y(dest.bottom_right()) {
            'for_x: for x in get_x(dest.upper_left())..=get_x(dest.bottom_right()) {
                let idx = grid_src.pos_to_index(src_x, src_y).unwrap();
                while tag_offset < tag_associations.len() && tag_associations[tag_offset].0 <= idx {
                    if tag_associations[tag_offset].2 {
                        stack.insert(&grid_src.tag_table()[&tag_associations[tag_offset].1]);
                    } else {
                        stack.remove(&grid_src.tag_table()[&tag_associations[tag_offset].1]);
                    }
                    tag_offset += 1;
                }
                self[(x, y)] = grid_src[(src_x, src_y)];
                for t in &stack {
                    if let Some(fg) = t.fg {
                        self[(x, y)].set_fg(fg).set_keep_fg(true);
                    }
                    if let Some(bg) = t.bg {
                        self[(x, y)].set_bg(bg).set_keep_bg(true);
                    }
                    if let Some(attrs) = t.attrs {
                        self[(x, y)].attrs |= attrs;
                        self[(x, y)].set_keep_attrs(true);
                    }
                }
                if src_x >= get_x(src.bottom_right()) {
                    break 'for_x;
                }
                src_x += 1;
            }
            src_x = get_x(src.upper_left());
            src_y += 1;
            if src_y > get_y(src.bottom_right()) {
                for row in self.bounds_iter(dest.skip_rows(y + 1 - get_y(dest.upper_left()))) {
                    for c in row {
                        self[c].set_ch(' ');
                    }
                }
                ret.1 = y;
                break;
            }
        }
        ret
    }

    #[allow(clippy::too_many_arguments)]
    /// Write an `&str` to a `CellBuffer` in a specified `Area` with the passed
    /// colors.
    pub fn write_string(
        &mut self,
        s: &str,
        fg_color: Color,
        bg_color: Color,
        attrs: Attr,
        og_area: Area,
        skip_cols: Option<usize>,
        // The left-most x coordinate.
        line_break: Option<usize>,
    ) -> Pos {
        let skip_cols = skip_cols.unwrap_or(0);
        let area = og_area.skip_cols(skip_cols);

        debug_assert_eq!(area.generation(), self.generation());
        if area.generation() != self.generation() {
            let backtrace = std::backtrace::Backtrace::force_capture();
            log::error!(
                "BUG: write_string() received an area argument of generation {} but the \
                 CellBuffer has generation of {}.",
                area.generation(),
                self.generation()
            );
            log::error!(
                "BUG: Please report this.\nString was: {:?}.\nArea was: {:?}\nBacktrace:\n{}",
                s,
                og_area,
                backtrace
            );
            return (0, 0);
        }
        if area.is_empty() {
            return (0, 0);
        }

        #[inline(always)]
        fn get_x(p: Pos) -> usize {
            p.0
        }

        #[inline(always)]
        fn get_y(p: Pos) -> usize {
            p.1
        }

        let mut bounds = self.size();
        let og_upper_left = og_area.upper_left();
        let mut upper_left = area.upper_left();
        let bottom_right = area.bottom_right();
        let (mut x, mut y) = upper_left;
        let mut prev_coords = upper_left;
        if y == get_y(bounds) || x == get_x(bounds) {
            if self.growable {
                if !self.resize(
                    std::cmp::max(self.cols, x + 2),
                    std::cmp::max(self.rows, y + 2),
                    None,
                ) {
                    return (x - upper_left.0, y - upper_left.1);
                }
                bounds = self.size();
            } else {
                return (x - upper_left.0, y - upper_left.1);
            }
        }

        if y > (get_y(bottom_right))
            || x > get_x(bottom_right)
            || y > get_y(bounds)
            || x > get_x(bounds)
        {
            if self.growable {
                if !self.resize(
                    std::cmp::max(self.cols, x + 2),
                    std::cmp::max(self.rows, y + 2),
                    None,
                ) {
                    return (x - upper_left.0, y - upper_left.1);
                }
            } else {
                log::debug!(" Invalid area with string {} and area {:?}", s, area);
                return (x - upper_left.0, y - upper_left.1);
            }
        }
        let input = if self.force_text_presentation {
            s.text_pr()
        } else {
            s.into()
        };
        // Outer loop goes through the iterator elements.
        // Inner loop handles only one element at a time.
        // It might need to repeat itself if the end of a line is reached and line
        // breaking is permitted.
        'char_loop: for mut c in input.chars() {
            'inner_loop: loop {
                macro_rules! cell_mut {
                    ($x:expr, $y:expr) => {{
                        let ret;
                        if let Some(c) = self.get_mut($x, $y) {
                            ret = c;
                        } else {
                            log::debug!(
                                "Could not access cell (x, y) = ({}, {}) while writing char c = \
                                 {:?} of string {:?} in area {:?}.",
                                $x,
                                $y,
                                c,
                                s,
                                og_area
                            );
                            break 'char_loop;
                        }
                        ret
                    }};
                }

                if c == crate::emoji_text_presentation_selector!() {
                    let prev_attrs = self[prev_coords].attrs();
                    self[prev_coords].set_attrs(prev_attrs | Attr::FORCE_TEXT);
                    continue 'char_loop;
                }

                if c == '\r' {
                    continue 'char_loop;
                }
                if c == '\n' {
                    prev_coords = (x, y);
                    y += 1;
                    if let Some(_x) = line_break {
                        x = if upper_left == og_upper_left {
                            _x + get_x(upper_left)
                        } else {
                            upper_left = og_upper_left;
                            _x + get_x(og_upper_left)
                        };
                        continue 'char_loop;
                    } else {
                        break 'char_loop;
                    }
                }
                if y > get_y(bottom_right)
                    || x > get_x(bottom_right)
                    || y > get_y(bounds)
                    || x > get_x(bounds)
                {
                    if let Some(_x) = line_break {
                        if !(y > get_y(bottom_right) || y > get_y(bounds)) {
                            x = if upper_left == og_upper_left {
                                _x + get_x(upper_left)
                            } else {
                                upper_left = og_upper_left;
                                _x + get_x(og_upper_left)
                            };
                            y += 1;
                            continue 'inner_loop;
                        }
                    }
                    break 'char_loop;
                }
                prev_coords = (x, y);
                if c == '\t' {
                    c = ' ';
                    for _ in 0..self.tab_width {
                        cell_mut!(x, y).set_ch(' ');
                        x += 1;
                        if let Some(c) = self.get_mut(x, y) {
                            c.set_ch(' ');
                        } else if let Some(_x) = line_break {
                            if !(y > get_y(bottom_right) || y > get_y(bounds)) {
                                x = if upper_left == og_upper_left {
                                    _x + get_x(upper_left)
                                } else {
                                    upper_left = og_upper_left;
                                    _x + get_x(og_upper_left)
                                };
                                y += 1;
                                continue 'char_loop;
                            } else {
                                break 'char_loop;
                            }
                        }
                    }
                } else {
                    cell_mut!(x, y).set_ch(c);
                }
                cell_mut!(x, y)
                    .set_fg(fg_color)
                    .set_bg(bg_color)
                    .set_attrs(attrs);

                match wcwidth(u32::from(c)) {
                    Some(0) | None => {
                        // Skip drawing zero width characters
                        self[(x, y)].empty = true;
                    }
                    Some(2) => {
                        // Grapheme takes more than one column, so the next cell will be drawn
                        // over. Set it as empty to skip drawing it.
                        if let Some(c) = self.get_mut(x + 1, y) {
                            x += 1;
                            *c = Cell::default();
                            c.set_fg(fg_color)
                                .set_bg(bg_color)
                                .set_attrs(attrs)
                                .set_empty(true);
                        }
                    }
                    _ => {}
                }
                x += 1;
                break 'inner_loop;
            }
        }
        if y - upper_left.1 > 0 && line_break.is_some() {
            return (x - og_upper_left.0, y - upper_left.1);
        }
        (x - upper_left.0, y - upper_left.1)
    }

    #[inline]
    pub const fn area(&self) -> Area {
        self.area
    }
}

impl Deref for CellBuffer {
    type Target = [Cell];

    fn deref(&self) -> &Self::Target {
        &self.buf
    }
}

impl DerefMut for CellBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buf
    }
}

impl Index<Pos> for CellBuffer {
    type Output = Cell;

    fn index(&self, index: Pos) -> &Self::Output {
        let (x, y) = index;
        self.get(x, y).expect("index out of bounds")
    }
}

impl IndexMut<Pos> for CellBuffer {
    fn index_mut(&mut self, index: Pos) -> &mut Cell {
        let (x, y) = index;
        self.get_mut(x, y).expect("index out of bounds")
    }
}

impl std::fmt::Display for CellBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for y in 0..self.rows {
            for x in 0..self.cols {
                let c: &char = &self[(x, y)].ch();
                if *c == '\n' {
                    break;
                }
                write!(f, "{}", *c)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

/// A single point on a terminal display.
///
/// A `Cell` contains a character and style.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Cell {
    ch: char,

    /// Set a `Cell` as empty when a previous cell spans multiple columns and it
    /// would "overflow" to this cell.
    empty: bool,
    fg: Color,
    bg: Color,
    attrs: Attr,
    keep_fg: bool,
    keep_bg: bool,
    keep_attrs: bool,
}

impl Cell {
    /// Creates a new `Cell` with the given `char`, `Color`s and `Attr`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Attr, Cell, Color};
    ///
    /// let cell = Cell::new('x', Color::Default, Color::Green, Attr::DEFAULT);
    /// assert_eq!(cell.ch(), 'x');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Green);
    /// assert_eq!(cell.attrs(), Attr::DEFAULT);
    /// ```
    pub const fn new(ch: char, fg: Color, bg: Color, attrs: Attr) -> Self {
        Self {
            ch,
            fg,
            bg,
            attrs,
            empty: false,
            keep_fg: false,
            keep_bg: false,
            keep_attrs: false,
        }
    }

    pub const fn new_default() -> Self {
        Self::new(' ', Color::Default, Color::Default, Attr::DEFAULT)
    }

    /// Creates a new `Cell` with the given `char` and default style.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Attr, Cell, Color};
    ///
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// assert_eq!(cell.attrs(), Attr::DEFAULT);
    /// ```
    pub fn with_char(ch: char) -> Self {
        Self::new(ch, Color::Default, Color::Default, Attr::DEFAULT)
    }

    /// Creates a new `Cell` with the given style and a blank `char`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Attr, Cell, Color};
    ///
    /// let mut cell = Cell::with_style(Color::Default, Color::Red, Attr::BOLD);
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Red);
    /// assert_eq!(cell.attrs(), Attr::BOLD);
    /// assert_eq!(cell.ch(), ' ');
    /// ```
    pub fn with_style(fg: Color, bg: Color, attr: Attr) -> Self {
        Self::new(' ', fg, bg, attr)
    }

    /// Returns the `Cell`'s character.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::Cell;
    ///
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    /// ```
    pub fn ch(&self) -> char {
        self.ch
    }

    /// Sets the `Cell`'s character to the given `char`
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::Cell;
    ///
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    ///
    /// cell.set_ch('y');
    /// assert_eq!(cell.ch(), 'y');
    /// ```
    pub fn set_ch(&mut self, newch: char) -> &mut Self {
        self.ch = newch;
        self.keep_fg = false;
        self.keep_bg = false;
        self.keep_attrs = false;
        self
    }

    /// Returns the `Cell`'s foreground `Color`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Attr, Cell, Color};
    ///
    /// let mut cell = Cell::with_style(Color::Blue, Color::Default, Attr::DEFAULT);
    /// assert_eq!(cell.fg(), Color::Blue);
    /// ```
    pub fn fg(&self) -> Color {
        self.fg
    }

    /// Sets the `Cell`'s foreground `Color` to the given `Color`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Cell, Color};
    ///
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.fg(), Color::Default);
    ///
    /// cell.set_fg(Color::White);
    /// assert_eq!(cell.fg(), Color::White);
    /// ```
    pub fn set_fg(&mut self, newfg: Color) -> &mut Self {
        if !self.keep_fg {
            self.fg = newfg;
        }
        self
    }

    /// Returns the `Cell`'s background `Color`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Attr, Cell, Color};
    ///
    /// let mut cell = Cell::with_style(Color::Default, Color::Green, Attr::DEFAULT);
    /// assert_eq!(cell.bg(), Color::Green);
    /// ```
    pub fn bg(&self) -> Color {
        self.bg
    }

    /// Sets the `Cell`'s background `Color` to the given `Color`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Cell, Color};
    ///
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.bg(), Color::Default);
    ///
    /// cell.set_bg(Color::Black);
    /// assert_eq!(cell.bg(), Color::Black);
    /// ```
    pub fn set_bg(&mut self, newbg: Color) -> &mut Self {
        if !self.keep_bg {
            self.bg = newbg;
        }
        self
    }

    pub fn attrs(&self) -> Attr {
        self.attrs
    }

    pub fn set_attrs(&mut self, mut newattrs: Attr) -> &mut Self {
        if self.attrs.intersects(Attr::FORCE_TEXT) {
            newattrs |= Attr::FORCE_TEXT;
        }
        if !self.keep_attrs {
            self.attrs = newattrs;
        }
        self
    }

    /// Set a `Cell` as empty when a previous cell spans multiple columns and it
    /// would "overflow" to this cell.
    pub fn empty(&self) -> bool {
        self.empty
    }

    pub fn set_empty(&mut self, new_val: bool) -> &mut Self {
        self.empty = new_val;
        self
    }

    /// Sets `keep_fg` field. If true, the foreground color will not be altered
    /// if attempted so until the character content of the cell is changed.
    pub fn set_keep_fg(&mut self, new_val: bool) -> &mut Self {
        self.keep_fg = new_val;
        self
    }

    /// Sets `keep_bg` field. If true, the background color will not be altered
    /// if attempted so until the character content of the cell is changed.
    pub fn set_keep_bg(&mut self, new_val: bool) -> &mut Self {
        self.keep_bg = new_val;
        self
    }

    /// Sets `keep_attrs` field. If true, the text attributes will not be
    /// altered if attempted so until the character content of the cell is
    /// changed.
    pub fn set_keep_attrs(&mut self, new_val: bool) -> &mut Self {
        self.keep_attrs = new_val;
        self
    }
}

impl Default for Cell {
    /// Constructs a new `Cell` with a blank `char` and default `Color`s.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::{Cell, Color};
    ///
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.ch(), ' ');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// ```
    fn default() -> Self {
        Self::new(' ', Color::Default, Color::Default, Attr::DEFAULT)
    }
}

impl From<ThemeAttribute> for Cell {
    fn from(ThemeAttribute { fg, bg, attrs, .. }: ThemeAttribute) -> Self {
        Self {
            fg,
            bg,
            attrs,
            ..Self::default()
        }
    }
}

bitflags::bitflags! {
    /// The attributes of a `Cell`.
    ///
    /// `Attr` enumerates all combinations of attributes a given style may have.
    ///
    /// `Attr::DEFAULT` represents no attribute.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use meli::Attr;
    ///
    /// // Default attribute.
    /// let def = Attr::DEFAULT;
    ///
    /// // Base attribute.
    /// let base = Attr::BOLD;
    ///
    /// // Combination.
    /// let comb = Attr::UNDERLINE | Attr::REVERSE;
    /// ```
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Attr: u16 {
        /// Terminal default.
        const DEFAULT    = 0;
        const BOLD       = 1;
        const DIM        = Self::BOLD.bits() << 1;
        const ITALICS    = Self::DIM.bits() << 1;
        const UNDERLINE  = Self::ITALICS.bits() << 1;
        const UNDERCURL  = Self::UNDERLINE.bits() << 1;
        const BLINK      = Self::UNDERCURL.bits() << 1;
        const REVERSE    = Self::BLINK.bits() << 1;
        const HIDDEN     = Self::REVERSE.bits() << 1;
        const FORCE_TEXT = Self::HIDDEN.bits() << 1;
    }
}

impl Default for Attr {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl std::fmt::Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::DEFAULT => write!(f, "Default"),
            Self::BOLD => write!(f, "Bold"),
            Self::DIM => write!(f, "Dim"),
            Self::ITALICS => write!(f, "Italics"),
            Self::UNDERLINE => write!(f, "Underline"),
            Self::UNDERCURL => write!(f, "Undercurl"),
            Self::BLINK => write!(f, "Blink"),
            Self::REVERSE => write!(f, "Reverse"),
            Self::HIDDEN => write!(f, "Hidden"),
            Self::FORCE_TEXT => write!(f, "ForceTextRepresentation"),
            combination => {
                let mut ctr = 0;
                if combination.intersects(Self::BOLD) {
                    ctr += 1;
                    Self::BOLD.fmt(f)?;
                }
                if combination.intersects(Self::DIM) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Self::DIM.fmt(f)?;
                }
                if combination.intersects(Self::ITALICS) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Self::ITALICS.fmt(f)?;
                }
                if combination.intersects(Self::UNDERLINE) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Self::UNDERLINE.fmt(f)?;
                }
                if combination.intersects(Self::UNDERCURL) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Self::UNDERCURL.fmt(f)?;
                }
                if combination.intersects(Self::BLINK) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Self::BLINK.fmt(f)?;
                }
                if combination.intersects(Self::REVERSE) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Self::REVERSE.fmt(f)?;
                }
                if combination.intersects(Self::HIDDEN) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    Self::HIDDEN.fmt(f)?;
                }
                if combination.intersects(Self::FORCE_TEXT) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    Self::FORCE_TEXT.fmt(f)?;
                }
                write!(f, "")
            }
        }
    }
}

impl<'de> Deserialize<'de> for Attr {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            Self::from_string_de::<'de, D, String>(s)
        } else {
            Err(de::Error::custom("Attributes value must be a string."))
        }
    }
}

impl Serialize for Attr {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Attr {
    pub fn from_string_de<'de, D, T: AsRef<str>>(s: T) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        match s.as_ref().trim() {
            "Default" => Ok(Self::DEFAULT),
            "Dim" => Ok(Self::DIM),
            "Bold" => Ok(Self::BOLD),
            "Italics" => Ok(Self::ITALICS),
            "Underline" => Ok(Self::UNDERLINE),
            "Undercurl" => Ok(Self::UNDERCURL),
            "Blink" => Ok(Self::BLINK),
            "Reverse" => Ok(Self::REVERSE),
            "Hidden" => Ok(Self::HIDDEN),
            "ForceTextRepresentation" => Ok(Self::FORCE_TEXT),
            combination if combination.contains('|') => {
                let mut ret = Self::DEFAULT;
                for c in combination.trim().split('|') {
                    ret |= Self::from_string_de::<'de, D, &str>(c)?;
                }
                Ok(ret)
            }
            _ => Err(de::Error::custom(
                r#"Text attribute value must either be a single attribute (eg "Bold") or a combination of attributes separated by "|" (eg "Bold|Underline"). Valid attributes are "Default", "Bold", "Italics", "Underline", "Undercurl", "Blink", "Reverse" and "Hidden"."#,
            )),
        }
    }

    pub fn write(self, prev: Self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match (self.intersects(Self::BOLD), prev.intersects(Self::BOLD)) {
            (true, true) | (false, false) => Ok(()),
            (false, true) => write!(stdout, "\x1B[22m"),
            (true, false) => write!(stdout, "\x1B[1m"),
        }
        .and_then(
            |_| match (self.intersects(Self::DIM), prev.intersects(Self::DIM)) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[22m"),
                (true, false) => write!(stdout, "\x1B[2m"),
            },
        )
        .and_then(|_| {
            match (
                self.intersects(Self::ITALICS),
                prev.intersects(Self::ITALICS),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[23m"),
                (true, false) => write!(stdout, "\x1B[3m"),
            }
        })
        .and_then(|_| {
            match (
                self.intersects(Self::UNDERLINE),
                prev.intersects(Self::UNDERLINE),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[24m"),
                (true, false) => write!(stdout, "\x1B[4m"),
            }
        })
        .and_then(|_| {
            match (
                self.intersects(Self::UNDERCURL),
                prev.intersects(Self::UNDERCURL),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[4:0m"),
                (true, false) => write!(stdout, "\x1B[4:3m"),
            }
        })
        .and_then(
            |_| match (self.intersects(Self::BLINK), prev.intersects(Self::BLINK)) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[25m"),
                (true, false) => write!(stdout, "\x1B[5m"),
            },
        )
        .and_then(|_| {
            match (
                self.intersects(Self::REVERSE),
                prev.intersects(Self::REVERSE),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[27m"),
                (true, false) => write!(stdout, "\x1B[7m"),
            }
        })
        .and_then(|_| {
            match (self.intersects(Self::HIDDEN), prev.intersects(Self::HIDDEN)) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[28m"),
                (true, false) => write!(stdout, "\x1B[8m"),
            }
        })
    }
}

/// Bounds-safe iterator of cell indices in a row.
///
/// Use [`RowIterator`] to iterate the cells of a row without the need to do any
/// bounds checking; the iterator will simply return `None` when it reaches the
/// end of the row. [`RowIterator`] can be created via the
/// [`CellBuffer::row_iter`] method and can be returned by [`BoundsIterator`]
/// which iterates each row.
///
/// ```rust,no_run
/// # use meli::terminal::{Screen, Virtual, Area};
/// # let mut screen = Screen::<Virtual>::new(Default::default());
/// # assert!(screen.resize(120, 20));
/// # let area = screen.area();
/// for c in screen.grid().row_iter(area, 0..area.width(), 2) {
///     screen.grid_mut()[c].set_ch('g');
/// }
/// ```
#[derive(Debug)]
pub struct RowIterator {
    row: usize,
    col: std::ops::Range<usize>,
    area: Area,
}

/// [`BoundsIterator`] iterates each row returning a [`RowIterator`].
///
/// ```rust,no_run
/// # use meli::terminal::{Screen, Virtual, Area};
/// # let mut screen = Screen::<Virtual>::new(Default::default());
/// # assert!(screen.resize(120, 20));
/// # let area = screen.area();
/// for row in screen.grid().bounds_iter(area) {
///     for c in row {
///         screen.grid_mut()[c].set_ch('g');
///     }
/// }
/// ```
#[derive(Clone)]
pub struct BoundsIterator {
    rows: std::ops::Range<usize>,
    cols: (usize, usize),
    width: usize,
    height: usize,
    area: Area,
}

impl std::fmt::Debug for BoundsIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct(melib::identify!(BoundsIterator))
            .field("rows", &self.rows)
            .field("cols", &self.cols)
            .field("width", &self.width)
            .field("height", &self.height)
            .field("is_empty", &self.is_empty())
            .field("bounds_area", &self.area)
            .finish()
    }
}

impl BoundsIterator {
    #[inline]
    pub fn area(&self) -> Area {
        self.area
    }

    #[inline]
    pub fn width(&self) -> usize {
        self.width
    }

    #[inline]
    pub fn height(&self) -> usize {
        self.height
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.area.is_empty() || self.width == 0 || self.height == 0 || self.rows.len() == 0
    }

    pub fn add_x(&mut self, x: usize) {
        if x == 0 {
            return;
        }

        self.width = self.width.saturating_sub(x);
        self.cols.0 += x;
        self.cols.0 = self.cols.0.min(self.cols.1);
        self.area = self.area.skip_cols(x);
    }

    pub const fn empty(generation: ScreenGeneration) -> Self {
        Self {
            width: 0,
            height: 0,
            rows: 0..0,
            cols: (0, 0),
            area: Area::new_empty(generation),
        }
    }
}

impl Iterator for BoundsIterator {
    type Item = RowIterator;
    fn next(&mut self) -> Option<Self::Item> {
        let row = self.rows.next()?;
        let area = self.area.nth_row(0);
        self.area = self.area.skip_rows(1);
        self.height = self.area.height();
        Some(RowIterator {
            row,
            col: self.cols.0..self.cols.1,
            area,
        })
    }
}

impl Iterator for RowIterator {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        if self.area.is_empty() {
            return None;
        }
        let x = self.col.next()?;
        self.area = self.area.skip_cols(1);
        Some((x, self.row))
    }
}

impl RowIterator {
    #[inline]
    pub const fn area(&self) -> Area {
        self.area
    }

    #[inline]
    pub const fn row_index(&self) -> usize {
        self.row
    }

    #[inline]
    pub fn cols(&self) -> std::ops::Range<usize> {
        self.col.clone()
    }

    pub const fn empty(generation: ScreenGeneration) -> Self {
        Self {
            row: 0,
            col: 0..0,
            area: Area::new_empty(generation),
        }
    }
}

pub use boundaries::create_box;
pub mod boundaries {
    use super::*;
    pub const HORZ_BOUNDARY: char = '─';
    pub const VERT_BOUNDARY: char = '│';
    pub const _TOP_LEFT_CORNER: char = '┌';
    pub const _TOP_RIGHT_CORNER: char = '┐';
    pub const _BOTTOM_LEFT_CORNER: char = '└';
    pub const _BOTTOM_RIGHT_CORNER: char = '┘';
    pub const LIGHT_VERTICAL_AND_RIGHT: char = '├';
    pub const _LIGHT_VERTICAL_AND_LEFT: char = '┤';
    pub const _LIGHT_DOWN_AND_HORIZONTAL: char = '┬';
    pub const _LIGHT_UP_AND_HORIZONTAL: char = '┴';
    pub const _DOUBLE_DOWN_AND_RIGHT: char = '╔';
    pub const _DOUBLE_DOWN_AND_LEFT: char = '╗';
    pub const _DOUBLE_UP_AND_LEFT: char = '╝';
    pub const _DOUBLE_UP_AND_RIGHT: char = '╚';

    bitflags::bitflags! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct Boundary: u8 {
            const RIGHT_HORZ = 0b0001;
            const LEFT_HORZ  = 0b0100;
            const FULL_HORZ  = 0b0101;
            const UP_VERT    = 0b0010;
            const DOWN_VERT  = 0b1000;
            const FULL_VERT  = 0b1010;
        }
    }

    impl Boundary {
        fn to_char(self, ascii_drawing: bool) -> char {
            if ascii_drawing {
                return match self.bits() {
                    0 => ' ',
                    0b0001 => '-',
                    0b0010 => '|',
                    0b0011 => '+',
                    0b0100 => '-',
                    0b0101 => '-',
                    0b0110 => '+',
                    0b0111 => '+',
                    0b1000 => '|',
                    0b1001 => '+',
                    0b1010 => '|',
                    0b1011 => '+',
                    0b1100 => '+',
                    0b1101 => '+',
                    0b1110 => '+',
                    0b1111 => '+',
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };
            }
            match self.bits() {
                0b0001 => '╶',
                0b0010 => '╵',
                0b0011 => '└',
                0b0100 => '╴',
                0b0101 => '─',
                0b0110 => '┘',
                0b0111 => '┴',
                0b1000 => '╷',
                0b1001 => '┌',
                0b1010 => '│',
                0b1011 => '├',
                0b1100 => '┐',
                0b1101 => '┬',
                0b1110 => '┤',
                0b1111 => '┼',
                _ => unsafe { std::hint::unreachable_unchecked() },
            }
        }

        const fn from_char(c: char) -> Option<Self> {
            match c {
                '╶' => Self::from_bits(0b0001),
                '╵' => Self::from_bits(0b0010),
                '└' => Self::from_bits(0b0011),
                '╴' => Self::from_bits(0b0100),
                '─' => Self::from_bits(0b0101),
                '┘' => Self::from_bits(0b0110),
                '┴' => Self::from_bits(0b0111),
                '╷' => Self::from_bits(0b1000),
                '┌' => Self::from_bits(0b1001),
                '│' => Self::from_bits(0b1010),
                '├' => Self::from_bits(0b1011),
                '┐' => Self::from_bits(0b1100),
                '┬' => Self::from_bits(0b1101),
                '┤' => Self::from_bits(0b1110),
                '┼' => Self::from_bits(0b1111),
                _ => None,
            }
        }
    }

    fn set_and_join_vert(grid: &mut CellBuffer, idx: Pos) -> Boundary {
        let (x, y) = idx;
        let ascii_drawing = grid.ascii_drawing;
        let mut bin_set: Boundary = Boundary::empty();
        // Check left side
        //
        //        1
        //   -> 2 │ 0
        //        3
        //
        if x > 0 {
            if let Some(cell) = grid.get_mut(x - 1, y) {
                if let Some(adj) = Boundary::from_char(cell.ch()) {
                    cell.set_ch((adj | Boundary::RIGHT_HORZ).to_char(ascii_drawing));
                    bin_set |= Boundary::LEFT_HORZ;
                }
            }
        }

        // Check right side
        //
        //        1
        //      2 │ 0 <-
        //        3
        //
        if let Some(cell) = grid.get_mut(x + 1, y) {
            if let Some(adj) = Boundary::from_char(cell.ch()) {
                cell.set_ch((adj | Boundary::LEFT_HORZ).to_char(ascii_drawing));
                bin_set |= Boundary::RIGHT_HORZ;
            }
        }

        // Set upper side
        //
        //        1 <-
        //      2 │ 0
        //        3
        //
        if y > 0 {
            if let Some(cell) = grid.get_mut(x, y - 1) {
                if let Some(adj) = Boundary::from_char(cell.ch()) {
                    cell.set_ch((adj | Boundary::DOWN_VERT).to_char(ascii_drawing));
                    bin_set |= Boundary::UP_VERT;
                }
            }
        }

        // Set bottom side
        //
        //        1
        //      2 │ 0
        //        3 <-
        //
        if let Some(cell) = grid.get_mut(x, y + 1) {
            if let Some(adj) = Boundary::from_char(cell.ch()) {
                cell.set_ch((adj | Boundary::UP_VERT).to_char(ascii_drawing));
                bin_set |= Boundary::DOWN_VERT;
            }
        }

        bin_set
    }

    fn set_and_join_horz(grid: &mut CellBuffer, idx: Pos) -> Boundary {
        let (x, y) = idx;
        let ascii_drawing = grid.ascii_drawing;
        let mut bin_set: Boundary = Boundary::empty();
        // Check upper side
        //
        //        1 <-
        //      2 ─ 0
        //        3
        //
        if y > 0 {
            if let Some(cell) = grid.get_mut(x, y - 1) {
                if let Some(adj) = Boundary::from_char(cell.ch()) {
                    cell.set_ch((adj | Boundary::DOWN_VERT).to_char(ascii_drawing));
                    bin_set |= Boundary::UP_VERT;
                }
            }
        }

        // Check bottom side
        //
        //        1
        //      2 ─ 0
        //        3 <-
        //
        if let Some(cell) = grid.get_mut(x, y + 1) {
            if let Some(adj) = Boundary::from_char(cell.ch()) {
                cell.set_ch((adj | Boundary::UP_VERT).to_char(ascii_drawing));
                bin_set |= Boundary::DOWN_VERT;
            }
        }

        // Set left side
        //
        //        1
        //   -> 2 ─ 0
        //        3
        //
        if x > 0 {
            if let Some(cell) = grid.get_mut(x - 1, y) {
                if let Some(adj) = Boundary::from_char(cell.ch()) {
                    cell.set_ch((adj | Boundary::RIGHT_HORZ).to_char(ascii_drawing));
                    bin_set |= Boundary::LEFT_HORZ;
                }
            }
        }

        // Set right side
        //
        //        1
        //      2 ─ 0 <-
        //        3
        //
        if let Some(cell) = grid.get_mut(x + 1, y) {
            if let Some(adj) = Boundary::from_char(cell.ch()) {
                cell.set_ch((adj | Boundary::LEFT_HORZ).to_char(ascii_drawing));
                bin_set |= Boundary::RIGHT_HORZ;
            }
        }

        bin_set
    }

    pub enum BoxBoundary {
        Horizontal,
        Vertical,
    }

    #[inline]
    fn set_and_join_box_corner(grid: &mut CellBuffer, idx: Pos, ch: BoxBoundary) {
        // Connected sides:
        //
        //        1
        //      2 c 0
        //        3
        //
        //     #3210
        //    0b____
        //

        let bin_set = match ch {
            BoxBoundary::Vertical => set_and_join_vert(grid, idx),
            BoxBoundary::Horizontal => set_and_join_horz(grid, idx),
        };

        if grid.ascii_drawing {
            grid[idx].set_ch('+');
        } else {
            grid[idx].set_ch(bin_set.to_char(false));
        }
    }

    /// Puts boundaries in `area`.
    /// Returns the inner area of the created box.
    pub fn create_box(grid: &mut CellBuffer, area: Area) -> Area {
        debug_assert_eq!(grid.generation(), area.generation());

        let ascii_drawing = grid.ascii_drawing;
        for (top, bottom) in grid
            .bounds_iter(area.nth_row(0))
            .zip(grid.bounds_iter(area.nth_row(area.height().saturating_sub(1))))
        {
            for c in top.chain(bottom) {
                let bin_set = set_and_join_horz(grid, c) | Boundary::FULL_HORZ;
                grid[c].set_ch(bin_set.to_char(ascii_drawing));
            }
        }

        for (left, right) in grid
            .bounds_iter(area.nth_col(0))
            .zip(grid.bounds_iter(area.nth_col(area.width().saturating_sub(1))))
        {
            for c in left.chain(right) {
                let bin_set = set_and_join_vert(grid, c) | Boundary::FULL_VERT;
                grid[c].set_ch(bin_set.to_char(ascii_drawing));
            }
        }

        set_and_join_box_corner(grid, area.upper_left(), BoxBoundary::Horizontal);
        set_and_join_box_corner(grid, area.upper_right(), BoxBoundary::Horizontal);
        set_and_join_box_corner(grid, area.bottom_left(), BoxBoundary::Vertical);
        set_and_join_box_corner(grid, area.bottom_right(), BoxBoundary::Vertical);

        area.skip(1, 1).skip_rows_from_end(1).skip_cols_from_end(1)
    }
}

impl KMP for CellBuffer {
    fn kmp_search(&self, pattern: &str) -> smallvec::SmallVec<[usize; 256]> {
        let (mut w, prev_ind) =
            pattern
                .char_indices()
                .skip(1)
                .fold((vec![], 0), |(mut acc, prev_ind), (i, _)| {
                    acc.push(&pattern[prev_ind..i]);
                    (acc, i)
                });
        w.push(&pattern[prev_ind..]);
        let t = Self::kmp_table(&w);
        let mut j = 0; // (the position of the current character in text)
        let mut k = 0; // (the position of the current character in pattern)
        let mut ret = smallvec::SmallVec::new();

        while j < self.buf.len() && k < w.len() as i32 {
            if self.buf[j].ch() == '\n' {
                j += 1;
                continue;
            }
            if w[k as usize] == self.buf[j].ch().encode_utf8(&mut [0; 4]) {
                j += 1;
                k += 1;
                if k as usize == w.len() {
                    ret.push(j - (k as usize));
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

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct FormatTag {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub attrs: Option<Attr>,
    pub priority: u8,
}

impl std::cmp::Ord for FormatTag {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl std::cmp::PartialOrd for FormatTag {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<ThemeAttribute> for FormatTag {
    fn from(val: ThemeAttribute) -> Self {
        let ThemeAttribute { fg, bg, attrs, .. } = val;
        Self {
            fg: Some(fg),
            bg: Some(bg),
            attrs: Some(attrs),
            priority: 0,
        }
    }
}

impl FormatTag {
    #[inline(always)]
    pub fn set_priority(mut self, new_val: u8) -> Self {
        self.priority = new_val;
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum WidgetWidth {
    Unset,
    Hold(usize),
    Set(usize),
}

#[cfg(test)]
mod tests {
    use crate::terminal::{Screen, Virtual};

    //use melib::text::{Reflow, TextProcessing, _ALICE_CHAPTER_1};

    #[test]
    fn test_cellbuffer_search() {
        //let lines: Vec<String> =
        // _ALICE_CHAPTER_1.split_lines_reflow(Reflow::All, Some(78));
        // let mut buf = CellBuffer::new(
        //    lines.iter().map(String::len).max().unwrap(),
        //    lines.len(),
        //    Cell::with_char(' '),
        //);
        //let width = buf.size().0;
        //for (i, l) in lines.iter().enumerate() {
        //    buf.write_string(
        //        l,
        //        Color::Default,
        //        Color::Default,
        //        Attr::DEFAULT,
        //        ((0, i), (width.saturating_sub(1), i)),
        //        None,
        //    );
        //}
        //for ind in buf.kmp_search("Alice") {
        //    for c in &buf.cellvec()[ind..std::cmp::min(buf.cellvec().len(),
        // ind + 25)] {        print!("{}", c.ch());
        //    }
        //    println!();
        //}
    }

    #[test]
    fn test_bounds_iter() {
        let mut screen = Screen::<Virtual>::new(Default::default());
        assert!(screen.resize(120, 20));
        let area = screen.area();
        assert_eq!(area.width(), 120);
        assert_eq!(area.height(), 20);

        let mut full_bounds = screen.grid().bounds_iter(area);
        assert_eq!(full_bounds.area(), area);
        assert_eq!(full_bounds.width(), area.width());
        assert_eq!(full_bounds.height(), area.height());
        assert!(!full_bounds.is_empty());

        full_bounds.add_x(0);
        assert_eq!(full_bounds.area(), area);

        full_bounds.add_x(1);
        assert_eq!(full_bounds.area().width(), area.width() - 1);
        full_bounds.add_x(area.width());
        assert_eq!(full_bounds.width(), 0);
        assert_eq!(full_bounds.area().width(), 0);

        let full_bounds = screen.grid().bounds_iter(area);
        let row_iters = full_bounds.into_iter().collect::<Vec<_>>();
        assert_eq!(row_iters.len(), area.height());

        for mid in 0..row_iters.len() {
            assert_eq!(mid, row_iters[mid].row_index());
            let (left, right) = row_iters.as_slice().split_at(mid);
            let mid = &right[0];
            assert!(area.contains(mid.area()));
            for l in left {
                assert!(area.contains(l.area()));
                assert!(!mid.area().contains(l.area()));
            }
            for r in &right[1..] {
                assert!(area.contains(r.area()));
                assert!(!mid.area().contains(r.area()));
            }
        }

        let inner_area = area.place_inside((60, 10), true, true);
        let bounds = screen.grid().bounds_iter(inner_area);
        let row_iters = bounds.into_iter().collect::<Vec<_>>();
        assert_eq!(row_iters.len(), inner_area.height());

        for mut row in row_iters {
            let row_index = row.row_index();
            assert_eq!(row.area().width(), 61);
            assert_eq!(row.next(), Some((2, row_index)));
            assert_eq!(row.area().width(), 60);
            assert_eq!(
                &row.collect::<Vec<(usize, usize)>>(),
                &(3..63)
                    .zip(std::iter::repeat(row_index))
                    .collect::<Vec<(usize, usize)>>()
            );
        }
    }

    #[test]
    fn test_create_box() {
        macro_rules! assert_eq_grid {
            ($left:expr, $right:expr) => {{
                let left = $left;
                let right = $right;
                assert_eq!(&left, &right, "left:\n{left}\nright:\n{right}");
            }};
        }

        let mut screen = Screen::<Virtual>::new(Default::default());
        assert!(screen.resize(40, 20));
        screen.grid_mut().ascii_drawing = false;
        let area = screen.area();
        let _ = super::create_box(screen.grid_mut(), area);
        let inner = area.place_inside((30, 10), false, false);
        let box_inner_area = super::create_box(screen.grid_mut(), inner);
        assert_eq_grid!(
            r#"
┌──────────────────────────────────────┐
│                                      │
│                                      │
│                                      │
│                                      │
│                                      │
│                                      │
│      ┌─────────────────────────────┐ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      └─────────────────────────────┘ │
│                                      │
└──────────────────────────────────────┘
"#
            .trim_start(),
            screen.grid().to_string()
        );
        assert_eq!(
            (box_inner_area.width(), box_inner_area.height()),
            (29, 9),
            "{box_inner_area:?}"
        );
        let inner = area.place_inside((15, 10), false, true);
        let _box_inner_area = super::create_box(screen.grid_mut(), inner);
        assert_eq_grid!(
            r#"
┌──────────────────────────────────────┐
│                                      │
│                     ┌──────────────┐ │
│                     │              │ │
│                     │              │ │
│                     │              │ │
│                     │              │ │
│      ┌──────────────┼──────────────┤ │
│      │              │              │ │
│      │              │              │ │
│      │              │              │ │
│      │              │              │ │
│      │              └──────────────┤ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      └─────────────────────────────┘ │
│                                      │
└──────────────────────────────────────┘
"#
            .trim_start(),
            screen.grid().to_string()
        );
        let _box_inner_area = super::create_box(screen.grid_mut(), area.take(5, 5));
        assert_eq_grid!(
            r#"
┌───┬──────────────────────────────────┐
│   │                                  │
│   │                 ┌──────────────┐ │
│   │                 │              │ │
├───┘                 │              │ │
│                     │              │ │
│                     │              │ │
│      ┌──────────────┼──────────────┤ │
│      │              │              │ │
│      │              │              │ │
│      │              │              │ │
│      │              │              │ │
│      │              └──────────────┤ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      │                             │ │
│      └─────────────────────────────┘ │
│                                      │
└──────────────────────────────────────┘
"#
            .trim_start(),
            screen.grid().to_string()
        );
        screen.grid_mut().clear(None);
        let area = screen.area();
        screen.grid_mut().set_ascii_drawing(true);
        let _ = super::create_box(screen.grid_mut(), area);
        let inner = area.place_inside((30, 10), false, false);
        _ = super::create_box(screen.grid_mut(), inner);
        assert_eq_grid!(
            r#"
+--------------------------------------+
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|      +-----------------------------+ |
|      |                             | |
|      |                             | |
|      |                             | |
|      |                             | |
|      |                             | |
|      |                             | |
|      |                             | |
|      |                             | |
|      |                             | |
|      +-----------------------------+ |
|                                      |
+--------------------------------------+
"#
            .trim_start(),
            screen.grid().to_string()
        );
    }
}
