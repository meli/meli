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
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use melib::{
    log,
    text_processing::{search::KMP, wcwidth},
};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use super::{position::*, Color};
use crate::{state::Context, ThemeAttribute};

/// In a scroll region up and down cursor movements shift the region vertically.
/// The new lines are empty.
///
/// See `CellBuffer::scroll_up` and `CellBuffer::scroll_down` for an explanation
/// of how `xterm` scrolling works.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
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
#[derive(Clone, PartialEq, Eq)]
pub struct CellBuffer {
    pub cols: usize,
    pub rows: usize,
    pub buf: Vec<Cell>,
    pub default_cell: Cell,
    /// ASCII-only flag.
    pub ascii_drawing: bool,
    /// If printing to this buffer and we run out of space, expand it.
    growable: bool,
    tag_table: HashMap<u64, FormatTag>,
    tag_associations: SmallVec<[(u64, (usize, usize)); 128]>,
}

impl fmt::Debug for CellBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("CellBuffer")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("buf cells", &self.buf.len())
            .field("default_cell", &self.default_cell)
            .field("ascii_drawing", &self.ascii_drawing)
            .field("growable", &self.growable)
            .field("tag_table", &self.tag_table)
            .field("tag_associations", &self.tag_associations)
            .finish()
    }
}

impl CellBuffer {
    pub const MAX_SIZE: usize = 300_000;
    pub fn area(&self) -> Area {
        (
            (0, 0),
            (self.cols.saturating_sub(1), self.rows.saturating_sub(1)),
        )
    }
    pub fn set_cols(&mut self, new_cols: usize) {
        self.cols = new_cols;
    }

    /// Constructs a new `CellBuffer` with the given number of columns and rows,
    /// using the given `cell` as a blank.
    pub fn new(cols: usize, rows: usize, default_cell: Cell) -> CellBuffer {
        CellBuffer {
            cols,
            rows,
            buf: vec![default_cell; cols * rows],
            default_cell,
            growable: false,
            ascii_drawing: false,
            tag_table: Default::default(),
            tag_associations: SmallVec::new(),
        }
    }

    pub fn new_with_context(
        cols: usize,
        rows: usize,
        default_cell: Option<Cell>,
        context: &Context,
    ) -> CellBuffer {
        let default_cell = default_cell.unwrap_or_else(|| {
            let mut ret = Cell::default();
            let theme_default = crate::conf::value(context, "theme_default");
            ret.set_fg(theme_default.fg)
                .set_bg(theme_default.bg)
                .set_attrs(theme_default.attrs);
            ret
        });
        CellBuffer {
            cols,
            rows,
            buf: vec![default_cell; cols * rows],
            default_cell,
            growable: false,
            ascii_drawing: context.settings.terminal.ascii_drawing,
            tag_table: Default::default(),
            tag_associations: SmallVec::new(),
        }
    }

    pub fn set_ascii_drawing(&mut self, new_val: bool) {
        self.ascii_drawing = new_val;
    }

    pub fn set_growable(&mut self, new_val: bool) {
        self.growable = new_val;
    }

    /// Resizes `CellBuffer` to the given number of rows and columns, using the
    /// given `Cell` as a blank.
    #[must_use]
    pub fn resize(&mut self, newcols: usize, newrows: usize, blank: Option<Cell>) -> bool {
        let newlen = newcols * newrows;
        if (self.cols, self.rows) == (newcols, newrows) || newlen >= Self::MAX_SIZE {
            return newlen < Self::MAX_SIZE;
        }

        let blank = blank.unwrap_or(self.default_cell);
        let mut newbuf: Vec<Cell> = Vec::with_capacity(newlen);
        for y in 0..newrows {
            for x in 0..newcols {
                let cell = self.get(x, y).unwrap_or(&blank);
                newbuf.push(*cell);
            }
        }
        self.buf = newbuf;
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
        BoundsIterator {
            width: width!(area),
            height: height!(area),
            rows: std::cmp::min(self.rows.saturating_sub(1), get_y(upper_left!(area)))
                ..(std::cmp::min(self.rows, get_y(bottom_right!(area)) + 1)),
            cols: (
                std::cmp::min(self.cols.saturating_sub(1), get_x(upper_left!(area))),
                std::cmp::min(self.cols, get_x(bottom_right!(area)) + 1),
            ),
        }
    }

    /// See `RowIterator` documentation.
    pub fn row_iter(&self, bounds: std::ops::Range<usize>, row: usize) -> RowIterator {
        if row < self.rows {
            RowIterator {
                row,
                col: std::cmp::min(self.cols.saturating_sub(1), bounds.start)
                    ..(std::cmp::min(self.cols, bounds.end)),
                _width: bounds.len(),
            }
        } else {
            RowIterator {
                row,
                col: 0..0,
                _width: 0,
            }
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

    fn index(&self, index: Pos) -> &Cell {
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

impl Default for CellBuffer {
    /// Constructs a new `CellBuffer` with a size of `(0, 0)`, using the default
    /// `Cell` as a blank.
    fn default() -> CellBuffer {
        CellBuffer::new(0, 0, Cell::default())
    }
}

impl fmt::Display for CellBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        '_y: for y in 0..self.rows {
            for x in 0..self.cols {
                let c: &char = &self[(x, y)].ch();
                write!(f, "{}", *c).unwrap();
                if *c == '\n' {
                    continue '_y;
                }
            }
        }
        Ok(())
    }
}

/// A single point on a terminal display.
///
/// A `Cell` contains a character and style.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    pub fn new(ch: char, fg: Color, bg: Color, attrs: Attr) -> Cell {
        Cell {
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
    pub fn with_char(ch: char) -> Cell {
        Cell::new(ch, Color::Default, Color::Default, Attr::DEFAULT)
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
    pub fn with_style(fg: Color, bg: Color, attr: Attr) -> Cell {
        Cell::new(' ', fg, bg, attr)
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
    pub fn set_ch(&mut self, newch: char) -> &mut Cell {
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
    pub fn set_fg(&mut self, newfg: Color) -> &mut Cell {
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
    pub fn set_bg(&mut self, newbg: Color) -> &mut Cell {
        if !self.keep_bg {
            self.bg = newbg;
        }
        self
    }

    pub fn attrs(&self) -> Attr {
        self.attrs
    }

    pub fn set_attrs(&mut self, newattrs: Attr) -> &mut Cell {
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

    pub fn set_empty(&mut self, new_val: bool) -> &mut Cell {
        self.empty = new_val;
        self
    }

    /// Sets `keep_fg` field. If true, the foreground color will not be altered
    /// if attempted so until the character content of the cell is changed.
    pub fn set_keep_fg(&mut self, new_val: bool) -> &mut Cell {
        self.keep_fg = new_val;
        self
    }

    /// Sets `keep_bg` field. If true, the background color will not be altered
    /// if attempted so until the character content of the cell is changed.
    pub fn set_keep_bg(&mut self, new_val: bool) -> &mut Cell {
        self.keep_bg = new_val;
        self
    }

    /// Sets `keep_attrs` field. If true, the text attributes will not be
    /// altered if attempted so until the character content of the cell is
    /// changed.
    pub fn set_keep_attrs(&mut self, new_val: bool) -> &mut Cell {
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
    fn default() -> Cell {
        Cell::new(' ', Color::Default, Color::Default, Attr::DEFAULT)
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
    pub struct Attr: u8 {
        /// Terminal default.
        const DEFAULT   = 0b000_0000;
        const BOLD      = 0b000_0001;
        const DIM       = 0b000_0010;
        const ITALICS   = 0b000_0100;
        const UNDERLINE = 0b000_1000;
        const BLINK     = 0b001_0000;
        const REVERSE   = 0b010_0000;
        const HIDDEN    = 0b100_0000;
    }
}

impl Default for Attr {
    fn default() -> Self {
        Attr::DEFAULT
    }
}

impl fmt::Display for Attr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Attr::DEFAULT => write!(f, "Default"),
            Attr::BOLD => write!(f, "Bold"),
            Attr::DIM => write!(f, "Dim"),
            Attr::ITALICS => write!(f, "Italics"),
            Attr::UNDERLINE => write!(f, "Underline"),
            Attr::BLINK => write!(f, "Blink"),
            Attr::REVERSE => write!(f, "Reverse"),
            Attr::HIDDEN => write!(f, "Hidden"),
            combination => {
                let mut ctr = 0;
                if combination.intersects(Attr::BOLD) {
                    ctr += 1;
                    Attr::BOLD.fmt(f)?;
                }
                if combination.intersects(Attr::DIM) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Attr::DIM.fmt(f)?;
                }
                if combination.intersects(Attr::ITALICS) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Attr::ITALICS.fmt(f)?;
                }
                if combination.intersects(Attr::UNDERLINE) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Attr::UNDERLINE.fmt(f)?;
                }
                if combination.intersects(Attr::BLINK) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Attr::BLINK.fmt(f)?;
                }
                if combination.intersects(Attr::REVERSE) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    ctr += 1;
                    Attr::REVERSE.fmt(f)?;
                }
                if combination.intersects(Attr::HIDDEN) {
                    if ctr > 0 {
                        write!(f, "|")?;
                    }
                    Attr::HIDDEN.fmt(f)?;
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
            Attr::from_string_de::<'de, D, String>(s)
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
            "Default" => Ok(Attr::DEFAULT),
            "Dim" => Ok(Attr::DIM),
            "Bold" => Ok(Attr::BOLD),
            "Italics" => Ok(Attr::ITALICS),
            "Underline" => Ok(Attr::UNDERLINE),
            "Blink" => Ok(Attr::BLINK),
            "Reverse" => Ok(Attr::REVERSE),
            "Hidden" => Ok(Attr::HIDDEN),
            combination if combination.contains('|') => {
                let mut ret = Attr::DEFAULT;
                for c in combination.trim().split('|') {
                    ret |= Self::from_string_de::<'de, D, &str>(c)?;
                }
                Ok(ret)
            }
            _ => Err(de::Error::custom(
                r#"Text attribute value must either be a single attribute (eg "Bold") or a combination of attributes separated by "|" (eg "Bold|Underline"). Valid attributes are "Default", "Bold", "Italics", "Underline", "Blink", "Reverse" and "Hidden"."#,
            )),
        }
    }
    pub fn write(self, prev: Attr, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match (self.intersects(Attr::BOLD), prev.intersects(Attr::BOLD)) {
            (true, true) | (false, false) => Ok(()),
            (false, true) => write!(stdout, "\x1B[22m"),
            (true, false) => write!(stdout, "\x1B[1m"),
        }
        .and_then(
            |_| match (self.intersects(Attr::DIM), prev.intersects(Attr::DIM)) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[22m"),
                (true, false) => write!(stdout, "\x1B[2m"),
            },
        )
        .and_then(|_| {
            match (
                self.intersects(Attr::ITALICS),
                prev.intersects(Attr::ITALICS),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[23m"),
                (true, false) => write!(stdout, "\x1B[3m"),
            }
        })
        .and_then(|_| {
            match (
                self.intersects(Attr::UNDERLINE),
                prev.intersects(Attr::UNDERLINE),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[24m"),
                (true, false) => write!(stdout, "\x1B[4m"),
            }
        })
        .and_then(
            |_| match (self.intersects(Attr::BLINK), prev.intersects(Attr::BLINK)) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[25m"),
                (true, false) => write!(stdout, "\x1B[5m"),
            },
        )
        .and_then(|_| {
            match (
                self.intersects(Attr::REVERSE),
                prev.intersects(Attr::REVERSE),
            ) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[27m"),
                (true, false) => write!(stdout, "\x1B[7m"),
            }
        })
        .and_then(|_| {
            match (self.intersects(Attr::HIDDEN), prev.intersects(Attr::HIDDEN)) {
                (true, true) | (false, false) => Ok(()),
                (false, true) => write!(stdout, "\x1B[28m"),
                (true, false) => write!(stdout, "\x1B[8m"),
            }
        })
    }
}

pub fn copy_area_with_break(
    grid_dest: &mut CellBuffer,
    grid_src: &CellBuffer,
    dest: Area,
    src: Area,
) -> Pos {
    if !is_valid_area!(dest) || !is_valid_area!(src) {
        log::debug!(
            "BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}",
            src,
            dest
        );
        return upper_left!(dest);
    }

    if grid_src.is_empty() || grid_dest.is_empty() {
        return upper_left!(dest);
    }

    let mut ret = bottom_right!(dest);
    let mut src_x = get_x(upper_left!(src));
    let mut src_y = get_y(upper_left!(src));

    'y_: for y in get_y(upper_left!(dest))..=get_y(bottom_right!(dest)) {
        'x_: for x in get_x(upper_left!(dest))..=get_x(bottom_right!(dest)) {
            if grid_src[(src_x, src_y)].ch() == '\n' {
                src_y += 1;
                src_x = 0;
                if src_y >= get_y(bottom_right!(src)) {
                    ret.1 = y;
                    break 'y_;
                }
                continue 'y_;
            }

            grid_dest[(x, y)] = grid_src[(src_x, src_y)];
            src_x += 1;
            if src_x >= get_x(bottom_right!(src)) {
                src_y += 1;
                src_x = 0;
                if src_y >= get_y(bottom_right!(src)) {
                    //clear_area(grid_dest, ((get_x(upper_left!(dest)), y), bottom_right!(dest)));
                    ret.1 = y;
                    break 'y_;
                }
                break 'x_;
            }
        }
    }
    ret
}

/// Copy a source `Area` to a destination.
pub fn copy_area(grid_dest: &mut CellBuffer, grid_src: &CellBuffer, dest: Area, src: Area) -> Pos {
    if !is_valid_area!(dest) || !is_valid_area!(src) {
        log::debug!(
            "BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}",
            src,
            dest
        );
        return upper_left!(dest);
    }

    if grid_src.is_empty() || grid_dest.is_empty() {
        return upper_left!(dest);
    }

    let mut ret = bottom_right!(dest);
    let mut src_x = get_x(upper_left!(src));
    let mut src_y = get_y(upper_left!(src));
    let (cols, rows) = grid_src.size();
    if src_x >= cols || src_y >= rows {
        log::debug!("BUG: src area outside of grid_src in copy_area",);
        return upper_left!(dest);
    }

    let tag_associations = grid_src.tag_associations();
    let start_idx = grid_src.pos_to_index(src_x, src_y).unwrap();
    let mut tag_offset: usize = tag_associations
        .binary_search_by(|probe| probe.0.cmp(&start_idx))
        .unwrap_or_else(|i| i);
    let mut stack: std::collections::BTreeSet<&FormatTag> = std::collections::BTreeSet::default();
    for y in get_y(upper_left!(dest))..=get_y(bottom_right!(dest)) {
        'for_x: for x in get_x(upper_left!(dest))..=get_x(bottom_right!(dest)) {
            let idx = grid_src.pos_to_index(src_x, src_y).unwrap();
            while tag_offset < tag_associations.len() && tag_associations[tag_offset].0 <= idx {
                if tag_associations[tag_offset].2 {
                    stack.insert(&grid_src.tag_table()[&tag_associations[tag_offset].1]);
                } else {
                    stack.remove(&grid_src.tag_table()[&tag_associations[tag_offset].1]);
                }
                tag_offset += 1;
            }
            grid_dest[(x, y)] = grid_src[(src_x, src_y)];
            for t in &stack {
                if let Some(fg) = t.fg {
                    grid_dest[(x, y)].set_fg(fg).set_keep_fg(true);
                }
                if let Some(bg) = t.bg {
                    grid_dest[(x, y)].set_bg(bg).set_keep_bg(true);
                }
                if let Some(attrs) = t.attrs {
                    grid_dest[(x, y)].attrs |= attrs;
                    grid_dest[(x, y)].set_keep_attrs(true);
                }
            }
            if src_x >= get_x(bottom_right!(src)) {
                break 'for_x;
            }
            src_x += 1;
        }
        src_x = get_x(upper_left!(src));
        src_y += 1;
        if src_y > get_y(bottom_right!(src)) {
            for row in
                grid_dest.bounds_iter(((get_x(upper_left!(dest)), y + 1), bottom_right!(dest)))
            {
                for c in row {
                    grid_dest[c].set_ch(' ');
                }
            }
            ret.1 = y;
            break;
        }
    }
    ret
}

/// Change foreground and background colors in an `Area`
pub fn change_colors(grid: &mut CellBuffer, area: Area, fg_color: Color, bg_color: Color) {
    if cfg!(feature = "debug-tracing") {
        let bounds = grid.size();
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let (x, y) = upper_left;
        if y > (get_y(bottom_right))
            || x > get_x(bottom_right)
            || y >= get_y(bounds)
            || x >= get_x(bounds)
        {
            log::debug!("BUG: Invalid area in change_colors:\n area: {:?}", area);
            return;
        }
        if !is_valid_area!(area) {
            log::debug!("BUG: Invalid area in change_colors:\n area: {:?}", area);
            return;
        }
    }
    for row in grid.bounds_iter(area) {
        for c in row {
            grid[c].set_fg(fg_color).set_bg(bg_color);
        }
    }
}

macro_rules! inspect_bounds {
    ($grid:ident, $area:ident, $x: ident, $y: ident, $line_break:ident) => {
        let bounds = $grid.size();
        let (upper_left, bottom_right) = $area;
        if $x > (get_x(bottom_right)) || $x >= get_x(bounds) {
            if $grid.growable {
                if !$grid.resize(std::cmp::max($x + 1, $grid.cols), $grid.rows, None) {
                    break;
                };
            } else {
                $x = get_x(upper_left);
                $y += 1;
                if let Some(_x) = $line_break {
                    $x = _x;
                } else {
                    break;
                }
            }
        }
        if $y > (get_y(bottom_right)) || $y >= get_y(bounds) {
            if $grid.growable {
                if !$grid.resize($grid.cols, std::cmp::max($y + 1, $grid.rows), None) {
                    break;
                };
            } else {
                return ($x, $y - 1);
            }
        }
    };
}

/// Write an `&str` to a `CellBuffer` in a specified `Area` with the passed
/// colors.
pub fn write_string_to_grid(
    s: &str,
    grid: &mut CellBuffer,
    fg_color: Color,
    bg_color: Color,
    attrs: Attr,
    area: Area,
    // The left-most x coordinate.
    line_break: Option<usize>,
) -> Pos {
    let mut bounds = grid.size();
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    let (mut x, mut y) = upper_left;
    if y == get_y(bounds) || x == get_x(bounds) {
        if grid.growable {
            if !grid.resize(
                std::cmp::max(grid.cols, x + 2),
                std::cmp::max(grid.rows, y + 2),
                None,
            ) {
                return (x, y);
            }
            bounds = grid.size();
        } else {
            return (x, y);
        }
    }

    if y > (get_y(bottom_right))
        || x > get_x(bottom_right)
        || y > get_y(bounds)
        || x > get_x(bounds)
    {
        if grid.growable {
            if !grid.resize(
                std::cmp::max(grid.cols, x + 2),
                std::cmp::max(grid.rows, y + 2),
                None,
            ) {
                return (x, y);
            }
        } else {
            log::debug!(" Invalid area with string {} and area {:?}", s, area);
            return (x, y);
        }
    }
    for c in s.chars() {
        inspect_bounds!(grid, area, x, y, line_break);
        if c == '\r' {
            continue;
        }
        if c == '\n' {
            y += 1;
            if let Some(_x) = line_break {
                x = _x;
                inspect_bounds!(grid, area, x, y, line_break);
                continue;
            } else {
                break;
            }
        }
        if c == '\t' {
            grid[(x, y)].set_ch(' ');
            x += 1;
            inspect_bounds!(grid, area, x, y, line_break);
            grid[(x, y)].set_ch(' ');
        } else {
            grid[(x, y)].set_ch(c);
        }
        grid[(x, y)]
            .set_fg(fg_color)
            .set_bg(bg_color)
            .set_attrs(attrs);

        match wcwidth(u32::from(c)) {
            Some(0) | None => {
                /* Skip drawing zero width characters */
                grid[(x, y)].empty = true;
            }
            Some(2) => {
                /* Grapheme takes more than one column, so the next cell will be
                 * drawn over. Set it as empty to skip drawing it. */
                x += 1;
                inspect_bounds!(grid, area, x, y, line_break);
                grid[(x, y)] = Cell::default();
                grid[(x, y)]
                    .set_fg(fg_color)
                    .set_bg(bg_color)
                    .set_attrs(attrs)
                    .set_empty(true);
            }
            _ => {}
        }
        x += 1;
    }
    (x, y)
}

/// Completely clear an `Area` with an empty char and the terminal's default
/// colors.
pub fn clear_area(grid: &mut CellBuffer, area: Area, attributes: crate::conf::ThemeAttribute) {
    if !is_valid_area!(area) {
        return;
    }
    for row in grid.bounds_iter(area) {
        for c in row {
            grid[c] = Cell::default();
            grid[c]
                .set_fg(attributes.fg)
                .set_bg(attributes.bg)
                .set_attrs(attributes.attrs);
        }
    }
}

/// Use `RowIterator` to iterate the cells of a row without the need to do any
/// bounds checking; the iterator will simply return `None` when it reaches the
/// end of the row. `RowIterator` can be created via the `CellBuffer::row_iter`
/// method and can be returned by `BoundsIterator` which iterates each row.
/// ```no_run
/// # let mut grid = meli::CellBuffer::new(1, 1, meli::Cell::default());
/// # let x = 0;
/// for c in grid.row_iter(x..(x + 11), 0) {
///     grid[c].set_ch('w');
/// }
/// ```
#[derive(Debug)]
pub struct RowIterator {
    row: usize,
    _width: usize,
    col: std::ops::Range<usize>,
}

/// `BoundsIterator` iterates each row returning a `RowIterator`.
/// ```no_run
/// # let mut grid = meli::CellBuffer::new(1, 1, meli::Cell::default());
/// # let area = ((0, 0), (1, 1));
/// /* Visit each `Cell` in `area`. */
/// for row in grid.bounds_iter(area) {
///     for c in row {
///         grid[c].set_ch('w');
///     }
/// }
/// ```
#[derive(Clone, Debug)]
pub struct BoundsIterator {
    rows: std::ops::Range<usize>,
    pub width: usize,
    pub height: usize,
    cols: (usize, usize),
}

impl BoundsIterator {
    const EMPTY: Self = BoundsIterator {
        rows: 0..0,
        width: 0,
        height: 0,
        cols: (0, 0),
    };

    pub fn area(&self) -> Area {
        (
            (self.cols.0, self.rows.start),
            (
                std::cmp::max(self.cols.0, self.cols.1.saturating_sub(1)),
                std::cmp::max(self.rows.start, self.rows.end.saturating_sub(1)),
            ),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.width == 0 || self.height == 0 || self.rows.len() == 0
    }

    pub fn add_x(&mut self, x: usize) -> Self {
        if x == 0 {
            return Self::EMPTY;
        }

        let ret = Self {
            rows: self.rows.clone(),
            width: self.width.saturating_sub(x),
            height: self.height,
            cols: self.cols,
        };
        if self.cols.0 + x < self.cols.1 && self.width > x {
            self.cols.0 += x;
            self.width -= x;
            return ret;
        }
        *self = Self::EMPTY;
        ret
    }
}

impl Iterator for BoundsIterator {
    type Item = RowIterator;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_row) = self.rows.next() {
            Some(RowIterator {
                row: next_row,
                _width: self.width,
                col: self.cols.0..self.cols.1,
            })
        } else {
            None
        }
    }
}

impl Iterator for RowIterator {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_col) = self.col.next() {
            Some((next_col, self.row))
        } else {
            None
        }
    }
}

impl RowIterator {
    pub fn forward_col(mut self, new_val: usize) -> Self {
        if self.col.start > new_val {
            self
        } else if self.col.end <= new_val {
            self.col.start = self.col.end;
            self
        } else {
            self.col.start = new_val;
            self
        }
    }

    pub fn area(&self) -> Area {
        ((self.col.start, self.row), (self.col.end, self.row))
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

    fn bin_to_ch(b: u32) -> char {
        match b {
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

    fn ch_to_bin(ch: char) -> Option<u32> {
        match ch {
            '└' => Some(0b0011),
            '─' => Some(0b0101),
            '┘' => Some(0b0110),
            '┴' => Some(0b0111),
            '┌' => Some(0b1001),

            '│' => Some(0b1010),

            '├' => Some(0b1011),
            '┐' => Some(0b1100),
            '┬' => Some(0b1101),

            '┤' => Some(0b1110),

            '┼' => Some(0b1111),
            '╷' => Some(0b1000),

            '╵' => Some(0b0010),
            '╴' => Some(0b0100),
            '╶' => Some(0b0001),
            _ => None,
        }
    }

    #[allow(clippy::never_loop)]
    fn set_and_join_vert(grid: &mut CellBuffer, idx: Pos) -> u32 {
        let (x, y) = idx;
        let mut bin_set = 0b1010;
        /* Check left side
         *
         *        1
         *   -> 2 │ 0
         *        3
         */
        loop {
            if x > 0 {
                if let Some(cell) = grid.get_mut(x - 1, y) {
                    if let Some(adj) = ch_to_bin(cell.ch()) {
                        if (adj & 0b0001) > 0 {
                            bin_set |= 0b0100;
                            break;
                        } else if adj == 0b0100 {
                            cell.set_ch(bin_to_ch(0b0101));
                            bin_set |= 0b0100;
                            break;
                        }
                    }
                }
            }
            bin_set &= 0b1011;
            break;
        }

        /* Check right side
         *
         *        1
         *      2 │ 0 <-
         *        3
         */
        loop {
            if let Some(cell) = grid.get_mut(x + 1, y) {
                if let Some(adj) = ch_to_bin(cell.ch()) {
                    if (adj & 0b0100) > 0 {
                        bin_set |= 0b0001;
                        break;
                    }
                }
            }
            bin_set &= 0b1110;
            break;
        }

        /* Set upper side
         *
         *        1 <-
         *      2 │ 0
         *        3
         */
        loop {
            if y > 0 {
                if let Some(cell) = grid.get_mut(x, y - 1) {
                    if let Some(adj) = ch_to_bin(cell.ch()) {
                        cell.set_ch(bin_to_ch(adj | 0b1000));
                    } else {
                        bin_set &= 0b1101;
                    }
                }
            }
            break;
        }

        /* Set bottom side
         *
         *        1
         *      2 │ 0
         *        3 <-
         */
        loop {
            if let Some(cell) = grid.get_mut(x, y + 1) {
                if let Some(adj) = ch_to_bin(cell.ch()) {
                    cell.set_ch(bin_to_ch(adj | 0b0010));
                } else {
                    bin_set &= 0b0111;
                }
            }
            break;
        }

        if bin_set == 0 {
            bin_set = 0b1010;
        }

        bin_set
    }

    #[allow(clippy::never_loop)]
    fn set_and_join_horz(grid: &mut CellBuffer, idx: Pos) -> u32 {
        let (x, y) = idx;
        let mut bin_set = 0b0101;
        /* Check upper side
         *
         *        1 <-
         *      2 ─ 0
         *        3
         */
        loop {
            if y > 0 {
                if let Some(cell) = grid.get_mut(x, y - 1) {
                    if let Some(adj) = ch_to_bin(cell.ch()) {
                        if (adj & 0b1000) > 0 {
                            bin_set |= 0b0010;
                            break;
                        } else if adj == 0b0010 {
                            bin_set |= 0b0010;
                            cell.set_ch(bin_to_ch(0b1010));
                            break;
                        }
                    }
                }
            }
            bin_set &= 0b1101;
            break;
        }

        /* Check bottom side
         *
         *        1
         *      2 ─ 0
         *        3 <-
         */
        loop {
            if let Some(cell) = grid.get_mut(x, y + 1) {
                if let Some(adj) = ch_to_bin(cell.ch()) {
                    if (adj & 0b0010) > 0 {
                        bin_set |= 0b1000;
                        break;
                    } else if adj == 0b1000 {
                        bin_set |= 0b1000;
                        cell.set_ch(bin_to_ch(0b1010));
                        break;
                    }
                }
            }
            bin_set &= 0b0111;
            break;
        }

        /* Set left side
         *
         *        1
         *   -> 2 ─ 0
         *        3
         */
        loop {
            if x > 0 {
                if let Some(cell) = grid.get_mut(x - 1, y) {
                    if let Some(adj) = ch_to_bin(cell.ch()) {
                        cell.set_ch(bin_to_ch(adj | 0b0001));
                    } else {
                        bin_set &= 0b1011;
                    }
                }
            }
            break;
        }

        /* Set right side
         *
         *        1
         *      2 ─ 0 <-
         *        3
         */
        loop {
            if let Some(cell) = grid.get_mut(x + 1, y) {
                if let Some(adj) = ch_to_bin(cell.ch()) {
                    cell.set_ch(bin_to_ch(adj | 0b0100));
                } else {
                    bin_set &= 0b1110;
                }
            }
            break;
        }

        if bin_set == 0 {
            bin_set = 0b0101;
        }

        bin_set
    }

    pub enum BoxBoundary {
        Horizontal,
        Vertical,
    }

    pub fn set_and_join_box(grid: &mut CellBuffer, idx: Pos, ch: BoxBoundary) {
        /* Connected sides:
         *
         *        1
         *      2 c 0
         *        3
         *
         *     #3210
         *    0b____
         */

        if grid.ascii_drawing {
            grid[idx].set_ch(match ch {
                BoxBoundary::Vertical => '|',
                BoxBoundary::Horizontal => '-',
            });
            return;
        }

        let bin_set = match ch {
            BoxBoundary::Vertical => set_and_join_vert(grid, idx),
            BoxBoundary::Horizontal => set_and_join_horz(grid, idx),
        };

        grid[idx].set_ch(bin_to_ch(bin_set));
    }

    /// Puts boundaries in `area`.
    /// Returns the inner area of the created box.
    pub fn create_box(grid: &mut CellBuffer, area: Area) -> Area {
        if !is_valid_area!(area) {
            return ((0, 0), (0, 0));
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        if !grid.ascii_drawing {
            for x in get_x(upper_left)..get_x(bottom_right) {
                grid[(x, get_y(upper_left))].set_ch(HORZ_BOUNDARY);
                grid[(x, get_y(bottom_right))].set_ch(HORZ_BOUNDARY);
            }

            for y in get_y(upper_left)..get_y(bottom_right) {
                grid[(get_x(upper_left), y)].set_ch(VERT_BOUNDARY);
                grid[(get_x(bottom_right), y)].set_ch(VERT_BOUNDARY);
            }
            set_and_join_box(grid, upper_left, BoxBoundary::Horizontal);
            set_and_join_box(
                grid,
                set_x(upper_left, get_x(bottom_right)),
                BoxBoundary::Horizontal,
            );
            set_and_join_box(
                grid,
                set_y(upper_left, get_y(bottom_right)),
                BoxBoundary::Vertical,
            );
            set_and_join_box(grid, bottom_right, BoxBoundary::Vertical);
        }

        (
            (
                std::cmp::min(
                    get_x(upper_left) + 2,
                    std::cmp::min(get_x(upper_left) + 1, get_x(bottom_right)),
                ),
                std::cmp::min(
                    get_y(upper_left) + 2,
                    std::cmp::min(get_y(upper_left) + 1, get_y(bottom_right)),
                ),
            ),
            (
                std::cmp::max(
                    get_x(bottom_right).saturating_sub(2),
                    std::cmp::max(get_x(bottom_right).saturating_sub(1), get_x(upper_left)),
                ),
                std::cmp::max(
                    get_y(bottom_right).saturating_sub(2),
                    std::cmp::max(get_y(bottom_right).saturating_sub(1), get_y(upper_left)),
                ),
            ),
        )
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

#[derive(Debug, Default, Copy, Hash, Clone, PartialEq, Eq)]
pub struct FormatTag {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub attrs: Option<Attr>,
    pub priority: u8,
}

impl core::cmp::Ord for FormatTag {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl core::cmp::PartialOrd for FormatTag {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
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

#[derive(Debug, Copy, Hash, Clone, PartialEq, Eq)]
pub enum WidgetWidth {
    Unset,
    Hold(usize),
    Set(usize),
}

#[cfg(test)]
mod tests {
    use melib::text_processing::{Reflow, TextProcessing, _ALICE_CHAPTER_1};

    use super::*;

    #[test]
    fn test_cellbuffer_search() {
        let lines: Vec<String> = _ALICE_CHAPTER_1.split_lines_reflow(Reflow::All, Some(78));
        let mut buf = CellBuffer::new(
            lines.iter().map(String::len).max().unwrap(),
            lines.len(),
            Cell::with_char(' '),
        );
        let width = buf.size().0;
        for (i, l) in lines.iter().enumerate() {
            write_string_to_grid(
                l,
                &mut buf,
                Color::Default,
                Color::Default,
                Attr::DEFAULT,
                ((0, i), (width.saturating_sub(1), i)),
                None,
            );
        }
        for ind in buf.kmp_search("Alice") {
            for c in &buf.cellvec()[ind..std::cmp::min(buf.cellvec().len(), ind + 25)] {
                print!("{}", c.ch());
            }
            println!();
        }
    }
}
