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

/*!
 Define a (x, y) point in the terminal display as a holder of a character, foreground/background
 colors and attributes.
*/

use super::position::*;
use crate::state::Context;
use melib::text_processing::wcwidth;

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use termion::color::{AnsiValue, Rgb as TermionRgb};

/// In a scroll region up and down cursor movements shift the region vertically. The new lines are
/// empty.
///
/// See `CellBuffer::scroll_up` and `CellBuffer::scroll_down` for an explanation of how `xterm`
/// scrolling works.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ScrollRegion {
    pub top: usize,
    pub bottom: usize,
    pub left: usize,
    pub right: usize,
}

/// An array of `Cell`s that represents a terminal display.
///
/// A `CellBuffer` is a two-dimensional array of `Cell`s, each pair of indices correspond to a
/// single point on the underlying terminal.
///
/// The first index, `Cellbuffer[y]`, corresponds to a row, and thus the y-axis. The second
/// index, `Cellbuffer[y][x]`, corresponds to a column within a row and thus the x-axis.
#[derive(Clone, PartialEq, Eq)]
pub struct CellBuffer {
    cols: usize,
    rows: usize,
    buf: Vec<Cell>,
    /// ASCII-only flag.
    pub ascii_drawing: bool,
    /// If printing to this buffer and we run out of space, expand it.
    growable: bool,
    tag_table: HashMap<u64, FormatTag>,
    tag_associations: SmallVec<[(u64, (usize, usize)); 128]>,
}

impl fmt::Debug for CellBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "CellBuffer {{ cols: {}, rows: {}, buf: {} cells",
            self.cols,
            self.rows,
            self.buf.len()
        )
    }
}

impl CellBuffer {
    pub fn area(&self) -> Area {
        (
            (0, 0),
            (self.cols.saturating_sub(1), self.rows.saturating_sub(1)),
        )
    }
    pub fn set_cols(&mut self, new_cols: usize) {
        self.cols = new_cols;
    }

    /// Constructs a new `CellBuffer` with the given number of columns and rows, using the given
    /// `cell` as a blank.
    pub fn new(cols: usize, rows: usize, cell: Cell) -> CellBuffer {
        CellBuffer {
            cols,
            rows,
            buf: vec![cell; cols * rows],
            growable: false,
            ascii_drawing: false,
            tag_table: Default::default(),
            tag_associations: SmallVec::new(),
        }
    }

    pub fn new_with_context(cols: usize, rows: usize, cell: Cell, context: &Context) -> CellBuffer {
        CellBuffer {
            cols,
            rows,
            buf: vec![cell; cols * rows],
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

    /// Resizes `CellBuffer` to the given number of rows and columns, using the given `Cell` as
    /// a blank.
    pub fn resize(&mut self, newcols: usize, newrows: usize, blank: Cell) {
        let newlen = newcols * newrows;
        if self.buf.len() == newlen {
            self.cols = newcols;
            self.rows = newrows;
            return;
        }

        if newlen >= 200_000 {
            return;
        }

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
    pub fn clear(&mut self, blank: Cell) {
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

    /// Returns a reference to the `Cell` at the given coordinates, or `None` if the index is out of
    /// bounds.
    ///
    /// # Examples
    ///
    /// ```no_run
    ///
    /// let mut term = Terminal::new().unwrap();
    ///
    /// let a_cell = term.get(5, 5);
    /// ```
    pub fn get(&self, x: usize, y: usize) -> Option<&Cell> {
        match self.pos_to_index(x, y) {
            Some(i) => self.cellvec().get(i),
            None => None,
        }
    }

    /// Returns a mutable reference to the `Cell` at the given coordinates, or `None` if the index
    /// is out of bounds.
    ///
    /// # Examples
    ///
    /// ```no_run
    ///
    /// let mut term = Terminal::new().unwrap();
    ///
    /// let a_mut_cell = term.get_mut(5, 5);
    /// ```
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
    ///
    pub fn scroll_up(&mut self, scroll_region: &ScrollRegion, top: usize, offset: usize) {
        //debug!(
        //    "scroll_up scroll_region {:?}, top: {} offset {}",
        //    scroll_region, top, offset
        //);
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
    ///
    pub fn scroll_down(&mut self, scroll_region: &ScrollRegion, top: usize, offset: usize) {
        //debug!(
        //    "scroll_down scroll_region {:?}, top: {} offset {}",
        //    scroll_region, top, offset
        //);
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
            }
        } else {
            RowIterator { row, col: 0..0 }
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
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        tag.hash(&mut hasher);
        let hash = hasher.finish();
        self.tag_table.insert(hash, tag);
        hash
    }

    pub fn set_tag(&mut self, tag: u64, start: (usize, usize), end: (usize, usize)) {
        let start = self
            .pos_to_index(start.0, start.1)
            .unwrap_or(self.buf.len().saturating_sub(1));
        let end = self
            .pos_to_index(end.0, end.1)
            .unwrap_or(self.buf.len().saturating_sub(1));
        if start != end {
            self.tag_associations.push((tag, (start, end)));
        }
    }
}

impl Deref for CellBuffer {
    type Target = [Cell];

    fn deref(&self) -> &[Cell] {
        &self.buf
    }
}

impl DerefMut for CellBuffer {
    fn deref_mut(&mut self) -> &mut [Cell] {
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
    /// Constructs a new `CellBuffer` with a size of `(0, 0)`, using the default `Cell` as a blank.
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

    /// Set a `Cell` as empty when a previous cell spans multiple columns and it would
    /// "overflow" to this cell.
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

    /// Set a `Cell` as empty when a previous cell spans multiple columns and it would
    /// "overflow" to this cell.
    pub fn empty(&self) -> bool {
        self.empty
    }

    pub fn set_empty(&mut self, new_val: bool) -> &mut Cell {
        self.empty = new_val;
        self
    }

    /// Sets `keep_fg` field. If true, the foreground color will not be altered if attempted so
    /// until the character content of the cell is changed.
    pub fn set_keep_fg(&mut self, new_val: bool) -> &mut Cell {
        self.keep_fg = new_val;
        self
    }

    /// Sets `keep_bg` field. If true, the background color will not be altered if attempted so
    /// until the character content of the cell is changed.
    pub fn set_keep_bg(&mut self, new_val: bool) -> &mut Cell {
        self.keep_bg = new_val;
        self
    }

    /// Sets `keep_attrs` field. If true, the text attributes will not be altered if attempted so
    /// until the character content of the cell is changed.
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
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.ch(), ' ');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// ```
    fn default() -> Cell {
        Cell::new(' ', Color::Default, Color::Default, Attr::DEFAULT)
    }
}

/// The color of a `Cell`.
///
/// `Color::Default` represents the default color of the underlying terminal.
///
/// The eight basic colors may be used directly and correspond to 0x00..0x07 in the 8-bit (256)
/// color range; in addition, the eight basic colors coupled with `Attr::BOLD` correspond to
/// 0x08..0x0f in the 8-bit color range.
///
/// `Color::Byte(..)` may be used to specify a color in the 8-bit range.
///
/// # Examples
///
/// ```no_run
/// // The default color.
/// let default = Color::Default;
///
/// // A basic color.
/// let red = Color::Red;
///
/// // An 8-bit color.
/// let fancy = Color::Byte(0x01);
///
/// // Basic colors are also 8-bit colors (but not vice-versa).
/// assert_eq!(red.as_byte(), fancy.as_byte())
/// ```
#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Byte(u8),
    Rgb(u8, u8, u8),
    /// Terminal default.
    Default,
}

impl Color {
    /// Returns the `u8` representation of the `Color`.
    pub fn as_byte(self) -> u8 {
        match self {
            Color::Black => 0x00,
            Color::Red => 0x01,
            Color::Green => 0x02,
            Color::Yellow => 0x03,
            Color::Blue => 0x04,
            Color::Magenta => 0x05,
            Color::Cyan => 0x06,
            Color::White => 0x07,
            Color::Byte(b) => b,
            Color::Rgb(_, _, _) => unreachable!(),
            Color::Default => 0x00,
        }
    }

    pub fn from_byte(val: u8) -> Self {
        match val {
            0x00 => Color::Black,
            0x01 => Color::Red,
            0x02 => Color::Green,
            0x03 => Color::Yellow,
            0x04 => Color::Blue,
            0x05 => Color::Magenta,
            0x06 => Color::Cyan,
            0x07 => Color::White,
            _ => Color::Default,
        }
    }

    pub fn write_fg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Color::Default => write!(stdout, "{}", termion::color::Fg(termion::color::Reset)),
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Fg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Fg(self.as_termion())),
        }
    }

    pub fn write_bg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Color::Default => write!(stdout, "{}", termion::color::Bg(termion::color::Reset)),
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Bg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Bg(self.as_termion())),
        }
    }

    pub fn as_termion(self) -> AnsiValue {
        match self {
            b @ Color::Black
            | b @ Color::Red
            | b @ Color::Green
            | b @ Color::Yellow
            | b @ Color::Blue
            | b @ Color::Magenta
            | b @ Color::Cyan
            | b @ Color::White
            | b @ Color::Default => AnsiValue(b.as_byte()),
            Color::Byte(b) => AnsiValue(b),
            Color::Rgb(_, _, _) => AnsiValue(0),
        }
    }

    pub fn from_string_de<'de, D>(s: String) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let byte = match s.as_str() {
            "Aqua" => 14,
            "Aquamarine1" => 122,
            "Aquamarine2" => 86,
            "Aquamarine3" => 79,
            "Black" => 0,
            "Blue" => 12,
            "Blue1" => 21,
            "Blue2" => 19,
            "Blue3" => 20,
            "BlueViolet" => 57,
            "CadetBlue" => 72,
            "CadetBlue1" => 73,
            "Chartreuse1" => 118,
            "Chartreuse2" => 112,
            "Chartreuse3" => 82,
            "Chartreuse4" => 70,
            "Chartreuse5" => 76,
            "Chartreuse6" => 64,
            "CornflowerBlue" => 69,
            "Cornsilk1" => 230,
            "Cyan1" => 51,
            "Cyan2" => 50,
            "Cyan3" => 43,
            "DarkBlue" => 18,
            "DarkCyan" => 36,
            "DarkGoldenrod" => 136,
            "DarkGreen" => 22,
            "DarkKhaki" => 143,
            "DarkMagenta" => 90,
            "DarkMagenta1" => 91,
            "DarkOliveGreen1" => 192,
            "DarkOliveGreen2" => 155,
            "DarkOliveGreen3" => 191,
            "DarkOliveGreen4" => 107,
            "DarkOliveGreen5" => 113,
            "DarkOliveGreen6" => 149,
            "DarkOrange" => 208,
            "DarkOrange2" => 130,
            "DarkOrange3" => 166,
            "DarkRed" => 52,
            "DarkRed2" => 88,
            "DarkSeaGreen" => 108,
            "DarkSeaGreen1" => 158,
            "DarkSeaGreen2" => 193,
            "DarkSeaGreen3" => 151,
            "DarkSeaGreen4" => 157,
            "DarkSeaGreen5" => 115,
            "DarkSeaGreen6" => 150,
            "DarkSeaGreen7" => 65,
            "DarkSeaGreen8" => 71,
            "DarkSlateGray1" => 123,
            "DarkSlateGray2" => 87,
            "DarkSlateGray3" => 116,
            "DarkTurquoise" => 44,
            "DarkViolet" => 128,
            "DarkViolet1" => 92,
            "DeepPink1" => 199,
            "DeepPink2" => 197,
            "DeepPink3" => 198,
            "DeepPink4" => 125,
            "DeepPink6" => 162,
            "DeepPink7" => 89,
            "DeepPink8" => 53,
            "DeepPink9" => 161,
            "DeepSkyBlue1" => 39,
            "DeepSkyBlue2" => 38,
            "DeepSkyBlue3" => 31,
            "DeepSkyBlue4" => 32,
            "DeepSkyBlue5" => 23,
            "DeepSkyBlue6" => 24,
            "DeepSkyBlue7" => 25,
            "DodgerBlue1" => 33,
            "DodgerBlue2" => 27,
            "DodgerBlue3" => 26,
            "Fuchsia" => 13,
            "Gold1" => 220,
            "Gold2" => 142,
            "Gold3" => 178,
            "Green" => 2,
            "Green1" => 46,
            "Green2" => 34,
            "Green3" => 40,
            "Green4" => 28,
            "GreenYellow" => 154,
            "Grey" => 8,
            "Grey0" => 16,
            "Grey100" => 231,
            "Grey11" => 234,
            "Grey15" => 235,
            "Grey19" => 236,
            "Grey23" => 237,
            "Grey27" => 238,
            "Grey3" => 232,
            "Grey30" => 239,
            "Grey35" => 240,
            "Grey37" => 59,
            "Grey39" => 241,
            "Grey42" => 242,
            "Grey46" => 243,
            "Grey50" => 244,
            "Grey53" => 102,
            "Grey54" => 245,
            "Grey58" => 246,
            "Grey62" => 247,
            "Grey63" => 139,
            "Grey66" => 248,
            "Grey69" => 145,
            "Grey7" => 233,
            "Grey70" => 249,
            "Grey74" => 250,
            "Grey78" => 251,
            "Grey82" => 252,
            "Grey84" => 188,
            "Grey85" => 253,
            "Grey89" => 254,
            "Grey93" => 255,
            "Honeydew2" => 194,
            "HotPink" => 205,
            "HotPink1" => 206,
            "HotPink2" => 169,
            "HotPink3" => 132,
            "HotPink4" => 168,
            "IndianRed" => 131,
            "IndianRed1" => 167,
            "IndianRed2" => 204,
            "IndianRed3" => 203,
            "Khaki1" => 228,
            "Khaki3" => 185,
            "LightCoral" => 210,
            "LightCyan2" => 195,
            "LightCyan3" => 152,
            "LightGoldenrod1" => 227,
            "LightGoldenrod2" => 222,
            "LightGoldenrod3" => 179,
            "LightGoldenrod4" => 221,
            "LightGoldenrod5" => 186,
            "LightGreen" => 119,
            "LightGreen1" => 120,
            "LightPink1" => 217,
            "LightPink2" => 174,
            "LightPink3" => 95,
            "LightSalmon1" => 216,
            "LightSalmon2" => 137,
            "LightSalmon3" => 173,
            "LightSeaGreen" => 37,
            "LightSkyBlue1" => 153,
            "LightSkyBlue2" => 109,
            "LightSkyBlue3" => 110,
            "LightSlateBlue" => 105,
            "LightSlateGrey" => 103,
            "LightSteelBlue" => 147,
            "LightSteelBlue1" => 189,
            "LightSteelBlue3" => 146,
            "LightYellow3" => 187,
            "Lime" => 10,
            "Magenta1" => 201,
            "Magenta2" => 165,
            "Magenta3" => 200,
            "Magenta4" => 127,
            "Magenta5" => 163,
            "Magenta6" => 164,
            "Maroon" => 1,
            "MediumOrchid" => 134,
            "MediumOrchid1" => 171,
            "MediumOrchid2" => 207,
            "MediumOrchid3" => 133,
            "MediumPurple" => 104,
            "MediumPurple1" => 141,
            "MediumPurple2" => 135,
            "MediumPurple3" => 140,
            "MediumPurple4" => 97,
            "MediumPurple5" => 98,
            "MediumPurple6" => 60,
            "MediumSpringGreen" => 49,
            "MediumTurquoise" => 80,
            "MediumVioletRed" => 126,
            "MistyRose1" => 224,
            "MistyRose3" => 181,
            "NavajoWhite1" => 223,
            "NavajoWhite3" => 144,
            "Navy" => 4,
            "NavyBlue" => 17,
            "Olive" => 3,
            "Orange1" => 214,
            "Orange2" => 172,
            "Orange3" => 58,
            "Orange4" => 94,
            "OrangeRed1" => 202,
            "Orchid" => 170,
            "Orchid1" => 213,
            "Orchid2" => 212,
            "PaleGreen1" => 121,
            "PaleGreen2" => 156,
            "PaleGreen3" => 114,
            "PaleGreen4" => 77,
            "PaleTurquoise1" => 159,
            "PaleTurquoise4" => 66,
            "PaleVioletRed1" => 211,
            "Pink1" => 218,
            "Pink3" => 175,
            "Plum1" => 219,
            "Plum2" => 183,
            "Plum3" => 176,
            "Plum4" => 96,
            "Purple" => 129,
            "Purple1" => 5,
            "Purple2" => 93,
            "Purple3" => 56,
            "Purple4" => 54,
            "Purple5" => 55,
            "Red" => 9,
            "Red1" => 196,
            "Red2" => 124,
            "Red3" => 160,
            "RosyBrown" => 138,
            "RoyalBlue1" => 63,
            "Salmon1" => 209,
            "SandyBrown" => 215,
            "SeaGreen1" => 84,
            "SeaGreen2" => 85,
            "SeaGreen3" => 83,
            "SeaGreen4" => 78,
            "Silver" => 7,
            "SkyBlue1" => 117,
            "SkyBlue2" => 111,
            "SkyBlue3" => 74,
            "SlateBlue1" => 99,
            "SlateBlue2" => 61,
            "SlateBlue3" => 62,
            "SpringGreen1" => 48,
            "SpringGreen2" => 42,
            "SpringGreen3" => 47,
            "SpringGreen4" => 35,
            "SpringGreen5" => 41,
            "SpringGreen6" => 29,
            "SteelBlue" => 67,
            "SteelBlue1" => 75,
            "SteelBlue2" => 81,
            "SteelBlue3" => 68,
            "Tan" => 180,
            "Teal" => 6,
            "Thistle1" => 225,
            "Thistle3" => 182,
            "Turquoise2" => 45,
            "Turquoise4" => 30,
            "Violet" => 177,
            "Wheat1" => 229,
            "Wheat4" => 101,
            "White" => 15,
            "Yellow" => 11,
            "Yellow1" => 226,
            "Yellow2" => 190,
            "Yellow3" => 184,
            "Yellow4" => 100,
            "Yellow5" => 106,
            "Yellow6" => 148,
            "Default" => return Ok(Color::Default),
            s if s.starts_with("#")
                && s.len() == 7
                && s[1..].as_bytes().iter().all(|&b| {
                    (b >= b'0' && b <= b'9') || (b >= b'a' && b <= b'f') || (b >= b'A' && b <= b'F')
                }) =>
            {
                return Ok(Color::Rgb(
                    u8::from_str_radix(&s[1..3], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    u8::from_str_radix(&s[3..5], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    u8::from_str_radix(&s[5..7], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                ))
            }
            s if s.starts_with("#")
                && s.len() == 4
                && s[1..].as_bytes().iter().all(|&b| {
                    (b >= b'0' && b <= b'9') || (b >= b'a' && b <= b'f') || (b >= b'A' && b <= b'F')
                }) =>
            {
                return Ok(Color::Rgb(
                    17 * u8::from_str_radix(&s[1..2], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    17 * u8::from_str_radix(&s[2..3], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                    17 * u8::from_str_radix(&s[3..4], 16)
                        .map_err(|_| de::Error::custom("invalid `color` value"))?,
                ))
            }
            _ => u8::from_str_radix(&s, 10)
                .map_err(|_| de::Error::custom("invalid `color` value"))?,
        };
        Ok(Color::Byte(byte))
    }
}

impl Default for Color {
    fn default() -> Self {
        Color::Default
    }
}

impl<'de> Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Ok(s) = <String>::deserialize(deserializer) {
            Color::from_string_de::<'de, D>(s)
        } else {
            Err(de::Error::custom("invalid `color` value"))
        }
    }
}

#[test]
fn test_color_de() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct V {
        k: Color,
    }

    macro_rules! test_color {
        ($s:literal, ok $v:expr) => {
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\"")),
                Ok(V { k: $v })
            );
        };
        ($s:literal, err $v:literal) => {
            assert_eq!(
                toml::from_str::<V>(std::concat!("k = \"", $s, "\""))
                    .unwrap_err()
                    .to_string(),
                $v.to_string()
            );
        };
    }
    test_color!("#Ff6600", ok Color::Rgb(255, 102, 0));
    test_color!("#f60", ok Color::Rgb(255, 102, 0));
    test_color!("#gb0", err "invalid `color` value for key `k` at line 1 column 1");
    test_color!("Olive", ok Color::Byte(3));
    test_color!("Oafahifdave", err "invalid `color` value for key `k` at line 1 column 1");
}

impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Color::Black | Color::Byte(0) => serializer.serialize_str("Black"),
            Color::Byte(1) => serializer.serialize_str("Maroon"),
            Color::Green | Color::Byte(2) => serializer.serialize_str("Green"),
            Color::Byte(3) => serializer.serialize_str("Olive"),
            Color::Byte(4) => serializer.serialize_str("Navy"),
            Color::Byte(5) | Color::Magenta => serializer.serialize_str("Purple"),
            Color::Byte(6) | Color::Cyan => serializer.serialize_str("Teal"),
            Color::Byte(7) => serializer.serialize_str("Silver"),
            Color::Byte(8) => serializer.serialize_str("Grey"),
            Color::Red | Color::Byte(9) => serializer.serialize_str("Red"),
            Color::Byte(10) => serializer.serialize_str("Lime"),
            Color::Yellow | Color::Byte(11) => serializer.serialize_str("Yellow"),
            Color::Blue | Color::Byte(12) => serializer.serialize_str("Blue"),
            Color::Byte(13) => serializer.serialize_str("Fuchsia"),
            Color::Byte(14) => serializer.serialize_str("Aqua"),
            Color::White | Color::Byte(15) => serializer.serialize_str("White"),
            Color::Byte(16) => serializer.serialize_str("Grey0"),
            Color::Byte(17) => serializer.serialize_str("NavyBlue"),
            Color::Byte(18) => serializer.serialize_str("DarkBlue"),
            Color::Byte(19) => serializer.serialize_str("Blue3"),
            Color::Byte(20) => serializer.serialize_str("Blue3"),
            Color::Byte(21) => serializer.serialize_str("Blue1"),
            Color::Byte(22) => serializer.serialize_str("DarkGreen"),
            Color::Byte(23) => serializer.serialize_str("DeepSkyBlue4"),
            Color::Byte(24) => serializer.serialize_str("DeepSkyBlue4"),
            Color::Byte(25) => serializer.serialize_str("DeepSkyBlue4"),
            Color::Byte(26) => serializer.serialize_str("DodgerBlue3"),
            Color::Byte(27) => serializer.serialize_str("DodgerBlue2"),
            Color::Byte(28) => serializer.serialize_str("Green4"),
            Color::Byte(29) => serializer.serialize_str("SpringGreen4"),
            Color::Byte(30) => serializer.serialize_str("Turquoise4"),
            Color::Byte(31) => serializer.serialize_str("DeepSkyBlue3"),
            Color::Byte(32) => serializer.serialize_str("DeepSkyBlue3"),
            Color::Byte(33) => serializer.serialize_str("DodgerBlue1"),
            Color::Byte(34) => serializer.serialize_str("Green3"),
            Color::Byte(35) => serializer.serialize_str("SpringGreen3"),
            Color::Byte(36) => serializer.serialize_str("DarkCyan"),
            Color::Byte(37) => serializer.serialize_str("LightSeaGreen"),
            Color::Byte(38) => serializer.serialize_str("DeepSkyBlue2"),
            Color::Byte(39) => serializer.serialize_str("DeepSkyBlue1"),
            Color::Byte(40) => serializer.serialize_str("Green3"),
            Color::Byte(41) => serializer.serialize_str("SpringGreen3"),
            Color::Byte(42) => serializer.serialize_str("SpringGreen2"),
            Color::Byte(43) => serializer.serialize_str("Cyan3"),
            Color::Byte(44) => serializer.serialize_str("DarkTurquoise"),
            Color::Byte(45) => serializer.serialize_str("Turquoise2"),
            Color::Byte(46) => serializer.serialize_str("Green1"),
            Color::Byte(47) => serializer.serialize_str("SpringGreen2"),
            Color::Byte(48) => serializer.serialize_str("SpringGreen1"),
            Color::Byte(49) => serializer.serialize_str("MediumSpringGreen"),
            Color::Byte(50) => serializer.serialize_str("Cyan2"),
            Color::Byte(51) => serializer.serialize_str("Cyan1"),
            Color::Byte(52) => serializer.serialize_str("DarkRed"),
            Color::Byte(53) => serializer.serialize_str("DeepPink4"),
            Color::Byte(54) => serializer.serialize_str("Purple4"),
            Color::Byte(55) => serializer.serialize_str("Purple4"),
            Color::Byte(56) => serializer.serialize_str("Purple3"),
            Color::Byte(57) => serializer.serialize_str("BlueViolet"),
            Color::Byte(58) => serializer.serialize_str("Orange4"),
            Color::Byte(59) => serializer.serialize_str("Grey37"),
            Color::Byte(60) => serializer.serialize_str("MediumPurple4"),
            Color::Byte(61) => serializer.serialize_str("SlateBlue3"),
            Color::Byte(62) => serializer.serialize_str("SlateBlue3"),
            Color::Byte(63) => serializer.serialize_str("RoyalBlue1"),
            Color::Byte(64) => serializer.serialize_str("Chartreuse4"),
            Color::Byte(65) => serializer.serialize_str("DarkSeaGreen4"),
            Color::Byte(66) => serializer.serialize_str("PaleTurquoise4"),
            Color::Byte(67) => serializer.serialize_str("SteelBlue"),
            Color::Byte(68) => serializer.serialize_str("SteelBlue3"),
            Color::Byte(69) => serializer.serialize_str("CornflowerBlue"),
            Color::Byte(70) => serializer.serialize_str("Chartreuse3"),
            Color::Byte(71) => serializer.serialize_str("DarkSeaGreen4"),
            Color::Byte(72) => serializer.serialize_str("CadetBlue"),
            Color::Byte(73) => serializer.serialize_str("CadetBlue"),
            Color::Byte(74) => serializer.serialize_str("SkyBlue3"),
            Color::Byte(75) => serializer.serialize_str("SteelBlue1"),
            Color::Byte(76) => serializer.serialize_str("Chartreuse3"),
            Color::Byte(77) => serializer.serialize_str("PaleGreen3"),
            Color::Byte(78) => serializer.serialize_str("SeaGreen3"),
            Color::Byte(79) => serializer.serialize_str("Aquamarine3"),
            Color::Byte(80) => serializer.serialize_str("MediumTurquoise"),
            Color::Byte(81) => serializer.serialize_str("SteelBlue1"),
            Color::Byte(82) => serializer.serialize_str("Chartreuse2"),
            Color::Byte(83) => serializer.serialize_str("SeaGreen2"),
            Color::Byte(84) => serializer.serialize_str("SeaGreen1"),
            Color::Byte(85) => serializer.serialize_str("SeaGreen1"),
            Color::Byte(86) => serializer.serialize_str("Aquamarine1"),
            Color::Byte(87) => serializer.serialize_str("DarkSlateGray2"),
            Color::Byte(88) => serializer.serialize_str("DarkRed"),
            Color::Byte(89) => serializer.serialize_str("DeepPink4"),
            Color::Byte(90) => serializer.serialize_str("DarkMagenta"),
            Color::Byte(91) => serializer.serialize_str("DarkMagenta"),
            Color::Byte(92) => serializer.serialize_str("DarkViolet"),
            Color::Byte(93) => serializer.serialize_str("Purple"),
            Color::Byte(94) => serializer.serialize_str("Orange4"),
            Color::Byte(95) => serializer.serialize_str("LightPink4"),
            Color::Byte(96) => serializer.serialize_str("Plum4"),
            Color::Byte(97) => serializer.serialize_str("MediumPurple3"),
            Color::Byte(98) => serializer.serialize_str("MediumPurple3"),
            Color::Byte(99) => serializer.serialize_str("SlateBlue1"),
            Color::Byte(100) => serializer.serialize_str("Yellow4"),
            Color::Byte(101) => serializer.serialize_str("Wheat4"),
            Color::Byte(102) => serializer.serialize_str("Grey53"),
            Color::Byte(103) => serializer.serialize_str("LightSlateGrey"),
            Color::Byte(104) => serializer.serialize_str("MediumPurple"),
            Color::Byte(105) => serializer.serialize_str("LightSlateBlue"),
            Color::Byte(106) => serializer.serialize_str("Yellow4"),
            Color::Byte(107) => serializer.serialize_str("DarkOliveGreen3"),
            Color::Byte(108) => serializer.serialize_str("DarkSeaGreen"),
            Color::Byte(109) => serializer.serialize_str("LightSkyBlue3"),
            Color::Byte(110) => serializer.serialize_str("LightSkyBlue3"),
            Color::Byte(111) => serializer.serialize_str("SkyBlue2"),
            Color::Byte(112) => serializer.serialize_str("Chartreuse2"),
            Color::Byte(113) => serializer.serialize_str("DarkOliveGreen3"),
            Color::Byte(114) => serializer.serialize_str("PaleGreen3"),
            Color::Byte(115) => serializer.serialize_str("DarkSeaGreen3"),
            Color::Byte(116) => serializer.serialize_str("DarkSlateGray3"),
            Color::Byte(117) => serializer.serialize_str("SkyBlue1"),
            Color::Byte(118) => serializer.serialize_str("Chartreuse1"),
            Color::Byte(119) => serializer.serialize_str("LightGreen"),
            Color::Byte(120) => serializer.serialize_str("LightGreen"),
            Color::Byte(121) => serializer.serialize_str("PaleGreen1"),
            Color::Byte(122) => serializer.serialize_str("Aquamarine1"),
            Color::Byte(123) => serializer.serialize_str("DarkSlateGray1"),
            Color::Byte(124) => serializer.serialize_str("Red3"),
            Color::Byte(125) => serializer.serialize_str("DeepPink4"),
            Color::Byte(126) => serializer.serialize_str("MediumVioletRed"),
            Color::Byte(127) => serializer.serialize_str("Magenta3"),
            Color::Byte(128) => serializer.serialize_str("DarkViolet"),
            Color::Byte(129) => serializer.serialize_str("Purple"),
            Color::Byte(130) => serializer.serialize_str("DarkOrange3"),
            Color::Byte(131) => serializer.serialize_str("IndianRed"),
            Color::Byte(132) => serializer.serialize_str("HotPink3"),
            Color::Byte(133) => serializer.serialize_str("MediumOrchid3"),
            Color::Byte(134) => serializer.serialize_str("MediumOrchid"),
            Color::Byte(135) => serializer.serialize_str("MediumPurple2"),
            Color::Byte(136) => serializer.serialize_str("DarkGoldenrod"),
            Color::Byte(137) => serializer.serialize_str("LightSalmon3"),
            Color::Byte(138) => serializer.serialize_str("RosyBrown"),
            Color::Byte(139) => serializer.serialize_str("Grey63"),
            Color::Byte(140) => serializer.serialize_str("MediumPurple2"),
            Color::Byte(141) => serializer.serialize_str("MediumPurple1"),
            Color::Byte(142) => serializer.serialize_str("Gold3"),
            Color::Byte(143) => serializer.serialize_str("DarkKhaki"),
            Color::Byte(144) => serializer.serialize_str("NavajoWhite3"),
            Color::Byte(145) => serializer.serialize_str("Grey69"),
            Color::Byte(146) => serializer.serialize_str("LightSteelBlue3"),
            Color::Byte(147) => serializer.serialize_str("LightSteelBlue"),
            Color::Byte(148) => serializer.serialize_str("Yellow3"),
            Color::Byte(149) => serializer.serialize_str("DarkOliveGreen3"),
            Color::Byte(150) => serializer.serialize_str("DarkSeaGreen3"),
            Color::Byte(151) => serializer.serialize_str("DarkSeaGreen2"),
            Color::Byte(152) => serializer.serialize_str("LightCyan3"),
            Color::Byte(153) => serializer.serialize_str("LightSkyBlue1"),
            Color::Byte(154) => serializer.serialize_str("GreenYellow"),
            Color::Byte(155) => serializer.serialize_str("DarkOliveGreen2"),
            Color::Byte(156) => serializer.serialize_str("PaleGreen1"),
            Color::Byte(157) => serializer.serialize_str("DarkSeaGreen2"),
            Color::Byte(158) => serializer.serialize_str("DarkSeaGreen1"),
            Color::Byte(159) => serializer.serialize_str("PaleTurquoise1"),
            Color::Byte(160) => serializer.serialize_str("Red3"),
            Color::Byte(161) => serializer.serialize_str("DeepPink3"),
            Color::Byte(162) => serializer.serialize_str("DeepPink3"),
            Color::Byte(163) => serializer.serialize_str("Magenta3"),
            Color::Byte(164) => serializer.serialize_str("Magenta3"),
            Color::Byte(165) => serializer.serialize_str("Magenta2"),
            Color::Byte(166) => serializer.serialize_str("DarkOrange3"),
            Color::Byte(167) => serializer.serialize_str("IndianRed"),
            Color::Byte(168) => serializer.serialize_str("HotPink3"),
            Color::Byte(169) => serializer.serialize_str("HotPink2"),
            Color::Byte(170) => serializer.serialize_str("Orchid"),
            Color::Byte(171) => serializer.serialize_str("MediumOrchid1"),
            Color::Byte(172) => serializer.serialize_str("Orange3"),
            Color::Byte(173) => serializer.serialize_str("LightSalmon3"),
            Color::Byte(174) => serializer.serialize_str("LightPink3"),
            Color::Byte(175) => serializer.serialize_str("Pink3"),
            Color::Byte(176) => serializer.serialize_str("Plum3"),
            Color::Byte(177) => serializer.serialize_str("Violet"),
            Color::Byte(178) => serializer.serialize_str("Gold3"),
            Color::Byte(179) => serializer.serialize_str("LightGoldenrod3"),
            Color::Byte(180) => serializer.serialize_str("Tan"),
            Color::Byte(181) => serializer.serialize_str("MistyRose3"),
            Color::Byte(182) => serializer.serialize_str("Thistle3"),
            Color::Byte(183) => serializer.serialize_str("Plum2"),
            Color::Byte(184) => serializer.serialize_str("Yellow3"),
            Color::Byte(185) => serializer.serialize_str("Khaki3"),
            Color::Byte(186) => serializer.serialize_str("LightGoldenrod2"),
            Color::Byte(187) => serializer.serialize_str("LightYellow3"),
            Color::Byte(188) => serializer.serialize_str("Grey84"),
            Color::Byte(189) => serializer.serialize_str("LightSteelBlue1"),
            Color::Byte(190) => serializer.serialize_str("Yellow2"),
            Color::Byte(191) => serializer.serialize_str("DarkOliveGreen1"),
            Color::Byte(192) => serializer.serialize_str("DarkOliveGreen1"),
            Color::Byte(193) => serializer.serialize_str("DarkSeaGreen1"),
            Color::Byte(194) => serializer.serialize_str("Honeydew2"),
            Color::Byte(195) => serializer.serialize_str("LightCyan1"),
            Color::Byte(196) => serializer.serialize_str("Red1"),
            Color::Byte(197) => serializer.serialize_str("DeepPink2"),
            Color::Byte(198) => serializer.serialize_str("DeepPink1"),
            Color::Byte(199) => serializer.serialize_str("DeepPink1"),
            Color::Byte(200) => serializer.serialize_str("Magenta2"),
            Color::Byte(201) => serializer.serialize_str("Magenta1"),
            Color::Byte(202) => serializer.serialize_str("OrangeRed1"),
            Color::Byte(203) => serializer.serialize_str("IndianRed1"),
            Color::Byte(204) => serializer.serialize_str("IndianRed1"),
            Color::Byte(205) => serializer.serialize_str("HotPink"),
            Color::Byte(206) => serializer.serialize_str("HotPink"),
            Color::Byte(207) => serializer.serialize_str("MediumOrchid1"),
            Color::Byte(208) => serializer.serialize_str("DarkOrange"),
            Color::Byte(209) => serializer.serialize_str("Salmon1"),
            Color::Byte(210) => serializer.serialize_str("LightCoral"),
            Color::Byte(211) => serializer.serialize_str("PaleVioletRed1"),
            Color::Byte(212) => serializer.serialize_str("Orchid2"),
            Color::Byte(213) => serializer.serialize_str("Orchid1"),
            Color::Byte(214) => serializer.serialize_str("Orange1"),
            Color::Byte(215) => serializer.serialize_str("SandyBrown"),
            Color::Byte(216) => serializer.serialize_str("LightSalmon1"),
            Color::Byte(217) => serializer.serialize_str("LightPink1"),
            Color::Byte(218) => serializer.serialize_str("Pink1"),
            Color::Byte(219) => serializer.serialize_str("Plum1"),
            Color::Byte(220) => serializer.serialize_str("Gold1"),
            Color::Byte(221) => serializer.serialize_str("LightGoldenrod2"),
            Color::Byte(222) => serializer.serialize_str("LightGoldenrod2"),
            Color::Byte(223) => serializer.serialize_str("NavajoWhite1"),
            Color::Byte(224) => serializer.serialize_str("MistyRose1"),
            Color::Byte(225) => serializer.serialize_str("Thistle1"),
            Color::Byte(226) => serializer.serialize_str("Yellow1"),
            Color::Byte(227) => serializer.serialize_str("LightGoldenrod1"),
            Color::Byte(228) => serializer.serialize_str("Khaki1"),
            Color::Byte(229) => serializer.serialize_str("Wheat1"),
            Color::Byte(230) => serializer.serialize_str("Cornsilk1"),
            Color::Byte(231) => serializer.serialize_str("Grey100"),
            Color::Byte(232) => serializer.serialize_str("Grey3"),
            Color::Byte(233) => serializer.serialize_str("Grey7"),
            Color::Byte(234) => serializer.serialize_str("Grey11"),
            Color::Byte(235) => serializer.serialize_str("Grey15"),
            Color::Byte(236) => serializer.serialize_str("Grey19"),
            Color::Byte(237) => serializer.serialize_str("Grey23"),
            Color::Byte(238) => serializer.serialize_str("Grey27"),
            Color::Byte(239) => serializer.serialize_str("Grey30"),
            Color::Byte(240) => serializer.serialize_str("Grey35"),
            Color::Byte(241) => serializer.serialize_str("Grey39"),
            Color::Byte(242) => serializer.serialize_str("Grey42"),
            Color::Byte(243) => serializer.serialize_str("Grey46"),
            Color::Byte(244) => serializer.serialize_str("Grey50"),
            Color::Byte(245) => serializer.serialize_str("Grey54"),
            Color::Byte(246) => serializer.serialize_str("Grey58"),
            Color::Byte(247) => serializer.serialize_str("Grey62"),
            Color::Byte(248) => serializer.serialize_str("Grey66"),
            Color::Byte(249) => serializer.serialize_str("Grey70"),
            Color::Byte(250) => serializer.serialize_str("Grey74"),
            Color::Byte(251) => serializer.serialize_str("Grey78"),
            Color::Byte(252) => serializer.serialize_str("Grey82"),
            Color::Byte(253) => serializer.serialize_str("Grey85"),
            Color::Byte(254) => serializer.serialize_str("Grey89"),
            Color::Byte(255) => serializer.serialize_str("Grey93"),
            Color::Rgb(r, g, b) => {
                serializer.serialize_str(&format!("#{:02x}{:02x}{:02x}", r, g, b))
            }
            Color::Default => serializer.serialize_str("Default"),
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
            combination if combination.contains("|") => {
                let mut ret = Attr::DEFAULT;
                for c in combination.trim().split("|") {
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
        debug!(
            "BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}",
            src, dest
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
        debug!(
            "BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}",
            src, dest
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
        debug!("BUG: src area outside of grid_src in copy_area",);
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
            debug!("BUG: Invalid area in change_colors:\n area: {:?}", area);
            return;
        }
        if !is_valid_area!(area) {
            debug!("BUG: Invalid area in change_colors:\n area: {:?}", area);
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
                $grid.resize($x + 1, $grid.rows, Cell::default());
            } else {
                $x = get_x(upper_left);
                $y += 1;
                if $line_break.is_none() {
                    break;
                } else {
                    $x = $line_break.unwrap();
                }
            }
        }
        if $y > (get_y(bottom_right)) || $y >= get_y(bounds) {
            if $grid.growable {
                $grid.resize($grid.cols, $y + 1, Cell::default());
            } else {
                return ($x, $y - 1);
            }
        }
    };
}

/// Write an `&str` to a `CellBuffer` in a specified `Area` with the passed colors.
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
    let bounds = grid.size();
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    let (mut x, mut y) = upper_left;
    if y == get_y(bounds) || x == get_x(bounds) {
        if grid.growable {
            grid.resize(
                std::cmp::max(grid.cols, x + 1),
                std::cmp::max(grid.rows, y + 1),
                Cell::default(),
            );
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
            grid.resize(
                std::cmp::max(grid.cols, x + 1),
                std::cmp::max(grid.rows, y + 1),
                Cell::default(),
            );
        } else {
            debug!(" Invalid area with string {} and area {:?}", s, area);
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
            if line_break.is_none() {
                break;
            } else {
                x = line_break.unwrap();
                inspect_bounds!(grid, area, x, y, line_break);
                continue;
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

/// Completely clear an `Area` with an empty char and the terminal's default colors.
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

pub mod ansi {
    //! Create a `CellBuffer` from a string slice containing ANSI escape codes.
    use super::{Attr, Cell, CellBuffer, Color};
    /// Create a `CellBuffer` from a string slice containing ANSI escape codes.
    pub fn ansi_to_cellbuffer(s: &str) -> Option<CellBuffer> {
        let mut bufs: Vec<Vec<Cell>> = Vec::with_capacity(2048);
        let mut row: Vec<Cell> = Vec::with_capacity(2048);

        enum State {
            Start,
            Csi,
            SetFg,
            SetBg,
        }
        use State::*;

        let mut rows = 0;
        let mut max_cols = 0;
        let mut current_fg = Color::Default;
        let mut current_bg = Color::Default;
        let mut current_attrs = Attr::DEFAULT;
        let mut cur_cell;
        let mut state: State;
        for l in s.lines() {
            cur_cell = Cell::default();
            state = State::Start;
            let mut chars = l.chars().peekable();
            if rows > 0 {
                max_cols = std::cmp::max(row.len(), max_cols);
                bufs.push(row);
                row = Vec::with_capacity(2048);
            }
            rows += 1;
            'line_loop: loop {
                let c = chars.next();
                if c.is_none() {
                    break 'line_loop;
                }
                match (&state, c.unwrap()) {
                    (Start, '\x1b') => {
                        if chars.next() != Some('[') {
                            return None;
                        }
                        state = Csi;
                    }
                    (Start, c) => {
                        cur_cell.set_ch(c);
                        cur_cell.set_fg(current_fg);
                        cur_cell.set_bg(current_bg);
                        cur_cell.set_attrs(current_attrs);
                        row.push(cur_cell);
                        cur_cell = Cell::default();
                    }
                    (Csi, 'm') => {
                        /* Reset styles */
                        current_fg = Color::Default;
                        current_bg = Color::Default;
                        current_attrs = Attr::DEFAULT;
                        state = Start;
                    }
                    (Csi, '0') if chars.peek() == Some(&'0') => {
                        current_attrs = Attr::DEFAULT;
                        chars.next();
                        let next = chars.next();
                        if next == Some('m') {
                            state = Start;
                        } else if next != Some(';') {
                            return None;
                        }
                    }
                    (Csi, c @ '0'..='8') if chars.peek() == Some(&'m') => {
                        chars.next();
                        state = Start;
                        match c {
                            '0' => {
                                //Reset all attributes
                                current_fg = Color::Default;
                                current_bg = Color::Default;
                                current_attrs = Attr::DEFAULT;
                            }
                            '1' => {
                                current_attrs.set(Attr::BOLD, true);
                            }
                            '2' => {
                                current_attrs.set(Attr::DIM, true);
                            }
                            '3' => {
                                current_attrs.set(Attr::ITALICS, true);
                            }
                            '4' => {
                                current_attrs.set(Attr::UNDERLINE, true);
                            }
                            '5' => {
                                current_attrs.set(Attr::BLINK, true);
                            }
                            '7' => {
                                current_attrs.set(Attr::REVERSE, true);
                            }
                            '8' => {
                                current_attrs.set(Attr::HIDDEN, true);
                            }
                            _ => return None,
                        }
                    }
                    (Csi, '0') => {
                        continue;
                    }
                    (Csi, '2') => {
                        match (chars.next(), chars.next()) {
                            (Some('2'), Some('m')) => {
                                current_attrs.set(Attr::BOLD, false);
                                current_attrs.set(Attr::DIM, false);
                            }
                            (Some('3'), Some('m')) => {
                                current_attrs.set(Attr::ITALICS, false);
                            }
                            (Some('4'), Some('m')) => {
                                current_attrs.set(Attr::UNDERLINE, false);
                            }
                            (Some('5'), Some('m')) => {
                                current_attrs.set(Attr::BLINK, false);
                            }
                            (Some('7'), Some('m')) => {
                                current_attrs.set(Attr::REVERSE, false);
                            }
                            (Some('8'), Some('m')) => {
                                current_attrs.set(Attr::HIDDEN, false);
                            }
                            (Some('9'), Some('m')) => { /* Not crossed out */ }
                            _ => return None,
                        }
                    }
                    (Csi, '3') => {
                        match chars.next() {
                            Some('8') => {
                                /* Set foreground color */
                                if chars.next() == Some(';') {
                                    state = SetFg;
                                    /* Next arguments are 5;n or 2;r;g;b */
                                    continue;
                                }
                                chars.next();
                                return None;
                            }
                            Some('9') => {
                                current_fg = Color::Default;
                                /* default foreground color */
                                let next = chars.next();
                                if next == Some('m') {
                                    state = Start;
                                } else if next != Some(';') {
                                    return None;
                                }
                                continue;
                            }
                            Some(c) if c >= '0' && c < '8' => {
                                current_fg = Color::from_byte(c as u8 - 0x30);
                                if chars.next() != Some('m') {
                                    return None;
                                }
                                state = Start;
                            }
                            _ => return None,
                        }
                    }
                    (Csi, '4') => {
                        match chars.next() {
                            Some('8') => {
                                /* Set background color */
                                if chars.next() == Some(';') {
                                    state = SetBg;
                                    /* Next arguments are 5;n or 2;r;g;b */
                                    continue;
                                }
                                return None;
                            }
                            Some('9') => {
                                /* default background color */
                                current_bg = Color::Default;
                                let next = chars.next();
                                if next == Some('m') {
                                    state = Start;
                                } else if next != Some(';') {
                                    return None;
                                }
                                continue;
                            }
                            Some(c) if c >= '0' && c < '8' => {
                                current_bg = Color::from_byte(c as u8 - 0x30);
                                if chars.next() != Some('m') {
                                    return None;
                                }
                                state = Start;
                            }
                            _ => return None,
                        }
                    }
                    (Csi, '9') => {
                        match chars.next() {
                            Some('0') => current_fg = Color::Black,
                            Some('1') => current_fg = Color::Red,
                            Some('2') => current_fg = Color::Green,
                            Some('3') => current_fg = Color::Yellow,
                            Some('4') => current_fg = Color::Blue,
                            Some('5') => current_fg = Color::Magenta,
                            Some('6') => current_fg = Color::Cyan,
                            Some('7') => current_fg = Color::White,
                            _ => {}
                        }
                        let next = chars.next();
                        if next != Some('m') {
                            //debug!(next);
                        }
                        state = Start;
                    }
                    (Csi, '1') if chars.peek() == Some(&'0') => {
                        chars.next();
                        match chars.next() {
                            Some('0') => current_bg = Color::Black,
                            Some('1') => current_bg = Color::Red,
                            Some('2') => current_bg = Color::Green,
                            Some('3') => current_bg = Color::Yellow,
                            Some('4') => current_bg = Color::Blue,
                            Some('5') => current_bg = Color::Magenta,
                            Some('6') => current_bg = Color::Cyan,
                            Some('7') => current_bg = Color::White,
                            _ => {}
                        }
                        let next = chars.next();
                        if next != Some('m') {
                            //debug!(next);
                        }

                        state = Start;
                    }
                    (SetFg, '5') => {
                        if chars.next() != Some(';') {
                            return None;
                        }
                        let mut accum = 0;
                        while chars.peek().is_some() && chars.peek() != Some(&'m') {
                            let c = chars.next().unwrap();
                            accum *= 10;
                            accum += c as u8 - 0x30;
                        }
                        if chars.next() != Some('m') {
                            return None;
                        }
                        current_fg = Color::from_byte(accum);
                        state = Start;
                    }
                    (SetFg, '2') => {
                        if chars.next() != Some(';') {
                            return None;
                        }
                        let mut rgb_color = Color::Rgb(0, 0, 0);
                        if let Color::Rgb(ref mut r, ref mut g, ref mut b) = rgb_color {
                            'rgb_fg: for val in &mut [r, g, b] {
                                let mut accum = 0;
                                while chars.peek().is_some()
                                    && chars.peek() != Some(&';')
                                    && chars.peek() != Some(&'m')
                                {
                                    let c = chars.next().unwrap();
                                    accum *= 10;
                                    accum += c as u8 - 0x30;
                                }
                                **val = accum;
                                match chars.peek() {
                                    Some(&'m') => {
                                        break 'rgb_fg;
                                    }
                                    Some(&';') => {
                                        chars.next();
                                    }
                                    _ => return None,
                                }
                            }
                        }
                        if chars.next() != Some('m') {
                            return None;
                        }
                        current_fg = rgb_color;
                        state = Start;
                    }
                    (SetBg, '5') => {
                        if chars.next() != Some(';') {
                            return None;
                        }
                        let mut accum = 0;
                        while chars.peek().is_some() && chars.peek() != Some(&'m') {
                            let c = chars.next().unwrap();
                            accum *= 10;
                            accum += c as u8 - 0x30;
                        }
                        if chars.next() != Some('m') {
                            return None;
                        }
                        current_bg = Color::from_byte(accum);
                        state = Start;
                    }
                    (SetBg, '2') => {
                        if chars.next() != Some(';') {
                            return None;
                        }
                        let mut rgb_color = Color::Rgb(0, 0, 0);
                        if let Color::Rgb(ref mut r, ref mut g, ref mut b) = rgb_color {
                            'rgb_bg: for val in &mut [r, g, b] {
                                let mut accum = 0;
                                while chars.peek().is_some()
                                    && chars.peek() != Some(&';')
                                    && chars.peek() != Some(&'m')
                                {
                                    let c = chars.next().unwrap();
                                    accum *= 10;
                                    accum += c as u8 - 0x30;
                                }
                                **val = accum;
                                match chars.peek() {
                                    Some(&'m') => {
                                        break 'rgb_bg;
                                    }
                                    Some(&';') => {
                                        chars.next();
                                    }
                                    _ => return None,
                                }
                            }
                        }
                        if chars.next() != Some('m') {
                            return None;
                        }
                        current_bg = rgb_color;
                        state = Start;
                    }
                    _ => unreachable!(),
                }
            }
        }
        max_cols = std::cmp::max(row.len(), max_cols);
        bufs.push(row);
        let mut buf: Vec<Cell> = Vec::with_capacity(max_cols * bufs.len());
        for l in bufs {
            let row_len = l.len();
            buf.extend(l.into_iter());
            if row_len < max_cols {
                for _ in row_len..max_cols {
                    buf.push(Cell::default());
                }
            }
        }

        if buf.len() != rows * max_cols {
            debug!(
                "BUG: rows: {} cols: {} = {}, but buf.len() = {}",
                rows,
                max_cols,
                rows * max_cols,
                buf.len()
            );
        }
        Some(CellBuffer {
            buf,
            rows,
            cols: max_cols,
            growable: false,
            ascii_drawing: false,
            tag_table: Default::default(),
            tag_associations: smallvec::SmallVec::new(),
        })
    }
}

/// Use `RowIterator` to iterate the cells of a row without the need to do any bounds checking;
/// the iterator will simply return `None` when it reaches the end of the row.
/// `RowIterator` can be created via the `CellBuffer::row_iter` method and can be returned by
/// `BoundsIterator` which iterates each row.
/// ```no_run
/// for c in grid.row_iter(
///     x..(x + 11),
///     0,
/// ) {
///     grid[c].set_ch('w');
/// }
/// ```
pub struct RowIterator {
    row: usize,
    col: std::ops::Range<usize>,
}

/// `BoundsIterator` iterates each row returning a `RowIterator`.
/// ```no_run
/// /* Visit each `Cell` in `area`. */
/// for c in grid.bounds_iter(area) {
///     grid[c].set_ch('w');
/// }
/// ```
pub struct BoundsIterator {
    rows: std::ops::Range<usize>,
    cols: (usize, usize),
}

impl Iterator for BoundsIterator {
    type Item = RowIterator;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_row) = self.rows.next() {
            Some(RowIterator {
                row: next_row,
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
}

pub use boundaries::create_box;
pub mod boundaries {
    use super::*;
    pub(crate) const HORZ_BOUNDARY: char = '─';
    pub(crate) const VERT_BOUNDARY: char = '│';
    pub(crate) const _TOP_LEFT_CORNER: char = '┌';
    pub(crate) const _TOP_RIGHT_CORNER: char = '┐';
    pub(crate) const _BOTTOM_LEFT_CORNER: char = '└';
    pub(crate) const _BOTTOM_RIGHT_CORNER: char = '┘';
    pub(crate) const LIGHT_VERTICAL_AND_RIGHT: char = '├';
    pub(crate) const _LIGHT_VERTICAL_AND_LEFT: char = '┤';
    pub(crate) const _LIGHT_DOWN_AND_HORIZONTAL: char = '┬';
    pub(crate) const _LIGHT_UP_AND_HORIZONTAL: char = '┴';
    pub(crate) const _DOUBLE_DOWN_AND_RIGHT: char = '╔';
    pub(crate) const _DOUBLE_DOWN_AND_LEFT: char = '╗';
    pub(crate) const _DOUBLE_UP_AND_LEFT: char = '╝';
    pub(crate) const _DOUBLE_UP_AND_RIGHT: char = '╚';

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
                            cell.set_fg(Color::Byte(240));
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
                        cell.set_fg(Color::Byte(240));
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
                    cell.set_fg(Color::Byte(240));
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
                            cell.set_fg(Color::Byte(240));
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
                        cell.set_fg(Color::Byte(240));
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
                        cell.set_fg(Color::Byte(240));
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
                    cell.set_fg(Color::Byte(240));
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

    pub(crate) enum BoxBoundary {
        Horizontal,
        Vertical,
    }

    pub(crate) fn set_and_join_box(grid: &mut CellBuffer, idx: Pos, ch: BoxBoundary) {
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

            grid[idx].set_fg(Color::Byte(240));
            return;
        }

        let bin_set = match ch {
            BoxBoundary::Vertical => set_and_join_vert(grid, idx),
            BoxBoundary::Horizontal => set_and_join_horz(grid, idx),
        };

        grid[idx].set_ch(bin_to_ch(bin_set));
        grid[idx].set_fg(Color::Byte(240));
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
                grid[(x, get_y(upper_left))]
                    .set_ch(HORZ_BOUNDARY)
                    .set_fg(Color::Byte(240));
                grid[(x, get_y(bottom_right))]
                    .set_ch(HORZ_BOUNDARY)
                    .set_fg(Color::Byte(240));
            }

            for y in get_y(upper_left)..get_y(bottom_right) {
                grid[(get_x(upper_left), y)]
                    .set_ch(VERT_BOUNDARY)
                    .set_fg(Color::Byte(240));
                grid[(get_x(bottom_right), y)]
                    .set_ch(VERT_BOUNDARY)
                    .set_fg(Color::Byte(240));
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

use melib::text_processing::search::KMP;

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

#[test]
fn test_cellbuffer_search() {
    use melib::text_processing::{Reflow, TextProcessing, _ALICE_CHAPTER_1};
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
        Some(self.cmp(&other))
    }
}
