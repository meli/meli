/*
 * meli - ui crate.
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
use text_processing::wcwidth;

use std::convert::From;
use std::fmt;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use termion::color::{AnsiValue, Rgb as TermionRgb};

/// Types and implementations taken from rustty for convenience.

pub trait CellAccessor: HasSize {
    fn cellvec(&self) -> &Vec<Cell>;
    fn cellvec_mut(&mut self) -> &mut Vec<Cell>;

    /// Clears `self`, using the given `Cell` as a blank.
    fn clear(&mut self, blank: Cell) {
        for cell in self.cellvec_mut().iter_mut() {
            *cell = blank;
        }
    }

    fn pos_to_index(&self, x: usize, y: usize) -> Option<usize> {
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
    /// ```norun
    /// use rustty::{Terminal, CellAccessor};
    ///
    /// let mut term = Terminal::new().unwrap();
    ///
    /// let a_cell = term.get(5, 5);
    /// ```
    fn get(&self, x: usize, y: usize) -> Option<&Cell> {
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
    /// ```norun
    /// use rustty::{Terminal, CellAccessor};
    ///
    /// let mut term = Terminal::new().unwrap();
    ///
    /// let a_mut_cell = term.get_mut(5, 5);
    /// ```
    fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut Cell> {
        match self.pos_to_index(x, y) {
            Some(i) => self.cellvec_mut().get_mut(i),
            None => None,
        }
    }
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
    pub ascii_drawing: bool,
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
            ascii_drawing: false,
        }
    }

    pub fn new_with_context(cols: usize, rows: usize, cell: Cell, context: &Context) -> CellBuffer {
        CellBuffer {
            cols,
            rows,
            buf: vec![cell; cols * rows],
            ascii_drawing: context.settings.terminal.ascii_drawing,
        }
    }

    pub fn set_ascii_drawing(&mut self, new_val: bool) {
        self.ascii_drawing = new_val;
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

    pub fn split_newlines(self) -> Self {
        let lines: Vec<&[Cell]> = self.split(|cell| cell.ch() == '\n').collect();
        let height = lines.len();
        let width = lines.iter().map(|l| l.len()).max().unwrap_or(0) + 1;
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));
        {
            let mut x;
            let c_slice: &mut [Cell] = &mut content;
            for (y, l) in lines.iter().enumerate() {
                let y_r = y * width;
                x = l.len() + y_r;
                c_slice[y_r..x].copy_from_slice(l);
                c_slice[x].set_ch('\n');
            }
        }
        content
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn empty(&mut self) {
        self.buf.clear();
        self.cols = 0;
        self.rows = 0;
    }
}

impl HasSize for CellBuffer {
    fn size(&self) -> Size {
        (self.cols, self.rows)
    }
}

impl CellAccessor for CellBuffer {
    fn cellvec(&self) -> &Vec<Cell> {
        &self.buf
    }

    fn cellvec_mut(&mut self) -> &mut Vec<Cell> {
        &mut self.buf
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

impl<'a> From<&'a str> for CellBuffer {
    fn from(s: &'a str) -> Self {
        let lines: Vec<&str> = s.lines().map(|l| l.trim_end()).collect();
        let len = s.len() + lines.len();
        let mut buf = CellBuffer::new(len, 1, Cell::default());
        let mut x = 0;
        for l in &lines {
            for (idx, c) in l.chars().enumerate() {
                buf[(x + idx, 0)].set_ch(c);
            }
            x += l.chars().count();
            buf[(x, 0)].set_ch('\n');
            x += 1;
        }
        buf
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

    empty: bool,
    fg: Color,
    bg: Color,
    attrs: Attr,
}

impl Cell {
    /// Creates a new `Cell` with the given `char`, `Color`s and `Attr`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let cell = Cell::new('x', Color::Default, Color::Green, Attr::Default);
    /// assert_eq!(cell.ch(), 'x');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Green);
    /// assert_eq!(cell.attrs(), Attr::Default);
    /// ```
    pub fn new(ch: char, fg: Color, bg: Color, attrs: Attr) -> Cell {
        Cell {
            ch,
            fg,
            bg,
            attrs,
            empty: false,
        }
    }

    /// Creates a new `Cell` with the given `char` and default style.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// assert_eq!(cell.attrs(), Attr::Default);
    /// ```
    pub fn with_char(ch: char) -> Cell {
        Cell::new(ch, Color::Default, Color::Default, Attr::Default)
    }

    /// Creates a new `Cell` with the given style and a blank `char`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let mut cell = Cell::with_style(Color::Default, Color::Red, Attr::Bold);
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Red);
    /// assert_eq!(cell.attrs(), Attr::Bold);
    /// assert_eq!(cell.ch(), ' ');
    /// ```
    pub fn with_style(fg: Color, bg: Color, attr: Attr) -> Cell {
        Cell::new(' ', fg, bg, attr)
    }

    /// Returns the `Cell`'s character.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::Cell;
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
    /// ```norun
    /// use rustty::Cell;
    ///
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    ///
    /// cell.set_ch('y');
    /// assert_eq!(cell.ch(), 'y');
    /// ```
    pub fn set_ch(&mut self, newch: char) -> &mut Cell {
        self.ch = newch;
        self
    }

    /// Returns the `Cell`'s foreground `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let mut cell = Cell::with_style(Color::Blue, Color::Default, Attr::Default);
    /// assert_eq!(cell.fg(), Color::Blue);
    /// ```
    pub fn fg(&self) -> Color {
        self.fg
    }

    /// Sets the `Cell`'s foreground `Color` to the given `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.fg(), Color::Default);
    ///
    /// cell.set_fg(Color::White);
    /// assert_eq!(cell.fg(), Color::White);
    /// ```
    pub fn set_fg(&mut self, newfg: Color) -> &mut Cell {
        self.fg = newfg;
        self
    }

    /// Returns the `Cell`'s background `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let mut cell = Cell::with_style(Color::Default, Color::Green, Attr::Default);
    /// assert_eq!(cell.bg(), Color::Green);
    /// ```
    pub fn bg(&self) -> Color {
        self.bg
    }

    /// Sets the `Cell`'s background `Color` to the given `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color, Attr};
    ///
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.bg(), Color::Default);
    ///
    /// cell.set_bg(Color::Black);
    /// assert_eq!(cell.bg(), Color::Black);
    /// ```
    pub fn set_bg(&mut self, newbg: Color) -> &mut Cell {
        self.bg = newbg;
        self
    }

    pub fn attrs(&self) -> Attr {
        self.attrs
    }

    pub fn set_attrs(&mut self, newattrs: Attr) -> &mut Cell {
        self.attrs = newattrs;
        self
    }

    pub fn empty(&self) -> bool {
        self.empty
    }
}

impl Default for Cell {
    /// Constructs a new `Cell` with a blank `char` and default `Color`s.
    ///
    /// # Examples
    ///
    /// ```norun
    /// use rustty::{Cell, Color};
    ///
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.ch(), ' ');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// ```
    fn default() -> Cell {
        Cell::new(' ', Color::Default, Color::Default, Attr::Default)
    }
}

/// The color of a `Cell`.
///
/// `Color::Default` represents the default color of the underlying terminal.
///
/// The eight basic colors may be used directly and correspond to 0x00..0x07 in the 8-bit (256)
/// color range; in addition, the eight basic colors coupled with `Attr::Bold` correspond to
/// 0x08..0x0f in the 8-bit color range.
///
/// `Color::Byte(..)` may be used to specify a color in the 8-bit range.
///
/// # Examples
///
/// ```norun
/// use rustty::Color;
///
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
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Fg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Fg(self.as_termion())),
        }
    }

    pub fn write_bg(self, stdout: &mut crate::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
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
            Color::Byte(b) => AnsiValue(b as u8),
            Color::Rgb(_, _, _) => AnsiValue(0),
        }
    }
}

/// The attributes of a `Cell`.
///
/// `Attr` enumerates all combinations of attributes a given style may have.
///
/// `Attr::Default` represents no attribute.
///
/// # Examples
///
/// ```norun
/// use rustty::Attr;
///
/// // Default attribute.
/// let def = Attr::Default;
///
/// // Base attribute.
/// let base = Attr::Bold;
///
/// // Combination.
/// let comb = Attr::UnderlineReverse;
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Attr {
    Default = 0b000,
    Bold = 0b001,
    Underline = 0b100,
    BoldUnderline = 0b011,
    Reverse = 0b010,
    BoldReverse = 0b101,
    UnderlineReverse = 0b110,
    BoldReverseUnderline = 0b111,
}

// TODO: word break.
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

    for y in get_y(upper_left!(dest))..=get_y(bottom_right!(dest)) {
        'for_x: for x in get_x(upper_left!(dest))..=get_x(bottom_right!(dest)) {
            grid_dest[(x, y)] = grid_src[(src_x, src_y)];
            if src_x >= get_x(bottom_right!(src)) {
                break 'for_x;
            }
            src_x += 1;
        }
        src_x = get_x(upper_left!(src));
        src_y += 1;
        if src_y > get_y(bottom_right!(src)) {
            clear_area(
                grid_dest,
                ((get_x(upper_left!(dest)), y + 1), bottom_right!(dest)),
            );
            ret.1 = y;
            break;
        }
    }
    ret
}

/// Change foreground and background colors in an `Area`
pub fn change_colors(grid: &mut CellBuffer, area: Area, fg_color: Color, bg_color: Color) {
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
    for y in get_y(upper_left!(area))..=get_y(bottom_right!(area)) {
        for x in get_x(upper_left!(area))..=get_x(bottom_right!(area)) {
            grid[(x, y)].set_fg(fg_color);
            grid[(x, y)].set_bg(bg_color);
        }
    }
}

macro_rules! inspect_bounds {
    ($grid:ident, $area:ident, $x: ident, $y: ident, $line_break:ident) => {
        let bounds = $grid.size();
        let (upper_left, bottom_right) = $area;
        if $x > (get_x(bottom_right)) || $x > get_x(bounds) {
            $x = get_x(upper_left);
            $y += 1;
            if $y > (get_y(bottom_right)) || $y > get_y(bounds) {
                return ($x, $y - 1);
            }
            if !$line_break {
                break;
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
    line_break: bool,
) -> Pos {
    let bounds = grid.size();
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    let (mut x, mut y) = upper_left;
    if y == get_y(bounds) || x == get_x(bounds) {
        return (x, y);
    }

    if y > (get_y(bottom_right))
        || x > get_x(bottom_right)
        || y > get_y(bounds)
        || x > get_x(bounds)
    {
        debug!(" Invalid area with string {} and area {:?}", s, area);
        return (x, y);
    }
    for c in s.chars() {
        if c == '\r' {
            continue;
        }
        grid[(x, y)].set_attrs(attrs);
        grid[(x, y)].set_fg(fg_color);
        grid[(x, y)].set_bg(bg_color);
        if c == '\t' {
            grid[(x, y)].set_ch(' ');
            x += 1;
            inspect_bounds!(grid, area, x, y, line_break);
            grid[(x, y)].set_ch(' ');
        } else {
            grid[(x, y)].set_ch(c);
        }

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
                grid[(x, y)].empty = true;
            }
            _ => {}
        }
        x += 1;

        inspect_bounds!(grid, area, x, y, line_break);
    }
    (x, y)
}

/// Completely clear an `Area` with an empty char and the terminal's default colors.
pub fn clear_area(grid: &mut CellBuffer, area: Area) {
    if !is_valid_area!(area) {
        return;
    }
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    for y in get_y(upper_left)..=get_y(bottom_right) {
        for x in get_x(upper_left)..=get_x(bottom_right) {
            grid[(x, y)].set_ch(' ');
            grid[(x, y)].set_bg(Color::Default);
            grid[(x, y)].set_fg(Color::Default);
            grid[(x, y)].set_attrs(Attr::Default);
            grid[(x, y)].empty = false;
        }
    }
}

pub fn center_area(area: Area, (width, height): (usize, usize)) -> Area {
    let mid_x = { std::cmp::max(width!(area) / 2, width / 2) - width / 2 };
    let mid_y = { std::cmp::max(height!(area) / 2, height / 2) - height / 2 };

    let (upper_x, upper_y) = upper_left!(area);
    let (max_x, max_y) = bottom_right!(area);
    (
        (
            std::cmp::min(max_x, upper_x + mid_x),
            std::cmp::min(max_y, upper_y + mid_y),
        ),
        (
            std::cmp::min(max_x, upper_x + mid_x + width),
            std::cmp::min(max_y, upper_y + mid_y + height),
        ),
    )
}

pub mod ansi {
    use super::{Cell, CellBuffer, Color};
    pub fn ansi_to_cellbuffer(s: &str) -> Option<CellBuffer> {
        let mut buf: Vec<Cell> = Vec::with_capacity(2048);

        enum State {
            Start,
            Csi,
            SetFg,
            SetBg,
        }
        use State::*;

        let mut rows = 0;
        let mut cols = 0;
        let mut current_fg = Color::Default;
        let mut current_bg = Color::Default;
        let mut cur_cell;
        let mut state: State;
        for l in s.lines() {
            cur_cell = Cell::default();
            state = State::Start;
            let mut chars = l.chars().peekable();
            cols = 0;
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
                        buf.push(cur_cell);
                        cur_cell = Cell::default();

                        cols += 1;
                    }
                    (Csi, 'm') => {
                        /* Reset styles */
                        current_fg = Color::Default;
                        current_bg = Color::Default;
                        state = Start;
                    }
                    (Csi, '0') => {
                        if chars.next() != Some('m') {
                            return None;
                        }
                        /* Reset styles */
                        current_fg = Color::Default;
                        current_bg = Color::Default;
                        state = Start;
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
                                return None;
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
        if buf.len() != rows * cols {
            debug!("rows: {} cols: {}, buf.len() = {}", rows, cols, buf.len());
        }
        Some(CellBuffer {
            buf,
            rows,
            cols,
            ascii_drawing: false,
        })
    }
}
