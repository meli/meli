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
Simple type definitions and macro helper for a (x,y) position on the terminal and the areas they define.

An `Area` consists of two points: the upper left and bottom right corners.
*/

/// A `(x, y)` position on screen.
pub type Pos = (usize, usize);

#[inline(always)]
pub fn get_x(p: Pos) -> usize {
    p.0
}
#[inline(always)]
pub fn get_y(p: Pos) -> usize {
    p.1
}
#[inline(always)]
pub fn set_x(p: Pos, new_x: usize) -> Pos {
    (new_x, p.1)
}
#[inline(always)]
pub fn set_y(p: Pos, new_y: usize) -> Pos {
    (p.0, new_y)
}
#[inline(always)]
pub fn pos_inc(p: Pos, inc: (usize, usize)) -> Pos {
    (p.0 + inc.0, p.1 + inc.1)
}

#[inline(always)]
pub fn pos_dec(p: Pos, dec: (usize, usize)) -> Pos {
    (p.0.saturating_sub(dec.0), p.1.saturating_sub(dec.1))
}

/// An `Area` consists of two points: the upper left and bottom right corners.
///
/// Example:
/// ```no_run
/// let new_area = ((0, 0), (1, 1));
/// ```
pub type Area = (Pos, Pos);

/// Get an area's height
///
/// Example:
/// ```no_run
/// use meli::height;
///
/// let new_area = ((0, 0), (1, 1));
/// assert_eq!(height!(new_area), 1);
/// ```
#[macro_export]
macro_rules! height {
    ($a:expr) => {
        ($crate::get_y($crate::bottom_right!($a)))
            .saturating_sub($crate::get_y($crate::upper_left!($a)))
    };
}

/// Get an area's width
///
/// Example:
/// ```no_run
/// use meli::width;
///
/// let new_area = ((0, 0), (1, 1));
/// assert_eq!(width!(new_area), 1);
/// ```
#[macro_export]
macro_rules! width {
    ($a:expr) => {
        ($crate::get_x($crate::bottom_right!($a)))
            .saturating_sub($crate::get_x($crate::upper_left!($a)))
    };
}

/// Get the upper left Position of an area
///
/// Example:
/// ```no_run
/// use meli::upper_left;
///
/// let new_area = ((0, 0), (1, 1));
/// assert_eq!(upper_left!(new_area), (0, 0));
/// ```
#[macro_export]
macro_rules! upper_left {
    ($a:expr) => {
        $a.0
    };
}

/// Get the bottom right Position of an area
///
/// Example:
/// ```no_run
/// use meli::bottom_right;
///
/// let new_area = ((0, 0), (1, 1));
/// assert_eq!(bottom_right!(new_area), (1, 1));
/// ```
#[macro_export]
macro_rules! bottom_right {
    ($a:expr) => {
        $a.1
    };
}

/// Check if area is valid.
///
/// Example:
/// ```no_run
/// use meli::is_valid_area;
///
/// let valid_area = ((0, 0), (1, 1));
/// assert!(is_valid_area!(valid_area));
///
/// let invalid_area = ((2, 2), (1, 1));
/// assert!(!is_valid_area!(invalid_area));
/// ```
///
#[macro_export]
macro_rules! is_valid_area {
    ($a:expr) => {{
        let upper_left = $crate::upper_left!($a);
        let bottom_right = $crate::bottom_right!($a);
        !($crate::get_y(upper_left) > $crate::get_y(bottom_right)
            || $crate::get_x(upper_left) > $crate::get_x(bottom_right))
    }};
}

/// Place box given by `(width, height)` in center of `area`
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Alignment {
    /// Stretch to fill all space if possible, center if no meaningful way to stretch.
    Fill,
    /// Snap to left or top side, leaving space on right or bottom.
    Start,
    /// Snap to right or bottom side, leaving space on left or top.
    End,
    /// Center natural width of widget inside the allocation.
    Center,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Center
    }
}

/// Place given area of dimensions `(width, height)` inside `area` according to given alignment
pub fn align_area(
    area: Area,
    (width, height): (usize, usize),
    vertical_alignment: Alignment,
    horizontal_alignment: Alignment,
) -> Area {
    let (top_x, width) = match horizontal_alignment {
        Alignment::Center => (
            { std::cmp::max(width!(area) / 2, width / 2) - width / 2 },
            width,
        ),
        Alignment::Start => (0, width),
        Alignment::End => (width!(area).saturating_sub(width), width!(area)),
        Alignment::Fill => (0, width!(area)),
    };
    let (top_y, height) = match vertical_alignment {
        Alignment::Center => (
            { std::cmp::max(height!(area) / 2, height / 2) - height / 2 },
            height,
        ),
        Alignment::Start => (0, height),
        Alignment::End => (height!(area).saturating_sub(height), height!(area)),
        Alignment::Fill => (0, height!(area)),
    };

    let (upper_x, upper_y) = upper_left!(area);
    let (max_x, max_y) = bottom_right!(area);
    (
        (
            std::cmp::min(max_x, upper_x + top_x),
            std::cmp::min(max_y, upper_y + top_y),
        ),
        (
            std::cmp::min(max_x, upper_x + top_x + width),
            std::cmp::min(max_y, upper_y + top_y + height),
        ),
    )
}

/// Place box given by `(width, height)` in corner of `area`
pub fn place_in_area(area: Area, (width, height): (usize, usize), upper: bool, left: bool) -> Area {
    let (upper_x, upper_y) = upper_left!(area);
    let (max_x, max_y) = bottom_right!(area);
    let x = if upper {
        upper_x + 2
    } else {
        max_x.saturating_sub(2).saturating_sub(width)
    };

    let y = if left {
        upper_y + 2
    } else {
        max_y.saturating_sub(2).saturating_sub(height)
    };

    (
        (std::cmp::min(x, max_x), std::cmp::min(y, max_y)),
        (
            std::cmp::min(x + width, max_x),
            std::cmp::min(y + height, max_y),
        ),
    )
}
