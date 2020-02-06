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
/// ```
/// let new_area = ((0, 0), (1, 1));
/// assert_eq!(height!(new_area), 1);
/// ```
#[macro_export]
macro_rules! height {
    ($a:expr) => {
        (get_y(bottom_right!($a))).saturating_sub(get_y(upper_left!($a)))
    };
}

/// Get an area's width
///
/// Example:
/// ```
/// let new_area = ((0, 0), (1, 1));
/// assert_eq!(width!(new_area), 1);
/// ```
#[macro_export]
macro_rules! width {
    ($a:expr) => {
        (get_x(bottom_right!($a))).saturating_sub(get_x(upper_left!($a)))
    };
}

/// Get the upper left Position of an area
///
/// Example:
/// ```
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
/// ```
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
/// ```
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
        let upper_left = upper_left!($a);
        let bottom_right = bottom_right!($a);
        !(get_y(upper_left) > get_y(bottom_right) || get_x(upper_left) > get_x(bottom_right))
    }};
}
