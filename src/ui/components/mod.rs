/*
 * meli - ui module.
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

pub mod utilities;
pub mod mail;
use super::*;

pub use utilities::*;
pub use mail::*;

use std::fmt;


use super::cells::{Color, CellBuffer};
use super::position::Pos;
use super::{UIEvent, UIEventType, Key};

/// The upper and lower boundary char.
const HORZ_BOUNDARY: char = '─';
/// The left and right boundary char.
const VERT_BOUNDARY: char = '│';

/// The top-left corner
const TOP_LEFT_CORNER: char = '┌';
/// The top-right corner
const TOP_RIGHT_CORNER: char = '┐';
/// The bottom-left corner
const BOTTOM_LEFT_CORNER: char = '└';
/// The bottom-right corner
const BOTTOM_RIGHT_CORNER: char = '┘';

const LIGHT_VERTICAL_AND_RIGHT: char = '├';

const LIGHT_VERTICAL_AND_LEFT: char = '┤';

const LIGHT_DOWN_AND_HORIZONTAL: char = '┬';

const LIGHT_UP_AND_HORIZONTAL: char = '┴';

/// `Entity` is a container for Components. Totally useless now so if it is not useful in the
/// future (ie hold some information, id or state) it should be removed.
pub struct Entity {
    //queue: VecDeque,
    pub component: Box<Component>, // more than one?
}

impl Entity {
    /// Pass events to child component.
    pub fn rcv_event(&mut self, event: &UIEvent, queue: &mut VecDeque<UIEvent>) {
        self.component.process_event(&event, queue);
    }
}

impl fmt::Debug for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Entity", )
    }
}

/// Types implementing this Trait can draw on the terminal and receive events.
/// If a type wants to skip drawing if it has not changed anything, it can hold some flag in its
/// fields (eg self.dirty = false) and act upon that in their `draw` implementation.
pub trait Component {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos);
    fn process_event(&mut self, event: &UIEvent, queue: &mut VecDeque<UIEvent>);
}


fn write_string_to_grid(s: &str, grid: &mut CellBuffer, fg_color: Color, bg_color: Color,  upper_left: Pos, bottom_right: Pos) -> usize {
    let (mut x, mut y) = upper_left;
    for c in s.chars() {
        grid[(x,y)].set_ch(c);
        grid[(x,y)].set_fg(fg_color);
        grid[(x,y)].set_bg(bg_color);
        x += 1;
        if x == (get_x(bottom_right)) {
            x = get_x(upper_left);
            y += 1;
            if y > (get_y(bottom_right)) {
                return x;
            }
        }
    }
    x
}

fn clear_area(grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
    for y in get_y(upper_left)..get_y(bottom_right) {
        for x in get_x(upper_left)..get_x(bottom_right) {
            grid[(x,y)].set_ch(' ');
            grid[(x,y)].set_bg(Color::Default);
            grid[(x,y)].set_fg(Color::Default);
        }
    }
}

