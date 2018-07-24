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

/*!
  Components are ways to handle application data. They can draw on the terminal and receive events, but also do other stuff as well. (For example, see the `notifications` module.)

  See the `Component` Trait for more details.
  */

use super::*;
pub mod utilities;
pub mod mail;
pub mod notifications;

pub use utilities::*;
pub use mail::*;

use super::cells::{Color, CellBuffer};
use super::position::{Area, };
use super::{UIEvent, UIEventType, Key};

/// The upper and lower boundary char.
const HORZ_BOUNDARY: char = '─';
/// The left and right boundary char.
const VERT_BOUNDARY: char = '│';

/// The top-left corner
const _TOP_LEFT_CORNER: char = '┌';
/// The top-right corner
const _TOP_RIGHT_CORNER: char = '┐';
/// The bottom-left corner
const _BOTTOM_LEFT_CORNER: char = '└';
/// The bottom-right corner
const _BOTTOM_RIGHT_CORNER: char = '┘';

const LIGHT_VERTICAL_AND_RIGHT: char = '├';

const _LIGHT_VERTICAL_AND_LEFT: char = '┤';

const LIGHT_DOWN_AND_HORIZONTAL: char = '┬';

const LIGHT_UP_AND_HORIZONTAL: char = '┴';

/// `Entity` is a container for Components. Totally useless now so if it is not useful in the
/// future (ie hold some information, id or state) it should be removed.
pub struct Entity {
    //context: VecDeque,
    pub component: Box<Component>, // more than one?
}

impl Entity {
    /// Pass events to child component.
    pub fn rcv_event(&mut self, event: &UIEvent, context: &mut Context) {
        self.component.process_event(&event, context);
    }
}

/// Types implementing this Trait can draw on the terminal and receive events.
/// If a type wants to skip drawing if it has not changed anything, it can hold some flag in its
/// fields (eg self.dirty = false) and act upon that in their `draw` implementation.
pub trait Component {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn process_event(&mut self, event: &UIEvent, context: &mut Context);
    fn is_dirty(&self) -> bool {
        true
    }
}


// TODO: word break.
pub fn copy_area_with_break(grid_dest: &mut CellBuffer, grid_src: &CellBuffer, dest: Area, src: Area) {
    if !is_valid_area!(dest) || !is_valid_area!(src) {
        eprintln!("BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}", src, dest);
        return;
    }
    let mut src_x = get_x(upper_left!(src));
    let mut src_y = get_y(upper_left!(src));

    'y_: for y in get_y(upper_left!(dest))..=get_y(bottom_right!(dest)) {
        'x_: for x in get_x(upper_left!(dest))..=get_x(bottom_right!(dest)) {
            if grid_src[(src_x, src_y)].ch() == '\n' {
                src_y += 1;
                src_x = 0;
                if src_y >= get_y(bottom_right!(src)) {
                    break 'y_;
                }
                continue 'y_;
            }

            grid_dest[(x,y)] = grid_src[(src_x, src_y)];
            src_x += 1;
            if src_x >= get_x(bottom_right!(src)) {
                src_y += 1;
                src_x = 0;
                if src_y >= get_y(bottom_right!(src)) {
                    //clear_area(grid_dest, ((get_x(upper_left!(dest)), y), bottom_right!(dest)));
                    break 'y_;
                }
                break 'x_;
            }
        }
    }
}


/// Copy a source `Area` to a destination.
pub fn copy_area(grid_dest: &mut CellBuffer, grid_src: &CellBuffer, dest: Area, src: Area) {
    if !is_valid_area!(dest) || !is_valid_area!(src) {
        eprintln!("BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}", src, dest);
        return;
    }
    let mut src_x = get_x(upper_left!(src));
    let mut src_y = get_y(upper_left!(src));

    for y in get_y(upper_left!(dest))..=get_y(bottom_right!(dest)) {
        'for_x: for x in get_x(upper_left!(dest))..=get_x(bottom_right!(dest)) {
            grid_dest[(x,y)] = grid_src[(src_x, src_y)];
            if src_x >= get_x(bottom_right!(src)) {
                break 'for_x;
            }
            src_x += 1;
        }
        src_x = get_x(upper_left!(src));
        if src_y >= get_y(bottom_right!(src)) {
            clear_area(grid_dest, ((get_x(upper_left!(dest)), y), bottom_right!(dest)));
            break;
        }
        src_y += 1;
    }
}

/// Change foreground and background colors in an `Area`
pub fn change_colors(grid: &mut CellBuffer, area: Area, fg_color: Color, bg_color: Color) {
    if !is_valid_area!(area) {
        eprintln!("BUG: Invalid area in change_colors:\n area: {:?}", area);
        return;
    }
    for y in get_y(upper_left!(area))..=get_y(bottom_right!(area)) {
        for x in get_x(upper_left!(area))..=get_x(bottom_right!(area)) {
            grid[(x,y)].set_fg(fg_color);
            grid[(x,y)].set_bg(bg_color);
        }
    }
}


/// Write an `&str` to a `CellBuffer` in a specified `Area` with the passed colors.
fn write_string_to_grid(s: &str, grid: &mut CellBuffer, fg_color: Color, bg_color: Color, area: Area, line_break: bool) -> Pos {
    let bounds = grid.size();
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    let (mut x, mut y) = upper_left;
    if y > (get_y(bottom_right)) || x > get_x(bottom_right) ||
       y > get_y(bounds) || x > get_x(bounds) {
           eprintln!(" Invalid area with string {} and area {:?}", s, area);
        return (x, y);
    }
    for l in s.lines() {
        'char: for c in l.chars() {
            grid[(x,y)].set_ch(c);
            grid[(x,y)].set_fg(fg_color);
            grid[(x,y)].set_bg(bg_color);
            x += 1;

            if x == (get_x(bottom_right))+1 || x > get_x(bounds) {
                x = get_x(upper_left);
                y += 1;
                if y >= (get_y(bottom_right)) || y > get_y(bounds) {
                    return (x, y - 1);
                }
                if !line_break {
                    break 'char;
                }
            }
        }
    }
    (x, y)
}

/// Completely clear an `Area` with an empty char and the terminal's default colors.
fn clear_area(grid: &mut CellBuffer, area: Area) {
    if !is_valid_area!(area) {
        return;
    }
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    for y in get_y(upper_left)..=get_y(bottom_right) {
        for x in get_x(upper_left)..=get_x(bottom_right) {
            grid[(x,y)].set_ch(' ');
            grid[(x,y)].set_bg(Color::Default);
            grid[(x,y)].set_fg(Color::Default);
        }
    }
}

fn new_draft(_context: &mut Context) -> Vec<u8> {
    // TODO: Generate proper message-id https://www.jwz.org/doc/mid.html
    let mut v = String::with_capacity(500);
    v.push_str("From: \n");
    v.push_str("To: \n");
    v.push_str("Subject: \n");
    v.push_str("Message-Id: \n\n");
    v.into_bytes()
}
