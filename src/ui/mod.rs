/*
 * meli - ui module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

pub mod index;
pub mod pager;

extern crate ncurses;

/* Color pairs; foreground && background. */
pub static COLOR_PAIR_DEFAULT: i16 = 1;
pub static COLOR_PAIR_CURSOR: i16 = 2;
pub static COLOR_PAIR_HEADERS: i16 = 3;
pub static COLOR_PAIR_THREAD_INDENT: i16 = 4;
pub static COLOR_PAIR_THREAD_ODD: i16 = 5;
pub static COLOR_PAIR_THREAD_EVEN: i16 = 6;

pub struct TUI;

impl TUI {
    pub fn initialize() -> Self {
        /* start ncurses */
        ncurses::initscr();
        ncurses::keypad(ncurses::stdscr(), true);
        ncurses::noecho();
        ncurses::curs_set(ncurses::CURSOR_VISIBILITY::CURSOR_INVISIBLE);
        /* Start colors. */

        ncurses::start_color();

        ncurses::init_pair(COLOR_PAIR_DEFAULT, 15, 0);
        ncurses::init_pair(COLOR_PAIR_CURSOR, 251, 235);
        ncurses::init_pair(COLOR_PAIR_HEADERS, 33, 0);
        ncurses::init_pair(COLOR_PAIR_THREAD_INDENT, 5, 0);
        ncurses::init_pair(COLOR_PAIR_THREAD_ODD, 15, 0);
        ncurses::init_pair(COLOR_PAIR_THREAD_EVEN, 15, 233);

        /* Set the window's background color. */
        ncurses::bkgd(
            ' ' as ncurses::chtype | ncurses::COLOR_PAIR(COLOR_PAIR_DEFAULT) as ncurses::chtype,
        );
        TUI {}
    }
}
impl Drop for TUI {
    fn drop(&mut self) {
        ncurses::endwin();
    }
}
