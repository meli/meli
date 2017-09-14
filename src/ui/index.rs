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
use mailbox::email::Mail;
use mailbox::*;
use error::MeliError;
use std::error::Error;

extern crate ncurses;

pub trait Window {
    fn draw(&mut self) -> ();
    fn redraw(&mut self) -> ();
    fn handle_input(&mut self, input: i32) -> ();
}

pub struct ErrorWindow {
    description: String,

    win: ncurses::WINDOW,
}

impl Window for ErrorWindow {
    fn draw(&mut self) -> () {
        ncurses::waddstr(self.win, &self.description);
        ncurses::wrefresh(self.win);
    }
    fn redraw(&mut self) -> () {
        ncurses::waddstr(self.win, &self.description);
        ncurses::wrefresh(self.win);
    }
    fn handle_input(&mut self, _: i32) -> () {


    }
}
impl ErrorWindow {
    pub fn new(err: MeliError) -> Self {
        /*
        let mut screen_height = 0;
        let mut screen_width = 0;
        /* Get the screen bounds. */
        ncurses::getmaxyx(ncurses::stdscr(), &mut screen_height, &mut screen_width);
        // let win = ncurses::newwin( ncurses::LINES(), ncurses::COLS()-30, 0, 30);
        */
        let win = ncurses::newwin(0, 0, 0, 0);
        ErrorWindow {
            description: err.description().to_string(),
            win: win,
        }
    }
}

/* Index represents a UI list of mails */
pub struct Index {
    mailbox: Mailbox,

    win: ncurses::WINDOW,
    pad: ncurses::WINDOW,
    screen_width: i32,
    screen_height: i32,

    /* threading */
    threaded: bool,


    cursor_idx: usize,
}


impl Window for Index {
    fn draw(&mut self) {
        if self.mailbox.get_length() == 0 {
            return;
        }
        let mut x = 0;
        let mut y = 0;
        ncurses::getbegyx(self.win, &mut y, &mut x);

        //ncurses::wclear(self.pad);

        if self.threaded {
            let mut indentations : Vec<bool> = Vec::with_capacity(6);
            /* Draw threaded view. */
            let mut iter = self.mailbox.threaded_collection.iter().enumerate().peekable();
            /* This is just a desugared for loop so that we can use .peek() */
            while let Some((idx, i)) = iter.next() {
                let container = self.mailbox.get_thread(*i);
                let indentation = container.get_indentation();

                assert_eq!(container.has_message(), true);
                match iter.peek() {
                    Some(&(_, x)) if self.mailbox.get_thread(*x).get_indentation() == indentation => {
                        indentations.pop();
                        indentations.push(true);
                    },
                    _ => {
                        indentations.pop();
                        indentations.push(false);
                    }
                }
                if container.has_sibling() {
                    indentations.pop();
                    indentations.push(true);
                } 
                let x = &self.mailbox.collection[container.get_message().unwrap()];
                Index::draw_entry(self.pad, x, idx, indentation, container.has_sibling(), container.has_parent(), idx == self.cursor_idx, container.get_show_subject(), Some(&indentations));
                match iter.peek() {
                    Some(&(_, x)) if self.mailbox.get_thread(*x).get_indentation() > indentation => {
                        indentations.push(false);
                    },
                    Some(&(_, x)) if self.mailbox.get_thread(*x).get_indentation() < indentation => {
                        for _ in 0..(indentation - self.mailbox.get_thread(*x).get_indentation()) {
                            indentations.pop();
                        }
                    },
                    _ => {
                    }
                }
            }
            /*
            for (idx, i) in self.mailbox.threaded_collection.iter().enumerate() {
                let container = self.mailbox.get_thread(*i);

                assert_eq!(container.has_message(), true);
                if container.has_sibling() {
                    indentations.pop();
                    indentations.push(true);
                } 
                let x = &self.mailbox.collection[container.get_message().unwrap()];
                Index::draw_entry(self.pad, x, idx, container.get_indentation(), container.has_sibling(), idx == self.cursor_idx, container.get_show_subject(), Some(&indentations));
                if container.has_children() {
                    indentations.push(false);
                } else {
                    indentations.pop();
                }
            }
            */
        } else {
            for (idx, x) in self.mailbox.collection.as_mut_slice().iter().enumerate() {
                Index::draw_entry(self.pad, x, idx, 0, false, false, idx == self.cursor_idx, true, None);
            }
        }
        ncurses::getmaxyx(self.win, &mut self.screen_height, &mut self.screen_width);
        let pminrow =
            (self.cursor_idx as i32).wrapping_div(self.screen_height) * self.screen_height;
        ncurses::prefresh(
            self.pad,
            pminrow,
            0,
            y,
            x,
            self.screen_height - 1,
            self.screen_width - 1,
        );
    }
    fn redraw(&mut self) -> () {
        ncurses::wnoutrefresh(self.win);
        ncurses::doupdate();
        if self.mailbox.get_length() == 0 {
            return;
        } 
        /* Draw newly highlighted entry */
        ncurses::wmove(self.pad, self.cursor_idx as i32, 0);
        let pair = super::COLOR_PAIR_CURSOR;
        ncurses::wchgat(self.pad, -1, 0, pair);
        ncurses::getmaxyx(self.win, &mut self.screen_height, &mut self.screen_width);
        let mut x = 0;
        let mut y = 0;
        ncurses::getbegyx(self.win, &mut y, &mut x);
        let pminrow =
            (self.cursor_idx as i32).wrapping_div(self.screen_height) * self.screen_height;
        ncurses::touchline(self.pad, 1, 1);
        ncurses::prefresh(
            self.pad,
            pminrow,
            0,
            y,
            x,
            self.screen_height - 1,
            self.screen_width - 1,
        );
        ncurses::wrefresh(self.win);
    }
    fn handle_input(&mut self, motion: i32) {
        if self.mailbox.get_length() == 0 {
            return;
        }
        ncurses::getmaxyx(self.win, &mut self.screen_height, &mut self.screen_width);
        if self.screen_height == 0 {
            return;
        }
        let mut x = 0;
        let mut y = 0;
        ncurses::getbegyx(self.win, &mut y, &mut x);
        let prev_idx = self.cursor_idx;
        match motion {
            ncurses::KEY_UP => {
                if self.cursor_idx > 0 {
                    self.cursor_idx -= 1;
                } else {
                    return;
                }
            },
            ncurses::KEY_DOWN => {
                if self.cursor_idx < self.mailbox.get_length() - 1 {
                    self.cursor_idx += 1;
                } else {
                    return;
                }
            },
            10 => {
                self.show_pager();
                self.redraw();
            },
            _ => {
                return;
            }
        }

        /* Draw newly highlighted entry */
        ncurses::wmove(self.pad, self.cursor_idx as i32, 0);
        let pair = super::COLOR_PAIR_CURSOR;
        ncurses::wchgat(self.pad, -1, 0, pair);
        /* Draw previous highlighted entry normally */
        ncurses::wmove(self.pad, prev_idx as i32, 0);
        {
            let pair = if self.threaded && prev_idx % 2 == 0 {
                super::COLOR_PAIR_THREAD_EVEN
            } else if self.threaded {
                super::COLOR_PAIR_THREAD_ODD
            } else {
                super::COLOR_PAIR_DEFAULT
            };
            ncurses::wchgat(self.pad, 32, 0, pair);
            ncurses::wmove(self.pad, prev_idx as i32, 32);
            /* If first character in subject column is space, we need to check for indentation
             * characters and highlight them appropriately */
            if (ncurses::winch(self.pad) & ncurses::A_CHARTEXT()) == ' ' as u64 {
                let mut x = 32;
                loop {
                    match ncurses::mvwinch(self.pad, prev_idx as i32, x) & ncurses::A_CHARTEXT() {
                        32 => {  /* ASCII code for space */
                            ncurses::wchgat(self.pad, x, 0, pair);
                        },
                        62 => { /* ASCII code for '>' */
                            ncurses::wchgat(self.pad, x, 0, super::COLOR_PAIR_THREAD_INDENT);
                            ncurses::wmove(self.pad, prev_idx as i32, x + 1);
                            break;
                        }
                        _ => {
                            ncurses::wchgat(self.pad, x, 0, super::COLOR_PAIR_THREAD_INDENT);
                        },
                    }
                    x += 1;
                }
            } 
            ncurses::wchgat(self.pad, -1, 0, pair);
        }

        /* Calculate the pad row of the first entry to be displayed in the window */
        let pminrow =
            (self.cursor_idx as i32).wrapping_div(self.screen_height) * self.screen_height;
        let pminrow_prev = (prev_idx as i32).wrapping_div(self.screen_height) * self.screen_height;
        /*
         * Refresh window if new page has less rows than window rows, ie
         * window rows = r
         * pad rows (total emails) = n
         * pminrow = i
         *
         *      ┌- i
         *      │  i+1
         *      │  i+2
         *    r ┤  ...
         *      │  n
         *      │  ..  ┐
         *      │  i-2 ├ 'dead' entries (must be cleared)
         *      └  i-1 ┘
         */
        if pminrow != pminrow_prev &&
            pminrow + self.screen_height > self.mailbox.get_length() as i32 {
            /* touch dead entries in index (tell ncurses to redraw the empty lines next refresh) */
            let live_entries = self.mailbox.get_length() as i32 - pminrow;
            ncurses::wredrawln(self.win, live_entries, self.screen_height);
            ncurses::wrefresh(self.win);
        }
        ncurses::prefresh(
            self.pad,
            pminrow,
            0,
            y,
            x,
            self.screen_height - 1,
            self.screen_width - 1,
        );
    }
}
impl Index {
    pub fn new(mailbox: &Mailbox) -> Index {
        let mailbox = (*mailbox).clone();
        let mut screen_height = 0;
        let mut screen_width = 0;
        /* Get the screen bounds. */
        ncurses::getmaxyx(ncurses::stdscr(), &mut screen_height, &mut screen_width);
        // let win = ncurses::newwin( ncurses::LINES(), ncurses::COLS()-30, 0, 30);
        let win = ncurses::newwin(0, 0, 0, 0);
        ncurses::getmaxyx(win, &mut screen_height, &mut screen_width);
        //eprintln!("length is {}\n", length);
        let mailbox_length = mailbox.get_length();
        let pad = ncurses::newpad(mailbox_length as i32, 1500);
        ncurses::wbkgd(
            pad,
            ' ' as ncurses::chtype |
                ncurses::COLOR_PAIR(super::COLOR_PAIR_DEFAULT) as ncurses::chtype,
        );
        if mailbox_length == 0 {
            ncurses::printw(&format!("Mailbox {} is empty.\n", mailbox.path));
            ncurses::refresh();
        }
        let mut color = true;
        let mut thread_color = Vec::with_capacity(mailbox_length);
        for i in &mailbox.threaded_collection {
            let container = mailbox.get_thread(*i);
            if !container.has_parent() {
                color = !color;
            }
            thread_color.push(color);
        }
        Index {
            mailbox: mailbox,
            win: win,
            pad: pad,
            screen_width: 0,
            screen_height: 0,
            threaded: true,
            cursor_idx: 0,
        }
    }

    /* draw_entry() doesn't take &mut self because borrow checker complains if it's called from
     * another method. */
    fn draw_entry(win: ncurses::WINDOW, mail: &Mail, i: usize, indent: usize,
                  has_sibling: bool, has_parent: bool, highlight: bool,
                  show_subject: bool, indentations: Option<&Vec<bool>>) {
        /* TODO: use addchstr */
        let pair =
            if highlight {
                super::COLOR_PAIR_CURSOR
            } else if i % 2 == 0 {
                super::COLOR_PAIR_THREAD_EVEN
            } else {
                super::COLOR_PAIR_THREAD_ODD
            };
        let attr = ncurses::COLOR_PAIR(pair);
        ncurses::wattron(win, attr);

        ncurses::waddstr(win, &format!("{}\t", i));
        ncurses::waddstr(win, &mail.get_datetime().format("%Y-%m-%d %H:%M:%S").to_string());
        ncurses::waddch(win, '\t' as u64);
        for i in 0..indent {
            if indentations.is_some() && indentations.unwrap().len() > i && indentations.unwrap()[i] {
                ncurses::wattron(win, ncurses::COLOR_PAIR(super::COLOR_PAIR_THREAD_INDENT));
                ncurses::waddstr(win, "│");
                ncurses::wattroff(win, ncurses::COLOR_PAIR(super::COLOR_PAIR_THREAD_INDENT));
                ncurses::wattron(win, attr);
            } else {
                ncurses::waddch(win, ' ' as u64);
            }
            if i > 0 {
                ncurses::waddch(win, ' ' as u64);
            }
        }
        if indent > 0 {
            ncurses::wattron(win, ncurses::COLOR_PAIR(super::COLOR_PAIR_THREAD_INDENT));
            if has_sibling && has_parent {
                ncurses::waddstr(win, "├");
            } else if has_sibling {
                ncurses::waddstr(win, "┬");
            } else {
                ncurses::waddstr(win, "└");
            }
            ncurses::waddstr(win, "─>");
            ncurses::wattroff(win, ncurses::COLOR_PAIR(super::COLOR_PAIR_THREAD_INDENT));
        }
        ncurses::wattron(win, attr);
        if show_subject {
            ncurses::waddstr(win, &format!("{:.85}",mail.get_subject()));
            /*
            if indent == 0 {
                if mail.get_subject().chars().count() < 85 {
                    for _ in 0..(85 - mail.get_subject().chars().count()) {
                        ncurses::waddstr(win, "▔");
                    }
                }
                ncurses::waddstr(win,"▔");
            }*/
        }
        let mut screen_height = 0;
        let mut screen_width = 0;
        /* Get the screen bounds. */
        let mut x = 0;
        let mut y = 0;
        ncurses::getmaxyx(win, &mut screen_height, &mut screen_width);
        ncurses::getyx(win, &mut y, &mut x);
        ncurses::waddstr(win, &" ".repeat((screen_width - x) as usize));
        ncurses::wattroff(win, attr);
    }
    fn show_pager(&mut self) {
        if self.mailbox.get_length() == 0 {
            return;
        }
        ncurses::getmaxyx(self.win,
                          &mut self.screen_height, &mut self.screen_width);
        let x: &mut Mail = if self.threaded {
            let i = self.mailbox.get_threaded_mail(self.cursor_idx);
            &mut self.mailbox.collection[i]
        } else {
            &mut self.mailbox.collection[self.cursor_idx]
        };
        let mut pager = super::pager::Pager::new(self.win, x);
        pager.scroll(ncurses::KEY_DOWN);
        pager.scroll(ncurses::KEY_UP);
        let mut ch = ncurses::getch();
        while ch != ncurses::KEY_F(1) {
            match ch {
                m @ ncurses::KEY_UP |
                m @ ncurses::KEY_DOWN |
                m @ ncurses::KEY_NPAGE |
                m @ ncurses::KEY_PPAGE => {
                    pager.scroll(m);
                }
                _ => {}
            }
            ch = ncurses::getch();
        }
    }
}
impl Drop for Index  {
    fn drop(&mut self) {
        ncurses::delwin(self.pad);
        ncurses::wclear(self.win);
        ncurses::delwin(self.win);
    }
}
