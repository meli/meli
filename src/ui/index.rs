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
extern crate ncurses;
extern crate maildir;
extern crate mailparse;
extern crate chrono;
use mailbox::*;

use self::chrono::NaiveDateTime;

/* Index represents a UI list of mails */
pub struct Index {
    mailbox: Mailbox,

    win: ncurses::WINDOW,
    pad: ncurses::WINDOW,
    screen_width: i32,
    screen_height: i32,

    threaded: bool,

    cursor_idx: usize,
}
impl Index {
    pub fn new(mailbox: Mailbox) -> Index {
        let mut screen_height = 0;
        let mut screen_width = 0;
        /* Get the screen bounds. */
        ncurses::getmaxyx(ncurses::stdscr(), &mut screen_height, &mut screen_width);
        // let win = ncurses::newwin( ncurses::LINES(), ncurses::COLS()-30, 0, 30);
        let win = ncurses::newwin(0, 0, 0, 0);
        ncurses::getmaxyx(win, &mut screen_height, &mut screen_width);
        //eprintln!("length is {}\n", length);
        let pad = ncurses::newpad(mailbox.get_length() as i32, screen_width);
        ncurses::wbkgd(
            pad,
            ' ' as ncurses::chtype |
                ncurses::COLOR_PAIR(super::COLOR_PAIR_DEFAULT) as ncurses::chtype,
        );
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

    pub fn draw(&mut self) {
        let mut x = 0;
        let mut y = 0;
        ncurses::getbegyx(self.win, &mut y, &mut x);

        ncurses::wclear(self.pad);

        let mut idx = 0;
        if self.threaded {
            /* Draw threaded view. */
            for i in self.mailbox.threaded_collection.iter() {
                let container = self.mailbox.get_thread(*i);

                assert!(container.has_message(), true);
                let x = &self.mailbox.collection[container.get_message().unwrap()];
                if idx == self.cursor_idx {
                    Index::draw_entry(self.pad, x, idx, container.get_indentation(), false, true);
                } else {
                    Index::draw_entry(self.pad, x, idx, container.get_indentation(), false, false);
                }
                idx += 1;

            }
        } else {
            for x in self.mailbox.collection.as_mut_slice() {
                if idx == self.cursor_idx {
                    Index::draw_entry(self.pad, x, idx, 0, false, true);
                } else {
                    Index::draw_entry(self.pad, x, idx, 0, false, false);
                }
                idx += 1;
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

    pub fn scroll(&mut self, motion: i32) {
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
            }
            ncurses::KEY_DOWN => {
                if self.cursor_idx < self.mailbox.get_length() - 1 {
                    self.cursor_idx += 1;
                } else {
                    return;
                }
            }
            _ => {
                return;
            }
        }

        /* Draw newly highlighted entry */
        ncurses::wmove(self.pad, self.cursor_idx as i32, 0);
        /* Borrow x from self.mailbox in separate scopes or else borrow checker complains */
        {
            let i: usize = 
            if self.threaded { 
                self.mailbox.get_threaded_mail(self.cursor_idx)
            } else {
                self.cursor_idx
            };
            let (ref mut x, thread) = self.mailbox.get_mail_and_thread(i);
            Index::draw_entry(self.pad, x, self.cursor_idx, thread.get_indentation(), false, true);
        }
        /* Draw previous highlighted entry normally */
        ncurses::wmove(self.pad, prev_idx as i32, 0);
        {
            let i: usize = 
            if self.threaded { 
                self.mailbox.get_threaded_mail(prev_idx)
            } else {
                prev_idx
            };
            let (ref mut x, thread) = self.mailbox.get_mail_and_thread(i);
            Index::draw_entry(self.pad, x, prev_idx, thread.get_indentation(), false, false);
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
            /* touch Index window (tell ncurses to redraw the entire window in
             * next refresh) */
            ncurses::touchwin(self.win);
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
    fn draw_entry(win: ncurses::WINDOW, mail: &Mail, i: usize, indent: usize,
                  has_sibling: bool, highlight: bool) {
        if highlight {
            ncurses::wattron(win,
                             ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
        }
        ncurses::waddstr(win, &format!("{}\t", i));
        let dt = NaiveDateTime::from_timestamp(mail.get_date(), 0);
        ncurses::waddstr(win, &dt.format("%Y-%m-%d %H:%M:%S").to_string());
        ncurses::waddch(win, '\t' as u64);
        for _ in 0..indent {
            ncurses::waddch(win, ' ' as u64);
        }
        if indent > 0 {
            ncurses::wattron(win,
                             ncurses::COLOR_PAIR(super::COLOR_PAIR_THREAD_INDENT));
            if has_sibling {
                ncurses::waddstr(win, "│");
            } else {
                ncurses::waddstr(win, "└");
            }
            ncurses::waddstr(win, "->");
            ncurses::wattroff(win,
                              ncurses::COLOR_PAIR(super::COLOR_PAIR_THREAD_INDENT));
        }
        if highlight {
            ncurses::wattron(win,
                             ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
        }
        ncurses::waddstr(win, &format!("{:.85}",mail.get_subject()));
        if highlight { 
            ncurses::wattroff(win,
                              ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
        }
        ncurses::waddstr(win, "\n");
    }
    pub fn show_pager(&mut self) {
        ncurses::getmaxyx(self.win,
                          &mut self.screen_height, &mut self.screen_width);
        let x: &mut Mail;
        
        if self.threaded { 
            let i = self.mailbox.get_threaded_mail(self.cursor_idx);
            x = &mut self.mailbox.collection[i];
        } else {
            x = &mut self.mailbox.collection[self.cursor_idx];
        }
        let mut pager = super::pager::Pager::new(self.win, &mut x.get_entry());
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
        drop(pager); // drop pager before next refresh
        ncurses::wrefresh(self.win);
    }
}
impl Drop for Index  {
    fn drop(&mut self) {
        ncurses::delwin(self.win);
        ncurses::delwin(self.pad);
        ncurses::endwin();
    }
}
