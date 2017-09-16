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

use mailbox;

extern crate ncurses;

/* Pager represents the part of the UI that shows the mail headers and body for
 * viewing */
pub struct Pager {
    win: ncurses::WINDOW,
    pad: ncurses::WINDOW,
    rows: i32,
    header_height: i32,

    curr_y: i32,
}

impl Pager {
    pub fn new(parent: ncurses::WINDOW,
               entry: &mut mailbox::Envelope) -> Pager {
        let mut screen_height = 0;
        let mut screen_width = 0;
        ncurses::getmaxyx(parent, &mut screen_height, &mut screen_width);
        let mut x = 0;
        let mut y = 0;
        ncurses::getbegyx(parent, &mut y, &mut x);
        let win = ncurses::subwin(
            parent,
            screen_height - (screen_height / 3),
            screen_width,
            y + (screen_height / 3),
            x,
        );
        ncurses::wclear(win);
        //ncurses::touchwin(win);
        for _ in 1..screen_width + 1 {
            ncurses::waddstr(win, "─");
        }
        let (pad, rows, header_height) = Pager::print_entry(win, entry);
        ncurses::wbkgd(
            pad,
            ' ' as ncurses::chtype |
                ncurses::COLOR_PAIR(super::COLOR_PAIR_DEFAULT) as ncurses::chtype,
        );
        ncurses::wrefresh(win);
        Pager {
            pad: pad,
            win: win,
            rows: rows,
            header_height: header_height,
            curr_y: 0,
        }
    }
    pub fn scroll(&mut self, motion: i32) {
        let mut h = 0;
        let mut w = 0;
        ncurses::getmaxyx(self.win, &mut h, &mut w);
        let mut x = 0;
        let mut y = 0;
        ncurses::getparyx(self.win, &mut y, &mut x);
        let mut p_x = 0;
        let mut p_y = 0;
        ncurses::getbegyx(self.win, &mut p_y, &mut p_x);
        let pager_size: i32 = h - self.header_height;
        if pager_size == 0 {
            return;
        }
        match motion {
            ncurses::KEY_UP => {
                if self.curr_y > 0 {
                    self.curr_y -= 1;
                }
            }
            ncurses::KEY_DOWN => {
                if self.curr_y < self.rows && self.rows - self.curr_y > pager_size {
                    self.curr_y += 1;
                }
            }
            ncurses::KEY_NPAGE => {
                if self.curr_y + h < self.rows && self.rows - self.curr_y - h > pager_size {
                    self.curr_y += pager_size;
                } else {
                    self.curr_y = if self.rows > h {
                        self.rows - pager_size
                    } else {
                        0
                    };
                }
            }
            ncurses::KEY_PPAGE => {
                if self.curr_y >= pager_size {
                    self.curr_y -= pager_size;
                } else {
                    self.curr_y = 0
                }
            }
            _ => {}
        }
        /*
         *  ┌ ┏━━━━━━━━━┓         ┐
         *  │ ┃         ┃         │
         *  y ┃         ┃         │
         *  │ ┃         ┃         │
         *  ├ x━━━━━━━━━┫ ┐       │ index
         *  │ ┃         ┃ │       │
         *  h ┃         ┃ │ pager │
         *  └ ┗━━━━━━━━━w ┘       ┘
         */
        ncurses::touchwin(self.win);
        ncurses::prefresh(
            self.pad,
            self.curr_y,
            0,
            y + self.header_height,
            p_x + x,
            y + h - 1,
            w - 1,
        );
    }
    fn print_entry_headers(win: ncurses::WINDOW, mail: &mut mailbox::Envelope) -> i32 {
        let mut i = 0;
        ncurses::wattron(win, ncurses::COLOR_PAIR(super::COLOR_PAIR_HEADERS));
        ncurses::waddstr(win, "Date: ");
        ncurses::waddstr(
            win,
            mail.get_date_as_str()
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::waddstr(win, "From: ");
        ncurses::waddstr(
            win,
            mail.get_from(),
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::waddstr(win, "To: ");
        ncurses::waddstr(
            win,
            mail.get_to(),
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::waddstr(win, "Subject: ");
        ncurses::waddstr(
            win,
            mail.get_subject(),
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::waddstr(win, "Message-ID: ");
        ncurses::waddstr(
            win,
            mail.get_message_id_raw(),
            //mail.get_message_id(),
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::waddstr(win, "References: ");
        ncurses::waddstr(
            win,
            &format!("{:?}", mail.get_references()),
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::waddstr(win, "In-Reply-To: ");
        ncurses::waddstr(
            win,
            mail.get_in_reply_to_raw(),
        );
        ncurses::waddstr(win, "\n");
        i += 1;
        ncurses::wattroff(win, ncurses::COLOR_PAIR(super::COLOR_PAIR_HEADERS));
        /* return number of header lines so we don't overwrite it  */
        i
    }
    fn print_entry_content(
        win: ncurses::WINDOW,
        mail: &mut mailbox::Envelope,
        height: i32) -> (ncurses::WINDOW, i32, i32) {
        let mut h = 0;
        let mut w = 0;
        /* height and width of self.win, the pager window */
        ncurses::getmaxyx(win, &mut h, &mut w);
        let mut x = 0;
        let mut y = 0;
        /* y,x coordinates of upper left corner of win */
        ncurses::getparyx(win, &mut y, &mut x);

        let text = mail.get_body().get_text();
        let lines: Vec<&str> = text.trim().split('\n').collect();
        let lines_length = lines.len();

        let pad = ncurses::newpad(lines_length as i32, 1024);
        ncurses::wclear(pad);
        for l in lines {
            ncurses::waddstr(pad, &l.replace("%", "%%"));
            ncurses::waddstr(pad, "\n");
        }
        /*
         *      ┌ ┏━━━━━━━━━┓         ┐
         *      │ ┃         ┃         │
         *      y ┃         ┃         │
         *      │ ┃         ┃         │
         *      ├ x━━━━━━━━━┫ ┐       │ index
         *      │ ┃         ┃ │       │
         *      h ┃         ┃ │ pager │
         *      └ ┗━━━━━━━━━w ┘       ┘
         */
        ncurses::pnoutrefresh(pad, 0, 0, y + height, x, y + height - 1, w - 1);
        (pad, lines_length as i32, height)
    }
    fn print_entry(
        win: ncurses::WINDOW,
        mail: &mut mailbox::Envelope) -> (ncurses::WINDOW, i32, i32) {
        let header_height = Pager::print_entry_headers(win, mail);
        Pager::print_entry_content(win, mail, header_height + 2)
    }
}

impl Drop for Pager {
    fn drop(&mut self) {
        ncurses::delwin(self.pad);
        ncurses::wclear(self.win);
        ncurses::delwin(self.win);
    }
}
