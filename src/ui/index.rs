extern crate ncurses;
extern crate maildir;
extern crate mailparse;
use mailparse::*;

pub struct Index {
    mailbox: Vec<(usize, maildir::MailEntry)>,
    length: usize,

    win: ncurses::WINDOW,
    pad: ncurses::WINDOW,
    screen_width: i32,
    screen_height: i32,

    cursor_idx: usize,
}
impl Index {
    pub fn new(iter: maildir::MailEntries) -> Index {
        let mut collection: Vec<(usize, maildir::MailEntry)> = Vec::new();
        for (i, x) in iter.enumerate() {
            collection.push((i, x.unwrap()));
        }
        let length = collection.len();
        let mut screen_height = 0;
        let mut screen_width = 0;
        /* Get the screen bounds. */
        ncurses::getmaxyx(ncurses::stdscr(), &mut screen_height, &mut screen_width);
        // let win = ncurses::newwin( ncurses::LINES(), ncurses::COLS()-30, 0, 30);
        let win = ncurses::newwin(0, 0, 0, 0);
        ncurses::getmaxyx(win, &mut screen_height, &mut screen_width);
        ncurses::wbkgd(
            win,
            ' ' as ncurses::chtype |
                ncurses::COLOR_PAIR(super::COLOR_PAIR_DEFAULT) as ncurses::chtype,
        );
        //eprintln!("length is {}\n", length);
        let pad = ncurses::newpad(length as i32, screen_width);
        Index {
            mailbox: collection,
            length: length,
            win: win,
            pad: pad,
            screen_width: 0,
            screen_height: 0,
            cursor_idx: 0,
        }
    }

    pub fn draw(&mut self) {
        let mut x = 0;
        let mut y = 0;
        ncurses::getbegyx(self.win, &mut y, &mut x);

        ncurses::wclear(self.pad);

        for &mut (i, ref mut x) in self.mailbox.as_mut_slice() {
            if i == self.cursor_idx {
                ncurses::wattron(self.pad, ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
            }
            Index::draw_entry(self.pad, x, i);
            if i == self.cursor_idx {
                ncurses::wattroff(self.pad, ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
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
                if self.cursor_idx < self.length - 1 {
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
            let (_, ref mut x) = self.mailbox.as_mut_slice()[self.cursor_idx];
            ncurses::wattron(self.pad, ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
            Index::draw_entry(self.pad, x, self.cursor_idx);
            ncurses::wattroff(self.pad, ncurses::COLOR_PAIR(super::COLOR_PAIR_CURSOR));
        }
        /* Draw previous highlighted entry normally */
        ncurses::wmove(self.pad, prev_idx as i32, 0);
        {
            let (_, ref mut x) = self.mailbox.as_mut_slice()[prev_idx];
            Index::draw_entry(self.pad, x, prev_idx);
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
         *    r ┥  ...
         *      │  n
         *      │  ..  ┐
         *      │  i-2 ├ 'dead' entries (must be empty)
         *      └  i-1 ┘
         */
        if pminrow != pminrow_prev && pminrow + self.screen_height > self.length as i32 {
            /* touch Index window (tell ncurses to redraw the entire window in next refresh */
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
    fn draw_entry(win: ncurses::WINDOW, entry: &mut maildir::MailEntry, i: usize) {
        ncurses::waddstr(win, &format!("{}\t", i));
        let d = entry.headers().unwrap().get_first_value("Date").unwrap();
        match d {
            Some(t) => {
                ncurses::waddstr(win, &t.to_string());
            }
            _ => {}
        }
        ncurses::waddch(win, ' ' as u64);
        let c = entry.headers().unwrap().get_first_value("Subject").unwrap();
        match c {
            Some(t) => {
                ncurses::waddstr(win, &t.to_string());
            }
            _ => {}
        }
        ncurses::wprintw(win, "\n");
    }
    pub fn show_pager(&mut self) {
        ncurses::getmaxyx(self.win, &mut self.screen_height, &mut self.screen_width);
        let (_, ref mut mail) = self.mailbox[self.cursor_idx];
        let mut pager = super::pager::Pager::new(self.win, mail);
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
            // ncurses::wprintw(pager, "test\n");
            //ncurses::wrefresh(pager);
            ch = ncurses::getch();
        }
        drop(pager);
        ncurses::wrefresh(self.win);
    }
}
impl Drop for Index {
    fn drop(&mut self) {
        /* Final prompt before closing. */
        //ncurses::mv(self.screen_height - 1, 0);
        //prompt();
        ncurses::delwin(self.win);
        ncurses::delwin(self.pad);
        ncurses::endwin();
    }
}
