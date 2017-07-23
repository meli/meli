extern crate ncurses;
pub mod index;
pub mod pager;
/* Color pairs; foreground && background. */
pub static COLOR_PAIR_DEFAULT: i16 = 1;
pub static COLOR_PAIR_CURSOR: i16 = 2;
pub static COLOR_PAIR_HEADERS: i16 = 3;
pub fn initialize() {
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

    /* Set the window's background color. */
    ncurses::bkgd(
        ' ' as ncurses::chtype | ncurses::COLOR_PAIR(COLOR_PAIR_DEFAULT) as ncurses::chtype,
    );
}
