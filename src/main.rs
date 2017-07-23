extern crate ncurses;
extern crate maildir;
extern crate mailparse;

mod ui;

use ui::index::*;

fn main() {
    let locale_conf = ncurses::LcCategory::all;
    ncurses::setlocale(locale_conf, "en_US.UTF-8");
    ui::initialize();
    let maildir = maildir::Maildir::from("./Inbox");
    let iter = maildir.list_cur();
    let mut index = Index::new(iter);
    ncurses::refresh();

    index.draw();

    let mut ch;
    loop {
        ch = ncurses::get_wch();
        match ch {
            Some(ncurses::WchResult::KeyCode(k @ ncurses::KEY_UP)) |
            Some(ncurses::WchResult::KeyCode(k @ ncurses::KEY_DOWN)) => {
                index.scroll(k);
                continue;
            }
            Some(ncurses::WchResult::Char(10)) => {
                index.show_pager();
                index.draw();
                continue;
            }
            Some(ncurses::WchResult::KeyCode(ncurses::KEY_F1)) => {
                break;
            }
            _ => {}
        }
    }
}
