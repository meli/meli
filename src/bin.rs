/*
 * meli - bin.rs
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

mod ui;
use ui::index::*;
use ui::ThreadEvent;

extern crate melib;
use melib::*;

extern crate ncurses;

use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::thread;

fn main() {
    let locale_conf = ncurses::LcCategory::all;
    ncurses::setlocale(locale_conf, "en_US.UTF-8");
    let set = Settings::new();
    let ui = ui::TUI::initialize();

    let (sender, receiver): (SyncSender<ThreadEvent>, Receiver<ThreadEvent>) =
        sync_channel(::std::mem::size_of::<ThreadEvent>());
    {
        let sender = sender.clone();
        let mut ch = None;
        thread::Builder::new()
            .name("input-thread".to_string())
            .spawn(move || loop {
                ch = ncurses::get_wch();
                if let Some(k) = ch {
                    sender.send(ThreadEvent::Input(k)).unwrap();
                }
            })
            .unwrap();
    }

    let mut j = 0;
    let folder_length = set.accounts["norn"].folders.len();
    let mut account = Account::new("norn".to_string(), set.accounts["norn"].clone());
    {
        let sender = sender.clone();
        account.watch(RefreshEventConsumer::new(Box::new(move |r| {
            sender.send(ThreadEvent::from(r)).unwrap();
        })));
    }
    'main: loop {
        ncurses::touchwin(ncurses::stdscr());
        ncurses::mv(0, 0);
        let mailbox = &mut account[j];
        let mut index: Box<Window> = match *mailbox.as_ref().unwrap() {
            Ok(ref v) => Box::new(Index::new(v)),
            Err(ref v) => Box::new(ErrorWindow::new((*v).clone())),
        };
        //eprintln!("{:?}", set);
        ncurses::refresh();

        index.draw();

        'inner: loop {
            match receiver.recv().unwrap() {
                ThreadEvent::Input(k) => match k {
                    ncurses::WchResult::KeyCode(k @ ncurses::KEY_UP)
                    | ncurses::WchResult::KeyCode(k @ ncurses::KEY_DOWN) => {
                        index.handle_input(k);
                        continue;
                    }
                    ncurses::WchResult::Char(k @ 10) => {
                        index.handle_input(k as i32);
                        continue;
                    }
                    ncurses::WchResult::KeyCode(k @ ncurses::KEY_F1) => {
                        if !index.handle_input(k) {
                            break 'main;
                        }
                    }
                    ncurses::WchResult::Char(113) => {
                        break 'main;
                    }
                    ncurses::WchResult::Char(74) => {
                        if j < folder_length - 1 {
                            j += 1;
                            break 'inner;
                        }
                    }
                    ncurses::WchResult::Char(75) => {
                        if j > 0 {
                            j -= 1;
                            break 'inner;
                        }
                    }
                    ncurses::WchResult::KeyCode(ncurses::KEY_RESIZE) => {
                        eprintln!("key_resize");
                        index.redraw();
                        continue;
                    }
                    _ => {}
                },
                ThreadEvent::RefreshMailbox { name: n } => {
                    eprintln!("Refresh mailbox {}", n);
                }
            }
        }
    }
    drop(ui);
}
