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
extern crate melib;
#[macro_use]
extern crate nom;
extern crate termion;

pub mod ui;
use ui::*;
pub use melib::*;

use std::sync::mpsc::{sync_channel, SyncSender, Receiver};
use std::thread;
use std::io::{stdout, stdin, };

fn main() {
    /* Lock all stdios */
    let _stdout = stdout();
    let mut _stdout = _stdout.lock();
    let stdin = stdin();
    let stdin = stdin;
    /*
       let _stderr = stderr();
       let mut _stderr = _stderr.lock();
       */



    let (sender, receiver): (SyncSender<ThreadEvent>, Receiver<ThreadEvent>) = sync_channel(::std::mem::size_of::<ThreadEvent>());
    {
        let sender = sender.clone();
        thread::Builder::new().name("input-thread".to_string()).spawn(move || {
            get_events(stdin, move | k| { sender.send(ThreadEvent::Input(k)).unwrap();
            })}).unwrap();
    }

    /*
       let folder_length = set.accounts["test_account"].folders.len();
       let mut account = Account::new("test_account".to_string(), set.accounts["test_account"].clone(), backends);

       {
       let sender = sender.clone();
       account.watch(RefreshEventConsumer::new(Box::new(move |r| {
       sender.send(ThreadEvent::from(r)).unwrap();
       })));
       }
       */
    let mut state = State::new(_stdout);

    let menu = Entity {component: Box::new(AccountMenu::new(&state.context.accounts)) };
    let listing = MailListing::new(Mailbox::new_dummy());
    let b = Entity { component: Box::new(listing) };
    let window  = Entity { component: Box::new(VSplit::new(menu,b,90)) };
    let status_bar = Entity { component: Box::new(StatusBar::new(window)) };
    state.register_entity(status_bar);

    /*
    let mut idxa = 0;
    let mut idxm = 0;
    let account_length = state.context.accounts.len();
    */
    let mut mode: UIMode = UIMode::Normal;
    'main: loop {
        /*
        state.refresh_mailbox(idxa,idxm);
        */
        /*
        let folder_length = state.context.accounts[idxa].len();
        */
        state.render();

        'inner: loop {
            let events: Vec<UIEvent> = state.context.get_replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();
            match receiver.recv().unwrap() {
                ThreadEvent::Input(k) => {
                    match mode {
                        UIMode::Normal => {
                            match k {
                                Key::Char('q') | Key::Char('Q') => {
                                    break 'main;
                                },
                                Key::Char(';') => {
                                    mode = UIMode::Execute;
                                    state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ChangeMode(mode)});
                                    state.redraw();
                                }
                                key  => {
                                    state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Input(key)});
                                    state.redraw();
                                },
                                _ => {}
                            }
                        },
                        UIMode::Execute => {
                            match k {
                                Key::Char('\n') | Key::Esc => {
                                    mode = UIMode::Normal;
                                    state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ChangeMode(mode)});
                                    state.redraw();
                                },
                                k @ Key::Char(_) => {
                                    state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ExInput(k)});
                                    state.redraw();
                                },
                                _ => {},
                            }
                        },
                    }
                },
                ThreadEvent::RefreshMailbox { name : n } => {
                    eprintln!("Refresh mailbox {}", n);
                },
                ThreadEvent::UIEventType(e) => {
                    state.rcv_event(UIEvent { id: 0, event_type: e});
                    state.render();
                },
            }
        }
    }
}
