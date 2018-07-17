/*
 * meli - bin.rs
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
extern crate melib;
#[macro_use]
extern crate nom;
extern crate termion;
extern crate notify_rust;

pub mod ui;
use ui::*;
pub use melib::*;

use std::thread;
use std::io::{stdout, stdin, };

#[macro_use]
extern crate chan;
extern crate chan_signal;

use chan_signal::Signal;


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

    /* Catch SIGWINCH to handle terminal resizing */
    let signal = chan_signal::notify(&[Signal::WINCH]);

    /* Create a channel to communicate with other threads. The main process is the sole receiver.
     * */
    let (sender, receiver) = chan::sync(::std::mem::size_of::<ThreadEvent>());

    {
        let sender = sender.clone();
        thread::Builder::new().name("input-thread".to_string()).spawn(move || {
            get_events(stdin, move | k| { sender.send(ThreadEvent::Input(k));
            })}).unwrap();
    }

    /* Create the application State. This is the 'System' part of an ECS architecture */
    let mut state = State::new(_stdout, sender);

    /* Register some reasonably useful interfaces */
    let menu = Entity {component: Box::new(AccountMenu::new(&state.context.accounts)) };
    let listing = MailListing::new();
    let b = Entity { component: Box::new(listing) };
    let window  = Entity { component: Box::new(VSplit::new(menu, b, 80)) };
    let status_bar = Entity { component: Box::new(StatusBar::new(window)) };
    state.register_entity(status_bar);


    let xdg_notifications = Entity { component: Box::new(ui::components::notifications::XDGNotifications {}) };
    state.register_entity(xdg_notifications);

    /* Keep track of the input mode. See ui::UIMode for details */
    let mut mode: UIMode = UIMode::Normal;
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any entities have sent reply events to State. */
            let events: Vec<UIEvent> = state.context.get_replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();
            /* Poll on all channels. Currently we have the input channel for stdin, watching events  and the signal watcher. */
            chan_select! {
                receiver.recv() -> r => {
                    match r.unwrap() {
                        ThreadEvent::Input(k) => {
                            match mode {
                                UIMode::Normal => {
                                    match k {
                                        Key::Char('q') | Key::Char('Q') => {
                                            drop(state);
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
                            state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Notification(n.clone())});
                            state.redraw();
                            /* Don't handle this yet. */
                            eprintln!("Refresh mailbox {}", n);
                        },
                        ThreadEvent::UIEventType(e) => {
                            state.rcv_event(UIEvent { id: 0, event_type: e});
                            state.render();
                        },
                    }
                },
                signal.recv() -> signal => {
                    if let Some(Signal::WINCH) = signal {
                        state.update_size();
                        state.render();
                        state.redraw();
                    }
                },
            }
        }
    }
}
