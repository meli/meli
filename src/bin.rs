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

/*! This crate contains the frontend stuff of the application. The application entry way on `src/bin.rs` creates an event loop and passes input to the `ui` module.

The mail handling stuff is done in the `melib` crate which includes all backend needs. The split is done to theoretically be able to create different frontends with the same innards.
 */

extern crate melib;
extern crate ui;

use ui::*;
pub use melib::*;

use std::thread;
use std::io::{stdout,};

#[macro_use]
extern crate chan;
extern crate chan_signal;

use chan_signal::Signal;

fn make_input_thread(sx: chan::Sender<ThreadEvent>, rx: chan::Receiver<bool>) -> () {
        let stdin = std::io::stdin();
        thread::Builder::new().name("input-thread".to_string()).spawn(move || {

            get_events(stdin,
                       |k| {
                           sx.send(ThreadEvent::Input(k));
                       },
                       || {
                           sx.send(ThreadEvent::UIEventType(UIEventType::ChangeMode(UIMode::Fork)));
                       }, rx)}).unwrap();


}
fn main() {
    /* Lock all stdio outs */
    let _stdout = stdout();
    let mut _stdout = _stdout.lock();
    /*
       let _stderr = stderr();
       let mut _stderr = _stderr.lock();
       */

    /* Catch SIGWINCH to handle terminal resizing */
    let signal = chan_signal::notify(&[Signal::WINCH]);

    /* Create a channel to communicate with other threads. The main process is the sole receiver.
     * */
    let (sender, receiver) = chan::sync(::std::mem::size_of::<ThreadEvent>());


    /*
     * Create async channel to block the input-thread if we need to fork and stop it from reading
     * stdin, see get_events() for details
     * */
    let (tx, rx) = chan::async();
    /* Get input thread handle to kill it if we need to */
    make_input_thread(sender.clone(), rx.clone());

    /* Create the application State. This is the 'System' part of an ECS architecture */
    let mut state = State::new(_stdout, sender.clone(), tx );

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
    'main: loop {
        state.render();
                    eprintln!("entered main loop");

        'inner: loop {
                    eprintln!("entered inner loop");
            /* Check if any entities have sent reply events to State. */
            let events: Vec<UIEvent> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }

            state.redraw();
            /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
            chan_select! {
                receiver.recv() -> r => {
                    eprintln!("received {:?}", r);
                    match r.unwrap() {
                        ThreadEvent::Input(k) => {
                    eprintln!(" match input");
                            match state.mode {
                                UIMode::Normal => {
                                    match k {
                                        Key::Char('q') | Key::Char('Q') => {
                                            drop(state);
                                            break 'main;
                                        },
                                        Key::Char(';') => {
                                            state.mode = UIMode::Execute;
                                            state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ChangeMode(UIMode::Execute)});
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
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ChangeMode(UIMode::Normal)});
                                            state.redraw();
                                        },
                                        k @ Key::Char(_) => {
                                            state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ExInput(k)});
                                            state.redraw();
                                        },
                                        _ => {},
                                    }
                                },
                                UIMode::Fork => {
                    eprintln!("UIMODE FORK");
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
                        },
                        ThreadEvent::RefreshMailbox { name : n } => {
                            state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Notification(n.clone())});
                            state.redraw();
                            /* Don't handle this yet. */
                            eprintln!("Refresh mailbox {}", n);
                        },
                        ThreadEvent::UIEventType(UIEventType::ChangeMode(f)) => {
                            state.mode = f;
                            break 'inner; // `goto` 'reap loop, and wait on child.
                        }
                        ThreadEvent::UIEventType(e) => {
                    eprintln!(" match event");
                            state.rcv_event(UIEvent { id: 0, event_type: e});
                            state.render();
                        },
                    }
                },
                signal.recv() -> signal => {
                    if state.mode != UIMode::Fork  {
                        if let Some(Signal::WINCH) = signal {
                            eprintln!("resize, mode is {:?}", state.mode);
                            state.update_size();
                            state.render();
                            state.redraw();
                        }
                    }
                },
            }
        } // end of 'inner

        'reap: loop {
            eprintln!("reached reap loop");
            match state.try_wait_on_child() {
                Some(true) => {
                    make_input_thread(sender.clone(), rx.clone());
                    state.mode = UIMode::Normal;
                    state.render();
                },
                Some(false) => {
                    use std::{thread, time};

                    let ten_millis = time::Duration::from_millis(1500);

                    thread::sleep(ten_millis);
                    continue 'reap;
                },
                None => {break 'reap;},
            }


        }
    }
}
