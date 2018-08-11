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

use std::alloc::System;

#[global_allocator]
static GLOBAL: System = System;

extern crate melib;
extern crate ui;

pub use melib::*;
pub use ui::*;

use std::thread;

#[macro_use]
extern crate chan;
extern crate chan_signal;

use chan_signal::Signal;

extern crate nix;

fn make_input_thread(
    sx: chan::Sender<ThreadEvent>,
    rx: chan::Receiver<bool>,
) -> thread::JoinHandle<()> {
    let stdin = std::io::stdin();
    thread::Builder::new()
        .name("input-thread".to_string())
        .spawn(move || {
            get_events(
                stdin,
                |k| {
                    sx.send(ThreadEvent::Input(k));
                },
                || {
                    sx.send(ThreadEvent::UIEvent(UIEventType::ChangeMode(UIMode::Fork)));
                },
                &rx,
            )
        })
        .unwrap()
}

fn main() {
    /* Lock all stdio outs */
    //let _stdout = stdout();
    //let mut _stdout = _stdout.lock();
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
    /* Get input thread handle to join it if we need to */
    let mut _thread_handler = make_input_thread(sender.clone(), rx.clone());

    /* Create the application State. This is the 'System' part of an ECS architecture */
    let mut state = State::new(sender.clone(), tx);

    /* Register some reasonably useful interfaces */
    let menu = Entity {
        component: Box::new(AccountMenu::new(&state.context.accounts)),
    };
    let listing = MailListing::new();
    let b = Entity {
        component: Box::new(listing),
    };
    let mut tabs = Box::new(Tabbed::new(vec![Box::new(VSplit::new(menu, b, 90, true))]));
    tabs.add_component(Box::new(Composer {}));
    let window = Entity { component: tabs };

    let status_bar = Entity {
        component: Box::new(StatusBar::new(window)),
    };
    state.register_entity(status_bar);

    let xdg_notifications = Entity {
        component: Box::new(ui::components::notifications::XDGNotifications {}),
    };
    state.register_entity(xdg_notifications);

    /* Keep track of the input mode. See ui::UIMode for details */
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any entities have sent reply events to State. */
            let events: Vec<UIEvent> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }

            /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
            chan_select! {
                receiver.recv() -> r => {
                    match r.unwrap() {
                        ThreadEvent::Input(Key::Ctrl('z')) => {
                            state.switch_to_main_screen();
                            //_thread_handler.join().expect("Couldn't join on the associated thread");
                            let self_pid = nix::unistd::Pid::this();
                            nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGSTOP).unwrap();
                            state.switch_to_alternate_screen();
                            _thread_handler = make_input_thread(sender.clone(), rx.clone());
                            // BUG: thread sends input event after one received key
                            state.update_size();
                            state.render();
                            state.redraw();
                        },
                        ThreadEvent::Input(k) => {
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
                                        k => {
                                            state.rcv_event(UIEvent { id: 0, event_type: UIEventType::ExInput(k)});
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Fork => {
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
                        },
                        ThreadEvent::RefreshMailbox { hash : h } => {
                            state.hash_to_folder(h);
                            //state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Notification(n.clone())});
                            state.redraw();
                        },
                        ThreadEvent::UIEvent(UIEventType::ChangeMode(f)) => {
                            state.mode = f;
                            break 'inner; // `goto` 'reap loop, and wait on child.
                        }
                        ThreadEvent::UIEvent(UIEventType::StartupCheck) => {
                            let mut flag = false;
                            for account in &mut state.context.accounts {
                                let len = account.len();
                                for i in 0..len {
                                    match account.status(i) {
                                        Ok(()) => { },
                                        Err(_) => {
                                            flag |= true;
                                        }
                                    }
                                }
                            }
                            if !flag {
                                state.finish_startup();
                            }
                            state.render();
                        }
                        ThreadEvent::UIEvent(e) => {
                            state.rcv_event(UIEvent { id: 0, event_type: e});
                            state.render();
                        },
                        ThreadEvent::ThreadJoin(id) => {
                            state.join(id);
                        },
                    }
                },
                signal.recv() -> signal => {
                    if state.mode != UIMode::Fork  {
                        if let Some(Signal::WINCH) = signal {
                            state.update_size();
                            state.render();
                            state.redraw();
                        }
                    }
                },
            }
        } // end of 'inner

        'reap: loop {
            match state.try_wait_on_child() {
                Some(true) => {
                    make_input_thread(sender.clone(), rx.clone());
                    state.mode = UIMode::Normal;
                    state.render();
                }
                Some(false) => {
                    use std::{thread, time};
                    let ten_millis = time::Duration::from_millis(1500);
                    thread::sleep(ten_millis);

                    continue 'reap;
                }
                None => {
                    break 'reap;
                }
            }
        }
    }
}
