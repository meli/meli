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

//! Command line client binary.
//!
//! This crate contains the frontend stuff of the application. The application
//! entry way on `src/bin.rs` creates an event loop and passes input to a
//! thread.
//!
//! The mail handling stuff is done in the `melib` crate which includes all
//! backend needs. The split is done to theoretically be able to create
//! different frontends with the same innards.

use args::*;
use meli::*;

fn main() {
    let opt = Opt::from_args();
    ::std::process::exit(match run_app(opt) {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("{}", err);
            1
        }
    });
}

fn run_app(mut opt: Opt) -> Result<()> {
    if let Some(config_location) = opt.config.as_ref() {
        std::env::set_var("MELI_CONFIG", config_location);
    }

    let view_subcmd = if matches!(opt.subcommand, Some(SubCommand::View { .. })) {
        opt.subcommand.take()
    } else {
        None
    };
    if let Some(result) = opt.execute() {
        return result;
    }

    /* Create a channel to communicate with other threads. The main process is
     * the sole receiver.
     */
    let (sender, receiver) = crossbeam::channel::bounded(32 * ::std::mem::size_of::<ThreadEvent>());
    /* Catch SIGWINCH to handle terminal resizing */
    let signals = &[
        /* Catch SIGWINCH to handle terminal resizing */
        signal_hook::consts::SIGWINCH,
        /* Catch SIGCHLD to handle embedded applications status change */
        signal_hook::consts::SIGCHLD,
    ];

    let signal_recvr = signal_handlers::notify(signals, sender.clone())?;

    /* Create the application State. */
    let mut state;

    if let Some(SubCommand::View { path }) = view_subcmd {
        state = subcommands::view(path, sender, receiver.clone())?;
    } else {
        state = State::new(None, sender, receiver.clone())?;
        // #[cfg(feature = "svgscreenshot")]
        // state.register_component(Box::new(svg::SVGScreenshotFilter::new()));
        let window = Box::new(Tabbed::new(
            vec![
                Box::new(listing::Listing::new(&mut state.context)),
                Box::new(contacts::list::ContactList::new(&state.context)),
            ],
            &state.context,
        ));

        let status_bar = Box::new(StatusBar::new(&state.context, window));
        state.register_component(status_bar);

        #[cfg(all(target_os = "linux", feature = "dbus-notifications"))]
        {
            let dbus_notifications =
                Box::new(notifications::DbusNotifications::new(&state.context));
            state.register_component(dbus_notifications);
        }
        state.register_component(Box::new(notifications::NotificationCommand::new()));
    }
    let enter_command_mode: Key = state
        .context
        .settings
        .shortcuts
        .general
        .enter_command_mode
        .clone();
    let quit_key: Key = state.context.settings.shortcuts.general.quit.clone();

    /* Keep track of the input mode. See UIMode for details */
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any components have sent reply events to State. */
            let events: smallvec::SmallVec<[UIEvent; 8]> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();

            /* Poll on all channels. Currently we have the input channel for stdin,
             * watching events and the signal watcher. */
            crossbeam::select! {
                recv(receiver) -> r => {
                    match r {
                         Ok(ThreadEvent::Pulse) | Ok(ThreadEvent::UIEvent(UIEvent::Timer(_))) => {},
                        _ => {
                            log::trace!("{:?}", &r);
                        }
                    }
                    match r.unwrap() {
                        ThreadEvent::Input((Key::Ctrl('z'), _)) if state.mode != UIMode::Embedded => {
                            state.switch_to_main_screen();
                            //_thread_handler.join().expect("Couldn't join on the associated thread");
                            let self_pid = nix::unistd::Pid::this();
                            nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGSTOP).unwrap();
                            state.switch_to_alternate_screen();
                            // BUG: thread sends input event after one received key
                            state.update_size();
                            state.render();
                            state.redraw();
                        },
                        ThreadEvent::Input(raw_input @ (Key::Ctrl('l'), _)) => {
                            /* Manual screen redraw */
                            state.update_size();
                            state.render();
                            state.redraw();
                            if state.mode == UIMode::Embedded {
                                state.rcv_event(UIEvent::EmbeddedInput(raw_input));
                                state.redraw();
                            }
                        },
                        ThreadEvent::Input((k, r)) => {
                            match state.mode {
                                UIMode::Normal => {
                                    match k {
                                        _ if k == quit_key => {
                                            if state.can_quit_cleanly() {
                                                drop(state);
                                                break 'main;
                                            } else {
                                                state.redraw();
                                            }
                                        },
                                        _ if k == enter_command_mode => {
                                            state.mode = UIMode::Command;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Command));
                                            state.redraw();
                                        }
                                        key  => {
                                            state.rcv_event(UIEvent::Input(key));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Insert => {
                                    match k {
                                        Key::Esc => {
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::InsertInput(k));
                                            state.redraw();
                                        },
                                    }
                                }
                                UIMode::Command => {
                                    match k {
                                        Key::Char('\n') => {
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::CmdInput(k));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Embedded => {
                                    state.rcv_event(UIEvent::EmbeddedInput((k,r)));
                                    state.redraw();
                                },
                                UIMode::Fork => {
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
                        },
                        ThreadEvent::MailboxChanges { account_hash, mailbox_hash, events} => {
                            state.refresh_event(account_hash, mailbox_hash, events);
                            state.redraw();
                        },
                        ThreadEvent::UIEvent(UIEvent::ChangeMode(f)) => {
                            state.mode = f;
                            if f == UIMode::Fork {
                                break 'inner; // `goto` 'reap loop, and wait on child.
                            }
                        }
                        ThreadEvent::UIEvent(e) => {
                            state.rcv_event(e);
                            state.redraw();
                        },
                        ThreadEvent::Pulse => {
                            state.pulse();
                        },
                        ThreadEvent::JobFinished(id) => {
                            log::trace!("Job finished {}", id);
                            state.context.main_loop_handler.job_executor.set_job_finished(id);
                            for account in state.context.accounts.values_mut() {
                                if account.process_event(&id) {
                                    break;
                                }
                            }
                        },
                    }
                },
                recv(signal_recvr) -> sig => {
                    match sig.unwrap() {
                        signal_hook::consts::SIGWINCH => {
                            if state.mode != UIMode::Fork  {
                                state.update_size();
                                state.render();
                                state.redraw();
                            }
                        },
                        signal_hook::consts::SIGCHLD => {
                            state.rcv_event(UIEvent::EmbeddedInput((Key::Null, vec![0])));
                            state.redraw();

                        }
                        other => {
                            log::trace!("got other signal: {:?}", other);
                        }
                    }
                },
            }
        } // end of 'inner

        'reap: loop {
            match state.try_wait_on_child() {
                Some(true) => {
                    state.restore_input();
                    state.switch_to_alternate_screen();
                }
                Some(false) => {
                    use std::{thread, time};
                    let ten_millis = time::Duration::from_millis(1500);
                    thread::sleep(ten_millis);

                    continue 'reap;
                }
                None => {
                    state.mode = UIMode::Normal;
                    state.render();
                    break 'reap;
                }
            }
        }
    }
    Ok(())
}
