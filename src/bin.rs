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

//!
//!  This crate contains the frontend stuff of the application. The application entry way on
//!  `src/bin.rs` creates an event loop and passes input to the `ui` module.
//!
//! The mail handling stuff is done in the `melib` crate which includes all backend needs. The
//! split is done to theoretically be able to create different frontends with the same innards.
//!

use std::alloc::System;
use std::io::Write;
use std::path::{Path, PathBuf};

#[global_allocator]
static GLOBAL: System = System;

use ui;

pub use melib::*;
pub use ui::*;

use nix;
use std::os::raw::c_int;
use xdg;

fn notify(
    signals: &[c_int],
    sender: crossbeam::channel::Sender<ThreadEvent>,
) -> std::result::Result<crossbeam::channel::Receiver<c_int>, std::io::Error> {
    let (s, r) = crossbeam::channel::bounded(100);
    let signals = signal_hook::iterator::Signals::new(signals)?;
    std::thread::spawn(move || {
        let mut ctr = 0;
        loop {
            ctr %= 3;
            if ctr == 0 {
                sender.send(ThreadEvent::Pulse).unwrap();
            }

            for signal in signals.pending() {
                s.send(signal).unwrap();
            }

            std::thread::sleep(std::time::Duration::from_millis(100));
            ctr += 1;
        }
    });
    Ok(r)
}

macro_rules! error_and_exit {
    ($($err:expr),*) => {{
            eprintln!($($err),*);
            std::process::exit(1);
    }}
}

#[derive(Debug)]
struct CommandLineArguments {
    create_config: Option<String>,
    config: Option<String>,
    help: bool,
    version: bool,
}

fn main() -> std::result::Result<(), std::io::Error> {
    enum CommandLineFlags {
        CreateConfig,
        Config,
    }
    use CommandLineFlags::*;
    let mut prev: Option<CommandLineFlags> = None;
    let mut args = CommandLineArguments {
        create_config: None,
        config: None,
        help: false,
        version: false,
    };

    for i in std::env::args().skip(1) {
        match i.as_str() {
            "--create-config" => match prev {
                None => prev = Some(CreateConfig),
                Some(CreateConfig) => error_and_exit!("invalid value for flag `--create-config`"),
                Some(Config) => error_and_exit!("invalid value for flag `--config`"),
            },
            "--config" | "-c" => match prev {
                None => prev = Some(Config),
                Some(CreateConfig) if args.create_config.is_none() => {
                    args.config = Some(String::new());
                    prev = Some(Config);
                }
                Some(CreateConfig) => error_and_exit!("Duplicate value for flag `--create-config`"),
                Some(Config) => error_and_exit!("invalid value for flag `--config`"),
            },
            "--help" | "-h" => {
                args.help = true;
            }
            "--version" | "-v" => {
                args.version = true;
            }
            e => match prev {
                None => error_and_exit!("error: value without command {}", e),
                Some(CreateConfig) if args.create_config.is_none() => {
                    args.create_config = Some(i);
                    prev = None;
                }
                Some(Config) if args.config.is_none() => {
                    args.config = Some(i);
                    prev = None;
                }
                Some(CreateConfig) => error_and_exit!("Duplicate value for flag `--create-config`"),
                Some(Config) => error_and_exit!("Duplicate value for flag `--config`"),
            },
        }
    }

    if args.help {
        println!("usage:\tmeli [--create-config[ PATH]] [--config[ PATH]|-c[ PATH]]");
        println!("\tmeli --help");
        println!("\tmeli --version");
        println!("");
        println!("\t--help, -h\t\tshow this message and exit");
        println!("\t--version, -v\t\tprint version and exit");
        println!("\t--create-config[ PATH]\tCreate a sample configuration file with available configuration options. If PATH is not specified, meli will try to create it in $XDG_CONFIG_HOME/meli/config");
        println!("\t--config PATH, -c PATH\tUse specified configuration file");
        std::process::exit(0);
    }

    if args.version {
        println!("meli {}", option_env!("CARGO_PKG_VERSION").unwrap_or("0.0"));
        std::process::exit(0);
    }

    match prev {
        None => {}
        Some(CreateConfig) if args.create_config.is_none() => args.create_config = Some("".into()),
        Some(CreateConfig) => error_and_exit!("Duplicate value for flag `--create-config`"),
        Some(Config) => error_and_exit!("error: flag without value: --config"),
    };

    if let Some(config_path) = args.create_config.as_mut() {
        let config_path: PathBuf = if config_path.is_empty() {
            let xdg_dirs = xdg::BaseDirectories::with_prefix("meli").unwrap();
            xdg_dirs.place_config_file("config").unwrap_or_else(|e| {
                error_and_exit!("Cannot create configuration directory:\n{}", e)
            })
        } else {
            Path::new(config_path).to_path_buf()
        };
        if config_path.exists() {
            println!("File `{}` already exists.\nMaybe you meant to specify another path with --create-config=PATH", config_path.display());
            std::process::exit(1);
        }
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(config_path.as_path())
            .unwrap_or_else(|e| error_and_exit!("Could not create config file:\n{}", e));
        file.write_all(include_bytes!("../sample-config"))
            .unwrap_or_else(|e| error_and_exit!("Could not write to config file:\n{}", e));
        println!("Written example configuration to {}", config_path.display());
        std::process::exit(0);
    }

    if let Some(config_location) = args.config.as_ref() {
        std::env::set_var("MELI_CONFIG", config_location);
    }

    /* Create the application State. */
    let mut state = State::new();

    let receiver = state.receiver();
    let sender = state.sender();

    /* Catch SIGWINCH to handle terminal resizing */
    let signals = &[
        /* Catch SIGWINCH to handle terminal resizing */
        signal_hook::SIGWINCH,
        /* Catch SIGCHLD to handle embed applications status change */
        signal_hook::SIGCHLD,
    ];

    let signal_recvr = notify(signals, sender)?;

    let window = Box::new(Tabbed::new(vec![
        Box::new(listing::Listing::new(&state.context.accounts)),
        Box::new(ContactList::new(&state.context)),
        Box::new(StatusPanel::new()),
    ]));

    let status_bar = Box::new(StatusBar::new(window));
    state.register_component(status_bar);

    let xdg_notifications = Box::new(ui::components::notifications::XDGNotifications {});
    state.register_component(xdg_notifications);
    state.register_component(Box::new(
        ui::components::notifications::NotificationFilter {},
    ));

    /* Keep track of the input mode. See ui::UIMode for details */
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any components have sent reply events to State. */
            let events: Vec<UIEvent> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();

            /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
            crossbeam::select! {
                recv(receiver) -> r => {
                    match r {
                         Ok(ThreadEvent::Pulse) => {},
                        _ => {debug!(&r);}
                    }
                    match r.unwrap() {
                        ThreadEvent::Input(Key::Ctrl('z')) => {
                            state.switch_to_main_screen();
                            //_thread_handler.join().expect("Couldn't join on the associated thread");
                            let self_pid = nix::unistd::Pid::this();
                            nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGSTOP).unwrap();
                            state.switch_to_alternate_screen();
                            state.restore_input();
                            // BUG: thread sends input event after one received key
                            state.update_size();
                            state.render();
                            state.redraw();
                        },
                        ThreadEvent::Input(Key::Ctrl('l')) => {
                            /* Manual screen redraw */
                            state.update_size();
                            state.render();
                            state.redraw();
                        },
                        ThreadEvent::Input(k) => {
                            match state.mode {
                                UIMode::Normal => {
                                    match k {
                                        Key::Char('q') | Key::Char('Q') => {
                                            if state.can_quit_cleanly() {
                                                drop(state);
                                                break 'main;
                                            } else {
                                                state.redraw();
                                            }
                                        },
                                        Key::Char(' ') => {
                                            state.mode = UIMode::Execute;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Execute));
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
                                        Key::Char('\n') | Key::Esc => {
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::InsertInput(k));
                                            state.redraw();
                                        },
                                    }
                                }
                                UIMode::Execute => {
                                    match k {
                                        Key::Char('\n') | Key::Esc => {
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::ExInput(k));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Embed => state.redraw(),

                                UIMode::Fork => {
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
                        },
                        ThreadEvent::InputRaw(raw_input) => {
                            state.rcv_event(UIEvent::EmbedInput(raw_input));
                            state.redraw();
                        },
                        ThreadEvent::RefreshMailbox(event) => {
                            state.refresh_event(*event);
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
                            state.render();
                        },
                        ThreadEvent::Pulse => {
                            state.redraw();
                        },
                        ThreadEvent::ThreadJoin(id) => {
                            state.join(id);
                        },
                    }
                },
                recv(signal_recvr) -> sig => {
                    match sig.unwrap() {
                        signal_hook::SIGWINCH => {
                            if state.mode != UIMode::Fork  {
                                state.update_size();
                                state.render();
                                state.redraw();
                            }
                        },
                        _ => {}
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
