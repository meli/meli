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
use std::path::{Path, PathBuf};

#[global_allocator]
static GLOBAL: System = System;

// Re export to put crates in the documentation's start page.
pub use melib;
pub use ui;

use melib::*;
use ui::*;

use nix;
use std::os::raw::c_int;

use xdg;

fn notify(
    signals: &[c_int],
    sender: crossbeam::channel::Sender<ThreadEvent>,
) -> std::result::Result<crossbeam::channel::Receiver<c_int>, std::io::Error> {
    let alarm_sender = sender.clone();
    let alarm_handler = move |info: &nix::libc::siginfo_t| {
        let value = unsafe { info.si_value().sival_ptr as u8 };
        alarm_sender
            .send(ThreadEvent::UIEvent(UIEvent::Timer(value)))
            .unwrap();
    };
    unsafe {
        signal_hook_registry::register_sigaction(signal_hook::SIGALRM, alarm_handler)?;
    }
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
            return Err(MeliError::new(format!($($err),*)));
    }}
}

#[derive(Debug)]
struct CommandLineArguments {
    create_config: Option<String>,
    test_config: Option<String>,
    config: Option<String>,
    help: bool,
    version: bool,
}

fn main() {
    ::std::process::exit(match run_app() {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("{}", err);
            1
        }
    });
}

fn run_app() -> Result<()> {
    enum CommandLineFlags {
        CreateConfig,
        TestConfig,
        Config,
    }
    use CommandLineFlags::*;
    let mut prev: Option<CommandLineFlags> = None;
    let mut args = CommandLineArguments {
        create_config: None,
        test_config: None,
        config: None,
        help: false,
        version: false,
    };

    for i in std::env::args().skip(1) {
        match i.as_str() {
            "--test-config" => match prev {
                None => prev = Some(TestConfig),
                Some(CreateConfig) => error_and_exit!("invalid value for flag `--create-config`"),
                Some(Config) => error_and_exit!("invalid value for flag `--config`"),
                Some(TestConfig) => error_and_exit!("invalid value for flag `--test-config`"),
            },
            "--create-config" => match prev {
                None => prev = Some(CreateConfig),
                Some(CreateConfig) => error_and_exit!("invalid value for flag `--create-config`"),
                Some(TestConfig) => error_and_exit!("invalid value for flag `--test-config`"),
                Some(Config) => error_and_exit!("invalid value for flag `--config`"),
            },
            "--config" | "-c" => match prev {
                None => prev = Some(Config),
                Some(CreateConfig) if args.create_config.is_none() => {
                    args.config = Some(String::new());
                    prev = Some(Config);
                }
                Some(CreateConfig) => error_and_exit!("invalid value for flag `--create-config`"),
                Some(Config) => error_and_exit!("invalid value for flag `--config`"),
                Some(TestConfig) => error_and_exit!("invalid value for flag `--test-config`"),
            },
            "--help" | "-h" => {
                args.help = true;
            }
            "--version" | "-v" => {
                args.version = true;
            }
            "--print-loaded-themes" => {
                let s = ui::conf::FileSettings::new()?;
                print!("{}", s.terminal.themes.to_string());
                return Ok(());
            }
            "--print-default-theme" => {
                print!(
                    "{}",
                    ui::conf::Theme::default().key_to_string("dark", false)
                );
                return Ok(());
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
                Some(TestConfig) if args.test_config.is_none() => {
                    args.test_config = Some(i);
                    prev = None;
                }
                Some(TestConfig) => error_and_exit!("Duplicate value for flag `--test-config`"),
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
        println!("\t--create-config[ PATH]\tcreate a sample configuration file with available configuration options. If PATH is not specified, meli will try to create it in $XDG_CONFIG_HOME/meli/config.toml");
        println!(
            "\t--test-config PATH\ttest a configuration file for syntax issues or missing options."
        );
        println!("\t--config PATH, -c PATH\tuse specified configuration file");
        println!("\t--print-loaded-themes\tprint loaded themes in full to stdout and exit.");
        println!("\t--print-default-theme\tprint default theme in full to stdout and exit.");
        return Ok(());
    }

    if args.version {
        println!("meli {}", option_env!("CARGO_PKG_VERSION").unwrap_or("0.0"));
        return Ok(());
    }

    match prev {
        None => {}
        Some(CreateConfig) if args.create_config.is_none() => args.create_config = Some("".into()),
        Some(CreateConfig) => error_and_exit!("Duplicate value for flag `--create-config`"),
        Some(Config) => error_and_exit!("error: flag without value: `--config`"),
        Some(TestConfig) => error_and_exit!("error: flag without value: `--test-config`"),
    };

    if let Some(config_path) = args.test_config.as_ref() {
        ui::conf::FileSettings::validate(config_path)?;
        return Ok(());
    }

    if let Some(config_path) = args.create_config.as_mut() {
        let config_path: PathBuf = if config_path.is_empty() {
            let xdg_dirs = xdg::BaseDirectories::with_prefix("meli").unwrap();
            xdg_dirs.place_config_file("config.toml").map_err(|e| {
                MeliError::new(format!(
                    "Cannot create configuration directory in {}:\n{}",
                    xdg_dirs.get_config_home().display(),
                    e
                ))
            })?
        } else {
            Path::new(config_path).to_path_buf()
        };
        if config_path.exists() {
            return Err(MeliError::new(format!("File `{}` already exists.\nMaybe you meant to specify another path with --create-config=PATH", config_path.display())));
        }
        ui::conf::create_config_file(&config_path)?;
        return Ok(());
    }

    if let Some(config_location) = args.config.as_ref() {
        std::env::set_var("MELI_CONFIG", config_location);
    }

    /* Create a channel to communicate with other threads. The main process is the sole receiver.
     * */
    let (sender, receiver) = crossbeam::channel::bounded(32 * ::std::mem::size_of::<ThreadEvent>());
    /* Catch SIGWINCH to handle terminal resizing */
    let signals = &[
        /* Catch SIGWINCH to handle terminal resizing */
        signal_hook::SIGWINCH,
        /* Catch SIGCHLD to handle embed applications status change */
        signal_hook::SIGCHLD,
    ];

    let signal_recvr = notify(signals, sender.clone())?;

    /* Create the application State. */
    let mut state = State::new(sender, receiver.clone())?;

    let window = Box::new(Tabbed::new(vec![
        Box::new(listing::Listing::new(&state.context.accounts)),
        Box::new(ContactList::new(&state.context)),
        Box::new(StatusPanel::new()),
    ]));

    let status_bar = Box::new(StatusBar::new(window));
    state.register_component(status_bar);

    let xdg_notifications = Box::new(ui::components::notifications::XDGNotifications::new());
    state.register_component(xdg_notifications);
    state.register_component(Box::new(
        ui::components::notifications::NotificationFilter {},
    ));

    /* Keep track of the input mode. See ui::UIMode for details */
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any components have sent reply events to State. */
            let events: ui::smallvec::SmallVec<[UIEvent; 8]> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();

            /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
            crossbeam::select! {
                recv(receiver) -> r => {
                    match r {
                         Ok(ThreadEvent::Pulse) | Ok(ThreadEvent::UIEvent(UIEvent::Timer(_))) => {},
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
                        ThreadEvent::InputRaw(raw_input @ (Key::Ctrl('l'), _)) => {
                            /* Manual screen redraw */
                            state.update_size();
                            state.render();
                            state.redraw();
                            state.rcv_event(UIEvent::EmbedInput(raw_input));
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
                            state.redraw();
                        },
                        ThreadEvent::Pulse => {
                            state.check_accounts();
                            state.redraw();
                        },
                        ThreadEvent::NewThread(id, name) => {
                            state.new_thread(id, name);
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
                        other => {
                            debug!("got other signal: {:?}", other);
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
