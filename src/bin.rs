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
//!  `src/bin.rs` creates an event loop and passes input to a thread.
//!
//! The mail handling stuff is done in the `melib` crate which includes all backend needs. The
//! split is done to theoretically be able to create different frontends with the same innards.
//!

use std::alloc::System;
use std::collections::VecDeque;
use std::path::{Path, PathBuf};
extern crate notify_rust;
extern crate xdg_utils;
#[macro_use]
extern crate serde_derive;
extern crate linkify;
extern crate uuid;

extern crate termion;

#[macro_use]
extern crate nom;

extern crate bitflags;
extern crate serde_json;
extern crate smallvec;

#[global_allocator]
static GLOBAL: System = System;

#[macro_use]
extern crate melib;
use melib::*;

mod unix;
use unix::*;

#[macro_use]
pub mod types;
use crate::types::*;

#[macro_use]
pub mod terminal;
use crate::terminal::*;

#[macro_use]
pub mod execute;
use crate::execute::*;

pub mod state;
use crate::state::*;

pub mod components;
use crate::components::*;

#[macro_use]
pub mod conf;
use crate::conf::*;

pub mod workers;
use crate::workers::*;

#[cfg(feature = "sqlite3")]
pub mod sqlite3;

pub mod mailcap;
pub mod plugins;

use nix;
use std::os::raw::c_int;

use xdg;

fn notify(
    signals: &[c_int],
    sender: crossbeam::channel::Sender<ThreadEvent>,
) -> std::result::Result<crossbeam::channel::Receiver<c_int>, std::io::Error> {
    use std::time::Duration;
    let (alarm_pipe_r, alarm_pipe_w) = nix::unistd::pipe().map_err(|err| {
        std::io::Error::from_raw_os_error(err.as_errno().map(|n| n as i32).unwrap_or(0))
    })?;
    let alarm_handler = move |info: &nix::libc::siginfo_t| {
        let value = unsafe { info.si_value().sival_ptr as u8 };
        let _ = nix::unistd::write(alarm_pipe_w, &[value]);
    };
    unsafe {
        signal_hook_registry::register_sigaction(signal_hook::SIGALRM, alarm_handler)?;
    }
    let (s, r) = crossbeam::channel::bounded(100);
    let signals = signal_hook::iterator::Signals::new(signals)?;
    let _ = nix::fcntl::fcntl(
        alarm_pipe_r,
        nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::O_NONBLOCK),
    );
    std::thread::spawn(move || {
        let mut buf = [0; 1];
        let mut ctr = 0;
        loop {
            ctr %= 3;
            if ctr == 0 {
                let _ = sender
                    .send_timeout(ThreadEvent::Pulse, Duration::from_millis(500))
                    .ok();
            }

            for signal in signals.pending() {
                let _ = s.send_timeout(signal, Duration::from_millis(500)).ok();
            }
            while nix::unistd::read(alarm_pipe_r, buf.as_mut())
                .map(|s| s > 0)
                .unwrap_or(false)
            {
                let value = buf[0];
                sender
                    .send_timeout(
                        ThreadEvent::UIEvent(UIEvent::Timer(value)),
                        Duration::from_millis(500),
                    )
                    .unwrap();
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

enum ManPages {
    Main = 0,
    Conf = 1,
    Themes = 2,
}

struct CommandLineArguments {
    print_manpage: Option<ManPages>,
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
        PrintManPage,
        CreateConfig,
        TestConfig,
        Config,
    }
    use CommandLineFlags::*;
    let mut prev: Option<CommandLineFlags> = None;
    let mut args = CommandLineArguments {
        print_manpage: None,
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
                Some(PrintManPage) => {
                    error_and_exit!("invalid value for flag `--print-documentation`")
                }
            },
            "--create-config" => match prev {
                None => prev = Some(CreateConfig),
                Some(CreateConfig) => error_and_exit!("invalid value for flag `--create-config`"),
                Some(TestConfig) => error_and_exit!("invalid value for flag `--test-config`"),
                Some(Config) => error_and_exit!("invalid value for flag `--config`"),
                Some(PrintManPage) => {
                    error_and_exit!("invalid value for flag `--print-documentation`")
                }
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
                Some(PrintManPage) => {
                    error_and_exit!("invalid value for flag `--print-documentation`")
                }
            },
            "--help" | "-h" => {
                args.help = true;
            }
            "--version" | "-v" => {
                args.version = true;
            }
            "--print-loaded-themes" => {
                let s = conf::FileSettings::new()?;
                print!("{}", s.terminal.themes.to_string());
                return Ok(());
            }
            "--print-default-theme" => {
                print!("{}", conf::Theme::default().key_to_string("dark", false));
                return Ok(());
            }
            "--print-documentation" => {
                if args.print_manpage.is_some() {
                    error_and_exit!("Multiple invocations of --print-documentation");
                }
                prev = Some(PrintManPage);
                args.print_manpage = Some(ManPages::Main);
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
                Some(PrintManPage) => {
                    match e {
                        "meli" | "main" => { /* This is the default */ }
                        "meli.conf" | "conf" | "config" | "configuration" => {
                            args.print_manpage = Some(ManPages::Conf);
                        }
                        "meli-themes" | "themes" | "theming" | "theme" => {
                            args.print_manpage = Some(ManPages::Themes);
                        }
                        _ => error_and_exit!("Invalid documentation page: {}", e),
                    }
                }
                Some(TestConfig) => error_and_exit!("Duplicate value for flag `--test-config`"),
                Some(CreateConfig) => error_and_exit!("Duplicate value for flag `--create-config`"),
                Some(Config) => error_and_exit!("Duplicate value for flag `--config`"),
            },
        }
    }

    if args.help {
        println!("usage:\tmeli [--config PATH|-c PATH]");
        println!("\tmeli --help");
        println!("\tmeli --version");
        println!("");
        println!("Other options:");
        println!("\t--help, -h\t\tshow this message and exit");
        println!("\t--version, -v\t\tprint version and exit");
        println!("\t--create-config[ PATH]\tcreate a sample configuration file with available configuration options. If PATH is not specified, meli will try to create it in $XDG_CONFIG_HOME/meli/config.toml");
        println!(
            "\t--test-config PATH\ttest a configuration file for syntax issues or missing options."
        );
        println!("\t--config PATH, -c PATH\tuse specified configuration file");
        println!("\t--print-loaded-themes\tprint loaded themes in full to stdout and exit.");
        println!("\t--print-default-theme\tprint default theme in full to stdout and exit.");
        #[cfg(feature = "cli-docs")]
        {
            println!("\t--print-documentation [meli conf themes]\n\t\t\t\tprint documentation page and exit (Piping to a pager is recommended.).");
        }
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
        Some(PrintManPage) => {}
    };

    if (args.print_manpage.is_some()
        ^ args.test_config.is_some()
        ^ args.create_config.is_some()
        ^ args.config.is_some())
        && !(args.print_manpage.is_some()
            || args.test_config.is_some()
            || args.create_config.is_some()
            || args.config.is_some())
    {
        error_and_exit!("error: illegal command-line flag combination");
    }

    if let Some(config_path) = args.test_config.as_ref() {
        conf::FileSettings::validate(config_path)?;
        return Ok(());
    } else if let Some(config_path) = args.create_config.as_mut() {
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
        conf::create_config_file(&config_path)?;
        return Ok(());
    } else if let Some(_page) = args.print_manpage {
        #[cfg(feature = "cli-docs")]
        {
            const MANPAGES: [&'static str; 3] = [
                include_str!(concat!(env!("OUT_DIR"), "/meli.txt")),
                include_str!(concat!(env!("OUT_DIR"), "/meli.conf.txt")),
                include_str!(concat!(env!("OUT_DIR"), "/meli-themes.txt")),
            ];
            println!("{}", MANPAGES[_page as usize]);
            return Ok(());
        }
        #[cfg(not(feature = "cli-docs"))]
        {
            error_and_exit!("error: this version of meli was not build with embedded documentation. You might have it installed as manpages (eg `man meli`), otherwise check https://meli.delivery");
        }
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

    let window = Box::new(Tabbed::new(
        vec![
            Box::new(listing::Listing::new(&mut state.context)),
            Box::new(ContactList::new(&state.context)),
            Box::new(StatusPanel::new(crate::conf::value(
                &state.context,
                "theme_default",
            ))),
        ],
        &state.context,
    ));

    let status_bar = Box::new(StatusBar::new(window));
    state.register_component(status_bar);

    let xdg_notifications = Box::new(components::notifications::XDGNotifications::new());
    state.register_component(xdg_notifications);
    state.register_component(Box::new(components::notifications::NotificationFilter {}));

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

            /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
            crossbeam::select! {
                recv(receiver) -> r => {
                    match r {
                         Ok(ThreadEvent::Pulse) | Ok(ThreadEvent::UIEvent(UIEvent::Timer(_))) => {},
                        _ => {debug!(&r);}
                    }
                    match r.unwrap() {
                        ThreadEvent::Input((Key::Ctrl('z'), _)) if state.mode != UIMode::Embed => {
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
                            if state.mode == UIMode::Embed {
                            state.rcv_event(UIEvent::EmbedInput(raw_input));
                            state.redraw();
                            }
                        },
                        ThreadEvent::Input((k, r)) => {
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
                                UIMode::Embed => {
                                    state.rcv_event(UIEvent::EmbedInput((k,r)));
                                    state.redraw();
                                },
                                UIMode::Fork => {
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
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
                        signal_hook::SIGCHLD => {
                            state.rcv_event(UIEvent::EmbedInput((Key::Null, vec![0])));
                            state.redraw();

                        }
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
