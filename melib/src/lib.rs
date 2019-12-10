/*
 * meli - lib.rs
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

//! A crate that performs mail client operations such as
//! - Hold an `Envelope` with methods convenient for mail client use. (see module `email`)
//! - Abstract through mail storages through the `MailBackend` trait, and handle
//!   read/writes/updates through it. (see module `melib::backends`)
//! - Decode attachments (see module `melib::email::attachments`)
//! - Create new mail (see `email::Draft`)
//! - Manage an `addressbook` i.e. have contacts (see module `addressbook`)
//! - Build thread structures out of a list of mail via their `In-Reply-To` and `References` header
//!   values (see module `thread`)
//!
//! Other exports are
//! - Thread management (see module `async_workers`)
//! - Basic mail account configuration to use with `backends` (see module `conf`)
//! - Parser combinators (see module `parsec`)
//! - A `ShellExpandTrait` to expand paths like a shell.
//! - A `debug` macro that works like `std::dbg` but for multiple threads. (see `dbg` module)
#[macro_use]
pub mod dbg {
    #[allow(clippy::redundant_closure)]
    #[macro_export]
    macro_rules! debug {
        ($val:literal) => {
            {
            if cfg!(feature="debug-tracing") {
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(std::string::ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                    file!(),
                    line!(),
                    column!()
                );
                eprintln!($val);
            }
            $val
        }
        };
        ($val:expr) => {
            if cfg!(feature="debug-tracing") {
                let stringify = stringify!($val);
                // Use of `match` here is intentional because it affects the lifetimes
                // of temporaries - https://stackoverflow.com/a/48732525/1063961
                match $val {
                    tmp => {
                        eprint!(
                            "[{:?}] {}:{}_{}:	",
                            std::thread::current()
                            .name()
                            .map(std::string::ToString::to_string)
                            .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                            file!(),
                            line!(),
                            column!()
                        );
                        eprintln!("{} = {:?}", stringify, tmp);
                        tmp
                    }
                }
            } else {
                $val
            }
        };
        ($fmt:literal, $($arg:tt)*) => {
            if cfg!(feature="debug-tracing") {
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(std::string::ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                    file!(),
                    line!(),
                    column!()
                );
                eprintln!($fmt, $($arg)*);
            }
        };
    }
}

#[cfg(feature = "unicode_algorithms")]
extern crate text_processing;

#[macro_use]
mod logging;
pub use self::logging::LoggingLevel::*;
pub use self::logging::*;

pub mod addressbook;
pub mod async_workers;
pub mod backends;
mod collection;
pub mod conf;
pub mod email;
pub mod error;
pub mod mailbox;
pub mod thread;
pub use crate::email::*;
pub use crate::thread::*;
mod structs;
pub use self::structs::*;
pub mod parsec;

#[macro_use]
extern crate serde_derive;
/* parser */
#[macro_use]
extern crate nom;
extern crate chrono;
extern crate data_encoding;
extern crate encoding;

#[macro_use]
extern crate bitflags;
extern crate fnv;
extern crate uuid;

pub use crate::conf::*;
pub use crate::mailbox::*;

pub use crate::backends::{Backends, RefreshEvent, RefreshEventConsumer, SpecialUsageMailbox};
pub use crate::email::{Envelope, Flag};
pub use crate::error::{MeliError, Result};

pub use crate::addressbook::*;

pub use shellexpand::ShellExpandTrait;
pub mod shellexpand {

    use std::path::*;

    pub trait ShellExpandTrait {
        fn expand(&self) -> PathBuf;
    }

    impl ShellExpandTrait for Path {
        fn expand(&self) -> PathBuf {
            let mut ret = PathBuf::new();
            for c in self.components() {
                let c_to_str = c.as_os_str().to_str();
                match c_to_str {
                    Some("~") => {
                        if let Some(home_dir) = std::env::var("HOME").ok() {
                            ret.push(home_dir)
                        } else {
                            return PathBuf::new();
                        }
                    }
                    Some(var) if var.starts_with("$") => {
                        let env_name = var.split_at(1).1;
                        if env_name.chars().all(char::is_uppercase) {
                            ret.push(std::env::var(env_name).unwrap_or(String::new()));
                        } else {
                            ret.push(c);
                        }
                    }
                    Some(_) => {
                        ret.push(c);
                    }
                    None => {
                        /* path is invalid */
                        return PathBuf::new();
                    }
                }
            }
            ret
        }
    }
}
