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
#[macro_use]
pub mod dbg {
    #[allow(clippy::redundant_closure)]
    #[macro_export]
    macro_rules! debug {
        ($val:literal) => {
            {
            if cfg!(debug_assertions) {
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(|v| v.to_string())
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
            {
            if cfg!(debug_assertions) {
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                    file!(),
                    line!(),
                    column!()
                );
                eprintln!("{} = {:?}", stringify!($val), $val);
            }
            $val
            }
        };
        ($fmt:literal, $($arg:tt)*) => {
            if cfg!(debug_assertions) {
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(|v| v.to_string())
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

pub mod addressbook;
pub mod async_workers;
pub mod backends;
mod collection;
pub mod conf;
pub mod email;
pub mod error;
pub mod grapheme_clusters;
pub mod mailbox;
pub mod thread;
pub use crate::email::*;
pub use thread::*;
mod wcwidth;
pub use self::grapheme_clusters::*;
pub use self::wcwidth::*;
mod structs;
pub use self::structs::*;

#[macro_use]
extern crate serde_derive;
/* parser */
#[macro_use]
extern crate nom;
extern crate chrono;
extern crate data_encoding;
extern crate encoding;
extern crate memmap;
#[macro_use]
extern crate chan;

#[macro_use]
extern crate bitflags;
extern crate fnv;
extern crate uuid;

pub use conf::*;
pub use mailbox::*;

pub use backends::{Backends, RefreshEvent, RefreshEventConsumer};
pub use email::{Envelope, Flag};
pub use error::{MeliError, Result};

pub use addressbook::*;
