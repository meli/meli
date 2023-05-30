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
//! - Hold an [`Envelope`](./email/struct.Envelope.html) with methods convenient
//!   for mail client use. (see module [`email`](./email/index.html))
//! - Abstract through mail storages through the
//!   [`MailBackend`](./backends/trait.MailBackend.html) trait, and handle
//!   read/writes/updates through it. (see module
//!   [`backends`](./backends/index.html))
//! - Decode attachments (see module
//!   [`email::attachments`](./email/attachments/index.html))
//! - Create new mail (see [`email::Draft`](./email/compose/struct.Draft.html))
//! - Send mail with an SMTP client (see module [`smtp`](./smtp/index.html))
//! - Manage an `addressbook` i.e. have contacts (see module
//!   [`addressbook`](./addressbook/index.html))
//! - Build thread structures out of a list of mail via their `In-Reply-To` and
//!   `References` header values (see module [`thread`](./thread/index.html))
//!
//! Other exports are
//! - Basic mail account configuration to use with
//!   [`backends`](./backends/index.html) (see module
//!   [`conf`](./conf/index.html))
//! - A `debug` macro that works like `std::dbg` but for multiple threads. (see
//!   [`debug` macro](./macro.debug.html))

#[macro_use]
pub mod dbg {

    #[allow(clippy::redundant_closure)]
    #[macro_export]
    macro_rules! debug {
        ($val:literal) => {
            {
                if cfg!(feature="debug-tracing") {
                    $crate::log::debug!($val);
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
                        $crate::log::debug!("{} = {:?}", stringify, tmp);
                        tmp
                    }
                }
            } else {
                $val
            }
        };
        ($fmt:literal, $($arg:tt)*) => {
            if cfg!(feature="debug-tracing") {
                $crate::log::debug!($fmt, $($arg)*);
            }
        };
    }
}

#[cfg(feature = "unicode_algorithms")]
pub mod text_processing;

pub use utils::{datetime::UnixTimestamp, *};

pub use self::logging::{LogLevel, StderrLogger};

pub mod addressbook;
pub use addressbook::*;
pub mod backends;
pub use backends::*;
mod collection;
pub mod sieve;
pub use collection::*;
pub mod conf;
pub use conf::*;
pub mod email;
pub use email::*;
pub mod error;
pub use crate::error::*;
pub mod thread;
pub use thread::*;
pub mod search;

#[macro_use]
mod utils;

#[cfg(feature = "gpgme")]
pub mod gpgme;
#[cfg(feature = "smtp")]
pub mod smtp;

#[macro_use]
extern crate serde_derive;
pub extern crate log;
/* parser */
extern crate data_encoding;
extern crate encoding;
pub extern crate nom;

#[macro_use]
extern crate bitflags;
pub extern crate futures;
pub extern crate indexmap;
pub extern crate smallvec;
pub extern crate smol;
pub extern crate uuid;
pub extern crate xdg_utils;

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct Bytes(pub usize);

impl Bytes {
    pub const KILOBYTE: f64 = 1024.0;
    pub const MEGABYTE: f64 = Self::KILOBYTE * 1024.0;
    pub const GIGABYTE: f64 = Self::MEGABYTE * 1024.0;
    pub const PETABYTE: f64 = Self::GIGABYTE * 1024.0;
}

impl core::fmt::Display for Bytes {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        let bytes: f64 = self.0 as f64;
        if bytes == 0.0 {
            write!(fmt, "0")
        } else if bytes < Self::KILOBYTE {
            write!(fmt, "{:.2} bytes", bytes)
        } else if bytes < Self::MEGABYTE {
            write!(fmt, "{:.2} KiB", bytes / Self::KILOBYTE)
        } else if bytes < Self::GIGABYTE {
            write!(fmt, "{:.2} MiB", bytes / Self::MEGABYTE)
        } else if bytes < Self::PETABYTE {
            write!(fmt, "{:.2} GiB", bytes / Self::GIGABYTE)
        } else {
            write!(fmt, "{:.2} PiB", bytes / Self::PETABYTE)
        }
    }
}

pub use shellexpand::ShellExpandTrait;
