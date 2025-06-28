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

#![deny(
    rustdoc::redundant_explicit_links,
    unsafe_op_in_unsafe_fn,
    /* groups */
    clippy::correctness,
    clippy::suspicious,
    clippy::complexity,
    clippy::perf,
    clippy::cargo,
    clippy::nursery,
    clippy::style,
    /* restriction */
    clippy::dbg_macro,
    clippy::rc_buffer,
    clippy::as_underscore,
    clippy::assertions_on_result_states,
    /* rustdoc */
    rustdoc::broken_intra_doc_links,
    /* pedantic */
    //clippy::cast_lossless,
    //clippy::cast_possible_wrap,
    //clippy::ptr_as_ptr,
    //clippy::bool_to_int_with_if,
    clippy::doc_markdown,
    clippy::expect_fun_call,
    clippy::or_fun_call,
    clippy::borrow_as_ptr,
    clippy::case_sensitive_file_extension_comparisons,
    //clippy::cast_lossless,
    //clippy::cast_ptr_alignment,
    clippy::large_futures,
    clippy::waker_clone_wake,
    clippy::unused_enumerate_index,
    clippy::unnecessary_fallible_conversions,
    clippy::struct_field_names,
    clippy::manual_hash_one,
    clippy::into_iter_without_iter,
)]
#![allow(
    clippy::option_if_let_else,
    clippy::missing_const_for_fn,
    clippy::significant_drop_tightening,
    clippy::multiple_crate_versions,
    clippy::significant_drop_in_scrutinee,
    clippy::cognitive_complexity
)]
/* Source Code Annotation Tags:
 *
 * Global tags (in tagref format <https://github.com/stepchowfun/tagref>) for source code
 * annotation:
 *
 * - [tag:needs_unit_test]
 * - [tag:needs_user_doc]
 * - [tag:needs_dev_doc]
 * - [tag:FIXME]
 * - [tag:TODO]
 * - [tag:VERIFY] Verify whether this is the correct way to do something
 * - [tag:DEBT] Technical debt
 * - [tag:msrv] Code that might need change when MSRV is bumped
 */
#![doc = include_str!("../README.md")]
//!
//! ## Description
//!
//! A crate that performs mail client operations such as
//! - Hold an [`Envelope`] with methods convenient for mail client use. (see
//!   module [`email`])
//! - Abstract through mail storages through the [`MailBackend`] trait, and
//!   handle read/writes/updates through it. (see module [`backends`])
//! - Decode attachments (see module [`email::attachments`])
//! - Create new mail (see [`email::Draft`](email::compose::Draft))
//! - Send mail with an SMTP client (see module [`smtp`])
//! - Manage contacts (see module [`contacts`])
//! - Build thread structures out of a list of mail via their `In-Reply-To` and
//!   `References` header values (see module [`thread`])
//!
//! Other exports are
//! - Basic mail account configuration to use with [`backends`] (see module
//!   [`conf`])
//! - A `debug` macro that works like `std::dbg` but for multiple threads. (see
//!   [`debug` macro](debug!))

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

pub mod text;

pub use utils::{
    datetime::UnixTimestamp,
    logging::{LogLevel, StderrLogger},
    SortField, SortOrder,
};

pub mod contacts;
pub use contacts::*;
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
pub use error::*;
pub mod thread;
pub use thread::*;
pub mod search;

#[macro_use]
pub mod utils;

#[cfg(feature = "gpgme")]
pub mod gpgme;
#[cfg(feature = "imap")]
pub mod imap;
#[cfg(feature = "jmap")]
pub mod jmap;
#[cfg(feature = "maildir")]
pub mod maildir;
pub mod mbox;
#[cfg(feature = "nntp")]
pub mod nntp;
#[cfg(feature = "notmuch")]
pub mod notmuch;
#[cfg(feature = "smtp")]
pub mod smtp;

#[macro_use]
extern crate serde_derive;
pub extern crate log;
/* parser */
extern crate data_encoding;
pub extern crate nom;

#[macro_use]
extern crate bitflags;
pub extern crate futures;
#[allow(unused_imports)]
#[macro_use]
pub extern crate indexmap;
#[cfg(feature = "sqlite3")]
pub extern crate rusqlite;
pub extern crate serde_path_to_error;
pub extern crate smallvec;
pub extern crate smol;
pub extern crate uuid;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct BytesDisplay(pub usize);

impl BytesDisplay {
    pub const KILOBYTE: f64 = 1024.0;
    pub const MEGABYTE: f64 = Self::KILOBYTE * 1024.0;
    pub const GIGABYTE: f64 = Self::MEGABYTE * 1024.0;
    pub const PETABYTE: f64 = Self::GIGABYTE * 1024.0;
}

impl std::fmt::Display for BytesDisplay {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let bytes: f64 = self.0 as f64;
        if bytes == 0.0 {
            write!(fmt, "0")
        } else if bytes < Self::KILOBYTE {
            write!(fmt, "{bytes:.2} bytes")
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

pub use utils::shellexpand::ShellExpandTrait;
