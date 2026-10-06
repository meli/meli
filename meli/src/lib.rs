/*
 * meli - lib.rs
 *
 * Copyright 2017-2022 Manos Pitsidianakis
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

/* Source Code Annotation Tags:
 *
 * Global tags (in tagref format <https://github.com/stepchowfun/tagref>) for source code
 * annotation:
 *
 * - tags from melib/src/lib.rs.
 */

//!
//!  This crate contains the frontend stuff of the application. The application
//! entry way on  `src/bin.rs` creates an event loop and passes input to a
//! thread.
//!
//! The mail handling stuff is done in the `melib` crate which includes all
//! backend needs. The split is done to theoretically be able to create
//! different frontends with the same innards.

use std::alloc::System;
pub use std::{collections::VecDeque, path::PathBuf};

#[macro_use]
extern crate serde_derive;
extern crate linkify;
pub use melib::uuid;

pub extern crate bitflags;
pub extern crate serde_json;
pub extern crate smallvec;
pub extern crate termion;

pub use structopt::StructOpt;

#[global_allocator]
static GLOBAL: System = System;

pub extern crate melib;
pub use melib::{
    error::*, log, AccountHash, ActionFlag, Envelope, EnvelopeHash, EnvelopeRef, Flag, LogLevel,
    Mail, Mailbox, MailboxHash, ThreadHash, ToggleFlag,
};

pub mod args;
#[cfg(feature = "cli-docs")]
pub mod manpages;
pub mod signal_handlers;
pub mod subcommands;

#[macro_use]
pub mod types;
pub use crate::types::*;

#[macro_use]
pub mod terminal;
pub use crate::terminal::*;

#[macro_use]
pub mod command;
pub use crate::command::*;

pub mod state;
pub use crate::state::*;

pub mod components;
pub use crate::components::*;

pub mod utilities;
pub use crate::utilities::*;

pub mod contacts;

pub mod mail;
pub use crate::mail::*;

pub mod notifications;

pub mod manage;
pub use manage::*;

// #[cfg(feature = "svgscreenshot")]
// pub mod svg;

#[macro_use]
pub mod conf;
pub use crate::conf::{
    data_types::{IndexStyle, SearchBackend},
    DotAddressable, Settings, Shortcuts,
};

#[cfg(feature = "sqlite3")]
pub mod sqlite3;

pub mod jobs;
pub mod mailcap;

pub mod accounts;
pub use self::accounts::Account;

pub mod version_migrations;
