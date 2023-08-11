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
#[macro_use]
pub extern crate smallvec;
pub extern crate termion;

pub use structopt::StructOpt;

#[global_allocator]
static GLOBAL: System = System;

pub extern crate melib;
pub use melib::{
    error::*, log, AccountHash, Envelope, EnvelopeHash, EnvelopeRef, Flag, LogLevel, Mail, Mailbox,
    MailboxHash, ThreadHash, ToggleFlag,
};

pub mod args;
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
pub use crate::contacts::*;

#[macro_use]
pub mod conf;
pub use crate::conf::{
    Account, DotAddressable, IndexStyle, SearchBackend, Settings, Shortcuts, ThemeAttribute,
};

#[cfg(feature = "sqlite3")]
pub mod sqlite3;

pub mod jobs;
pub mod mailcap;
