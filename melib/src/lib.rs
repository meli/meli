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
pub mod conf;
pub mod error;
pub mod mailbox;
pub mod utilities;
pub mod async;

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

pub use conf::*;
pub use mailbox::*;

pub use error::{MeliError, Result};
pub use mailbox::backends::{Backends, RefreshEvent, RefreshEventConsumer};
pub use mailbox::email::{Envelope, Flag};
pub use utilities::*;
