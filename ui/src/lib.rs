/*
 * meli - ui crate.
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

/*!
 * This library exports the public types and methods of its modules
 */

extern crate melib;
extern crate mime_apps;
extern crate notify_rust;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate chan;
extern crate chan_signal;
extern crate linkify;
extern crate uuid;

extern crate fnv;
extern crate termion;

#[macro_use]
extern crate nom;

use melib::*;
use std::collections::VecDeque;

#[macro_use]
mod types;
pub use types::*;

#[macro_use]
mod execute;
use execute::*;

pub mod state;
pub use state::*;

mod compose;
pub(crate) use compose::*;

pub mod components;
pub use components::*;

pub mod conf;
pub use conf::*;
