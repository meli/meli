/*
 * meli
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

//! Entities that handle Mail specific functions.

use indexmap::IndexMap;
use melib::{
    backends::{AccountHash, Mailbox, MailboxHash},
    email::{attachment_types::*, attachments::*},
    thread::ThreadNodeHash,
};

use super::*;
use crate::{
    melib::text_processing::{TextProcessing, Truncate},
    uuid::Uuid,
};

pub mod listing;
pub use crate::listing::*;
pub mod view;
pub use crate::view::*;
pub mod compose;
pub use self::compose::*;

#[cfg(feature = "gpgme")]
pub mod pgp;

pub mod status;
pub use self::status::*;
