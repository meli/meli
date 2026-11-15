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

use std::{future::Future, pin::Pin};

use melib::{email::attachments::*, error::Result};

pub type AttachmentBoxFuture = Pin<Box<dyn Future<Output = Result<AttachmentBuilder>> + Send>>;
pub type AttachmentFilterBox = Box<dyn FnOnce(AttachmentBuilder) -> AttachmentBoxFuture + Send>;

pub mod compose;
pub mod listing;
pub mod pgp;
pub mod status;
pub mod view;
