/*
 * meli - mailbox module.
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

/*!
 * Mail related code.
 *
 * This module handles reading emails from various backends, handling account data etc
 */

use crate::backends::Folder;
pub use crate::email::*;
use crate::error::Result;
use crate::thread::ThreadHash;

pub use crate::thread::{SortField, SortOrder, ThreadNode, Threads};

pub use crate::collection::*;

use fnv::{FnvHashMap, FnvHashSet};
/// `Mailbox` represents a folder of mail.
#[derive(Debug, Deserialize, Serialize, Clone, Default)]
pub struct Mailbox {
    #[serde(skip_serializing, skip_deserializing)]
    pub folder: Folder,
    name: String,
    pub envelopes: FnvHashSet<EnvelopeHash>,
    pub thread_root_set: FnvHashSet<ThreadHash>,
    has_sent: bool,
}

impl Mailbox {
    pub fn new(
        folder: Folder,
        envelopes: Result<&FnvHashMap<EnvelopeHash, Envelope>>,
    ) -> Result<Mailbox> {
        let envelopes = envelopes?;
        let name = folder.name().into();
        let envelopes = envelopes.keys().cloned().collect();
        Ok(Mailbox {
            folder,
            name,
            envelopes,
            ..Default::default()
        })
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_empty(&self) -> bool {
        self.envelopes.is_empty()
    }
    pub fn len(&self) -> usize {
        self.envelopes.len()
    }
    pub fn insert(&mut self, h: EnvelopeHash) {
        self.envelopes.insert(h);
    }
    pub fn rename(&mut self, old_hash: EnvelopeHash, new_hash: EnvelopeHash) {
        self.envelopes.remove(&old_hash);

        self.envelopes.insert(new_hash);
    }
    pub fn remove(&mut self, h: EnvelopeHash) {
        self.envelopes.remove(&h);
    }
}
