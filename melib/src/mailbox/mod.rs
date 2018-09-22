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

pub mod email;
pub use self::email::*;
/* Mail backends. Currently only maildir is supported */
pub mod backends;
use error::Result;
use mailbox::backends::{folder_default, Folder};
pub mod thread;
pub use mailbox::thread::{SortField, SortOrder, ThreadNode, Threads};

mod collection;
pub use self::collection::*;

use std::option::Option;

/// `Mailbox` represents a folder of mail.
#[derive(Debug)]
pub struct Mailbox {
    pub folder: Folder,
    name: String,
    pub collection: Collection,
    has_sent: bool,
}

impl Clone for Mailbox {
    fn clone(&self) -> Self {
        Mailbox {
            folder: self.folder.clone(),
            collection: self.collection.clone(),
            has_sent: self.has_sent,
            name: self.name.clone(),
        }
    }
}
impl Default for Mailbox {
    fn default() -> Self {
        Mailbox {
            folder: folder_default(),
            collection: Collection::default(),
            has_sent: false,
            name: String::new(),
        }
    }
}

impl Mailbox {
    pub fn new(folder: Folder, envelopes: Result<Vec<Envelope>>) -> Result<Mailbox> {
        let mut envelopes: Vec<Envelope> = envelopes?;
        envelopes.sort_by(|a, b| a.date().cmp(&b.date()));
        let collection = Collection::new(envelopes, &folder);
        let name = folder.name().into();
        Ok(Mailbox {
            folder,
            collection,
            name: name,
            ..Default::default()
        })
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_empty(&self) -> bool {
        self.collection.is_empty()
    }
    pub fn len(&self) -> usize {
        self.collection.len()
    }
    pub fn thread_to_mail_mut(&mut self, i: usize) -> &mut Envelope {
        self.collection
            .envelopes
            .entry(self.collection.threads.thread_to_mail(i))
            .or_default()
    }
    pub fn thread_to_mail(&self, i: usize) -> &Envelope {
        &self.collection.envelopes[&self.collection.threads.thread_to_mail(i)]
    }
    pub fn threaded_mail(&self, i: usize) -> EnvelopeHash {
        self.collection.threads.thread_to_mail(i)
    }
    pub fn mail_and_thread(&mut self, i: EnvelopeHash) -> (&mut Envelope, &ThreadNode) {
        let thread;
        {
            let x = &mut self.collection.envelopes.entry(i).or_default();
            thread = &self.collection.threads[x.thread()];
        }
        (self.collection.envelopes.entry(i).or_default(), thread)
    }
    pub fn thread(&self, i: usize) -> &ThreadNode {
        &self.collection.threads.thread_nodes()[i]
    }

    pub fn insert_sent_folder(&mut self, sent: &Mailbox) {
        if !self.has_sent {
            for envelope in sent.collection.envelopes.values().cloned() {
                self.insert_reply(envelope);
            }
            self.has_sent = true;
        }
    }

    pub fn update(&mut self, old_hash: EnvelopeHash, envelope: Envelope) {
        self.collection.remove(&old_hash);
        self.collection.insert(envelope);
    }

    pub fn insert(&mut self, envelope: Envelope) -> &Envelope {
        let hash = envelope.hash();
        self.collection.insert(envelope);
        &self.collection[&hash]
    }

    fn insert_reply(&mut self, envelope: Envelope) {
        self.collection.insert_reply(envelope);
    }

    pub fn remove(&mut self, envelope_hash: EnvelopeHash) {
        self.collection.remove(&envelope_hash);
        //   eprintln!("envelope_hash: {}\ncollection:\n{:?}", envelope_hash, self.collection);
    }
}
