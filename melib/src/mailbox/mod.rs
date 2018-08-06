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
use mailbox::backends::MailBackend;
pub mod accounts;
pub use mailbox::accounts::Account;
pub mod thread;
pub use mailbox::thread::{build_threads, Container};

use conf::Folder;

use std::option::Option;

/// `Mailbox` represents a folder of mail.
#[derive(Debug, Clone)]
pub struct Mailbox {
    pub folder: Folder,
    pub collection: Vec<Envelope>,
    pub threaded_collection: Vec<usize>,
    threads: Vec<Container>,
}

impl Mailbox {
    pub fn new_dummy() -> Self {
        Mailbox {
            folder: Folder::default(),
            collection: Vec::with_capacity(0),
            threaded_collection: Vec::with_capacity(0),
            threads: Vec::with_capacity(0),
        }
    }
    pub fn new(
        folder: &Folder,
        sent_folder: &Option<Result<Mailbox>>,
        collection: Result<Vec<Envelope>>,
    ) -> Result<Mailbox> {
        let mut collection: Vec<Envelope> = collection?;
        collection.sort_by(|a, b| a.date().cmp(&b.date()));
        let (threads, threaded_collection) = build_threads(&mut collection, sent_folder);
        Ok(Mailbox {
            folder: folder.clone(),
            collection: collection,
            threads: threads,
            threaded_collection: threaded_collection,
        })
    }
    pub fn len(&self) -> usize {
        self.collection.len()
    }
    pub fn threaded_mail(&self, i: usize) -> usize {
        let thread = self.threads[self.threaded_collection[i]];
        thread.message().unwrap()
    }
    pub fn mail_and_thread(&mut self, i: usize) -> (&mut Envelope, Container) {
        let x = &mut self.collection.as_mut_slice()[i];
        let thread = self.threads[x.thread()];
        (x, thread)
    }
    pub fn thread(&self, i: usize) -> &Container {
        &self.threads[i]
    }
}
