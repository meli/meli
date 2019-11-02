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

use melib::backends::{FolderHash, MailBackend};
use melib::email::{EnvelopeHash, Flag, UnixTimestamp};
use melib::mailbox::*;
use melib::thread::{ThreadHash, ThreadNode};

#[derive(Debug)]
pub enum Query {
    Before(UnixTimestamp),
    After(UnixTimestamp),
    Between(UnixTimestamp, UnixTimestamp),
    On(UnixTimestamp),
    /* * * * */
    From(String),
    To(String),
    Cc(String),
    Bcc(String),
    InReplyTo(String),
    References(String),
    AllAddresses(String),
    /* * * * */
    Body(String),
    Subject(String),
    AllText(String),
    /* * * * */
    Flag(Flag),
}

enum CacheType {
    Sqlite3,
}

/*
pub struct Cache {
    collection: Collection,
    kind: CacheType,
    backend: Box<dyn MailBackend>,
}

impl Cache {
    pub fn build_index(&mut self) {
        unimplemented!()
    }

    pub fn new(backend: Box<dyn MailBackend>) -> Self {
        unimplemented!()
    }
    pub fn get_env(&self, h: &EnvelopeHash) -> &Envelope {
        &self.collection[h]
    }
    pub fn get_env_mut(&mut self, h: &EnvelopeHash) -> &mut Envelope {
        self.collection.entry(*h).or_default()
    }
    pub fn contains_key(&self, h: EnvelopeHash) -> bool {
        self.collection.contains_key(&h)
    }
    /*
    pub fn operation(&self, h: EnvelopeHash) -> Box<dyn BackendOp> {
                        //let operation = self.backend.operation(h, m.folder.hash())
                            unimplemented!()
        unreachable!()
    }
    */
    pub fn thread_to_mail_mut(&mut self, h: ThreadHash, f: FolderHash) -> &mut Envelope {
        self.collection
            .envelopes
            .entry(self.collection.threads[&f].thread_to_mail(h))
            .or_default()
    }
    pub fn thread_to_mail(&self, h: ThreadHash, f: FolderHash) -> &Envelope {
        &self.collection.envelopes[&self.collection.threads[&f].thread_to_mail(h)]
    }
    pub fn threaded_mail(&self, h: ThreadHash, f: FolderHash) -> EnvelopeHash {
        self.collection.threads[&f].thread_to_mail(h)
    }
    pub fn mail_and_thread(
        &mut self,
        i: EnvelopeHash,
        f: FolderHash,
    ) -> (&mut Envelope, &ThreadNode) {
        let thread;
        {
            let x = &mut self.collection.envelopes.entry(i).or_default();
            thread = &self.collection.threads[&f][&x.thread()];
        }
        (self.collection.envelopes.entry(i).or_default(), thread)
    }
    pub fn thread(&self, h: ThreadHash, f: FolderHash) -> &ThreadNode {
        &self.collection.threads[&f].thread_nodes()[&h]
    }
}
*/
