//
// melib
//
// Copyright 2026 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of melib.
//
// melib is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// melib is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with melib. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{
    collections::HashMap,
    io::Read,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use crate::{
    backends::prelude::*,
    maildir::{
        utilities::{MaildirFilePathExt, MaildirMailboxPathExt, MaildirPath},
        MaildirMailbox,
    },
};

#[derive(Debug, Default)]
pub struct HashIndex {
    pub index: HashMap<EnvelopeHash, MaildirPath>,
    pub reverse_index: HashMap<PathBuf, EnvelopeHash>,
    pub mailbox_hash: MailboxHash,
}

impl Deref for HashIndex {
    type Target = HashMap<EnvelopeHash, MaildirPath>;

    fn deref(&self) -> &Self::Target {
        &self.index
    }
}

impl DerefMut for HashIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.index
    }
}

impl HashIndex {
    pub fn path_to_hash(&self, path: &Path) -> Option<EnvelopeHash> {
        self.reverse_index.get(path).cloned()
    }

    pub fn remove_env_hash(&mut self, env_hash: &EnvelopeHash) -> Option<MaildirPath> {
        let path = self.index.remove(env_hash)?;
        self.reverse_index.remove(&path.buf);
        Some(path)
    }
}

#[derive(Clone, Debug)]
pub struct Cache {
    pub account_name: Arc<String>,
    pub account_hash: AccountHash,
    pub mailboxes: Arc<Mutex<HashMap<MailboxHash, MaildirMailbox>>>,
    pub mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    pub hash_indexes: Arc<Mutex<HashMap<MailboxHash, HashIndex>>>,
    pub buffer: Vec<u8>,
}

impl Cache {
    pub fn create(&mut self, path: &Path) -> Result<(MailboxHash, Envelope)> {
        let Some(mailbox_hash): Option<MailboxHash> = path.to_mailbox_hash() else {
            return Err(Error::new("Invalid mailbox path").set_kind(ErrorKind::ValueError));
        };
        let mut hi_lck = self.hash_indexes.lock().unwrap();
        let mut mi = self.mailbox_index.lock().unwrap();
        let mailboxes_lck = self.mailboxes.lock().unwrap();
        let Some(mailbox) = mailboxes_lck.get(&mailbox_hash) else {
            return Err(Error::new("Mailbox does not exist in cache").set_kind(ErrorKind::NotFound));
        };

        let env_hash = path.to_envelope_hash();
        let mut reader = std::io::BufReader::new(std::fs::File::open(path)?);
        self.buffer.clear();
        reader.read_to_end(&mut self.buffer)?;

        let mut env = Envelope::from_bytes(self.buffer.as_slice(), Some(path.flags()))?;
        env.set_hash(env_hash);

        let hi = hi_lck.entry(mailbox_hash).or_default();
        hi.index.insert(env_hash, path.to_path_buf().into());
        hi.reverse_index.insert(path.to_path_buf(), env_hash);
        mi.insert(env.hash(), mailbox_hash);
        *mailbox.total.lock().unwrap() += 1;
        if !env.is_seen() {
            *mailbox.unseen.lock().unwrap() += 1;
        }
        Ok((mailbox_hash, env))
    }

    pub fn remove(&self, path: &Path) -> Option<(MailboxHash, EnvelopeHash)> {
        let mut hi = self.hash_indexes.lock().unwrap();
        let mut mi = self.mailbox_index.lock().unwrap();
        let mailboxes_lck = self.mailboxes.lock().unwrap();
        let (mailbox_hash, env_hash) = hi
            .iter()
            .find_map(|(k, v)| v.path_to_hash(path).map(|v| (*k, v)))?;
        let mailbox = mailboxes_lck.get(&mailbox_hash)?;
        mi.remove(&env_hash);
        hi.entry(mailbox_hash)
            .or_default()
            .remove_env_hash(&env_hash);
        *mailbox.total.lock().unwrap() -= 1;
        let flags = path.flags();
        let was_unseen: bool = !flags.contains(Flag::SEEN);
        if was_unseen {
            *mailbox.unseen.lock().unwrap() -= 1;
        }
        Some((mailbox_hash, env_hash))
    }

    pub fn remove_env_hash(&self, env_hash: EnvelopeHash) -> bool {
        let mut hi = self.hash_indexes.lock().unwrap();
        let mut mi = self.mailbox_index.lock().unwrap();
        let mailboxes_lck = self.mailboxes.lock().unwrap();
        let Some(mailbox_hash) = mi.remove(&env_hash) else {
            return false;
        };
        let Some(mailbox) = mailboxes_lck.get(&mailbox_hash) else {
            return false;
        };
        let Some(path) = hi
            .entry(mailbox_hash)
            .or_default()
            .remove_env_hash(&env_hash)
        else {
            return false;
        };
        *mailbox.total.lock().unwrap() -= 1;
        let flags = path.flags();
        let was_unseen: bool = !flags.contains(Flag::SEEN);
        if was_unseen {
            *mailbox.unseen.lock().unwrap() -= 1;
        }
        true
    }

    pub fn path_to_hash(&self, path: &Path) -> Option<(MailboxHash, EnvelopeHash)> {
        self.hash_indexes
            .lock()
            .unwrap()
            .iter()
            .find_map(|(k, v)| v.path_to_hash(path).map(|v| (*k, v)))
    }
}
