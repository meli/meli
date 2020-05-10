/*
 * meli - melib crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

use super::*;
use crate::backends::MailboxHash;
use core::ops::{Index, IndexMut};
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use std::collections::{HashMap, HashSet};

pub struct EnvelopeRef<'g> {
    guard: RwLockReadGuard<'g, HashMap<EnvelopeHash, Envelope>>,
    env_hash: EnvelopeHash,
}

impl Deref for EnvelopeRef<'_> {
    type Target = Envelope;

    fn deref(&self) -> &Envelope {
        self.guard.get(&self.env_hash).unwrap()
    }
}

pub struct EnvelopeRefMut<'g> {
    guard: RwLockWriteGuard<'g, HashMap<EnvelopeHash, Envelope>>,
    env_hash: EnvelopeHash,
}

impl Deref for EnvelopeRefMut<'_> {
    type Target = Envelope;

    fn deref(&self) -> &Envelope {
        self.guard.get(&self.env_hash).unwrap()
    }
}

impl DerefMut for EnvelopeRefMut<'_> {
    fn deref_mut(&mut self) -> &mut Envelope {
        self.guard.get_mut(&self.env_hash).unwrap()
    }
}

#[derive(Debug, Clone, Deserialize, Default, Serialize)]
pub struct Collection {
    pub envelopes: Arc<RwLock<HashMap<EnvelopeHash, Envelope>>>,
    message_ids: HashMap<Vec<u8>, EnvelopeHash>,
    date_index: BTreeMap<UnixTimestamp, EnvelopeHash>,
    subject_index: Option<BTreeMap<String, EnvelopeHash>>,
    pub threads: HashMap<MailboxHash, Threads>,
    sent_mailbox: Option<MailboxHash>,
    pub mailboxes: HashMap<MailboxHash, HashSet<EnvelopeHash>>,
}

impl Drop for Collection {
    fn drop(&mut self) {
        /*
            let cache_dir: xdg::BaseDirectories =
                xdg::BaseDirectories::with_profile("meli", "threads".to_string()).unwrap();
            if let Ok(cached) = cache_dir.place_cache_file("threads") {
                /* place result in cache directory */
                let f = match fs::File::create(cached) {
                    Ok(f) => f,
                    Err(e) => {
                        panic!("{}", e);
                    }
                };
                let writer = io::BufWriter::new(f);
                bincode::serialize_into(writer, &self.threads).unwrap();
            }
        */
    }
}

impl Collection {
    pub fn new(envelopes: HashMap<EnvelopeHash, Envelope>) -> Collection {
        let date_index = BTreeMap::new();
        let subject_index = None;
        let message_ids = HashMap::with_capacity_and_hasher(2048, Default::default());

        /* Scrap caching for now. When a cached threads file is loaded, we must remove/rehash the
         * thread nodes that shouldn't exist anymore (e.g. because their file moved from /new to
         * /cur, or it was deleted).
         */
        let threads = HashMap::with_capacity_and_hasher(16, Default::default());
        let mailboxes = HashMap::with_capacity_and_hasher(16, Default::default());

        Collection {
            envelopes: Arc::new(RwLock::new(envelopes)),
            date_index,
            message_ids,
            subject_index,
            threads,
            mailboxes,
            sent_mailbox: None,
        }
    }

    pub fn len(&self) -> usize {
        self.envelopes.read().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.envelopes.read().unwrap().is_empty()
    }

    pub fn remove(&mut self, envelope_hash: EnvelopeHash, mailbox_hash: MailboxHash) {
        debug!("DEBUG: Removing {}", envelope_hash);
        self.envelopes.write().unwrap().remove(&envelope_hash);
        self.mailboxes.entry(mailbox_hash).and_modify(|m| {
            m.remove(&envelope_hash);
        });
        self.threads
            .entry(mailbox_hash)
            .or_default()
            .remove(envelope_hash);
        for (h, t) in self.threads.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.remove(envelope_hash);
        }
    }

    pub fn rename(
        &mut self,
        old_hash: EnvelopeHash,
        new_hash: EnvelopeHash,
        mailbox_hash: MailboxHash,
    ) {
        if !self.envelopes.write().unwrap().contains_key(&old_hash) {
            return;
        }
        let mut envelope = self.envelopes.write().unwrap().remove(&old_hash).unwrap();
        self.mailboxes.entry(mailbox_hash).and_modify(|m| {
            m.remove(&old_hash);
            m.insert(new_hash);
        });
        envelope.set_hash(new_hash);
        self.message_ids
            .insert(envelope.message_id().raw().to_vec(), new_hash);
        self.envelopes.write().unwrap().insert(new_hash, envelope);
        {
            if self
                .threads
                .entry(mailbox_hash)
                .or_default()
                .update_envelope(&self.envelopes, old_hash, new_hash)
                .is_ok()
            {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        self.threads
            .entry(mailbox_hash)
            .or_default()
            .insert(&mut self.envelopes, new_hash);
        for (h, t) in self.threads.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.update_envelope(&self.envelopes, old_hash, new_hash)
                .ok()
                .take();
        }
    }

    /// Merge new mailbox to collection and update threads.
    /// Returns a list of already existing mailboxs whose threads were updated
    pub fn merge(
        &mut self,
        mut new_envelopes: HashMap<EnvelopeHash, Envelope>,
        mailbox_hash: MailboxHash,
        sent_mailbox: Option<MailboxHash>,
    ) -> Option<SmallVec<[MailboxHash; 8]>> {
        self.sent_mailbox = sent_mailbox;
        for (h, e) in new_envelopes.iter() {
            self.message_ids.insert(e.message_id().raw().to_vec(), *h);
        }

        let &mut Collection {
            ref mut threads,
            ref mut envelopes,
            ref mut mailboxes,
            ref sent_mailbox,
            ..
        } = self;

        if !threads.contains_key(&mailbox_hash) {
            threads.insert(mailbox_hash, Threads::new(new_envelopes.len()));
            mailboxes.insert(mailbox_hash, new_envelopes.keys().cloned().collect());
            for (h, e) in new_envelopes {
                envelopes.write().unwrap().insert(h, e);
            }
        } else {
            mailboxes.entry(mailbox_hash).and_modify(|m| {
                m.extend(new_envelopes.keys().cloned());
            });
            threads.entry(mailbox_hash).and_modify(|t| {
                let mut ordered_hash_set =
                    new_envelopes.keys().cloned().collect::<Vec<EnvelopeHash>>();
                ordered_hash_set.sort_by(|a, b| {
                    new_envelopes[a]
                        .date()
                        .partial_cmp(&new_envelopes[b].date())
                        .unwrap()
                });
                for h in ordered_hash_set {
                    envelopes
                        .write()
                        .unwrap()
                        .insert(h, new_envelopes.remove(&h).unwrap());
                    t.insert(envelopes, h);
                }
            });
        }

        let mut ret = SmallVec::new();
        let keys = threads.keys().cloned().collect::<Vec<MailboxHash>>();
        for t_fh in keys {
            if t_fh == mailbox_hash {
                continue;
            }
            if sent_mailbox.map(|f| f == mailbox_hash).unwrap_or(false) {
                let envelopes_lck = envelopes.read().unwrap();
                let mut ordered_hash_set = threads[&mailbox_hash]
                    .hash_set
                    .iter()
                    .cloned()
                    .collect::<Vec<EnvelopeHash>>();
                ordered_hash_set.sort_by(|a, b| {
                    envelopes_lck[a]
                        .date()
                        .partial_cmp(&envelopes_lck[b].date())
                        .unwrap()
                });
                drop(envelopes_lck);
                let mut updated = false;
                for h in ordered_hash_set {
                    updated |= threads.entry(t_fh).or_default().insert_reply(envelopes, h);
                }
                if updated {
                    ret.push(t_fh);
                }
                continue;
            }
            if sent_mailbox.map(|f| f == t_fh).unwrap_or(false) {
                let envelopes_lck = envelopes.read().unwrap();
                let mut ordered_hash_set = threads[&t_fh]
                    .hash_set
                    .iter()
                    .cloned()
                    .collect::<Vec<EnvelopeHash>>();
                ordered_hash_set.sort_by(|a, b| {
                    envelopes_lck[a]
                        .date()
                        .partial_cmp(&envelopes_lck[b].date())
                        .unwrap()
                });
                drop(envelopes_lck);
                let mut updated = false;
                for h in ordered_hash_set {
                    updated |= threads
                        .entry(mailbox_hash)
                        .or_default()
                        .insert_reply(envelopes, h);
                }
                if updated {
                    ret.push(mailbox_hash);
                }
            }
        }
        if ret.is_empty() {
            None
        } else {
            Some(ret)
        }
    }

    pub fn update(
        &mut self,
        old_hash: EnvelopeHash,
        mut envelope: Envelope,
        mailbox_hash: MailboxHash,
    ) {
        let old_env = self.envelopes.write().unwrap().remove(&old_hash).unwrap();
        envelope.set_thread(old_env.thread());
        let new_hash = envelope.hash();
        self.mailboxes.entry(mailbox_hash).and_modify(|m| {
            m.remove(&old_hash);
            m.insert(new_hash);
        });
        self.message_ids
            .insert(envelope.message_id().raw().to_vec(), new_hash);
        self.envelopes.write().unwrap().insert(new_hash, envelope);
        if self
            .sent_mailbox
            .map(|f| f == mailbox_hash)
            .unwrap_or(false)
        {
            for (_, t) in self.threads.iter_mut() {
                t.update_envelope(&self.envelopes, old_hash, new_hash)
                    .unwrap_or(());
            }
        }
        {
            if self
                .threads
                .entry(mailbox_hash)
                .or_default()
                .update_envelope(&self.envelopes, old_hash, new_hash)
                .is_ok()
            {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        self.threads
            .entry(mailbox_hash)
            .or_default()
            .insert(&mut self.envelopes, new_hash);
        for (h, t) in self.threads.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.update_envelope(&self.envelopes, old_hash, new_hash)
                .ok()
                .take();
        }
    }

    pub fn insert(&mut self, envelope: Envelope, mailbox_hash: MailboxHash) -> bool {
        let hash = envelope.hash();
        if self.message_ids.contains_key(envelope.message_id().raw()) {
            /* Duplicate. For example could be same message sent to two mailing lists and we get
             * it twice */
            return true;
        };
        self.mailboxes.entry(mailbox_hash).and_modify(|m| {
            m.insert(hash);
        });
        self.message_ids
            .insert(envelope.message_id().raw().to_vec(), hash);
        self.envelopes.write().unwrap().insert(hash, envelope);
        self.threads
            .entry(mailbox_hash)
            .or_default()
            .insert(&mut self.envelopes, hash);
        if self
            .sent_mailbox
            .map(|f| f == mailbox_hash)
            .unwrap_or(false)
        {
            self.insert_reply(hash);
        }
        false
    }

    pub fn insert_reply(&mut self, env_hash: EnvelopeHash) {
        debug_assert!(self.envelopes.read().unwrap().contains_key(&env_hash));
        for (_, t) in self.threads.iter_mut() {
            t.insert_reply(&mut self.envelopes, env_hash);
        }
    }

    pub fn get_env<'g>(&'g self, env_hash: EnvelopeHash) -> EnvelopeRef<'g> {
        let guard: RwLockReadGuard<'g, _> = self.envelopes.read().unwrap();
        EnvelopeRef { guard, env_hash }
    }

    pub fn get_env_mut<'g>(&'g mut self, env_hash: EnvelopeHash) -> EnvelopeRefMut<'g> {
        let guard = self.envelopes.write().unwrap();
        EnvelopeRefMut { guard, env_hash }
    }

    pub fn contains_key(&self, env_hash: &EnvelopeHash) -> bool {
        self.envelopes.read().unwrap().contains_key(env_hash)
    }

    pub fn new_mailbox(&mut self, mailbox_hash: MailboxHash) {
        if !self.mailboxes.contains_key(&mailbox_hash) {
            self.mailboxes.insert(mailbox_hash, Default::default());
            self.threads.insert(mailbox_hash, Threads::default());
        }
    }
}

impl Index<&MailboxHash> for Collection {
    type Output = HashSet<EnvelopeHash>;
    fn index(&self, index: &MailboxHash) -> &HashSet<EnvelopeHash> {
        &self.mailboxes[index]
    }
}

impl IndexMut<&MailboxHash> for Collection {
    fn index_mut(&mut self, index: &MailboxHash) -> &mut HashSet<EnvelopeHash> {
        self.mailboxes.get_mut(index).unwrap()
    }
}
