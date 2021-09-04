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
use smallvec::SmallVec;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use std::collections::{BTreeMap, HashMap, HashSet};

pub type EnvelopeRef<'g> = RwRef<'g, EnvelopeHash, Envelope>;
pub type EnvelopeRefMut<'g> = RwRefMut<'g, EnvelopeHash, Envelope>;

#[derive(Debug, Clone)]
pub struct Collection {
    pub envelopes: Arc<RwLock<HashMap<EnvelopeHash, Envelope>>>,
    pub message_id_index: Arc<RwLock<HashMap<Vec<u8>, EnvelopeHash>>>,
    pub threads: Arc<RwLock<HashMap<MailboxHash, Threads>>>,
    pub sent_mailbox: Arc<RwLock<Option<MailboxHash>>>,
    pub mailboxes: Arc<RwLock<HashMap<MailboxHash, HashSet<EnvelopeHash>>>>,
    pub tag_index: Arc<RwLock<BTreeMap<u64, String>>>,
}

impl Default for Collection {
    fn default() -> Self {
        Self::new()
    }
}

/*
impl Drop for Collection {
    fn drop(&mut self) {
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
                let _ = bincode::Options::serialize_into(
                    bincode::config::DefaultOptions::new(),
                    writer,
                    &self.thread,
                );
            }
    }
}
*/

impl Collection {
    pub fn new() -> Collection {
        let message_id_index = Arc::new(RwLock::new(HashMap::with_capacity_and_hasher(
            16,
            Default::default(),
        )));
        let threads = Arc::new(RwLock::new(HashMap::with_capacity_and_hasher(
            16,
            Default::default(),
        )));
        let mailboxes = Arc::new(RwLock::new(HashMap::with_capacity_and_hasher(
            16,
            Default::default(),
        )));

        Collection {
            envelopes: Arc::new(RwLock::new(Default::default())),
            tag_index: Arc::new(RwLock::new(BTreeMap::default())),
            message_id_index,
            threads,
            mailboxes,
            sent_mailbox: Arc::new(RwLock::new(None)),
        }
    }

    pub fn len(&self) -> usize {
        self.envelopes.read().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.envelopes.read().unwrap().is_empty()
    }

    pub fn remove(&self, envelope_hash: EnvelopeHash, mailbox_hash: MailboxHash) {
        debug!("DEBUG: Removing {}", envelope_hash);
        self.envelopes.write().unwrap().remove(&envelope_hash);
        self.mailboxes
            .write()
            .unwrap()
            .entry(mailbox_hash)
            .and_modify(|m| {
                m.remove(&envelope_hash);
            });
        let mut threads_lck = self.threads.write().unwrap();
        threads_lck
            .entry(mailbox_hash)
            .or_default()
            .remove(envelope_hash);
        for (h, t) in threads_lck.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.remove(envelope_hash);
        }
    }

    pub fn rename(
        &self,
        old_hash: EnvelopeHash,
        new_hash: EnvelopeHash,
        mailbox_hash: MailboxHash,
    ) -> bool {
        if !self.envelopes.read().unwrap().contains_key(&old_hash) {
            return false;
        }
        let mut envelope = self.envelopes.write().unwrap().remove(&old_hash).unwrap();
        self.mailboxes
            .write()
            .unwrap()
            .entry(mailbox_hash)
            .and_modify(|m| {
                m.remove(&old_hash);
                m.insert(new_hash);
            });
        envelope.set_hash(new_hash);
        self.envelopes.write().unwrap().insert(new_hash, envelope);
        let mut threads_lck = self.threads.write().unwrap();
        {
            if threads_lck
                .entry(mailbox_hash)
                .or_default()
                .update_envelope(&self.envelopes, old_hash, new_hash)
                .is_ok()
            {
                return true;
            }
        }
        /* envelope is not in threads, so insert it */
        threads_lck
            .entry(mailbox_hash)
            .or_default()
            .insert(&self.envelopes, new_hash);
        for (h, t) in threads_lck.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.update_envelope(&self.envelopes, old_hash, new_hash)
                .ok()
                .take();
        }
        true
    }

    /// Merge new mailbox to collection and update threads.
    /// Returns a list of already existing mailboxs whose threads were updated
    pub fn merge(
        &self,
        mut new_envelopes: HashMap<EnvelopeHash, Envelope>,
        mailbox_hash: MailboxHash,
        sent_mailbox: Option<MailboxHash>,
    ) -> Option<SmallVec<[MailboxHash; 8]>> {
        *self.sent_mailbox.write().unwrap() = sent_mailbox;

        let Collection {
            ref threads,
            ref envelopes,
            ref mailboxes,
            ref sent_mailbox,
            ..
        } = self;

        let mut threads_lck = threads.write().unwrap();
        let mut mailboxes_lck = mailboxes.write().unwrap();
        if !threads_lck.contains_key(&mailbox_hash) {
            threads_lck.insert(mailbox_hash, Threads::new(new_envelopes.len()));
            mailboxes_lck.insert(mailbox_hash, new_envelopes.keys().cloned().collect());
            for (h, e) in new_envelopes {
                envelopes.write().unwrap().insert(h, e);
            }
        } else {
            mailboxes_lck.entry(mailbox_hash).and_modify(|m| {
                m.extend(new_envelopes.keys().cloned());
            });
            threads_lck.entry(mailbox_hash).and_modify(|t| {
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
        let keys = threads_lck.keys().cloned().collect::<Vec<MailboxHash>>();
        for t_fh in keys {
            if t_fh == mailbox_hash {
                continue;
            }
            if sent_mailbox
                .read()
                .unwrap()
                .map(|f| f == mailbox_hash)
                .unwrap_or(false)
            {
                let envelopes_lck = envelopes.read().unwrap();
                let mut ordered_hash_set = threads_lck[&mailbox_hash]
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
                    updated |= threads_lck
                        .entry(t_fh)
                        .or_default()
                        .insert_reply(envelopes, h);
                }
                if updated {
                    ret.push(t_fh);
                }
                continue;
            }
            if sent_mailbox
                .read()
                .unwrap()
                .map(|f| f == t_fh)
                .unwrap_or(false)
            {
                let envelopes_lck = envelopes.read().unwrap();
                let mut ordered_hash_set = threads_lck[&t_fh]
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
                    updated |= threads_lck
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
        &self,
        old_hash: EnvelopeHash,
        mut envelope: Envelope,
        mailbox_hash: MailboxHash,
    ) {
        let old_env = self.envelopes.write().unwrap().remove(&old_hash).unwrap();
        envelope.set_thread(old_env.thread());
        let new_hash = envelope.hash();
        self.mailboxes
            .write()
            .unwrap()
            .entry(mailbox_hash)
            .and_modify(|m| {
                m.remove(&old_hash);
                m.insert(new_hash);
            });
        self.envelopes.write().unwrap().insert(new_hash, envelope);
        let mut threads_lck = self.threads.write().unwrap();
        if self
            .sent_mailbox
            .read()
            .unwrap()
            .map(|f| f == mailbox_hash)
            .unwrap_or(false)
        {
            for (_, t) in threads_lck.iter_mut() {
                t.update_envelope(&self.envelopes, old_hash, new_hash)
                    .unwrap_or(());
            }
        }
        {
            if threads_lck
                .entry(mailbox_hash)
                .or_default()
                .update_envelope(&self.envelopes, old_hash, new_hash)
                .is_ok()
            {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        threads_lck
            .entry(mailbox_hash)
            .or_default()
            .insert(&self.envelopes, new_hash);
        for (h, t) in threads_lck.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.update_envelope(&self.envelopes, old_hash, new_hash)
                .ok()
                .take();
        }
    }

    pub fn update_flags(&self, env_hash: EnvelopeHash, mailbox_hash: MailboxHash) {
        let mut threads_lck = self.threads.write().unwrap();
        if self
            .sent_mailbox
            .read()
            .unwrap()
            .map(|f| f == mailbox_hash)
            .unwrap_or(false)
        {
            for (_, t) in threads_lck.iter_mut() {
                t.update_envelope(&self.envelopes, env_hash, env_hash)
                    .unwrap_or(());
            }
        }
        {
            if threads_lck
                .entry(mailbox_hash)
                .or_default()
                .update_envelope(&self.envelopes, env_hash, env_hash)
                .is_ok()
            {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        threads_lck
            .entry(mailbox_hash)
            .or_default()
            .insert(&self.envelopes, env_hash);
        for (h, t) in threads_lck.iter_mut() {
            if *h == mailbox_hash {
                continue;
            }
            t.update_envelope(&self.envelopes, env_hash, env_hash)
                .ok()
                .take();
        }
    }

    pub fn insert(&self, envelope: Envelope, mailbox_hash: MailboxHash) -> bool {
        let hash = envelope.hash();
        self.mailboxes
            .write()
            .unwrap()
            .entry(mailbox_hash)
            .and_modify(|m| {
                m.insert(hash);
            });
        self.envelopes.write().unwrap().insert(hash, envelope);
        self.threads
            .write()
            .unwrap()
            .entry(mailbox_hash)
            .or_default()
            .insert(&self.envelopes, hash);
        if self
            .sent_mailbox
            .read()
            .unwrap()
            .map(|f| f == mailbox_hash)
            .unwrap_or(false)
        {
            self.insert_reply(hash);
        }
        false
    }

    pub fn insert_reply(&self, env_hash: EnvelopeHash) {
        debug_assert!(self.envelopes.read().unwrap().contains_key(&env_hash));
        for (_, t) in self.threads.write().unwrap().iter_mut() {
            t.insert_reply(&self.envelopes, env_hash);
        }
    }

    pub fn get_env(&'_ self, hash: EnvelopeHash) -> EnvelopeRef<'_> {
        let guard: RwLockReadGuard<'_, _> = self.envelopes.read().unwrap();
        EnvelopeRef { guard, hash }
    }

    pub fn get_env_mut(&'_ self, hash: EnvelopeHash) -> EnvelopeRefMut<'_> {
        let guard = self.envelopes.write().unwrap();
        EnvelopeRefMut { guard, hash }
    }

    pub fn get_threads(&'_ self, hash: MailboxHash) -> RwRef<'_, MailboxHash, Threads> {
        let guard = self.threads.read().unwrap();
        RwRef { guard, hash }
    }

    pub fn get_mailbox(
        &'_ self,
        hash: MailboxHash,
    ) -> RwRef<'_, MailboxHash, HashSet<EnvelopeHash>> {
        let guard = self.mailboxes.read().unwrap();
        RwRef { guard, hash }
    }

    pub fn contains_key(&self, env_hash: &EnvelopeHash) -> bool {
        self.envelopes.read().unwrap().contains_key(env_hash)
    }

    pub fn new_mailbox(&self, mailbox_hash: MailboxHash) {
        let mut mailboxes_lck = self.mailboxes.write().unwrap();
        if !mailboxes_lck.contains_key(&mailbox_hash) {
            mailboxes_lck.insert(mailbox_hash, Default::default());
            self.threads
                .write()
                .unwrap()
                .insert(mailbox_hash, Threads::default());
        }
    }
}

pub struct RwRef<'g, K: std::cmp::Eq + std::hash::Hash, V> {
    guard: RwLockReadGuard<'g, HashMap<K, V>>,
    hash: K,
}

impl<K: std::cmp::Eq + std::hash::Hash, V> Deref for RwRef<'_, K, V> {
    type Target = V;

    fn deref(&self) -> &V {
        self.guard.get(&self.hash).unwrap()
    }
}

pub struct RwRefMut<'g, K: std::cmp::Eq + std::hash::Hash, V> {
    guard: RwLockWriteGuard<'g, HashMap<K, V>>,
    hash: K,
}

impl<K: std::cmp::Eq + std::hash::Hash, V> DerefMut for RwRefMut<'_, K, V> {
    fn deref_mut(&mut self) -> &mut V {
        self.guard.get_mut(&self.hash).unwrap()
    }
}

impl<K: std::cmp::Eq + std::hash::Hash, V> Deref for RwRefMut<'_, K, V> {
    type Target = V;

    fn deref(&self) -> &V {
        self.guard.get(&self.hash).unwrap()
    }
}
