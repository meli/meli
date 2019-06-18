use super::*;
use crate::backends::FolderHash;
use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::ops::{Deref, DerefMut};

use fnv::FnvHashMap;

#[derive(Debug, Clone, Deserialize, Default, Serialize)]
pub struct Collection {
    pub envelopes: FnvHashMap<EnvelopeHash, Envelope>,
    message_ids: FnvHashMap<Vec<u8>, EnvelopeHash>,
    date_index: BTreeMap<UnixTimestamp, EnvelopeHash>,
    subject_index: Option<BTreeMap<String, EnvelopeHash>>,
    pub threads: FnvHashMap<FolderHash, Threads>,
    sent_folder: Option<FolderHash>,
}

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
            bincode::serialize_into(writer, &self.threads).unwrap();
        }
    }
}

impl Collection {
    pub fn new(envelopes: FnvHashMap<EnvelopeHash, Envelope>) -> Collection {
        let date_index = BTreeMap::new();
        let subject_index = None;
        let message_ids = FnvHashMap::with_capacity_and_hasher(2048, Default::default());

        /* Scrap caching for now. When a cached threads file is loaded, we must remove/rehash the
         * thread nodes that shouldn't exist anymore (e.g. because their file moved from /new to
         * /cur, or it was deleted).
         */
        let threads = FnvHashMap::with_capacity_and_hasher(16, Default::default());

        Collection {
            envelopes,
            date_index,
            message_ids,
            subject_index,
            threads,
            sent_folder: None,
        }
    }

    pub fn len(&self) -> usize {
        self.envelopes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.envelopes.is_empty()
    }

    pub fn remove(&mut self, envelope_hash: EnvelopeHash, folder_hash: FolderHash) {
        debug!("DEBUG: Removing {}", envelope_hash);
        self.envelopes.remove(&envelope_hash);
        self.threads
            .entry(folder_hash)
            .or_default()
            .remove(envelope_hash, &mut self.envelopes);
    }

    pub fn rename(
        &mut self,
        old_hash: EnvelopeHash,
        new_hash: EnvelopeHash,
        folder_hash: FolderHash,
    ) {
        if !self.envelopes.contains_key(&old_hash) {
            return;
        }
        let mut env = self.envelopes.remove(&old_hash).unwrap();
        env.set_hash(new_hash);
        self.message_ids
            .insert(env.message_id().raw().to_vec(), new_hash);
        self.envelopes.insert(new_hash, env);
        {
            if self
                .threads
                .entry(folder_hash)
                .or_default()
                .update_envelope(old_hash, new_hash, &self.envelopes)
                .is_ok()
            {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        let env = self.envelopes.entry(new_hash).or_default() as *mut Envelope;
        unsafe {
            self.threads
                .entry(folder_hash)
                .or_default()
                .insert(&mut (*env), &self.envelopes);
        }
    }

    pub fn merge(
        &mut self,
        mut envelopes: FnvHashMap<EnvelopeHash, Envelope>,
        folder_hash: FolderHash,
        mailbox: &mut Result<Mailbox>,
        sent_folder: Option<FolderHash>,
    ) {
        self.sent_folder = sent_folder;
        envelopes.retain(|&h, e| {
            if self.message_ids.contains_key(e.message_id().raw()) {
                /* skip duplicates until a better way to handle them is found. */
                //FIXME
                if let Ok(mailbox) = mailbox.as_mut() {
                    mailbox.remove(h);
                }
                false
            } else {
                self.message_ids.insert(e.message_id().raw().to_vec(), h);
                true
            }
        });
        let mut new_threads = Threads::new(&mut envelopes);

        for (h, e) in envelopes {
            self.envelopes.insert(h, e);
        }
        let &mut Collection {
            ref mut threads,
            ref mut envelopes,
            ref sent_folder,
            ..
        } = self;
        for (t_fh, t) in threads.iter_mut() {
            if sent_folder.map(|f| f == folder_hash).unwrap_or(false) {
                let mut ordered_hash_set = new_threads
                    .hash_set
                    .iter()
                    .cloned()
                    .collect::<Vec<EnvelopeHash>>();
                ordered_hash_set.sort_by(|a, b| {
                    envelopes[a]
                        .date()
                        .partial_cmp(&envelopes[b].date())
                        .unwrap()
                });
                for h in ordered_hash_set {
                    t.insert_reply(envelopes, h);
                }
                continue;
            }
            if sent_folder.map(|f| f == *t_fh).unwrap_or(false) {
                let mut ordered_hash_set =
                    t.hash_set.iter().cloned().collect::<Vec<EnvelopeHash>>();
                ordered_hash_set.sort_by(|a, b| {
                    envelopes[a]
                        .date()
                        .partial_cmp(&envelopes[b].date())
                        .unwrap()
                });
                for h in ordered_hash_set {
                    new_threads.insert_reply(envelopes, h);
                }
            }
        }
        threads.insert(folder_hash, new_threads);
    }

    pub fn update(&mut self, old_hash: EnvelopeHash, envelope: Envelope, folder_hash: FolderHash) {
        self.envelopes.remove(&old_hash);
        let new_hash = envelope.hash();
        self.message_ids
            .insert(envelope.message_id().raw().to_vec(), new_hash);
        self.envelopes.insert(new_hash, envelope);
        if self.sent_folder.map(|f| f == folder_hash).unwrap_or(false) {
            for (_, t) in self.threads.iter_mut() {
                t.update_envelope(old_hash, new_hash, &self.envelopes)
                    .unwrap_or(());
            }
        }
        {
            if self
                .threads
                .entry(folder_hash)
                .or_default()
                .update_envelope(old_hash, new_hash, &self.envelopes)
                .is_ok()
            {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        let env = self.envelopes.entry(new_hash).or_default() as *mut Envelope;
        unsafe {
            self.threads
                .entry(folder_hash)
                .or_default()
                .insert(&mut (*env), &self.envelopes);
        }
    }

    pub fn insert(&mut self, envelope: Envelope, folder_hash: FolderHash) -> &Envelope {
        let hash = envelope.hash();
        self.message_ids
            .insert(envelope.message_id().raw().to_vec(), hash);
        self.envelopes.insert(hash, envelope);
        self.threads
            .entry(folder_hash)
            .or_default()
            .insert_reply(&mut self.envelopes, hash);
        &self.envelopes[&hash]
    }
    pub fn insert_reply(&mut self, env_hash: EnvelopeHash) {
        for (_, t) in self.threads.iter_mut() {
            t.insert_reply(&mut self.envelopes, env_hash);
        }
    }
}

impl Deref for Collection {
    type Target = FnvHashMap<EnvelopeHash, Envelope>;

    fn deref(&self) -> &FnvHashMap<EnvelopeHash, Envelope> {
        &self.envelopes
    }
}

impl DerefMut for Collection {
    fn deref_mut(&mut self) -> &mut FnvHashMap<EnvelopeHash, Envelope> {
        &mut self.envelopes
    }
}
