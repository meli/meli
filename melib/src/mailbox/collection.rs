extern crate bincode;
extern crate xdg;

use super::*;
use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::ops::{Deref, DerefMut};

extern crate fnv;
use self::fnv::FnvHashMap;

/// `Mailbox` represents a folder of mail.
#[derive(Debug, Clone, Default)]
pub struct Collection {
    folder: Folder,
    pub envelopes: FnvHashMap<EnvelopeHash, Envelope>,
    date_index: BTreeMap<UnixTimestamp, EnvelopeHash>,
    subject_index: Option<BTreeMap<String, EnvelopeHash>>,
    pub threads: Threads,
}

impl Drop for Collection {
    fn drop(&mut self) {
        let cache_dir =
            xdg::BaseDirectories::with_profile("meli", format!("{}_Thread", self.folder.hash()))
                .unwrap();
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
    pub fn new(vec: Vec<Envelope>, folder: &Folder) -> Collection {
        let mut envelopes: FnvHashMap<EnvelopeHash, Envelope> =
            FnvHashMap::with_capacity_and_hasher(vec.len(), Default::default());
        for e in vec {
            envelopes.insert(e.hash(), e);
        }
        let date_index = BTreeMap::new();
        let subject_index = None;

        /* Scrap caching for now. When a cached threads file is loaded, we must remove/rehash the
         * thread nodes that shouldn't exist anymore (e.g. because their file moved from /new to
         * /cur, or it was deleted).
         */
        let threads = Threads::new(&mut envelopes);

        /*let cache_dir =
                    xdg::BaseDirectories::with_profile("meli", format!("{}_Thread", folder.hash()))
                        .unwrap();
                if let Some(cached) = cache_dir.find_cache_file("threads") {
                    let reader = io::BufReader::new(fs::File::open(cached).unwrap());
                    let result: result::Result<Threads, _> = bincode::deserialize_from(reader);
                    let ret = if let Ok(mut cached_t) = result {
        use std::iter::FromIterator;
                        eprintln!("loaded cache, our hash set is {:?}\n and the cached one is {:?}", FnvHashSet::from_iter(envelopes.keys().cloned()), cached_t.hash_set);
                        cached_t.amend(&mut envelopes);
                        cached_t
                    } else {
                        Threads::new(&mut envelopes)
                    };
                    ret
                } else {
                    Threads::new(&mut envelopes)
                };
                */

        Collection {
            folder: folder.clone(),
            envelopes,
            date_index,
            subject_index,
            threads,
        }
    }

    pub fn len(&self) -> usize {
        self.envelopes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.envelopes.is_empty()
    }

    pub fn remove(&mut self, envelope_hash: EnvelopeHash) {
        eprintln!("DEBUG: Removing {}", envelope_hash);
        self.envelopes.remove(&envelope_hash);
        self.threads.remove(envelope_hash, &mut self.envelopes);
    }

    pub fn rename(&mut self, old_hash: EnvelopeHash, new_hash: EnvelopeHash) {
        let mut env = self.envelopes.remove(&old_hash).unwrap();
        env.set_hash(new_hash);
        self.envelopes.insert(new_hash, env);
        {
            if self.threads.update_envelope(old_hash, new_hash).is_ok() {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        let env = self.envelopes.entry(new_hash).or_default() as *mut Envelope;
        unsafe {
            self.threads.insert(&mut (*env), &self.envelopes);
        }
    }

    pub fn update_envelope(&mut self, old_hash: EnvelopeHash, envelope: Envelope) {
        self.envelopes.remove(&old_hash);
        let new_hash = envelope.hash();
        self.envelopes.insert(new_hash, envelope);
        {
            if self.threads.update_envelope(old_hash, new_hash).is_ok() {
                return;
            }
        }
        /* envelope is not in threads, so insert it */
        let env = self.envelopes.entry(new_hash).or_default() as *mut Envelope;
        unsafe {
            self.threads.insert(&mut (*env), &self.envelopes);
        }
    }

    pub fn insert(&mut self, envelope: Envelope) {
        let hash = envelope.hash();
        eprintln!("DEBUG: Inserting hash {} in {}", hash, self.folder.name());
        self.envelopes.insert(hash, envelope);
        let env = self.envelopes.entry(hash).or_default() as *mut Envelope;
        unsafe {
            self.threads.insert(&mut (*env), &self.envelopes);
        }
    }
    pub(crate) fn insert_reply(&mut self, _envelope: &Envelope) {
        return;
        /*
        //self.insert(envelope);
        eprintln!("insert_reply in collections");
        self.threads.insert_reply(envelope, &mut self.envelopes);
        */
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
