use super::*;
use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};

extern crate fnv;
use self::fnv::FnvHashMap;

/// `Mailbox` represents a folder of mail.
#[derive(Debug, Clone, Default)]
pub struct Collection {
    pub envelopes: FnvHashMap<EnvelopeHash, Envelope>,
    date_index: BTreeMap<UnixTimestamp, EnvelopeHash>,
    subject_index: Option<BTreeMap<String, EnvelopeHash>>,
    pub threads: Threads,
}

impl Collection {
    pub fn new(vec: Vec<Envelope>) -> Collection {
        let mut envelopes: FnvHashMap<EnvelopeHash, Envelope> =
            FnvHashMap::with_capacity_and_hasher(vec.len(), Default::default());
        for e in vec {
            envelopes.insert(e.hash(), e);
        }
        let date_index = BTreeMap::new();
        let subject_index = None;

        let threads = Threads::new(&mut envelopes); // sent_folder);
        Collection {
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

    pub fn insert(&mut self, hash: EnvelopeHash, mut envelope: Envelope) {
        self.threads.insert(&mut envelope);
        self.envelopes.insert(hash, envelope);
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
