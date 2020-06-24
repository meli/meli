/*
 * meli - imap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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
use crate::backends::{
    BackendMailbox, Mailbox, MailboxHash, MailboxPermissions, SpecialUsageMailbox,
};
use crate::email::EnvelopeHash;
use crate::error::*;
use std::collections::BTreeSet;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug, Default, Clone)]
pub struct LazyCountSet {
    not_yet_seen: usize,
    set: BTreeSet<EnvelopeHash>,
}

impl LazyCountSet {
    pub fn set_not_yet_seen(&mut self, new_val: usize) {
        self.not_yet_seen = new_val;
    }

    pub fn insert_existing(&mut self, new_val: EnvelopeHash) -> bool {
        if self.not_yet_seen == 0 {
            false
        } else {
            self.not_yet_seen -= 1;
            self.set.insert(new_val);
            true
        }
    }

    pub fn insert_existing_set(&mut self, set: BTreeSet<EnvelopeHash>) -> bool {
        debug!("insert_existing_set {:?}", &set);
        if self.not_yet_seen < set.len() {
            false
        } else {
            self.not_yet_seen -= set.len();
            self.set.extend(set.into_iter());
            true
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.set.len() + self.not_yet_seen
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.set.clear();
        self.not_yet_seen = 0;
    }

    pub fn insert_new(&mut self, new_val: EnvelopeHash) {
        self.set.insert(new_val);
    }

    pub fn insert_set(&mut self, set: BTreeSet<EnvelopeHash>) {
        debug!("insert__set {:?}", &set);
        self.set.extend(set.into_iter());
    }

    pub fn remove(&mut self, new_val: EnvelopeHash) -> bool {
        self.set.remove(&new_val)
    }
}

#[test]
fn test_lazy_count_set() {
    let mut new = LazyCountSet::default();
    new.set_not_yet_seen(10);
    for i in 0..10 {
        assert!(new.insert_existing(i));
    }
    assert!(!new.insert_existing(10));
}

#[derive(Debug, Default, Clone)]
pub struct ImapMailbox {
    pub(super) hash: MailboxHash,
    pub(super) imap_path: String,
    pub(super) path: String,
    pub(super) name: String,
    pub(super) parent: Option<MailboxHash>,
    pub(super) children: Vec<MailboxHash>,
    pub separator: u8,
    pub usage: Arc<RwLock<SpecialUsageMailbox>>,
    pub no_select: bool,
    pub is_subscribed: bool,

    pub permissions: Arc<Mutex<MailboxPermissions>>,
    pub exists: Arc<Mutex<LazyCountSet>>,
    pub unseen: Arc<Mutex<LazyCountSet>>,
}

impl ImapMailbox {
    pub fn imap_path(&self) -> &str {
        &self.imap_path
    }
}

impl BackendMailbox for ImapMailbox {
    fn hash(&self) -> MailboxHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &str {
        &self.path
    }

    fn change_name(&mut self, s: &str) {
        self.name = s.to_string();
    }

    fn children(&self) -> &[MailboxHash] {
        &self.children
    }

    fn clone(&self) -> Mailbox {
        Box::new(std::clone::Clone::clone(self))
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        *self.usage.read().unwrap()
    }

    fn parent(&self) -> Option<MailboxHash> {
        self.parent
    }

    fn permissions(&self) -> MailboxPermissions {
        *self.permissions.lock().unwrap()
    }
    fn is_subscribed(&self) -> bool {
        self.is_subscribed
    }
    fn set_is_subscribed(&mut self, new_val: bool) -> Result<()> {
        self.is_subscribed = new_val;
        Ok(())
    }

    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()> {
        *self.usage.write()? = new_val;
        Ok(())
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((self.unseen.lock()?.len(), self.exists.lock()?.len()))
    }
}
