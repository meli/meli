/*
 * meli - notmuch backend
 *
 * Copyright 2019 - 2023 Manos Pitsidianakis
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

use std::sync::{Arc, Mutex, RwLock};

use crate::{
    backends::{BackendMailbox, Mailbox, MailboxHash, MailboxPermissions, SpecialUsageMailbox},
    error::Result,
    notmuch::{DbConnection, Query},
};

#[derive(Clone, Debug, Default)]
pub struct NotmuchMailbox {
    pub(super) hash: MailboxHash,
    pub(super) children: Vec<MailboxHash>,
    pub(super) parent: Option<MailboxHash>,
    pub(super) name: String,
    pub(super) path: String,
    pub(super) query_str: String,
    pub(super) usage: Arc<RwLock<SpecialUsageMailbox>>,
    pub(super) total: Arc<Mutex<usize>>,
    pub(super) unseen: Arc<Mutex<usize>>,
}

impl NotmuchMailbox {
    /// Get the actual notmuch query used to build this mailbox.
    pub fn query_value(&self) -> &str {
        &self.query_str
    }

    /// Query the database to update total and unread message counts.
    pub fn update_counts(&self, database: &DbConnection) -> Result<()> {
        *self.total.lock().unwrap() = Query::new(database, &self.query_str)?.count()? as usize;
        *self.unseen.lock().unwrap() =
            Query::new(database, &format!("{} tag:unread", self.query_str))?.count()? as usize;
        Ok(())
    }
}

impl BackendMailbox for NotmuchMailbox {
    fn hash(&self) -> MailboxHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &str {
        &self.path
    }

    fn clone(&self) -> Mailbox {
        Box::new(Clone::clone(self))
    }

    fn children(&self) -> &[MailboxHash] {
        &self.children
    }

    fn parent(&self) -> Option<MailboxHash> {
        self.parent
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        *self.usage.read().unwrap()
    }

    fn permissions(&self) -> MailboxPermissions {
        MailboxPermissions::default()
    }

    fn is_subscribed(&self) -> bool {
        true
    }

    fn set_is_subscribed(&mut self, _new_val: bool) -> Result<()> {
        Ok(())
    }

    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()> {
        *self.usage.write()? = new_val;
        Ok(())
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((*self.unseen.lock()?, *self.total.lock()?))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}
