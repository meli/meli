/*
 * meli - nntp module.
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

use std::sync::{Arc, Mutex};

use crate::{
    backends::{
        BackendMailbox, LazyCountSet, Mailbox, MailboxHash, MailboxPermissions, SpecialUsageMailbox,
    },
    error::*,
    UnixTimestamp,
};

#[derive(Clone, Debug, Default)]
pub struct NntpMailbox {
    pub(super) hash: MailboxHash,
    pub(super) nntp_path: String,

    pub high_watermark: Arc<Mutex<usize>>,
    pub low_watermark: Arc<Mutex<usize>>,

    pub exists: Arc<Mutex<LazyCountSet>>,
    pub unseen: Arc<Mutex<LazyCountSet>>,

    pub latest_article: Arc<Mutex<Option<UnixTimestamp>>>,
}

impl NntpMailbox {
    pub fn nntp_path(&self) -> &str {
        &self.nntp_path
    }
}

impl BackendMailbox for NntpMailbox {
    fn hash(&self) -> MailboxHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.nntp_path
    }

    fn path(&self) -> &str {
        &self.nntp_path
    }

    fn children(&self) -> &[MailboxHash] {
        &[]
    }

    fn clone(&self) -> Mailbox {
        Box::new(std::clone::Clone::clone(self))
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        SpecialUsageMailbox::default()
    }

    fn parent(&self) -> Option<MailboxHash> {
        None
    }

    fn permissions(&self) -> MailboxPermissions {
        MailboxPermissions::default()
    }

    fn is_subscribed(&self) -> bool {
        true
    }

    fn set_is_subscribed(&mut self, _new_val: bool) -> Result<()> {
        Err(Error::new("Cannot set subscription in NNTP."))
    }

    fn set_special_usage(&mut self, _new_val: SpecialUsageMailbox) -> Result<()> {
        Err(Error::new("Cannot set special usage in NNTP."))
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((self.unseen.lock()?.len(), self.exists.lock()?.len()))
    }
}
