/*
 * meli - imap melib
 *
 * Copyright 2020 Manos Pitsidianakis
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

use std::{convert::TryFrom, path::Path};

use super::*;
use crate::{
    backends::MailboxHash,
    email::{Envelope, EnvelopeHash},
    error::*,
};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ModSequence(pub std::num::NonZeroU64);

impl TryFrom<i64> for ModSequence {
    type Error = ();
    fn try_from(val: i64) -> std::result::Result<Self, ()> {
        std::num::NonZeroU64::new(val as u64)
            .map(|u| Ok(Self(u)))
            .unwrap_or(Err(()))
    }
}

impl std::fmt::Display for ModSequence {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", &self.0)
    }
}

#[derive(Debug)]
pub struct CachedEnvelope {
    pub inner: Envelope,
    pub uid: UID,
    pub mailbox_hash: MailboxHash,
    pub modsequence: Option<ModSequence>,
}

#[derive(Clone, Copy, Debug)]
pub struct CachedState {
    pub uidvalidity: UID,
    pub highestmodseq: Option<ModSequence>,
}

/// Helper function for ignoring cache misses with
/// `.or_else(ignore_not_found)?`.
pub fn ignore_not_found(err: Error) -> Result<()> {
    if matches!(err.kind, ErrorKind::NotFound) {
        return Ok(());
    }
    Err(err)
}

pub trait ImapCache: Send + std::fmt::Debug {
    fn reset(&mut self) -> Result<()>;
    fn mailbox_state(&mut self, mailbox_hash: MailboxHash) -> Result<Option<CachedState>>;

    fn max_uid(&mut self, mailbox_hash: MailboxHash) -> Result<Option<UID>>;

    fn find_envelope(
        &mut self,
        identifier: std::result::Result<UID, EnvelopeHash>,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<CachedEnvelope>>;

    fn update(
        &mut self,
        mailbox_hash: MailboxHash,
        refresh_events: &[(UID, RefreshEvent)],
    ) -> Result<()>;

    fn update_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()>;

    fn insert_envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        fetches: &[FetchResponse<'_>],
    ) -> Result<()>;

    fn envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        max_uid: UID,
        batch_size: usize,
    ) -> Result<Option<Vec<EnvelopeHash>>>;

    fn init_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()>;

    fn rfc822(
        &mut self,
        identifier: std::result::Result<UID, EnvelopeHash>,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<Vec<u8>>>;

    fn update_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: Vec<FlagOp>,
    ) -> Result<()>;
}

pub trait ImapCacheReset: Send + std::fmt::Debug {
    fn reset_db(uid_store: &UIDStore, data_dir: Option<&Path>) -> Result<()>
    where
        Self: Sized;
}

impl ImapCache for Arc<UIDStore> {
    fn reset(&mut self) -> Result<()> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(());
        }
        #[cfg(feature = "sqlite3")]
        {
            sync::sqlite3_cache::Sqlite3Cache::reset_db(self, None)?;
        }
        Ok(())
    }

    fn mailbox_state(&mut self, mailbox_hash: MailboxHash) -> Result<Option<CachedState>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.mailbox_state(mailbox_hash);
        }
        Ok(None)
    }

    fn max_uid(&mut self, mailbox_hash: MailboxHash) -> Result<Option<UID>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.max_uid(mailbox_hash);
        }
        Ok(None)
    }

    fn find_envelope(
        &mut self,
        identifier: std::result::Result<UID, EnvelopeHash>,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<CachedEnvelope>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.find_envelope(identifier, mailbox_hash);
        }
        Ok(None)
    }

    fn update(
        &mut self,
        mailbox_hash: MailboxHash,
        refresh_events: &[(UID, RefreshEvent)],
    ) -> Result<()> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(());
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.update(mailbox_hash, refresh_events);
        }
        Ok(())
    }

    fn update_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(());
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.update_mailbox(mailbox_hash, select_response);
        }
        Ok(())
    }

    fn insert_envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        fetches: &[FetchResponse<'_>],
    ) -> Result<()> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(());
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.insert_envelopes(mailbox_hash, fetches);
        }
        Ok(())
    }

    fn envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        max_uid: UID,
        batch_size: usize,
    ) -> Result<Option<Vec<EnvelopeHash>>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.envelopes(mailbox_hash, max_uid, batch_size);
        }
        Ok(None)
    }

    fn init_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(());
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.init_mailbox(mailbox_hash, select_response);
        }
        Ok(())
    }

    fn rfc822(
        &mut self,
        identifier: std::result::Result<UID, EnvelopeHash>,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<Vec<u8>>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.rfc822(identifier, mailbox_hash);
        }
        Ok(None)
    }

    fn update_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: Vec<FlagOp>,
    ) -> Result<()> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(());
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.update_flags(env_hashes, mailbox_hash, flags);
        }
        Ok(())
    }
}
