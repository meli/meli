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

impl From<ModSequence> for std::num::NonZeroU64 {
    #[inline]
    fn from(m: ModSequence) -> Self {
        m.0
    }
}

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
        write!(fmt, "{}", self.0)
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
#[inline(always)]
pub fn ignore_not_found(err: Error) -> Result<()> {
    if matches!(err.kind, ErrorKind::NotFound) {
        return Ok(());
    }
    Err(err)
}

pub trait ImapCache: Send + std::fmt::Debug {
    fn reset(&mut self) -> Result<()>;
    fn mailbox_state(&mut self, mailbox_hash: MailboxHash) -> Result<Option<CachedState>>;

    fn lastseenuid(&mut self, mailbox_hash: MailboxHash) -> Result<Option<UID>>;

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
        lastseenuid: UID,
        batch_size: usize,
    ) -> Result<Option<Vec<EnvelopeHash>>>;

    fn init_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()>;

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

    fn lastseenuid(&mut self, mailbox_hash: MailboxHash) -> Result<Option<UID>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.lastseenuid(mailbox_hash);
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
            cache_handle.insert_envelopes(mailbox_hash, fetches)?;
        }
        let mut env_lck = self.envelopes.lock().unwrap();
        let mut hash_index_lck = self.hash_index.lock().unwrap();
        let mut uid_index_lck = self.uid_index.lock().unwrap();
        let mut msn_index_lck = self.msn_index.lock().unwrap();
        for item in fetches {
            if let FetchResponse {
                uid: Some(uid),
                message_sequence_number,
                modseq,
                flags: _,
                body: _,
                references: _,
                envelope: Some(env),
                raw_fetch_value: _,
                bodystructure: _,
            } = item
            {
                let uid = *uid;
                let modseq = *modseq;
                msn_index_lck
                    .entry(mailbox_hash)
                    .or_default()
                    .insert(message_sequence_number.saturating_sub(1), uid);
                hash_index_lck.insert(env.hash(), (uid, mailbox_hash));
                uid_index_lck.insert((mailbox_hash, uid), env.hash());
                env_lck.insert(
                    env.hash(),
                    CachedEnvelope {
                        inner: env.clone(),
                        uid,
                        mailbox_hash,
                        modsequence: modseq,
                    },
                );
            }
        }
        Ok(())
    }

    fn envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        lastseenuid: UID,
        batch_size: usize,
    ) -> Result<Option<Vec<EnvelopeHash>>> {
        if !self.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut mutex = self.offline_cache.lock().unwrap();
        self.init_cache(&mut mutex)?;

        if let Some(ref mut cache_handle) = *mutex {
            return cache_handle.envelopes(mailbox_hash, lastseenuid, batch_size);
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
