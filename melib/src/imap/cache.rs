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

use std::convert::TryFrom;

use super::*;
use crate::{
    backends::MailboxHash,
    email::{Envelope, EnvelopeHash},
    error::*,
};

pub mod ram_cache;
#[cfg(feature = "sqlite3")]
pub mod sqlite3_cache;
pub mod sync;

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

pub trait ImapCache: Send + std::fmt::Debug {
    fn reset(&mut self) -> Result<()>;
    fn mailbox_state(&mut self, mailbox_hash: MailboxHash) -> Result<Option<()>>;

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

    fn envelopes(&mut self, mailbox_hash: MailboxHash) -> Result<Option<Vec<EnvelopeHash>>>;

    fn clear(&mut self, mailbox_hash: MailboxHash, select_response: &SelectResponse) -> Result<()>;

    fn rfc822(
        &mut self,
        identifier: std::result::Result<UID, EnvelopeHash>,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<Vec<u8>>>;

    fn update_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> Result<()>;
}

pub trait ImapCacheReset: Send + std::fmt::Debug {
    fn reset_db(uid_store: &UIDStore) -> Result<()>
    where
        Self: Sized;
}

pub(super) async fn fetch_cached_envs(state: &mut FetchState) -> Result<Option<Vec<Envelope>>> {
    let FetchState {
        stage: _,
        ref mut connection,
        mailbox_hash,
        ref uid_store,
        cache_handle: _,
    } = state;
    let mailbox_hash = *mailbox_hash;
    if !*uid_store.keep_offline_cache.lock().unwrap() {
        return Ok(None);
    }
    {
        let mut conn = connection.lock().await;
        match conn.load_cache(mailbox_hash).await {
            None => Ok(None),
            Some(Ok(env_hashes)) => {
                let env_lck = uid_store.envelopes.lock().unwrap();
                Ok(Some(
                    env_hashes
                        .into_iter()
                        .filter_map(|env_hash| {
                            env_lck.get(&env_hash).map(|c_env| c_env.inner.clone())
                        })
                        .collect::<Vec<Envelope>>(),
                ))
            }
            Some(Err(err)) => Err(err),
        }
    }
}
