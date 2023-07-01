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

use super::*;
mod sync;
use std::convert::TryFrom;

use crate::{
    backends::MailboxHash,
    email::{Envelope, EnvelopeHash},
    error::*,
};

#[derive(Debug, PartialEq, Hash, Eq, Ord, PartialOrd, Copy, Clone)]
pub struct ModSequence(pub std::num::NonZeroU64);

impl TryFrom<i64> for ModSequence {
    type Error = ();
    fn try_from(val: i64) -> std::result::Result<Self, ()> {
        std::num::NonZeroU64::new(val as u64)
            .map(|u| Ok(Self(u)))
            .unwrap_or(Err(()))
    }
}

impl core::fmt::Display for ModSequence {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
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

pub trait ImapCache: Send + core::fmt::Debug {
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
}

pub trait ImapCacheReset: Send + core::fmt::Debug {
    fn reset_db(uid_store: &UIDStore) -> Result<()>
    where
        Self: Sized;
}

#[cfg(feature = "sqlite3")]
pub use sqlite3_m::*;

#[cfg(feature = "sqlite3")]
mod sqlite3_m {
    use super::*;
    use crate::utils::sqlite3::{
        self,
        rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput},
        Connection, DatabaseDescription,
    };

    type Sqlite3UID = i32;

    #[derive(Debug)]
    pub struct Sqlite3Cache {
        connection: Connection,
        loaded_mailboxes: BTreeSet<MailboxHash>,
        uid_store: Arc<UIDStore>,
    }

    const DB_DESCRIPTION: DatabaseDescription = DatabaseDescription {
        name: "header_cache.db",
        init_script: Some(
            "PRAGMA foreign_keys = true;
    PRAGMA encoding = 'UTF-8';

    CREATE TABLE IF NOT EXISTS envelopes (
                    hash             INTEGER NOT NULL,
                    mailbox_hash     INTEGER NOT NULL,
                    uid              INTEGER NOT NULL,
                    modsequence      INTEGER,
                    rfc822           BLOB,
                    envelope         BLOB NOT NULL,
                    PRIMARY KEY (mailbox_hash, uid),
                    FOREIGN KEY (mailbox_hash) REFERENCES mailbox(mailbox_hash) ON DELETE CASCADE
                   );
    CREATE TABLE IF NOT EXISTS mailbox (
                mailbox_hash     INTEGER UNIQUE,
                uidvalidity      INTEGER,
                flags            BLOB NOT NULL,
                highestmodseq    INTEGER,
                PRIMARY KEY (mailbox_hash)
               );
    CREATE INDEX IF NOT EXISTS envelope_uid_idx ON envelopes(mailbox_hash, uid);
    CREATE INDEX IF NOT EXISTS envelope_idx ON envelopes(hash);
    CREATE INDEX IF NOT EXISTS mailbox_idx ON mailbox(mailbox_hash);",
        ),
        version: 3,
    };

    impl ToSql for ModSequence {
        fn to_sql(&self) -> rusqlite::Result<ToSqlOutput> {
            Ok(ToSqlOutput::from(self.0.get() as i64))
        }
    }

    impl FromSql for ModSequence {
        fn column_result(value: rusqlite::types::ValueRef) -> FromSqlResult<Self> {
            let i: i64 = FromSql::column_result(value)?;
            if i == 0 {
                return Err(FromSqlError::OutOfRange(0));
            }
            Ok(Self::try_from(i).unwrap())
        }
    }

    impl Sqlite3Cache {
        pub fn get(uid_store: Arc<UIDStore>) -> Result<Box<dyn ImapCache>> {
            Ok(Box::new(Self {
                connection: sqlite3::open_or_create_db(
                    &DB_DESCRIPTION,
                    Some(&uid_store.account_name),
                )?,
                loaded_mailboxes: BTreeSet::default(),
                uid_store,
            }))
        }

        fn max_uid(&self, mailbox_hash: MailboxHash) -> Result<UID> {
            let mut stmt = self
                .connection
                .prepare("SELECT MAX(uid) FROM envelopes WHERE mailbox_hash = ?1;")?;

            let mut ret: Vec<UID> = stmt
                .query_map(sqlite3::params![mailbox_hash], |row| {
                    row.get(0).map(|i: Sqlite3UID| i as UID)
                })?
                .collect::<std::result::Result<_, _>>()?;
            Ok(ret.pop().unwrap_or(0))
        }
    }

    impl ImapCacheReset for Sqlite3Cache {
        fn reset_db(uid_store: &UIDStore) -> Result<()> {
            sqlite3::reset_db(&DB_DESCRIPTION, Some(&uid_store.account_name))
        }
    }

    impl ImapCache for Sqlite3Cache {
        fn reset(&mut self) -> Result<()> {
            Self::reset_db(&self.uid_store)
        }

        fn mailbox_state(&mut self, mailbox_hash: MailboxHash) -> Result<Option<()>> {
            if self.loaded_mailboxes.contains(&mailbox_hash) {
                return Ok(Some(()));
            }
            debug!("loading mailbox state {} from cache", mailbox_hash);
            let mut stmt = self.connection.prepare(
                "SELECT uidvalidity, flags, highestmodseq FROM mailbox WHERE mailbox_hash = ?1;",
            )?;

            let mut ret = stmt.query_map(sqlite3::params![mailbox_hash], |row| {
                Ok((
                    row.get(0).map(|u: Sqlite3UID| u as UID)?,
                    row.get(1)?,
                    row.get(2)?,
                ))
            })?;
            if let Some(v) = ret.next() {
                let (uidvalidity, flags, highestmodseq): (
                    UIDVALIDITY,
                    Vec<u8>,
                    Option<ModSequence>,
                ) = v?;
                debug!(
                    "mailbox state {} in cache uidvalidity {}",
                    mailbox_hash, uidvalidity
                );
                debug!(
                    "mailbox state {} in cache highestmodseq {:?}",
                    mailbox_hash, &highestmodseq
                );
                debug!(
                    "mailbox state {} inserting flags: {:?}",
                    mailbox_hash,
                    to_str!(&flags)
                );
                self.uid_store
                    .highestmodseqs
                    .lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .and_modify(|entry| *entry = highestmodseq.ok_or(()))
                    .or_insert_with(|| highestmodseq.ok_or(()));
                self.uid_store
                    .uidvalidity
                    .lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .and_modify(|entry| *entry = uidvalidity)
                    .or_insert(uidvalidity);
                let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
                for f in to_str!(&flags).split('\0') {
                    let hash = TagHash::from_bytes(f.as_bytes());
                    tag_lck.entry(hash).or_insert_with(|| f.to_string());
                }
                self.loaded_mailboxes.insert(mailbox_hash);
                Ok(Some(()))
            } else {
                debug!("mailbox state {} not in cache", mailbox_hash);
                Ok(None)
            }
        }

        fn clear(
            &mut self,
            mailbox_hash: MailboxHash,
            select_response: &SelectResponse,
        ) -> Result<()> {
            debug!("clear mailbox_hash {} {:?}", mailbox_hash, select_response);
            self.loaded_mailboxes.remove(&mailbox_hash);
            self.connection
                .execute(
                    "DELETE FROM mailbox WHERE mailbox_hash = ?1",
                    sqlite3::params![mailbox_hash],
                )
                .chain_err_summary(|| {
                    format!(
                        "Could not clear cache of mailbox {} account {}",
                        mailbox_hash, self.uid_store.account_name
                    )
                })?;

            if let Some(Ok(highestmodseq)) = select_response.highestmodseq {
                self.connection
                    .execute(
                        "INSERT OR IGNORE INTO mailbox (uidvalidity, flags, highestmodseq, \
                         mailbox_hash) VALUES (?1, ?2, ?3, ?4)",
                        sqlite3::params![
                            select_response.uidvalidity as Sqlite3UID,
                            select_response
                                .flags
                                .1
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Vec<&str>>()
                                .join("\0")
                                .as_bytes(),
                            highestmodseq,
                            mailbox_hash
                        ],
                    )
                    .chain_err_summary(|| {
                        format!(
                            "Could not insert uidvalidity {} in header_cache of account {}",
                            select_response.uidvalidity, self.uid_store.account_name
                        )
                    })?;
            } else {
                self.connection
                    .execute(
                        "INSERT OR IGNORE INTO mailbox (uidvalidity, flags, mailbox_hash) VALUES \
                         (?1, ?2, ?3)",
                        sqlite3::params![
                            select_response.uidvalidity as Sqlite3UID,
                            select_response
                                .flags
                                .1
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Vec<&str>>()
                                .join("\0")
                                .as_bytes(),
                            mailbox_hash
                        ],
                    )
                    .chain_err_summary(|| {
                        format!(
                            "Could not insert mailbox {} in header_cache of account {}",
                            select_response.uidvalidity, self.uid_store.account_name
                        )
                    })?;
            }
            Ok(())
        }

        fn update_mailbox(
            &mut self,
            mailbox_hash: MailboxHash,
            select_response: &SelectResponse,
        ) -> Result<()> {
            if self.mailbox_state(mailbox_hash)?.is_none() {
                return self.clear(mailbox_hash, select_response);
            }

            if let Some(Ok(highestmodseq)) = select_response.highestmodseq {
                self.connection
                    .execute(
                        "UPDATE mailbox SET flags=?1, highestmodseq =?2 where mailbox_hash = ?3;",
                        sqlite3::params![
                            select_response
                                .flags
                                .1
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Vec<&str>>()
                                .join("\0")
                                .as_bytes(),
                            highestmodseq,
                            mailbox_hash
                        ],
                    )
                    .chain_err_summary(|| {
                        format!(
                            "Could not update mailbox {} in header_cache of account {}",
                            mailbox_hash, self.uid_store.account_name
                        )
                    })?;
            } else {
                self.connection
                    .execute(
                        "UPDATE mailbox SET flags=?1 where mailbox_hash = ?2;",
                        sqlite3::params![
                            select_response
                                .flags
                                .1
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Vec<&str>>()
                                .join("\0")
                                .as_bytes(),
                            mailbox_hash
                        ],
                    )
                    .chain_err_summary(|| {
                        format!(
                            "Could not update mailbox {} in header_cache of account {}",
                            mailbox_hash, self.uid_store.account_name
                        )
                    })?;
            }
            Ok(())
        }

        fn envelopes(&mut self, mailbox_hash: MailboxHash) -> Result<Option<Vec<EnvelopeHash>>> {
            debug!("envelopes mailbox_hash {}", mailbox_hash);
            if self.mailbox_state(mailbox_hash)?.is_none() {
                return Ok(None);
            }

            let ret: Vec<(UID, Envelope, Option<ModSequence>)> = match {
                let mut stmt = self.connection.prepare(
                    "SELECT uid, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1;",
                )?;

                #[allow(clippy::let_and_return)] // false positive, the let binding is needed
                // for the temporary to live long enough
                let x = stmt
                    .query_map(sqlite3::params![mailbox_hash], |row| {
                        Ok((
                            row.get(0).map(|i: Sqlite3UID| i as UID)?,
                            row.get(1)?,
                            row.get(2)?,
                        ))
                    })?
                    .collect::<std::result::Result<_, _>>();
                x
            } {
                Err(err) if matches!(&err, rusqlite::Error::FromSqlConversionFailure(_, _, _)) => {
                    drop(err);
                    self.reset()?;
                    return Ok(None);
                }
                Err(err) => return Err(err.into()),
                Ok(v) => v,
            };
            let mut max_uid = 0;
            let mut env_lck = self.uid_store.envelopes.lock().unwrap();
            let mut hash_index_lck = self.uid_store.hash_index.lock().unwrap();
            let mut uid_index_lck = self.uid_store.uid_index.lock().unwrap();
            let mut env_hashes = Vec::with_capacity(ret.len());
            for (uid, env, modseq) in ret {
                env_hashes.push(env.hash());
                max_uid = std::cmp::max(max_uid, uid);
                hash_index_lck.insert(env.hash(), (uid, mailbox_hash));
                uid_index_lck.insert((mailbox_hash, uid), env.hash());
                env_lck.insert(
                    env.hash(),
                    CachedEnvelope {
                        inner: env,
                        uid,
                        mailbox_hash,
                        modsequence: modseq,
                    },
                );
            }
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, max_uid);
            Ok(Some(env_hashes))
        }

        fn insert_envelopes(
            &mut self,
            mailbox_hash: MailboxHash,
            fetches: &[FetchResponse<'_>],
        ) -> Result<()> {
            debug!(
                "insert_envelopes mailbox_hash {} len {}",
                mailbox_hash,
                fetches.len()
            );
            let mut max_uid = self
                .uid_store
                .max_uids
                .lock()
                .unwrap()
                .get(&mailbox_hash)
                .cloned()
                .unwrap_or_default();
            if self.mailbox_state(mailbox_hash)?.is_none() {
                return Err(Error::new("Mailbox is not in cache").set_kind(ErrorKind::Bug));
            }
            let Self {
                ref mut connection,
                ref uid_store,
                loaded_mailboxes: _,
            } = self;
            let tx = connection.transaction()?;
            for item in fetches {
                if let FetchResponse {
                    uid: Some(uid),
                    message_sequence_number: _,
                    modseq,
                    flags: _,
                    body: _,
                    references: _,
                    envelope: Some(envelope),
                    raw_fetch_value: _,
                } = item
                {
                    max_uid = std::cmp::max(max_uid, *uid);
                    tx.execute(
                        "INSERT OR REPLACE INTO envelopes (hash, uid, mailbox_hash, modsequence, \
                         envelope) VALUES (?1, ?2, ?3, ?4, ?5)",
                        sqlite3::params![
                            envelope.hash(),
                            *uid as Sqlite3UID,
                            mailbox_hash,
                            modseq,
                            &envelope
                        ],
                    )
                    .chain_err_summary(|| {
                        format!(
                            "Could not insert envelope {} {} in header_cache of account {}",
                            envelope.message_id(),
                            envelope.hash(),
                            uid_store.account_name
                        )
                    })?;
                }
            }
            tx.commit()?;
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, max_uid);
            Ok(())
        }

        fn update(
            &mut self,
            mailbox_hash: MailboxHash,
            refresh_events: &[(UID, RefreshEvent)],
        ) -> Result<()> {
            if self.mailbox_state(mailbox_hash)?.is_none() {
                return Err(Error::new("Mailbox is not in cache").set_kind(ErrorKind::Bug));
            }
            let Self {
                ref mut connection,
                ref uid_store,
                loaded_mailboxes: _,
            } = self;
            let tx = connection.transaction()?;
            let mut hash_index_lck = uid_store.hash_index.lock().unwrap();
            for (uid, event) in refresh_events {
                match &event.kind {
                    RefreshEventKind::Remove(env_hash) => {
                        hash_index_lck.remove(env_hash);
                        tx.execute(
                            "DELETE FROM envelopes WHERE mailbox_hash = ?1 AND uid = ?2;",
                            sqlite3::params![mailbox_hash, *uid as Sqlite3UID],
                        )
                        .chain_err_summary(|| {
                            format!(
                                "Could not remove envelope {} uid {} from  mailbox {} account {}",
                                env_hash, *uid, mailbox_hash, uid_store.account_name
                            )
                        })?;
                    }
                    RefreshEventKind::NewFlags(env_hash, (flags, tags)) => {
                        let mut stmt = tx.prepare(
                            "SELECT envelope FROM envelopes WHERE mailbox_hash = ?1 AND uid = ?2;",
                        )?;

                        let mut ret: Vec<Envelope> = stmt
                            .query_map(sqlite3::params![mailbox_hash, *uid as Sqlite3UID], |row| {
                                row.get(0)
                            })?
                            .collect::<std::result::Result<_, _>>()?;
                        if let Some(mut env) = ret.pop() {
                            env.set_flags(*flags);
                            env.tags_mut().clear();
                            env.tags_mut()
                                .extend(tags.iter().map(|t| TagHash::from_bytes(t.as_bytes())));
                            tx.execute(
                                "UPDATE envelopes SET envelope = ?1 WHERE mailbox_hash = ?2 AND \
                                 uid = ?3;",
                                sqlite3::params![&env, mailbox_hash, *uid as Sqlite3UID],
                            )
                            .chain_err_summary(|| {
                                format!(
                                    "Could not update envelope {} uid {} from  mailbox {} account \
                                     {}",
                                    env_hash, *uid, mailbox_hash, uid_store.account_name
                                )
                            })?;
                            uid_store
                                .envelopes
                                .lock()
                                .unwrap()
                                .entry(*env_hash)
                                .and_modify(|entry| {
                                    entry.inner = env;
                                });
                        }
                    }
                    _ => {}
                }
            }
            tx.commit()?;
            let new_max_uid = self.max_uid(mailbox_hash).unwrap_or(0);
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, new_max_uid);
            Ok(())
        }

        fn find_envelope(
            &mut self,
            identifier: std::result::Result<UID, EnvelopeHash>,
            mailbox_hash: MailboxHash,
        ) -> Result<Option<CachedEnvelope>> {
            let mut ret: Vec<(UID, Envelope, Option<ModSequence>)> = match identifier {
                Ok(uid) => {
                    let mut stmt = self.connection.prepare(
                        "SELECT uid, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1 \
                         AND uid = ?2;",
                    )?;

                    #[allow(clippy::let_and_return)] // false positive, the let binding is needed
                    // for the temporary to live long enough
                    let x = stmt
                        .query_map(sqlite3::params![mailbox_hash, uid as Sqlite3UID], |row| {
                            Ok((
                                row.get(0).map(|u: Sqlite3UID| u as UID)?,
                                row.get(1)?,
                                row.get(2)?,
                            ))
                        })?
                        .collect::<std::result::Result<_, _>>()?;
                    x
                }
                Err(env_hash) => {
                    let mut stmt = self.connection.prepare(
                        "SELECT uid, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1 \
                         AND hash = ?2;",
                    )?;

                    #[allow(clippy::let_and_return)] // false positive, the let binding is needed
                    // for the temporary to live long enough
                    let x = stmt
                        .query_map(sqlite3::params![mailbox_hash, env_hash], |row| {
                            Ok((
                                row.get(0).map(|u: Sqlite3UID| u as UID)?,
                                row.get(1)?,
                                row.get(2)?,
                            ))
                        })?
                        .collect::<std::result::Result<_, _>>()?;
                    x
                }
            };
            if ret.len() != 1 {
                return Ok(None);
            }
            let (uid, inner, modsequence) = ret.pop().unwrap();
            Ok(Some(CachedEnvelope {
                inner,
                uid,
                mailbox_hash,
                modsequence,
            }))
        }

        fn rfc822(
            &mut self,
            identifier: std::result::Result<UID, EnvelopeHash>,
            mailbox_hash: MailboxHash,
        ) -> Result<Option<Vec<u8>>> {
            let mut ret: Vec<Option<Vec<u8>>> = match identifier {
                Ok(uid) => {
                    let mut stmt = self.connection.prepare(
                        "SELECT rfc822 FROM envelopes WHERE mailbox_hash = ?1 AND uid = ?2;",
                    )?;
                    #[allow(clippy::let_and_return)] // false positive, the let binding is needed
                    // for the temporary to live long enough
                    let x = stmt
                        .query_map(sqlite3::params![mailbox_hash, uid as Sqlite3UID], |row| {
                            row.get(0)
                        })?
                        .collect::<std::result::Result<_, _>>()?;
                    x
                }
                Err(env_hash) => {
                    let mut stmt = self.connection.prepare(
                        "SELECT rfc822 FROM envelopes WHERE mailbox_hash = ?1 AND hash = ?2;",
                    )?;
                    #[allow(clippy::let_and_return)] // false positive, the let binding is needed
                    // for the temporary to live long enough
                    let x = stmt
                        .query_map(sqlite3::params![mailbox_hash, env_hash], |row| row.get(0))?
                        .collect::<std::result::Result<_, _>>()?;
                    x
                }
            };

            if ret.len() != 1 {
                return Ok(None);
            }
            Ok(ret.pop().unwrap())
        }
    }
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
    if !uid_store.keep_offline_cache {
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

#[cfg(not(feature = "sqlite3"))]
pub use default_m::*;

#[cfg(not(feature = "sqlite3"))]
mod default_m {
    use super::*;
    #[derive(Debug)]
    pub struct DefaultCache;

    impl DefaultCache {
        pub fn get(_uid_store: Arc<UIDStore>) -> Result<Box<dyn ImapCache>> {
            Ok(Box::new(Self))
        }
    }

    impl ImapCacheReset for DefaultCache {
        fn reset_db(uid_store: &UIDStore) -> Result<()> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }
    }

    impl ImapCache for DefaultCache {
        fn reset(&mut self) -> Result<()> {
            DefaultCache::reset_db(&self.uid_store)
        }

        fn mailbox_state(&mut self, _mailbox_hash: MailboxHash) -> Result<Option<()>> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn clear(
            &mut self,
            _mailbox_hash: MailboxHash,
            _select_response: &SelectResponse,
        ) -> Result<()> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn envelopes(&mut self, _mailbox_hash: MailboxHash) -> Result<Option<Vec<EnvelopeHash>>> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn insert_envelopes(
            &mut self,
            _mailbox_hash: MailboxHash,
            _fetches: &[FetchResponse<'_>],
        ) -> Result<()> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn update_mailbox(
            &mut self,
            _mailbox_hash: MailboxHash,
            _select_response: &SelectResponse,
        ) -> Result<()> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn update(
            &mut self,
            _mailbox_hash: MailboxHash,
            _refresh_events: &[(UID, RefreshEvent)],
        ) -> Result<()> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn find_envelope(
            &mut self,
            _identifier: std::result::Result<UID, EnvelopeHash>,
            _mailbox_hash: MailboxHash,
        ) -> Result<Option<CachedEnvelope>> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }

        fn rfc822(
            &mut self,
            _identifier: std::result::Result<UID, EnvelopeHash>,
            _mailbox_hash: MailboxHash,
        ) -> Result<Option<Vec<u8>>> {
            Err(Error::new("melib is not built with any imap cache").set_kind(ErrorKind::Bug))
        }
    }
}
