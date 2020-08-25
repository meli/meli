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
use crate::{
    backends::MailboxHash,
    email::{Envelope, EnvelopeHash},
    error::*,
};

use std::convert::TryFrom;

#[derive(Debug, PartialEq, Hash, Eq, Ord, PartialOrd, Copy, Clone)]
pub struct ModSequence(pub std::num::NonZeroU64);

impl TryFrom<i64> for ModSequence {
    type Error = ();
    fn try_from(val: i64) -> std::result::Result<ModSequence, ()> {
        std::num::NonZeroU64::new(val as u64)
            .map(|u| Ok(ModSequence(u)))
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
    pub mailbox_hash: MailboxHash,
    pub modsequence: Option<ModSequence>,
}

pub struct CacheHandle {
    #[cfg(feature = "sqlite3")]
    connection: crate::sqlite3::Connection,
    uid_store: Arc<UIDStore>,
}

#[cfg(feature = "sqlite3")]
pub use sqlite3_m::*;

#[cfg(feature = "sqlite3")]
mod sqlite3_m {
    use super::*;
    use crate::sqlite3::rusqlite::types::{
        FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput,
    };
    use crate::sqlite3::{self, DatabaseDescription};
    const DB_DESCRIPTION: DatabaseDescription = DatabaseDescription {
        name: "header_cache.db",
        init_script: Some("PRAGMA foreign_keys = true;
    PRAGMA encoding = 'UTF-8';

    CREATE TABLE IF NOT EXISTS envelopes (
                    mailbox_hash     INTEGER,
                    uid              INTEGER,
                    modsequence      INTEGER,
                    envelope         BLOB NOT NULL,
                    PRIMARY KEY (mailbox_hash, uid),
                    FOREIGN KEY (mailbox_hash) REFERENCES uidvalidity(mailbox_hash) ON DELETE CASCADE
                   );
    CREATE TABLE IF NOT EXISTS uidvalidity (
                uid              INTEGER UNIQUE,
                mailbox_hash     INTEGER UNIQUE,
                highestmodseq    INTEGER,
                PRIMARY KEY (mailbox_hash, uid)
               );
    CREATE INDEX IF NOT EXISTS envelope_idx ON envelopes(mailbox_hash);
    CREATE INDEX IF NOT EXISTS uidvalidity_idx ON uidvalidity(mailbox_hash);"),
    version: 1,
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
            Ok(ModSequence::try_from(i).unwrap())
        }
    }

    impl CacheHandle {
        pub fn get(uid_store: Arc<UIDStore>) -> Result<Self> {
            Ok(Self {
                connection: sqlite3::open_or_create_db(
                    &DB_DESCRIPTION,
                    Some(uid_store.account_name.as_str()),
                )?,
                uid_store,
            })
        }

        pub fn mailbox_state(
            &self,
            mailbox_hash: MailboxHash,
        ) -> Result<Option<(UID, Option<ModSequence>)>> {
            let mut stmt = self
                .connection
                .prepare("SELECT uid, highestmodseq FROM uidvalidity WHERE mailbox_hash = ?1;")?;

            let mut ret = stmt.query_map(sqlite3::params![mailbox_hash as i64], |row| {
                Ok((row.get(0).map(|u: i64| u as usize)?, row.get(1)?))
            })?;
            if let Some(row_res) = ret.next() {
                Ok(Some(row_res?))
            } else {
                Ok(None)
            }
        }

        pub fn clear(
            &self,
            mailbox_hash: MailboxHash,
            new_uidvalidity: UID,
            highestmodseq: Option<ModSequence>,
        ) -> Result<()> {
            debug!("clear mailbox_hash {}", mailbox_hash);
            debug!(new_uidvalidity);
            debug!(&highestmodseq);
            self.connection
                .execute(
                    "DELETE FROM uidvalidity WHERE mailbox_hash = ?1",
                    sqlite3::params![mailbox_hash as i64],
                )
                .chain_err_summary(|| {
                    format!(
                        "Could not clear cache of mailbox {} account {}",
                        mailbox_hash, self.uid_store.account_name
                    )
                })?;

            self.connection.execute(
                "INSERT OR IGNORE INTO uidvalidity (uid, highestmodseq, mailbox_hash) VALUES (?1, ?2, ?3)",
                sqlite3::params![new_uidvalidity as i64, highestmodseq, mailbox_hash as i64],
            )
            .chain_err_summary(|| {
                format!(
                    "Could not insert uidvalidity {} in header_cache of account {}",
                    new_uidvalidity, self.uid_store.account_name
                )
            })?;
            Ok(())
        }

        pub fn envelopes(&self, mailbox_hash: MailboxHash) -> Result<Option<Vec<EnvelopeHash>>> {
            debug!("envelopes mailbox_hash {}", mailbox_hash);
            if debug!(self.mailbox_state(mailbox_hash)?.is_none()) {
                return Ok(None);
            }

            let mut stmt = self.connection.prepare(
                "SELECT uid, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1;",
            )?;

            let ret: Vec<(UID, Envelope, Option<ModSequence>)> = stmt
                .query_map(sqlite3::params![mailbox_hash as i64], |row| {
                    Ok((
                        row.get(0).map(|i: i64| i as usize)?,
                        row.get(1)?,
                        row.get(2)?,
                    ))
                })?
                .collect::<std::result::Result<_, _>>()?;
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

        pub fn insert_envelopes(
            &mut self,
            mailbox_hash: MailboxHash,
            fetches: &[FetchResponse<'_>],
        ) -> Result<()> {
            debug!(
                "insert_envelopes mailbox_hash {} len {}",
                mailbox_hash,
                fetches.len()
            );
            if self.mailbox_state(mailbox_hash)?.is_none() {
                debug!(self.mailbox_state(mailbox_hash)?.is_none());
                let uidvalidity = self
                    .uid_store
                    .uidvalidity
                    .lock()
                    .unwrap()
                    .get(&mailbox_hash)
                    .cloned();
                let highestmodseq = self
                    .uid_store
                    .highestmodseqs
                    .lock()
                    .unwrap()
                    .get(&mailbox_hash)
                    .cloned();
                debug!(&uidvalidity);
                debug!(&highestmodseq);
                if let Some(uidvalidity) = uidvalidity {
                    debug!(self.clear(
                        mailbox_hash,
                        uidvalidity,
                        highestmodseq.and_then(|v| v.ok()),
                    ))?;
                }
            }
            let Self {
                ref mut connection,
                ref uid_store,
            } = self;
            let tx = connection.transaction()?;
            for item in fetches {
                if let FetchResponse {
                    uid: Some(uid),
                    message_sequence_number: _,
                    modseq,
                    flags: _,
                    body: _,
                    envelope: Some(envelope),
                } = item
                {
                    tx.execute(
                "INSERT OR REPLACE INTO envelopes (uid, mailbox_hash, modsequence, envelope) VALUES (?1, ?2, ?3, ?4)",
                sqlite3::params![*uid as i64, mailbox_hash as i64, modseq, &envelope],
            ).chain_err_summary(|| format!("Could not insert envelope {} {} in header_cache of account {}", envelope.message_id(), envelope.hash(), uid_store.account_name))?;
                }
            }
            tx.commit()?;
            Ok(())
        }
    }
}

#[cfg(not(feature = "sqlite3"))]
pub use filesystem_m::*;

#[cfg(not(feature = "sqlite3"))]
mod filesystem_m {
    use super::*;
    impl CacheHandle {
        pub fn get(uid_store: Arc<UIDStore>) -> Result<Self> {
            Ok(Self { uid_store })
        }

        pub fn mailbox_state(
            &self,
            _mailbox_hash: MailboxHash,
        ) -> Result<Option<(UID, Option<ModSequence>)>> {
            Ok(None)
        }

        pub fn clear(
            &self,
            _mailbox_hash: MailboxHash,
            _new_uidvalidity: UID,
            _highestmodseq: Option<ModSequence>,
        ) -> Result<()> {
            Ok(())
        }

        pub fn envelopes(&self, _mailbox_hash: MailboxHash) -> Result<Option<Vec<EnvelopeHash>>> {
            Ok(None)
        }

        pub fn insert_envelopes(
            &mut self,
            _mailbox_hash: MailboxHash,
            _fetches: &[FetchResponse<'_>],
        ) -> Result<()> {
            Ok(())
        }
    }
}

pub(super) async fn fetch_cached_envs(state: &mut FetchState) -> Result<Option<Vec<Envelope>>> {
    let FetchState {
        stage: _,
        ref mut connection,
        mailbox_hash,
        can_create_flags: _,
        ref uid_store,
    } = state;
    debug!(uid_store.keep_offline_cache);
    let mailbox_hash = *mailbox_hash;
    if !uid_store.keep_offline_cache {
        return Ok(None);
    }
    {
        let mut conn = connection.lock().await;
        match debug!(conn.load_cache(mailbox_hash).await) {
            None => return Ok(None),
            Some(Ok(env_hashes)) => {
                uid_store
                    .mailboxes
                    .lock()
                    .await
                    .entry(mailbox_hash)
                    .and_modify(|entry| {
                        entry
                            .exists
                            .lock()
                            .unwrap()
                            .insert_set(env_hashes.iter().cloned().collect());
                        let env_lck = uid_store.envelopes.lock().unwrap();
                        entry.unseen.lock().unwrap().insert_set(
                            env_hashes
                                .iter()
                                .filter_map(|h| {
                                    if !env_lck[h].inner.is_seen() {
                                        Some(*h)
                                    } else {
                                        None
                                    }
                                })
                                .collect(),
                        );
                    });
                let env_lck = uid_store.envelopes.lock().unwrap();

                return Ok(Some(
                    env_hashes
                        .into_iter()
                        .filter_map(|env_hash| {
                            env_lck.get(&env_hash).map(|c_env| c_env.inner.clone())
                        })
                        .collect::<Vec<Envelope>>(),
                ));
            }
            Some(Err(err)) => return debug!(Err(err)),
        }
    }
}
