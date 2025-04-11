//
// melib - IMAP
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of melib.
//
// melib is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// melib is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with melib. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use smallvec::SmallVec;

use crate::{
    backends::{EnvelopeHashBatch, FlagOp, MailboxHash, RefreshEvent, RefreshEventKind, TagHash},
    email::{Envelope, EnvelopeHash},
    error::{Error, ErrorKind, Result, ResultIntoError},
    imap::{
        sync::cache::{CachedEnvelope, CachedState, ImapCache, ImapCacheReset},
        FetchResponse, ModSequence, SelectResponse, UIDStore, UID, UIDVALIDITY,
    },
    utils::sqlite3::{
        self,
        rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput, Value},
        Connection, DatabaseDescription,
    },
};

type Sqlite3UID = i32;

#[derive(Debug)]
pub struct Sqlite3Cache {
    connection: Connection,
    loaded_mailboxes: BTreeMap<MailboxHash, CachedState>,
    uid_store: Arc<UIDStore>,
    data_dir: Option<PathBuf>,
}

const DB_DESCRIPTION: DatabaseDescription = DatabaseDescription {
    name: "header_cache.db",
    identifier: None,
    application_prefix: "meli",
    directory: None,
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
                max_uid          INTEGER,
                flags            BLOB NOT NULL,
                highestmodseq    INTEGER,
                PRIMARY KEY (mailbox_hash)
               );
    CREATE INDEX IF NOT EXISTS envelope_uid_idx ON envelopes(mailbox_hash, uid);
    CREATE INDEX IF NOT EXISTS envelope_idx ON envelopes(hash);
    CREATE INDEX IF NOT EXISTS mailbox_idx ON mailbox(mailbox_hash);",
    ),
    version: 4,
};

impl From<EnvelopeHash> for Value {
    fn from(env_hash: EnvelopeHash) -> Self {
        (env_hash.0 as i64).into()
    }
}

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
    pub fn get(uid_store: Arc<UIDStore>, data_dir: Option<&Path>) -> Result<Box<dyn ImapCache>> {
        let data_dir = data_dir.map(|p| p.to_path_buf());
        let db_desc = DatabaseDescription {
            identifier: Some(uid_store.account_name.to_string().into()),
            directory: data_dir.clone().map(|p| p.into()),
            ..DB_DESCRIPTION.clone()
        };
        let connection = match db_desc.open_or_create_db() {
            Ok(c) => Ok(c),
            Err(err) => {
                // try resetting database on error, but only one time.
                if db_desc.reset_db().is_ok() {
                    db_desc.open_or_create_db()
                } else {
                    Err(err)
                }
            }
        }?;

        Ok(Box::new(Self {
            connection,
            loaded_mailboxes: BTreeMap::default(),
            uid_store,
            data_dir,
        }))
    }
}

impl ImapCacheReset for Sqlite3Cache {
    fn reset_db(uid_store: &UIDStore, data_dir: Option<&Path>) -> Result<()> {
        let db_desc = DatabaseDescription {
            identifier: Some(uid_store.account_name.to_string().into()),
            directory: data_dir.map(|p| p.to_path_buf().into()),
            ..DB_DESCRIPTION.clone()
        };
        db_desc.reset_db()
    }
}

impl ImapCache for Sqlite3Cache {
    fn reset(&mut self) -> Result<()> {
        Self::reset_db(&self.uid_store, self.data_dir.as_deref())
    }

    fn max_uid(&mut self, mailbox_hash: MailboxHash) -> Result<Option<UID>> {
        let tx = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let env_max_uid: Option<UID> = {
            let mut stmt = tx.prepare("SELECT MAX(uid) FROM envelopes WHERE mailbox_hash = ?1;")?;

            let ret: Option<UID> = stmt.query_row(sqlite3::params![mailbox_hash], |row| {
                row.get(0).map(|i: Option<Sqlite3UID>| i.map(|i| i as UID))
            })?;
            drop(stmt);
            ret
        };
        let mut max_uid = {
            let mut stmt = tx.prepare("SELECT max_uid FROM mailbox WHERE mailbox_hash = ?1;")?;

            let ret: Option<UID> = stmt.query_row(sqlite3::params![mailbox_hash], |row| {
                row.get(0).map(|i: Option<Sqlite3UID>| i.map(|i| i as UID))
            })?;
            drop(stmt);
            ret
        };
        if let (Some(env_max_uid), true) = (env_max_uid, max_uid != env_max_uid) {
            max_uid = Some(env_max_uid);
            tx.execute(
                "UPDATE mailbox SET max_uid=?1 where mailbox_hash = ?2;",
                sqlite3::params![env_max_uid, mailbox_hash],
            )?;
            tx.commit()?;
        }
        if let Some(max_uid) = max_uid {
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, max_uid);
        }
        Ok(max_uid)
    }

    fn mailbox_state(&mut self, mailbox_hash: MailboxHash) -> Result<Option<CachedState>> {
        if let Some(s) = self.loaded_mailboxes.get(&mailbox_hash) {
            return Ok(Some(*s));
        }
        let tx = self.connection.transaction()?;
        let mut stmt = tx.prepare(
            "SELECT uidvalidity, max_uid, flags, highestmodseq FROM mailbox WHERE mailbox_hash = \
             ?1;",
        )?;

        let mut ret = stmt.query_map(sqlite3::params![mailbox_hash], |row| {
            Ok((
                row.get(0).map(|u: Sqlite3UID| u as UID)?,
                row.get(1)
                    .map(|u: Option<Sqlite3UID>| u.map(|u| u as UID))?,
                row.get(2)?,
                row.get(3)?,
            ))
        })?;
        if let Some(v) = ret.next() {
            let (uidvalidity, max_uid, flags, highestmodseq): (
                UIDVALIDITY,
                Option<UID>,
                Vec<u8>,
                Option<ModSequence>,
            ) = v?;
            drop(ret);
            drop(stmt);
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
            if let Some(max_uid) = max_uid {
                self.uid_store
                    .max_uids
                    .lock()
                    .unwrap()
                    .insert(mailbox_hash, max_uid);
            };
            let retval = CachedState {
                highestmodseq,
                uidvalidity,
            };
            self.loaded_mailboxes.insert(mailbox_hash, retval);
            Ok(Some(retval))
        } else {
            Ok(None)
        }
    }

    fn init_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()> {
        self.loaded_mailboxes.remove(&mailbox_hash);
        let tx = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        tx.execute(
            "DELETE FROM mailbox WHERE mailbox_hash = ?1",
            sqlite3::params![mailbox_hash],
        )
        .chain_err_summary(|| {
            format!(
                "Could not clear cache of mailbox {} account {}",
                mailbox_hash, self.uid_store.account_name
            )
        })?;

        let highestmodseq: Option<ModSequence> =
            select_response.highestmodseq.transpose().unwrap_or(None);
        tx.execute(
            "INSERT OR IGNORE INTO mailbox (uidvalidity, flags, highestmodseq, mailbox_hash) \
             VALUES (?1, ?2, ?3, ?4)",
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
        tx.commit()?;
        let val = CachedState {
            highestmodseq,
            uidvalidity: select_response.uidvalidity,
        };
        self.loaded_mailboxes.insert(mailbox_hash, val);
        Ok(())
    }

    fn update_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        select_response: &SelectResponse,
    ) -> Result<()> {
        if self.mailbox_state(mailbox_hash)?.is_none() {
            return Err(Error::new("Mailbox is not in cache").set_kind(ErrorKind::NotFound));
        }

        let tx = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let highestmodseq: Option<ModSequence> =
            select_response.highestmodseq.transpose().unwrap_or(None);
        tx.execute(
            "UPDATE mailbox SET flags = ?1, highestmodseq = ?2, uidvalidity = ?3 where \
             mailbox_hash = ?4;",
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
                select_response.uidvalidity,
                mailbox_hash,
            ],
        )
        .chain_err_summary(|| {
            format!(
                "Could not update mailbox {} in header_cache of account {}",
                mailbox_hash, self.uid_store.account_name
            )
        })?;
        tx.commit()?;
        let val = CachedState {
            highestmodseq,
            uidvalidity: select_response.uidvalidity,
        };
        self.loaded_mailboxes.insert(mailbox_hash, val);
        Ok(())
    }

    fn envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        max_uid: UID,
        batch_size: usize,
    ) -> Result<Option<Vec<EnvelopeHash>>> {
        if self.mailbox_state(mailbox_hash)?.is_none() {
            return Ok(None);
        }

        let res = {
            let min = max_uid.saturating_sub(batch_size).max(1);
            let max = max_uid;
            let tx = self.connection.transaction()?;
            let mut stmt = tx.prepare(
                "SELECT uid, hash, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1 \
                 AND uid <= ?2 AND uid >= ?3;",
            )?;

            #[allow(clippy::let_and_return)] // false positive, the let binding is needed
            // for the temporary to live long enough
            let x = stmt
                .query_map(sqlite3::params![mailbox_hash, max, min], |row| {
                    Ok((
                        row.get(0).map(|i: Sqlite3UID| i as UID)?,
                        row.get(1)?,
                        row.get(2)?,
                        row.get(3)?,
                    ))
                })?
                .collect::<std::result::Result<_, _>>();
            x
        };
        let ret: Vec<(UID, EnvelopeHash, Envelope, Option<ModSequence>)> = match res {
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
        for (uid, hash, env, modseq) in ret {
            if hash != env.hash() {
                return Ok(None);
            }
            env_hashes.push(env.hash());
            max_uid = max_uid.max(uid);
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
        Ok(Some(env_hashes))
    }

    fn insert_envelopes(
        &mut self,
        mailbox_hash: MailboxHash,
        fetches: &[FetchResponse<'_>],
    ) -> Result<()> {
        let mut max_uid = self
            .uid_store
            .max_uids
            .lock()
            .unwrap()
            .get(&mailbox_hash)
            .cloned()
            .unwrap_or_default();
        let Self {
            ref mut connection,
            ref uid_store,
            loaded_mailboxes: _,
            data_dir: _,
        } = self;
        let tx = connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
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
                bodystructure: _,
            } = item
            {
                max_uid = max_uid.max(*uid);
                let result = tx.execute(
                    "INSERT OR REPLACE INTO envelopes (hash, uid, mailbox_hash, modsequence, \
                     envelope) VALUES (?1, ?2, ?3, ?4, ?5)",
                    sqlite3::params![
                        envelope.hash(),
                        *uid as Sqlite3UID,
                        mailbox_hash,
                        modseq,
                        &envelope
                    ],
                );
                if let Err(err) = result {
                    let summary = format!(
                        "Could not insert envelope {} {} in header_cache of account {}",
                        envelope.message_id(),
                        envelope.hash(),
                        uid_store.account_name
                    );
                    if err.sqlite_error_code() == Some(rusqlite::ErrorCode::ConstraintViolation) {
                        // Our only constraint is the mailbox_hash foreign key.
                        return Err(Error::from(err)
                            .set_summary(summary)
                            .set_kind(ErrorKind::NotFound));
                    }
                    return Err(Error::from(err).set_summary(summary));
                }
            }
        }
        tx.commit()?;
        if let Ok(Some(new_max_uid)) = self.max_uid(mailbox_hash) {
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, new_max_uid);
        }
        Ok(())
    }

    fn update_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> Result<()> {
        let Self {
            ref mut connection,
            ref uid_store,
            loaded_mailboxes: _,
            data_dir: _,
        } = self;
        let tx = connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let values = std::rc::Rc::new(env_hashes.iter().map(Value::from).collect::<Vec<Value>>());

        let mut stmt =
            tx.prepare("SELECT uid, envelope FROM envelopes WHERE hash IN rarray(?1);")?;
        let rows = stmt
            .query_map([values], |row| Ok((row.get(0)?, row.get(1)?)))?
            .filter_map(|r| r.ok())
            .collect::<Vec<(UID, Envelope)>>();
        drop(stmt);
        let mut stmt =
            tx.prepare("UPDATE envelopes SET envelope = ?1 WHERE mailbox_hash = ?2 AND uid = ?3;")?;
        for (uid, mut env) in rows {
            for op in flags.iter() {
                match op {
                    FlagOp::UnSet(flag) | FlagOp::Set(flag) => {
                        let mut f = env.flags();
                        f.set(*flag, op.as_bool());
                        env.set_flags(f);
                    }
                    FlagOp::UnSetTag(tag) | FlagOp::SetTag(tag) => {
                        let hash = TagHash::from_bytes(tag.as_bytes());
                        if op.as_bool() {
                            env.tags_mut().insert(hash);
                        } else {
                            env.tags_mut().shift_remove(&hash);
                        }
                    }
                }
            }
            stmt.execute(sqlite3::params![&env, mailbox_hash, uid as Sqlite3UID])?;
            uid_store
                .envelopes
                .lock()
                .unwrap()
                .entry(env.hash())
                .and_modify(|entry| {
                    entry.inner = env;
                });
        }
        drop(stmt);
        tx.commit()?;
        if let Ok(Some(new_max_uid)) = self.max_uid(mailbox_hash) {
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, new_max_uid);
        }
        Ok(())
    }

    fn update(
        &mut self,
        mailbox_hash: MailboxHash,
        refresh_events: &[(UID, RefreshEvent)],
    ) -> Result<()> {
        {
            let Self {
                ref mut connection,
                ref uid_store,
                loaded_mailboxes: _,
                data_dir: _,
            } = self;
            let tx =
                connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
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
        }
        if let Ok(Some(new_max_uid)) = self.max_uid(mailbox_hash) {
            self.uid_store
                .max_uids
                .lock()
                .unwrap()
                .insert(mailbox_hash, new_max_uid);
        }
        Ok(())
    }

    fn find_envelope(
        &mut self,
        identifier: std::result::Result<UID, EnvelopeHash>,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<CachedEnvelope>> {
        let mut ret: Vec<(UID, Envelope, Option<ModSequence>)> = match identifier {
            Ok(uid) => {
                let tx = self.connection.transaction()?;
                let mut stmt = tx.prepare(
                    "SELECT uid, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1 AND \
                     uid = ?2;",
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
                let tx = self.connection.transaction()?;
                let mut stmt = tx.prepare(
                    "SELECT uid, envelope, modsequence FROM envelopes WHERE mailbox_hash = ?1 AND \
                     hash = ?2;",
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
                let tx = self.connection.transaction()?;
                let mut stmt = tx.prepare(
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
                let tx = self.connection.transaction()?;
                let mut stmt = tx.prepare(
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
