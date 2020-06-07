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

use super::UID;
use crate::{
    backends::{AccountHash, MailboxHash},
    email::Envelope,
    error::*,
};

pub type MaxUID = UID;

#[cfg(feature = "sqlite3")]
pub use sqlite3_m::*;

#[cfg(feature = "sqlite3")]
mod sqlite3_m {
    use super::*;
    use crate::sqlite3;
    const DB_NAME: &'static str = "header_cache.db";
    const INIT_SCRIPT: &'static str = "PRAGMA foreign_keys = true;
    PRAGMA encoding = 'UTF-8';

    CREATE TABLE IF NOT EXISTS envelopes (
                    mailbox_hash     INTEGER,
                    uid              INTEGER,
                    validity         INTEGER,
                    envelope         BLOB NOT NULL UNIQUE,
                    PRIMARY KEY (mailbox_hash, uid, validity),
                    FOREIGN KEY (mailbox_hash, validity) REFERENCES uidvalidity(mailbox_hash, uid) ON DELETE CASCADE
                   );
    CREATE TABLE IF NOT EXISTS uidvalidity (
                uid              INTEGER UNIQUE,
                mailbox_hash     INTEGER UNIQUE,
                PRIMARY KEY (mailbox_hash, uid)
               );
    CREATE INDEX IF NOT EXISTS envelope_idx ON envelopes(mailbox_hash, uid, validity);
    CREATE INDEX IF NOT EXISTS uidvalidity_idx ON uidvalidity(mailbox_hash);";

    pub fn get_envelopes(
        account_hash: AccountHash,
        mailbox_hash: MailboxHash,
        uidvalidity: usize,
    ) -> Result<(MaxUID, Vec<(UID, Envelope)>)> {
        let conn = sqlite3::open_or_create_db(
            &format!("{}_{}", account_hash, DB_NAME),
            Some(INIT_SCRIPT),
        )?;
        let mut stmt = conn
            .prepare("SELECT MAX(uid) FROM envelopes WHERE mailbox_hash = ? AND validity = ?")
            .unwrap();
        let max_uid: usize = stmt
            .query_map(
                sqlite3::params![mailbox_hash as i64, uidvalidity as i64],
                |row| row.get(0).map(|u: i64| u as usize),
            )
            .chain_err_summary(|| {
                format!(
                    "Error while performing query {:?}",
                    "SELECT MAX(uid) FROM envelopes WHERE mailbox_hash = ? AND validity = ?"
                )
            })?
            .next()
            .unwrap()
            .unwrap_or(0);
        let mut stmt = conn
            .prepare("SELECT uid, envelope FROM envelopes WHERE mailbox_hash = ? AND validity = ?")
            .unwrap();
        let results: Vec<(UID, Vec<u8>)> = stmt
            .query_map(
                sqlite3::params![mailbox_hash as i64, uidvalidity as i64],
                |row| Ok((row.get::<_, i64>(0)? as usize, row.get(1)?)),
            )
            .chain_err_summary(|| {
                format!(
                    "Error while performing query {:?}",
                    "SELECT uid, envelope FROM envelopes WHERE mailbox_hash = ? AND validity = ?",
                )
            })?
            .collect::<std::result::Result<_, _>>()?;
        debug!(
            "imap cache max_uid: {} results len: {}",
            max_uid,
            results.len()
        );
        Ok((
            max_uid,
            results
                .into_iter()
                .map(|(uid, env)| {
                    Ok((
                        uid,
                        bincode::deserialize(&env).map_err(|e| MeliError::new(e.to_string()))?,
                    ))
                })
                .collect::<Result<Vec<(UID, Envelope)>>>()?,
        ))
    }

    pub fn save_envelopes(
        account_hash: AccountHash,
        mailbox_hash: MailboxHash,
        uidvalidity: usize,
        envs: &[(UID, &Envelope)],
    ) -> Result<()> {
        let conn =
            sqlite3::open_or_create_db(&format!("{}_{}", account_hash, DB_NAME), Some(INIT_SCRIPT))
                .chain_err_summary(|| {
                    format!(
                        "Could not create header_cache.db for account {}",
                        account_hash
                    )
                })?;
        conn.execute(
            "INSERT OR REPLACE INTO uidvalidity (uid, mailbox_hash) VALUES (?1, ?2)",
            sqlite3::params![uidvalidity as i64, mailbox_hash as i64],
        )
        .chain_err_summary(|| {
            format!(
                "Could not insert uidvalidity {} in header_cache of account {}",
                uidvalidity, account_hash
            )
        })?;
        for (uid, env) in envs {
            conn.execute(
                "INSERT OR REPLACE INTO envelopes (uid, mailbox_hash, validity, envelope) VALUES (?1, ?2, ?3, ?4)",
                sqlite3::params![*uid as i64, mailbox_hash as i64, uidvalidity as i64, bincode::serialize(env).map_err(|e| MeliError::new(e.to_string()))?],
            ).chain_err_summary(|| format!("Could not insert envelope with hash {} in header_cache of account {}", env.hash(), account_hash))?;
        }
        Ok(())
    }
}

#[cfg(not(feature = "sqlite3"))]
pub use filesystem_m::*;

#[cfg(not(feature = "sqlite3"))]
mod filesystem_m {
    use super::*;
    pub fn get_envelopes(
        _account_hash: AccountHash,
        _mailbox_hash: MailboxHash,
        _uidvalidity: usize,
    ) -> Result<(MaxUID, Vec<(UID, Envelope)>)> {
        Ok((0, vec![]))
    }

    pub fn save_envelopes(
        _account_hash: AccountHash,
        _mailbox_hash: MailboxHash,
        _uidvalidity: usize,
        _envs: &[(UID, &Envelope)],
    ) -> Result<()> {
        Ok(())
    }
}
