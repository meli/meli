/*
 * meli - nntp module.
 *
 * Copyright 2023 Manos Pitsidianakis
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

//! Store article seen/read flags in an sqlite3 database, since NNTP has no
//! concept of server-side flag bookkeeping.

pub use inner::*;

#[cfg(feature = "sqlite3")]
mod inner {
    use crate::{
        email::Flag,
        nntp::UID,
        utils::sqlite3::{self, Connection, DatabaseDescription},
        EnvelopeHash, MailboxHash, Result,
    };

    pub const DB_DESCRIPTION: DatabaseDescription = DatabaseDescription {
        name: "nntp_store.db",
        application_prefix: "meli",
        identifier: None,
        init_script: Some(
            "PRAGMA foreign_keys = true;
PRAGMA encoding = 'UTF-8';

CREATE TABLE IF NOT EXISTS article (
    hash             INTEGER NOT NULL,
    mailbox_hash     INTEGER NOT NULL,
    uid              INTEGER NOT NULL,
    flags            INTEGER NOT NULL DEFAULT 0,
    tags             TEXT,
    PRIMARY KEY (mailbox_hash, uid)
);
    CREATE INDEX IF NOT EXISTS article_uid_idx ON article(mailbox_hash, uid);
    CREATE INDEX IF NOT EXISTS article_idx ON article(hash);",
        ),
        version: 1,
    };

    #[derive(Debug)]
    pub struct Store {
        pub connection: Connection,
    }

    impl Store {
        pub fn new(id: &str) -> Result<Self> {
            let db_desc = DatabaseDescription {
                identifier: Some(id.to_string().into()),
                ..DB_DESCRIPTION
            };
            Ok(Self {
                connection: db_desc.open_or_create_db()?,
            })
        }

        pub fn set_flags(
            &self,
            envelope_hash: EnvelopeHash,
            mailbox_hash: MailboxHash,
            uid: UID,
            new_value: Flag,
        ) -> Result<()> {
            self.connection.execute(
                "INSERT OR REPLACE INTO article(hash, mailbox_hash, uid, flags) VALUES (?, ?, ?, \
                 ?)",
                sqlite3::params![&envelope_hash, &mailbox_hash, &uid, &new_value.bits()],
            )?;

            Ok(())
        }

        pub fn flags(
            &self,
            envelope_hash: EnvelopeHash,
            mailbox_hash: MailboxHash,
            uid: UID,
        ) -> Result<Flag> {
            self.connection.execute(
                "INSERT OR IGNORE INTO article(hash,mailbox_hash,uid,flags) VALUES(?1,?2,?3,0);",
                sqlite3::params![&envelope_hash, &mailbox_hash, &uid],
            )?;
            let mut stmt = self.connection.prepare(
                "SELECT flags FROM article WHERE hash = ?1 AND mailbox_hash = ?2 AND uid = ?3;",
            )?;
            Ok(Flag::from_bits({
                stmt.query_row(
                    sqlite3::params![&envelope_hash, &mailbox_hash, &uid],
                    |row| {
                        let flag: u8 = row.get(0)?;
                        Ok(flag)
                    },
                )?
            })
            .unwrap_or_default())
        }
    }
}

#[cfg(not(feature = "sqlite3"))]
mod inner {
    use crate::{email::Flag, nntp::UID, EnvelopeHash, Error, ErrorKind, MailboxHash, Result};

    #[derive(Debug)]
    pub struct Store;

    impl Store {
        pub fn new(_: &str) -> Result<Self> {
            Ok(Self)
        }

        pub fn set_flags(&self, _: EnvelopeHash, _: MailboxHash, _: UID, _: Flag) -> Result<()> {
            Ok(())
        }

        pub fn flags(&self, _: EnvelopeHash, _: MailboxHash, _: UID) -> Result<Flag> {
            Err(Error::new(
                "NNTP store flag cache accessed but this copy of melib isn't built with sqlite3 \
                 support. This is a bug and should be reported.",
            )
            .set_kind(ErrorKind::Bug))
        }
    }
}
