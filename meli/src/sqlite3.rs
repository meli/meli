/*
 * meli - sqlite3.rs
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

//! Use an sqlite3 database for fast searching.
use std::{
    path::PathBuf,
    sync::{Arc, RwLock},
};

use melib::{
    backends::MailBackend,
    email::{attachment_types::Text, Envelope, EnvelopeHash},
    log,
    search::{
        escape_double_quote,
        Query::{self, *},
    },
    smol,
    utils::sqlite3::{rusqlite::params, DatabaseDescription},
    Error, Result, ResultIntoError, SortField, SortOrder,
};
use smallvec::SmallVec;

const DB: DatabaseDescription = DatabaseDescription {
    name: "index.db",
    identifier: None,
    application_prefix: "meli",
    init_script: Some(
        "CREATE TABLE IF NOT EXISTS envelopes (
                    id               INTEGER PRIMARY KEY,
                    account_id       INTEGER REFERENCES accounts ON UPDATE CASCADE,
                    hash             BLOB NOT NULL UNIQUE,
                    date             TEXT NOT NULL,
                    _from            TEXT NOT NULL,
                    _to              TEXT NOT NULL,
                    cc               TEXT NOT NULL,
                    bcc              TEXT NOT NULL,
                    subject          TEXT NOT NULL,
                    message_id       TEXT NOT NULL,
                    in_reply_to      TEXT NOT NULL,
                    _references      TEXT NOT NULL,
                    flags            INTEGER NOT NULL,
                    has_attachments  BOOLEAN NOT NULL,
                    body_text        TEXT NOT NULL,
                    timestamp        BLOB NOT NULL
                   );
        CREATE TABLE IF NOT EXISTS folders (
                    id               INTEGER PRIMARY KEY,
                    account_id       INTEGER NOT NULL REFERENCES accounts ON UPDATE CASCADE,
                    hash             BLOB NOT NULL,
                    date             TEXT NOT NULL,
                    name             TEXT NOT NULL
                  );
        CREATE TABLE IF NOT EXISTS accounts (
                    id               INTEGER PRIMARY KEY,
                    name             TEXT NOT NULL UNIQUE
                  );
        CREATE TABLE IF NOT EXISTS folder_and_envelope (
                    folder_id        INTEGER NOT NULL,
                    envelope_id      INTEGER NOT NULL,
                    PRIMARY KEY (folder_id, envelope_id),
                    FOREIGN KEY(folder_id) REFERENCES folders(id) ON UPDATE CASCADE,
                    FOREIGN KEY(envelope_id) REFERENCES envelopes(id) ON UPDATE CASCADE
                  );
      CREATE INDEX IF NOT EXISTS folder_env_idx ON folder_and_envelope(folder_id);
      CREATE INDEX IF NOT EXISTS env_folder_idx ON folder_and_envelope(envelope_id);
      CREATE UNIQUE INDEX IF NOT EXISTS acc_idx ON accounts(name);


CREATE INDEX IF NOT EXISTS envelope_timestamp_index ON envelopes (timestamp);
CREATE INDEX IF NOT EXISTS envelope__from_index ON envelopes (_from);
CREATE INDEX IF NOT EXISTS envelope__to_index ON envelopes (_to);
CREATE INDEX IF NOT EXISTS envelope_cc_index ON envelopes (cc);
CREATE INDEX IF NOT EXISTS envelope_bcc_index ON envelopes (bcc);
CREATE INDEX IF NOT EXISTS envelope_message_id_index ON envelopes (message_id);

        CREATE VIRTUAL TABLE IF NOT EXISTS fts USING fts5(subject, body_text, content=envelopes, \
         content_rowid=id);

-- Triggers to keep the FTS index up to date.
CREATE TRIGGER IF NOT EXISTS envelopes_ai AFTER INSERT ON envelopes BEGIN
  INSERT INTO fts(rowid, subject, body_text) VALUES (new.id, new.subject, new.body_text);
END;

CREATE TRIGGER IF NOT EXISTS envelopes_ad AFTER DELETE ON envelopes BEGIN
  INSERT INTO fts(fts, rowid, subject, body_text) VALUES('delete', old.id, old.subject, \
         old.body_text);
END;

CREATE TRIGGER IF NOT EXISTS envelopes_au AFTER UPDATE ON envelopes BEGIN
  INSERT INTO fts(fts, rowid, subject, body_text) VALUES('delete', old.id, old.subject, \
         old.body_text);
  INSERT INTO fts(rowid, subject, body_text) VALUES (new.id, new.subject, new.body_text);
END; ",
    ),
    version: 1,
};

//#[inline(always)]
//fn fts5_bareword(w: &str) -> Cow<str> {
//    if w == "AND" || w == "OR" || w == "NOT" {
//        Cow::from(w)
//    } else {
//        if !w.is_ascii() {
//            Cow::from(format!("\"{}\"", escape_double_quote(w)))
//        } else {
//            for &b in w.as_bytes() {
//                if !(b > 0x2f && b < 0x3a)
//                    || !(b > 0x40 && b < 0x5b)
//                    || !(b > 0x60 && b < 0x7b)
//                    || b != 0x60
//                    || b != 26
//                {
//                    return Cow::from(format!("\"{}\"",
// escape_double_quote(w)));                }
//            }
//            Cow::from(w)
//        }
//    }
//}
//

pub struct AccountCache;

impl AccountCache {
    #[inline]
    pub const fn is_async() -> crate::jobs::IsAsync {
        crate::jobs::IsAsync::Blocking
    }

    pub async fn insert(
        envelope: Envelope,
        backend: Arc<RwLock<Box<dyn MailBackend>>>,
        acc_name: String,
    ) -> Result<()> {
        let db_desc = DatabaseDescription {
            identifier: Some(acc_name.clone().into()),
            ..DB.clone()
        };

        if !db_desc.exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Database hasn't been initialised. Run `reindex {acc_name}` command"
            )));
        }

        let op = backend
            .read()
            .unwrap()
            .operation(envelope.hash())?
            .as_bytes()?;

        let body = match op.await.map(|bytes| envelope.body_bytes(&bytes)) {
            Ok(body) => body.text(Text::Plain),
            Err(err) => {
                log::error!(
                    "Failed to open envelope {}: {err}",
                    envelope.message_id_display(),
                );
                return Err(err);
            }
        };
        smol::unblock(move || {
            let mut conn = db_desc.open_or_create_db()?;

            let tx =
                conn.transaction_with_behavior(melib::rusqlite::TransactionBehavior::Immediate)?;
            if let Err(err) = tx.execute(
                "INSERT OR IGNORE INTO accounts (name) VALUES (?1)",
                params![acc_name,],
            ) {
                log::error!(
                    "Failed to insert envelope {}: {err}",
                    envelope.message_id_display(),
                );
                return Err(Error::new(format!(
                    "Failed to insert envelope {}: {err}",
                    envelope.message_id_display(),
                )));
            }
            let account_id: i32 = {
                let mut stmt = tx
                    .prepare("SELECT id FROM accounts WHERE name = ?")
                    .unwrap();
                let x = stmt
                    .query_map(params![acc_name], |row| row.get(0))
                    .unwrap()
                    .next()
                    .unwrap()
                    .unwrap();
                x
            };
            if let Err(err) = tx
                .execute(
                    "INSERT OR REPLACE INTO envelopes (account_id, hash, date, _from, _to, cc, \
                     bcc, subject, message_id, in_reply_to, _references, flags, has_attachments, \
                     body_text, timestamp)
              VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15)",
                    params![
                        account_id,
                        envelope.hash().to_be_bytes().to_vec(),
                        envelope.date_as_str(),
                        envelope.field_from_to_string(),
                        envelope.field_to_to_string(),
                        envelope.field_cc_to_string(),
                        envelope.field_bcc_to_string(),
                        envelope.subject().into_owned().trim_end_matches('\u{0}'),
                        envelope.message_id_display().to_string(),
                        envelope
                            .in_reply_to_display()
                            .map(|f| f.to_string())
                            .unwrap_or_default(),
                        envelope.field_references_to_string(),
                        i64::from(envelope.flags().bits()),
                        i32::from(envelope.has_attachments()),
                        body,
                        envelope.date().to_be_bytes().to_vec()
                    ],
                )
                .map_err(|e| Error::new(e.to_string()))
            {
                drop(tx);
                log::error!(
                    "Failed to insert envelope {}: {err}",
                    envelope.message_id_display(),
                );
            } else {
                tx.commit()?;
            }
            Ok(())
        })
        .await?;
        Ok(())
    }

    pub async fn remove(acc_name: String, env_hash: EnvelopeHash) -> Result<()> {
        let db_desc = DatabaseDescription {
            identifier: Some(acc_name.clone().into()),
            ..DB.clone()
        };
        let db_path = db_desc.db_path()?;
        if !db_path.exists() {
            return Err(Error::new(format!(
                "Database hasn't been initialised. Run `reindex {acc_name}` command"
            )));
        }

        smol::unblock(move || {
            let mut conn = db_desc.open_or_create_db()?;
            let tx =
                conn.transaction_with_behavior(melib::rusqlite::TransactionBehavior::Immediate)?;
            if let Err(err) = tx.execute(
                "DELETE FROM envelopes WHERE hash = ?",
                params![env_hash.to_be_bytes().to_vec(),],
            ) {
                drop(tx);
                log::error!("Failed to remove envelope {env_hash}: {err}");
                return Err(Error::new(format!(
                    "Failed to remove envelope {env_hash}: {err}"
                )));
            }
            tx.commit()?;
            Ok(())
        })
        .await?;
        Ok(())
    }

    pub async fn index(
        acc_name: Arc<String>,
        collection: melib::Collection,
        backend_mutex: Arc<RwLock<Box<dyn MailBackend>>>,
    ) -> Result<()> {
        let acc_mutex = collection.envelopes.clone();
        let db_desc = Arc::new(DatabaseDescription {
            identifier: Some(acc_name.to_string().into()),
            ..DB.clone()
        });
        let env_hashes = acc_mutex
            .read()
            .unwrap()
            .keys()
            .cloned()
            .collect::<Vec<_>>();

        /* Sleep, index and repeat in order not to block the main process */
        let account_id: i32 = {
            let acc_name = Arc::clone(&acc_name);
            let db_desc = Arc::clone(&db_desc);
            smol::unblock(move || {
                let mut conn = db_desc.open_or_create_db()?;
                let tx = conn
                    .transaction_with_behavior(melib::rusqlite::TransactionBehavior::Immediate)?;
                tx.execute(
                    "INSERT OR REPLACE INTO accounts (name) VALUES (?1)",
                    params![acc_name.as_str(),],
                )
                .chain_err_summary(|| "Failed to update index:")?;
                let account_id = {
                    let mut stmt = tx
                        .prepare("SELECT id FROM accounts WHERE name = ?")
                        .unwrap();
                    let x = stmt
                        .query_map(params![acc_name.as_str()], |row| row.get(0))
                        .unwrap()
                        .next()
                        .unwrap()
                        .unwrap();
                    x
                };
                tx.commit()?;
                Ok::<i32, Error>(account_id)
            })
            .await?
        };
        let mut ctr = 0;
        log::trace!(
            "Rebuilding {} index. {}/{}",
            acc_name,
            ctr,
            env_hashes.len()
        );
        for chunk in env_hashes.chunks(200) {
            ctr += chunk.len();
            let mut chunk_bytes = Vec::with_capacity(chunk.len());
            for &env_hash in chunk {
                let op = backend_mutex.read().unwrap().operation(env_hash)?;
                let bytes = op
                    .as_bytes()?
                    .await
                    .chain_err_summary(|| format!("Failed to open envelope {}", env_hash))?;
                chunk_bytes.push((env_hash, bytes));
            }
            {
                let acc_mutex = acc_mutex.clone();
                let db_desc = Arc::clone(&db_desc);
                smol::unblock(move || {
                    let mut conn = db_desc.open_or_create_db()?;
                    let tx = conn.transaction_with_behavior(
                        melib::rusqlite::TransactionBehavior::Immediate,
                    )?;
                    let envelopes_lck = acc_mutex.read().unwrap();
                    for (env_hash, bytes) in chunk_bytes {
                        if let Some(e) = envelopes_lck.get(&env_hash) {
                            let body = e.body_bytes(&bytes).text(Text::Plain).replace('\0', "");
                            tx.execute(
                                "INSERT OR REPLACE INTO envelopes (account_id, hash, date, _from, \
                                 _to, cc, bcc, subject, message_id, in_reply_to, _references, \
                                 flags, has_attachments, body_text, timestamp)
              VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15)",
                                params![
                                    account_id,
                                    e.hash().to_be_bytes().to_vec(),
                                    e.date_as_str(),
                                    e.field_from_to_string(),
                                    e.field_to_to_string(),
                                    e.field_cc_to_string(),
                                    e.field_bcc_to_string(),
                                    e.subject().into_owned().trim_end_matches('\u{0}'),
                                    e.message_id_display().to_string(),
                                    e.in_reply_to_display()
                                        .map(|f| f.to_string())
                                        .unwrap_or_default(),
                                    e.field_references_to_string(),
                                    i64::from(e.flags().bits()),
                                    i32::from(e.has_attachments()),
                                    body,
                                    e.date().to_be_bytes().to_vec()
                                ],
                            )
                            .chain_err_summary(|| {
                                format!("Failed to insert envelope {}", e.message_id_display())
                            })?;
                        }
                    }
                    tx.commit()?;
                    Ok::<(), Error>(())
                })
                .await?;
            }
            let sleep_dur = std::time::Duration::from_millis(50);
            smol::Timer::after(sleep_dur).await;
        }
        Ok(())
    }

    pub async fn search(
        acc_name: String,
        query: Query,
        (sort_field, sort_order): (SortField, SortOrder),
    ) -> Result<SmallVec<[EnvelopeHash; 512]>> {
        let db_desc = DatabaseDescription {
            identifier: Some(acc_name.clone().into()),
            ..DB.clone()
        };

        if !db_desc.exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Database hasn't been initialised for account `{}`. Run `reindex` command to \
                 build an index.",
                acc_name
            )));
        }
        let query = query_to_sql(&query);

        smol::unblock(move || {
            let mut conn = db_desc.open_or_create_db()?;

            let sort_field = match sort_field {
                SortField::Subject => "subject",
                SortField::Date => "timestamp",
            };

            let sort_order = match sort_order {
                SortOrder::Asc => "ASC",
                SortOrder::Desc => "DESC",
            };

            let tx = conn.transaction()?;
            let mut stmt = tx
                .prepare(&format!(
                    "SELECT hash FROM envelopes WHERE {} ORDER BY {} {};",
                    query, sort_field, sort_order
                ))
                .map_err(|e| Error::new(e.to_string()))?;

            #[allow(clippy::let_and_return)] // false positive, the let binding is needed
            // for the temporary to live long enough
            let x = stmt
                .query_map([], |row| row.get::<_, EnvelopeHash>(0))
                .map_err(Error::from)?
                .map(|item| item.map_err(Error::from))
                .collect::<Result<SmallVec<[EnvelopeHash; 512]>>>();
            x
        })
        .await
    }

    pub fn db_path(acc_name: &str) -> Result<Option<PathBuf>> {
        let db_desc = DatabaseDescription {
            identifier: Some(acc_name.to_string().into()),
            ..DB.clone()
        };
        let db_path = db_desc.db_path()?;
        if !db_path.exists() {
            return Ok(None);
        }
        Ok(Some(db_path))
    }
}

/// Translates a `Query` to an Sqlite3 expression in a `String`.
pub fn query_to_sql(q: &Query) -> String {
    fn rec(q: &Query, s: &mut String) {
        match q {
            Subject(t) => {
                s.push_str("subject LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\" ");
            }
            From(t) => {
                s.push_str("_from LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\" ");
            }
            To(t) => {
                s.push_str("_to LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\" ");
            }
            Cc(t) => {
                s.push_str("cc LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\" ");
            }
            Bcc(t) => {
                s.push_str("bcc LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\" ");
            }
            AllText(t) => {
                s.push_str("body_text LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\" ");
            }
            And(q1, q2) => {
                s.push('(');
                rec(q1, s);
                s.push_str(") AND (");
                rec(q2, s);
                s.push_str(") ");
            }
            Or(q1, q2) => {
                s.push('(');
                rec(q1, s);
                s.push_str(") OR (");
                rec(q2, s);
                s.push_str(") ");
            }
            Not(q) => {
                s.push_str("NOT (");
                rec(q, s);
                s.push_str(") ");
            }
            Flags(v) => {
                let total = v.len();
                if total > 1 {
                    s.push('(');
                }
                for (i, f) in v.iter().enumerate() {
                    match f.as_str() {
                        "draft" => {
                            s.push_str(" (flags & 8 > 0) ");
                        }
                        "deleted" | "trashed" => {
                            s.push_str(" (flags & 6 > 0) ");
                        }
                        "flagged" => {
                            s.push_str(" (flags & 16 > 0) ");
                        }
                        "recent" => {
                            s.push_str(" (flags & 4 == 0) ");
                        }
                        "seen" | "read" => {
                            s.push_str(" (flags & 4 > 0) ");
                        }
                        "unseen" | "unread" => {
                            s.push_str(" (flags & 4 == 0) ");
                        }
                        "answered" | "replied" => {
                            s.push_str(" (flags & 2 > 0) ");
                        }
                        "unanswered" => {
                            s.push_str(" (flags & 2 == 0) ");
                        }
                        _ => {
                            continue;
                        }
                    }
                    if total > 1 && i != total - 1 {
                        s.push_str(" AND ");
                    }
                }
                if total > 1 {
                    s.push_str(") ");
                }
            }
            HasAttachment => {
                s.push_str("has_attachments == 1 ");
            }
            _ => {}
        }
    }
    let mut ret = String::new();
    rec(q, &mut ret);
    ret
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_to_sql() {
        use melib::{search::query, utils::parsec::Parser};
        assert_eq!(
            "(subject LIKE \"%test%\" ) AND (body_text LIKE \"%i%\" ) ",
            &query_to_sql(&query().parse_complete("subject:test and i").unwrap().1)
        );
        assert_eq!(
            "(subject LIKE \"%github%\" ) OR ((_from LIKE \"%epilys%\" ) AND ((subject LIKE \
             \"%lib%\" ) OR (subject LIKE \"%meli%\" ) ) ) ",
            &query_to_sql(
                &query()
                    .parse_complete(
                        "subject:github or (from:epilys and (subject:lib or subject:meli))"
                    )
                    .unwrap()
                    .1
            )
        );
    }
}
