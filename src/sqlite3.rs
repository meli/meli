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

/*! Use an sqlite3 database for fast searching.
 */
use crate::melib::ResultIntoMeliError;
use melib::search::{
    escape_double_quote,
    Query::{self, *},
};
use melib::{
    backends::{MailBackend, ResultFuture},
    email::{Envelope, EnvelopeHash},
    log,
    sqlite3::{
        self as melib_sqlite3,
        rusqlite::{self, params},
        DatabaseDescription,
    },
    thread::{SortField, SortOrder},
    MeliError, Result, ERROR,
};

use smallvec::SmallVec;
use std::convert::TryInto;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

const DB: DatabaseDescription = DatabaseDescription {
name: "index.db",
init_script:Some( "CREATE TABLE IF NOT EXISTS envelopes (
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

        CREATE VIRTUAL TABLE IF NOT EXISTS fts USING fts5(subject, body_text, content=envelopes, content_rowid=id);

-- Triggers to keep the FTS index up to date.
CREATE TRIGGER IF NOT EXISTS envelopes_ai AFTER INSERT ON envelopes BEGIN
  INSERT INTO fts(rowid, subject, body_text) VALUES (new.id, new.subject, new.body_text);
END;

CREATE TRIGGER IF NOT EXISTS envelopes_ad AFTER DELETE ON envelopes BEGIN
  INSERT INTO fts(fts, rowid, subject, body_text) VALUES('delete', old.id, old.subject, old.body_text);
END;

CREATE TRIGGER IF NOT EXISTS envelopes_au AFTER UPDATE ON envelopes BEGIN
  INSERT INTO fts(fts, rowid, subject, body_text) VALUES('delete', old.id, old.subject, old.body_text);
  INSERT INTO fts(rowid, subject, body_text) VALUES (new.id, new.subject, new.body_text);
END; "),
version: 1,
};

pub fn db_path() -> Result<PathBuf> {
    melib_sqlite3::db_path(DB.name)
}

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
//                    return Cow::from(format!("\"{}\"", escape_double_quote(w)));
//                }
//            }
//            Cow::from(w)
//        }
//    }
//}
//
//
pub fn insert(
    envelope: &Envelope,
    backend: &Arc<RwLock<Box<dyn MailBackend>>>,
    acc_name: &str,
) -> Result<()> {
    let db_path = db_path()?;
    if !db_path.exists() {
        return Err(MeliError::new(
            "Database hasn't been initialised. Run `reindex` command",
        ));
    }

    let conn = melib_sqlite3::open_db(db_path)?;
    let backend_lck = backend.read().unwrap();
    let body = match backend_lck
        .operation(envelope.hash())
        .and_then(|op| envelope.body(op))
    {
        Ok(body) => body.text(),
        Err(err) => {
            debug!(
                "{}",
                format!(
                    "Failed to open envelope {}: {}",
                    envelope.message_id_display(),
                    err.to_string()
                )
            );
            log(
                format!(
                    "Failed to open envelope {}: {}",
                    envelope.message_id_display(),
                    err.to_string()
                ),
                ERROR,
            );
            return Err(err);
        }
    };

    if let Err(err) = conn.execute(
        "INSERT OR IGNORE INTO accounts (name) VALUES (?1)",
        params![acc_name,],
    ) {
        debug!(
            "Failed to insert envelope {}: {}",
            envelope.message_id_display(),
            err.to_string()
        );
        log(
            format!(
                "Failed to insert envelope {}: {}",
                envelope.message_id_display(),
                err.to_string()
            ),
            ERROR,
        );
        return Err(MeliError::new(err.to_string()));
    }
    let account_id: i32 = {
        let mut stmt = conn
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
    if let Err(err) = conn.execute(
            "INSERT OR REPLACE INTO envelopes (account_id, hash, date, _from, _to, cc, bcc, subject, message_id, in_reply_to, _references, flags, has_attachments, body_text, timestamp)
              VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15)",
              params![account_id, envelope.hash().to_be_bytes().to_vec(), envelope.date_as_str(), envelope.field_from_to_string(), envelope.field_to_to_string(), envelope.field_cc_to_string(), envelope.field_bcc_to_string(), envelope.subject().into_owned().trim_end_matches('\u{0}'), envelope.message_id_display().to_string(), envelope.in_reply_to_display().map(|f| f.to_string()).unwrap_or(String::new()), envelope.field_references_to_string(), i64::from(envelope.flags().bits()), if envelope.has_attachments() { 1 } else { 0 }, body, envelope.date().to_be_bytes().to_vec()],
        )
            .map_err(|e| MeliError::new(e.to_string())) {
                debug!(
                        "Failed to insert envelope {}: {}",
                        envelope.message_id_display(),
                        err.to_string()
                    );
                log(
                    format!(
                        "Failed to insert envelope {}: {}",
                        envelope.message_id_display(),
                        err.to_string()
                    ),
                    ERROR,
                );
              }
    Ok(())
}

pub fn remove(env_hash: EnvelopeHash) -> Result<()> {
    let db_path = db_path()?;
    if !db_path.exists() {
        return Err(MeliError::new(
            "Database hasn't been initialised. Run `reindex` command",
        ));
    }

    let conn = melib_sqlite3::open_db(db_path)?;
    if let Err(err) = conn
        .execute(
            "DELETE FROM envelopes WHERE hash = ?",
            params![env_hash.to_be_bytes().to_vec(),],
        )
        .map_err(|e| MeliError::new(e.to_string()))
    {
        debug!(
            "Failed to remove envelope {}: {}",
            env_hash,
            err.to_string()
        );
        log(
            format!(
                "Failed to remove envelope {}: {}",
                env_hash,
                err.to_string()
            ),
            ERROR,
        );
        return Err(err);
    }
    Ok(())
}

pub fn index(context: &mut crate::state::Context, account_index: usize) -> ResultFuture<()> {
    let account = &context.accounts[account_index];
    let (acc_name, acc_mutex, backend_mutex): (String, Arc<RwLock<_>>, Arc<_>) = (
        account.name().to_string(),
        account.collection.envelopes.clone(),
        account.backend.clone(),
    );
    let conn = melib_sqlite3::open_or_create_db(&DB, None)?;
    let env_hashes = acc_mutex
        .read()
        .unwrap()
        .keys()
        .cloned()
        .collect::<Vec<_>>();

    /* Sleep, index and repeat in order not to block the main process */
    Ok(Box::pin(async move {
        conn.execute(
            "INSERT OR REPLACE INTO accounts (name) VALUES (?1)",
            params![acc_name.as_str(),],
        )
        .chain_err_summary(|| "Failed to update index:")?;
        let account_id: i32 = {
            let mut stmt = conn
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
        let mut ctr = 0;
        debug!(
            "{}",
            format!(
                "Rebuilding {} index. {}/{}",
                acc_name,
                ctr,
                env_hashes.len()
            )
        );
        for chunk in env_hashes.chunks(200) {
            ctr += chunk.len();
            let envelopes_lck = acc_mutex.read().unwrap();
            let backend_lck = backend_mutex.read().unwrap();
            for env_hash in chunk {
                if let Some(e) = envelopes_lck.get(&env_hash) {
                    let body = backend_lck
                        .operation(e.hash())
                        .and_then(|op| e.body(op))
                        .chain_err_summary(|| {
                            format!("Failed to open envelope {}", e.message_id_display(),)
                        })?
                        .text()
                        .replace('\0', "");
                    conn.execute("INSERT OR REPLACE INTO envelopes (account_id, hash, date, _from, _to, cc, bcc, subject, message_id, in_reply_to, _references, flags, has_attachments, body_text, timestamp)
              VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15)",
              params![account_id, e.hash().to_be_bytes().to_vec(), e.date_as_str(), e.field_from_to_string(), e.field_to_to_string(), e.field_cc_to_string(), e.field_bcc_to_string(), e.subject().into_owned().trim_end_matches('\u{0}'), e.message_id_display().to_string(), e.in_reply_to_display().map(|f| f.to_string()).unwrap_or(String::new()), e.field_references_to_string(), i64::from(e.flags().bits()), if e.has_attachments() { 1 } else { 0 }, body, e.date().to_be_bytes().to_vec()],
                        ).chain_err_summary(|| format!( "Failed to insert envelope {}", e.message_id_display()))?;
                }
            }
            drop(envelopes_lck);
            let sleep_dur = std::time::Duration::from_millis(20);
            std::thread::sleep(sleep_dur);
        }
        Ok(())
    }))
}

pub fn search(
    query: &Query,
    (sort_field, sort_order): (SortField, SortOrder),
) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
    let db_path = db_path()?;
    if !db_path.exists() {
        return Err(MeliError::new(
            "Database hasn't been initialised. Run `reindex` command",
        ));
    }

    let conn = melib_sqlite3::open_db(db_path)?;

    let sort_field = match debug!(sort_field) {
        SortField::Subject => "subject",
        SortField::Date => "timestamp",
    };

    let sort_order = match debug!(sort_order) {
        SortOrder::Asc => "ASC",
        SortOrder::Desc => "DESC",
    };

    let mut stmt = conn
        .prepare(
            debug!(format!(
                "SELECT hash FROM envelopes WHERE {} ORDER BY {} {};",
                query_to_sql(&query),
                sort_field,
                sort_order
            ))
            .as_str(),
        )
        .map_err(|e| MeliError::new(e.to_string()))?;

    let results = stmt
        .query_map(rusqlite::NO_PARAMS, |row| Ok(row.get(0)?))
        .map_err(|e| MeliError::new(e.to_string()))?
        .map(|r: std::result::Result<Vec<u8>, rusqlite::Error>| {
            Ok(u64::from_be_bytes(
                r.map_err(|e| MeliError::new(e.to_string()))?
                    .as_slice()
                    .try_into()
                    .map_err(|e: std::array::TryFromSliceError| MeliError::new(e.to_string()))?,
            ))
        })
        .collect::<Result<SmallVec<[EnvelopeHash; 512]>>>();
    Ok(Box::pin(async { results }))
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
                s.push_str("(");
                rec(q1, s);
                s.push_str(") AND (");
                rec(q2, s);
                s.push_str(") ");
            }
            Or(q1, q2) => {
                s.push_str("(");
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
                    s.push_str("(");
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

#[test]
fn test_query_to_sql() {
    use melib::parsec::Parser;
    use melib::search::query;
    assert_eq!(
        "(subject LIKE \"%test%\" ) AND (body_text LIKE \"%i%\" ) ",
        &query_to_sql(&query().parse_complete("subject: test and i").unwrap().1)
    );
    assert_eq!(
        "(subject LIKE \"%github%\" ) OR ((_from LIKE \"%epilys%\" ) AND ((subject LIKE \"%lib%\" ) OR (subject LIKE \"%meli%\" ) ) ) ",
        &query_to_sql(
            &query()
                .parse_complete(
                    "subject: github or (from: epilys and (subject:lib or subject: meli))"
                )
                .unwrap()
                .1
        )
    );
}
