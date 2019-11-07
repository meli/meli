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

use crate::cache::query;
use crate::cache::Query::{self, *};
use crate::melib::parsec::Parser;
use melib::{
    backends::MailBackend,
    email::{Envelope, EnvelopeHash},
    log,
    thread::{SortField, SortOrder},
    MeliError, Result, StackVec, ERROR,
};
use rusqlite::{params, Connection};
use std::borrow::Cow;
use std::convert::TryInto;
use std::sync::{Arc, RwLock};

#[inline(always)]
fn escape_double_quote(w: &str) -> Cow<str> {
    if w.contains('"') {
        Cow::from(w.replace('"', "\"\""))
    } else {
        Cow::from(w)
    }
}

#[inline(always)]
fn fts5_bareword(w: &str) -> Cow<str> {
    if w == "AND" || w == "OR" || w == "NOT" {
        Cow::from(w)
    } else {
        if !w.is_ascii() {
            Cow::from(format!("\"{}\"", escape_double_quote(w)))
        } else {
            for &b in w.as_bytes() {
                if !(b > 0x2f && b < 0x3a)
                    || !(b > 0x40 && b < 0x5b)
                    || !(b > 0x60 && b < 0x7b)
                    || b != 0x60
                    || b != 26
                {
                    return Cow::from(format!("\"{}\"", escape_double_quote(w)));
                }
            }
            Cow::from(w)
        }
    }
}

pub fn open_db() -> Result<Connection> {
    let data_dir =
        xdg::BaseDirectories::with_prefix("meli").map_err(|e| MeliError::new(e.to_string()))?;
    let db_path = data_dir
        .place_data_file("index.db")
        .map_err(|e| MeliError::new(e.to_string()))?;
    if !db_path.exists() {
        log(
            format!("Creating index database in {}", db_path.display()),
            melib::INFO,
        );
    }
    let conn = Connection::open(db_path).map_err(|e| MeliError::new(e.to_string()))?;
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS envelopes (
                    id               INTEGER PRIMARY KEY,
                    hash             BLOB NOT NULL,
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
                    hash             BLOB NOT NULL,
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
END; ",
    )
    .map_err(|e| MeliError::new(e.to_string()))?;

    Ok(conn)
}

pub fn insert(envelope: &Envelope, backend: &Arc<RwLock<Box<dyn MailBackend>>>) -> Result<()> {
    let conn = open_db()?;
    let backend_lck = backend.read().unwrap();
    let op = backend_lck.operation(envelope.hash());
    let body = match envelope.body(op) {
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
            "INSERT OR REPLACE INTO envelopes (hash, date, _from, _to, cc, bcc, subject, message_id, in_reply_to, _references, flags, has_attachments, body_text, timestamp)
              VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14)",
              params![envelope.hash().to_be_bytes().to_vec(), envelope.date_as_str(), envelope.field_from_to_string(), envelope.field_to_to_string(), envelope.field_cc_to_string(), envelope.field_bcc_to_string(), envelope.subject().into_owned().trim_end_matches('\u{0}'), envelope.message_id_display().to_string(), envelope.in_reply_to_display().map(|f| f.to_string()).unwrap_or(String::new()), envelope.field_references_to_string(), i64::from(envelope.flags().bits()), if envelope.has_attachments() { 1 } else { 0 }, body, envelope.date().to_be_bytes().to_vec()],
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
pub fn index(context: &mut crate::state::Context) -> Result<()> {
    let conn = open_db()?;
    let work_context = context.work_controller().get_context();
    let mutexes = context
        .accounts
        .iter()
        .filter(|acc| acc.settings.account().format() != "imap")
        .map(|acc| (acc.collection.envelopes.clone(), acc.backend.clone()))
        .collect::<Vec<(Arc<RwLock<_>>, Arc<_>)>>();
    let env_hashes = mutexes
        .iter()
        .map(|m| m.0.read().unwrap().keys().cloned().collect::<Vec<_>>())
        .collect::<Vec<Vec<_>>>();

    /* Sleep, index and repeat in order not to block the main process */
    let handle = std::thread::Builder::new().name(String::from("rebuilding index")).spawn(move || {
        let thread_id = std::thread::current().id();

        let sleep_dur = std::time::Duration::from_millis(20);
        for ((acc_mutex, backend_mutex), env_hashes) in mutexes.into_iter().zip(env_hashes.into_iter())  {
            let mut ctr = 0;
            debug!("{}", format!("Rebuilding index. {}/{}", ctr, env_hashes.len()));
            work_context
                .set_status
                .send((thread_id, format!("Rebuilding index. {}/{}", ctr, env_hashes.len())))
                .unwrap();
            for chunk in env_hashes.chunks(200) {
                ctr += chunk.len();
                let envelopes_lck = acc_mutex.read().unwrap();
                let backend_lck = backend_mutex.read().unwrap();
                for env_hash in chunk {
                    if let Some(e) = envelopes_lck.get(&env_hash) {
                        let op = backend_lck.operation(e.hash());
                        let body = match e.body(op) {
                            Ok(body) => body.text(),
                            Err(err) => {
                                debug!("{}",
                                    format!(
                                        "Failed to open envelope {}: {}",
                                        e.message_id_display(),
                                        err.to_string()
                                    ));
                                log(
                                    format!(
                                        "Failed to open envelope {}: {}",
                                        e.message_id_display(),
                                        err.to_string()
                                    ),
                                    ERROR,
                                );
                                return;
                            }
                        };
                        if let Err(err) = conn.execute(
                            "INSERT OR REPLACE INTO envelopes (hash, date, _from, _to, cc, bcc, subject, message_id, in_reply_to, _references, flags, has_attachments, body_text, timestamp)
              VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14)",
              params![e.hash().to_be_bytes().to_vec(), e.date_as_str(), e.field_from_to_string(), e.field_to_to_string(), e.field_cc_to_string(), e.field_bcc_to_string(), e.subject().into_owned().trim_end_matches('\u{0}'), e.message_id_display().to_string(), e.in_reply_to_display().map(|f| f.to_string()).unwrap_or(String::new()), e.field_references_to_string(), i64::from(e.flags().bits()), if e.has_attachments() { 1 } else { 0 }, body, e.date().to_be_bytes().to_vec()],
                        )
                            .map_err(|e| MeliError::new(e.to_string())) {
                                debug!("{}",
                                    format!(
                                        "Failed to insert envelope {}: {}",
                                        e.message_id_display(),
                                        err.to_string()
                                    ));
                                log(
                                    format!(
                                        "Failed to insert envelope {}: {}",
                                        e.message_id_display(),
                                        err.to_string()
                                    ),
                                    ERROR,
                                );
              }
                    }
                }
                drop(envelopes_lck);
                work_context
                    .set_status
                    .send((thread_id, format!("Rebuilding index. {}/{}", ctr, env_hashes.len())))
                    .unwrap();
                std::thread::sleep(sleep_dur);
            }
        }
        work_context.finished.send(thread_id).unwrap();
    })?;
    context.work_controller().static_threads.lock()?.insert(
        handle.thread().id(),
        String::from("Rebuilding sqlite3 index").into(),
    );

    Ok(())
}

pub fn search(
    term: &str,
    (sort_field, sort_order): (SortField, SortOrder),
) -> Result<StackVec<EnvelopeHash>> {
    let conn = open_db()?;

    let sort_field = match debug!(sort_field) {
        SortField::Subject => "subject",
        SortField::Date => "timestamp",
    };

    let sort_order = match debug!(sort_order) {
        SortOrder::Asc => "ASC",
        SortOrder::Desc => "DESC",
    };

    /*
    debug!("SELECT hash FROM envelopes INNER JOIN fts ON fts.rowid = envelopes.id WHERE fts MATCH ? ORDER BY {} {};", sort_field, sort_order);
    let mut stmt = conn.prepare(
                format!("SELECT hash FROM envelopes INNER JOIN fts ON fts.rowid = envelopes.id WHERE fts MATCH ? ORDER BY {} {};", sort_field, sort_order).as_str())
    */
    let mut stmt = conn
        .prepare(
            debug!(format!(
                "SELECT hash FROM envelopes WHERE {} ORDER BY {} {};",
                query_to_sql(&query().parse(term)?.1),
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
        .collect::<Result<StackVec<EnvelopeHash>>>();
    results
}

pub fn from(term: &str) -> Result<StackVec<EnvelopeHash>> {
    let conn = open_db()?;
    let mut stmt = conn
        .prepare("SELECT hash FROM envelopes WHERE _from LIKE ?;")
        .map_err(|e| MeliError::new(e.to_string()))?;

    let results = stmt
        .query_map(&[term.trim()], |row| Ok(row.get(0)?))
        .map_err(|e| MeliError::new(e.to_string()))?
        .map(|r: std::result::Result<Vec<u8>, rusqlite::Error>| {
            Ok(u64::from_be_bytes(
                r.map_err(|e| MeliError::new(e.to_string()))?
                    .as_slice()
                    .try_into()
                    .map_err(|e: std::array::TryFromSliceError| MeliError::new(e.to_string()))?,
            ))
        })
        .collect::<Result<StackVec<EnvelopeHash>>>();
    results
}

pub fn query_to_sql(q: &Query) -> String {
    fn rec(q: &Query, s: &mut String) {
        match q {
            Subject(t) => {
                s.push_str(" subject LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\"");
            }
            From(t) => {
                s.push_str(" _from LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\"");
            }
            AllText(t) => {
                s.push_str(" body_text LIKE \"%");
                s.extend(escape_double_quote(t).chars());
                s.push_str("%\"");
            }
            And(q1, q2) => {
                s.push_str(" (");
                rec(q1, s);
                s.push_str(") ");

                s.push_str(" AND ");
                s.push_str(" (");
                rec(q2, s);
                s.push_str(") ");
            }
            Or(q1, q2) => {
                s.push_str(" (");
                rec(q1, s);
                s.push_str(") ");
                s.push_str(" OR ");
                s.push_str(" (");
                rec(q2, s);
                s.push_str(") ");
            }
            Not(q) => {
                s.push_str(" NOT ");
                s.push_str("(");
                rec(q, s);
                s.push_str(")");
            }
            _ => {}
        }
    }
    let mut ret = String::new();
    rec(q, &mut ret);
    ret

    //"SELECT hash FROM envelopes INNER JOIN fts ON fts.rowid = envelopes.id WHERE fts MATCH ? ORDER BY {} {};"
}

#[test]
fn test_query_to_sql() {
    assert_eq!(
        " subject LIKE \"%test%\" AND  body_text LIKE \"%i%\"",
        &query_to_sql(&query().parse_complete("subject: test and i").unwrap().1)
    );
    assert_eq!(
        " subject LIKE \"%github%\" OR  ( _from LIKE \"%epilys%\" AND  ( subject LIKE \"%lib%\" OR  subject LIKE \"%meli%\") ) ",
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
