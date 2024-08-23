/*
 * meli - melib
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

use std::{
    borrow::Cow,
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
    sync::Arc,
};

use rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput};
pub use rusqlite::{self, config::DbConfig, params, Connection};

use crate::{error::*, log, Envelope};

/// A description for creating, opening and handling application databases.
#[derive(Clone, Debug)]
pub struct DatabaseDescription {
    /// A name that represents the function of this database, e.g.
    /// `headers_cache`, `contacts`, `settings`, etc.
    pub name: &'static str,
    /// An optional identifier string that along with
    /// [`DatabaseDescription::name`] makes a specialized identifier for the
    /// database. E.g. an account name, a date, etc.
    pub identifier: Option<Cow<'static, str>>,
    /// The name of the application to use when storing the database in `XDG`
    /// directories, used for when the consumer application is not `meli`
    /// itself.
    pub application_prefix: &'static str,
    /// Optionally override file system location instead of saving at `XDG` data
    /// directory.
    pub directory: Option<Cow<'static, Path>>,
    /// A script that initializes the schema of the database.
    pub init_script: Option<&'static str>,
    /// The current value of the `user_version` `PRAGMA` of the `sqlite3`
    /// database, used for schema versioning.
    pub version: u32,
}

impl DatabaseDescription {
    /// Returns whether the computed database path for this description exist.
    pub fn exists(&self) -> Result<bool> {
        let path = self.db_path()?;
        Ok(path.exists())
    }

    /// Returns the computed database path for this description.
    pub fn db_path(&self) -> Result<PathBuf> {
        let name: Cow<'static, str> = self.identifier.as_ref().map_or_else(
            || self.name.into(),
            |id| format!("{}_{}", id, self.name).into(),
        );

        for (field_name, field_value) in [
            ("name", self.name),
            ("identifier", self.identifier.as_deref().unwrap_or_default()),
            ("application_prefix", self.application_prefix),
        ] {
            if field_value.contains(std::path::MAIN_SEPARATOR_STR) {
                return Err(Error::new(format!(
                    "Database description for `{}{}{}` field {} cannot contain current platform's \
                     path separator {}. Got: {}.",
                    self.identifier.as_deref().unwrap_or_default(),
                    if self.identifier.is_none() { "" } else { ":" },
                    self.name,
                    field_name,
                    std::path::MAIN_SEPARATOR_STR,
                    field_value,
                ))
                .set_kind(ErrorKind::ValueError));
            }
        }

        if let Some(directory) = self.directory.as_deref() {
            if !directory.is_dir() {
                return Err(Error::new(format!(
                    "Database description for `{}{}{}` expects a valid directory path value. Got: \
                     {}.",
                    self.identifier.as_deref().unwrap_or_default(),
                    if self.identifier.is_none() { "" } else { ":" },
                    self.name,
                    directory.display()
                ))
                .set_kind(ErrorKind::ValueError));
            }
            return Ok(directory.join(name.as_ref()));
        }
        let data_dir =
            xdg::BaseDirectories::with_prefix(self.application_prefix).map_err(|err| {
                Error::new(format!(
                    "Could not create sqlite3 database file for `{}{}{}` in XDG data directory.",
                    self.identifier.as_deref().unwrap_or_default(),
                    if self.identifier.is_none() { "" } else { ":" },
                    self.name,
                ))
                .set_details(format!(
                    "Could not open XDG data directory with prefix {}",
                    self.application_prefix
                ))
                .set_kind(ErrorKind::Platform)
                .set_source(Some(Arc::new(err)))
            })?;
        data_dir.place_data_file(name.as_ref()).map_err(|err| {
            Error::new(format!(
                "Could not create sqlite3 database file for `{}{}{}` in XDG data directory.",
                self.identifier.as_deref().unwrap_or_default(),
                if self.identifier.is_none() { "" } else { ":" },
                self.name,
            ))
            .set_kind(ErrorKind::Platform)
            .set_source(Some(Arc::new(err)))
        })
    }

    /// Returns an [`rusqlite::Connection`] for this description.
    pub fn open_or_create_db(&self) -> Result<Connection> {
        let mut second_try: bool = false;
        let db_path = self.db_path()?;
        let set_mode = !db_path.exists();
        if set_mode {
            log::info!("Creating {} database in {}", self.name, db_path.display());
        }
        loop {
            let mut inner_fn = || {
                let conn = Connection::open(&db_path)?;
                conn.busy_timeout(std::time::Duration::new(10, 0))?;
                for conf_flag in [
                    DbConfig::SQLITE_DBCONFIG_ENABLE_FKEY,
                    DbConfig::SQLITE_DBCONFIG_ENABLE_TRIGGER,
                ]
                .into_iter()
                {
                    conn.set_db_config(conf_flag, true)?;
                }
                rusqlite::vtab::array::load_module(&conn)?;
                if set_mode {
                    let file = std::fs::File::open(&db_path)?;
                    let metadata = file.metadata()?;
                    let mut permissions = metadata.permissions();

                    permissions.set_mode(0o600); // Read/write for owner only.
                    file.set_permissions(permissions)?;
                }
                let _: String =
                    conn.pragma_update_and_check(None, "journal_mode", "WAL", |row| row.get(0))?;
                let version: i32 =
                    conn.pragma_query_value(None, "user_version", |row| row.get(0))?;
                if version != 0_i32 && version as u32 != self.version {
                    log::info!(
                        "Database version mismatch, is {} but expected {}. Attempting to recreate \
                         database.",
                        version,
                        self.version
                    );
                    if second_try {
                        return Err(Error::new(format!(
                            "Database version mismatch, is {} but expected {}. Could not recreate \
                             database.",
                            version, self.version
                        )));
                    }
                    self.reset_db()?;
                    second_try = true;
                    return Ok(conn);
                }

                if version == 0 {
                    conn.pragma_update(None, "user_version", self.version)?;
                }
                if let Some(s) = self.init_script {
                    conn.execute_batch(s)
                        .map_err(|err| Error::new(err.to_string()))?;
                }

                Ok(conn)
            };
            inner_fn().unwrap();
            match inner_fn() {
                Ok(_) if second_try => continue,
                Ok(conn) => return Ok(conn),
                Err(err) => {
                    return Err(Error::new(format!(
                        "{}: Could not open or create database",
                        db_path.display()
                    ))
                    .set_source(Some(Arc::new(err))))
                }
            }
        }
    }

    /// Reset database to a clean slate.
    pub fn reset_db(&self) -> Result<()> {
        let db_path = self.db_path()?;
        if !db_path.exists() {
            return Ok(());
        }
        log::info!("Resetting {} database in {}", self.name, db_path.display());
        std::fs::remove_file(&db_path).map_err(|err| {
            Error::new(format!("{}: could not remove file", db_path.display()))
                .set_kind(ErrorKind::from(err.kind()))
                .set_source(Some(Arc::new(err)))
        })?;
        Ok(())
    }
}

impl ToSql for Envelope {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput> {
        let v: Vec<u8> = serde_json::to_vec(self).map_err(|e| {
            rusqlite::Error::ToSqlConversionFailure(Box::new(Error::new(e.to_string())))
        })?;
        Ok(ToSqlOutput::from(v))
    }
}

impl FromSql for Envelope {
    fn column_result(value: rusqlite::types::ValueRef) -> FromSqlResult<Self> {
        let b: Vec<u8> = FromSql::column_result(value)?;

        serde_json::from_slice(&b).map_err(|e| FromSqlError::Other(Box::new(e)))
    }
}
