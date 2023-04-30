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

use std::path::PathBuf;

use rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput};
pub use rusqlite::{self, params, Connection};

use crate::{error::*, logging::log, Envelope};

#[derive(Copy, Clone, Debug)]
pub struct DatabaseDescription {
    pub name: &'static str,
    pub init_script: Option<&'static str>,
    pub version: u32,
}

pub fn db_path(name: &str) -> Result<PathBuf> {
    let data_dir =
        xdg::BaseDirectories::with_prefix("meli").map_err(|e| Error::new(e.to_string()))?;
    data_dir
        .place_data_file(name)
        .map_err(|err| Error::new(err.to_string()))
}

pub fn open_db(db_path: PathBuf) -> Result<Connection> {
    if !db_path.exists() {
        return Err(Error::new("Database doesn't exist"));
    }
    Connection::open(&db_path).map_err(|e| Error::new(e.to_string()))
}

pub fn open_or_create_db(
    description: &DatabaseDescription,
    identifier: Option<&str>,
) -> Result<Connection> {
    let mut second_try: bool = false;
    loop {
        let db_path = if let Some(id) = identifier {
            db_path(&format!("{}_{}", id, description.name))
        } else {
            db_path(description.name)
        }?;
        let mut set_mode = false;
        if !db_path.exists() {
            log(
                format!(
                    "Creating {} database in {}",
                    description.name,
                    db_path.display()
                ),
                crate::INFO,
            );
            set_mode = true;
        }
        let conn = Connection::open(&db_path).map_err(|e| Error::new(e.to_string()))?;
        if set_mode {
            use std::os::unix::fs::PermissionsExt;
            let file = std::fs::File::open(&db_path)?;
            let metadata = file.metadata()?;
            let mut permissions = metadata.permissions();

            permissions.set_mode(0o600); // Read/write for owner only.
            file.set_permissions(permissions)?;
        }
        let version: i32 = conn.pragma_query_value(None, "user_version", |row| row.get(0))?;
        if version != 0_i32 && version as u32 != description.version {
            log(
                format!(
                    "Database version mismatch, is {} but expected {}",
                    version, description.version
                ),
                crate::INFO,
            );
            if second_try {
                return Err(Error::new(format!(
                    "Database version mismatch, is {} but expected {}. Could not recreate \
                     database.",
                    version, description.version
                )));
            }
            reset_db(description, identifier)?;
            second_try = true;
            continue;
        }

        if version == 0 {
            conn.pragma_update(None, "user_version", description.version)?;
        }
        if let Some(s) = description.init_script {
            conn.execute_batch(s)
                .map_err(|e| Error::new(e.to_string()))?;
        }

        return Ok(conn);
    }
}

/// Return database to a clean slate.
pub fn reset_db(description: &DatabaseDescription, identifier: Option<&str>) -> Result<()> {
    let db_path = if let Some(id) = identifier {
        db_path(&format!("{}_{}", id, description.name))
    } else {
        db_path(description.name)
    }?;
    if !db_path.exists() {
        return Ok(());
    }
    log(
        format!(
            "Resetting {} database in {}",
            description.name,
            db_path.display()
        ),
        crate::INFO,
    );
    std::fs::remove_file(&db_path)?;
    Ok(())
}

impl ToSql for Envelope {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput> {
        let v: Vec<u8> = bincode::Options::serialize(bincode::config::DefaultOptions::new(), self)
            .map_err(|e| {
                rusqlite::Error::ToSqlConversionFailure(Box::new(Error::new(e.to_string())))
            })?;
        Ok(ToSqlOutput::from(v))
    }
}

impl FromSql for Envelope {
    fn column_result(value: rusqlite::types::ValueRef) -> FromSqlResult<Self> {
        use std::convert::TryFrom;

        let b: Vec<u8> = FromSql::column_result(value)?;

        bincode::Options::deserialize(
            bincode::Options::with_limit(
                bincode::config::DefaultOptions::new(),
                2 * u64::try_from(b.len()).map_err(|e| FromSqlError::Other(Box::new(e)))?,
            ),
            &b,
        )
        .map_err(|e| FromSqlError::Other(Box::new(e)))
    }
}
