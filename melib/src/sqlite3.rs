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

use crate::{error::*, logging::log};
pub use rusqlite::{self, params, Connection};
use std::path::PathBuf;

pub fn db_path(name: &str) -> Result<PathBuf> {
    let data_dir =
        xdg::BaseDirectories::with_prefix("meli").map_err(|e| MeliError::new(e.to_string()))?;
    Ok(data_dir
        .place_data_file(name)
        .map_err(|e| MeliError::new(e.to_string()))?)
}

pub fn open_db(db_path: PathBuf) -> Result<Connection> {
    if !db_path.exists() {
        return Err(MeliError::new("Database doesn't exist"));
    }
    Connection::open(&db_path).map_err(|e| MeliError::new(e.to_string()))
}

pub fn open_or_create_db(name: &str, init_script: Option<&str>) -> Result<Connection> {
    let db_path = db_path(name)?;
    let mut set_mode = false;
    if !db_path.exists() {
        log(
            format!("Creating {} database in {}", name, db_path.display()),
            crate::INFO,
        );
        set_mode = true;
    }
    let conn = Connection::open(&db_path).map_err(|e| MeliError::new(e.to_string()))?;
    if set_mode {
        use std::os::unix::fs::PermissionsExt;
        let file = std::fs::File::open(&db_path)?;
        let metadata = file.metadata()?;
        let mut permissions = metadata.permissions();

        permissions.set_mode(0o600); // Read/write for owner only.
        file.set_permissions(permissions)?;
    }

    if let Some(s) = init_script {
        conn.execute_batch(s)
            .map_err(|e| MeliError::new(e.to_string()))?;
    }

    Ok(conn)
}
