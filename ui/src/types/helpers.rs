/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

use std;
use std::io::Write;
use std::path::PathBuf;

use uuid::Uuid;

#[derive(Debug)]
pub struct File {
    path: PathBuf,
}

impl Drop for File {
    fn drop(&mut self) {
        std::fs::remove_file(self.path()).unwrap_or_else(|_| {});
    }
}

impl File {
    pub fn file(&mut self) -> std::fs::File {
        std::fs::File::create(&self.path).unwrap()
    }
    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}

/// Returned `File` will be deleted when dropped, so make sure to add it on `context.temp_files`
/// to reap it later.
pub fn create_temp_file(bytes: &[u8], filename: Option<&PathBuf>) -> File {
    let mut dir = std::env::temp_dir();

    let path = if let Some(p) = filename {
        p
    } else {
        dir.push("meli");
        std::fs::DirBuilder::new()
            .recursive(true)
            .create(&dir)
            .unwrap();
        let u = Uuid::new_v4();
        dir.push(u.hyphenated().to_string());
        &dir
    };

    let mut f = std::fs::File::create(path).unwrap();

    f.write_all(bytes).unwrap();
    f.flush().unwrap();
    File { path: path.clone() }
}
