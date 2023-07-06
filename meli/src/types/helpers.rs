/*
 * meli
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

use std::{
    fs,
    fs::OpenOptions,
    io::{Read, Write},
    os::unix::fs::PermissionsExt,
    path::PathBuf,
};

use melib::uuid::Uuid;

#[derive(Debug)]
pub struct File {
    pub path: PathBuf,
    delete_on_drop: bool,
}

impl Drop for File {
    fn drop(&mut self) {
        if self.delete_on_drop {
            let _ = std::fs::remove_file(self.path());
        }
    }
}

impl File {
    pub fn file(&self) -> std::fs::File {
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&self.path)
            .unwrap()
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn read_to_string(&self) -> String {
        let mut buf = Vec::new();
        let mut f = fs::File::open(&self.path)
            .unwrap_or_else(|_| panic!("Can't open {}", &self.path.display()));
        f.read_to_end(&mut buf)
            .unwrap_or_else(|_| panic!("Can't read {}", &self.path.display()));
        String::from_utf8(buf).unwrap()
    }
}

/// Returned `File` will be deleted when dropped if delete_on_drop is set, so
/// make sure to add it on `context.temp_files` to reap it later.
pub fn create_temp_file(
    bytes: &[u8],
    filename: Option<&str>,
    path: Option<&mut PathBuf>,
    extension: Option<&str>,
    delete_on_drop: bool,
) -> File {
    let mut dir = std::env::temp_dir();

    let path = path.unwrap_or_else(|| {
        dir.push("meli");
        std::fs::DirBuilder::new()
            .recursive(true)
            .create(&dir)
            .unwrap();
        if let Some(filename) = filename {
            dir.push(filename)
        } else {
            let u = Uuid::new_v4();
            dir.push(u.as_simple().to_string());
        }
        &mut dir
    });
    if let Some(ext) = extension {
        path.set_extension(ext);
    }

    let mut f = std::fs::File::create(&path).unwrap();
    let metadata = f.metadata().unwrap();
    let mut permissions = metadata.permissions();

    permissions.set_mode(0o600); // Read/write for owner only.
    f.set_permissions(permissions).unwrap();

    f.write_all(bytes).unwrap();
    f.flush().unwrap();
    File {
        path: path.clone(),
        delete_on_drop,
    }
}
