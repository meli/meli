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
    os::{
        fd::{FromRawFd, OwnedFd},
        unix::fs::PermissionsExt,
    },
    path::{Path, PathBuf},
};

use melib::{error::*, uuid::Uuid, ShellExpandTrait};

/// Temporary file that can optionally cleaned up when it is dropped.
#[derive(Debug)]
pub struct File {
    /// File's path.
    path: PathBuf,
    /// Delete file when it is dropped.
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
    /// Open as a standard library file type.
    pub fn as_std_file(&self) -> Result<std::fs::File> {
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&self.path)
            .chain_err_summary(|| format!("Could not create/open path {}", self.path.display()))
    }

    /// The file's path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Convenience method to read `File` to `String`.
    pub fn read_to_string(&self) -> Result<String> {
        fn inner(path: &Path) -> Result<String> {
            let mut buf = Vec::new();
            let mut f = fs::File::open(path)?;
            f.read_to_end(&mut buf)?;
            Ok(String::from_utf8(buf)?)
        }
        inner(&self.path).chain_err_summary(|| format!("Can't read {}", self.path.display()))
    }

    /// Returned `File` will be deleted when dropped if `delete_on_drop` is set,
    /// so make sure to add it on `context.temp_files` to reap it later.
    pub fn create_temp_file(
        bytes: &[u8],
        filename: Option<&str>,
        path: Option<&mut PathBuf>,
        extension: Option<&str>,
        delete_on_drop: bool,
    ) -> Result<Self> {
        let mut dir = std::env::temp_dir();

        let path = if let Some(p) = path {
            p
        } else {
            dir.push("meli");
            std::fs::DirBuilder::new().recursive(true).create(&dir)?;
            if let Some(filename) = filename {
                dir.push(filename)
            } else {
                let u = Uuid::new_v4();
                dir.push(u.as_simple().to_string());
            }
            &mut dir
        };
        if let Some(ext) = extension {
            path.set_extension(ext);
        }
        fn inner(path: &Path, bytes: &[u8], delete_on_drop: bool) -> Result<File> {
            let path = path.expand();
            let mut f = std::fs::File::create(&path)?;
            let metadata = f.metadata()?;
            let mut permissions = metadata.permissions();

            permissions.set_mode(0o600); // Read/write for owner only.
            f.set_permissions(permissions)?;

            f.write_all(bytes)?;
            f.flush()?;
            Ok(File {
                path,
                delete_on_drop,
            })
        }
        inner(path, bytes, delete_on_drop)
            .chain_err_summary(|| format!("Could not create file at path {}", path.display()))
    }
}

pub fn pipe() -> Result<(OwnedFd, OwnedFd)> {
    nix::unistd::pipe()
        .map(|(fd1, fd2)| unsafe { (OwnedFd::from_raw_fd(fd1), OwnedFd::from_raw_fd(fd2)) })
        .map_err(|err| {
            Error::new("Could not create pipe")
                .set_source(Some(
                    (Box::new(err) as Box<dyn std::error::Error + Send + Sync + 'static>).into(),
                ))
                .set_kind(ErrorKind::OSError)
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_invalid_path() {
        let f = File {
            path: PathBuf::from("//////"),
            delete_on_drop: true,
        };
        f.as_std_file().unwrap_err();
    }

    #[test]
    fn test_file_delete_on_drop() {
        const S: &str = "hello world";
        let tempdir = tempfile::tempdir().unwrap();

        let delete_on_drop = File::create_temp_file(
            S.as_bytes(),
            None,
            Some(&mut tempdir.path().join("test")),
            None,
            true,
        )
        .unwrap();
        assert_eq!(&delete_on_drop.read_to_string().unwrap(), S);
        drop(delete_on_drop);
        assert!(!tempdir.path().join("test").try_exists().unwrap());

        let persist = File::create_temp_file(
            S.as_bytes(),
            None,
            Some(&mut tempdir.path().join("test")),
            None,
            false,
        )
        .unwrap();
        assert_eq!(&persist.read_to_string().unwrap(), S);
        drop(persist);
        assert!(tempdir.path().join("test").try_exists().unwrap());

        _ = tempdir.close();
    }
}
