/*
 * meli - lib.rs
 *
 * Copyright 2017 Manos Pitsidianakis
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

//! A `ShellExpandTrait` to expand paths like a shell.

use std::{
    ffi::OsStr,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

use smallvec::SmallVec;

pub trait ShellExpandTrait {
    fn expand(&self) -> PathBuf;
    fn complete(&self, force: bool) -> SmallVec<[String; 128]>;
}

impl ShellExpandTrait for Path {
    fn expand(&self) -> PathBuf {
        let mut ret = PathBuf::new();
        for c in self.components() {
            let c_to_str = c.as_os_str().to_str();
            match c_to_str {
                Some("~") => {
                    if let Ok(home_dir) = std::env::var("HOME") {
                        ret.push(home_dir)
                    } else {
                        return PathBuf::new();
                    }
                }
                Some(var) if var.starts_with('$') => {
                    let env_name = var.split_at(1).1;
                    if env_name.chars().all(char::is_uppercase) {
                        ret.push(std::env::var(env_name).unwrap_or_default());
                    } else {
                        ret.push(c);
                    }
                }
                Some(_) => {
                    ret.push(c);
                }
                None => {
                    /* path is invalid */
                    return PathBuf::new();
                }
            }
        }
        ret
    }

    #[cfg(target_os = "linux")]
    fn complete(&self, force: bool) -> SmallVec<[String; 128]> {
        use std::os::unix::io::AsRawFd;

        use libc::dirent64;
        use nix::fcntl::OFlag;

        const BUF_SIZE: ::libc::size_t = 8 << 10;

        let (prefix, _match) = if self.as_os_str().as_bytes().ends_with(b"/.") {
            (self.components().as_path(), OsStr::from_bytes(b"."))
        } else if self.exists() && (!force || self.as_os_str().as_bytes().ends_with(b"/")) {
            return SmallVec::new();
        } else {
            let last_component = self
                .components()
                .last()
                .map(|c| c.as_os_str())
                .unwrap_or_else(|| OsStr::from_bytes(b""));
            let prefix = if let Some(p) = self.parent() {
                p
            } else {
                return SmallVec::new();
            };
            (prefix, last_component)
        };

        let dir = match ::nix::dir::Dir::openat(
            ::libc::AT_FDCWD,
            prefix,
            OFlag::O_DIRECTORY | OFlag::O_NOATIME | OFlag::O_RDONLY | OFlag::O_CLOEXEC,
            ::nix::sys::stat::Mode::S_IRUSR | ::nix::sys::stat::Mode::S_IXUSR,
        )
        .or_else(|_| {
            ::nix::dir::Dir::openat(
                ::libc::AT_FDCWD,
                prefix,
                OFlag::O_DIRECTORY | OFlag::O_RDONLY | OFlag::O_CLOEXEC,
                ::nix::sys::stat::Mode::S_IRUSR | ::nix::sys::stat::Mode::S_IXUSR,
            )
        }) {
            Ok(dir) => dir,
            Err(err) => {
                debug!(prefix);
                debug!(err);
                return SmallVec::new();
            }
        };

        let mut buf: Vec<u8> = Vec::with_capacity(BUF_SIZE);
        let mut entries = SmallVec::new();
        loop {
            let n: i64 = unsafe {
                ::libc::syscall(
                    ::libc::SYS_getdents64,
                    dir.as_raw_fd(),
                    buf.as_ptr(),
                    BUF_SIZE - 256,
                )
            };

            let Ok(n) = usize::try_from(n) else {
                return SmallVec::new();
            };
            if n == 0 {
                break;
            }
            unsafe {
                buf.set_len(n);
            }
            let mut pos = 0;
            while pos < n {
                let dir = unsafe { std::mem::transmute::<&[u8], &[dirent64]>(&buf[pos..]) };
                let entry = unsafe { std::ffi::CStr::from_ptr(dir[0].d_name.as_ptr()) };
                if entry.to_bytes() != b"."
                    && entry.to_bytes() != b".."
                    && entry.to_bytes().starts_with(_match.as_bytes())
                {
                    if dir[0].d_type == ::libc::DT_DIR && !entry.to_bytes().ends_with(b"/") {
                        let mut s = unsafe {
                            String::from_utf8_unchecked(
                                entry.to_bytes()[_match.as_bytes().len()..].to_vec(),
                            )
                        };
                        s.push('/');
                        entries.push(s);
                    } else {
                        entries.push(unsafe {
                            String::from_utf8_unchecked(
                                entry.to_bytes()[_match.as_bytes().len()..].to_vec(),
                            )
                        });
                    }
                }
                pos += dir[0].d_reclen as usize;
            }
            // https://github.com/romkatv/gitstatus/blob/caf44f7aaf33d0f46e6749e50595323c277e0908/src/dir.cc
            // "It's tempting to bail here if n + sizeof(linux_dirent64) +
            // 512 <= n. After all, there was enough space
            // for another entry but SYS_getdents64 didn't write it, so this
            // must be the end of the directory listing,
            // right? Unfortunately, no. SYS_getdents64 is finicky.
            // It sometimes writes a partial list of entries even if the
            // full list would fit."
        }
        entries
    }

    #[cfg(not(target_os = "linux"))]
    fn complete(&self, force: bool) -> SmallVec<[String; 128]> {
        let mut entries = SmallVec::new();
        let (prefix, _match) = {
            if self.exists() && (!force || self.as_os_str().as_bytes().ends_with(b"/")) {
                // println!("{} {:?}", self.display(), self.components().last());
                return entries;
            } else {
                let last_component = self
                    .components()
                    .last()
                    .map(|c| c.as_os_str())
                    .unwrap_or_else(|| OsStr::from_bytes(b""));
                let prefix = if let Some(p) = self.parent() {
                    p
                } else {
                    return entries;
                };
                (prefix, last_component)
            }
        };
        if force && self.is_dir() && !self.as_os_str().as_bytes().ends_with(b"/") {
            entries.push("/".to_string());
        }

        if let Ok(iter) = std::fs::read_dir(prefix) {
            for entry in iter.flatten() {
                if entry.path().as_os_str().as_bytes() != b"."
                    && entry.path().as_os_str().as_bytes() != b".."
                    && entry
                        .path()
                        .as_os_str()
                        .as_bytes()
                        .starts_with(_match.as_bytes())
                {
                    if entry.path().is_dir() && !entry.path().as_os_str().as_bytes().ends_with(b"/")
                    {
                        let mut s = unsafe {
                            String::from_utf8_unchecked(
                                entry.path().as_os_str().as_bytes()[_match.as_bytes().len()..]
                                    .to_vec(),
                            )
                        };
                        s.push('/');
                        entries.push(s);
                    } else {
                        entries.push(unsafe {
                            String::from_utf8_unchecked(
                                entry.path().as_os_str().as_bytes()[_match.as_bytes().len()..]
                                    .to_vec(),
                            )
                        });
                    }
                }
            }
        }
        entries
    }
}

#[test]
fn test_shellexpandtrait() {
    assert!(Path::new("~").expand().complete(false).is_empty());
    assert!(!Path::new("~").expand().complete(true).is_empty());
}
