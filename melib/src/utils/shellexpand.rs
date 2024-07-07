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

//! A [`ShellExpandTrait`] to expand paths like a shell.

use std::{
    ffi::OsStr,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

/// The return type of [`ShellExpandTrait`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Completions {
    /// Completion entries, e.g. strings that if appended to the path being
    /// completed, would result in all valid paths that exist with the path
    /// as a prefix.
    Entries(Vec<String>),
    /// Path is a file and there are no completions.
    IsFile,
    /// Path is a directory and there are no completions.
    IsDirectory,
    /// Path is invalid and there are no completions.
    IsInvalid,
}

impl IntoIterator for Completions {
    type Item = String;
    type IntoIter = <Vec<String> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Entries(e) => e.into_iter(),
            _ => Vec::default().into_iter(),
        }
    }
}

/// Utility trait to expand paths like an interactive shell does.
pub trait ShellExpandTrait {
    /// Expands `~` to the content of `${HOME}` and environment variables to
    /// their content.
    fn expand(&self) -> PathBuf;
    /// Returns what completions are available for this path depending on the
    /// input options given.
    ///
    /// `force`: whether to force completion if `self` is a directory. If
    /// `false` and `self` is a directory, it returns
    /// `Completions::IsDirectory`. Otherwise the return value depends on
    /// the value of `treat_as_dir`.
    /// `treat_as_dir`: whether to treat the last path component in `self` as a
    /// pattern or a directory name.
    fn complete(&self, force: bool, treat_as_dir: bool) -> Completions;
}

impl ShellExpandTrait for Path {
    fn expand(&self) -> PathBuf {
        // [ref:TODO]: ShellExpandTrait: add support for parameters in braces ${ }
        // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_06_02
        let mut ret = PathBuf::new();
        for c in self.components() {
            let c_to_str = c.as_os_str();
            match c_to_str {
                tilde if tilde == "~" && ret.as_os_str().is_empty() => {
                    if let Ok(home_dir) = std::env::var("HOME") {
                        ret.push(home_dir);
                    } else {
                        // POSIX says that if HOME is unset, the results of tilde expansion is
                        // unspecified.
                        // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_06_01
                        // Abort expansion.
                        return self.to_path_buf();
                    }
                }
                var if var.to_string_lossy().starts_with('$') => {
                    let var = var.to_string_lossy();
                    let env_name = var.split_at(1).1;
                    if env_name.chars().all(char::is_uppercase) {
                        ret.push(std::env::var(env_name).unwrap_or_default());
                    } else {
                        ret.push(c);
                    }
                }
                _ => {
                    ret.push(c);
                }
            }
        }
        ret
    }

    fn complete(&self, force: bool, treat_as_dir: bool) -> Completions {
        #[cfg(not(target_os = "linux"))]
        {
            impls::inner_complete_generic(self, force, treat_as_dir)
        }
        #[cfg(target_os = "linux")]
        {
            impls::inner_complete_linux(self, force, treat_as_dir)
        }
    }
}

pub mod impls {
    //! Inner implementations for [`ShellExpandTrait`]'s complete function.
    use super::*;

    /// Completing implementation for all platforms.
    pub fn inner_complete_generic(_self: &Path, force: bool, treat_as_dir: bool) -> Completions {
        let mut entries = Vec::new();
        let (prefix, _match) = {
            if _self.exists()
                && !treat_as_dir
                && (!force || _self.as_os_str().as_bytes().ends_with(b"/"))
            {
                return Completions::IsDirectory;
            } else if treat_as_dir {
                (_self, OsStr::from_bytes(b""))
            } else {
                let last_component = _self
                    .components()
                    .last()
                    .map(|c| c.as_os_str())
                    .unwrap_or_else(|| OsStr::from_bytes(b""));
                let prefix = if let Some(p) = _self.parent() {
                    p
                } else {
                    return Completions::IsInvalid;
                };
                (prefix, last_component)
            }
        };

        if let Ok(iter) = std::fs::read_dir(prefix) {
            for entry in iter.flatten() {
                if entry.path().as_os_str().as_bytes() != b"."
                    && entry.path().as_os_str().as_bytes() != b".."
                    && entry
                        .path()
                        .as_os_str()
                        .ext_trim_prefix(prefix.as_os_str())
                        .ext_starts_with(_match)
                {
                    if entry.path().is_dir() && !entry.path().as_os_str().as_bytes().ends_with(b"/")
                    {
                        let mut s = unsafe {
                            String::from_utf8_unchecked(
                                entry
                                    .path()
                                    .as_os_str()
                                    .ext_trim_prefix(prefix.as_os_str())
                                    .as_bytes()[_match.as_bytes().len()..]
                                    .to_vec(),
                            )
                        };
                        s.push('/');
                        entries.push(s);
                    } else {
                        entries.push(unsafe {
                            String::from_utf8_unchecked(
                                entry
                                    .path()
                                    .as_os_str()
                                    .ext_trim_prefix(prefix.as_os_str())
                                    .as_bytes()[_match.as_bytes().len()..]
                                    .to_vec(),
                            )
                        });
                    }
                }
            }
        }

        if force && treat_as_dir {
            for entry in &mut entries {
                let entry: &mut String = entry;
                if entry.starts_with(std::path::MAIN_SEPARATOR) {
                    entry.remove(0);
                }
            }
        }

        entries.sort();
        Completions::Entries(entries)
    }

    #[cfg(target_os = "linux")]
    /// Completing implementation for Linux only, because it uses the
    /// [`getdents64`](::libc::SYS_getdents64) to get results faster, since we
    /// only care for raw name byte matching.
    pub fn inner_complete_linux(_self: &Path, force: bool, treat_as_dir: bool) -> Completions {
        use std::os::unix::io::AsRawFd;

        use nix::fcntl::OFlag;

        const BUF_SIZE: ::libc::size_t = 8 << 10;

        let (prefix, _match): (&Path, &OsStr) = if _self.as_os_str().as_bytes().ends_with(b"/.") {
            (_self.components().as_path(), OsStr::from_bytes(b"."))
        } else if _self.exists()
            && !treat_as_dir
            && (!force || _self.as_os_str().as_bytes().ends_with(b"/"))
        {
            return Completions::IsDirectory;
        } else if treat_as_dir {
            (_self, OsStr::from_bytes(b""))
        } else {
            let last_component = _self
                .components()
                .last()
                .map(|c| c.as_os_str())
                .unwrap_or_else(|| OsStr::from_bytes(b""));
            let prefix = if let Some(p) = _self.parent() {
                p
            } else {
                return Completions::IsInvalid;
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
                return Completions::IsInvalid;
            }
        };

        let mut buf: Vec<u8> = vec![0; BUF_SIZE * std::mem::size_of::<::libc::dirent64>()];
        let mut entries = Vec::new();
        loop {
            let n: i64 = unsafe {
                ::libc::syscall(
                    ::libc::SYS_getdents64,
                    dir.as_raw_fd(),
                    buf.as_ptr(),
                    BUF_SIZE,
                )
            };

            let Ok(n) = usize::try_from(n) else {
                return Completions::IsInvalid;
            };
            if n == 0 {
                break;
            }
            unsafe {
                buf.set_len(n);
            }
            let mut pos = 0;
            while pos < n {
                let dir = unsafe { std::mem::transmute::<&[u8], &[::libc::dirent64]>(&buf[pos..]) };
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

        entries.sort();
        Completions::Entries(entries)
    }

    trait AsBytesExt {
        fn as_bytes_ext(&self) -> &[u8];
    }

    impl AsBytesExt for [u8] {
        #[inline]
        fn as_bytes_ext(&self) -> &[u8] {
            self
        }
    }

    impl AsBytesExt for OsStr {
        #[inline]
        fn as_bytes_ext(&self) -> &[u8] {
            self.as_bytes()
        }
    }

    trait TrimExt {
        fn ext_trim_prefix<'s>(&'s self, prefix: &Self) -> &'s Self;
        fn ext_starts_with<B: AsBytesExt + ?Sized>(&self, prefix: &B) -> bool;
    }

    impl TrimExt for OsStr {
        #[inline]
        fn ext_trim_prefix<'s>(&'s self, prefix: &Self) -> &'s Self {
            Self::from_bytes(
                self.as_bytes()
                    .strip_prefix(prefix.as_bytes())
                    .and_then(|s| s.strip_prefix(b"/"))
                    .unwrap_or_else(|| {
                        self.as_bytes()
                            .strip_prefix(b"/")
                            .unwrap_or_else(|| self.as_bytes())
                    }),
            )
        }

        #[inline]
        fn ext_starts_with<B: AsBytesExt + ?Sized>(&self, prefix: &B) -> bool {
            let prefix = prefix.as_bytes_ext();
            self.as_bytes().starts_with(prefix)
        }
    }
}
