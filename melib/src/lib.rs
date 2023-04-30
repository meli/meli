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

//! A crate that performs mail client operations such as
//! - Hold an [`Envelope`](./email/struct.Envelope.html) with methods convenient
//!   for mail client use. (see module [`email`](./email/index.html))
//! - Abstract through mail storages through the
//!   [`MailBackend`](./backends/trait.MailBackend.html) trait, and handle
//!   read/writes/updates through it. (see module
//!   [`backends`](./backends/index.html))
//! - Decode attachments (see module
//!   [`email::attachments`](./email/attachments/index.html))
//! - Create new mail (see [`email::Draft`](./email/compose/struct.Draft.html))
//! - Send mail with an SMTP client (see module [`smtp`](./smtp/index.html))
//! - Manage an `addressbook` i.e. have contacts (see module
//!   [`addressbook`](./addressbook/index.html))
//! - Build thread structures out of a list of mail via their `In-Reply-To` and
//!   `References` header values (see module [`thread`](./thread/index.html))
//!
//! Other exports are
//! - Basic mail account configuration to use with
//!   [`backends`](./backends/index.html) (see module
//!   [`conf`](./conf/index.html))
//! - Parser combinators (see module [`parsec`](./parsec/index.html))
//! - A `ShellExpandTrait` to expand paths like a shell.
//! - A `debug` macro that works like `std::dbg` but for multiple threads. (see
//!   [`debug` macro](./macro.debug.html))
#[macro_use]
pub mod dbg {

    #[macro_export]
    macro_rules! log_tag {
        () => {
            eprint!(
                "[{}][{:?}] {}:{}_{}: ",
                $crate::datetime::timestamp_to_string(
                    $crate::datetime::now(),
                    Some("%Y-%m-%d %T"),
                    false
                ),
                std::thread::current()
                    .name()
                    .map(std::string::ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                file!(),
                line!(),
                column!()
            );
        };
    }

    #[allow(clippy::redundant_closure)]
    #[macro_export]
    macro_rules! debug {
        ($val:literal) => {
            {
            if cfg!(feature="debug-tracing") {
                log_tag!();
                eprintln!($val);
            }
            $val
        }
        };
        ($val:expr) => {
            if cfg!(feature="debug-tracing") {
                let stringify = stringify!($val);
                // Use of `match` here is intentional because it affects the lifetimes
                // of temporaries - https://stackoverflow.com/a/48732525/1063961
                match $val {
                    tmp => {
                        log_tag!();
                        eprintln!("{} = {:?}", stringify, tmp);
                        tmp
                    }
                }
            } else {
                $val
            }
        };
        ($fmt:literal, $($arg:tt)*) => {
            if cfg!(feature="debug-tracing") {
                log_tag!();
                eprintln!($fmt, $($arg)*);
            }
        };
    }
}

#[cfg(feature = "unicode_algorithms")]
pub mod text_processing;

pub mod datetime;
pub use datetime::UnixTimestamp;

#[macro_use]
mod logging;
pub use self::logging::{LoggingLevel::*, *};

pub mod addressbook;
pub use addressbook::*;
pub mod backends;
pub use backends::*;
mod collection;
pub mod sieve;
pub use collection::*;
pub mod conf;
pub use conf::*;
pub mod email;
pub use email::*;
pub mod error;
pub use crate::error::*;
pub mod thread;
pub use thread::*;
pub mod connections;
pub mod parsec;
pub mod search;

#[cfg(feature = "gpgme")]
pub mod gpgme;
#[cfg(feature = "smtp")]
pub mod smtp;
#[cfg(feature = "sqlite3")]
pub mod sqlite3;

#[macro_use]
extern crate serde_derive;
/* parser */
extern crate data_encoding;
extern crate encoding;
pub extern crate nom;

#[macro_use]
extern crate bitflags;
pub extern crate futures;
pub extern crate indexmap;
pub extern crate smallvec;
pub extern crate smol;
pub extern crate uuid;
pub extern crate xdg_utils;

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct Bytes(pub usize);

impl Bytes {
    pub const KILOBYTE: f64 = 1024.0;
    pub const MEGABYTE: f64 = Self::KILOBYTE * 1024.0;
    pub const GIGABYTE: f64 = Self::MEGABYTE * 1024.0;
    pub const PETABYTE: f64 = Self::GIGABYTE * 1024.0;
}

impl core::fmt::Display for Bytes {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        let bytes: f64 = self.0 as f64;
        if bytes == 0.0 {
            write!(fmt, "0")
        } else if bytes < Self::KILOBYTE {
            write!(fmt, "{:.2} bytes", bytes)
        } else if bytes < Self::MEGABYTE {
            write!(fmt, "{:.2} KiB", bytes / Self::KILOBYTE)
        } else if bytes < Self::GIGABYTE {
            write!(fmt, "{:.2} MiB", bytes / Self::MEGABYTE)
        } else if bytes < Self::PETABYTE {
            write!(fmt, "{:.2} GiB", bytes / Self::GIGABYTE)
        } else {
            write!(fmt, "{:.2} PiB", bytes / Self::PETABYTE)
        }
    }
}

pub use shellexpand::ShellExpandTrait;
pub mod shellexpand {

    #[cfg(not(any(target_os = "netbsd", target_os = "macos")))]
    use std::os::unix::io::AsRawFd;
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
                if n < 0 {
                    return SmallVec::new();
                } else if n == 0 {
                    break;
                }

                let n = n as usize;
                unsafe {
                    buf.set_len(n);
                }
                let mut pos = 0;
                while pos < n {
                    let dir = unsafe { std::mem::transmute::<&[u8], &[dirent64]>(&buf[pos..]) };
                    let entry = unsafe { std::ffi::CStr::from_ptr(dir[0].d_name.as_ptr()) };
                    if entry.to_bytes() != b"." && entry.to_bytes() != b".." {
                        if entry.to_bytes().starts_with(_match.as_bytes()) {
                            if dir[0].d_type == ::libc::DT_DIR && !entry.to_bytes().ends_with(b"/")
                            {
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

            if let Ok(iter) = std::fs::read_dir(&prefix) {
                for entry in iter.flatten() {
                    if entry.path().as_os_str().as_bytes() != b"."
                        && entry.path().as_os_str().as_bytes() != b".."
                        && entry
                            .path()
                            .as_os_str()
                            .as_bytes()
                            .starts_with(_match.as_bytes())
                    {
                        if entry.path().is_dir()
                            && !entry.path().as_os_str().as_bytes().ends_with(b"/")
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
}

#[macro_export]
macro_rules! declare_u64_hash {
    ($type_name:ident) => {
        #[derive(
            Hash,
            Eq,
            PartialEq,
            Debug,
            Ord,
            PartialOrd,
            Default,
            Serialize,
            Deserialize,
            Copy,
            Clone,
        )]
        #[repr(transparent)]
        pub struct $type_name(pub u64);

        impl $type_name {
            #[inline(always)]
            pub fn from_bytes(bytes: &[u8]) -> Self {
                use std::{collections::hash_map::DefaultHasher, hash::Hasher};
                let mut h = DefaultHasher::new();
                h.write(bytes);
                Self(h.finish())
            }

            #[inline(always)]
            pub const fn to_be_bytes(self) -> [u8; 8] {
                self.0.to_be_bytes()
            }

            #[inline(always)]
            pub const fn is_null(self) -> bool {
                self.0 == 0
            }
        }

        impl core::fmt::Display for $type_name {
            fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(fmt, "{}", self.0)
            }
        }
        #[cfg(feature = "sqlite3")]
        impl rusqlite::types::ToSql for $type_name {
            fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput> {
                Ok(rusqlite::types::ToSqlOutput::from(self.0 as i64))
            }
        }

        #[cfg(feature = "sqlite3")]
        impl rusqlite::types::FromSql for $type_name {
            fn column_result(
                value: rusqlite::types::ValueRef,
            ) -> rusqlite::types::FromSqlResult<Self> {
                let b: i64 = rusqlite::types::FromSql::column_result(value)?;

                Ok($type_name(b as u64))
            }
        }
    };
}
