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
//! - Hold an `Envelope` with methods convenient for mail client use. (see module `email`)
//! - Abstract through mail storages through the `MailBackend` trait, and handle
//!   read/writes/updates through it. (see module `melib::backends`)
//! - Decode attachments (see module `melib::email::attachments`)
//! - Create new mail (see `email::Draft`)
//! - Manage an `addressbook` i.e. have contacts (see module `addressbook`)
//! - Build thread structures out of a list of mail via their `In-Reply-To` and `References` header
//!   values (see module `thread`)
//!
//! Other exports are
//! - Thread management (see module `async_workers`)
//! - Basic mail account configuration to use with `backends` (see module `conf`)
//! - Parser combinators (see module `parsec`)
//! - A `ShellExpandTrait` to expand paths like a shell.
//! - A `debug` macro that works like `std::dbg` but for multiple threads. (see `dbg` module)
#[macro_use]
pub mod dbg {
    #[allow(clippy::redundant_closure)]
    #[macro_export]
    macro_rules! debug {
        ($val:literal) => {
            {
            if cfg!(feature="debug-tracing") {
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(std::string::ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                    file!(),
                    line!(),
                    column!()
                );
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
                        eprint!(
                            "[{:?}] {}:{}_{}:	",
                            std::thread::current()
                            .name()
                            .map(std::string::ToString::to_string)
                            .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                            file!(),
                            line!(),
                            column!()
                        );
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
                eprint!(
                    "[{:?}] {}:{}_{}:	",
                    std::thread::current()
                    .name()
                    .map(std::string::ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", std::thread::current().id())),
                    file!(),
                    line!(),
                    column!()
                );
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
pub use self::logging::LoggingLevel::*;
pub use self::logging::*;

pub mod addressbook;
pub mod async_workers;
pub mod backends;
mod collection;
pub mod conf;
pub mod email;
pub mod error;
pub mod thread;
pub use crate::email::*;
pub use crate::thread::*;
pub mod parsec;
pub mod search;

#[macro_use]
extern crate serde_derive;
/* parser */
#[macro_use]
extern crate nom;
extern crate data_encoding;
extern crate encoding;

#[macro_use]
extern crate bitflags;
extern crate fnv;
extern crate uuid;

pub use crate::backends::{Backends, RefreshEvent, RefreshEventConsumer, SpecialUsageMailbox};
pub use crate::collection::*;
pub use crate::conf::*;
pub use crate::email::{Envelope, EnvelopeHash, Flag};
pub use crate::error::{MeliError, Result};

pub use crate::addressbook::*;

pub use shellexpand::ShellExpandTrait;
pub mod shellexpand {

    use smallvec::SmallVec;
    use std::path::{Path, PathBuf};

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
                        if let Some(home_dir) = std::env::var("HOME").ok() {
                            ret.push(home_dir)
                        } else {
                            return PathBuf::new();
                        }
                    }
                    Some(var) if var.starts_with("$") => {
                        let env_name = var.split_at(1).1;
                        if env_name.chars().all(char::is_uppercase) {
                            ret.push(std::env::var(env_name).unwrap_or(String::new()));
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

        fn complete(&self, force: bool) -> SmallVec<[String; 128]> {
            use libc::dirent64;
            use nix::fcntl::OFlag;
            use std::ffi::OsStr;
            use std::os::unix::ffi::OsStrExt;
            use std::os::unix::io::AsRawFd;
            const BUF_SIZE: ::libc::size_t = 8 << 10;

            let (prefix, _match) = if self.as_os_str().as_bytes().ends_with(b"/.") {
                (self.components().as_path(), OsStr::from_bytes(b"."))
            } else {
                if self.exists() && (!force || self.as_os_str().as_bytes().ends_with(b"/")) {
                    // println!("{} {:?}", self.display(), self.components().last());
                    return SmallVec::new();
                } else {
                    let last_component = self
                        .components()
                        .last()
                        .map(|c| c.as_os_str())
                        .unwrap_or(OsStr::from_bytes(b""));
                    let prefix = if let Some(p) = self.parent() {
                        p
                    } else {
                        return SmallVec::new();
                    };
                    (prefix, last_component)
                }
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
                // "It's tempting to bail here if n + sizeof(linux_dirent64) + 512 <= n. After all, there
                // was enough space for another entry but SYS_getdents64 didn't write it, so this must be
                // the end of the directory listing, right? Unfortunately, no. SYS_getdents64 is finicky.
                // It sometimes writes a partial list of entries even if the full list would fit."
            }
            return entries;
        }
    }

    #[test]
    fn test_shellexpandtrait() {
        assert!(Path::new("~").expand().complete(false).is_empty());
        assert!(!Path::new("~").expand().complete(true).is_empty());
    }
}
