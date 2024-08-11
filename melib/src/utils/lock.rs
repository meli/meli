//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

/// Process-associated advisory locking (POSX) and open file description locking
/// (Linux-only).
use std::{os::fd::AsRawFd, path::Path, thread::sleep, time::Duration};

use crate::error::{Errno, Error, ErrorKind, Result};

pub enum FileLockOptions {
    Nonblocking {
        max_tries: u8,
        try_wait: Option<Duration>,
    },
    Blocking,
}

impl FileLockOptions {
    pub const fn try_thrice() -> Self {
        Self::Nonblocking {
            max_tries: 3,
            try_wait: Some(Duration::from_millis(100)),
        }
    }
}

#[derive(Debug)]
pub struct FileLock<T: AsRawFd>(T);

// F_OFD_SETLKW
#[cfg(any(target_os = "linux", target_os = "android"))]
const F_SETLKW: libc::c_int = 38;
// F_OFD_SETLK
#[cfg(any(target_os = "linux", target_os = "android"))]
const F_SETLK: libc::c_int = 37;
#[cfg(not(any(target_os = "linux", target_os = "android")))]
const F_SETLKW: libc::c_int = libc::F_SETLKW;
#[cfg(not(any(target_os = "linux", target_os = "android")))]
const F_SETLK: libc::c_int = libc::F_SETLK;

impl<T> Drop for FileLock<T>
where
    T: AsRawFd + Sized,
{
    fn drop(&mut self) {
        let fd: libc::c_int = self.0.as_raw_fd();
        let mut flock: libc::flock = libc::flock {
            l_type: libc::F_UNLCK as libc::c_short,
            l_whence: libc::SEEK_SET as libc::c_short,
            l_start: 0,
            l_len: 0, /* "Specifying 0 for l_len has the special meaning: lock all bytes starting
                       * at the location specified by l_whence and l_start through to the end of
                       * file, no matter how large the file grows. */
            l_pid: 0, /* "By contrast with traditional record locks, the l_pid field of that structure must be set to zero when using the commands described below." */
        };
        let ret_val = unsafe { libc::fcntl(fd, F_SETLK, &mut flock) };
        log::debug!("dropped unix fd lock for mbox fd {}, got {}", fd, ret_val);
    }
}

pub trait FileLockTrait: AsRawFd + Sized {
    fn lock(self, options: FileLockOptions, path: &Path) -> Result<FileLock<Self>>;
}

impl<T> FileLockTrait for T
where
    T: AsRawFd,
{
    // Open file description locking
    // # man fcntl
    fn lock(self, options: FileLockOptions, path: &Path) -> Result<FileLock<T>> {
        let fd: libc::c_int = self.as_raw_fd();
        let mut flock: libc::flock = libc::flock {
            l_type: libc::F_WRLCK as libc::c_short,
            l_whence: libc::SEEK_SET as libc::c_short,
            l_start: 0,
            l_len: 0, /* Specifying 0 for l_len has the special meaning: lock all bytes starting
                       * at the location specified by l_whence and l_start through to the end of
                       * file, no matter how large the file grows. */
            l_pid: 0, /* By contrast with traditional record locks, the l_pid field of that structure must be set to zero when using the commands described below. */
            #[cfg(target_os = "freebsd")]
            l_sysid: 0,
        };
        let op = match options {
            FileLockOptions::Blocking => F_SETLKW,
            FileLockOptions::Nonblocking { .. } => F_SETLK,
        };
        let ret_val = unsafe { libc::fcntl(fd, op, &mut flock) };
        if ret_val == 0 {
            return Ok(FileLock(self));
        }
        let err = Errno::last();

        let (max_tries, try_wait) = if let FileLockOptions::Nonblocking {
            max_tries,
            try_wait,
        } = options
        {
            (max_tries.saturating_sub(1), try_wait)
        } else {
            (0, None)
        };
        for _ in 0..max_tries {
            if let Some(dur) = try_wait {
                sleep(dur);
            }
            let ret_val = unsafe { libc::fcntl(fd, op, &mut flock) };
            if ret_val == 0 {
                return Ok(FileLock(self));
            }
        }

        Err(Error::new(format!(
            "Could not lock {}: fcntl() returned {}",
            path.display(),
            err.desc()
        ))
        .set_kind(ErrorKind::OSError(err)))
    }
}

impl<T: AsRawFd> FileLock<T> {
    pub fn into_inner(mut self) -> T {
        // SAFETY: Drop is not executed because we call `forget` afterwards.
        let inner = std::mem::replace(&mut self.0, unsafe {
            std::mem::MaybeUninit::zeroed().assume_init()
        });
        std::mem::forget(self);
        inner
    }
}

impl<T: AsRawFd> std::ops::Deref for FileLock<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: AsRawFd> std::ops::DerefMut for FileLock<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: AsRawFd + std::io::Read> std::io::Read for FileLock<T> {
    fn read(&mut self, input: &mut [u8]) -> std::result::Result<usize, std::io::Error> {
        self.0.read(input)
    }
}

impl<T: AsRawFd + std::io::Seek> std::io::Seek for FileLock<T> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::result::Result<u64, std::io::Error> {
        self.0.seek(pos)
    }
}

impl<T: AsRawFd + std::io::Write> std::io::Write for FileLock<T> {
    fn write(&mut self, buf: &[u8]) -> std::result::Result<usize, std::io::Error> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::result::Result<(), std::io::Error> {
        self.0.flush()
    }
}

impl<T: AsRawFd + std::io::BufRead> std::io::BufRead for FileLock<T> {
    fn fill_buf(&mut self) -> std::result::Result<&[u8], std::io::Error> {
        self.0.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        self.0.consume(amt)
    }
}
