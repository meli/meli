/*
 * meli
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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
    ffi::{CString, OsStr},
    os::unix::{
        ffi::OsStrExt,
        io::{AsRawFd, FromRawFd, IntoRawFd},
    },
};

use melib::{error::*, log};
#[cfg(not(target_os = "macos"))]
use nix::{
    fcntl::{open, OFlag},
    pty::{grantpt, posix_openpt, ptsname, unlockpt},
    sys::stat,
};
use nix::{
    ioctl_none_bad, ioctl_write_ptr_bad,
    libc::{STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO},
    pty::Winsize,
    unistd::{dup2, fork, ForkResult},
};
use smallvec::SmallVec;

pub mod escape_codes;
pub mod terminal;

#[cfg(not(target_os = "macos"))]
use std::path::Path;
use std::{
    convert::TryFrom,
    io::{Read, Write},
    sync::{Arc, Mutex},
};

/// ioctl request code to "Make the given terminal the controlling terminal of
/// the calling process"
use libc::TIOCSCTTY;
/// ioctl request code to set window size of pty:
use libc::TIOCSWINSZ;
pub use terminal::{EmbeddedGrid, ScreenBuffer, Terminal};

ioctl_write_ptr_bad!(
    /// Macro generated function that calls ioctl to set window size of backend
    /// PTY end.
    set_window_size,
    TIOCSWINSZ,
    Winsize
);

ioctl_none_bad!(
    /// Set controlling terminal fd for current session.
    set_controlling_terminal,
    TIOCSCTTY
);

/// Create a new pseudoterminal (PTY) with given width, size and execute
/// `command` in it.
pub fn create_pty(width: usize, height: usize, command: String) -> Result<Arc<Mutex<Terminal>>> {
    #[cfg(not(target_os = "macos"))]
    let (frontend_fd, backend_name) = {
        // Open a new PTY frontend
        let frontend_fd = posix_openpt(OFlag::O_RDWR)?;

        // Allow a backend to be generated for it
        grantpt(&frontend_fd)?;
        unlockpt(&frontend_fd)?;

        // Get the name of the backend
        let backend_name = unsafe { ptsname(&frontend_fd) }?;

        {
            let winsize = Winsize {
                ws_row: <u16>::try_from(height).unwrap(),
                ws_col: <u16>::try_from(width).unwrap(),
                ws_xpixel: 0,
                ws_ypixel: 0,
            };

            let frontend_fd = frontend_fd.as_raw_fd();
            unsafe { set_window_size(frontend_fd, &winsize)? };
        }
        (frontend_fd, backend_name)
    };
    #[cfg(target_os = "macos")]
    let (frontend_fd, backend_fd) = {
        let winsize = Winsize {
            ws_row: <u16>::try_from(height).unwrap(),
            ws_col: <u16>::try_from(width).unwrap(),
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        let ends = nix::pty::openpty(Some(&winsize), None)?;
        (ends.master, ends.slave)
    };

    let child_pid = match unsafe { fork()? } {
        ForkResult::Child => {
            #[cfg(not(target_os = "macos"))]
            /* Open backend end for pseudoterminal */
            let backend_fd = open(Path::new(&backend_name), OFlag::O_RDWR, stat::Mode::empty())?;

            // assign stdin, stdout, stderr to the pty
            dup2(backend_fd, STDIN_FILENO).unwrap();
            dup2(backend_fd, STDOUT_FILENO).unwrap();
            dup2(backend_fd, STDERR_FILENO).unwrap();
            /* Become session leader */
            nix::unistd::setsid().unwrap();
            match unsafe { set_controlling_terminal(backend_fd) } {
                Ok(c) if c < 0 => {
                    log::error!(
                        "Could not execute `{command}`: ioctl(fd, TIOCSCTTY, NULL) returned {c}",
                    );
                    std::process::exit(c);
                }
                Ok(_) => {}
                Err(err) => {
                    log::error!(
                        "Could not execute `{command}`: ioctl(fd, TIOCSCTTY, NULL) returned {err}",
                    );
                    std::process::exit(-1);
                }
            }
            /* Find posix sh location, because POSIX shell is not always at /bin/sh */
            let path_var = std::process::Command::new("getconf")
                .args(["PATH"])
                .output()?
                .stdout;
            for mut p in std::env::split_paths(&OsStr::from_bytes(&path_var[..])) {
                p.push("sh");
                if p.exists() {
                    if let Err(e) = nix::unistd::execv(
                        &CString::new(p.as_os_str().as_bytes()).unwrap(),
                        &[
                            &CString::new("sh").unwrap(),
                            &CString::new("-c").unwrap(),
                            &CString::new(command.as_bytes()).unwrap(),
                        ],
                    ) {
                        log::error!("Could not execute `{command}`: {e}");
                        std::process::exit(-1);
                    }
                }
            }
            log::error!(
                "Could not execute `{command}`: did not find the standard POSIX sh shell in PATH \
                 = {}",
                String::from_utf8_lossy(&path_var),
            );
            // We are in a separate process, so doing exit(-1) here won't affect the parent
            // process.
            std::process::exit(-1);
        }
        ForkResult::Parent { child } => child,
    };

    let stdin = unsafe { std::fs::File::from_raw_fd(frontend_fd.as_raw_fd()) };
    let mut embedded_pty = Terminal::new(stdin, child_pid);
    embedded_pty.set_terminal_size((width, height));
    let pty = Arc::new(Mutex::new(embedded_pty));
    let pty_ = pty.clone();

    std::thread::Builder::new()
        .spawn(move || {
            let frontend_fd = frontend_fd.into_raw_fd();
            let frontend_file = unsafe { std::fs::File::from_raw_fd(frontend_fd) };
            Terminal::forward_pty_translate_escape_codes(pty_, frontend_file);
        })
        .unwrap();
    Ok(pty)
}
