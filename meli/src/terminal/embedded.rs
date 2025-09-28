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

#[cfg(target_os = "macos")]
use std::os::fd::OwnedFd;
use std::{
    ffi::{CStr, CString, OsStr},
    mem::ManuallyDrop,
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
use nix::{ioctl_none_bad, ioctl_write_ptr_bad, pty::Winsize};
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
pub fn create_pty(width: usize, height: usize, command: &str) -> Result<Arc<Mutex<Terminal>>> {
    let command_cstr = CString::new(command)
        .chain_err_kind(ErrorKind::ValueError)
        .chain_err_summary(|| {
            format!(
                "Could not convert command `{command}` into a C string; it should contain no NUL \
                 bytes."
            )
        })?;

    #[cfg(not(target_os = "macos"))]
    let (frontend_fd, backend_name): (nix::pty::PtyMaster, String) = {
        // Open a new PTY frontend
        let frontend_fd = posix_openpt(OFlag::O_RDWR)?;

        // Allow a backend to be generated for it
        grantpt(&frontend_fd)?;
        unlockpt(&frontend_fd)?;

        // Get the name of the backend
        let backend_name = unsafe { ptsname(&frontend_fd) }?;

        {
            let winsize = Winsize {
                ws_row: <u16>::try_from(height).unwrap_or(u16::MAX),
                ws_col: <u16>::try_from(width).unwrap_or(u16::MAX),
                ws_xpixel: 0,
                ws_ypixel: 0,
            };

            let frontend_fd = frontend_fd.as_raw_fd();
            unsafe { set_window_size(frontend_fd, &winsize)? };
        }
        (frontend_fd, backend_name)
    };
    #[cfg(target_os = "macos")]
    let (frontend_fd, backend_fd): (OwnedFd, OwnedFd) = {
        let winsize = Winsize {
            ws_row: <u16>::try_from(height).unwrap_or(u16::MAX),
            ws_col: <u16>::try_from(width).unwrap_or(u16::MAX),
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        let ends = nix::pty::openpty(Some(&winsize), None)?;
        (ends.master, ends.slave)
    };

    let mut shell_path = None;

    // Find posix sh location, because POSIX shell is not always at /bin/sh
    let path_var = std::process::Command::new("getconf")
        .args(["PATH"])
        .output()?
        .stdout;
    for mut p in std::env::split_paths(&OsStr::from_bytes(&path_var[..])) {
        p.push("sh");
        if p.exists() {
            shell_path = Some(
                CString::new(p.as_os_str().as_bytes())
                    .chain_err_kind(ErrorKind::ValueError)
                    .chain_err_summary(|| {
                        format!(
                            "Could not convert shell path `{}` into a C string; it should contain \
                             no NUL bytes.",
                            p.display()
                        )
                    })?,
            );
            break;
        }
    }

    let Some(shell_path) = shell_path else {
        return Err(Error::new(format!(
            "Could not execute `{command}`: did not find the standard POSIX sh shell in PATH = {}",
            String::from_utf8_lossy(&path_var)
        )));
    };

    let child_pid = match unsafe { nix::unistd::fork()? } {
        nix::unistd::ForkResult::Child => {
            #[cfg(not(target_os = "macos"))]
            // Open backend end for pseudoterminal
            let backend_fd = open(Path::new(&backend_name), OFlag::O_RDWR, stat::Mode::empty())?;

            // assign stdin, stdout, stderr to the pty
            nix::unistd::dup2_stdin(&backend_fd).expect("could not dup2 STDIN");
            nix::unistd::dup2_stdout(&backend_fd).expect("could not dup2 STDOUT");
            nix::unistd::dup2_stderr(&backend_fd).expect("could not dup2 STDERR");
            // Become session leader
            nix::unistd::setsid().expect("Forked terminal process could not become session leader");
            match unsafe { set_controlling_terminal(backend_fd.as_raw_fd()) } {
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
            const SH: &CStr = c"sh";
            const COMMAND_FLAG: &CStr = c"-c";
            // nix::unistd::execv never fails, returns `Result<Infallible>` because it
            // never returns.
            nix::unistd::execv(&shell_path, &[SH, COMMAND_FLAG, &command_cstr])
                .expect("Infallible");
            // We are in a separate process, so doing exit(-1) here won't affect the parent
            // process.
            std::process::exit(-1);
        }
        nix::unistd::ForkResult::Parent { child } => child,
    };

    let stdin = unsafe { std::fs::File::from_raw_fd(frontend_fd.as_raw_fd()) };
    // We will let the spawned thread close frontend_fd
    let mut embedded_pty = Terminal::new(ManuallyDrop::new(stdin), child_pid);
    embedded_pty.set_terminal_size((width, height));
    let pty = Arc::new(Mutex::new(embedded_pty));
    let pty_ = pty.clone();

    std::thread::Builder::new()
        .spawn(move || {
            let frontend_fd = frontend_fd.into_raw_fd();
            let frontend_file = unsafe { std::fs::File::from_raw_fd(frontend_fd) };
            Terminal::forward_pty_translate_escape_codes(pty_, frontend_file);
        })
        .chain_err_summary(|| {
            "Could not spawn controlling thread for forked embedded terminal process"
        })?;
    Ok(pty)
}
