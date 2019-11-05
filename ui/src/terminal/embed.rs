use crate::split_command;
use crate::terminal::position::Area;
use crate::terminal::position::*;
use melib::log;
use melib::ERROR;

use nix::fcntl::{open, OFlag};
use nix::ioctl_write_ptr_bad;
use nix::libc::{STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO};
use nix::pty::{grantpt, posix_openpt, ptsname, unlockpt, Winsize};
use nix::sys::{stat, wait::waitpid};
use nix::unistd::{dup2, fork, ForkResult};
use std::ffi::CString;
use std::os::unix::io::{AsRawFd, FromRawFd, IntoRawFd};

mod grid;

pub use grid::EmbedGrid;

// ioctl command to set window size of pty:
use libc::TIOCSWINSZ;
use std::path::Path;

use std::convert::TryFrom;
use std::io::Read;
use std::io::Write;
use std::sync::{Arc, Mutex};

// Macro generated function that calls ioctl to set window size of slave pty end
ioctl_write_ptr_bad!(set_window_size, TIOCSWINSZ, Winsize);

pub fn create_pty(area: Area, command: String) -> nix::Result<Arc<Mutex<EmbedGrid>>> {
    // Open a new PTY master
    let master_fd = posix_openpt(OFlag::O_RDWR)?;

    // Allow a slave to be generated for it
    grantpt(&master_fd)?;
    unlockpt(&master_fd)?;

    // Get the name of the slave
    let slave_name = unsafe { ptsname(&master_fd) }?;

    // Try to open the slave
    //let _slave_fd = open(Path::new(&slave_name), OFlag::O_RDWR, Mode::empty())?;
    {
        let winsize = Winsize {
            ws_row: <u16>::try_from(height!(area)).unwrap(),
            ws_col: <u16>::try_from(width!(area)).unwrap(),
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        let master_fd = master_fd.clone().into_raw_fd();
        unsafe { set_window_size(master_fd, &winsize).unwrap() };
    }

    let child_pid = match fork() {
        Ok(ForkResult::Child) => {
            /* Open slave end for pseudoterminal */
            let slave_fd = open(Path::new(&slave_name), OFlag::O_RDWR, stat::Mode::empty())?;

            let child_pid = match fork() {
                Ok(ForkResult::Child) => {
                    // assign stdin, stdout, stderr to the tty
                    dup2(slave_fd, STDIN_FILENO).unwrap();
                    dup2(slave_fd, STDOUT_FILENO).unwrap();
                    dup2(slave_fd, STDERR_FILENO).unwrap();
                    let parts = split_command!(command);
                    let (cmd, _) = (parts[0], &parts[1..]);
                    if let Err(e) = nix::unistd::execv(
                        &CString::new(cmd).unwrap(),
                        &parts
                            .iter()
                            .map(|&a| CString::new(a).unwrap())
                            .collect::<Vec<CString>>(),
                    ) {
                        log(format!("Could not execute `{}`: {}", command, e,), ERROR);
                        std::process::exit(-1);
                    }
                    /* This path shouldn't be executed. */
                    std::process::exit(0);
                }
                Ok(ForkResult::Parent { child }) => child,
                Err(e) => panic!(e),
            };
            waitpid(child_pid, None).unwrap();
            std::process::exit(0);
        }
        Ok(ForkResult::Parent { child }) => child,
        Err(e) => panic!(e),
    };

    let stdin = unsafe { std::fs::File::from_raw_fd(master_fd.clone().into_raw_fd()) };
    let mut embed_grid = EmbedGrid::new(stdin, child_pid);
    embed_grid.set_terminal_size((width!(area), height!(area)));
    let grid = Arc::new(Mutex::new(embed_grid));
    let grid_ = grid.clone();

    std::thread::Builder::new()
        .spawn(move || {
            let master_fd = master_fd.into_raw_fd();
            let master_file = unsafe { std::fs::File::from_raw_fd(master_fd) };
            forward_pty_translate_escape_codes(master_file, grid_);
        })
        .unwrap();
    Ok(grid)
}

fn forward_pty_translate_escape_codes(pty_fd: std::fs::File, grid: Arc<Mutex<EmbedGrid>>) {
    let mut bytes_iter = pty_fd.bytes();
    debug!("waiting for bytes");
    while let Some(Ok(byte)) = bytes_iter.next() {
        debug!("got byte {}", byte as char);
        grid.lock().unwrap().process_byte(byte);
    }
}

#[derive(Debug)]
pub enum State {
    ExpectingControlChar,
    G0,            // Designate G0 Character Set
    Osc1(Vec<u8>), //ESC ] Operating System Command (OSC  is 0x9d).
    Osc2(Vec<u8>, Vec<u8>),
    Csi, // ESC [ Control Sequence Introducer (CSI  is 0x9b).
    Csi1(Vec<u8>),
    Csi2(Vec<u8>, Vec<u8>),
    Csi3(Vec<u8>, Vec<u8>, Vec<u8>),
    CsiQ(Vec<u8>),
    Normal,
}

/* Used for debugging */
struct EscCode<'a>(&'a State, u8);

impl<'a> From<(&'a mut State, u8)> for EscCode<'a> {
    fn from(val: (&mut State, u8)) -> EscCode {
        let (s, b) = val;
        EscCode(s, b)
    }
}

impl<'a> From<(&'a State, u8)> for EscCode<'a> {
    fn from(val: (&State, u8)) -> EscCode {
        let (s, b) = val;
        EscCode(s, b)
    }
}

impl std::fmt::Display for EscCode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use State::*;
        macro_rules! unsafestr {
            ($buf:ident) => {
                unsafe { std::str::from_utf8_unchecked($buf) }
            };
        }
        match self {
            EscCode(G0, b'B') => write!(f, "ESC(B\t\tG0 USASCII charset set"),
            EscCode(G0, c) => write!(f, "ESC({}\t\tG0 charset set", *c as char),
            EscCode(Osc1(ref buf), ref c) => {
                write!(f, "ESC]{}{}\t\tOSC", unsafestr!(buf), *c as char)
            }
            EscCode(Osc2(ref buf1, ref buf2), c) => write!(
                f,
                "ESC]{};{}{}\t\tOSC [UNKNOWN]",
                unsafestr!(buf1),
                unsafestr!(buf2),
                *c as char
            ),
            EscCode(Csi, b'm') => write!(
                f,
                "ESC[m\t\tCSI Character Attributes | Set Attr and Color to Normal (default)"
            ),
            EscCode(Csi, b'K') => write!(
                f,
                "ESC[K\t\tCSI Erase from the cursor to the end of the line"
            ),
            EscCode(Csi, b'J') => write!(
                f,
                "ESC[J\t\tCSI Erase from the cursor to the end of the screen"
            ),
            EscCode(Csi, b'H') => write!(f, "ESC[H\t\tCSI Move the cursor to home position."),
            EscCode(Csi, c) => write!(f, "ESC[{}\t\tCSI [UNKNOWN]", *c as char),
            EscCode(Csi1(ref buf), b'm') => write!(
                f,
                "ESC[{}m\t\tCSI Character Attributes | Set fg, bg color",
                unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'n') => write!(
                f,
                "ESC[{}n\t\tCSI Device Status Report (DSR)| Report Cursor Position",
                unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b't') if buf == b"18" => write!(
                f,
                "ESC[18t\t\tReport the size of the text area in characters",
            ),
            EscCode(Csi1(ref buf), b't') => write!(
                f,
                "ESC[{buf}t\t\tWindow manipulation, skipped",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'B') => write!(
                f,
                "ESC[{buf}B\t\tCSI Cursor Down {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'C') => write!(
                f,
                "ESC[{buf}C\t\tCSI Cursor Forward {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'D') => write!(
                f,
                "ESC[{buf}D\t\tCSI Cursor Backward {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'E') => write!(
                f,
                "ESC[{buf}E\t\tCSI Cursor Next Line {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'F') => write!(
                f,
                "ESC[{buf}F\t\tCSI Cursor Preceding Line {buf} Times",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'G') => write!(
                f,
                "ESC[{buf}G\t\tCursor Character Absolute  [column={buf}] (default = [row,1])",
                buf = unsafestr!(buf)
            ),

            EscCode(Csi1(ref buf), b'P') => write!(
                f,
                "ESC[{buf}P\t\tDelete P s Character(s) (default = 1) (DCH).  ",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'S') => write!(
                f,
                "ESC[{buf}S\t\tCSI P s S Scroll up P s lines (default = 1) (SU), VT420, EC",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), b'J') => write!(
                f,
                "Erase in display {buf}",
                buf = unsafestr!(buf)
            ),
            EscCode(Csi1(ref buf), c) => {
                write!(f, "ESC[{}{}\t\tCSI [UNKNOWN]", unsafestr!(buf), *c as char)
            }
            EscCode(Csi2(ref buf1, ref buf2), b'r') => write!(
                f,
                "ESC[{};{}r\t\tCSI Set Scrolling Region [top;bottom] (default = full size of window) (DECSTBM), VT100.",
                unsafestr!(buf1),
                unsafestr!(buf2),
            ),
            EscCode(Csi2(ref buf1, ref buf2), c) => write!(
                f,
                "ESC[{};{}{}\t\tCSI",
                unsafestr!(buf1),
                unsafestr!(buf2),
                *c as char
            ),
            EscCode(Csi3(ref buf1, ref buf2, ref buf3), b'm') => write!(
                f,
                "ESC[{};{};{}m\t\tCSI Character Attributes | Set fg, bg color",
                unsafestr!(buf1),
                unsafestr!(buf2),
                unsafestr!(buf3),
            ),
            EscCode(Csi3(ref buf1, ref buf2, ref buf3), c) => write!(
                f,
                "ESC[{};{};{}{}\t\tCSI [UNKNOWN]",
                unsafestr!(buf1),
                unsafestr!(buf2),
                unsafestr!(buf3),
                *c as char
            ),
            EscCode(CsiQ(ref buf), b's') => write!(
                f,
                "ESC[?{}r\t\tCSI Save DEC Private Mode Values",
                unsafestr!(buf)
            ),
            EscCode(CsiQ(ref buf), b'r') => write!(
                f,
                "ESC[?{}r\t\tCSI Restore DEC Private Mode Values",
                unsafestr!(buf)
            ),
            EscCode(CsiQ(ref buf), b'h') if buf == b"25" => write!(
                f,
                "ESC[?25h\t\tCSI DEC Private Mode Set (DECSET) show cursor",
            ),
            EscCode(CsiQ(ref buf), b'h') if buf == b"12" => write!(
                f,
                "ESC[?12h\t\tCSI DEC Private Mode Set (DECSET) Start Blinking Cursor.",
            ),
            EscCode(CsiQ(ref buf), b'h') => write!(
                f,
                "ESC[?{}h\t\tCSI DEC Private Mode Set (DECSET). [UNKNOWN]",
                unsafestr!(buf)
            ),
            EscCode(CsiQ(ref buf), b'l') if buf == b"12" => write!(
                f,
                "ESC[?12l\t\tCSI DEC Private Mode Set (DECSET) Stop Blinking Cursor",
            ),
            EscCode(CsiQ(ref buf), b'l') if buf == b"25" => write!(
                f,
                "ESC[?25l\t\tCSI DEC Private Mode Set (DECSET) hide cursor",
            ),
            EscCode(CsiQ(ref buf), c) => {
                write!(f, "ESC[?{}{}\t\tCSI [UNKNOWN]", unsafestr!(buf), *c as char)
            }
            EscCode(unknown, c) => {
                write!(f, "{:?}{} [UNKNOWN]", unknown, c)
            }
        }
    }
}
