/*
 * meli - ui crate.
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

/*!
 * This library exports the public types and methods of its modules
 */

#[macro_use]
extern crate melib;
pub extern crate mime_apps;
extern crate notify_rust;
extern crate text_processing;
#[macro_use]
extern crate serde_derive;
extern crate linkify;
extern crate uuid;

extern crate fnv;
extern crate termion;

#[macro_use]
extern crate nom;

extern crate serde_json;
pub extern crate smallvec;

use melib::*;
use std::collections::VecDeque;
use text_processing::*;

#[macro_use]
mod types;
pub use crate::types::*;

#[macro_use]
mod terminal;
pub use crate::terminal::*;

#[macro_use]
mod execute;
use crate::execute::*;

pub mod state;
pub use crate::state::*;

pub mod components;
pub use crate::components::*;

#[macro_use]
pub mod conf;
pub use crate::conf::*;

pub mod workers;
pub use crate::workers::*;

#[cfg(feature = "sqlite3")]
pub mod sqlite3;

pub mod cache;

pub mod mailcap;

pub mod plugins;

pub use crate::username::*;
pub mod username {
    use libc;
    use std::ptr::null_mut;
    /* taken from whoami-0.1.1 */
    fn getpwuid() -> libc::passwd {
        let mut pwentp = null_mut();
        #[cfg(target_arch = "aarch64")]
        let mut buffer = [0u8; 16384]; // from the man page
        #[cfg(not(target_arch = "aarch64"))]
        let mut buffer = [0i8; 16384]; // from the man page
        #[cfg(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "freebsd",
            target_os = "dragonfly",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            let mut pwent = libc::passwd {
                pw_name: null_mut(),
                pw_passwd: null_mut(),
                pw_uid: 0,
                pw_gid: 0,
                pw_change: 0,
                pw_class: null_mut(),
                pw_gecos: null_mut(),
                pw_dir: null_mut(),
                pw_shell: null_mut(),
                pw_expire: 0,
            };
            unsafe {
                libc::getpwuid_r(
                    libc::geteuid(),
                    &mut pwent,
                    &mut buffer[0],
                    16384,
                    &mut pwentp,
                );
            }

            pwent
        }
        #[cfg(target_os = "linux")]
        {
            let mut pwent = libc::passwd {
                pw_name: null_mut(),
                pw_passwd: null_mut(),
                pw_uid: 0,
                pw_gid: 0,
                pw_gecos: null_mut(),
                pw_dir: null_mut(),
                pw_shell: null_mut(),
            };

            unsafe {
                libc::getpwuid_r(
                    libc::geteuid(),
                    &mut pwent,
                    &mut buffer[0],
                    16384,
                    &mut pwentp,
                );
            }

            pwent
        }
    }
    #[cfg(target_arch = "aarch64")]
    fn ptr_to_string(name: *mut u8) -> String {
        let uname = name as *const u8;

        let s;
        let string;

        unsafe {
            s = ::std::slice::from_raw_parts(uname, libc::strlen(name));
            string = String::from_utf8_lossy(s).to_string();
        }

        string
    }

    #[cfg(not(target_arch = "aarch64"))]
    fn ptr_to_string(name: *mut i8) -> String {
        let uname = name as *mut _ as *mut u8;

        let s;
        let string;

        unsafe {
            s = ::std::slice::from_raw_parts(uname, libc::strlen(name));
            string = String::from_utf8_lossy(s).to_string();
        }

        string
    }
    pub fn username() -> String {
        let pwent = getpwuid();

        ptr_to_string(pwent.pw_name)
    }
}

pub mod timer {
    use super::{MeliError, Result};
    use libc::clockid_t;
    use libc::sigevent;
    use libc::{itimerspec, timespec};
    use nix::sys::signal::{SigEvent, SigevNotify};
    use std::cell::RefCell;
    use std::convert::TryInto;
    use std::time::Duration;

    thread_local!(static TIMER_IDS: RefCell<u8> = RefCell::new(0));

    #[allow(non_camel_case_types)]
    pub type timer_t = libc::intptr_t;

    #[link(name = "rt")]
    extern "C" {
        fn timer_create(clockid: clockid_t, sevp: *const sigevent, timerid: *mut timer_t) -> i32;
        fn timer_settime(
            timerid: timer_t,
            flags: i32,
            new_value: *const itimerspec,
            old_value: *const itimerspec,
        ) -> i32;

        fn timer_delete(timerid: timer_t) -> i32;
    }

    #[derive(Debug)]
    pub struct PosixTimer {
        timer_id: timer_t,
        interval: Duration,
        value: Duration,
        pub si_value: u8,
    }

    impl Drop for PosixTimer {
        fn drop(&mut self) {
            unsafe {
                timer_delete(self.timer_id);
            }
        }
    }

    impl PosixTimer {
        pub fn rearm(&mut self) {
            debug!("posixtimer rearm");
            let spec = itimerspec {
                it_interval: timespec {
                    tv_sec: self.interval.as_secs().try_into().unwrap_or(0),
                    tv_nsec: self.interval.subsec_nanos().try_into().unwrap_or(0),
                },
                it_value: timespec {
                    tv_sec: self.value.as_secs().try_into().unwrap_or(0),
                    tv_nsec: self.value.subsec_nanos().try_into().unwrap_or(0),
                },
            };
            let ret =
                unsafe { timer_settime(self.timer_id, 0, &spec as *const _, std::ptr::null_mut()) };
            if ret != 0 {
                match ret {
                    libc::EFAULT => {
                        panic!(
                            "EFAULT: new_value, old_value, or curr_value is not a valid pointer."
                        );
                    }
                    libc::EINVAL => {
                        panic!("EINVAL: timerid is invalid.");
                    }
                    _ => {}
                }
            }
        }

        pub fn set_value(&mut self, value: Duration) {
            let spec = itimerspec {
                it_interval: timespec {
                    tv_sec: self.interval.as_secs().try_into().unwrap_or(0),
                    tv_nsec: self.interval.subsec_nanos().try_into().unwrap_or(0),
                },
                it_value: timespec {
                    tv_sec: value.as_secs().try_into().unwrap_or(0),
                    tv_nsec: value.subsec_nanos().try_into().unwrap_or(0),
                },
            };
            let ret =
                unsafe { timer_settime(self.timer_id, 0, &spec as *const _, std::ptr::null_mut()) };
            if ret != 0 {
                match ret {
                    libc::EFAULT => {
                        panic!(
                            "EFAULT: new_value, old_value, or curr_value is not a valid pointer."
                        );
                    }
                    libc::EINVAL => {
                        panic!("EINVAL: timerid is invalid.");
                    }
                    _ => {}
                }
            }
            self.value = value;
        }

        pub fn set_interval(&mut self, interval: Duration) {
            let spec = itimerspec {
                it_interval: timespec {
                    tv_sec: interval.as_secs().try_into().unwrap_or(0),
                    tv_nsec: interval.subsec_nanos().try_into().unwrap_or(0),
                },
                it_value: timespec {
                    tv_sec: self.value.as_secs().try_into().unwrap_or(0),
                    tv_nsec: self.value.subsec_nanos().try_into().unwrap_or(0),
                },
            };
            let ret =
                unsafe { timer_settime(self.timer_id, 0, &spec as *const _, std::ptr::null_mut()) };
            if ret != 0 {
                match ret {
                    libc::EFAULT => {
                        panic!(
                            "EFAULT: new_value, old_value, or curr_value is not a valid pointer."
                        );
                    }
                    libc::EINVAL => {
                        panic!("EINVAL: timerid is invalid.");
                    }
                    _ => {}
                }
            }
            self.interval = interval;
        }

        pub fn arm(&mut self, value: Duration) {
            let spec = itimerspec {
                it_interval: timespec {
                    tv_sec: self.interval.as_secs().try_into().unwrap_or(0),
                    tv_nsec: self.interval.subsec_nanos().try_into().unwrap_or(0),
                },
                it_value: timespec {
                    tv_sec: value.as_secs().try_into().unwrap_or(0),
                    tv_nsec: value.subsec_nanos().try_into().unwrap_or(0),
                },
            };
            let ret =
                unsafe { timer_settime(self.timer_id, 0, &spec as *const _, std::ptr::null_mut()) };
            if ret != 0 {
                match ret {
                    libc::EFAULT => {
                        panic!(
                            "EFAULT: new_value, old_value, or curr_value is not a valid pointer."
                        );
                    }
                    libc::EINVAL => {
                        panic!("EINVAL: timerid is invalid.");
                    }
                    _ => {}
                }
            }
            self.value = value;
        }

        pub fn new_with_signal(
            interval: Duration,
            value: Duration,
            signal: nix::sys::signal::Signal,
        ) -> Result<PosixTimer> {
            let mut timer_id = Default::default();

            let mut si_value = 0;
            TIMER_IDS.with(|t| {
                si_value = *t.borrow_mut();
                *t.borrow_mut() += 1;
            });

            let sigev_notify = SigevNotify::SigevSignal {
                signal,
                si_value: si_value as isize,
            };
            let event = SigEvent::new(sigev_notify);

            let ret = unsafe {
                timer_create(
                    libc::CLOCK_MONOTONIC,
                    &event.sigevent() as *const _,
                    &mut timer_id as *mut _,
                )
            };

            if ret != 0 {
                match ret {
                    libc::EAGAIN => {
                        return Err(MeliError::new(
                            "Temporary error during kernel allocation of timer",
                        ));
                    }
                    libc::EINVAL => {
                        panic!("Clock ID, sigev_notify, sigev_signo, or sigev_notify_thread_id is invalid.");
                    }
                    libc::ENOMEM => {
                        return Err(MeliError::new("Could not allocate memory."));
                    }
                    _ => {}
                }
            }

            let spec = itimerspec {
                it_interval: timespec {
                    tv_sec: interval.as_secs().try_into().unwrap_or(0),
                    tv_nsec: interval.subsec_nanos().try_into().unwrap_or(0),
                },
                it_value: timespec {
                    tv_sec: value.as_secs().try_into().unwrap_or(0),
                    tv_nsec: value.subsec_nanos().try_into().unwrap_or(0),
                },
            };

            let ret =
                unsafe { timer_settime(timer_id, 0, &spec as *const _, std::ptr::null_mut()) };
            if ret != 0 {
                match ret {
                    libc::EFAULT => {
                        panic!(
                            "EFAULT: new_value, old_value, or curr_value is not a valid pointer."
                        );
                    }
                    libc::EINVAL => {
                        panic!("EINVAL: timerid is invalid.");
                    }
                    _ => {}
                }
            }

            Ok(PosixTimer {
                timer_id,
                interval,
                value,
                si_value,
            })
        }
    }
}
