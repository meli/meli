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

/*! UNIX and POSIX interfaces.
 */

pub mod timer {
    //! POSIX timers
    //!
    //! # Example usage
    //! ```no_run
    //! let timer = crate::timer::PosixTimer::new_with_signal(
    //!           std::time::Duration::from_secs(0),
    //!           std::time::Duration::from_secs(1),
    //!           nix::sys::signal::Signal::SIGALRM,
    //!       )
    //!       .unwrap();
    //!
    //! // some time passes, we should receive and handle the SIGALRM
    //! // The timer remains unarmed since the interval given was zero, until we rearm it explicitly.
    //! timer.rearm();
    //! ```
    use libc::clockid_t;
    use libc::sigevent;
    use libc::{itimerspec, timespec};
    use melib::{MeliError, Result};
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
        /// Interval for periodic timer.
        interval: Duration,
        /// Time until next expiration.
        value: Duration,
        /// `si_value` is a byte accessible from the signal handler when it receives signals from this timer.
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
        /// Arm without changing interval and value.
        pub fn rearm(&self) {
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

        /// Sets value without arming timer
        pub fn set_value(&mut self, value: Duration) -> &mut Self {
            self.value = value;
            self
        }

        /// Sets interval without arming timer
        pub fn set_interval(&mut self, interval: Duration) -> &mut Self {
            self.interval = interval;
            self
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

            let ret = PosixTimer {
                timer_id,
                interval,
                value,
                si_value,
            };

            ret.rearm();
            Ok(ret)
        }
    }
}
