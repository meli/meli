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
