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

//! Interface for `libc`'s `fnmatch(3)`.

use std::ffi::CString;

mod ffi {
    #![allow(dead_code)]

    use std::os::raw::{c_char, c_int};

    use cfg_if::cfg_if;

    pub(super) const FNM_PERIOD: c_int = 1 << 2;
    pub(super) const FNM_CASEFOLD: c_int = 1 << 4;
    pub(super) const FNM_NOMATCH: c_int = 1;

    cfg_if! {
        if #[cfg(any(
                target_os = "macos",
                target_os = "freebsd",
                target_os = "android",
        ))] {
            pub(super) const FNM_PATHNAME: c_int = 1 << 1;
            pub(super) const FNM_NOESCAPE: c_int = 1 << 0;
        } else {
            pub(super) const FNM_PATHNAME: c_int = 1 << 0;
            pub(super) const FNM_NOESCAPE: c_int = 1 << 1;
        }
    }

    extern "C" {
        pub(super) fn fnmatch(pattern: *const c_char, name: *const c_char, flags: c_int) -> c_int;
    }
}

/// See `fnmatch(3)`.
pub trait Fnmatch {
    /// Returns `true` if glob pattern argument matches `self`.
    fn fnmatches(&self, pattern: &str) -> bool;
}

impl Fnmatch for str {
    fn fnmatches(&self, pattern: &str) -> bool {
        let (Ok(name_c), Ok(pattern_c)) = (CString::new(self), CString::new(pattern)) else {
            return false;
        };
        // SAFETY: both `pattern_c` and `name_c` are valid and NULL-terminated.
        unsafe {
            ffi::fnmatch(
                pattern_c.as_c_str().as_ptr(),
                name_c.as_c_str().as_ptr(),
                ffi::FNM_PERIOD,
            ) == 0
        }
    }
}
