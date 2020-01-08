/*
 * meli - melib POSIX libc time interface
 *
 * Copyright 2020 Manos Pitsidianakis
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

use std::convert::TryInto;
use std::ffi::{CStr, CString};

pub type UnixTimestamp = u64;

use libc::{timeval, timezone};

extern "C" {
    fn strptime(
        s: *const ::std::os::raw::c_char,
        format: *const ::std::os::raw::c_char,
        tm: *mut ::libc::tm,
    ) -> *const ::std::os::raw::c_char;

    fn strftime(
        s: *mut ::std::os::raw::c_char,
        max: ::libc::size_t,
        format: *const ::std::os::raw::c_char,
        tm: *const ::libc::tm,
    ) -> ::libc::size_t;

    fn mktime(tm: *const ::libc::tm) -> ::libc::time_t;

    fn localtime_r(timep: *const ::libc::time_t, tm: *mut ::libc::tm) -> *mut ::libc::tm;

    fn gettimeofday(tv: *mut timeval, tz: *mut timezone) -> i32;
}

pub fn timestamp_to_string(timestamp: UnixTimestamp, fmt: Option<&str>) -> String {
    let mut new_tm: ::libc::tm = unsafe { std::mem::zeroed() };
    unsafe {
        let i: i64 = timestamp.try_into().unwrap_or(0);
        localtime_r(&i as *const i64, &mut new_tm as *mut ::libc::tm);
    }
    let fmt = fmt.map(|slice| CString::new(slice).unwrap());
    let format: &CStr = if let Some(ref s) = fmt {
        &s
    } else {
        unsafe { CStr::from_bytes_with_nul_unchecked(b"%a, %d %b %Y %T %z\0") }
    };
    let s: CString;
    unsafe {
        let mut vec: [u8; 256] = [0; 256];
        let ret = strftime(
            vec.as_mut_ptr() as *mut _,
            256,
            format.as_ptr(),
            &new_tm as *const _,
        );
        s = CString::new(&vec[0..ret]).unwrap();
    }

    s.to_string_lossy().to_string()
}

pub fn rfc822_to_timestamp<T>(s: T) -> UnixTimestamp
where
    T: Into<Vec<u8>>,
{
    let mut new_tm: ::libc::tm = unsafe { std::mem::zeroed() };
    unsafe {
        let fmt = CStr::from_bytes_with_nul_unchecked(b"%a, %e %h %Y %H:%M:%S %z\0");
        let ret = strptime(
            CString::new(s).unwrap().as_ptr(),
            fmt.as_ptr(),
            &mut new_tm as *mut _,
        );
        if ret.is_null() {
            return 0;
        }
        return mktime(&new_tm as *const _) as u64;
    }
}

pub fn timestamp_from_string<T>(s: T, fmt: &str) -> Option<UnixTimestamp>
where
    T: Into<Vec<u8>>,
{
    let mut new_tm: ::libc::tm = unsafe { std::mem::zeroed() };
    let fmt = CString::new(fmt).unwrap();
    unsafe {
        let ret = strptime(
            CString::new(s).unwrap().as_ptr(),
            fmt.as_ptr(),
            &mut new_tm as *mut _,
        );
        if ret.is_null() {
            return None;
        }
        return Some(mktime(&new_tm as *const _) as u64);
    }
}

pub fn now() -> UnixTimestamp {
    use std::mem::MaybeUninit;
    let mut tv = MaybeUninit::<::libc::timeval>::uninit();
    let mut tz = MaybeUninit::<::libc::timezone>::uninit();
    unsafe {
        let ret = gettimeofday(tv.as_mut_ptr(), tz.as_mut_ptr());
        if ret == -1 {
            unreachable!("gettimeofday returned -1");
        }
        (tv.assume_init()).tv_sec as UnixTimestamp
    }
}

#[test]
fn test_timestamp() {
    timestamp_to_string(0);
}
