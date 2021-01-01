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

//! Functions for dealing with date strings and UNIX Epoch timestamps.
//!
//! # Examples
//!
//! ```rust
//! # use melib::datetime::*;
//! // Get current UNIX Epoch timestamp.
//! let now: UnixTimestamp = now();
//!
//! // Parse date from string
//! let date_val = "Wed, 8 Jan 2020 10:44:03 -0800";
//! let timestamp = rfc822_to_timestamp(date_val).unwrap();
//! assert_eq!(timestamp, 1578509043);
//!
//! // Convert timestamp back to string
//! let s = timestamp_to_string(timestamp, Some("%Y-%m-%d"));
//! assert_eq!(s, "2020-01-08");
//! ```
use crate::error::Result;
use std::convert::TryInto;
use std::ffi::{CStr, CString};

pub type UnixTimestamp = u64;

use libc::{timeval, timezone, uselocale};

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
    let fmt = fmt
        .map(CString::new)
        .map(|res| res.ok())
        .and_then(|opt| opt);
    let format: &CStr = if let Some(ref s) = fmt {
        &s
    } else {
        unsafe { CStr::from_bytes_with_nul_unchecked(b"%a, %d %b %Y %T %z\0") }
    };
    let mut vec: [u8; 256] = [0; 256];
    let ret = unsafe {
        strftime(
            vec.as_mut_ptr() as *mut _,
            256,
            format.as_ptr(),
            &new_tm as *const _,
        )
    };

    String::from_utf8_lossy(&vec[0..ret]).into_owned()
}

fn tm_to_secs(tm: ::libc::tm) -> std::result::Result<i64, ()> {
    let mut is_leap = false;
    let mut year = tm.tm_year;
    let mut month = tm.tm_mon;
    if month >= 12 || month < 0 {
        let mut adj = month / 12;
        month %= 12;
        if month < 0 {
            adj -= 1;
            month += 12;
        }
        year += adj;
    }
    let mut t = year_to_secs(year.into(), &mut is_leap)?;
    t += month_to_secs(month.try_into().unwrap_or(0), is_leap);
    t += 86400 * (tm.tm_mday - 1) as i64;
    t += 3600 * (tm.tm_hour) as i64;
    t += 60 * (tm.tm_min) as i64;
    t += tm.tm_sec as i64;
    Ok(t)
}

fn year_to_secs(year: i64, is_leap: &mut bool) -> std::result::Result<i64, ()> {
    if year < -100 {
        /* Sorry time travelers. */
        return Err(());
    }

    if year - 2 <= 136 {
        let y = year;
        let mut leaps = (y - 68) >> 2;
        if (y - 68) & 3 == 0 {
            leaps -= 1;
            *is_leap = true;
        } else {
            *is_leap = false;
        }
        return Ok((31536000 * (y - 70) + 86400 * leaps)
            .try_into()
            .unwrap_or(0));
    }

    let cycles = (year - 100) / 400;
    let centuries;
    let mut leaps;
    let mut rem;

    rem = (year - 100) % 400;

    if rem == 0 {
        *is_leap = true;
        centuries = 0;
        leaps = 0;
    } else {
        if rem >= 200 {
            if rem >= 300 {
                centuries = 3;
                rem -= 300;
            } else {
                centuries = 2;
                rem -= 200;
            }
        } else if rem >= 100 {
            centuries = 1;
            rem -= 100;
        } else {
            centuries = 0;
        }
        if rem == 0 {
            *is_leap = false;
            leaps = 0;
        } else {
            leaps = rem / 4;
            rem %= 4;
            *is_leap = rem == 0;
        }
    }

    leaps += 97 * cycles + 24 * centuries - if *is_leap { 1 } else { 0 };

    match (year - 100).overflowing_mul(31536000) {
        (_, true) => Err(()),
        (res, false) => Ok(res + leaps * 86400 + 946684800 + 86400),
    }
}

fn month_to_secs(month: usize, is_leap: bool) -> i64 {
    const SECS_THROUGH_MONTH: [i64; 12] = [
        0,
        31 * 86400,
        59 * 86400,
        90 * 86400,
        120 * 86400,
        151 * 86400,
        181 * 86400,
        212 * 86400,
        243 * 86400,
        273 * 86400,
        304 * 86400,
        334 * 86400,
    ];
    let mut t = SECS_THROUGH_MONTH[month];
    if is_leap && month >= 2 {
        t += 86400;
    }
    t
}

pub fn rfc822_to_timestamp<T>(s: T) -> Result<UnixTimestamp>
where
    T: Into<Vec<u8>>,
{
    let s = CString::new(s)?;
    let mut new_tm: ::libc::tm = unsafe { std::mem::zeroed() };
    for fmt in &[
        &b"%a, %e %h %Y %H:%M:%S \0"[..],
        &b"%e %h %Y %H:%M:%S \0"[..],
    ] {
        unsafe {
            let fmt = CStr::from_bytes_with_nul_unchecked(fmt);

            let locale = ::libc::newlocale(
                ::libc::LC_TIME,
                b"C\0".as_ptr() as *const i8,
                std::ptr::null_mut(),
            );

            let old_locale = uselocale(locale);
            let ret = strptime(s.as_ptr(), fmt.as_ptr(), &mut new_tm as *mut _);
            uselocale(old_locale);

            ::libc::freelocale(locale);

            if ret.is_null() {
                continue;
            }
            let rest = CStr::from_ptr(ret);
            let tm_gmtoff = if rest.to_bytes().len() > 4
                && rest.to_bytes().is_ascii()
                && rest.to_bytes()[1..5].iter().all(u8::is_ascii_digit)
            {
                let offset = std::str::from_utf8_unchecked(&rest.to_bytes()[0..5]);
                if let (Ok(mut hr_offset), Ok(mut min_offset)) =
                    (offset[1..3].parse::<i64>(), offset[3..5].parse::<i64>())
                {
                    if rest.to_bytes()[0] == b'-' {
                        hr_offset = -hr_offset;
                        min_offset = -min_offset;
                    }
                    hr_offset * 60 * 60 + min_offset * 60
                } else {
                    0
                }
            } else {
                let rest = if rest.to_bytes().starts_with(b"(") && rest.to_bytes().ends_with(b")") {
                    &rest.to_bytes()[1..rest.to_bytes().len() - 1]
                } else {
                    rest.to_bytes()
                };

                if let Ok(idx) = TIMEZONE_ABBR.binary_search_by(|probe| probe.0.cmp(rest)) {
                    let (hr_offset, min_offset) = TIMEZONE_ABBR[idx].1;
                    (hr_offset as i64) * 60 * 60 + (min_offset as i64) * 60
                } else {
                    0
                }
            };
            return Ok(tm_to_secs(new_tm)
                .map(|res| (res - tm_gmtoff) as u64)
                .unwrap_or(0));
        }
    }
    Ok(0)
}

pub fn rfc3339_to_timestamp<T>(s: T) -> Result<UnixTimestamp>
where
    T: Into<Vec<u8>>,
{
    let s = CString::new(s)?;
    let mut new_tm: ::libc::tm = unsafe { std::mem::zeroed() };
    for fmt in &[&b"%Y-%m-%dT%H:%M:%S\0"[..], &b"%Y-%m-%d\0"[..]] {
        unsafe {
            let fmt = CStr::from_bytes_with_nul_unchecked(fmt);
            let ret = strptime(s.as_ptr(), fmt.as_ptr(), &mut new_tm as *mut _);
            if ret.is_null() {
                continue;
            }
            let rest = CStr::from_ptr(ret);
            let tm_gmtoff = if rest.to_bytes().len() > 4
                && rest.to_bytes().is_ascii()
                && rest.to_bytes()[1..3].iter().all(u8::is_ascii_digit)
                && rest.to_bytes()[4..6].iter().all(u8::is_ascii_digit)
            {
                let offset = std::str::from_utf8_unchecked(&rest.to_bytes()[0..6]);
                if let (Ok(mut hr_offset), Ok(mut min_offset)) =
                    (offset[1..3].parse::<i64>(), offset[4..6].parse::<i64>())
                {
                    if rest.to_bytes()[0] == b'-' {
                        hr_offset = -hr_offset;
                        min_offset = -min_offset;
                    }
                    hr_offset * 60 * 60 + min_offset * 60
                } else {
                    0
                }
            } else {
                let rest = if rest.to_bytes().starts_with(b"(") && rest.to_bytes().ends_with(b")") {
                    &rest.to_bytes()[1..rest.to_bytes().len() - 1]
                } else {
                    rest.to_bytes()
                };

                if let Ok(idx) = TIMEZONE_ABBR.binary_search_by(|probe| probe.0.cmp(rest)) {
                    let (hr_offset, min_offset) = debug!(TIMEZONE_ABBR[idx]).1;
                    (hr_offset as i64) * 60 * 60 + (min_offset as i64) * 60
                } else {
                    0
                }
            };
            return Ok(tm_to_secs(new_tm)
                .map(|res| (res - tm_gmtoff) as u64)
                .unwrap_or(0));
        }
    }
    Ok(0)
}

// FIXME: Handle non-local timezone?
pub fn timestamp_from_string<T>(s: T, fmt: &str) -> Result<Option<UnixTimestamp>>
where
    T: Into<Vec<u8>>,
{
    let mut new_tm: ::libc::tm = unsafe { std::mem::zeroed() };
    let fmt = CString::new(fmt)?;
    unsafe {
        let ret = strptime(
            CString::new(s)?.as_ptr(),
            fmt.as_ptr(),
            &mut new_tm as *mut _,
        );
        if ret.is_null() {
            return Ok(None);
        }
        Ok(Some(mktime(&new_tm as *const _) as u64))
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
    timestamp_to_string(0, None);
}

#[test]
fn test_rfcs() {
    /* Some tests were lazily stolen from https://rachelbythebay.com/w/2013/06/11/time/ */

    assert_eq!(
        rfc822_to_timestamp("Wed, 8 Jan 2020 10:44:03 -0800").unwrap(),
        1578509043
    );

    /*
    macro_rules! mkt {
        ($year:literal, $month:literal, $day:literal, $hour:literal, $minute:literal, $second:literal) => {
            ::libc::tm {
                tm_sec: $second,
                tm_min: $minute,
                tm_hour: $hour,
                tm_mday: $day,
                tm_mon: $month - 1,
                tm_year: $year - 1900,
                tm_wday: 0,
                tm_yday: 0,
                tm_isdst: 0,
                tm_gmtoff: 0,
                tm_zone: std::ptr::null(),
            }
        };
    }
    */
    //unsafe { __tm_to_secs(&mkt!(2009, 02, 13, 23, 31, 30) as *const _) },
    assert_eq!(
        rfc822_to_timestamp("Fri, 13 Feb 2009 15:31:30 -0800").unwrap(),
        1234567890
    );

    //unsafe { __tm_to_secs(&mkt!(2931, 05, 05, 00, 33, 09) as *const _) },
    assert_eq!(
        rfc822_to_timestamp("Sat, 05 May 2931 00:33:09 +0000").unwrap(),
        30336942789
    );
    //2214-11-06 20:05:12 = 7726651512 [OK]
    assert_eq!(
        rfc822_to_timestamp("Sun, 06 Nov 2214 17:05:12 -0300").unwrap(), //2214-11-06 20:05:12
        7726651512
    );
    assert_eq!(
        rfc822_to_timestamp("Sun, 06 Nov 2214 17:05:12 -0300").unwrap(), //2214-11-06 20:05:12
        rfc822_to_timestamp("Sun, 06 Nov 2214 17:05:12 (ADT)").unwrap(), //2214-11-06 20:05:12
    );
    //2661-11-06 06:38:02 = 21832612682 [OK]
    assert_eq!(
        rfc822_to_timestamp("Wed, 06 Nov 2661 06:38:02 +0000").unwrap(), //2661-11-06 06:38:02
        21832612682
    );
    //2508-12-09 04:27:08 = 17007251228 [OK]
    assert_eq!(
        rfc822_to_timestamp("Sun, 09 Dec 2508 04:27:08 +0000").unwrap(), //2508-12-09 04:27:08
        17007251228
    );
    //2375-11-07 05:08:24 = 12807349704 [OK]
    assert_eq!(
        rfc822_to_timestamp("Fri, 07 Nov 2375 05:08:24 +0000").unwrap(), //2375-11-07 05:08:24
        12807349704
    );
    //2832-09-03 02:46:10 = 27223353970 [OK]
    assert_eq!(
        rfc822_to_timestamp("Fri, 03 Sep 2832 02:46:10 +0000").unwrap(), //2832-09-03 02:46:10
        27223353970
    );
    //2983-02-25 12:47:17 = 31972020437 [OK]
    assert_eq!(
        rfc822_to_timestamp("Tue, 25 Feb 2983 15:47:17 +0300").unwrap(), //2983-02-25 12:47:17
        31972020437
    );

    assert_eq!(
        rfc822_to_timestamp("Thu, 30 Mar 2017 17:32:06 +0300 (EEST)").unwrap(),
        1490884326
    );

    assert_eq!(
        rfc822_to_timestamp("Mon, 24 Apr 2017 17:36:34 +0530").unwrap(),
        1493035594
    );

    assert_eq!(
        rfc822_to_timestamp("Mon, 24 Apr 2017 17:36:34 +0530").unwrap(),
        rfc822_to_timestamp("Mon, 24 Apr 2017 12:06:34 +0000").unwrap(),
    );

    assert_eq!(
        rfc822_to_timestamp("Mon, 24 Apr 2017 17:36:34 +0530").unwrap(),
        rfc822_to_timestamp("Mon, 24 Apr 2017 17:36:34 (SLST)").unwrap(),
    );

    assert_eq!(
        rfc822_to_timestamp("Mon, 24 Apr 2017 17:36:34 +0530").unwrap(),
        rfc822_to_timestamp("Mon, 24 Apr 2017 17:36:34 SLST").unwrap(),
    );

    assert_eq!(
        rfc822_to_timestamp("27 Dec 2019 14:42:46 +0100").unwrap(),
        1577454166
    );

    assert_eq!(
        rfc822_to_timestamp("Mon, 16 Mar 2020 10:23:01 +0200").unwrap(),
        1584346981
    );
}

#[allow(clippy::zero_prefixed_literal)]
const TIMEZONE_ABBR: &[(&[u8], (i8, i8))] = &[
    (b"ACDT", (10, 30)),
    (b"ACST", (09, 30)),
    (b"ACT", (-05, 0)),
    (b"ACWST", (08, 45)),
    (b"ADT", (-03, 0)),
    (b"AEDT", (11, 0)),
    (b"AEST", (10, 0)),
    (b"AFT", (04, 30)),
    (b"AKDT", (-08, 0)),
    (b"AKST", (-09, 0)),
    (b"ALMT", (06, 0)),
    (b"AMST", (-03, 0)),
    (b"AMT", (-04, 0)), /* Amazon Time */
    (b"ANAT", (12, 0)),
    (b"AQTT", (05, 0)),
    (b"ART", (-03, 0)),
    (b"AST", (-04, 0)),
    (b"AST", (03, 0)),
    (b"AWST", (08, 0)),
    (b"AZOST", (0, 0)),
    (b"AZOT", (-01, 0)),
    (b"AZT", (04, 0)),
    (b"BDT", (08, 0)),
    (b"BIOT", (06, 0)),
    (b"BIT", (-12, 0)),
    (b"BOT", (-04, 0)),
    (b"BRST", (-02, 0)),
    (b"BRT", (-03, 0)),
    (b"BST", (06, 0)),
    (b"BTT", (06, 0)),
    (b"CAT", (02, 0)),
    (b"CCT", (06, 30)),
    (b"CDT", (-05, 0)),
    (b"CEST", (02, 0)),
    (b"CET", (01, 0)),
    (b"CHADT", (13, 45)),
    (b"CHAST", (12, 45)),
    (b"CHOST", (09, 0)),
    (b"CHOT", (08, 0)),
    (b"CHST", (10, 0)),
    (b"CHUT", (10, 0)),
    (b"CIST", (-08, 0)),
    (b"CIT", (08, 0)),
    (b"CKT", (-10, 0)),
    (b"CLST", (-03, 0)),
    (b"CLT", (-04, 0)),
    (b"COST", (-04, 0)),
    (b"COT", (-05, 0)),
    (b"CST", (-06, 0)),
    (b"CT", (08, 0)),
    (b"CVT", (-01, 0)),
    (b"CWST", (08, 45)),
    (b"CXT", (07, 0)),
    (b"DAVT", (07, 0)),
    (b"DDUT", (10, 0)),
    (b"DFT", (01, 0)),
    (b"EASST", (-05, 0)),
    (b"EAST", (-06, 0)),
    (b"EAT", (03, 0)),
    (b"ECT", (-05, 0)),
    (b"EDT", (-04, 0)),
    (b"EEST", (03, 0)),
    (b"EET", (02, 0)),
    (b"EGST", (0, 0)),
    (b"EGT", (-01, 0)),
    (b"EIT", (09, 0)),
    (b"EST", (-05, 0)),
    (b"FET", (03, 0)),
    (b"FJT", (12, 0)),
    (b"FKST", (-03, 0)),
    (b"FKT", (-04, 0)),
    (b"FNT", (-02, 0)),
    (b"GALT", (-06, 0)),
    (b"GAMT", (-09, 0)),
    (b"GET", (04, 0)),
    (b"GFT", (-03, 0)),
    (b"GILT", (12, 0)),
    (b"GIT", (-09, 0)),
    (b"GMT", (0, 0)),
    (b"GST", (04, 0)),
    (b"GYT", (-04, 0)),
    (b"HAEC", (02, 0)),
    (b"HDT", (-09, 0)),
    (b"HKT", (08, 0)),
    (b"HMT", (05, 0)),
    (b"HOVST", (08, 0)),
    (b"HOVT", (07, 0)),
    (b"HST", (-10, 0)),
    (b"ICT", (07, 0)),
    (b"IDLW", (-12, 0)),
    (b"IDT", (03, 0)),
    (b"IOT", (03, 0)),
    (b"IRDT", (04, 30)),
    (b"IRKT", (08, 0)),
    (b"IRST", (03, 30)),
    (b"IST", (05, 30)),
    (b"JST", (09, 0)),
    (b"KALT", (02, 0)),
    (b"KGT", (06, 0)),
    (b"KOST", (11, 0)),
    (b"KRAT", (07, 0)),
    (b"KST", (09, 0)),
    (b"LHST", (10, 30)),
    (b"LINT", (14, 0)),
    (b"MAGT", (12, 0)),
    (b"MART", (-09, -30)),
    (b"MAWT", (05, 0)),
    (b"MDT", (-06, 0)),
    (b"MEST", (02, 0)),
    (b"MET", (01, 0)),
    (b"MHT", (12, 0)),
    (b"MIST", (11, 0)),
    (b"MIT", (-09, -30)),
    (b"MMT", (06, 30)),
    (b"MSK", (03, 0)),
    (b"MST", (08, 0)),
    (b"MUT", (04, 0)),
    (b"MVT", (05, 0)),
    (b"MYT", (08, 0)),
    (b"NCT", (11, 0)),
    (b"NDT", (-02, -30)),
    (b"NFT", (11, 0)),
    (b"NOVT", (07, 0)),
    (b"NPT", (05, 45)),
    (b"NST", (-03, -30)),
    (b"NT", (-03, -30)),
    (b"NUT", (-11, 0)),
    (b"NZDT", (13, 0)),
    (b"NZST", (12, 0)),
    (b"OMST", (06, 0)),
    (b"ORAT", (05, 0)),
    (b"PDT", (-07, 0)),
    (b"PET", (-05, 0)),
    (b"PETT", (12, 0)),
    (b"PGT", (10, 0)),
    (b"PHOT", (13, 0)),
    (b"PHT", (08, 0)),
    (b"PKT", (05, 0)),
    (b"PMDT", (-02, 0)),
    (b"PMST", (-03, 0)),
    (b"PONT", (11, 0)),
    (b"PST", (-08, 0)),
    (b"PST", (08, 0)),
    (b"PYST", (-03, 0)),
    (b"PYT", (-04, 0)),
    (b"RET", (04, 0)),
    (b"ROTT", (-03, 0)),
    (b"SAKT", (11, 0)),
    (b"SAMT", (04, 0)),
    (b"SAST", (02, 0)),
    (b"SBT", (11, 0)),
    (b"SCT", (04, 0)),
    (b"SDT", (-10, 0)),
    (b"SGT", (08, 0)),
    (b"SLST", (05, 30)),
    (b"SRET", (11, 0)),
    (b"SRT", (-03, 0)),
    (b"SST", (08, 0)),
    (b"SYOT", (03, 0)),
    (b"TAHT", (-10, 0)),
    (b"TFT", (05, 0)),
    (b"THA", (07, 0)),
    (b"TJT", (05, 0)),
    (b"TKT", (13, 0)),
    (b"TLT", (09, 0)),
    (b"TMT", (05, 0)),
    (b"TOT", (13, 0)),
    (b"TRT", (03, 0)),
    (b"TVT", (12, 0)),
    (b"ULAST", (09, 0)),
    (b"ULAT", (08, 0)),
    (b"UTC", (0, 0)),
    (b"UYST", (-02, 0)),
    (b"UYT", (-03, 0)),
    (b"UZT", (05, 0)),
    (b"VET", (-04, 0)),
    (b"VLAT", (10, 0)),
    (b"VOLT", (04, 0)),
    (b"VOST", (06, 0)),
    (b"VUT", (11, 0)),
    (b"WAKT", (12, 0)),
    (b"WAST", (02, 0)),
    (b"WAT", (01, 0)),
    (b"WEST", (01, 0)),
    (b"WET", (0, 0)),
    (b"WIT", (07, 0)),
    (b"WST", (08, 0)),
    (b"YAKT", (09, 0)),
    (b"YEKT", (05, 0)),
];
