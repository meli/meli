/*
 * melib - gpgme module
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

use std::io::{self, Read, Seek, Write};

use super::*;

#[repr(C)]
struct TagData {
    idx: usize,
    fd: ::std::os::raw::c_int,
    io_state: Arc<Mutex<IoState>>,
}

pub unsafe extern "C" fn gpgme_register_io_cb(
    data: *mut ::std::os::raw::c_void,
    fd: ::std::os::raw::c_int,
    dir: ::std::os::raw::c_int,
    fnc: gpgme_io_cb_t,
    fnc_data: *mut ::std::os::raw::c_void,
    tag: *mut *mut ::std::os::raw::c_void,
) -> gpgme_error_t {
    let io_state: Arc<Mutex<IoState>> = Arc::from_raw(data as *const _);
    let io_state_copy = io_state.clone();
    let mut io_state_lck = io_state.lock().unwrap();
    let idx = io_state_lck.max_idx;
    io_state_lck.max_idx += 1;
    let (sender, receiver) = smol::channel::unbounded();
    let gpgfd = GpgmeFd {
        fd,
        fnc,
        fnc_data,
        idx,
        write: dir == 0,
        sender,
        receiver,
        io_state: io_state_copy.clone(),
    };
    let tag_data = Arc::into_raw(Arc::new(TagData {
        idx,
        fd,
        io_state: io_state_copy,
    }));
    core::ptr::write(tag, tag_data as *mut _);
    io_state_lck.ops.insert(idx, gpgfd);
    drop(io_state_lck);
    let _ = Arc::into_raw(io_state);
    0
}

pub unsafe extern "C" fn gpgme_remove_io_cb(tag: *mut ::std::os::raw::c_void) {
    let tag_data: Arc<TagData> = Arc::from_raw(tag as *const _);
    let mut io_state_lck = tag_data.io_state.lock().unwrap();
    let fd = io_state_lck.ops.remove(&tag_data.idx).unwrap();
    fd.sender.try_send(()).unwrap();
    drop(io_state_lck);
    let _ = Arc::into_raw(tag_data);
}

pub unsafe extern "C" fn gpgme_event_io_cb(
    data: *mut ::std::os::raw::c_void,
    type_: gpgme_event_io_t,
    type_data: *mut ::std::os::raw::c_void,
) {
    if type_ == gpgme_event_io_t_GPGME_EVENT_DONE {
        let err = type_data as gpgme_io_event_done_data_t;
        let io_state: Arc<Mutex<IoState>> = Arc::from_raw(data as *const _);
        let io_state_lck = io_state.lock().unwrap();
        io_state_lck.sender.try_send(()).unwrap();
        *io_state_lck.done.lock().unwrap() = Some(gpgme_error_try(&io_state_lck.lib, (*err).err));
        drop(io_state_lck);
        let _ = Arc::into_raw(io_state);
    } else if type_ == gpgme_event_io_t_GPGME_EVENT_NEXT_KEY {
        if let Some(inner) = core::ptr::NonNull::new(type_data as gpgme_key_t) {
            let io_state: Arc<Mutex<IoState>> = Arc::from_raw(data as *const _);
            let io_state_lck = io_state.lock().unwrap();
            io_state_lck
                .key_sender
                .try_send(KeyInner { inner })
                .unwrap();
            drop(io_state_lck);
            let _ = Arc::into_raw(io_state);
        }
    }
}

impl Read for Data {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let result = unsafe {
            let (buf, len) = (buf.as_mut_ptr() as *mut _, buf.len());
            call!(self.lib, gpgme_data_read)(self.inner.as_ptr(), buf, len)
        };
        if result >= 0 {
            Ok(result as usize)
        } else {
            Err(io::Error::last_os_error())
        }
    }
}

impl Write for Data {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let result = unsafe {
            let (buf, len) = (buf.as_ptr() as *const _, buf.len());
            call!(self.lib, gpgme_data_write)(self.inner.as_ptr(), buf, len)
        };
        if result >= 0 {
            Ok(result as usize)
        } else {
            Err(io::Error::last_os_error())
        }
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Seek for Data {
    #[inline]
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        use std::convert::TryInto;
        let (off, whence) = match pos {
            io::SeekFrom::Start(off) => (off.try_into().unwrap_or(i64::MAX), libc::SEEK_SET),
            io::SeekFrom::End(off) => (off.saturating_abs(), libc::SEEK_END),
            io::SeekFrom::Current(off) => (off, libc::SEEK_CUR),
        };
        let result = unsafe { call!(self.lib, gpgme_data_seek)(self.inner.as_ptr(), off, whence) };
        if result >= 0 {
            Ok(result as u64)
        } else {
            Err(io::Error::last_os_error())
        }
    }
}
