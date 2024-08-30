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

use std::{
    ffi::{c_int, c_void},
    io::{self, Read, Seek, Write},
    mem::ManuallyDrop,
    os::fd::{FromRawFd, OwnedFd},
    ptr::NonNull,
};

use super::*;

#[derive(Debug)]
#[repr(C)]
struct TagData {
    idx: usize,
    fd: c_int,
    io_state: Arc<Mutex<IoState>>,
}

/// Wrapper type to automatically leak iostate Arc on drop.
#[repr(transparent)]
struct IoStateWrapper(ManuallyDrop<Arc<Mutex<IoState>>>);

impl IoStateWrapper {
    // SAFETY: `ptr` must be the iostate reference that was leaked in
    // `Context::new`.
    unsafe fn from_raw(ptr: *mut c_void) -> Self {
        let io_state: Arc<Mutex<IoState>> =
            unsafe { Arc::from_raw(ptr.cast_const().cast::<Mutex<IoState>>()) };
        Self(ManuallyDrop::new(io_state))
    }
}

impl Drop for IoStateWrapper {
    fn drop(&mut self) {
        // SAFETY: struct unit value is ManuallyDrop, so no Drop impls are called on the
        // uninit value.
        let inner = unsafe { ManuallyDrop::take(&mut self.0) };
        let _ = Arc::into_raw(inner);
    }
}

///
/// # Safety
///
/// Must only be used if `add_priv` in `gpgme_io_cbs` is `Arc<Mutex<IoState>>`.
pub unsafe extern "C" fn gpgme_register_io_cb(
    data: *mut c_void,
    fd: c_int,
    dir: c_int,
    fnc: gpgme_io_cb_t,
    fnc_data: *mut c_void,
    tag: *mut *mut c_void,
) -> gpgme_error_t {
    // SAFETY: This is the iostate reference that was leaked in `Context::new`.
    let io_state: IoStateWrapper = unsafe { IoStateWrapper::from_raw(data) };
    let io_state_copy = Arc::clone(&io_state.0);
    if let Ok(mut io_state_lck) = io_state.0.lock() {
        let idx = io_state_lck.max_idx;
        io_state_lck.max_idx += 1;
        let (sender, receiver) = smol::channel::unbounded();
        let gpgfd = {
            // SAFETY: `fd` is a valid file descriptor which we don't own, but wrapping it
            // in ManuallyDrop allows as to borrow it with AsFd trait.
            let fd = unsafe { OwnedFd::from_raw_fd(fd) };
            GpgmeFd {
                fd: ManuallyDrop::new(fd).into(),
                fnc,
                fnc_data,
                idx,
                write: dir == 0,
                sender,
                receiver,
                io_state: io_state_copy.clone(),
            }
        };
        {
            let tag_data = Arc::into_raw(Arc::new(TagData {
                idx,
                fd,
                io_state: io_state_copy,
            }));
            // SAFETY: tag_data is a valid pointer from the caller by contract, the tag_data
            // allocation is leaked and will only be accessed when the cb is removed.
            unsafe { std::ptr::write(tag, tag_data.cast_mut().cast::<c_void>()) };
        }
        io_state_lck.ops.insert(idx, gpgfd);
    }
    0
}

///
/// # Safety
///
/// The callback must have been registered with a `TagData` value, therefore
/// `tag` can only hold a valid `Arc<TagData>` allocation. We assume that gpgme
/// will only call the remove cb with this tag data once.
pub unsafe extern "C" fn gpgme_remove_io_cb(tag: *mut c_void) {
    let tag_data: Arc<TagData> = unsafe { Arc::from_raw(tag.cast_const().cast::<TagData>()) };
    let gpgfd: GpgmeFd = {
        let Ok(mut io_state_lck) = tag_data.io_state.lock() else {
            // mutex is poisoned, bail out.
            return;
        };
        io_state_lck.ops.remove(&tag_data.idx).unwrap_or_else(|| {
            panic!(
                "gpgme_remove_io_cb called with tag_data {:?}, but idx {} is not included in \
                 io_state ops. This is a bug.",
                tag_data, tag_data.idx
            )
        })
    };
    _ = gpgfd.sender.try_send(());
}

///
/// # Safety
///
/// Must only be used if `add_priv` in `gpgme_io_cbs` is `Arc<Mutex<IoState>>`.
pub unsafe extern "C" fn gpgme_event_io_cb(
    data: *mut c_void,
    r#type: gpgme_event_io_t,
    type_data: *mut c_void,
) {
    if r#type == gpgme_event_io_t_GPGME_EVENT_START {
        return;
    }

    // SAFETY: This is the iostate reference that was leaked in `Context::new`.
    let io_state: IoStateWrapper = unsafe { IoStateWrapper::from_raw(data) };

    if r#type == gpgme_event_io_t_GPGME_EVENT_DONE {
        let Some(status) = NonNull::new(type_data.cast::<gpgme_io_event_done_data>()) else {
            log::error!("gpgme_event_io_cb DONE event with NULL type_data. This is a gpgme bug.",);
            return;
        };
        // SAFETY: since type is DONE and type_data is not NULL, status is a valid
        // gpgme_io_event_done_data pointer.
        let err = unsafe { status.as_ref().err };
        if let Ok(io_state_lck) = io_state.0.lock() {
            _ = io_state_lck.sender.try_send(());
            if let Ok(mut done) = io_state_lck.done.lock() {
                *done = Some(gpgme_error_try(&io_state_lck.lib, err));
            }
        }
        return;
    }

    if r#type == gpgme_event_io_t_GPGME_EVENT_NEXT_KEY {
        let Some(ptr) = NonNull::new(type_data.cast::<_gpgme_key>()) else {
            log::error!(
                "gpgme_event_io_cb NEXT_KEY event with NULL type_data. This is a gpgme bug.",
            );
            return;
        };
        if let Ok(io_state_lck) = io_state.0.lock() {
            _ = io_state_lck.key_sender.try_send(KeyInner { ptr });
        }
        return;
    }

    log::error!(
        "gpgme_event_io_cb called with unexpected event type: {}",
        r#type
    );
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
