//
// meli
//
// Copyright 2020- Manos Pitsidianakis <manos@pitsidianak.is>
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

use std::{
    collections::{HashMap, VecDeque},
    ffi::{c_int, c_void},
    io::{self, Read, Seek, Write},
    mem::ManuallyDrop,
    os::{
        fd::{AsFd, BorrowedFd, FromRawFd, OwnedFd},
        unix::io::{AsRawFd, RawFd},
    },
    ptr::NonNull,
    sync::{Arc, Mutex},
};

use futures::{
    future::Either,
    stream::{FuturesUnordered, StreamExt},
};

use super::{bindings::gpgme_io_event_done_data, *};

enum IoMessage {
    Register(GpgmeFd),
    Remove(Arc<TagData>),
    EventStart,
    EventDone(Result<()>),
    EventNextKey(KeyInner),
}

#[derive(Debug)]
#[repr(C)]
struct TagData {
    idx: usize,
    fd: c_int,
    io_state: Arc<Mutex<IoStateInner>>,
}

/// Wrapper type to automatically leak iostate Arc on drop.
#[repr(transparent)]
pub struct IoState(ManuallyDrop<Arc<Mutex<IoStateInner>>>);

impl IoState {
    // SAFETY: `ptr` must be the iostate reference that was leaked in
    // `Context::new`.
    unsafe fn from_raw(ptr: *mut c_void) -> ManuallyDrop<Self> {
        let io_state: Arc<Mutex<IoStateInner>> =
            // SAFETY: caller guarantees `ptr` is valid
            unsafe { Arc::from_raw(ptr.cast_const().cast::<Mutex<IoStateInner>>()) };
        ManuallyDrop::new(Self(ManuallyDrop::new(io_state)))
    }

    pub(super) fn new(lib: Arc<libloading::Library>) -> (Arc<Self>, gpgme_io_cbs) {
        let (key_sender, key_receiver) = smol::channel::unbounded();
        let (io_sender, io_receiver) = smol::channel::unbounded();
        let state = IoStateInner {
            max_idx: 0,
            key_sender,
            key_receiver,
            io_sender,
            io_receiver,
            lib,
        };

        let inner = Arc::new(Mutex::new(state));
        let add_priv = Arc::into_raw(Arc::clone(&inner))
            .cast_mut()
            .cast::<c_void>();
        let event_priv = Arc::into_raw(Arc::clone(&inner))
            .cast_mut()
            .cast::<c_void>();

        let io_cbs = gpgme_io_cbs {
            add: Some(gpgme_register_io_cb),
            add_priv,
            remove: Some(gpgme_remove_io_cb),
            event: Some(gpgme_event_io_cb),
            event_priv,
        };

        (Arc::new(Self(ManuallyDrop::new(inner))), io_cbs)
    }

    /// Wait for an operation to complete.
    pub(super) async fn wait_for_op(&self) -> Result<()> {
        let (receiver, key_sender) = {
            let lck = self.0.lock().unwrap();
            (lck.io_receiver.clone(), lck.key_sender.clone())
        };

        // To complete an operation, loop continuously until we get an I/O error, or if gpgme calls
        // `gpgme_event_io_cb` with `GPGME_EVENT_DONE`.

        // We will await two types of futures:
        // - A message generated from gpgme calling one of the `gpgme_io_cbs` callbacks
        // - A file descriptor becoming ready to read/write, upon which we will read/write to it by
        //   calling the associated callback

        type FutureSet<'a> = FuturesUnordered<
            futures::future::BoxFuture<'a, Either<Result<IoMessage>, (usize, std::io::Result<()>)>>,
        >;

        // Only start operation I/O upon receiving `GPGME_EVENT_START`.
        let mut started = false;

        let mut queue = VecDeque::new();
        let mut fds: HashMap<usize, Async<GpgmeFd>> = HashMap::new();
        loop {
            while let Some(io_message) = queue.pop_front() {
                match io_message {
                    IoMessage::Register(fd) => {
                        let idx = fd.idx;
                        let fut = Async::new(fd)?;

                        fds.insert(idx, fut);
                    }
                    IoMessage::Remove(tag_idx) => {
                        let idx = tag_idx.idx;
                        if fds.remove(&idx).is_none() {
                            log::error!(
                                "gpgme_remove_io_cb called with tag_data {tag_idx:?}, but idx \
                                 {idx} is not included in io_state ops. This is a bug.",
                            );
                        }
                    }
                    IoMessage::EventStart => {
                        started = true;
                    }
                    IoMessage::EventDone(result) => return result,
                    IoMessage::EventNextKey(key) => {
                        _ = key_sender.try_send(key);
                    }
                }
            }
            let mut fut_set: FutureSet = FuturesUnordered::new();
            fut_set.push({
                let receive_fut = receiver.recv();
                async move {
                    Either::Left(receive_fut.await.map_err(|_| {
                        Error::new("Could not use gpgme library")
                            .set_details("The IO event loop panicked.")
                            .set_kind(ErrorKind::Bug)
                    }))
                }
                .boxed()
            });
            if started {
                for fds in fds.values() {
                    let idx = fds.as_ref().idx;
                    if fds.as_ref().write {
                        fut_set.push(
                            async move {
                                let result = fds.writable().await;
                                Either::Right((idx, result))
                            }
                            .boxed(),
                        );
                    } else {
                        fut_set.push(
                            async move {
                                let result = fds.readable().await;
                                Either::Right((idx, result))
                            }
                            .boxed(),
                        );
                    }
                }
            }
            if let Some(next) = fut_set.next().await {
                match next {
                    Either::Left(Err(err)) => {
                        return Err(err);
                    }
                    Either::Left(Ok(io_message)) => {
                        queue.push_back(io_message);
                    }
                    Either::Right((idx, io_result)) => {
                        let f = fds[&idx].as_ref();
                        // SAFETY: gpgme provided `fnc`, `fnc_data` and the raw fd as valid
                        unsafe { (f.fnc.unwrap())(f.fnc_data, f.as_raw_fd()) };
                        io_result?;
                    }
                }
            }
        }
    }

    #[inline]
    pub(super) fn key_receiver(&self) -> Receiver<KeyInner> {
        self.0.lock().unwrap().key_receiver.clone()
    }
}

impl Drop for IoState {
    fn drop(&mut self) {
        // SAFETY: `self.0` is valid.
        let inner: Arc<Mutex<IoStateInner>> = unsafe { ManuallyDrop::take(&mut self.0) };
        let strong_count = Arc::strong_count(&inner);
        if strong_count >= 3 {
            // SAFETY: take add_priv reference
            unsafe { Arc::decrement_strong_count(Arc::as_ptr(&inner)) };
            // SAFETY: take event_priv reference
            unsafe { Arc::decrement_strong_count(Arc::as_ptr(&inner)) };
        }
        if strong_count != 3 && cfg!(debug_assertions) {
            eprintln!(
                "BUG: On Drop, IoState expects three references to Arc<Mutex<IoStateInner>> but \
                 got {strong_count}. This suggests a memory leak."
            );
        }
    }
}

///
/// # Safety
///
/// Must only be used if `add_priv` in `gpgme_io_cbs` is `Arc<Mutex<IoStateInner>>`.
unsafe extern "C" fn gpgme_register_io_cb(
    data: *mut c_void,
    fd: c_int,
    dir: c_int,
    fnc: gpgme_io_cb_t,
    fnc_data: *mut c_void,
    tag: *mut *mut c_void,
) -> gpgme_error_t {
    // SAFETY: This is the iostate reference that was leaked in `IoState::new`.
    let io_state = unsafe { IoState::from_raw(data) };
    let io_state_copy = Arc::clone(&io_state.0);
    if let Ok(mut io_state_lck) = io_state.0.lock() {
        let idx = io_state_lck.max_idx;
        io_state_lck.max_idx += 1;
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
                io_state: io_state_copy.clone(),
            }
        };
        {
            // The allocation is reclaimed in `gpgme_remove_io_cb`
            let tag_data = Arc::into_raw(Arc::new(TagData {
                idx,
                fd,
                io_state: io_state_copy,
            }));
            // SAFETY: tag is a valid pointer from the caller by contract, the tag_data allocation
            // is leaked and will only be accessed when the cb is removed in `gpgme_remove_io_cb`.
            unsafe { std::ptr::write(tag, tag_data.cast_mut().cast::<c_void>()) };
        }
        _ = io_state_lck.io_sender.try_send(IoMessage::Register(gpgfd));
    }
    0
}

///
/// # Safety
///
/// The callback must have been registered with a `TagData` value, therefore
/// `tag` can only hold a valid `Arc<TagData>` allocation. We assume that gpgme
/// will only call the remove cb with this tag data once.
unsafe extern "C" fn gpgme_remove_io_cb(tag: *mut c_void) {
    // SAFETY: tag is a valid pointer from the caller by contract, the tag_data allocation
    // was leaked in `gpgme_register_io_cb`
    let tag_data: Arc<TagData> = unsafe { Arc::from_raw(tag.cast_const().cast::<TagData>()) };
    let io_state = tag_data.io_state.clone();
    let Ok(io_state_lck) = io_state.lock() else {
        // mutex is poisoned, bail out.
        return;
    };
    _ = io_state_lck.io_sender.try_send(IoMessage::Remove(tag_data));
}

///
/// # Safety
///
/// Must only be used if `event_priv` in `gpgme_io_cbs` is `Arc<Mutex<IoStateInner>>`.
unsafe extern "C" fn gpgme_event_io_cb(
    data: *mut c_void,
    r#type: gpgme_event_io_t,
    type_data: *mut c_void,
) {
    // SAFETY: This is the iostate reference that was leaked in `IoState::new`.
    let io_state = unsafe { IoState::from_raw(data) };

    if r#type == gpgme_event_io_t::GPGME_EVENT_START {
        if let Ok(io_state_lck) = io_state.0.lock() {
            _ = io_state_lck.io_sender.try_send(IoMessage::EventStart);
        }
        return;
    }
    if r#type == gpgme_event_io_t::GPGME_EVENT_DONE {
        let Some(status) = NonNull::new(type_data.cast::<gpgme_io_event_done_data>()) else {
            log::error!("gpgme_event_io_cb DONE event with NULL type_data. This is a gpgme bug.",);
            return;
        };
        // SAFETY: since type is DONE and type_data is not NULL, status is a valid
        // gpgme_io_event_done_data pointer.
        let err = unsafe { status.as_ref().err };
        if let Ok(io_state_lck) = io_state.0.lock() {
            _ = io_state_lck
                .io_sender
                .try_send(IoMessage::EventDone(gpgme_error_try(
                    &io_state_lck.lib,
                    err,
                )));
        }
        return;
    }

    if r#type == gpgme_event_io_t::GPGME_EVENT_NEXT_KEY {
        let Some(ptr) = NonNull::new(type_data.cast::<_gpgme_key>()) else {
            log::error!(
                "gpgme_event_io_cb NEXT_KEY event with NULL type_data. This is a gpgme bug.",
            );
            return;
        };
        if let Ok(io_state_lck) = io_state.0.lock() {
            _ = io_state_lck
                .io_sender
                .try_send(IoMessage::EventNextKey(KeyInner { ptr }));
        }
        return;
    }

    log::error!(
        "gpgme_event_io_cb called with unexpected event type: {}",
        r#type as u32
    );
}

impl Read for Data {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let result = unsafe {
            let (buf, len) = (buf.as_mut_ptr() as *mut _, buf.len());
            call!(self.lib, gpgme_data_read)(
                self.inner.as_ptr(),
                buf,
                len.try_into()
                    .map_err(|_| io::Error::from_raw_os_error(libc::EOVERFLOW))?,
            )
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
            call!(self.lib, gpgme_data_write)(
                self.inner.as_ptr(),
                buf,
                len.try_into()
                    .map_err(|_| io::Error::from_raw_os_error(libc::EOVERFLOW))?,
            )
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
            io::SeekFrom::Start(off) => (
                off.try_into()
                    .map_err(|_| io::Error::from_raw_os_error(libc::EOVERFLOW))?,
                libc::SEEK_SET,
            ),
            io::SeekFrom::End(off) => (off.saturating_abs(), libc::SEEK_END),
            io::SeekFrom::Current(off) => (off, libc::SEEK_CUR),
        };
        let result = unsafe {
            // Allow .into() for both 32bit and 64bit targets
            #[allow(clippy::useless_conversion)]
            call!(self.lib, gpgme_data_seek)(
                self.inner.as_ptr(),
                libc::off_t::try_from(off)
                    .map_err(|_| io::Error::from_raw_os_error(libc::EOVERFLOW))?
                    .into(),
                whence,
            )
        };
        if result >= 0 {
            Ok(u64::try_from(result).map_err(|_| io::Error::from_raw_os_error(libc::EOVERFLOW))?)
        } else {
            Err(io::Error::last_os_error())
        }
    }
}

#[derive(Debug)]
pub struct Data {
    inner: NonNull<bindings::gpgme_data>,
    lib: Arc<libloading::Library>,
}

impl Data {
    pub fn new(lib: Arc<libloading::Library>) -> Result<Self> {
        let mut inner: gpgme_data_t = std::ptr::null_mut();
        unsafe {
            gpgme_error_try(&lib, call!(&lib, gpgme_data_new)(&raw mut inner))?;
        }
        let inner = NonNull::new(inner).ok_or_else(|| {
            Error::new("internal libgpgme error").set_kind(ErrorKind::LinkedLibrary("gpgme"))
        })?;
        Ok(Self { lib, inner })
    }

    pub fn new_mem(lib: Arc<libloading::Library>, bytes: &[u8]) -> Result<Self> {
        let mut ptr = std::ptr::null_mut();
        unsafe {
            gpgme_error_try(
                &lib,
                call!(&lib, gpgme_data_new_from_mem)(
                    &raw mut ptr,
                    bytes.as_ptr() as *const ::std::os::raw::c_char,
                    bytes
                        .len()
                        .try_into()
                        .map_err(|_| std::io::Error::from_raw_os_error(libc::EOVERFLOW))?,
                    1,
                ),
            )?;
        }

        Ok(Self {
            lib,
            inner: NonNull::new(ptr).ok_or_else(|| {
                Error::new("Could not create libgpgme data").set_kind(ErrorKind::Bug)
            })?,
        })
    }

    pub fn into_bytes(mut self) -> Result<Vec<u8>> {
        use std::io::Read;
        let mut buf = vec![];
        self.read_to_end(&mut buf)?;
        Ok(buf)
    }

    pub const fn as_ptr(&mut self) -> *mut bindings::gpgme_data {
        self.inner.as_ptr()
    }
}

unsafe impl Send for Data {}
unsafe impl Sync for Data {}

impl Drop for Data {
    #[inline]
    fn drop(&mut self) {
        unsafe { call!(self.lib, gpgme_data_release)(self.inner.as_mut()) };
    }
}

#[derive(Clone)]
#[repr(C)]
struct GpgmeFd {
    fd: Arc<ManuallyDrop<OwnedFd>>,
    fnc: gpgme_io_cb_t,
    fnc_data: *mut c_void,
    idx: usize,
    write: bool,
    io_state: Arc<Mutex<IoStateInner>>,
}

impl std::fmt::Debug for GpgmeFd {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(identify!(GpgmeFd))
            .field("idx", &self.idx)
            .field("fd", &self.fd)
            .field("fnc", &self.fnc)
            .field("fnc_data", &self.fnc_data)
            .field("write", &self.write)
            .finish_non_exhaustive()
    }
}

unsafe impl Send for GpgmeFd {}
unsafe impl Sync for GpgmeFd {}

impl AsRawFd for GpgmeFd {
    fn as_raw_fd(&self) -> RawFd {
        self.fd.as_raw_fd()
    }
}

impl AsFd for GpgmeFd {
    fn as_fd(&'_ self) -> BorrowedFd<'_> {
        self.fd.as_fd()
    }
}

#[derive(Debug)]
struct IoStateInner {
    max_idx: usize,
    io_sender: Sender<IoMessage>,
    io_receiver: Receiver<IoMessage>,
    key_sender: Sender<KeyInner>,
    key_receiver: Receiver<KeyInner>,
    lib: Arc<libloading::Library>,
}
