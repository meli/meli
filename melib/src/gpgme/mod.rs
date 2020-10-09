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

use crate::email::{
    pgp::{DecryptionMetadata, Recipient},
    Address,
};
use crate::error::{ErrorKind, IntoMeliError, MeliError, Result, ResultIntoMeliError};
use futures::FutureExt;
use smol::Async;
use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{CStr, CString, OsStr};
use std::future::Future;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::Path;
use std::sync::{Arc, Mutex};

const GPGME_MIN_VERSION: &str = "1.12.0";

macro_rules! call {
    ($lib:expr, $func:ty) => {{
        let func: libloading::Symbol<$func> =
            $lib.get(stringify!($func).as_bytes()).expect(concat!(
                "Could not use libgpgme: symbol ",
                stringify!($func),
                " not found!"
            ));
        func
    }};
}

macro_rules! c_string_literal {
    ($lit:literal) => {{
        unsafe {
            CStr::from_bytes_with_nul_unchecked(concat!($lit, "\0").as_bytes()).as_ptr()
                as *const ::std::os::raw::c_char
        }
    }};
}
mod bindings;
use bindings::*;
mod io;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GpgmeFlag {
    ///"auto-key-retrieve"
    AutoKeyRetrieve,
    OfflineMode,
}

bitflags! {
    pub struct LocateKey: u8 {
        /// Locate a key using DNS CERT, as specified in RFC-4398.
        const CERT = 0b1;
        /// Locate a key using DNS PKA.
        const PKA  = 0b10;
        /// Locate a key using DANE, as specified in draft-ietf-dane-openpgpkey-05.txt.
        const DANE  = 0b100;
        /// Locate a key using the Web Key Directory protocol.
        const WKD  = 0b1000;
        /// Using DNS Service Discovery, check the domain in question for any LDAP keyservers to use. If this fails, attempt to locate the key using the PGP Universal method of checking ‘ldap://keys.(thedomain)’.
        const LDAP = 0b10000;
        /// Locate a key using a keyserver.
        const KEYSERVER  = 0b100000;
        /// In addition, a keyserver URL as used in the dirmngr configuration may be used here to query that particular keyserver.
        const KEYSERVER_URL = 0b1000000;
        /// Locate the key using the local keyrings. This mechanism allows the user to select the order a local key lookup is done. Thus using ‘--auto-key-locate local’ is identical to --no-auto-key-locate.
        const LOCAL = 0b10000000;
        /// This flag disables the standard local key lookup, done before any of the mechanisms defined by the --auto-key-locate are tried. The position of this mechanism in the list does not matter. It is not required if local is also used.
        const NODEFAULT = 0;
    }
}

struct IoState {
    max_idx: usize,
    ops: HashMap<usize, GpgmeFd>,
    done: Arc<Mutex<Option<Result<()>>>>,
    sender: smol::channel::Sender<()>,
    receiver: smol::channel::Receiver<()>,
    key_sender: smol::channel::Sender<KeyInner>,
    key_receiver: smol::channel::Receiver<KeyInner>,
    lib: Arc<libloading::Library>,
}

unsafe impl Send for IoState {}
unsafe impl Sync for IoState {}

pub struct ContextInner {
    inner: core::ptr::NonNull<gpgme_context>,
    lib: Arc<libloading::Library>,
}

unsafe impl Send for ContextInner {}
unsafe impl Sync for ContextInner {}

pub struct Context {
    inner: Arc<ContextInner>,
    io_state: Arc<Mutex<IoState>>,
}

unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Drop for ContextInner {
    #[inline]
    fn drop(&mut self) {
        unsafe { call!(self.lib, gpgme_release)(self.inner.as_mut()) }
    }
}

impl Context {
    pub fn new() -> Result<Self> {
        let version = CString::new(GPGME_MIN_VERSION).unwrap();
        let lib = Arc::new(libloading::Library::new(libloading::library_filename(
            "gpgme",
        ))?);
        if unsafe { call!(&lib, gpgme_check_version)(version.as_c_str().as_ptr() as *mut _) }
            .is_null()
        {
            return Err(MeliError::new(format!(
                "Could not use libgpgme: requested version compatible with {} but got {}",
                GPGME_MIN_VERSION,
                unsafe {
                    CStr::from_ptr(call!(&lib, gpgme_check_version)(std::ptr::null_mut()))
                        .to_string_lossy()
                },
            ))
            .set_kind(ErrorKind::External));
        };
        let (sender, receiver) = smol::channel::unbounded();
        let (key_sender, key_receiver) = smol::channel::unbounded();

        let mut ptr = core::ptr::null_mut();
        let io_state = Arc::new(Mutex::new(IoState {
            max_idx: 0,
            ops: HashMap::default(),
            done: Arc::new(Mutex::new(None)),
            sender,
            receiver,
            key_sender,
            key_receiver,
            lib: lib.clone(),
        }));
        let add_priv_data = io_state.clone();
        let event_priv_data = io_state.clone();

        let mut io_cbs = gpgme_io_cbs {
            add: Some(io::gpgme_register_io_cb),
            add_priv: Arc::into_raw(add_priv_data) as *mut ::std::os::raw::c_void, //add_priv: *mut ::std::os::raw::c_void,
            remove: Some(io::gpgme_remove_io_cb),
            event: Some(io::gpgme_event_io_cb),
            event_priv: Arc::into_raw(event_priv_data) as *mut ::std::os::raw::c_void, //pub event_priv: *mut ::std::os::raw::c_void,
        };

        unsafe {
            gpgme_error_try(&lib, call!(&lib, gpgme_new)(&mut ptr))?;
            call!(&lib, gpgme_set_io_cbs)(ptr, &mut io_cbs);
        }
        let ret = Context {
            inner: Arc::new(ContextInner {
                inner: core::ptr::NonNull::new(ptr).ok_or_else(|| {
                    MeliError::new("Could not use libgpgme").set_kind(ErrorKind::Bug)
                })?,
                lib,
            }),
            io_state,
        };
        ret.set_flag(GpgmeFlag::AutoKeyRetrieve, false)?;
        ret.set_flag(GpgmeFlag::OfflineMode, true)?;
        ret.set_auto_key_locate(LocateKey::LOCAL)?;
        Ok(ret)
    }

    fn set_flag_inner(
        &self,
        raw_flag: *const ::std::os::raw::c_char,
        raw_value: *const ::std::os::raw::c_char,
    ) -> Result<()> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_set_ctx_flag)(
                    self.inner.inner.as_ptr(),
                    raw_flag,
                    raw_value,
                ),
            )?;
        }
        Ok(())
    }

    pub fn set_flag(&self, flag: GpgmeFlag, value: bool) -> Result<()> {
        match flag {
            GpgmeFlag::AutoKeyRetrieve => {}
            GpgmeFlag::OfflineMode => {
                unsafe {
                    call!(&self.inner.lib, gpgme_set_offline)(
                        self.inner.inner.as_ptr(),
                        if value { 1 } else { 0 },
                    );
                };
                return Ok(());
            }
        };
        const VALUE_ON: &[u8; 2] = b"1\0";
        const VALUE_OFF: &[u8; 2] = b"0\0";
        let raw_flag = match flag {
            GpgmeFlag::AutoKeyRetrieve => c_string_literal!("auto-key-retrieve"),
            GpgmeFlag::OfflineMode => unreachable!(),
        };
        self.set_flag_inner(
            raw_flag,
            if value { VALUE_ON } else { VALUE_OFF }.as_ptr() as *const ::std::os::raw::c_char,
        )
    }

    fn get_flag_inner(
        &self,
        raw_flag: *const ::std::os::raw::c_char,
    ) -> *const ::std::os::raw::c_char {
        unsafe { call!(&self.inner.lib, gpgme_get_ctx_flag)(self.inner.inner.as_ptr(), raw_flag) }
    }

    pub fn get_flag(&self, flag: GpgmeFlag) -> Result<bool> {
        let raw_flag = match flag {
            GpgmeFlag::AutoKeyRetrieve => c_string_literal!("auto-key-retrieve"),
            GpgmeFlag::OfflineMode => {
                return Ok(unsafe {
                    call!(&self.inner.lib, gpgme_get_offline)(self.inner.inner.as_ptr()) > 0
                });
            }
        };
        let val = self.get_flag_inner(raw_flag);
        Ok(!val.is_null())
    }

    pub fn set_auto_key_locate(&self, val: LocateKey) -> Result<()> {
        let auto_key_locate: *const ::std::os::raw::c_char = c_string_literal!("auto-key-locate");
        if val == LocateKey::NODEFAULT {
            self.set_flag_inner(auto_key_locate, c_string_literal!("clear,nodefault"))
        } else {
            let mut accum = String::new();
            macro_rules! is_set {
                ($flag:expr, $string:literal) => {{
                    if val.intersects($flag) {
                        accum.push_str($string);
                        accum.push_str(",");
                    }
                }};
            }
            is_set!(LocateKey::CERT, "cert");
            is_set!(LocateKey::PKA, "pka");
            is_set!(LocateKey::WKD, "wkd");
            is_set!(LocateKey::LDAP, "ldap");
            is_set!(LocateKey::KEYSERVER, "keyserver");
            is_set!(LocateKey::KEYSERVER_URL, "keyserver-url");
            is_set!(LocateKey::LOCAL, "local");
            accum.pop();
            accum.push('\0');
            self.set_flag_inner(
                auto_key_locate,
                CStr::from_bytes_with_nul(accum.as_bytes())
                    .expect(accum.as_str())
                    .as_ptr() as *const _,
            )
        }
    }

    pub fn get_auto_key_locate(&self) -> Result<LocateKey> {
        let auto_key_locate: *const ::std::os::raw::c_char = c_string_literal!("auto-key-locate");
        let raw_value =
            unsafe { CStr::from_ptr(self.get_flag_inner(auto_key_locate)) }.to_string_lossy();
        let mut val = LocateKey::NODEFAULT;
        if !raw_value.contains("nodefault") {
            for mechanism in raw_value.split(",") {
                match mechanism {
                    "cert" => val.set(LocateKey::CERT, true),
                    "pka" => {
                        val.set(LocateKey::PKA, true);
                    }
                    "wkd" => {
                        val.set(LocateKey::WKD, true);
                    }
                    "ldap" => {
                        val.set(LocateKey::LDAP, true);
                    }
                    "keyserver" => {
                        val.set(LocateKey::KEYSERVER, true);
                    }
                    "keyserver-url" => {
                        val.set(LocateKey::KEYSERVER_URL, true);
                    }
                    "local" => {
                        val.set(LocateKey::LOCAL, true);
                    }
                    unknown => {
                        debug!("unknown mechanism: {}", unknown);
                    }
                }
            }
        }
        Ok(val)
    }

    pub fn new_data_mem(&self, bytes: &[u8]) -> Result<Data> {
        let mut ptr = core::ptr::null_mut();
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_data_new_from_mem)(
                    &mut ptr,
                    bytes.as_ptr() as *const ::std::os::raw::c_char,
                    bytes.len(),
                    1,
                ),
            )?;
        }

        Ok(Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            inner: core::ptr::NonNull::new(ptr).ok_or_else(|| {
                MeliError::new("Could not create libgpgme data").set_kind(ErrorKind::Bug)
            })?,
        })
    }

    pub fn new_data_file<P: AsRef<Path>>(&self, r: P) -> Result<Data> {
        let path: &Path = r.as_ref();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "File `{}` doesn't exist.",
                path.display()
            )));
        }
        let os_str: &OsStr = path.as_ref();
        let b = CString::new(os_str.as_bytes())?;
        let mut ptr = core::ptr::null_mut();
        unsafe {
            let ret: GpgmeError = call!(&self.inner.lib, gpgme_data_new_from_file)(
                &mut ptr,
                b.as_ptr() as *const ::std::os::raw::c_char,
                1,
            );
            gpgme_error_try(&self.inner.lib, ret)?;
        }

        Ok(Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            inner: core::ptr::NonNull::new(ptr).ok_or_else(|| {
                MeliError::new("Could not create libgpgme data").set_kind(ErrorKind::Bug)
            })?,
        })
    }

    pub fn verify(
        &mut self,
        mut signature: Data,
        mut text: Data,
    ) -> Result<impl Future<Output = Result<()>> + Send> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_verify_start)(
                    self.inner.inner.as_ptr(),
                    signature.inner.as_mut(),
                    text.inner.as_mut(),
                    std::ptr::null_mut(),
                ),
            )?;
        }

        let ctx = self.inner.clone();
        let io_state = self.io_state.clone();
        let io_state_lck = self.io_state.lock().unwrap();
        let done = io_state_lck.done.clone();
        let fut = io_state_lck
            .ops
            .values()
            .map(|a| Async::new(a.clone()).unwrap())
            .collect::<Vec<Async<GpgmeFd>>>();
        drop(io_state_lck);
        Ok(async move {
            let _s = signature;
            let _t = text;
            futures::future::join_all(fut.iter().map(|fut| {
                let done = done.clone();
                if fut.get_ref().write {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.write_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                } else {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.read_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                }
            }))
            .await;
            debug!("done with fut join");
            let rcv = {
                let io_state_lck = io_state.lock().unwrap();
                io_state_lck.receiver.clone()
            };
            let _ = rcv.recv().await;
            {
                let verify_result =
                    unsafe { call!(&ctx.lib, gpgme_op_verify_result)(ctx.inner.as_ptr()) };
                if verify_result.is_null() {
                    return Err(MeliError::new(
                        "Unspecified libgpgme error: gpgme_op_verify_result returned NULL.",
                    )
                    .set_err_kind(ErrorKind::External));
                }
                drop(verify_result);
            }
            let io_state_lck = io_state.lock().unwrap();
            let ret = io_state_lck
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(MeliError::new("Unspecified libgpgme error")));
            ret
        })
    }

    pub fn keylist(
        &mut self,
        secret: bool,
        pattern: Option<String>,
    ) -> Result<impl Future<Output = Result<Vec<Key>>>> {
        let pattern = if let Some(pattern) = pattern {
            Some(CString::new(pattern)?)
        } else {
            None
        };
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_keylist_start)(
                    self.inner.inner.as_ptr(),
                    pattern
                        .as_ref()
                        .map(|cs| cs.as_ptr())
                        .unwrap_or(std::ptr::null_mut())
                        as *const ::std::os::raw::c_char,
                    if secret { 1 } else { 0 },
                ),
            )?;
        }

        let ctx = self.inner.clone();
        let io_state = self.io_state.clone();
        let io_state_lck = self.io_state.lock().unwrap();
        let done = io_state_lck.done.clone();
        let fut = io_state_lck
            .ops
            .values()
            .map(|a| Async::new(a.clone()).unwrap())
            .collect::<Vec<Async<GpgmeFd>>>();
        drop(io_state_lck);
        Ok(async move {
            futures::future::join_all(fut.iter().map(|fut| {
                let done = done.clone();
                if fut.get_ref().write {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.write_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                } else {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.read_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                }
            }))
            .await;
            let (rcv, key_receiver) = {
                let io_state_lck = io_state.lock().unwrap();
                (
                    io_state_lck.receiver.clone(),
                    io_state_lck.key_receiver.clone(),
                )
            };
            let _ = rcv.recv().await;
            unsafe {
                gpgme_error_try(
                    &ctx.lib,
                    call!(&ctx.lib, gpgme_op_keylist_end)(ctx.inner.as_ptr()),
                )?;
            }
            let io_state_lck = io_state.lock().unwrap();
            io_state_lck
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(MeliError::new("Unspecified libgpgme error")))?;
            let mut keys = vec![];
            while let Ok(inner) = key_receiver.try_recv() {
                let key = Key::new(inner, ctx.lib.clone());
                keys.push(key);
            }
            Ok(keys)
        })
    }

    pub fn sign<'d>(
        &mut self,
        text: &'d mut Data,
    ) -> Result<impl Future<Output = Result<()>> + 'd> {
        let sig = std::ptr::null_mut();
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_sign_start)(
                    self.inner.inner.as_ptr(),
                    text.inner.as_mut(),
                    sig,
                    gpgme_sig_mode_t_GPGME_SIG_MODE_DETACH,
                ),
            )?;
        }

        let io_state = self.io_state.clone();
        let io_state_lck = self.io_state.lock().unwrap();
        let done = io_state_lck.done.clone();
        let fut = io_state_lck
            .ops
            .values()
            .map(|a| Async::new(a.clone()).unwrap())
            .collect::<Vec<Async<GpgmeFd>>>();
        drop(io_state_lck);
        Ok(async move {
            futures::future::join_all(fut.iter().map(|fut| {
                let done = done.clone();
                if fut.get_ref().write {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.write_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                } else {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.read_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                }
            }))
            .await;
            let rcv = {
                let io_state_lck = io_state.lock().unwrap();
                io_state_lck.receiver.clone()
            };
            let _ = rcv.recv().await;
            let io_state_lck = io_state.lock().unwrap();
            let ret = io_state_lck
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(MeliError::new("Unspecified libgpgme error")));
            ret
        })
    }

    pub fn decrypt(
        &mut self,
        mut cipher: Data,
    ) -> Result<impl Future<Output = Result<(DecryptionMetadata, Vec<u8>)>> + Send> {
        let mut plain: gpgme_data_t = std::ptr::null_mut();
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_data_new)(&mut plain),
            )?;
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_decrypt_start)(
                    self.inner.inner.as_ptr(),
                    cipher.inner.as_mut(),
                    plain,
                ),
            )?;
        }
        let mut plain = Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            inner: core::ptr::NonNull::new(plain).ok_or_else(|| {
                MeliError::new("internal libgpgme error").set_kind(ErrorKind::Bug)
            })?,
        };

        let ctx = self.inner.clone();
        let io_state = self.io_state.clone();
        let io_state_lck = self.io_state.lock().unwrap();
        let done = io_state_lck.done.clone();
        let fut = io_state_lck
            .ops
            .values()
            .map(|a| Async::new(a.clone()).unwrap())
            .collect::<Vec<Async<GpgmeFd>>>();
        drop(io_state_lck);
        Ok(async move {
            let _c = cipher;
            futures::future::join_all(fut.iter().map(|fut| {
                let done = done.clone();
                if fut.get_ref().write {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.write_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                } else {
                    futures::future::select(
                        fut.get_ref().receiver.recv().boxed(),
                        fut.read_with(move |_f| {
                            if done.lock().unwrap().is_some() {
                                return Ok(());
                            }
                            unsafe {
                                (fut.get_ref().fnc.unwrap())(
                                    fut.get_ref().fnc_data,
                                    fut.get_ref().fd,
                                )
                            };
                            if done.lock().unwrap().is_none() {
                                return Err(std::io::ErrorKind::WouldBlock.into());
                            }
                            Ok(())
                        })
                        .boxed(),
                    )
                    .boxed()
                }
            }))
            .await;
            let rcv = {
                let io_state_lck = io_state.lock().unwrap();
                io_state_lck.receiver.clone()
            };
            let _ = rcv.recv().await;
            let io_state_lck = io_state.lock().unwrap();
            io_state_lck
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(MeliError::new("Unspecified libgpgme error")))?;

            let decrypt_result =
                unsafe { call!(&ctx.lib, gpgme_op_decrypt_result)(ctx.inner.as_ptr()) };
            if decrypt_result.is_null() {
                return Err(MeliError::new(
                    "Unspecified libgpgme error: gpgme_op_decrypt_result returned NULL.",
                )
                .set_err_kind(ErrorKind::External));
            }
            let mut recipients = vec![];
            let is_mime;
            let file_name;
            let session_key;
            unsafe {
                is_mime = (*decrypt_result).is_mime() > 0;
                file_name = if !(*decrypt_result).file_name.is_null() {
                    Some(
                        CStr::from_ptr((*decrypt_result).file_name)
                            .to_string_lossy()
                            .to_string(),
                    )
                } else {
                    None
                };
                session_key = if !(*decrypt_result).session_key.is_null() {
                    Some(
                        CStr::from_ptr((*decrypt_result).session_key)
                            .to_string_lossy()
                            .to_string(),
                    )
                } else {
                    None
                };
                let mut recipient_iter = (*decrypt_result).recipients;
                while !recipient_iter.is_null() {
                    recipients.push(Recipient {
                        keyid: if !(*recipient_iter).keyid.is_null() {
                            Some(
                                CStr::from_ptr((*recipient_iter).keyid)
                                    .to_string_lossy()
                                    .to_string(),
                            )
                        } else {
                            None
                        },
                        status: gpgme_error_try(&ctx.lib, (*recipient_iter).status),
                    });
                    recipient_iter = (*recipient_iter).next;
                }
            }
            use std::io::Seek;
            /* Rewind cursor */
            plain
                .seek(std::io::SeekFrom::Start(0))
                .chain_err_summary(|| "libgpgme error: could not perform seek on plain text")?;
            Ok((
                DecryptionMetadata {
                    recipients,
                    file_name,
                    session_key,
                    is_mime,
                },
                plain.into_bytes()?,
            ))
        })
    }
}

fn gpgme_error_try(lib: &libloading::Library, error_code: GpgmeError) -> Result<()> {
    const ERR_MAX_LEN: usize = 256;
    if error_code == 0 {
        return Ok(());
    }
    let mut buf: Vec<u8> = vec![0; ERR_MAX_LEN];
    unsafe {
        call!(lib, gpgme_strerror_r)(
            error_code,
            buf.as_mut_ptr() as *mut ::std::os::raw::c_char,
            ERR_MAX_LEN,
        );
    }
    while buf.ends_with(&b"\0"[..]) {
        buf.pop();
    }
    Err(MeliError::from(
        String::from_utf8(buf)
            .unwrap_or_else(|err| String::from_utf8_lossy(&err.into_bytes()).to_string()),
    )
    .set_summary(format!("libgpgme error {}", error_code)))
}

#[derive(Debug)]
enum DataKind {
    Memory,
}

#[derive(Debug)]
pub struct Data {
    inner: core::ptr::NonNull<bindings::gpgme_data>,
    kind: DataKind,
    lib: Arc<libloading::Library>,
}

impl Data {
    pub fn into_bytes(mut self) -> Result<Vec<u8>> {
        use std::io::Read;
        let mut buf = vec![];
        self.read_to_end(&mut buf)?;
        Ok(buf)
    }
}

unsafe impl Send for Data {}
unsafe impl Sync for Data {}

impl Drop for Data {
    #[inline]
    fn drop(&mut self) {
        if !self.inner.as_ptr().is_null() {
            match self.kind {
                DataKind::Memory => unsafe {
                    call!(self.lib, gpgme_data_release)(self.inner.as_mut())
                },
            }
        }
    }
}

#[repr(C)]
#[derive(Clone)]
struct GpgmeFd {
    fd: RawFd,
    fnc: GpgmeIOCb,
    fnc_data: *mut ::std::os::raw::c_void,
    idx: usize,
    write: bool,
    sender: smol::channel::Sender<()>,
    receiver: smol::channel::Receiver<()>,
    io_state: Arc<Mutex<IoState>>,
}

unsafe impl Send for GpgmeFd {}
unsafe impl Sync for GpgmeFd {}

impl AsRawFd for GpgmeFd {
    fn as_raw_fd(&self) -> RawFd {
        self.fd
    }
}

#[derive(Clone)]
struct KeyInner {
    inner: core::ptr::NonNull<_gpgme_key>,
}

unsafe impl Send for KeyInner {}
unsafe impl Sync for KeyInner {}

pub struct Key {
    inner: KeyInner,
    lib: Arc<libloading::Library>,
}
unsafe impl Send for Key {}
unsafe impl Sync for Key {}

impl Clone for Key {
    fn clone(&self) -> Self {
        let lib = self.lib.clone();
        unsafe {
            call!(&self.lib, gpgme_key_ref)(self.inner.inner.as_ptr());
        }
        Key {
            inner: self.inner.clone(),
            lib,
        }
    }
}

impl Key {
    #[inline(always)]
    fn new(inner: KeyInner, lib: Arc<libloading::Library>) -> Self {
        Key { inner, lib }
    }

    pub fn primary_uid(&self) -> Option<Address> {
        unsafe {
            if (*(self.inner.inner.as_ptr())).uids.is_null() {
                return None;
            }
            let uid = (*(self.inner.inner.as_ptr())).uids;
            if (*uid).name.is_null() && (*uid).email.is_null() {
                None
            } else if (*uid).name.is_null() {
                Some(Address::new(
                    None,
                    CStr::from_ptr((*uid).email).to_string_lossy().to_string(),
                ))
            } else if (*uid).email.is_null() {
                Some(Address::new(
                    None,
                    CStr::from_ptr((*uid).name).to_string_lossy().to_string(),
                ))
            } else {
                Some(Address::new(
                    Some(CStr::from_ptr((*uid).name).to_string_lossy().to_string()),
                    CStr::from_ptr((*uid).email).to_string_lossy().to_string(),
                ))
            }
        }
    }

    pub fn revoked(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).revoked() > 0 }
    }

    pub fn expired(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).expired() > 0 }
    }

    pub fn disabled(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).disabled() > 0 }
    }

    pub fn invalid(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).invalid() > 0 }
    }

    pub fn can_encrypt(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).can_encrypt() > 0 }
    }

    pub fn can_sign(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).can_sign() > 0 }
    }

    pub fn secret(&self) -> bool {
        unsafe { (*self.inner.inner.as_ptr()).secret() > 0 }
    }

    pub fn fingerprint(&self) -> Cow<'_, str> {
        (unsafe { CStr::from_ptr((*(self.inner.inner.as_ptr())).fpr) }).to_string_lossy()
    }
}

impl std::fmt::Debug for Key {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Key")
            .field("fingerprint", &self.fingerprint())
            .field("uid", &self.primary_uid())
            .field("can_encrypt", &self.can_encrypt())
            .field("can_sign", &self.can_sign())
            .field("secret", &self.secret())
            .field("revoked", &self.revoked())
            .field("expired", &self.expired())
            .field("invalid", &self.invalid())
            .finish()
    }
}

impl Drop for Key {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            call!(&self.lib, gpgme_key_unref)(self.inner.inner.as_ptr());
        }
    }
}

//#[test]
//fn test_gpgme() {
//    std::thread::spawn(move || {
//        let ex = smol::Executor::new();
//        futures::executor::block_on(ex.run(futures::future::pending::<()>()));
//    });
//    let mut ctx = Context::new().unwrap();
//    //let sig = ctx.new_data_mem("sign").unwrap();
//    //let text = ctx.new_data_mem("file").unwrap();
//    let sig = ctx.new_data_mem(include_bytes!("/tmp/sig")).unwrap();
//    let text = ctx.new_data_mem(include_bytes!("/tmp/data")).unwrap();
//
//    futures::executor::block_on(ctx.verify(sig, text).unwrap()).unwrap();
//    println!(
//        "keys = {:#?}",
//        futures::executor::block_on(ctx.keylist().unwrap()).unwrap()
//    );
//    let cipher = ctx.new_data_file("/tmp/msg.asc").unwrap();
//    let plain = futures::executor::block_on(ctx.decrypt(cipher).unwrap()).unwrap();
//    println!(
//       "buf: {}",
//       String::from_utf8_lossy(&plain.into_bytes().unwrap())
//    );
//}
