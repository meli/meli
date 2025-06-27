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

// [ref:DEBT] segfaults on libgpgme code can crash the entire app; it should be
// handled.

use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::{c_void, CStr, CString, OsStr},
    future::Future,
    io::Seek,
    mem::ManuallyDrop,
    os::{
        fd::{AsFd, BorrowedFd, OwnedFd},
        unix::{
            ffi::OsStrExt,
            io::{AsRawFd, RawFd},
        },
    },
    path::Path,
    pin::Pin,
    ptr::NonNull,
    sync::{Arc, Mutex},
};

use futures::FutureExt;
use serde::{
    de::{self, Deserialize},
    Deserializer, Serialize, Serializer,
};
use smol::{
    channel::{Receiver, Sender},
    Async,
};

use crate::{
    email::pgp::{DecryptionMetadata, Recipient},
    error::{Error, ErrorKind, Result, ResultIntoError},
};

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

#[allow(
    non_camel_case_types,
    non_upper_case_globals,
    non_snake_case,
    clippy::useless_transmute,
    clippy::too_many_arguments,
    clippy::use_self
)]
pub mod bindings;
#[cfg(test)]
mod tests;
use bindings::*;
pub mod key;
pub use key::*;
pub mod io;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GpgmeFlag {
    /// "auto-key-retrieve"
    AutoKeyRetrieve,
    OfflineMode,
    AsciiArmor,
}

impl GpgmeFlag {
    // SAFETY: Value is NUL terminated.
    const AUTO_KEY_RETRIEVE: &'static CStr =
        unsafe { CStr::from_bytes_with_nul_unchecked(b"auto-key-retrieve\0") };
    // SAFETY: Value is NUL terminated.
    const AUTO_KEY_LOCATE: &'static CStr =
        unsafe { CStr::from_bytes_with_nul_unchecked(b"auto-key-locate\0") };
}

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl<'de> Deserialize<'de> for LocateKey {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        <String>::deserialize(deserializer).map_or_else(
            |_| Err(de::Error::custom("LocateKey value must be a string.")),
            |s| Self::from_string_de::<'de, D, String>(s),
        )
    }
}

impl Serialize for LocateKey {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl LocateKey {
    pub fn from_string_de<'de, D, T: AsRef<str>>(s: T) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(match s.as_ref().trim() {
            s if s.eq_ignore_ascii_case("cert") => Self::CERT,
            s if s.eq_ignore_ascii_case("pka") => Self::PKA,
            s if s.eq_ignore_ascii_case("dane") => Self::DANE,
            s if s.eq_ignore_ascii_case("wkd") => Self::WKD,
            s if s.eq_ignore_ascii_case("ldap") => Self::LDAP,
            s if s.eq_ignore_ascii_case("keyserver") => Self::KEYSERVER,
            s if s.eq_ignore_ascii_case("keyserver-url") => Self::KEYSERVER_URL,
            s if s.eq_ignore_ascii_case("local") => Self::LOCAL,
            combination if combination.contains(',') => {
                let mut ret = Self::NODEFAULT;
                for c in combination.trim().split(',') {
                    ret |= Self::from_string_de::<'de, D, &str>(c.trim())?;
                }
                ret
            }
            _ => {
                return Err(de::Error::custom(
                    r#"Takes valid auto-key-locate GPG values: "cert", "pka", "dane", "wkd", "ldap", "keyserver", "keyserver-URL", "local", "nodefault""#,
                ))
            }
        })
    }
}

impl std::fmt::Display for LocateKey {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if *self == Self::NODEFAULT {
            write!(fmt, "clear,nodefault")
        } else {
            let mut accum = String::new();
            macro_rules! is_set {
                ($flag:expr, $string:literal) => {{
                    if self.intersects($flag) {
                        accum.push_str($string);
                        accum.push(',');
                    }
                }};
            }
            is_set!(Self::CERT, "cert");
            is_set!(Self::PKA, "pka");
            is_set!(Self::WKD, "wkd");
            is_set!(Self::LDAP, "ldap");
            is_set!(Self::KEYSERVER, "keyserver");
            is_set!(Self::KEYSERVER_URL, "keyserver-url");
            is_set!(Self::LOCAL, "local");
            accum.pop();
            write!(fmt, "{accum}")
        }
    }
}

type Done = Arc<Mutex<Option<Result<()>>>>;

#[derive(Debug)]
struct IoState {
    max_idx: usize,
    ops: HashMap<usize, GpgmeFd>,
    done: Done,
    sender: Sender<()>,
    receiver: Receiver<()>,
    key_sender: Sender<KeyInner>,
    key_receiver: Receiver<KeyInner>,
    // ops: HashMap<usize, Arc<GpgmeFd>>,
    lib: Arc<libloading::Library>,
}

unsafe impl Send for IoState {}
unsafe impl Sync for IoState {}

pub struct ContextInner {
    ptr: NonNull<gpgme_context>,
    lib: Arc<libloading::Library>,
}

unsafe impl Send for ContextInner {}
unsafe impl Sync for ContextInner {}

#[derive(Clone)]
pub struct Context {
    inner: Arc<ContextInner>,
    io_state: Arc<IoStateWrapper>,
}

impl Drop for ContextInner {
    #[inline]
    fn drop(&mut self) {
        unsafe { call!(self.lib, gpgme_release)(self.ptr.as_mut()) }
    }
}

impl Context {
    pub fn new() -> Result<Self> {
        let lib = Arc::new(
            match unsafe { libloading::Library::new(libloading::library_filename("gpgme")) } {
                Ok(v) => v,
                Err(err) => {
                    let source = Error::from(err).set_kind(ErrorKind::LinkedLibrary("gpgme"));
                    let mut err =
                        Error::new("Could not use libgpgme").set_kind(ErrorKind::NotFound);
                    err.source = Some(Box::new(source));
                    return Err(err);
                }
            },
        );
        if unsafe { call!(&lib, gpgme_check_version)(GPGME_VERSION.as_ptr()) }.is_null() {
            return Err(Error::new(format!(
                "Could not use libgpgme: requested version compatible with {} but got {}",
                GPGME_VERSION.to_string_lossy(),
                unsafe {
                    CStr::from_ptr(call!(&lib, gpgme_check_version)(std::ptr::null_mut()))
                        .to_string_lossy()
                },
            ))
            .set_kind(ErrorKind::LinkedLibrary("gpgme")));
        };
        let (sender, receiver) = smol::channel::unbounded();
        let (key_sender, key_receiver) = smol::channel::unbounded();

        let mut ptr = std::ptr::null_mut();

        let (io_state, mut io_cbs) = IoStateWrapper::new(IoState {
            max_idx: 0,
            ops: HashMap::default(),
            done: Arc::new(Mutex::new(None)),
            sender,
            receiver,
            key_sender,
            key_receiver,
            lib: lib.clone(),
        });

        unsafe {
            gpgme_error_try(&lib, call!(&lib, gpgme_new)(&mut ptr))?;
            call!(&lib, gpgme_set_io_cbs)(ptr, &mut io_cbs);
        }
        let mut ret = Self {
            inner: Arc::new(ContextInner {
                ptr: NonNull::new(ptr).ok_or_else(|| {
                    Error::new("Could not use libgpgme")
                        .set_details(
                            "gpgme_new
                            returned a NULL value.",
                        )
                        .set_kind(ErrorKind::LinkedLibrary("gpgme"))
                })?,
                lib,
            }),
            io_state,
        };
        ret.set_flag(GpgmeFlag::AutoKeyRetrieve, false)?
            .set_flag(GpgmeFlag::OfflineMode, true)?
            .set_flag(GpgmeFlag::AsciiArmor, true)?
            .set_auto_key_locate(LocateKey::LOCAL)?;
        Ok(ret)
    }

    fn set_flag_inner(&self, raw_flag: &'static CStr, raw_value: &CStr) -> Result<()> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_set_ctx_flag)(
                    self.inner.ptr.as_ptr(),
                    raw_flag.as_ptr(),
                    raw_value.as_ptr(),
                ),
            )?;
        }
        Ok(())
    }

    pub fn set_flag(&mut self, flag: GpgmeFlag, value: bool) -> Result<&mut Self> {
        match flag {
            GpgmeFlag::AutoKeyRetrieve => {}
            GpgmeFlag::OfflineMode => {
                unsafe {
                    call!(&self.inner.lib, gpgme_set_offline)(
                        self.inner.ptr.as_ptr(),
                        if value { 1 } else { 0 },
                    );
                };
                return Ok(self);
            }
            GpgmeFlag::AsciiArmor => {
                unsafe {
                    call!(&self.inner.lib, gpgme_set_armor)(
                        self.inner.ptr.as_ptr(),
                        if value { 1 } else { 0 },
                    );
                };
                return Ok(self);
            }
        };
        // SAFETY: Value is NUL terminated.
        const VALUE_ON: &CStr = unsafe { CStr::from_bytes_with_nul_unchecked(b"1\0") };
        // SAFETY: Value is NUL terminated.
        const VALUE_OFF: &CStr = unsafe { CStr::from_bytes_with_nul_unchecked(b"0\0") };
        let raw_flag = match flag {
            GpgmeFlag::AutoKeyRetrieve => GpgmeFlag::AUTO_KEY_RETRIEVE,
            GpgmeFlag::AsciiArmor | GpgmeFlag::OfflineMode => unreachable!(),
        };
        self.set_flag_inner(raw_flag, if value { VALUE_ON } else { VALUE_OFF })?;
        Ok(self)
    }

    fn get_flag_inner(&self, raw_flag: &'static CStr) -> *const ::std::os::raw::c_char {
        unsafe {
            call!(&self.inner.lib, gpgme_get_ctx_flag)(self.inner.ptr.as_ptr(), raw_flag.as_ptr())
        }
    }

    pub fn get_flag(&self, flag: GpgmeFlag) -> Result<bool> {
        let raw_flag = match flag {
            GpgmeFlag::AutoKeyRetrieve => GpgmeFlag::AUTO_KEY_RETRIEVE,
            GpgmeFlag::OfflineMode => {
                return Ok(unsafe {
                    call!(&self.inner.lib, gpgme_get_offline)(self.inner.ptr.as_ptr()) > 0
                });
            }
            GpgmeFlag::AsciiArmor => {
                return Ok(unsafe {
                    call!(&self.inner.lib, gpgme_get_armor)(self.inner.ptr.as_ptr()) > 0
                });
            }
        };
        let val = self.get_flag_inner(raw_flag);
        Ok(!val.is_null())
    }

    pub fn set_auto_key_locate(&mut self, val: LocateKey) -> Result<&mut Self> {
        if val == LocateKey::NODEFAULT {
            self.set_flag_inner(
                GpgmeFlag::AUTO_KEY_LOCATE,
                // SAFETY: Value is NUL terminated.
                unsafe { CStr::from_bytes_with_nul_unchecked(b"clear,nodefault\0") },
            )?;
        } else {
            let mut accum = val.to_string();
            accum.push('\0');
            self.set_flag_inner(
                GpgmeFlag::AUTO_KEY_LOCATE,
                CStr::from_bytes_with_nul(accum.as_bytes())
                    .map_err(|err| format!("Expected `{}`: {}", accum.as_str(), err))?,
            )?;
        }
        Ok(self)
    }

    pub fn get_auto_key_locate(&self) -> Result<LocateKey> {
        let raw_value = unsafe { CStr::from_ptr(self.get_flag_inner(GpgmeFlag::AUTO_KEY_LOCATE)) }
            .to_string_lossy();
        let mut val = LocateKey::NODEFAULT;
        if !raw_value.contains("nodefault") {
            for mechanism in raw_value.split(',') {
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
        let mut ptr = std::ptr::null_mut();
        let bytes: Pin<Vec<u8>> = Pin::new(bytes.to_vec());
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_data_new_from_mem)(
                    &mut ptr,
                    bytes.as_ptr() as *const ::std::os::raw::c_char,
                    bytes
                        .len()
                        .try_into()
                        .map_err(|_| std::io::Error::from_raw_os_error(libc::EOVERFLOW))?,
                    1,
                ),
            )?;
        }

        Ok(Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            bytes,
            inner: NonNull::new(ptr).ok_or_else(|| {
                Error::new("Could not create libgpgme data").set_kind(ErrorKind::Bug)
            })?,
        })
    }

    pub fn new_data_file<P: AsRef<Path>>(&self, r: P) -> Result<Data> {
        let path: &Path = r.as_ref();
        if !path.exists() {
            return Err(Error::new(format!(
                "File `{}` doesn't exist.",
                path.display()
            )));
        }
        let os_str: &OsStr = path.as_ref();
        let bytes = Pin::new(os_str.as_bytes().to_vec());
        let mut ptr = std::ptr::null_mut();
        unsafe {
            let ret: gpgme_error_t = call!(&self.inner.lib, gpgme_data_new_from_file)(
                &mut ptr,
                bytes.as_ptr() as *const ::std::os::raw::c_char,
                1,
            );
            gpgme_error_try(&self.inner.lib, ret)?;
        }

        Ok(Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            bytes,
            inner: NonNull::new(ptr).ok_or_else(|| {
                Error::new("Could not create libgpgme data").set_kind(ErrorKind::Bug)
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
                    self.inner.ptr.as_ptr(),
                    signature.inner.as_mut(),
                    text.inner.as_mut(),
                    std::ptr::null_mut(),
                ),
            )?;
        }

        let ctx = self.clone();
        let (done, fut) = self.io_state.done_fut()?;
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
                                    fut.get_ref().as_raw_fd(),
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
                                    fut.get_ref().as_raw_fd(),
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
            log::trace!("done with fut join");
            let rcv = {
                let io_state_lck = ctx.io_state.lock().unwrap();
                io_state_lck.receiver.clone()
            };
            let _ = rcv.recv().await;
            {
                let verify_result: gpgme_verify_result_t = unsafe {
                    call!(&ctx.inner.lib, gpgme_op_verify_result)(ctx.inner.ptr.as_ptr())
                };
                if verify_result.is_null() {
                    return Err(Error::new(
                        "Unspecified libgpgme error: gpgme_op_verify_result returned NULL.",
                    )
                    .set_kind(ErrorKind::External));
                }
            }
            let io_state_lck = ctx.io_state.lock().unwrap();
            let ret = io_state_lck.done.lock().unwrap().take().unwrap_or_else(|| {
                Err(Error::new("Unspecified libgpgme error").set_kind(ErrorKind::Bug))
            });
            ret
        })
    }

    pub fn keylist(
        &self,
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
                    self.inner.ptr.as_ptr(),
                    pattern
                        .as_ref()
                        .map(|cs| cs.as_ptr())
                        .unwrap_or(std::ptr::null_mut())
                        as *const ::std::os::raw::c_char,
                    secret.into(),
                ),
            )?;
        }

        let ctx = self.clone();
        let (done, fut) = self.io_state.done_fut()?;
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
                                    fut.get_ref().as_raw_fd(),
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
                                    fut.get_ref().as_raw_fd(),
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
                let io_state_lck = ctx.io_state.lock().unwrap();
                (
                    io_state_lck.receiver.clone(),
                    io_state_lck.key_receiver.clone(),
                )
            };
            let _ = rcv.recv().await;
            unsafe {
                gpgme_error_try(
                    &ctx.inner.lib,
                    call!(&ctx.inner.lib, gpgme_op_keylist_end)(ctx.inner.ptr.as_ptr()),
                )?;
            }
            ctx.io_state
                .lock()
                .unwrap()
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(Error::new("Unspecified libgpgme error")))?;
            let mut keys = vec![];
            while let Ok(inner) = key_receiver.try_recv() {
                let key = Key::new(inner, ctx.inner.lib.clone());
                keys.push(key);
            }
            drop(ctx);
            Ok(keys)
        })
    }

    pub fn sign(
        &mut self,
        sign_keys: Vec<Key>,
        mut text: Data,
    ) -> Result<impl Future<Output = Result<Vec<u8>>>> {
        if sign_keys.is_empty() {
            return Err(
                Error::new("gpgme: Call to sign() with zero keys.").set_kind(ErrorKind::Bug)
            );
        }
        let mut sig: gpgme_data_t = std::ptr::null_mut();
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_data_new)(&mut sig),
            )?;
            call!(&self.inner.lib, gpgme_signers_clear)(self.inner.ptr.as_ptr());
            for k in sign_keys {
                gpgme_error_try(
                    &self.inner.lib,
                    call!(&self.inner.lib, gpgme_signers_add)(
                        self.inner.ptr.as_ptr(),
                        k.inner.ptr.as_ptr(),
                    ),
                )?;
            }
        }

        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_sign_start)(
                    self.inner.ptr.as_ptr(),
                    text.inner.as_mut(),
                    sig,
                    gpgme_sig_mode_t::GPGME_SIG_MODE_DETACH,
                ),
            )?;
        }
        let mut sig = Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            bytes: Pin::new(vec![]),
            inner: NonNull::new(sig).ok_or_else(|| {
                Error::new("internal libgpgme error").set_kind(ErrorKind::LinkedLibrary("gpgme"))
            })?,
        };

        let ctx = self.clone();
        let (done, fut) = self.io_state.done_fut()?;
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
                                    fut.get_ref().as_raw_fd(),
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
                                    fut.get_ref().as_raw_fd(),
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
            {
                let rcv = ctx.io_state.lock().unwrap().receiver.clone();
                let _ = rcv.recv().await;
            }
            ctx.io_state
                .lock()
                .unwrap()
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(Error::new("Unspecified libgpgme error")))?;
            sig.seek(std::io::SeekFrom::Start(0))
                .chain_err_summary(|| {
                    "libgpgme error: could not perform seek on signature data object"
                })?;
            // disjoint-capture-in-closures
            let _ = &text;
            sig.into_bytes()
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
                    self.inner.ptr.as_ptr(),
                    cipher.inner.as_mut(),
                    plain,
                ),
            )?;
        }
        let mut plain = Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            bytes: Pin::new(vec![]),
            inner: NonNull::new(plain).ok_or_else(|| {
                Error::new("internal libgpgme error").set_kind(ErrorKind::LinkedLibrary("gpgme"))
            })?,
        };

        let ctx = self.clone();
        let (done, fut) = self.io_state.done_fut()?;
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
                                    fut.get_ref().as_raw_fd(),
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
                                    fut.get_ref().as_raw_fd(),
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
            let rcv = { ctx.io_state.lock().unwrap().receiver.clone() };
            let _ = rcv.recv().await;
            ctx.io_state
                .lock()
                .unwrap()
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(Error::new("Unspecified libgpgme error")))?;

            let decrypt_result =
                unsafe { call!(&ctx.inner.lib, gpgme_op_decrypt_result)(ctx.inner.ptr.as_ptr()) };
            if decrypt_result.is_null() {
                return Err(Error::new(
                    "Unspecified libgpgme error: gpgme_op_decrypt_result returned NULL.",
                )
                .set_kind(ErrorKind::LinkedLibrary("gpgme")));
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
                        status: gpgme_error_try(&ctx.inner.lib, (*recipient_iter).status),
                    });
                    recipient_iter = (*recipient_iter).next;
                }
            }
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

    pub fn encrypt(
        &mut self,
        encrypt_keys: Vec<Key>,
        mut plain: Data,
    ) -> Result<impl Future<Output = Result<Vec<u8>>> + Send> {
        if encrypt_keys.is_empty() {
            return Err(
                Error::new("gpgme: Call to encrypt() with zero keys.").set_kind(ErrorKind::Bug)
            );
        }
        unsafe {
            call!(&self.inner.lib, gpgme_signers_clear)(self.inner.ptr.as_ptr());
        }

        let mut cipher: gpgme_data_t = std::ptr::null_mut();
        let mut raw_keys: Vec<gpgme_key_t> = Vec::with_capacity(encrypt_keys.len() + 1);
        raw_keys.extend(encrypt_keys.iter().map(|k| k.inner.ptr.as_ptr()));
        raw_keys.push(std::ptr::null_mut());
        debug_assert_eq!(raw_keys.len(), encrypt_keys.len() + 1);
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_data_new)(&mut cipher),
            )?;
            if let Err(mut err) = gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_encrypt_start)(
                    self.inner.ptr.as_ptr(),
                    raw_keys.as_mut_slice().as_mut_ptr(),
                    gpgme_encrypt_flags_t::GPGME_ENCRYPT_NO_ENCRYPT_TO
                        | gpgme_encrypt_flags_t::GPGME_ENCRYPT_NO_COMPRESS
                        | gpgme_encrypt_flags_t::GPGME_ENCRYPT_ALWAYS_TRUST,
                    plain.inner.as_mut(),
                    cipher,
                ),
            ) {
                let result =
                    call!(&self.inner.lib, gpgme_op_encrypt_result)(self.inner.ptr.as_ptr());
                if let Some(ptr) = NonNull::new(result) {
                    let error = InvalidKeysIter::new(
                        ptr.as_ref().invalid_recipients,
                        self.inner.lib.clone(),
                    )
                    .map(|err| err.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                    if !error.is_empty() {
                        err = err.set_details(error);
                    }
                }

                return Err(err);
            };
        }
        let mut cipher = Data {
            lib: self.inner.lib.clone(),
            kind: DataKind::Memory,
            bytes: Pin::new(vec![]),
            inner: NonNull::new(cipher).ok_or_else(|| {
                Error::new("internal libgpgme error").set_kind(ErrorKind::LinkedLibrary("gpgme"))
            })?,
        };

        let ctx = self.clone();
        let (done, fut) = self.io_state.done_fut()?;
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
                                    fut.get_ref().as_raw_fd(),
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
                                    fut.get_ref().as_raw_fd(),
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
            let rcv = { ctx.io_state.lock().unwrap().receiver.clone() };
            let _ = rcv.recv().await;
            if let Err(mut err) = ctx
                .io_state
                .lock()
                .unwrap()
                .done
                .lock()
                .unwrap()
                .take()
                .unwrap_or_else(|| Err(Error::new("Unspecified libgpgme error")))
            {
                let result = unsafe {
                    call!(&ctx.inner.lib, gpgme_op_encrypt_result)(ctx.inner.ptr.as_ptr())
                };
                if let Some(ptr) = NonNull::new(result) {
                    let error = InvalidKeysIter::new(
                        unsafe { ptr.as_ref() }.invalid_recipients,
                        ctx.inner.lib.clone(),
                    )
                    .map(|err| err.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                    if !error.is_empty() {
                        err = err.set_details(error);
                    }
                }

                return Err(err.set_kind(ErrorKind::LinkedLibrary("gpgme")));
            }

            // Rewind cursor
            cipher
                .seek(std::io::SeekFrom::Start(0))
                .chain_err_summary(|| "libgpgme error: could not perform seek on plain text")?;
            // Keep plain alive long enough
            let _ = &plain;
            cipher.into_bytes()
        })
    }

    pub fn engine_info(&self) -> Result<Vec<EngineInfo>> {
        let mut ptr: gpgme_engine_info_t =
            unsafe { call!(&self.inner.lib, gpgme_ctx_get_engine_info)(self.inner.ptr.as_ptr()) };
        let mut retval = vec![];
        macro_rules! to_s {
            ($p:expr) => {{
                if $p.is_null() {
                    None
                } else {
                    unsafe { Some(CStr::from_ptr($p).to_string_lossy().to_string()) }
                }
            }};
        }
        while let Some(eng) = NonNull::new(ptr) {
            let eng_ref = unsafe { eng.as_ref() };
            ptr = eng_ref.next;
            retval.push(EngineInfo {
                protocol: eng_ref.protocol.into(),
                file_name: to_s! {eng_ref.file_name},
                version: to_s! {eng_ref.version},
                req_version: to_s! {eng_ref.req_version},
                home_dir: to_s! {eng_ref.home_dir},
            });
        }

        Ok(retval)
    }

    pub fn set_engine_info(
        &mut self,
        protocol: Protocol,
        file_name: Option<Cow<'static, CStr>>,
        home_dir: Option<Cow<'static, CStr>>,
    ) -> Result<()> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_ctx_set_engine_info)(
                    self.inner.ptr.as_ptr(),
                    protocol.into(),
                    file_name
                        .as_ref()
                        .map(|c| c.as_ptr())
                        .unwrap_or_else(std::ptr::null),
                    home_dir
                        .as_ref()
                        .map(|c| c.as_ptr())
                        .unwrap_or_else(std::ptr::null),
                ),
            )?;
        }
        Ok(())
    }

    pub fn set_protocol(&mut self, protocol: Protocol) -> Result<()> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_set_protocol)(
                    self.inner.ptr.as_ptr(),
                    protocol.into(),
                ),
            )?;
        }
        Ok(())
    }

    pub fn import_key(&mut self, mut key_data: Data) -> Result<()> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_import)(
                    self.inner.ptr.as_ptr(),
                    key_data.inner.as_mut(),
                ),
            )?;
        }
        let result =
            unsafe { call!(&self.inner.lib, gpgme_op_import_result)(self.inner.ptr.as_ptr()) };
        if let Some(ptr) = NonNull::new(result) {
            let res = unsafe { ptr.as_ref() };
            if res.imported == 0 && res.secret_imported == 0 {
                return Err(Error::new("Key was not imported."));
            }
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn set_passphrase_cb(
        &mut self,
        cb: gpgme_passphrase_cb_t,
        data: Option<*mut c_void>,
    ) -> Result<()> {
        unsafe {
            call!(&self.inner.lib, gpgme_get_pinentry_mode)(self.inner.ptr.as_ptr());
        }
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_set_pinentry_mode)(
                    self.inner.ptr.as_ptr(),
                    if cb.is_none() {
                        gpgme_pinentry_mode_t::GPGME_PINENTRY_MODE_DEFAULT
                    } else {
                        gpgme_pinentry_mode_t::GPGME_PINENTRY_MODE_LOOPBACK
                    },
                ),
            )?;
        }
        unsafe {
            call!(&self.inner.lib, gpgme_set_passphrase_cb)(
                self.inner.ptr.as_ptr(),
                cb,
                data.unwrap_or(std::ptr::null_mut()),
            );
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct EngineInfo {
    pub protocol: Protocol,
    pub file_name: Option<String>,
    pub version: Option<String>,
    pub req_version: Option<String>,
    pub home_dir: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum Protocol {
    OpenPGP = 0,
    CMS = 1,
    GPGCONF = 2,
    ASSUAN = 3,
    G13 = 4,
    UISERVER = 5,
    SPAWN = 6,
    DEFAULT = 254,
    UNKNOWN = 255,
}

impl From<u32> for Protocol {
    fn from(val: u32) -> Self {
        match val {
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_OpenPGP as u32 => Self::OpenPGP,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_CMS as u32 => Self::CMS,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_GPGCONF as u32 => Self::GPGCONF,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_ASSUAN as u32 => Self::ASSUAN,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_G13 as u32 => Self::G13,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_UISERVER as u32 => Self::UISERVER,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_SPAWN as u32 => Self::SPAWN,
            val if val == gpgme_protocol_t::GPGME_PROTOCOL_DEFAULT as u32 => Self::DEFAULT,
            _ => Self::UNKNOWN,
        }
    }
}

impl From<gpgme_protocol_t> for Protocol {
    fn from(val: gpgme_protocol_t) -> Self {
        match val {
            gpgme_protocol_t::GPGME_PROTOCOL_OpenPGP => Self::OpenPGP,
            gpgme_protocol_t::GPGME_PROTOCOL_CMS => Self::CMS,
            gpgme_protocol_t::GPGME_PROTOCOL_GPGCONF => Self::GPGCONF,
            gpgme_protocol_t::GPGME_PROTOCOL_ASSUAN => Self::ASSUAN,
            gpgme_protocol_t::GPGME_PROTOCOL_G13 => Self::G13,
            gpgme_protocol_t::GPGME_PROTOCOL_UISERVER => Self::UISERVER,
            gpgme_protocol_t::GPGME_PROTOCOL_SPAWN => Self::SPAWN,
            gpgme_protocol_t::GPGME_PROTOCOL_DEFAULT => Self::DEFAULT,
            gpgme_protocol_t::GPGME_PROTOCOL_UNKNOWN => Self::UNKNOWN,
        }
    }
}

impl From<Protocol> for gpgme_protocol_t {
    fn from(val: Protocol) -> Self {
        match val {
            Protocol::OpenPGP => Self::GPGME_PROTOCOL_OpenPGP,
            Protocol::CMS => Self::GPGME_PROTOCOL_CMS,
            Protocol::GPGCONF => Self::GPGME_PROTOCOL_GPGCONF,
            Protocol::ASSUAN => Self::GPGME_PROTOCOL_ASSUAN,
            Protocol::G13 => Self::GPGME_PROTOCOL_G13,
            Protocol::UISERVER => Self::GPGME_PROTOCOL_UISERVER,
            Protocol::SPAWN => Self::GPGME_PROTOCOL_SPAWN,
            Protocol::DEFAULT => Self::GPGME_PROTOCOL_DEFAULT,
            Protocol::UNKNOWN => Self::GPGME_PROTOCOL_UNKNOWN,
        }
    }
}

fn gpgme_error_to_string(lib: &libloading::Library, error_code: gpgme_error_t) -> String {
    const ERR_MAX_LEN: bindings::size_t = 256;
    let mut buf: Vec<u8> = vec![0; ERR_MAX_LEN as usize];
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
    String::from_utf8(buf)
        .unwrap_or_else(|err| String::from_utf8_lossy(&err.into_bytes()).to_string())
}

fn gpgme_error_try(lib: &libloading::Library, error_code: gpgme_error_t) -> Result<()> {
    if error_code == 0 {
        return Ok(());
    }
    Err(Error::from(gpgme_error_to_string(lib, error_code))
        .set_summary(format!("libgpgme error {error_code}")))
}

#[derive(Debug)]
enum DataKind {
    Memory,
}

#[derive(Debug)]
pub struct Data {
    inner: NonNull<bindings::gpgme_data>,
    kind: DataKind,
    #[allow(dead_code)]
    bytes: std::pin::Pin<Vec<u8>>,
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
        match self.kind {
            DataKind::Memory => unsafe { call!(self.lib, gpgme_data_release)(self.inner.as_mut()) },
        }
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
    sender: Sender<()>,
    receiver: Receiver<()>,
    io_state: Arc<Mutex<IoState>>,
}

impl std::fmt::Debug for GpgmeFd {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(identify!(GpgmeFd))
            .field("fd", &self.fd)
            .field("fnc", &self.fnc)
            .field("fnc_data", &self.fnc_data)
            .field("idx", &self.idx)
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
//    let plain =
// futures::executor::block_on(ctx.decrypt(cipher).unwrap()).unwrap();
//    println!(
//       "buf: {}",
//       String::from_utf8_lossy(&plain.into_bytes().unwrap())
//    );
//}

mod wrapper {
    use super::*;

    /// Wrapper type to free IO state and `add_priv`, `event_priv` leaked
    /// references.
    #[repr(transparent)]
    pub(super) struct IoStateWrapper(ManuallyDrop<Arc<Mutex<IoState>>>);

    impl IoStateWrapper {
        pub(super) fn new(state: IoState) -> (Arc<Self>, gpgme_io_cbs) {
            let inner = Arc::new(Mutex::new(state));
            let add_priv = Arc::into_raw(Arc::clone(&inner))
                .cast_mut()
                .cast::<c_void>();
            let event_priv = Arc::into_raw(Arc::clone(&inner))
                .cast_mut()
                .cast::<c_void>();

            let io_cbs = gpgme_io_cbs {
                add: Some(io::gpgme_register_io_cb),
                add_priv,
                remove: Some(io::gpgme_remove_io_cb),
                event: Some(io::gpgme_event_io_cb),
                event_priv,
            };

            (Arc::new(Self(ManuallyDrop::new(inner))), io_cbs)
        }

        pub(super) fn done_fut(&self) -> Result<(Done, Vec<Async<GpgmeFd>>)> {
            let (done, fut) = if let Ok(io_state_lck) = self.0.lock() {
                let done = io_state_lck.done.clone();
                (
                    done,
                    io_state_lck
                        .ops
                        .values()
                        .map(|a| Async::new(a.clone()))
                        .collect::<std::io::Result<Vec<Async<GpgmeFd>>>>()?,
                )
            } else {
                return Err(Error::new("Could not use gpgme library")
                    .set_details("The context's IO state mutex was poisoned.")
                    .set_kind(ErrorKind::Bug));
            };
            Ok((done, fut))
        }
    }

    impl Drop for IoStateWrapper {
        fn drop(&mut self) {
            // SAFETY: struct unit value is ManuallyDrop, so no Drop impls are called on the
            // uninit value.
            // let inner = unsafe { ManuallyDrop::take(&mut self.0) };
            // SAFETY: take add_priv reference
            unsafe { Arc::decrement_strong_count(&self.0) };
            // SAFETY: take event_priv reference
            unsafe { Arc::decrement_strong_count(&self.0) };
        }
    }

    impl std::ops::Deref for IoStateWrapper {
        type Target = Arc<Mutex<IoState>>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}

use wrapper::IoStateWrapper;
