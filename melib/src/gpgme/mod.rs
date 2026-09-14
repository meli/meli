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
    ffi::{CStr, CString},
    future::Future,
    io::Seek,
    ptr::NonNull,
    sync::Arc,
};

use futures::FutureExt;
use smol::{
    channel::{Receiver, Sender},
    Async,
};

use crate::{
    email::pgp::{DecryptionMetadata, LocateKey, Recipient, SignaturesMetadata},
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
    // [ref:msrv] unnecessary_transmutes is introduced in 1.88.0
    unknown_lints,
    unnecessary_transmutes,
    unpredictable_function_pointer_comparisons,
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
pub mod sign;

use io::{Data, IoState};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GpgmeFlag {
    /// "auto-key-retrieve"
    AutoKeyRetrieve,
    OfflineMode,
    AsciiArmor,
}

impl GpgmeFlag {
    const AUTO_KEY_RETRIEVE: &'static CStr = c"auto-key-retrieve";
    const AUTO_KEY_LOCATE: &'static CStr = c"auto-key-locate";
}

pub struct ContextInner {
    ptr: NonNull<gpgme_context>,
    lib: Arc<libloading::Library>,
}

unsafe impl Send for ContextInner {}
unsafe impl Sync for ContextInner {}

#[derive(Clone)]
pub struct Context {
    inner: Arc<ContextInner>,
    io_state: Arc<IoState>,
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

        let (io_state, mut io_cbs) = IoState::new(lib.clone());

        let mut ptr = core::mem::MaybeUninit::zeroed();
        // SAFETY: `&raw mut ptr` points to valid, stack allocated memory
        unsafe {
            gpgme_error_try(&lib, call!(&lib, gpgme_new)(ptr.as_mut_ptr()))?;
        }
        // SAFETY: `gpgme_new()` succeeded so this pointer is initialized.
        let ptr = unsafe { ptr.assume_init() };
        let mut ret = Self {
            inner: Arc::new(ContextInner {
                ptr: NonNull::new(ptr).ok_or_else(|| {
                    Error::new("Could not use libgpgme")
                        .set_details("gpgme_new returned a NULL value.")
                        .set_kind(ErrorKind::LinkedLibrary("gpgme"))
                })?,
                lib,
            }),
            io_state,
        };
        // SAFETY: `ptr` and `io_cbs` are both valid.
        unsafe { call!(&ret.inner.lib, gpgme_set_io_cbs)(ret.inner.ptr.as_ptr(), &raw mut io_cbs) };
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
        const VALUE_ON: &CStr = c"1";
        const VALUE_OFF: &CStr = c"0";
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
            self.set_flag_inner(GpgmeFlag::AUTO_KEY_LOCATE, c"clear,nodefault")?;
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
        Data::new_mem(self.inner.lib.clone(), bytes)
    }

    pub fn verify(
        &mut self,
        mut signature: Data,
        mut text: Data,
    ) -> Result<impl Future<Output = Result<SignaturesMetadata>> + Send> {
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_verify_start)(
                    self.inner.ptr.as_ptr(),
                    signature.as_ptr(),
                    text.as_ptr(),
                    std::ptr::null_mut(),
                ),
            )?;
        }

        let ctx = self.clone();
        Ok(async move {
            let _s = signature;
            let _t = text;
            ctx.io_state.wait_for_op().await?;
            let ret = {
                let Some(verify_result) = sign::VerifyResult::retrieve(&ctx.inner.lib, &ctx) else {
                    return Err(Error::new(
                        "Unspecified libgpgme error: gpgme_op_verify_result returned NULL.",
                    )
                    .set_kind(ErrorKind::External));
                };
                let signatures = verify_result.signatures(false).collect::<Vec<_>>();
                if signatures.is_empty() {
                    return Err(Error::new("No signatures found.").set_kind(ErrorKind::NotFound));
                }
                Ok(SignaturesMetadata { signatures })
            };
            ret
        })
    }

    pub fn verify_cleartext(
        &mut self,
        mut text: Data,
    ) -> Result<impl Future<Output = Result<(SignaturesMetadata, Vec<u8>)>> + Send> {
        let mut plain_text = Data::new(self.inner.lib.clone())?;
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_verify_start)(
                    self.inner.ptr.as_ptr(),
                    text.as_ptr(),
                    std::ptr::null_mut(),
                    plain_text.as_ptr(),
                ),
            )?;
        }

        let ctx = self.clone();
        Ok(async move {
            let _s = text;
            ctx.io_state.wait_for_op().await?;
            let ret = {
                let Some(verify_result) = sign::VerifyResult::retrieve(&ctx.inner.lib, &ctx) else {
                    return Err(Error::new(
                        "Unspecified libgpgme error: gpgme_op_verify_result returned NULL.",
                    )
                    .set_kind(ErrorKind::External));
                };
                let signatures = verify_result.signatures(true).collect::<Vec<_>>();
                if signatures.is_empty() {
                    return Err(Error::new("No signatures found.").set_kind(ErrorKind::NotFound));
                }
                plain_text
                    .seek(std::io::SeekFrom::Start(0))
                    .chain_err_summary(|| {
                        "libgpgme error: could not perform seek on signature data object"
                    })?;
                let plain_text = plain_text.into_bytes().chain_err_summary(|| {
                    "libgpgme error: could not read plain text after successfull signature \
                     verification"
                })?;
                Ok((SignaturesMetadata { signatures }, plain_text))
            };
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
        Ok(async move {
            let res = ctx.io_state.wait_for_op().await;
            let key_receiver = ctx.io_state.key_receiver();
            unsafe {
                gpgme_error_try(
                    &ctx.inner.lib,
                    call!(&ctx.inner.lib, gpgme_op_keylist_end)(ctx.inner.ptr.as_ptr()),
                )?;
            }
            res?;
            let mut keys = vec![];
            while let Ok(inner) = key_receiver.try_recv() {
                let key = Key::new(inner, ctx.inner.lib.clone());
                keys.push(key);
            }
            Ok(keys)
        })
    }

    pub fn sign(
        &mut self,
        sign_keys: Vec<Key>,
        mut text: Data,
        is_binary: bool,
    ) -> Result<impl Future<Output = Result<(sign::NewSignature, Vec<u8>)>>> {
        if sign_keys.is_empty() {
            return Err(
                Error::new("gpgme: Call to sign() with zero keys.").set_kind(ErrorKind::Bug)
            );
        }
        let canonical_text_mode = !is_binary;
        unsafe {
            call!(&self.inner.lib, gpgme_set_textmode)(
                self.inner.ptr.as_ptr(),
                canonical_text_mode.into(),
            );
        };
        unsafe {
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

        let mut sig = Data::new(self.inner.lib.clone())?;
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_sign_start)(
                    self.inner.ptr.as_ptr(),
                    text.as_ptr(),
                    sig.as_ptr(),
                    gpgme_sig_mode_t::GPGME_SIG_MODE_DETACH,
                ),
            )?;
        }

        let ctx = self.clone();
        Ok(async move {
            ctx.io_state.wait_for_op().await?;
            let sign_result = sign::SignResult::retrieve(&ctx).unwrap();
            let mut signatures = sign_result.signatures().collect::<Vec<_>>();
            // [ref:FIXME]: can there be more than one new signature?
            let Some(new_sig) = signatures.pop() else {
                return Err(
                    Error::new("libgpgme returned no signatures after signing op")
                        .set_kind(ErrorKind::External),
                );
            };
            sig.seek(std::io::SeekFrom::Start(0))
                .chain_err_summary(|| {
                    "libgpgme error: could not perform seek on signature data object"
                })?;
            // disjoint-capture-in-closures
            let _ = &text;
            Ok((new_sig, sig.into_bytes()?))
        })
    }

    pub fn decrypt(
        &mut self,
        mut cipher: Data,
    ) -> Result<impl Future<Output = Result<(DecryptionMetadata, Vec<u8>)>> + Send> {
        let mut plain: Data = Data::new(self.inner.lib.clone())?;
        unsafe {
            gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_decrypt_start)(
                    self.inner.ptr.as_ptr(),
                    cipher.as_ptr(),
                    plain.as_ptr(),
                ),
            )?;
        }

        let ctx = self.clone();
        Ok(async move {
            let _c = cipher;
            ctx.io_state.wait_for_op().await?;
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
                    if !(*recipient_iter).keyid.is_null() {
                        recipients.push(Recipient {
                            keyid: CStr::from_ptr((*recipient_iter).keyid)
                                .to_string_lossy()
                                .to_string(),
                            status: gpgme_error_try(&ctx.inner.lib, (*recipient_iter).status),
                        });
                    }
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

        let mut cipher: Data = Data::new(self.inner.lib.clone())?;
        let mut raw_keys: Vec<gpgme_key_t> = Vec::with_capacity(encrypt_keys.len() + 1);
        raw_keys.extend(encrypt_keys.iter().map(|k| k.inner.ptr.as_ptr()));
        raw_keys.push(std::ptr::null_mut());
        debug_assert_eq!(raw_keys.len(), encrypt_keys.len() + 1);
        unsafe {
            if let Err(mut err) = gpgme_error_try(
                &self.inner.lib,
                call!(&self.inner.lib, gpgme_op_encrypt_start)(
                    self.inner.ptr.as_ptr(),
                    raw_keys.as_mut_slice().as_mut_ptr(),
                    gpgme_encrypt_flags_t::GPGME_ENCRYPT_NO_ENCRYPT_TO
                        | gpgme_encrypt_flags_t::GPGME_ENCRYPT_NO_COMPRESS
                        | gpgme_encrypt_flags_t::GPGME_ENCRYPT_ALWAYS_TRUST,
                    plain.as_ptr(),
                    cipher.as_ptr(),
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

        let ctx = self.clone();
        Ok(async move {
            let res = ctx.io_state.wait_for_op().await;
            if let Err(mut err) = res {
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
                call!(&self.inner.lib, gpgme_op_import)(self.inner.ptr.as_ptr(), key_data.as_ptr()),
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
