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

use std::{borrow::Cow, ffi::CStr, ptr::NonNull, sync::Arc};

use crate::{
    email::Address,
    gpgme::{bindings::*, gpgme_error_to_string},
};

#[derive(Clone)]
pub struct KeyInner {
    pub ptr: NonNull<_gpgme_key>,
}

unsafe impl Send for KeyInner {}
unsafe impl Sync for KeyInner {}

pub struct Key {
    pub inner: KeyInner,
    pub lib: Arc<libloading::Library>,
}
unsafe impl Send for Key {}
unsafe impl Sync for Key {}

impl Clone for Key {
    fn clone(&self) -> Self {
        let lib = self.lib.clone();
        unsafe {
            call!(&self.lib, gpgme_key_ref)(self.inner.ptr.as_ptr());
        }
        Self {
            inner: self.inner.clone(),
            lib,
        }
    }
}

impl Key {
    #[inline]
    pub fn new(inner: KeyInner, lib: Arc<libloading::Library>) -> Self {
        Self { inner, lib }
    }

    pub fn primary_uid(&self) -> Option<Address> {
        unsafe {
            if (self.inner.ptr.as_ref()).uids.is_null() {
                return None;
            }
            let uid = self.inner.ptr.as_ref().uids;
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
        unsafe { self.inner.ptr.as_ref().revoked() > 0 }
    }

    pub fn expired(&self) -> bool {
        unsafe { self.inner.ptr.as_ref().expired() > 0 }
    }

    pub fn disabled(&self) -> bool {
        unsafe { self.inner.ptr.as_ref().disabled() > 0 }
    }

    pub fn invalid(&self) -> bool {
        unsafe { self.inner.ptr.as_ref().invalid() > 0 }
    }

    pub fn can_encrypt(&self) -> bool {
        unsafe { self.inner.ptr.as_ref().can_encrypt() > 0 }
    }

    pub fn can_sign(&self) -> bool {
        unsafe { self.inner.ptr.as_ref().can_sign() > 0 }
    }

    pub fn secret(&self) -> bool {
        unsafe { self.inner.ptr.as_ref().secret() > 0 }
    }

    pub fn fingerprint(&self) -> Cow<'_, str> {
        // SAFETY: self.inner.ptr is valid.
        let fpr = unsafe { self.inner.ptr.as_ref().fpr };
        let Some(fpr_pr) = NonNull::new(fpr) else {
            return Cow::Borrowed("");
        };
        unsafe { CStr::from_ptr(fpr_pr.as_ptr()) }.to_string_lossy()
    }
}

impl std::fmt::Debug for Key {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(crate::identify!(Key))
            .field("fingerprint", &self.fingerprint())
            .field("uid", &self.primary_uid())
            .field("can_encrypt", &self.can_encrypt())
            .field("can_sign", &self.can_sign())
            .field("secret", &self.secret())
            .field("revoked", &self.revoked())
            .field("expired", &self.expired())
            .field("invalid", &self.invalid())
            .finish_non_exhaustive()
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.fingerprint() == other.fingerprint()
    }
}

impl Eq for Key {}

impl Drop for Key {
    fn drop(&mut self) {
        unsafe {
            call!(&self.lib, gpgme_key_unref)(self.inner.ptr.as_ptr());
        }
    }
}

pub struct InvalidKeyError {
    pub fingerprint: String,
    pub reason: String,
    pub gpgme_error: gpgme_error_t,
}

pub(super) struct InvalidKeysIter<'a> {
    lib: Arc<libloading::Library>,
    ptr: gpgme_invalid_key_t,
    _ph: std::marker::PhantomData<&'a _gpgme_invalid_key>,
}

impl InvalidKeysIter<'_> {
    pub(super) fn new(ptr: gpgme_invalid_key_t, lib: Arc<libloading::Library>) -> Self {
        Self {
            lib,
            ptr,
            _ph: std::marker::PhantomData,
        }
    }
}

impl Iterator for InvalidKeysIter<'_> {
    type Item = InvalidKeyError;

    fn next(&mut self) -> Option<Self::Item> {
        let invalid_key = NonNull::new(self.ptr)?;
        let invalid_key_ref = unsafe { invalid_key.as_ref() };
        self.ptr = invalid_key_ref.next;
        Some(InvalidKeyError {
            fingerprint: unsafe { CStr::from_ptr(invalid_key_ref.fpr) }
                .to_string_lossy()
                .to_string(),
            reason: gpgme_error_to_string(&self.lib, invalid_key_ref.reason),
            gpgme_error: invalid_key_ref.reason,
        })
    }
}

impl std::fmt::Display for InvalidKeyError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(crate::identify!(InvalidKeyError))
            .field("Fingerprint", &self.fingerprint)
            .field("Reason", &self.reason)
            .field("Gpgme error code", &self.gpgme_error)
            .finish()
    }
}
