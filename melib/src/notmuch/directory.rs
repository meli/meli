//
// meli
//
// Copyright 2025 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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
    ffi::{CStr, CString},
    ptr::NonNull,
    sync::{Arc, Mutex},
};

use crate::notmuch::{ffi, DbPointer, NotmuchError, NotmuchLibrary};

pub struct NotmuchDirectory {
    pub lib: Arc<NotmuchLibrary>,
    pub path: CString,
    pub db: Arc<Mutex<DbPointer>>,
    pub inner: NonNull<ffi::notmuch_directory_t>,
}

impl std::fmt::Debug for NotmuchDirectory {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("NotmuchDirectory")
            .field("path", &self.path)
            .finish_non_exhaustive()
    }
}

impl Drop for NotmuchDirectory {
    fn drop(&mut self) {
        // SAFETY: `self.inner` is a valid `notmuch_directory_t` pointer.
        unsafe { call!(self.lib, ffi::notmuch_directory_destroy)(self.inner.as_mut()) };
    }
}

impl NotmuchDirectory {
    #[inline]
    /// Returns a [`NotmuchFilenames`] iterator that returns *only* the
    /// filenames/basenames of child files.
    ///
    /// For the filename version see
    /// [`NotmuchDirectory::child_files_filenames`].
    pub fn child_files_paths(&mut self) -> NotmuchFilenames {
        NotmuchFilenames {
            path: Some(self.path.clone()),
            inner: NonNull::new(unsafe {
                call!(self.lib, ffi::notmuch_directory_get_child_files)(self.inner.as_mut())
            })
            .unwrap(),
            lib: self.lib.clone(),
            db: self.db.clone(),
        }
    }

    #[inline]
    /// Returns a [`NotmuchFilenames`] iterator that returns *only* the
    /// filenames/basenames of child files.
    ///
    /// For the absolute path version see
    /// [`NotmuchDirectory::child_files_paths`].
    pub fn child_files_filenames(&mut self) -> NotmuchFilenames {
        NotmuchFilenames {
            path: None,
            inner: NonNull::new(unsafe {
                call!(self.lib, ffi::notmuch_directory_get_child_files)(self.inner.as_mut())
            })
            .unwrap(),
            lib: self.lib.clone(),
            db: self.db.clone(),
        }
    }

    #[inline]
    /// Returns a [`NotmuchFilenames`] iterator that returns *only* the
    /// filenames/basenames of child directories.
    ///
    /// For the filename version see
    /// [`NotmuchDirectory::child_directories_filenames`].
    pub fn child_directories_paths(&mut self) -> NotmuchFilenames {
        NotmuchFilenames {
            path: Some(self.path.clone()),
            inner: NonNull::new(unsafe {
                call!(self.lib, ffi::notmuch_directory_get_child_directories)(self.inner.as_mut())
            })
            .unwrap(),
            lib: self.lib.clone(),
            db: self.db.clone(),
        }
    }

    #[inline]
    /// Returns a [`NotmuchFilenames`] iterator that returns *only* the
    /// filenames/basenames of child directories.
    ///
    /// For the absolute path version see
    /// [`NotmuchDirectory::child_directories_paths`].
    pub fn child_directories_filenames(&mut self) -> NotmuchFilenames {
        NotmuchFilenames {
            path: None,
            inner: NonNull::new(unsafe {
                call!(self.lib, ffi::notmuch_directory_get_child_directories)(self.inner.as_mut())
            })
            .unwrap(),
            lib: self.lib.clone(),
            db: self.db.clone(),
        }
    }

    pub fn child_directories(&mut self) -> NotmuchDirectories {
        NotmuchDirectories {
            filenames: self.child_directories_filenames(),
            path: self.path.clone(),
            lib: self.lib.clone(),
            db: self.db.clone(),
        }
    }

    pub fn mtime(&mut self) -> libc::time_t {
        // SAFETY: `self.inner` is a valid `notmuch_directory_t` pointer.
        unsafe { call!(self.lib, ffi::notmuch_directory_get_mtime)(self.inner.as_mut()) }
    }

    pub fn set_mtime(&mut self, value: libc::time_t) -> Result<(), NotmuchError> {
        // SAFETY: `self.inner` is a valid `notmuch_directory_t` pointer.
        unsafe {
            try_call!(
                self.lib,
                call!(self.lib, ffi::notmuch_directory_set_mtime)(self.inner.as_mut(), value)
            )
        }
    }
}

pub struct NotmuchFilenames {
    pub path: Option<CString>,
    pub lib: Arc<NotmuchLibrary>,
    pub db: Arc<Mutex<DbPointer>>,
    pub inner: NonNull<ffi::notmuch_filenames_t>,
}

impl std::fmt::Debug for NotmuchFilenames {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("NotmuchFilenames")
            .field("path", &self.path)
            .finish_non_exhaustive()
    }
}

impl Drop for NotmuchFilenames {
    fn drop(&mut self) {
        // SAFETY: `self.inner` is a valid `notmuch_filenames_t` pointer.
        unsafe { call!(self.lib, ffi::notmuch_filenames_destroy)(self.inner.as_mut()) };
    }
}

impl Iterator for NotmuchFilenames {
    type Item = CString;

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: `self.inner` is a valid `notmuch_filenames_t` pointer.
        if unsafe { call!(self.lib, ffi::notmuch_filenames_valid)(self.inner.as_mut()) } == 0 {
            return None;
        }
        // SAFETY: `self.inner` is a valid `notmuch_filenames_t` pointer.
        let next_ptr = unsafe { call!(self.lib, ffi::notmuch_filenames_get)(self.inner.as_mut()) };
        if next_ptr.is_null() {
            return None;
        }
        // SAFETY: `next_ptr` is guaranteed by notmuch library to be a valid C string
        // pointer.
        let next_basename = unsafe { CStr::from_ptr(next_ptr) };
        let ret = if let Some(path) = self.path.as_ref() {
            let mut path = path.clone().into_bytes();
            path.extend_from_slice(std::path::MAIN_SEPARATOR_STR.as_bytes());
            path.extend_from_slice(next_basename.to_bytes());
            // SAFETY: `path` is built from CStr/CString bytes, which are guaranteed not to
            // include any NUL bytes.
            unsafe { CString::from_vec_unchecked(path) }
        } else {
            next_basename.into()
        };
        // SAFETY: `self.inner` is a valid `notmuch_filenames_t` pointer.
        unsafe { call!(self.lib, ffi::notmuch_filenames_move_to_next)(self.inner.as_mut()) };
        Some(ret)
    }
}

pub struct NotmuchDirectories {
    pub filenames: NotmuchFilenames,
    pub lib: Arc<NotmuchLibrary>,
    pub path: CString,
    pub db: Arc<Mutex<DbPointer>>,
}

impl std::fmt::Debug for NotmuchDirectories {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("NotmuchDirectories")
            .field("path", &self.path)
            .finish_non_exhaustive()
    }
}

impl Iterator for NotmuchDirectories {
    type Item = NotmuchDirectory;

    fn next(&mut self) -> Option<Self::Item> {
        let next_basename = self.filenames.next()?;
        let mut path = self.path.clone().into_bytes();
        path.extend_from_slice(std::path::MAIN_SEPARATOR_STR.as_bytes());
        path.extend_from_slice(next_basename.as_bytes());
        // SAFETY: `path` is built from CStr/CString bytes, which are guaranteed not to
        // include any NUL bytes.
        let path: CString = unsafe { CString::from_vec_unchecked(path) };
        let mut directory_ptr = std::ptr::null_mut();
        // SAFETY: `self.db`, `path`, `directory_ptr` are all valid.
        if unsafe {
            call!(self.lib, ffi::notmuch_database_get_directory)(
                self.db.lock().unwrap().as_mut(),
                path.as_ptr(),
                &mut directory_ptr,
            )
        } != ffi::NOTMUCH_STATUS_SUCCESS
        {
            return self.next();
        }

        Some(NotmuchDirectory {
            path,
            lib: self.lib.clone(),
            db: self.db.clone(),
            inner: NonNull::new(directory_ptr)?,
        })
    }
}
