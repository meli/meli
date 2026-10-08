//
// meli
//
// Copyright 2026 - Manos Pitsidianakis
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

//! Notmuch FFI library API
//!
//! See [`NotmuchLibrary`] documentation.

use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    sync::OnceLock,
};

use libloading::{os::unix::Symbol as RawSymbol, Library, Symbol};

use crate::notmuch::{ffi::notmuch_config_key_t, DbPointer};

macro_rules! declare_api {
    ($($accessor:ident: $symbol:ty),*$(,)?) => {
        /// Store notmuch dynamic library and symbols
        #[derive(Debug)]
        pub struct NotmuchLibrary {
            /// The `libloading` library object
            pub inner: Library,
            /// The path we loaded it from
            pub dlpath: Cow<'static, str>,
            $($accessor: OnceLock<RawSymbol<$symbol>>),*
        }

        impl NotmuchLibrary {
            /// Create new library
            pub fn new(inner: Library, dlpath: Cow<'static, str>) -> Self {
                Self {
                    inner,
                    dlpath,
                    $($accessor: OnceLock::new()),*
                }
            }

            $(
                #[inline(always)]
                pub fn $accessor(&'_ self) ->  Symbol<'_, $symbol> {
                    const S: &str = stringify!($symbol);
                    let raw_symbol = self.$accessor.get_or_init(||{
                        let symbol: Symbol<'_, $symbol> =
                            unsafe { self.inner
                                .get(S.split("::").last().unwrap_or(S).trim().as_bytes()) }
                        .unwrap();

                        unsafe { symbol.into_raw() }
                    });
                    unsafe { Symbol::from_raw(raw_symbol.clone(), &self.inner) }
                }
            )*
        }
    };
}

declare_api! {
    config_get: crate::notmuch::ffi::notmuch_config_get,
    database_close: crate::notmuch::ffi::notmuch_database_close,
    database_reopen: crate::notmuch::ffi::notmuch_database_reopen,
    database_destroy: crate::notmuch::ffi::notmuch_database_destroy,
    database_find_message: crate::notmuch::ffi::notmuch_database_find_message,
    database_find_message_by_filename: crate::notmuch::ffi::notmuch_database_find_message_by_filename,
    database_get_directory: crate::notmuch::ffi::notmuch_database_get_directory,
    database_open: crate::notmuch::ffi::notmuch_database_open,
    database_index_file: crate::notmuch::ffi::notmuch_database_index_file,
    database_get_revision: crate::notmuch::ffi::notmuch_database_get_revision,
    directory_destroy: crate::notmuch::ffi::notmuch_directory_destroy,
    directory_get_child_directories: crate::notmuch::ffi::notmuch_directory_get_child_directories,
    directory_get_child_files: crate::notmuch::ffi::notmuch_directory_get_child_files,
    directory_get_mtime: crate::notmuch::ffi::notmuch_directory_get_mtime,
    directory_set_mtime: crate::notmuch::ffi::notmuch_directory_set_mtime,
    filenames_destroy: crate::notmuch::ffi::notmuch_filenames_destroy,
    filenames_get: crate::notmuch::ffi::notmuch_filenames_get,
    filenames_move_to_next: crate::notmuch::ffi::notmuch_filenames_move_to_next,
    filenames_valid: crate::notmuch::ffi::notmuch_filenames_valid,
    message_add_tag: crate::notmuch::ffi::notmuch_message_add_tag,
    message_destroy: crate::notmuch::ffi::notmuch_message_destroy,
    message_freeze: crate::notmuch::ffi::notmuch_message_freeze,
    message_get_date: crate::notmuch::ffi::notmuch_message_get_date,
    message_get_header: crate::notmuch::ffi::notmuch_message_get_header,
    message_remove_tag: crate::notmuch::ffi::notmuch_message_remove_tag,
    message_get_filename: crate::notmuch::ffi::notmuch_message_get_filename,
    message_get_message_id: crate::notmuch::ffi::notmuch_message_get_message_id,
    message_get_replies: crate::notmuch::ffi::notmuch_message_get_replies,
    message_get_tags: crate::notmuch::ffi::notmuch_message_get_tags,
    message_tags_to_maildir_flags: crate::notmuch::ffi::notmuch_message_tags_to_maildir_flags,
    message_thaw: crate::notmuch::ffi::notmuch_message_thaw,
    messages_move_to_next: crate::notmuch::ffi::notmuch_messages_move_to_next,
    messages_valid: crate::notmuch::ffi::notmuch_messages_valid,
    messages_get: crate::notmuch::ffi::notmuch_messages_get,
    query_count_messages: crate::notmuch::ffi::notmuch_query_count_messages,
    query_create: crate::notmuch::ffi::notmuch_query_create,
    query_destroy: crate::notmuch::ffi::notmuch_query_destroy,
    query_search_messages: crate::notmuch::ffi::notmuch_query_search_messages,
    status_to_string: crate::notmuch::ffi::notmuch_status_to_string,
    tags_destroy: crate::notmuch::ffi::notmuch_tags_destroy,
    tags_get: crate::notmuch::ffi::notmuch_tags_get,
    tags_move_to_next: crate::notmuch::ffi::notmuch_tags_move_to_next,
    tags_valid: crate::notmuch::ffi::notmuch_tags_valid,
    thread_destroy: crate::notmuch::ffi::notmuch_thread_destroy,
    thread_get_messages: crate::notmuch::ffi::notmuch_thread_get_messages,
    thread_get_newest_date: crate::notmuch::ffi::notmuch_thread_get_newest_date,
    thread_get_thread_id: crate::notmuch::ffi::notmuch_thread_get_thread_id,
    thread_get_total_messages: crate::notmuch::ffi::notmuch_thread_get_total_messages,
    threads_valid: crate::notmuch::ffi::notmuch_threads_valid,
    threads_get: crate::notmuch::ffi::notmuch_threads_get,
    threads_move_to_next: crate::notmuch::ffi::notmuch_threads_move_to_next,
}

impl NotmuchLibrary {
    pub fn notmuch_config_get<'a>(
        &self,
        db: &'a mut DbPointer,
        key: notmuch_config_key_t,
    ) -> Option<CString> {
        // SAFETY: fn symbol, db are valid pointers.
        let ptr = unsafe { (self.config_get())(db.as_mut(), key) };
        if ptr.is_null() {
            None
        } else {
            let cstr: &'a CStr = // SAFETY: notmuch lib promises us this is a valid string with a nul terminator and has the same
                // lifetime as the db.
                unsafe { CStr::from_ptr(ptr) };
            let cstring = cstr.into();
            Some(cstring)
        }
    }
}
