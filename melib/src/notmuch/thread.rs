/*
 * melib - notmuch backend
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
use super::*;
use crate::{
    notmuch::ffi::{
        notmuch_thread_destroy, notmuch_thread_get_messages, notmuch_thread_get_newest_date,
        notmuch_thread_get_thread_id, notmuch_thread_get_total_messages, notmuch_thread_t,
        notmuch_threads_get, notmuch_threads_move_to_next, notmuch_threads_t,
        notmuch_threads_valid,
    },
    thread::ThreadHash,
};

pub struct Thread<'query> {
    pub lib: Arc<NotmuchLibrary>,
    pub inner: NonNull<notmuch_thread_t>,
    pub _ph: std::marker::PhantomData<*const Query<'query>>,
}

impl<'q> Thread<'q> {
    #[inline]
    pub fn id(&self) -> ThreadHash {
        let thread_id = unsafe {
            // SAFETY:
            // All pointers used here are NonNull<_> wrapped.
            call!(self.lib, notmuch_thread_get_thread_id)(self.inner.as_ptr())
        };
        let c_str = unsafe { CStr::from_ptr(thread_id) };
        ThreadHash::from(c_str.to_bytes())
    }

    #[inline]
    pub fn date(&self) -> crate::UnixTimestamp {
        (unsafe {
            // SAFETY:
            // All pointers used here are NonNull<_> wrapped.
            call!(self.lib, notmuch_thread_get_newest_date)(self.inner.as_ptr())
        }) as u64
    }

    #[inline]
    pub fn len(&self) -> usize {
        (unsafe {
            // SAFETY:
            // All pointers used here are NonNull<_> wrapped.
            call!(self.lib, notmuch_thread_get_total_messages)(self.inner.as_ptr())
        }) as usize
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&'q self) -> MessageIterator<'q> {
        let messages = NonNull::new(unsafe {
            // SAFETY:
            // All pointers used here are NonNull<_> wrapped.
            call!(self.lib, notmuch_thread_get_messages)(self.inner.as_ptr())
        });
        MessageIterator {
            lib: self.lib.clone(),
            messages,
            is_from_thread: true,
            _ph: std::marker::PhantomData,
        }
    }
}

impl Drop for Thread<'_> {
    fn drop(&mut self) {
        unsafe {
            // SAFETY:
            // All pointers used here are NonNull<_> wrapped.
            call!(self.lib, notmuch_thread_destroy)(self.inner.as_ptr())
        }
    }
}

/// notmuch threads iterator.
///
///
/// Quoting the docs:
///
/// > Note that there's no explicit destructor needed for the
/// > notmuch_threads_t object. (For consistency, we do provide a
/// > notmuch_threads_destroy function, but there's no good reason
/// > to call it if the query is about to be destroyed).
///
/// So there's no need to implement Drop for this type.
pub struct ThreadsIterator<'query> {
    pub lib: Arc<NotmuchLibrary>,
    pub inner: Option<NonNull<notmuch_threads_t>>,
    pub _ph: std::marker::PhantomData<*const Query<'query>>,
}

impl<'q> Iterator for ThreadsIterator<'q> {
    type Item = Thread<'q>;
    fn next(&mut self) -> Option<Self::Item> {
        let inner = self.inner?;
        if unsafe { call!(self.lib, notmuch_threads_valid)(inner.as_ptr()) } == 1 {
            let Some(thread_inner) = NonNull::new(unsafe {
                // SAFETY:
                // All pointers used here are NonNull<_> wrapped.
                call!(self.lib, notmuch_threads_get)(inner.as_ptr())
            }) else {
                self.inner = None;
                return None;
            };
            unsafe {
                call!(self.lib, notmuch_threads_move_to_next)(inner.as_ptr());
            }
            Some(Thread {
                lib: self.lib.clone(),
                inner: thread_inner,
                _ph: std::marker::PhantomData,
            })
        } else {
            self.inner = None;
            None
        }
    }
}
