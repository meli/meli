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
use crate::thread::ThreadHash;

pub struct Thread<'query> {
    pub lib: Arc<libloading::Library>,
    pub ptr: *mut notmuch_thread_t,
    pub _ph: std::marker::PhantomData<*const Query<'query>>,
}

impl<'q> Thread<'q> {
    pub fn id(&self) -> ThreadHash {
        let thread_id = unsafe { call!(self.lib, notmuch_thread_get_thread_id)(self.ptr) };
        let c_str = unsafe { CStr::from_ptr(thread_id) };
        ThreadHash::from(c_str.to_bytes())
    }

    pub fn date(&self) -> crate::UnixTimestamp {
        (unsafe { call!(self.lib, notmuch_thread_get_newest_date)(self.ptr) }) as u64
    }

    pub fn len(&self) -> usize {
        (unsafe { call!(self.lib, notmuch_thread_get_total_messages)(self.ptr) }) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&'q self) -> MessageIterator<'q> {
        let ptr = unsafe { call!(self.lib, notmuch_thread_get_messages)(self.ptr) };
        MessageIterator {
            lib: self.lib.clone(),
            messages: ptr,
            is_from_thread: true,
            _ph: std::marker::PhantomData,
        }
    }
}

impl Drop for Thread<'_> {
    fn drop(&mut self) {
        unsafe { call!(self.lib, notmuch_thread_destroy)(self.ptr) }
    }
}

pub struct ThreadsIterator<'query> {
    pub lib: Arc<libloading::Library>,
    pub threads: *mut notmuch_threads_t,
    pub _ph: std::marker::PhantomData<*const Query<'query>>,
}

impl<'q> Iterator for ThreadsIterator<'q> {
    type Item = Thread<'q>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.threads.is_null() {
            None
        } else if unsafe { call!(self.lib, notmuch_threads_valid)(self.threads) } == 1 {
            let thread = unsafe { call!(self.lib, notmuch_threads_get)(self.threads) };
            unsafe {
                call!(self.lib, notmuch_threads_move_to_next)(self.threads);
            }
            Some(Thread {
                lib: self.lib.clone(),
                ptr: thread,
                _ph: std::marker::PhantomData,
            })
        } else {
            self.threads = std::ptr::null_mut();
            None
        }
    }
}
