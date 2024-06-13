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

use std::ptr::NonNull;

use super::*;
use crate::notmuch::ffi::{
    notmuch_message_get_filename, notmuch_message_get_tags, notmuch_tags_destroy, notmuch_tags_get,
    notmuch_tags_move_to_next, notmuch_tags_t, notmuch_tags_valid,
};

pub struct TagIterator<'m> {
    pub tags: Option<NonNull<notmuch_tags_t>>,
    pub message: &'m Message<'m>,
}

impl Drop for TagIterator<'_> {
    fn drop(&mut self) {
        if let Some(tags) = self.tags {
            unsafe { call!(self.message.lib, notmuch_tags_destroy)(tags.as_ptr()) };
        }
    }
}

impl<'m> TagIterator<'m> {
    pub fn new(message: &'m Message<'m>) -> Self {
        Self {
            tags: NonNull::new(unsafe {
                call!(message.lib, notmuch_message_get_tags)(message.message.as_ptr())
            }),
            message,
        }
    }

    pub fn collect_flags_and_tags(self) -> (Flag, Vec<String>) {
        fn flags(fs_path: NonNull<std::ffi::c_char>) -> Flag {
            let path = unsafe { CStr::from_ptr(fs_path.as_ptr()) };
            let mut flag = Flag::default();
            let mut ptr = path.to_bytes().len().saturating_sub(1);
            let mut is_valid = true;
            while !path.to_bytes()[..ptr + 1].ends_with(b":2,") {
                match path.to_bytes()[ptr] {
                    b'D' => flag |= Flag::DRAFT,
                    b'F' => flag |= Flag::FLAGGED,
                    b'P' => flag |= Flag::PASSED,
                    b'R' => flag |= Flag::REPLIED,
                    b'S' => flag |= Flag::SEEN,
                    b'T' => flag |= Flag::TRASHED,
                    _ => {
                        is_valid = false;
                        break;
                    }
                }
                if ptr == 0 {
                    is_valid = false;
                    break;
                }
                ptr -= 1;
            }

            if !is_valid {
                return Flag::default();
            }

            flag
        }
        let fs_path = unsafe {
            // SAFETY;
            // all used pointers here are NonNull<wrapped>, and the cast to *mut _
            // afterwards is only to wrap the retval into a NonNull as well.
            call!(self.message.lib, notmuch_message_get_filename)(self.message.message.as_ptr())
        } as *mut std::ffi::c_char;

        let tags = self.collect::<Vec<&CStr>>();
        let mut flag = Flag::default();
        let mut vec = vec![];
        for t in tags {
            match t.to_bytes() {
                b"draft" => {
                    flag.set(Flag::DRAFT, true);
                }
                b"flagged" => {
                    flag.set(Flag::FLAGGED, true);
                }
                b"passed" => {
                    flag.set(Flag::PASSED, true);
                }
                b"replied" => {
                    flag.set(Flag::REPLIED, true);
                }
                b"unread" => {
                    flag.set(Flag::SEEN, false);
                }
                b"trashed" => {
                    flag.set(Flag::TRASHED, true);
                }
                _other => {
                    vec.push(t.to_string_lossy().into_owned());
                }
            }
        }

        (
            flag | NonNull::new(fs_path).map(flags).unwrap_or_default(),
            vec,
        )
    }
}

impl<'m> Iterator for TagIterator<'m> {
    type Item = &'m CStr;

    fn next(&mut self) -> Option<Self::Item> {
        let tags = self.tags?;
        if unsafe { call!(self.message.lib, notmuch_tags_valid)(tags.as_ptr()) } == 1 {
            let ret = Some(unsafe {
                CStr::from_ptr(call!(self.message.lib, notmuch_tags_get)(tags.as_ptr()))
            });
            unsafe {
                call!(self.message.lib, notmuch_tags_move_to_next)(tags.as_ptr());
            }
            ret
        } else {
            unsafe { call!(self.message.lib, notmuch_tags_destroy)(tags.as_ptr()) };
            self.tags = None;
            None
        }
    }
}
