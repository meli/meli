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
use crate::notmuch::ffi::notmuch_tags_t;

pub struct TagIterator<'m> {
    pub tags: Option<NonNull<notmuch_tags_t>>,
    pub message: &'m Message<'m>,
}

impl Drop for TagIterator<'_> {
    fn drop(&mut self) {
        if let Some(tags) = self.tags {
            unsafe { (self.message.lib.tags_destroy())(tags.as_ptr()) };
        }
    }
}

impl<'m> TagIterator<'m> {
    pub fn new(message: &'m Message<'m>) -> Self {
        Self {
            tags: NonNull::new(unsafe {
                (message.lib.message_get_tags())(message.message.as_ptr())
            }),
            message,
        }
    }

    pub fn collect_flags_and_tags(self) -> (Flag, Vec<String>) {
        let tags = self.collect::<Vec<&CStr>>();
        let mut flag = Flag::default();
        flag.set(Flag::SEEN, true);
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

        (flag, vec)
    }
}

impl<'m> Iterator for TagIterator<'m> {
    type Item = &'m CStr;

    fn next(&mut self) -> Option<Self::Item> {
        let tags = self.tags?;
        if unsafe { (self.message.lib.tags_valid())(tags.as_ptr()) } == 1 {
            let ret = Some(unsafe { CStr::from_ptr((self.message.lib.tags_get())(tags.as_ptr())) });
            unsafe {
                (self.message.lib.tags_move_to_next())(tags.as_ptr());
            }
            ret
        } else {
            unsafe { (self.message.lib.tags_destroy())(tags.as_ptr()) };
            self.tags = None;
            None
        }
    }
}
