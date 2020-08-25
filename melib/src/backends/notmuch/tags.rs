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

pub struct TagIterator<'m> {
    pub tags: *mut notmuch_tags_t,
    pub message: &'m Message<'m>,
}

impl Drop for TagIterator<'_> {
    fn drop(&mut self) {
        unsafe { call!(self.message.lib, notmuch_tags_destroy)(self.tags) };
    }
}

impl<'m> TagIterator<'m> {
    pub fn new(message: &'m Message<'m>) -> TagIterator<'m> {
        TagIterator {
            tags: unsafe { call!(message.lib, notmuch_message_get_tags)(message.message) },
            message,
        }
    }

    pub fn collect_flags_and_tags(self) -> (Flag, Vec<String>) {
        fn flags(path: &CStr) -> Flag {
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
        let fs_path =
            unsafe { call!(self.message.lib, notmuch_message_get_filename)(self.message.message) };
        let c_str = unsafe { CStr::from_ptr(fs_path) };

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

        (flag | flags(c_str), vec)
    }
}

impl<'m> Iterator for TagIterator<'m> {
    type Item = &'m CStr;
    fn next(&mut self) -> Option<Self::Item> {
        if self.tags.is_null() {
            None
        } else if unsafe { call!(self.message.lib, notmuch_tags_valid)(self.tags) } == 1 {
            let ret = Some(unsafe {
                CStr::from_ptr(call!(self.message.lib, notmuch_tags_get)(self.tags))
            });
            unsafe {
                call!(self.message.lib, notmuch_tags_move_to_next)(self.tags);
            }
            ret
        } else {
            self.tags = std::ptr::null_mut();
            None
        }
    }
}
