/*
 * meli - notmuch backend
 *
 * Copyright 2019 - 2022 Manos Pitsidianakis
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

pub trait MelibQueryToNotmuchQuery {
    fn query_to_string(&self, ret: &mut String);
}

impl MelibQueryToNotmuchQuery for crate::search::Query {
    fn query_to_string(&self, ret: &mut String) {
        use crate::search::Query::*;
        match self {
            Before(timestamp) => {
                ret.push_str("date:..@");
                ret.push_str(&timestamp.to_string());
            }
            After(timestamp) => {
                ret.push_str("date:@");
                ret.push_str(&timestamp.to_string());
                ret.push_str("..");
            }
            Between(a, b) => {
                ret.push_str("date:@");
                ret.push_str(&a.to_string());
                ret.push_str("..@");
                ret.push_str(&b.to_string());
            }
            On(timestamp) => {
                ret.push_str("date:@");
                ret.push_str(&timestamp.to_string());
            }
            /* * * * */
            From(s) => {
                ret.push_str("from:\"");
                for c in s.chars() {
                    if c == '"' {
                        ret.push_str("\\\"");
                    } else {
                        ret.push(c);
                    }
                }
                ret.push('"');
            }
            To(s) | Cc(s) | Bcc(s) => {
                ret.push_str("to:\"");
                for c in s.chars() {
                    if c == '"' {
                        ret.push_str("\\\"");
                    } else {
                        ret.push(c);
                    }
                }
                ret.push('"');
            }
            InReplyTo(_s) | References(_s) | AllAddresses(_s) => {}
            /* * * * */
            Body(s) => {
                ret.push_str("body:\"");
                for c in s.chars() {
                    if c == '"' {
                        ret.push_str("\\\"");
                    } else {
                        ret.push(c);
                    }
                }
                ret.push('"');
            }
            Subject(s) => {
                ret.push_str("subject:\"");
                for c in s.chars() {
                    if c == '"' {
                        ret.push_str("\\\"");
                    } else {
                        ret.push(c);
                    }
                }
                ret.push('"');
            }
            AllText(s) => {
                ret.push('"');
                for c in s.chars() {
                    if c == '"' {
                        ret.push_str("\\\"");
                    } else {
                        ret.push(c);
                    }
                }
                ret.push('"');
            }
            /* * * * */
            Flags(v) => {
                for f in v {
                    ret.push_str("tag:\"");
                    for c in f.chars() {
                        if c == '"' {
                            ret.push_str("\\\"");
                        } else {
                            ret.push(c);
                        }
                    }
                    ret.push_str("\" ");
                }
                if !v.is_empty() {
                    ret.pop();
                }
            }
            HasAttachment => {
                ret.push_str("tag:attachment");
            }
            And(q1, q2) => {
                ret.push('(');
                q1.query_to_string(ret);
                ret.push_str(") AND (");
                q2.query_to_string(ret);
                ret.push(')');
            }
            Or(q1, q2) => {
                ret.push('(');
                q1.query_to_string(ret);
                ret.push_str(") OR (");
                q2.query_to_string(ret);
                ret.push(')');
            }
            Not(q) => {
                ret.push_str("(NOT (");
                q.query_to_string(ret);
                ret.push_str("))");
            }
        }
    }
}

pub struct Query<'s> {
    #[allow(dead_code)]
    pub lib: Arc<libloading::Library>,
    pub ptr: *mut notmuch_query_t,
    pub query_str: &'s str,
}

impl<'s> Query<'s> {
    pub fn new(database: &DbConnection, query_str: &'s str) -> Result<Self> {
        let lib: Arc<libloading::Library> = database.state.lib.clone();
        let query_cstr = std::ffi::CString::new(query_str)?;
        let query: *mut notmuch_query_t = unsafe {
            call!(lib, notmuch_query_create)(*database.inner.read().unwrap(), query_cstr.as_ptr())
        };
        if query.is_null() {
            return Err(MeliError::new("Could not create query. Out of memory?"));
        }
        Ok(Query {
            lib,
            ptr: query,
            query_str,
        })
    }

    pub fn count(&self) -> Result<u32> {
        let mut count = 0_u32;
        unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_query_count_messages)(self.ptr, &mut count as *mut _)
            )
            .map_err(|err| err.0)?;
        }
        Ok(count)
    }

    pub fn search(&'s self) -> Result<MessageIterator<'s>> {
        let mut messages: *mut notmuch_messages_t = std::ptr::null_mut();
        let status = unsafe {
            call!(self.lib, notmuch_query_search_messages)(self.ptr, &mut messages as *mut _)
        };
        if status != 0 {
            return Err(MeliError::new(format!(
                "Search for {} returned {}",
                self.query_str, status,
            )));
        }
        assert!(!messages.is_null());
        Ok(MessageIterator {
            messages,
            lib: self.lib.clone(),
            _ph: std::marker::PhantomData,
            is_from_thread: false,
        })
    }
}

impl Drop for Query<'_> {
    fn drop(&mut self) {
        unsafe {
            call!(self.lib, notmuch_query_destroy)(self.ptr);
        }
    }
}
