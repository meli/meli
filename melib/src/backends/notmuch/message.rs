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
use crate::thread::{ThreadHash, ThreadNode, ThreadNodeHash};

#[derive(Clone)]
pub struct Message<'m> {
    pub lib: Arc<libloading::Library>,
    pub message: *mut notmuch_message_t,
    pub is_from_thread: bool,
    pub _ph: std::marker::PhantomData<&'m notmuch_message_t>,
}

impl<'m> Message<'m> {
    pub fn find_message(db: &'m DbConnection, msg_id: &CStr) -> Result<Message<'m>> {
        let mut message: *mut notmuch_message_t = std::ptr::null_mut();
        let lib = db.lib.clone();
        unsafe {
            call!(lib, notmuch_database_find_message)(
                *db.inner.read().unwrap(),
                msg_id.as_ptr(),
                &mut message as *mut _,
            )
        };
        if message.is_null() {
            return Err(Error::new(format!(
                "Message with message id {:?} not found in notmuch database.",
                msg_id
            )));
        }
        Ok(Message {
            lib,
            message,
            is_from_thread: false,
            _ph: std::marker::PhantomData,
        })
    }

    pub fn env_hash(&self) -> EnvelopeHash {
        let msg_id = unsafe { call!(self.lib, notmuch_message_get_message_id)(self.message) };
        let c_str = unsafe { CStr::from_ptr(msg_id) };
        EnvelopeHash::from_bytes(c_str.to_bytes_with_nul())
    }

    pub fn header(&self, header: &CStr) -> Option<&[u8]> {
        let header_val =
            unsafe { call!(self.lib, notmuch_message_get_header)(self.message, header.as_ptr()) };
        if header_val.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(header_val).to_bytes() })
        }
    }

    pub fn msg_id(&self) -> &[u8] {
        let c_str = self.msg_id_cstr();
        c_str.to_bytes()
    }

    pub fn msg_id_cstr(&self) -> &CStr {
        let msg_id = unsafe { call!(self.lib, notmuch_message_get_message_id)(self.message) };
        unsafe { CStr::from_ptr(msg_id) }
    }

    pub fn date(&self) -> crate::UnixTimestamp {
        (unsafe { call!(self.lib, notmuch_message_get_date)(self.message) }) as u64
    }

    pub fn into_envelope(
        self,
        index: &RwLock<HashMap<EnvelopeHash, CString>>,
        tag_index: &RwLock<BTreeMap<TagHash, String>>,
    ) -> Envelope {
        let env_hash = self.env_hash();
        let mut env = Envelope::new(env_hash);
        index
            .write()
            .unwrap()
            .insert(env_hash, self.msg_id_cstr().into());
        let mut tag_lock = tag_index.write().unwrap();
        let (flags, tags) = TagIterator::new(&self).collect_flags_and_tags();
        for tag in tags {
            let num = TagHash::from_bytes(tag.as_bytes());
            if !tag_lock.contains_key(&num) {
                tag_lock.insert(num, tag);
            }
            env.tags_mut().push(num);
        }
        unsafe {
            use crate::email::parser::address::rfc2822address_list;
            env.set_message_id(self.msg_id())
                .set_date(
                    self.header(CStr::from_bytes_with_nul_unchecked(b"Date\0"))
                        .unwrap_or_default(),
                )
                .set_from(
                    rfc2822address_list(
                        self.header(CStr::from_bytes_with_nul_unchecked(b"From\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
                )
                .set_to(
                    rfc2822address_list(
                        self.header(CStr::from_bytes_with_nul_unchecked(b"To\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
                )
                .set_cc(
                    rfc2822address_list(
                        self.header(CStr::from_bytes_with_nul_unchecked(b"Cc\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
                )
                .set_bcc(
                    rfc2822address_list(
                        self.header(CStr::from_bytes_with_nul_unchecked(b"Bcc\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default()
                    .to_vec(),
                )
                .set_subject(
                    self.header(CStr::from_bytes_with_nul_unchecked(b"Subject\0"))
                        .unwrap_or_default()
                        .to_vec(),
                )
                .set_references(
                    self.header(CStr::from_bytes_with_nul_unchecked(b"References\0"))
                        .unwrap_or_default(),
                )
                .set_in_reply_to(
                    self.header(CStr::from_bytes_with_nul_unchecked(b"In-Reply-To\0"))
                        .unwrap_or_default(),
                )
                .set_datetime(self.date())
                .set_flags(flags);
        }
        env
    }

    pub fn replies_iter(&self) -> Option<MessageIterator> {
        if self.is_from_thread {
            let messages = unsafe { call!(self.lib, notmuch_message_get_replies)(self.message) };
            if messages.is_null() {
                None
            } else {
                Some(MessageIterator {
                    lib: self.lib.clone(),
                    messages,
                    _ph: std::marker::PhantomData,
                    is_from_thread: true,
                })
            }
        } else {
            None
        }
    }

    pub fn into_thread_node(&self) -> (ThreadNodeHash, ThreadNode) {
        (
            ThreadNodeHash::from(self.msg_id()),
            ThreadNode {
                message: Some(self.env_hash()),
                parent: None,
                other_mailbox: false,
                children: vec![],
                date: self.date(),
                show_subject: true,
                group: ThreadHash::new(),
                unseen: false,
            },
        )
    }

    pub fn add_tag(&self, tag: &CStr) -> Result<()> {
        if let Err(err) = unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_message_add_tag)(self.message, tag.as_ptr())
            )
        } {
            return Err(Error::new("Could not set tag.").set_source(Some(Arc::new(err))));
        }
        Ok(())
    }

    pub fn remove_tag(&self, tag: &CStr) -> Result<()> {
        if let Err(err) = unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_message_remove_tag)(self.message, tag.as_ptr())
            )
        } {
            return Err(Error::new("Could not set tag.").set_source(Some(Arc::new(err))));
        }
        Ok(())
    }

    pub fn tags(&'m self) -> TagIterator<'m> {
        TagIterator::new(self)
    }

    pub fn tags_to_maildir_flags(&self) -> Result<()> {
        if let Err(err) = unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_message_tags_to_maildir_flags)(self.message)
            )
        } {
            return Err(Error::new("Could not set flags.").set_source(Some(Arc::new(err))));
        }
        Ok(())
    }

    pub fn get_filename(&self) -> &OsStr {
        let fs_path = unsafe { call!(self.lib, notmuch_message_get_filename)(self.message) };
        let c_str = unsafe { CStr::from_ptr(fs_path) };
        OsStr::from_bytes(c_str.to_bytes())
    }
}

impl Drop for Message<'_> {
    fn drop(&mut self) {
        unsafe { call!(self.lib, notmuch_message_destroy)(self.message) };
    }
}

pub struct MessageIterator<'query> {
    pub lib: Arc<libloading::Library>,
    pub messages: *mut notmuch_messages_t,
    pub is_from_thread: bool,
    pub _ph: std::marker::PhantomData<*const Query<'query>>,
}

impl<'q> Iterator for MessageIterator<'q> {
    type Item = Message<'q>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.messages.is_null() {
            None
        } else if unsafe { call!(self.lib, notmuch_messages_valid)(self.messages) } == 1 {
            let message = unsafe { call!(self.lib, notmuch_messages_get)(self.messages) };
            unsafe {
                call!(self.lib, notmuch_messages_move_to_next)(self.messages);
            }
            Some(Message {
                lib: self.lib.clone(),
                message,
                is_from_thread: self.is_from_thread,
                _ph: std::marker::PhantomData,
            })
        } else {
            self.messages = std::ptr::null_mut();
            None
        }
    }
}
