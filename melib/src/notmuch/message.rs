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

use std::{borrow::Cow, cell::Cell, marker::PhantomData, ptr::NonNull};

use super::*;
use crate::{
    notmuch::ffi::{
        notmuch_database_find_message, notmuch_message_add_tag, notmuch_message_destroy,
        notmuch_message_freeze, notmuch_message_get_date, notmuch_message_get_filename,
        notmuch_message_get_header, notmuch_message_get_message_id, notmuch_message_get_replies,
        notmuch_message_remove_tag, notmuch_message_tags_to_maildir_flags, notmuch_message_thaw,
        notmuch_messages_get, notmuch_messages_move_to_next, notmuch_messages_valid,
        NOTMUCH_STATUS_READ_ONLY_DATABASE, NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW,
    },
    thread::{ThreadHash, ThreadNode, ThreadNodeHash},
};

pub struct Message<'m> {
    pub lib: Arc<NotmuchLibrary>,
    pub message: NonNull<ffi::notmuch_message_t>,
    pub is_from_thread: bool,
    pub freezes: Cell<u8>,
    pub _ph: PhantomData<&'m ffi::notmuch_message_t>,
}

impl<'m> Message<'m> {
    pub fn find_message(db: &'m DbConnection, msg_id: &CStr) -> Result<Self> {
        let mut message: *mut ffi::notmuch_message_t = std::ptr::null_mut();
        let lib = db.lib.clone();
        unsafe {
            call!(lib, notmuch_database_find_message)(
                db.inner.lock().unwrap().as_mut(),
                msg_id.as_ptr(),
                std::ptr::addr_of_mut!(message),
            )
        };
        Ok(Self {
            lib,
            message: NonNull::new(message).ok_or_else(|| {
                Error::new(format!(
                    "Message with message id {msg_id:?} not found in notmuch database.",
                ))
            })?,
            is_from_thread: false,
            freezes: 0.into(),
            _ph: PhantomData,
        })
    }

    #[doc(alias = "find_message_by_filename")]
    pub fn find_message_by_path(db: &'m DbConnection, path: &CStr) -> Result<Option<Self>> {
        let mut message: *mut ffi::notmuch_message_t = std::ptr::null_mut();
        let lib = db.lib.clone();
        unsafe {
            call!(lib, ffi::notmuch_database_find_message_by_filename)(
                db.inner.lock().unwrap().as_mut(),
                path.as_ptr(),
                std::ptr::addr_of_mut!(message),
            )
        };
        Ok(NonNull::new(message).map(|message| Self {
            lib,
            message,
            is_from_thread: false,
            freezes: 0.into(),
            _ph: PhantomData,
        }))
    }

    pub fn env_hash(&self) -> EnvelopeHash {
        let msg_id =
            unsafe { call!(self.lib, notmuch_message_get_message_id)(self.message.as_ptr()) };
        let c_str = unsafe { CStr::from_ptr(msg_id) };
        EnvelopeHash::from_bytes(c_str.to_bytes_with_nul())
    }

    pub fn header(&self, header: &CStr) -> Option<&[u8]> {
        let header_val = unsafe {
            call!(self.lib, notmuch_message_get_header)(self.message.as_ptr(), header.as_ptr())
        };
        if header_val.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(header_val).to_bytes() })
        }
    }

    #[inline]
    /// Return the `Message-ID` associated with this message as bytes.
    ///
    /// See also [`Message::msg_id_cstr`] and [`Message::msg_id_str`].
    pub fn msg_id(&self) -> &[u8] {
        let c_str = self.msg_id_cstr();
        c_str.to_bytes()
    }

    #[inline]
    /// Return the `Message-ID` associated with this message as a C string.
    ///
    /// See also [`Message::msg_id_cstr`] and [`Message::msg_id_str`].
    pub fn msg_id_cstr(&self) -> &CStr {
        let msg_id =
            unsafe { call!(self.lib, notmuch_message_get_message_id)(self.message.as_ptr()) };
        unsafe { CStr::from_ptr(msg_id) }
    }

    #[inline]
    /// Return the `Message-ID` associated with this message as a string slice.
    ///
    /// See also [`Message::msg_id_cstr`] and [`Message::msg_id_str`].
    pub fn msg_id_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.msg_id())
    }

    pub fn date(&self) -> crate::UnixTimestamp {
        (unsafe { call!(self.lib, notmuch_message_get_date)(self.message.as_ptr()) }) as u64
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
            tag_lock.entry(num).or_insert(tag);
            env.tags_mut().insert(num);
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
            let messages = Some(NonNull::new(unsafe {
                call!(self.lib, notmuch_message_get_replies)(self.message.as_ptr())
            })?);
            Some(MessageIterator {
                lib: self.lib.clone(),
                messages,
                _ph: PhantomData,
                is_from_thread: true,
            })
        } else {
            None
        }
    }

    pub fn into_thread_node(&self) -> (ThreadNodeHash, ThreadNode) {
        let (flags, _) = TagIterator::new(self).collect_flags_and_tags();
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
                unseen: !flags.intersects(Flag::SEEN),
            },
        )
    }

    #[doc(alias = "notmuch_message_add_tag")]
    pub fn add_tag(&self, tag: &CStr) -> Result<()> {
        if let Err(err) = unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_message_add_tag)(self.message.as_ptr(), tag.as_ptr())
            )
        } {
            return Err(Error::new("Could not set tag.").set_source(Some(Arc::new(err))));
        }
        Ok(())
    }

    #[doc(alias = "notmuch_message_remove_tag")]
    pub fn remove_tag(&self, tag: &CStr) -> Result<()> {
        if let Err(err) = unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_message_remove_tag)(self.message.as_ptr(), tag.as_ptr())
            )
        } {
            return Err(Error::new("Could not set tag.").set_source(Some(Arc::new(err))));
        }
        Ok(())
    }

    pub fn tags(&'m self) -> TagIterator<'m> {
        TagIterator::new(self)
    }

    #[doc(alias = "notmuch_message_tags_to_maildir_flags")]
    pub fn tags_to_maildir_flags(&self) -> Result<()> {
        if let Err(err) = unsafe {
            try_call!(
                self.lib,
                call!(self.lib, notmuch_message_tags_to_maildir_flags)(self.message.as_ptr())
            )
        } {
            return Err(Error::new("Could not set flags.").set_source(Some(Arc::new(err))));
        }
        Ok(())
    }

    /// Get a filename for the email corresponding to 'message'.
    ///
    /// Quoted from `libnotmuch` C header:
    ///
    /// The returned filename is an absolute filename, (the initial
    /// component will match
    /// [`notmuch_database_get_path`](crate::notmuch::ffi::notmuch_database_get_path)).
    ///
    /// The returned string belongs to the message so should not be
    /// modified or freed by the caller (nor should it be referenced after
    /// the message is destroyed).
    ///
    /// Note: If this message corresponds to multiple files in the mail
    /// store, (that is, multiple files contain identical message IDs),
    /// this function will arbitrarily return a single one of those
    /// filenames. See
    /// [`notmuch_message_get_filenames`](crate::notmuch::ffi::notmuch_message_get_filenames)
    /// for returning the
    /// complete list of filenames.
    #[doc(alias = "notmuch_message_get_filename")]
    pub fn get_filename(&self) -> &OsStr {
        let fs_path =
            unsafe { call!(self.lib, notmuch_message_get_filename)(self.message.as_ptr()) };
        let c_str = unsafe { CStr::from_ptr(fs_path) };
        OsStr::from_bytes(c_str.to_bytes())
    }

    /// Freeze the current state of 'message' within the database.
    ///
    /// Quoted from `libnotmuch` C header:
    ///
    /// This means that changes to the message state, (via
    /// [`notmuch_message_add_tag`], [`notmuch_message_remove_tag`](, and
    /// [`notmuch_message_remove_all_tags`](crate::notmuch::ffi::notmuch_message_remove_all_tags)), will not be committed to the
    /// database until the message is thawed with
    /// [`notmuch_message_thaw`].
    ///
    /// Multiple calls to freeze/thaw are valid and these calls will
    /// "stack". That is there must be as many calls to thaw as to freeze
    /// before a message is actually thawed.
    ///
    /// The ability to do freeze/thaw allows for safe transactions to
    /// change tag values. For example, explicitly setting a message to
    /// have a given set of tags might look like this:
    ///
    /// ```c
    /// notmuch_message_freeze(message);
    ///
    /// notmuch_message_remove_all_tags(message);
    ///
    /// for (i = 0; i < NUM_TAGS; i++)
    ///     notmuch_message_add_tag(message, tags[i]);
    ///
    /// notmuch_message_thaw(message);
    /// ```
    ///
    /// With freeze/thaw used like this, the message in the database is
    /// guaranteed to have either the full set of original tag values, or
    /// the full set of new tag values, but nothing in between.
    ///
    /// Imagine the example above without freeze/thaw and the operation
    /// somehow getting interrupted. This could result in the message being
    /// left with no tags if the interruption happened after
    /// [`notmuch_message_remove_all_tags`](crate::notmuch::ffi::notmuch_message_remove_all_tags) but before [`notmuch_message_add_tag`].
    ///
    /// Return value:
    ///
    /// - [`NOTMUCH_STATUS_SUCCESS`](crate::notmuch::ffi::NOTMUCH_STATUS_SUCCESS): Message successfully frozen.
    /// - [`NOTMUCH_STATUS_READ_ONLY_DATABASE`]: Database was opened in
    ///   read-only mode so message cannot be modified.
    #[doc(alias = "notmuch_message_freeze")]
    pub fn freeze(&self) {
        if NOTMUCH_STATUS_READ_ONLY_DATABASE
            == unsafe { call!(self.lib, notmuch_message_freeze)(self.message.as_ptr()) }
        {
            return;
        }
        self.freezes.set(self.freezes.get() + 1);
    }

    /// Thaw the current 'message', synchronizing any changes that may have
    /// occurred while 'message' was frozen into the notmuch database.
    ///
    /// Quoted from `libnotmuch` C header:
    ///
    /// See [`notmuch_message_freeze`](Message::freeze) for an example of how
    /// to use this function to safely provide tag changes.
    ///
    /// Multiple calls to freeze/thaw are valid and these calls with "stack".
    /// That is there must be as many calls to thaw as to freeze before a
    /// message is actually thawed.
    ///
    /// Return value:
    ///
    /// - [`NOTMUCH_STATUS_SUCCESS`](crate::notmuch::ffi::NOTMUCH_STATUS_SUCCESS): Message successfully thawed, (or at least its frozen count has successfully been reduced by 1).
    /// - [`NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW`]: An attempt was made to thaw
    ///   an unfrozen message. That is, there have been an unbalanced number of
    ///   calls to [`notmuch_message_freeze`] and [`notmuch_message_thaw`].
    #[doc(alias = "notmuch_message_thaw")]
    pub fn thaw(&self) {
        if self.freezes.get() == 0 {
            return;
        }

        if NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW
            == unsafe { call!(self.lib, notmuch_message_thaw)(self.message.as_ptr()) }
        {
            return;
        }
        self.freezes.set(self.freezes.get() - 1);
    }
}

impl Drop for Message<'_> {
    fn drop(&mut self) {
        while self.freezes.get() > 0 {
            self.thaw();
        }
        unsafe { call!(self.lib, notmuch_message_destroy)(self.message.as_ptr()) };
    }
}

pub struct MessageIterator<'query> {
    pub lib: Arc<NotmuchLibrary>,
    pub messages: Option<NonNull<ffi::notmuch_messages_t>>,
    pub is_from_thread: bool,
    pub _ph: PhantomData<*const Query<'query>>,
}

impl<'q> Iterator for MessageIterator<'q> {
    type Item = Message<'q>;

    fn next(&mut self) -> Option<Self::Item> {
        let messages = self.messages?;
        if unsafe { call!(self.lib, notmuch_messages_valid)(messages.as_ptr()) } == 1 {
            let message = unsafe { call!(self.lib, notmuch_messages_get)(messages.as_ptr()) };
            unsafe {
                call!(self.lib, notmuch_messages_move_to_next)(messages.as_ptr());
            }
            Some(Message {
                lib: self.lib.clone(),
                message: NonNull::new(message)?,
                is_from_thread: self.is_from_thread,
                freezes: 0.into(),
                _ph: PhantomData,
            })
        } else {
            self.messages = None;
            None
        }
    }
}
