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
    collections::{BTreeMap, HashMap},
    ffi::{CStr, CString},
    sync::{Arc, RwLock},
};

use crate::{
    backends::prelude::*,
    notmuch::{DbConnection, Message, TagIterator},
};

#[derive(Debug)]
pub struct Snapshot {
    /// A snapshot of the database connection at the latest point of
    /// synchronisation.
    ///
    /// A connection represents the state of the database at its time of
    /// creation. Keeping it stored allows comparing its state to newer
    /// connections and being able to compute differences.
    pub connection: DbConnection,
    /// Index from [`EnvelopeHash`] to `Message-ID`.
    pub message_id_index: HashMap<EnvelopeHash, CString>,
    /// Index of which mailboxes an envelope is in.
    pub env_to_mailbox_index: HashMap<EnvelopeHash, SmallVec<[MailboxHash; 16]>>,
    /// Mutex copy of [`Collection::tag_index`] associated with this account.
    pub tag_index: Arc<RwLock<BTreeMap<TagHash, String>>>,
    pub account_hash: AccountHash,
}

impl Snapshot {
    /// Convert `message` into an [`Envelope`], update inner caches and return
    /// it.
    pub fn insert_envelope(&mut self, message: &Message<'_>) -> Envelope {
        let env_hash = message.env_hash();
        let mut env = Envelope::new(env_hash);
        self.message_id_index
            .insert(env_hash, message.msg_id_cstr().into());
        let (flags, tags) = TagIterator::new(message).collect_flags_and_tags();
        {
            let mut tag_index_lck = self.tag_index.write().unwrap();
            for tag in tags {
                let num = TagHash::from_bytes(tag.as_bytes());
                tag_index_lck.entry(num).or_insert(tag);
                env.tags_mut().insert(num);
            }
        }
        unsafe {
            use crate::email::parser::address::rfc2822address_list;
            env.set_message_id(message.msg_id())
                .set_date(
                    message
                        .header(CStr::from_bytes_with_nul_unchecked(b"Date\0"))
                        .unwrap_or_default(),
                )
                .set_from(
                    rfc2822address_list(
                        message
                            .header(CStr::from_bytes_with_nul_unchecked(b"From\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
                )
                .set_to(
                    rfc2822address_list(
                        message
                            .header(CStr::from_bytes_with_nul_unchecked(b"To\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
                )
                .set_cc(
                    rfc2822address_list(
                        message
                            .header(CStr::from_bytes_with_nul_unchecked(b"Cc\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
                )
                .set_bcc(
                    rfc2822address_list(
                        message
                            .header(CStr::from_bytes_with_nul_unchecked(b"Bcc\0"))
                            .unwrap_or_default(),
                    )
                    .map(|(_, v)| v)
                    .unwrap_or_default()
                    .to_vec(),
                )
                .set_subject(
                    message
                        .header(CStr::from_bytes_with_nul_unchecked(b"Subject\0"))
                        .unwrap_or_default()
                        .to_vec(),
                )
                .set_references(
                    message
                        .header(CStr::from_bytes_with_nul_unchecked(b"References\0"))
                        .unwrap_or_default(),
                )
                .set_in_reply_to(
                    message
                        .header(CStr::from_bytes_with_nul_unchecked(b"In-Reply-To\0"))
                        .unwrap_or_default(),
                )
                .set_datetime(message.date())
                .set_flags(flags);
        }
        env
    }

    /// Remove `env_hash` from inner caches and return any associated refresh
    /// events.
    pub fn remove_envelope(&mut self, env_hash: EnvelopeHash) -> Vec<RefreshEvent> {
        let mut events = vec![];
        for mailbox_hash in self
            .env_to_mailbox_index
            .entry(env_hash)
            .or_default()
            .drain(..)
        {
            // [ref:TODO]: fix total/unseen counts for mailbox
            events.push(RefreshEvent {
                account_hash: self.account_hash,
                mailbox_hash,
                kind: RefreshEventKind::Remove(env_hash),
            });
        }
        self.message_id_index.remove(&env_hash);
        self.env_to_mailbox_index.remove(&env_hash);
        events
    }
}
