//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

//! Retrieve patches from sources like `lore.kernel.org`.
//!
//! # Example
//!
//! ```no_run
//! use meli::patch_retrieve::*;
//! use melib::{MessageID, StrBuild};
//!
//! let msg_id = b"20240910-rust-pl011-v10-8-85a89ee33c40@linaro.org";
//! let lore = LoreKernelOrg::new("qemu-devel.nongnu.org").unwrap();
//! assert_eq!(lore.groupname(), "org.nongnu.qemu-devel");
//! let mail =
//!     futures::executor::block_on(lore.fetch(MessageID::new(msg_id, msg_id)).unwrap()).unwrap();
//! println!("{}", String::from_utf8_lossy(&mail.as_mbox()));
//! ```

use melib::{email::Mail, nntp::NntpType, prelude::*, MessageID};

pub trait PatchSource
where
    Self: Send + Sync,
{
    fn fetch_many(&self, message_ids: &[MessageID]) -> ResultFuture<Vec<Mail>>;
    fn fetch(&self, message_id: MessageID) -> ResultFuture<Mail>;
}

pub trait PublicInbox {
    fn server_hostname(&self) -> &str;
    fn server_port(&self) -> Option<u16> {
        None
    }
    fn groupname(&self) -> &str;
    fn nntp_connection(&self) -> Result<Box<NntpType>> {
        let mut extra = indexmap::indexmap! {
            "server_hostname".into() => self.server_hostname().to_string(),
            "require_auth".into() => false.to_string(),
            "store_flags_locally".into() => false.to_string(),
        };
        if let Some(port) = self.server_port() {
            extra.insert("server_port".to_string(), port.to_string());
        }
        let event_consumer = BackendEventConsumer::new(Arc::new(|_, _| {}));
        let account_conf = melib::AccountSettings {
            name: self.server_hostname().to_string(),
            root_mailbox: self.groupname().to_string(),
            format: "nntp".to_string(),
            identity: "user@localhost".to_string(),
            extra_identities: vec![],
            read_only: true,
            display_name: None,
            order: Default::default(),
            subscribed_mailboxes: vec![self.groupname().to_string()],
            mailboxes: vec![(self.groupname().to_string(), Default::default())]
                .into_iter()
                .collect(),
            manual_refresh: true,
            extra,
        };

        NntpType::new(&account_conf, Box::new(|_| true), event_consumer)
    }
}

impl<T: PublicInbox + Send + Sync> PatchSource for T {
    fn fetch_many(&self, message_ids: &[MessageID]) -> ResultFuture<Vec<Mail>> {
        let backend = self.nntp_connection()?;
        let message_ids = message_ids.to_vec();
        let mbox_hash = MailboxHash::from_bytes(self.groupname().as_bytes());
        Ok(Box::pin(async move {
            let mut ret = vec![];
            for msg_id in message_ids {
                ret.push(backend.article_message_id(mbox_hash, msg_id)?.await?);
            }
            Ok(ret)
        }))
    }

    fn fetch(&self, message_id: MessageID) -> ResultFuture<Mail> {
        let fut = self.fetch_many(&[message_id])?;
        Ok(Box::pin(async move {
            let mut v = fut.await?;
            Ok(v.remove(0))
        }))
    }
}

pub struct LoreKernelOrg {
    list: Box<str>,
}

impl LoreKernelOrg {
    pub fn new(list: &str) -> Result<Self> {
        let mut list = list.split(".").collect::<Vec<_>>();
        list.reverse();

        let list = list.join(".").into();
        Ok(Self { list })
    }
}

impl PublicInbox for LoreKernelOrg {
    fn server_hostname(&self) -> &str {
        "nntp.lore.kernel.org"
    }

    fn groupname(&self) -> &str {
        self.list.as_ref()
    }
}
