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
//! use melib::{utils::patch_retrieve::*, MessageID, StrBuild};
//!
//! let lore = PublicInboxNNTP {
//!     server_hostname: "nntp.lore.kernel.org".to_string(),
//!     port: None,
//! };
//! let msg_id = b"20240910-rust-pl011-v10-8-85a89ee33c40@linaro.org";
//! let mail = futures::executor::block_on(
//!     lore.fetch("qemu-devel.nongnu.org", MessageID::new(msg_id, msg_id))
//!         .unwrap(),
//! )
//! .unwrap();
//! println!("{}", String::from_utf8_lossy(&mail.as_mbox()));
//! ```

use crate::{email::Mail, prelude::*, MessageID};

pub trait PatchSource
where
    Self: Send + Sync,
{
    fn fetch_many(&self, list: &str, msg_ids: &[MessageID]) -> ResultFuture<Vec<Mail>>;
    fn fetch_thread(&self, list: &str, msg_id: MessageID) -> ResultFuture<Vec<Mail>>;
    fn fetch(&self, list: &str, msg_id: MessageID) -> ResultFuture<Mail>;
}

#[cfg(feature = "http")]
pub use http::*;
#[cfg(feature = "nntp")]
pub use nttp::*;

#[cfg(feature = "nntp")]
mod nttp {
    use super::*;
    use crate::nntp::NntpType;

    pub struct PublicInboxNNTP {
        pub server_hostname: String,
        pub port: Option<u16>,
    }

    impl PublicInboxNNTP {
        fn nntp_connection(&self, list: &str) -> Result<Box<NntpType>> {
            let mut extra = indexmap::indexmap! {
                "server_hostname".into() => self.server_hostname.clone(),
                "require_auth".into() => false.to_string(),
                "store_flags_locally".into() => false.to_string(),
            };
            if let Some(port) = self.port {
                extra.insert("server_port".to_string(), port.to_string());
            }
            let groupname = Self::groupname(list);
            let event_consumer = BackendEventConsumer::new(Arc::new(|_, _| {}));
            let account_conf = crate::AccountSettings {
                name: self.server_hostname.clone(),
                root_mailbox: groupname.clone(),
                format: "nntp".to_string(),
                identity: "user@localhost".to_string(),
                extra_identities: vec![],
                read_only: true,
                display_name: None,
                subscribed_mailboxes: vec![groupname.clone()],
                mailboxes: vec![(groupname, Default::default())].into_iter().collect(),
                manual_refresh: true,
                extra,
            };

            NntpType::new(&account_conf, Default::default(), event_consumer)
        }

        fn groupname(list: &str) -> String {
            let mut list = list.split(".").collect::<Vec<_>>();
            list.reverse();

            list.join(".")
        }
    }

    impl PatchSource for PublicInboxNNTP {
        fn fetch_many(&self, list: &str, msg_ids: &[MessageID]) -> ResultFuture<Vec<Mail>> {
            let backend = self.nntp_connection(list)?;
            let msg_ids = msg_ids.to_vec();
            let mbox_hash = MailboxHash::from_bytes(Self::groupname(list).as_bytes());
            Ok(Box::pin(async move {
                let mut ret = vec![];
                for msg_id in msg_ids {
                    ret.push(backend.article_message_id(mbox_hash, msg_id)?.await?);
                }
                Ok(ret)
            }))
        }

        fn fetch_thread(&self, _list: &str, _msg_id: MessageID) -> ResultFuture<Vec<Mail>> {
            Err(Error::new("Fetching a thread with NNTP is unimplemented.")
                .set_kind(ErrorKind::NotImplemented))
            // [ref:TODO] Need to do some more clever discovery of the thread
            // since NNTP does not allow us to search. We need to
            // establish the earliest ARTICLE number accessible from
            // given message id, and then query ranges starting from that number
            // to discover all articles that contain
            // in-reply-to/references to this thread. let backend =
            // self.nntp_connection(list)?; let mbox_hash =
            // MailboxHash::from_bytes(Self::groupname(list).as_bytes());
            // Ok(Box::pin(async move {
            //    let mut retrieved = HashSet::new();
            //    let mut to_retrieve = VecDeque::new();
            //    to_retrieve.push_back(msg_id);
            //
            //    let mut ret = vec![];
            //    while let Some(msg_id) = to_retrieve.pop_front() {
            //        let mail = backend
            //            .article_message_id(mbox_hash, msg_id.clone())?
            //            .await?;
            //        retrieved.insert(msg_id);
            //        if let Some(in_reply_to) = mail.in_reply_to() {
            //            for r in in_reply_to.refs() {
            //                if !retrieved.contains(&r) {
            //                    to_retrieve.push_back(r.clone());
            //                }
            //            }
            //        }
            //        for r in mail.references() {
            //            if !retrieved.contains(&r) {
            //                to_retrieve.push_back(r.clone());
            //            }
            //        }
            //
            //        ret.push(mail);
            //    }
            //    Ok(ret)
            //}))
        }

        fn fetch(&self, list: &str, msg_id: MessageID) -> ResultFuture<Mail> {
            let backend = self.nntp_connection(list)?;
            let mbox_hash = MailboxHash::from_bytes(Self::groupname(list).as_bytes());
            Ok(Box::pin(async move {
                backend.article_message_id(mbox_hash, msg_id)?.await
            }))
        }
    }
}

#[cfg(feature = "http")]
mod http {
    use std::{io::Read, time::Duration};

    use flate2::bufread::GzDecoder;
    use isahc::{
        config::{Configurable, DnsCache, RedirectPolicy},
        http, AsyncReadResponseExt, HttpClient,
    };
    use url::Url;

    use super::*;
    use crate::mbox::*;

    pub struct PublicInboxHTTP {
        url: Url,
    }

    impl PublicInboxHTTP {
        pub fn new<U: TryInto<Url>>(url: U) -> Result<Self>
        where
            <U as std::convert::TryInto<Url>>::Error: std::fmt::Display,
        {
            let url = url.try_into().map_err(|err| {
                Error::new(format!("Could not parse url: {err}",)).set_kind(ErrorKind::ValueError)
            })?;
            if !["http", "https"].contains(&url.scheme()) || url.cannot_be_a_base() {
                return Err(Error::new(format!(
                    "Scheme must be http/https, got: {scheme}",
                    scheme = url.scheme()
                ))
                .set_kind(ErrorKind::ValueError));
            }
            Ok(Self { url })
        }

        pub fn http_connection(&self) -> Result<HttpClient> {
            let client = HttpClient::builder()
                .timeout(Duration::from_secs(10))
                .default_header(http::header::USER_AGENT, "melib")
                .dns_cache(DnsCache::Forever)
                .connection_cache_size(8)
                .connection_cache_ttl(Duration::from_secs(30 * 60))
                .tcp_nodelay()
                .tcp_keepalive(Duration::new(60 * 9, 0))
                .redirect_policy(RedirectPolicy::Limit(10));

            Ok(client.build()?)
        }

        async fn fetch_one(client: &HttpClient, url: &Url) -> Result<Mail> {
            let mut resp = client.get_async(url.as_str()).await?;
            if !resp.status().is_success() {
                let kind: crate::error::NetworkErrorKind = resp.status().into();
                let res_text = resp.text().await.unwrap_or_default();
                let err =
                    Error::new(format!("Reply from server: {res_text}",)).set_kind(kind.into());
                return Err(err);
            }
            let bytes = resp.bytes().await?;
            Mail::new(bytes, None)
        }

        fn get_raw_url(&self, list: &str, msg_id: &MessageID) -> Url {
            let mut url = self.url.clone();
            url.path_segments_mut()
                .expect("This should not happen; url must !url.cannot_be_a_base()")
                .push(list)
                .push(msg_id.as_str())
                .push("raw");
            url
        }

        fn get_thread_url(&self, list: &str, msg_id: &MessageID) -> Url {
            let mut url = self.url.clone();
            url.path_segments_mut()
                .expect("This should not happen; url must !url.cannot_be_a_base()")
                .push(list)
                .push(msg_id.as_str())
                .push("t.mbox.gz");
            url
        }
    }

    impl PatchSource for PublicInboxHTTP {
        fn fetch_many(&self, list: &str, msg_ids: &[MessageID]) -> ResultFuture<Vec<Mail>> {
            let urls = msg_ids
                .iter()
                .map(|msg_id| self.get_raw_url(list, msg_id))
                .collect::<Vec<_>>();
            let client = self.http_connection()?;
            Ok(Box::pin(async move {
                let mut ret = vec![];

                for url in urls {
                    ret.push(Self::fetch_one(&client, &url).await?);
                }

                Ok(ret)
            }))
        }

        fn fetch_thread(&self, list: &str, msg_id: MessageID) -> ResultFuture<Vec<Mail>> {
            let url = self.get_thread_url(list, &msg_id);
            let client = self.http_connection()?;
            Ok(Box::pin(async move {
                let mut resp = client.get_async(url.as_str()).await?;
                if !resp.status().is_success() {
                    let kind: crate::error::NetworkErrorKind = resp.status().into();
                    let res_text = resp.text().await.unwrap_or_default();
                    let err =
                        Error::new(format!("Reply from server: {res_text}",)).set_kind(kind.into());
                    return Err(err);
                }
                let bytes = resp.bytes().await?;
                let mut gz = GzDecoder::new(bytes.as_slice());
                let mut v = vec![];
                gz.read_to_end(&mut v)?;
                let mut message_iter = MessageIterator {
                    index: Default::default(),
                    input: v.as_slice(),
                    offset: 0,
                    file_offset: 0,
                    format: MboxFormat::MboxRd,
                    is_crlf: false,
                };
                let mut ret = vec![];
                while let Some(res) = message_iter.next() {
                    let envelope = res?;
                    let bytes = message_iter.env_bytes(&envelope.hash()).to_vec();
                    ret.push(Mail { envelope, bytes });
                }
                Ok(ret)
            }))
        }

        fn fetch(&self, list: &str, msg_id: MessageID) -> ResultFuture<Mail> {
            let url = self.get_raw_url(list, &msg_id);
            let client = self.http_connection()?;
            Ok(Box::pin(
                async move { Self::fetch_one(&client, &url).await },
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use futures::executor::block_on;

    use super::*;
    use crate::email::StrBuild;

    #[test]
    #[ignore = "accesses the internet/network"]
    #[cfg(feature = "nntp")]
    fn test_lore_nntp_fetch() {
        let lore = PublicInboxNNTP {
            server_hostname: "nntp.lore.kernel.org".to_string(),
            port: None,
        };
        let msg_id = b"20240910-rust-pl011-v10-8-85a89ee33c40@linaro.org";
        let mail = futures::executor::block_on(
            lore.fetch("qemu-devel.nongnu.org", MessageID::new(msg_id, msg_id))
                .unwrap(),
        )
        .unwrap();
        eprintln!("{}", String::from_utf8_lossy(&mail.as_mbox()));
    }

    #[test]
    #[ignore = "accesses the internet/network"]
    #[cfg(feature = "http")]
    fn test_lore_http_fetch() {
        let lore = PublicInboxHTTP::new("https://lore.kernel.org").unwrap();
        let msg_id = MessageID::new(
            b"<20260525-hw_random_registration_rng_list-v1-1-ee1c215d544d@pitsidianak.is>",
            b"20260525-hw_random_registration_rng_list-v1-1-ee1c215d544d@pitsidianak.is",
        );
        eprintln!("{:?}", block_on(lore.fetch("all", msg_id).unwrap()));
    }

    #[test]
    #[ignore = "accesses the internet/network"]
    #[cfg(feature = "http")]
    fn test_lore_http_fetch_thread() {
        let lore = PublicInboxHTTP::new("https://lore.kernel.org").unwrap();
        let msg_id = MessageID::new(
            b"<20260521-rust-gitignore-long-types-txt-v1-1-5be5e6fa427c@pitsidianak.is>",
            b"20260521-rust-gitignore-long-types-txt-v1-1-5be5e6fa427c@pitsidianak.is",
        );
        eprintln!(
            "{:#?}",
            block_on(lore.fetch_thread("rust-for-linux", msg_id).unwrap())
        );
    }
}
