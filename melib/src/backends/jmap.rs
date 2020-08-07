/*
 * meli - jmap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use crate::async_workers::{Async, WorkContext};
use crate::backends::*;
use crate::conf::AccountSettings;
use crate::email::*;
use crate::error::{MeliError, Result};
use futures::lock::Mutex as FutureMutex;
use isahc::prelude::HttpClient;
use isahc::ResponseExt;
use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;
use std::str::FromStr;
use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;

#[macro_export]
macro_rules! _impl {
        ($(#[$outer:meta])*$field:ident : $t:ty) => {
            $(#[$outer])*
            pub fn $field(mut self, new_val: $t) -> Self {
                self.$field = new_val;
                self
            }
        };
        (get_mut $(#[$outer:meta])*$method:ident, $field:ident : $t:ty) => {
            $(#[$outer])*
            pub fn $method(&mut self) -> &mut $t {
                &mut self.$field
            }
        };
        (get $(#[$outer:meta])*$method:ident, $field:ident : $t:ty) => {
            $(#[$outer])*
            pub fn $method(&self) -> &$t {
                &self.$field
            }
        }
    }

pub mod operations;
use operations::*;

pub mod connection;
use connection::*;

pub mod protocol;
use protocol::*;

pub mod rfc8620;
use rfc8620::*;

pub mod objects;
use objects::*;

pub mod mailbox;
use mailbox::*;

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<String>,
    headers: Option<String>,
    body: Option<String>,
    flags: Option<Flag>,
}

#[derive(Debug, Clone)]
pub struct JmapServerConf {
    pub server_hostname: String,
    pub server_username: String,
    pub server_password: String,
    pub server_port: u16,
    pub danger_accept_invalid_certs: bool,
}

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
                "Configuration error ({}): JMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    MeliError::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

impl JmapServerConf {
    pub fn new(s: &AccountSettings) -> Result<Self> {
        Ok(JmapServerConf {
            server_hostname: get_conf_val!(s["server_hostname"])?.to_string(),
            server_username: get_conf_val!(s["server_username"])?.to_string(),
            server_password: get_conf_val!(s["server_password"])?.to_string(),
            server_port: get_conf_val!(s["server_port"], 443)?,
            danger_accept_invalid_certs: get_conf_val!(s["danger_accept_invalid_certs"], false)?,
        })
    }
}

struct IsSubscribedFn(Box<dyn Fn(&str) -> bool + Send + Sync>);

impl std::fmt::Debug for IsSubscribedFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "IsSubscribedFn Box")
    }
}

impl std::ops::Deref for IsSubscribedFn {
    type Target = Box<dyn Fn(&str) -> bool + Send + Sync>;
    fn deref(&self) -> &Box<dyn Fn(&str) -> bool + Send + Sync> {
        &self.0
    }
}
macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
                "Configuration error ({}): JMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    MeliError::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

#[derive(Debug, Default)]
pub struct Store {
    byte_cache: HashMap<EnvelopeHash, EnvelopeCache>,
    id_store: HashMap<EnvelopeHash, Id>,
    blob_id_store: HashMap<EnvelopeHash, Id>,
}

#[derive(Debug)]
pub struct JmapType {
    account_name: String,
    online: Arc<FutureMutex<(Instant, Result<()>)>>,
    is_subscribed: Arc<IsSubscribedFn>,
    server_conf: JmapServerConf,
    connection: Arc<FutureMutex<JmapConnection>>,
    store: Arc<RwLock<Store>>,
    tag_index: Arc<RwLock<BTreeMap<u64, String>>>,
    mailboxes: Arc<RwLock<HashMap<MailboxHash, JmapMailbox>>>,
}

impl MailBackend for JmapType {
    fn capabilities(&self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            is_async: true,
            is_remote: true,
            supports_search: true,
            extensions: None,
            supports_tags: true,
        };
        CAPABILITIES
    }

    fn is_online_async(&self) -> ResultFuture<()> {
        let online = self.online.clone();
        Ok(Box::pin(async move {
            //match timeout(std::time::Duration::from_secs(3), connection.lock()).await {
            let online_lck = online.lock().await;
            if online_lck.1.is_err()
                && Instant::now().duration_since(online_lck.0) >= std::time::Duration::new(2, 0)
            {
                //let _ = self.mailboxes();
            }
            online_lck.1.clone()
        }))
    }

    fn fetch_async(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        let mailboxes = self.mailboxes.clone();
        let store = self.store.clone();
        let tag_index = self.tag_index.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async_stream::try_stream! {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let res = protocol::fetch(
                &conn,
                &store,
                &tag_index,
                &mailboxes,
                mailbox_hash,
            ).await?;
            yield res;
        }))
    }

    fn watch_async(&self, _sender: RefreshEventConsumer) -> ResultFuture<()> {
        Err(MeliError::from("JMAP watch for updates is unimplemented"))
    }

    fn mailboxes_async(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let mailboxes = self.mailboxes.clone();
        let connection = self.connection.clone();
        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            if mailboxes.read().unwrap().is_empty() {
                let new_mailboxes = debug!(protocol::get_mailboxes(&conn).await)?;
                *mailboxes.write().unwrap() = new_mailboxes;
            }

            let ret = mailboxes
                .read()
                .unwrap()
                .iter()
                .filter(|(_, f)| f.is_subscribed)
                .map(|(&h, f)| (h, BackendMailbox::clone(f) as Mailbox))
                .collect();

            Ok(ret)
        }))
    }

    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        Ok(Box::new(JmapOp::new(
            hash,
            self.connection.clone(),
            self.store.clone(),
        )))
    }

    fn save(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn as_any(&self) -> &dyn ::std::any::Any {
        self
    }

    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        Some(self.tag_index.clone())
    }

    fn search(
        &self,
        q: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        let connection = self.connection.clone();
        let filter = if let Some(mailbox_hash) = mailbox_hash {
            let mailbox_id = self.mailboxes.read().unwrap()[&mailbox_hash].id.clone();

            let mut f = Filter::Condition(
                EmailFilterCondition::new()
                    .in_mailbox(Some(mailbox_id))
                    .into(),
            );
            f &= Filter::<EmailFilterCondition, EmailObject>::from(q);
            f
        } else {
            Filter::<EmailFilterCondition, EmailObject>::from(q)
        };

        Ok(Box::pin(async move {
            let mut conn = connection.lock().await;
            conn.connect().await?;
            let email_call: EmailQuery = EmailQuery::new(
                Query::new()
                    .account_id(conn.mail_account_id().to_string())
                    .filter(Some(filter))
                    .position(0),
            )
            .collapse_threads(false);

            let mut req = Request::new(conn.request_no.clone());
            req.add_call(&email_call);

            let mut res = conn
                .client
                .post_async(&conn.session.api_url, serde_json::to_string(&req)?)
                .await?;

            let res_text = res.text_async().await?;
            let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
            *conn.online_status.lock().await = (std::time::Instant::now(), Ok(()));
            let m = QueryResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            let QueryResponse::<EmailObject> { ids, .. } = m;
            let ret = ids
                .into_iter()
                .map(|id| {
                    use std::hash::Hasher;
                    let mut h = std::collections::hash_map::DefaultHasher::new();
                    h.write(id.as_bytes());
                    h.finish()
                })
                .collect();
            Ok(ret)
        }))
    }

    fn fetch(&mut self, _mailbox_hash: MailboxHash) -> Result<Async<Result<Vec<Envelope>>>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn watch(
        &self,
        _sender: RefreshEventConsumer,
        _work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        Err(MeliError::new("Unimplemented."))
    }

    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>> {
        Err(MeliError::new("Unimplemented."))
    }
}

impl JmapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
    ) -> Result<Box<dyn MailBackend>> {
        let online = Arc::new(FutureMutex::new((
            std::time::Instant::now(),
            Err(MeliError::new("Account is uninitialised.")),
        )));
        let server_conf = JmapServerConf::new(s)?;

        Ok(Box::new(JmapType {
            connection: Arc::new(FutureMutex::new(JmapConnection::new(
                &server_conf,
                online.clone(),
            )?)),
            store: Arc::new(RwLock::new(Store::default())),
            tag_index: Arc::new(RwLock::new(Default::default())),
            mailboxes: Arc::new(RwLock::new(HashMap::default())),
            account_name: s.name.clone(),
            online,
            is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            server_conf,
        }))
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"])?;
        get_conf_val!(s["server_password"])?;
        get_conf_val!(s["server_port"], 443)?;
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        Ok(())
    }
}
