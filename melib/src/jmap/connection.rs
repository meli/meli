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

use std::{
    convert::TryFrom,
    sync::Arc,
    time::{Duration, Instant},
};

use futures::lock::{MappedMutexGuard as FutureMappedMutexGuard, Mutex as FutureMutex};
use isahc::{
    config::{Configurable, DnsCache, RedirectPolicy, SslOption},
    http, AsyncReadResponseExt, HttpClient,
};
use smallvec::SmallVec;
use url::Url;

use crate::{
    email::parser::BytesExt,
    error::{Error, ErrorKind, NetworkErrorKind, Result},
    jmap::{
        argument::Argument,
        capabilities::*,
        deserialize_from_str,
        email::{
            EmailChanges, EmailFilterCondition, EmailGet, EmailObject, EmailQueryChanges,
            EmailQueryChangesResponse,
        },
        filters::Filter,
        identity::{IdentityGet, IdentityObject, IdentitySet},
        mailbox::MailboxObject,
        methods::{
            AddedItem, Changes, ChangesResponse, Get, GetResponse, MethodResponse, QueryChanges,
            QueryChangesResponse, ResultField, Set,
        },
        objects::{Id, State},
        protocol::{self, JmapMailCapability, Request},
        session::Session,
        JmapServerConf, Store,
    },
    BackendEvent, Flag, MailboxHash, RefreshEvent, RefreshEventKind,
};

#[derive(Debug)]
pub struct JmapConnection {
    pub request_no: Arc<FutureMutex<usize>>,
    pub client: Arc<HttpClient>,
    pub server_conf: JmapServerConf,
    pub store: Arc<Store>,
    pub last_method_response: Option<String>,
}

impl JmapConnection {
    pub fn new(server_conf: &JmapServerConf, store: Arc<Store>) -> Result<Self> {
        let client = HttpClient::builder()
            .timeout(Duration::from_secs(10))
            .dns_cache(DnsCache::Forever)
            .connection_cache_size(8)
            .connection_cache_ttl(Duration::from_secs(30 * 60))
            .default_header(http::header::CONTENT_TYPE, "application/json")
            .ssl_options(if server_conf.danger_accept_invalid_certs {
                SslOption::DANGER_ACCEPT_INVALID_CERTS
                    | SslOption::DANGER_ACCEPT_INVALID_HOSTS
                    | SslOption::DANGER_ACCEPT_REVOKED_CERTS
            } else {
                SslOption::NONE
            })
            .tcp_nodelay()
            .tcp_keepalive(Duration::new(60 * 9, 0))
            .redirect_policy(RedirectPolicy::Limit(10));
        let client = if let Some(dur) = server_conf.timeout.filter(|dur| *dur != Duration::ZERO) {
            client
                .timeout(dur)
                .connect_timeout(dur + Duration::from_secs(300))
        } else {
            client
        };
        let client = if server_conf.use_token {
            client
                .authentication(isahc::auth::Authentication::none())
                .default_header(
                    http::header::AUTHORIZATION,
                    format!("Bearer {}", &server_conf.server_password),
                )
        } else {
            client
                .authentication(isahc::auth::Authentication::basic())
                .credentials(isahc::auth::Credentials::new(
                    &server_conf.server_username,
                    &server_conf.server_password,
                ))
        };
        let client = client.build()?;
        let server_conf = server_conf.clone();
        Ok(Self {
            request_no: Arc::new(FutureMutex::new(0)),
            client: Arc::new(client),
            server_conf,
            store,
            last_method_response: None,
        })
    }

    pub async fn connect(&mut self) -> Result<()> {
        if self.store.online_status.is_ok().await {
            return Ok(());
        }

        fn to_well_known(uri: &Url) -> Url {
            let mut uri = uri.clone();
            uri.set_path(".well-known/jmap");
            uri
        }

        let mut jmap_session_resource_url = to_well_known(&self.server_conf.server_url);

        let mut req = match self.get_async(&jmap_session_resource_url).await {
            Err(err) => 'block: {
                if matches!(err.kind, ErrorKind::Network(NetworkErrorKind::ProtocolViolation) if self.server_conf.server_url.scheme() == "http")
                {
                    // attempt recovery by trying https://
                    self.server_conf.server_url.set_scheme("https").expect(
                        "set_scheme to https must succeed here because we checked earlier that \
                         current scheme is http",
                    );
                    jmap_session_resource_url = to_well_known(&self.server_conf.server_url);
                    if let Ok(s) = self.get_async(&jmap_session_resource_url).await {
                        log::error!(
                            "Account {} server URL should start with `https`. Please correct your \
                             configuration value. Its current value is `{}`.",
                            self.store.account_name,
                            self.server_conf.server_url
                        );
                        break 'block s;
                    }
                }

                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {}. Is your server url setting \
                     correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     supported)\n\nError connecting to server: {}",
                    &self.server_conf.server_url, &err
                ))
                .set_source(Some(Arc::new(err)));
                _ = self.store.online_status.set(None, Err(err.clone())).await;
                return Err(err);
            }
            Ok(s) => s,
        };
        let req_instant = Instant::now();

        if !req.status().is_success() {
            let kind: crate::error::NetworkErrorKind = req.status().into();
            let res_text = req.text().await.unwrap_or_default();
            let mut err = Error::new(format!(
                "Could not connect to JMAP server endpoint for {}. Reply from server: {}",
                &self.server_conf.server_url, res_text
            ))
            .set_kind(kind.into());
            if req.status() == 401 {
                let mut supports_bearer = false;
                let mut supports_basic = false;
                for val in req.headers().get_all(http::header::WWW_AUTHENTICATE).iter() {
                    supports_bearer |= val.as_bytes().contains_subsequence(b"Bearer".as_slice());
                    supports_basic |= val.as_bytes().contains_subsequence(b"Basic".as_slice());
                }
                match (self.server_conf.use_token, supports_bearer, supports_basic) {
                    (false, true, _) => {
                        err = err.set_details(
                            "The server rejected your authentication credentials because it \
                             expects authentication with a Bearer token instead of a password. \
                             Check your provider's client connection documentation. Note that to \
                             use Bearer token authentication, you must explicitly set \
                             `use_token=true` in the account's configuration.",
                        );
                    }
                    (true, false, true) => {
                        err = err.set_details(
                            "The server rejected your authentication credentials because it \
                             expects authentication with a username and password but `use_token` \
                             is set to `true`. Try setting it to `false`.",
                        );
                    }
                    (_, false, false) => {
                        let schemes = req
                            .headers()
                            .get_all(http::header::WWW_AUTHENTICATE)
                            .iter()
                            .map(|val| String::from_utf8_lossy(val.as_bytes()).to_string())
                            .collect::<Vec<String>>();
                        if schemes.is_empty() {
                            err = err
                                .set_details(
                                    "The server fails to report what authentication schemes it \
                                     supports. Please report this to your e-mail provider! (The \
                                     server is expected to provide the supported schemes with the \
                                     WWW-Authenticate HTTP header)",
                                )
                                .set_kind(ErrorKind::ProtocolError);
                        } else {
                            err = err.set_details(format!(
                                "The server does not support any of the implemented \
                                 authentication mechanisms (Basic or Bearer token). Here are the \
                                 authentication schemes it reports to support: {}",
                                schemes.join(", ")
                            ));
                        }
                    }
                    (true, true, _) | (false, _, true) => {
                        err = err.set_details(
                            "The server rejected your authentication credentials. Confirm you are \
                             not using an invalid password or token value.",
                        );
                    }
                }
            }
            _ = self
                .store
                .online_status
                .set(Some(req_instant), Err(err.clone()))
                .await;
            return Err(err);
        }

        let res_text = match req.text().await {
            Err(err) => {
                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {}. Is your server url setting \
                     correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     supported)\n\nReply from server: {}",
                    &self.server_conf.server_url, &err
                ))
                .set_source(Some(Arc::new(err)));
                _ = self
                    .store
                    .online_status
                    .set(Some(req_instant), Err(err.clone()))
                    .await;
                return Err(err);
            }
            Ok(s) => s,
        };

        let session: Session = match deserialize_from_str(&res_text) {
            Err(err) => {
                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {}. Is your server url setting \
                     correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     supported)\n\nReply from server: {}",
                    &self.server_conf.server_url, &res_text
                ))
                .set_source(Some(Arc::new(err)));
                _ = self
                    .store
                    .online_status
                    .set(Some(req_instant), Err(err.clone()))
                    .await;
                return Err(err);
            }
            Ok(s) => s,
        };
        macro_rules! check_for_cap {
            ($cap:ident) => {{
                if !session.capabilities.contains_key($cap::URI) {
                    let err = Error::new(format!(
                        "Server {} did not return {name} ({uri}). Returned capabilities were: {}",
                        &self.server_conf.server_url,
                        session
                            .capabilities
                            .keys()
                            .map(String::as_str)
                            .collect::<Vec<&str>>()
                            .join(", "),
                        name = $cap::NAME,
                        uri = $cap::URI
                    ));
                    _ = self
                        .store
                        .online_status
                        .set(Some(req_instant), Err(err.clone()))
                        .await;
                    return Err(err);
                }
            }};
        }

        check_for_cap! { JmapCoreCapability };
        check_for_cap! { JmapMailCapability };

        self.store
            .core_capabilities
            .lock()
            .unwrap()
            .clone_from(&session.capabilities);
        let mail_account_id = session.mail_account_id();
        _ = self
            .store
            .online_status
            .set(Some(req_instant), Ok(session))
            .await;

        // Fetch account identities.

        let mut id_list = {
            let mut req = Request::new(self.request_no.clone());
            let identity_get = IdentityGet::new().account_id(mail_account_id.clone());
            req.add_call(&identity_get).await;
            let res_text = self
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;
            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = self
                        .store
                        .online_status
                        .set(Some(req_instant), Err(err.clone()))
                        .await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse::<IdentityObject> { list, .. } =
                GetResponse::<IdentityObject>::try_from(v.method_responses.remove(0))?;
            list
        };
        if id_list.is_empty() {
            let mut req = Request::new(self.request_no.clone());
            let identity_set = IdentitySet(
                Set::<IdentityObject>::new(None)
                    .account_id(mail_account_id.clone())
                    .create(Some({
                        let address =
                            crate::email::Address::try_from(self.store.main_identity.as_str())
                                .unwrap_or_else(|_| {
                                    crate::email::Address::new(
                                        None,
                                        self.store.main_identity.clone(),
                                    )
                                });
                        let id: Id<IdentityObject> = Id::new_uuid_v4();
                        log::trace!(
                            "identity id = {}, {:#?}",
                            id,
                            IdentityObject {
                                id: id.clone(),
                                name: address.get_display_name().unwrap_or_default(),
                                email: address.get_email(),
                                ..IdentityObject::default()
                            }
                        );
                        indexmap! {
                            id.clone().into() => IdentityObject {
                                id,
                                name: address.get_display_name().unwrap_or_default(),
                                email: address.get_email(),
                                ..IdentityObject::default()
                            }
                        }
                    })),
            );
            req.add_call(&identity_set).await;
            let res_text = self
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;
            let _: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = self
                        .store
                        .online_status
                        .set(Some(req_instant), Err(err.clone()))
                        .await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let mut req = Request::new(self.request_no.clone());
            let identity_get = IdentityGet::new().account_id(mail_account_id.clone());
            req.add_call(&identity_get).await;
            let res_text = self
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;
            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = self
                        .store
                        .online_status
                        .set(Some(req_instant), Err(err.clone()))
                        .await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let GetResponse::<IdentityObject> { list, .. } =
                GetResponse::<IdentityObject>::try_from(v.method_responses.remove(0))?;
            id_list = list;
        }
        self.session_guard().await?.identities =
            id_list.into_iter().map(|id| (id.id.clone(), id)).collect();

        Ok(())
    }

    #[inline]
    pub async fn session_guard(
        &'_ self,
    ) -> Result<FutureMappedMutexGuard<'_, (Instant, Result<Session>), Session>> {
        self.store.online_status.session_guard().await
    }

    pub fn add_refresh_event(&self, event: RefreshEvent) {
        (self.store.event_consumer)(self.store.account_hash, BackendEvent::Refresh(event));
    }

    pub async fn email_changes(&self, mailbox_hash: MailboxHash) -> Result<()> {
        let mut current_state: State<EmailObject> =
            if let Some(s) = self.store.email_state.lock().await.clone() {
                s
            } else {
                return Ok(());
            };
        let mail_account_id = self.session_guard().await?.mail_account_id();
        loop {
            let email_changes_call: EmailChanges = EmailChanges::new(
                Changes::<EmailObject>::new()
                    .account_id(mail_account_id.clone())
                    .since_state(current_state.clone()),
            );

            let mut req = Request::new(self.request_no.clone());
            let prev_seq = req.add_call(&email_changes_call).await;
            let email_get_call: EmailGet = EmailGet::new(
                Get::new()
                    .ids(Some(Argument::reference::<
                        EmailChanges,
                        EmailObject,
                        EmailObject,
                    >(
                        prev_seq,
                        ResultField::<EmailChanges, EmailObject>::new("/created"),
                    )))
                    .account_id(mail_account_id.clone()),
            );

            req.add_call(&email_get_call).await;
            let mailbox = self
                .store
                .mailboxes
                .read()
                .unwrap()
                .get(&mailbox_hash)
                .map(|m| {
                    let email_query_state = m.email_query_state.lock().unwrap().clone();
                    let mailbox_id: Id<MailboxObject> = m.id.clone();
                    (email_query_state, mailbox_id)
                });
            if let Some((Some(email_query_state), mailbox_id)) = mailbox {
                let email_query_changes_call = EmailQueryChanges::new(
                    QueryChanges::new(mail_account_id.clone(), email_query_state).filter(Some(
                        Filter::Condition(
                            EmailFilterCondition::new()
                                .in_mailbox(Some(mailbox_id.clone()))
                                .into(),
                        ),
                    )),
                );
                let seq_no = req.add_call(&email_query_changes_call).await;
                let email_get_call: EmailGet = EmailGet::new(
                    Get::new()
                        .ids(Some(Argument::reference::<
                            EmailQueryChanges,
                            EmailObject,
                            EmailObject,
                        >(
                            seq_no,
                            ResultField::<EmailQueryChanges, EmailObject>::new("/removed"),
                        )))
                        .account_id(mail_account_id.clone())
                        .properties(Some(vec!["keywords".to_string(), "mailboxIds".to_string()])),
                );
                req.add_call(&email_get_call).await;
            } else {
                return Ok(());
            }
            let res_text = self
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;
            if cfg!(feature = "jmap-trace") {
                log::trace!(
                    "email_changes(): for mailbox {mailbox_hash} response {:?}",
                    res_text
                );
            }
            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = self.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let changes_response =
                ChangesResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            if changes_response.new_state == current_state {
                return Ok(());
            }
            let get_response = GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;

            {
                /* process get response */
                let GetResponse::<EmailObject> { list, .. } = get_response;

                let mut mailbox_hashes: Vec<SmallVec<[MailboxHash; 8]>> =
                    Vec::with_capacity(list.len());
                for envobj in &list {
                    let v = self
                        .store
                        .mailboxes
                        .read()
                        .unwrap()
                        .iter()
                        .filter(|(_, m)| envobj.mailbox_ids.contains_key(&m.id))
                        .map(|(k, _)| *k)
                        .collect::<SmallVec<[MailboxHash; 8]>>();
                    mailbox_hashes.push(v);
                }
                for (obj, mailbox_hashes) in list.into_iter().zip(mailbox_hashes) {
                    let env = self.store.add_envelope(obj).await;
                    for mailbox_hash in mailbox_hashes.iter().skip(1).cloned() {
                        let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                        mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                            if !env.is_seen() {
                                mbox.unread_emails.lock().unwrap().insert_new(env.hash());
                            }
                            mbox.total_emails.lock().unwrap().insert_new(env.hash());
                        });
                        self.add_refresh_event(RefreshEvent {
                            account_hash: self.store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Create(Box::new(env.clone())),
                        });
                    }
                    if let Some(mailbox_hash) = mailbox_hashes.first().cloned() {
                        let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                        mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                            if !env.is_seen() {
                                mbox.unread_emails.lock().unwrap().insert_new(env.hash());
                            }
                            mbox.total_emails.lock().unwrap().insert_new(env.hash());
                        });
                        self.add_refresh_event(RefreshEvent {
                            account_hash: self.store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Create(Box::new(env)),
                        });
                    }
                }
            }
            let reverse_id_store_lck = self.store.reverse_id_store.lock().await;
            let response = v.method_responses.remove(0);
            match EmailQueryChangesResponse::try_from(response) {
                Ok(EmailQueryChangesResponse {
                    collapse_threads: _,
                    query_changes_response:
                        QueryChangesResponse {
                            account_id: _,
                            old_query_state,
                            new_query_state,
                            total: _,
                            removed,
                            added,
                        },
                }) if old_query_state != new_query_state => {
                    self.store
                        .mailboxes
                        .write()
                        .unwrap()
                        .entry(mailbox_hash)
                        .and_modify(|mbox| {
                            *mbox.email_query_state.lock().unwrap() = Some(new_query_state);
                        });
                    /*  If the "filter" or "sort" includes a mutable property, the server
                    MUST include all Foos in the current results for which this
                    property may have changed.  The position of these may have moved
                    in the results, so they must be reinserted by the client to ensure
                    its query cache is correct.  */
                    for email_obj_id in removed
                        .into_iter()
                        .filter(|id| !added.iter().any(|item| item.id == *id))
                    {
                        if let Some(env_hash) = reverse_id_store_lck.get(&email_obj_id) {
                            let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                            mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                                mbox.unread_emails.lock().unwrap().remove(*env_hash);
                                mbox.total_emails.lock().unwrap().insert_new(*env_hash);
                            });
                            self.add_refresh_event(RefreshEvent {
                                account_hash: self.store.account_hash,
                                mailbox_hash,
                                kind: RefreshEventKind::Remove(*env_hash),
                            });
                        }
                    }
                    for AddedItem {
                        id: _email_obj_id,
                        index: _,
                    } in added
                    {
                        // [ref:TODO] do something with added items
                    }
                }
                Ok(_) => {}
                Err(err) => {
                    log::error!(
                        "Could not deserialize EmailQueryChangesResponse from server response:
- mailbox_hash: {mailbox_hash}
- error: {err}
- debug details:
  Json request was: {:?}
  Json reply was: {}",
                        serde_json::to_string(&req),
                        res_text
                    );
                }
            }
            let GetResponse::<EmailObject> { list, .. } =
                GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            {
                let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                for envobj in list {
                    if let Some(env_hash) = reverse_id_store_lck.get(&envobj.id) {
                        let new_flags = protocol::keywords_to_flags(
                            envobj.keywords().keys().cloned().collect(),
                        );
                        mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                            if new_flags.0.contains(Flag::SEEN) {
                                mbox.unread_emails.lock().unwrap().remove(*env_hash);
                            } else {
                                mbox.unread_emails.lock().unwrap().insert_new(*env_hash);
                            }
                        });
                        self.add_refresh_event(RefreshEvent {
                            account_hash: self.store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::NewFlags(*env_hash, new_flags),
                        });
                    }
                }
            }
            if changes_response.has_more_changes {
                current_state = changes_response.new_state;
            } else {
                *self.store.email_state.lock().await = Some(changes_response.new_state);

                break;
            }
        }

        Ok(())
    }

    pub async fn send_request(&self, request: String) -> Result<String> {
        if cfg!(feature = "jmap-trace") {
            log::trace!("send_request(): request {:?}", request);
        }
        let res_text = self.post_async(None, request).await?.text().await?;
        if cfg!(feature = "jmap-trace") {
            log::trace!("send_request(): response {:?}", res_text);
        }
        let _: MethodResponse = match deserialize_from_str(&res_text) {
            Err(err) => {
                log::error!("{}", &err);
                _ = self.store.online_status.set(None, Err(err.clone())).await;
                return Err(err);
            }
            Ok(s) => s,
        };
        Ok(res_text)
    }

    pub async fn get_async(&self, url: &Url) -> Result<isahc::Response<isahc::AsyncBody>> {
        if cfg!(feature = "jmap-trace") {
            let res = self.client.get_async(url.as_str()).await;
            log::trace!("get_async(): url `{}` response {:?}", url, res);
            Ok(res?)
        } else {
            Ok(self.client.get_async(url.as_str()).await?)
        }
    }

    pub async fn post_async<T: Into<Vec<u8>> + Send + Sync>(
        &self,
        api_url: Option<&Url>,
        request: T,
    ) -> Result<isahc::Response<isahc::AsyncBody>> {
        let request: Vec<u8> = request.into();
        if cfg!(feature = "jmap-trace") {
            log::trace!(
                "post_async(): request {:?}",
                String::from_utf8_lossy(&request)
            );
        }
        if let Some(api_url) = api_url {
            Ok(self.client.post_async(api_url.as_str(), request).await?)
        } else {
            let api_url = self.session_guard().await?.api_url.clone();
            Ok(self.client.post_async(api_url.as_str(), request).await?)
        }
    }
}
