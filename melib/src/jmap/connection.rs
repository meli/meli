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
    convert::TryFrom as _,
    sync::{atomic::AtomicUsize, Arc},
    time::{Duration, Instant},
};

use futures::lock::MappedMutexGuard as FutureMappedMutexGuard;
use isahc::{
    config::{Configurable as _, DnsCache, RedirectPolicy, SslOption},
    http, AsyncReadResponseExt as _, HttpClient,
};
use url::Url;

use crate::{
    email::parser::BytesExt as _,
    error::{Error, ErrorKind, NetworkErrorKind, Result, ResultIntoError as _},
    jmap::{
        argument::Argument,
        capabilities::*,
        deserialize_from_str,
        email::{EmailChanges, EmailGet, EmailObject},
        identity::{Identity, IdentityGet, IdentitySet},
        methods::{Changes, ChangesResponse, Get, GetResponse, MethodResponse, ResultField, Set},
        objects::{Id, State},
        protocol::{self, Request},
        session::Session,
        JmapServerConf, Store,
    },
    BackendEvent, Flag, RefreshEvent, RefreshEventKind,
};

#[derive(Debug)]
pub struct JmapClient {
    pub request_no: Arc<AtomicUsize>,
    pub http_client: Arc<HttpClient>,
    pub server_conf: JmapServerConf,
    pub store: Arc<Store>,
}

#[derive(Debug)]
pub enum JmapConnection {
    Offline {
        server_conf: JmapServerConf,
        store: Arc<Store>,
    },
    Connected {
        inner: JmapClient,
    },
}

impl JmapConnection {
    pub fn new(server_conf: JmapServerConf, store: Arc<Store>) -> Self {
        Self::Offline { server_conf, store }
    }

    pub async fn client(&mut self) -> Result<&mut JmapClient> {
        match self {
            Self::Offline { server_conf, store } => {
                let inner = JmapClient::new(server_conf, store).await?;
                *self = Self::Connected { inner };
                let Self::Connected { ref mut inner } = self else {
                    unreachable!()
                };
                Ok(inner)
            }
            Self::Connected { ref mut inner } => Ok(inner),
        }
    }
}

impl JmapClient {
    pub async fn new(server_conf: &JmapServerConf, store: &Arc<Store>) -> Result<Self> {
        let http_client = HttpClient::builder()
            .dns_cache(DnsCache::Forever)
            .connect_timeout(Duration::from_secs(60))
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
        let http_client =
            if let Some(dur) = server_conf.timeout.filter(|dur| *dur != Duration::ZERO) {
                http_client.timeout(dur)
            } else {
                http_client
            };
        let username = server_conf
            .username
            .value_with_timeout(server_conf.timeout.unwrap_or(Duration::from_millis(100)))
            .await?;
        let password = server_conf
            .password
            .value_with_timeout(server_conf.timeout.unwrap_or(Duration::from_millis(100)))
            .await?;
        let http_client = if server_conf.use_token {
            http_client
                .authentication(isahc::auth::Authentication::none())
                .default_header(http::header::AUTHORIZATION, format!("Bearer {password}"))
        } else {
            http_client
                .authentication(isahc::auth::Authentication::basic())
                .credentials(isahc::auth::Credentials::new(&username, &password))
        };
        let http_client = http_client.build()?;
        let server_conf = server_conf.clone();
        let store = store.clone();
        Ok(Self {
            request_no: Arc::new(AtomicUsize::new(0)),
            http_client: Arc::new(http_client),
            server_conf,
            store,
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

        let mut url = self
            .server_conf
            .url
            .value_with_timeout(
                self.server_conf
                    .timeout
                    .unwrap_or(Duration::from_millis(100)),
            )
            .await?
            .parse::<Url>()
            .chain_err_summary(|| {
                format!(
                    "{}: JMAP backend connection failed",
                    self.store.account_name
                )
            })
            .chain_err_kind(ErrorKind::Configuration)?;
        let mut jmap_session_resource_url = to_well_known(&url);

        let mut resp = match self.get_async(&jmap_session_resource_url).await {
            Err(err) => 'block: {
                if matches!(err.kind, ErrorKind::Network(NetworkErrorKind::ProtocolViolation) if url.scheme() == "http")
                {
                    // attempt recovery by trying https://
                    url.set_scheme("https").expect(
                        "set_scheme to https must succeed here because we checked earlier that \
                         current scheme is http",
                    );
                    jmap_session_resource_url = to_well_known(&url);
                    if let Ok(s) = self.get_async(&jmap_session_resource_url).await {
                        log::error!(
                            "Account {} server URL should start with `https`. Please correct your \
                             configuration value. Its current value is `{url}`.",
                            self.store.account_name,
                        );
                        break 'block s;
                    }
                }

                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {url}. Is your server url \
                     setting correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     supported)\n\nError connecting to server: {err}",
                ))
                .set_source(Some(Box::new(err)));
                _ = self.store.online_status.set(None, Err(err.clone())).await;
                return Err(err);
            }
            Ok(s) => s,
        };
        let req_instant = Instant::now();

        if !resp.status().is_success() {
            let kind: crate::error::NetworkErrorKind = resp.status().into();
            let res_text = resp.text().await.unwrap_or_default();
            let mut err = Error::new(format!(
                "Could not connect to JMAP server endpoint for {url}. Reply from server: \
                 {res_text}",
            ))
            .set_kind(kind.into());
            if resp.status() == 401 {
                let mut supports_bearer = false;
                let mut supports_basic = false;
                for val in resp
                    .headers()
                    .get_all(http::header::WWW_AUTHENTICATE)
                    .iter()
                {
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
                        let schemes = resp
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

        let res_text = match resp.text().await {
            Err(err) => {
                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {url}. Is your server url \
                     setting correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     supported)\n\nReply from server: {err}",
                ))
                .set_source(Some(Box::new(err)));
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
                    "Could not connect to JMAP server endpoint for {url}. Is your server url \
                     setting correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     supported)\n\nReply from server: {res_text}",
                ))
                .set_source(Some(Box::new(err)));
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
                        "Server {url} did not return {name} ({uri}). Returned capabilities were: \
                         {}",
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
        {
            let mut metadata = self.store.metadata.lock().unwrap();
            metadata.insert("session".into(), serde_json::json! {session});
        };
        _ = self
            .store
            .online_status
            .set(Some(req_instant), Ok(session))
            .await;

        // Fetch account identities.

        let mut id_list = {
            let mut req = Request::new(self.request_no.clone());
            let identity_get = IdentityGet::new(Get::new().account_id(mail_account_id.clone()));
            req.add_call(&identity_get);
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
            let GetResponse::<Identity> { list, .. } =
                GetResponse::<Identity>::try_from(v.method_responses.remove(0))?;
            list
        };
        if id_list.is_empty() {
            let mut req = Request::new(self.request_no.clone());
            let identity_set = IdentitySet(
                Set::<Identity>::new(None)
                    .account_id(mail_account_id.clone())
                    .create(Some({
                        let address =
                            crate::email::Address::try_from(self.store.main_identity.as_str())
                                .unwrap_or_else(|_| {
                                    crate::email::Address::new(
                                        None::<&str>,
                                        self.store.main_identity.clone(),
                                    )
                                });
                        let id: Id<Identity> = Id::new_random();
                        log::trace!(
                            "identity id = {}, {:#?}",
                            id,
                            Identity {
                                id: id.clone(),
                                name: address.get_display_name().unwrap_or_default().into(),
                                email: address.get_email().into(),
                                ..Identity::default()
                            }
                        );
                        indexmap! {
                            id.clone().into() => Identity {
                                id,
                                name: address.get_display_name().unwrap_or_default().into(),
                                email: address.get_email().into(),
                                ..Identity::default()
                            }
                        }
                    })),
            );
            req.add_call(&identity_set);
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
            let identity_get = IdentityGet::new(Get::new().account_id(mail_account_id.clone()));
            req.add_call(&identity_get);
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
            let GetResponse::<Identity> { list, .. } =
                GetResponse::<Identity>::try_from(v.method_responses.remove(0))?;
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

    #[inline]
    pub fn add_backend_event(&self, ev: BackendEvent) {
        (self.store.event_consumer)(self.store.account_hash, ev);
    }

    pub async fn email_changed(
        &self,
        new_state: Option<State<EmailObject>>,
    ) -> Result<Option<BackendEvent>> {
        let mut cached_state: State<EmailObject> =
            if let Some(s) = self.store.email_state.lock().await.as_ref() {
                if Some(s) == new_state.as_ref() {
                    return Ok(None);
                }
                s.clone()
            } else {
                return Ok(None);
            };
        let mail_account_id = self.session_guard().await?.mail_account_id();
        let mut events = vec![];
        loop {
            let email_changes_call: EmailChanges = EmailChanges::new(
                Changes::<EmailObject>::new()
                    .account_id(mail_account_id.clone())
                    .since_state(cached_state.clone()),
            );

            let mut req = Request::new(self.request_no.clone());
            let prev_seq = req.add_call(&email_changes_call);
            req.add_call(&EmailGet::new(
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
            ));
            req.add_call(&EmailGet::new(
                Get::new()
                    .ids(Some(Argument::reference::<
                        EmailChanges,
                        EmailObject,
                        EmailObject,
                    >(
                        prev_seq,
                        ResultField::<EmailChanges, EmailObject>::new("/updated"),
                    )))
                    .account_id(mail_account_id.clone()),
            ));

            let res_text = self
                .post_async(None, serde_json::to_string(&req)?)
                .await?
                .text()
                .await?;
            if self.server_conf.trace {
                log::trace!("email_since_state(): response {res_text:?}");
            }
            let mut v: MethodResponse = match deserialize_from_str(&res_text) {
                Err(err) => {
                    _ = self.store.online_status.set(None, Err(err.clone())).await;
                    return Err(err);
                }
                Ok(s) => s,
            };
            let mut changes_response =
                ChangesResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            if changes_response.new_state == cached_state {
                return Ok(None);
            }
            for destroyed_id in std::mem::take(&mut changes_response.destroyed) {
                if let Some((env_hash, mailbox_hashes)) =
                    self.store.remove_envelope(destroyed_id).await
                {
                    for mailbox_hash in mailbox_hashes {
                        events.push(RefreshEvent {
                            account_hash: self.store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Remove(env_hash),
                        });
                    }
                }
            }
            let get_response = GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;

            {
                // Created
                let GetResponse::<EmailObject> { list, .. } = get_response;

                for envobj in list {
                    let mailbox_hashes = envobj
                        .mailbox_ids
                        .iter()
                        .map(|(id, _)| id.into_hash())
                        .collect::<Vec<_>>();
                    let env = self.store.add_envelope(envobj).await;
                    for mailbox_hash in mailbox_hashes {
                        let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                        mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                            let mut counters = mbox.counters.lock().unwrap();
                            if !env.is_seen() {
                                counters.unseen.insert_new(env.hash());
                            }
                            counters.total.insert_new(env.hash());
                        });
                        events.push(RefreshEvent {
                            account_hash: self.store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Create(Box::new(env.clone())),
                        });
                    }
                }
            }
            let get_response = GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;

            {
                let reverse_id_store_lck = self.store.reverse_id_store.lock().await;
                // Updated
                let GetResponse::<EmailObject> { list, .. } = get_response;

                let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
                for envobj in list {
                    if let Some(env_hash) = reverse_id_store_lck.get(&envobj.id) {
                        let new_flags = protocol::keywords_to_flags(
                            envobj.keywords().keys().cloned().collect(),
                        );
                        for mailbox_id in envobj.mailbox_ids.keys() {
                            let mailbox_hash = mailbox_id.into_hash();
                            mailboxes_lck.entry(mailbox_hash).and_modify(|mbox| {
                                if new_flags.0.contains(Flag::SEEN) {
                                    mbox.counters.lock().unwrap().unseen.remove(*env_hash);
                                } else {
                                    mbox.counters.lock().unwrap().unseen.insert_new(*env_hash);
                                }
                            });
                            events.push(RefreshEvent {
                                account_hash: self.store.account_hash,
                                mailbox_hash,
                                kind: RefreshEventKind::NewFlags(*env_hash, new_flags.clone()),
                            });
                        }
                    }
                }
            }
            if !v.method_responses.is_empty() {
                panic!("{:?}", v.method_responses);
            }
            if changes_response.has_more_changes {
                cached_state = changes_response.new_state;
            } else {
                *self.store.email_state.lock().await = Some(changes_response.new_state);

                break;
            }
        }

        Ok(events.try_into().ok())
    }

    pub async fn send_request(&self, request: String) -> Result<String> {
        if self.server_conf.trace {
            log::trace!("send_request(): request {:?}", request);
        }
        let res_text = self.post_async(None, request).await?.text().await?;
        if self.server_conf.trace {
            log::trace!("send_request(): response {:?}", res_text);
        }
        let _: MethodResponse = match deserialize_from_str(&res_text) {
            Err(err) => {
                log::error!("Could not deserialize response {res_text:?}: {err}");
                _ = self.store.online_status.set(None, Err(err.clone())).await;
                return Err(err);
            }
            Ok(s) => s,
        };
        Ok(res_text)
    }

    pub async fn get_async(&self, url: &Url) -> Result<isahc::Response<isahc::AsyncBody>> {
        let mut resp = if self.server_conf.trace {
            let res = self.http_client.get_async(url.as_str()).await;
            log::trace!("get_async(): url `{}` response {:?}", url, res);
            res?
        } else {
            self.http_client.get_async(url.as_str()).await?
        };
        if !resp.status().is_success() {
            let kind: crate::error::NetworkErrorKind = resp.status().into();
            let res_text = resp.text().await.unwrap_or_default();
            let err = Error::new(format!(
                "Could not connect to JMAP server endpoint {url} for {}. Reply from server: \
                 {res_text}",
                self.store.account_name
            ))
            .set_kind(kind.into());
            _ = self
                .store
                .online_status
                .set(Some(Instant::now()), Err(err.clone()))
                .await;
            return Err(err);
        }
        Ok(resp)
    }

    pub async fn post_async<T: Into<Vec<u8>> + Send + Sync>(
        &self,
        api_url: Option<&Url>,
        request: T,
    ) -> Result<isahc::Response<isahc::AsyncBody>> {
        let request: Vec<u8> = request.into();
        if self.server_conf.trace {
            log::trace!(
                "post_async(): request {:?}",
                String::from_utf8_lossy(&request)
            );
        }
        let api_url = if let Some(api_url) = api_url {
            api_url.clone()
        } else {
            Url::clone(&self.session_guard().await?.api_url)
        };
        let resp = self.http_client.post_async(api_url.as_str(), request).await;
        if self.server_conf.trace {
            log::trace!("post_async(): response {resp:?}",);
        }
        let mut resp = resp?;
        if !resp.status().is_success() {
            let kind: crate::error::NetworkErrorKind = resp.status().into();
            let res_text = resp.text().await.unwrap_or_default();
            let err = Error::new(format!(
                "Could not connect to JMAP server endpoint {api_url} for {}. Reply from server: \
                 {res_text}",
                self.store.account_name
            ))
            .set_kind(kind.into());
            _ = self
                .store
                .online_status
                .set(Some(Instant::now()), Err(err.clone()))
                .await;
            return Err(err);
        }
        Ok(resp)
    }
}
