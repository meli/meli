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

use std::sync::MutexGuard;

use isahc::config::Configurable;

use super::*;
use crate::error::NetworkErrorKind;

#[derive(Debug)]
pub struct JmapConnection {
    pub session: Arc<Mutex<JmapSession>>,
    pub request_no: Arc<Mutex<usize>>,
    pub client: Arc<HttpClient>,
    pub server_conf: JmapServerConf,
    pub store: Arc<Store>,
}

impl JmapConnection {
    pub fn new(server_conf: &JmapServerConf, store: Arc<Store>) -> Result<Self> {
        let client = HttpClient::builder()
            .timeout(std::time::Duration::from_secs(10))
            .connection_cache_size(8)
            .connection_cache_ttl(std::time::Duration::from_secs(30 * 60))
            .default_header("Content-Type", "application/json")
            .ssl_options(if server_conf.danger_accept_invalid_certs {
                isahc::config::SslOption::DANGER_ACCEPT_INVALID_CERTS
                    | isahc::config::SslOption::DANGER_ACCEPT_INVALID_HOSTS
                    | isahc::config::SslOption::DANGER_ACCEPT_REVOKED_CERTS
            } else {
                isahc::config::SslOption::NONE
            })
            .redirect_policy(RedirectPolicy::Limit(10));
        let client = if server_conf.use_token {
            client
                .authentication(isahc::auth::Authentication::none())
                .default_header(
                    "Authorization",
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
            session: Arc::new(Mutex::new(Default::default())),
            request_no: Arc::new(Mutex::new(0)),
            client: Arc::new(client),
            server_conf,
            store,
        })
    }

    pub async fn connect(&mut self) -> Result<()> {
        if self.store.online_status.lock().await.1.is_ok() {
            return Ok(());
        }

        fn to_well_known(uri: &str) -> String {
            let uri = uri.trim_start_matches('/');
            format!("{uri}/.well-known/jmap")
        }

        let mut jmap_session_resource_url = to_well_known(&self.server_conf.server_url);

        let mut req = match self.client.get_async(&jmap_session_resource_url).await {
            Err(err) => 'block: {
                if matches!(NetworkErrorKind::from(err.kind()), NetworkErrorKind::ProtocolViolation if self.server_conf.server_url.starts_with("http://"))
                {
                    // attempt recovery by trying https://
                    self.server_conf.server_url = format!(
                        "https{}",
                        self.server_conf.server_url.trim_start_matches("http")
                    );
                    jmap_session_resource_url = to_well_known(&self.server_conf.server_url);
                    if let Ok(s) = self.client.get_async(&jmap_session_resource_url).await {
                        log::error!(
                            "Account {} server URL should start with `https`. Please correct \
                                 your configuration value. Its current value is `{}`.",
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
                     suppported.)\n\nError connecting to server: {}",
                    &self.server_conf.server_url, &err
                ))
                .set_source(Some(Arc::new(err)));
                *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                return Err(err);
            }
            Ok(s) => s,
        };

        if !req.status().is_success() {
            let kind: crate::error::NetworkErrorKind = req.status().into();
            let res_text = req.text().await.unwrap_or_default();
            let err = Error::new(format!(
                "Could not connect to JMAP server endpoint for {}. Reply from server: {}",
                &self.server_conf.server_url, res_text
            ))
            .set_kind(kind.into());
            *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }

        let res_text = match req.text().await {
            Err(err) => {
                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {}. Is your server url setting \
                     correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     suppported.)\n\nReply from server: {}",
                    &self.server_conf.server_url, &err
                ))
                .set_source(Some(Arc::new(err)));
                *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                return Err(err);
            }
            Ok(s) => s,
        };

        let session: JmapSession = match serde_json::from_str(&res_text) {
            Err(err) => {
                let err = Error::new(format!(
                    "Could not connect to JMAP server endpoint for {}. Is your server url setting \
                     correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource \
                     discovery via /.well-known/jmap is supported. DNS SRV records are not \
                     suppported.)\n\nReply from server: {}",
                    &self.server_conf.server_url, &res_text
                ))
                .set_source(Some(Arc::new(err)));
                *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                return Err(err);
            }
            Ok(s) => s,
        };
        if !session
            .capabilities
            .contains_key("urn:ietf:params:jmap:core")
        {
            let err = Error::new(format!(
                "Server {} did not return JMAP Core capability (urn:ietf:params:jmap:core). \
                 Returned capabilities were: {}",
                &self.server_conf.server_url,
                session
                    .capabilities
                    .keys()
                    .map(String::as_str)
                    .collect::<Vec<&str>>()
                    .join(", ")
            ));
            *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }
        if !session
            .capabilities
            .contains_key("urn:ietf:params:jmap:mail")
        {
            let err = Error::new(format!(
                "Server {} does not support JMAP Mail capability (urn:ietf:params:jmap:mail). \
                 Returned capabilities were: {}",
                &self.server_conf.server_url,
                session
                    .capabilities
                    .keys()
                    .map(String::as_str)
                    .collect::<Vec<&str>>()
                    .join(", ")
            ));
            *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }

        *self.store.online_status.lock().await = (Instant::now(), Ok(()));
        *self.session.lock().unwrap() = session;
        Ok(())
    }

    pub fn mail_account_id(&self) -> Id<Account> {
        self.session.lock().unwrap().primary_accounts["urn:ietf:params:jmap:mail"].clone()
    }

    pub fn session_guard(&'_ self) -> MutexGuard<'_, JmapSession> {
        self.session.lock().unwrap()
    }

    pub fn add_refresh_event(&self, event: RefreshEvent) {
        (self.store.event_consumer)(self.store.account_hash, BackendEvent::Refresh(event));
    }

    pub async fn email_changes(&self, mailbox_hash: MailboxHash) -> Result<()> {
        let mut current_state: State<EmailObject> = if let Some(s) = self
            .store
            .mailboxes
            .read()
            .unwrap()
            .get(&mailbox_hash)
            .and_then(|mbox| mbox.email_state.lock().unwrap().clone())
        {
            s
        } else {
            return Ok(());
        };
        loop {
            let email_changes_call: EmailChanges = EmailChanges::new(
                Changes::<EmailObject>::new()
                    .account_id(self.mail_account_id().clone())
                    .since_state(current_state.clone()),
            );

            let mut req = Request::new(self.request_no.clone());
            let prev_seq = req.add_call(&email_changes_call);
            let email_get_call: EmailGet = EmailGet::new(
                Get::new()
                    .ids(Some(JmapArgument::reference(
                        prev_seq,
                        ResultField::<EmailChanges, EmailObject>::new("/created"),
                    )))
                    .account_id(self.mail_account_id().clone()),
            );

            req.add_call(&email_get_call);
            if let Some(mailbox) = self.store.mailboxes.read().unwrap().get(&mailbox_hash) {
                if let Some(email_query_state) = mailbox.email_query_state.lock().unwrap().clone() {
                    let email_query_changes_call = EmailQueryChanges::new(
                        QueryChanges::new(self.mail_account_id().clone(), email_query_state)
                            .filter(Some(Filter::Condition(
                                EmailFilterCondition::new()
                                    .in_mailbox(Some(mailbox.id.clone()))
                                    .into(),
                            ))),
                    );
                    let seq_no = req.add_call(&email_query_changes_call);
                    let email_get_call: EmailGet = EmailGet::new(
                        Get::new()
                            .ids(Some(JmapArgument::reference(
                                seq_no,
                                ResultField::<EmailQueryChanges, EmailObject>::new("/removed"),
                            )))
                            .account_id(self.mail_account_id().clone())
                            .properties(Some(vec![
                                "keywords".to_string(),
                                "mailboxIds".to_string(),
                            ])),
                    );
                    req.add_call(&email_get_call);
                } else {
                    return Ok(());
                }
            } else {
                return Ok(());
            }
            let api_url = self.session.lock().unwrap().api_url.clone();
            let mut res = self
                .client
                .post_async(api_url.as_str(), serde_json::to_string(&req)?)
                .await?;

            let res_text = res.text().await?;
            debug!(&res_text);
            let mut v: MethodResponse = match serde_json::from_str(&res_text) {
                Err(err) => {
                    let err = Error::new(format!(
                        "BUG: Could not deserialize {} server JSON response properly, please \
                         report this!\nReply from server: {}",
                        &self.server_conf.server_url, &res_text
                    ))
                    .set_source(Some(Arc::new(err)))
                    .set_kind(ErrorKind::Bug);
                    *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
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
                for (env, mailbox_hashes) in list
                    .into_iter()
                    .map(|obj| self.store.add_envelope(obj))
                    .zip(mailbox_hashes)
                {
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
            let reverse_id_store_lck = self.store.reverse_id_store.lock().unwrap();
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
                    debug!(mailbox_hash);
                    debug!(err);
                }
            }
            let GetResponse::<EmailObject> { list, .. } =
                GetResponse::<EmailObject>::try_from(v.method_responses.remove(0))?;
            let mut mailboxes_lck = self.store.mailboxes.write().unwrap();
            for envobj in list {
                if let Some(env_hash) = reverse_id_store_lck.get(&envobj.id) {
                    let new_flags =
                        protocol::keywords_to_flags(envobj.keywords().keys().cloned().collect());
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
            drop(mailboxes_lck);
            if changes_response.has_more_changes {
                current_state = changes_response.new_state;
            } else {
                self.store
                    .mailboxes
                    .write()
                    .unwrap()
                    .entry(mailbox_hash)
                    .and_modify(|mbox| {
                        *mbox.email_state.lock().unwrap() = Some(changes_response.new_state);
                    });

                break;
            }
        }

        Ok(())
    }
}
