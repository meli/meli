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

use super::*;
use isahc::config::Configurable;

#[derive(Debug)]
pub struct JmapConnection {
    pub session: JmapSession,
    pub request_no: Arc<Mutex<usize>>,
    pub client: Arc<HttpClient>,
    pub server_conf: JmapServerConf,
    pub store: Arc<Store>,
}

impl JmapConnection {
    pub fn new(server_conf: &JmapServerConf, store: Arc<Store>) -> Result<Self> {
        let client = HttpClient::builder()
            .timeout(std::time::Duration::from_secs(10))
            .authentication(isahc::auth::Authentication::basic())
            .credentials(isahc::auth::Credentials::new(
                &server_conf.server_username,
                &server_conf.server_password,
            ))
            .build()?;
        let server_conf = server_conf.clone();
        Ok(JmapConnection {
            session: Default::default(),
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
        let mut jmap_session_resource_url =
            if self.server_conf.server_hostname.starts_with("https://") {
                self.server_conf.server_hostname.to_string()
            } else {
                format!("https://{}", &self.server_conf.server_hostname)
            };
        if self.server_conf.server_port != 443 {
            jmap_session_resource_url.push(':');
            jmap_session_resource_url.push_str(&self.server_conf.server_port.to_string());
        }
        jmap_session_resource_url.push_str("/.well-known/jmap");

        let mut req = self.client.get_async(&jmap_session_resource_url).await?;
        let res_text = req.text_async().await?;

        let session: JmapSession = match serde_json::from_str(&res_text) {
            Err(err) => {
                let err = MeliError::new(format!("Could not connect to JMAP server endpoint for {}. Is your server hostname setting correct? (i.e. \"jmap.mailserver.org\") (Note: only session resource discovery via /.well-known/jmap is supported. DNS SRV records are not suppported.)\nReply from server: {}", &self.server_conf.server_hostname, &res_text)).set_source(Some(Arc::new(err)));
                *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
                return Err(err);
            }
            Ok(s) => s,
        };
        if !session
            .capabilities
            .contains_key("urn:ietf:params:jmap:core")
        {
            let err = MeliError::new(format!("Server {} did not return JMAP Core capability (urn:ietf:params:jmap:core). Returned capabilities were: {}", &self.server_conf.server_hostname, session.capabilities.keys().map(String::as_str).collect::<Vec<&str>>().join(", ")));
            *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }
        if !session
            .capabilities
            .contains_key("urn:ietf:params:jmap:mail")
        {
            let err = MeliError::new(format!("Server {} does not support JMAP Mail capability (urn:ietf:params:jmap:mail). Returned capabilities were: {}", &self.server_conf.server_hostname, session.capabilities.keys().map(String::as_str).collect::<Vec<&str>>().join(", ")));
            *self.store.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }

        *self.store.online_status.lock().await = (Instant::now(), Ok(()));
        self.session = session;
        Ok(())
    }

    pub fn mail_account_id(&self) -> &Id<Account> {
        &self.session.primary_accounts["urn:ietf:params:jmap:mail"]
    }

    pub fn add_refresh_event(&self, event: RefreshEvent) {
        (self.store.event_consumer)(self.store.account_hash, BackendEvent::Refresh(event));
    }

    pub async fn email_changes(&self) -> Result<()> {
        let mut current_state: State<EmailObject> = self.store.email_state.lock().unwrap().clone();
        if current_state.is_empty() {
            debug!("{:?}: has no saved state", EmailObject::NAME);
            return Ok(());
        }
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
                        ResultField::<EmailChanges, EmailObject>::new("created"),
                    )))
                    .account_id(self.mail_account_id().clone()),
            );

            req.add_call(&email_get_call);
            
            let mut res = self
                .client
                .post_async(&self.session.api_url, serde_json::to_string(&req)?)
                .await?;

            let res_text = res.text_async().await?;
            debug!(&res_text);
            let mut v: MethodResponse = serde_json::from_str(&res_text).unwrap();
            let get_response =
                GetResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
            debug!(&get_response);
            let GetResponse::<EmailObject> { list, .. } = get_response;
            let changes_response =
                ChangesResponse::<EmailObject>::try_from(v.method_responses.pop().unwrap())?;
            if changes_response.new_state == current_state {
                return Ok(());
            }

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
                    self.add_refresh_event(RefreshEvent {
                        account_hash: self.store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Create(Box::new(env.clone())),
                    });
                }
                if let Some(mailbox_hash) = mailbox_hashes.first().cloned() {
                    self.add_refresh_event(RefreshEvent {
                        account_hash: self.store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Create(Box::new(env)),
                    });
                }
            }

            let ChangesResponse::<EmailObject> {
                account_id: _,
                new_state,
                old_state: _,
                has_more_changes,
                created: _,
                updated,
                destroyed,
                _ph: _,
            } = changes_response;
            for (env_hash, mailbox_hashes) in destroyed
                .into_iter()
                .filter_map(|obj_id| self.store.remove_envelope(obj_id))
            {
                for mailbox_hash in mailbox_hashes {
                    self.add_refresh_event(RefreshEvent {
                        account_hash: self.store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Remove(env_hash),
                    });
                }
            }

            if has_more_changes {
                current_state = new_state;
            } else {
                *self.store.email_state.lock().unwrap() = new_state;
                break;
            }
        }

        Ok(())
    }
}
