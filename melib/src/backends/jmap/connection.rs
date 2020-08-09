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
    pub online_status: Arc<FutureMutex<(Instant, Result<()>)>>,
    pub server_conf: JmapServerConf,
    pub account_id: Arc<Mutex<String>>,
    pub method_call_states: Arc<Mutex<HashMap<&'static str, String>>>,
    pub refresh_events: Arc<Mutex<Vec<RefreshEvent>>>,
    pub sender: Arc<Mutex<Option<RefreshEventConsumer>>>,
}

impl JmapConnection {
    pub fn new(
        server_conf: &JmapServerConf,
        online_status: Arc<FutureMutex<(Instant, Result<()>)>>,
    ) -> Result<Self> {
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
            online_status,
            server_conf,
            account_id: Arc::new(Mutex::new(String::new())),
            method_call_states: Arc::new(Mutex::new(Default::default())),
            refresh_events: Arc::new(Mutex::new(Default::default())),
            sender: Arc::new(Mutex::new(Default::default())),
        })
    }

    pub async fn connect(&mut self) -> Result<()> {
        if self.online_status.lock().await.1.is_ok() {
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
                *self.online_status.lock().await = (Instant::now(), Err(err.clone()));
                return Err(err);
            }
            Ok(s) => s,
        };
        if !session
            .capabilities
            .contains_key("urn:ietf:params:jmap:core")
        {
            let err = MeliError::new(format!("Server {} did not return JMAP Core capability (urn:ietf:params:jmap:core). Returned capabilities were: {}", &self.server_conf.server_hostname, session.capabilities.keys().map(String::as_str).collect::<Vec<&str>>().join(", ")));
            *self.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }
        if !session
            .capabilities
            .contains_key("urn:ietf:params:jmap:mail")
        {
            let err = MeliError::new(format!("Server {} does not support JMAP Mail capability (urn:ietf:params:jmap:mail). Returned capabilities were: {}", &self.server_conf.server_hostname, session.capabilities.keys().map(String::as_str).collect::<Vec<&str>>().join(", ")));
            *self.online_status.lock().await = (Instant::now(), Err(err.clone()));
            return Err(err);
        }

        *self.online_status.lock().await = (Instant::now(), Ok(()));
        self.session = session;
        Ok(())
    }

    pub fn mail_account_id(&self) -> &Id {
        &self.session.primary_accounts["urn:ietf:params:jmap:mail"]
    }

    pub fn add_refresh_event(&self, event: RefreshEvent) {
        if let Some(ref sender) = self.sender.lock().unwrap().as_ref() {
            for event in self.refresh_events.lock().unwrap().drain(..) {
                sender.send(event);
            }
            sender.send(event);
        } else {
            self.refresh_events.lock().unwrap().push(event);
        }
    }
}
