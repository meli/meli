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

#[derive(Debug)]
pub struct JmapConnection {
    pub request_no: Arc<Mutex<usize>>,
    pub client: Arc<Mutex<Client>>,
    pub online_status: Arc<Mutex<bool>>,
    pub server_conf: JmapServerConf,
}

impl JmapConnection {
    pub fn new(server_conf: &JmapServerConf, online_status: Arc<Mutex<bool>>) -> Result<Self> {
        use reqwest::header;
        let mut headers = header::HeaderMap::new();
        headers.insert(
            header::AUTHORIZATION,
            header::HeaderValue::from_static("fc32dffe-14e7-11ea-a277-2477037a1804"),
        );
        headers.insert(
            header::ACCEPT,
            header::HeaderValue::from_static("application/json"),
        );
        headers.insert(
            header::CONTENT_TYPE,
            header::HeaderValue::from_static("application/json"),
        );
        let client = reqwest::blocking::ClientBuilder::new()
            .danger_accept_invalid_certs(server_conf.danger_accept_invalid_certs)
            .default_headers(headers)
            .build()?;

        let res_text = client.get(&server_conf.server_hostname).send()?.text()?;
        debug!(&res_text);

        let server_conf = server_conf.clone();
        Ok(JmapConnection {
            request_no: Arc::new(Mutex::new(0)),
            client: Arc::new(Mutex::new(client)),
            online_status,
            server_conf,
        })
    }
}
