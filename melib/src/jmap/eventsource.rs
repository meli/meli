//
// meli
//
// Copyright 2021,2026 - Manos Pitsidianakis <manos@pitsidianak.is>
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
    sync::{Arc, LazyLock},
    time::Duration,
};

use async_fn_stream::try_fn_stream;
use futures::{io::AsyncReadExt, stream::Stream};
use isahc::{config::RedirectPolicy, http, http::request::Request, prelude::*, HttpClient};
use regex::Regex;
use url::Url;

use crate::{
    error::Result,
    jmap::{url_template::event_source_request_format, JmapClient, JmapServerConf, Store},
};

/// A single Server-Sent Event.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Event {
    /// Corresponds to the `id` field.
    pub id: Option<String>,
    /// Corresponds to the `event` field.
    pub event_type: Option<String>,
    /// Number of `data` blocks
    pub data_blocks: usize,
    /// All `data` fields concatenated by newlines.
    pub data: String,
}

/// Possible results from parsing a single event-stream line.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseResult {
    /// Line parsed successfully, but the event is not complete yet.
    Next,
    /// The event is complete now. Pass a new (empty) event for the next call.
    Dispatch,
    /// Set retry time.
    SetRetry(Duration),
    Comment(String),
}

static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?:\r\n)|(?:\n)|(?:\r)").unwrap());

pub fn parse_event_line(line: &str, event: &mut Event) -> Option<(ParseResult, usize)> {
    let ending = RE.find_iter(line).next()?;

    let line = &line[..ending.start()];
    let len = line.len() + ending.len();
    if line.trim_end_matches(['\r', '\n']).is_empty() {
        Some((ParseResult::Dispatch, len))
    } else {
        if line.starts_with(':') {
            return Some((ParseResult::Comment(line.into()), len));
        }
        let (field, value) = if let Some((f, v)) = line.split_once(':') {
            // Strip optional space.
            let v = v.strip_prefix(' ').unwrap_or(v);
            (f, v)
        } else {
            (line, "")
        };
        match field {
            "event" => {
                event.event_type = Some(value.to_string());
            }
            "data" => {
                if event.data_blocks > 0 {
                    event.data.push('\n');
                }
                event.data.push_str(value);
                event.data_blocks += 1;
            }
            "id" => {
                event.id = Some(value.to_string());
            }
            "retry" => {
                if let Ok(retry) = value.parse::<u64>() {
                    return Some((ParseResult::SetRetry(Duration::from_millis(retry)), len));
                }
            }
            _ => (), // ignored
        }

        Some((ParseResult::Next, len))
    }
}

#[test]
fn test_eventsource() {
    let input = ": new event source connection\r\n\r\nevent: state\r\ndata: \
                 {\"@type\":\"StateChange\",\"changed\":{\"u9971407b\":{\"Email\":\"J63441\",\"\
                 AddressBook\":\"38150\",\"ContactCard\":\"38150\",\"Thread\":\"J63441\",\"\
                 EmailDelivery\":\"J63441\",\"Mailbox\":\"J63441\",\"Identity\":\"S1\"}},\"type\":\
                 \"connect\"}\r\n\r\n";
    let mut event = Event::new();
    let (result, len) = parse_event_line(input, &mut event).unwrap();
    assert_eq!(
        result,
        ParseResult::Comment(": new event source connection".to_string())
    );
    let mut i = len;
    assert_eq!(len, ": new event source connection\r\n".len());
    let (result, len) = parse_event_line(&input[i..], &mut event).unwrap();
    i += len;
    assert_eq!(result, ParseResult::Dispatch);
    assert_eq!(len, "\r\n".len());
    assert!(event.is_empty());
    let (result, len) = parse_event_line(&input[i..], &mut event).unwrap();
    i += len;
    assert_eq!(result, ParseResult::Next);
    assert_eq!(len, "event: state\r\n".len());
    let (result, len) = parse_event_line(&input[i..], &mut event).unwrap();
    i += len;
    assert_eq!(result, ParseResult::Next);
    let data_line = "data: {\"@type\":\"StateChange\",\"changed\":{\"u9971407b\":{\"Email\":\"\
                     J63441\",\"AddressBook\":\"38150\",\"ContactCard\":\"38150\",\"Thread\":\"\
                     J63441\",\"EmailDelivery\":\"J63441\",\"Mailbox\":\"J63441\",\"Identity\":\"\
                     S1\"}},\"type\":\"connect\"}\r\n";
    assert_eq!(len, data_line.len());
    let (result, len) = parse_event_line(&input[i..], &mut event).unwrap();
    i += len;
    assert_eq!(result, ParseResult::Dispatch);
    assert_eq!(len, "\r\n".len());
    assert!(!event.is_empty());
    let expected_data = r#"{"@type":"StateChange","changed":{"u9971407b":{"Email":"J63441","AddressBook":"38150","ContactCard":"38150","Thread":"J63441","EmailDelivery":"J63441","Mailbox":"J63441","Identity":"S1"}},"type":"connect"}"#;
    assert_eq!(
        Event {
            id: None,
            event_type: Some("state".into()),
            data: expected_data.into(),
            data_blocks: 1,
        },
        event
    );
    assert_eq!(&input[i..], "");
}

impl Event {
    /// Creates an empty event.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns `true` if the event is empty.
    ///
    /// An event is empty if it has no id or event type and its data field is empty.
    pub fn is_empty(&self) -> bool {
        self.id.is_none() && self.event_type.is_none() && self.data.trim().is_empty()
    }

    /// Makes the event empty.
    pub fn clear(&mut self) {
        self.id = None;
        self.event_type = None;
        self.data.clear();
        self.data_blocks = 0;
    }
}

/// A client for a Server-Sent Events endpoint.
///
/// Read events by iterating over the client.
pub struct EventSourceConnection {
    pub http_client: Arc<HttpClient>,
    pub store: Arc<Store>,
    pub server_conf: JmapServerConf,
    url: Url,
    last_event_id: Option<String>,
    pub retry: Option<Duration>,
}

impl EventSourceConnection {
    pub async fn new(server_conf: JmapServerConf, store: Arc<Store>) -> Result<Self> {
        let client = JmapClient::new(&server_conf, &store).await?;
        let url = {
            let g = client.session_guard().await?;
            g.event_source_url.clone()
        };
        let url = event_source_request_format(&url, "*", "no", "300")?;

        let JmapClient { http_client, .. } = client;

        Ok(Self {
            http_client,
            server_conf,
            store,
            url,
            last_event_id: None,
            retry: None,
        })
    }

    pub fn next_event(&mut self) -> impl Stream<Item = Result<Event>> + use<'_> {
        let mut start = 0;
        let mut cursor = 0;
        let mut line = vec![0; 1024];
        try_fn_stream(|emitter| async move {
            'main_loop: loop {
                if let Some(dur) = self.retry.take() {
                    crate::utils::futures::sleep(dur).await;
                }

                let mut request = Request::get(self.url.as_str())
                    .header(http::header::ACCEPT, "text/event-stream")
                    .header(http::header::CACHE_CONTROL, "no-cache")
                    .ssl_options(if self.server_conf.danger_accept_invalid_certs {
                        isahc::config::SslOption::DANGER_ACCEPT_INVALID_CERTS
                            | isahc::config::SslOption::DANGER_ACCEPT_INVALID_HOSTS
                            | isahc::config::SslOption::DANGER_ACCEPT_REVOKED_CERTS
                    } else {
                        isahc::config::SslOption::NONE
                    })
                    .redirect_policy(RedirectPolicy::Limit(10));
                let username = self
                    .server_conf
                    .username
                    .value_with_timeout(
                        self.server_conf
                            .timeout
                            .unwrap_or(Duration::from_millis(100)),
                    )
                    .await?;
                let password = self
                    .server_conf
                    .password
                    .value_with_timeout(
                        self.server_conf
                            .timeout
                            .unwrap_or(Duration::from_millis(100)),
                    )
                    .await?;
                request = if self.server_conf.use_token {
                    request
                        .authentication(isahc::auth::Authentication::none())
                        .header(http::header::AUTHORIZATION, format!("Bearer {password}"))
                } else {
                    request
                        .authentication(isahc::auth::Authentication::basic())
                        .credentials(isahc::auth::Credentials::new(&username, &password))
                };
                if let Some(ref id) = self.last_event_id {
                    request = request.header("Last-Event-ID", id.as_str());
                }
                let request = request.body(()).map_err(|err| err.to_string())?;

                let mut response = self.http_client.send_async(request).await?;
                // Check status code and Content-Type.
                {
                    let status = response.status();
                    if !status.is_success() {
                        let res_text = response.text().await?;
                        return Err(format!("{} {}", status.as_str(), res_text).into());
                    }

                    if let Some(content_type_hv) =
                        response.headers().get(isahc::http::header::CONTENT_TYPE)
                    {
                        if !content_type_hv
                            .to_str()
                            .unwrap()
                            .starts_with("text/event-stream")
                        {
                            log::error!(
                                "got content-type: {}",
                                String::from_utf8_lossy(content_type_hv.as_ref())
                            );
                            return Err(format!(
                                "expectected Content-Type text/event-stream, got: {:?}",
                                String::from_utf8_lossy(content_type_hv.as_ref())
                            )
                            .into());
                        }
                    }
                }
                let mut reader = response.into_body();
                let mut event = Event::new();
                loop {
                    match reader.read(&mut line[cursor..]).await {
                        Ok(0) => {
                            return Ok(());
                        }
                        // Got new bytes from stream
                        Ok(n) => {
                            cursor += n;
                            while let Some((parse_result, len)) =
                                std::str::from_utf8(&line[start..cursor])
                                    .ok()
                                    .and_then(|s| parse_event_line(s, &mut event))
                            {
                                log::trace!(
                                    "read line {}",
                                    String::from_utf8_lossy(&line[start..cursor])
                                );
                                match parse_result {
                                    ParseResult::Comment(comment) => {
                                        log::trace!("comment: {comment:?}");
                                        start += len;
                                    }
                                    ParseResult::Next => {
                                        // okay, just continue
                                        start += len;
                                    }
                                    ParseResult::Dispatch => {
                                        if !event.is_empty() {
                                            if let Some(ref id) = event.id {
                                                self.last_event_id = Some(id.clone());
                                            }
                                            emitter.emit(std::mem::take(&mut event)).await;
                                        }
                                        start += len;
                                    }
                                    ParseResult::SetRetry(retry) => {
                                        self.retry = Some(retry);
                                        start += len;
                                        continue 'main_loop;
                                    }
                                }
                                if start >= cursor {
                                    start = 0;
                                    cursor = 0;
                                    break;
                                }
                            }
                        }
                        Err(err) => {
                            return Err(err.into());
                        }
                    }
                }
            }
        })
    }
}
