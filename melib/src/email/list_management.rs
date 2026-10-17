/*
 * meli
 *
 * Copyright 2017-2019 Manos Pitsidianakis
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

//! Parsing of `RFC2369` and `RFC2919` `List-*` headers.

use super::{parser, Envelope};
use crate::HeaderName;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ListAction {
    Url(Vec<u8>),
    Email(Vec<u8>),
    ///`List-Post` field may contain the special value "NO".
    No,
}

impl From<&[u8]> for ListAction {
    fn from(value: &[u8]) -> Self {
        if value.starts_with(b"mailto:") {
            /* if branch looks if value looks like a mailto url but doesn't validate it.
             * parser::mailto() will handle this if user tries to unsubscribe.
             */
            Self::Email(value.to_vec())
        } else if value.starts_with(b"NO") {
            Self::No
        } else {
            /* Otherwise treat it as url. There's no foolproof way to check if this is
             * valid, so postpone it until we try an HTTP request.
             */
            Self::Url(value.to_vec())
        }
    }
}

impl ListAction {
    pub fn parse_options_list(input: &[u8]) -> Option<Vec<Self>> {
        parser::mailing_lists::rfc_2369_list_headers_action_list(input)
            .map(|(_, mut vec)| {
                /* Prefer email options first, since this _is_ a mail client after all and
                 * it's more automated */
                vec.sort_unstable_by(|a, b| {
                    match (a.starts_with(b"mailto:"), b.starts_with(b"mailto:")) {
                        (true, false) => std::cmp::Ordering::Less,
                        (false, true) => std::cmp::Ordering::Greater,
                        _ => std::cmp::Ordering::Equal,
                    }
                });

                vec.into_iter().map(Self::from).collect::<Vec<Self>>()
            })
            .ok()
            .filter(|l| !l.is_empty())
    }

    /// Returns `true` if `self` is [`Self::Url`].
    pub const fn is_url(&self) -> bool {
        matches!(self, Self::Url(_))
    }

    /// Returns `true` if `self` is [`Self::Email`].
    pub const fn is_email(&self) -> bool {
        matches!(self, Self::Email(_))
    }
}

#[derive(Debug, Default)]
pub struct ListActions {
    pub id: Option<String>,
    pub archive: Option<Vec<ListAction>>,
    pub post: Option<Vec<ListAction>>,
    pub unsubscribe: Option<Vec<ListAction>>,
    pub subscribe: Option<Vec<ListAction>>,
    pub owner: Option<Vec<ListAction>>,
    pub help: Option<Vec<ListAction>>,
}

pub fn list_id_header(envelope: &Envelope) -> Option<String> {
    envelope
        .other_headers()
        .get(HeaderName::LIST_ID)
        .map(|s| s.to_owned())
}

pub fn list_id(header: Option<&'_ str>) -> Option<&'_ str> {
    /* rfc2919 https://tools.ietf.org/html/rfc2919 */
    /* list-id-header = "List-ID:" [phrase] "<" list-id ">" CRLF */
    header.and_then(|v| {
        if let Some(l) = v.rfind('<') {
            if let Some(r) = v.rfind('>') {
                if l < r {
                    return Some(&v[l + 1..r]);
                }
            }
        }
        None
    })
}

impl ListActions {
    pub fn detect(envelope: &Envelope) -> Option<Self> {
        let mut ret = Self {
            id: list_id_header(envelope),
            ..Self::default()
        };

        if let Some(archive) = envelope.other_headers().get(HeaderName::LIST_ARCHIVE) {
            ret.archive = ListAction::parse_options_list(archive.as_bytes());
        }

        if let Some(post) = envelope.other_headers().get(HeaderName::LIST_POST) {
            ret.post = ListAction::parse_options_list(post.as_bytes());
        }

        if let Some(unsubscribe) = envelope.other_headers().get(HeaderName::LIST_UNSUBSCRIBE) {
            ret.unsubscribe = ListAction::parse_options_list(unsubscribe.as_bytes());
        }

        if let Some(subscribe) = envelope.other_headers().get(HeaderName::LIST_SUBSCRIBE) {
            ret.subscribe = ListAction::parse_options_list(subscribe.as_bytes());
        }

        if let Some(help) = envelope.other_headers().get(HeaderName::LIST_HELP) {
            ret.help = ListAction::parse_options_list(help.as_bytes());
        }

        if let Some(owner) = envelope.other_headers().get(HeaderName::LIST_OWNER) {
            ret.owner = ListAction::parse_options_list(owner.as_bytes());
        }

        if ret.id.is_none()
            && ret.archive.is_none()
            && ret.post.is_none()
            && ret.unsubscribe.is_none()
            && ret.subscribe.is_none()
            && ret.help.is_none()
            && ret.owner.is_none()
        {
            None
        } else {
            Some(ret)
        }
    }
}
