/*
 * meli - ui crate.
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

use super::parser;
use super::Envelope;
use smallvec::SmallVec;
use std::convert::From;

#[derive(Debug, Copy)]
pub enum ListAction<'a> {
    Url(&'a [u8]),
    Email(&'a [u8]),
}

impl<'a> From<&'a [u8]> for ListAction<'a> {
    fn from(value: &'a [u8]) -> Self {
        if value.starts_with(b"mailto:") {
            /* if branch looks if value looks like a mailto url but doesn't validate it.
             * parser::mailto() will handle this if user tries to unsubscribe.
             */
            ListAction::Email(value)
        } else {
            /* Otherwise treat it as url. There's no foolproof way to check if this is valid, so
             * postpone it until we try an HTTP request.
             */
            ListAction::Url(value)
        }
    }
}

impl<'a> ListAction<'a> {
    pub fn parse_options_list(input: &'a [u8]) -> Option<SmallVec<[ListAction<'a>; 4]>> {
        parser::angle_bracket_delimeted_list(input)
            .map(|mut vec| {
                /* Prefer email options first, since this _is_ a mail client after all and it's
                 * more automated */
                vec.sort_unstable_by(|a, b| {
                    match (a.starts_with(b"mailto:"), b.starts_with(b"mailto:")) {
                        (true, false) => std::cmp::Ordering::Less,
                        (false, true) => std::cmp::Ordering::Greater,
                        _ => std::cmp::Ordering::Equal,
                    }
                });

                vec.into_iter()
                    .map(|elem| ListAction::from(elem))
                    .collect::<SmallVec<[ListAction<'a>; 4]>>()
            })
            .to_full_result()
            .ok()
    }
}

impl<'a> Clone for ListAction<'a> {
    fn clone(&self) -> Self {
        match self {
            ListAction::Url(a) => ListAction::Url(<&[u8]>::clone(a)),
            ListAction::Email(a) => ListAction::Email(<&[u8]>::clone(a)),
        }
    }
}

#[derive(Default, Debug)]
pub struct ListActions<'a> {
    pub id: Option<&'a str>,
    pub archive: Option<&'a str>,
    pub post: Option<SmallVec<[ListAction<'a>; 4]>>,
    pub unsubscribe: Option<SmallVec<[ListAction<'a>; 4]>>,
}

pub fn list_id_header<'a>(envelope: &'a Envelope) -> Option<&'a str> {
    envelope
        .other_headers()
        .get("List-ID")
        .or(envelope.other_headers().get("List-Id"))
        .map(String::as_str)
}

pub fn list_id<'a>(header: Option<&'a str>) -> Option<&'a str> {
    /* rfc2919 https://tools.ietf.org/html/rfc2919 */
    /* list-id-header = "List-ID:" [phrase] "<" list-id ">" CRLF */
    header.and_then(|v| {
        if let Some(l) = v.rfind("<") {
            if let Some(r) = v.rfind(">") {
                if l < r {
                    return Some(&v[l + 1..r]);
                }
            }
        }
        None
    })
}

impl<'a> ListActions<'a> {
    pub fn detect(envelope: &'a Envelope) -> Option<ListActions<'a>> {
        let mut ret = ListActions::default();

        ret.id = list_id_header(envelope);

        if let Some(archive) = envelope.other_headers().get("List-Archive") {
            if archive.starts_with("<") {
                if let Some(pos) = archive.find(">") {
                    ret.archive = Some(&archive[1..pos]);
                } else {
                    ret.archive = Some(archive);
                }
            } else {
                ret.archive = Some(archive);
            }
        }

        if let Some(post) = envelope.other_headers().get("List-Post") {
            ret.post = ListAction::parse_options_list(post.as_bytes());
        }

        if let Some(unsubscribe) = envelope.other_headers().get("List-Unsubscribe") {
            ret.unsubscribe = ListAction::parse_options_list(unsubscribe.as_bytes());
        }

        if ret.id.is_none()
            && ret.archive.is_none()
            && ret.post.is_none()
            && ret.unsubscribe.is_none()
        {
            None
        } else {
            Some(ret)
        }
    }
}
