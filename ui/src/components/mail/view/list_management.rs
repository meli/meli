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
use melib::parser;
use melib::Envelope;
use melib::StackVec;
use std::convert::From;

#[derive(Debug, Copy)]
pub enum UnsubscribeOption<'a> {
    Url(&'a [u8]),
    Email(&'a [u8]),
}

impl<'a> From<&'a [u8]> for UnsubscribeOption<'a> {
    fn from(value: &'a [u8]) -> Self {
        if value.starts_with(b"mailto:") {
            /* if branch looks if value looks like a mailto url but doesn't validate it.
             * parser::mailto() will handle this if user tries to unsubscribe.
             */
            UnsubscribeOption::Email(value)
        } else {
            /* Otherwise treat it as url. There's no foolproof way to check if this is valid, so
             * postpone it until we try an HTTP request.
             */
            UnsubscribeOption::Url(value)
        }
    }
}

/* Required for StackVec's place holder elements, never actually used */
impl<'a> Default for UnsubscribeOption<'a> {
    fn default() -> Self {
        UnsubscribeOption::Email(b"")
    }
}

impl<'a> Clone for UnsubscribeOption<'a> {
    fn clone(&self) -> Self {
        match self {
            UnsubscribeOption::Url(a) => UnsubscribeOption::Url(<&[u8]>::clone(a)),
            UnsubscribeOption::Email(a) => UnsubscribeOption::Email(<&[u8]>::clone(a)),
        }
    }
}

#[derive(Default, Debug)]
pub struct ListActions<'a> {
    pub id: Option<&'a str>,
    pub archive: Option<&'a str>,
    pub post: Option<&'a str>,
    pub unsubscribe: Option<StackVec<UnsubscribeOption<'a>>>,
}

pub fn detect<'a>(envelope: &'a Envelope) -> Option<ListActions<'a>> {
    let mut ret = ListActions::default();

    if let Some(id) = envelope.other_headers().get("List-ID") {
        ret.id = Some(id);
    } else if let Some(id) = envelope.other_headers().get("List-Id") {
        ret.id = Some(id);
    }

    if let Some(archive) = envelope.other_headers().get("List-Archive") {
        ret.archive = Some(archive);
    }

    if let Some(post) = envelope.other_headers().get("List-Post") {
        ret.post = Some(post);
    }

    if let Some(unsubscribe) = envelope.other_headers().get("List-Unsubscribe") {
        ret.unsubscribe = parser::angle_bracket_delimeted_list(unsubscribe.as_bytes())
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
                    .map(|elem| UnsubscribeOption::from(elem))
                    .collect::<StackVec<UnsubscribeOption<'a>>>()
            })
            .to_full_result()
            .ok();
    }

    if ret.id.is_none() && ret.archive.is_none() && ret.post.is_none() && ret.unsubscribe.is_none()
    {
        None
    } else {
        Some(ret)
    }
}
