/*
 * meli - parser module
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

//! Parsing of `mailto` addresses.
//!
//! Conforming to [RFC6068](https://www.rfc-editor.org/rfc/rfc6068) which obsoletes
//! [RFC2368](https://www.rfc-editor.org/rfc/rfc2368).

use std::convert::TryFrom;

use super::*;
use crate::{
    email::headers::HeaderMap,
    utils::percent_encoding::{AsciiSet, CONTROLS},
};

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailto {
    pub address: Vec<Address>,
    pub body: Option<String>,
    pub headers: HeaderMap,
}

impl Mailto {
    pub const IGNORE_HEADERS: &'static [HeaderName] = &[
        HeaderName::FROM,
        HeaderName::DATE,
        HeaderName::MESSAGE_ID,
        HeaderName::APPARENTLY_TO,
        HeaderName::ARC_AUTHENTICATION_RESULTS,
        HeaderName::ARC_MESSAGE_SIGNATURE,
        HeaderName::ARC_SEAL,
        HeaderName::AUTHENTICATION_RESULTS,
        HeaderName::AUTOFORWARDED,
        HeaderName::AUTO_SUBMITTED,
        HeaderName::AUTOSUBMITTED,
        HeaderName::BASE,
        HeaderName::CONTENT_ALTERNATIVE,
        HeaderName::CONTENT_BASE,
        HeaderName::CONTENT_DESCRIPTION,
        HeaderName::CONTENT_DISPOSITION,
        HeaderName::CONTENT_DURATION,
        HeaderName::CONTENT_FEATURES,
        HeaderName::CONTENT_ID,
        HeaderName::CONTENT_IDENTIFIER,
        HeaderName::CONTENT_LANGUAGE,
        HeaderName::CONTENT_LENGTH,
        HeaderName::CONTENT_LOCATION,
        HeaderName::CONTENT_MD5,
        HeaderName::CONTENT_RETURN,
        HeaderName::CONTENT_TRANSFER_ENCODING,
        HeaderName::CONTENT_TRANSLATION_TYPE,
        HeaderName::CONTENT_TYPE,
        HeaderName::DELIVERED_TO,
        HeaderName::DKIM_SIGNATURE,
        HeaderName::ENCRYPTED,
        HeaderName::FORWARDED,
        HeaderName::MAIL_FOLLOWUP_TO,
        HeaderName::MAIL_REPLY_TO,
        HeaderName::MIME_VERSION,
        HeaderName::ORIGINAL_ENCODED_INFORMATION_TYPES,
        HeaderName::ORIGINAL_FROM,
        HeaderName::ORIGINAL_MESSAGE_ID,
        HeaderName::ORIGINAL_RECIPIENT,
        HeaderName::ORIGINAL_SUBJECT,
        HeaderName::ORIGINATOR_RETURN_ADDRESS,
        HeaderName::RECEIVED,
        HeaderName::RECEIVED_SPF,
        HeaderName::RESENT_BCC,
        HeaderName::RESENT_CC,
        HeaderName::RESENT_DATE,
        HeaderName::RESENT_FROM,
        HeaderName::RESENT_MESSAGE_ID,
        HeaderName::RESENT_REPLY_TO,
        HeaderName::RESENT_SENDER,
        HeaderName::RESENT_TO,
        HeaderName::RETURN_PATH,
        HeaderName::SENDER,
        HeaderName::USER_AGENT,
    ];

    pub const MAILTO_CHARSET: &'static AsciiSet = &CONTROLS
        .add(b' ')
        .add(b'"')
        .add(b'"')
        .add(b'#')
        .add(b'%')
        .add(b'/')
        .add(b'<')
        .add(b'>')
        .add(b'?')
        .add(b'`')
        .add(b'{')
        .add(b'}');
}

impl From<Mailto> for Draft {
    fn from(val: Mailto) -> Self {
        let mut ret = Self::default();
        let Mailto {
            address: _,
            body,
            headers,
        } = val;
        for (hdr, val) in headers.into_inner() {
            ret.set_header(hdr, val);
        }
        ret.set_body(body.unwrap_or_default());
        ret
    }
}

impl From<&Mailto> for Draft {
    fn from(val: &Mailto) -> Self {
        Self::from(val.clone())
    }
}

impl TryFrom<&[u8]> for Mailto {
    type Error = String;

    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        super::parser::generic::mailto(value)
            .map(|(_, v)| v)
            .map_err(|err| {
                log::debug!(
                    "parser::mailto returned error while parsing {}:\n{:?}",
                    String::from_utf8_lossy(value),
                    &err,
                );
                format!("{err:?}")
            })
    }
}

impl TryFrom<&str> for Mailto {
    type Error = String;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        super::parser::generic::mailto(value.as_bytes())
            .map(|(_, v)| v)
            .map_err(|err| {
                log::debug!(
                    "parser::mailto returned error while parsing {}:\n{:?}",
                    value,
                    &err
                );
                format!("{err:?}")
            })
    }
}
