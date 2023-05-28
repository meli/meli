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

/*! Parsing of `mailto` addresses */
use std::convert::TryFrom;

use super::*;

#[derive(Debug, Clone)]
pub struct Mailto {
    pub address: Address,
    pub subject: Option<String>,
    pub cc: Option<String>,
    pub bcc: Option<String>,
    pub body: Option<String>,
}

impl From<Mailto> for Draft {
    fn from(val: Mailto) -> Self {
        let mut ret = Draft::default();
        let Mailto {
            address,
            subject,
            cc,
            bcc,
            body,
        } = val;
        ret.set_header(HeaderName::SUBJECT, subject.unwrap_or_default());
        ret.set_header(HeaderName::CC, cc.unwrap_or_default());
        ret.set_header(HeaderName::BCC, bcc.unwrap_or_default());
        ret.set_header(HeaderName::TO, address.to_string());
        ret.set_body(body.unwrap_or_default());
        ret
    }
}

impl From<&Mailto> for Draft {
    fn from(val: &Mailto) -> Self {
        Draft::from(val.clone())
    }
}

impl TryFrom<&[u8]> for Mailto {
    type Error = String;

    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        let parse_res = super::parser::generic::mailto(value).map(|(_, v)| v);
        if let Ok(res) = parse_res {
            Ok(res)
        } else {
            debug!(
                "parser::mailto returned error while parsing {}:\n{:?}",
                String::from_utf8_lossy(value),
                parse_res.as_ref().err().unwrap()
            );
            Err(format!("{:?}", parse_res.err().unwrap()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mailto() {
        let test_address = super::parser::address::address(b"info@example.com")
            .map(|(_, v)| v)
            .unwrap();
        let mailto = Mailto::try_from(&b"mailto:info@example.com?subject=email%20subject"[0..])
            .expect("Could not parse mailto link.");
        let Mailto {
            ref address,
            ref subject,
            ref cc,
            ref bcc,
            ref body,
        } = mailto;

        assert_eq!(
            (
                address,
                subject.as_ref().map(String::as_str),
                cc.as_ref().map(String::as_str),
                bcc.as_ref().map(String::as_str),
                body.as_ref().map(String::as_str),
            ),
            (&test_address, Some("email subject"), None, None, None)
        );
        let mailto = Mailto::try_from(&b"mailto:info@example.com?cc=8cc9@example.com"[0..])
            .expect("Could not parse mailto link.");
        let Mailto {
            ref address,
            ref subject,
            ref cc,
            ref bcc,
            ref body,
        } = mailto;
        assert_eq!(
            (
                address,
                subject.as_ref().map(String::as_str),
                cc.as_ref().map(String::as_str),
                bcc.as_ref().map(String::as_str),
                body.as_ref().map(String::as_str),
            ),
            (&test_address, None, Some("8cc9@example.com"), None, None)
        );
        let mailto = Mailto::try_from(
            &b"mailto:info@example.com?bcc=7bcc8@example.com&body=line%20first%0Abut%20not%0Alast"
                [0..],
        )
        .expect("Could not parse mailto link.");
        let Mailto {
            ref address,
            ref subject,
            ref cc,
            ref bcc,
            ref body,
        } = mailto;
        assert_eq!(
            (
                address,
                subject.as_ref().map(String::as_str),
                cc.as_ref().map(String::as_str),
                bcc.as_ref().map(String::as_str),
                body.as_ref().map(String::as_str),
            ),
            (
                &test_address,
                None,
                None,
                Some("7bcc8@example.com"),
                Some("line first\nbut not\nlast")
            )
        );
    }
}
