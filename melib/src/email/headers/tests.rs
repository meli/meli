//
// melib
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use super::*;

#[test]
fn test_email_headers_names_headername_display() {
    assert_eq!(&HeaderName::SUBJECT.to_string(), "Subject");
    assert_eq!(&HeaderName::CC.to_string(), "Cc");
    assert_eq!(&HeaderName::IN_REPLY_TO.to_string(), "In-Reply-To");
    assert_eq!(
        &HeaderName::ORIGINAL_MESSAGE_ID.to_string(),
        "Original-Message-ID"
    );
    assert_eq!(
        &HeaderName::try_from("x-user-agent").unwrap().to_string(),
        "X-User-Agent"
    );
    assert_eq!(
        &HeaderName::try_from("arc-foobar").unwrap().to_string(),
        "ARC-Foobar"
    );
    assert_eq!(
        &HeaderName::try_from("x-rss-feed").unwrap().to_string(),
        "X-RSS-Feed"
    );
    assert_eq!(
        &HeaderName::try_from("With-regards-to").unwrap().to_string(),
        "With-Regards-To"
    );
    assert_eq!(
        &HeaderName::try_from("in-response-to-id")
            .unwrap()
            .to_string(),
        "In-Response-To-ID"
    );
    assert_eq!(
        &HeaderName::try_from("something-dKim").unwrap().to_string(),
        "Something-DKIM"
    );
}

#[test]
fn test_email_headers_names_parse_standard_headers() {
    use super::super::standards::TEST_HEADERS;

    for &(std, name) in TEST_HEADERS {
        // Test lower case
        assert_eq!(
            HeaderName::from_bytes(name.to_ascii_lowercase().as_bytes()).unwrap(),
            HeaderName::from(std)
        );

        // Test upper case
        let upper = std::str::from_utf8(name.as_bytes())
            .expect("byte string constants are all utf-8")
            .to_uppercase();
        assert_eq!(
            HeaderName::from_bytes(upper.as_bytes()).unwrap(),
            HeaderName::from(std)
        );
    }
}

#[test]
fn test_headers_case_sensitivity() {
    let mut headers = HeaderMap::default();
    headers.insert("from".try_into().unwrap(), "Myself <a@b.c>".into());
    assert_eq!(&headers["From"], "Myself <a@b.c>");
    assert_eq!(&headers["From"], &headers["from"]);
    assert_eq!(&headers["fROm"], &headers["from"]);
    headers.get_mut("from").unwrap().pop();
    assert_eq!(&headers["From"], "Myself <a@b.c");
    headers.insert("frOM".try_into().unwrap(), "nada".into());
    assert_eq!(&headers["fROm"], "nada");
}

#[test]
fn test_headers_map_index() {
    let mut headers = HeaderMap::default();
    headers.insert(HeaderName::SUBJECT, "foobar".into());
    headers.insert(HeaderName::MESSAGE_ID, "foobar@examplecom".into());
    assert_eq!(&headers[0], "foobar");
    assert_eq!(&headers[HeaderName::SUBJECT], "foobar");
    assert_eq!(&headers[&HeaderName::SUBJECT], "foobar");
    assert_eq!(&headers["subject"], "foobar");
    assert_eq!(&headers["Subject"], "foobar");
    assert_eq!(&headers[b"Subject".as_slice()], "foobar");
    assert!(&headers[HeaderName::MESSAGE_ID] != "foobar");
}
