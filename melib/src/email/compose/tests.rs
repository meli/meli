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

use std::str::FromStr;

use super::*;

#[test]
fn test_email_compose_new_draft() {
    let mut default = Draft::default();
    assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
    default.set_body("αδφαφσαφασ".to_string());
    assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
    default.set_body("ascii only".to_string());
    assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
}

#[test]
fn test_email_compose_draft_update() {
    let mut default = Draft::default();
    default
        .set_wrap_header_preamble(Some(("<!--".to_string(), "-->".to_string())))
        .set_body("αδφαφσαφασ".to_string())
        .set_header(HeaderName::SUBJECT, "test_update()".into())
        .set_header(HeaderName::DATE, "Sun, 16 Jun 2013 17:56:45 +0200".into());

    let original = default.clone();
    let s = default.to_edit_string();
    assert_eq!(
        s,
        "<!--\nDate: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
         test_update()\n-->\n\nαδφαφσαφασ"
    );
    assert!(!default.update(&s).unwrap());
    assert_eq!(&original, &default);

    default.set_wrap_header_preamble(Some(("".to_string(), "".to_string())));
    let original = default.clone();
    let s = default.to_edit_string();
    assert_eq!(
        s,
        "Date: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
         test_update()\n\nαδφαφσαφασ"
    );
    assert!(!default.update(&s).unwrap());
    assert_eq!(&original, &default);

    default.set_wrap_header_preamble(None);
    let original = default.clone();
    let s = default.to_edit_string();
    assert_eq!(
        s,
        "Date: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
         test_update()\n\nαδφαφσαφασ"
    );
    assert!(!default.update(&s).unwrap());
    assert_eq!(&original, &default);

    default.set_wrap_header_preamble(Some((
        "{-\n\n\n===========".to_string(),
        "</mixed>".to_string(),
    )));
    let original = default.clone();
    let s = default.to_edit_string();
    assert_eq!(
        s,
        "{-\n\n\n===========\nDate: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \
         \nSubject: test_update()\n</mixed>\n\nαδφαφσαφασ"
    );
    assert!(!default.update(&s).unwrap());
    assert_eq!(&original, &default);

    default
        .set_body("hellohello<!--\n<!--\n<--hellohello\nhellohello-->\n-->\n-->hello\n".to_string())
        .set_wrap_header_preamble(Some(("<!--".to_string(), "-->".to_string())));
    let original = default.clone();
    let s = default.to_edit_string();
    #[rustfmt::skip]
        assert_eq!(
            s,
            "<!--\nDate: Sun, 16 Jun 2013 17:56:45 +0200\nFrom: \nTo: \nCc: \nBcc: \nSubject: \
             test_update()\n-->\n\nhellohello<!--\n<!--\n<--hellohello\nhellohello-->\n-->\n-->hello\n"
        );
    assert!(!default.update(&s).unwrap());
    assert_eq!(&original, &default);
}

/*
#[test]
fn test_email_compose_attachments() {
    let mut default = Draft::default();
    default.set_body("αδφαφσαφασ".to_string());

    let mut file = std::fs::File::open("file path").unwrap();
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).unwrap();

    let mut attachment = AttachmentBuilder::new(b"");
    attachment
        .set_raw(contents)
        .set_content_type(ContentType::Other {
            name: Some("images.jpeg".to_string()),
            tag: b"image/jpeg".to_vec(),
        })
        .set_content_transfer_encoding(ContentTransferEncoding::Base64);
    default.attachments_mut().push(attachment);
    println!("{}", default.finalise().unwrap());
}
    */

#[test]
fn test_composer_mime_encode_header() {
    use crate::email::compose::mime;
    let words = "compilers/2020a σε Rust";
    assert_eq!(
        "compilers/2020a =?UTF-8?B?z4POtSA=?=Rust",
        &mime::encode_header(words),
    );
    assert_eq!(
        &std::str::from_utf8(
            &crate::email::parser::encodings::phrase(mime::encode_header(words).as_bytes(), false)
                .unwrap()
                .1
        )
        .unwrap(),
        &words,
    );
    let words = "[internal] =?UTF-8?B?zp3Orc6/z4Igzp/OtM63zrPPjM+CIM6jz4U=?= \
                 =?UTF-8?B?zrPOs8+BzrHPhs6uz4I=?=";
    let words_enc = r#"[internal] Νέος Οδηγός Συγγραφής"#;
    assert_eq!(words, &mime::encode_header(words_enc),);
    assert_eq!(
        r#"[internal] Νέος Οδηγός Συγγραφής"#,
        std::str::from_utf8(
            &crate::email::parser::encodings::phrase(
                mime::encode_header(words_enc).as_bytes(),
                false
            )
            .unwrap()
            .1
        )
        .unwrap(),
    );
    //let words = "[Advcomparch]
    // =?utf-8?b?zqPPhc68z4DOtc+BzrnPhs6/z4HOrCDPg861IGZs?=\n\t=?utf-8?b?
    // dXNoIM67z4zOs8+JIG1pc3ByZWRpY3Rpb24gzrrOsc+Ezqwgz4TOt869?=\n\t=?utf-8?b?
    // IM61zrrPhM6tzrvOtc+Dzrcgc3RvcmU=?=";
    let words_enc = "[Advcomparch] Συμπεριφορά σε flush λόγω misprediction κατά την εκτέλεση store";
    assert_eq!(
        "[Advcomparch] Συμπεριφορά σε flush λόγω misprediction κατά την εκτέλεση store",
        std::str::from_utf8(
            &crate::email::parser::encodings::phrase(
                mime::encode_header(words_enc).as_bytes(),
                false
            )
            .unwrap()
            .1
        )
        .unwrap(),
    );
}

#[test]
fn test_composer_draft_from_str() {
    assert_eq!(
        Draft::from_str("").unwrap_err().to_string(),
        Error::new("Empty input in Draft::from_str")
            .set_kind(ErrorKind::ValueError)
            .to_string()
    );
    let err = Draft::from_str("safd ffsadfa asfd ").unwrap_err();
    assert_eq!(
        &err.summary,
        "Parsing error. In input: \"safd ffsadfa asfd ...\",\nError: Alternative, Many1, \
         Alternative, header_with_val(): invalid character: ' '. Could not parse mail. Could not \
         parse e-mail into a Draft"
    );
    assert_eq!(err.kind, ErrorKind::ValueError);
}
