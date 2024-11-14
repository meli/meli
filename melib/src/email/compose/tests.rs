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

const GREEK_FOOBAR: &str = "αδφαφσαφασ";

#[test]
fn test_email_compose_new_draft() {
    let mut default = Draft::default();
    assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
    default.set_body(GREEK_FOOBAR.to_string());
    assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
    default.set_body("ascii only".to_string());
    assert_eq!(Draft::from_str(&default.to_edit_string()).unwrap(), default);
}

#[test]
fn test_email_compose_draft_update() {
    let mut default = Draft::default();
    default
        .set_wrap_header_preamble(Some(("<!--".to_string(), "-->".to_string())))
        .set_body(GREEK_FOOBAR.to_string())
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

#[test]
fn test_email_compose_attachments() {
    let mut default = Draft::default();
    default.set_body(GREEK_FOOBAR.to_string()).set_header(
        HeaderName::DATE,
        "Thu, 14 Nov 2024 19:26:59 +0200".to_string(),
    );

    // sourced from
    // <https://stackoverflow.com/questions/2253404/what-is-the-smallest-valid-jpeg-file-size-in-bytes/2349470#2349470>
    const EMPTY_JPEG: &[u8] = &[
        0xff, 0xd8, 0xff, 0xe0, 0x00, 0x10, 0x4a, 0x46, 0x49, 0x46, 0x00, 0x01, 0x01, 0x01, 0x00,
        0x48, 0x00, 0x48, 0x00, 0x00, 0xff, 0xdb, 0x00, 0x43, 0x00, 0x03, 0x02, 0x02, 0x02, 0x02,
        0x02, 0x03, 0x02, 0x02, 0x02, 0x03, 0x03, 0x03, 0x03, 0x04, 0x06, 0x04, 0x04, 0x04, 0x04,
        0x04, 0x08, 0x06, 0x06, 0x05, 0x06, 0x09, 0x08, 0x0a, 0x0a, 0x09, 0x08, 0x09, 0x09, 0x0a,
        0x0c, 0x0f, 0x0c, 0x0a, 0x0b, 0x0e, 0x0b, 0x09, 0x09, 0x0d, 0x11, 0x0d, 0x0e, 0x0f, 0x10,
        0x10, 0x11, 0x10, 0x0a, 0x0c, 0x12, 0x13, 0x12, 0x10, 0x13, 0x0f, 0x10, 0x10, 0x10, 0xff,
        0xc9, 0x00, 0x0b, 0x08, 0x00, 0x01, 0x00, 0x01, 0x01, 0x01, 0x11, 0x00, 0xff, 0xcc, 0x00,
        0x06, 0x00, 0x10, 0x10, 0x05, 0xff, 0xda, 0x00, 0x08, 0x01, 0x01, 0x00, 0x00, 0x3f, 0x00,
        0xd2, 0xcf, 0x20, 0xff, 0xd9,
    ];

    let mut attachment = AttachmentBuilder::new(b"");
    attachment
        .set_raw(EMPTY_JPEG)
        .set_content_type(ContentType::Other {
            name: Some("images.jpeg".to_string()),
            tag: b"image/jpeg".to_vec(),
            parameters: vec![],
        })
        .set_content_transfer_encoding(ContentTransferEncoding::Base64);
    default.attachments_mut().push(attachment);

    let bytes = default.finalise().unwrap();
    let boundary_def = bytes.find("bzz_bzz__bzz__").unwrap();
    let boundary_end = boundary_def + bytes[boundary_def..].find('\"').unwrap();
    let boundary = bytes[boundary_def..boundary_end].to_string();
    let boundary_str = &boundary["bzz_bzz__bzz__".len()..];

    let bytes = bytes.replace(boundary_str, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

    assert_eq!(
        bytes.as_str(),
        "Date: Thu, 14 Nov 2024 19:26:59 +0200\x0d
From: \x0d
To: \x0d
Cc: \x0d
Bcc: \x0d
Subject: \x0d
MIME-Version: 1.0\x0d
Content-Type: multipart/mixed; charset=\"utf-8\"; \
         boundary=\"bzz_bzz__bzz__xxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"\x0d
\x0d
This is a MIME formatted message with attachments. Use a MIME-compliant client to view it \
         properly.\x0d
--bzz_bzz__bzz__xxxxxxxxxxxxxxxxxxxxxxxxxxxxx\x0d
Content-Type: text/plain; charset=\"utf-8\"\x0d
\x0d
αδφαφσαφασ\x0d
--bzz_bzz__bzz__xxxxxxxxxxxxxxxxxxxxxxxxxxxxx\x0d
Content-Type: image/jpeg; name=\"images.jpeg\"; charset=\"utf-8\"\x0d
Content-Disposition: attachment\x0d
Content-Transfer-Encoding: base64\x0d
\x0d
/9j/4AAQSkZJRgABAQEASABIAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgKCgkI\x0d
CQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/yQALCAABAAEBAREA/8wABgAQEAX/2gAI\x0d
AQEAAD8A0s8g/9k=\x0d
--bzz_bzz__bzz__xxxxxxxxxxxxxxxxxxxxxxxxxxxxx--\x0d
"
    );
    #[cfg(target_family = "unix")]
    {
        use std::process::{Command, Stdio};

        let Ok(find_python3) = Command::new("sh")
            .arg("-c")
            .arg("command -v python3")
            .stdout(Stdio::piped())
            .spawn()
        else {
            return;
        };
        let python3_bin_path = match find_python3.wait_with_output() {
            Ok(output) if output.status.success() => output.stdout,
            otherwise => {
                // Report failure, but do not fail the test itself.
                println!(
                    "Could not locate python3 binary with `sh -c 'command -v python3'` \
                     invocation: {otherwise:?}"
                );
                return;
            }
        };

        let mut child = Command::new(
            <std::ffi::OsStr as std::os::unix::ffi::OsStrExt>::from_bytes(
                python3_bin_path.trim_end(),
            ),
        )
        .arg("-c")
        .arg(
            r#"import sys
from email.parser import BytesParser
from email import policy

data = sys.stdin.buffer.read()
parser = BytesParser(policy=policy.strict)
msg = parser.parsebytes(data)
richest = msg.get_body()
partfiles = {}
if richest['content-type'].maintype == 'text' and richest['content-type'].subtype == 'plain':
    sys.stdout.write(richest.get_content())
else:
    print("Unexpected content-type: {}".format(richest.get_content_type()))
    "#,
        )
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .stdin(Stdio::piped())
        .spawn()
        .expect("Failed to start python3 process");
        let mut stdin = child
            .stdin
            .take()
            .expect("Failed to open python3 process stdin");
        std::thread::spawn(move || {
            use std::io::Write;

            stdin
                .write_all(bytes.as_bytes())
                .expect("Failed to write to python3 process stdin");
        });

        let output = child
            .wait_with_output()
            .expect("python3 process wasn't running");
        assert_eq!(&output.stdout, GREEK_FOOBAR.as_bytes());
    }
}

#[test]
fn test_email_compose_mime_encode_header() {
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
fn test_email_compose_draft_from_str() {
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
