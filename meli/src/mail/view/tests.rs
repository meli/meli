//
// meli
//
// Copyright 2023 Manos Pitsidianakis
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

use rusty_fork::rusty_fork_test;
use smallvec::SmallVec;

use crate::{
    melib::{Attachment, AttachmentBuilder},
    view::{ViewFilter, ViewFilterContent, ViewSettings},
    Context,
};

rusty_fork_test! {
#[test]
fn test_view_filter_text_plain() {
    let bytes = b"Content-Transfer-Encoding: 8bit
Content-Type: text/plain; charset=utf-8

foobar
";
    let settings = ViewSettings::default();
    let tempdir = tempfile::tempdir().unwrap();
    let ctx = Context::new_mock(&tempdir);
    let att: Attachment = AttachmentBuilder::new(bytes).build();
    let value = ViewFilter::new_attachment(&att, SmallVec::new(), &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/plain");
}

#[test]
fn test_view_filter_text_html() {
    let bytes = b"Content-Transfer-Encoding: 8bit
Content-Type: text/html

foobar
";
    let settings = ViewSettings::default();
    let tempdir = tempfile::tempdir().unwrap();
    let ctx = Context::new_mock(&tempdir);
    let att: Attachment = AttachmentBuilder::new(bytes).build();
    let value = ViewFilter::new_attachment(&att, SmallVec::new(), &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/html");
}

#[test]
fn test_view_filter_multipart_alternative_plain_and_html() {
    let bytes = b"Content-Transfer-Encoding: 8bit
Content-Type: multipart/alternative; boundary=\"0000000000000000000000000000\"

--0000000000000000000000000000
Content-Type: text/plain; charset=\"UTF-8\"
Content-Transfer-Encoding: 8bit

plain foobar

--0000000000000000000000000000
Content-Type: text/html; charset=\"UTF-8\"
Content-Transfer-Encoding: 8bit

html foobar
";
    let settings = ViewSettings {
        auto_choose_multipart_alternative: true,
        ..ViewSettings::default()
    };

    let tempdir = tempfile::tempdir().unwrap();
    let ctx = Context::new_mock(&tempdir);
    let att: Attachment = AttachmentBuilder::new(bytes).build();
    let value = ViewFilter::new_attachment(&att, SmallVec::new(), &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/plain");
}

#[test]
fn test_view_filter_multipart_alternative_empty_plain_and_html() {
    let bytes = b"Content-Transfer-Encoding: 8bit
Content-Type: multipart/alternative; boundary=\"0000000000000000000000000000\"

--0000000000000000000000000000
Content-Type: text/plain; charset=\"UTF-8\"
Content-Transfer-Encoding: 8bit

--0000000000000000000000000000
Content-Type: text/html; charset=\"UTF-8\"
Content-Transfer-Encoding: 8bit

html foobar
";
    let mut settings = ViewSettings {
        auto_choose_multipart_alternative: true,
        ..ViewSettings::default()
    };

    let tempdir = tempfile::tempdir().unwrap();
    let ctx = Context::new_mock(&tempdir);
    let att: Attachment = AttachmentBuilder::new(bytes).build();
    let value = ViewFilter::new_attachment(&att, SmallVec::new(), &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/html");

    settings.auto_choose_multipart_alternative = false;

    let value = ViewFilter::new_attachment(&att, SmallVec::new(), &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/plain");
}

#[test]
fn test_view_filter_multipart_relative_cids() {
    const RFC2392: &str = r#"Date: Sat, 01 Aug 1998 02:00:00 +0200
From: foo1@bar.net
To: foo2@bar.net
Subject: A simple example
Mime-Version: 1.0
Message-ID: <rfc2392-section2@example.com>
Content-Type: multipart/related; boundary="boundary-example-1";
 type=Text/HTML

--boundary-example-1
Content-Type: Text/HTML; charset=US-ASCII

to the other body part, for example through a statement such as:
<IMG SRC="cid:foo4*foo1@bar.net" ALT="IETF logo">

--boundary-example-1
Content-ID: <foo4*foo1@bar.net>
Content-Type: IMAGE/GIF; name=ietf-logo.gif
Content-Transfer-Encoding: BASE64

R0lGODlhKgAXAPcAACEeHiUjIzAtLTUzMzg1NTw5OUZEREhGRkxJSV5WQ1RSUmVjY2lmZmpoaHNx
cYd6V76mXbqlZr6oZbekabmlaraldLqoc7ane7aofMiybsu2eM25eoSCgouKipaUlJiXl7aqh7Wq
ibWsk760lb61mKWkpKmoqLKuorWxp7WzrLq2rLa1tb27tbe3ube4u7q7vcOzhMy8i8m9l8C+uNTE
k9bHmNjJnMPAuNzTt9/Wury8wb/AxMLCxMjHxsvJwsXGycfIy8rKzdLRzNjVy83O0c/Q09LT1NjX
19bW2dfY2trb3Obh0uXi2N/f4d/g4uPj5O3q4ebm6Ofo6ejo6Ojo6unp6+nq6+rq7Orr7Ovs7e3s
7Ozs7e3t7e7t7e3t7u7u7u7u7+rq6vDt4vDw8PDw8fHx8fHx8vLy8vLy8/Pz8/X08fT08/f28vPz
9PT09PT09fX19fX19vX29vb29/f39/T09Pj39fj49vn49/f4+Pj4+fr5+Pr6+fn6+vr6+vv7+/v7
/Pz8/Pz8/f39/f39/v7+/v7//////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAAAAAAALAAAAAAqABcA
AAj/AA8JHEhQ4BoiWwhuIbKGYBMkBSNKnIjkxY+Eh9b80EEkj8AnO3QomUhSYkUdOn6s0YiSI44h
PFoiyWGn5ECMCpG0DCJlSxCUL0BAoJACZYsKEGoQXINzoJMdIwci4fFEZ5AnQYJECfIiBFGvK1xU
uLDCgo2BRXjg5OFDRYoUQsSs0amDqhCsKK+SIKojKIWxL3SUtQHFx1sWPEYWeEBhwgQLG2C4aKki
A4mWOkhomIFZRgzML2JsoEB6QgIOh5Qo0dHixZM1NS5MRhEhxAQRKEVEuBFlo0glSJ7weKE2Y8UV
iXFu+fFEIB4aGE5EwJ3itu6iRJ4AUdIkZBInFwkqywnSsGB55zQg4EaZIoKEon0ZPsGs5PxA+yTX
LEnSkggTJkTIdMhyKO3QnE0I8keeQT9BJNAWMR2IIIJN2LdGEwVtIeGEHHbo4YcghijiiCSah999
W6Q4EVMpnmhQixmt4UADLppggAILbDgQBwYcYMACTe14IwdBlLCGAge46AEAHbzgYhAdBNCAkxIt
IEAJQSjBw5EIKAnAAQ64eEgQAXxAEgMBIOCBQQp0KZEHAXjgg5g+xEnSAgP4gBGXXg5wwBET1bnm
RAsUgFNAADs=

--boundary-example-1--
"#;
    let settings = ViewSettings::default();

    let tempdir = tempfile::tempdir().unwrap();
    let mut ctx = Context::new_mock(&tempdir);
    ctx.settings.pager.html_open = Some("echo".into());

    let att: Attachment = AttachmentBuilder::new(RFC2392.as_bytes()).build();
    let value = ViewFilter::new_attachment(&att, SmallVec::new(), &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "multipart/related");
    let ViewFilterContent::InlineAttachments { parts, .. } = &value.body_text else {
        panic!(
            "expected ViewFilterContent::InlineAttachments got: {:?}",
            value.body_text
        );
    };
    assert_eq!(parts.len(), 2, "expected two parts: {parts:?}");
    parts[0].launch_html_open(&mut ctx).unwrap();
    assert_eq!(
        ctx.temp_files.len(),
        2,
        "expected two temp_files: {:?}",
        ctx.temp_files
    );
    assert_eq!(
        ctx.children.len(),
        1,
        "expected one fork child: {:?}",
        ctx.children
    );
    assert!(
        &ctx.children.contains_key("echo"),
        "expected html_open in children: {:?}",
        ctx.children
    );
    let html_path = ctx
        .temp_files
        .iter()
        .find_map(|f| {
            if f.path().extension().unwrap() == "html" {
                Some(f.path())
            } else {
                None
            }
        })
        .unwrap();
    let gif_path = ctx
        .temp_files
        .iter()
        .find_map(|f| {
            if f.path().extension().unwrap() == "gif" {
                Some(f.path())
            } else {
                None
            }
        })
        .unwrap();

    let rendered_html = std::fs::read_to_string(html_path).unwrap();
    assert_eq!(
        &rendered_html,
        &format!(
            "to the other body part, for example through a statement such as:\n<IMG SRC=\"{}\" \
             ALT=\"IETF logo\">\n",
            gif_path.display()
        )
    );
    drop(ctx);
    _ = tempdir.close();
}
}
