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

use crate::{
    melib::{Attachment, AttachmentBuilder},
    view::{ViewFilter, ViewSettings},
    Context,
};

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
    let value = ViewFilter::new_attachment(&att, &settings, &ctx).unwrap();
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
    let value = ViewFilter::new_attachment(&att, &settings, &ctx).unwrap();
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
    let value = ViewFilter::new_attachment(&att, &settings, &ctx).unwrap();
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
    let value = ViewFilter::new_attachment(&att, &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/html");

    settings.auto_choose_multipart_alternative = false;

    let value = ViewFilter::new_attachment(&att, &settings, &ctx).unwrap();
    assert_eq!(&value.content_type.to_string(), "text/plain");
}
