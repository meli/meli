/*
 * meli
 *
 * Copyright 2017 Manos Pitsidianakis
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

use std::{borrow::Cow, fmt::Write as IoWrite};

use melib::{
    attachment_types::Charset, conf::ActionFlag, email::headers::HeaderName, error::*,
    pgp::DecryptionMetadata, Attachment, Result,
};

use crate::{
    conf::shortcuts::EnvelopeViewShortcuts,
    jobs::{JobId, JoinHandle},
    types::{Link, LinkKind},
    ShortcutMap, ThemeAttribute,
};

#[derive(Clone, Debug)]
pub struct ViewSettings {
    pub pager_filter: Option<String>,
    pub html_filter: Option<String>,
    pub url_launcher: Option<String>,
    pub expand_headers: bool,
    pub theme_default: ThemeAttribute,
    pub env_view_shortcuts: ShortcutMap,
    /// `"mail.view.body"`
    pub body_theme: ThemeAttribute,
    pub auto_choose_multipart_alternative: bool,
    pub sticky_headers: bool,
    pub show_date_in_my_timezone: bool,
    pub show_extra_headers: Vec<HeaderName>,
    pub auto_verify_signatures: ActionFlag,
    pub auto_decrypt: ActionFlag,
    pub charset: Option<Charset>,
}

impl Default for ViewSettings {
    fn default() -> Self {
        Self {
            theme_default: Default::default(),
            body_theme: Default::default(),
            pager_filter: None,
            html_filter: None,
            url_launcher: None,
            env_view_shortcuts: EnvelopeViewShortcuts::default().key_values(),
            auto_choose_multipart_alternative: true,
            expand_headers: false,
            sticky_headers: false,
            show_date_in_my_timezone: false,
            show_extra_headers: vec![],
            auto_verify_signatures: ActionFlag::InternalVal(true),
            auto_decrypt: ActionFlag::InternalVal(true),
            charset: None,
        }
    }
}

impl ViewSettings {
    /// Format a `Date` header value according to
    /// [`Self::show_date_in_my_timezone`] setting.
    pub fn format_date_value<'hdr>(
        &self,
        timestamp: melib::UnixTimestamp,
        val: &'hdr str,
    ) -> Cow<'hdr, str> {
        let find_offset = |s: &str| -> (bool, (i64, i64)) {
            let mut diff = (true, (0, 0));
            if let Some(pos) = s.as_bytes().iter().position(|b| *b == b'+' || *b == b'-') {
                let offset = &s[pos..];
                diff.0 = offset.starts_with('+');
                if let (Some(hr_offset), Some(min_offset)) = (
                    offset.get(1..3).and_then(|slice| slice.parse::<i64>().ok()),
                    offset.get(3..5).and_then(|slice| slice.parse::<i64>().ok()),
                ) {
                    diff.1 .0 = hr_offset;
                    diff.1 .1 = min_offset;
                }
            }
            diff
        };
        if self.show_date_in_my_timezone {
            use melib::utils::datetime;
            let local_date = datetime::timestamp_to_string(
                timestamp,
                Some(datetime::formats::RFC822_DATE),
                false,
            );
            let orig_offset = find_offset(val);
            let local_offset = find_offset(&local_date);
            if orig_offset == local_offset {
                val.into()
            } else {
                format!(
                    "{} [actual timezone: {}{:02}{:02}]",
                    local_date,
                    if orig_offset.0 { '+' } else { '-' },
                    orig_offset.1 .0,
                    orig_offset.1 .1
                )
                .into()
            }
        } else {
            val.into()
        }
    }

    /// Check if `header` should be displayed to the user or not.
    #[inline]
    fn filter_header(&self, header: &HeaderName) -> bool {
        match header {
            &HeaderName::DATE
            | &HeaderName::FROM
            | &HeaderName::TO
            | &HeaderName::SUBJECT
            | &HeaderName::CC
            | &HeaderName::MESSAGE_ID => true,
            &HeaderName::IN_REPLY_TO | &HeaderName::REFERENCES => self.expand_headers,
            other => self.show_extra_headers.contains(other),
        }
    }

    pub fn header_iter<'a, 'b>(
        &'a self,
        env: &'b melib::Envelope,
    ) -> ViewSettingsHeaderIterator<'a, 'b> {
        ViewSettingsHeaderIterator {
            index: 0,
            settings: self,
            env,
        }
    }
}

pub struct ViewSettingsHeaderIterator<'a, 'b> {
    index: usize,
    settings: &'a ViewSettings,
    env: &'b melib::Envelope,
}

impl<'a, 'b> Iterator for ViewSettingsHeaderIterator<'a, 'b> {
    type Item = (&'a HeaderName, Cow<'b, str>);

    fn next(&mut self) -> Option<Self::Item> {
        const HEADERS: &[HeaderName] = &[
            HeaderName::DATE,
            HeaderName::FROM,
            HeaderName::TO,
            HeaderName::SUBJECT,
            HeaderName::CC,
            HeaderName::MESSAGE_ID,
            HeaderName::IN_REPLY_TO,
            HeaderName::REFERENCES,
        ];
        fn get_header<'env>(
            envelope: &'env melib::Envelope,
            view_settings: &ViewSettings,
            hdr: &HeaderName,
        ) -> Option<Cow<'env, str>> {
            Some(match hdr {
                &HeaderName::DATE => {
                    view_settings.format_date_value(envelope.timestamp, envelope.date_as_str())
                }
                &HeaderName::FROM => envelope.field_from_to_string().into(),
                &HeaderName::TO => envelope.field_to_to_string().into(),
                &HeaderName::CC => {
                    if envelope.other_headers().contains_key(HeaderName::CC)
                        && !envelope.other_headers()[HeaderName::CC].is_empty()
                    {
                        envelope.field_cc_to_string().into()
                    } else {
                        return None;
                    }
                }
                &HeaderName::SUBJECT => envelope.subject(),
                &HeaderName::MESSAGE_ID => {
                    envelope.message_id().display_brackets().to_string().into()
                }
                &HeaderName::IN_REPLY_TO => {
                    let val = envelope.in_reply_to()?;
                    melib::MessageID::display_slice(val.refs(), Some(" ")).into()
                }
                &HeaderName::REFERENCES => {
                    _ = envelope.in_reply_to()?;
                    melib::MessageID::display_slice(envelope.references(), Some(" ")).into()
                }
                other
                    if envelope.other_headers().contains_key(other)
                        && !envelope.other_headers()[other].is_empty() =>
                {
                    envelope.other_headers().get(other)?.into()
                }
                _ => return None,
            })
        }
        while let Some(hdr) = HEADERS.get(self.index) {
            self.index += 1;
            if self.settings.filter_header(hdr) {
                if let Some(val) = get_header(self.env, self.settings, hdr) {
                    return Some((hdr, val));
                }
            }
        }
        None
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Source {
    Decoded,
    Raw,
}

bitflags::bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct ViewOptions: u8 {
        const DEFAULT           = 0;
        const URL               = 1;
        const SOURCE            = Self::URL.bits() << 1;
        const SOURCE_RAW        = Self::SOURCE.bits() << 1;
    }
}

impl Default for ViewOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl ViewOptions {
    pub fn convert(
        &self,
        links: &mut Vec<Link<'static>>,
        attachment: &melib::Attachment,
        text: &str,
    ) -> String {
        let mut text = if self.contains(Self::SOURCE) {
            if self.contains(Self::SOURCE_RAW) {
                String::from_utf8_lossy(attachment.raw()).into_owned()
            } else {
                /* Decode each header value */
                let mut ret = String::new();
                match melib::email::parser::headers::headers(attachment.raw()).map(|(_, v)| v) {
                    Ok(headers) => {
                        for (h, v) in headers {
                            _ = match melib::email::parser::encodings::phrase(v, true) {
                                Ok((_, v)) => ret.write_fmt(format_args!(
                                    "{h}: {}\n",
                                    String::from_utf8_lossy(&v)
                                )),
                                Err(err) => ret.write_fmt(format_args!("{h}: {err}\n")),
                            };
                        }
                    }
                    Err(err) => {
                        _ = write!(&mut ret, "{err}");
                    }
                }
                if !ret.ends_with("\n\n") {
                    ret.push_str("\n\n");
                }
                ret.push_str(text);
                ret
            }
        } else {
            text.to_string()
        };

        while text.ends_with("\n\n") {
            text.pop();
            text.pop();
        }

        if self.contains(Self::URL) {
            if links.is_empty() {
                let finder = linkify::LinkFinder::new();
                *links = finder
                    .links(&text)
                    .filter_map(|l| {
                        Some(Link {
                            start: l.start(),
                            end: l.end(),
                            value: l.as_str().to_string().into(),
                            kind: match l.kind() {
                                linkify::LinkKind::Url => LinkKind::Url,
                                linkify::LinkKind::Email => LinkKind::Email,
                                _ => return None,
                            },
                        })
                    })
                    .collect::<Vec<Link>>();
            }
            for (lidx, l) in links.iter().enumerate().rev() {
                let mut start = l.start;
                while start < text.len() && !text.is_char_boundary(start) {
                    start += 1;
                }
                if start < text.len() {
                    text.insert_str(start, &format!("[{lidx}]"));
                }
            }
        }

        text
    }
}

#[derive(Debug)]
pub enum AttachmentDisplay {
    Alternative {
        inner: Box<Attachment>,
        shown_display: usize,
        display: Vec<Self>,
    },
    InlineText {
        inner: Box<Attachment>,
        comment: Option<String>,
        text: String,
    },
    InlineOther {
        inner: Box<Attachment>,
    },
    Attachment {
        inner: Box<Attachment>,
    },
    SignedPending {
        inner: Box<Attachment>,
        display: Vec<Self>,
        handle: JoinHandle<Result<()>>,
        job_id: JobId,
    },
    SignedFailed {
        inner: Box<Attachment>,
        display: Vec<Self>,
        error: Error,
    },
    SignedUnverified {
        inner: Box<Attachment>,
        display: Vec<Self>,
    },
    SignedVerified {
        inner: Box<Attachment>,
        display: Vec<Self>,
        description: String,
    },
    EncryptedPending {
        inner: Box<Attachment>,
        handle: JoinHandle<Result<(DecryptionMetadata, Vec<u8>)>>,
    },
    EncryptedFailed {
        inner: Box<Attachment>,
        error: Error,
    },
    EncryptedSuccess {
        inner: Box<Attachment>,
        plaintext: Box<Attachment>,
        plaintext_display: Vec<Self>,
        description: String,
    },
}
