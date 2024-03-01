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

use std::fmt::Write as IoWrite;

use melib::{
    attachment_types::Charset, conf::ActionFlag, error::*, pgp::DecryptionMetadata, Attachment,
    Result,
};

use crate::{
    conf::shortcuts::EnvelopeViewShortcuts,
    jobs::{JobId, JoinHandle},
    ShortcutMap, ThemeAttribute, UIDialog,
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
    pub show_extra_headers: Vec<String>,
    pub auto_verify_signatures: ActionFlag,
    pub auto_decrypt: ActionFlag,
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
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LinkKind {
    Url,
    Email,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Link {
    pub start: usize,
    pub end: usize,
    pub value: String,
    pub kind: LinkKind,
}

#[derive(Debug, Default)]
pub enum ForceCharset {
    #[default]
    None,
    Dialog(Box<UIDialog<Option<Charset>>>),
    Forced(Charset),
}

impl From<&ForceCharset> for Option<Charset> {
    fn from(val: &ForceCharset) -> Self {
        match val {
            ForceCharset::Forced(val) => Some(*val),
            ForceCharset::None | ForceCharset::Dialog(_) => None,
        }
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
        links: &mut Vec<Link>,
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
                            value: l.as_str().to_string(),
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
                    text.insert_str(start, &format!("[{}]", lidx));
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
        display: Vec<AttachmentDisplay>,
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
        display: Vec<AttachmentDisplay>,
        handle: JoinHandle<Result<()>>,
        job_id: JobId,
    },
    SignedFailed {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
        error: Error,
    },
    SignedUnverified {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
    },
    SignedVerified {
        inner: Box<Attachment>,
        display: Vec<AttachmentDisplay>,
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
        plaintext_display: Vec<AttachmentDisplay>,
        description: String,
    },
}
