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

use melib::{attachment_types::Charset, pgp::DecryptionMetadata, Attachment, Error, Result};

use crate::{
    conf::shortcuts::EnvelopeViewShortcuts,
    jobs::{JobId, JoinHandle},
    ShortcutMap, ThemeAttribute, UIDialog,
};

#[derive(Debug, Clone)]
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
    pub auto_verify_signatures: bool,
    pub auto_decrypt: bool,
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
            auto_verify_signatures: true,
            auto_decrypt: true,
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum LinkKind {
    Url,
    Email,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Link {
    pub start: usize,
    pub end: usize,
    pub kind: self::LinkKind,
}

#[derive(Debug, Default)]
pub enum ForceCharset {
    #[default]
    None,
    Dialog(Box<UIDialog<Option<Charset>>>),
    Forced(Charset),
}

impl Into<Option<Charset>> for &ForceCharset {
    fn into(self) -> Option<Charset> {
        match self {
            ForceCharset::Forced(val) => Some(*val),
            ForceCharset::None | ForceCharset::Dialog(_) => None,
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Source {
    Decoded,
    Raw,
}

#[derive(PartialEq, Debug, Default)]
pub enum ViewMode {
    #[default]
    Normal,
    Url,
    Attachment(usize),
    Source(Source),
    Subview,
}

macro_rules! is_variant {
    ($n:ident, $($var:tt)+) => {
        #[inline]
        pub fn $n(&self) -> bool {
            matches!(self, Self::$($var)*)
        }
    };
}

impl ViewMode {
    is_variant! { is_normal, Normal }
    is_variant! { is_url, Url }
    is_variant! { is_attachment, Attachment(_) }
    is_variant! { is_source, Source(_) }
    is_variant! { is_subview, Subview }
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
