/*
 * meli - attachments module
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

//! Attachment encoding and decoding.

use std::str;

use data_encoding::BASE64_MIME;
use smallvec::SmallVec;

use crate::{
    email::{
        address::StrBuilder,
        attachment_types::*,
        parser::{self, BytesExt},
        HeaderName, Mail,
    },
    BytesDisplay,
};

/// Type alias for function that takes an [`Attachment`] and appends a bytes
/// representation in its second argument.
pub type Filter<'a> = Box<dyn FnMut(&Attachment, &mut Vec<u8>) + 'a>;

#[derive(Default)]
/// Options for decoding an [`Attachment`].
pub struct DecodeOptions<'att> {
    /// [`Filter`] to use.
    pub filter: Option<Filter<'att>>,
    /// Override the attachment's [`Charset`], if any, with a user-provided
    /// value.
    pub force_charset: Option<Charset>,
}

impl From<Option<Charset>> for DecodeOptions<'_> {
    fn from(force_charset: Option<Charset>) -> Self {
        Self {
            filter: None,
            force_charset,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
/// A struct analogous to [`Attachment`] which can have incomplete and partial
/// content before being turned into an [`Attachment`] with
/// [`AttachmentBuilder::build`].
pub struct AttachmentBuilder {
    pub content_type: ContentType,
    pub content_transfer_encoding: ContentTransferEncoding,
    pub content_disposition: ContentDisposition,

    pub raw: Vec<u8>,
    pub body: StrBuilder,
}

impl AttachmentBuilder {
    pub fn new(content: &[u8]) -> Self {
        let (headers, body) = match parser::attachments::attachment(content) {
            Ok((_, v)) => v,
            Err(err) => {
                if !content.trim().is_empty() {
                    log::debug!("error in parsing attachment: {}", err);
                    log::debug!("\n-------------------------------");
                    log::debug!("{}\n", String::from_utf8_lossy(content));
                    log::debug!("-------------------------------\n");
                }

                return Self {
                    content_type: Default::default(),
                    content_transfer_encoding: ContentTransferEncoding::_7Bit,
                    content_disposition: ContentDisposition::default(),
                    raw: content.to_vec(),
                    body: StrBuilder {
                        length: content.len(),
                        offset: 0,
                    },
                };
            }
        };

        let raw = content.into();
        let body = StrBuilder {
            offset: content.len() - body.len(),
            length: body.len(),
        };
        let mut builder = Self {
            raw,
            body,
            ..Default::default()
        };
        for (name, value) in headers {
            if name == HeaderName::CONTENT_TYPE {
                builder.set_content_type_from_bytes(value);
            } else if name == HeaderName::CONTENT_TRANSFER_ENCODING {
                builder.set_content_transfer_encoding(ContentTransferEncoding::from(value));
            } else if name == HeaderName::CONTENT_DISPOSITION {
                builder.set_content_disposition(ContentDisposition::from(value));
            }
        }
        builder
    }

    pub fn new_plaintext(content: Vec<u8>) -> Self {
        let mut ret = Self::new(&[]);
        ret.set_raw(content).set_body_to_raw();

        ret
    }

    pub fn raw(&self) -> &[u8] {
        &self.raw
    }

    pub fn body(&self) -> &[u8] {
        self.body.display_bytes(&self.raw)
    }

    fn set_raw_inner(&mut self, raw: Vec<u8>) -> &mut Self {
        self.raw = raw;
        self
    }

    pub fn set_raw<B: Into<Vec<u8>>>(&mut self, raw: B) -> &mut Self {
        let raw: Vec<u8> = raw.into();
        self.set_raw_inner(raw)
    }

    /// Set body to the entire raw contents, use this if raw contains only data
    /// and no headers If raw contains data and headers pass it through
    /// [`AttachmentBuilder::new`].
    pub fn set_body_to_raw(&mut self) -> &mut Self {
        self.body = StrBuilder {
            offset: 0,
            length: self.raw.len(),
        };
        self
    }

    pub fn set_content_type(&mut self, val: ContentType) -> &mut Self {
        self.content_type = val;
        self
    }

    pub fn content_type(&self) -> &ContentType {
        &self.content_type
    }

    pub fn set_content_transfer_encoding(&mut self, val: ContentTransferEncoding) -> &mut Self {
        self.content_transfer_encoding = val;
        self
    }

    pub fn set_content_disposition(&mut self, val: ContentDisposition) -> &mut Self {
        self.content_disposition = val;
        self
    }

    pub fn content_disposition(&self) -> &ContentDisposition {
        &self.content_disposition
    }

    pub fn content_transfer_encoding(&self) -> &ContentTransferEncoding {
        &self.content_transfer_encoding
    }

    pub fn set_content_type_from_bytes(&mut self, value: &[u8]) -> &mut Self {
        match parser::attachments::content_type(value) {
            Ok((_, (ct, cst, params))) => {
                if ct.eq_ignore_ascii_case(b"multipart") {
                    let mut boundary = None;
                    for (n, v) in &params {
                        if n.eq_ignore_ascii_case(b"boundary") {
                            boundary = Some(v);
                            break;
                        }
                    }
                    if let Some(boundary) = boundary {
                        let parts = Self::parts(self.body(), boundary);

                        let boundary = boundary.to_vec();
                        self.content_type = ContentType::Multipart {
                            boundary,
                            kind: MultipartType::from(cst),
                            parameters: params
                                .into_iter()
                                .map(|(kb, vb)| (kb.to_vec(), vb.to_vec()))
                                .collect::<Vec<(Vec<u8>, Vec<u8>)>>(),
                            parts,
                        };
                    } else {
                        self.content_type = ContentType::default();
                        return self;
                    }
                } else if ct.eq_ignore_ascii_case(b"text") {
                    self.content_type = ContentType::default();
                    for (n, v) in params {
                        if n.eq_ignore_ascii_case(b"charset") {
                            if let ContentType::Text {
                                charset: ref mut c, ..
                            } = self.content_type
                            {
                                *c = Charset::from(v);
                            }
                        }
                        if let ContentType::Text {
                            parameters: ref mut p,
                            ..
                        } = self.content_type
                        {
                            p.push((n.to_vec(), v.to_vec()));
                        }
                    }
                    if cst.eq_ignore_ascii_case(b"html") {
                        if let ContentType::Text {
                            kind: ref mut k, ..
                        } = self.content_type
                        {
                            *k = Text::Html;
                        }
                    } else if !cst.eq_ignore_ascii_case(b"plain") {
                        if let ContentType::Text {
                            kind: ref mut k, ..
                        } = self.content_type
                        {
                            *k = Text::Other { tag: cst.into() };
                        }
                    }
                } else if ct.eq_ignore_ascii_case(b"message") && cst.eq_ignore_ascii_case(b"rfc822")
                {
                    self.content_type = ContentType::MessageRfc822;
                } else if ct.eq_ignore_ascii_case(b"application")
                    && cst.eq_ignore_ascii_case(b"pgp-signature")
                {
                    self.content_type = ContentType::PGPSignature;
                } else if ct.eq_ignore_ascii_case(b"application")
                    && cst.eq_ignore_ascii_case(b"pkcs7-signature")
                {
                    self.content_type = ContentType::CMSSignature;
                } else {
                    let mut name: Option<String> = None;
                    for (n, v) in &params {
                        if n.eq_ignore_ascii_case(b"name") {
                            if let Ok(v) = crate::email::parser::encodings::phrase(v.trim(), false)
                                .as_ref()
                                .map(|(_, r)| String::from_utf8_lossy(r).to_string())
                            {
                                name = Some(v);
                            } else {
                                name = Some(String::from_utf8_lossy(v).into());
                            }
                            break;
                        }
                    }
                    let mut tag: Vec<u8> = Vec::with_capacity(ct.len() + cst.len() + 1);
                    tag.extend(ct);
                    tag.push(b'/');
                    tag.extend(cst);
                    self.content_type = ContentType::Other {
                        tag,
                        name,
                        parameters: params
                            .into_iter()
                            .map(|(kb, vb)| (kb.to_vec(), vb.to_vec()))
                            .collect::<Vec<(Vec<u8>, Vec<u8>)>>(),
                    };
                }
            }
            Err(err) => {
                log::debug!(
                    "parsing error in content_type: {:?} {}",
                    String::from_utf8_lossy(value),
                    err
                );
            }
        }
        self
    }

    pub fn build(self) -> Attachment {
        Attachment {
            content_type: self.content_type,
            content_transfer_encoding: self.content_transfer_encoding,
            content_disposition: self.content_disposition,
            raw: self.raw,
            body: self.body,
        }
    }

    pub fn parts(raw: &[u8], boundary: &[u8]) -> Vec<Attachment> {
        if raw.is_empty() {
            return Vec::new();
        }

        match parser::attachments::parts(raw, boundary) {
            Ok((_, attachments)) => {
                let mut vec = Vec::with_capacity(attachments.len());
                for a in attachments {
                    let mut builder = Self::default();
                    let (headers, body) = match parser::attachments::attachment(a) {
                        Ok((_, v)) => v,
                        Err(err) => {
                            log::debug!("error in parsing attachment: {}", err);
                            log::debug!("\n-------------------------------");
                            log::debug!("{}\n", String::from_utf8_lossy(a));
                            log::debug!("-------------------------------\n");

                            continue;
                        }
                    };

                    builder.raw = a.into();
                    builder.body = StrBuilder {
                        offset: a.len() - body.len(),
                        length: body.len(),
                    };
                    for (name, value) in headers {
                        if name == HeaderName::CONTENT_TYPE {
                            builder.set_content_type_from_bytes(value);
                        } else if name == HeaderName::CONTENT_TRANSFER_ENCODING {
                            builder.set_content_transfer_encoding(ContentTransferEncoding::from(
                                value,
                            ));
                        } else if name == HeaderName::CONTENT_DISPOSITION {
                            builder.set_content_disposition(ContentDisposition::from(value));
                        }
                    }
                    vec.push(builder.build());
                }
                vec
            }
            a => {
                log::debug!(
                    "error {:?}\n\traw: {:?}\n\tboundary: {:?}",
                    a,
                    str::from_utf8(raw).unwrap(),
                    boundary
                );
                Vec::new()
            }
        }
    }
}

impl From<Attachment> for AttachmentBuilder {
    fn from(val: Attachment) -> Self {
        let Attachment {
            content_type,
            content_disposition,
            content_transfer_encoding,
            raw,
            body,
        } = val;
        Self {
            content_type,
            content_disposition,
            content_transfer_encoding,
            raw,
            body,
        }
    }
}

impl From<AttachmentBuilder> for Attachment {
    fn from(val: AttachmentBuilder) -> Self {
        let AttachmentBuilder {
            content_type,
            content_transfer_encoding,
            content_disposition,
            raw,
            body,
        } = val;
        Self {
            content_type,
            content_transfer_encoding,
            content_disposition,
            raw,
            body,
        }
    }
}

/// Immutable attachment type.
#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Attachment {
    pub content_type: ContentType,
    pub content_transfer_encoding: ContentTransferEncoding,
    pub content_disposition: ContentDisposition,

    pub raw: Vec<u8>,
    pub body: StrBuilder,
}

impl std::fmt::Debug for Attachment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut text = Vec::with_capacity(4096);
        self.get_text_recursive(&Text::Plain, &mut text);
        f.debug_struct(crate::identify!(Attachment))
            .field("Content-Type", &self.content_type)
            .field("Content-Transfer-Encoding", &self.content_transfer_encoding)
            .field("bytes", &self.raw.len())
            .field("body", &String::from_utf8_lossy(&text))
            .finish()
    }
}

impl std::fmt::Display for Attachment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.content_type {
            ContentType::MessageRfc822 => {
                match Mail::new(self.body.display_bytes(&self.raw).to_vec(), None) {
                    Ok(wrapper) => write!(
                        f,
                        "{} {} {} [message/rfc822] {}",
                        wrapper.subject(),
                        wrapper.field_from_to_string(),
                        wrapper.date_as_str(),
                        BytesDisplay(self.raw.len()),
                    ),
                    Err(err) => write!(
                        f,
                        "could not parse: {} [message/rfc822] {}",
                        err,
                        BytesDisplay(self.raw.len()),
                    ),
                }
            }
            ContentType::PGPSignature => write!(f, "pgp signature [{}]", self.mime_type()),
            ContentType::CMSSignature => write!(f, "S/MIME signature [{}]", self.mime_type()),
            ContentType::OctetStream { .. } | ContentType::Other { .. } => {
                if let Some(name) = self.filename() {
                    write!(
                        f,
                        "\"{}\", [{}] {}",
                        name,
                        self.mime_type(),
                        BytesDisplay(self.raw.len())
                    )
                } else {
                    write!(
                        f,
                        "Data attachment [{}] {}",
                        self.mime_type(),
                        BytesDisplay(self.raw.len())
                    )
                }
            }
            ContentType::Text { .. } => {
                if let Some(name) = self.filename() {
                    write!(
                        f,
                        "\"{}\", [{}] {}",
                        name,
                        self.mime_type(),
                        BytesDisplay(self.raw.len())
                    )
                } else {
                    write!(
                        f,
                        "Text attachment [{}] {}",
                        self.mime_type(),
                        BytesDisplay(self.raw.len())
                    )
                }
            }
            ContentType::Multipart {
                parts: ref sub_att_vec,
                ..
            } => write!(
                f,
                "{} attachment with {} parts",
                self.mime_type(),
                sub_att_vec.len()
            ),
        }
    }
}

impl Attachment {
    pub fn new(
        content_type: ContentType,
        content_transfer_encoding: ContentTransferEncoding,
        raw: Vec<u8>,
    ) -> Self {
        Self {
            content_type,
            content_disposition: ContentDisposition::default(),
            content_transfer_encoding,
            body: StrBuilder {
                length: raw.len(),
                offset: 0,
            },
            raw,
        }
    }

    pub fn raw(&self) -> &[u8] {
        &self.raw
    }

    pub fn body(&self) -> &[u8] {
        self.body.display_bytes(&self.raw)
    }

    pub fn part_boundaries(&self) -> Vec<StrBuilder> {
        if self.raw.is_empty() {
            return Vec::new();
        }

        match self.content_type {
            ContentType::Multipart { ref boundary, .. } => {
                match parser::attachments::multipart_parts(self.body(), boundary) {
                    Ok((_, v)) => v,
                    Err(e) => {
                        log::debug!("error in parsing attachment");
                        log::debug!("\n-------------------------------");
                        log::debug!("{}\n", String::from_utf8_lossy(&self.raw));
                        log::debug!("-------------------------------\n");
                        log::debug!("{:?}\n", e);
                        Vec::new()
                    }
                }
            }
            _ => Vec::new(),
        }
    }

    /* Call on the body of a multipart/mixed Envelope to check if there are
     * attachments without completely parsing them */
    pub fn check_if_has_attachments_quick(bytes: &[u8], boundary: &[u8]) -> bool {
        if bytes.is_empty() {
            return false;
        }

        match parser::attachments::multipart_parts(bytes, boundary) {
            Ok((_, parts)) => {
                for p in parts {
                    let (body, headers) = match parser::headers::headers_raw(p.display_bytes(bytes))
                    {
                        Ok(v) => v,
                        Err(_err) => return false,
                    };
                    let headers = crate::email::parser::generic::HeaderIterator(headers)
                        .collect::<SmallVec<[(HeaderName, &[u8]); 16]>>();
                    let disposition = headers
                        .iter()
                        .find(|(n, _)| n == HeaderName::CONTENT_DISPOSITION)
                        .map(|(_, v)| ContentDisposition::from(*v))
                        .unwrap_or_default();
                    if disposition.kind.is_attachment() {
                        return true;
                    }
                    if let Some(boundary) = headers
                        .iter()
                        .find(|(n, _)| n == HeaderName::CONTENT_TYPE)
                        .and_then(|(_, v)| {
                            if let Ok((_, (ct, _cst, params))) =
                                parser::attachments::content_type(v)
                            {
                                if ct.eq_ignore_ascii_case(b"multipart") {
                                    let mut boundary = None;
                                    for (n, v) in params {
                                        if n.eq_ignore_ascii_case(b"boundary") {
                                            boundary = Some(v);
                                            break;
                                        }
                                    }
                                    return boundary;
                                }
                            }
                            None
                        })
                    {
                        if Self::check_if_has_attachments_quick(body, boundary) {
                            return true;
                        }
                    }
                }
            }
            Err(e) => {
                log::debug!("error in parsing multipart_parts");
                log::debug!("\n-------------------------------");
                log::debug!("{}\n", String::from_utf8_lossy(bytes));
                log::debug!("-------------------------------\n");
                log::debug!("{:?}\n", e);
            }
        }
        false
    }

    fn get_text_recursive(&self, kind: &Text, text: &mut Vec<u8>) {
        match self.content_type {
            ContentType::Text { .. } | ContentType::PGPSignature | ContentType::CMSSignature => {
                text.extend(self.decode(Default::default()));
            }
            ContentType::Multipart {
                kind: MultipartType::Related,
                ref parts,
                ref parameters,
                ..
            } => {
                if let Some(main_attachment) = parameters
                    .iter()
                    .find_map(|(k, v)| if k == b"type" { Some(v) } else { None })
                    .and_then(|t| parts.iter().find(|a| a.content_type == t.as_slice()))
                {
                    main_attachment.get_text_recursive(kind, text);
                } else {
                    for a in parts {
                        if a.content_disposition.kind.is_inline()
                            && matches!(&a.content_type, ContentType::Text { kind: a_kind, .. } if a_kind == kind)
                        {
                            a.get_text_recursive(kind, text);
                            break;
                        }
                    }
                }
            }
            ContentType::Multipart {
                kind: MultipartType::Alternative,
                ref parts,
                ..
            } => {
                for a in parts {
                    if a.content_disposition.kind.is_inline()
                        && matches!(&a.content_type, ContentType::Text { kind: a_kind, .. } if a_kind == kind)
                    {
                        a.get_text_recursive(kind, text);
                        break;
                    }
                }
            }
            ContentType::Multipart {
                kind: _, ref parts, ..
            } => {
                for a in parts {
                    if a.content_disposition.kind.is_inline() {
                        a.get_text_recursive(kind, text);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn text(&self, kind: Text) -> String {
        let mut text = Vec::with_capacity(self.body.length);
        self.get_text_recursive(&kind, &mut text);
        String::from_utf8_lossy(text.as_slice()).into()
    }

    pub fn mime_type(&self) -> String {
        self.content_type.to_string()
    }

    pub fn attachments(&self) -> Vec<Self> {
        let mut ret = Vec::new();
        fn count_recursive(att: &Attachment, ret: &mut Vec<Attachment>) {
            match att.content_type {
                ContentType::Multipart {
                    parts: ref sub_att_vec,
                    ..
                } => {
                    ret.push(att.clone());
                    // [ref:FIXME]: Wrong count
                    for a in sub_att_vec {
                        count_recursive(a, ret);
                    }
                }
                _ => ret.push(att.clone()),
            }
        }

        count_recursive(self, &mut ret);
        ret
    }

    pub fn count_attachments(&self) -> usize {
        self.attachments().len()
    }

    pub fn content_type(&self) -> &ContentType {
        &self.content_type
    }

    pub fn content_transfer_encoding(&self) -> &ContentTransferEncoding {
        &self.content_transfer_encoding
    }

    pub fn is_text(&self) -> bool {
        matches!(self.content_type, ContentType::Text { .. })
    }

    pub fn is_html(&self) -> bool {
        match self.content_type {
            ContentType::Text {
                kind: Text::Html, ..
            } => true,
            ContentType::Text {
                kind: Text::Plain, ..
            } => false,
            ContentType::Multipart {
                kind: MultipartType::Digest,
                ..
            } => false,
            ContentType::Multipart {
                kind: MultipartType::Alternative,
                ref parts,
                ..
            } => parts.iter().all(Self::is_html),
            _ => false,
        }
    }

    pub fn is_encrypted(&self) -> bool {
        matches!(
            self.content_type,
            ContentType::Multipart {
                kind: MultipartType::Encrypted,
                ..
            }
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self.content_type,
            ContentType::Multipart {
                kind: MultipartType::Signed,
                ..
            }
        )
    }

    pub fn into_raw(&self) -> String {
        let mut ret = String::with_capacity(2 * self.raw.len());
        fn into_raw_helper(a: &Attachment, ret: &mut String) {
            ret.push_str(&format!(
                "Content-Transfer-Encoding: {}\r\n",
                a.content_transfer_encoding
            ));
            match &a.content_type {
                ContentType::Text {
                    kind: _,
                    parameters,
                    charset,
                } => {
                    ret.push_str(&format!(
                        "Content-Type: {}; charset={}",
                        a.content_type, charset
                    ));
                    for (n, v) in parameters {
                        ret.push_str("; ");
                        ret.push_str(&String::from_utf8_lossy(n));
                        ret.push('=');
                        if v.contains(&b' ') {
                            ret.push('"');
                        }
                        ret.push_str(&String::from_utf8_lossy(v));
                        if v.contains(&b' ') {
                            ret.push('"');
                        }
                    }

                    ret.push_str("\r\n\r\n");
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
                ContentType::Multipart {
                    boundary,
                    kind,
                    parts,
                    parameters,
                } => {
                    let boundary = String::from_utf8_lossy(boundary);
                    ret.push_str(&format!("Content-Type: {kind}; boundary={boundary}"));
                    if *kind == MultipartType::Signed {
                        ret.push_str("; micalg=pgp-sha512; protocol=\"application/pgp-signature\"");
                    }
                    for (n, v) in parameters {
                        ret.push_str("; ");
                        ret.push_str(&String::from_utf8_lossy(n));
                        ret.push('=');
                        if v.contains(&b' ') {
                            ret.push('"');
                        }
                        ret.push_str(&String::from_utf8_lossy(v));
                        if v.contains(&b' ') {
                            ret.push('"');
                        }
                    }
                    ret.push_str("\r\n");

                    let boundary_start = format!("\r\n--{boundary}\r\n");
                    for p in parts {
                        ret.push_str(&boundary_start);
                        into_raw_helper(p, ret);
                    }
                    if !ret.ends_with("\r\n") {
                        ret.push_str("\r\n");
                    }
                    ret.push_str(&format!("--{boundary}--\r\n\r\n"));
                }
                ContentType::MessageRfc822 => {
                    ret.push_str(&format!("Content-Type: {}\r\n\r\n", a.content_type));
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
                ContentType::CMSSignature | ContentType::PGPSignature => {
                    ret.push_str(&format!("Content-Type: {}\r\n\r\n", a.content_type));
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
                ContentType::OctetStream { name, parameters } => {
                    if let Some(name) = name {
                        ret.push_str(&format!("Content-Type: {}; name={}", a.content_type, name));
                    } else {
                        ret.push_str(&format!("Content-Type: {}", a.content_type));
                    }
                    for (n, v) in parameters {
                        ret.push_str("; ");
                        ret.push_str(&String::from_utf8_lossy(n));
                        ret.push('=');
                        if v.contains(&b' ') {
                            ret.push('"');
                        }
                        ret.push_str(&String::from_utf8_lossy(v));
                        if v.contains(&b' ') {
                            ret.push('"');
                        }
                    }
                    ret.push_str("\r\n\r\n");
                    ret.push_str(BASE64_MIME.encode(a.body()).trim());
                }
                _ => {
                    ret.push_str(&format!("Content-Type: {}\r\n\r\n", a.content_type));
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
            }
        }
        into_raw_helper(self, &mut ret);
        ret
    }

    pub fn parameters(&self) -> Vec<(&[u8], &[u8])> {
        let mut ret = Vec::new();
        let (headers, _) = match parser::attachments::attachment(&self.raw) {
            Ok((_, v)) => v,
            Err(_) => return ret,
        };
        for (name, value) in headers {
            if name == HeaderName::CONTENT_TYPE {
                if let Ok((_, (_, _, params))) = parser::attachments::content_type(value) {
                    ret = params;
                }
                break;
            }
        }

        ret
    }

    pub fn filename(&self) -> Option<String> {
        if self.content_disposition.kind.is_attachment() {
            self.content_disposition.filename.clone()
        } else {
            None
        }
        .or_else(|| match &self.content_type {
            ContentType::Text { parameters, .. } => parameters
                .iter()
                .find(|(h, _)| {
                    h.eq_ignore_ascii_case(b"name") | h.eq_ignore_ascii_case(b"filename")
                })
                .map(|(_, v)| String::from_utf8_lossy(v).to_string()),
            ContentType::Other { .. } | ContentType::OctetStream { .. } => {
                self.content_type.name().map(|s| s.to_string())
            }
            _ => None,
        })
        .map(|s| {
            crate::email::parser::encodings::phrase(s.as_bytes(), false)
                .map(|(_, v)| v)
                .ok()
                .and_then(|n| String::from_utf8(n).ok())
                .unwrap_or(s)
        })
        .map(|n| n.replace(|c| std::path::is_separator(c) || c.is_ascii_control(), "_"))
    }

    fn decode_rec_helper(&self, options: &mut DecodeOptions<'_>) -> Vec<u8> {
        match self.content_type {
            ContentType::Other { .. } => Vec::new(),
            ContentType::Text { .. } => self.decode_helper(options),
            ContentType::OctetStream {
                ref name,
                parameters: _,
            } => name
                .clone()
                .unwrap_or_else(|| self.mime_type())
                .into_bytes(),
            ContentType::CMSSignature | ContentType::PGPSignature => Vec::new(),
            ContentType::MessageRfc822 => {
                if self.content_disposition.kind.is_inline() {
                    AttachmentBuilder::new(self.body())
                        .build()
                        .decode_rec_helper(options)
                } else {
                    b"message/rfc822 attachment".to_vec()
                }
            }
            ContentType::Multipart {
                ref kind,
                ref parts,
                parameters: _,
                ..
            } => match kind {
                MultipartType::Alternative => {
                    for a in parts {
                        if let ContentType::Text {
                            kind: Text::Plain, ..
                        } = a.content_type
                        {
                            return a.decode_helper(options);
                        }
                    }
                    self.decode_helper(options)
                }
                MultipartType::Signed => {
                    let mut vec = Vec::new();
                    for a in parts {
                        vec.extend(a.decode_rec_helper(options));
                    }
                    vec.extend(self.decode_helper(options));
                    vec
                }
                MultipartType::Encrypted => {
                    let mut vec = Vec::new();
                    for a in parts {
                        if a.content_type == "application/octet-stream" {
                            vec.extend(a.decode_rec_helper(options));
                        }
                    }
                    vec.extend(self.decode_helper(options));
                    vec
                }
                _ => {
                    let mut vec = Vec::new();
                    for a in parts {
                        if a.content_disposition.kind.is_inline() {
                            vec.extend(a.decode_rec_helper(options));
                        }
                    }
                    vec
                }
            },
        }
    }

    pub fn decode_rec(&self, mut options: DecodeOptions<'_>) -> Vec<u8> {
        self.decode_rec_helper(&mut options)
    }

    fn decode_helper(&self, options: &mut DecodeOptions<'_>) -> Vec<u8> {
        let charset = options
            .force_charset
            .unwrap_or_else(|| match self.content_type {
                ContentType::Text { charset, .. } => charset,
                _ => Default::default(),
            });

        let bytes = match self.content_transfer_encoding {
            ContentTransferEncoding::Base64 => match BASE64_MIME.decode(self.body()) {
                Ok(v) => v,
                _ => self.body().to_vec(),
            },
            ContentTransferEncoding::QuotedPrintable => {
                parser::encodings::quoted_printable_bytes(self.body())
                    .unwrap()
                    .1
            }
            ContentTransferEncoding::_7Bit
            | ContentTransferEncoding::_8Bit
            | ContentTransferEncoding::Other { .. } => self.body().to_vec(),
        };

        let mut ret = if self.content_type.is_text() {
            if let Ok(v) = parser::encodings::decode_charset(&bytes, charset) {
                v.into_bytes()
            } else {
                self.body().to_vec()
            }
        } else {
            bytes.to_vec()
        };

        if let Some(filter) = options.filter.as_mut() {
            filter(self, &mut ret);
        }

        ret
    }

    pub fn decode(&self, mut options: DecodeOptions<'_>) -> Vec<u8> {
        self.decode_helper(&mut options)
    }
}
