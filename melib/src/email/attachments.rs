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
use crate::email::address::StrBuilder;
use crate::email::parser;
use crate::email::parser::BytesExt;
use crate::email::EnvelopeWrapper;
use core::fmt;
use core::str;
use data_encoding::BASE64_MIME;

pub use crate::email::attachment_types::*;

#[derive(Default, Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
            Err(_) => {
                debug!("error in parsing attachment");
                debug!("\n-------------------------------");
                debug!("{}\n", ::std::string::String::from_utf8_lossy(content));
                debug!("-------------------------------\n");

                return AttachmentBuilder {
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
        let mut builder = AttachmentBuilder {
            raw,
            body,
            ..Default::default()
        };
        for (name, value) in headers {
            if name.eq_ignore_ascii_case(b"content-type") {
                builder.set_content_type_from_bytes(value);
            } else if name.eq_ignore_ascii_case(b"content-transfer-encoding") {
                builder.set_content_transfer_encoding(ContentTransferEncoding::from(value));
            } else if name.eq_ignore_ascii_case(b"content-disposition") {
                builder.set_content_disposition(ContentDisposition::from(value));
            }
        }
        builder
    }

    pub fn raw(&self) -> &[u8] {
        &self.raw
    }

    pub fn body(&self) -> &[u8] {
        self.body.display_bytes(&self.raw)
    }

    pub fn set_raw(&mut self, raw: Vec<u8>) -> &mut Self {
        self.raw = raw;
        self
    }

    /// Set body to the entire raw contents, use this if raw contains only data and no headers
    /// If raw contains data and headers pass it through AttachmentBuilder::new().
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
                    for (n, v) in params {
                        if n.eq_ignore_ascii_case(b"boundary") {
                            boundary = Some(v);
                            break;
                        }
                    }
                    assert!(boundary.is_some());
                    let boundary = boundary.unwrap().to_vec();
                    let parts = Self::parts(self.body(), &boundary);

                    self.content_type = ContentType::Multipart {
                        boundary,
                        kind: MultipartType::from(cst),
                        parts,
                    };
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
                } else {
                    let mut name: Option<String> = None;
                    for (n, v) in params {
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
                    self.content_type = ContentType::Other { tag, name };
                }
            }
            Err(e) => {
                debug!(
                    "parsing error in content_type: {:?} {:?}",
                    String::from_utf8_lossy(value),
                    e
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
                    let mut builder = AttachmentBuilder::default();
                    let (headers, body) = match parser::attachments::attachment(&a) {
                        Ok((_, v)) => v,
                        Err(_) => {
                            debug!("error in parsing attachment");
                            debug!("\n-------------------------------");
                            debug!("{}\n", ::std::string::String::from_utf8_lossy(a));
                            debug!("-------------------------------\n");

                            continue;
                        }
                    };

                    builder.raw = a.into();
                    builder.body = StrBuilder {
                        offset: a.len() - body.len(),
                        length: body.len(),
                    };
                    for (name, value) in headers {
                        if name.eq_ignore_ascii_case(b"content-type") {
                            builder.set_content_type_from_bytes(value);
                        } else if name.eq_ignore_ascii_case(b"content-transfer-encoding") {
                            builder.set_content_transfer_encoding(ContentTransferEncoding::from(
                                value,
                            ));
                        } else if name.eq_ignore_ascii_case(b"content-disposition") {
                            builder.set_content_disposition(ContentDisposition::from(value));
                        }
                    }
                    vec.push(builder.build());
                }
                vec
            }
            a => {
                debug!(
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
        AttachmentBuilder {
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
        Attachment {
            content_type,
            content_transfer_encoding,
            content_disposition,
            raw,
            body,
        }
    }
}

/// Immutable attachment type.
#[derive(Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Attachment {
    pub content_type: ContentType,
    pub content_transfer_encoding: ContentTransferEncoding,
    pub content_disposition: ContentDisposition,

    pub raw: Vec<u8>,
    pub body: StrBuilder,
}

impl fmt::Debug for Attachment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Attachment {{\n content_type: {:?},\n content_transfer_encoding: {:?},\n raw: Vec of {} bytes\n, body:\n{}\n}}",
        self.content_type,
        self.content_transfer_encoding,
        self.raw.len(),
        {
            let mut text = Vec::with_capacity(4096);
            self.get_text_recursive(&mut text);
            std::str::from_utf8(&text).map(std::string::ToString::to_string).unwrap_or_else(|e| format!("Unicode error {}", e))
        }
        )
    }
}

impl fmt::Display for Attachment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.content_type {
            ContentType::MessageRfc822 => {
                match EnvelopeWrapper::new(self.body.display_bytes(&self.raw).to_vec()) {
                    Ok(wrapper) => write!(
                        f,
                        "message/rfc822: {} - {} - {}",
                        wrapper.date(),
                        wrapper.field_from_to_string(),
                        wrapper.subject()
                    ),
                    Err(e) => write!(f, "{}", e),
                }
            }
            ContentType::PGPSignature => write!(f, "pgp signature {}", self.mime_type()),
            ContentType::OctetStream { ref name } => {
                write!(f, "{}", name.clone().unwrap_or_else(|| self.mime_type()))
            }
            ContentType::Other {
                name: Some(ref name),
                ..
            } => write!(f, "\"{}\", [{}]", name, self.mime_type()),
            ContentType::Other { .. } => write!(f, "Data attachment of type {}", self.mime_type()),
            ContentType::Text { ref parameters, .. }
                if parameters
                    .iter()
                    .any(|(name, _)| name.eq_ignore_ascii_case(b"name")) =>
            {
                let name = String::from_utf8_lossy(
                    parameters
                        .iter()
                        .find(|(name, _)| name == b"name")
                        .map(|(_, value)| value)
                        .unwrap(),
                );
                write!(f, "\"{}\", [{}]", name, self.mime_type())
            }
            ContentType::Text { .. } => write!(f, "Text attachment of type {}", self.mime_type()),
            ContentType::Multipart {
                parts: ref sub_att_vec,
                ..
            } => write!(
                f,
                "{} attachment with {} subs",
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
        Attachment {
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
                        debug!("error in parsing attachment");
                        debug!("\n-------------------------------");
                        debug!("{}\n", ::std::string::String::from_utf8_lossy(&self.raw));
                        debug!("-------------------------------\n");
                        debug!("{:?}\n", e);
                        Vec::new()
                    }
                }
            }
            _ => Vec::new(),
        }
    }

    /* Call on the body of a multipart/mixed Envelope to check if there are attachments without
     * completely parsing them */
    pub fn check_if_has_attachments_quick(bytes: &[u8], boundary: &[u8]) -> bool {
        if bytes.is_empty() {
            return false;
        }
        // FIXME: check if any part is multipart/mixed as well

        match parser::attachments::multipart_parts(bytes, boundary) {
            Ok((_, parts)) => {
                for p in parts {
                    for (n, v) in
                        crate::email::parser::generic::HeaderIterator(p.display_bytes(bytes))
                    {
                        if !n.eq_ignore_ascii_case(b"content-type") && !v.starts_with(b"text/") {
                            return true;
                        }
                    }
                }
            }
            Err(e) => {
                debug!("error in parsing multipart_parts");
                debug!("\n-------------------------------");
                debug!("{}\n", ::std::string::String::from_utf8_lossy(bytes));
                debug!("-------------------------------\n");
                debug!("{:?}\n", e);
            }
        }
        false
    }

    fn get_text_recursive(&self, text: &mut Vec<u8>) {
        match self.content_type {
            ContentType::Text { .. } | ContentType::PGPSignature => {
                text.extend(decode(self, None));
            }
            ContentType::Multipart {
                ref kind,
                ref parts,
                ..
            } => match kind {
                MultipartType::Alternative => {
                    for a in parts {
                        if a.content_disposition.kind.is_inline() {
                            if let ContentType::Text {
                                kind: Text::Plain, ..
                            } = a.content_type
                            {
                                a.get_text_recursive(text);
                                break;
                            }
                        }
                    }
                }
                _ => {
                    for a in parts {
                        if a.content_disposition.kind.is_inline() {
                            a.get_text_recursive(text);
                        }
                    }
                }
            },
            _ => {}
        }
    }
    pub fn text(&self) -> String {
        let mut text = Vec::with_capacity(self.body.length);
        self.get_text_recursive(&mut text);
        String::from_utf8_lossy(text.as_slice()).into()
    }

    pub fn mime_type(&self) -> String {
        self.content_type.to_string()
    }
    pub fn attachments(&self) -> Vec<Attachment> {
        let mut ret = Vec::new();
        fn count_recursive(att: &Attachment, ret: &mut Vec<Attachment>) {
            match att.content_type {
                ContentType::Multipart {
                    parts: ref sub_att_vec,
                    ..
                } => {
                    ret.push(att.clone());
                    // FIXME: Wrong count
                    for a in sub_att_vec {
                        count_recursive(a, ret);
                    }
                }
                _ => ret.push(att.clone()),
            }
        }

        count_recursive(&self, &mut ret);
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
        match self.content_type {
            ContentType::Text { .. } => true,
            _ => false,
        }
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
                kind: MultipartType::Alternative,
                ref parts,
                ..
            } => parts.iter().all(Attachment::is_html),

            ContentType::Multipart { ref parts, .. } => parts.iter().any(Attachment::is_html),
            _ => false,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self.content_type {
            ContentType::Multipart {
                kind: MultipartType::Signed,
                ..
            } => true,
            _ => false,
        }
    }

    pub fn into_raw(&self) -> String {
        let mut ret = String::with_capacity(2 * self.raw.len());
        fn into_raw_helper(a: &Attachment, ret: &mut String) {
            ret.push_str(&format!(
                "Content-Transfer-Encoding: {}\n",
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
                        ret.push_str("=");
                        if v.contains(&b' ') {
                            ret.push_str("\"");
                        }
                        ret.push_str(&String::from_utf8_lossy(v));
                        if v.contains(&b' ') {
                            ret.push_str("\"");
                        }
                    }

                    ret.push_str("\n\n");
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
                ContentType::Multipart {
                    boundary,
                    kind,
                    parts,
                } => {
                    let boundary = String::from_utf8_lossy(boundary);
                    ret.push_str(&format!("Content-Type: {}; boundary={}", kind, boundary));
                    if *kind == MultipartType::Signed {
                        ret.push_str("; micalg=pgp-sha512; protocol=\"application/pgp-signature\"");
                    }
                    ret.push('\n');

                    let boundary_start = format!("\n--{}\n", boundary);
                    for p in parts {
                        ret.push_str(&boundary_start);
                        into_raw_helper(p, ret);
                    }
                    ret.push_str(&format!("--{}--\n\n", boundary));
                }
                ContentType::MessageRfc822 => {
                    ret.push_str(&format!("Content-Type: {}\n\n", a.content_type));
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
                ContentType::PGPSignature => {
                    ret.push_str(&format!("Content-Type: {}\n\n", a.content_type));
                    ret.push_str(&String::from_utf8_lossy(a.body()));
                }
                ContentType::OctetStream { ref name } => {
                    if let Some(name) = name {
                        ret.push_str(&format!(
                            "Content-Type: {}; name={}\n\n",
                            a.content_type, name
                        ));
                    } else {
                        ret.push_str(&format!("Content-Type: {}\n\n", a.content_type));
                    }
                    ret.push_str(&BASE64_MIME.encode(a.body()).trim());
                }
                _ => {
                    ret.push_str(&format!("Content-Type: {}\n\n", a.content_type));
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
            if name.eq_ignore_ascii_case(b"content-type") {
                match parser::attachments::content_type(value) {
                    Ok((_, (_, _, params))) => {
                        ret = params;
                    }
                    _ => {}
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
            ContentType::Other { name, .. } | ContentType::OctetStream { name, .. } => name.clone(),
            _ => None,
        })
        .map(|n| n.replace(|c| std::path::is_separator(c) || c.is_ascii_control(), "_"))
    }
}

pub fn interpret_format_flowed(_t: &str) -> String {
    unimplemented!()
}

fn decode_rfc822(_raw: &[u8]) -> Attachment {
    // FIXME
    let builder = AttachmentBuilder::new(b"message/rfc822 cannot be displayed");
    builder.build()
}

type Filter<'a> = Box<dyn FnMut(&'a Attachment, &mut Vec<u8>) -> () + 'a>;

fn decode_rec_helper<'a>(a: &'a Attachment, filter: &mut Option<Filter<'a>>) -> Vec<u8> {
    match a.content_type {
        ContentType::Other { .. } => Vec::new(),
        ContentType::Text { .. } => decode_helper(a, filter),
        ContentType::OctetStream { ref name } => {
            name.clone().unwrap_or_else(|| a.mime_type()).into_bytes()
        }
        ContentType::PGPSignature => Vec::new(),
        ContentType::MessageRfc822 => {
            let temp = decode_rfc822(a.body());
            decode_rec(&temp, None)
        }
        ContentType::Multipart {
            ref kind,
            ref parts,
            ..
        } => match kind {
            MultipartType::Alternative => {
                for a in parts {
                    if let ContentType::Text {
                        kind: Text::Plain, ..
                    } = a.content_type
                    {
                        return decode_helper(a, filter);
                    }
                }
                decode_helper(a, filter)
            }
            MultipartType::Signed => {
                let mut vec = Vec::new();
                for a in parts {
                    vec.extend(decode_rec_helper(a, filter));
                }
                vec.extend(decode_helper(a, filter));
                vec
            }
            _ => {
                let mut vec = Vec::new();
                for a in parts {
                    if a.content_disposition.kind.is_inline() {
                        vec.extend(decode_rec_helper(a, filter));
                    }
                }
                vec
            }
        },
    }
}

pub fn decode_rec<'a>(a: &'a Attachment, mut filter: Option<Filter<'a>>) -> Vec<u8> {
    decode_rec_helper(a, &mut filter)
}

fn decode_helper<'a>(a: &'a Attachment, filter: &mut Option<Filter<'a>>) -> Vec<u8> {
    let charset = match a.content_type {
        ContentType::Text { charset: c, .. } => c,
        _ => Default::default(),
    };

    let bytes = match a.content_transfer_encoding {
        ContentTransferEncoding::Base64 => match BASE64_MIME.decode(a.body()) {
            Ok(v) => v,
            _ => a.body().to_vec(),
        },
        ContentTransferEncoding::QuotedPrintable => {
            parser::encodings::quoted_printable_bytes(a.body())
                .unwrap()
                .1
        }
        ContentTransferEncoding::_7Bit
        | ContentTransferEncoding::_8Bit
        | ContentTransferEncoding::Other { .. } => a.body().to_vec(),
    };

    let mut ret = if a.content_type.is_text() {
        if let Ok(v) = parser::encodings::decode_charset(&bytes, charset) {
            v.into_bytes()
        } else {
            a.body().to_vec()
        }
    } else {
        bytes.to_vec()
    };
    if let Some(filter) = filter {
        filter(a, &mut ret);
    }

    ret
}

pub fn decode<'a>(a: &'a Attachment, mut filter: Option<Filter<'a>>) -> Vec<u8> {
    decode_helper(a, &mut filter)
}
