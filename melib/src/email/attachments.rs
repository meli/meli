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
use crate::email::parser;
use crate::email::parser::BytesExt;
use crate::email::EnvelopeWrapper;
use data_encoding::BASE64_MIME;
use std::fmt;
use std::str;

pub use crate::email::attachment_types::*;

/*
 *
 * Data
 * Text { content: Vec<u8> }
 * Multipart
 */
// TODO: Add example.
//
#[derive(Default, PartialEq)]
pub struct AttachmentBuilder {
    content_type: ContentType,
    content_transfer_encoding: ContentTransferEncoding,

    raw: Vec<u8>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq)]
pub struct Attachment {
    content_type: ContentType,
    content_transfer_encoding: ContentTransferEncoding,

    raw: Vec<u8>,
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
            std::str::from_utf8(&text).map(|r| r.to_string()).unwrap_or_else(|e| format!("Unicode error {}", e))
        }
        )
    }
}

impl AttachmentBuilder {
    pub fn new(content: &[u8]) -> Self {
        AttachmentBuilder {
            content_type: Default::default(),
            content_transfer_encoding: ContentTransferEncoding::_7Bit,
            raw: content.to_vec(),
        }
    }
    pub fn content_type(&mut self) -> &ContentType {
        &self.content_type
    }

    pub fn set_content_type(&mut self, value: &[u8]) -> &Self {
        match parser::content_type(value).to_full_result() {
            Ok((ct, cst, params)) => {
                if ct.eq_ignore_ascii_case(b"multipart") {
                    let mut boundary = None;
                    for (n, v) in params {
                        if n.eq_ignore_ascii_case(b"boundary") {
                            boundary = Some(v);
                            break;
                        }
                    }
                    assert!(boundary.is_some());
                    let _boundary = boundary.unwrap();
                    let offset =
                        (_boundary.as_ptr() as usize).wrapping_sub(value.as_ptr() as usize);
                    let boundary = SliceBuild::new(offset, _boundary.len());
                    let subattachments = Self::subattachments(&self.raw, boundary.get(&value));
                    // Invalid mail or wrong assumption?
                    // assert!(!subattachments.is_empty());
                    self.content_type = ContentType::Multipart {
                        boundary,
                        kind: if cst.eq_ignore_ascii_case(b"mixed") {
                            MultipartType::Mixed
                        } else if cst.eq_ignore_ascii_case(b"alternative") {
                            MultipartType::Alternative
                        } else if cst.eq_ignore_ascii_case(b"digest") {
                            MultipartType::Digest
                        } else if cst.eq_ignore_ascii_case(b"signed") {
                            MultipartType::Signed
                        } else {
                            Default::default()
                        },
                        subattachments,
                    };
                } else if ct.eq_ignore_ascii_case(b"text") {
                    self.content_type = ContentType::Text {
                        kind: Text::Plain,
                        charset: Charset::UTF8,
                    };
                    for (n, v) in params {
                        if n.eq_ignore_ascii_case(b"charset") {
                            if let ContentType::Text {
                                charset: ref mut c, ..
                            } = self.content_type
                            {
                                *c = Charset::from(v);
                            }
                            break;
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
                    let mut tag: Vec<u8> = Vec::with_capacity(ct.len() + cst.len() + 1);
                    tag.extend(ct);
                    tag.push(b'/');
                    tag.extend(cst);
                    self.content_type = ContentType::Unsupported { tag };
                }
            }
            Err(v) => {
                debug!("parsing error in content_type: {:?} {:?}", value, v);
            }
        }
        self
    }
    pub fn set_content_transfer_encoding(&mut self, value: &[u8]) -> &Self {
        self.content_transfer_encoding = if value.eq_ignore_ascii_case(b"base64") {
            ContentTransferEncoding::Base64
        } else if value.eq_ignore_ascii_case(b"7bit") {
            ContentTransferEncoding::_7Bit
        } else if value.eq_ignore_ascii_case(b"8bit") {
            ContentTransferEncoding::_8Bit
        } else if value.eq_ignore_ascii_case(b"quoted-printable") {
            ContentTransferEncoding::QuotedPrintable
        } else {
            ContentTransferEncoding::Other {
                tag: value.to_ascii_lowercase(),
            }
        };
        self
    }
    /*
    fn decode(&self) -> Vec<u8> {
        // TODO merge this and standalone decode() function
        let charset = match self.content_type {
            ContentType::Text { charset: c, .. } => c,
            _ => Default::default(),
        };

        let bytes = match self.content_transfer_encoding {
            ContentTransferEncoding::Base64 => match BASE64_MIME.decode(&self.raw) {
                Ok(v) => v,
                _ => self.raw.to_vec(),
            },
            ContentTransferEncoding::QuotedPrintable => parser::quoted_printable_bytes(&self.raw)
                .to_full_result()
                .unwrap(),
            ContentTransferEncoding::_7Bit
            | ContentTransferEncoding::_8Bit
            | ContentTransferEncoding::Other { .. } => self.raw.to_vec(),
        };

        if let Ok(b) = parser::decode_charset(&bytes, charset) {
            b.into_bytes()
        } else {
            self.raw.to_vec()
        }
    }
    */
    pub fn build(self) -> Attachment {
        Attachment {
            content_type: self.content_type,
            content_transfer_encoding: self.content_transfer_encoding,
            raw: self.raw,
        }
    }

    pub fn subattachments(raw: &[u8], boundary: &[u8]) -> Vec<Attachment> {
        match parser::attachments(raw, boundary).to_full_result() {
            Ok(attachments) => {
                let mut vec = Vec::with_capacity(attachments.len());
                for a in attachments {
                    let mut builder = AttachmentBuilder::default();
                    let (headers, body) = match parser::attachment(&a).to_full_result() {
                        Ok(v) => v,
                        Err(_) => {
                            debug!("error in parsing attachment");
                            debug!("\n-------------------------------");
                            debug!("{}\n", ::std::string::String::from_utf8_lossy(a));
                            debug!("-------------------------------\n");

                            continue;
                        }
                    };

                    let body_slice = {
                        let offset = (body.as_ptr() as usize).wrapping_sub(a.as_ptr() as usize);
                        SliceBuild::new(offset, body.len())
                    };
                    builder.raw = body_slice.get(a).ltrim().into();
                    for (name, value) in headers {
                        if name.eq_ignore_ascii_case(b"content-type") {
                            builder.set_content_type(value);
                        } else if name.eq_ignore_ascii_case(b"content-transfer-encoding") {
                            builder.set_content_transfer_encoding(value);
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

impl fmt::Display for Attachment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.content_type {
            ContentType::MessageRfc822 => match EnvelopeWrapper::new(self.bytes().to_vec()) {
                Ok(wrapper) => write!(
                    f,
                    "message/rfc822: {} - {} - {}",
                    wrapper.date(),
                    wrapper.field_from_to_string(),
                    wrapper.subject()
                ),
                Err(e) => write!(f, "{}", e),
            },
            ContentType::PGPSignature => write!(f, "pgp signature {}", self.mime_type()),
            ContentType::Unsupported { .. } => {
                write!(f, "Data attachment of type {}", self.mime_type())
            }
            ContentType::Text { .. } => write!(f, "Text attachment of type {}", self.mime_type()),
            ContentType::Multipart {
                subattachments: ref sub_att_vec,
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
            content_transfer_encoding,
            raw,
        }
    }

    pub fn bytes(&self) -> &[u8] {
        &self.raw
    }
    fn get_text_recursive(&self, text: &mut Vec<u8>) {
        match self.content_type {
            ContentType::Text { .. } => {
                text.extend(decode(self, None));
            }
            ContentType::Multipart {
                ref kind,
                ref subattachments,
                ..
            } => match kind {
                MultipartType::Alternative => {
                    for a in subattachments {
                        if let ContentType::Text {
                            kind: Text::Plain, ..
                        } = a.content_type
                        {
                            a.get_text_recursive(text);
                            break;
                        }
                    }
                }
                _ => {
                    for a in subattachments {
                        a.get_text_recursive(text)
                    }
                }
            },
            _ => {}
        }
    }
    pub fn text(&self) -> String {
        let mut text = Vec::with_capacity(self.raw.len());
        self.get_text_recursive(&mut text);
        String::from_utf8_lossy(text.as_slice().trim()).into()
    }
    pub fn description(&self) -> Vec<String> {
        self.attachments().iter().map(|a| a.text()).collect()
    }
    pub fn mime_type(&self) -> String {
        format!("{}", self.content_type).to_string()
    }
    pub fn attachments(&self) -> Vec<Attachment> {
        let mut ret = Vec::new();
        fn count_recursive(att: &Attachment, ret: &mut Vec<Attachment>) {
            match att.content_type {
                ContentType::Multipart {
                    subattachments: ref sub_att_vec,
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
                ref subattachments,
                ..
            } => {
                for a in subattachments.iter() {
                    if let ContentType::Text {
                        kind: Text::Plain, ..
                    } = a.content_type
                    {
                        return false;
                    }
                }
                return true;
            }
            ContentType::Multipart {
                kind: MultipartType::Signed,
                ref subattachments,
                ..
            } => subattachments
                .iter()
                .find(|s| s.content_type != ContentType::PGPSignature)
                .map(|s| s.is_html())
                .unwrap_or(false),
            ContentType::Multipart {
                ref subattachments, ..
            } => subattachments
                .iter()
                .fold(true, |acc, a| match &a.content_type {
                    ContentType::Text {
                        kind: Text::Plain, ..
                    } => false,
                    ContentType::Text {
                        kind: Text::Html, ..
                    } => acc,
                    ContentType::Multipart {
                        kind: MultipartType::Alternative,
                        ..
                    } => a.is_html(),
                    _ => acc,
                }),
            _ => false,
        }
    }
}

pub fn interpret_format_flowed(_t: &str) -> String {
    //let mut n = String::with_capacity(t.len());
    unimplemented!()
}

fn decode_rfc822(_raw: &[u8]) -> Attachment {
    let builder = AttachmentBuilder::new(b"");
    builder.build()

    /*
    debug!("raw is\n{:?}", str::from_utf8(raw).unwrap());
         let e = match Envelope::from_bytes(raw) {
             Some(e) => e,
             None => {
             debug!("error in parsing mail");
                 let error_msg = b"Mail cannot be shown because of errors.";
                 let mut builder = AttachmentBuilder::new(error_msg);
                 return builder.build();
             }
         };
         e.body(None)
         */
}

type Filter<'a> = Box<FnMut(&'a Attachment, &mut Vec<u8>) -> () + 'a>;

fn decode_rec_helper<'a>(a: &'a Attachment, filter: &mut Option<Filter<'a>>) -> Vec<u8> {
    let ret = match a.content_type {
        ContentType::Unsupported { .. } => Vec::new(),
        ContentType::Text { .. } => decode_helper(a, filter),
        ContentType::PGPSignature => a.content_type.to_string().into_bytes(),
        ContentType::MessageRfc822 => decode_rec(&decode_rfc822(&a.raw), None),
        ContentType::Multipart {
            ref kind,
            ref subattachments,
            ..
        } => match kind {
            MultipartType::Alternative => {
                for a in subattachments {
                    if let ContentType::Text {
                        kind: Text::Plain, ..
                    } = a.content_type
                    {
                        return decode_helper(a, filter);
                    }
                }
                decode_helper(a, filter)
            }
            _ => {
                let mut vec = Vec::new();
                for a in subattachments {
                    vec.extend(decode_rec_helper(a, filter));
                }
                vec
            }
        },
    };
    ret
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
        ContentTransferEncoding::Base64 => match BASE64_MIME.decode(a.bytes()) {
            Ok(v) => v,
            _ => a.bytes().to_vec(),
        },
        ContentTransferEncoding::QuotedPrintable => parser::quoted_printable_bytes(a.bytes())
            .to_full_result()
            .unwrap(),
        ContentTransferEncoding::_7Bit
        | ContentTransferEncoding::_8Bit
        | ContentTransferEncoding::Other { .. } => a.bytes().to_vec(),
    };

    let mut ret = if a.content_type.is_text() {
        if let Ok(v) = parser::decode_charset(&bytes, charset) {
            v.into_bytes()
        } else {
            a.bytes().to_vec()
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
