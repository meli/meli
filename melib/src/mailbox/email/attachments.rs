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
use mailbox::email::parser;

use std::fmt::{Display, Formatter, Result as FmtResult};

/*
 *
 * Data
 * Text { content: String }
 * Multipart
 */

#[derive(Clone, Debug, PartialEq)]
pub enum MultipartType {
    Mixed,
    Alternative,
    Digest,
    Unsupported { tag: String },
}
#[derive(Clone, Debug)]
pub enum AttachmentType {
    Data { tag: String },
    Text { content: String },
    Multipart {
        of_type: MultipartType,
        subattachments: Vec<Attachment>,
    },
}
#[derive(Clone, Debug)]
pub enum ContentType {
    Text,
    Multipart { boundary: String },
    Unsupported { tag: String },
}

impl Display for ContentType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            ContentType::Text => write!(f, "text"),
            ContentType::Multipart { .. } => write!(f, "multipart"),
            ContentType::Unsupported { tag: ref t } => write!(f, "{}", t),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum ContentSubType {
    Plain,
    Other { tag: String },
}
impl Display for ContentSubType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            ContentSubType::Plain => write!(f, "plain"),
            ContentSubType::Other { tag: ref t } => write!(f, "{}", t),
        }
    }
}
#[derive(Clone, Debug)]
pub enum ContentTransferEncoding {
    _8Bit,
    _7Bit,
    Base64,
    QuotedPrintable,
    Other { tag: String },
}

/// TODO: Add example.
///
pub struct AttachmentBuilder {
    content_type: (ContentType, ContentSubType),
    content_transfer_encoding: ContentTransferEncoding,

    raw: Vec<u8>,
}

impl AttachmentBuilder {
    pub fn new(content: &[u8]) -> Self {
        AttachmentBuilder {
            content_type: (ContentType::Text, ContentSubType::Plain),
            content_transfer_encoding: ContentTransferEncoding::_7Bit,
            raw: content.to_vec(),
        }
    }
    pub fn content_type(&mut self, value: &str) -> &Self {
        match parser::content_type(value.as_bytes()).to_full_result() {
            Ok((ct, cst, params)) => if ct.eq_ignore_ascii_case("multipart") {
                let mut boundary = None;
                for (n, v) in params {
                    if n.eq_ignore_ascii_case("boundary") {
                        boundary = Some(format!("--{}--", v).to_string());
                        break;
                    }
                }
                assert!(boundary.is_some());
                self.content_type.0 = ContentType::Multipart {
                    boundary: boundary.unwrap(),
                };
                self.content_type.1 = ContentSubType::Other {
                    tag: cst.to_string(),
                };
            } else if ct.eq_ignore_ascii_case("text") {
                self.content_type.0 = ContentType::Text;
                if !cst.eq_ignore_ascii_case("plain") {
                    self.content_type.1 = ContentSubType::Other {
                        tag: cst.to_ascii_lowercase(),
                    };
                }
            } else {
                self.content_type.0 = ContentType::Unsupported {
                    tag: ct.to_ascii_lowercase(),
                };
                self.content_type.1 = ContentSubType::Other {
                    tag: cst.to_ascii_lowercase(),
                };
            },
            Err(v) => {
                eprintln!("parsing error in content_type: {:?} {:?}", value, v);
            }
        }
        self
    }
    pub fn content_transfer_encoding(&mut self, value: &str) -> &Self {
        self.content_transfer_encoding = if value.eq_ignore_ascii_case("base64") {
            ContentTransferEncoding::Base64
        } else if value.eq_ignore_ascii_case("7bit") {
            ContentTransferEncoding::_7Bit
        } else if value.eq_ignore_ascii_case("8bit") {
            ContentTransferEncoding::_8Bit
        } else if value.eq_ignore_ascii_case("quoted-printable") {
            ContentTransferEncoding::QuotedPrintable
        } else {
            ContentTransferEncoding::Other {
                tag: value.to_ascii_lowercase(),
            }
        };
        self
    }
    fn decode(&self) -> String {
        // TODO: Use charset for decoding
        match self.content_transfer_encoding {
            ContentTransferEncoding::Base64 => {
                match ::base64::decode(&::std::str::from_utf8(&self.raw)
                    .unwrap()
                    .trim()
                    .lines()
                    .fold(String::with_capacity(self.raw.len()), |mut acc, x| {
                        acc.push_str(x);
                        acc
                    })) {
                    Ok(ref v) => {
                        let s = String::from_utf8_lossy(v);
                        if s.find("\r\n").is_some() {
                            s.replace("\r\n", "\n")
                        } else {
                            s.into_owned()
                        }
                    }
                    _ => String::from_utf8_lossy(&self.raw).into_owned(),
                }
            }
            ContentTransferEncoding::QuotedPrintable => parser::quoted_printable_text(&self.raw)
                                                        .to_full_result()
                                                        .unwrap(),
            ContentTransferEncoding::_7Bit |
            ContentTransferEncoding::_8Bit |
            ContentTransferEncoding::Other { .. } => {
                String::from_utf8_lossy(&self.raw).into_owned()
            }
        }
    }
    pub fn build(self) -> Attachment {
        let attachment_type = match self.content_type.0 {
            ContentType::Text => AttachmentType::Text {
                content: self.decode(),
            },
            ContentType::Multipart { boundary: ref b } => {
                let multipart_type = match self.content_type.1 {
                    ContentSubType::Other { ref tag } => match tag.as_ref() {
                        "mixed" => MultipartType::Mixed,
                        "alternative" => MultipartType::Alternative,
                        "digest" => MultipartType::Digest,
                        t => MultipartType::Unsupported { tag: t.to_string() },
                    },
                    _ => panic!(),
                };
                AttachmentType::Multipart {
                    of_type: multipart_type,
                    subattachments: Self::subattachments(&self.raw, b),
                }
            }
            ContentType::Unsupported { ref tag } => AttachmentType::Data { tag: tag.clone() },
        };
        Attachment {
            content_type: self.content_type,
            content_transfer_encoding: self.content_transfer_encoding,
            raw: self.raw,
            attachment_type: attachment_type,
        }
    }

    pub fn subattachments(raw: &[u8], boundary: &str) -> Vec<Attachment> {
        let boundary_length = boundary.len();
        match parser::attachments(raw, &boundary[0..boundary_length - 2], boundary).to_full_result()
        {
            Ok(attachments) => {
                let mut vec = Vec::with_capacity(attachments.len());
                for a in attachments {
                    let (headers, body) = match parser::attachment(a).to_full_result() {
                        Ok(v) => v,
                        Err(_) => {
                            eprintln!("error in parsing attachment");
                            eprintln!("\n-------------------------------");
                            eprintln!("{}\n", ::std::string::String::from_utf8_lossy(a));
                            eprintln!("-------------------------------\n");

                            continue;
                        }
                    };
                    let mut builder = AttachmentBuilder::new(body);
                    for (name, value) in headers {
                        if name.eq_ignore_ascii_case("content-type") {
                            builder.content_type(value);
                        } else if name.eq_ignore_ascii_case("content-transfer-encoding") {
                            builder.content_transfer_encoding(value);
                        }
                    }
                    vec.push(builder.build());
                }
                vec
            }
            a => {
                eprintln!(
                    "error {:?}\n\traw: {:?}\n\tboundary: {:?}",
                    a,
                    ::std::str::from_utf8(raw).unwrap(),
                    boundary
                );
                Vec::new()
            }
        }
    }
}


#[derive(Clone, Debug)]
pub struct Attachment {
    content_type: (ContentType, ContentSubType),
    content_transfer_encoding: ContentTransferEncoding,

    raw: Vec<u8>,

    attachment_type: AttachmentType,
}

impl Display for Attachment {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self.attachment_type {
            AttachmentType::Data { .. } => {
                write!(f, "Data attachment of type {}", self.tag())
            }
            AttachmentType::Text { content: ref t } => {
                write!(f, "Text attachment")
            }
            AttachmentType::Multipart {
                of_type: ref multipart_type,
                subattachments: ref sub_att_vec,
            } => if *multipart_type == MultipartType::Alternative {
                write!(f, "Multipart/alternative attachment with {} subs", sub_att_vec.len())
            } else {
                write!(f, "Multipart attachment with {} subs", sub_att_vec.len())
            },
        }

    }
}

impl Attachment {
    fn get_text_recursive(&self, text: &mut String) {
        match self.attachment_type {
            AttachmentType::Data { .. } => {
                //text.push_str(&format!("Data attachment of type {}", self.tag()));
            }
            AttachmentType::Text { content: ref t } => {
                text.push_str(t);
            }
            AttachmentType::Multipart {
                of_type: ref multipart_type,
                subattachments: ref sub_att_vec,
            } => if *multipart_type == MultipartType::Alternative {
                for a in sub_att_vec {
                    if a.content_type.1 == ContentSubType::Plain {
                        a.get_text_recursive(text);
                        break;
                    }
                }
            } else {
                for a in sub_att_vec {
                    a.get_text_recursive(text);
                    text.push_str("\n\n");
                }
            },
        }
    }
    pub fn text(&self) -> String {
        let mut text = String::with_capacity(self.raw.len());
        self.get_text_recursive(&mut text);
        text
    }
    pub fn description(&self) -> Vec<String> {
        self.attachments().iter().map(|a| a.text()).collect()
    }
    pub fn tag(&self) -> String {
        format!("{}/{}", self.content_type.0, self.content_type.1).to_string()
    }
    pub fn attachments(&self) -> Vec<Attachment> {
        let mut ret = Vec::new();
        fn count_recursive(att: &Attachment, ret: &mut Vec<Attachment>) {
            match att.attachment_type {
                AttachmentType::Data { .. } | AttachmentType::Text { .. } => {
                    ret.push(att.clone())
                }
                AttachmentType::Multipart {
                    of_type: ref multipart_type,
                    subattachments: ref sub_att_vec,
                } => if *multipart_type != MultipartType::Alternative {
                    // TODO: Fix this, wrong count
                    for a in sub_att_vec {
                        count_recursive(a, ret);
                    }
                },
            }
        }

        count_recursive(&self, &mut ret);
        ret


    }
    pub fn count_attachments(&self) -> usize {
        self.attachments().len()
    }
}


pub fn interpret_format_flowed(t: &str) -> String {
    //let mut n = String::with_capacity(t.len());
    


    unimplemented!()
}
