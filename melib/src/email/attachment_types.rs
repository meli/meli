/*
 * meli
 *
 * Copyright 2017-2019 Manos Pitsidianakis
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
use crate::email::attachments::Attachment;
use crate::email::parser::BytesExt;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::str;

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum Charset {
    Ascii,
    UTF8,
    UTF16,
    ISO8859_1,
    ISO8859_2,
    ISO8859_7,
    ISO8859_15,
    Windows1251,
    Windows1252,
    Windows1253,
    GBK,
    GB2312,
    BIG5,
    ISO2022JP,
}

impl Default for Charset {
    fn default() -> Self {
        Charset::UTF8
    }
}

impl<'a> From<&'a [u8]> for Charset {
    fn from(b: &'a [u8]) -> Self {
        // TODO: Case insensitivity
        match b.trim() {
            b"us-ascii" | b"ascii" | b"US-ASCII" => Charset::Ascii,
            b"utf-8" | b"UTF-8" => Charset::UTF8,
            b"utf-16" | b"UTF-16" => Charset::UTF16,
            b"iso-8859-1" | b"ISO-8859-1" => Charset::ISO8859_1,
            b"iso-8859-2" | b"ISO-8859-2" => Charset::ISO8859_2,
            b"iso-8859-7" | b"ISO-8859-7" | b"iso8859-7" => Charset::ISO8859_7,
            b"iso-8859-15" | b"ISO-8859-15" => Charset::ISO8859_15,
            b"windows-1251" | b"Windows-1251" => Charset::Windows1251,
            b"windows-1252" | b"Windows-1252" => Charset::Windows1252,
            b"windows-1253" | b"Windows-1253" => Charset::Windows1253,
            b"GBK" | b"gbk" => Charset::GBK,
            b"gb2312" | b"GB2312" => Charset::GB2312,
            b"BIG5" | b"big5" => Charset::BIG5,
            b"ISO-2022-JP" | b"iso-2022-JP" => Charset::ISO2022JP,
            _ => {
                debug!("unknown tag is {:?}", str::from_utf8(b));
                Charset::Ascii
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MultipartType {
    Mixed,
    Alternative,
    Digest,
    Signed,
}

impl Default for MultipartType {
    fn default() -> Self {
        MultipartType::Mixed
    }
}

impl Display for MultipartType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            MultipartType::Mixed => write!(f, "multipart/mixed"),
            MultipartType::Alternative => write!(f, "multipart/alternative"),
            MultipartType::Digest => write!(f, "multipart/digest"),
            MultipartType::Signed => write!(f, "multipart/signed"),
        }
    }
}

impl From<&[u8]> for MultipartType {
    fn from(val: &[u8]) -> MultipartType {
        if val.eq_ignore_ascii_case(b"mixed") {
            MultipartType::Mixed
        } else if val.eq_ignore_ascii_case(b"alternative") {
            MultipartType::Alternative
        } else if val.eq_ignore_ascii_case(b"digest") {
            MultipartType::Digest
        } else if val.eq_ignore_ascii_case(b"signed") {
            MultipartType::Signed
        } else {
            Default::default()
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum ContentType {
    Text {
        kind: Text,
        charset: Charset,
    },
    Multipart {
        boundary: Vec<u8>,
        kind: MultipartType,
        subattachments: Vec<Attachment>,
    },
    MessageRfc822,
    PGPSignature,
    Unsupported {
        tag: Vec<u8>,
    },
}

impl Default for ContentType {
    fn default() -> Self {
        ContentType::Text {
            kind: Text::Plain,
            charset: Charset::UTF8,
        }
    }
}

impl Display for ContentType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ContentType::Text { kind: t, .. } => t.fmt(f),
            ContentType::Multipart { kind: k, .. } => k.fmt(f),
            ContentType::Unsupported { tag: ref t } => write!(f, "{}", String::from_utf8_lossy(t)),
            ContentType::PGPSignature => write!(f, "application/pgp-signature"),
            ContentType::MessageRfc822 => write!(f, "message/rfc822"),
        }
    }
}

impl ContentType {
    pub fn is_text(&self) -> bool {
        if let ContentType::Text { .. } = self {
            true
        } else {
            false
        }
    }
    pub fn is_text_html(&self) -> bool {
        if let ContentType::Text {
            kind: Text::Html, ..
        } = self
        {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Text {
    Plain,
    Html,
    Rfc822,
    Other { tag: Vec<u8> },
}

impl Text {
    pub fn is_html(&self) -> bool {
        if let Text::Html = self {
            true
        } else {
            false
        }
    }
}

impl Display for Text {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            Text::Plain => write!(f, "text/plain"),
            Text::Html => write!(f, "text/html"),
            Text::Rfc822 => write!(f, "text/rfc822"),
            Text::Other { tag: ref t } => write!(f, "text/{}", String::from_utf8_lossy(t)),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum ContentTransferEncoding {
    _8Bit,
    _7Bit,
    Base64,
    QuotedPrintable,
    Other { tag: Vec<u8> },
}

impl Default for ContentTransferEncoding {
    fn default() -> Self {
        ContentTransferEncoding::_7Bit
    }
}

impl Display for ContentTransferEncoding {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            ContentTransferEncoding::_7Bit => write!(f, "7bit"),
            ContentTransferEncoding::_8Bit => write!(f, "8bit"),
            ContentTransferEncoding::Base64 => write!(f, "base64"),
            ContentTransferEncoding::QuotedPrintable => write!(f, "quoted-printable"),
            ContentTransferEncoding::Other { tag: ref t } => {
                panic!("unknown encoding {:?}", str::from_utf8(t))
            }
        }
    }
}
impl From<&[u8]> for ContentTransferEncoding {
    fn from(val: &[u8]) -> ContentTransferEncoding {
        if val.eq_ignore_ascii_case(b"base64") {
            ContentTransferEncoding::Base64
        } else if val.eq_ignore_ascii_case(b"7bit") {
            ContentTransferEncoding::_7Bit
        } else if val.eq_ignore_ascii_case(b"8bit") {
            ContentTransferEncoding::_8Bit
        } else if val.eq_ignore_ascii_case(b"quoted-printable") {
            ContentTransferEncoding::QuotedPrintable
        } else {
            ContentTransferEncoding::Other {
                tag: val.to_ascii_lowercase(),
            }
        }
    }
}
