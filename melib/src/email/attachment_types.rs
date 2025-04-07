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
use std::str;

use crate::email::{
    attachments::{Attachment, AttachmentBuilder},
    parser::BytesExt,
};

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Charset {
    Ascii,
    #[default]
    UTF8,
    UTF16,
    ISO8859_1,
    ISO8859_2,
    ISO8859_3,
    ISO8859_4,
    ISO8859_5,
    ISO8859_6,
    ISO8859_7,
    ISO8859_8,
    ISO8859_10,
    ISO8859_13,
    ISO8859_14,
    ISO8859_15,
    ISO8859_16,
    Windows1250,
    Windows1251,
    Windows1252,
    Windows1253,
    GBK,
    GB2312,
    GB18030,
    BIG5,
    ISO2022JP,
    EUCJP,
    KOI8R,
    KOI8U,
    KSX1001,
}

impl<'a> From<&'a [u8]> for Charset {
    fn from(b: &'a [u8]) -> Self {
        match b.trim() {
            b if b.eq_ignore_ascii_case(b"us-ascii") || b.eq_ignore_ascii_case(b"ascii") => {
                Self::Ascii
            }
            b if b.eq_ignore_ascii_case(b"utf-8") || b.eq_ignore_ascii_case(b"utf8") => Self::UTF8,
            b if b.eq_ignore_ascii_case(b"utf-16") || b.eq_ignore_ascii_case(b"utf16") => {
                Self::UTF16
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-1") || b.eq_ignore_ascii_case(b"iso8859-1") => {
                Self::ISO8859_1
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-2") || b.eq_ignore_ascii_case(b"iso8859-2") => {
                Self::ISO8859_2
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-3") || b.eq_ignore_ascii_case(b"iso8859-3") => {
                Self::ISO8859_3
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-4") || b.eq_ignore_ascii_case(b"iso8859-4") => {
                Self::ISO8859_4
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-5") || b.eq_ignore_ascii_case(b"iso8859-5") => {
                Self::ISO8859_5
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-6") || b.eq_ignore_ascii_case(b"iso8859-6") => {
                Self::ISO8859_6
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-7") || b.eq_ignore_ascii_case(b"iso8859-7") => {
                Self::ISO8859_7
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-8") || b.eq_ignore_ascii_case(b"iso8859-8") => {
                Self::ISO8859_8
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-10")
                || b.eq_ignore_ascii_case(b"iso8859-10") =>
            {
                Self::ISO8859_10
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-13")
                || b.eq_ignore_ascii_case(b"iso8859-13") =>
            {
                Self::ISO8859_13
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-14")
                || b.eq_ignore_ascii_case(b"iso8859-14") =>
            {
                Self::ISO8859_14
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-15")
                || b.eq_ignore_ascii_case(b"iso8859-15") =>
            {
                Self::ISO8859_15
            }
            b if b.eq_ignore_ascii_case(b"iso-8859-16")
                || b.eq_ignore_ascii_case(b"iso8859-16") =>
            {
                Self::ISO8859_16
            }
            b if b.eq_ignore_ascii_case(b"windows-1250")
                || b.eq_ignore_ascii_case(b"windows1250") =>
            {
                Self::Windows1250
            }
            b if b.eq_ignore_ascii_case(b"windows-1251")
                || b.eq_ignore_ascii_case(b"windows1251") =>
            {
                Self::Windows1251
            }
            b if b.eq_ignore_ascii_case(b"windows-1252")
                || b.eq_ignore_ascii_case(b"windows1252") =>
            {
                Self::Windows1252
            }
            b if b.eq_ignore_ascii_case(b"windows-1253")
                || b.eq_ignore_ascii_case(b"windows1253")
                || b.eq_ignore_ascii_case(b"cp1253")
                || b.eq_ignore_ascii_case(b"cp-1253") =>
            {
                Self::Windows1253
            }
            b if b.eq_ignore_ascii_case(b"gbk") => Self::GBK,
            b if b.eq_ignore_ascii_case(b"gb18030") || b.eq_ignore_ascii_case(b"gb-18030") => {
                Self::GB18030
            }
            b if b.eq_ignore_ascii_case(b"gb2312") || b.eq_ignore_ascii_case(b"gb-2312") => {
                Self::GB2312
            }
            b if b.eq_ignore_ascii_case(b"big5") => Self::BIG5,
            b if b.eq_ignore_ascii_case(b"iso-2022-jp") => Self::ISO2022JP,
            b if b.eq_ignore_ascii_case(b"euc-jp") => Self::EUCJP,
            b if b.eq_ignore_ascii_case(b"koi8-r") => Self::KOI8R,
            b if b.eq_ignore_ascii_case(b"koi8-u") => Self::KOI8U,
            b if b.eq_ignore_ascii_case(b"ks_c_5601-1987") => Self::KSX1001,
            _ => {
                debug!("unknown tag is {:?}", str::from_utf8(b));
                Self::Ascii
            }
        }
    }
}

impl std::fmt::Display for Charset {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ascii => write!(f, "us-ascii"),
            Self::UTF8 => write!(f, "utf-8"),
            Self::UTF16 => write!(f, "utf-16"),
            Self::ISO8859_1 => write!(f, "iso-8859-1"),
            Self::ISO8859_2 => write!(f, "iso-8859-2"),
            Self::ISO8859_3 => write!(f, "iso-8859-3"),
            Self::ISO8859_4 => write!(f, "iso-8859-4"),
            Self::ISO8859_5 => write!(f, "iso-8859-5"),
            Self::ISO8859_6 => write!(f, "iso-8859-6"),
            Self::ISO8859_7 => write!(f, "iso-8859-7"),
            Self::ISO8859_8 => write!(f, "iso-8859-8"),
            Self::ISO8859_10 => write!(f, "iso-8859-10"),
            Self::ISO8859_13 => write!(f, "iso-8859-13"),
            Self::ISO8859_14 => write!(f, "iso-8859-14"),
            Self::ISO8859_15 => write!(f, "iso-8859-15"),
            Self::ISO8859_16 => write!(f, "iso-8859-16"),
            Self::Windows1250 => write!(f, "windows-1250"),
            Self::Windows1251 => write!(f, "windows-1251"),
            Self::Windows1252 => write!(f, "windows-1252"),
            Self::Windows1253 => write!(f, "windows-1253"),
            Self::GBK => write!(f, "gbk"),
            Self::GB2312 => write!(f, "gb2312"),
            Self::GB18030 => write!(f, "gb18030"),
            Self::BIG5 => write!(f, "big5"),
            Self::ISO2022JP => write!(f, "iso-2022-jp"),
            Self::EUCJP => write!(f, "euc-jp"),
            Self::KSX1001 => write!(f, "ks_c_5601-1987"),
            Self::KOI8R => write!(f, "koi8-r"),
            Self::KOI8U => write!(f, "koi8-u"),
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum MultipartType {
    Alternative,
    Digest,
    Encrypted,
    #[default]
    Mixed,
    Related,
    Signed,
}

impl std::fmt::Display for MultipartType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Alternative => "multipart/alternative",
                Self::Digest => "multipart/digest",
                Self::Encrypted => "multipart/encrypted",
                Self::Mixed => "multipart/mixed",
                Self::Related => "multipart/related",
                Self::Signed => "multipart/signed",
            }
        )
    }
}

impl<B: AsRef<[u8]>> From<B> for MultipartType {
    fn from(val: B) -> Self {
        let val = val.as_ref();
        if val.eq_ignore_ascii_case(b"mixed") {
            Self::Mixed
        } else if val.eq_ignore_ascii_case(b"alternative") {
            Self::Alternative
        } else if val.eq_ignore_ascii_case(b"digest") {
            Self::Digest
        } else if val.eq_ignore_ascii_case(b"encrypted") {
            Self::Encrypted
        } else if val.eq_ignore_ascii_case(b"signed") {
            Self::Signed
        } else if val.eq_ignore_ascii_case(b"related") {
            Self::Related
        } else {
            Default::default()
        }
    }
}

#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum ContentType {
    Text {
        kind: Text,
        parameters: Vec<(Vec<u8>, Vec<u8>)>,
        charset: Charset,
    },
    Multipart {
        boundary: Vec<u8>,
        parameters: Vec<(Vec<u8>, Vec<u8>)>,
        kind: MultipartType,
        parts: Vec<Attachment>,
    },
    MessageRfc822,
    PGPSignature,
    CMSSignature,
    Other {
        tag: Vec<u8>,
        name: Option<String>,
        parameters: Vec<(Vec<u8>, Vec<u8>)>,
    },
    OctetStream {
        name: Option<String>,
        parameters: Vec<(Vec<u8>, Vec<u8>)>,
    },
}

impl std::fmt::Debug for ContentType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        macro_rules! fmt_params {
            ($p:expr) => {
                &$p.iter()
                    .map(|(b1, b2)| (String::from_utf8_lossy(b1), String::from_utf8_lossy(b2)))
                    .collect::<Vec<(_, _)>>()
            };
        }
        match self {
            Self::Text {
                kind,
                parameters,
                charset,
            } => fmt
                .debug_struct(stringify!(ContentType::Text))
                .field("kind", &kind)
                .field("parameters", fmt_params! { parameters })
                .field("charset", &charset)
                .finish(),
            Self::Multipart {
                boundary,
                parameters,
                kind,
                parts,
            } => fmt
                .debug_struct(stringify!(ContentType::Multipart))
                .field("kind", &kind)
                .field("boundary", &String::from_utf8_lossy(boundary))
                .field("parameters", fmt_params! { parameters })
                .field("parts", &parts)
                .finish(),
            Self::MessageRfc822 => fmt
                .debug_struct(stringify!(ContentType::MessageRfc822))
                .finish(),
            Self::PGPSignature => fmt
                .debug_struct(stringify!(ContentType::PGPSignature))
                .finish(),
            Self::CMSSignature => fmt
                .debug_struct(stringify!(ContentType::CMSSignature))
                .finish(),
            Self::Other {
                tag,
                name,
                parameters,
            } => fmt
                .debug_struct(stringify!(ContentType::Other))
                .field("tag", &String::from_utf8_lossy(tag))
                .field("name", name)
                .field("parameters", fmt_params! { parameters })
                .finish(),
            Self::OctetStream { name, parameters } => fmt
                .debug_struct(stringify!(ContentType::OctetStream))
                .field("name", name)
                .field("parameters", fmt_params! { parameters })
                .finish(),
        }
    }
}

impl Default for ContentType {
    fn default() -> Self {
        Self::Text {
            kind: Text::Plain,
            parameters: Vec::new(),
            charset: Charset::UTF8,
        }
    }
}

impl PartialEq<&[u8]> for ContentType {
    fn eq(&self, other: &&[u8]) -> bool {
        match (self, *other) {
            (
                Self::Text {
                    kind: Text::Plain, ..
                },
                b"text/plain",
            ) => true,
            (
                Self::Text {
                    kind: Text::Html, ..
                },
                b"text/html",
            ) => true,
            (
                Self::Multipart {
                    kind: MultipartType::Alternative,
                    ..
                },
                b"multipart/alternative",
            ) => true,
            (
                Self::Multipart {
                    kind: MultipartType::Digest,
                    ..
                },
                b"multipart/digest",
            ) => true,
            (
                Self::Multipart {
                    kind: MultipartType::Encrypted,
                    ..
                },
                b"multipart/encrypted",
            ) => true,
            (
                Self::Multipart {
                    kind: MultipartType::Mixed,
                    ..
                },
                b"multipart/mixed",
            ) => true,
            (
                Self::Multipart {
                    kind: MultipartType::Related,
                    ..
                },
                b"multipart/related",
            ) => true,
            (
                Self::Multipart {
                    kind: MultipartType::Signed,
                    ..
                },
                b"multipart/signed",
            ) => true,
            (Self::PGPSignature, b"application/pgp-signature") => true,
            (Self::CMSSignature, b"application/pkcs7-signature") => true,
            (Self::MessageRfc822, b"message/rfc822") => true,
            (Self::Other { tag, .. }, _) => other.eq_ignore_ascii_case(tag),
            (Self::OctetStream { .. }, b"application/octet-stream") => true,
            _ => false,
        }
    }
}

impl PartialEq<&str> for ContentType {
    fn eq(&self, other: &&str) -> bool {
        self.eq(&other.as_bytes())
    }
}

impl std::fmt::Display for ContentType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Text { kind: t, .. } => t.fmt(f),
            Self::Multipart { kind: k, .. } => k.fmt(f),
            Self::Other { ref tag, .. } => write!(f, "{}", String::from_utf8_lossy(tag)),
            Self::PGPSignature => write!(f, "application/pgp-signature"),
            Self::CMSSignature => write!(f, "application/pkcs7-signature"),
            Self::MessageRfc822 => write!(f, "message/rfc822"),
            Self::OctetStream { .. } => write!(f, "application/octet-stream"),
        }
    }
}

impl ContentType {
    pub fn is_text(&self) -> bool {
        matches!(self, Self::Text { .. })
    }

    pub fn is_text_html(&self) -> bool {
        matches!(
            self,
            Self::Text {
                kind: Text::Html,
                ..
            }
        )
    }

    pub fn is_text_plain(&self) -> bool {
        matches!(
            self,
            Self::Text {
                kind: Text::Plain,
                ..
            }
        )
    }

    pub fn make_boundary(parts: &[AttachmentBuilder]) -> String {
        use crate::email::compose::random::gen_boundary;
        let mut boundary = "bzz_bzz__bzz__".to_string();
        let mut random_boundary = gen_boundary();

        let mut loop_counter = 4096;
        'loo: loop {
            let mut flag = true;
            for sub in parts {
                'sub_loop: loop {
                    if sub.raw().find(random_boundary.as_bytes()).is_some() {
                        random_boundary = gen_boundary();
                        flag = false;
                    } else {
                        break 'sub_loop;
                    }
                }
            }
            if flag {
                break 'loo;
            }
            loop_counter -= 1;
            if loop_counter == 0 {
                panic!("Can't generate randomness. This is a BUG");
            }
        }

        boundary.push_str(&random_boundary);
        /* rfc134
         * "The only mandatory parameter for the multipart Content-Type is the
         * boundary parameter, which consists of 1 to 70 characters from a
         * set of characters known to be very robust through email gateways,
         * and NOT ending with white space" */
        boundary.truncate(70);
        boundary
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Other { ref name, .. } => name.as_ref().map(|n| n.as_ref()),
            Self::OctetStream {
                ref name,
                parameters: _,
            } => name.as_ref().map(|n| n.as_ref()),
            _ => None,
        }
    }

    pub fn parts(&self) -> Option<&[Attachment]> {
        if let Self::Multipart { ref parts, .. } = self {
            Some(parts)
        } else {
            None
        }
    }
}

#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Text {
    Plain,
    Html,
    Rfc822,
    Other { tag: Vec<u8> },
}

impl Text {
    pub fn is_html(&self) -> bool {
        matches!(self, Self::Html)
    }
}

impl std::fmt::Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::Plain => write!(f, "text/plain"),
            Self::Html => write!(f, "text/html"),
            Self::Rfc822 => write!(f, "text/rfc822"),
            Self::Other { tag: ref t } => write!(f, "text/{}", String::from_utf8_lossy(t)),
        }
    }
}

impl std::fmt::Debug for Text {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Plain => write!(fmt, "plain"),
            Self::Html => write!(fmt, "html"),
            Self::Rfc822 => write!(fmt, "rfc822"),
            Self::Other { tag } => write!(fmt, "{}", String::from_utf8_lossy(tag)),
        }
    }
}

#[derive(Clone, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum ContentTransferEncoding {
    #[default]
    _8Bit,
    _7Bit,
    Base64,
    QuotedPrintable,
    Other {
        tag: Vec<u8>,
    },
}

impl std::fmt::Debug for ContentTransferEncoding {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::_7Bit => fmt
                .debug_struct(stringify!(ContentTransferEncoding::_7Bit))
                .finish(),
            Self::_8Bit => fmt
                .debug_struct(stringify!(ContentTransferEncoding::_8Bit))
                .finish(),
            Self::Base64 => fmt
                .debug_struct(stringify!(ContentTransferEncoding::Base64))
                .finish(),
            Self::QuotedPrintable => fmt
                .debug_struct(stringify!(ContentTransferEncoding::QuotedPrintable))
                .finish(),
            Self::Other { tag } => fmt
                .debug_struct(stringify!(ContentTransferEncoding::Other))
                .field("tag", &String::from_utf8_lossy(tag))
                .finish(),
        }
    }
}

impl std::fmt::Display for ContentTransferEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::_7Bit => write!(f, "7bit"),
            Self::_8Bit => write!(f, "8bit"),
            Self::Base64 => write!(f, "base64"),
            Self::QuotedPrintable => write!(f, "quoted-printable"),
            Self::Other { tag: ref t } => {
                panic!("unknown encoding {:?}", str::from_utf8(t))
            }
        }
    }
}
impl<B: AsRef<[u8]>> From<B> for ContentTransferEncoding {
    fn from(val: B) -> Self {
        let val = val.as_ref();
        if val.eq_ignore_ascii_case(b"base64") {
            Self::Base64
        } else if val.eq_ignore_ascii_case(b"7bit") {
            Self::_7Bit
        } else if val.eq_ignore_ascii_case(b"8bit") {
            Self::_8Bit
        } else if val.eq_ignore_ascii_case(b"quoted-printable") {
            Self::QuotedPrintable
        } else {
            Self::Other {
                tag: val.to_ascii_lowercase(),
            }
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ContentDisposition {
    pub kind: ContentDispositionKind,
    pub filename: Option<String>,
    pub creation_date: Option<String>,
    pub modification_date: Option<String>,
    pub read_date: Option<String>,
    pub size: Option<String>,
    pub parameter: Vec<String>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum ContentDispositionKind {
    #[default]
    Inline,
    Attachment,
}

impl ContentDispositionKind {
    pub fn is_inline(&self) -> bool {
        matches!(self, Self::Inline)
    }

    pub fn is_attachment(&self) -> bool {
        matches!(self, Self::Attachment)
    }
}

impl std::fmt::Display for ContentDispositionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::Inline => write!(f, "inline"),
            Self::Attachment => write!(f, "attachment"),
        }
    }
}
impl<B: AsRef<[u8]>> From<B> for ContentDisposition {
    fn from(val: B) -> Self {
        let val = val.as_ref();
        crate::email::parser::attachments::content_disposition(val)
            .map(|(_, v)| v)
            .unwrap_or_default()
    }
}

impl From<ContentDispositionKind> for ContentDisposition {
    fn from(kind: ContentDispositionKind) -> Self {
        Self {
            kind,
            ..Self::default()
        }
    }
}
