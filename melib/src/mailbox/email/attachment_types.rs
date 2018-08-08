use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Charset {
    Ascii,
    UTF8,
    UTF16,
    ISO8859_1,
    ISO8859_2,
    ISO8859_7,
    Windows1252,
    Windows1253,
    GBK,
    GB2312,
}

impl Default for Charset {
    fn default() -> Self {
        Charset::UTF8
    }
}

impl<'a> From<&'a[u8]> for Charset {
    fn from(b: &'a [u8]) -> Self {
        // TODO: Case insensitivity
        match b {
            b"us-ascii" | b"ascii" | b"US-ASCII" => Charset::Ascii,
            b"utf-8" | b"UTF-8" => Charset::UTF8,
            b"utf-16" | b"UTF-16" => Charset::UTF16,
            b"iso-8859-1" | b"ISO-8859-1" => Charset::ISO8859_1,
            b"iso-8859-2" | b"ISO-8859-2" => Charset::ISO8859_2,
            b"iso-8859-7" | b"ISO-8859-7" => Charset::ISO8859_7,
            b"windows-1252" | b"Windows-1252" => Charset::Windows1252,
            b"windows-1253" | b"Windows-1253" => Charset::Windows1253,
            b"GBK" | b"gbk" => Charset::GBK,
            b"gb2312" | b"GB2312" => Charset::GB2312,
            _ => Charset::Ascii,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MultipartType {
    Mixed,
    Alternative,
    Digest,
    Unsupported { tag: Vec<u8> },
}

impl Display for MultipartType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            MultipartType::Mixed => write!(f, "multipart/mixed"),
            MultipartType::Alternative => write!(f, "multipart/alternative"),
            MultipartType::Digest => write!(f, "multipart/digest"),
            MultipartType::Unsupported { tag: ref t } => {
                write!(f, "multipart/{}", String::from_utf8_lossy(t))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum ContentType {
    Text { charset: Charset },
    Multipart { boundary: Vec<u8> },
    Unsupported { tag: Vec<u8> },
}

impl Default for ContentType {
    fn default() -> Self {
        ContentType::Text{ charset: Charset::UTF8 }
    }

}

impl Display for ContentType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            ContentType::Text { .. } => write!(f, "text"),
            ContentType::Multipart { .. } => write!(f, "multipart"),
            ContentType::Unsupported { tag: ref t } => write!(f, "{}", String::from_utf8_lossy(t)),
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum ContentSubType {
    Plain,
    Html,
    Other { tag: Vec<u8> },
}

impl ContentSubType {
    pub fn is_html(&self) -> bool {
        if let ContentSubType::Html = self {
            true
        } else {
            false
        }
    }
}

impl Display for ContentSubType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            ContentSubType::Plain => write!(f, "plain"),
            ContentSubType::Html => write!(f, "html"),
            ContentSubType::Other { tag: ref t } => write!(f, "{}", String::from_utf8_lossy(t)),
        }
    }
}
#[derive(Clone, Debug)]
pub enum ContentTransferEncoding {
    _8Bit,
    _7Bit,
    Base64,
    QuotedPrintable,
    Other { tag: Vec<u8> },
}

