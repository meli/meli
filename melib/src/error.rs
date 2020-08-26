/*
 * meli - error module
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

/*!
 * An error object for `melib`
 */

use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::io;
use std::result;
use std::str;
use std::string;
use std::sync::Arc;

pub type Result<T> = result::Result<T, MeliError>;

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum ErrorKind {
    None,
    Authentication,
    Network,
    Timeout,
}

impl ErrorKind {
    pub fn is_network(&self) -> bool {
        match self {
            ErrorKind::Network => true,
            _ => false,
        }
    }

    pub fn is_timeout(&self) -> bool {
        match self {
            ErrorKind::Timeout => true,
            _ => false,
        }
    }

    pub fn is_authentication(&self) -> bool {
        match self {
            ErrorKind::Authentication => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MeliError {
    pub summary: Option<Cow<'static, str>>,
    pub details: Cow<'static, str>,
    pub source: Option<std::sync::Arc<dyn Error + Send + Sync + 'static>>,
    pub kind: ErrorKind,
}

pub trait IntoMeliError {
    fn set_err_summary<M>(self, msg: M) -> MeliError
    where
        M: Into<Cow<'static, str>>;
    fn set_err_kind(self, kind: ErrorKind) -> MeliError;
}

pub trait ResultIntoMeliError<T> {
    fn chain_err_summary<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>;

    fn chain_err_kind(self, kind: ErrorKind) -> Result<T>;
}

impl<I: Into<MeliError>> IntoMeliError for I {
    #[inline]
    fn set_err_summary<M>(self, msg: M) -> MeliError
    where
        M: Into<Cow<'static, str>>,
    {
        let err: MeliError = self.into();
        err.set_summary(msg)
    }

    #[inline]
    fn set_err_kind(self, kind: ErrorKind) -> MeliError {
        let err: MeliError = self.into();
        err.set_kind(kind)
    }
}

impl<T, I: Into<MeliError>> ResultIntoMeliError<T> for std::result::Result<T, I> {
    #[inline]
    fn chain_err_summary<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>,
    {
        self.map_err(|err| err.set_err_summary(msg_fn()))
    }

    #[inline]
    fn chain_err_kind(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|err| err.set_err_kind(kind))
    }
}

impl MeliError {
    pub fn new<M>(msg: M) -> MeliError
    where
        M: Into<Cow<'static, str>>,
    {
        MeliError {
            summary: None,
            details: msg.into(),
            source: None,
            kind: ErrorKind::None,
        }
    }

    pub fn set_summary<M>(mut self, summary: M) -> MeliError
    where
        M: Into<Cow<'static, str>>,
    {
        if let Some(old_summary) = self.summary.take() {
            self.summary = Some(format!("{}. {}", old_summary, summary.into()).into());
        } else {
            self.summary = Some(summary.into());
        }
        self
    }

    pub fn set_source(
        mut self,
        new_val: Option<std::sync::Arc<dyn Error + Send + Sync + 'static>>,
    ) -> MeliError {
        self.source = new_val;
        self
    }

    pub fn set_kind(mut self, new_val: ErrorKind) -> MeliError {
        self.kind = new_val;
        self
    }
}

impl fmt::Display for MeliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(summary) = self.summary.as_ref() {
            writeln!(f, "Summary: {}", summary)?;
        }
        write!(f, "{}", self.details)?;
        if let Some(source) = self.source.as_ref() {
            write!(f, "\nCaused by: {}", source)?;
        }
        Ok(())
    }
}

impl Error for MeliError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_ref().map(|s| &(*(*s)) as _)
    }
}

impl From<io::Error> for MeliError {
    #[inline]
    fn from(kind: io::Error) -> MeliError {
        MeliError::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

impl<'a> From<Cow<'a, str>> for MeliError {
    #[inline]
    fn from(kind: Cow<'_, str>) -> MeliError {
        MeliError::new(format!("{:?}", kind))
    }
}

impl From<string::FromUtf8Error> for MeliError {
    #[inline]
    fn from(kind: string::FromUtf8Error) -> MeliError {
        MeliError::new(format!("{:?}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<str::Utf8Error> for MeliError {
    #[inline]
    fn from(kind: str::Utf8Error) -> MeliError {
        MeliError::new(format!("{:?}", kind)).set_source(Some(Arc::new(kind)))
    }
}
//use std::option;
//impl From<option::NoneError> for MeliError {
//    #[inline]
//    fn from(kind: option::NoneError) -> MeliError {
//        MeliError::new(format!("{:?}", kind))
//    }
//}

impl<T> From<std::sync::PoisonError<T>> for MeliError {
    #[inline]
    fn from(kind: std::sync::PoisonError<T>) -> MeliError {
        MeliError::new(format!("{}", kind))
    }
}

#[cfg(feature = "tls")]
impl<T: Sync + Send + 'static + core::fmt::Debug> From<native_tls::HandshakeError<T>>
    for MeliError
{
    #[inline]
    fn from(kind: native_tls::HandshakeError<T>) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "tls")]
impl From<native_tls::Error> for MeliError {
    #[inline]
    fn from(kind: native_tls::Error) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<std::num::ParseIntError> for MeliError {
    #[inline]
    fn from(kind: std::num::ParseIntError) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "jmap_backend")]
impl From<isahc::Error> for MeliError {
    #[inline]
    fn from(kind: isahc::Error) -> MeliError {
        MeliError::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "jmap_backend")]
impl From<serde_json::error::Error> for MeliError {
    #[inline]
    fn from(kind: serde_json::error::Error) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<Box<dyn Error + Sync + Send + 'static>> for MeliError {
    #[inline]
    fn from(kind: Box<dyn Error + Sync + Send + 'static>) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(kind.into()))
    }
}

impl From<std::ffi::NulError> for MeliError {
    #[inline]
    fn from(kind: std::ffi::NulError) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<Box<bincode::ErrorKind>> for MeliError {
    #[inline]
    fn from(kind: Box<bincode::ErrorKind>) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<nix::Error> for MeliError {
    #[inline]
    fn from(kind: nix::Error) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "sqlite3")]
impl From<rusqlite::Error> for MeliError {
    #[inline]
    fn from(kind: rusqlite::Error) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<libloading::Error> for MeliError {
    #[inline]
    fn from(kind: libloading::Error) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

impl From<&str> for MeliError {
    #[inline]
    fn from(kind: &str) -> MeliError {
        MeliError::new(kind.to_string())
    }
}

impl From<String> for MeliError {
    #[inline]
    fn from(kind: String) -> MeliError {
        MeliError::new(kind)
    }
}

impl From<nom::Err<(&[u8], nom::error::ErrorKind)>> for MeliError {
    #[inline]
    fn from(kind: nom::Err<(&[u8], nom::error::ErrorKind)>) -> MeliError {
        MeliError::new("Parsing error")
            .set_source(Some(Arc::new(MeliError::new(format!("{}", kind)))))
    }
}

impl From<nom::Err<(&str, nom::error::ErrorKind)>> for MeliError {
    #[inline]
    fn from(kind: nom::Err<(&str, nom::error::ErrorKind)>) -> MeliError {
        MeliError::new("Parsing error")
            .set_source(Some(Arc::new(MeliError::new(format!("{}", kind)))))
    }
}

impl<'a> From<&'a mut MeliError> for MeliError {
    #[inline]
    fn from(kind: &'a mut MeliError) -> MeliError {
        kind.clone()
    }
}

impl<'a> From<&'a MeliError> for MeliError {
    #[inline]
    fn from(kind: &'a MeliError) -> MeliError {
        kind.clone()
    }
}
