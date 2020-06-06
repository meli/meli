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

use nom;

pub type Result<T> = result::Result<T, MeliError>;

#[derive(Debug, Clone)]
pub struct MeliError {
    pub summary: Option<Cow<'static, str>>,
    pub details: Cow<'static, str>,
    pub source: Option<std::sync::Arc<dyn Error + Send + Sync + 'static>>,
}

pub trait IntoMeliError {
    fn set_err_summary<M>(self, msg: M) -> MeliError
    where
        M: Into<Cow<'static, str>>;
}

pub trait ResultIntoMeliError<T> {
    fn chain_err_summary<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>;
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
        }
    }

    pub fn set_summary<M>(mut self, summary: M) -> MeliError
    where
        M: Into<Cow<'static, str>>,
    {
        self.summary = Some(summary.into());
        self
    }

    pub fn set_source(
        mut self,
        new_val: Option<std::sync::Arc<dyn Error + Send + Sync + 'static>>,
    ) -> MeliError {
        self.source = new_val;
        self
    }
}

impl fmt::Display for MeliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(summary) = self.summary.as_ref() {
            write!(f, "Summary: {}\n", summary)?;
        }
        let ret = write!(f, "{}", self.details)?;
        if let Some(source) = self.source.as_ref() {
            write!(f, "\nCaused by: {}", source)?;
        }
        Ok(ret)
    }
}

impl Into<String> for MeliError {
    fn into(self) -> String {
        self.details.into()
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

impl From<nom::IError> for MeliError {
    #[inline]
    fn from(kind: nom::IError) -> MeliError {
        MeliError::new(format!("{:?}", kind))
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

#[cfg(feature = "imap_backend")]
impl From<native_tls::HandshakeError<std::net::TcpStream>> for MeliError {
    #[inline]
    fn from(kind: native_tls::HandshakeError<std::net::TcpStream>) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "imap_backend")]
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
impl From<reqwest::Error> for MeliError {
    #[inline]
    fn from(kind: reqwest::Error) -> MeliError {
        MeliError::new(format!("{}", kind)).set_source(Some(Arc::new(kind)))
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

impl From<nix::Error> for MeliError {
    #[inline]
    fn from(kind: nix::Error) -> MeliError {
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
