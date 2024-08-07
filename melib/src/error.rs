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

//! Library error type.

use std::{borrow::Cow, io, result, str, string, sync::Arc};

pub type Result<T> = result::Result<T, Error>;

mod network;
pub use network::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    None,
    External,
    LinkedLibrary(&'static str),
    Authentication,
    Configuration,
    /// Protocol error.
    ///
    /// `EPROTO 71 Protocol error`
    ProtocolError,
    /// Protocol is not supported.
    /// It could be the wrong type or version.
    ProtocolNotSupported,
    Bug,
    Network(NetworkErrorKind),
    TimedOut,
    OSError,
    Platform,
    NotImplemented,
    NotSupported,
    NotFound,
    ValueError,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::None => write!(fmt, "None"),
            Self::External => write!(fmt, "External"),
            Self::LinkedLibrary(ref name) => write!(fmt, "Linked library error: {name}"),
            Self::Authentication => write!(fmt, "Authentication"),
            Self::Bug => write!(fmt, "Bug, please report this!"),
            Self::Network(ref inner) => write!(fmt, "{}", inner.as_str()),
            Self::ProtocolError => write!(fmt, "Protocol error"),
            Self::ProtocolNotSupported => write!(
                fmt,
                "Protocol is not supported. It could be the wrong type or version."
            ),
            Self::Platform => write!(fmt, "Platform/Runtime environment; OS or hardware"),
            Self::TimedOut => write!(fmt, "Timed Out"),
            Self::OSError => write!(fmt, "OS Error"),
            Self::Configuration => write!(fmt, "Configuration"),
            Self::NotImplemented => write!(fmt, "Not implemented"),
            Self::NotSupported => write!(fmt, "Not supported"),
            Self::NotFound => write!(fmt, "Not found"),
            Self::ValueError => write!(fmt, "Invalid value"),
        }
    }
}

macro_rules! is_variant {
    ($n:ident, $($var:tt)+) => {
        #[inline]
        pub fn $n(&self) -> bool {
            matches!(self, Self::$($var)*)
        }
    };
}

impl ErrorKind {
    is_variant! { is_authentication, Authentication }
    is_variant! { is_bug, Bug }
    is_variant! { is_configuration, Configuration }
    is_variant! { is_external, External }
    is_variant! { is_from_linked_library, LinkedLibrary(_) }
    is_variant! { is_network, Network(_) }
    is_variant! { is_network_down, Network(ref k) if k.is_network_down() }
    is_variant! { is_not_implemented, NotImplemented }
    is_variant! { is_not_supported, NotSupported }
    is_variant! { is_not_found, NotFound }
    is_variant! { is_oserror, OSError }
    is_variant! { is_protocol_error, ProtocolError }
    is_variant! { is_protocol_not_supported, ProtocolNotSupported }
    is_variant! { is_timeout, TimedOut }
    is_variant! { is_value_error, ValueError }

    #[inline]
    pub fn is_recoverable(&self) -> bool {
        !(self.is_authentication()
            || self.is_configuration()
            || self.is_bug()
            || self.is_external()
            || (self.is_network() && !self.is_network_down())
            || self.is_not_implemented()
            || self.is_not_supported()
            || self.is_not_found()
            || self.is_protocol_error()
            || self.is_protocol_not_supported()
            || self.is_value_error())
    }
}

#[macro_export]
macro_rules! src_err_arc_wrap {
    ($err:expr) => {{
        (Box::new($err) as Box<dyn std::error::Error + Send + Sync + 'static>).into()
    }};
}

#[derive(Clone, Debug)]
pub struct Error {
    pub summary: Cow<'static, str>,
    pub details: Option<Cow<'static, str>>,
    pub source: Option<std::sync::Arc<dyn std::error::Error + Send + Sync + 'static>>,
    pub kind: ErrorKind,
}

pub trait IntoError {
    fn set_err_summary<M>(self, msg: M) -> Error
    where
        M: Into<Cow<'static, str>>;

    fn set_err_details<M>(self, msg: M) -> Error
    where
        M: Into<Cow<'static, str>>;
    fn set_err_kind(self, kind: ErrorKind) -> Error;
}

pub trait ResultIntoError<T> {
    fn chain_err_summary<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>;
    fn chain_err_details<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>;
    fn chain_err_kind(self, kind: ErrorKind) -> Result<T>;
}

pub trait WrapResultIntoError<T, I>
where
    I: Send + Sync + std::error::Error + 'static,
{
    /// Wrap a result into a new [`Error`] that sets its source to the original
    /// value.
    fn wrap_err<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>;
}

impl<I: Into<Error>> IntoError for I {
    #[inline]
    fn set_err_summary<M>(self, msg: M) -> Error
    where
        M: Into<Cow<'static, str>>,
    {
        let err: Error = self.into();
        err.set_summary(msg)
    }

    #[inline]
    fn set_err_details<M>(self, msg: M) -> Error
    where
        M: Into<Cow<'static, str>>,
    {
        let err: Error = self.into();
        err.set_details(msg)
    }

    #[inline]
    fn set_err_kind(self, kind: ErrorKind) -> Error {
        let err: Error = self.into();
        err.set_kind(kind)
    }
}

impl<T, I: Into<Error>> ResultIntoError<T> for std::result::Result<T, I> {
    #[inline]
    fn chain_err_summary<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>,
    {
        self.map_err(|err| err.set_err_summary(msg_fn()))
    }

    #[inline]
    fn chain_err_details<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>,
    {
        self.map_err(|err| err.set_err_details(msg_fn()))
    }

    #[inline]
    fn chain_err_kind(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|err| err.set_err_kind(kind))
    }
}

impl<T, I> WrapResultIntoError<T, I> for std::result::Result<T, I>
where
    I: Send + Sync + std::error::Error + 'static,
{
    #[inline]
    /// Wrap a result into a new [`Error`] that sets its source to the original
    /// value.
    fn wrap_err<M, F>(self, msg_fn: F) -> Result<T>
    where
        F: Fn() -> M,
        M: Into<Cow<'static, str>>,
    {
        self.map_err(|err| Error::new(msg_fn()).set_source(Some(Arc::new(err))))
    }
}

impl Error {
    pub fn new<M>(msg: M) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        Self {
            summary: msg.into(),
            details: None,
            source: None,
            kind: ErrorKind::None,
        }
    }

    pub fn set_details<M>(mut self, details: M) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        if let Some(old_details) = self.details.as_ref() {
            self.details = Some(format!("{}. {}", old_details, details.into()).into());
        } else {
            self.details = Some(details.into());
        }
        self
    }

    pub fn set_summary<M>(mut self, summary: M) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        if self.summary.is_empty() {
            self.summary = summary.into();
        } else {
            self.summary = format!("{}. {}", self.summary, summary.into()).into();
        }
        self
    }

    pub fn set_source(
        mut self,
        new_val: Option<std::sync::Arc<dyn std::error::Error + Send + Sync + 'static>>,
    ) -> Self {
        self.source = new_val;
        self
    }

    pub fn set_kind(mut self, new_val: ErrorKind) -> Self {
        self.kind = new_val;
        self
    }

    #[inline]
    pub fn is_recoverable(&self) -> bool {
        !(self.kind.is_authentication()
            || self.kind.is_configuration()
            || self.kind.is_bug()
            || self.kind.is_external()
            || (self.kind.is_network() && !self.kind.is_network_down())
            || self.kind.is_not_implemented()
            || self.kind.is_not_supported()
            || self.kind.is_not_found()
            || self.kind.is_protocol_error()
            || self.kind.is_protocol_not_supported()
            || self.kind.is_value_error())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.summary)?;
        if let Some(details) = self.details.as_ref() {
            if !details.trim().is_empty() {
                write!(f, "\n{}", details)?;
            }
        }
        if let Some(source) = self.source.as_ref() {
            write!(f, "\nCaused by: {}", source)?;
        }
        if self.kind != ErrorKind::None {
            write!(f, "\nKind: {}", self.kind)?;
        }
        Ok(())
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|s| &(*(*s)) as &(dyn std::error::Error + 'static))
    }
}

impl From<io::ErrorKind> for ErrorKind {
    fn from(kind: io::ErrorKind) -> Self {
        match kind {
            io::ErrorKind::ConnectionRefused
            | io::ErrorKind::ConnectionReset
            | io::ErrorKind::ConnectionAborted
            | io::ErrorKind::NotConnected => Self::Network(NetworkErrorKind::ConnectionFailed),
            io::ErrorKind::TimedOut => Self::TimedOut,
            _ => Self::OSError,
        }
    }
}

impl From<std::convert::Infallible> for Error {
    fn from(_: std::convert::Infallible) -> Self {
        unreachable!()
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        let s = err.to_string();
        let kind = if s.contains("failed to lookup address information") {
            ErrorKind::Network(NetworkErrorKind::HostLookupFailed)
        } else {
            err.kind().into()
        };
        Self::new(s)
            .set_details(err.kind().to_string())
            .set_source(Some(Arc::new(err)))
            .set_kind(kind)
    }
}

impl<'a> From<Cow<'a, str>> for Error {
    #[inline]
    fn from(kind: Cow<'_, str>) -> Self {
        Self::new(kind.to_string())
    }
}

impl From<string::FromUtf8Error> for Error {
    #[inline]
    fn from(kind: string::FromUtf8Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

impl From<str::Utf8Error> for Error {
    #[inline]
    fn from(kind: str::Utf8Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}
//use std::option;
//impl From<option::NoneError> for Error {
//    #[inline]
//    fn from(kind: option::NoneError) -> Error {
//        Error::new(format!("{:?}", kind))
//    }
//}

impl<T> From<std::sync::PoisonError<T>> for Error {
    #[inline]
    fn from(kind: std::sync::PoisonError<T>) -> Self {
        Self::new(kind.to_string()).set_kind(ErrorKind::Bug)
    }
}

#[cfg(feature = "tls")]
impl<T: Sync + Send + 'static + std::fmt::Debug> From<native_tls::HandshakeError<T>> for Error {
    #[inline]
    fn from(kind: native_tls::HandshakeError<T>) -> Self {
        Self::new(kind.to_string())
            .set_source(Some(Arc::new(kind)))
            .set_kind(ErrorKind::Network(NetworkErrorKind::InvalidTLSConnection))
    }
}

#[cfg(feature = "tls")]
impl From<native_tls::Error> for Error {
    #[inline]
    fn from(kind: native_tls::Error) -> Self {
        Self::new(kind.to_string())
            .set_source(Some(Arc::new(kind)))
            .set_kind(ErrorKind::Network(NetworkErrorKind::InvalidTLSConnection))
    }
}

impl From<std::num::ParseIntError> for Error {
    #[inline]
    fn from(kind: std::num::ParseIntError) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

impl From<std::fmt::Error> for Error {
    #[inline]
    fn from(kind: std::fmt::Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "http")]
impl From<isahc::Error> for Error {
    #[inline]
    fn from(val: isahc::Error) -> Self {
        let kind: NetworkErrorKind = val.kind().into();
        Self::new(val.to_string())
            .set_source(Some(Arc::new(val)))
            .set_kind(ErrorKind::Network(kind))
    }
}

#[cfg(feature = "jmap")]
impl From<serde_json::error::Error> for Error {
    #[inline]
    fn from(kind: serde_json::error::Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

impl From<Box<dyn std::error::Error + Sync + Send + 'static>> for Error {
    #[inline]
    fn from(kind: Box<dyn std::error::Error + Sync + Send + 'static>) -> Self {
        Self::new(kind.to_string()).set_source(Some(kind.into()))
    }
}

impl From<std::ffi::NulError> for Error {
    #[inline]
    fn from(kind: std::ffi::NulError) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

impl From<nix::Error> for Error {
    #[inline]
    fn from(kind: nix::Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(feature = "sqlite3")]
impl From<rusqlite::Error> for Error {
    #[inline]
    fn from(kind: rusqlite::Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

#[cfg(any(feature = "maildir", feature = "mbox", feature = "notmuch"))]
impl From<notify::Error> for Error {
    #[inline]
    fn from(err: notify::Error) -> Self {
        let kind = match err.kind {
            notify::ErrorKind::MaxFilesWatch
            | notify::ErrorKind::WatchNotFound
            | notify::ErrorKind::Generic(_) => ErrorKind::External,
            notify::ErrorKind::Io(_) => ErrorKind::OSError,
            notify::ErrorKind::PathNotFound => ErrorKind::Configuration,
            notify::ErrorKind::InvalidConfig(_) => ErrorKind::Bug,
        };
        Self::new(err.to_string())
            .set_source(Some(Arc::new(err)))
            .set_kind(kind)
    }
}

impl From<libloading::Error> for Error {
    #[inline]
    fn from(kind: libloading::Error) -> Self {
        Self::new(kind.to_string()).set_source(Some(Arc::new(kind)))
    }
}

impl From<&str> for Error {
    #[inline]
    fn from(kind: &str) -> Self {
        Self::new(kind.to_string())
    }
}

impl From<String> for Error {
    #[inline]
    fn from(kind: String) -> Self {
        Self::new(kind)
    }
}

impl From<nom::Err<(&[u8], nom::error::ErrorKind)>> for Error {
    #[inline]
    fn from(kind: nom::Err<(&[u8], nom::error::ErrorKind)>) -> Self {
        Self::new("Parsing error").set_source(Some(Arc::new(Self::new(kind.to_string()))))
    }
}

impl From<nom::Err<(&str, nom::error::ErrorKind)>> for Error {
    #[inline]
    fn from(kind: nom::Err<(&str, nom::error::ErrorKind)>) -> Self {
        Self::new("Parsing error").set_details(kind.to_string())
    }
}

impl From<crate::email::InvalidHeaderName> for Error {
    #[inline]
    fn from(kind: crate::email::InvalidHeaderName) -> Self {
        Self::new(kind.to_string())
            .set_source(Some(Arc::new(kind)))
            .set_kind(ErrorKind::Network(NetworkErrorKind::InvalidTLSConnection))
    }
}

impl<'a> From<&'a mut Self> for Error {
    #[inline]
    fn from(kind: &'a mut Self) -> Self {
        kind.clone()
    }
}

impl<'a> From<&'a Self> for Error {
    #[inline]
    fn from(kind: &'a Self) -> Self {
        kind.clone()
    }
}

impl From<base64::DecodeError> for Error {
    #[inline]
    fn from(kind: base64::DecodeError) -> Self {
        Self::new("base64 decoding failed")
            .set_source(Some(Arc::new(kind)))
            .set_kind(ErrorKind::ValueError)
    }
}
