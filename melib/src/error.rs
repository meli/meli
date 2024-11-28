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

use std::{
    borrow::Cow,
    error, io,
    path::{Path, PathBuf},
    result, str, string,
    sync::Arc,
};

pub use nix::errno::Errno;

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
    OSError(Errno),
    Platform,
    NotImplemented,
    NotSupported,
    NotFound,
    ValueError,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::None => write!(fmt, ""),
            Self::External => write!(fmt, "External error"),
            Self::LinkedLibrary(ref name) => write!(fmt, "Linked library `{name}` error"),
            Self::Authentication => write!(fmt, "Authentication error"),
            Self::Bug => write!(fmt, "Bug, please report this!"),
            Self::Network(ref inner) => write!(fmt, "{}", inner.as_str()),
            Self::ProtocolError => write!(fmt, "Protocol error"),
            Self::ProtocolNotSupported => write!(
                fmt,
                "Protocol is not supported. It could be the wrong type or version."
            ),
            Self::Platform => write!(fmt, "Platform/Runtime environment error (OS or hardware)"),
            Self::TimedOut => write!(fmt, "Timed out error"),
            Self::OSError(errno) => write!(fmt, "OS error {}: {}", *errno as i32, errno.desc()),
            Self::Configuration => write!(fmt, "Configuration error"),
            Self::NotImplemented => write!(fmt, "Not implemented error"),
            Self::NotSupported => write!(fmt, "Not supported error"),
            Self::NotFound => write!(fmt, "Not found error"),
            Self::ValueError => write!(fmt, "Invalid value error"),
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
    /// Handy alias for "Already exists" error.
    pub const ALREADY_EXISTS: Self = Self::OSError(Errno::EEXIST);

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
    is_variant! { is_oserror, OSError(_) }
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
    pub source: Option<Box<Self>>,
    pub inner: Option<Arc<dyn error::Error + Send + Sync + 'static>>,
    pub related_path: Option<PathBuf>,
    pub kind: ErrorKind,
}

#[cfg(test)]
impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.to_string().eq(&other.to_string())
    }
}

pub trait IntoError {
    fn set_err_summary<M>(self, msg: M) -> Error
    where
        M: Into<Cow<'static, str>>;

    fn set_err_details<M>(self, msg: M) -> Error
    where
        M: Into<Cow<'static, str>>;
    fn set_err_kind(self, kind: ErrorKind) -> Error;
    fn set_err_related_path(self, p: &std::path::Path) -> Error;
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
    fn chain_err_related_path(self, p: &std::path::Path) -> Result<T>;
}

pub trait WrapResultIntoError<T, I>
where
    I: Send + Sync + error::Error + 'static,
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
    fn set_err_related_path(self, p: &std::path::Path) -> Error {
        let err: Error = self.into();
        err.set_related_path(Some(p.to_path_buf()))
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
    fn chain_err_related_path(self, p: &Path) -> Result<T> {
        self.map_err(|err| err.set_err_related_path(p))
    }

    #[inline]
    fn chain_err_kind(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|err| err.set_err_kind(kind))
    }
}

impl<T, I> WrapResultIntoError<T, I> for std::result::Result<T, I>
where
    I: Send + Sync + error::Error + 'static,
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
            inner: None,
            related_path: None,
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
        new_val: Option<std::sync::Arc<dyn error::Error + Send + Sync + 'static>>,
    ) -> Self {
        self.source = new_val.map(|inner| {
            Box::new(Self {
                summary: "".into(),
                details: None,
                inner: Some(inner),
                source: None,
                related_path: None,
                kind: ErrorKind::External,
            })
        });
        self
    }

    pub fn from_inner(inner: std::sync::Arc<dyn error::Error + Send + Sync + 'static>) -> Self {
        Self {
            summary: "".into(),
            details: None,
            inner: Some(inner),
            source: None,
            related_path: None,
            kind: ErrorKind::External,
        }
    }

    pub fn set_kind(mut self, new_val: ErrorKind) -> Self {
        self.kind = new_val;
        self
    }

    pub fn set_related_path<P: Into<PathBuf>>(mut self, new_val: Option<P>) -> Self {
        let new_val = new_val.map(Into::into);
        self.related_path = new_val;
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

    /// Display error chain to user.
    fn display_chain(&'_ self) -> impl std::fmt::Display + '_ {
        ErrorChainDisplay {
            current: self,
            counter: 1,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.display_chain().fmt(f)
    }
}

#[derive(Clone, Copy)]
struct ErrorChainDisplay<'e> {
    current: &'e Error,
    counter: usize,
}

impl std::fmt::Display for ErrorChainDisplay<'_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut cur = *self;
        loop {
            if cur.counter > 1 {
                write!(fmt, "[{}] ", cur.counter)?;
            }
            match cur.current.kind {
                ErrorKind::External | ErrorKind::None => {}
                other => write!(fmt, "{other}: ")?,
            }
            if let Some(ref inner) = cur.current.inner {
                write!(fmt, "{}", inner)?;
            } else {
                write!(fmt, "{}", cur.current.summary)?;
                if let Some(details) = cur.current.details.as_ref() {
                    if !details.trim().is_empty() {
                        write!(fmt, "\n{}", details)?;
                    }
                }
                if let Some(ref path) = cur.current.related_path {
                    write!(fmt, "\nRelated path: {}", path.display())?;
                }
            }
            if let Some(ref source) = cur.current.source {
                writeln!(fmt, "\nCaused by:")?;
                cur = Self {
                    current: source,
                    counter: cur.counter + 1,
                };
            } else {
                return Ok(());
            }
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|s| &(*(*s)) as &(dyn error::Error + 'static))
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
            _ => Self::Platform,
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
        } else if let Some(errno) = err.raw_os_error() {
            ErrorKind::OSError(Errno::from_raw(errno))
        } else {
            err.kind().into()
        };
        Self::from_inner(Arc::new(err)).set_kind(kind)
    }
}

impl From<Cow<'_, str>> for Error {
    #[inline]
    fn from(err: Cow<'_, str>) -> Self {
        Self::new(err.to_string())
    }
}

impl From<string::FromUtf8Error> for Error {
    #[inline]
    fn from(err: string::FromUtf8Error) -> Self {
        Self::from_inner(Arc::new(err)).set_kind(ErrorKind::ValueError)
    }
}

impl From<str::Utf8Error> for Error {
    #[inline]
    fn from(err: str::Utf8Error) -> Self {
        Self::from_inner(Arc::new(err)).set_kind(ErrorKind::ValueError)
    }
}

impl<T> From<std::sync::PoisonError<T>> for Error {
    #[inline]
    fn from(err: std::sync::PoisonError<T>) -> Self {
        Self::new(err.to_string()).set_kind(ErrorKind::Bug)
    }
}

#[cfg(feature = "tls")]
impl<T: Sync + Send + 'static + std::fmt::Debug> From<native_tls::HandshakeError<T>> for Error {
    #[inline]
    fn from(err: native_tls::HandshakeError<T>) -> Self {
        Self::from_inner(Arc::new(err))
            .set_kind(ErrorKind::Network(NetworkErrorKind::InvalidTLSConnection))
    }
}

#[cfg(feature = "tls")]
impl From<native_tls::Error> for Error {
    #[inline]
    fn from(err: native_tls::Error) -> Self {
        Self::from_inner(Arc::new(err))
            .set_kind(ErrorKind::Network(NetworkErrorKind::InvalidTLSConnection))
    }
}

impl From<std::num::ParseIntError> for Error {
    #[inline]
    fn from(err: std::num::ParseIntError) -> Self {
        Self::new(err.to_string()).set_kind(ErrorKind::ValueError)
    }
}

impl From<std::fmt::Error> for Error {
    #[inline]
    fn from(err: std::fmt::Error) -> Self {
        Self::new(err.to_string()).set_kind(ErrorKind::Bug)
    }
}

#[cfg(feature = "http")]
impl From<isahc::Error> for Error {
    #[inline]
    fn from(val: isahc::Error) -> Self {
        let kind: NetworkErrorKind = val.kind().into();
        Self::from_inner(Arc::new(val)).set_kind(ErrorKind::Network(kind))
    }
}

impl From<serde_json::error::Error> for Error {
    #[inline]
    fn from(err: serde_json::error::Error) -> Self {
        Self::from_inner(Arc::new(err))
    }
}

impl From<Box<dyn error::Error + Sync + Send + 'static>> for Error {
    #[inline]
    fn from(err: Box<dyn error::Error + Sync + Send + 'static>) -> Self {
        Self::from_inner(err.into())
    }
}

impl From<std::ffi::NulError> for Error {
    #[inline]
    fn from(err: std::ffi::NulError) -> Self {
        Self::from_inner(Arc::new(err)).set_kind(ErrorKind::Bug)
    }
}

impl From<nix::Error> for Error {
    #[inline]
    fn from(err: nix::Error) -> Self {
        Self::from_inner(Arc::new(err)).set_kind(ErrorKind::OSError(err))
    }
}

#[cfg(feature = "sqlite3")]
impl From<rusqlite::Error> for Error {
    #[inline]
    fn from(err: rusqlite::Error) -> Self {
        Self::from_inner(Arc::new(err)).set_kind(ErrorKind::LinkedLibrary("sqlite3"))
    }
}

#[cfg(any(feature = "maildir", feature = "mbox-notify", feature = "notmuch"))]
impl From<notify::Error> for Error {
    #[inline]
    fn from(err: notify::Error) -> Self {
        let kind = match err.kind {
            notify::ErrorKind::MaxFilesWatch
            | notify::ErrorKind::WatchNotFound
            | notify::ErrorKind::Generic(_)
            | notify::ErrorKind::Io(_) => ErrorKind::Platform,
            notify::ErrorKind::PathNotFound => ErrorKind::Configuration,
            notify::ErrorKind::InvalidConfig(_) => ErrorKind::Bug,
        };
        Self::from_inner(Arc::new(err)).set_kind(kind)
    }
}

impl From<libloading::Error> for Error {
    #[inline]
    fn from(err: libloading::Error) -> Self {
        Self::from_inner(Arc::new(err))
    }
}

impl From<&str> for Error {
    #[inline]
    fn from(err: &str) -> Self {
        Self::new(err.to_string())
    }
}

impl From<String> for Error {
    #[inline]
    fn from(err: String) -> Self {
        Self::new(err)
    }
}

impl From<nom::Err<(&[u8], nom::error::ErrorKind)>> for Error {
    #[inline]
    fn from(err: nom::Err<(&[u8], nom::error::ErrorKind)>) -> Self {
        Self::new("Parsing error").set_details(err.to_string())
    }
}

impl From<nom::Err<(&str, nom::error::ErrorKind)>> for Error {
    #[inline]
    fn from(err: nom::Err<(&str, nom::error::ErrorKind)>) -> Self {
        Self::new("Parsing error").set_details(err.to_string())
    }
}

impl From<crate::email::InvalidHeaderName> for Error {
    #[inline]
    fn from(err: crate::email::InvalidHeaderName) -> Self {
        Self::from_inner(Arc::new(err)).set_kind(ErrorKind::ValueError)
    }
}

impl<'a> From<&'a mut Self> for Error {
    #[inline]
    fn from(err: &'a mut Self) -> Self {
        err.clone()
    }
}

impl<'a> From<&'a Self> for Error {
    #[inline]
    fn from(err: &'a Self) -> Self {
        err.clone()
    }
}

impl From<base64::DecodeError> for Error {
    #[inline]
    fn from(err: base64::DecodeError) -> Self {
        Self::from_inner(Arc::new(err))
            .set_summary("base64 decoding failed")
            .set_kind(ErrorKind::ValueError)
    }
}

impl From<xdg::BaseDirectoriesError> for Error {
    fn from(err: xdg::BaseDirectoriesError) -> Self {
        Self::new("Could not detect XDG directories for user")
            .set_source(Some(std::sync::Arc::new(Box::new(err))))
            .set_kind(ErrorKind::NotSupported)
    }
}
