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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NetworkErrorKind {
    /// Unspecified
    None,
    /// Name lookup of host failed.
    HostLookupFailed,
    /// Bad client Certificate
    BadClientCertificate,
    /// Bad server certificate
    BadServerCertificate,
    /// Client initialization
    ClientInitialization,
    /// Connection failed
    ConnectionFailed,
    /// Invalid content encoding
    InvalidContentEncoding,
    /// Invalid credentials
    InvalidCredentials,
    /// Invalid request
    InvalidRequest,
    /// IO Error
    Io,
    /// Name resolution
    NameResolution,
    /// Protocol violation
    ProtocolViolation,
    /// Request body not rewindable
    RequestBodyNotRewindable,
    /// Connection (not request) timeout.
    Timeout,
    /// TooManyRedirects
    TooManyRedirects,
    /// Invalid TLS connection
    InvalidTLSConnection,
    /// Equivalent to HTTP status code 400 Bad Request
    /// [[RFC7231, Section 6.5.1](https://tools.ietf.org/html/rfc7231#section-6.5.1)]
    BadRequest,
    /// Equivalent to HTTP status code 401 Unauthorized
    /// [[RFC7235, Section 3.1](https://tools.ietf.org/html/rfc7235#section-3.1)]
    Unauthorized,
    /// Equivalent to HTTP status code 402 Payment Required
    /// [[RFC7231, Section 6.5.2](https://tools.ietf.org/html/rfc7231#section-6.5.2)]
    PaymentRequired,
    /// Equivalent to HTTP status code 403 Forbidden
    /// [[RFC7231, Section 6.5.3](https://tools.ietf.org/html/rfc7231#section-6.5.3)]
    Forbidden,
    /// Equivalent to HTTP status code 404 Not Found
    /// [[RFC7231, Section 6.5.4](https://tools.ietf.org/html/rfc7231#section-6.5.4)]
    NotFound,
    /// Equivalent to HTTP status code 405 Method Not Allowed
    /// [[RFC7231, Section 6.5.5](https://tools.ietf.org/html/rfc7231#section-6.5.5)]
    MethodNotAllowed,
    /// Equivalent to HTTP status code 406 Not Acceptable
    /// [[RFC7231, Section 6.5.6](https://tools.ietf.org/html/rfc7231#section-6.5.6)]
    NotAcceptable,
    /// Equivalent to HTTP status code 407 Proxy Authentication Required
    /// [[RFC7235, Section 3.2](https://tools.ietf.org/html/rfc7235#section-3.2)]
    ProxyAuthenticationRequired,
    /// Equivalent to HTTP status code 408 Request Timeout
    /// [[RFC7231, Section 6.5.7](https://tools.ietf.org/html/rfc7231#section-6.5.7)]
    RequestTimeout,
    /// Equivalent to HTTP status code 409 Conflict
    /// [[RFC7231, Section 6.5.8](https://tools.ietf.org/html/rfc7231#section-6.5.8)]
    Conflict,
    /// Equivalent to HTTP status code 410 Gone
    /// [[RFC7231, Section 6.5.9](https://tools.ietf.org/html/rfc7231#section-6.5.9)]
    Gone,
    /// Equivalent to HTTP status code 411 Length Required
    /// [[RFC7231, Section 6.5.10](https://tools.ietf.org/html/rfc7231#section-6.5.10)]
    LengthRequired,
    /// Equivalent to HTTP status code 412 Precondition Failed
    /// [[RFC7232, Section 4.2](https://tools.ietf.org/html/rfc7232#section-4.2)]
    PreconditionFailed,
    /// Equivalent to HTTP status code 413 Payload Too Large
    /// [[RFC7231, Section 6.5.11](https://tools.ietf.org/html/rfc7231#section-6.5.11)]
    PayloadTooLarge,
    /// Equivalent to HTTP status code 414 URI Too Long
    /// [[RFC7231, Section 6.5.12](https://tools.ietf.org/html/rfc7231#section-6.5.12)]
    URITooLong,
    /// Equivalent to HTTP status code 415 Unsupported Media Type
    /// [[RFC7231, Section 6.5.13](https://tools.ietf.org/html/rfc7231#section-6.5.13)]
    UnsupportedMediaType,
    /// Equivalent to HTTP status code 416 Range Not Satisfiable
    /// [[RFC7233, Section 4.4](https://tools.ietf.org/html/rfc7233#section-4.4)]
    RangeNotSatisfiable,
    /// Equivalent to HTTP status code 417 Expectation Failed
    /// [[RFC7231, Section 6.5.14](https://tools.ietf.org/html/rfc7231#section-6.5.14)]
    ExpectationFailed,
    /// Equivalent to HTTP status code 421 Misdirected Request
    /// [RFC7540, Section 9.1.2](http://tools.ietf.org/html/rfc7540#section-9.1.2)
    MisdirectedRequest,
    /// Equivalent to HTTP status code 422 Unprocessable Entity
    /// [[RFC4918](https://tools.ietf.org/html/rfc4918)]
    UnprocessableEntity,
    /// Equivalent to HTTP status code 423 Locked
    /// [[RFC4918](https://tools.ietf.org/html/rfc4918)]
    Locked,
    /// Equivalent to HTTP status code 424 Failed Dependency
    /// [[RFC4918](https://tools.ietf.org/html/rfc4918)]
    FailedDependency,

    /// Equivalent to HTTP status code 426 Upgrade Required
    /// [[RFC7231, Section 6.5.15](https://tools.ietf.org/html/rfc7231#section-6.5.15)]
    UpgradeRequired,

    /// Equivalent to HTTP status code 428 Precondition Required
    /// [[RFC6585](https://tools.ietf.org/html/rfc6585)]
    PreconditionRequired,
    /// Equivalent to HTTP status code 429 Too Many Requests
    /// [[RFC6585](https://tools.ietf.org/html/rfc6585)]
    TooManyRequests,

    /// Equivalent to HTTP status code 431 Request Header Fields Too Large
    /// [[RFC6585](https://tools.ietf.org/html/rfc6585)]
    RequestHeaderFieldsTooLarge,

    /// Equivalent to HTTP status code 451 Unavailable For Legal Reasons
    /// [[RFC7725](http://tools.ietf.org/html/rfc7725)]
    UnavailableForLegalReasons,

    /// Equivalent to HTTP status code 500 Internal Server Error
    /// [[RFC7231, Section 6.6.1](https://tools.ietf.org/html/rfc7231#section-6.6.1)]
    InternalServerError,
    /// Equivalent to HTTP status code 501 Not Implemented
    /// [[RFC7231, Section 6.6.2](https://tools.ietf.org/html/rfc7231#section-6.6.2)]
    NotImplemented,
    /// Equivalent to HTTP status code 502 Bad Gateway
    /// [[RFC7231, Section 6.6.3](https://tools.ietf.org/html/rfc7231#section-6.6.3)]
    BadGateway,
    /// Equivalent to HTTP status code 503 Service Unavailable
    /// [[RFC7231, Section 6.6.4](https://tools.ietf.org/html/rfc7231#section-6.6.4)]
    ServiceUnavailable,
    /// Equivalent to HTTP status code 504 Gateway Timeout
    /// [[RFC7231, Section 6.6.5](https://tools.ietf.org/html/rfc7231#section-6.6.5)]
    GatewayTimeout,
    /// Equivalent to HTTP status code 505 HTTP Version Not Supported
    /// [[RFC7231, Section 6.6.6](https://tools.ietf.org/html/rfc7231#section-6.6.6)]
    HTTPVersionNotSupported,
    /// Equivalent to HTTP status code 506 Variant Also Negotiates
    /// [[RFC2295](https://tools.ietf.org/html/rfc2295)]
    VariantAlsoNegotiates,
    /// Equivalent to HTTP status code 507 Insufficient Storage
    /// [[RFC4918](https://tools.ietf.org/html/rfc4918)]
    InsufficientStorage,
    /// Equivalent to HTTP status code 508 Loop Detected
    /// [[RFC5842](https://tools.ietf.org/html/rfc5842)]
    LoopDetected,
    /// Equivalent to HTTP status code 510 Not Extended
    /// [[RFC2774](https://tools.ietf.org/html/rfc2774)]
    NotExtended,
    /// Equivalent to HTTP status code 511 Network Authentication Required
    /// [[RFC6585](https://tools.ietf.org/html/rfc6585)]
    NetworkAuthenticationRequired,
}

impl NetworkErrorKind {
    pub const fn as_str(&self) -> &'static str {
        use NetworkErrorKind::*;
        match self {
            None => "Network",
            HostLookupFailed => "Name lookup of host failed.",
            BadClientCertificate => "Bad client Certificate",
            BadServerCertificate => "Bad server certificate",
            ClientInitialization => "Client initialization",
            ConnectionFailed => "Connection failed",
            InvalidContentEncoding => "Invalid content encoding",
            InvalidCredentials => "Invalid credentials",
            InvalidRequest => "Invalid request",
            Io => "IO Error",
            NameResolution => "Name resolution",
            ProtocolViolation => "Protocol violation",
            RequestBodyNotRewindable => "Request body not rewindable",
            Timeout => "Connection (not request) timeout.",
            TooManyRedirects => "TooManyRedirects",
            InvalidTLSConnection => "Invalid TLS connection",
            BadRequest => "Bad Request",
            Unauthorized => "Unauthorized",
            PaymentRequired => "Payment Required",
            Forbidden => "Forbidden",
            NotFound => "Not Found",
            MethodNotAllowed => "Method Not Allowed",
            NotAcceptable => "Not Acceptable",
            ProxyAuthenticationRequired => "Proxy Authentication Required",
            RequestTimeout => "Request Timeout",
            Conflict => "Conflict",
            Gone => "Gone",
            LengthRequired => "Length Required",
            PreconditionFailed => "Precondition Failed",
            PayloadTooLarge => "Payload Too Large",
            URITooLong => "URI Too Long",
            UnsupportedMediaType => "Unsupported Media Type",
            RangeNotSatisfiable => "Range Not Satisfiable",
            ExpectationFailed => "Expectation Failed",
            MisdirectedRequest => "Misdirected Request",
            UnprocessableEntity => "Unprocessable Entity",
            Locked => "Locked",
            FailedDependency => "Failed Dependency",
            UpgradeRequired => "Upgrade Required",
            PreconditionRequired => "Precondition Required",
            TooManyRequests => "Too Many Requests",
            RequestHeaderFieldsTooLarge => "Request Header Fields Too Large",
            UnavailableForLegalReasons => "Unavailable For Legal Reasons",
            InternalServerError => "Internal Server Error",
            NotImplemented => "Not Implemented",
            BadGateway => "Bad Gateway",
            ServiceUnavailable => "Service Unavailable",
            GatewayTimeout => "Gateway Timeout",
            HTTPVersionNotSupported => "HTTP Version Not Supported",
            VariantAlsoNegotiates => "Variant Also Negotiates",
            InsufficientStorage => "Insufficient Storage",
            LoopDetected => "Loop Detected",
            NotExtended => "Not Extended",
            NetworkAuthenticationRequired => "Network Authentication Required",
        }
    }

    /// Error kind means network is certainly down.
    pub const fn is_network_down(&self) -> bool {
        use NetworkErrorKind::*;
        matches!(
            self,
            BadGateway
                | ServiceUnavailable
                | GatewayTimeout
                | NetworkAuthenticationRequired
                | ConnectionFailed
        )
    }
}

impl Default for NetworkErrorKind {
    fn default() -> Self {
        Self::None
    }
}

#[cfg(feature = "http")]
impl From<isahc::http::StatusCode> for NetworkErrorKind {
    fn from(val: isahc::http::StatusCode) -> Self {
        match val {
            isahc::http::StatusCode::BAD_REQUEST => Self::BadRequest,
            isahc::http::StatusCode::UNAUTHORIZED => Self::Unauthorized,
            isahc::http::StatusCode::PAYMENT_REQUIRED => Self::PaymentRequired,
            isahc::http::StatusCode::FORBIDDEN => Self::Forbidden,
            isahc::http::StatusCode::NOT_FOUND => Self::NotFound,
            isahc::http::StatusCode::METHOD_NOT_ALLOWED => Self::MethodNotAllowed,
            isahc::http::StatusCode::NOT_ACCEPTABLE => Self::NotAcceptable,
            isahc::http::StatusCode::PROXY_AUTHENTICATION_REQUIRED => {
                Self::ProxyAuthenticationRequired
            }
            isahc::http::StatusCode::REQUEST_TIMEOUT => Self::RequestTimeout,
            isahc::http::StatusCode::CONFLICT => Self::Conflict,
            isahc::http::StatusCode::GONE => Self::Gone,
            isahc::http::StatusCode::LENGTH_REQUIRED => Self::LengthRequired,
            isahc::http::StatusCode::PRECONDITION_FAILED => Self::PreconditionFailed,
            isahc::http::StatusCode::PAYLOAD_TOO_LARGE => Self::PayloadTooLarge,
            isahc::http::StatusCode::URI_TOO_LONG => Self::URITooLong,
            isahc::http::StatusCode::UNSUPPORTED_MEDIA_TYPE => Self::UnsupportedMediaType,
            isahc::http::StatusCode::RANGE_NOT_SATISFIABLE => Self::RangeNotSatisfiable,
            isahc::http::StatusCode::EXPECTATION_FAILED => Self::ExpectationFailed,
            isahc::http::StatusCode::MISDIRECTED_REQUEST => Self::MisdirectedRequest,
            isahc::http::StatusCode::UNPROCESSABLE_ENTITY => Self::UnprocessableEntity,
            isahc::http::StatusCode::LOCKED => Self::Locked,
            isahc::http::StatusCode::FAILED_DEPENDENCY => Self::FailedDependency,
            isahc::http::StatusCode::UPGRADE_REQUIRED => Self::UpgradeRequired,
            isahc::http::StatusCode::PRECONDITION_REQUIRED => Self::PreconditionRequired,
            isahc::http::StatusCode::TOO_MANY_REQUESTS => Self::TooManyRequests,
            isahc::http::StatusCode::REQUEST_HEADER_FIELDS_TOO_LARGE => {
                Self::RequestHeaderFieldsTooLarge
            }
            isahc::http::StatusCode::UNAVAILABLE_FOR_LEGAL_REASONS => {
                Self::UnavailableForLegalReasons
            }
            isahc::http::StatusCode::INTERNAL_SERVER_ERROR => Self::InternalServerError,
            isahc::http::StatusCode::NOT_IMPLEMENTED => Self::NotImplemented,
            isahc::http::StatusCode::BAD_GATEWAY => Self::BadGateway,
            isahc::http::StatusCode::SERVICE_UNAVAILABLE => Self::ServiceUnavailable,
            isahc::http::StatusCode::GATEWAY_TIMEOUT => Self::GatewayTimeout,
            isahc::http::StatusCode::HTTP_VERSION_NOT_SUPPORTED => Self::HTTPVersionNotSupported,
            isahc::http::StatusCode::VARIANT_ALSO_NEGOTIATES => Self::VariantAlsoNegotiates,
            isahc::http::StatusCode::INSUFFICIENT_STORAGE => Self::InsufficientStorage,
            isahc::http::StatusCode::LOOP_DETECTED => Self::LoopDetected,
            isahc::http::StatusCode::NOT_EXTENDED => Self::NotExtended,
            isahc::http::StatusCode::NETWORK_AUTHENTICATION_REQUIRED => {
                Self::NetworkAuthenticationRequired
            }
            _ => Self::default(),
        }
    }
}

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
    NotImplemented,
    NotSupported,
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
            Self::TimedOut => write!(fmt, "Timed Out"),
            Self::OSError => write!(fmt, "OS Error"),
            Self::Configuration => write!(fmt, "Configuration"),
            Self::NotImplemented => write!(fmt, "Not implemented"),
            Self::NotSupported => write!(fmt, "Not supported"),
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
    is_variant! { is_oserror, OSError }
    is_variant! { is_protocol_error, ProtocolError }
    is_variant! { is_protocol_not_supported, ProtocolNotSupported }
    is_variant! { is_timeout, TimedOut }
    is_variant! { is_value_error, ValueError }
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

    fn chain_err_kind(self, kind: ErrorKind) -> Result<T>;
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
    fn chain_err_kind(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|err| err.set_err_kind(kind))
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

impl From<io::Error> for Error {
    #[inline]
    fn from(err: io::Error) -> Self {
        let kind = err.kind().into();
        Self::new(err.to_string())
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

#[cfg(feature = "http")]
impl From<&isahc::error::ErrorKind> for NetworkErrorKind {
    #[inline]
    fn from(val: &isahc::error::ErrorKind) -> Self {
        use isahc::error::ErrorKind::*;
        match val {
            BadClientCertificate => Self::BadClientCertificate,
            BadServerCertificate => Self::BadServerCertificate,
            ClientInitialization => Self::ClientInitialization,
            ConnectionFailed => Self::ConnectionFailed,
            InvalidContentEncoding => Self::InvalidContentEncoding,
            InvalidCredentials => Self::InvalidCredentials,
            InvalidRequest => Self::BadRequest,
            Io => Self::Io,
            NameResolution => Self::HostLookupFailed,
            ProtocolViolation => Self::ProtocolViolation,
            RequestBodyNotRewindable => Self::RequestBodyNotRewindable,
            Timeout => Self::Timeout,
            TlsEngine => Self::InvalidTLSConnection,
            TooManyRedirects => Self::TooManyRedirects,
            _ => Self::None,
        }
    }
}

impl From<NetworkErrorKind> for ErrorKind {
    #[inline]
    fn from(kind: NetworkErrorKind) -> Self {
        Self::Network(kind)
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
