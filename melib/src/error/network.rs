//
// meli - error module
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: GPL-3.0-or-later

//! Error kinds for network related errors.

use super::ErrorKind;

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
    /// TLS Connection failed
    TLSConnectionFailed,
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
    /// Too many redirects
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
            None => "Unspecified network error",
            HostLookupFailed => "Name lookup of host failed",
            BadClientCertificate => "Bad client certificate",
            BadServerCertificate => "Bad server Certificate",
            ClientInitialization => "Client initialization",
            ConnectionFailed => "Connection failed",
            TLSConnectionFailed => "TLS connection failed",
            InvalidContentEncoding => "Invalid content encoding",
            InvalidCredentials => "Invalid credentials",
            InvalidRequest => "Invalid request",
            Io => "IO error",
            NameResolution => "Name resolution",
            ProtocolViolation => "Protocol violation",
            RequestBodyNotRewindable => "Request body not rewindable",
            Timeout => "Connection (not request) timeout",
            TooManyRedirects => "Too many redirects",
            InvalidTLSConnection => "Invalid TLS connection",
            BadRequest => "Bad request",
            Unauthorized => "Unauthorized",
            PaymentRequired => "Payment required",
            Forbidden => "Forbidden",
            NotFound => "Not found",
            MethodNotAllowed => "Method not allowed",
            NotAcceptable => "Not acceptable",
            ProxyAuthenticationRequired => "Proxy authentication required",
            RequestTimeout => "Request timeout",
            Conflict => "Conflict",
            Gone => "Gone",
            LengthRequired => "Length required",
            PreconditionFailed => "Precondition failed",
            PayloadTooLarge => "Payload too large",
            URITooLong => "URI too long",
            UnsupportedMediaType => "Unsupported media type",
            RangeNotSatisfiable => "Range not satisfiable",
            ExpectationFailed => "Expectation failed",
            MisdirectedRequest => "Misdirected request",
            UnprocessableEntity => "Unprocessable entity",
            Locked => "Locked",
            FailedDependency => "Failed dependency",
            UpgradeRequired => "Upgrade required",
            PreconditionRequired => "Precondition required",
            TooManyRequests => "Too many requests",
            RequestHeaderFieldsTooLarge => "Request header fields too large",
            UnavailableForLegalReasons => "Unavailable for legal reasons",
            InternalServerError => "Internal server error",
            NotImplemented => "Not implemented",
            BadGateway => "Bad gateway",
            ServiceUnavailable => "Service unavailable",
            GatewayTimeout => "Gateway timeout",
            HTTPVersionNotSupported => "HTTP version not supported",
            VariantAlsoNegotiates => "Variant also negotiates",
            InsufficientStorage => "Insufficient storage",
            LoopDetected => "Loop detected",
            NotExtended => "Not extended",
            NetworkAuthenticationRequired => "Network authentication required",
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
                | TLSConnectionFailed
        )
    }

    /// Error kind means there has been a loss of connection.
    pub const fn is_disconnected(&self) -> bool {
        use NetworkErrorKind::*;
        matches!(
            self,
            ConnectionFailed | TLSConnectionFailed | InvalidTLSConnection
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
