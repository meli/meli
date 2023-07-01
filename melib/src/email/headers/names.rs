/*
 * meli - melib crate.
 *
 * Copyright 2023 Manos Pitsidianakis
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

//! E-mail header names. Also referred to as `Fields` in `RFC5322`.
#![allow(non_upper_case_globals)]

use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
    convert::TryFrom,
    error::Error,
    hash::{Hash, Hasher},
    str::FromStr,
};

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use crate::email::parser::BytesExt;

bitflags! {
    #[derive(Default, Serialize, Deserialize)]
    pub struct Protocol: u32 {
        const None    =  0b00000001;
        const Mail    =  Self::None.bits() << 1;
        const NNTP    =  Self::Mail.bits() << 1;
        const MIME    =  Self::NNTP.bits() << 1;
    }
}

/// Case insensitive wrapper for a header name. As of `RFC5322` it's
/// guaranteed to be ASCII.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct HeaderName {
    inner: Repr<Custom>,
}

impl Custom {
    fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(&*self.0) }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum Repr<T> {
    Standard(StandardHeader),
    Custom(T),
}

impl<T: std::fmt::Display> std::fmt::Display for Repr<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Standard(inner) => write!(fmt, "{}", inner.as_str()),
            Self::Custom(inner) => inner.fmt(fmt),
        }
    }
}

// Used to hijack the Hash impl
#[derive(Debug, Clone, Eq, PartialEq)]
struct Custom(SmallVec<[u8; 32]>);

/// A possible error when converting a `HeaderName` from another type.
pub struct InvalidHeaderName;

impl Error for InvalidHeaderName {}

impl std::fmt::Debug for InvalidHeaderName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", "Invalid header name.")
    }
}

impl std::fmt::Display for InvalidHeaderName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", stringify!(InvalidHeaderName))
    }
}

macro_rules! standard_headers {
    (
        $(
            $(#[$docs:meta])*
            ($konst:ident, $upcase:ident, $name:literal, $template:expr, $(Protocol::$var:tt)|+,$status:expr,$standards:expr);
        )+
    ) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
        pub enum StandardHeader {
            $(
                $konst,
            )+
        }

        $(
            $(#[$docs])*
            pub const $upcase: HeaderName = HeaderName {
                inner: Repr::Standard(StandardHeader::$konst),
            };
        )+

        impl HeaderName {
            $(
                pub const $upcase: Self = $upcase;
            )+
        }

        impl StandardHeader {
            #[inline]
            pub const fn as_str(&self) -> &'static str {
                match *self {
                    $(
                        Self::$konst => $name,
                    )+
                }
            }

            #[inline]
            pub const fn protocol(&self) -> Protocol {
                match *self {
                    $(
                        Self::$konst => Protocol::from_bits_truncate($(Protocol::$var.bits()|)* u32::MAX),
                    )+
                }
            }

            #[inline]
            pub const fn status(&self) -> Status {
                match *self {
                    $(
                        Self::$konst => $status,
                    )+
                }
            }

            #[inline]
            pub const fn standards(&self) -> &[Standard] {
                match *self {
                    $(
                        Self::$konst => $standards,
                    )+
                }
            }

            pub fn from_bytes(name_bytes: &[u8]) -> Option<Self> {
                match name_bytes {
                    $(
                        _ if name_bytes.eq_ignore_ascii_case($name.as_bytes()) => Some(Self::$konst),
                    )+
                    _ => None,
                }
            }
        }

        #[cfg(test)]
        const TEST_HEADERS: &[(StandardHeader, &str)] = &[
            $(
                (StandardHeader::$konst, $name),
            )+
        ];
    }
}

macro_rules! standards {
    (
        $(
            $(#[$docs:meta])*
            ($konst:ident, $upcase:ident, $name:literal, $lowername:literal );
        )+
    ) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub enum Standard {
            $(
                $konst,
            )+
        }

        $(
            $(#[$docs])*
            pub const $upcase: Standard = Standard::$konst;
        )+

        impl Standard {
            #[inline]
            pub const fn as_str(&self) -> &'static str {
                match *self {
                    $(
                        Self::$konst => $name,
                    )+
                }
            }

            #[inline]
            pub const fn url(&self) -> &str {
                match *self {
                    $(
                        Self::$konst => concat!("https://www.rfc-editor.org/rfc/", $lowername, ".html"),
                    )+
                }

            }

            pub fn from_bytes(name_bytes: &[u8]) -> Option<Self> {
                match name_bytes {
                    $(
                        _ if name_bytes.eq_ignore_ascii_case($name.as_bytes()) => Some(Self::$konst),
                    )+
                        _ => None,
                }
            }
        }
    };
}

standards! {
    (RFC0850, RFC0850, "RFC0850", "rfc0850");
    (RFC1808, RFC1808, "RFC1808", "rfc1808");
    (RFC1849, RFC1849, "RFC1849", "rfc1849");
    (RFC2068, RFC2068, "RFC2068", "rfc2068");
    (RFC2076, RFC2076, "RFC2076", "rfc2076");
    (RFC2110, RFC2110, "RFC2110", "rfc2110");
    (RFC2156, RFC2156, "RFC2156", "rfc2156");
    (RFC2557, RFC2557, "RFC2557", "rfc2557");
    (RFC2616, RFC2616, "RFC2616", "rfc2616");
    (RFC2980, RFC2980, "RFC2980", "rfc2980");
    (RFC3798, RFC3798, "RFC3798", "rfc3798");
    (RFC3834, RFC3834, "RFC3834", "rfc3834");
    (RFC3865, RFC3865, "RFC3865", "rfc3865");
    (RFC3977, RFC3977, "RFC3977", "rfc3977");
    (RFC4021, RFC4021, "RFC4021", "rfc4021");
    (RFC5064, RFC5064, "RFC5064", "rfc5064");
    (RFC5321, RFC5321, "RFC5321", "rfc5321");
    (RFC5322, RFC5322, "RFC5322", "rfc5322");
    (RFC5337, RFC5337, "RFC5337", "rfc5337");
    (RFC5504, RFC5504, "RFC5504", "rfc5504");
    (RFC5518, RFC5518, "RFC5518", "rfc5518");
    (RFC5536, RFC5536, "RFC5536", "rfc5536");
    (RFC5537, RFC5537, "RFC5537", "rfc5537");
    (RFC5703, RFC5703, "RFC5703", "rfc5703");
    (RFC6017, RFC6017, "RFC6017", "rfc6017");
    (RFC6068, RFC6068, "RFC6068", "rfc6068");
    (RFC6109, RFC6109, "RFC6109", "rfc6109");
    (RFC6376, RFC6376, "RFC6376", "rfc6376");
    (RFC6477, RFC6477, "RFC6477", "rfc6477");
    (RFC6758, RFC6758, "RFC6758", "rfc6758");
    (RFC6854, RFC6854, "RFC6854", "rfc6854");
    (RFC6857, RFC6857, "RFC6857", "rfc6857");
    (RFC7208, RFC7208, "RFC7208", "rfc7208");
    (RFC7259, RFC7259, "RFC7259", "rfc7259");
    (RFC7293, RFC7293, "RFC7293", "rfc7293");
    (RFC7444, RFC7444, "RFC7444", "rfc7444");
    (RFC7681, RFC7681, "RFC7681", "rfc7681");
    (RFC8058, RFC8058, "RFC8058", "rfc8058");
    (RFC8255, RFC8255, "RFC8255", "rfc8255");
    (RFC8315, RFC8315, "RFC8315", "rfc8315");
    (RFC8460, RFC8460, "RFC8460", "rfc8460");
    (RFC8601, RFC8601, "RFC8601", "rfc8601");
    (RFC8617, RFC8617, "RFC8617", "rfc8617");
    (RFC8689, RFC8689, "RFC8689", "rfc8689");
    (RFC9057, RFC9057, "RFC9057", "rfc9057");
    (RFC9228, RFC9228, "RFC9228", "rfc9228");
}

/// Status of field at the moment of writing.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Status {
    /// Deprecated,
    Deprecated,
    /// Experimental,
    Experimental,
    /// Informational,
    Informational,
    /// None,
    None,
    /// Obsoleted,
    Obsoleted,
    /// Reserved,
    Reserved,
    /// Standard,
    Standard,
}

// Generate constants for all standard e-mail field headers.
standard_headers! {
/*  Unit Variant                         |Constant ident                         |Actual field value                       |Template value                 |Protocols                        |Status                |Standards */
/*  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
    (Subject,                             SUBJECT,                                "Subject",                                None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (ReplyTo,                             REPLY_TO,                               "Reply-To",                               None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (InReplyTo,                           IN_REPLY_TO,                            "In-Reply-To",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (References,                          REFERENCES,                             "References",                             None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (MailReplyTo,                         MAIL_REPLY_TO,                          "Mail-Reply-To",                          None,                           Protocol::Mail,                   Status::None,          &[]);
    (MailFollowupTo,                      MAIL_FOLLOWUP_TO,                       "Mail-Followup-To",                       None,                           Protocol::Mail,                   Status::None,          &[]);
    (DeliveredTo,                         DELIVERED_TO,                           "Delivered-To",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC9228]);
    (Comments,                            COMMENTS,                               "Comments",                               None,                           Protocol::Mail,                   Status::None,          &[]);
    (Keywords,                            KEYWORDS,                               "Keywords",                               None,                           Protocol::Mail,                   Status::None,          &[]);
    (Received,                            RECEIVED,                               "Received",                               None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322, Standard::RFC5321]);
    (ContentLanguage,                     CONTENT_LANGUAGE,                       "Content-Language",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentLength,                       CONTENT_LENGTH,                         "Content-Length",                         None,                           Protocol::Mail,                   Status::None,          &[]);
    (Forwarded,                           FORWARDED,                              "Forwarded",                              None,                           Protocol::Mail,                   Status::None,          &[]);
    (AcceptLanguage,                      ACCEPT_LANGUAGE,                        "Accept-Language",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (AlsoControl,                         ALSO_CONTROL,                           "Also-Control",                           None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (AlternateRecipient,                  ALTERNATE_RECIPIENT,                    "Alternate-Recipient",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Approved,                            APPROVED,                               "Approved",                               None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (ArcAuthenticationResults,            ARC_AUTHENTICATION_RESULTS,             "ARC-Authentication-Results",             None,                           Protocol::Mail,                   Status::Experimental,  &[Standard::RFC8617]);
    (ArcMessageSignature,                 ARC_MESSAGE_SIGNATURE,                  "ARC-Message-Signature",                  None,                           Protocol::Mail,                   Status::Experimental,  &[Standard::RFC8617]);
    (ArcSeal,                             ARC_SEAL,                               "ARC-Seal",                               None,                           Protocol::Mail,                   Status::Experimental,  &[Standard::RFC8617]);
    (Archive,                             ARCHIVE,                                "Archive",                                None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (ArchivedAt,                          ARCHIVED_AT,                            "Archived-At",                            None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5064]);
    (ArticleNames,                        ARTICLE_NAMES,                          "Article-Names",                          None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (ArticleUpdates,                      ARTICLE_UPDATES,                        "Article-Updates",                        None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (AuthenticationResults,               AUTHENTICATION_RESULTS,                 "Authentication-Results",                 None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8601]);
    (AutoSubmitted,                       AUTO_SUBMITTED,                         "Auto-Submitted",                         None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC3834]);
    (Autoforwarded,                       AUTOFORWARDED,                          "Autoforwarded",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Autosubmitted,                       AUTOSUBMITTED,                          "Autosubmitted",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Base,                                BASE,                                   "Base",                                   None,                           Protocol::MIME,                   Status::Obsoleted,     &[Standard::RFC1808, Standard::RFC2068]);
    (Bcc,                                 BCC,                                    "Bcc",                                    None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (Body,                                BODY,                                   "Body",                                   None,                           Protocol::None,                   Status::Reserved,      &[Standard::RFC6068]);
    (CancelKey,                           CANCEL_KEY,                             "Cancel-Key",                             None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC8315]);
    (CancelLock,                          CANCEL_LOCK,                            "Cancel-Lock",                            None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC8315]);
    (Cc,                                  CC,                                     "Cc",                                     None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ContentAlternative,                  CONTENT_ALTERNATIVE,                    "Content-Alternative",                    None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentBase,                         CONTENT_BASE,                           "Content-Base",                           None,                           Protocol::MIME,                   Status::Obsoleted,     &[Standard::RFC2110, Standard::RFC2557]);
    (ContentDescription,                  CONTENT_DESCRIPTION,                    "Content-Description",                    None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentDisposition,                  CONTENT_DISPOSITION,                    "Content-Disposition",                    None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentDuration,                     CONTENT_DURATION,                       "Content-Duration",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentFeatures,                     CONTENT_FEATURES,                       "Content-Features",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentId,                           CONTENT_ID,                             "Content-ID",                             None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentIdentifier,                   CONTENT_IDENTIFIER,                     "Content-Identifier",                     None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ContentLocation,                     CONTENT_LOCATION,                       "Content-Location",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentMd5,                          CONTENT_MD5,                            "Content-MD5",                            None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentReturn,                       CONTENT_RETURN,                         "Content-Return",                         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ContentTransferEncoding,             CONTENT_TRANSFER_ENCODING,              "Content-Transfer-Encoding",              None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentTranslationType,              CONTENT_TRANSLATION_TYPE,               "Content-Translation-Type",               None,                           Protocol::MIME,                   Status::Standard,      &[Standard::RFC8255]);
    (ContentType,                         CONTENT_TYPE,                           "Content-Type",                           None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (Control,                             CONTROL,                                "Control",                                None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (Conversion,                          CONVERSION,                             "Conversion",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ConversionWithLoss,                  CONVERSION_WITH_LOSS,                   "Conversion-With-Loss",                   None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DlExpansionHistory,                  DL_EXPANSION_HISTORY,                   "DL-Expansion-History",                   None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Date,                                DATE,                                   "Date",                                   None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (DateReceived,                        DATE_RECEIVED,                          "Date-Received",                          None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC0850, Standard::RFC5536]);
    (DeferredDelivery,                    DEFERRED_DELIVERY,                      "Deferred-Delivery",                      None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DeliveryDate,                        DELIVERY_DATE,                          "Delivery-Date",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DiscardedX400IpmsExtensions,         DISCARDED_X400_IPMS_EXTENSIONS,         "Discarded-X400-IPMS-Extensions",         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DiscardedX400MtsExtensions,          DISCARDED_X400_MTS_EXTENSIONS,          "Discarded-X400-MTS-Extensions",          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DiscloseRecipients,                  DISCLOSE_RECIPIENTS,                    "Disclose-Recipients",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DispositionNotificationOptions,      DISPOSITION_NOTIFICATION_OPTIONS,       "Disposition-Notification-Options",       None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DispositionNotificationTo,           DISPOSITION_NOTIFICATION_TO,            "Disposition-Notification-To",            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Distribution,                        DISTRIBUTION,                           "Distribution",                           None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (DkimSignature,                       DKIM_SIGNATURE,                         "DKIM-Signature",                         None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6376]);
    (DowngradedBcc,                       DOWNGRADED_BCC,                         "Downgraded-Bcc",                         None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedCc,                        DOWNGRADED_CC,                          "Downgraded-Cc",                          None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedDispositionNotificationTo, DOWNGRADED_DISPOSITION_NOTIFICATION_TO, "Downgraded-Disposition-Notification-To", None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedFinalRecipient,            DOWNGRADED_FINAL_RECIPIENT,             "Downgraded-Final-Recipient",             None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedFrom,                      DOWNGRADED_FROM,                        "Downgraded-From",                        None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedInReplyTo,                 DOWNGRADED_IN_REPLY_TO,                 "Downgraded-In-Reply-To",                 None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedMailFrom,                  DOWNGRADED_MAIL_FROM,                   "Downgraded-Mail-From",                   None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedMessageId,                 DOWNGRADED_MESSAGE_ID,                  "Downgraded-Message-Id",                  None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedOriginalRecipient,         DOWNGRADED_ORIGINAL_RECIPIENT,          "Downgraded-Original-Recipient",          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedRcptTo,                    DOWNGRADED_RCPT_TO,                     "Downgraded-Rcpt-To",                     None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedReferences,                DOWNGRADED_REFERENCES,                  "Downgraded-References",                  None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedReplyTo,                   DOWNGRADED_REPLY_TO,                    "Downgraded-Reply-To",                    None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentBcc,                 DOWNGRADED_RESENT_BCC,                  "Downgraded-Resent-Bcc",                  None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentCc,                  DOWNGRADED_RESENT_CC,                   "Downgraded-Resent-Cc",                   None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentFrom,                DOWNGRADED_RESENT_FROM,                 "Downgraded-Resent-From",                 None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentReplyTo,             DOWNGRADED_RESENT_REPLY_TO,             "Downgraded-Resent-Reply-To",             None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentSender,              DOWNGRADED_RESENT_SENDER,               "Downgraded-Resent-Sender",               None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentTo,                  DOWNGRADED_RESENT_TO,                   "Downgraded-Resent-To",                   None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedReturnPath,                DOWNGRADED_RETURN_PATH,                 "Downgraded-Return-Path",                 None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedSender,                    DOWNGRADED_SENDER,                      "Downgraded-Sender",                      None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedTo,                        DOWNGRADED_TO,                          "Downgraded-To",                          None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (Encoding,                            ENCODING,                               "Encoding",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Encrypted,                           ENCRYPTED,                              "Encrypted",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Expires,                             EXPIRES,                                "Expires",                                None,                           Protocol::Mail | Protocol::NNTP,  Status::None,          &[Standard::RFC4021, Standard::RFC5536]);
    (ExpiryDate,                          EXPIRY_DATE,                            "Expiry-Date",                            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (FollowupTo,                          FOLLOWUP_TO,                            "Followup-To",                            None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (From,                                FROM,                                   "From",                                   None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (GenerateDeliveryReport,              GENERATE_DELIVERY_REPORT,               "Generate-Delivery-Report",               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Importance,                          IMPORTANCE,                             "Importance",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (IncompleteCopy,                      INCOMPLETE_COPY,                        "Incomplete-Copy",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (InjectionDate,                       INJECTION_DATE,                         "Injection-Date",                         None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (InjectionInfo,                       INJECTION_INFO,                         "Injection-Info",                         None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (Language,                            LANGUAGE,                               "Language",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (LatestDeliveryTime,                  LATEST_DELIVERY_TIME,                   "Latest-Delivery-Time",                   None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Lines,                               LINES,                                  "Lines",                                  None,                           Protocol::NNTP,                   Status::Deprecated,    &[Standard::RFC5536, Standard::RFC3977]);
    (ListArchive,                         LIST_ARCHIVE,                           "List-Archive",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListHelp,                            LIST_HELP,                              "List-Help",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListId,                              LIST_ID,                                "List-ID",                                None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListOwner,                           LIST_OWNER,                             "List-Owner",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListPost,                            LIST_POST,                              "List-Post",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListSubscribe,                       LIST_SUBSCRIBE,                         "List-Subscribe",                         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListUnsubscribe,                     LIST_UNSUBSCRIBE,                       "List-Unsubscribe",                       Some("perm/list-unsubscribe"),  Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListUnsubscribePost,                 LIST_UNSUBSCRIBE_POST,                  "List-Unsubscribe-Post",                  None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8058]);
    (MessageContext,                      MESSAGE_CONTEXT,                        "Message-Context",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (MessageId,                           MESSAGE_ID,                             "Message-ID",                             None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5322, Standard::RFC5536]);
    (MessageType,                         MESSAGE_TYPE,                           "Message-Type",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (MimeVersion,                         MIME_VERSION,                           "MIME-Version",                           None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (MtPriority,                          MT_PRIORITY,                            "MT-Priority",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6758]);
    (Newsgroups,                          NEWSGROUPS,                             "Newsgroups",                             None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (NntpPostingDate,                     NNTP_POSTING_DATE,                      "NNTP-Posting-Date",                      None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC5536]);
    (NntpPostingHost,                     NNTP_POSTING_HOST,                      "NNTP-Posting-Host",                      None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC2980, Standard::RFC5536]);
    (Obsoletes,                           OBSOLETES,                              "Obsoletes",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Organization,                        ORGANIZATION,                           "Organization",                           None,                           Protocol::Mail | Protocol::NNTP,  Status::Informational, &[Standard::RFC7681,   Standard::RFC5536]);
    (OriginalEncodedInformationTypes,     ORIGINAL_ENCODED_INFORMATION_TYPES,     "Original-Encoded-Information-Types",     None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (OriginalFrom,                        ORIGINAL_FROM,                          "Original-From",                          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5703]);
    (OriginalMessageId,                   ORIGINAL_MESSAGE_ID,                    "Original-Message-ID",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (OriginalRecipient,                   ORIGINAL_RECIPIENT,                     "Original-Recipient",                     Some("perm/original-recipient"),Protocol::Mail,                   Status::Standard,      &[Standard::RFC3798, Standard::RFC5337]);
    (OriginalSender,                      ORIGINAL_SENDER,                        "Original-Sender",                        None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5537]);
    (OriginatorReturnAddress,             ORIGINATOR_RETURN_ADDRESS,              "Originator-Return-Address",              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (OriginalSubject,                     ORIGINAL_SUBJECT,                       "Original-Subject",                       None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5703]);
    (Path,                                PATH,                                   "Path",                                   None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (PicsLabel,                           PICS_LABEL,                             "PICS-Label",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (PostingVersion,                      POSTING_VERSION,                        "Posting-Version",                        None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC0850, Standard::RFC5536]);
    (PreventNondeliveryReport,            PREVENT_NONDELIVERY_REPORT,             "Prevent-NonDelivery-Report",             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Priority,                            PRIORITY,                               "Priority",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ReceivedSpf,                         RECEIVED_SPF,                           "Received-SPF",                           None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC7208]);
    (RelayVersion,                        RELAY_VERSION,                          "Relay-Version",                          None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC0850, Standard::RFC5536]);
    (ReplyBy,                             REPLY_BY,                               "Reply-By",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (RequireRecipientValidSince,          REQUIRE_RECIPIENT_VALID_SINCE,          "Require-Recipient-Valid-Since",          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC7293]);
    (ResentBcc,                           RESENT_BCC,                             "Resent-Bcc",                             None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentCc,                            RESENT_CC,                              "Resent-Cc",                              None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentDate,                          RESENT_DATE,                            "Resent-Date",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentFrom,                          RESENT_FROM,                            "Resent-From",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (ResentMessageId,                     RESENT_MESSAGE_ID,                      "Resent-Message-ID",                      None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentReplyTo,                       RESENT_REPLY_TO,                        "Resent-Reply-To",                        None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5322]);
    (ResentSender,                        RESENT_SENDER,                          "Resent-Sender",                          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (ResentTo,                            RESENT_TO,                              "Resent-To",                              None,                           Protocol::Mail,                   Status::Standard,       &[Standard::RFC5322]);
    (ReturnPath,                          RETURN_PATH,                            "Return-Path",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (SeeAlso,                             SEE_ALSO,                               "See-Also",                               None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (Sender,                              SENDER,                                 "Sender",                                 None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (Sensitivity,                         SENSITIVITY,                            "Sensitivity",                            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Solicitation,                        SOLICITATION,                           "Solicitation",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC3865]);
    (Summary,                             SUMMARY,                                "Summary",                                None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (Supersedes,                          SUPERSEDES,                             "Supersedes",                             None,                           Protocol::Mail | Protocol::NNTP,  Status::None,          &[Standard::RFC5536, Standard::RFC2156]);
    (TlsReportDomain,                     TLS_REPORT_DOMAIN,                      "TLS-Report-Domain",                      None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8460]);
    (TlsReportSubmitter,                  TLS_REPORT_SUBMITTER,                   "TLS-Report-Submitter",                   None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8460]);
    (TlsRequired,                         TLS_REQUIRED,                           "TLS-Required",                           None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8689]);
    (To,                                  TO,                                     "To",                                     None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (UserAgent,                           USER_AGENT,                             "User-Agent",                             None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536, Standard::RFC2616]);
    (VbrInfo,                             VBR_INFO,                               "VBR-Info",                               None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5518]);
    (X400ContentIdentifier,               X400_CONTENT_IDENTIFIER,                "X400-Content-Identifier",                None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400ContentReturn,                   X400_CONTENT_RETURN,                    "X400-Content-Return",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400ContentType,                     X400_CONTENT_TYPE,                      "X400-Content-Type",                      None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400MtsIdentifier,                   X400_MTS_IDENTIFIER,                    "X400-MTS-Identifier",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Originator,                      X400_ORIGINATOR,                        "X400-Originator",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Received,                        X400_RECEIVED,                          "X400-Received",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Recipients,                      X400_RECIPIENTS,                        "X400-Recipients",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Trace,                           X400_TRACE,                             "X400-Trace",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Xref,                                XREF,                                   "Xref",                                   None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (ApparentlyTo,                        APPARENTLY_TO,                          "Apparently-To",                          Some("prov/apparently-to"),     Protocol::Mail,                   Status::None,          &[Standard::RFC2076]);
    (Author,                              AUTHOR,                                 "Author",                                 None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC9057]);
    (EdiintFeatures,                      EDIINT_FEATURES,                        "EDIINT-Features",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6017]);
    (EesstVersion,                        EESST_VERSION,                          "Eesst-Version",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC7681]);
    (ErrorsTo,                            ERRORS_TO,                              "Errors-To",                              Some("prov/errors-to"),         Protocol::Mail,                   Status::None,          &[Standard::RFC2076]);
    (JabberId,                            JABBER_ID,                              "Jabber-ID",                              Some("prov/jabber-id"),         Protocol::Mail | Protocol::NNTP,  Status::None,          &[Standard::RFC7259]);
    (SioLabel,                            SIO_LABEL,                              "SIO-Label",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC7444]);
    (SioLabelHistory,                     SIO_LABEL_HISTORY,                      "SIO-Label-History",                      None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC7444]);
    (XArchivedAt,                         X_ARCHIVED_AT,                          "X-Archived-At",                          Some("prov/x-archived-at"),     Protocol::Mail | Protocol::NNTP,  Status::Deprecated,    &[Standard::RFC5064]);
    (XMittente,                           X_MITTENTE,                             "X-Mittente",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XRicevuta,                           X_RICEVUTA,                             "X-Ricevuta",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XRiferimentoMessageId,               X_RIFERIMENTO_MESSAGE_ID,               "X-Riferimento-Message-ID",               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XTiporicevuta,                       X_TIPORICEVUTA,                         "X-TipoRicevuta",                         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XTrasporto,                          X_TRASPORTO,                            "X-Trasporto",                            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XVerificasicurezza,                  X_VERIFICASICUREZZA,                    "X-VerificaSicurezza",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
}

/// Valid header name ASCII bytes
///
/// Source: [RFC5322 3.6.8.](https://datatracker.ietf.org/doc/html/rfc5322#autoid-35)
/// ```text
/// field-name      =   1*ftext
///
/// ftext           =   %d33-57 /          ; Printable US-ASCII
///                     %d59-126           ;  characters not including
///                                        ;  ":".
/// ```
const HEADER_CHARS: [u8; 128] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //   x
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //  1x
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //  2x
    0, 0, 0, b'!', b'"', b'#', b'$', b'%', b'&', b'\'', //  3x
    0, 0, b'*', b'+', 0, b'-', b'.', 0, b'0', b'1', //  4x
    b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', 0, 0, //  5x
    0, 0, 0, 0, 0, b'a', b'b', b'c', b'd', b'e', //  6x
    b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', //  7x
    b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', //  8x
    b'z', 0, 0, 0, b'^', b'_', b'`', b'a', b'b', b'c', //  9x
    b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', // 10x
    b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', // 11x
    b'x', b'y', b'z', 0, b'|', 0, b'~', 0, // 128
];

impl HeaderName {
    /// Returns a `str` representation of the header.
    ///
    /// The returned string will always be lower case. Use `Display` for a
    /// properly formatted representation.
    #[inline]
    pub fn as_str(&self) -> &str {
        match self.inner {
            Repr::Standard(v) => v.as_str(),
            Repr::Custom(ref v) => v.as_str(),
        }
    }

    /// Returns a `&[u8]` representation of the header.
    ///
    /// The returned string will always be lower case. Use `Display` for a
    /// properly formatted representation.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        match self.inner {
            Repr::Standard(v) => v.as_str().as_bytes(),
            Repr::Custom(ref v) => v.0.as_ref(),
        }
    }

    pub fn from_bytes(src: &[u8]) -> Result<Self, InvalidHeaderName> {
        if let Some(std) = StandardHeader::from_bytes(src.trim()) {
            Ok(Self {
                inner: Repr::Standard(std),
            })
        } else {
            let mut buf = SmallVec::<[u8; 32]>::new();
            for b in src {
                if let Some(b) = HEADER_CHARS.get(*b as usize).filter(|b| **b != 0) {
                    buf.push(*b);
                } else {
                    return Err(InvalidHeaderName::new());
                }
            }

            Ok(Self {
                inner: Repr::Custom(Custom(buf)),
            })
        }
    }

    pub const fn is_standard(&self) -> bool {
        matches!(
            self,
            Self {
                inner: Repr::Standard(_)
            }
        )
    }
}

impl FromStr for HeaderName {
    type Err = InvalidHeaderName;

    fn from_str(s: &str) -> Result<HeaderName, InvalidHeaderName> {
        HeaderName::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

impl AsRef<str> for HeaderName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<[u8]> for HeaderName {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl Borrow<str> for HeaderName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl std::fmt::Display for HeaderName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", &self.inner)
    }
}

impl<'de> Deserialize<'de> for HeaderName {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Serialize, Deserialize)]
        #[serde(untagged)]
        enum Helper {
            S(String),
            B(Vec<u8>),
        }
        if let Ok(s) = <Helper>::deserialize(deserializer) {
            Self::from_bytes(match &s {
                Helper::S(v) => v.as_bytes(),
                Helper::B(v) => v.as_slice(),
            })
            .map_err(|_| de::Error::custom("invalid header name value"))
        } else {
            Err(de::Error::custom("invalid header name value"))
        }
    }
}

impl Serialize for HeaderName {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl InvalidHeaderName {
    const fn new() -> InvalidHeaderName {
        InvalidHeaderName
    }
}

impl<'a> From<&'a HeaderName> for HeaderName {
    fn from(src: &'a HeaderName) -> Self {
        src.clone()
    }
}

impl From<&HeaderName> for Cow<'static, str> {
    fn from(src: &HeaderName) -> Self {
        match src.inner {
            Repr::Standard(s) => Cow::Borrowed(s.as_str()),
            Repr::Custom(_) => Cow::Owned(src.to_string()),
        }
    }
}

impl<'a> TryFrom<&'a str> for HeaderName {
    type Error = InvalidHeaderName;
    #[inline]
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

impl<'a> TryFrom<&'a String> for HeaderName {
    type Error = InvalidHeaderName;
    #[inline]
    fn try_from(s: &'a String) -> Result<Self, Self::Error> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

impl<'a> TryFrom<&'a [u8]> for HeaderName {
    type Error = InvalidHeaderName;
    #[inline]
    fn try_from(s: &'a [u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(s).map_err(|_| InvalidHeaderName::new())
    }
}

impl TryFrom<String> for HeaderName {
    type Error = InvalidHeaderName;

    #[inline]
    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::from_bytes(s.as_bytes()).map_err(|_| InvalidHeaderName::new())
    }
}

impl TryFrom<Vec<u8>> for HeaderName {
    type Error = InvalidHeaderName;

    #[inline]
    fn try_from(vec: Vec<u8>) -> Result<Self, Self::Error> {
        Self::from_bytes(&vec).map_err(|_| InvalidHeaderName::new())
    }
}

#[doc(hidden)]
impl From<StandardHeader> for HeaderName {
    fn from(src: StandardHeader) -> HeaderName {
        HeaderName {
            inner: Repr::Standard(src),
        }
    }
}

#[doc(hidden)]
impl From<Custom> for HeaderName {
    fn from(src: Custom) -> HeaderName {
        HeaderName {
            inner: Repr::Custom(src),
        }
    }
}

impl<'a> PartialEq<&'a HeaderName> for HeaderName {
    #[inline]
    fn eq(&self, other: &&'a HeaderName) -> bool {
        *self == **other
    }
}

impl<'a> PartialEq<HeaderName> for &'a HeaderName {
    #[inline]
    fn eq(&self, other: &HeaderName) -> bool {
        *other == *self
    }
}

impl PartialEq<str> for HeaderName {
    /// Performs a case-insensitive comparison of the string against the header
    /// name
    ///
    /// # Examples
    ///
    /// ```
    /// use melib::email::headers::HeaderName;
    ///
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "content-length");
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "Content-Length");
    /// assert_ne!(HeaderName::CONTENT_LENGTH, "content length");
    /// ```
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<HeaderName> for str {
    /// Performs a case-insensitive comparison of the string against the header
    /// name
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use melib::email::headers::HeaderName;
    ///
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "content-length");
    /// assert_eq!(HeaderName::CONTENT_LENGTH, "Content-Length");
    /// assert_ne!(HeaderName::CONTENT_LENGTH, "content length");
    /// assert_eq!(
    ///     HeaderName::CONTENT_LENGTH,
    ///     HeaderName::try_from("content-length").unwrap()
    /// );
    /// ```
    #[inline]
    fn eq(&self, other: &HeaderName) -> bool {
        *other == *self
    }
}

impl<'a> PartialEq<&'a str> for HeaderName {
    /// Performs a case-insensitive comparison of the string against the header
    /// name
    #[inline]
    fn eq(&self, other: &&'a str) -> bool {
        *self == **other
    }
}

impl<'a> PartialEq<HeaderName> for &'a str {
    /// Performs a case-insensitive comparison of the string against the header
    /// name
    #[inline]
    fn eq(&self, other: &HeaderName) -> bool {
        *other == *self
    }
}

impl Hash for Custom {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for b in self.0.as_slice() {
            hasher.write_u8(b.to_ascii_lowercase())
        }
    }
}
const UPPERCASE_TOKENS: &[&str] = &[
    "ARC", "DKIM", "DL", "EDIINT", "ID", "IPMS", "MD5", "MIME", "MT", "MTS", "NNTP", "PICS", "RSS",
    "SIO", "SPF", "TLS", "VBR",
];

impl std::fmt::Display for Custom {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let as_str = self.as_str();
        let len = as_str.len();
        let mut bytes_count = 0;
        for chunk in as_str.split('-') {
            if let Ok(tok) = UPPERCASE_TOKENS.binary_search_by(|probe| {
                if probe.eq_ignore_ascii_case(chunk) {
                    Ordering::Equal
                } else {
                    let mut iter = AsciiIgnoreCaseCmp {
                        ord: Ordering::Equal,
                        a: probe.as_bytes(),
                        b: chunk.as_bytes(),
                    };
                    let _cnt: usize = iter.by_ref().fuse().count();
                    debug_assert!(
                        _cnt <= probe.len(),
                        "_cnt {} should be lte probe.len() {}, for probe {} and chunk {}",
                        _cnt,
                        probe.len(),
                        probe,
                        chunk
                    );
                    debug_assert!(
                        _cnt <= chunk.len(),
                        "_cnt {} should be lte chunk.len() {}, for probe {} and chunk {}",
                        _cnt,
                        chunk.len(),
                        probe,
                        chunk
                    );
                    iter.ord
                }
            }) {
                write!(fmt, "{}", UPPERCASE_TOKENS[tok])?;
            } else {
                if let Some(first) = chunk.chars().next() {
                    write!(fmt, "{}", first.to_ascii_uppercase())?;
                }
                for ch in chunk.chars().skip(1) {
                    write!(fmt, "{}", ch.to_ascii_lowercase())?
                }
            }
            bytes_count += chunk.len();
            if bytes_count != len {
                bytes_count += 1;
                write!(fmt, "-")?;
            }
        }
        Ok(())
    }
}

// an iterator which alternates between Some and None
struct AsciiIgnoreCaseCmp<'a, 'b> {
    ord: Ordering,
    a: &'a [u8],
    b: &'b [u8],
}

impl<'a, 'b> Iterator for AsciiIgnoreCaseCmp<'a, 'b> {
    type Item = ();

    fn next(&mut self) -> Option<()> {
        match (self.a.get(0), self.b.get(0)) {
            (Some(a_char), Some(b_char)) => {
                self.ord = a_char
                    .to_ascii_lowercase()
                    .cmp(&b_char.to_ascii_lowercase());
                self.a = &self.a[1..];
                self.b = &self.b[1..];
                if self.ord == Ordering::Equal {
                    Some(())
                } else {
                    None
                }
            }
            (Some(_), None) => {
                self.ord = Ordering::Greater;
                None
            }
            (None, Some(_)) => {
                self.ord = Ordering::Less;
                None
            }
            (None, None) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_email_headers_headername_display() {
        assert_eq!(&HeaderName::SUBJECT.to_string(), "Subject");
        assert_eq!(&HeaderName::CC.to_string(), "Cc");
        assert_eq!(&HeaderName::IN_REPLY_TO.to_string(), "In-Reply-To");
        assert_eq!(
            &HeaderName::ORIGINAL_MESSAGE_ID.to_string(),
            "Original-Message-ID"
        );
        assert_eq!(
            &HeaderName::try_from("x-user-agent").unwrap().to_string(),
            "X-User-Agent"
        );
        assert_eq!(
            &HeaderName::try_from("arc-foobar").unwrap().to_string(),
            "ARC-Foobar"
        );
        assert_eq!(
            &HeaderName::try_from("x-rss-feed").unwrap().to_string(),
            "X-RSS-Feed"
        );
        assert_eq!(
            &HeaderName::try_from("With-regards-to").unwrap().to_string(),
            "With-Regards-To"
        );
        assert_eq!(
            &HeaderName::try_from("in-response-to-id")
                .unwrap()
                .to_string(),
            "In-Response-To-ID"
        );
        assert_eq!(
            &HeaderName::try_from("something-dKim").unwrap().to_string(),
            "Something-DKIM"
        );
    }

    #[test]
    fn test_email_headers_parse_standard_headers() {
        for &(std, name) in TEST_HEADERS {
            // Test lower case
            assert_eq!(
                HeaderName::from_bytes(name.to_ascii_lowercase().as_bytes()).unwrap(),
                HeaderName::from(std)
            );

            // Test upper case
            let upper = std::str::from_utf8(name.as_bytes())
                .expect("byte string constants are all utf-8")
                .to_uppercase();
            assert_eq!(
                HeaderName::from_bytes(upper.as_bytes()).unwrap(),
                HeaderName::from(std)
            );
        }
    }
}
