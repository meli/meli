/*
 * meli - headers
 *
 * Copyright 2020 Manos Pitsidianakis
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

//! Standard header names, associated with standards which define them.
//!
//! This module exposes the types [`Protocol`], [`Standard`] and
//! [`StandardHeader`].

use super::names::*;

bitflags! {
    /// A protocol associated with a standard e-mail header.
    #[derive(Default, Serialize, Deserialize)]
    pub struct Protocol: u32 {
        const None    =  0b00000001;
        const Mail    =  Self::None.bits() << 1;
        const NNTP    =  Self::Mail.bits() << 1;
        const MIME    =  Self::NNTP.bits() << 1;
        const Mbox    =  Self::MIME.bits() << 1;
    }
}

macro_rules! standard_headers {
    (
        $(
            $(#[$docs:meta])*
            ($konst:ident, $upcase:ident, $name:literal, $lowercase_name:literal, $template:expr, $(Protocol::$var:tt)|+,$status:expr,$standards:expr);
        )+
    ) => {
        /// An enumerator type over statically encoded header names.
        ///
        /// Each variant value corresponds to an associated constant exposing it as a
        /// [`HeaderName`] under both [`StandardHeader`] and [`HeaderName`] types.
        #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
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
            /// Returns a `str` representation of the header.
            ///
            /// The returned string will always be lower case.
            #[inline]
            #[expect(clippy::string_lit_as_bytes)]
            pub const fn as_str(&self) -> &'static str {
                match *self {
                    $(
                        Self::$konst => {
                            const _: () = {
                                let mut i = 0;
                                while i < $lowercase_name.as_bytes().len() {
                                    if $lowercase_name.as_bytes()[i] == b'-' {
                                    } else if $lowercase_name.as_bytes()[i].is_ascii_digit() {
                                    } else if !$lowercase_name.as_bytes()[i].is_ascii_lowercase() { panic!("{}", $lowercase_name) }
                                    i+=1;
                                }
                            };
                            $lowercase_name
                        }
                    )+
                }
            }

            /// Returns a cased `str` representation of the header.
            #[inline]
            pub const fn as_cased_str(&self) -> &'static str {
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

            /// Returns the deprecation status of this header.
            #[inline]
            pub const fn status(&self) -> Status {
                match *self {
                    $(
                        Self::$konst => $status,
                    )+
                }
            }

            /// Returns which standards the definition of this header was lifted from.
            #[inline]
            pub const fn standards(&self) -> &[Standard] {
                match *self {
                    $(
                        Self::$konst => $standards,
                    )+
                }
            }

            // invalid clippy lint match here
            #[allow(clippy::string_lit_as_bytes)]
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
        pub(super) const TEST_HEADERS: &[(StandardHeader, &str)] = &[
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
            ($konst:ident, $name:literal );
        )+
    ) => {
        /// An enumerator type over known e-mail standards.
        ///
        /// Each variant value also corresponds to an associated constant.
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub enum Standard {
            $(
                $konst,
            )+
        }

        $(
            $(#[$docs])*
            pub const $konst: Standard = Standard::$konst;
        )+

        impl Standard {
            #[inline]
            pub const fn as_str(&self) -> &'static str {
                match *self {
                    $(
                        Self::$konst => concat!("RFC", $name),
                    )+
                }
            }

            /// Returns the associated standard URL on the IETF data-tracker website.
            ///
            /// It's value is of the form `https://datatracker.ietf.org/doc/html/rfcN.html` where
            /// `N` is the Request for Comments publication number.
            #[inline]
            pub const fn url(&self) -> &str {
                match *self {
                    $(
                        Self::$konst => concat!("https://datatracker.ietf.org/doc/html/rfc", $name, ".html"),
                    )+
                }

            }

            // invalid clippy lint match here
            #[allow(clippy::string_lit_as_bytes)]
            pub fn from_bytes(name_bytes: &[u8]) -> Option<Self> {
                match name_bytes {
                    $(
                        _ if name_bytes.eq_ignore_ascii_case($konst.as_str().as_bytes()) => Some(Self::$konst),
                    )+
                        _ => None,
                }
            }
        }
    };
}

standards! {
    (RFC0850, "0850");
    (RFC1808, "1808");
    (RFC1849, "1849");
    (RFC2068, "2068");
    (RFC2076, "2076");
    (RFC2110, "2110");
    (RFC2156, "2156");
    (RFC2183, "2183");
    (RFC2557, "2557");
    (RFC2616, "2616");
    (RFC2980, "2980");
    (RFC3798, "3798");
    (RFC3834, "3834");
    (RFC3865, "3865");
    (RFC3977, "3977");
    (RFC4021, "4021");
    (RFC5064, "5064");
    (RFC5321, "5321");
    (RFC5322, "5322");
    (RFC5337, "5337");
    (RFC5504, "5504");
    (RFC5518, "5518");
    (RFC5536, "5536");
    (RFC5537, "5537");
    (RFC5703, "5703");
    (RFC6017, "6017");
    (RFC6068, "6068");
    (RFC6109, "6109");
    (RFC6376, "6376");
    (RFC6477, "6477");
    (RFC6758, "6758");
    (RFC6854, "6854");
    (RFC6857, "6857");
    (RFC7208, "7208");
    (RFC7259, "7259");
    (RFC7293, "7293");
    (RFC7444, "7444");
    (RFC7681, "7681");
    (RFC8058, "8058");
    (RFC8255, "8255");
    (RFC8315, "8315");
    (RFC8460, "8460");
    (RFC8601, "8601");
    (RFC8617, "8617");
    (RFC8689, "8689");
    (RFC9057, "9057");
    (RFC9228, "9228");
}

/// Status of header name field at the moment of writing.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
    (Subject,                             SUBJECT,                                "Subject","subject",                                None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (ReplyTo,                             REPLY_TO,                               "Reply-To","reply-to",                               None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (InReplyTo,                           IN_REPLY_TO,                            "In-Reply-To","in-reply-to",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (References,                          REFERENCES,                             "References","references",                             None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (MailReplyTo,                         MAIL_REPLY_TO,                          "Mail-Reply-To","mail-reply-to",                          None,                           Protocol::Mail,                   Status::None,          &[]);
    (MailFollowupTo,                      MAIL_FOLLOWUP_TO,                       "Mail-Followup-To","mail-followup-to",                       None,                           Protocol::Mail,                   Status::None,          &[]);
    (DeliveredTo,                         DELIVERED_TO,                           "Delivered-To","delivered-to",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC9228]);
    (Comments,                            COMMENTS,                               "Comments","comments",                               None,                           Protocol::Mail,                   Status::None,          &[]);
    (Keywords,                            KEYWORDS,                               "Keywords","keywords",                               None,                           Protocol::Mail,                   Status::None,          &[]);
    (Received,                            RECEIVED,                               "Received","received",                               None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322, Standard::RFC5321]);
    (ContentLanguage,                     CONTENT_LANGUAGE,                       "Content-Language","content-language",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentLength,                       CONTENT_LENGTH,                         "Content-Length","content-length",                         None,                           Protocol::Mail,                   Status::None,          &[]);
    (Forwarded,                           FORWARDED,                              "Forwarded","forwarded",                              None,                           Protocol::Mail,                   Status::None,          &[]);
    (AcceptLanguage,                      ACCEPT_LANGUAGE,                        "Accept-Language","accept-language",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (AlsoControl,                         ALSO_CONTROL,                           "Also-Control","also-control",                           None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (AlternateRecipient,                  ALTERNATE_RECIPIENT,                    "Alternate-Recipient","alternate-recipient",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Approved,                            APPROVED,                               "Approved","approved",                               None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (ArcAuthenticationResults,            ARC_AUTHENTICATION_RESULTS,             "ARC-Authentication-Results","arc-authentication-results",             None,                           Protocol::Mail,                   Status::Experimental,  &[Standard::RFC8617]);
    (ArcMessageSignature,                 ARC_MESSAGE_SIGNATURE,                  "ARC-Message-Signature","arc-message-signature",                  None,                           Protocol::Mail,                   Status::Experimental,  &[Standard::RFC8617]);
    (ArcSeal,                             ARC_SEAL,                               "ARC-Seal","arc-seal",                               None,                           Protocol::Mail,                   Status::Experimental,  &[Standard::RFC8617]);
    (Archive,                             ARCHIVE,                                "Archive","archive",                                None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (ArchivedAt,                          ARCHIVED_AT,                            "Archived-At","archived-at",                            None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5064]);
    (ArticleNames,                        ARTICLE_NAMES,                          "Article-Names","article-names",                          None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (ArticleUpdates,                      ARTICLE_UPDATES,                        "Article-Updates","article-updates",                        None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (AuthenticationResults,               AUTHENTICATION_RESULTS,                 "Authentication-Results","authentication-results",                 None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8601]);
    (AutoSubmitted,                       AUTO_SUBMITTED,                         "Auto-Submitted","auto-submitted",                         None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC3834]);
    (Autoforwarded,                       AUTOFORWARDED,                          "Autoforwarded","autoforwarded",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Autosubmitted,                       AUTOSUBMITTED,                          "Autosubmitted","autosubmitted",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Base,                                BASE,                                   "Base","base",                                   None,                           Protocol::MIME,                   Status::Obsoleted,     &[Standard::RFC1808, Standard::RFC2068]);
    (Bcc,                                 BCC,                                    "Bcc","bcc",                                    None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (Body,                                BODY,                                   "Body","body",                                   None,                           Protocol::None,                   Status::Reserved,      &[Standard::RFC6068]);
    (CancelKey,                           CANCEL_KEY,                             "Cancel-Key","cancel-key",                             None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC8315]);
    (CancelLock,                          CANCEL_LOCK,                            "Cancel-Lock","cancel-lock",                            None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC8315]);
    (Cc,                                  CC,                                     "Cc","cc",                                     None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ContentAlternative,                  CONTENT_ALTERNATIVE,                    "Content-Alternative","content-alternative",                    None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentBase,                         CONTENT_BASE,                           "Content-Base","content-base",                           None,                           Protocol::MIME,                   Status::Obsoleted,     &[Standard::RFC2110, Standard::RFC2557]);
    (ContentDescription,                  CONTENT_DESCRIPTION,                    "Content-Description","content-description",                    None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentDisposition,                  CONTENT_DISPOSITION,                    "Content-Disposition","content-disposition",                    None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC2183, Standard::RFC4021]);
    (ContentDuration,                     CONTENT_DURATION,                       "Content-Duration","content-duration",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentFeatures,                     CONTENT_FEATURES,                       "Content-Features","content-features",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentId,                           CONTENT_ID,                             "Content-ID","content-id",                             None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentIdentifier,                   CONTENT_IDENTIFIER,                     "Content-Identifier","content-identifier",                     None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ContentLocation,                     CONTENT_LOCATION,                       "Content-Location","content-location",                       None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentMd5,                          CONTENT_MD5,                            "Content-MD5","content-md5",                            None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentReturn,                       CONTENT_RETURN,                         "Content-Return","content-return",                         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ContentTransferEncoding,             CONTENT_TRANSFER_ENCODING,              "Content-Transfer-Encoding","content-transfer-encoding",              None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (ContentTranslationType,              CONTENT_TRANSLATION_TYPE,               "Content-Translation-Type","content-translation-type",               None,                           Protocol::MIME,                   Status::Standard,      &[Standard::RFC8255]);
    (ContentType,                         CONTENT_TYPE,                           "Content-Type","content-type",                           None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (Control,                             CONTROL,                                "Control","control",                                None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (Conversion,                          CONVERSION,                             "Conversion","conversion",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ConversionWithLoss,                  CONVERSION_WITH_LOSS,                   "Conversion-With-Loss","conversion-with-loss",                   None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DlExpansionHistory,                  DL_EXPANSION_HISTORY,                   "DL-Expansion-History","dl-expansion-history",                   None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Date,                                DATE,                                   "Date","date",                                   None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5536, Standard::RFC5322]);
    (DateReceived,                        DATE_RECEIVED,                          "Date-Received","date-received",                          None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC0850, Standard::RFC5536]);
    (DeferredDelivery,                    DEFERRED_DELIVERY,                      "Deferred-Delivery","deferred-delivery",                      None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DeliveryDate,                        DELIVERY_DATE,                          "Delivery-Date","delivery-date",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DiscardedX400IpmsExtensions,         DISCARDED_X400_IPMS_EXTENSIONS,         "Discarded-X400-IPMS-Extensions","discarded-x400-ipms-extensions",         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DiscardedX400MtsExtensions,          DISCARDED_X400_MTS_EXTENSIONS,          "Discarded-X400-MTS-Extensions","discarded-x400-mts-extensions",          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DiscloseRecipients,                  DISCLOSE_RECIPIENTS,                    "Disclose-Recipients","disclose-recipients",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DispositionNotificationOptions,      DISPOSITION_NOTIFICATION_OPTIONS,       "Disposition-Notification-Options","disposition-notification-options",       None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (DispositionNotificationTo,           DISPOSITION_NOTIFICATION_TO,            "Disposition-Notification-To","disposition-notification-to",            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Distribution,                        DISTRIBUTION,                           "Distribution","distribution",                           None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (DkimSignature,                       DKIM_SIGNATURE,                         "DKIM-Signature","dkim-signature",                         None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6376]);
    (DowngradedBcc,                       DOWNGRADED_BCC,                         "Downgraded-Bcc","downgraded-bcc",                         None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedCc,                        DOWNGRADED_CC,                          "Downgraded-Cc","downgraded-cc",                          None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedDispositionNotificationTo, DOWNGRADED_DISPOSITION_NOTIFICATION_TO, "Downgraded-Disposition-Notification-To","downgraded-disposition-notification-to", None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedFinalRecipient,            DOWNGRADED_FINAL_RECIPIENT,             "Downgraded-Final-Recipient","downgraded-final-recipient",             None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedFrom,                      DOWNGRADED_FROM,                        "Downgraded-From","downgraded-from",                        None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedInReplyTo,                 DOWNGRADED_IN_REPLY_TO,                 "Downgraded-In-Reply-To","downgraded-in-reply-to",                 None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedMailFrom,                  DOWNGRADED_MAIL_FROM,                   "Downgraded-Mail-From","downgraded-mail-from",                   None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedMessageId,                 DOWNGRADED_MESSAGE_ID,                  "Downgraded-Message-Id","downgraded-message-id",                  None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedOriginalRecipient,         DOWNGRADED_ORIGINAL_RECIPIENT,          "Downgraded-Original-Recipient","downgraded-original-recipient",          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedRcptTo,                    DOWNGRADED_RCPT_TO,                     "Downgraded-Rcpt-To","downgraded-rcpt-to",                     None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedReferences,                DOWNGRADED_REFERENCES,                  "Downgraded-References","downgraded-references",                  None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6857]);
    (DowngradedReplyTo,                   DOWNGRADED_REPLY_TO,                    "Downgraded-Reply-To","downgraded-reply-to",                    None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentBcc,                 DOWNGRADED_RESENT_BCC,                  "Downgraded-Resent-Bcc","downgraded-resent-bcc",                  None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentCc,                  DOWNGRADED_RESENT_CC,                   "Downgraded-Resent-Cc","downgraded-resent-cc",                   None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentFrom,                DOWNGRADED_RESENT_FROM,                 "Downgraded-Resent-From","downgraded-resent-from",                 None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentReplyTo,             DOWNGRADED_RESENT_REPLY_TO,             "Downgraded-Resent-Reply-To","downgraded-resent-reply-to",             None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentSender,              DOWNGRADED_RESENT_SENDER,               "Downgraded-Resent-Sender","downgraded-resent-sender",               None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedResentTo,                  DOWNGRADED_RESENT_TO,                   "Downgraded-Resent-To","downgraded-resent-to",                   None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedReturnPath,                DOWNGRADED_RETURN_PATH,                 "Downgraded-Return-Path","downgraded-return-path",                 None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedSender,                    DOWNGRADED_SENDER,                      "Downgraded-Sender","downgraded-sender",                      None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (DowngradedTo,                        DOWNGRADED_TO,                          "Downgraded-To","downgraded-to",                          None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5504, Standard::RFC6857]);
    (Encoding,                            ENCODING,                               "Encoding","encoding",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Encrypted,                           ENCRYPTED,                              "Encrypted","encrypted",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Expires,                             EXPIRES,                                "Expires","expires",                                None,                           Protocol::Mail | Protocol::NNTP,  Status::None,          &[Standard::RFC4021, Standard::RFC5536]);
    (ExpiryDate,                          EXPIRY_DATE,                            "Expiry-Date","expiry-date",                            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (FollowupTo,                          FOLLOWUP_TO,                            "Followup-To","followup-to",                            None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (From,                                FROM,                                   "From","from",                                   None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (GenerateDeliveryReport,              GENERATE_DELIVERY_REPORT,               "Generate-Delivery-Report","generate-delivery-report",               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Importance,                          IMPORTANCE,                             "Importance","importance",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (IncompleteCopy,                      INCOMPLETE_COPY,                        "Incomplete-Copy","incomplete-copy",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (InjectionDate,                       INJECTION_DATE,                         "Injection-Date","injection-date",                         None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (InjectionInfo,                       INJECTION_INFO,                         "Injection-Info","injection-info",                         None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (Language,                            LANGUAGE,                               "Language","language",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (LatestDeliveryTime,                  LATEST_DELIVERY_TIME,                   "Latest-Delivery-Time","latest-delivery-time",                   None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Lines,                               LINES,                                  "Lines","lines",                                  None,                           Protocol::NNTP,                   Status::Deprecated,    &[Standard::RFC5536, Standard::RFC3977]);
    (ListArchive,                         LIST_ARCHIVE,                           "List-Archive","list-archive",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListHelp,                            LIST_HELP,                              "List-Help","list-help",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListId,                              LIST_ID,                                "List-ID","list-id",                                None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListOwner,                           LIST_OWNER,                             "List-Owner","list-owner",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListPost,                            LIST_POST,                              "List-Post","list-post",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListSubscribe,                       LIST_SUBSCRIBE,                         "List-Subscribe","list-subscribe",                         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListUnsubscribe,                     LIST_UNSUBSCRIBE,                       "List-Unsubscribe","list-unsubscribe",                       Some("perm/list-unsubscribe"),  Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ListUnsubscribePost,                 LIST_UNSUBSCRIBE_POST,                  "List-Unsubscribe-Post","list-unsubscribe-post",                  None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8058]);
    (MessageContext,                      MESSAGE_CONTEXT,                        "Message-Context","message-context",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (MessageId,                           MESSAGE_ID,                             "Message-ID","message-id",                             None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5322, Standard::RFC5536]);
    (MessageType,                         MESSAGE_TYPE,                           "Message-Type","message-type",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (MimeVersion,                         MIME_VERSION,                           "MIME-Version","mime-version",                           None,                           Protocol::MIME,                   Status::None,          &[Standard::RFC4021]);
    (MtPriority,                          MT_PRIORITY,                            "MT-Priority","mt-priority",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC6758]);
    (Newsgroups,                          NEWSGROUPS,                             "Newsgroups","newsgroups",                             None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (NntpPostingDate,                     NNTP_POSTING_DATE,                      "NNTP-Posting-Date","nntp-posting-date",                      None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC5536]);
    (NntpPostingHost,                     NNTP_POSTING_HOST,                      "NNTP-Posting-Host","nntp-posting-host",                      None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC2980, Standard::RFC5536]);
    (Obsoletes,                           OBSOLETES,                              "Obsoletes","obsoletes",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Organization,                        ORGANIZATION,                           "Organization","organization",                           None,                           Protocol::Mail | Protocol::NNTP,  Status::Informational, &[Standard::RFC7681,   Standard::RFC5536]);
    (OriginalEncodedInformationTypes,     ORIGINAL_ENCODED_INFORMATION_TYPES,     "Original-Encoded-Information-Types","original-encoded-information-types",     None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (OriginalFrom,                        ORIGINAL_FROM,                          "Original-From","original-from",                          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5703]);
    (OriginalMessageId,                   ORIGINAL_MESSAGE_ID,                    "Original-Message-ID","original-message-id",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (OriginalRecipient,                   ORIGINAL_RECIPIENT,                     "Original-Recipient","original-recipient",                     Some("perm/original-recipient"),Protocol::Mail,                   Status::Standard,      &[Standard::RFC3798, Standard::RFC5337]);
    (OriginalSender,                      ORIGINAL_SENDER,                        "Original-Sender","original-sender",                        None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5537]);
    (OriginatorReturnAddress,             ORIGINATOR_RETURN_ADDRESS,              "Originator-Return-Address","originator-return-address",              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (OriginalSubject,                     ORIGINAL_SUBJECT,                       "Original-Subject","original-subject",                       None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5703]);
    (Path,                                PATH,                                   "Path","path",                                   None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (PicsLabel,                           PICS_LABEL,                             "PICS-Label","pics-label",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (PostingVersion,                      POSTING_VERSION,                        "Posting-Version","posting-version",                        None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC0850, Standard::RFC5536]);
    (PreventNondeliveryReport,            PREVENT_NONDELIVERY_REPORT,             "Prevent-NonDelivery-Report","prevent-non-delivery-report",             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Priority,                            PRIORITY,                               "Priority","priority",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (ReceivedSpf,                         RECEIVED_SPF,                           "Received-SPF","received-spf",                           None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC7208]);
    (RelayVersion,                        RELAY_VERSION,                          "Relay-Version","relay-version",                          None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC0850, Standard::RFC5536]);
    (ReplyBy,                             REPLY_BY,                               "Reply-By","reply-by",                               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (RequireRecipientValidSince,          REQUIRE_RECIPIENT_VALID_SINCE,          "Require-Recipient-Valid-Since","require-recipient-valid-since",          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC7293]);
    (ResentBcc,                           RESENT_BCC,                             "Resent-Bcc","resent-bcc",                             None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentCc,                            RESENT_CC,                              "Resent-Cc","resent-cc",                              None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentDate,                          RESENT_DATE,                            "Resent-Date","resent-date",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentFrom,                          RESENT_FROM,                            "Resent-From","resent-from",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (ResentMessageId,                     RESENT_MESSAGE_ID,                      "Resent-Message-ID","resent-message-id",                      None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ResentReplyTo,                       RESENT_REPLY_TO,                        "Resent-Reply-To","resent-reply-to",                        None,                           Protocol::Mail,                   Status::Obsoleted,     &[Standard::RFC5322]);
    (ResentSender,                        RESENT_SENDER,                          "Resent-Sender","resent-sender",                          None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (ResentTo,                            RESENT_TO,                              "Resent-To","resent-to",                              None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (ReturnPath,                          RETURN_PATH,                            "Return-Path","return-path",                            None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (SeeAlso,                             SEE_ALSO,                               "See-Also","see-also",                               None,                           Protocol::NNTP,                   Status::Obsoleted,     &[Standard::RFC1849, Standard::RFC5536]);
    (Sender,                              SENDER,                                 "Sender","sender",                                 None,                           Protocol::Mail | Protocol::NNTP,  Status::Standard,      &[Standard::RFC5322, Standard::RFC6854]);
    (Sensitivity,                         SENSITIVITY,                            "Sensitivity","sensitivity",                            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Solicitation,                        SOLICITATION,                           "Solicitation","solicitation",                           None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC3865]);
    (Status,                              STATUS,                                 "Status","status",                                 None,                           Protocol::Mbox,                   Status::None,          &[]);
    (Summary,                             SUMMARY,                                "Summary","summary",                                None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (Supersedes,                          SUPERSEDES,                             "Supersedes","supersedes",                             None,                           Protocol::Mail | Protocol::NNTP,  Status::None,          &[Standard::RFC5536, Standard::RFC2156]);
    (TlsReportDomain,                     TLS_REPORT_DOMAIN,                      "TLS-Report-Domain","tls-report-domain",                      None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8460]);
    (TlsReportSubmitter,                  TLS_REPORT_SUBMITTER,                   "TLS-Report-Submitter","tls-report-submitter",                   None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8460]);
    (TlsRequired,                         TLS_REQUIRED,                           "TLS-Required","tls-required",                           None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC8689]);
    (To,                                  TO,                                     "To","to",                                     None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5322]);
    (UserAgent,                           USER_AGENT,                             "User-Agent","user-agent",                             None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536, Standard::RFC2616]);
    (VbrInfo,                             VBR_INFO,                               "VBR-Info","vbr-info",                               None,                           Protocol::Mail,                   Status::Standard,      &[Standard::RFC5518]);
    (X400ContentIdentifier,               X400_CONTENT_IDENTIFIER,                "X400-Content-Identifier","x400-content-identifier",                None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400ContentReturn,                   X400_CONTENT_RETURN,                    "X400-Content-Return","x400-content-return",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400ContentType,                     X400_CONTENT_TYPE,                      "X400-Content-Type","x400-content-type",                      None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400MtsIdentifier,                   X400_MTS_IDENTIFIER,                    "X400-MTS-Identifier","x400-mts-identifier",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Originator,                      X400_ORIGINATOR,                        "X400-Originator","x400-originator",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Received,                        X400_RECEIVED,                          "X400-Received","x400-received",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Recipients,                      X400_RECIPIENTS,                        "X400-Recipients","x400-recipients",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (X400Trace,                           X400_TRACE,                             "X400-Trace","x400-trace",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC4021]);
    (Xref,                                XREF,                                   "Xref","xref",                                   None,                           Protocol::NNTP,                   Status::Standard,      &[Standard::RFC5536]);
    (ApparentlyTo,                        APPARENTLY_TO,                          "Apparently-To","apparently-to",                          Some("prov/apparently-to"),     Protocol::Mail,                   Status::None,          &[Standard::RFC2076]);
    (Author,                              AUTHOR,                                 "Author","author",                                 None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC9057]);
    (EdiintFeatures,                      EDIINT_FEATURES,                        "EDIINT-Features","ediint-features",                        None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6017]);
    (EesstVersion,                        EESST_VERSION,                          "Eesst-Version","eesst-version",                          None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC7681]);
    (ErrorsTo,                            ERRORS_TO,                              "Errors-To","errors-to",                              Some("prov/errors-to"),         Protocol::Mail,                   Status::None,          &[Standard::RFC2076]);
    (JabberId,                            JABBER_ID,                              "Jabber-ID","jabber-id",                              Some("prov/jabber-id"),         Protocol::Mail | Protocol::NNTP,  Status::None,          &[Standard::RFC7259]);
    (SioLabel,                            SIO_LABEL,                              "SIO-Label","sio-label",                              None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC7444]);
    (SioLabelHistory,                     SIO_LABEL_HISTORY,                      "SIO-Label-History","sio-label-history",                      None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC7444]);
    (XArchivedAt,                         X_ARCHIVED_AT,                          "X-Archived-At","x-archived-at",                          Some("prov/x-archived-at"),     Protocol::Mail | Protocol::NNTP,  Status::Deprecated,    &[Standard::RFC5064]);
    (XKeywords,                           X_KEYWORDS,                             "X-Keywords","x-keywords",                             None,                           Protocol::Mbox,                   Status::None,          &[]);
    (XMittente,                           X_MITTENTE,                             "X-Mittente","x-mittente",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XRicevuta,                           X_RICEVUTA,                             "X-Ricevuta","x-ricevuta",                             None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XRiferimentoMessageId,               X_RIFERIMENTO_MESSAGE_ID,               "X-Riferimento-Message-ID","x-riferimento-message-id",               None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XStatus,                             X_STATUS,                               "X-Status","x-status",                               None,                           Protocol::Mbox,                   Status::None,          &[]);
    (XTiporicevuta,                       X_TIPORICEVUTA,                         "X-TipoRicevuta","x-tipo-ricevuta",                         None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XTrasporto,                          X_TRASPORTO,                            "X-Trasporto","x-trasporto",                            None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
    (XVerificasicurezza,                  X_VERIFICASICUREZZA,                    "X-VerificaSicurezza","x-verifica-sicurezza",                    None,                           Protocol::Mail,                   Status::None,          &[Standard::RFC6109]);
}
