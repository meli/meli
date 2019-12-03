/*
 * meli - jmap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use super::*;
use crate::backends::jmap::protocol::*;
use crate::backends::jmap::rfc8620::bool_false;
use std::collections::HashMap;

// 4.1.1.
// Metadata
// These properties represent metadata about the message in the mail
// store and are not derived from parsing the message itself.
//
//   o  id: "Id" (immutable; server-set)
//
//      The id of the Email object.  Note that this is the JMAP object id,
//      NOT the Message-ID header field value of the message [RFC5322].
//
//   o  blobId: "Id" (immutable; server-set)
//
//      The id representing the raw octets of the message [RFC5322] for
//      this Email.  This may be used to download the raw original message
//      or to attach it directly to another Email, etc.
//
//   o  threadId: "Id" (immutable; server-set)
//
//      The id of the Thread to which this Email belongs.
//
//   o  mailboxIds: "Id[Boolean]"
//
//      The set of Mailbox ids this Email belongs to.  An Email in the
//      mail store MUST belong to one or more Mailboxes at all times
//      (until it is destroyed).  The set is represented as an object,
//      with each key being a Mailbox id.  The value for each key in the
//      object MUST be true.
//
//   o  keywords: "String[Boolean]" (default: {})
//
//      A set of keywords that apply to the Email.  The set is represented
//      as an object, with the keys being the keywords.  The value for
//      each key in the object MUST be true.
//
//      Keywords are shared with IMAP.  The six system keywords from IMAP
//      get special treatment.  The following four keywords have their
//      first character changed from "\" in IMAP to "$" in JMAP and have
//      particular semantic meaning:
//
//      *  "$draft": The Email is a draft the user is composing.
//
//      *  "$seen": The Email has been read.
//
//      *  "$flagged": The Email has been flagged for urgent/special
//         attention.
//
//      *  "$answered": The Email has been replied to.
//
//      The IMAP "\Recent" keyword is not exposed via JMAP.  The IMAP
//      "\Deleted" keyword is also not present: IMAP uses a delete+expunge
//      model, which JMAP does not.  Any message with the "\Deleted"
//      keyword MUST NOT be visible via JMAP (and so are not counted in
//      the "totalEmails", "unreadEmails", "totalThreads", and
//      "unreadThreads" Mailbox properties).
//
//      Users may add arbitrary keywords to an Email.  For compatibility
//      with IMAP, a keyword is a case-insensitive string of 1-255
//      characters in the ASCII subset %x21-%x7e (excludes control chars
//      and space), and it MUST NOT include any of these characters:
//
//                              ( ) { ] % * " \
//
//      Because JSON is case sensitive, servers MUST return keywords in
//      lowercase.
//
//      The IANA "IMAP and JMAP Keywords" registry at
//      <https://www.iana.org/assignments/imap-jmap-keywords/> as
//      established in [RFC5788] assigns semantic meaning to some other
//      keywords in common use.  New keywords may be established here in
//      the future.  In particular, note:
//
//      *  "$forwarded": The Email has been forwarded.
//
//      *  "$phishing": The Email is highly likely to be phishing.
//         Clients SHOULD warn users to take care when viewing this Email
//         and disable links and attachments.
//
//      *  "$junk": The Email is definitely spam.  Clients SHOULD set this
//         flag when users report spam to help train automated spam-
//         detection systems.
//
//      *  "$notjunk": The Email is definitely not spam.  Clients SHOULD
//         set this flag when users indicate an Email is legitimate, to
//         help train automated spam-detection systems.
//
//   o  size: "UnsignedInt" (immutable; server-set)
//
//      The size, in octets, of the raw data for the message [RFC5322] (as
//      referenced by the "blobId", i.e., the number of octets in the file
//      the user would download).
//
//   o  receivedAt: "UTCDate" (immutable; default: time of creation on
//      server)
//
//      The date the Email was received by the message store.  This is the
//      "internal date" in IMAP [RFC3501]./

#[derive(Deserialize, Serialize, Debug)]
pub struct EmailObject {
    pub id: Id,
    pub blob_id: Id,
    pub thread_id: Id,
    pub mailbox_ids: HashMap<Id, bool>,
    pub keywords: HashMap<String, bool>,
    pub size: u64,
    pub received_at: String,
}

impl Object for EmailObject {}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct EmailQueryResponse {
    pub account_id: Id,
    pub can_calculate_changes: bool,
    pub collapse_threads: bool,
    // FIXME
    pub filter: String,
    pub ids: Vec<Id>,
    pub position: u64,
    pub query_state: String,
    pub sort: Option<String>,
    pub total: usize,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct EmailQueryCall {
    pub filter: EmailFilterCondition, /* "inMailboxes": [ folder.id ] },*/
    pub collapse_threads: bool,
    pub position: u64,
    pub fetch_threads: bool,
    pub fetch_messages: bool,
    pub fetch_message_properties: Vec<MessageProperty>,
}

impl Method<EmailObject> for EmailQueryCall {
    const NAME: &'static str = "Email/query";
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct EmailGetCall {
    #[serde(flatten)]
    pub get_call: GetCall<EmailObject>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub body_properties: Vec<String>,
    #[serde(default = "bool_false")]
    pub fetch_text_body_values: bool,
    #[serde(default = "bool_false")]
    pub fetch_html_body_values: bool,
    #[serde(default = "bool_false")]
    pub fetch_all_body_values: bool,
    #[serde(default)]
    pub max_body_value_bytes: u64,
}

impl Method<EmailObject> for EmailGetCall {
    const NAME: &'static str = "Email/get";
}

impl EmailGetCall {
    pub fn new(get_call: GetCall<EmailObject>) -> Self {
        EmailGetCall {
            get_call,
            body_properties: Vec::new(),
            fetch_text_body_values: false,
            fetch_html_body_values: false,
            fetch_all_body_values: false,
            max_body_value_bytes: 0,
        }
    }

    _impl!(get_call: GetCall<EmailObject>);
    _impl!(body_properties: Vec<String>);
    _impl!(fetch_text_body_values: bool);
    _impl!(fetch_html_body_values: bool);
    _impl!(fetch_all_body_values: bool);
    _impl!(max_body_value_bytes: u64);
}

#[derive(Serialize, Deserialize, Default, Debug)]
#[serde(rename_all = "camelCase")]
pub struct EmailFilterCondition {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub in_mailboxes: Vec<Id>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub in_mailbox_other_than: Vec<Id>,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub before: UtcDate,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub after: UtcDate,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_size: Option<u64>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_size: Option<u64>,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub all_in_thread_have_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub some_in_thread_have_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub none_in_thread_have_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub has_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub not_keyword: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub has_attachment: Option<bool>,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub text: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub from: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub to: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub cc: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub bcc: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub subject: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub body: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub header: Vec<String>,
}

impl EmailFilterCondition {
    _impl!(in_mailboxes: Vec<Id>);
    _impl!(in_mailbox_other_than: Vec<Id>);
    _impl!(before: UtcDate);
    _impl!(after: UtcDate);
    _impl!(min_size: Option<u64>);
    _impl!(max_size: Option<u64>);
    _impl!(all_in_thread_have_keyword: String);
    _impl!(some_in_thread_have_keyword: String);
    _impl!(none_in_thread_have_keyword: String);
    _impl!(has_keyword: String);
    _impl!(not_keyword: String);
    _impl!(has_attachment: Option<bool>);
    _impl!(text: String);
    _impl!(from: String);
    _impl!(to: String);
    _impl!(cc: String);
    _impl!(bcc: String);
    _impl!(subject: String);
    _impl!(body: String);
    _impl!(header: Vec<String>);
}

impl FilterTrait<EmailObject> for EmailFilterCondition {}

// The following convenience properties are also specified for the Email
//    object:
//
//    o  messageId: "String[]|null" (immutable)
//
//       The value is identical to the value of "header:Message-
//       ID:asMessageIds".  For messages conforming to RFC 5322, this will
//       be an array with a single entry.
//
//    o  inReplyTo: "String[]|null" (immutable)
//
//       The value is identical to the value of "header:In-Reply-
//       To:asMessageIds".
//
//    o  references: "String[]|null" (immutable)
//
//       The value is identical to the value of
//       "header:References:asMessageIds".
//
//    o  sender: "EmailAddress[]|null" (immutable)
//
//       The value is identical to the value of
//       "header:Sender:asAddresses".
//
//    o  from: "EmailAddress[]|null" (immutable)
//
//       The value is identical to the value of "header:From:asAddresses".
//
//    o  to: "EmailAddress[]|null" (immutable)
//
//       The value is identical to the value of "header:To:asAddresses".
//
//    o  cc: "EmailAddress[]|null" (immutable)
//
//       The value is identical to the value of "header:Cc:asAddresses".
//
//    o  bcc: "EmailAddress[]|null" (immutable)
//
//       The value is identical to the value of "header:Bcc:asAddresses".
//
//    o  replyTo: "EmailAddress[]|null" (immutable)
//
//       The value is identical to the value of "header:Reply-
//       To:asAddresses".
//
//    o  subject: "String|null" (immutable)
//
//       The value is identical to the value of "header:Subject:asText".
//
//
//
// Jenkins & Newman             Standards Track                   [Page 34]
//
// RFC 8621                        JMAP Mail                    August 2019
//
//
//    o  sentAt: "Date|null" (immutable; default on creation: current
//       server time)
//
//       The value is identical to the value of "header:Date:asDate".
