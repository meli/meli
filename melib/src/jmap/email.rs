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

use std::marker::PhantomData;

use indexmap::IndexMap;
use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, Serializer},
};
use serde_json::{value::RawValue, Value};
use smallvec::SmallVec;

use crate::{
    conf::false_val,
    email::{
        address::{Address, MailboxAddress},
        headers::HeaderName,
    },
    jmap::{
        comparator::Comparator,
        deserialize_from_str,
        filters::{Filter, FilterTrait},
        mailbox::MailboxObject,
        methods::{
            u64_zero, Changes, Get, Query, QueryChanges, QueryChangesResponse, ResultField, Set,
        },
        objects::{BlobObject, Id, Object},
        protocol::Method,
        thread::ThreadObject,
        UtcDate,
    },
    utils::datetime,
    EnvelopeHash, StrBuilder,
};

mod import;
pub use import::*;

impl Id<EmailObject> {
    pub fn into_hash(&self) -> EnvelopeHash {
        EnvelopeHash::from_bytes(self.inner.as_bytes())
    }
}

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
//      * "$draft": The Email is a draft the user is composing.
//
//      * "$seen": The Email has been read.
//
//      * "$flagged": The Email has been flagged for urgent/special attention.
//
//      * "$answered": The Email has been replied to.
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
//      * "$forwarded": The Email has been forwarded.
//
//      * "$phishing": The Email is highly likely to be phishing. Clients SHOULD
//        warn users to take care when viewing this Email and disable links and
//        attachments.
//
//      * "$junk": The Email is definitely spam.  Clients SHOULD set this flag
//        when users report spam to help train automated spam- detection
//        systems.
//
//      * "$notjunk": The Email is definitely not spam.  Clients SHOULD set this
//        flag when users indicate an Email is legitimate, to help train
//        automated spam-detection systems.
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

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailObject {
    #[serde(default)]
    pub id: Id<EmailObject>,
    #[serde(default)]
    pub blob_id: Id<BlobObject>,
    #[serde(default)]
    pub mailbox_ids: IndexMap<Id<MailboxObject>, bool>,
    #[serde(default)]
    pub size: u64,
    #[serde(default)]
    pub received_at: String,
    #[serde(default, deserialize_with = "deserialize_none_default")]
    pub message_id: Vec<String>,
    #[serde(default)]
    pub to: Option<SmallVec<[EmailAddress; 1]>>,
    #[serde(default)]
    pub bcc: Option<Vec<EmailAddress>>,
    #[serde(default)]
    pub reply_to: Option<Vec<EmailAddress>>,
    #[serde(default)]
    pub cc: Option<SmallVec<[EmailAddress; 1]>>,
    #[serde(default)]
    pub sender: Option<Vec<EmailAddress>>,
    #[serde(default)]
    pub from: Option<SmallVec<[EmailAddress; 1]>>,
    #[serde(default)]
    pub in_reply_to: Option<Vec<String>>,
    #[serde(default)]
    pub references: Option<Vec<String>>,
    #[serde(default)]
    pub keywords: IndexMap<String, bool>,
    #[serde(default)]
    pub attached_emails: Option<Id<BlobObject>>,
    #[serde(default)]
    pub attachments: Vec<Value>,
    #[serde(default)]
    pub has_attachment: bool,
    #[serde(default)]
    #[serde(
        deserialize_with = "deserialize_header",
        serialize_with = "serialize_header"
    )]
    pub headers: IndexMap<String, String>,
    #[serde(default)]
    pub html_body: Vec<EmailBodyPart>,
    #[serde(default)]
    pub preview: Option<String>,
    #[serde(default)]
    pub sent_at: Option<String>,
    #[serde(default)]
    pub subject: Option<String>,
    #[serde(default)]
    pub text_body: Vec<EmailBodyPart>,
    #[serde(default)]
    pub thread_id: Id<ThreadObject>,
    #[serde(flatten)]
    pub extra: IndexMap<String, Value>,
}

/// Deserializer that uses `Default::default()` in place of a present but `null`
/// value. Note that `serde(default)` doesn't apply if the key is present but
/// has a value of `null`.
fn deserialize_none_default<'de, D, T>(deserializer: D) -> std::result::Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de> + Default,
{
    let v = Option::<T>::deserialize(deserializer)?;
    Ok(v.unwrap_or_default())
}

impl EmailObject {
    _impl!(get keywords, keywords: IndexMap<String, bool>);
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Header {
    pub name: String,
    pub value: String,
}

fn deserialize_header<'de, D>(
    deserializer: D,
) -> std::result::Result<IndexMap<String, String>, D::Error>
where
    D: Deserializer<'de>,
{
    let v = <Vec<Header>>::deserialize(deserializer)?;
    Ok(v.into_iter().map(|t| (t.name, t.value)).collect())
}

fn serialize_header<S>(
    value: &IndexMap<String, String>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    value
        .iter()
        .map(|(n, v)| (n.clone(), v.clone()))
        .collect::<Vec<_>>()
        .serialize(serializer)
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailAddress {
    pub email: String,
    pub name: Option<String>,
}

impl From<crate::email::Address> for EmailAddress {
    fn from(val: crate::email::Address) -> Self {
        Self {
            email: val.get_email(),
            name: val.get_display_name(),
        }
    }
}

impl From<EmailAddress> for crate::email::Address {
    fn from(val: EmailAddress) -> Self {
        let EmailAddress { email, name } = val;
        crate::make_address!(name.unwrap_or_default(), email)
    }
}

impl std::fmt::Display for EmailAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.name.is_some() {
            write!(f, "{} <{}>", self.name.as_ref().unwrap(), &self.email)
        } else {
            write!(f, "{}", &self.email)
        }
    }
}

impl From<EmailObject> for crate::Envelope {
    fn from(mut t: EmailObject) -> Self {
        use crate::email::parser::{address::rfc2822address_list, dates::rfc5322_date};

        let mut env = Self::new(t.id.into_hash());
        if let Ok(d) = rfc5322_date(env.date_as_str().as_bytes()) {
            env.set_datetime(d);
        }
        if let Some(sent_at) = t.sent_at.take() {
            let unix = datetime::rfc3339_to_timestamp(sent_at.as_bytes()).unwrap_or(0);
            env.set_datetime(unix);
            env.set_date(sent_at.as_bytes());
        }

        if let Some(v) = t.message_id.first() {
            env.set_message_id(v.as_bytes());
        }
        if let Some(v) = t.headers.get(HeaderName::REFERENCES.as_str()) {
            env.set_references(v.as_bytes());
        }
        if let Some(ref in_reply_to) = t.in_reply_to {
            env.set_in_reply_to(in_reply_to[0].as_bytes());
            if let Some(in_reply_to) = env.in_reply_to().map(|r| r.as_ref().clone()) {
                env.push_references(&in_reply_to);
            }
        }
        if let Some(v) = t.headers.get(HeaderName::DATE.as_str()) {
            env.set_date(v.as_bytes());
            if let Ok(d) = rfc5322_date(v.as_bytes()) {
                env.set_datetime(d);
            }
        } else if let Ok(d) = rfc5322_date(t.received_at.as_bytes()) {
            env.set_datetime(d);
        }
        env.set_has_attachments(t.has_attachment);
        if let Some(subject) = t.subject.take() {
            env.set_subject(subject.into_bytes());
        }

        if let Some(from) = t.from.take() {
            env.set_from(
                from.into_iter()
                    .map(|addr| addr.into())
                    .collect::<SmallVec<[crate::email::Address; 1]>>(),
            );
        }
        if let Some(to) = t.to.take() {
            env.set_to(
                to.into_iter()
                    .map(|addr| addr.into())
                    .collect::<SmallVec<[crate::email::Address; 1]>>(),
            );
        }

        if let Some(cc) = t.cc.take() {
            env.set_cc(
                cc.into_iter()
                    .map(|addr| addr.into())
                    .collect::<SmallVec<[crate::email::Address; 1]>>(),
            );
        }

        if let Some(bcc) = t.bcc.take() {
            env.set_bcc(
                bcc.into_iter()
                    .map(|addr| addr.into())
                    .collect::<Vec<crate::email::Address>>(),
            );
        }

        if let Some(from) = t.headers.get(HeaderName::FROM.as_str()) {
            env.set_from(
                rfc2822address_list(from.as_bytes())
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
            );
        }
        if let Some(to) = t.headers.get(HeaderName::TO.as_str()) {
            env.set_to(
                rfc2822address_list(to.as_bytes())
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
            );
        }

        if let Some(cc) = t.headers.get(HeaderName::CC.as_str()) {
            env.set_cc(
                rfc2822address_list(cc.as_bytes())
                    .map(|(_, v)| v)
                    .unwrap_or_default(),
            );
        }

        if let Some(bcc) = t.headers.get(HeaderName::BCC.as_str()) {
            env.set_bcc(
                rfc2822address_list(bcc.as_bytes())
                    .map(|(_, v)| v)
                    .unwrap_or_default()
                    .to_vec(),
            );
        }

        if let (Some(ref mut r), message_id) = (&mut env.references, &env.message_id) {
            r.remove(message_id);
        }

        env
    }
}

impl From<&crate::Mail> for EmailObject {
    fn from(env: &crate::Mail) -> Self {
        Self {
            id: Id::new_random(),
            blob_id: Id::new_random(),
            mailbox_ids: IndexMap::new(),
            size: env.bytes.len().try_into().unwrap_or_default(),
            received_at: String::new(),
            message_id: vec![env.message_id().to_string()],
            to: if env.from.is_empty() {
                None
            } else {
                Some(env.from.iter().cloned().map(Into::into).collect())
            },
            bcc: None,
            reply_to: None,
            cc: None,
            sender: None,
            from: None,
            in_reply_to: None,
            references: None,
            keywords: IndexMap::new(),
            attached_emails: None,
            attachments: vec![],
            has_attachment: env.has_attachments,
            headers: IndexMap::new(),
            html_body: vec![],
            preview: None,
            sent_at: None,
            subject: None,
            text_body: vec![],
            thread_id: Id::new_random(),
            extra: IndexMap::new(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailBodyPart {
    #[serde(default)]
    pub part_id: Option<String>,
    #[serde(default)]
    pub blob_id: Option<Id<BlobObject>>,
    #[serde(default)]
    pub size: u64,
    #[serde(default)]
    pub headers: Value,
    #[serde(default)]
    pub name: Option<String>,
    pub r#type: String,
    #[serde(default)]
    pub charset: Option<String>,
    #[serde(default)]
    pub disposition: Option<String>,
    #[serde(default)]
    pub cid: Option<String>,
    #[serde(default)]
    pub language: Option<Vec<String>>,
    #[serde(default)]
    pub location: Option<String>,
    #[serde(default)]
    pub sub_parts: Vec<Value>,
}

impl Object for EmailObject {
    const NAME: &'static str = "Email";
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailQuery {
    #[serde(flatten)]
    pub query_call: Query<Filter<EmailFilterCondition, EmailObject>, EmailObject>,
    //pub filter: EmailFilterCondition, /* "inMailboxes": [ mailbox.id ] },*/
    pub collapse_threads: bool,
}

impl Method<EmailObject> for EmailQuery {
    const NAME: &'static str = "Email/query";
}

impl EmailQuery {
    pub const RESULT_FIELD_IDS: ResultField<Self, EmailObject> = ResultField::<Self, EmailObject> {
        field: "/ids",
        _ph: PhantomData,
    };

    pub fn new(
        mut query_call: Query<Filter<EmailFilterCondition, EmailObject>, EmailObject>,
    ) -> Self {
        if query_call.sort.is_none() {
            query_call.sort = Some(vec![Comparator::new("receivedAt".into())]);
        }
        Self {
            query_call,
            collapse_threads: false,
        }
    }

    _impl!(collapse_threads: bool);
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailGet {
    #[serde(flatten)]
    pub get_call: Get<EmailObject>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub body_properties: Vec<String>,
    #[serde(default = "false_val")]
    pub fetch_text_body_values: bool,
    #[serde(default = "false_val")]
    #[serde(rename = "fetchHTMLBodyValues")]
    pub fetch_html_body_values: bool,
    #[serde(default = "false_val")]
    pub fetch_all_body_values: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "u64_zero")]
    pub max_body_value_bytes: u64,
}

impl Method<EmailObject> for EmailGet {
    const NAME: &'static str = "Email/get";
}

impl EmailGet {
    pub fn new(get_call: Get<EmailObject>) -> Self {
        Self {
            get_call,
            body_properties: Vec::new(),
            fetch_text_body_values: false,
            fetch_html_body_values: false,
            fetch_all_body_values: false,
            max_body_value_bytes: 0,
        }
    }

    _impl!(body_properties: Vec<String>);
    _impl!(fetch_text_body_values: bool);
    _impl!(fetch_html_body_values: bool);
    _impl!(fetch_all_body_values: bool);
    _impl!(max_body_value_bytes: u64);
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailFilterCondition {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub in_mailbox: Option<Id<MailboxObject>>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub in_mailbox_other_than: Vec<Id<MailboxObject>>,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub before: UtcDate,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub after: UtcDate,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub min_size: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub max_size: Option<u64>,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub all_in_thread_have_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub some_in_thread_have_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub none_in_thread_have_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub has_keyword: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub not_keyword: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub has_attachment: Option<bool>,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub text: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub from: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub to: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub cc: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub bcc: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub subject: String,
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub body: String,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub header: Vec<Value>,
}

impl EmailFilterCondition {
    pub fn new() -> Self {
        Self::default()
    }

    _impl!(in_mailbox: Option<Id<MailboxObject>>);
    _impl!(in_mailbox_other_than: Vec<Id<MailboxObject>>);
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
    _impl!(header: Vec<Value>);
}

impl FilterTrait<EmailObject> for EmailFilterCondition {}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum MessageProperty {
    ThreadId,
    MailboxIds,
    Keywords,
    Size,
    ReceivedAt,
    IsUnread,
    IsFlagged,
    IsAnswered,
    IsDraft,
    HasAttachment,
    From,
    To,
    Cc,
    Bcc,
    ReplyTo,
    Subject,
    SentAt,
    Preview,
    Id,
    BlobId,
    MessageId,
    InReplyTo,
    Sender,
}

impl From<crate::search::Query> for Filter<EmailFilterCondition, EmailObject> {
    fn from(val: crate::search::Query) -> Self {
        let mut ret = Self::Condition(EmailFilterCondition::new());
        fn rec(q: &crate::search::Query, f: &mut Filter<EmailFilterCondition, EmailObject>) {
            use datetime::{formats::RFC3339_DATE, timestamp_to_string_utc};

            use crate::search::Query::*;

            match q {
                Subject(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().subject(t.clone()));
                }
                From(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().from(t.clone()));
                }
                To(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().to(t.clone()));
                }
                Cc(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().cc(t.clone()));
                }
                Bcc(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().bcc(t.clone()));
                }
                AllText(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().text(t.clone()));
                }
                Body(t) => {
                    *f = Filter::Condition(EmailFilterCondition::new().body(t.clone()));
                }
                Before(t) => {
                    *f = Filter::Condition(
                        EmailFilterCondition::new().before(timestamp_to_string_utc(
                            *t,
                            Some(RFC3339_DATE),
                            true,
                        )),
                    );
                }
                After(t) => {
                    *f = Filter::Condition(
                        EmailFilterCondition::new().after(timestamp_to_string_utc(
                            *t,
                            Some(RFC3339_DATE),
                            true,
                        )),
                    );
                }
                Between(a, b) => {
                    *f = Filter::Condition(
                        EmailFilterCondition::new().after(timestamp_to_string_utc(
                            *a,
                            Some(RFC3339_DATE),
                            true,
                        )),
                    );
                    *f &= Filter::Condition(
                        EmailFilterCondition::new().before(timestamp_to_string_utc(
                            *b,
                            Some(RFC3339_DATE),
                            true,
                        )),
                    );
                }
                On(t) => {
                    rec(&Between(*t, *t), f);
                }
                InReplyTo(ref s) => {
                    *f = Filter::Condition(EmailFilterCondition::new().header(vec![
                        HeaderName::IN_REPLY_TO.as_str().into(),
                        s.to_string().into(),
                    ]));
                }
                References(ref s) => {
                    *f = Filter::Condition(EmailFilterCondition::new().header(vec![
                        HeaderName::REFERENCES.as_str().into(),
                        s.to_string().into(),
                    ]));
                }
                Header(ref t, ref v) => {
                    *f = Filter::Condition(
                        EmailFilterCondition::new()
                            .header(vec![t.as_str().into(), v.to_string().into()]),
                    );
                }
                AllAddresses(_) => {
                    // [ref:TODO]: implement AllAddresses query for jmap
                }
                Flags(v) => {
                    fn flag_to_filter(f: &str) -> Filter<EmailFilterCondition, EmailObject> {
                        match f {
                            "draft" => Filter::Condition(
                                EmailFilterCondition::new().has_keyword("$draft".to_string()),
                            ),
                            "flagged" => Filter::Condition(
                                EmailFilterCondition::new().has_keyword("$flagged".to_string()),
                            ),
                            "seen" | "read" => Filter::Condition(
                                EmailFilterCondition::new().has_keyword("$seen".to_string()),
                            ),
                            "unseen" | "unread" => Filter::Condition(
                                EmailFilterCondition::new().not_keyword("$seen".to_string()),
                            ),
                            "answered" => Filter::Condition(
                                EmailFilterCondition::new().has_keyword("$answered".to_string()),
                            ),
                            "unanswered" => Filter::Condition(
                                EmailFilterCondition::new().not_keyword("$answered".to_string()),
                            ),
                            keyword => Filter::Condition(
                                EmailFilterCondition::new().not_keyword(keyword.to_string()),
                            ),
                        }
                    }
                    let mut accum = if let Some(first) = v.first() {
                        flag_to_filter(first.as_str())
                    } else {
                        Filter::Condition(EmailFilterCondition::new())
                    };
                    for f in v.iter().skip(1) {
                        accum &= flag_to_filter(f.as_str());
                    }
                    *f = accum;
                }
                HasAttachment => {
                    *f = Filter::Condition(EmailFilterCondition::new().has_attachment(Some(true)));
                }
                And(q1, q2) => {
                    let mut rhs = Filter::Condition(EmailFilterCondition::new());
                    let mut lhs = Filter::Condition(EmailFilterCondition::new());
                    rec(q1, &mut rhs);
                    rec(q2, &mut lhs);
                    rhs &= lhs;
                    *f = rhs;
                }
                Or(q1, q2) => {
                    let mut rhs = Filter::Condition(EmailFilterCondition::new());
                    let mut lhs = Filter::Condition(EmailFilterCondition::new());
                    rec(q1, &mut rhs);
                    rec(q2, &mut lhs);
                    rhs |= lhs;
                    *f = rhs;
                }
                Not(q) => {
                    let mut qhs = Filter::Condition(EmailFilterCondition::new());
                    rec(q, &mut qhs);
                    *f = !qhs;
                }
                Answered => {
                    // [ref:TODO]: implement Answered query for jmap
                }
                AnsweredBy { .. } => {
                    // [ref:TODO]: implement AnsweredBy query for jmap
                }
                Larger { .. } => {
                    // [ref:TODO]: implement Larger than query for jmap
                }
                Smaller { .. } => {
                    // [ref:TODO]: implement Smaller than query for jmap
                }
            }
        }
        rec(&val, &mut ret);
        ret
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailSet {
    #[serde(flatten)]
    pub set_call: Set<EmailObject>,
}

impl Method<EmailObject> for EmailSet {
    const NAME: &'static str = "Email/set";
}

impl EmailSet {
    pub fn new(set_call: Set<EmailObject>) -> Self {
        Self { set_call }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailChanges {
    #[serde(flatten)]
    pub changes_call: Changes<EmailObject>,
}

impl Method<EmailObject> for EmailChanges {
    const NAME: &'static str = "Email/changes";
}

impl EmailChanges {
    pub fn new(changes_call: Changes<EmailObject>) -> Self {
        Self { changes_call }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailQueryChanges {
    #[serde(flatten)]
    pub query_changes_call: QueryChanges<Filter<EmailFilterCondition, EmailObject>, EmailObject>,
}

impl Method<EmailObject> for EmailQueryChanges {
    const NAME: &'static str = "Email/queryChanges";
}

impl EmailQueryChanges {
    pub fn new(
        query_changes_call: QueryChanges<Filter<EmailFilterCondition, EmailObject>, EmailObject>,
    ) -> Self {
        Self { query_changes_call }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct EmailQueryChangesResponse {
    ///o  The "collapseThreads" argument that was used with "Email/query".
    #[serde(default = "false_val")]
    pub collapse_threads: bool,
    #[serde(flatten)]
    pub query_changes_response: QueryChangesResponse<EmailObject>,
}

impl std::convert::TryFrom<&RawValue> for EmailQueryChangesResponse {
    type Error = crate::error::Error;

    fn try_from(t: &RawValue) -> std::result::Result<Self, Self::Error> {
        let res: (String, Self, String) = deserialize_from_str(t.get())?;
        assert_eq!(&res.0, "Email/queryChanges");
        Ok(res.1)
    }
}
