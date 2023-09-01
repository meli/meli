/*
 * meli
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

use indexmap::IndexMap;
use serde::ser::{Serialize, SerializeStruct, Serializer};

use super::*;

/// `UndoStatus`
///
/// This represents whether the submission may be canceled.  This is
/// server set on create and MUST be one of the following values:
/// * `pending`: It may be possible to cancel this submission.
/// * `final`: The message has been relayed to at least one recipient
/// in a manner that cannot be recalled.  It is no longer possible
/// to cancel this submission.
/// * `canceled`: The submission was canceled and will not be
/// delivered to any recipient.
/// On systems that do not support unsending, the value of this
/// property will always be `final`.  On systems that do support
/// canceling submission, it will start as `pending` and MAY
/// transition to `final` when the server knows it definitely cannot
/// recall the message, but it MAY just remain `pending`.  If in
/// pending state, a client can attempt to cancel the submission by
/// setting this property to `canceled`; if the update succeeds, the
/// submission was successfully canceled, and the message has not been
/// delivered to any of the original recipients.
#[derive(Deserialize, Serialize, Default, Clone, Copy, Debug)]
#[serde(rename_all = "camelCase")]
pub enum UndoStatus {
    /// It may be possible to cancel this submission.
    Pending,
    /// The message has been relayed to at least one recipient in a manner that
    /// cannot be recalled.  It is no longer possible to cancel this
    /// submission.
    #[default]
    Final,
    /// The submission was canceled and will not be delivered to any recipient.
    Canceled,
}

/// This represents the delivery status for each of the submission's
/// recipients, if known.  This property MAY not be supported by all
/// servers, in which case it will remain null.  Servers that support
/// it SHOULD update the EmailSubmission object each time the status
/// of any of the recipients changes, even if some recipients are
/// still being retried.
#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct DeliveryStatusObject {
    /// The SMTP reply string returned for this recipient when the
    /// server last tried to relay the message, or in a later Delivery
    /// Status Notification (DSN, as defined in `[RFC3464]`) response for
    /// the message.  This SHOULD be the response to the RCPT TO stage,
    /// unless this was accepted and the message as a whole was
    /// rejected at the end of the DATA stage, in which case the DATA
    /// stage reply SHOULD be used instead.
    ///
    /// Multi-line SMTP responses should be concatenated to a single
    /// string as follows:
    ///
    /// + The hyphen following the SMTP code on all but the last line
    /// is replaced with a space.
    ///
    /// + Any prefix in common with the first line is stripped from
    /// lines after the first.
    ///
    /// + CRLF is replaced by a space.
    ///
    /// For example:
    ///
    /// 550-5.7.1 Our system has detected that this message is
    /// 550 5.7.1 likely spam.
    ///
    /// would become:
    ///
    /// 550 5.7.1 Our system has detected that this message is likely spam.
    ///
    /// For messages relayed via an alternative to SMTP, the server MAY
    /// generate a synthetic string representing the status instead.
    /// If it does this, the string MUST be of the following form:
    ///
    /// + A 3-digit SMTP reply code, as defined in `[RFC5321]`,
    /// Section 4.2.3.
    ///
    /// + Then a single space character.
    ///
    /// + Then an SMTP Enhanced Mail System Status Code as defined in
    /// `[RFC3463]`, with a registry defined in `[RFC5248]`.
    ///
    /// + Then a single space character.
    ///
    /// + Then an implementation-specific information string with a
    /// human-readable explanation of the response.
    pub smtp_reply: String,

    /// Represents whether the message has been successfully delivered
    /// to the recipient.
    pub delivered: String,

    /// Represents whether the message has been displayed to the recipient.
    pub displayed: Displayed,
}

/// Represents whether the message has been displayed to the recipient.
/// If a Message Disposition Notification (MDN) is received for
/// this recipient with Disposition-Type (as per `[RFC8098]`,
/// Section 3.2.6.2) equal to `displayed`, this property SHOULD be
/// set to `yes`.
#[derive(Deserialize, Serialize, Default, Clone, Copy, Debug)]
#[serde(rename_all = "camelCase")]
pub enum Displayed {
    /// The recipient's system claims the message content has been displayed to
    /// the recipient. Note that there is no guarantee that the recipient
    /// has noticed, read, or understood the content.
    Yes,
    /// The display status is unknown. This is the initial value.
    #[default]
    Unknown,
}

/// Represents whether the message has been successfully delivered
/// to the recipient.
///
/// Note that successful relaying to an external SMTP server SHOULD
/// NOT be taken as an indication that the message has successfully
/// reached the final mail store.  In this case though, the server
/// may receive a DSN response, if requested.
///
/// If a DSN is received for the recipient with Action equal to
/// `delivered`, as per `[RFC3464]`, Section 2.3.3, then the
/// `delivered` property SHOULD be set to `yes`; if the Action
/// equals `failed`, the property SHOULD be set to `no`.  Receipt
/// of any other DSN SHOULD NOT affect this property.
///
/// The server MAY also set this property based on other feedback
/// channels.
#[derive(Deserialize, Serialize, Default, Clone, Copy, Debug)]
#[serde(rename_all = "camelCase")]
pub enum Delivered {
    /// The message is in a local mail queue and the status will change once it
    /// exits the local mail queues. The `smtpReply` property may still
    /// change.
    #[default]
    Queued,
    /// The message was successfully delivered to the mail store of the
    /// recipient. The `smtpReply` property is final.
    Yes,
    /// Delivery to the recipient permanently failed. The `smtpReply` property
    /// is final.
    No,
    /// The final delivery status is unknown, (e.g., it was relayed to an
    /// external machine and no further information is available). The
    /// `smtpReply` property may still change if a DSN arrives.
    Unknown,
}

/// # Email Submission
///
/// An *EmailSubmission* object represents the submission of an Email for
/// delivery to one or more recipients.  It has the following properties:
#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EmailSubmissionObject {
    /// accountId: `Id`
    /// The id of the account to use.
    #[serde(skip_serializing)]
    pub account_id: Id<Account>,
    /// identityId: `Id` (immutable)
    /// The id of the Identity to associate with this submission.
    pub identity_id: Id<IdentityObject>,
    /// The id of the Email to send.  The Email being sent does not have
    /// to be a draft, for example, when "redirecting" an existing Email
    /// to a different address.
    pub email_id: Argument<Id<EmailObject>>,
    /// The Thread id of the Email to send.  This is set by the server to
    /// the `threadId` property of the Email referenced by the `emailId`.
    #[serde(skip_serializing)]
    pub thread_id: Id<ThreadObject>,
    /// Information for use when sending via SMTP.
    pub envelope: Option<EnvelopeObject>,
    /// sendAt: `UTCDate` (immutable; server-set)
    /// The date the submission was/will be released for delivery.  If the
    /// client successfully used `FUTURERELEASE` `[RFC4865]` with the
    /// submission, this MUST be the time when the server will release the
    /// message; otherwise, it MUST be the time the `EmailSubmission` was
    /// created.
    #[serde(skip_serializing)]
    pub send_at: String,
    /// This represents whether the submission may be canceled.
    pub undo_status: UndoStatus,
    /// deliveryStatus: `String[DeliveryStatus]|null` (server-set)
    ///
    /// This represents the delivery status for each of the submission's
    /// recipients, if known.  This property MAY not be supported by all
    /// servers, in which case it will remain null.  Servers that support
    /// it SHOULD update the `EmailSubmission` object each time the status
    /// of any of the recipients changes, even if some recipients are
    /// still being retried.
    #[serde(skip_serializing)]
    pub delivery_status: Option<IndexMap<String, DeliveryStatusObject>>,
    /// dsnBlobIds: `Id[]` (server-set)
    /// A list of blob ids for DSNs `[RFC3464]` received for this
    /// submission, in order of receipt, oldest first.  The blob is the
    /// whole MIME message (with a top-level content-type of "multipart/
    /// report"), as received.
    #[serde(skip_serializing)]
    pub dsn_blob_ids: Vec<Id<BlobObject>>,
    /// mdnBlobIds: `Id[]` (server-set)
    /// A list of blob ids for MDNs `[RFC8098]` received for this
    /// submission, in order of receipt, oldest first.  The blob is the
    /// whole MIME message (with a top-level content-type of "multipart/
    /// report"), as received.
    #[serde(skip_serializing)]
    pub mdn_blob_ids: Vec<Id<BlobObject>>,
}

impl Object for EmailSubmissionObject {
    const NAME: &'static str = "EmailSubmission";
}

impl EmailSubmissionObject {
    /// Create a new `EmailSubmissionObject`, with all the server-set fields
    /// initialized as empty.
    pub fn new(
        account_id: Id<Account>,
        identity_id: Id<IdentityObject>,
        email_id: impl Into<Argument<Id<EmailObject>>>,
        envelope: Option<EnvelopeObject>,
        undo_status: Option<UndoStatus>,
    ) -> Self {
        Self {
            account_id,
            identity_id,
            email_id: email_id.into(),
            thread_id: "".into(),
            envelope,
            send_at: String::new(),
            undo_status: undo_status.unwrap_or_default(),
            delivery_status: None,
            dsn_blob_ids: vec![],
            mdn_blob_ids: vec![],
        }
    }
}

impl Serialize for EmailSubmissionObject {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct(stringify! {EmailSubmissionObject}, 4)?;
        state.serialize_field("identityId", &self.identity_id)?;
        state.serialize_field(
            if matches!(self.email_id, Argument::Value(_)) {
                "emailId"
            } else {
                "#emailId"
            },
            &self.email_id,
        )?;
        state.serialize_field("envelope", &self.envelope)?;
        state.serialize_field("undoStatus", &self.undo_status)?;

        state.end()
    }
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct EmailSubmissionSet {
    #[serde(flatten)]
    pub set_call: Set<EmailSubmissionObject>,
    /// onSuccessUpdateEmail: `Id[PatchObject]|null`
    /// A map of [`EmailSubmissionObject`] id to an object containing properties to
    /// update on the [`Email`](EmailObject) object referenced by the
    /// [`EmailSubmissionObject`] if the create/update/destroy succeeds.  (For
    /// references to EmailSubmissions created in the same
    /// `/set` invocation, this is equivalent to a creation-reference, so the id
    /// will be the creation id prefixed with a `#`.)
    #[serde(default)]
    pub on_success_update_email: Option<IndexMap<Id<EmailSubmissionObject>, PatchObject>>,
    /// onSuccessDestroyEmail: `Id[]|null`
    /// A list of EmailSubmission ids for which the Email with the
    /// corresponding `emailId` should be destroyed if the create/update/
    /// destroy succeeds.  (For references to EmailSubmission creations,
    /// this is equivalent to a creation-reference, so the id will be the
    /// creation id prefixed with a `#`.)
    #[serde(default)]
    pub on_success_destroy_email: Option<Vec<Id<EmailSubmissionObject>>>,
}

impl Method<EmailSubmissionObject> for EmailSubmissionSet {
    const NAME: &'static str = "EmailSubmission/set";
}

impl EmailSubmissionSet {
    pub fn new(set_call: Set<EmailSubmissionObject>) -> Self {
        Self {
            set_call,
            on_success_update_email: None,
            on_success_destroy_email: None,
        }
    }

    pub fn on_success_update_email(
        self,
        on_success_update_email: Option<IndexMap<Id<EmailSubmissionObject>, PatchObject>>,
    ) -> Self {
        Self {
            on_success_update_email,
            ..self
        }
    }

    pub fn on_success_destroy_email(
        self,
        on_success_destroy_email: Option<Vec<Id<EmailSubmissionObject>>>,
    ) -> Self {
        Self {
            on_success_destroy_email,
            ..self
        }
    }
}

/// Information for use when sending via SMTP.
#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EnvelopeObject {
    /// The email address to use as the return address in the SMTP
    /// submission, plus any parameters to pass with the MAIL FROM
    /// address.  The JMAP server MAY allow the address to be the empty
    /// string.

    /// When a JMAP server performs an SMTP message submission, it MAY
    /// use the same id string for the ENVID parameter `[RFC3461]` and
    /// the EmailSubmission object id.  Servers that do this MAY
    /// replace a client-provided value for ENVID with a server-
    /// provided value.
    pub mail_from: Address,

    /// The email addresses to send the message to, and any RCPT TO
    /// parameters to pass with the recipient.
    pub rcpt_to: Vec<Address>,
}

impl Object for EnvelopeObject {
    const NAME: &'static str = "Envelope";
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Address {
    /// The email address being represented by the object.  This is a
    /// "Mailbox" as used in the Reverse-path or Forward-path of the
    /// MAIL FROM or RCPT TO command in `[RFC5321]`.
    pub email: String,

    /// Any parameters to send with the email address (either mail-
    /// parameter or rcpt-parameter as appropriate, as specified in
    /// `[RFC5321]`).  If supplied, each key in the object is a parameter
    /// name, and the value is either the parameter value (type
    /// `String`) or null if the parameter does not take a value.  For
    /// both name and value, any xtext or unitext encodings are removed
    /// (see `[RFC3461]` and `[RFC6533]`) and JSON string encoding is
    /// applied.
    pub parameters: Option<Value>,
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_jmap_undo_status() {
        let account_id: Id<Account> = "blahblah".into();
        let ident_id: Id<IdentityObject> = "sdusssssss".into();
        let email_id: Id<EmailObject> = Id::from("683f9246-56d4-4d7d-bd0c-3d4de6db7cbf");
        let mut obj = EmailSubmissionObject::new(
            account_id,
            ident_id.clone(),
            email_id.clone(),
            None,
            /* undo_status */ None,
        );

        assert_eq!(
            json!(&obj),
            json!({
                "emailId": email_id,
                "envelope": null,
                "identityId": &ident_id,
                "undoStatus": "final",
            })
        );

        obj.undo_status = UndoStatus::Pending;
        assert_eq!(
            json!(&obj),
            json!({
                "emailId": email_id,
                "envelope": null,
                "identityId": &ident_id,
                "undoStatus": "pending",
            })
        );
        obj.undo_status = UndoStatus::Final;
        assert_eq!(
            json!(&obj),
            json!({
                "emailId": email_id,
                "envelope": null,
                "identityId": &ident_id,
                "undoStatus": "final",
            })
        );
        obj.undo_status = UndoStatus::Canceled;
        assert_eq!(
            json!(&obj),
            json!({
                "emailId": email_id,
                "envelope": null,
                "identityId": &ident_id,
                "undoStatus": "canceled",
            })
        );
    }

    #[test]
    fn test_jmap_email_submission_object() {
        let account_id: Id<Account> = "blahblah".into();
        let ident_id: Id<IdentityObject> = "sdusssssss".into();
        let email_id: Id<EmailObject> = Id::from("683f9246-56d4-4d7d-bd0c-3d4de6db7cbf");
        let obj = EmailSubmissionObject::new(
            account_id.clone(),
            ident_id.clone(),
            email_id.clone(),
            None,
            /* undo_status */ None,
        );

        assert_eq!(
            json!(&obj),
            json!({
                "emailId": email_id,
                "envelope": null,
                "identityId": &ident_id,
                "undoStatus": "final",
            })
        );

        let obj = EmailSubmissionObject::new(
            account_id,
            ident_id.clone(),
            /* email_id: */
            Argument::reference::<EmailImport, EmailObject, EmailObject>(
                42,
                ResultField::<EmailImport, EmailObject>::new("/id"),
            ),
            None,
            Some(UndoStatus::Final),
        );

        assert_eq!(
            json!(&obj),
            json!({
                "#emailId": {
                    "name": "Email/import",
                    "path": "/id",
                    "resultOf": "m42",
                 },
                "envelope": null,
                "identityId": &ident_id,
                "undoStatus": "final",
            })
        );
    }
}
