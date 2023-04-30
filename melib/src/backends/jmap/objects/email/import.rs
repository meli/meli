/*
 * meli -
 *
 * Copyright  Manos Pitsidianakis
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

use serde_json::value::RawValue;

use super::*;

/// #`import`
///
///    Objects of type `Foo` are imported via a call to `Foo/import`.
///
///    It takes the following arguments:
///
///    - `account_id`: "Id"
///
///       The id of the account to use.
#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ImportCall {
    ///accountId: "Id"
    ///The id of the account to use.
    pub account_id: Id<Account>,
    ///ifInState: "String|null"
    ///This is a state string as returned by the "Email/get" method.  If
    ///supplied, the string must match the current state of the account
    ///referenced by the accountId; otherwise, the method will be aborted
    ///and a "stateMismatch" error returned.  If null, any changes will
    ///be applied to the current state.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub if_in_state: Option<State<EmailObject>>,
    ///o  emails: "Id[EmailImport]"
    ///A map of creation id (client specified) to EmailImport objects.
    pub emails: HashMap<Id<EmailObject>, EmailImport>,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct EmailImport {
    ///o  blobId: "Id"
    ///The id of the blob containing the raw message [RFC5322].
    pub blob_id: Id<BlobObject>,
    ///o  mailboxIds: "Id[Boolean]"
    ///The ids of the Mailboxes to assign this Email to.  At least one
    ///Mailbox MUST be given.
    pub mailbox_ids: HashMap<Id<MailboxObject>, bool>,
    ///o  keywords: "String[Boolean]" (default: {})
    ///The keywords to apply to the Email.
    pub keywords: HashMap<String, bool>,

    ///o  receivedAt: "UTCDate" (default: time of most recent Received
    ///header, or time of import on server if none)
    ///The "receivedAt" date to set on the Email.
    pub received_at: Option<String>,
}

impl ImportCall {
    pub fn new() -> Self {
        Self {
            account_id: Id::new(),
            if_in_state: None,
            emails: HashMap::default(),
        }
    }

    _impl!(
        ///   - accountId: "Id"
        ///
        ///      The id of the account to use.
        account_id: Id<Account>
    );
    _impl!(if_in_state: Option<State<EmailObject>>);
    _impl!(emails: HashMap<Id<EmailObject>, EmailImport>);
}

impl Method<EmailObject> for ImportCall {
    const NAME: &'static str = "Email/import";
}

impl EmailImport {
    pub fn new() -> Self {
        Self {
            blob_id: Id::new(),
            mailbox_ids: HashMap::default(),
            keywords: HashMap::default(),
            received_at: None,
        }
    }

    _impl!(blob_id: Id<BlobObject>);
    _impl!(mailbox_ids: HashMap<Id<MailboxObject>, bool>);
    _impl!(keywords: HashMap<String, bool>);
    _impl!(received_at: Option<String>);
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum ImportError {
    ///The server MAY forbid two Email objects with the same exact content
    ///   [RFC5322], or even just with the same Message-ID [RFC5322], to
    ///   coexist within an account.  In this case, it MUST reject attempts to
    ///   import an Email considered to be a duplicate with an "alreadyExists"
    ///   SetError.
    AlreadyExists {
        description: Option<String>,
        /// An "existingId" property of type "Id" MUST be included on
        ///the SetError object with the id of the existing Email.  If
        /// duplicates are allowed, the newly created Email object MUST
        /// have a separate id and independent mutable properties to the
        /// existing object.
        existing_id: Id<EmailObject>,
    },
    ///If the "blobId", "mailboxIds", or "keywords" properties are invalid
    ///(e.g., missing, wrong type, id not found), the server MUST reject the
    ///import with an "invalidProperties" SetError.
    InvalidProperties {
        description: Option<String>,
        properties: Vec<String>,
    },
    ///If the Email cannot be imported because it would take the account
    ///over quota, the import should be rejected with an "overQuota"
    ///SetError.
    OverQuota { description: Option<String> },
    ///If the blob referenced is not a valid message [RFC5322], the server
    ///MAY modify the message to fix errors (such as removing NUL octets or
    ///fixing invalid headers).  If it does this, the "blobId" on the
    ///response MUST represent the new representation and therefore be
    ///different to the "blobId" on the EmailImport object.  Alternatively,
    ///the server MAY reject the import with an "invalidEmail" SetError.
    InvalidEmail { description: Option<String> },
    ///An "ifInState" argument was supplied, and it does not match the current
    /// state.
    StateMismatch,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ImportResponse {
    ///o  accountId: "Id"
    ///The id of the account used for this call.
    pub account_id: Id<Account>,

    ///o  oldState: "String|null"
    ///The state string that would have been returned by "Email/get" on
    ///this account before making the requested changes, or null if the
    ///server doesn't know what the previous state string was.
    pub old_state: Option<State<EmailObject>>,

    ///o  newState: "String"
    ///The state string that will now be returned by "Email/get" on this
    ///account.
    pub new_state: Option<State<EmailObject>>,

    ///o  created: "Id[Email]|null"
    ///A map of the creation id to an object containing the "id",
    ///"blobId", "threadId", and "size" properties for each successfully
    ///imported Email, or null if none.
    pub created: HashMap<Id<EmailObject>, ImportEmailResult>,

    ///o  notCreated: "Id[SetError]|null"
    ///A map of the creation id to a SetError object for each Email that
    ///failed to be created, or null if all successful.  The possible
    ///errors are defined above.
    pub not_created: HashMap<Id<EmailObject>, ImportError>,
}

impl std::convert::TryFrom<&RawValue> for ImportResponse {
    type Error = crate::error::Error;
    fn try_from(t: &RawValue) -> Result<ImportResponse> {
        let res: (String, ImportResponse, String) =
            serde_json::from_str(t.get()).map_err(|err| {
                crate::error::Error::new(format!(
                    "BUG: Could not deserialize server JSON response properly, please report \
                     this!\nReply from server: {}",
                    &t
                ))
                .set_source(Some(Arc::new(err)))
                .set_kind(ErrorKind::Bug)
            })?;
        assert_eq!(&res.0, &ImportCall::NAME);
        Ok(res.1)
    }
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ImportEmailResult {
    pub id: Id<EmailObject>,
    pub blob_id: Id<BlobObject>,
    pub thread_id: Id<ThreadObject>,
    pub size: usize,
}
