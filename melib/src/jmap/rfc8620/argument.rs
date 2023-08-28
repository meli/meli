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

use std::hash::Hash;

use crate::jmap::{
    protocol::Method,
    rfc8620::{Object, ResultField},
};

#[derive(Deserialize, Serialize, Clone, PartialEq, Eq, Hash, Debug)]
#[serde(rename_all = "camelCase", untagged)]
pub enum Argument<T: Clone + PartialEq + Eq + Hash> {
    Value(T),
    #[serde(rename_all = "camelCase")]
    ResultReference {
        result_of: String,
        name: String,
        path: String,
    },
}

impl<T: Clone + PartialEq + Eq + Hash> Argument<T> {
    pub fn value(v: T) -> Self {
        Self::Value(v)
    }

    pub fn reference<M, OBJ, MethodOBJ>(result_of: usize, path: ResultField<M, MethodOBJ>) -> Self
    where
        M: Method<MethodOBJ>,
        MethodOBJ: Object,
        OBJ: Object,
    {
        Self::ResultReference {
            result_of: format!("m{}", result_of),
            name: M::NAME.to_string(),
            path: path.field.to_string(),
        }
    }
}

impl<T: Clone + PartialEq + Eq + Hash> From<T> for Argument<T> {
    fn from(v: T) -> Self {
        Self::Value(v)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use serde_json::json;

    use crate::jmap::*;

    #[test]
    fn test_jmap_argument_serde() {
        let account_id = "blahblah";
        let blob_id: Id<BlobObject> = Id::new_uuid_v4();
        let draft_mailbox_id: Id<MailboxObject> = Id::new_uuid_v4();
        let sent_mailbox_id: Id<MailboxObject> = Id::new_uuid_v4();
        let prev_seq = 33;

        let mut req = Request::new(Arc::new(Mutex::new(prev_seq)));
        let creation_id: Id<EmailObject> = "1".into();
        let import_call: EmailImport =
            EmailImport::new()
                .account_id(account_id.into())
                .emails(indexmap! {
                    creation_id =>
                        EmailImportObject::new()
                        .blob_id(blob_id.clone())
                        .keywords(indexmap! {
                            "$draft".to_string() => true,
                        })
                    .mailbox_ids(indexmap! {
                        draft_mailbox_id.clone() => true,
                    }),
                });

        let prev_seq = req.add_call(&import_call);

        let subm_set_call: EmailSubmissionSet = EmailSubmissionSet::new(
                Set::<EmailSubmissionObject>::new()
                    .account_id(account_id.into())
                    .create(Some(indexmap! {
                        Argument::from(Id::from("k1490")) => EmailSubmissionObject::new(
                            /* account_id: */ account_id.into(),
                            /* identity_id: */ account_id.into(),
                            /* email_id: */ Argument::reference::<EmailImport, EmailObject, EmailObject>(prev_seq, ResultField::<EmailImport, EmailObject>::new("/id")),
                            /* envelope: */ None,
                            /* undo_status: */ None
                            )
                    })),
            )
            .on_success_update_email(Some(
                indexmap! {
                    "#k1490".into() => json!({
                        format!("mailboxIds/{draft_mailbox_id}"): null,
                        format!("mailboxIds/{sent_mailbox_id}"): true,
                        "keywords/$draft": null
                    })
                }
            ));
        _ = req.add_call(&subm_set_call);

        assert_eq!(
            json! {&subm_set_call},
            json! {{
                "accountId": account_id,
                "create": {
                    "k1490": {
                        "#emailId": {
                            "name": "Email/import",
                            "path":"/id",
                            "resultOf":"m33"
                        },
                        "envelope": null,
                        "identityId": account_id,
                        "undoStatus": "final"
                    }
                },
                "destroy": null,
                "ifInState": null,
                "onSuccessDestroyEmail": null,
                "onSuccessUpdateEmail": {
                    "#k1490": {
                        "keywords/$draft": null,
                        format!("mailboxIds/{draft_mailbox_id}"): null,
                        format!("mailboxIds/{sent_mailbox_id}"): true
                    }
                },
                "update": null,
            }},
        );
        assert_eq!(
            json! {&req},
            json! {{
                "methodCalls": [
                    [
                        "Email/import",
                        {
                            "accountId": account_id,
                            "emails": {
                                "1": {
                                    "blobId": blob_id.to_string(),
                                    "keywords": {
                                        "$draft": true
                                    },
                                    "mailboxIds": {
                                        draft_mailbox_id.to_string(): true
                                    },
                                    "receivedAt": null
                                }
                            }
                        },
                        "m33"
                    ],
                    [
                        "EmailSubmission/set",
                        {
                            "accountId": account_id,
                            "create": {
                                "k1490": {
                                    "#emailId": {
                                        "name": "Email/import",
                                        "path": "/id",
                                        "resultOf": "m33"
                                    },
                                    "envelope": null,
                                    "identityId": account_id,
                                    "undoStatus": "final"
                                }
                            },
                            "destroy": null,
                            "ifInState": null,
                            "onSuccessDestroyEmail": null,
                            "onSuccessUpdateEmail": {
                                "#k1490": {
                                    "keywords/$draft": null,
                                    format!("mailboxIds/{draft_mailbox_id}"): null,
                                    format!("mailboxIds/{sent_mailbox_id}"): true
                                }
                            },
                            "update": null
                        },
                        "m34"
                            ]
                            ],
                            "using": [
                                "urn:ietf:params:jmap:core",
                                "urn:ietf:params:jmap:mail"
                            ]
            }},
        );
    }
}
