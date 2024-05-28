//
// melib - jmap module.
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

#[test]
fn test_jmap_query() {
    use std::sync::Arc;

    use futures::lock::Mutex as FutureMutex;

    use crate::jmap::{
        email::{EmailFilterCondition, EmailObject, EmailQuery},
        protocol::Request,
        rfc8620::{filters::Filter, Query},
    };
    let q: crate::search::Query = crate::search::Query::try_from(
        "subject:wah or (from:Manos and (subject:foo or subject:bar))",
    )
    .unwrap();
    let f: Filter<EmailFilterCondition, EmailObject> = Filter::from(q);
    assert_eq!(
        r#"{"operator":"OR","conditions":[{"subject":"wah"},{"operator":"AND","conditions":[{"from":"Manos"},{"operator":"OR","conditions":[{"subject":"foo"},{"subject":"bar"}]}]}]}"#,
        serde_json::to_string(&f).unwrap().as_str()
    );
    let filter = {
        let mailbox_id = "mailbox_id".to_string();

        let mut r = Filter::Condition(
            EmailFilterCondition::new()
                .in_mailbox(Some(mailbox_id.into()))
                .into(),
        );
        r &= f;
        r
    };

    let email_call: EmailQuery = EmailQuery::new(
        Query::new()
            .account_id("account_id".to_string().into())
            .filter(Some(filter))
            .position(0),
    )
    .collapse_threads(false);

    let request_no = Arc::new(FutureMutex::new(0));
    let mut req = Request::new(request_no.clone());
    futures::executor::block_on(req.add_call(&email_call));

    assert_eq!(
        r#"{"using":["urn:ietf:params:jmap:core","urn:ietf:params:jmap:mail"],"methodCalls":[["Email/query",{"accountId":"account_id","calculateTotal":false,"collapseThreads":false,"filter":{"conditions":[{"inMailbox":"mailbox_id"},{"conditions":[{"subject":"wah"},{"conditions":[{"from":"Manos"},{"conditions":[{"subject":"foo"},{"subject":"bar"}],"operator":"OR"}],"operator":"AND"}],"operator":"OR"}],"operator":"AND"},"position":0,"sort":null},"m0"]]}"#,
        serde_json::to_string(&req).unwrap().as_str()
    );
    assert_eq!(*futures::executor::block_on(request_no.lock()), 1);
}

#[test]
fn test_jmap_undo_status() {
    use serde_json::json;

    use crate::jmap::{
        email::EmailObject,
        identity::IdentityObject,
        rfc8620::{Account, Id},
        submission::{EmailSubmissionObject, UndoStatus},
    };
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
    use serde_json::json;

    use crate::jmap::{
        email::{EmailImport, EmailObject},
        identity::IdentityObject,
        rfc8620::{argument::Argument, Account, Id, ResultField},
        submission::{EmailSubmissionObject, UndoStatus},
    };
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

#[test]
fn test_jmap_identity_methods() {
    use std::sync::Arc;

    use futures::lock::Mutex as FutureMutex;
    use serde_json::json;

    use crate::jmap::{
        identity::{IdentityGet, IdentityObject, IdentitySet},
        protocol::Request,
        rfc8620::{Id, Set},
    };
    let account_id = "blahblah";
    let prev_seq = 33;
    let main_identity = "user@example.com";
    let mut req = Request::new(Arc::new(FutureMutex::new(prev_seq)));

    let identity_set = IdentitySet(
        Set::<IdentityObject>::new()
            .account_id(account_id.into())
            .create(Some({
                let id: Id<IdentityObject> = main_identity.into();
                let address = crate::email::Address::try_from(main_identity)
                    .unwrap_or_else(|_| crate::email::Address::new(None, main_identity.into()));
                indexmap! {
                    id.clone().into() => IdentityObject {
                        id,
                        name: address.get_display_name().unwrap_or_default(),
                        email: address.get_email(),
                        ..IdentityObject::default()
                    }
                }
            })),
    );
    futures::executor::block_on(req.add_call(&identity_set));

    let identity_get = IdentityGet::new().account_id(account_id.into());
    futures::executor::block_on(req.add_call(&identity_get));

    assert_eq!(
        json! {&req},
        json! {{
            "methodCalls" : [
                [
                    "Identity/set",
                    {
                        "accountId" : account_id,
                        "create" : {
                            "user@example.com" : {
                                "bcc" : null,
                                "email" : main_identity,
                                "htmlSignature" : "",
                                "name" : "",
                                "replyTo" : null,
                                "textSignature" : ""
                            }
                        },
                        "destroy" : null,
                        "ifInState" : null,
                        "update" : null
                    },
                    "m33"
                ],
                [
                    "Identity/get",
                    {
                        "accountId": account_id
                    },
                    "m34"
                ]
                ],
                "using" : [
                    "urn:ietf:params:jmap:core",
                    "urn:ietf:params:jmap:mail"
                ]
        }},
    );
}

#[test]
fn test_jmap_argument_serde() {
    use std::sync::Arc;

    use futures::lock::Mutex as FutureMutex;
    use serde_json::json;

    use crate::jmap::{
        email::{EmailImport, EmailImportObject, EmailObject},
        mailbox::MailboxObject,
        protocol::Request,
        rfc8620::{argument::Argument, BlobObject, Id, ResultField, Set},
        submission::{EmailSubmissionObject, EmailSubmissionSet},
    };

    let account_id = "blahblah";
    let blob_id: Id<BlobObject> = Id::new_uuid_v4();
    let draft_mailbox_id: Id<MailboxObject> = Id::new_uuid_v4();
    let sent_mailbox_id: Id<MailboxObject> = Id::new_uuid_v4();
    let prev_seq = 33;

    let mut req = Request::new(Arc::new(FutureMutex::new(prev_seq)));
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

    let prev_seq = futures::executor::block_on(req.add_call(&import_call));

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
    _ = futures::executor::block_on(req.add_call(&subm_set_call));

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
