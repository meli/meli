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
        filters::Filter,
        methods::Query,
        protocol::Request,
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
        r#"{"using":["urn:ietf:params:jmap:core","urn:ietf:params:jmap:mail","urn:ietf:params:jmap:submission"],"methodCalls":[["Email/query",{"accountId":"account_id","calculateTotal":false,"collapseThreads":false,"filter":{"conditions":[{"inMailbox":"mailbox_id"},{"conditions":[{"subject":"wah"},{"conditions":[{"from":"Manos"},{"conditions":[{"subject":"foo"},{"subject":"bar"}],"operator":"OR"}],"operator":"AND"}],"operator":"OR"}],"operator":"AND"},"position":0,"sort":[{"collation":null,"isAscending":false,"property":"receivedAt"}]},"m0"]]}"#,
        serde_json::to_string(&req).unwrap().as_str()
    );
    assert_eq!(*futures::executor::block_on(request_no.lock()), 1);
}

#[test]
fn test_jmap_undo_status() {
    use serde_json::json;

    use crate::jmap::{
        email::EmailObject,
        identity::Identity,
        objects::{Account, Id},
        submission::{EmailSubmissionObject, UndoStatus},
    };
    let account_id: Id<Account> = "blahblah".into();
    let ident_id: Id<Identity> = "sdusssssss".into();
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
        argument::Argument,
        email::{EmailImport, EmailObject},
        identity::Identity,
        methods::ResultField,
        objects::{Account, Id},
        submission::{EmailSubmissionObject, UndoStatus},
    };
    let account_id: Id<Account> = "blahblah".into();
    let ident_id: Id<Identity> = "sdusssssss".into();
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
        identity::{Identity, IdentityGet, IdentitySet},
        methods::Set,
        objects::Id,
        protocol::Request,
    };
    let account_id = "blahblah";
    let prev_seq = 33;
    let main_identity = "user@example.com";
    let mut req = Request::new(Arc::new(FutureMutex::new(prev_seq)));

    let identity_set = IdentitySet(
        Set::<Identity>::new(None)
            .account_id(account_id.into())
            .create(Some({
                let id: Id<Identity> = main_identity.into();
                let address = crate::email::Address::try_from(main_identity)
                    .unwrap_or_else(|_| crate::email::Address::new(None, main_identity.into()));
                indexmap! {
                    id.clone().into() => Identity {
                        id,
                        name: address.get_display_name().unwrap_or_default(),
                        email: address.get_email(),
                        ..Identity::default()
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
                    "urn:ietf:params:jmap:mail",
                    "urn:ietf:params:jmap:submission"
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
        argument::Argument,
        email::{EmailImport, EmailImportObject, EmailObject},
        mailbox::MailboxObject,
        methods::{ResultField, Set},
        objects::{BlobObject, Id},
        protocol::Request,
        submission::{EmailSubmissionObject, EmailSubmissionSet},
    };

    let account_id = "blahblah";
    let blob_id: Id<BlobObject> = Id::new_random();
    let draft_mailbox_id: Id<MailboxObject> = Id::new_random();
    let sent_mailbox_id: Id<MailboxObject> = Id::new_random();
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
                Set::<EmailSubmissionObject>::new(None)
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
            "onSuccessDestroyEmail": null,
            "onSuccessUpdateEmail": {
                "#k1490": {
                    "keywords/$draft": null,
                    format!("mailboxIds/{draft_mailbox_id}"): null,
                    format!("mailboxIds/{sent_mailbox_id}"): true
                }
            },
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
                        "onSuccessDestroyEmail": null,
                        "onSuccessUpdateEmail": {
                            "#k1490": {
                                "keywords/$draft": null,
                                format!("mailboxIds/{draft_mailbox_id}"): null,
                                format!("mailboxIds/{sent_mailbox_id}"): true
                            }
                        },
                    },
                    "m34"
                        ]
                        ],
                        "using": [
                            "urn:ietf:params:jmap:core",
                            "urn:ietf:params:jmap:mail",
                            "urn:ietf:params:jmap:submission"
                        ]
        }},
    );
}

#[test]
fn test_jmap_request_url_template() {
    use serde_json::json;
    use url::Url;

    use crate::jmap::{
        methods::{download_request_format, upload_request_format, RequestUrlTemplate},
        objects::{Account, BlobObject, Id},
    };

    const DOWNLOAD_TEMPLATE: &str = "https://jmap.example.com/download/{accountId}/{blobId}/{name}";
    const UPLOAD_TEMPLATE: &str = "https://jmap.example.com/upload/{accountId}/";

    let account_id: Id<Account> = "blahblah".into();
    let blob_id: Id<BlobObject> = Id::from("683f9246-56d4-4d7d-bd0c-3d4de6db7cbf");
    let download_template_url: RequestUrlTemplate =
        serde_json::from_value(json!(DOWNLOAD_TEMPLATE)).unwrap();
    assert_eq!(
        download_request_format(
            &download_template_url,
            &account_id,
            &blob_id,
            Some("attachment.txt".into())
        )
        .unwrap(),
        serde_json::from_str::<Url>(
            &json!("https://jmap.example.com/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/attachment.txt").to_string()
        )
        .unwrap()
    );
    assert_eq!(
        download_request_format(
            &download_template_url,
            &account_id,
            &blob_id,
            Some("attachment filename.txt".into()),
        )
        .unwrap(),
        serde_json::from_str::<Url>(
            &json!("https://jmap.example.com/download/blahblah/683f9246-56d4-4d7d-bd0c-3d4de6db7cbf/attachment%20filename.txt").to_string()
        )
        .unwrap()
    );

    let upload_template_url: RequestUrlTemplate =
        serde_json::from_value(json!(UPLOAD_TEMPLATE)).unwrap();
    assert_eq!(
        upload_request_format(&upload_template_url, &account_id).unwrap(),
        serde_json::from_str::<Url>(
            &json!("https://jmap.example.com/upload/blahblah/").to_string()
        )
        .unwrap()
    );
}

#[test]
fn test_jmap_session_serde() {
    use serde_json::json;

    use crate::jmap::{
        objects::{Account, Id, State},
        session::{CapabilitiesObject, Session},
    };

    const RFC_EXAMPLE: &str = r###"{
     "capabilities": {
       "urn:ietf:params:jmap:core": {
         "maxSizeUpload": 50000000,
         "maxConcurrentUpload": 8,
         "maxSizeRequest": 10000000,
         "maxConcurrentRequests": 8,
         "maxCallsInRequest": 32,
         "maxObjectsInGet": 256,
         "maxObjectsInSet": 128,
         "collationAlgorithms": [
           "i;ascii-numeric",
           "i;ascii-casemap",
           "i;unicode-casemap"
         ]
       },
       "urn:ietf:params:jmap:mail": {},
       "urn:ietf:params:jmap:contacts": {},
       "https://example.com/apis/foobar": {
         "maxFoosFinangled": 42
       }
     },
     "accounts": {
       "A13824": {
         "name": "john@example.com",
         "isPersonal": true,
         "isReadOnly": false,
         "accountCapabilities": {
           "urn:ietf:params:jmap:mail": {
             "maxMailboxesPerEmail": null,
             "maxMailboxDepth": 10
           },
           "urn:ietf:params:jmap:contacts": {
           }
         }
       },
       "A97813": {
         "name": "jane@example.com",
         "isPersonal": false,
         "isReadOnly": true,
         "accountCapabilities": {
           "urn:ietf:params:jmap:mail": {
             "maxMailboxesPerEmail": 1,
             "maxMailboxDepth": 10
           }
         }
       }
     },
     "primaryAccounts": {
       "urn:ietf:params:jmap:mail": "A13824",
       "urn:ietf:params:jmap:contacts": "A13824"
     },
     "username": "john@example.com",
     "apiUrl": "https://jmap.example.com/api/",
     "downloadUrl": "https://jmap.example.com/download/{accountId}/{blobId}/{name}?accept={type}",
     "uploadUrl": "https://jmap.example.com/upload/{accountId}/",
     "eventSourceUrl": "https://jmap.example.com/eventsource/?types={types}&closeafter={closeafter}&ping={ping}",
     "state": "75128aab4b1b"
   }"###;
    let expected = Session {
        capabilities: indexmap::indexmap! {
            "urn:ietf:params:jmap:core".to_string() => CapabilitiesObject {
                max_size_upload: 50000000,
                max_concurrent_upload: 8,
                max_size_request: 10000000,
                max_concurrent_requests: 8,
                max_calls_in_request: 32,
                max_objects_in_get: 256,
                max_objects_in_set: 128,
                collation_algorithms: vec![
                    "i;ascii-numeric".to_string(),
                    "i;ascii-casemap".to_string(),
                    "i;unicode-casemap".to_string(),
                ],
                extra_properties: indexmap::IndexMap::default(),
            },
            "urn:ietf:params:jmap:mail".to_string() => CapabilitiesObject {
                max_size_upload: 0,
                max_concurrent_upload: 0,
                max_size_request: 0,
                max_concurrent_requests: 0,
                max_calls_in_request: 0,
                max_objects_in_get: 0,
                max_objects_in_set: 0,
                collation_algorithms: vec![],
                extra_properties: indexmap::IndexMap::default(),
            },
            "urn:ietf:params:jmap:contacts".to_string() => CapabilitiesObject {
                max_size_upload: 0,
                max_concurrent_upload: 0,
                max_size_request: 0,
                max_concurrent_requests: 0,
                max_calls_in_request: 0,
                max_objects_in_get: 0,
                max_objects_in_set: 0,
                collation_algorithms: vec![],
                extra_properties: indexmap::IndexMap::default(),
            },
            "https://example.com/apis/foobar".to_string() => CapabilitiesObject {
                max_size_upload: 0,
                max_concurrent_upload: 0,
                max_size_request: 0,
                max_concurrent_requests: 0,
                max_calls_in_request: 0,
                max_objects_in_get: 0,
                max_objects_in_set: 0,
                collation_algorithms: vec![],
                extra_properties: indexmap::indexmap! {
                    "maxFoosFinangled".to_string() => json!(42),
                },
            },
        },
        accounts: indexmap::indexmap! {
            Id::<Account>::from(
                "A13824",
            ) => Account {
                name: "john@example.com".to_string(),
                is_personal: true,
                is_read_only: false,
                account_capabilities: indexmap::indexmap! {
                    "urn:ietf:params:jmap:mail".to_string() => json!({
                        "maxMailboxDepth": 10,
                        "maxMailboxesPerEmail": null
                    }),
                    "urn:ietf:params:jmap:contacts".to_string() => json!({}),
                },
                extra_properties: indexmap::indexmap! {},
            },
            Id::<Account>::from(
                "A97813",
            ) => Account {
                name: "jane@example.com".to_string(),
                is_personal: false,
                is_read_only: true,
                account_capabilities: indexmap::indexmap! {
                    "urn:ietf:params:jmap:mail".to_string() => json!({
                        "maxMailboxDepth": 10,
                        "maxMailboxesPerEmail": 1
                    }),
                },
                extra_properties: indexmap::indexmap! {},
            },
        },
        primary_accounts: indexmap::indexmap! {
            "urn:ietf:params:jmap:mail".to_string() => Id::<Account>::from(
                "A13824",
            ),
            "urn:ietf:params:jmap:contacts".to_string() => Id::<Account>::from(
                "A13824",
            ),
        },
        identities: indexmap::indexmap! {},
        username: "john@example.com".to_string(),
        api_url: serde_json::from_value(json!("https://jmap.example.com/api/")).unwrap(),
        download_url: serde_json::from_value(json!(
            "https://jmap.example.com/download/{accountId}/{blobId}/{name}?accept={type}"
        ))
        .unwrap(),
        upload_url: serde_json::from_value(json!("https://jmap.example.com/upload/{accountId}/"))
            .unwrap(),
        event_source_url: serde_json::from_value(json!(
            "https://jmap.example.com/eventsource/?types={types}&closeafter={closeafter}&ping={ping}"
        ))
        .unwrap(),
        state: State {
            inner: "75128aab4b1b".into(),
            _ph: std::marker::PhantomData,
        },
        extra_properties: indexmap::indexmap! {},
    };
    assert_eq!(
        json!(serde_json::from_str::<Session>(RFC_EXAMPLE).unwrap()),
        json!(expected)
    );
    assert_eq!(
        json!(serde_json::from_str::<serde_json::Value>(RFC_EXAMPLE).unwrap()),
        json!(expected)
    );
    assert_eq!(
        json!(serde_json::from_str::<serde_json::Value>(RFC_EXAMPLE).unwrap()),
        json!(serde_json::from_str::<Session>(RFC_EXAMPLE).unwrap()),
    );
    assert_eq!(
        serde_json::from_str::<serde_json::Value>(
            &json!(serde_json::from_str::<serde_json::Value>(RFC_EXAMPLE).unwrap()).to_string()
        )
        .unwrap(),
        json!(serde_json::from_str::<Session>(RFC_EXAMPLE).unwrap()),
    );
    assert_eq!(
        serde_json::from_str::<serde_json::Value>(
            &json!(serde_json::from_str::<serde_json::Value>(RFC_EXAMPLE).unwrap()).to_string()
        )
        .unwrap(),
        serde_json::from_str::<serde_json::Value>(
            &json!(serde_json::from_str::<Session>(RFC_EXAMPLE).unwrap()).to_string()
        )
        .unwrap(),
    );
}

#[test]
/// Check that `Set` method calls and `Set` responses are serialized and
/// serialized properly.
fn test_jmap_server_set_method_and_response() {
    use std::sync::Arc;

    use futures::lock::Mutex as FutureMutex;
    use serde_json::json;

    use crate::jmap::{
        mailbox,
        methods::{Set, SetError, SetResponse},
        objects::Id,
        protocol::Request,
    };
    let account_id = "blahblah";
    let prev_seq = 33;
    let mut req = Request::new(Arc::new(FutureMutex::new(prev_seq)));
    let path = "new_mbox";

    let mailbox_set_call = mailbox::MailboxSet::new(
        Set::<mailbox::MailboxObject>::new(None)
            .account_id(account_id.into())
            .create(Some({
                let id: Id<mailbox::MailboxObject> = path.into();
                indexmap! {
                    id.clone().into() => mailbox::MailboxObject {
                        id,
                        name: path.to_string(),
                        ..mailbox::MailboxObject::default()
                    }
                }
            })),
    )
    .on_destroy_remove_emails(true);
    futures::executor::block_on(req.add_call(&mailbox_set_call));

    assert_eq!(
        json! {&req},
        json! {{
            "methodCalls" : [
                [
                    "Mailbox/set",
                    {
                        "accountId" : account_id,
                        "create" : {
                            "new_mbox": {
                                "isSubscribed": false,
                                "name": path,
                                "parentId": null,
                                "role": null,
                                "sortOrder": 0,
                            }
                        },
                        "onDestroyRemoveEmails": true
                    },
                    "m33"
                ],
                ],
                "using" : [
                    "urn:ietf:params:jmap:core",
                    "urn:ietf:params:jmap:mail",
                    "urn:ietf:params:jmap:submission"
                ]
        }},
    );
    // Example "where we try to rename one Mailbox and destroy another" from RFC
    // 8621 section-2.6
    let id = Id::<mailbox::MailboxObject>::from("MB674cc24095db49ce");
    let destroy_id = Id::<mailbox::MailboxObject>::from("MB23cfa8094c0f41e6");
    let mailbox_set_call = mailbox::MailboxSet::new(
        Set::<mailbox::MailboxObject>::new(None)
            .account_id(account_id.into())
            .if_in_state(Some("78542".to_string().into()))
            .update(Some({
                indexmap! {
                    id.clone().into() => json! {{
                        "name" : "Maybe important mail"
                    }}
                }
            }))
            .destroy(Some(vec![destroy_id.clone().into()])),
    );
    let expected_json = json! {{
        "accountId": &account_id,
        "ifInState": "78542",
        "update": {
            id.as_str(): {
                "name": "Maybe important mail"
            }
        },
        "destroy": [ destroy_id.as_str() ]
    }};
    assert_eq!(json! {&mailbox_set_call}, expected_json);
    assert_eq!(
        json! {&serde_json::from_value::<mailbox::MailboxSet>(json! {&mailbox_set_call}).unwrap()},
        expected_json,
    );
    // > Suppose the rename succeeds, but we don't have permission to destroy the
    // > Mailbox we tried
    // > to destroy; we might get back:

    let expected_json = json! {{
        "accountId": "blahblah",
        "oldState": "78542",
        "newState": "78549",
        "updated": {
            "MB674cc24095db49ce": null
        },
        "notDestroyed": {
            "MB23cfa8094c0f41e6": {
                "type": "forbidden"
            }
        }
    }};
    let mailbox_set_response: SetResponse<mailbox::MailboxObject> = SetResponse {
        account_id: account_id.into(),
        old_state: Some("78542".to_string().into()),
        new_state: "78549".to_string().into(),
        created: None,
        updated: Some(indexmap! {
            id => None
        }),
        destroyed: None,
        not_created: None,
        not_updated: None,
        not_destroyed: Some(indexmap! {
            destroy_id => SetError::Forbidden(None)
        }),
    };
    assert_eq!(json! {&mailbox_set_response}, expected_json);
    assert_eq!(
        json! {&serde_json::from_value::<SetResponse<mailbox::MailboxObject>>(json! {&mailbox_set_response}).unwrap()},
        expected_json,
    );

    // Check SetError ser/de
    let description = Some("some text".to_string());
    let set_errors = [
        (json! {{ "type": "forbidden" }}, SetError::Forbidden(None)),
        (
            json! {{ "type": "forbidden", "description": "some text" }},
            SetError::Forbidden(description.clone()),
        ),
        (json! {{ "type": "overQuota" }}, SetError::OverQuota(None)),
        (
            json! {{ "type": "overQuota", "description": "some text" }},
            SetError::OverQuota(description.clone()),
        ),
        (json! {{ "type": "tooLarge" }}, SetError::TooLarge(None)),
        (
            json! {{ "type": "tooLarge", "description": "some text" }},
            SetError::TooLarge(description.clone()),
        ),
        (json! {{ "type": "rateLimit" }}, SetError::RateLimit(None)),
        (
            json! {{ "type": "rateLimit", "description": "some text" }},
            SetError::RateLimit(description.clone()),
        ),
        (json! {{ "type": "notFound" }}, SetError::NotFound(None)),
        (
            json! {{ "type": "notFound", "description": "some text" }},
            SetError::NotFound(description.clone()),
        ),
        (
            json! {{ "type": "invalidPatch" }},
            SetError::InvalidPatch(None),
        ),
        (
            json! {{ "type": "invalidPatch", "description": "some text" }},
            SetError::InvalidPatch(description.clone()),
        ),
        (
            json! {{ "type": "willDestroy" }},
            SetError::WillDestroy(None),
        ),
        (
            json! {{ "type": "willDestroy", "description": "some text" }},
            SetError::WillDestroy(description.clone()),
        ),
        (
            json! {{ "type": "invalidProperties", "properties": ["someProperty"] }},
            SetError::InvalidProperties {
                description: None,
                properties: vec!["someProperty".to_string()],
            },
        ),
        (
            json! {{ "type": "invalidProperties", "description": "some text", "properties": ["someProperty"] }},
            SetError::InvalidProperties {
                description: description.clone(),
                properties: vec!["someProperty".to_string()],
            },
        ),
        (json! {{ "type": "singleton" }}, SetError::Singleton(None)),
        (
            json! {{ "type": "singleton", "description": "some text" }},
            SetError::Singleton(description.clone()),
        ),
        (
            json! {{ "type": "requestTooLarge" }},
            SetError::RequestTooLarge(None),
        ),
        (
            json! {{ "type": "requestTooLarge", "description": "some text" }},
            SetError::RequestTooLarge(description.clone()),
        ),
        (
            json! {{ "type": "stateMismatch" }},
            SetError::StateMismatch(None),
        ),
        (
            json! {{ "type": "stateMismatch", "description": "some text" }},
            SetError::StateMismatch(description),
        ),
    ];
    for (expected_json, set_error) in set_errors {
        assert_eq!(json! {&set_error}, expected_json);
        assert_eq!(
            json! {&serde_json::from_value::<SetError>(json! {&set_error}).unwrap()},
            expected_json,
        );
    }
    assert_eq!(
        serde_json::from_value::<SetError>(json! {{
            "type": "invalidProperties",
        }})
        .unwrap_err()
        .to_string(),
        "missing field `properties`"
    );
    assert_eq!(
        serde_json::from_value::<SetError>(json! {{
            "type": "forbidden",
            "properties": vec!["someProperty".to_string()],
        }})
        .unwrap_err()
        .to_string(),
        "unknown field `properties`, expected `type` or `description`"
    );
    assert_eq!(
        serde_json::from_value::<SetError>(json! {{
            "type": "unknown variant",
            "description": "foo",
        }})
        .unwrap_err()
        .to_string(),
        "unknown variant `unknown variant`, expected one of `forbidden`, `overQuota`, `tooLarge`, \
         `rateLimit`, `notFound`, `invalidPatch`, `willDestroy`, `invalidProperties`, \
         `singleton`, `requestTooLarge`, `stateMismatch`"
    );
}
