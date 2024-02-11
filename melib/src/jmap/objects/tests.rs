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

use serde_json::json;

use super::*;

#[test]
fn test_jmap_query() {
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

#[test]
fn test_jmap_identity_methods() {
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
