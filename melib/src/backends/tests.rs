//
// meli - backends module
//
// Copyright 2017 Manos Pitsidianakis
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

use crate::{
    backends::{
        AccountHash, BackendEvent, EnvelopeHash, LazyCountSet, MailboxHash, RefreshEvent,
        RefreshEventKind,
    },
    utils::logging::LogLevel,
};

#[test]
fn test_lazy_count_set() {
    let mut new = LazyCountSet::default();
    assert_eq!(new.len(), 0);
    new.set_not_yet_seen(10);
    assert_eq!(new.len(), 10);
    for i in 0..10 {
        assert!(new.insert_existing(EnvelopeHash(i)));
    }
    assert_eq!(new.len(), 10);
    assert!(new.insert_existing(EnvelopeHash(10)));
    assert_eq!(new.len(), 11);
}

#[test]
fn test_backend_event_flatten() {
    const NOTICE: BackendEvent = BackendEvent::Notice {
        description: String::new(),
        content: None,
        level: LogLevel::ERROR,
    };
    const ACS: BackendEvent = BackendEvent::AccountStateChange {
        message: std::borrow::Cow::<'static, str>::Borrowed(""),
    };
    const REFRESH_1: RefreshEvent = RefreshEvent {
        account_hash: AccountHash(0),
        mailbox_hash: MailboxHash(0),
        kind: RefreshEventKind::Rescan,
    };
    const REFRESH_2: RefreshEvent = RefreshEvent {
        account_hash: AccountHash(0),
        mailbox_hash: MailboxHash(0),
        kind: RefreshEventKind::MailboxDelete(MailboxHash(0)),
    };
    const REFRESH_3: RefreshEvent = RefreshEvent {
        account_hash: AccountHash(0),
        mailbox_hash: MailboxHash(1),
        kind: RefreshEventKind::MailboxDelete(MailboxHash(1)),
    };
    const REFRESH_4: RefreshEvent = RefreshEvent {
        account_hash: AccountHash(0),
        mailbox_hash: MailboxHash(1),
        kind: RefreshEventKind::MailboxSubscribe(MailboxHash(1)),
    };

    assert_eq!(BackendEvent::flatten(vec![]), vec![]);
    assert_eq!(
        BackendEvent::flatten(vec![NOTICE, ACS.clone()]),
        vec![NOTICE, ACS.clone()]
    );
    assert_eq!(
        BackendEvent::flatten(vec![
            NOTICE,
            BackendEvent::Refresh(REFRESH_1.clone()),
            ACS.clone()
        ]),
        vec![
            NOTICE,
            BackendEvent::Refresh(REFRESH_1.clone()),
            ACS.clone()
        ]
    );
    assert_eq!(
        BackendEvent::flatten(vec![
            NOTICE,
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone()]),
            ACS.clone()
        ]),
        vec![
            NOTICE,
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone()]),
            ACS.clone()
        ]
    );
    assert_eq!(
        BackendEvent::flatten(vec![
            NOTICE,
            BackendEvent::Refresh(REFRESH_1.clone()),
            BackendEvent::Refresh(REFRESH_2.clone()),
            ACS.clone()
        ]),
        vec![
            NOTICE,
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone(), REFRESH_2.clone()]),
            ACS.clone()
        ]
    );
    assert_eq!(
        BackendEvent::flatten(vec![
            NOTICE,
            BackendEvent::Refresh(REFRESH_1.clone()),
            BackendEvent::RefreshBatch(vec![REFRESH_2.clone()]),
            ACS.clone()
        ]),
        vec![
            NOTICE,
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone(), REFRESH_2.clone()]),
            ACS.clone()
        ]
    );
    assert_eq!(
        BackendEvent::flatten(vec![
            NOTICE,
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone()]),
            BackendEvent::Refresh(REFRESH_2.clone()),
            ACS.clone()
        ]),
        vec![
            NOTICE,
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone(), REFRESH_2.clone()]),
            ACS.clone()
        ]
    );
    assert_eq!(
        BackendEvent::flatten(vec![
            ACS.clone(),
            BackendEvent::RefreshBatch(vec![REFRESH_1.clone()]),
            BackendEvent::Refresh(REFRESH_2.clone()),
            BackendEvent::Refresh(REFRESH_3.clone()),
            BackendEvent::Refresh(REFRESH_4.clone()),
        ]),
        vec![
            ACS.clone(),
            BackendEvent::RefreshBatch(vec![
                REFRESH_1.clone(),
                REFRESH_2.clone(),
                REFRESH_3.clone(),
                REFRESH_4.clone(),
            ]),
        ]
    );
}
