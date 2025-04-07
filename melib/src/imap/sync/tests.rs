//
// meli
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

#[cfg(feature = "sqlite3")]
#[test]
fn test_imap_sync_sqlite() {
    use crate::{backends::IsSubscribedFn, imap::*};

    let tempdir = tempfile::tempdir().unwrap();
    let account_hash = AccountHash::from_bytes(b"test".as_slice());
    let account_name = "test".to_string().into();
    let event_consumer = BackendEventConsumer::new(Arc::new(|_, _| {}));
    let uid_store: Arc<UIDStore> = Arc::new(UIDStore {
        offline_cache: Arc::new(Mutex::new(None)),
        ..UIDStore::new(
            IsSubscribedFn::default(),
            account_hash,
            account_name,
            event_consumer,
            None,
            true,
        )
    });
    let mut value =
        sync::sqlite3_cache::Sqlite3Cache::get(Arc::clone(&uid_store), Some(tempdir.path()))
            .unwrap();

    let mailbox_hash = MailboxHash::from(b"test".as_slice());
    let fetch = FetchResponse {
        uid: Some(1),
        message_sequence_number: 1,
        modseq: None,
        flags: None,
        body: None,
        references: None,
        envelope: Some(Envelope::default()),
        bodystructure: false,
        raw_fetch_value: &[],
    };
    let fetches = &[fetch];
    let err = value.insert_envelopes(mailbox_hash, fetches).unwrap_err();
    assert_eq!(
        err.inner
            .unwrap()
            .downcast_ref::<rusqlite::Error>()
            .unwrap()
            .sqlite_error_code(),
        Some(rusqlite::ErrorCode::ConstraintViolation)
    );
    assert_eq!(err.kind, ErrorKind::NotFound);
    value
        .init_mailbox(mailbox_hash, &SelectResponse::default())
        .unwrap();

    value.insert_envelopes(mailbox_hash, fetches).unwrap();
    _ = tempdir.close();
}
