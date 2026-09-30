//
// meli
//
// Copyright 2026  Manos Pitsidianakis
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

use std::sync::Arc;

use melib::{text::Truncate, AccountHash, Contacts};

use crate::{account_settings, utilities::AutoCompleteFn, AutoCompleteEntry, ValidateFn};

#[inline]
pub(super) fn newsgroups_complete_fn(account_hash: AccountHash) -> AutoCompleteFn {
    Box::new(move |c, term| {
        c.accounts[&account_hash]
            .mailbox_entries
            .values()
            .filter_map(|v| {
                if v.path.starts_with(term) {
                    Some(v.path.to_string())
                } else {
                    None
                }
            })
            .map(AutoCompleteEntry::from)
            .collect::<Vec<AutoCompleteEntry>>()
    })
}

#[inline]
pub(super) fn from_complete_fn(_: AccountHash) -> AutoCompleteFn {
    Box::new(move |c, _term| {
        c.accounts
            .values()
            .map(|acc| {
                let addr = acc.settings.account.main_identity_address();
                let desc = match account_settings!(c[&acc.hash()].send_mail) {
                    crate::conf::composing::SendMail::ShellCommand(ref cmd) => {
                        let mut cmd = cmd.as_str();
                        cmd.truncate_at_boundary(10);
                        format!("{} [exec: {}]", acc.name(), cmd)
                    }
                    #[cfg(feature = "smtp")]
                    crate::conf::composing::SendMail::Smtp(ref inner) => {
                        let hostname = match inner.hostname {
                            melib::conf::Secret::Value(ref val) => Some(val.as_str()),
                            melib::conf::Secret::Evaluate { .. } => None,
                        };
                        if let Some(mut hostname) = hostname {
                            hostname.truncate_at_boundary(10);
                            format!("{} [smtp: {}]", acc.name(), hostname)
                        } else {
                            format!("{} [smtp]", acc.name())
                        }
                    }
                    crate::conf::composing::SendMail::ServerSubmission => {
                        format!("{} [server submission]", acc.name())
                    }
                };

                (addr.to_string(), desc)
            })
            .map(AutoCompleteEntry::from)
            .collect::<Vec<AutoCompleteEntry>>()
    })
}

#[inline]
pub(super) fn generic_address_complete_fn(account_hash: AccountHash) -> AutoCompleteFn {
    Box::new(move |c, term| {
        let book: &Contacts = &c.accounts[&account_hash].contacts;
        let results = book.search(term);
        results
            .into_iter()
            .map(|c| c.as_address().to_string())
            .map(AutoCompleteEntry::from)
            .collect::<Vec<AutoCompleteEntry>>()
    })
}

#[inline]
pub(super) fn date_validate_fn() -> Option<ValidateFn> {
    Some(Arc::new(|d| -> bool {
        let Ok(t) = melib::email::parser::dates::rfc5322_date(d.as_bytes()) else {
            return false;
        };
        t != 0
    }))
}

#[inline]
pub(super) fn generic_address_validate_fn() -> Option<ValidateFn> {
    Some(Arc::new(|i| -> bool {
        matches!(
            melib::email::parser::address::group_list(i.as_bytes()),
            Ok((&[], _))
        )
    }))
}
