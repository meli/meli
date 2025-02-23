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

use melib::{parser::BytesExt, text::Truncate, AccountHash, Contacts};

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
        let mut valid = vec![];
        let mut rest = term.as_bytes();
        while let Ok((input, m)) = melib::email::parser::address::mailbox(rest) {
            valid.push(m);
            if !input.starts_with(b",") {
                break;
            }
            rest = &input[1..];
        }
        let rest = String::from_utf8_lossy(rest.ltrim());
        let stripped_term = term.strip_suffix(rest.as_ref()).unwrap();
        let pad = if term.ends_with(",") { " " } else { "" };
        let contacts: &Contacts = &c.accounts[&account_hash].contacts;
        contacts
            .books
            .iter()
            .flat_map(|(k, v)| {
                v.search(&rest)
                    .into_iter()
                    .map(|card| card.as_address())
                    .filter(|addr| !valid.contains(addr))
                    .map(|addr| addr.to_string())
                    .map(|r| format!("{stripped_term}{pad}{r}"))
                    .filter(|c| c != term)
                    .map(|entry| AutoCompleteEntry {
                        entry,
                        description: k.to_string().into(),
                    })
            })
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

#[cfg(test)]
mod tests {
    use rusty_fork::rusty_fork_test;

    use super::*;

    rusty_fork_test! {
        #[test]
        fn test_compose_address_complete() {
            run_compose_address_complete();
        }
    }

    fn run_compose_address_complete() {
        use melib::contacts::Card;

        let tempdir = tempfile::tempdir().unwrap();
        let mut context = crate::Context::new_mock(&tempdir);
        let card_a = Card {
            email: "foo@example.com".into(),
            ..Card::default()
        };
        let card_b = Card {
            name: "Bar Jr".into(),
            email: "bar@example.com".into(),
            ..Card::default()
        };
        let card_c = Card {
            name: "Nightmare D. Macdonald".into(),
            email: "nightd@example.com".into(),
            ..Card::default()
        };
        let account_hash = context.accounts[0].hash;
        context.accounts[0].contacts.books[0].add_card(card_a);
        context.accounts[0].contacts.books[0].add_card(card_b);
        context.accounts[0].contacts.books[0].add_card(card_c);

        let complete_fn = generic_address_complete_fn(account_hash);

        // Ensure no completion without matches
        assert_eq!(complete_fn(&context, "aaaaaaaaa"), vec![]);
        // Ensure first completion without name
        assert_eq!(
            complete_fn(&context, "foo"),
            vec![AutoCompleteEntry {
                entry: "foo@example.com".into(),
                description: "default".into()
            }]
        );
        // Ensure first completion is not quoted if not necessary
        assert_eq!(
            complete_fn(&context, "bar"),
            vec![AutoCompleteEntry {
                entry: "Bar Jr <bar@example.com>".into(),
                description: "default".into()
            }]
        );
        // Ensure first completion is properly quoted if necessary
        assert_eq!(
            complete_fn(&context, "Nightmare"),
            vec![AutoCompleteEntry {
                entry: "\"Nightmare D. Macdonald\" <nightd@example.com>".into(),
                description: "default".into()
            }]
        );
        // Ensure a full match is not completed until you add a comma
        assert_eq!(complete_fn(&context, "foo@example.com"), vec![]);
        assert_eq!(
            complete_fn(&context, "foo@example.com,"),
            vec![
                AutoCompleteEntry {
                    entry: "foo@example.com, Bar Jr <bar@example.com>".into(),
                    description: "default".into()
                },
                AutoCompleteEntry {
                    entry: "foo@example.com, \"Nightmare D. Macdonald\" <nightd@example.com>"
                        .into(),
                    description: "default".into()
                }
            ]
        );
        assert_eq!(
            complete_fn(&context, "foo@example.com, "),
            vec![
                AutoCompleteEntry {
                    entry: "foo@example.com, Bar Jr <bar@example.com>".into(),
                    description: "default".into()
                },
                AutoCompleteEntry {
                    entry: "foo@example.com, \"Nightmare D. Macdonald\" <nightd@example.com>"
                        .into(),
                    description: "default".into()
                }
            ]
        );

        // Ensure followup completion is properly quoted if necessary
        assert_eq!(
            complete_fn(&context, "foo@example.com, Nightm"),
            vec![AutoCompleteEntry {
                entry: "foo@example.com, \"Nightmare D. Macdonald\" <nightd@example.com>".into(),
                description: "default".into()
            }]
        );
        // Ensure values are not repeated
        assert_eq!(complete_fn(&context, "foo@example.com, foo"), vec![]);
    }
}
