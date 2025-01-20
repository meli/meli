//
// meli
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

//! IMAP-specific tools for [`Envelope`](crate::email::Envelope) metadata
//! handling.

use imap_codec::imap_types::{
    core::{AString, Atom, Vec1},
    fetch::{MacroOrMessageDataItemNames, MessageDataItemName, Section},
    flag::Flag,
};

use crate::imap::protocol_parser::RequiredResponses;

// [ref:TODO]: (#222) Make this `const` as soon as it is possible.
pub fn common_attributes() -> (RequiredResponses, MacroOrMessageDataItemNames<'static>) {
    (
        RequiredResponses::FETCH_UID
            | RequiredResponses::FETCH_FLAGS
            | RequiredResponses::FETCH_ENVELOPE
            | RequiredResponses::FETCH_REFERENCES
            | RequiredResponses::FETCH_BODYSTRUCTURE,
        MacroOrMessageDataItemNames::MessageDataItemNames(vec![
            MessageDataItemName::Uid,
            MessageDataItemName::Flags,
            MessageDataItemName::Envelope,
            MessageDataItemName::BodyExt {
                section: Some(Section::HeaderFields(
                    None,
                    Vec1::from(AString::from(Atom::unvalidated("REFERENCES"))),
                )),
                partial: None,
                peek: true,
            },
            MessageDataItemName::BodyStructure,
        ]),
    )
}

/// Convert [`Flag`](crate::email::Flag) into a list of
/// [`imap_codec::imap_types::flag::Flag`].
///
/// The conversions are:
///
/// - [`Flag::REPLIED`](crate::email::Flag::REPLIED) to
///   [`imap_codec::imap_types::flag::Flag::Answered`].
/// - [`Flag::SEEN`](crate::email::Flag::SEEN) to
///   [`imap_codec::imap_types::flag::Flag::Seen`].
/// - [`Flag::TRASHED`](crate::email::Flag::TRASHED) to
///   [`imap_codec::imap_types::flag::Flag::Deleted`].
/// - [`Flag::DRAFT`](crate::email::Flag::DRAFT) to
///   [`imap_codec::imap_types::flag::Flag::Draft`].
/// - [`Flag::FLAGGED`](crate::email::Flag::FLAGGED) to
///   [`imap_codec::imap_types::flag::Flag::Flagged`].
impl From<crate::email::Flag> for Vec<Flag<'static>> {
    fn from(val: crate::email::Flag) -> Self {
        let mut flags = vec![];

        // Ignore PASSED, it comes from http://cr.yp.to/proto/maildir.html and is not
        // meaningful in IMAP.

        if val.is_replied() {
            flags.push(Flag::Answered);
        }

        if val.is_seen() {
            flags.push(Flag::Seen);
        }

        if val.is_trashed() {
            flags.push(Flag::Deleted);
        }

        if val.is_draft() {
            flags.push(Flag::Draft);
        }

        if val.is_flagged() {
            flags.push(Flag::Flagged);
        }

        flags
    }
}
