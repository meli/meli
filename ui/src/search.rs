/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

use crate::state::Context;
use melib::{backends::FolderHash, email::EnvelopeHash, error::Result, StackVec};

pub fn filter(
    filter_term: &str,
    context: &Context,
    account_idx: usize,
    folder_hash: FolderHash,
) -> Result<StackVec<EnvelopeHash>> {
    #[cfg(feature = "sqlite3")]
    {
        crate::sqlite3::search(filter_term, context, account_idx, folder_hash)
    }

    #[cfg(not(feature = "sqlite3"))]
    {
        let mut ret = StackVec::new();

        let account = &context.accounts[account_idx];
        for env_hash in account.folders[folder_hash].as_result()?.envelopes {
            let envelope = &account.collection[&env_hash];
            if envelope.subject().contains(&filter_term) {
                ret.push(env_hash);
                continue;
            }
            if envelope.field_from_to_string().contains(&filter_term) {
                ret.push(env_hash);
                continue;
            }
            let op = account.operation(env_hash);
            let body = envelope.body(op)?;
            let decoded = decode_rec(&body, None);
            let body_text = String::from_utf8_lossy(&decoded);
            if body_text.contains(&filter_term) {
                ret.push(env_hash);
            }
        }
        ret
    }
}
