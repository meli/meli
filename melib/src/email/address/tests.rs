//
// melib
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

use super::*;

const M_ID: &str = "<20170825132332.6734-1@mail.ntua.gr>";
const M_LEN: usize = M_ID.len();

#[test]
fn test_email_address_message_id_strbuilder() {
    let (_, val) = parser::address::msg_id(M_ID.as_bytes()).unwrap();
    assert_eq!(
        val,
        MessageID(
            M_ID.as_bytes().to_vec(),
            StrBuilder {
                offset: 1,
                length: 35,
            }
        )
    );
}

#[test]
fn test_email_address_message_id_comparisons() {
    let (_, val) = parser::address::msg_id(M_ID.as_bytes()).unwrap();
    assert_eq!(val, M_ID);
    assert_eq!(val, M_ID[1..][..M_LEN - 2]);
}
