//
// meli
//
// Copyright 2026-  Manos Pitsidianakis
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

use serde_test2::{
    assert_de_tokens, assert_de_tokens_error, assert_ser_tokens, assert_tokens, Token,
};

#[test]
fn test_config_serde_toggleflag() {
    use crate::conf::ToggleFlag;

    // Round-trip
    assert_tokens(&ToggleFlag::False, &[Token::Bool(false)]);
    assert_tokens(&ToggleFlag::True, &[Token::Bool(true)]);

    // Serialization always succeeds
    assert_ser_tokens(&ToggleFlag::Unset, &[Token::None]);
    assert_ser_tokens(&ToggleFlag::InternalVal(false), &[Token::None]);
    assert_ser_tokens(&ToggleFlag::InternalVal(true), &[Token::None]);

    // Deserialization can fail if not given a boolean
    assert_de_tokens_error::<ToggleFlag>(
        &[Token::Str("yes")],
        "invalid type: string \"yes\", expected a boolean",
    );
}

#[test]
fn test_config_serde_actionflag() {
    use crate::conf::ActionFlag;

    // Round-trip
    assert_tokens(&ActionFlag::False, &[Token::Bool(false)]);
    assert_tokens(&ActionFlag::True, &[Token::Bool(true)]);

    // Serialization always succeeds
    assert_ser_tokens(&ActionFlag::InternalVal(true), &[Token::None]);
    assert_ser_tokens(&ActionFlag::InternalVal(false), &[Token::None]);
    assert_ser_tokens(&ActionFlag::False, &[Token::Bool(false)]);
    assert_ser_tokens(&ActionFlag::True, &[Token::Bool(true)]);
    assert_ser_tokens(&ActionFlag::Ask, &[Token::Str("ask")]);

    // Deserialization accepts only booleans and the string "ask"
    assert_de_tokens(&ActionFlag::Ask, &[Token::Str("ask")]);
    assert_de_tokens_error::<ActionFlag>(
        &[Token::Str("yes")],
        "invalid value: string \"yes\", expected either a boolean or the string \"ask\"",
    );
    assert_de_tokens_error::<ActionFlag>(
        &[Token::Map { len: None }],
        "invalid type: map, expected either a boolean or the string \"ask\"",
    );
}

#[test]
fn test_config_serde_secret() {
    use crate::conf::Secret;

    const VALUE: &str = "value";

    assert_tokens(&Secret::Value(VALUE.to_string()), &[Token::Str(VALUE)]);
    assert_tokens(
        &Secret::Evaluate {
            command: VALUE.to_string(),
            store_in_memory: true,
        },
        &[
            Token::Map { len: Some(1) },
            Token::Str("command"),
            Token::Str(VALUE),
            Token::MapEnd,
        ],
    );
    assert_de_tokens(
        &Secret::Evaluate {
            command: VALUE.to_string(),
            store_in_memory: true,
        },
        &[
            Token::Map { len: Some(2) },
            Token::Str("command"),
            Token::Str(VALUE),
            Token::Str("store_in_memory"),
            Token::Bool(true),
            Token::MapEnd,
        ],
    );
    assert_tokens(
        &Secret::Evaluate {
            command: VALUE.to_string(),
            store_in_memory: false,
        },
        &[
            Token::Map { len: Some(2) },
            Token::Str("command"),
            Token::Str(VALUE),
            Token::Str("store_in_memory"),
            Token::Bool(false),
            Token::MapEnd,
        ],
    );
    assert_de_tokens_error::<Secret>(
        &[
            Token::Map { len: Some(2) },
            Token::Str("comman"),
            Token::MapEnd,
        ],
        "unknown field `comman`, expected `command` or `store_in_memory`",
    );
    assert_de_tokens_error::<Secret>(
        &[Token::Bool(true)],
        "invalid type: boolean `true`, expected either a string literal or map with keys \
         \"command\" (string) and optionally \"store_in_memory\" (bool)",
    );

    // Backwards-compatibility for melib::smtp::Password
    assert_de_tokens(
        &Secret::Value(VALUE.to_string()),
        &[
            Token::Map { len: Some(2) },
            Token::Str("type"),
            Token::Str("raw"),
            Token::Str("value"),
            Token::Str(VALUE),
            Token::MapEnd,
        ],
    );
    assert_de_tokens(
        &Secret::Value(VALUE.to_string()),
        &[
            Token::Map { len: Some(2) },
            Token::Str("value"),
            Token::Str(VALUE),
            Token::Str("type"),
            Token::Str("raw"),
            Token::MapEnd,
        ],
    );
    assert_de_tokens(
        &Secret::Evaluate {
            command: VALUE.to_string(),
            store_in_memory: false,
        },
        &[
            Token::Map { len: Some(2) },
            Token::Str("type"),
            Token::Str("command_eval"),
            Token::Str("value"),
            Token::Str(VALUE),
            Token::MapEnd,
        ],
    );
    assert_de_tokens(
        &Secret::Evaluate {
            command: VALUE.to_string(),
            store_in_memory: false,
        },
        &[
            Token::Map { len: Some(2) },
            Token::Str("value"),
            Token::Str(VALUE),
            Token::Str("type"),
            Token::Str("command_eval"),
            Token::MapEnd,
        ],
    );
}
