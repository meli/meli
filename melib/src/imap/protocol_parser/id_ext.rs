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

//! # Parsing `ID` extension command queries and responses
//!
//! This module provides a parsing function for the parameter list of the `ID`
//! command arguments and the `ID` command response.
//!
//! The `ID` extension is defined in [RFC2971](https://datatracker.ietf.org/doc/rfc2971/).

use indexmap::IndexMap;
use nom::bytes::complete::tag;

use super::{quoted, quoted_or_nil};
use crate::email::parser::{BytesExt, IResult};

#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
pub struct IDResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Name of the program
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Version number of the program
    pub version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Name of the operating system
    pub os: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Version of the operating system
    pub os_version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Vendor of the client/server
    pub vendor: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// URL to contact for support
    pub support_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Postal address of contact/vendor
    pub address: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Date program was released, specified as a date-time in `IMAP4rev1`
    pub date: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Command used to start the program
    pub command: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Arguments supplied on the command line, if any if any
    pub arguments: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Description of environment, i.e., UNIX environment variables or Windows
    /// registry settings
    pub environment: Option<String>,
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    pub extra: IndexMap<String, Option<String>>,
}

pub fn id_ext_params_list(input: &[u8]) -> IResult<&[u8], Option<IDResponse>> {
    // id_params_list ::= "(" #(string SPACE nstring) ")" / nil ;; list of field
    // value pairs

    let is_nil: IResult<&[u8], _> = tag("NIL")(input);
    if let Ok((input, _)) = is_nil {
        return Ok((input, None));
    }

    let mut retval = IDResponse::default();

    let (mut input, _) = tag("(")(input)?;

    let mut hits = 0;
    // Implementations MUST NOT send more than 30 field-value pairs.
    for _ in 0..30 {
        let (_input, mut field) = quoted(input)?;
        // Strings are not case-sensitive.
        if field.len() > 30 {
            return Err(nom::Err::Error(
                (
                    input,
                    "id_ext_params_list(): Field strings MUST NOT be longer than 30 octets.",
                )
                    .into(),
            ));
        }
        for byte in field.iter_mut() {
            byte.make_ascii_lowercase();
        }

        let Ok(field) = String::from_utf8(field) else {
            return Err(nom::Err::Error(
                (input, "id_ext_params_list(): invalid field value.").into(),
            ));
        };
        let (_input, _) = tag(" ")(_input)?;
        let (_input, value) = quoted_or_nil(_input)?;
        let value = if let Some(value) = value {
            // Value strings MUST NOT be longer than 1024 octets.
            if value.len() > 1024 {
                return Err(nom::Err::Error(
                    (
                        input,
                        "id_ext_params_list(): Field value strings MUST NOT be longer than 1024 \
                         octets.",
                    )
                        .into(),
                ));
            }
            let Ok(value) = String::from_utf8(value) else {
                return Err(nom::Err::Error(
                    (input, "id_ext_params_list(): invalid value content.").into(),
                ));
            };
            hits += 1;
            Some(value)
        } else {
            None
        };
        macro_rules! field_name {
            ($field:ident) => {{
                if retval.$field.is_some() {
                    // Implementations MUST NOT send the same field name more than once.
                    return Err(nom::Err::Error(
                        (input, "id_ext_params_list(): Repeated field value.").into(),
                    ));
                }
                retval.$field = value;
            }};
        }
        match field.as_str() {
            "name" => field_name! { name },
            "version" => field_name! { version },
            "os" => field_name! { os },
            "os-version" => field_name! { os_version },
            "vendor" => field_name! { vendor },
            "support-url" => field_name! { support_url },
            "address" => field_name! { address },
            "date" => field_name! { date },
            "command" => field_name! { command },
            "arguments" => field_name! { arguments },
            "environment" => field_name! { environment },
            _ => {
                // Implementations MUST NOT send the same field name more than once.
                if retval.extra.contains_key(field.as_str()) {
                    return Err(nom::Err::Error(
                        (input, "id_ext_params_list(): Repeated field value.").into(),
                    ));
                }
                retval.extra.insert(field, value);
            }
        }
        input = _input;
        if input.starts_with(b")") {
            break;
        }
        let (_input, _) = tag(" ")(input)?;
        input = _input;
    }

    let (input, _) = tag(")")(input)?;

    Ok((
        input,
        if hits == 0 && retval.extra.is_empty() {
            None
        } else {
            Some(retval)
        },
    ))
}

pub fn id_ext_response(input: &[u8]) -> IResult<&[u8], Option<IDResponse>> {
    // id_response ::= "ID" SPACE id_params_list
    let (input, _) = tag("* ID ")(input.ltrim())?;
    id_ext_params_list(input)
}

#[test]
fn test_imap_id_ext() {
    assert_eq!(
        id_ext_response(br#" * ID ("name" "Cyrus" "version" "1.5" "os" "sunos" "os-version" "5.5" "support-url" "mailto:cyrus-bugs+@andrew.cmu.edu")"#).unwrap(),
        (
            b"".as_slice(),
            Some(IDResponse {
                name: Some("Cyrus".to_string()),
                version: Some("1.5".to_string()),
                os: Some("sunos".to_string()),
                os_version: Some("5.5".to_string()),
                support_url: Some("mailto:cyrus-bugs+@andrew.cmu.edu".to_string()),
                ..Default::default()
            })
        )
    );
    assert_eq!(
        id_ext_response(b"* ID NIL").unwrap(),
        (b"".as_slice(), None)
    );
    assert_eq!(
        id_ext_params_list(b"NIL").unwrap(),
        id_ext_response(b"* ID NIL").unwrap(),
    );
    assert_eq!(
        id_ext_response(br#" * ID ("name" "Cyrus" "version" NIL)"#).unwrap(),
        (
            b"".as_slice(),
            Some(IDResponse {
                name: Some("Cyrus".to_string()),
                ..Default::default()
            })
        )
    );
    assert_eq!(
        id_ext_response(br#" * ID ("foo" "Example" "bar" NIL "name" "Cyrus")"#).unwrap(),
        (
            b"".as_slice(),
            Some(IDResponse {
                name: Some("Cyrus".to_string()),
                extra: indexmap::indexmap! {
                    "foo".to_string() => Some("Example".to_string()),
                    "bar".to_string() => None,
                },
                ..Default::default()
            })
        )
    );
    // Errors:
    let repeated_field_value: &[u8] = br#" * ID ("name" "Cyrus" "name" NIL)"#;
    assert_eq!(
        id_ext_response(repeated_field_value).unwrap_err(),
        nom::Err::Error(crate::email::parser::ParsingError::<&[u8]>::new(
            &repeated_field_value[22..],
            std::borrow::Cow::Borrowed("id_ext_params_list(): Repeated field value.")
        ))
    );
    let field_over_than_30_octets: &[u8] =
        br#" * ID ("aktinochrysofaidrovrontolamprofengofotostolistos" NIL)"#;
    assert_eq!(
        id_ext_response(field_over_than_30_octets).unwrap_err(),
        nom::Err::Error(crate::email::parser::ParsingError::<&[u8]>::new(
            &field_over_than_30_octets[7..],
            std::borrow::Cow::Borrowed(
                "id_ext_params_list(): Field strings MUST NOT be longer than 30 octets."
            )
        ))
    );
    let value_over_1024_octets: &[u8] = br#" * ID ("song" "In Spite of Ourselves" "lyrics" "She don't like her eggs all runny She thinks crossin' her legs is funny She looks down her nose at money She gets it on like the Easter Bunny She's my baby, I'm her honey I'm never gonna let her go He ain't got laid in a month of Sundays I caught him once and he was sniffin' my undies He ain't too sharp but he gets things done Drinks his beer like it's oxygen He's my baby, and I'm his honey Never gonna let him go In spite of ourselves We'll end up a-sittin' on a rainbow Against all odds Honey, we're the big door prize We're gonna spite Our noses right off of our faces There won't be nothin' but big old hearts Dancin' in our eyes She thinks all my jokes are corny Convict movies make her horny She likes ketchup on her scrambled eggs Swears like a sailor when she shaves her legs She takes a lickin' and keeps on tickin' I'm never gonna let her go He's got more balls than a big brass monkey He's a wacked out werido and a lovebug junkie Sly as a fox and crazy as a loon Payday comes and he's howlin' at the moon He's my baby, I don't mean maybe Never gonna let him go In spite of ourselves We'll end up a-sittin' on a rainbow Against all odds Honey, we're the big door prize We're gonna spite Our noses right off of our faces There won't be nothin' but big old hearts Dancin' in our eyes In spite of ourselves We'll end up a-sittin' on a rainbow Against all odds Honey, we're the big door prize We're gonna spite Our noses right off of our faces There won't be nothin' but big old hearts Dancin' in our eyes There won't be nothin' but big old hearts Dancin' in our eyes In spite of ourselves")"#;
    assert_eq!(
        id_ext_response(value_over_1024_octets).unwrap_err(),
        nom::Err::Error(crate::email::parser::ParsingError::<&[u8]>::new(
            &value_over_1024_octets[38..],
            std::borrow::Cow::Borrowed(
                "id_ext_params_list(): Field value strings MUST NOT be longer than 1024 octets."
            )
        ))
    );
}
