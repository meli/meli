//
// meli - sieve module
//
// Copyright 2022 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use super::{
    parser::*, ActionCommand::*, AddressOperator::*, CharacterOperator::*, ConditionRule::*,
    ControlCommand::*, IntegerOperator::*, MatchOperator::*, Rule::*, RuleBlock,
};
use crate::utils::parsec::Parser;

#[test]
fn test_sieve_parse_strings() {
    assert_eq!(
        Ok(("", vec!["fileinto".to_string(), "reject".to_string()])),
        parse_string_list().parse(r#"["fileinto", "reject"]"#)
    );

    assert_eq!(
        Ok(("", vec!["fileinto".to_string()])),
        parse_string_list().parse(r#""fileinto""#)
    );
}

#[test]
fn test_sieve_parse_conditionals() {
    /* Operators that start with : like :matches are unordered and optional,
     * since they have defaults. But that means we must handle any order
     * correctly, which is tricky if we use an optional parser; for an
     * optional parser both None and Some(_) are valid values.
     */

    /* Permutations of two */
    let raw_input = r#"header :contains :comparator "i;octet" "Subject"
                   "MAKE MONEY FAST""#;
    let (_, first) = parse_sieve_test().parse(raw_input).unwrap();
    assert_eq!(
        Header {
            comparator: Some(Octet),
            match_operator: Some(Contains),
            header_names: ["Subject".to_string()].to_vec(),
            key_list: ["MAKE MONEY FAST".to_string()].to_vec()
        },
        first
    );

    let raw_input = r#"header :comparator "i;octet" :contains "Subject"
                   "MAKE MONEY FAST""#;
    assert_eq!(Ok(("", first)), parse_sieve_test().parse(raw_input));

    /* Permutations of three */
    let raw_input = r#"address :DOMAIN :comparator "i;octet" :is ["From", "To"] "example.com""#;
    let (_, first) = parse_sieve_test().parse(raw_input).unwrap();

    assert_eq!(
        &Address {
            comparator: Some(Octet),
            address_part: Some(Domain),
            match_type: Some(Is),
            header_list: ["From".to_string(), "To".to_string()].to_vec(),
            key_list: ["example.com".to_string()].to_vec()
        },
        &first
    );

    let raw_input = r#"address :DOMAIN :is :comparator "i;octet"  ["From", "To"] "example.com""#;
    assert_eq!(Ok(("", first.clone())), parse_sieve_test().parse(raw_input));

    let raw_input = r#"address :is :DOMAIN :comparator "i;octet"  ["From", "To"] "example.com""#;
    assert_eq!(Ok(("", first.clone())), parse_sieve_test().parse(raw_input));

    let raw_input = r#"address :is :comparator "i;octet" :DOMAIN ["From", "To"] "example.com""#;
    assert_eq!(Ok(("", first)), parse_sieve_test().parse(raw_input));
}

#[test]
fn test_sieve_parse_ifs() {
    let raw_input = "if true {\nstop ;\n}";
    assert_eq!(
        Ok((
            "",
            Control(If {
                condition: (Literal(true), RuleBlock([Control(Stop)].to_vec())),
                elsif: None,
                else_: None
            })
        )),
        parse_sieve_rule().parse(raw_input)
    );

    let raw_input = r#"# Reject all messages that contain the string "ivnten"in the Subject.
if header :contains "subject" "ivnten"
{
    discard;
} else {
    keep;
}"#;

    assert_eq!(
        Ok((
            "",
            [Control(If {
                condition: (
                    Header {
                        comparator: None,
                        match_operator: Some(Contains),
                        header_names: ["subject".to_string()].to_vec(),
                        key_list: ["ivnten".to_string()].to_vec()
                    },
                    RuleBlock([Action(Discard)].to_vec())
                ),
                elsif: None,
                else_: Some(RuleBlock([Action(Keep)].to_vec()))
            })]
            .to_vec()
        )),
        parse_sieve().parse(raw_input)
    );

    let raw_input = r#"# Reject all messages that contain the string "ivnten"in the Subject.
if header :contains "subject" "ivnten"
{
    discard;
}
# Silently discard all messages sent from the tax man
elsif address :matches :domain "from" "*hmrc.gov.uk"
{
    keep;
}"#;
    assert_eq!(
        Ok((
            "",
            [Control(If {
                condition: (
                    Header {
                        comparator: None,
                        match_operator: Some(Contains),
                        header_names: ["subject".to_string()].to_vec(),
                        key_list: ["ivnten".to_string()].to_vec()
                    },
                    RuleBlock([Action(Discard)].to_vec())
                ),
                elsif: Some((
                    Address {
                        comparator: None,
                        address_part: Some(Domain),
                        match_type: Some(Matches),
                        header_list: ["from".to_string()].to_vec(),
                        key_list: ["*hmrc.gov.uk".to_string()].to_vec()
                    },
                    RuleBlock([Action(Keep)].to_vec())
                )),
                else_: None
            })]
            .to_vec()
        )),
        parse_sieve().parse(raw_input)
    );
}

#[test]
fn test_sieve_parse() {
    let raw_input = r#"# The hash character starts a one-line comment.

"#;
    assert_eq!(Ok(("", vec![])), parse_sieve().parse(raw_input));

    let raw_input = r#"# The hash character starts a one-line comment.
# Everything after a # character until the end of line is ignored.

/* this is a bracketed (C-style) comment. This type of comment can stretch
 * over many lines. A bracketed comment begins with a forward slash, followed
 * by an asterisk and ends with the inverse sequence: an asterisk followed
 * by a forward slash. */
"#;

    assert_eq!(Ok(("", vec![])), parse_sieve().parse(raw_input));
    // Test Lists (allof, anyof)

    let raw_input = r#"# This test checks against Spamassassin's header fields:
# If the spam level is 4 or more and the Subject contains too
# many illegal characters, then silently discard the mail.
if allof (header :contains "X-Spam-Level" "****",
          header :contains "X-Spam-Report" "FROM_ILLEGAL_CHARS")
{
    discard;
}
# Discard mails that do not have a Date: or From: header field
# or mails that are sent from the marketing department at example.com.
elsif anyof (not exists ["from", "date"],
        header :contains "from" "marketing@example.com") {
    discard;
}"#;

    assert_eq!(
        Ok((
            "",
            [Control(If {
                condition: (
                    AllOf(
                        [
                            Header {
                                comparator: None,
                                match_operator: Some(Contains),
                                header_names: ["X-Spam-Level".to_string()].to_vec(),
                                key_list: ["****".to_string()].to_vec()
                            },
                            Header {
                                comparator: None,
                                match_operator: Some(Contains),
                                header_names: ["X-Spam-Report".to_string()].to_vec(),
                                key_list: ["FROM_ILLEGAL_CHARS".to_string()].to_vec()
                            }
                        ]
                        .to_vec()
                    ),
                    RuleBlock([Action(Discard)].to_vec())
                ),
                elsif: Some((
                    AnyOf(
                        [
                            Not(Box::new(Exists(
                                ["from".to_string(), "date".to_string()].to_vec()
                            ))),
                            Header {
                                comparator: None,
                                match_operator: Some(Contains),
                                header_names: ["from".to_string()].to_vec(),
                                key_list: ["marketing@example.com".to_string()].to_vec()
                            }
                        ]
                        .to_vec()
                    ),
                    RuleBlock([Action(Discard)].to_vec())
                )),
                else_: None
            })]
            .to_vec()
        )),
        parse_sieve().parse(raw_input)
    );
    // Filter on message size
    let raw_input = r#"# Delete messages greater than half a MB
if size :over 500K
{
    discard;
}
# Also delete small mails, under 1k
if size :under 1k
{
    discard;
}"#;
    assert_eq!(
        Ok((
            "",
            [
                Control(If {
                    condition: (
                        Size {
                            operator: Over,
                            limit: 500000
                        },
                        RuleBlock([Action(Discard)].to_vec())
                    ),
                    elsif: None,
                    else_: None
                }),
                Control(If {
                    condition: (
                        Size {
                            operator: Under,
                            limit: 1000
                        },
                        RuleBlock([Action(Discard)].to_vec())
                    ),
                    elsif: None,
                    else_: None
                })
            ]
            .to_vec()
        )),
        parse_sieve().parse(raw_input)
    );

    assert_eq!(
        Ok((
            "",
            [
                Control(Require(["fileinto".to_string()].to_vec())),
                Control(If {
                    condition: (
                        Header {
                            comparator: None,
                            match_operator: Some(Contains),
                            header_names: ["from".to_string()].to_vec(),
                            key_list: ["coyote".to_string()].to_vec()
                        },
                        RuleBlock([Action(Discard)].to_vec())
                    ),
                    elsif: Some((
                        Header {
                            comparator: None,
                            match_operator: Some(Contains),
                            header_names: ["subject".to_string()].to_vec(),
                            key_list: ["$$$".to_string()].to_vec()
                        },
                        RuleBlock([Action(Discard)].to_vec())
                    )),
                    else_: Some(RuleBlock(
                        [Action(Fileinto {
                            mailbox: "INBOX".to_string()
                        })]
                        .to_vec()
                    ))
                })
            ]
            .to_vec()
        )),
        parse_sieve().parse(
            r#"require "fileinto";
             if header :contains "from" "coyote" {
                discard;
             } elsif header :contains ["subject"] ["$$$"] {
                discard;
             } else {
                fileinto "INBOX";
             }"#
        )
    );
}
