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

use super::*;

macro_rules! parse_action {
    ($parser_name:ident, $lit:literal, $t:ty, $action:expr) => {
        pub fn $parser_name<'a>() -> impl Parser<'a, $t> {
            move |input| {
                map(
                    ws(right(match_literal_anycase($lit), ws(match_literal(";")))),
                    |_| $action,
                )
                .parse(input)
            }
        }
    };
    ($parser_name:ident, $lit:literal, $t:ty, $action:expr, $argument:ident) => {
        pub fn $parser_name<'a>() -> impl Parser<'a, $t> {
            move |input| {
                map(
                    ws(right(
                        parse_token($lit),
                        left(ws(string()), ws(parse_token(";"))),
                    )),
                    |$argument| $action,
                )
                .parse(input)
            }
        }
    };
}

parse_action! { parse_sieve_keep, "keep", ActionCommand, ActionCommand::Keep }
parse_action! { parse_sieve_discard, "discard", ActionCommand, ActionCommand::Discard }
parse_action! { parse_sieve_stop, "stop", ControlCommand, ControlCommand::Stop }
parse_action! { parse_sieve_fileinto, "fileinto", ActionCommand, ActionCommand::Fileinto { mailbox }, mailbox }
parse_action! { parse_sieve_redirect, "redirect", ActionCommand, ActionCommand::Redirect { address }, address }

#[inline(always)]
pub fn parse_token<'a>(literal: &'static str) -> impl Parser<'a, ()> {
    move |input| map(ws(match_literal_anycase(literal)), |_| ()).parse(input)
}

#[inline(always)]
fn ws_inner<'a>() -> impl Parser<'a, ()> {
    move |input: &'a str| {
        let mut offset = 0;
        let input_b = input.as_bytes();
        while offset < input_b.len() {
            while offset < input_b.len() && [b' ', b'\t', b'\n', b'\r'].contains(&input_b[offset]) {
                offset += 1;
            }
            if offset >= input_b.len() {
                break;
            }
            if input_b[offset] == b'#' {
                while offset < input_b.len()
                    && !input[offset..].starts_with("\r\n")
                    && !input[offset..].starts_with('\n')
                {
                    offset += 1;
                }
                if offset >= input_b.len() {
                    break;
                }
                if input[offset..].starts_with("\r\n") {
                    offset += 2;
                } else if input[offset..].starts_with('\n') {
                    offset += 1;
                }
            } else if input[offset..].starts_with("/*") {
                while offset < input_b.len() && !input[offset..].starts_with("*/") {
                    offset += 1;
                }
                if offset >= input_b.len() {
                    break;
                }
                if input[offset..].starts_with("*/") {
                    offset += 2;
                }
            } else {
                break;
            }
        }
        Ok((&input[offset..], ()))
    }
}

pub fn ws<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    move |input1| {
        let (input2, ()) = ws_inner().parse(input1)?;
        let (input3, res) = parser.parse(input2)?;
        let (input4, ()) = ws_inner().parse(input3)?;
        Ok((input4, res))
    }
}

// string       = quoted-string / multi-line
//
// quoted-other       = "\" octet-not-qspecial
//                        ; represents just the octet-no-qspecial
//                        ; character.  SHOULD NOT be used

// quoted-safe        = CRLF / octet-not-qspecial
//                        ; either a CRLF pair, OR a single octet other
//                        ; than NUL, CR, LF, double-quote, or backslash

// quoted-special     = "\" (DQUOTE / "\")
//                        ; represents just a double-quote or backslash

// quoted-string      = DQUOTE quoted-text DQUOTE

// quoted-text        = *(quoted-safe / quoted-special / quoted-other)

pub fn string<'a>() -> impl Parser<'a, String> {
    #[inline(always)]
    fn quoted_text<'a>() -> impl Parser<'a, String> {
        move |input: &'a str| {
            let mut offset = 0;
            let mut unescape_dquote: bool = false;
            let mut unescape_slash: bool = false;
            while offset < input.len() {
                if input.len() >= offset + 2 {
                    if input.starts_with("\r\n") {
                        offset += 2;
                    } else if input.starts_with("\\\"") {
                        unescape_dquote = true;
                        offset += 2;
                    } else if input.starts_with("\\\\") {
                        unescape_slash = true;
                        offset += 2;
                    }
                }
                // a single octet other ; than NUL, CR, LF, double-quote, or backslash
                if [b'\x00', b'\r', b'\n', b'"', b'\\'].contains(&input.as_bytes()[offset]) {
                    break;
                }
                offset += 1;
            }
            match (unescape_dquote, unescape_slash) {
                (false, false) => Ok((&input[offset..], input[..offset].to_string())),
                (true, false) => Ok((&input[offset..], input[..offset].replace("\\\"", "\""))),
                (false, true) => Ok((&input[offset..], input[..offset].replace("\\\\", "\\"))),
                (true, true) => Ok((
                    &input[offset..],
                    input[..offset].replace("\\\"", "\"").replace("\\\\", "\\"),
                )),
            }
        }
    }

    #[inline(always)]
    fn quoted_string<'a>() -> impl Parser<'a, String> {
        delimited(parse_token("\""), quoted_text(), parse_token("\""))
    }

    //fn multiline() -> impl Parser<'a, String> {}
    //either(quoted_string(), multiline())
    quoted_string()
}

// number             = 1*DIGIT [ QUANTIFIER ]
// QUANTIFIER         = "K" / "M" / "G"
pub fn number<'a>() -> impl Parser<'a, u64> {
    map_res(
        pair(
            is_a(b"0123456789"),
            pred(any_char, |c| {
                ['k', 'm', 'g'].contains(&c.to_ascii_lowercase())
            }),
        ),
        |(num_s, quant)| {
            Ok(match (num_s.parse::<u64>(), quant.to_ascii_lowercase()) {
                (Ok(num), 'k') => num * 1_000,
                (Ok(num), 'm') => num * 1_000_000,
                (Ok(num), 'g') => num * 1_000_000_000,
                _ => return Err(num_s),
            })
        },
    )
}

pub fn parse_sieve_integer_operator<'a>() -> impl Parser<'a, (IntegerOperator, u64)> {
    move |input| {
        ws(pair(
            either(
                map(parse_token(":over"), |_| IntegerOperator::Over),
                map(parse_token(":under"), |_| IntegerOperator::Under),
            ),
            ws(number()),
        ))
        .parse(input)
    }
}
// ":comparator" <comparator-name: string>
pub fn parse_sieve_comparator<'a>() -> impl Parser<'a, CharacterOperator> {
    move |input| {
        ws(right(
            parse_token(":comparator"),
            ws(map_res(string(), |s| {
                if s == "i;octet" {
                    Ok(CharacterOperator::Octet)
                } else if s == "i;ascii-casemap" {
                    Ok(CharacterOperator::AsciiCasemap)
                } else {
                    Err("invalid comparator")
                }
            })),
        ))
        .parse(input)
    }
}

// MATCH-TYPE   = ":is" / ":contains" / ":matches"
pub fn parse_sieve_match_type<'a>() -> impl Parser<'a, MatchOperator> {
    move |input| {
        either(
            map(parse_token(":is"), |_| MatchOperator::Is),
            either(
                map(parse_token(":contains"), |_| MatchOperator::Contains),
                map(parse_token(":matches"), |_| MatchOperator::Matches),
            ),
        )
        .parse(input)
    }
}

/* string-list  = "[" string *("," string) "]" / string
                ; if there is only a single string, the brackets
                ; are optional
*/
pub fn parse_string_list<'a>() -> impl Parser<'a, Vec<String>> {
    move |input| {
        either(
            delimited(
                ws(parse_token("[")),
                separated_list0(string(), ws(parse_token(",")), false),
                ws(parse_token("]")),
            ),
            map(string(), |s| vec![s]),
        )
        .parse(input)
    }
}

/* Usage:   header [COMPARATOR] [MATCH-TYPE]
 * <header-names: string-list> <key-list: string-list>
 */
pub fn parse_sieve_header<'a>() -> impl Parser<'a, ConditionRule> {
    move |input| {
        map(
            ws(pair(
                right(parse_token("header"), move |input| {
                    crate::permutation! {
                        input,
                        comparator, Option<CharacterOperator>, opt(parse_sieve_comparator()),
                        match_type, Option<MatchOperator>, opt(parse_sieve_match_type())
                    }
                }),
                pair(ws(parse_string_list()), ws(parse_string_list())),
            )),
            |((comparator, match_operator), (header_names, key_list))| ConditionRule::Header {
                comparator,
                match_operator,
                header_names,
                key_list,
            },
        )
        .parse(input)
    }
}

// ADDRESS-PART = ":localpart" / ":domain" / ":all"
pub fn parse_sieve_address_type<'a>() -> impl Parser<'a, AddressOperator> {
    move |input| {
        either(
            map(parse_token(":localpart"), |_| AddressOperator::Localpart),
            either(
                map(parse_token(":domain"), |_| AddressOperator::Domain),
                map(parse_token(":all"), |_| AddressOperator::All),
            ),
        )
        .parse(input)
    }
}

// address [COMPARATOR] [ADDRESS-PART] [MATCH-TYPE] <header-list: string-list>
// <key-list: string-list>
pub fn parse_sieve_address<'a>() -> impl Parser<'a, ConditionRule> {
    move |input| {
        map(
            ws(pair(
                right(parse_token("address"), move |input| {
                    crate::permutation! {
                        input,
                        match_type, Option<MatchOperator>, opt(parse_sieve_match_type()),
                        comparator, Option<CharacterOperator>, opt(parse_sieve_comparator()),
                        address_type, Option<AddressOperator>, opt(parse_sieve_address_type())
                    }
                }),
                pair(ws(parse_string_list()), ws(parse_string_list())),
            )),
            |((match_type, comparator, address_part), (header_list, key_list))| {
                ConditionRule::Address {
                    comparator,
                    address_part,
                    match_type,
                    header_list,
                    key_list,
                }
            },
        )
        .parse(input)
    }
}

pub fn parse_sieve_test<'a>() -> impl Parser<'a, ConditionRule> {
    move |input| {
        either(
            either(
                map(parse_token("true"), |_| ConditionRule::Literal(true)),
                map(parse_token("false"), |_| ConditionRule::Literal(false)),
            ),
            either(
                either(
                    map(
                        right(ws(parse_token("exists")), ws(parse_string_list())),
                        ConditionRule::Exists,
                    ),
                    map(
                        right(ws(parse_token("size")), ws(parse_sieve_integer_operator())),
                        |(operator, limit)| ConditionRule::Size { operator, limit },
                    ),
                ),
                either(
                    either(
                        map(right(ws(parse_token("not")), parse_sieve_test()), |cond| {
                            ConditionRule::Not(Box::new(cond))
                        }),
                        either(parse_sieve_header(), parse_sieve_address()),
                    ),
                    either(
                        map(
                            right(ws(parse_token("allof")), parse_test_list()),
                            ConditionRule::AllOf,
                        ),
                        map(
                            right(ws(parse_token("anyof")), parse_test_list()),
                            ConditionRule::AnyOf,
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

/* test-list  = "(" test *("," test) ")"
 */
pub fn parse_test_list<'a>() -> impl Parser<'a, Vec<ConditionRule>> {
    move |input| {
        delimited(
            ws(parse_token("(")),
            separated_list0(ws(parse_sieve_test()), ws(parse_token(",")), false),
            ws(parse_token(")")),
        )
        .parse(input)
    }
}

pub fn parse_sieve_rule<'a>() -> impl Parser<'a, Rule> {
    either(
        map(
            either(
                either(parse_sieve_stop(), parse_sieve_require()),
                parse_sieve_if(),
            ),
            Rule::Control,
        ),
        map(
            either(
                either(parse_sieve_keep(), parse_sieve_fileinto()),
                either(parse_sieve_redirect(), parse_sieve_discard()),
            ),
            Rule::Action,
        ),
    )
}

pub fn parse_sieve_block<'a>() -> impl Parser<'a, RuleBlock> {
    move |input| {
        map(
            ws(delimited(
                parse_token("{"),
                ws(zero_or_more(parse_sieve_rule())),
                parse_token("}"),
            )),
            RuleBlock,
        )
        .parse(input)
    }
}

pub fn parse_sieve_if<'a>() -> impl Parser<'a, ControlCommand> {
    either(
        map(
            pair(
                parse_sieve_if_bare(),
                ws(right(parse_token("else"), ws(parse_sieve_block()))),
            ),
            |(ifbare, else_)| match ifbare {
                ControlCommand::If {
                    condition,
                    elsif,
                    else_: _,
                } => ControlCommand::If {
                    condition,
                    elsif,
                    else_: Some(else_),
                },
                _ => unreachable!(),
            },
        ),
        parse_sieve_if_bare(),
    )
}

pub fn parse_sieve_if_bare<'a>() -> impl Parser<'a, ControlCommand> {
    either(
        map(
            pair(
                ws(pair(
                    ws(right(parse_token("if"), ws(parse_sieve_test()))),
                    ws(parse_sieve_block()),
                )),
                ws(pair(
                    ws(right(parse_token("elsif"), ws(parse_sieve_test()))),
                    ws(parse_sieve_block()),
                )),
            ),
            |(condition, elsif)| ControlCommand::If {
                condition,
                elsif: Some(Box::new(elsif)),
                else_: None,
            },
        ),
        map(
            pair(
                ws(right(parse_token("if"), ws(parse_sieve_test()))),
                ws(parse_sieve_block()),
            ),
            |(cond, block)| ControlCommand::If {
                condition: (cond, block),
                elsif: None,
                else_: None,
            },
        ),
    )
}

pub fn parse_sieve_require<'a>() -> impl Parser<'a, ControlCommand> {
    move |input| {
        right(
            ws(parse_token("require")),
            ws(left(
                map(parse_string_list(), |string_list| {
                    ControlCommand::Require(string_list)
                }),
                ws(parse_token(";")),
            )),
        )
        .parse(input)
    }
}

pub fn parse_sieve<'a>() -> impl Parser<'a, Vec<Rule>> {
    ws(zero_or_more(ws(parse_sieve_rule())))
}
