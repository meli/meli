/*
 * melib - sieve module
 *
 * Copyright 2022 Manos Pitsidianakis
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

//! Types representing the Sieve's language abstract syntax tree.

use super::Capability;

use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A list of [rules](Rule).
pub struct RuleBlock(pub Vec<Rule>);

/*
   MATCH-TYPE =/ COUNT / VALUE

  COUNT = ":count" relational-match

  VALUE = ":value" relational-match

  relational-match = DQUOTE
          ("gt" / "ge" / "lt" / "le" / "eq" / "ne") DQUOTE
          ; "gt" means "greater than", the C operator ">".
          ; "ge" means "greater than or equal", the C operator ">=".
          ; "lt" means "less than", the C operator "<".
          ; "le" means "less than or equal", the C operator "<=".
          ; "eq" means "equal to", the C operator "==".
          ; "ne" means "not equal to", the C operator "!=".
*/
/// Sieve action commands.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ActionCommand {
    /// `keep`
    Keep,
    /// `fileinto`
    FileInto {
        ///
        mailbox: String,
    },
    /// `redirect`
    Redirect {
        ///
        address: String,
    },
    /// `discard`
    Discard,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Sieve control commands.
pub enum ControlCommand {
    /// `stop`
    ///
    /// > The "stop" action ends all processing.  If the implicit keep has not
    /// > been cancelled, then it is taken.
    Stop,
    /// `require`
    Require(Vec<String>),
    /// an `if`-`elsif`-`else` condition.
    If {
        ///
        condition: (ConditionRule, RuleBlock),
        ///
        elsif: Option<(ConditionRule, RuleBlock)>,
        ///
        else_: Option<RuleBlock>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Sieve rule commands.
pub enum Rule {
    /// A list of rules enclosed by braces.
    Block(RuleBlock),
    /// An action command.
    Action(ActionCommand),
    /// A control command.
    Control(ControlCommand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Specifies which part of an e-mail address to examine in conditionals..
pub enum AddressOperator {
    /// The entire address.
    All,
    /// The localpart (the part before the `@` character).
    Localpart,
    /// The domain (the part after the `@` character).
    Domain,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Defines what integer operation to perform.
pub enum IntegerOperator {
    /// Over
    Over,
    /// Under
    Under,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// RFC 5231 Sieve Email Filtering: Relational Extension
pub enum RelationalMatch {
    /// "gt" means "greater than", the C operator ">".
    Gt,
    /// "ge" means "greater than or equal", the C operator ">=".
    Ge,
    /// "lt" means "less than", the C operator "<".
    Lt,
    /// "le" means "less than or equal", the C operator "<=".
    Le,
    /// "eq" means "equal to", the C operator "==".
    Eq,
    /// "ne" means "not equal to", the C operator "!=".
    Ne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Defines what match operation to perform.
pub enum MatchOperator {
    /// Exact equality.
    Is,
    /// Pattern match.
    Matches,
    /// Content query.
    Contains,
    /// Count query.
    Count(RelationalMatch),
    /// Numerical value query.
    Value(RelationalMatch),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Defines how to compare strings/characters.
pub enum CharacterOperator {
    /// `i;octet,` compares as raw bytes.
    Octet,
    /// `i;ascii-casemap` compares case-insensitive.
    AsciiCasemap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Part of datetime to examine.
pub enum ZoneRule {
    /// "year"      => the year, "0000" .. "9999".
    Year,
    /// "month"     => the month, "01" .. "12".
    Month,
    /// "day"       => the day, "01" .. "31".
    Day,
    /// "date"      => the date in "yyyy-mm-dd" format.
    Date,
    /// "julian"    => the Modified Julian Day, that is, the date
    /// expressed as an integer number of days since
    /// 00:00 UTC on November 17, 1858 (using the Gregorian
    /// calendar).  This corresponds to the regular
    /// Julian Day minus 2400000.5.  Sample routines to
    /// convert to and from modified Julian dates are
    /// given in Appendix A.
    Julian,
    /// "hour"      => the hour, "00" .. "23".
    Hour,
    /// "minute"    => the minute, "00" .. "59".
    Minute,
    /// "second"    => the second, "00" .. "60".
    Second,
    /// "time"      => the time in "hh:mm:ss" format.
    Time,
    /// "iso8601"   => the date and time in restricted ISO 8601 format.
    Iso8601,
    /// "std11"     => the date and time in a format appropriate
    /// for use in a Date: header field [RFC2822].
    Std11,
    /// "zone"      => the time zone in use.  If the user specified a
    ///time zone with ":zone", "zone" will
    ///contain that value.  If :originalzone is specified
    ///this value will be the original zone specified
    ///in the date-time value.  If neither argument is
    ///specified the value will be the server's default
    ///time zone in offset format "+hhmm" or "-hhmm".  An
    ///offset of 0 (Zulu) always has a positive sign.
    Zone,
    /// "weekday"   => the day of the week expressed as an integer between "0" and "6". "0" is Sunday, "1" is Monday, etc.
    Weekday,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Condition rules.
pub enum ConditionRule {
    /// Logical OR operation.
    AnyOf(Vec<ConditionRule>),
    /// Logical AND operation.
    AllOf(Vec<ConditionRule>),
    /// Header values exist.
    Exists(Vec<String>),
    /// Header value check.
    Header {
        ///
        comparator: Option<CharacterOperator>,
        ///
        match_type: Option<MatchOperator>,
        ///
        header_names: Vec<String>,
        ///
        key_list: Vec<String>,
    },
    /// Date value check.
    Date {
        ///
        comparator: Option<CharacterOperator>,
        ///
        match_type: Option<MatchOperator>,
        ///
        zone: ZoneRule,
        ///
        header_name: String,
        ///
        date_part: String,
        ///
        key_list: Vec<String>,
    },
    /// Address value check.
    Address {
        ///
        comparator: Option<CharacterOperator>,
        ///
        address_part: Option<AddressOperator>,
        ///
        match_type: Option<MatchOperator>,
        ///
        header_list: Vec<String>,
        ///
        key_list: Vec<String>,
    },
    /// Test envelope ("envelope" capability).
    Envelope {
        ///
        comparator: Option<CharacterOperator>,
        ///
        address_part: Option<AddressOperator>,
        ///
        match_type: Option<MatchOperator>,
        ///
        envelope_part: Vec<String>,
        ///
        key_list: Vec<String>,
    },
    /// Invert a conditional.
    Not(Box<ConditionRule>),
    /// Check the size of an e-mail.
    Size {
        ///
        operator: IntegerOperator,
        ///
        limit: u64,
    },
    /// Literal `true` or `false`.
    Literal(bool),
}

/// Returns what capabilities an AST item requires, if any.
pub trait RequiredCapabilities {
    fn requires(&self) -> Option<HashSet<Capability>> {
        None
    }
}

impl RequiredCapabilities for ActionCommand {
    fn requires(&self) -> Option<HashSet<Capability>> {
        if matches!(self, ActionCommand::FileInto { .. }) {
            Some(HashSet::from([Capability::FileInto]))
        } else {
            None
        }
    }
}

impl RequiredCapabilities for ConditionRule {
    fn requires(&self) -> Option<HashSet<Capability>> {
        macro_rules! opt_map {
            ($id:ident) => {
                $id.as_ref().and_then(RequiredCapabilities::requires)
            };
        }
        match self {
            ConditionRule::Address {
                comparator,
                match_type,
                address_part: _,
                header_list: _,
                key_list: _,
            }
            | ConditionRule::Header {
                comparator,
                match_type,
                header_names: _,
                key_list: _,
            } => {
                let ret = IntoIterator::into_iter([opt_map!(comparator), opt_map!(match_type)])
                    .filter_map(std::convert::identity)
                    .flatten()
                    .collect::<HashSet<Capability>>();
                if ret.is_empty() {
                    None
                } else {
                    Some(ret)
                }
            }
            ConditionRule::Date {
                comparator,
                match_type,
                zone: _,
                header_name: _,
                date_part: _,
                key_list: _,
            } => {
                let ret = IntoIterator::into_iter([opt_map!(comparator), opt_map!(match_type)])
                    .filter_map(std::convert::identity)
                    .flatten()
                    .chain(Some(Capability::Date).into_iter())
                    .collect::<HashSet<Capability>>();
                Some(ret)
            }
            ConditionRule::Envelope {
                comparator,
                match_type,
                address_part: _,
                envelope_part: _,
                key_list: _,
            } => {
                let ret = IntoIterator::into_iter([opt_map!(comparator), opt_map!(match_type)])
                    .filter_map(std::convert::identity)
                    .flatten()
                    .chain(Some(Capability::Envelope).into_iter())
                    .collect::<HashSet<Capability>>();
                Some(ret)
            }
            ConditionRule::Not(ref inner) => inner.requires(),
            ConditionRule::AnyOf(ref vec) | ConditionRule::AllOf(ref vec) => {
                let ret = vec
                    .iter()
                    .filter_map(RequiredCapabilities::requires)
                    .flatten()
                    .collect::<HashSet<Capability>>();
                if ret.is_empty() {
                    None
                } else {
                    Some(ret)
                }
            }
            ConditionRule::Literal(_) | ConditionRule::Size { .. } | ConditionRule::Exists(_) => {
                None
            }
        }
    }
}

impl RequiredCapabilities for MatchOperator {
    fn requires(&self) -> Option<HashSet<Capability>> {
        if matches!(self, MatchOperator::Count(_) | MatchOperator::Value(_)) {
            Some(HashSet::from([Capability::Relational]))
        } else {
            None
        }
    }
}

impl RequiredCapabilities for CharacterOperator {}

impl RequiredCapabilities for Rule {
    fn requires(&self) -> Option<HashSet<Capability>> {
        match self {
            Rule::Block(bl) => bl.requires(),
            Rule::Action(cmd) => cmd.requires(),
            Rule::Control(cmd) => cmd.requires(),
        }
    }
}

impl RequiredCapabilities for RuleBlock {
    fn requires(&self) -> Option<HashSet<Capability>> {
        let ret = self
            .0
            .iter()
            .filter_map(RequiredCapabilities::requires)
            .flatten()
            .collect::<HashSet<Capability>>();
        if ret.is_empty() {
            None
        } else {
            Some(ret)
        }
    }
}
impl RequiredCapabilities for ControlCommand {
    fn requires(&self) -> Option<HashSet<Capability>> {
        match self {
            ControlCommand::Stop | ControlCommand::Require(_) => None,
            ControlCommand::If {
                condition: (cond, ruleblock),
                elsif,
                else_,
            } => {
                let ret = else_
                    .as_ref()
                    .into_iter()
                    .filter_map(RequiredCapabilities::requires)
                    .chain(elsif.as_ref().into_iter().flat_map(|(cond, ruleblock)| {
                        cond.requires()
                            .into_iter()
                            .chain(ruleblock.requires().into_iter())
                    }))
                    .chain(cond.requires().into_iter())
                    .chain(ruleblock.requires().into_iter())
                    .flatten()
                    .collect::<HashSet<Capability>>();
                if ret.is_empty() {
                    None
                } else {
                    Some(ret)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use std::iter::FromIterator;

    use super::*;

    use super::ActionCommand::*;
    use super::AddressOperator::*;
    // use super::CharacterOperator::*;
    use super::ConditionRule::*;
    use super::ControlCommand::*;
    // use super::IntegerOperator::*;
    use super::MatchOperator::*;
    use super::Rule::*;
    use super::RuleBlock;

    #[test]
    fn test_sieve_capabilities_detect() {
        let cond = Envelope {
            comparator: None,
            address_part: Some(All),
            match_type: Some(Is),
            envelope_part: ["from".to_string()].to_vec(),
            key_list: ["tim@example.com".to_string()].to_vec(),
        };

        assert_eq!(
            Header {
                comparator: None,
                match_type: Some(Contains),
                header_names: ["from".to_string()].to_vec(),
                key_list: ["coyote".to_string()].to_vec()
            }
            .requires(),
            None
        );

        assert_eq!(
            Action(FileInto {
                mailbox: "INBOX".to_string()
            })
            .requires()
            .unwrap(),
            HashSet::from([Capability::FileInto])
        );
        assert_eq!(
            cond.requires().unwrap(),
            HashSet::from([Capability::Envelope])
        );
        assert_eq!(
            HashSet::from_iter(
                Control(If {
                    condition: (
                        Envelope {
                            comparator: None,
                            address_part: Some(All),
                            match_type: Some(Is),
                            envelope_part: ["from".to_string()].to_vec(),
                            key_list: ["tim@example.com".to_string()].to_vec()
                        },
                        RuleBlock([Action(Discard)].to_vec())
                    ),
                    elsif: Some((
                        Header {
                            comparator: None,
                            match_type: Some(Contains),
                            header_names: ["subject".to_string()].to_vec(),
                            key_list: ["$$$".to_string()].to_vec()
                        },
                        RuleBlock([Action(Discard)].to_vec())
                    )),
                    else_: Some(RuleBlock(
                        [Action(FileInto {
                            mailbox: "INBOX".to_string()
                        })]
                        .to_vec()
                    ))
                })
                .requires()
                .unwrap()
                .into_iter()
            ),
            HashSet::from([Capability::FileInto, Capability::Envelope])
        );
    }
}
