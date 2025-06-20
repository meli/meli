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

pub mod parser;
#[cfg(test)]
mod tests;

use crate::utils::parsec::*;

#[derive(Clone, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ActionCommand {
    Keep,
    Fileinto { mailbox: String },
    Redirect { address: String },
    Discard,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ControlCommand {
    Stop,
    Require(Vec<String>),
    If {
        condition: (ConditionRule, RuleBlock),
        elsif: Option<Box<(ConditionRule, RuleBlock)>>,
        else_: Option<RuleBlock>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Rule {
    Block(RuleBlock),
    Action(ActionCommand),
    Control(ControlCommand),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AddressOperator {
    All,
    Localpart,
    Domain,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IntegerOperator {
    Over,
    Under,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MatchOperator {
    Is,
    Matches,
    Contains,
    Count(RelationalMatch),
    Value(RelationalMatch),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CharacterOperator {
    /// i;octet,
    Octet,
    ///i;ascii-casemap
    AsciiCasemap,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    /// for use in a Date: header field (`RFC2822`).
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
    /// "weekday"   => the day of the week expressed as an integer between "0"
    /// and "6". "0" is Sunday, "1" is Monday, etc.
    Weekday,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ConditionRule {
    /// Logical OR operation.
    AnyOf(Vec<ConditionRule>),
    /// Logical AND operation.
    AllOf(Vec<ConditionRule>),
    /// Header values exist.
    Exists(Vec<String>),
    Header {
        comparator: Option<CharacterOperator>,
        match_operator: Option<MatchOperator>,
        header_names: Vec<String>,
        key_list: Vec<String>,
    },
    Date {
        comparator: Option<CharacterOperator>,
        match_type: Option<MatchOperator>,
        zone: ZoneRule,
        header_name: String,
        date_part: String,
        key_list: Vec<String>,
    },
    Address {
        comparator: Option<CharacterOperator>,
        address_part: Option<AddressOperator>,
        match_type: Option<MatchOperator>,
        header_list: Vec<String>,
        key_list: Vec<String>,
    },
    Not(Box<ConditionRule>),
    Size {
        operator: IntegerOperator,
        limit: u64,
    },
    Literal(bool),
}
