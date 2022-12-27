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

//! Parsing and interpreting the [RFC 5228 - Sieve: An Email Filtering Language]
//!
//! [RFC 5228 - Sieve: An Email Filtering Language]: https://www.rfc-editor.org/rfc/rfc5228.html

use crate::error::{Error, ErrorKind, Result};
use crate::parsec::Parser;

pub mod ast;
pub mod parser;

use ast::Rule;

use std::collections::{HashSet, VecDeque};
use std::convert::TryFrom;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Capability {
    /// "body"
    Body,
    /// "fileinto"
    FileInto,
    /// "envelope"
    Envelope,
    /// "relational" <https://www.rfc-editor.org/rfc/rfc5231>
    Relational,
    /// "date" <https://www.rfc-editor.org/rfc/rfc5260.html>
    Date,
}

impl TryFrom<&str> for Capability {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        use Capability::*;
        for (literal, ext) in [
            ("body", Body),
            ("fileinto", FileInto),
            ("envelope", Envelope),
            ("relational", Relational),
            ("date", Date),
        ] {
            if value.eq_ignore_ascii_case(literal) {
                return Ok(ext);
            }
        }
        Err(
            Error::new(format!("Unrecognized Sieve capability: `{}`.", value))
                .set_kind(ErrorKind::NotSupported),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SieveFilter {
    rules: Vec<Rule>,
    capabilities: HashSet<Capability>,
}

impl SieveFilter {
    /// Parse and create a new Sieve script from string.
    pub fn from_str(input: &str) -> Result<Self> {
        match parser::parse_sieve().parse(input) {
            Ok(("", rules)) => Self::new(rules),
            Err(unparsed) | Ok((unparsed, _)) => Err(Error::new(format!(
                "Could not parse part of Sieve filter input: {:?}.",
                unparsed
            ))),
        }
    }

    /// Create a new Sieve script from a vector of rules.
    pub fn new(rules: Vec<Rule>) -> Result<Self> {
        Ok(Self {
            capabilities: Self::validate_rules(&rules)?,
            rules,
        })
    }

    /// Validate a slice of rules.
    ///
    /// ```rust
    /// use melib::parsec::Parser;
    /// use melib::sieve::{parser::parse_sieve, Capability, SieveFilter};
    /// use std::collections::HashSet;
    ///
    ///  assert_eq!(
    ///      SieveFilter::validate_rules(
    ///          &parse_sieve()
    ///              .parse(
    ///                  r#"require "fileinto";
    ///          if header :contains "from" "coyote" {
    ///             discard;
    ///          } elsif header :contains ["subject"] ["$$$"] {
    ///             discard;
    ///          } else {
    ///             fileinto "INBOX";
    ///          }"#
    ///              )
    ///              .unwrap()
    ///              .1
    ///      )
    ///      .unwrap(),
    ///      HashSet::from([Capability::FileInto])
    ///  );
    ///
    ///  // These should err:
    ///  for s in [
    ///      "require \"date\";\nif envelope :all :is \"from\" \"tim@example.com\" {\ndiscard;\n}",
    ///      "if header :contains \"from\" \"coyote\" {\ndiscard;\n} elsif header :contains [\"subject\"] [\"$$$\"] {\ndiscard;\n} else {\nfileinto \"INBOX\";\n}"
    ///  ] {
    ///      assert!(
    ///          SieveFilter::validate_rules(
    ///              &parse_sieve()
    ///                  .parse(s)
    ///              .unwrap()
    ///              .1
    ///          )
    ///          .is_err()
    ///      );
    ///    }
    /// ```
    pub fn validate_rules(rules: &[Rule]) -> Result<HashSet<Capability>> {
        use ast::{ControlCommand::*, RequiredCapabilities, Rule::*};

        let mut capabilities = HashSet::default();
        let mut rule_queue = rules.iter().collect::<VecDeque<&Rule>>();

        while let Some(rule) = rule_queue.pop_front() {
            match rule {
                Control(Require(ref required)) => {
                    for ext in required {
                        capabilities.insert(Capability::try_from(ext.as_str())?);
                    }
                }
                other_rule => {
                    if let Some(required_caps) = other_rule.requires() {
                        if required_caps.difference(&capabilities).count() > 0 {
                            return Err(Error::new(format!(
                                "Rules require capabilities {:?} but
                                        they are not declared with `required`.",
                                required_caps
                                    .difference(&capabilities)
                                    .collect::<Vec<&Capability>>()
                            )));
                        }
                    }
                }
            }
        }
        Ok(capabilities)
    }
}

/// Possible errors when parsing, validating and/or executing Sieve scripts.
#[derive(Debug, Clone)]
pub enum SieveError {
    /// Script validity error.
    ValidScriptError {
        /// Encapsulated error value.
        inner: Error,
    },
    /// Script runtime error.
    RuntimeScriptError {
        /// Encapsulated error value.
        inner: Error,
    },
    /// Logic bug error.
    Bug {
        /// Encapsulated error value.
        inner: Error,
    },
}

/// Succesful outcome of a sieve script execution for an [`Envelope`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// Keep.
    Keep,
    /// Discard.
    Discard,
    /// File into.
    FileInto {
        /// Destination
        destination_mailbox: String,
    },
    /// Redirect to address.
    Redirect {
        /// Destination
        destination_address: String,
    },
}

/// Optional action of a sieve script execution for an [`Envelope`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Copy.
    Copy {
        /// Destination
        destination_mailbox: String,
    },
    /// Forward.
    Forward {
        /// Destination
        destination_address: String,
    },
    /// Modify
    Modify,
}

pub trait Sieve {
    fn passthrough(
        &self,
        script: &SieveFilter,
    ) -> std::result::Result<(Outcome, Vec<Action>), SieveError>;
}

impl Sieve for crate::Envelope {
    fn passthrough(
        &self,
        script: &SieveFilter,
    ) -> std::result::Result<(Outcome, Vec<Action>), SieveError> {
        use ast::{ActionCommand, ControlCommand::*, Rule::*};

        // Implicit keep.
        let mut outcome: Outcome = Outcome::Keep;
        let actions: Vec<self::Action> = Vec::with_capacity(0);
        let mut rule_queue = script.rules.iter().collect::<VecDeque<&Rule>>();

        while let Some(rule) = rule_queue.pop_front() {
            match rule {
                Action(ActionCommand::Discard) => {
                    outcome = Outcome::Discard;
                }
                Action(ActionCommand::Keep) => {
                    outcome = Outcome::Keep;
                }
                Action(ActionCommand::Redirect { ref address }) => {
                    outcome = Outcome::Redirect {
                        destination_address: address.clone(),
                    };
                }
                Action(ActionCommand::FileInto { ref mailbox }) => {
                    outcome = Outcome::FileInto {
                        destination_mailbox: mailbox.clone(),
                    };
                }
                Control(Stop) => {
                    break;
                }
                Control(Require(_)) => {}
                Control(If {
                    condition: (ifrule, ifthen),
                    elsif,
                    else_,
                }) => {
                    for (cond, block) in Some((Some(ifrule), ifthen))
                        .into_iter()
                        .chain(elsif.as_ref().map(|(c, b)| (Some(c), b)).into_iter())
                        .chain(else_.as_ref().map(|b| (None, b)).into_iter())
                    {
                        if let Some(_cond) = cond {
                            todo!()
                        } else {
                            rule_queue.extend(block.0.iter());
                            break;
                        }
                    }
                }
                Block(ref ruleblock) => {
                    rule_queue.extend(ruleblock.0.iter());
                }
            }
        }
        Ok((outcome, actions))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Envelope;

    const MESSAGE_A: &str = r#"Date: Tue, 1 Apr 1997 09:06:31 -0800 (PST)
From: coyote@desert.example.org
To: roadrunner@acme.example.com
Subject: I have a present for you

Look, I'm sorry about the whole anvil thing, and I really
didn't mean to try and drop it on you from the top of the
cliff.  I want to try to make it up to you.  I've got some
great birdseed over here at my place--top of the line
stuff--and if you come by, I'll have it all wrapped up
for you.  I'm really sorry for all the problems I've caused
for you over the years, but I know we can work this out.
--
Wile E. Coyote   "Super Genius"   coyote@desert.example.org"#;

    #[test]
    fn test_sieve_discard_keep() {
        let f = SieveFilter::from_str(r#"keep;"#).unwrap();
        let envelope =
            Envelope::from_bytes(MESSAGE_A.as_bytes(), None).expect("Could not parse mail");
        assert_eq!((Outcome::Keep, vec![]), envelope.passthrough(&f).unwrap());
    }
}
