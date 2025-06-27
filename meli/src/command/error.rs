/*
 * meli
 *
 * Copyright 2017 Manos Pitsidianakis
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

use super::*;

#[derive(Clone, Debug)]
pub enum CommandError {
    Parsing {
        inner: Cow<'static, str>,
        kind: Cow<'static, str>,
    },
    BadValue {
        inner: Cow<'static, str>,
        suggestions: Option<&'static [&'static str]>,
    },
    WrongNumberOfArguments {
        too_many: bool,
        takes: (u8, Option<u8>),
        given: u8,
        __func__: &'static str,
        inner: Cow<'static, str>,
    },
    Other {
        inner: Cow<'static, str>,
    },
}

impl<'a> From<nom::Err<melib::nom::error::Error<&'a [u8]>>> for CommandError {
    fn from(res: nom::Err<melib::nom::error::Error<&'a [u8]>>) -> Self {
        match res {
            nom::Err::Incomplete(_) => Self::Parsing {
                inner: res.to_string().into(),
                kind: "".into(),
            },
            nom::Err::Error(e) | nom::Err::Failure(e) => Self::Parsing {
                inner: String::from_utf8_lossy(e.input).to_string().into(),
                kind: format!("{:?}", e.code).into(),
            },
        }
    }
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Parsing { inner, kind: _ } => {
                write!(fmt, "Could not parse command: {inner}")
            }
            Self::BadValue {
                inner,
                suggestions: Some(suggs),
            } => {
                write!(fmt, "Bad value/argument: {inner}. Possible values are: ")?;
                let len = suggs.len();
                for (i, val) in suggs.iter().enumerate() {
                    if i == len.saturating_sub(1) {
                        write!(fmt, "{val}")?;
                    } else {
                        write!(fmt, "{val}, ")?;
                    }
                }
                write!(fmt, "")
            }
            Self::BadValue {
                inner,
                suggestions: None,
            } => {
                write!(fmt, "Bad value/argument: {inner}")
            }
            Self::WrongNumberOfArguments {
                too_many,
                takes,
                given,
                __func__,
                inner: _,
            } => {
                if *too_many {
                    match takes {
                        (min, None) => {
                            write!(
                                fmt,
                                "{__func__}: Too many arguments. Command takes {min} arguments, \
                                 but {given} were given."
                            )
                        }
                        (min, Some(max)) => {
                            write!(
                                fmt,
                                "{__func__}: Too many arguments. Command takes from {min} to \
                                 {max} arguments, but {given} were given."
                            )
                        }
                    }
                } else {
                    match takes {
                        (min, None) => {
                            write!(
                                fmt,
                                "{__func__}: Not enough arguments. Command takes {min} arguments, \
                                 but {given} were given."
                            )
                        }
                        (min, Some(max)) => {
                            write!(
                                fmt,
                                "{__func__}: Not enough arguments. Command takes from {min} to \
                                 {max} arguments, but {given} were given."
                            )
                        }
                    }
                }
            }
            Self::Other { inner } => {
                write!(fmt, "Error: {inner}")
            }
        }
    }
}

impl std::error::Error for CommandError {}
