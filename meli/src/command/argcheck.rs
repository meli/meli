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

//! Helper type for showing the exact reason why a command was invalid.

use super::*;

pub enum ArgCheck<const MIN: u8, const MAX: u8> {
    Start { __func__: &'static str },
    BeforeArgument { so_far: u8, __func__: &'static str },
    Eof { so_far: u8, __func__: &'static str },
}

impl<const MIN: u8, const MAX: u8> ArgCheck<MIN, MAX> {
    #[inline]
    pub fn new(__func__: &'static str) -> Self {
        Self::Start { __func__ }
    }

    #[inline]
    pub fn start(&mut self, input: &[u8]) -> Result<(), CommandError> {
        let Self::Start { __func__ } = *self else {
            unreachable!(
                "ArgCheck::start called with invalid variant: {}",
                if matches!(self, Self::BeforeArgument { .. }) {
                    "BeforeArgument"
                } else {
                    "Eof"
                }
            );
        };
        let is_empty = dbg!(input.trim().is_empty());
        if is_empty && dbg!(MIN) > 0 {
            return dbg!(Err(CommandError::WrongNumberOfArguments {
                too_many: false,
                takes: (MIN, MAX.into()),
                given: 0,
                __func__,
                inner: format!(
                    "needs {}{} arguments.",
                    if MIN == MAX { "at least " } else { "" },
                    MIN
                )
                .into(),
            }));
        }
        *self = Self::BeforeArgument {
            so_far: 0,
            __func__,
        };

        Ok(())
    }

    #[inline]
    pub fn inc(&mut self, input: &[u8]) -> Result<(), CommandError> {
        let Self::BeforeArgument { __func__, so_far } = *self else {
            unreachable!(
                "ArgCheck::inc called with invalid variant: {}",
                if matches!(self, Self::Start { .. }) {
                    "Start"
                } else {
                    "Eof"
                }
            );
        };
        let is_empty = input.trim().is_empty();
        let new_value = so_far + 1;
        if is_empty && new_value > MAX {
            return Err(CommandError::WrongNumberOfArguments {
                too_many: true,
                takes: (MIN, MAX.into()),
                given: new_value,
                __func__,
                inner: format!(
                    "needs {}{} arguments.",
                    if MIN == MAX { "at least " } else { "" },
                    MIN
                )
                .into(),
            });
        }
        *self = Self::BeforeArgument {
            so_far: new_value,
            __func__,
        };

        Ok(())
    }

    #[inline]
    pub fn finish(&mut self, input: &[u8]) -> Result<(), CommandError> {
        let Self::BeforeArgument { __func__, so_far } = *self else {
            unreachable!(
                "ArgCheck::finish called with invalid variant: {}",
                if matches!(self, Self::Start { .. }) {
                    "Start"
                } else {
                    "Eof"
                }
            );
        };
        let is_empty = input.trim().is_empty();
        if !is_empty {
            assert!(so_far <= MAX);
            assert!(so_far >= MIN);
            return Err(CommandError::WrongNumberOfArguments {
                too_many: true,
                takes: (MIN, MAX.into()),
                given: so_far + 1,
                __func__,
                inner: format!(
                    "needs {}{} arguments.",
                    if MIN == MAX { "at least " } else { "" },
                    MIN
                )
                .into(),
            });
        }
        *self = Self::Eof { so_far, __func__ };

        Ok(())
    }
}

macro_rules! arg_init {
    (min_arg: $n:expr, max_arg: $x:expr, $func:tt) => {{
        ArgCheck::<$n, $x>::new(stringify!($func))
    }};
}

macro_rules! arg_value_check {
    ($tag:literal, $input:expr) => {{
        if tag::<&'_ str, &'_ [u8], melib::nom::error::Error<&[u8]>>($tag)($input).is_err() {
            return Ok((
                $input,
                Err(CommandError::BadValue {
                    inner: $tag.to_string().into(),
                }),
            ));
        }
        tag($tag)($input)
    }};
}

macro_rules! arg_chk {
    (start $check:ident, $input:expr) => {{
        if let Err(err) = $check.start($input) {
            return Ok(($input, Err(err)));
        };
    }};
    (inc $check:ident, $input:expr) => {{
        if let Err(err) = $check.inc($input) {
            return Ok(($input, Err(err)));
        };
    }};
    (finish $check:ident, $input:expr) => {{
        if let Err(err) = $check.finish($input) {
            return Ok(($input, Err(err)));
        };
    }};
}
