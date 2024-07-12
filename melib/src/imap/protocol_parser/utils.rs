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

//! # IMAP Parsing utility functions

use nom::{branch::alt, bytes::complete::tag, combinator::map};

use crate::email::parser::IResult;

#[inline]
pub fn nil_to_none<'i, T>(
    parser: fn(&'i [u8]) -> IResult<&'i [u8], T>,
) -> impl FnMut(&'i [u8]) -> IResult<&'i [u8], Option<T>> {
    alt((map(tag("NIL"), |_| None), map(parser, Some)))
}

#[inline]
pub fn nil_to_default<'i, T: Default>(
    parser: fn(&'i [u8]) -> IResult<&'i [u8], T>,
) -> impl FnMut(&'i [u8]) -> IResult<&'i [u8], T> {
    alt((map(tag("NIL"), |_| T::default()), parser))
}
