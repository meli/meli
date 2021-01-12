/*
 * meli - melib crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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
use crate::email::parser::IResult;
use nom::{
    bytes::complete::{is_not, tag},
    combinator::opt,
};
use std::str::FromStr;

pub struct NntpLineIterator<'a> {
    slice: &'a str,
}

impl<'a> std::iter::DoubleEndedIterator for NntpLineIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.slice.is_empty() {
            None
        } else if let Some(pos) = self.slice.rfind("\r\n") {
            if self.slice[..pos].is_empty() {
                self.slice = &self.slice[..pos];
                None
            } else if let Some(prev_pos) = self.slice[..pos].rfind("\r\n") {
                let ret = &self.slice[prev_pos + 2..pos + 2];
                self.slice = &self.slice[..prev_pos + 2];
                Some(ret)
            } else {
                let ret = self.slice;
                self.slice = &self.slice[ret.len()..];
                Some(ret)
            }
        } else {
            let ret = self.slice;
            self.slice = &self.slice[ret.len()..];
            Some(ret)
        }
    }
}

impl<'a> Iterator for NntpLineIterator<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        if self.slice.is_empty() {
            None
        } else if let Some(pos) = self.slice.find("\r\n") {
            let ret = &self.slice[..pos + 2];
            self.slice = &self.slice[pos + 2..];
            Some(ret)
        } else {
            let ret = self.slice;
            self.slice = &self.slice[ret.len()..];
            Some(ret)
        }
    }
}

pub trait NntpLineSplit {
    fn split_rn(&self) -> NntpLineIterator;
}

impl NntpLineSplit for str {
    fn split_rn(&self) -> NntpLineIterator {
        NntpLineIterator { slice: self }
    }
}

pub fn over_article(input: &str) -> IResult<&str, (UID, Envelope)> {
    /*
    "0" or article number (see below)
    Subject header content
    From header content
    Date header content
    Message-ID header content
    References header content
    :bytes metadata item
    :lines metadata item
    */
    let (input, num) = is_not("\t")(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, subject) = opt(is_not("\t"))(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, from) = opt(is_not("\t"))(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, date) = opt(is_not("\t"))(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, message_id) = opt(is_not("\t"))(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, references) = opt(is_not("\t"))(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, _bytes) = opt(is_not("\t"))(input)?;
    let (input, _) = tag("\t")(input)?;
    let (input, _lines) = opt(is_not("\t\r\n"))(input)?;
    let (input, _other_headers) = opt(is_not("\r\n"))(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((
        input,
        ({
            let env_hash = {
                let mut hasher = DefaultHasher::new();
                hasher.write(num.as_bytes());
                hasher.write(message_id.unwrap_or_default().as_bytes());
                hasher.finish()
            };
            let mut env = Envelope::new(env_hash);
            if let Some(date) = date {
                env.set_date(date.as_bytes());
                if let Ok(d) =
                    crate::email::parser::dates::rfc5322_date(env.date_as_str().as_bytes())
                {
                    env.set_datetime(d);
                }
            }

            if let Some(subject) = subject {
                env.set_subject(subject.into());
            }

            if let Some(from) = from {
                if let Ok((_, from)) =
                    crate::email::parser::address::rfc2822address_list(from.as_bytes())
                {
                    env.set_from(from);
                }
            }

            if let Some(references) = references {
                env.set_references(references.as_bytes());
            }

            if let Some(message_id) = message_id {
                env.set_message_id(message_id.as_bytes());
            }
            (usize::from_str(num).unwrap(), env)
        }),
    ))
}
