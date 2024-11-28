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

use melib::error::{Result, WrapResultIntoError};
use serde::{Deserialize, Deserializer};

const fn lf_val() -> u8 {
    b'\n'
}

#[derive(Clone, Debug)]
pub enum RegexValue {
    Default {
        pattern: regex::Regex,
    },
    Builder {
        pattern: regex::Regex,
        options: RegexOptions,
    },
}

impl RegexValue {
    pub fn new_with_options(pattern: &str, o: RegexOptions) -> Result<Self> {
        let mut b = regex::RegexBuilder::new(pattern);
        b.unicode(o.unicode)
            .case_insensitive(o.case_insensitive)
            .multi_line(o.multi_line)
            .dot_matches_new_line(o.dot_matches_new_line)
            .crlf(o.crlf)
            .line_terminator(o.line_terminator)
            .swap_greed(o.swap_greed)
            .ignore_whitespace(o.ignore_whitespace)
            .octal(o.octal);
        if let Some(v) = o.size_limit {
            b.size_limit(v);
        }
        let pattern = b
            .build()
            .wrap_err(|| format!("Could not compile regular expression `{}`", pattern))?;
        Ok(Self::Builder {
            pattern,
            options: o,
        })
    }

    pub fn find_iter<'w, 's>(&'w self, s: &'s str) -> FindIter<'w, 's> {
        let (Self::Default { pattern } | Self::Builder { pattern, .. }) = self;
        FindIter {
            iter: pattern.find_iter(s),
            char_indices: s.char_indices(),
            char_offset: 0,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct RegexOptions {
    #[serde(default = "crate::conf::true_val")]
    unicode: bool,
    #[serde(default = "crate::conf::false_val")]
    case_insensitive: bool,
    #[serde(default = "crate::conf::false_val")]
    multi_line: bool,
    #[serde(default = "crate::conf::false_val")]
    dot_matches_new_line: bool,
    #[serde(default = "crate::conf::false_val")]
    crlf: bool,
    #[serde(default = "lf_val")]
    line_terminator: u8,
    #[serde(default = "crate::conf::false_val")]
    swap_greed: bool,
    #[serde(default = "crate::conf::false_val")]
    ignore_whitespace: bool,
    #[serde(default = "crate::conf::false_val")]
    octal: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    size_limit: Option<usize>,
}

impl Default for RegexOptions {
    fn default() -> Self {
        Self {
            unicode: true,
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            crlf: false,
            line_terminator: b'\n',
            swap_greed: false,
            ignore_whitespace: false,
            octal: false,
            size_limit: None,
        }
    }
}

impl<'de> Deserialize<'de> for RegexValue {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // [ref:FIXME]: clippy false positive, remove when resolved.
        #![allow(clippy::collection_is_never_read)]

        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Inner<'a> {
            Default {
                pattern: &'a str,
            },
            Builder {
                pattern: &'a str,
                #[serde(flatten)]
                o: RegexOptions,
            },
        }
        let s = <Inner>::deserialize(deserializer);
        Ok(
            match s.map_err(|err| {
                serde::de::Error::custom(format!(
                    r#"expected one of "true", "false", "ask", found `{}`"#,
                    err
                ))
            })? {
                Inner::Default { pattern } => Self::Default {
                    pattern: regex::Regex::new(pattern).map_err(|err| {
                        serde::de::Error::custom(format!(
                            "Could not compile regular expression `{}`: {}",
                            pattern, err
                        ))
                    })?,
                },
                Inner::Builder { pattern, o } => {
                    let mut b = regex::RegexBuilder::new(pattern);
                    b.unicode(o.unicode)
                        .case_insensitive(o.case_insensitive)
                        .multi_line(o.multi_line)
                        .dot_matches_new_line(o.dot_matches_new_line)
                        .crlf(o.crlf)
                        .line_terminator(o.line_terminator)
                        .swap_greed(o.swap_greed)
                        .ignore_whitespace(o.ignore_whitespace)
                        .octal(o.octal);
                    if let Some(v) = o.size_limit {
                        b.size_limit(v);
                    }
                    let pattern = b.build().map_err(|err| {
                        serde::de::Error::custom(format!(
                            "Could not compile regular expression `{}`: {}",
                            pattern, err
                        ))
                    })?;
                    Self::Builder {
                        pattern,
                        options: o,
                    }
                }
            },
        )
    }
}

pub struct FindIter<'r, 's> {
    iter: regex::Matches<'r, 's>,
    char_indices: std::str::CharIndices<'s>,
    char_offset: usize,
}

impl Iterator for FindIter<'_, '_> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let next_byte_offset = self.iter.next()?;

        let mut next_char_index = self.char_indices.next()?;

        while next_byte_offset.start() < next_char_index.0 {
            self.char_offset += 1;
            next_char_index = self.char_indices.next()?;
        }
        let start = self.char_offset;

        while next_byte_offset.end()
            > self
                .char_indices
                .next()
                .map(|(v, _)| v)
                .unwrap_or_else(|| next_byte_offset.end())
        {
            self.char_offset += 1;
        }
        let end = self.char_offset + 1;

        Some((start, end))
    }
}
