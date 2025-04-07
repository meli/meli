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
use crate::text::grapheme_clusters::TextProcessing;

pub fn encode_header(value: &str) -> String {
    let mut ret = String::with_capacity(value.len());
    let mut is_current_window_ascii = true;
    let mut current_window_start = 0;
    {
        let graphemes = value.graphemes_indices();
        for (idx, g) in graphemes {
            match (g.is_ascii(), is_current_window_ascii) {
                (true, true) => {
                    ret.push_str(g);
                }
                (true, false) => {
                    /* If !g.is_whitespace()
                     *
                     * Whitespaces inside encoded tokens must be greedily taken,
                     * instead of splitting each non-ascii word into separate encoded tokens. */
                    if !g.is_empty() && !g.trim_matches(&[' ', '\t'] as &[_]).is_empty() {
                        ret.push_str(&format!(
                            "=?UTF-8?B?{}?=",
                            BASE64_MIME
                                .encode(&value.as_bytes()[current_window_start..idx])
                                .trim()
                        ));
                        is_current_window_ascii = true;
                        current_window_start = idx;
                        ret.push_str(g);
                    }
                }
                (false, true) => {
                    current_window_start = idx;
                    is_current_window_ascii = false;
                }
                /* RFC2047 recommends:
                 * 'While there is no limit to the length of a multiple-line header field, each
                 * line of a header field that contains one or more
                 * 'encoded-word's is limited to 76 characters.'
                 * This is a rough compliance.
                 */
                (false, false) if (((4 * (idx - current_window_start) / 3) + 3) & !3) > 33 => {
                    ret.push_str(&format!(
                        "=?UTF-8?B?{}?=",
                        BASE64_MIME
                            .encode(&value.as_bytes()[current_window_start..idx])
                            .trim()
                    ));
                    if idx != value.len() - 1 {
                        ret.push(' ');
                    }
                    current_window_start = idx;
                }
                (false, false) => {}
            }
        }
    }
    /* If the last part of the header value is encoded, it won't be pushed inside
     * the previous for block */
    if !is_current_window_ascii {
        ret.push_str(&format!(
            "=?UTF-8?B?{}?=",
            BASE64_MIME
                .encode(&value.as_bytes()[current_window_start..])
                .trim()
        ));
    }
    ret
}
