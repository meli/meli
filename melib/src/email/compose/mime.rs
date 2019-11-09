use super::*;

#[cfg(feature = "unicode_algorithms")]
use text_processing::grapheme_clusters::TextProcessing;

pub fn encode_header(value: &str) -> String {
    let mut ret = String::with_capacity(value.len());
    let mut is_current_window_ascii = true;
    let mut current_window_start = 0;
    #[cfg(feature = "unicode_algorithms")]
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
                    if !g.split_whitespace().collect::<Vec<&str>>().is_empty() {
                        ret.push_str(&format!(
                            "=?UTF-8?B?{}?=",
                            BASE64_MIME
                                .encode(value[current_window_start..idx].as_bytes())
                                .trim()
                        ));
                        if idx != value.len() - 1 {
                            ret.push(' ');
                        }
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
                 * 'While there is no limit to the length of a multiple-line header field, each line of
                 * a header field that contains one or more 'encoded-word's is limited to 76
                 * characters.'
                 * This is a rough compliance.
                 */
                (false, false) if (((4 * (idx - current_window_start) / 3) + 3) & !3) > 33 => {
                    ret.push_str(&format!(
                        "=?UTF-8?B?{}?=",
                        BASE64_MIME
                            .encode(value[current_window_start..idx].as_bytes())
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
    #[cfg(not(feature = "unicode_algorithms"))]
    {
        /* TODO: test this. If it works as fine as the one above, there's no need to keep the above
         * implementation.*/
        let mut idx = 0;
        for g in value.chars() {
            match (g.is_ascii(), is_current_window_ascii) {
                (true, true) => {
                    ret.push(g);
                }
                (true, false) => {
                    /* If !g.is_whitespace()
                     *
                     * Whitespaces inside encoded tokens must be greedily taken,
                     * instead of splitting each non-ascii word into separate encoded tokens. */
                    if !g.is_whitespace() {
                        ret.push_str(&format!(
                            "=?UTF-8?B?{}?=",
                            BASE64_MIME
                                .encode(value[current_window_start..idx].as_bytes())
                                .trim()
                        ));
                        if idx != value.len() - 1 {
                            ret.push(' ');
                        }
                        is_current_window_ascii = true;
                        current_window_start = idx;
                        ret.push(g);
                    }
                }
                (false, true) => {
                    current_window_start = idx;
                    is_current_window_ascii = false;
                }
                /* RFC2047 recommends:
                 * 'While there is no limit to the length of a multiple-line header field, each line of
                 * a header field that contains one or more 'encoded-word's is limited to 76
                 * characters.'
                 * This is a rough compliance.
                 */
                (false, false) if (((4 * (idx - current_window_start) / 3) + 3) & !3) > 33 => {
                    ret.push_str(&format!(
                        "=?UTF-8?B?{}?=",
                        BASE64_MIME
                            .encode(value[current_window_start..idx].as_bytes())
                            .trim()
                    ));
                    if idx != value.len() - 1 {
                        ret.push(' ');
                    }
                    current_window_start = idx;
                }
                (false, false) => {}
            }
            idx += std::mem::size_of::<char>();
        }
    }
    /* If the last part of the header value is encoded, it won't be pushed inside the previous for
     * block */
    if !is_current_window_ascii {
        ret.push_str(&format!(
            "=?UTF-8?B?{}?=",
            BASE64_MIME
                .encode(value[current_window_start..].as_bytes())
                .trim()
        ));
    }
    ret
}
