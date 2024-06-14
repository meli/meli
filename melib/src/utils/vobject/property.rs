// Copyright (c) 2014-2018 Markus Unterwaditzer & contributors
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use indexmap::IndexMap;

#[derive(Clone, Debug)]
pub struct Property {
    /// Key in component.
    pub name: String,

    /// Parameters.
    pub params: IndexMap<String, String>,

    /// Value as unparsed string.
    pub raw_value: String,

    /// Property group. E.g. a contentline like `foo.FN:Markus` would result in
    /// the group being `"foo"`.
    pub prop_group: Option<String>,
}

impl Property {
    /// Create property from unescaped string.
    pub fn new<N, V>(name: N, value: V) -> Self
    where
        N: Into<String>,
        V: AsRef<str>,
    {
        Self {
            name: name.into(),
            params: IndexMap::new(),
            raw_value: escape_chars(value.as_ref()),
            prop_group: None,
        }
    }

    /// Get value as unescaped string.
    pub fn value_as_string(&self) -> String {
        unescape_chars(&self.raw_value)
    }
}

/// Escape text for a `VObject` property value.
pub fn escape_chars(s: &str) -> String {
    // Order matters! Lifted from icalendar.parser
    // https://github.com/collective/icalendar/
    s.replace("\\N", "\n")
        .replace('\\', "\\\\")
        .replace(';', "\\;")
        .replace(',', "\\,")
        .replace("\r\n", "\\n")
        .replace('\n', "\\n")
}

/// Unescape text from a `VObject` property value.
pub fn unescape_chars(s: &str) -> String {
    // Order matters! Lifted from icalendar.parser
    // https://github.com/collective/icalendar/
    s.replace("\\N", "\\n")
        .replace("\r\n", "\n")
        .replace("\\n", "\n")
        .replace("\\,", ",")
        .replace("\\;", ";")
        .replace("\\\\", "\\")
}
