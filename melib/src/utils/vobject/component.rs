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

use std::str::FromStr;

use indexmap::IndexMap;

use super::{
    error::VObjectError,
    parser::{ParseErrorReason, Parser},
    property::Property,
};

#[derive(Clone, Debug)]
pub struct Component {
    /// The name of the component, such as `VCARD` or `VEVENT`.
    pub name: String,
    /// The component's properties.
    pub props: IndexMap<String, Vec<Property>>,
    /// The component's child- or sub-components.
    pub subcomponents: Vec<Component>,
}

impl Component {
    pub fn new<N: Into<String>>(name: N) -> Self {
        Self {
            name: name.into(),
            props: IndexMap::new(),
            subcomponents: vec![],
        }
    }

    /// Append the given property, preserve other same-named properties.
    pub fn push(&mut self, prop: Property) {
        self.props.entry(prop.name.clone()).or_default().push(prop);
    }

    /// Set the given property, remove other same-named properties.
    pub fn set(&mut self, prop: Property) {
        self.props.insert(prop.name.clone(), vec![prop]);
    }

    /// Retrieve one property by key. Returns `None` if not exactly one property
    /// was found.
    pub fn get_only<P: AsRef<str>>(&self, name: P) -> Option<&Property> {
        match self.props.get(name.as_ref()) {
            Some(x) if x.len() == 1 => x.first(),
            _ => None,
        }
    }

    /// Retrieve properties by key. Returns an empty slice if key doesn't exist.
    pub fn get_all<P: AsRef<str>>(&self, name: P) -> &[Property] {
        static EMPTY: &[Property] = &[];
        match self.props.get(name.as_ref()) {
            Some(values) => &values[..],
            None => EMPTY,
        }
    }

    /// Remove a single property.
    pub fn pop<P: AsRef<str>>(&mut self, name: P) -> Option<Property> {
        match self.props.get_mut(name.as_ref()) {
            Some(values) => values.pop(),
            None => None,
        }
    }

    /// Remove all properties
    pub fn remove<P: AsRef<str>>(&mut self, name: P) -> Option<Vec<Property>> {
        self.props.shift_remove(name.as_ref())
    }
}

impl FromStr for Component {
    type Err = VObjectError;

    /// Same as [`parse_component`]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_component(s)
    }
}

/// Parse exactly one component. Trailing data generates errors.
pub fn parse_component(s: &str) -> Result<Component, VObjectError> {
    let (rv, new_s) = read_component(s)?;
    if !new_s.is_empty() {
        return Err(ParseErrorReason::TrailingData(new_s.into()).into());
    }

    Ok(rv)
}

/// Parse one component and return the rest of the string.
pub fn read_component(s: &str) -> Result<(Component, &str), VObjectError> {
    let mut parser = Parser::new(s);
    let rv = parser.consume_component()?;
    let new_s = if parser.eof() {
        ""
    } else {
        &parser.input[parser.pos..]
    };
    Ok((rv, new_s))
}

/// Write a component to a String.
pub fn write_component(c: &Component) -> String {
    fn inner(buf: &mut String, c: &Component) {
        buf.push_str("BEGIN:");
        buf.push_str(&c.name);
        buf.push_str("\r\n");
        buf.push_str("VERSION:4.0\r\n");

        for (prop_name, props) in &c.props {
            for prop in props.iter() {
                if let Some(ref x) = prop.prop_group {
                    buf.push_str(x);
                    buf.push('.');
                };
                buf.push_str(prop_name);
                for (param_key, param_value) in &prop.params {
                    buf.push(';');
                    buf.push_str(param_key);
                    buf.push('=');
                    buf.push_str(param_value);
                }
                buf.push(':');
                buf.push_str(&fold_line(&prop.raw_value));
                buf.push_str("\r\n");
            }
        }

        for subcomponent in &c.subcomponents {
            inner(buf, subcomponent);
        }

        buf.push_str("END:");
        buf.push_str(&c.name);
        buf.push_str("\r\n");
    }

    let mut buf = String::new();
    inner(&mut buf, c);
    buf
}

/// Fold contentline to 75 bytes or less. This function assumes the input
/// to be unfolded, which means no '\n' or '\r' in it.
pub fn fold_line(line: &str) -> String {
    const LIMIT: usize = 75;
    let len = line.len();
    if len <= LIMIT {
        return line.to_string();
    }
    let mut bytes_remaining = len;
    let mut ret = String::with_capacity(len + (len / LIMIT * 3));

    let mut pos = 0;
    let mut next_pos = LIMIT;
    while bytes_remaining > LIMIT {
        while !line.is_char_boundary(next_pos) {
            next_pos -= 1;
        }
        ret.push_str(&line[pos..next_pos]);
        ret.push_str("\r\n ");

        bytes_remaining -= next_pos - pos;
        pos = next_pos;
        next_pos += LIMIT;
    }

    ret.push_str(&line[len - bytes_remaining..]);
    ret
}
