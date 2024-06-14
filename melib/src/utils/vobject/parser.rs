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

use super::{component::Component, property::Property};

#[derive(Clone, Debug)]
pub enum ParseErrorReason {
    //#[error("trailing data: {}", _0)]
    TrailingData(String),
    //#[error("expected {}, found EOL", _0)]
    UnexpectedEol(char),
    //#[error("expected {}, found {}", _0, _1)]
    UnexpectedChar(char, char),
    //#[error("expected EOL")]
    ExpectedEol,
    //#[error("no property name found")]
    NoPropertyName,
    //#[error("no parameter name found")]
    NoParameterName,
    //#[error("expected BEGIN tag")]
    ExpectedBegin,
    //#[error("mismatched tags: BEGIN:{} vs END:{}", _0, _1)]
    MismatchedTag(String, String),
}

type ParseResult<T> = Result<T, ParseErrorReason>;

pub struct Parser<'s> {
    pub input: &'s str,
    pub pos: usize,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s str) -> Self {
        Parser { input, pos: 0 }
    }

    /// look-ahead for next char at given offset from current position
    /// (self.pos), taking [line unfolding]
    /// (<https://tools.ietf.org/html/rfc5545#section-3.1>) into account,
    /// without actually
    /// consuming it (immutable self).
    ///
    /// Return an option for next char, and needed increment to consume it
    /// from current position.
    /// CR characters get always skipped, resulting in CRLF to be simplified as
    /// LF, which seems to be acceptable because
    /// - the remainders of the lib do accept a lone LF as a line termination (a
    ///   bit laxer than RFC 5545)
    /// - CR alone [is not acceptable content] (<https://tools.ietf.org/html/rfc5545#section-3.1>)
    fn peek_at(&self, at: usize) -> Option<(char, usize)> {
        match self.input[self.pos + at..].chars().next() {
            None => None,
            Some('\r') => self.peek_at(at + 1),
            Some('\n') => match self.peek_at(at + 1) {
                Some((' ', offset)) | Some(('\t', offset)) => self.peek_at(offset),
                _ => Some(('\n', at + 1)),
            },
            Some(x) => Some((x, at + x.len_utf8())),
        }
    }

    #[inline]
    fn peek(&self) -> Option<(char, usize)> {
        self.peek_at(0)
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn assert_char(&self, c: char) -> ParseResult<()> {
        let real_c = match self.peek() {
            Some((x, _)) => x,
            None => return Err(ParseErrorReason::UnexpectedEol(c)),
        };

        if real_c != c {
            return Err(ParseErrorReason::UnexpectedChar(c, real_c));
        };

        Ok(())
    }

    pub fn consume_char(&mut self) -> Option<char> {
        match self.peek() {
            Some((c, offset)) => {
                self.pos += offset;
                Some(c)
            }
            None => None,
        }
    }

    /// If next peeked char is the given `c`, consume it and return `true`,
    /// otherwise return `false`.
    pub fn consume_only_char(&mut self, c: char) -> bool {
        match self.peek() {
            Some((d, offset)) if d == c => {
                self.pos += offset;
                true
            }
            _ => false,
        }
    }

    fn consume_eol(&mut self) -> ParseResult<()> {
        let start_pos = self.pos;

        let consumed = match self.consume_char() {
            Some('\n') => true,
            Some('\r') => matches!(self.consume_char(), Some('\n')),
            _ => false,
        };

        if consumed {
            return Ok(());
        }
        self.pos = start_pos;
        Err(ParseErrorReason::ExpectedEol)
    }

    fn sloppy_terminate_line(&mut self) -> ParseResult<()> {
        if !self.eof() {
            self.consume_eol()?;
            while self.consume_eol().is_ok() {}
        };

        Ok(())
    }

    // GR this used to return just a slice from input, but line unfolding
    // makes it contradictory, unless one'd want to rescan everything.
    // Since actually useful calls used to_owned() on the result, which
    // does copy into a String's buffer, let's create a String right away
    // implementation detail : instead of pushing char after char, we
    // do it by the biggest contiguous slices possible, because I believe it
    // to be more efficient (less checks for reallocation etc).
    pub fn consume_while<F: Fn(char) -> bool>(&mut self, test: F) -> String {
        let mut sl_start_pos = self.pos;
        let mut res = String::new();
        while !self.eof() {
            match self.peek() {
                Some((c, offset)) => {
                    if !test(c) {
                        break;
                    } else {
                        if offset > c.len_utf8() {
                            // we have some skipping and therefore need to flush
                            res.push_str(&self.input[sl_start_pos..self.pos]);
                            res.push(c);
                            sl_start_pos = self.pos + offset;
                        }
                        self.pos += offset;
                    }
                }
                _ => break,
            }
        }
        // Final flush
        if sl_start_pos < self.pos {
            res.push_str(&self.input[sl_start_pos..self.pos])
        }
        res
    }

    pub fn consume_property(&mut self) -> ParseResult<Property> {
        let group = self.consume_property_group().ok();
        let name = self.consume_property_name()?;
        let params = self.consume_params();

        self.assert_char(':')?;
        self.consume_char();

        let value = self.consume_property_value()?;

        Ok(Property {
            name,
            params,
            raw_value: value,
            prop_group: group,
        })
    }

    fn consume_property_name(&mut self) -> ParseResult<String> {
        let rv = self.consume_while(|x| x == '-' || x.is_alphanumeric());
        if rv.is_empty() {
            Err(ParseErrorReason::NoPropertyName)
        } else {
            Ok(rv)
        }
    }

    fn consume_property_group(&mut self) -> ParseResult<String> {
        let start_pos = self.pos;
        let name = self.consume_property_name();

        let e = match name {
            Ok(name) => match self.assert_char('.') {
                Ok(_) => {
                    self.consume_char();
                    return Ok(name);
                }
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        };

        self.pos = start_pos;
        e
    }

    fn consume_property_value(&mut self) -> ParseResult<String> {
        let rv = self.consume_while(|x| x != '\r' && x != '\n');
        self.sloppy_terminate_line()?;
        Ok(rv)
    }

    fn consume_param_name(&mut self) -> ParseResult<String> {
        self.consume_property_name()
            .map_err(|_| ParseErrorReason::NoParameterName)
    }

    fn consume_param_value(&mut self) -> ParseResult<String> {
        let qsafe = |x| x != '"' && x != '\r' && x != '\n' && x != '\u{7F}' && x > '\u{1F}';

        if self.consume_only_char('"') {
            let rv = self.consume_while(qsafe);
            self.assert_char('"')?;
            self.consume_char();
            Ok(rv)
        } else {
            Ok(self.consume_while(|x| qsafe(x) && x != ';' && x != ':'))
        }
    }

    fn consume_param(&mut self) -> ParseResult<(String, String)> {
        let name = self.consume_param_name()?;
        let start_pos = self.pos;
        let value = if self.consume_only_char('=') {
            match self.consume_param_value() {
                Ok(x) => x,
                Err(e) => {
                    self.pos = start_pos;
                    return Err(e);
                }
            }
        } else {
            String::new()
        };

        Ok((name, value))
    }

    fn consume_params(&mut self) -> IndexMap<String, String> {
        let mut rv: IndexMap<String, String> = IndexMap::new();
        while self.consume_only_char(';') {
            match self.consume_param() {
                Ok((name, value)) => {
                    rv.insert(name.to_owned(), value.to_owned());
                }
                Err(_) => break,
            }
        }
        rv
    }

    pub fn consume_component(&mut self) -> ParseResult<Component> {
        let start_pos = self.pos;
        let mut property = self.consume_property()?;
        if property.name != "BEGIN" {
            self.pos = start_pos;
            return Err(ParseErrorReason::ExpectedBegin);
        };

        // Create a component with the name of the BEGIN tag's value
        let mut component = Component::new(property.raw_value);

        loop {
            let previous_pos = self.pos;
            property = self.consume_property()?;
            if property.name == "BEGIN" {
                self.pos = previous_pos;
                component.subcomponents.push(self.consume_component()?);
            } else if property.name == "END" {
                if property.raw_value != component.name {
                    self.pos = start_pos;
                    return Err(ParseErrorReason::MismatchedTag(
                        component.name,
                        property.raw_value,
                    ));
                }

                break;
            } else {
                component.push(property);
            }
        }

        Ok(component)
    }
}
