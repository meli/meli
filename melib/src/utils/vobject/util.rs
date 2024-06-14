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

macro_rules! make_getter_function_for_optional {
    ($fnname:ident, $name:expr, $mapper:ty) => {
        pub fn $fnname(&self) -> Option<$mapper> {
            self.0.get_only($name).cloned().map(From::from)
        }
    };
}

macro_rules! make_getter_function_for_values {
    ($fnname:ident, $name:expr, $mapper:ty) => {
        pub fn $fnname(&self) -> Vec<$mapper> {
            self.0
                .get_all($name)
                .iter()
                .map(Clone::clone)
                .map(From::from)
                .collect()
        }
    };
}

macro_rules! create_data_type {
    ( $name:ident ) => {
        #[derive(Eq, PartialEq, Debug)]
        pub struct $name(String, indexmap::IndexMap<String, String>);

        impl $name {
            pub fn new(raw: String, params: indexmap::IndexMap<String, String>) -> $name {
                $name(raw, params)
            }

            pub fn from_raw(raw: String) -> $name {
                $name(raw, indexmap::IndexMap::new())
            }

            pub fn raw(&self) -> &String {
                &self.0
            }

            pub fn into_raw(self) -> String {
                self.0
            }

            pub fn params(&self) -> &indexmap::IndexMap<String, String> {
                &self.1
            }
        }

        impl From<Property> for $name {
            fn from(p: Property) -> $name {
                $name::new(p.raw_value, p.params)
            }
        }
    };
}

pub const DATE_TIME_FMT: &str = "%Y%m%dT%H%M%SZ";

pub const DATE_FMT: &str = "%Y%m%d";
