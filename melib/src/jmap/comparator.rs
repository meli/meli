/*
 * meli - jmap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use std::marker::PhantomData;

use crate::jmap::objects::Object;

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Comparator<OBJ: Object> {
    property: String,
    #[serde(default = "bool_true")]
    is_ascending: bool,
    // [ref:TODO] implement collations
    collation: Option<String>,
    //#[serde(flatten)]
    additional_properties: Vec<String>,

    _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ: Object> Comparator<OBJ> {
    pub fn new() -> Self {
        Self {
            property: String::new(),
            is_ascending: true,
            collation: None,
            additional_properties: Vec::new(),
            _ph: PhantomData,
        }
    }

    _impl!(property: String);
    _impl!(is_ascending: bool);
    _impl!(collation: Option<String>);
    _impl!(additional_properties: Vec<String>);
}

impl<OBJ: Object> Default for Comparator<OBJ> {
    fn default() -> Self {
        Self::new()
    }
}
