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

use std::{borrow::Cow, marker::PhantomData};

use indexmap::IndexMap;

use crate::jmap::objects::Object;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Comparator<OBJ: Object> {
    pub property: Cow<'static, str>,
    pub is_ascending: bool,
    // [ref:TODO] implement collations
    pub collation: Option<Cow<'static, str>>,
    #[serde(flatten, default, skip_serializing_if = "IndexMap::is_empty")]
    pub additional_properties: IndexMap<Cow<'static, str>, Cow<'static, str>>,
    #[serde(skip)]
    pub _ph: PhantomData<fn() -> OBJ>,
}

impl<OBJ: Object> Comparator<OBJ> {
    pub fn new(property: Cow<'static, str>) -> Self {
        Self {
            property,
            is_ascending: false,
            collation: None,
            additional_properties: IndexMap::new(),
            _ph: PhantomData,
        }
    }

    _impl!(property: Cow<'static, str>);
    _impl!(is_ascending: bool);
    _impl!(collation: Option<Cow<'static, str>>);
    _impl!(additional_properties: IndexMap<Cow<'static, str>, Cow<'static, str>>);
}
