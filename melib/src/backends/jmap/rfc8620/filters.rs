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

use super::*;

pub trait FilterTrait<T> {}
#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum Filter<F: FilterTrait<OBJ>, OBJ: Object> {
    Operator {
        operator: FilterOperator,
        conditions: Vec<Filter<F, OBJ>>,
    },
    Condition(FilterCondition<F, OBJ>),
}

#[derive(Serialize, Debug)]
pub struct FilterCondition<F: FilterTrait<OBJ>, OBJ: Object> {
    #[serde(flatten)]
    cond: F,
    #[serde(skip)]
    _ph: PhantomData<*const OBJ>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "UPPERCASE")]
pub enum FilterOperator {
    And,
    Or,
    Not,
}
