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

pub trait FilterTrait<T>: Default + Send + Sync {}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum Filter<F: FilterTrait<OBJ>, OBJ: Object> {
    Operator {
        operator: FilterOperator,
        conditions: Vec<Filter<F, OBJ>>,
        #[serde(skip)]
        _ph: PhantomData<fn() -> OBJ>,
    },
    #[serde(untagged)]
    Condition(F),
}

impl<F: FilterTrait<OBJ>, OBJ: Object> FilterTrait<OBJ> for Filter<F, OBJ> {}

#[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum FilterOperator {
    And,
    Or,
    Not,
}

impl<F: FilterTrait<OBJ>, OBJ: Object> Default for Filter<F, OBJ> {
    fn default() -> Self {
        Self::Condition(F::default())
    }
}

use std::ops::{BitAndAssign, BitOrAssign, Not};

impl<F: FilterTrait<OBJ>, OBJ: Object> BitAndAssign for Filter<F, OBJ> {
    fn bitand_assign(&mut self, rhs: Self) {
        match self {
            Self::Operator {
                operator: FilterOperator::And,
                ref mut conditions,
                _ph: _,
            } => {
                conditions.push(rhs);
            }
            Self::Condition(_) | Self::Operator { .. } => {
                *self = Self::Operator {
                    operator: FilterOperator::And,
                    conditions: vec![std::mem::take(self), rhs],
                    _ph: PhantomData,
                };
            }
        }
    }
}

impl<F: FilterTrait<OBJ>, OBJ: Object> BitOrAssign for Filter<F, OBJ> {
    fn bitor_assign(&mut self, rhs: Self) {
        match self {
            Self::Operator {
                operator: FilterOperator::Or,
                ref mut conditions,
                _ph: _,
            } => {
                conditions.push(rhs);
            }
            Self::Condition(_) | Self::Operator { .. } => {
                *self = Self::Operator {
                    operator: FilterOperator::Or,
                    conditions: vec![std::mem::take(self), rhs],
                    _ph: PhantomData,
                };
            }
        }
    }
}

impl<F: FilterTrait<OBJ>, OBJ: Object> Not for Filter<F, OBJ> {
    type Output = Self;
    fn not(self) -> Self {
        match self {
            Self::Operator {
                operator: FilterOperator::Not,
                conditions,
                _ph: _,
            } => Self::Operator {
                operator: FilterOperator::Or,
                conditions,
                _ph: PhantomData,
            },
            Self::Condition(_) | Self::Operator { .. } => Self::Operator {
                operator: FilterOperator::Not,
                conditions: vec![self],
                _ph: PhantomData,
            },
        }
    }
}
