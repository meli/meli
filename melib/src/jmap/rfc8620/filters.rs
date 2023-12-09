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

pub trait FilterTrait<T>: Default {}
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum Filter<F: FilterTrait<OBJ>, OBJ: Object> {
    Operator {
        operator: FilterOperator,
        conditions: Vec<Filter<F, OBJ>>,
    },
    Condition(FilterCondition<F, OBJ>),
}

impl<F: FilterTrait<OBJ>, OBJ: Object> FilterTrait<OBJ> for Filter<F, OBJ> {}
impl<F: FilterTrait<OBJ>, OBJ: Object> FilterTrait<OBJ> for FilterCondition<F, OBJ> {}

#[derive(Debug, Serialize)]
pub struct FilterCondition<F: FilterTrait<OBJ>, OBJ: Object> {
    #[serde(flatten)]
    pub cond: F,
    #[serde(skip)]
    pub _ph: PhantomData<fn() -> OBJ>,
}

#[derive(Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum FilterOperator {
    And,
    Or,
    Not,
}

impl<F: FilterTrait<OBJ>, OBJ: Object> FilterCondition<F, OBJ> {
    pub fn new() -> Self {
        Self {
            cond: F::default(),
            _ph: PhantomData,
        }
    }
}

impl<F: FilterTrait<OBJ>, OBJ: Object> Default for FilterCondition<F, OBJ> {
    fn default() -> Self {
        Self::new()
    }
}

impl<F: FilterTrait<OBJ>, OBJ: Object> Default for Filter<F, OBJ> {
    fn default() -> Self {
        Self::Condition(FilterCondition::default())
    }
}

use std::ops::{BitAndAssign, BitOrAssign, Not};

impl<F: FilterTrait<OBJ>, OBJ: Object> BitAndAssign for Filter<F, OBJ> {
    fn bitand_assign(&mut self, rhs: Self) {
        match self {
            Self::Operator {
                operator: FilterOperator::And,
                ref mut conditions,
            } => {
                conditions.push(rhs);
            }
            Self::Condition(_) | Self::Operator { .. } => {
                *self = Self::Operator {
                    operator: FilterOperator::And,
                    conditions: vec![
                        std::mem::replace(self, Self::Condition(FilterCondition::new())),
                        rhs,
                    ],
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
            } => {
                conditions.push(rhs);
            }
            Self::Condition(_) | Self::Operator { .. } => {
                *self = Self::Operator {
                    operator: FilterOperator::Or,
                    conditions: vec![
                        std::mem::replace(self, Self::Condition(FilterCondition::new())),
                        rhs,
                    ],
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
            } => Self::Operator {
                operator: FilterOperator::Or,
                conditions,
            },
            Self::Condition(_) | Self::Operator { .. } => Self::Operator {
                operator: FilterOperator::Not,
                conditions: vec![self],
            },
        }
    }
}
