//
// meli
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

//! default value functions for deserializing

pub fn false_val<T: From<bool>>() -> T {
    false.into()
}

pub fn true_val<T: From<bool>>() -> T {
    true.into()
}

pub fn zero_val<T: From<usize>>() -> T {
    0.into()
}

pub fn eighty_val<T: From<usize>>() -> T {
    80.into()
}

pub fn none<T>() -> Option<T> {
    None
}

pub fn internal_value_false<T: From<melib::conf::ToggleFlag>>() -> T {
    melib::conf::ToggleFlag::InternalVal(false).into()
}

pub fn internal_value_true<T: From<melib::conf::ToggleFlag>>() -> T {
    melib::conf::ToggleFlag::InternalVal(true).into()
}

pub fn action_internal_value_false<T: From<melib::ActionFlag>>() -> T {
    melib::conf::ActionFlag::InternalVal(false).into()
}

//pub fn action_internal_value_true<
//    T: From<melib::conf::ActionFlag>,
//>() -> T {
//    melib::conf::ActionFlag::InternalVal(true).into()
//}

pub fn ask<T: From<melib::conf::ActionFlag>>() -> T {
    melib::conf::ActionFlag::Ask.into()
}
