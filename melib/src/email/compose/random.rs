/*
 * meli - melib crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

use crate::utils::{
    base36,
    random::{clock, random_u64},
};

pub fn gen_message_id(fqdn: &str) -> String {
    let clock = base36(clock());
    let rand = base36(random_u64());

    format!("<{clock}.{rand}@{fqdn}>")
}

pub fn gen_boundary() -> String {
    let clock = base36(clock());
    let rand = base36(random_u64());
    let rand2 = base36(random_u64());

    format!("{rand}{clock}{rand2}")
}
