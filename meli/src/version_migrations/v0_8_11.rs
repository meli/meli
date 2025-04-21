//
// meli
//
// Copyright 2025 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

//! <https://release.meli-email.org/v0.8.11>

use crate::version_migrations::*;

/// <https://release.meli-email.org/v0.8.11>
pub const V0_8_11_ID: VersionIdentifier = VersionIdentifier {
    string: "0.8.11",
    major: 0,
    minor: 8,
    patch: 11,
    pre: "",
};

/// <https://release.meli-email.org/v0.8.11>
#[derive(Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
pub struct V0_8_11;

impl Version for V0_8_11 {
    fn version(&self) -> &VersionIdentifier {
        &V0_8_11_ID
    }

    fn migrations(&self) -> Vec<Box<dyn Migration + Send + Sync + 'static>> {
        vec![]
    }
}
