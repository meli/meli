//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use super::*;

#[test]
fn test_version_migrations_version_map() {
    let version_map = indexmap::indexmap! {
        v0_8_8::V0_8_8_ID => Box::new(V0_8_8) as Box<dyn Version + Send + Sync + 'static>,
        v0_8_9::V0_8_9_ID => Box::new(V0_8_9) as Box<dyn Version + Send + Sync + 'static>,
    };
    assert!(
        version_map.contains_key("0.8.8"),
        "Could not access Version identifier by &str key in version map"
    );
    assert!(
        version_map.contains_key("0.8.9"),
        "Could not access Version identifier by &str key in version map"
    );
    assert!(!version_map.contains_key("0.0.0"),);
    assert!(
        version_map.contains_key(&v0_8_8::V0_8_8_ID),
        "Could not access Version identifier by VersionIdentifier key in version map"
    );
}

#[test]
fn test_version_migrations_returns_correct_migration() {
    let version_map = indexmap::indexmap! {
        v0_8_8::V0_8_8_ID => Box::new(V0_8_8) as Box<dyn Version + Send + Sync + 'static>,
        v0_8_9::V0_8_9_ID => Box::new(V0_8_9) as Box<dyn Version + Send + Sync + 'static>,
    };
    let migrations = calculate_migrations(Some("0.8.8"), &version_map);
    assert!(
        migrations.is_empty(),
        "Calculated migrations between 0.8.8 and 0.8.9 are not empty: {:?}",
        migrations
    );
    let migrations = calculate_migrations(None, &version_map);
    assert!(
        !migrations.is_empty(),
        "Calculated migrations between no version and 0.8.8 are empty",
    );
}
