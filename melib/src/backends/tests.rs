//
// meli - backends module
//
// Copyright 2017 Manos Pitsidianakis
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

use super::{EnvelopeHash, LazyCountSet};

#[test]
fn test_lazy_count_set() {
    let mut new = LazyCountSet::default();
    assert_eq!(new.len(), 0);
    new.set_not_yet_seen(10);
    assert_eq!(new.len(), 10);
    for i in 0..10 {
        assert!(new.insert_existing(EnvelopeHash(i)));
    }
    assert_eq!(new.len(), 10);
    assert!(!new.insert_existing(EnvelopeHash(10)));
    assert_eq!(new.len(), 10);
}
