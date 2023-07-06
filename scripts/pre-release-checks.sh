#!/bin/bash
#
# Copyright 2017 Manos Pitsidianakis
# 
# This file is part of meli.
# 
# meli is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# meli is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with meli. If not, see <http://www.gnu.org/licenses/>.

set -euxo pipefail

cargo check -p melib --all-features
cargo clippy -p melib --all-features
cargo test -p melib --all-features --all
cargo publish -p melib --dry-run

cargo check -p meli --all-features
cargo clippy -p meli --all-features
cargo test -p meli --all-features --all
cargo publish -p meli --dry-run
