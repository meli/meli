//
// meli
//
// Copyright 2026  Manos Pitsidianakis
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

use melib::text::TextProcessing;
use melib::{
    error::{Error, ErrorKind, Result, ResultIntoError as _},
    text::{Line, LineBreakText, Reflow},
};

fn main() -> Result<()> {
    let width = std::env::args().nth(2).unwrap().parse::<usize>().unwrap();
    let text = std::fs::read_to_string(std::env::args().nth(1).unwrap())?;
    let lines = LineBreakText::new(text, Reflow::All, Some(width))
        .into_iter()
        .collect::<Vec<_>>();
    for l in lines {
        let l = &l.content.trim_end();
        assert!(l.grapheme_len() <= width);
        println!("{l}");
    }
    Ok(())
}
