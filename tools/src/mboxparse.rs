/*
 * meli - mboxparse.rs
 *
 * Copyright 2020 Manos Pitsidianakis
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

extern crate melib;
use melib::{mbox, parser::BytesExt, text::Truncate, Result};

/// Parses e-mail from files and prints the debug information of the parsed
/// `Envelope`
///
/// # Example invocation
/// ```sh
/// ./mboxparse /path/to/mbox"
/// ```
fn main() -> Result<()> {
    if std::env::args().len() == 1 {
        eprintln!("Usage: ./mboxparse /path/to/mbox");
        std::process::exit(1);
    }

    for i in std::env::args().skip(1) {
        println!("Path is {}", i);
        let filename = std::path::PathBuf::from(&i);

        if filename.exists() && filename.is_file() {
            let buffer = std::fs::read(&filename).unwrap_or_else(|err| {
                panic!("Something went wrong reading the file {}: {}", i, err)
            });
            let is_crlf: bool = buffer.find(b"\r\n").is_some();
            let res = mbox::mbox_parse(
                Default::default(),
                buffer.as_slice(),
                0,
                mbox::MboxFormat::MboxCl,
                is_crlf,
            );
            match res {
                Ok((_, v)) => {
                    println!("{} envelopes parsed", v.len());
                }
                Err((error_location, err)) => {
                    let error_offset = buffer.len() - error_location.len();
                    let line_number = 1 + String::from_utf8_lossy(&buffer[..error_offset])
                        .lines()
                        .count();
                    println!(
                        "Error in parsing file:\nError: {}\nLocation: line {}\n{:?}",
                        err,
                        line_number,
                        String::from_utf8_lossy(error_location)
                            .as_ref()
                            .trim_at_boundary(150)
                    );
                }
            }
        } else {
            println!("{} is not a valid file.", i);
        }
    }
    Ok(())
}
