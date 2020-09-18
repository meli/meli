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
use melib::Result;

/// Parses e-mail from files and prints the debug information of the parsed `Envelope`
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
            let buffer = std::fs::read_to_string(&filename)
                .expect(&format!("Something went wrong reading the file {}", i,));
            let res =
                melib::backends::mbox::mbox_parse(Default::default(), buffer.as_bytes(), 0, None);
            match res {
                Ok((_, v)) => {
                    println!("{} envelopes parsed", v.len());
                }
                Err(melib::nom::Err::Error(err)) => {
                    println!(
                        "Error in parsing {:?}",
                        unsafe { std::str::from_utf8_unchecked(err.0) }
                            .chars()
                            .take(150)
                            .collect::<String>()
                    );
                }
                Err(err) => {
                    println!("Error in parsing {:?}", err);
                }
            }
        } else {
            println!("{} is not a valid file.", i);
        }
    }
    Ok(())
}
