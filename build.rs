/*
 * meli - bin.rs
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
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;
use std::process::Command;

fn main() -> Result<(), std::io::Error> {
    if let Err(e) = std::fs::create_dir("src/manuals") {
        if e.kind() != std::io::ErrorKind::AlreadyExists {
            Err(e)?;
        }
    }
    let meli_1_metadata = std::fs::metadata("meli.1")?;
    if let Ok(metadata) = std::fs::metadata("src/manuals/meli.txt") {
        if metadata.modified()? < meli_1_metadata.modified()? {
            let output = Command::new("mandoc").args(&["meli.1"]).output()?;
            let man_path = PathBuf::from("src/manuals/meli.txt");
            let file = File::create(&man_path)?;
            BufWriter::new(file).write_all(&output.stdout)?;
        }
    }
    let meli_conf_5_metadata = std::fs::metadata("meli.conf.5")?;
    if let Ok(metadata) = std::fs::metadata("src/manuals/meli_conf.txt") {
        if metadata.modified()? < meli_conf_5_metadata.modified()? {
            let output = Command::new("mandoc").args(&["meli.conf.5"]).output()?;
            let man_path = PathBuf::from("src/manuals/meli_conf.txt");
            let file = File::create(&man_path)?;
            BufWriter::new(file).write_all(&output.stdout)?;
        }
    }
    Ok(())
}
