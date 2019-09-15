/*
 * melib
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

use chrono::offset::Local;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Copy, Clone, PartialEq, PartialOrd, Hash, Debug)]
pub enum LoggingLevel {
    OFF,
    FATAL,
    ERROR,
    WARN,
    INFO,
    DEBUG,
    TRACE,
}

impl std::fmt::Display for LoggingLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OFF => "OFF",
                FATAL => "FATAL",
                ERROR => "ERROR",
                WARN => "WARN",
                INFO => "INFO",
                DEBUG => "DEBUG",
                TRACE => "TRACE",
            }
        )
    }
}

use LoggingLevel::*;

struct LoggingBackend {
    dest: BufWriter<std::fs::File>,
    level: LoggingLevel,
}

thread_local!(static LOG: Arc<Mutex<LoggingBackend>> = Arc::new(Mutex::new({
    let data_dir = xdg::BaseDirectories::with_prefix("meli").unwrap();
let log_file = OpenOptions::new().append(true) /* writes will append to a file instead of overwriting previous contents */
                         .create(true) /* a new file will be created if the file does not yet already exist.*/
                         .read(true)
                         .open(data_dir.place_data_file("meli.log").unwrap()).unwrap();
LoggingBackend {
    dest: BufWriter::new(log_file),
    level: INFO,
}
})));

pub fn log(val: String, level: LoggingLevel) {
    LOG.with(|f| {
        let mut b = f.lock().unwrap();
        if level <= b.level {
            b.dest
                .write_all(Local::now().to_string().as_bytes())
                .unwrap();
            b.dest.write_all(b" [").unwrap();
            b.dest.write_all(level.to_string().as_bytes()).unwrap();
            b.dest.write_all(b"]: ").unwrap();
            b.dest.write_all(val.as_bytes()).unwrap();
            b.dest.write_all(b"\n").unwrap();
            b.dest.flush().unwrap();
        }
    });
}

pub fn get_log_level() -> LoggingLevel {
    let mut level = INFO;
    LOG.with(|f| {
        level = f.lock().unwrap().level;
    });
    level
}

pub fn change_log_dest(path: PathBuf) {
    LOG.with(|f| {
        let mut backend = f.lock().unwrap();
        backend.dest = BufWriter::new(OpenOptions::new().append(true) /* writes will append to a file instead of overwriting previous contents */
                         .create(true) /* a new file will be created if the file does not yet already exist.*/
                         .read(true)
                         .open(path).unwrap());
    });
}

pub fn change_log_level(new_val: LoggingLevel) {
    LOG.with(|f| {
        let mut backend = f.lock().unwrap();
        backend.level = new_val;
    });
}
