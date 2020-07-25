/*
 * meli
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

use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::sync::{Arc, Mutex};

thread_local!(static CMD_HISTORY_FILE: Arc<Mutex<std::fs::File>> = Arc::new(Mutex::new({
    let data_dir = xdg::BaseDirectories::with_prefix("meli").unwrap();
OpenOptions::new().append(true) /* writes will append to a file instead of overwriting previous contents */
                         .create(true) /* a new file will be created if the file does not yet already exist.*/
                         .read(true)
                         .open(data_dir.place_data_file("cmd_history").unwrap()).unwrap()
})));

pub fn log_cmd(mut cmd: String) {
    CMD_HISTORY_FILE.with(|f| {
        cmd.push('\n');
        f.lock().unwrap().write_all(cmd.as_bytes()).unwrap();
    });
}

pub fn old_cmd_history() -> Vec<String> {
    let mut ret = Vec::new();
    CMD_HISTORY_FILE.with(|f| {
        let mut old_history = String::new();
        f.lock().unwrap().read_to_string(&mut old_history).unwrap();
        ret.extend(old_history.lines().map(|s| s.to_string()));
    });
    ret
}
