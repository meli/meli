/*
 * meli - managesieve REPL
 *
 * Copyright 2020  Manos Pitsidianakis
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

#[macro_use]
extern crate melib;
use melib::*;

use std::collections::VecDeque;
extern crate notify_rust;
extern crate xdg_utils;
#[macro_use]
extern crate serde_derive;
extern crate linkify;
extern crate uuid;

extern crate serde_json;
extern crate smallvec;
extern crate termion;

use melib::backends::imap::managesieve::new_managesieve_connection;
use melib::Result;

mod unix;
use unix::*;

#[macro_use]
pub mod types;
use crate::types::*;

#[macro_use]
pub mod terminal;
use crate::terminal::*;

#[macro_use]
pub mod command;
use crate::command::*;

pub mod state;
use crate::state::*;

pub mod components;
use crate::components::*;

#[macro_use]
pub mod conf;
use crate::conf::*;

#[cfg(feature = "sqlite3")]
pub mod sqlite3;

pub mod jobs;
pub mod mailcap;
pub mod plugins;

use futures::executor::block_on;

/// Opens an interactive shell on a managesieve server. Suggested use is with rlwrap(1)
///
/// # Example invocation:
/// ```sh
/// ./manage_sieve server_hostname server_username server_password server_port");
/// ```
///
/// `danger_accept_invalid_certs` is turned on by default, so no certificate validation is performed.

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.len() != 2 {
        eprintln!("Usage: manage_sieve CONFIG_PATH ACCOUNT_NAME");
        std::process::exit(1);
    }

    let (config_path, account_name) = (
        std::mem::replace(&mut args[0], String::new()),
        std::mem::replace(&mut args[1], String::new()),
    );
    std::env::set_var("MELI_CONFIG", config_path);
    let settings = conf::Settings::new()?;
    if !settings.accounts.contains_key(&account_name) {
        eprintln!(
            "Account not found. available accounts: {}",
            settings
                .accounts
                .keys()
                .cloned()
                .collect::<Vec<String>>()
                .join(", ")
        );
        std::process::exit(1);
    }
    let mut conn = new_managesieve_connection(&settings.accounts[&account_name].account)?;
    block_on(conn.connect())?;
    let mut res = String::with_capacity(8 * 1024);

    let mut input = String::new();
    println!("managesieve shell: use 'logout'");
    loop {
        use std::io;
        use std::io::Write;
        input.clear();
        print!("> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if input.trim().eq_ignore_ascii_case("logout") {
                    break;
                }
                block_on(conn.send_command(input.as_bytes()))?;
                block_on(conn.read_lines(&mut res, String::new()))?;
                println!("out: {}", res.trim());
            }
            Err(error) => println!("error: {}", error),
        }
    }

    Ok(())
}
