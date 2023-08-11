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

extern crate linkify;
extern crate meli;
extern crate melib;
extern crate serde_derive;
extern crate serde_json;
extern crate smallvec;
extern crate termion;

use futures::executor::block_on;
use meli::*;
use melib::{imap::managesieve::ManageSieveConnection, Result, *};

/// Opens an interactive shell on a managesieve server. Suggested use is with
/// rlwrap(1)
///
/// # Example invocation:
/// ```sh
/// ./manage_sieve server_hostname server_username server_password server_port");
/// ```
///
/// `danger_accept_invalid_certs` is turned on by default, so no certificate
/// validation is performed.

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.len() != 2 {
        eprintln!("Usage: manage_sieve CONFIG_PATH ACCOUNT_NAME");
        std::process::exit(1);
    }

    let (config_path, account_name) = (std::mem::take(&mut args[0]), std::mem::take(&mut args[1]));
    std::env::set_var("MELI_CONFIG", config_path);
    let settings = meli::conf::Settings::new()?;
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
    let account = &settings.accounts[&account_name].account;
    let mut conn = ManageSieveConnection::new(
        AccountHash::default(),
        account_name.clone(),
        account,
        melib::backends::BackendEventConsumer::new(std::sync::Arc::new(|_, _| {})),
    )?;
    block_on(conn.inner.connect())?;

    let mut input = String::new();
    const AVAILABLE_COMMANDS: &[&str] = &[
        "help",
        "logout",
        "listscripts",
        "checkscript",
        "putscript",
        "setactive",
        "getscript",
        "deletescript",
    ];
    const COMMANDS_HELP: &[&str] = &[
        "help",
        "logout",
        "listscripts and whether they are active",
        "paste a script to check for validity without uploading it",
        "upload a script",
        "set a script as active",
        "get a script by its name",
        "delete a script by its name",
    ];
    println!("managesieve shell: use 'help' for available commands");
    enum PrevCmd {
        None,
        Checkscript,
        PutscriptName,
        PutscriptString(String),
        SetActiveName,
        GetScriptName,
    }
    use PrevCmd::*;
    let mut prev_cmd: PrevCmd = None;
    loop {
        use std::{io, io::Write};
        input.clear();
        print!("> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                if input.eq_ignore_ascii_case("logout") {
                    break;
                }
                if input.eq_ignore_ascii_case("help") {
                    println!("available commands: [{}]", AVAILABLE_COMMANDS.join(", "));
                    continue;
                }
                if input.len() >= "help ".len()
                    && input[0.."help ".len()].eq_ignore_ascii_case("help ")
                {
                    if let Some(i) = AVAILABLE_COMMANDS
                        .iter()
                        .position(|cmd| cmd.eq_ignore_ascii_case(&input["help ".len()..]))
                    {
                        println!("{}", COMMANDS_HELP[i]);
                    } else {
                        println!("invalid command `{}`", &input["help ".len()..]);
                    }
                    continue;
                }
                if input.eq_ignore_ascii_case("listscripts") {
                    let scripts = block_on(conn.listscripts())?;
                    println!("Got {} scripts:", scripts.len());
                    for (script, active) in scripts {
                        println!(
                            "{}active: {}",
                            if active { "" } else { "in" },
                            String::from_utf8_lossy(&script)
                        );
                    }
                } else if input.eq_ignore_ascii_case("checkscript") {
                    prev_cmd = Checkscript;
                    println!("insert file path of script");
                } else if input.eq_ignore_ascii_case("putscript") {
                    prev_cmd = PutscriptName;
                    println!("Insert script name");
                } else if input.eq_ignore_ascii_case("setactive") {
                    prev_cmd = SetActiveName;
                } else if input.eq_ignore_ascii_case("getscript") {
                    prev_cmd = GetScriptName;
                } else if input.eq_ignore_ascii_case("deletescript") {
                    println!("unimplemented `{}`", input);
                } else {
                    match prev_cmd {
                        None => println!("invalid command `{}`", input),
                        Checkscript => {
                            let content = std::fs::read_to_string(input).unwrap();
                            let result = block_on(conn.checkscript(content.as_bytes()));
                            println!("Got {:?}", result);
                            prev_cmd = None;
                        }
                        PutscriptName => {
                            prev_cmd = PutscriptString(input.to_string());
                            println!("insert file path of script");
                        }
                        PutscriptString(name) => {
                            prev_cmd = None;
                            let content = std::fs::read_to_string(input).unwrap();
                            let result =
                                block_on(conn.putscript(name.as_bytes(), content.as_bytes()));
                            println!("Got {:?}", result);
                        }
                        SetActiveName => {
                            prev_cmd = None;
                            let result = block_on(conn.setactive(input.as_bytes()));
                            println!("Got {:?}", result);
                        }
                        GetScriptName => {
                            prev_cmd = None;
                            let result = block_on(conn.getscript(input.as_bytes()));
                            println!("Got {:?}", result);
                        }
                    }
                }

                //block_on(conn.send_command(input.as_bytes()))?;
                //block_on(conn.read_lines(&mut res, String::new()))?;
                //println!("out: {}", res.trim());
            }
            Err(error) => println!("error: {}", error),
        }
    }

    Ok(())
}
