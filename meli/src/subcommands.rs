/*
 * meli - subcommands.rs
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

#[cfg(feature = "cli-docs")]
use std::io::prelude::*;
use std::{
    path::PathBuf,
    process::{Command, Stdio},
};

use crossbeam::channel::{Receiver, Sender};
use melib::{Result, ShellExpandTrait};

use crate::*;

pub fn create_config(path: Option<PathBuf>) -> Result<()> {
    let config_path = if let Some(path) = path {
        path.expand()
    } else {
        conf::get_config_file()?
    };
    if config_path.exists() {
        return Err(Error::new(format!(
            "File `{}` already exists.\nMaybe you meant to specify another path?",
            config_path.display()
        )));
    }
    conf::create_config_file(&config_path)?;
    Ok(())
}

pub fn edit_config() -> Result<()> {
    let editor = std::env::var("EDITOR")
        .or_else(|_| std::env::var("VISUAL"))
        .map_err(|err| {
            format!("Could not find any value in environment variables EDITOR and VISUAL. {err}")
        })?;
    let config_path = crate::conf::get_config_file()?;

    let mut cmd = Command::new(editor);

    let mut handle = &mut cmd;
    for c in crate::conf::get_included_configs(config_path)? {
        handle = handle.arg(&c);
    }
    let mut handle = handle
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()?;
    handle.wait()?;
    Ok(())
}

#[cfg(feature = "cli-docs")]
pub fn man(page: manpages::ManPages, source: bool) -> Result<String> {
    page.read(source)
}

#[cfg(feature = "cli-docs")]
pub fn pager(v: String, no_raw: Option<Option<bool>>) -> Result<()> {
    if let Some(no_raw) = no_raw {
        match no_raw {
            Some(true) => {}
            None if (unsafe { libc::isatty(libc::STDOUT_FILENO) == 1 }) => {}
            Some(false) | None => {
                println!("{v}");
                return Ok(());
            }
        }
    } else if unsafe { libc::isatty(libc::STDOUT_FILENO) != 1 } {
        println!("{v}");
        return Ok(());
    }

    let mut handle = Command::new("sh")
        .arg("-c")
        .arg(std::env::var("PAGER").unwrap_or_else(|_| "more".to_string()))
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()?;
    handle.stdin.take().unwrap().write_all(v.as_bytes())?;
    handle.wait()?;

    Ok(())
}

#[cfg(not(feature = "cli-docs"))]
pub fn man(_: crate::args::ManOpt) -> Result<()> {
    Err(Error::new("error: this version of meli was not build with embedded documentation (cargo feature `cli-docs`). You might have it installed as manpages (eg `man meli`), otherwise check https://meli-email.org"))
}

pub fn compiled_with() -> Result<()> {
    #[cfg(feature = "notmuch")]
    println!("notmuch");
    #[cfg(feature = "jmap")]
    println!("jmap");
    #[cfg(feature = "sqlite3")]
    println!("sqlite3");
    #[cfg(feature = "smtp")]
    println!("smtp");
    #[cfg(feature = "regexp")]
    println!("regexp");
    #[cfg(feature = "dbus-notifications")]
    println!("dbus-notifications");
    #[cfg(feature = "cli-docs")]
    println!("cli-docs");
    #[cfg(feature = "gpgme")]
    println!("gpgme");
    Ok(())
}

pub fn test_config(path: Option<PathBuf>) -> Result<()> {
    let config_path = if let Some(path) = path {
        path.expand()
    } else {
        crate::conf::get_config_file()?
    };
    conf::FileSettings::validate(config_path, true, false)?;
    Ok(())
}

pub fn view(
    path: PathBuf,
    sender: Sender<ThreadEvent>,
    receiver: Receiver<ThreadEvent>,
) -> Result<State> {
    let path = path.expand();
    if !path.exists() {
        return Err(Error::new(format!(
            "`{}` is not a valid path",
            path.display()
        )));
    } else if !path.is_file() {
        return Err(Error::new(format!("`{}` is a directory", path.display())));
    }
    let bytes = std::fs::read(&path)
        .chain_err_summary(|| format!("Could not read from `{}`", path.display()))?;
    let wrapper = Mail::new(bytes, Some(Flag::SEEN))
        .chain_err_summary(|| format!("Could not parse `{}`", path.display()))?;
    let mut state = State::new(
        Some(Settings::without_accounts().unwrap_or_default()),
        sender,
        receiver,
    )?;
    let main_loop_handler = state.context.main_loop_handler.clone();
    state.register_component(Box::new(EnvelopeView::new(
        wrapper,
        None,
        None,
        None,
        main_loop_handler,
    )));
    Ok(state)
}

pub fn tool(path: Option<PathBuf>, opt: crate::args::ToolOpt) -> Result<()> {
    use melib::utils::futures::timeout;

    use crate::{args::ToolOpt, conf::composing::SendMail};

    let config_path = if let Some(path) = path {
        path.expand()
    } else {
        crate::conf::get_config_file()?
    };
    let conf = conf::FileSettings::validate(config_path, true, false)?;
    let account = match opt {
        ToolOpt::ImapShell { ref account } => account,
        #[cfg(feature = "smtp")]
        ToolOpt::SmtpShell { ref account } => account,
        #[cfg(feature = "jmap")]
        ToolOpt::JmapShell { ref account } => account,
    };
    if !conf.accounts.contains_key(&account.clone()) {
        println!(
            "The configuration file does not contain the account `{}`. It contains the following:",
            account
        );
        for a in conf.accounts.keys() {
            println!("{}", a);
        }
        return Err("Try again with a valid account name.".into());
    }
    let file_account_conf = conf.accounts[account].clone();
    let account_conf: conf::AccountConf = file_account_conf.clone().into();
    match opt {
        #[cfg(feature = "smtp")]
        ToolOpt::SmtpShell { .. } => {
            let send_mail = file_account_conf
                .conf_override
                .composing
                .send_mail
                .as_ref()
                .unwrap_or(&conf.composing.send_mail)
                .clone();
            let SendMail::Smtp(smtp_conf) = send_mail else {
                panic!(
                    "smtp shell requires an smtp configuration for account {}",
                    account
                );
            };
            std::thread::spawn(move || {
                let ex = melib::smol::Executor::new();
                futures::executor::block_on(ex.run(futures::future::pending::<()>()));
            });

            let mut conn = futures::executor::block_on(
                melib::smtp::SmtpConnection::new_connection(smtp_conf),
            )?;
            let mut input = String::new();
            let mut res = String::with_capacity(8 * 1024);
            println!(
                "Connected. Type a valid SMTP command such as EHLO and hit Return. Exit with \
                 Ctrl+C or with the command QUIT."
            );
            loop {
                use std::io;
                input.clear();
                res.clear();

                match io::stdin().read_line(&mut input) {
                    Ok(_) => {
                        futures::executor::block_on(timeout(
                            None,
                            conn.send_command(&[input.trim().as_bytes()]),
                        ))
                        .unwrap()
                        .unwrap();
                        futures::executor::block_on(timeout(None, conn.read_lines(&mut res, None)))
                            .unwrap()
                            .unwrap();
                        println!("\rC: {}", input.trim());
                        print!("S: {}", res);
                        if input.trim().eq_ignore_ascii_case("quit") {
                            break;
                        }
                    }
                    Err(error) => println!("error: {}", error),
                }
            }
        }
        ToolOpt::ImapShell { .. } => {
            use melib::imap::{imap_codec::imap_types::command::CommandBody, RequiredResponses};

            let mut imap = melib::imap::ImapType::new(
                &account_conf.account,
                Box::new(|_| true),
                melib::BackendEventConsumer::new(std::sync::Arc::new(|_, _| ())),
            )?;

            std::thread::spawn(move || {
                let ex = melib::smol::Executor::new();
                futures::executor::block_on(ex.run(futures::future::pending::<()>()));
            });

            let imap = (imap.as_any_mut())
                .downcast_mut::<melib::imap::ImapType>()
                .unwrap();
            let mut conn = melib::imap::ImapConnection::new_connection(
                &imap.server_conf,
                "ImapType::shell".into(),
                imap.uid_store.clone(),
            );

            futures::executor::block_on(timeout(imap.server_conf.timeout, conn.connect()))
                .unwrap()
                .unwrap();
            let mut res = Vec::with_capacity(8 * 1024);
            futures::executor::block_on(timeout(
                imap.server_conf.timeout,
                conn.send_command(CommandBody::Noop),
            ))
            .unwrap()
            .unwrap();
            futures::executor::block_on(timeout(
                imap.server_conf.timeout,
                conn.read_response(&mut res, RequiredResponses::empty()),
            ))
            .unwrap()
            .unwrap();

            let mut input = String::new();
            println!(
                "Connected. Type a valid IMAP command such as LIST \"\" \"*\" and CAPABILITY hit \
                 Return. Exit with Ctrl+C or with the command QUIT."
            );
            loop {
                use std::io;
                input.clear();
                res.clear();

                match io::stdin().read_line(&mut input) {
                    Ok(_) => {
                        futures::executor::block_on(timeout(
                            imap.server_conf.timeout,
                            conn.send_command_raw(input.as_bytes()),
                        ))
                        .unwrap()
                        .unwrap();
                        futures::executor::block_on(timeout(
                            imap.server_conf.timeout,
                            conn.read_lines(&mut res, Vec::new()),
                        ))
                        .unwrap()
                        .unwrap();
                        if input.trim().eq_ignore_ascii_case("logout") {
                            break;
                        }
                        println!("\rC: {}", input.trim());
                        print!("S: {}", String::from_utf8_lossy(&res));
                    }
                    Err(error) => println!("error: {}", error),
                }
            }
        }
        #[cfg(feature = "jmap")]
        ToolOpt::JmapShell { .. } => {
            unimplemented!("Sorry, no JMAP shell yet.");
        }
    }
    Ok(())
}
