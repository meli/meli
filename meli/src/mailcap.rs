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

//! # mailcap file - Find mailcap entries to execute attachments.
//!
//! Implements [RFC1524 A User Agent Configuration Mechanism For Multimedia
//! Mail Format Information](https://www.rfc-editor.org/rfc/inline-errata/rfc1524.html)

use std::{
    io::{Read, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

use melib::{email::Attachment, log, utils::fnmatch::Fnmatch, Error, Result};

use crate::{
    state::Context,
    types::{File, ForkType, UIEvent},
};

macro_rules! split_command {
    ($cmd:expr) => {{
        $cmd.split_whitespace().collect::<Vec<&str>>()
    }};
}

pub struct MailcapEntry {
    command: String,
    /* Pass to pager */
    copiousoutput: bool,
}

impl MailcapEntry {
    pub fn execute(a: &Attachment, context: &mut Context) -> Result<()> {
        /* lookup order:
         *  $XDG_CONFIG_HOME/meli/mailcap:$XDG_CONFIG_HOME/.mailcap:$HOME/.mailcap:/
         * etc/mailcap:/usr/etc/mailcap:/usr/local/etc/mailcap
         */
        let xdg_dirs =
            xdg::BaseDirectories::with_prefix("meli").map_err(|e| Error::new(e.to_string()))?;
        let mut mailcap_path = xdg_dirs
            .place_config_file("mailcap")
            .map_err(|e| Error::new(e.to_string()))?;
        if !mailcap_path.exists() {
            mailcap_path = xdg::BaseDirectories::new()
                .map_err(|e| Error::new(e.to_string()))?
                .place_config_file("mailcap")?;
            if !mailcap_path.exists() {
                if let Ok(home) = std::env::var("HOME") {
                    mailcap_path = PathBuf::from(format!("{}/.mailcap", home));
                }
                if !mailcap_path.exists() {
                    mailcap_path = PathBuf::from("/etc/mailcap");
                    if !mailcap_path.exists() {
                        mailcap_path = PathBuf::from("/usr/etc/mailcap");
                        if !mailcap_path.exists() {
                            mailcap_path = PathBuf::from("/usr/local/etc/mailcap");
                        }
                        if !mailcap_path.exists() {
                            return Err(Error::new("No mailcap file found."));
                        }
                    }
                }
            }
        }

        let mut content = String::new();

        std::fs::File::open(mailcap_path.as_path())?.read_to_string(&mut content)?;
        let content_type = a.content_type().to_string();

        let mut result = None;
        let mut lines_iter = content.lines();
        while let Some(l) = lines_iter.next() {
            let l = l.trim();
            if l.starts_with('#') {
                continue;
            }
            if l.is_empty() {
                continue;
            }

            if l.ends_with('\\') {
                let l = format!("{}{}", &l[..l.len() - 2], lines_iter.next().unwrap());
                let mut parts_iter = l.split(';');
                let key = parts_iter.next().unwrap();
                let cmd = parts_iter.next().unwrap();
                //let flags = parts_iter.next().unwrap();
                if key.starts_with(&content_type) || key.fnmatches(&content_type) {
                    let mut copiousoutput = false;
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(flag) = parts_iter.next() {
                        if flag.trim() == "copiousoutput" {
                            copiousoutput = true;
                        } else {
                            log::trace!("unknown mailcap flag: {}", flag);
                        }
                    }

                    result = Some(Self {
                        command: cmd.to_string(),
                        copiousoutput,
                    });
                    break;
                }
            } else {
                let mut parts_iter = l.split(';');
                let key = parts_iter.next().unwrap();
                let cmd = parts_iter.next().unwrap();
                //let flags = parts_iter.next().unwrap();
                if key.starts_with(&content_type) || key.fnmatches(&content_type) {
                    let mut copiousoutput = false;
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(flag) = parts_iter.next() {
                        if flag.trim() == "copiousoutput" {
                            copiousoutput = true;
                        } else {
                            log::trace!("unknown mailcap flag: {}", flag);
                        }
                    }

                    result = Some(Self {
                        command: cmd.to_string(),
                        copiousoutput,
                    });
                    break;
                }
            }
        }

        match result {
            None => Err(Error::new("Not found")),
            Some(Self {
                command,
                copiousoutput,
            }) => {
                let parts = split_command!(command);
                let (cmd, args) = (parts[0], &parts[1..]);
                let mut needs_stdin = true;
                let params = a.parameters();
                /* [ref:TODO]: See mailcap(5)
                 * - replace "\%" with "%" and unescape other blackslash uses.
                 * - "%n" and "%F".
                 * - test=xxx field.
                 */
                let args = args
                    .iter()
                    .map(|arg| match *arg {
                        "%s" => {
                            needs_stdin = false;
                            let _f = File::create_temp_file(
                                &a.decode(Default::default()),
                                None,
                                None,
                                None,
                                true,
                            )?;
                            let p = _f.path().display().to_string();
                            Ok(p)
                        }
                        "%t" => Ok(a.content_type().to_string()),
                        param if param.starts_with("%{") && param.ends_with('}') => {
                            let param = &param["%{".len()..param.len() - 1];
                            Ok(
                                if let Some(v) = params.iter().find(|(k, _)| *k == param.as_bytes())
                                {
                                    String::from_utf8_lossy(v.1).into()
                                } else if param == "charset" {
                                    String::from("utf-8")
                                } else {
                                    String::new()
                                },
                            )
                        }
                        a => Ok(a.to_string()),
                    })
                    .collect::<Result<Vec<String>>>()?;
                let cmd_string = format!("{} {}", cmd, args.join(" "));
                log::trace!("Executing: sh -c \"{}\"", cmd_string.replace('"', "\\\""));
                if copiousoutput {
                    let out = if needs_stdin {
                        let mut child = Command::new("sh")
                            .args(["-c", &cmd_string])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()?;

                        child
                            .stdin
                            .as_mut()
                            .unwrap()
                            .write_all(&a.decode(Default::default()))?;
                        child.wait_with_output()?.stdout
                    } else {
                        let child = Command::new("sh")
                            .args(["-c", &cmd_string])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()?;

                        child.wait_with_output()?.stdout
                    };
                    let pager_cmd = if let Ok(v) = std::env::var("PAGER") {
                        std::borrow::Cow::from(v)
                    } else {
                        std::borrow::Cow::from("less")
                    };

                    let mut pager = Command::new("sh")
                        .args(["-c", pager_cmd.as_ref()])
                        .stdin(Stdio::piped())
                        .stdout(Stdio::inherit())
                        .spawn()?;
                    pager.stdin.as_mut().unwrap().write_all(&out)?;
                    let _output = pager.wait_with_output()?;
                    log::trace!("stdout = {}", String::from_utf8_lossy(&_output.stdout));
                } else if needs_stdin {
                    let mut child = Command::new("sh")
                        .args(["-c", &cmd_string])
                        .stdin(Stdio::piped())
                        .stdout(Stdio::inherit())
                        .spawn()?;

                    child
                        .stdin
                        .as_mut()
                        .unwrap()
                        .write_all(&a.decode(Default::default()))?;
                    let _output = child.wait_with_output()?;
                    log::trace!("stdout = {}", String::from_utf8_lossy(&_output.stdout));
                } else {
                    let child = Command::new("sh")
                        .args(["-c", &cmd_string])
                        .stdin(Stdio::inherit())
                        .stdout(Stdio::inherit())
                        .spawn()?;

                    let _output = child.wait_with_output()?;
                    log::trace!("stdout = {}", String::from_utf8_lossy(&_output.stdout));
                }
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                Ok(())
            }
        }
    }
}
