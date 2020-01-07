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
use crate::split_command;
use crate::state::Context;
use crate::types::{create_temp_file, ForkType, UIEvent};
use fnv::FnvHashMap;
use melib::attachments::decode;
use melib::{email::Attachment, MeliError, Result};
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use text_processing::GlobMatch;

pub struct MailcapEntry {
    command: String,
    /* Pass to pager */
    copiousoutput: bool,
}

impl MailcapEntry {
    pub fn execute(a: &Attachment, context: &mut Context) -> Result<()> {
        /* lookup order:
         *  $XDG_CONFIG_HOME/meli/mailcap:$XDG_CONFIG_HOME/.mailcap:$HOME/.mailcap:/etc/mailcap:/usr/etc/mailcap:/usr/local/etc/mailcap
         */
        let xdg_dirs =
            xdg::BaseDirectories::with_prefix("meli").map_err(|e| MeliError::new(e.to_string()))?;
        let mut mailcap_path = xdg_dirs
            .place_config_file("mailcap")
            .map_err(|e| MeliError::new(e.to_string()))?;
        if !mailcap_path.exists() {
            mailcap_path = xdg::BaseDirectories::new()
                .map_err(|e| MeliError::new(e.to_string()))?
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
                            return Err(MeliError::new("No mailcap file found."));
                        }
                    }
                }
            }
        }

        let mut hash_map = FnvHashMap::default();
        let mut content = String::new();

        std::fs::File::open(mailcap_path.as_path())?.read_to_string(&mut content)?;
        let content_type = a.content_type().to_string();

        let mut result = None;
        let mut lines_iter = content.lines();
        while let Some(l) = lines_iter.next() {
            let l = l.trim();
            if l.starts_with("#") {
                continue;
            }
            if l.is_empty() {
                continue;
            }

            if l.ends_with("\\") {
                let l = format!("{}{}", &l[..l.len() - 2], lines_iter.next().unwrap());
                let mut parts_iter = l.split(";");
                let key = parts_iter.next().unwrap();
                let cmd = parts_iter.next().unwrap();
                //let flags = parts_iter.next().unwrap();
                if key.starts_with(&content_type) || key.matches_glob(&content_type) {
                    let mut copiousoutput = false;
                    while let Some(flag) = parts_iter.next() {
                        if flag.trim() == "copiousoutput" {
                            copiousoutput = true;
                        } else {
                            debug!("unknown mailcap flag: {}", flag);
                        }
                    }

                    result = Some(MailcapEntry {
                        command: cmd.to_string(),
                        copiousoutput,
                    });
                    break;
                }
                hash_map.insert(key.to_string(), cmd.to_string());
            } else {
                let mut parts_iter = l.split(";");
                let key = parts_iter.next().unwrap();
                let cmd = parts_iter.next().unwrap();
                //let flags = parts_iter.next().unwrap();
                if key.starts_with(&content_type) || key.matches_glob(&content_type) {
                    let mut copiousoutput = false;
                    while let Some(flag) = parts_iter.next() {
                        if flag.trim() == "copiousoutput" {
                            copiousoutput = true;
                        } else {
                            debug!("unknown mailcap flag: {}", flag);
                        }
                    }

                    result = Some(MailcapEntry {
                        command: cmd.to_string(),
                        copiousoutput,
                    });
                    break;
                }
                hash_map.insert(key.to_string(), cmd.to_string());
            }
        }

        match result {
            None => Err(MeliError::new("Not found")),
            Some(MailcapEntry {
                command,
                copiousoutput,
            }) => {
                let parts = split_command!(command);
                let (cmd, args) = (parts[0], &parts[1..]);
                let mut f = None;
                let mut needs_stdin = true;
                let params = a.parameters();
                /* TODO: See mailcap(5)
                 * - replace "\%" with "%" and unescape other blackslash uses.
                 * - "%n" and "%F".
                 * - test=xxx field.
                 */
                let args = args
                    .iter()
                    .map(|arg| match *arg {
                        "%s" => {
                            needs_stdin = false;
                            let _f = create_temp_file(&decode(a, None), None, None, true);
                            let p = _f.path().display().to_string();
                            f = Some(_f);
                            p
                        }
                        "%t" => a.content_type().to_string(),
                        param if param.starts_with("%{") && param.ends_with("}") => {
                            let param = &param["%{".len()..param.len() - 1];
                            if let Some(v) = params.iter().find(|(k, _)| *k == param.as_bytes()) {
                                String::from_utf8_lossy(v.1).into()
                            } else if param == "charset" {
                                String::from("utf-8")
                            } else {
                                String::new()
                            }
                        }
                        a => a.to_string(),
                    })
                    .collect::<Vec<String>>();
                {
                    context.input_kill();
                }
                if copiousoutput {
                    let out = if needs_stdin {
                        let mut child = Command::new(cmd)
                            .args(args)
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()?;

                        child.stdin.as_mut().unwrap().write_all(&decode(a, None))?;
                        child.wait_with_output()?.stdout
                    } else {
                        let child = Command::new(cmd)
                            .args(args)
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

                    let mut pager = Command::new(pager_cmd.as_ref())
                        .stdin(Stdio::piped())
                        .stdout(Stdio::inherit())
                        .spawn()?;
                    pager.stdin.as_mut().unwrap().write_all(&out)?;
                    debug!(pager.wait_with_output()?.stdout);
                } else {
                    if needs_stdin {
                        let mut child = Command::new(cmd)
                            .args(args)
                            .stdin(Stdio::piped())
                            .stdout(Stdio::inherit())
                            .spawn()?;

                        child.stdin.as_mut().unwrap().write_all(&decode(a, None))?;
                        debug!(child.wait_with_output()?.stdout);
                    } else {
                        let child = Command::new(cmd)
                            .args(args)
                            .stdin(Stdio::inherit())
                            .stdout(Stdio::inherit())
                            .spawn()?;

                        debug!(child.wait_with_output()?.stdout);
                    }
                }
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                context.restore_input();
                Ok(())
            }
        }
    }
}
