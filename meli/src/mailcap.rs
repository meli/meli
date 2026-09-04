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
    components::ComponentId,
    state::Context,
    types::{File, NotificationType, ProcessResultFn, SpawnInteractionFn, UIEvent},
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
    pub fn execute(owner: ComponentId, a: &Attachment, context: &mut Context) -> Result<()> {
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
                    mailcap_path = PathBuf::from(format!("{home}/.mailcap"));
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
                            let file = File::create_temp_file(
                                &a.decode(Default::default()),
                                None,
                                None,
                                None,
                                false,
                            )?;
                            let p = file.path().display().to_string();
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
                let decoded_bytes = if needs_stdin {
                    Some(a.decode(Default::default()))
                } else {
                    None
                };
                let cmd_string = format!("{} {}", cmd, args.join(" "));
                if copiousoutput {
                    context.replies.push_back(UIEvent::ProcessRequest {
                        owner,
                        command: {
                            let mut cmd = Command::new("sh");
                            cmd.args(["-c", &cmd_string])
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .stderr(Stdio::piped());
                            cmd
                        },
                        spawn: Some(SpawnInteractionFn(Box::new(move |mut child| {
                            if let Some(decoded_bytes) = decoded_bytes {
                                child
                                    .stdin
                                    .as_mut()
                                    .expect("handle present")
                                    .write_all(&decoded_bytes)?;
                            }
                            Ok(child)
                        }))),
                        result_cb: ProcessResultFn(Box::new(move |output| {
                            let output = match output {
                                Ok(v) => v,
                                Err(err) => {
                                    return Some(Box::new(UIEvent::Notification {
                                        title: None,
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                    }));
                                }
                            };
                            let pager_cmd = if let Ok(v) = std::env::var("PAGER") {
                                std::borrow::Cow::from(v)
                            } else {
                                std::borrow::Cow::from("less")
                            };

                            Some(Box::new(UIEvent::ProcessRequest {
                                owner,
                                command: {
                                    let mut cmd = Command::new("sh");
                                    cmd.args(["-c", &pager_cmd])
                                        .stdin(Stdio::piped())
                                        .stdout(Stdio::inherit())
                                        .stderr(Stdio::inherit());
                                    cmd
                                },
                                spawn: Some(SpawnInteractionFn(Box::new(move |mut child| {
                                    child
                                        .stdin
                                        .as_mut()
                                        .expect("handle present")
                                        .write_all(&output.stdout)?;
                                    Ok(child)
                                }))),
                                result_cb: ProcessResultFn(Box::new(|_output| {
                                    log::trace!("output = {_output:?}");
                                    None
                                })),
                            }))
                        })),
                    });
                } else if let Some(decoded_bytes) = decoded_bytes {
                    context.replies.push_back(UIEvent::ProcessRequest {
                        owner,
                        command: {
                            let mut cmd = Command::new("sh");
                            cmd.args(["-c", &cmd_string])
                                .stdin(Stdio::piped())
                                .stdout(Stdio::inherit())
                                .stderr(Stdio::inherit());
                            cmd
                        },
                        spawn: Some(SpawnInteractionFn(Box::new(move |mut child| {
                            child
                                .stdin
                                .as_mut()
                                .expect("handle present")
                                .write_all(&decoded_bytes)?;
                            Ok(child)
                        }))),
                        result_cb: ProcessResultFn(Box::new(|_output| {
                            log::trace!("output = {_output:?}");
                            None
                        })),
                    });
                } else {
                    context.replies.push_back(UIEvent::ProcessRequest {
                        owner,
                        command: {
                            let mut cmd = Command::new("sh");
                            cmd.args(["-c", &cmd_string])
                                .stdin(Stdio::inherit())
                                .stdout(Stdio::inherit())
                                .stderr(Stdio::inherit());
                            cmd
                        },
                        spawn: Some(SpawnInteractionFn::default()),
                        result_cb: ProcessResultFn(Box::new(|_output| {
                            log::trace!("output = {_output:?}");
                            None
                        })),
                    });
                }
                Ok(())
            }
        }
    }
}
