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
    borrow::Cow,
    io::{Read, Write},
    path::PathBuf,
    process::{Command, Stdio},
    sync::Arc,
};

use melib::{
    email::{attachment_types::ContentType, Attachment},
    log,
    utils::fnmatch::Fnmatch,
    uuid::Uuid,
    Error, ErrorKind, Result,
};

use crate::{
    components::ComponentId,
    types::{File, NotificationType, ProcessRequest, ProcessResultFn, SpawnInteractionFn, UIEvent},
};

#[derive(Default, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct MailcapEntry<'a> {
    /// Content type key.
    pub key: Cow<'a, str>,
    pub view_command: Cow<'a, str>,
    /// A program that can be used to compose a new body or body part in the
    /// given format.
    pub compose: Option<Cow<'a, str>>,
    pub composetyped: Option<Cow<'a, str>>,
    pub print: Option<Cow<'a, str>>,
    pub edit: Option<Cow<'a, str>>,
    pub test: Vec<Cow<'a, str>>,
    /// > Indicates that the output from the view-command will be an extended
    /// > stream of output, and
    /// > is to be interpreted as advice to the UA (User Agent mail-reading
    /// > program) that the output
    /// > should be either paged or made scrollable.
    pub copiousoutput: bool,
    /// > Indicates that the view-command must be run on an interactive >
    /// > terminal.
    pub needsterminal: bool,
    /// > Textual description, optionally quoted, that describes the type of
    /// > data, to be used
    /// > optionally by mail readers that wish to describe the data before
    /// > offering to display it
    pub description: Cow<'a, str>,
    /// Indicates that this type of data is line-oriented and that, if encoded
    /// in base64, all newlines should be converted to canonical form (CRLF)
    /// before encoding, and will be in that form after decoding.
    pub textualnewlines: bool,
    /// A file name format, in which `%s` will be replaced by a short unique
    /// string to give the name of the temporary file to be passed to the
    /// viewing command.
    pub nametemplate: Option<Cow<'a, str>>,
}

fn expand_nametemplate(nametemplate: Option<&str>, a: &Attachment) -> Option<String> {
    let mut nametemplate = if let Some(t) = nametemplate {
        t.to_string()
    } else {
        return a.filename().map(|f| f.to_string());
    };
    let mut cursor = 0;
    let name = if let Some(filename) = a.filename() {
        filename.to_string()
    } else {
        Uuid::new_v4().as_simple().to_string()
    };
    while cursor < nametemplate.len() {
        let Some(percent) = nametemplate[cursor..].find("%s") else {
            break;
        };
        if nametemplate[cursor..][..=percent].ends_with("\\%") {
            cursor += percent + 1;
            continue;
        }
        nametemplate.replace_range((cursor + percent)..=((cursor + percent) + 2), &name);
        cursor += percent + name.len();
    }
    Some(nametemplate)
}

fn expand_args(
    command: &str,
    nametemplate: Option<&str>,
    temporary_files: &mut Vec<Arc<File>>,
    a: &Attachment,
) -> Result<(bool, String)> {
    let params = a.parameters();
    let mut command = command.to_string();
    let mut cursor = 0;
    let mut needs_stdin = true;

    let mut file = None;

    while cursor < command.len() {
        let Some(percent) = command[cursor..].find('%') else {
            break;
        };
        if command[cursor..][..=percent].ends_with("\\%") {
            cursor += percent + 1;
            continue;
        }
        let arg = &command[cursor..][percent..];
        match arg {
            arg if arg.starts_with("%s") => {
                if file.is_none() {
                    file = Some(File::create_temp_file(
                        &a.decode(Default::default()),
                        expand_nametemplate(nametemplate, a).as_deref(),
                        None,
                        None,
                        false,
                    )?);
                }
                let file = file.as_ref().unwrap();
                let mut p = file.path().display().to_string();
                if p.contains('"') {
                    p = p.replace('"', "\\\"");
                }
                if p.contains(' ') {
                    p = format!("\"{p}\"");
                }
                command.replace_range((cursor + percent)..=((cursor + percent) + 1), &p);
                needs_stdin = false;
                cursor += percent + p.len();
            }
            arg if arg.starts_with("%t") => {
                let t = a.content_type().to_string();
                command.replace_range((cursor + percent)..=((cursor + percent) + 1), &t);
                cursor += percent + t.len();
            }
            arg if arg.starts_with("%n") => {
                // RFC1524 Page 9: If the content-type is "multipart" (any subtype), then the two
                // characters "%n" will be replaced by an integer giving the number of sub-parts
                // within the multipart entity.
                let n = if let ContentType::Multipart { ref parts, .. } = a.content_type() {
                    Cow::Owned(parts.len().to_string())
                } else {
                    Cow::Borrowed("0")
                };
                command.replace_range((cursor + percent)..=((cursor + percent) + 1), &n);
                cursor += percent + n.len();
            }
            arg if arg.starts_with("%F") => {
                // RFC1524 Page 9: Also, the two characters "%F" will be replaced by a set of
                // arguments, twice as many arguments as the number of sub-parts, consisting of
                // alternating content-types and file names for each part in turn.  Thus if
                // multipart entity has three parts, "%F" will be replaced by the equivalent of
                // "content-type1 file-name1 content-type2 file-name2 content-type3 file-name3".
                let parts = if let ContentType::Multipart { ref parts, .. } = a.content_type() {
                    parts.as_slice()
                } else {
                    &[]
                };
                if !parts.is_empty() {
                    needs_stdin = false;
                }
                let mut f = String::new();
                for a in parts {
                    let file = File::create_temp_file(
                        &a.decode(Default::default()),
                        None,
                        None,
                        None,
                        false,
                    )?;
                    let mut p = file.path().display().to_string();
                    if p.contains('"') {
                        p = p.replace('"', "\\\"");
                    }
                    if p.contains(' ') {
                        p = format!("\"{p}\"");
                    }
                    if !f.is_empty() {
                        f.push(' ');
                    }
                    f = format!("{f}{} {p}", a.content_type());
                    temporary_files.push(file.into());
                }
                command.replace_range((cursor + percent)..=((cursor + percent) + 1), &f);
                cursor += percent + f.len();
            }
            param if param.starts_with("%{") && param.contains('}') => {
                let param = &param[..param.find('}').unwrap()];
                let name = param.strip_prefix("%{").unwrap().strip_suffix('}').unwrap();
                let value = if let Some(v) = params.iter().find(|(k, _)| *k == name.as_bytes()) {
                    String::from_utf8_lossy(v.1).into()
                } else if name == "charset" {
                    String::from("utf-8")
                } else {
                    String::new()
                };
                command.replace_range(
                    (cursor + percent)..=((cursor + percent) + param.len()),
                    &value,
                );
                cursor += percent + value.len();
            }
            a => panic!("{a}"),
        }
    }
    if let Some(file) = file {
        temporary_files.push(file.into());
    }
    Ok((needs_stdin, command))
}

impl MailcapEntry<'static> {
    pub fn execute(owner: ComponentId, a: &Attachment) -> Result<ProcessRequest> {
        // lookup order:
        // $XDG_CONFIG_HOME/meli/mailcap:$XDG_CONFIG_HOME/.mailcap:$HOME/.mailcap:
        // /etc/mailcap:/usr/etc/mailcap:/usr/local/etc/mailcap
        let mut file_candidates = vec![];
        let find_xdg_dir_meli = || {
            xdg::BaseDirectories::with_prefix("meli")
                .ok()?
                .place_config_file("mailcap")
                .ok()
        };
        let find_xdg_dir = || {
            xdg::BaseDirectories::new()
                .ok()?
                .place_config_file("mailcap")
                .ok()
        };
        let find_home_mailcap = || {
            let home = std::env::var("HOME").ok()?;
            Some(PathBuf::from(format!("{}/.mailcap", home)))
        };
        let find_etc_mailcap = || Some(PathBuf::from("/etc/mailcap"));
        let find_usr_etc_mailcap = || Some(PathBuf::from("/usr/etc/mailcap"));
        let find_usr_local_etc_mailcap = || Some(PathBuf::from("/usr/local/etc/mailcap"));

        for find_fn in [
            find_xdg_dir_meli,
            find_xdg_dir,
            find_home_mailcap,
            find_etc_mailcap,
            find_usr_etc_mailcap,
            find_usr_local_etc_mailcap,
        ] {
            if let Some(mailcap_path) = find_fn() {
                if mailcap_path.exists() {
                    file_candidates.push(mailcap_path);
                }
            }
        }
        if file_candidates.is_empty() {
            return Err(Error::new("No mailcap file found.").set_kind(ErrorKind::NotFound));
        }

        let content_type = a.content_type().to_string();

        let mut candidates = vec![];
        let mut content = String::new();
        'mailcap_candidates: for mailcap_path in file_candidates {
            if let Err(err) = std::fs::File::open(mailcap_path.as_path())
                .and_then(|mut fs| fs.read_to_string(&mut content))
            {
                log::warn!("Could not read {}: {err}", mailcap_path.display());
                continue;
            }
            for entry in MailcapEntry::parser(&content) {
                let entry = match entry {
                    Ok(entry) => entry,
                    Err(err) => {
                        log::warn!("Could not parse {}: {err}", mailcap_path.display());
                        continue 'mailcap_candidates;
                    }
                };
                if entry.key.starts_with(&content_type) || content_type.fnmatches(&entry.key) {
                    candidates.push(entry.into_static());
                }
            }
        }

        Self::run_candidates(owner, a.clone(), candidates)
    }

    fn run_candidates(
        owner: ComponentId,
        a: Attachment,
        mut candidates: Vec<Self>,
    ) -> Result<ProcessRequest> {
        if !candidates.is_empty() {
            let mut candidate = candidates.remove(0);
            if candidate.test.is_empty() {
                return candidate.run(owner, a);
            } else {
                let mut temporary_files = vec![];
                let test = candidate.test.remove(0);
                let (_, test_string) = expand_args(&test, None, &mut temporary_files, &a)?;
                return Ok(ProcessRequest {
                    owner,
                    command: {
                        let mut cmd = Command::new("sh");
                        cmd.args(["-c", &test_string])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .stderr(Stdio::piped());
                        cmd
                    },
                    spawn: None,
                    result_cb: ProcessResultFn(Box::new({
                        move |output| {
                            if output.is_err() {
                                // If there are remaining tests for this entry candidate,
                                // reconsider it.
                                if !candidate.test.is_empty() {
                                    candidates.insert(0, candidate);
                                }
                                return match MailcapEntry::run_candidates(owner, a, candidates) {
                                    Err(err) => Some(Box::new(UIEvent::Notification {
                                        title: None,
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                    })),
                                    Ok(p) => Some(Box::new(UIEvent::ProcessRequest(Box::new(p)))),
                                };
                            }
                            match candidate.run(owner, a) {
                                Err(err) => Some(Box::new(UIEvent::Notification {
                                    title: None,
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(NotificationType::Error(err.kind)),
                                })),
                                Ok(p) => Some(Box::new(UIEvent::ProcessRequest(Box::new(p)))),
                            }
                        }
                    })),
                    temporary_files,
                });
            }
        }

        Err(Error::new(format!(
            "No mailcap entry found for {content_type}.",
            content_type = a.content_type()
        ))
        .set_kind(ErrorKind::NotFound))
    }
}

impl MailcapEntry<'_> {
    pub fn into_static(self) -> MailcapEntry<'static> {
        let Self {
            key,
            view_command,
            compose,
            composetyped,
            print,
            edit,
            test,
            copiousoutput,
            needsterminal,
            description,
            textualnewlines,
            nametemplate,
        } = self;

        MailcapEntry {
            key: Cow::Owned(key.into_owned()),
            view_command: Cow::Owned(view_command.into_owned()),
            compose: compose.map(Cow::into_owned).map(Cow::Owned),
            composetyped: composetyped.map(Cow::into_owned).map(Cow::Owned),
            print: print.map(Cow::into_owned).map(Cow::Owned),
            edit: edit.map(Cow::into_owned).map(Cow::Owned),
            test: test
                .into_iter()
                .map(|e| Cow::Owned(e.into_owned()))
                .collect(),
            copiousoutput,
            needsterminal,
            description: Cow::Owned(description.into_owned()),
            textualnewlines,
            nametemplate: nametemplate.map(Cow::into_owned).map(Cow::Owned),
        }
    }

    pub fn run(&self, owner: ComponentId, a: Attachment) -> Result<ProcessRequest> {
        let mut temporary_files = vec![];
        let (needs_stdin, cmd_string) = expand_args(
            &self.view_command,
            self.nametemplate.as_deref(),
            &mut temporary_files,
            &a,
        )?;
        let decoded_bytes = if needs_stdin {
            Some(a.decode(Default::default()))
        } else {
            None
        };
        let ret = if self.copiousoutput {
            ProcessRequest {
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
                result_cb: ProcessResultFn(Box::new({
                    let temporary_files = temporary_files.clone();
                    move |output| {
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

                        Some(Box::new(UIEvent::ProcessRequest(Box::new(
                            ProcessRequest {
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
                                result_cb: ProcessResultFn(Box::new(|_output| None)),
                                temporary_files,
                            },
                        ))))
                    }
                })),
                temporary_files,
            }
        } else if let Some(decoded_bytes) = decoded_bytes {
            ProcessRequest {
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
                result_cb: ProcessResultFn(Box::new(|_output| None)),
                temporary_files,
            }
        } else {
            ProcessRequest {
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
                result_cb: ProcessResultFn(Box::new(|_output| None)),
                temporary_files,
            }
        };
        Ok(ret)
    }

    pub fn parser<'a>(buffer: &'a str) -> MailcapParser<'a> {
        MailcapParser { buffer, cursor: 0 }
    }
}

pub struct MailcapParser<'a> {
    buffer: &'a str,
    cursor: usize,
}

impl<'a> Iterator for MailcapParser<'a> {
    type Item = Result<MailcapEntry<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cursor < self.buffer.len() {
            let mut line_end = 0;
            while let Some(next_ln) = self.buffer[self.cursor..][line_end..].find('\n') {
                if self.buffer[self.cursor..][line_end..][..next_ln + 1].ends_with("\\\n") {
                    line_end += next_ln + 1;
                    if self.buffer[self.cursor..][line_end..].find('\n').is_none() {
                        line_end = self.buffer[self.cursor..].len();
                        break;
                    }
                    continue;
                }
                line_end += next_ln + 1;
                break;
            }
            if line_end == 0 {
                line_end = self.buffer[self.cursor..].len();
            }

            let l = self.buffer[self.cursor..][..line_end].trim();
            self.cursor += line_end;
            if l.starts_with('#') || l.is_empty() {
                continue;
            }

            let mut parts_iter = l
                .match_indices(';')
                .map(|(i, _)| i)
                .filter(|i| !l[..=*i].ends_with("\\;"))
                .zip(
                    l.match_indices(';')
                        .map(|(i, _)| i)
                        .filter(|i| !l[..=*i].ends_with("\\;"))
                        .skip(1)
                        .chain(std::iter::once(l.len())),
                )
                .map(|(start, end)| {
                    let tok = &l[start + 1..end];
                    let tok = tok.trim_matches(|c| c == '\t' || c == ' ').trim();
                    if tok.contains("\\\n") || tok.contains("\\;") {
                        Cow::Owned(tok.replace("\\\n", "").replace("\\;", ";"))
                    } else {
                        Cow::Borrowed(tok)
                    }
                })
                .filter(|tok| !tok.is_empty());
            let Some((key, _)) = l.split_once(';') else {
                continue;
            };
            let Some(view_command) = parts_iter.next() else {
                continue;
            };
            let mut ret = MailcapEntry {
                key: Cow::Borrowed(key.trim()),
                view_command,
                ..Default::default()
            };
            for token in parts_iter {
                macro_rules! parse_flag {
                    ($name:literal) => {
                        match token {
                            Cow::Owned(mut tok) => {
                                tok.replace_range(0..$name.len(), "");
                                Cow::Owned(tok)
                            }
                            Cow::Borrowed(tok) => Cow::Borrowed(tok.trim_start_matches($name)),
                        }
                    };
                }
                if token == "copiousoutput" {
                    ret.copiousoutput = true;
                } else if token == "needsterminal" {
                    ret.needsterminal = true;
                } else if token == "textualnewlines" {
                    ret.textualnewlines = true;
                } else if token.starts_with("compose=") {
                    ret.compose = Some(parse_flag!("compose="));
                } else if token.starts_with("composetyped=") {
                    ret.composetyped = Some(parse_flag!("composetyped="));
                } else if token.starts_with("print=") {
                    ret.print = Some(parse_flag!("print="));
                } else if token.starts_with("edit=") {
                    ret.edit = Some(parse_flag!("edit="));
                } else if token.starts_with("test=") {
                    ret.test.push(parse_flag!("test="));
                } else if token.starts_with("description=") {
                    ret.description = parse_flag!("description=");
                } else if token.starts_with("nametemplate=") {
                    ret.nametemplate = Some(parse_flag!("nametemplate="));
                } else {
                    log::warn!("unknown flag/setting {token:?}",);
                }
            }
            return Some(Ok(ret));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use melib::{
        email::{
            attachment_types::{Charset, ContentTransferEncoding, ContentType, Text},
            AttachmentBuilder,
        },
        utils::logging::{LogLevel, Logger},
    };
    use rusty_fork::rusty_fork_test;

    use super::*;

    #[test]
    fn test_mailcap_parser() {
        let s = r#"image/*; imv %s; test=test -n "$DISPLAY";
application/pdf; zathura %s; test=test -n "$DISPLAY";
application/pdf; pdftotext -layout %s -; copiousoutput
application/pdf; pdftotext -layout \
%s -; copiousoutput
application/pdf; pdftotext -layout \
%s \
-; \
copiousoutput
application/msword; lowriter %s; test=test -n "$DISPLAY";
application/vnd.openxmlformats-officedocument.wordprocessingml.document; lowriter %s; test=test -n "$DISPLAY";
application/vnd.msword; lowriter %s; test=test -n "$DISPLAY";
text/html; "$BROWSER" %s && exit 1; test=test -n "$DISPLAY"; needsterminal;
text/html; links -html-numbered-links 1 -dump %s; nametemplate=%s.html; copiousoutput;"#;

        let mut parser = MailcapEntry::parser(s)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "imv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/pdf".into(),
                view_command: "zathura %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/pdf".into(),
                view_command: "pdftotext -layout %s -".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/pdf".into(),
                view_command: "pdftotext -layout %s -".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/pdf".into(),
                view_command: "pdftotext -layout %s -".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/msword".into(),
                view_command: "lowriter %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                    .into(),
                view_command: "lowriter %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.msword".into(),
                view_command: "lowriter %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/html".into(),
                view_command: "\"$BROWSER\" %s && exit 1".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/html".into(),
                view_command: "links -html-numbered-links 1 -dump %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: Some("%s.html".into())
            }
        );
        assert!(parser.is_empty(), "{parser:?}");

        let s = r#"
audio/*; mpv %s
image/gif; sxiv %s
image/jpeg; sxiv %s
image/jpg; sxiv %s
image/png; sxiv %s
text/html; lynx -assume_charset=%{charset} -display_charset=utf-8 -collapse_br_tags -dump %s; nametemplate=%s.html; copiousoutput
video/*; mpv %s
video/mpeg; mpv %s
"#;

        let mut parser = MailcapEntry::parser(s)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "audio/*".into(),
                view_command: "mpv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/gif".into(),
                view_command: "sxiv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/jpeg".into(),
                view_command: "sxiv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/jpg".into(),
                view_command: "sxiv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/png".into(),
                view_command: "sxiv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/html".into(),
                view_command: "lynx -assume_charset=%{charset} -display_charset=utf-8 \
                               -collapse_br_tags -dump %s"
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: Some("%s.html".into())
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "video/*".into(),
                view_command: "mpv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "video/mpeg".into(),
                view_command: "mpv %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert!(parser.is_empty(), "{parser:?}");

        let s = r#"###############
# Interactive #
###############

# application/pdf; llpp %s; test=test -n "$DISPLAY"; description=Portable Document Format; nametemplate=%s.pdf
application/pdf; xdg-open %s; needsterminal
# application/postscript; zathura %s; test=test -n "$DISPLAY"
application/postscript; xdg-open %s; needsterminal
application/vnd.ms-excel; gnumeric %s; test=test -n "$DISPLAY"
application/vnd.oasis.opendocument.text; wps %s; test=test -n "$DISPLAY"
application/vnd.openxmlformats-officedocument.wordprocessingml.document; wps %s; test=test -n "$DISPLAY"

application/x-gzip; zless; needsterminal
application/x-bzip2; bzless; needsterminal
application/x-xz; xzless; needsterminal

#image/*; feh %s; test=test -n "$DISPLAY"
#image/*; fbi %s
#
#audio/*; vlc %s; test=test -n "$DISPLAY"
#video/*; vlc %s; test=test -n "$DISPLAY"

image/* ; /usr/bin/xdg-open %s ; needsterminal
audio/* ; /usr/bin/xdg-open %s ; needsterminal
video/* ; /usr/bin/xdg-open %s ; needsterminal

##########
# Inline #
##########

application/x-shellscript; cat %s; copiousoutput
application/x-tex; cat %s; copiousoutput
text/html; t=%{charset} \; w3m -dump -I ${t/2312/18030} -T text/html %s | uniq; copiousoutput
text/plain; cat %s; test=test "`echo %{charset} | tr '[A-Z]' '[a-z]'`" = utf-8 ; copiousoutput

application/msword; catdoc %s; copiousoutput
application/pdf; pdftotext -enc UTF-8 %s /dev/stdout; copiousoutput
application/postscript; ps2ascii %s; copiousoutput

application/x-gzip; zcat; copiousoutput
application/x-cpio; cpio -tvF --quiet %s; copiousoutput
application/x-tar; tar tvf %s; copiousoutput
application/x-gtar; tar tvfz %s; copiousoutput
application/rar; rar l %s | sed -n '/Name/,$p'; copiousoutput
application/x-7z-compressed; 7z l %s | sed -n '/Date/,$p'; copiousoutput
application/zip; 7z l %s | sed -n '/Date/,$p'; copiousoutput
application/x-xz; tar tvf %s; copiousoutput

#image/*; anytopnm %s | pnmscale -xsize 80 | convert - pbm:- | pbmtoascii; copiousoutput
image/*; exiftool -common '%s'; copiousoutput
"#;

        let mut parser = MailcapEntry::parser(s)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();

        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/pdf".into(),
                view_command: "xdg-open %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/postscript".into(),
                view_command: "xdg-open %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.ms-excel".into(),
                view_command: "gnumeric %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.oasis.opendocument.text".into(),
                view_command: "wps %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                    .into(),
                view_command: "wps %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test -n \"$DISPLAY\"".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-gzip".into(),
                view_command: "zless".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-bzip2".into(),
                view_command: "bzless".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-xz".into(),
                view_command: "xzless".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "/usr/bin/xdg-open %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "audio/*".into(),
                view_command: "/usr/bin/xdg-open %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "video/*".into(),
                view_command: "/usr/bin/xdg-open %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-shellscript".into(),
                view_command: "cat %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-tex".into(),
                view_command: "cat %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/html".into(),
                view_command: "t=%{charset} ; w3m -dump -I ${t/2312/18030} -T text/html %s | uniq"
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/plain".into(),
                view_command: "cat %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["test \"`echo %{charset} | tr '[A-Z]' '[a-z]'`\" = utf-8".into()],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/msword".into(),
                view_command: "catdoc %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/pdf".into(),
                view_command: "pdftotext -enc UTF-8 %s /dev/stdout".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/postscript".into(),
                view_command: "ps2ascii %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-gzip".into(),
                view_command: "zcat".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-cpio".into(),
                view_command: "cpio -tvF --quiet %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-tar".into(),
                view_command: "tar tvf %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-gtar".into(),
                view_command: "tar tvfz %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/rar".into(),
                view_command: "rar l %s | sed -n '/Name/,$p'".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-7z-compressed".into(),
                view_command: "7z l %s | sed -n '/Date/,$p'".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/zip".into(),
                view_command: "7z l %s | sed -n '/Date/,$p'".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-xz".into(),
                view_command: "tar tvf %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "exiftool -common '%s'".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert!(parser.is_empty(), "{parser:?}");

        let s = r#"## .mailcap -- MIME Viewer configuration file

# Copyright (C) 2004-2021 Fabrice Niessen. All rights reserved.

# Author: Fabrice Niessen <fni@missioncriticalit.com>
# Keywords: mailcap, dotfile

#* Commentary:

# There should also be a system-wide setting in `/etc/mailcap'...
#
# `%s' means "put the datafile name here when the viewer is executed"
#
# `test=test -n "$DISPLAY"' is used to determine if the current session is
# X-capable (by checking for the existence of a DISPLAY environment
# variable) -- DOES NOT WORK IN CYGWIN!?
#
# `copiousoutput' indicates that the output of the command may be
# *voluminous*; hence, requiring a pager (such as `more' or `less') or a
# scrolling window.

#* Code:

# application/pdf;                cygstart "%s"
# text/html;                      cygstart "%s"
# application/vnd.ms-excel;       cygstart "%s"
# application/vnd.ms-powerpoint;  cygstart "%s"
# video/*;                        cygstart "%s"

application/*;                  xdg-open "%s"; test=sh -c 'test $DISPLAY'
application/ms-tnef;            tnef -w "%s"
application/msword;             "C:/Program Files/Microsoft Office/OFFICE11/WINWORD.EXE" /n /dde "%s"
# application/msword;           strings "%s" | more; needsterminal
application/rtf;                "C:/Program Files/Microsoft Office/OFFICE11/WINWORD.EXE" /n /dde "%s"
application/vnd.ms-excel;       "C:/Program Files/Microsoft Office/OFFICE11/EXCEL.EXE" /e "%s"
application/vnd.ms-powerpoint;  "C:/Program Files/Microsoft Office/OFFICE11/POWERPNT.EXE" "%s"
application/postscript;         gsview32 "%s"
application/postscript;         ps2ascii "%s"; copiousoutput
application/zip;                "C:/Program Files/7-Zip/7zFM.exe" "%s"
application/octet-stream;       "C:/Program Files/Mozilla Firefox/firefox.exe" "%s"
audio/*;                        xdg-open "%s"; test=sh -c 'test $DISPLAY'
audio/*;                        "C:/Program Files/Windows Media Player/wmplayer.exe" "%s"
image/*;                        xdg-open "%s"; test=sh -c 'test $DISPLAY'
image/*;                        "C:/Program Files/Mozilla Firefox/firefox.exe" "%s"
text/*;                         cat; copiousoutput; edit=$EDITOR "%s"

## .mailcap ends here
"#;
        let mut parser = MailcapEntry::parser(s)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();

        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/*".into(),
                view_command: "xdg-open \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["sh -c 'test $DISPLAY'".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/ms-tnef".into(),
                view_command: "tnef -w \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/msword".into(),
                view_command: "\"C:/Program Files/Microsoft Office/OFFICE11/WINWORD.EXE\" /n /dde \
                               \"%s\""
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/rtf".into(),
                view_command: "\"C:/Program Files/Microsoft Office/OFFICE11/WINWORD.EXE\" /n /dde \
                               \"%s\""
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.ms-excel".into(),
                view_command: "\"C:/Program Files/Microsoft Office/OFFICE11/EXCEL.EXE\" /e \"%s\""
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/vnd.ms-powerpoint".into(),
                view_command: "\"C:/Program Files/Microsoft Office/OFFICE11/POWERPNT.EXE\" \"%s\""
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/postscript".into(),
                view_command: "gsview32 \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/postscript".into(),
                view_command: "ps2ascii \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/zip".into(),
                view_command: "\"C:/Program Files/7-Zip/7zFM.exe\" \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/octet-stream".into(),
                view_command: "\"C:/Program Files/Mozilla Firefox/firefox.exe\" \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "audio/*".into(),
                view_command: "xdg-open \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["sh -c 'test $DISPLAY'".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "audio/*".into(),
                view_command: "\"C:/Program Files/Windows Media Player/wmplayer.exe\" \"%s\""
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "xdg-open \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["sh -c 'test $DISPLAY'".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "\"C:/Program Files/Mozilla Firefox/firefox.exe\" \"%s\"".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/*".into(),
                view_command: "cat".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: Some("$EDITOR \"%s\"".into()),
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert!(parser.is_empty(), "{parser:?}");

        let jcs = r#"# things mutt can view inline
application/msword; antiword %s; copiousoutput
text/html; ~/bin/html_mail %s; copiousoutput
application/x-gunzip; gzcat %s; copiousoutput

# things that need external programs
#application/pdf; /usr/local/bin/mupdf %s
image/*; ~/bin/mutt_bgrun %t %s

application/*; ~/bin/mutt_bgrun %t %s
"#;
        let mut parser = MailcapEntry::parser(jcs)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();

        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/msword".into(),
                view_command: "antiword %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/html".into(),
                view_command: "~/bin/html_mail %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/x-gunzip".into(),
                view_command: "gzcat %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "~/bin/mutt_bgrun %t %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/*".into(),
                view_command: "~/bin/mutt_bgrun %t %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert!(parser.is_empty(), "{parser:?}");

        let s = r#"application/postscript; ps-to-terminal %s; needsterminal
application/postscript; ps-to-terminal %s; compose=idraw %s
"#;
        let mut parser = MailcapEntry::parser(s)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();

        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/postscript".into(),
                view_command: "ps-to-terminal %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/postscript".into(),
                view_command: "ps-to-terminal %s".into(),
                compose: Some("idraw %s".into()),
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert!(parser.is_empty(), "{parser:?}");

        let rfc = r#"# Mailcap file for Bellcore lab 214.
#
# The next line sends "richtext" to the richtext program
text/richtext; richtext %s; copiousoutput
#
# Next, basic u-law audio
audio/*; showaudio; test=/usr/local/bin/hasaudio
#
# Next, use the xview program to handle several image formats
image/*; xview %s; test=/usr/local/bin/RunningX
#
# The ATOMICMAIL interpreter uses curses, so needs a terminal
application/atomicmail; /usr/local/bin/atomicmail %s; \
needsterminal
#
# The next line handles Andrew format,
#   if ez and ezview are installed
x-be2; /usr/andrew/bin/ezview %s; \
print=/usr/andrew/bin/ezprint %s ; \
compose=/usr/andrew/bin/ez -d %s ;\
edit=/usr/andrew/bin/ez -d %s; ;\
copiousoutput
#
# The next silly example demonstrates the use of quoting
application/*; echo "This is \"%t\" but \
is 50 \% Greek to me" \; cat %s; copiousoutput"#;

        let mut parser = MailcapEntry::parser(rfc)
            .collect::<Result<VecDeque<_>>>()
            .unwrap();

        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "text/richtext".into(),
                view_command: "richtext %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "audio/*".into(),
                view_command: "showaudio".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["/usr/local/bin/hasaudio".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "image/*".into(),
                view_command: "xview %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec!["/usr/local/bin/RunningX".into()],
                copiousoutput: false,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/atomicmail".into(),
                view_command: "/usr/local/bin/atomicmail %s".into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: false,
                needsterminal: true,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "x-be2".into(),
                view_command: "/usr/andrew/bin/ezview %s".into(),
                compose: Some("/usr/andrew/bin/ez -d %s".into()),
                composetyped: None,
                print: Some("/usr/andrew/bin/ezprint %s".into()),
                edit: Some("/usr/andrew/bin/ez -d %s".into()),
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert_eq!(
            parser.pop_front().unwrap(),
            MailcapEntry {
                key: "application/*".into(),
                view_command: "echo \"This is \\\"%t\\\" but is 50 \\% Greek to me\" ; cat %s"
                    .into(),
                compose: None,
                composetyped: None,
                print: None,
                edit: None,
                test: vec![],
                copiousoutput: true,
                needsterminal: false,
                description: "".into(),
                textualnewlines: false,
                nametemplate: None
            }
        );
        assert!(parser.is_empty(), "{parser:?}");
    }

    rusty_fork_test! {
        #[test]
        fn test_mailcap_execution() {
            run_mailcap_execution();
        }
    }

    fn run_mailcap_execution() {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let temp_dir = tempfile::tempdir().unwrap();
        for var in [
            "HOME",
            "XDG_CACHE_HOME",
            "XDG_STATE_HOME",
            "XDG_CONFIG_DIRS",
            "XDG_CONFIG_HOME",
            "XDG_DATA_DIRS",
            "XDG_DATA_HOME",
        ] {
            std::env::remove_var(var);
        }
        for (var, dir) in [
            ("HOME", temp_dir.path().to_path_buf()),
            ("XDG_CACHE_HOME", temp_dir.path().join(".cache")),
            ("XDG_STATE_HOME", temp_dir.path().join(".local/state")),
            ("XDG_CONFIG_HOME", temp_dir.path().join(".config")),
            ("XDG_DATA_HOME", temp_dir.path().join(".local/share")),
        ] {
            std::fs::create_dir_all(&dir).unwrap_or_else(|err| {
                panic!("Could not create {} path, {}: {}", var, dir.display(), err);
            });
            std::env::set_var(var, &dir);
        }

        std::fs::write(
            temp_dir.path().join(".config").join("mailcap"),
            r#"
application/*; /usr/bin/open %s
image/*; /usr/bin/open %s
text/plain; less; needsterminal
video/*; /usr/bin/open %s
text/html; "$BROWSER" %s && exit 1; test=test -n "$BROWSER_TEST"; needsterminal;
text/html; less; needsterminal;
"#,
        )
        .unwrap();
        let owner = ComponentId::default();
        {
            // Not found
            let mut attachment = AttachmentBuilder::new(b"");
            attachment
                .set_raw("")
                .set_content_type(ContentType::Other {
                    name: Some("01 - Untitled.ogg".to_string()),
                    tag: b"audio/ogg".to_vec(),
                    parameters: vec![],
                })
                .set_content_transfer_encoding(ContentTransferEncoding::Base64);
            let attachment = attachment.build();
            assert_eq!(
                &MailcapEntry::<'static>::execute(owner, &attachment)
                    .unwrap_err()
                    .to_string(),
                "Not found error: No mailcap entry found for audio/ogg."
            );
        }
        {
            // Base case: first entry applies.
            let mut attachment = AttachmentBuilder::new(b"");
            attachment
                .set_raw("hello")
                .set_body_to_raw()
                .set_content_type(ContentType::Other {
                    name: Some("image.jpeg".to_string()),
                    tag: b"image/jpeg".to_vec(),
                    parameters: vec![],
                })
                .set_content_transfer_encoding(ContentTransferEncoding::Base64);
            let attachment = attachment.build();
            let process_request = MailcapEntry::<'static>::execute(owner, &attachment).unwrap();
            let ProcessRequest {
                owner: reply_owner,
                command,
                spawn: Some(_),
                result_cb: _,
                mut temporary_files,
            } = process_request
            else {
                panic!("Unexpected UIEvent: {process_request:?}");
            };
            assert_eq!(reply_owner, owner);
            assert_eq!(command.get_program(), "sh");
            let args: Vec<&str> = command
                .get_args()
                .map(|osstr| osstr.to_str().unwrap())
                .collect();
            let file = temporary_files.pop().unwrap();
            assert!(temporary_files.is_empty());
            assert_eq!(
                &args,
                &["-c", &format!("/usr/bin/open {}", file.path().display())]
            );
            assert_eq!(&file.read_to_string().unwrap(), "hello");
        }
        {
            std::env::set_var("BROWSER_TEST", "foobar");
            // Entry with `test=test -n "$BROWSER_TEST"` is tried and succeeds, then we remove the
            // var and it fails, so next is returned
            let mut attachment = AttachmentBuilder::new(b"");
            attachment
                .set_raw("hello world")
                .set_body_to_raw()
                .set_content_type(ContentType::Text {
                    kind: Text::Html,
                    parameters: vec![],
                    charset: Charset::UTF8,
                })
                .set_content_transfer_encoding(ContentTransferEncoding::_8Bit);
            let attachment = attachment.build();
            let process_request = MailcapEntry::<'static>::execute(owner, &attachment).unwrap();
            let ProcessRequest {
                owner: reply_owner,
                mut command,
                spawn: None,
                result_cb,
                temporary_files,
            } = process_request
            else {
                panic!("Unexpected UIEvent: {process_request:?}");
            };
            assert_eq!(reply_owner, owner);
            assert_eq!(command.get_program(), "sh");
            let args: Vec<&str> = command
                .get_args()
                .map(|osstr| osstr.to_str().unwrap())
                .collect();
            assert!(temporary_files.is_empty());
            assert_eq!(&args, &["-c", "test -n \"$BROWSER_TEST\""]);
            let content = (result_cb.0)(command.output().map_err(Into::into).and_then(|output| {
                let status = output.status;
                if status.success() {
                    return Ok(output);
                }
                Err(Error::new(match status.code() {
                    Some(code) => {
                        format!("Process exited with status code: {code}")
                    }
                    None => "Process terminated by signal".to_string(),
                })
                .set_details(format!("Captured output was: {output:?}")))
            }))
            .unwrap();
            let UIEvent::ProcessRequest(process_request) =
                content.downcast_ref::<UIEvent>().unwrap()
            else {
                panic!("Unexpected reply: {content:?}");
            };
            // This should be `text/html; "$BROWSER" %s && exit 1; test=test -n "$BROWSER_TEST";
            // needsterminal;`
            let ProcessRequest {
                owner: reply_owner,
                command,
                spawn: Some(_),
                result_cb: _,
                temporary_files,
            } = &**process_request
            else {
                panic!("Unexpected UIEvent: {process_request:?}");
            };
            assert_eq!(*reply_owner, owner);
            assert_eq!(command.get_program(), "sh");
            let args: Vec<&str> = command
                .get_args()
                .map(|osstr| osstr.to_str().unwrap())
                .collect();
            assert_eq!(temporary_files.len(), 1);
            let f = &temporary_files[0];
            assert_eq!(
                &args,
                &[
                    "-c",
                    &format!("\"$BROWSER\" {} && exit 1", f.path().display())
                ]
            );
            std::env::remove_var("BROWSER_TEST");

            let process_request = MailcapEntry::<'static>::execute(owner, &attachment).unwrap();
            let ProcessRequest {
                owner: reply_owner,
                mut command,
                spawn: None,
                result_cb,
                temporary_files,
            } = process_request
            else {
                panic!("Unexpected UIEvent: {process_request:?}");
            };
            assert_eq!(reply_owner, owner);
            assert_eq!(command.get_program(), "sh");
            let args: Vec<&str> = command
                .get_args()
                .map(|osstr| osstr.to_str().unwrap())
                .collect();
            assert!(temporary_files.is_empty());
            assert_eq!(&args, &["-c", "test -n \"$BROWSER_TEST\""]);
            let content = (result_cb.0)(command.output().map_err(Into::into).and_then(|output| {
                let status = output.status;
                if status.success() {
                    return Ok(output);
                }
                Err(Error::new(match status.code() {
                    Some(code) => {
                        format!("Process exited with status code: {code}")
                    }
                    None => "Process terminated by signal".to_string(),
                })
                .set_details(format!("Captured output was: {output:?}")))
            }))
            .unwrap();
            let UIEvent::ProcessRequest(process_request) =
                content.downcast_ref::<UIEvent>().unwrap()
            else {
                panic!("Unexpected reply: {content:?}");
            };
            // This should be `text/html; less; needsterminal;`
            let ProcessRequest {
                owner: reply_owner,
                command,
                spawn: Some(_),
                result_cb: _,
                temporary_files,
            } = &**process_request
            else {
                panic!("Unexpected UIEvent: {process_request:?}");
            };
            assert_eq!(*reply_owner, owner);
            assert_eq!(command.get_program(), "sh");
            let args: Vec<&str> = command
                .get_args()
                .map(|osstr| osstr.to_str().unwrap())
                .collect();
            assert!(temporary_files.is_empty());
            assert_eq!(&args, &["-c", "less"]);
        }
    }
}
