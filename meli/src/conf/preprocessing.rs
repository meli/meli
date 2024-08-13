//
// meli
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

//! Preprocess configuration files by unfolding `include` macros.

use std::{
    io::{self, BufRead, Read, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Arc,
};

use melib::{
    error::{Error, ErrorKind, Result, ResultIntoError, WrapResultIntoError},
    utils::parsec::*,
    ShellExpandTrait,
};

/// Try to parse line into a path to be included.
pub fn include_directive<'a>() -> impl Parser<'a, Option<&'a str>> {
    move |input: &'a str| {
        enum State {
            Start,
            Path,
        }
        use State::*;
        let mut state = State::Start;

        let mut i = 0;
        while i < input.len() {
            match (&state, input.as_bytes()[i]) {
                (Start, b'#') => {
                    return Ok(("", None));
                }
                (Start, b) if (b as char).is_whitespace() => { /* consume */ }
                (Start, _) if input.as_bytes()[i..].starts_with(b"include(") => {
                    i += "include(".len();
                    state = Path;
                    continue;
                }
                (Start, _) => {
                    return Ok(("", None));
                }
                (Path, b'"') | (Path, b'\'') | (Path, b'`') => {
                    let mut end = i + 1;
                    while end < input.len() && input.as_bytes()[end] != input.as_bytes()[i] {
                        end += 1;
                    }
                    if end == input.len() {
                        return Err(input);
                    }
                    let ret = &input[i + 1..end];
                    end += 1;
                    if end < input.len() && input.as_bytes()[end] != b')' {
                        /* Nothing else allowed in line */
                        return Err(input);
                    }
                    end += 1;
                    while end < input.len() {
                        if !(input.as_bytes()[end] as char).is_whitespace() {
                            /* Nothing else allowed in line */
                            return Err(input);
                        }
                        end += 1;
                    }
                    return Ok(("", Some(ret)));
                }
                (Path, _) => return Err(input),
            }
            i += 1;
        }
        Ok(("", None))
    }
}

/// Expands `include` macros in path.
fn pp_helper(path: &Path, level: u8) -> Result<String> {
    if level > 7 {
        return Err(Error::new(format!(
            "Maximum recursion limit reached while unfolding include directives in {}. Have you \
             included a config file within itself?",
            path.display()
        ))
        .set_kind(ErrorKind::ValueError));
    }
    let mut contents = String::new();
    let mut file = std::fs::File::open(path)?;
    file.read_to_string(&mut contents)?;
    let mut ret = String::with_capacity(contents.len());

    for (i, l) in contents.lines().enumerate() {
        if let (_, Some(sub_path)) = include_directive().parse(l).map_err(|l| {
            Error::new(format!(
                "Malformed include directive in line {} of file {}: {}\nConfiguration uses the \
                 standard m4 macro include(\"filename\").",
                i,
                path.display(),
                l
            ))
            .set_kind(ErrorKind::ValueError)
        })? {
            let mut p = Path::new(sub_path).expand();
            if p.is_relative() {
                /* We checked that path is ok above so we can do unwrap here */
                let prefix = path.parent().unwrap();
                p = prefix.join(p)
            }

            ret.push_str(&pp_helper(&p, level + 1).chain_err_related_path(&p)?);
        } else {
            ret.push_str(l);
            ret.push('\n');
        }
    }

    Ok(ret)
}

fn pp_inner(path: &Path) -> Result<String> {
    let p_buf: PathBuf = if path.is_relative() {
        path.expand().canonicalize()?
    } else {
        path.expand()
    };

    let mut ret = expand_config(&p_buf)?;
    if let Ok(xdg_dirs) = xdg::BaseDirectories::with_prefix("meli") {
        for theme_mailbox in xdg_dirs.find_config_files("themes") {
            let read_dir =
                std::fs::read_dir(&theme_mailbox).chain_err_related_path(&theme_mailbox)?;
            for theme in read_dir {
                let theme_path = theme?.path();
                if let Some(extension) = theme_path.extension() {
                    if extension == "toml" {
                        ret.push_str(
                            &pp_helper(&theme_path, 0).chain_err_related_path(&theme_path)?,
                        );
                    }
                }
            }
        }
    }
    Ok(ret)
}

/// Expands `include` macros in configuration file and other configuration
/// files (eg. themes) in the filesystem.
pub fn pp(path: &Path) -> Result<String> {
    pp_inner(path)
        .wrap_err(|| "Could not preprocess configuration file")
        .chain_err_related_path(path)
        .chain_err_kind(ErrorKind::Configuration)
}

pub fn expand_config(conf_path: &Path) -> Result<String> {
    fn inner(conf_path: &Path) -> Result<String> {
        let _paths = get_included_configs(conf_path)?;
        const M4_PREAMBLE: &str = r#"define(`builtin_include', defn(`include'))dnl
define(`include', `builtin_include(substr($1,1,decr(decr(len($1)))))dnl')dnl
"#;
        let mut contents = String::new();
        contents.clear();
        let mut file = std::fs::File::open(conf_path)?;
        file.read_to_string(&mut contents)?;

        let mut handle = Command::new("m4")
            .current_dir(conf_path.parent().unwrap_or_else(|| Path::new("/")))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let mut stdin = handle.stdin.take().unwrap();
        stdin.write_all(M4_PREAMBLE.as_bytes())?;
        stdin.write_all(contents.as_bytes())?;
        drop(stdin);
        let stdout = handle.wait_with_output()?.stdout;
        Ok(String::from_utf8_lossy(&stdout).to_string())
    }

    inner(conf_path).chain_err_related_path(conf_path)
}

pub fn get_included_configs(conf_path: &Path) -> Result<Vec<PathBuf>> {
    const M4_PREAMBLE: &str = r#"divert(-1)dnl
define(`include', `divert(0)$1
divert(-1)
')dnl
changequote(`"', `"')dnl
"#;
    let mut ret = vec![];
    let prefix = conf_path.parent().unwrap().to_path_buf();
    let mut stack = vec![(None::<PathBuf>, conf_path.to_path_buf())];
    let mut contents = String::new();
    while let Some((parent, p)) = stack.pop() {
        if !p.exists() || p.is_dir() {
            return Err(Error::new(format!(
                "Path {}{included}{in_parent} {msg}.",
                p.display(),
                included = if parent.is_some() {
                    " which is included in "
                } else {
                    ""
                },
                in_parent = if let Some(parent) = parent {
                    std::borrow::Cow::Owned(parent.display().to_string())
                } else {
                    std::borrow::Cow::Borrowed("")
                },
                msg = if !p.exists() {
                    "does not exist"
                } else {
                    "is a directory, not a text file"
                }
            ))
            .set_kind(ErrorKind::ValueError));
        }
        contents.clear();
        let mut file = std::fs::File::open(&p).chain_err_related_path(&p)?;
        file.read_to_string(&mut contents)
            .chain_err_related_path(&p)?;

        let mut handle = match Command::new("m4")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(handle) => handle,
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => {
                    return Err(Error::new(
                        "`m4` executable not found in PATH. Please provide an m4 binary.",
                    )
                    .set_kind(ErrorKind::Platform))
                }
                _ => {
                    return Err(Error::new("Could not process configuration with `m4`")
                        .set_source(Some(Arc::new(err)))
                        .set_kind(ErrorKind::Platform))
                }
            },
        };

        let mut stdin = handle.stdin.take().unwrap();
        stdin.write_all(M4_PREAMBLE.as_bytes())?;
        stdin.write_all(contents.as_bytes())?;
        drop(stdin);
        let stdout = handle.wait_with_output()?.stdout.clone();
        for subpath in stdout.lines() {
            let subpath = subpath?;
            let path = &Path::new(&subpath);
            if path.is_absolute() {
                stack.push((Some(p.to_path_buf()), path.to_path_buf()));
            } else {
                stack.push((Some(p.to_path_buf()), prefix.join(path)));
            }
        }
        ret.push(p.to_path_buf());
    }

    Ok(ret)
}
