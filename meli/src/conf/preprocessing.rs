//
// meli
//
// Copyright 2017, 2026 Manos Pitsidianakis <manos@pitsidianak.is>
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
    borrow::Cow,
    collections::VecDeque,
    path::{Path, PathBuf},
};

use indexmap::IndexMap;
use melib::{
    error::{Error, ErrorKind, Result, ResultIntoError as _, WrapResultIntoError as _},
    utils::{parsec::*, shellexpand::ShellExpandTrait as _},
};

/// An included configuration file
#[derive(Clone, Debug)]
pub struct Spanned {
    /// Byte range inside expanded configuration.
    pub span: std::ops::Range<usize>,
    /// The path source.
    pub path: PathBuf,
    /// The spanned value.
    pub value: String,
}

/// An expanded configuration.
#[derive(Clone, Debug)]
pub struct Preprocessed {
    /// The main configuration file.
    pub source: PathBuf,
    /// The expanded contents.
    pub expanded_contents: String,
    /// Any unfolded expansions.
    pub expansions: IndexMap<std::ops::Range<usize>, Spanned>,
}

impl Preprocessed {
    /// Preprocess the contents of file at `conf_path`.
    pub fn new(conf_path: &Path) -> Result<Self> {
        let source: PathBuf = if conf_path.is_relative() {
            conf_path
                .expand()
                .canonicalize()
                .chain_err_related_path(conf_path)?
        } else {
            conf_path.expand()
        };
        let contents = std::fs::read_to_string(&source).chain_err_related_path(&source)?;

        let mut expanded_contents = String::with_capacity(contents.len());

        struct Item<'a> {
            path: Cow<'a, Path>,
            row: usize,
            start: usize,
            contents: String,
        }

        let mut stack: VecDeque<Item<'_>> = VecDeque::new();
        stack.push_front(Item {
            path: Cow::Borrowed(&source),
            row: 0,
            start: 0,
            contents,
        });
        let mut expansions = IndexMap::new();

        'stack: while let Some(item) = stack.pop_back() {
            let mut line_iter = item
                .contents
                .split_inclusive('\n')
                .skip(item.row)
                .enumerate();
            while let Some((i, line)) = line_iter.next() {
                let (_, Some(sub_path)) = include_directive().parse(line).map_err(|line| {
                    Error::new(format!(
                        "Malformed include directive in line {i} of file {file}: \
                         {line}\nConfiguration uses the standard m4 macro include(\"filename\").",
                        file = item.path.as_ref().display(),
                    ))
                    .set_related_path(Some(item.path.as_ref()))
                    .set_kind(ErrorKind::ValueError)
                })?
                else {
                    expanded_contents.push_str(line);
                    continue;
                };
                let mut sub_path = Path::new(sub_path).expand();
                drop(line_iter);
                if sub_path.is_relative() {
                    if let Some(prefix) = item.path.parent() {
                        sub_path = prefix.join(sub_path);
                    }
                }
                let contents =
                    match std::fs::read_to_string(&sub_path).chain_err_related_path(&sub_path) {
                        Ok(v) => v,
                        Err(err) => {
                            return Err(Error::new(format!(
                                "Path {} which is included in {in_parent} line {i} {msg}",
                                sub_path.display(),
                                in_parent = item.path.display(),
                                msg = if !sub_path.exists() {
                                    "does not exist"
                                } else {
                                    "is a directory, not a text file"
                                }
                            ))
                            .set_related_path(Some(item.path.as_ref()))
                            .set_source(Some(Box::new(err)))
                            .set_kind(ErrorKind::ValueError));
                        }
                    };
                {
                    {
                        let Item {
                            path,
                            row,
                            start,
                            contents,
                        } = item;
                        stack.push_back(Item {
                            path,
                            row: row + i + 1,
                            start,
                            contents,
                        });
                    }
                }
                stack.push_back(Item {
                    path: Cow::Owned(sub_path),
                    row: 0,
                    contents,
                    start: expanded_contents.len(),
                });
                continue 'stack;
            }
            let Item {
                path,
                row: _,
                start,
                contents,
            } = item;
            if let Cow::Owned(path) = path {
                expansions.insert(
                    start..expanded_contents.len(),
                    Spanned {
                        span: start..expanded_contents.len(),
                        path,
                        value: contents,
                    },
                );
            }
        }

        Ok(Self {
            source,
            expansions,
            expanded_contents,
        })
    }
}

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

/// Expand `path` with [`Preprocessed::new`] function and also appends theme files from
/// any `$XDG_CONFIG_DIRS/meli/themes` directory.
fn pp_inner(path: &Path) -> Result<String> {
    let conf_path: PathBuf = if path.is_relative() {
        path.expand().canonicalize()?
    } else {
        path.expand()
    };

    let Preprocessed {
        source: _,
        expanded_contents: mut ret,
        expansions: _,
    } = Preprocessed::new(&conf_path)?;
    if let Ok(xdg_dirs) = xdg::BaseDirectories::with_prefix("meli") {
        for theme_mailbox in xdg_dirs.find_config_files("themes") {
            let read_dir =
                std::fs::read_dir(&theme_mailbox).chain_err_related_path(&theme_mailbox)?;
            for theme in read_dir {
                let theme_path = theme?.path();
                if let Some(extension) = theme_path.extension() {
                    if extension == "toml" {
                        let Preprocessed {
                            source: _,
                            expanded_contents: theme,
                            expansions: _,
                        } = Preprocessed::new(&theme_path)?;
                        ret.push_str(&theme);
                    }
                }
            }
        }
    }
    Ok(ret)
}

/// Expands `include` macros in configuration file and other configuration
/// files (eg. themes from any `$XDG_CONFIG_DIRS/meli/themes` directory).
pub fn pp(path: &Path) -> Result<String> {
    pp_inner(path)
        .wrap_err(|| "Could not preprocess configuration file")
        .chain_err_related_path(path)
        .chain_err_kind(ErrorKind::Configuration)
}

pub fn get_included_configs(conf_path: &Path) -> Result<Vec<PathBuf>> {
    let Preprocessed {
        source,
        expanded_contents: _,
        expansions,
    } = Preprocessed::new(conf_path)?;

    Ok(std::iter::once(source)
        .chain(expansions.into_iter().map(|(_, s)| s.path))
        .collect())
}
