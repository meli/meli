//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use std::{
    env, fs,
    io::Read,
    path::{Path, PathBuf},
    sync::Arc,
};

use flate2::bufread::GzDecoder;
use melib::{log, ShellExpandTrait};

use crate::{Error, Result};

pub const POSSIBLE_VALUES: &[&str] = &[
    "meli",
    "meli.1",
    "conf",
    "meli.conf",
    "meli.conf.5",
    "themes",
    "meli-themes",
    "meli-themes.5",
    "guide",
    "meli.7",
];

pub fn parse_manpage(src: &str) -> Result<ManPages> {
    match src {
        "" | "meli" | "meli.1" | "main" => Ok(ManPages::Main),
        "meli.7" | "guide" => Ok(ManPages::Guide),
        "meli.conf" | "meli.conf.5" | "conf" | "config" | "configuration" => Ok(ManPages::Conf),
        "meli-themes" | "meli-themes.5" | "themes" | "theming" | "theme" => Ok(ManPages::Themes),
        _ => Err(Error::new(format!("Invalid documentation page: {src}",))),
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Choose manpage
pub enum ManPages {
    /// meli(1)
    Main = 0,
    /// meli.conf(5)
    Conf = 1,
    /// meli-themes(5)
    Themes = 2,
    /// meli(7)
    Guide = 3,
}

impl std::fmt::Display for ManPages {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            fmt,
            "{}",
            match self {
                Self::Main => "meli.1",
                Self::Conf => "meli.conf.5",
                Self::Themes => "meli-themes.5",
                Self::Guide => "meli.7",
            }
        )
    }
}

impl ManPages {
    const MANPAGES: [&'static [u8]; 4] = [
        include_bytes!(concat!(env!("OUT_DIR"), "/meli.txt.gz")),
        include_bytes!(concat!(env!("OUT_DIR"), "/meli.conf.txt.gz")),
        include_bytes!(concat!(env!("OUT_DIR"), "/meli-themes.txt.gz")),
        include_bytes!(concat!(env!("OUT_DIR"), "/meli.7.txt.gz")),
    ];
    const MANPAGES_MDOC: [&'static [u8]; 4] = [
        include_bytes!(concat!(env!("OUT_DIR"), "/meli.mdoc.gz")),
        include_bytes!(concat!(env!("OUT_DIR"), "/meli.conf.mdoc.gz")),
        include_bytes!(concat!(env!("OUT_DIR"), "/meli-themes.mdoc.gz")),
        include_bytes!(concat!(env!("OUT_DIR"), "/meli.7.mdoc.gz")),
    ];

    pub fn install(destination: Option<PathBuf>) -> Result<PathBuf> {
        fn path_valid(p: &Path, tries: &mut Vec<PathBuf>) -> bool {
            tries.push(p.into());
            let p = p.expand();
            p.exists()
                && p.is_dir()
                && fs::metadata(p)
                    .ok()
                    .map(|m| !m.permissions().readonly())
                    .unwrap_or(false)
        }

        let mut tries = vec![];
        let Some(mut path) = destination
            .filter(|p| path_valid(p, &mut tries))
            .or_else(|| {
                if let Some(paths) = env::var_os("MANPATH") {
                    if let Some(path) = env::split_paths(&paths).find(|p| path_valid(p, &mut tries))
                    {
                        return Some(path);
                    }
                }
                None
            })
            .or_else(|| {
                #[allow(deprecated)]
                env::home_dir()
                    .map(|p| p.join(".local").join("share").join("man"))
                    .filter(|p| path_valid(p, &mut tries))
            })
        else {
            return Err(format!("Could not write to any of these paths: {:?}", tries).into());
        };
        path = path.expand();

        for (p, dir) in [
            (Self::Main, "man1"),
            (Self::Conf, "man5"),
            (Self::Themes, "man5"),
            (Self::Guide, "man7"),
        ] {
            let text = crate::subcommands::man(p, true)?;
            path.push(dir);
            std::fs::create_dir_all(&path).map_err(|err| {
                Error::new(format!("Could not create {} directory.", path.display()))
                    .set_source(Some(Arc::new(err)))
            })?;
            path.push(&p.to_string());

            fs::write(&path, text.as_bytes()).map_err(|err| {
                Error::new(format!("Could not write to {}", path.display()))
                    .set_source(Some(Arc::new(err)))
            })?;
            log::trace!("Installed {} to {}", p, path.display());
            path.pop();
            path.pop();
        }

        Ok(path)
    }

    pub fn mdoc_gz(self) -> &'static [u8] {
        Self::MANPAGES_MDOC[self as usize]
    }

    pub fn text_gz(self) -> &'static [u8] {
        Self::MANPAGES[self as usize]
    }

    pub fn read(self, source: bool) -> Result<String> {
        let mut gz = GzDecoder::new(if source {
            self.mdoc_gz()
        } else {
            self.text_gz()
        });
        let mut v = String::with_capacity(
            str::parse::<usize>(unsafe {
                std::str::from_utf8_unchecked(gz.header().unwrap().comment().unwrap())
            })
            .unwrap_or_else(|_| panic!("{:?} was not compressed with size comment header", self)),
        );
        gz.read_to_string(&mut v)?;

        Ok(v)
    }

    /// Helper function to remove backspace markup from mandoc output.
    pub fn remove_markup(input: &str) -> Result<String> {
        use std::{
            io::Write,
            process::{Command, Stdio},
        };
        let mut child = Command::new("col")
            .arg("-b")
            .arg("-x")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        child.stdin.as_mut().unwrap().write_all(input.as_bytes())?;
        Ok(String::from_utf8_lossy(&child.wait_with_output()?.stdout).to_string())
    }
}
