/*
 * meli - args.rs
 *
 * Copyright 2017-2023 Manos Pitsidianakis
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

//! Command line arguments.

use super::*;

#[derive(Debug, StructOpt)]
#[structopt(name = "meli", about = "terminal mail client", version_short = "v")]
pub struct Opt {
    /// use specified configuration file
    #[structopt(short, long, parse(from_os_str))]
    pub config: Option<PathBuf>,

    #[structopt(subcommand)]
    pub subcommand: Option<SubCommand>,
}

#[derive(Debug, StructOpt)]
pub enum SubCommand {
    /// print default theme in full to stdout and exit.
    PrintDefaultTheme,
    /// print loaded themes in full to stdout and exit.
    PrintLoadedThemes,
    /// print all directories that meli creates/uses.
    PrintAppDirectories,
    /// print location of configuration file that will be loaded on normal app
    /// startup.
    PrintConfigPath,
    /// edit configuration files with `$EDITOR`/`$VISUAL`.
    EditConfig,
    /// create a sample configuration file with available configuration options.
    /// If PATH is not specified, meli will try to create it in
    /// $XDG_CONFIG_HOME/meli/config.toml
    #[structopt(display_order = 1)]
    CreateConfig {
        #[structopt(value_name = "NEW_CONFIG_PATH", parse(from_os_str))]
        path: Option<PathBuf>,
    },
    /// test a configuration file for syntax issues or missing options.
    #[structopt(display_order = 2)]
    TestConfig {
        #[structopt(value_name = "CONFIG_PATH", parse(from_os_str))]
        path: Option<PathBuf>,
    },
    #[structopt(visible_alias="docs", aliases=&["docs", "manpage", "manpages"])]
    #[structopt(display_order = 3)]
    /// print documentation page and exit (Piping to a pager is recommended.).
    Man(ManOpt),

    #[structopt(display_order = 4)]
    /// Install manual pages to the first location provided by $MANPATH /
    /// manpath(1), unless you specify the directory as an argument.
    InstallMan {
        #[structopt(value_name = "DESTINATION_PATH", parse(from_os_str))]
        destination_path: Option<PathBuf>,
    },
    #[structopt(display_order = 5)]
    /// print compile time feature flags of this binary
    CompiledWith,
    /// Print log file location.
    PrintLogPath,
    /// View mail from input file.
    View {
        #[structopt(value_name = "INPUT", parse(from_os_str))]
        path: PathBuf,
    },
}

#[derive(Debug, StructOpt)]
pub struct ManOpt {
    #[structopt(default_value = "meli", possible_values=&["meli", "conf", "themes", "meli.7", "guide"], value_name="PAGE", parse(try_from_str = manpages::parse_manpage))]
    #[cfg(feature = "cli-docs")]
    pub page: manpages::ManPages,
    /// If true, output text in stdout instead of spawning $PAGER.
    #[structopt(long = "no-raw", alias = "no-raw", value_name = "bool")]
    #[cfg(feature = "cli-docs")]
    pub no_raw: Option<Option<bool>>,
}

#[cfg(feature = "cli-docs")]
pub mod manpages {
    use std::{
        env, fs,
        path::{Path, PathBuf},
        sync::Arc,
    };

    use melib::log;

    use crate::{Error, Result};

    pub fn parse_manpage(src: &str) -> Result<ManPages> {
        match src {
            "" | "meli" | "meli.1" | "main" => Ok(ManPages::Main),
            "meli.7" | "guide" => Ok(ManPages::Guide),
            "meli.conf" | "meli.conf.5" | "conf" | "config" | "configuration" => Ok(ManPages::Conf),
            "meli-themes" | "meli-themes.5" | "themes" | "theming" | "theme" => {
                Ok(ManPages::Themes)
            }
            _ => Err(Error::new(format!("Invalid documentation page: {src}",))),
        }
    }

    #[derive(Clone, Copy, Debug)]
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
        pub fn install(destination: Option<PathBuf>) -> Result<PathBuf> {
            fn path_valid(p: &Path, tries: &mut Vec<PathBuf>) -> bool {
                tries.push(p.into());
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
                        if let Some(path) =
                            env::split_paths(&paths).find(|p| path_valid(p, &mut tries))
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
    }
}
