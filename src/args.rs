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

use meli::*;

#[cfg(feature = "cli-docs")]
fn parse_manpage(src: &str) -> Result<ManPages> {
    match src {
        "" | "meli" | "meli.1" | "main" => Ok(ManPages::Main),
        "meli.7" | "guide" => Ok(ManPages::Guide),
        "meli.conf" | "meli.conf.5" | "conf" | "config" | "configuration" => Ok(ManPages::Conf),
        "meli-themes" | "meli-themes.5" | "themes" | "theming" | "theme" => Ok(ManPages::Themes),
        _ => Err(Error::new(format!("Invalid documentation page: {}", src))),
    }
}

#[cfg(feature = "cli-docs")]
#[derive(Copy, Clone, Debug)]
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
    /// edit configuration files in `$EDITOR`/`$VISUAL`.
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
    /// print compile time feature flags of this binary
    CompiledWith,

    /// View mail from input file.
    View {
        #[structopt(value_name = "INPUT", parse(from_os_str))]
        path: PathBuf,
    },
}

#[derive(Debug, StructOpt)]
pub struct ManOpt {
    #[structopt(default_value = "meli", possible_values=&["meli", "conf", "themes", "meli.7", "guide"], value_name="PAGE", parse(try_from_str = parse_manpage))]
    #[cfg(feature = "cli-docs")]
    pub page: ManPages,
    /// If true, output text in stdout instead of spawning $PAGER.
    #[structopt(long = "no-raw", alias = "no-raw", value_name = "bool")]
    #[cfg(feature = "cli-docs")]
    pub no_raw: Option<Option<bool>>,
}
