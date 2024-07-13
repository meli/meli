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

use std::{ffi::OsStr, os::unix::ffi::OsStrExt};

use super::*;
#[cfg(feature = "cli-docs")]
use crate::manpages;

fn try_path_or_stdio(input: &OsStr) -> PathOrStdio {
    if input.as_bytes() == b"-" {
        PathOrStdio::Stdio
    } else {
        PathOrStdio::Path(PathBuf::from(input))
    }
}

/// `Pathbuf` or standard stream (`-` operand).
#[derive(Debug)]
pub enum PathOrStdio {
    /// Path
    Path(PathBuf),
    /// standard stream (`-` operand)
    Stdio,
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
    /// print all directories that meli creates/uses.
    PrintAppDirectories,
    /// print location of configuration file that will be loaded on normal app
    /// startup.
    PrintConfigPath,
    /// edit configuration files with `$EDITOR`/`$VISUAL`.
    EditConfig,
    /// create a sample configuration file with available configuration options.
    /// If `PATH` is not specified, meli will try to create it in
    /// `$XDG_CONFIG_HOME/meli/config.toml`. Path `-` will output to standard
    /// output instead.
    #[structopt(display_order = 1)]
    CreateConfig {
        #[structopt(value_name = "NEW_CONFIG_PATH", parse(from_os_str = try_path_or_stdio))]
        path: Option<PathOrStdio>,
    },
    /// test a configuration file for syntax issues or missing options.
    /// If `PATH` is not specified, meli will try to read it from
    /// `$XDG_CONFIG_HOME/meli/config.toml`. Path `-` will read input from
    /// standard input instead.
    #[structopt(display_order = 2)]
    TestConfig {
        #[structopt(value_name = "CONFIG_PATH", parse(from_os_str = try_path_or_stdio))]
        path: Option<PathOrStdio>,
    },
    #[structopt(display_order = 3)]
    /// Testing tools such as IMAP, SMTP shells for debugging.
    Tools(ToolOpt),
    #[structopt(visible_alias="docs", aliases=&["docs", "manpage", "manpages"])]
    #[structopt(display_order = 4)]
    /// print documentation page and exit (Piping to a pager is recommended.).
    Man(ManOpt),
    #[structopt(display_order = 5)]
    /// Install manual pages to the first location provided by `$MANPATH` /
    /// `manpath(1)`, unless you specify the directory as an argument.
    InstallMan {
        #[structopt(value_name = "DESTINATION_PATH", parse(from_os_str))]
        destination_path: Option<PathBuf>,
    },
    #[structopt(display_order = 6)]
    /// Print compile time feature flags of this binary
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
    /// If set, output text in stdout instead of spawning `$PAGER`.
    #[cfg(feature = "cli-docs")]
    #[cfg_attr(feature = "cli-docs", structopt(long = "no-raw", alias = "no-raw"))]
    pub no_raw: bool,
    /// If set, output compressed gzip manpage in binary form in stdout.
    #[cfg(feature = "cli-docs")]
    #[cfg_attr(feature = "cli-docs", structopt(long = "gzipped"))]
    pub gzipped: bool,
    #[cfg(feature = "cli-docs")]
    #[cfg_attr(feature = "cli-docs", structopt(default_value = "meli", possible_values=manpages::POSSIBLE_VALUES, value_name="PAGE", parse(try_from_str = manpages::parse_manpage)))]
    /// Name of manual page.
    pub page: manpages::ManPages,
}

#[derive(Debug, StructOpt)]
pub enum ToolOpt {
    ImapShell {
        #[structopt(value_name = "CONFIG_TOML_ACCOUNT_NAME")]
        account: String,
    },
    #[cfg(feature = "smtp")]
    SmtpShell {
        #[structopt(value_name = "CONFIG_TOML_ACCOUNT_NAME")]
        account: String,
    },
    #[cfg(feature = "jmap")]
    JmapShell {
        #[structopt(value_name = "CONFIG_TOML_ACCOUNT_NAME")]
        account: String,
    },
}

impl Opt {
    /// Execute `self.subcommand` if any, and return its result. Otherwise
    /// return `None`.
    pub fn execute(self) -> Option<Result<()>> {
        macro_rules! ret_err {
            ($sth:expr) => {
                match $sth {
                    Ok(v) => v,
                    Err(err) => return Some(Err(err.into())),
                }
            };
        }
        Some(match self.subcommand? {
            SubCommand::View { .. } => { return None ; }
            SubCommand::TestConfig { path } => {
                subcommands::test_config(path)
            }
            SubCommand::Tools(toolopt) => {
                 subcommands::tool(self.config, toolopt)
            }
            SubCommand::CreateConfig { path } => {
                subcommands::create_config(path)
            }
            SubCommand::EditConfig => {
                subcommands::edit_config()
            }
            SubCommand::PrintConfigPath => {
                let config_path = ret_err!(crate::conf::get_config_file());
                println!("{}", config_path.display());
                Ok(())
            }
            #[cfg(not(feature = "cli-docs"))]
            SubCommand::Man(ManOpt {}) => {
                 Err(Error::new("error: this version of meli was not build with embedded documentation (cargo feature `cli-docs`). You might have it installed as manpages (eg `man meli`), otherwise check https://meli-email.org"))
            }
            #[cfg(feature = "cli-docs")]
            SubCommand::Man(ManOpt {
                page,
                no_raw,
                gzipped: true,
            }) => {
                use std::io::Write;

                ret_err!(std::io::stdout().write_all(if no_raw {
                    page.text_gz()
                } else {
                    page.mdoc_gz()
                }));
                Ok(())
            }
            #[cfg(feature = "cli-docs")]
            SubCommand::Man(ManOpt {
                page,
                no_raw,
                gzipped: false,
            }) => {
                subcommands::man(page, false).and_then(|s| subcommands::pager(s, no_raw))
            }
            SubCommand::CompiledWith => {
                subcommands::compiled_with()
            }
            SubCommand::PrintLoadedThemes => {
                let s = ret_err!(conf::FileSettings::new());
                print!("{}", s.terminal.themes);
                Ok(())
            }
            SubCommand::PrintDefaultTheme => {
                print!("{}", conf::Themes::default().key_to_string("dark", false));
                Ok(())
            }
            SubCommand::PrintAppDirectories => {
                println!(
                    "{}",
                    xdg::BaseDirectories::with_prefix("meli")
                        .expect(
                            "Could not find your XDG directories. If this is unexpected, please \
                         report it as a bug."
                        )
                        .get_data_file("")
                        .display()
                );
                let mut temp_dir = std::env::temp_dir();
                temp_dir.push("meli");
                println!("{}", temp_dir.display());
                Ok(())
            }
            #[cfg(not(feature = "cli-docs"))]
            SubCommand::InstallMan {
                destination_path: _,
            } => {
                Err(Error::new("error: this version of meli was not build with embedded documentation (cargo feature `cli-docs`). You might have it installed as manpages (eg `man meli`), otherwise check https://meli-email.org"))
            }
            #[cfg(feature = "cli-docs")]
            SubCommand::InstallMan { destination_path } => {
                match crate::manpages::ManPages::install(destination_path) {
                    Ok(p) => println!("Installed at {}.", p.display()),
                    Err(err) => return Some(Err(err)),
                }
                Ok(())
            }
            SubCommand::PrintLogPath => {
                let settings = ret_err!(crate::conf::Settings::new());
                println!("{}", settings._logger.log_dest().display());
                Ok(())
            }
        })
    }
}
