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

//! Helping users move to newer `meli` versions.

mod v0_8_8;
use v0_8_8::V0_8_8;

type VersionMap = IndexMap<VersionIdentifier, Box<dyn Version + Send + Sync + 'static>>;

fn versions() -> &'static VersionMap {
    use std::sync::OnceLock;

    static VERSIONS: OnceLock<VersionMap> = OnceLock::new();
    VERSIONS.get_or_init(|| {
        indexmap::indexmap! {
            v0_8_8::V0_8_8_ID => Box::new(V0_8_8) as Box<dyn Version + Send + Sync + 'static>,
        }
    })
}

use std::{cmp::Ordering, path::Path};

use indexmap::{self, IndexMap};
use melib::{error::*, log};

use crate::{conf::FileSettings, terminal::Ask};

pub const LATEST: VersionIdentifier = v0_8_8::V0_8_8_ID;

/// Inspect current/previous version setup, perform migrations if necessary,
/// etc.
pub fn version_setup(config: &Path) -> Result<()> {
    if let Ok(xdg_dirs) = xdg::BaseDirectories::with_prefix("meli") {
        let version_file = match xdg_dirs.place_data_file(".version") {
            Ok(v) => v,
            Err(err) => {
                log::debug!(
                    "Could not place file with version metadata, .version, in your \
                     ${{XDG_DATA_HOME}}: {}",
                    err
                );
                return Ok(());
            }
        };
        let stored_version = if !version_file.try_exists().unwrap_or(false) {
            None
        } else {
            let mut stored_version =
                std::fs::read_to_string(&version_file).chain_err_related_path(&version_file)?;
            while stored_version.ends_with(['\r', '\n', ' ', '\t']) {
                stored_version.pop();
            }
            if LATEST.as_str() == stored_version {
                return Ok(());
            }
            Some(stored_version)
        };
        let version_map = versions();
        if let Some(newer_versions) = stored_version
            .as_ref()
            .and_then(|v| version_map.get_index_of(v.as_str()))
            .or(Some(0))
            .and_then(|i| version_map.get_range(i..))
        {
            let mut migrations = vec![];
            for (k, v) in newer_versions {
                let vec = v.migrations();
                if !vec.is_empty() {
                    migrations.push((k, vec));
                }
            }
            if migrations.is_empty() {
                return Ok(());
            }
            if let Some(prev) = stored_version {
                println!(
                    "meli appears updated; file {} contains the value {:?} and the latest version \
                     is {}",
                    version_file.display(),
                    prev,
                    LATEST
                );
            } else {
                // Check if any migrations are applicable; they might not be any (for example if
                // user runs meli for the first time).
                if !migrations.iter().any(|(_, migrs)| {
                    migrs
                        .iter()
                        .any(|migr| migr.is_applicable(config) != Some(false))
                }) {
                    log::info!(
                        "Creating version info file {} with value {}",
                        version_file.display(),
                        LATEST
                    );
                    std::fs::write(&version_file, LATEST.as_str())
                        .chain_err_related_path(&version_file)?;
                    return Ok(());
                }
                println!(
                    "meli appears updated; version file {} was not found and there are potential \
                     migrations to be made.",
                    version_file.display()
                );
            }
            println!(
                "You might need to migrate your configuration data for the new version to \
                 work.\nYou can skip any changes you don't want to happen and you can quit at any \
                 time."
            );
            println!(
                "{} migration{} {} about to be performed:",
                migrations.len(),
                if migrations.len() == 1 { "" } else { "s" },
                if migrations.len() == 1 { "is" } else { "are" }
            );
            for (vers, migrs) in &migrations {
                for m in migrs {
                    println!("v{}/{}: {}", vers, m.id(), m.description());
                }
            }
            let ask = Ask::new(format!(
                "Perform {} migration{}?",
                migrations.len(),
                if migrations.len() == 1 { "" } else { "s" }
            ));
            if !ask.run() {
                let ask = Ask::new("Update .version file despite not attempting migrations?")
                    .yes_by_default(false);
                if ask.run() {
                    std::fs::write(&version_file, LATEST.as_str())
                        .chain_err_related_path(&version_file)?;
                    return Ok(());
                }
                return Ok(());
            }
            let mut perform_history: Vec<Box<dyn Migration + Send + Sync + 'static>> = vec![];
            for (vers, migrs) in migrations {
                println!("Updating to {}...", vers);
                'migrations: for m in migrs {
                    let ask = Ask::new(m.question());
                    if ask.run() {
                        if let Err(err) = m.perform(config, false, true) {
                            println!("\nCould not perform migration: {}", err);
                            let ask = Ask::new("Continue?");
                            if ask.run() {
                                continue 'migrations;
                            }
                            if !perform_history.is_empty() {
                                let ask =
                                    Ask::new("Undo already performed migrations before exiting?")
                                        .without_default();
                                if ask.run() {
                                    while let Some(m) = perform_history.pop() {
                                        print!("Undoing {}...", m.id());
                                        if let Err(err) = m.revert(config, false, true) {
                                            println!(
                                                " [ERROR] could not revert migration: {}",
                                                err
                                            );
                                        } else {
                                            println!(" [OK]");
                                        }
                                    }
                                }
                            }
                            return Ok(());
                        }
                        println!("v{}/{} [OK]", vers, m.id());
                        perform_history.push(m);
                    }
                }
            }
            std::fs::write(&version_file, LATEST.as_str()).chain_err_related_path(&version_file)?;
        }
    }

    Ok(())
}

/// An application version identifier.
#[derive(Clone, Copy, Debug, Eq)]
pub struct VersionIdentifier {
    string: &'static str,
    major: u8,
    minor: u8,
    patch: u8,
    pre: Option<&'static str>,
}

impl VersionIdentifier {
    /// The identifier as a string.
    pub const fn as_str(&self) -> &'static str {
        self.string
    }

    /// The major part of the version (`MAJOR.MINOR.PATCH[-PRE]`).
    pub const fn major(&self) -> u8 {
        self.major
    }

    /// The minor part of the version (`MAJOR.MINOR.PATCH[-PRE]`).
    pub const fn minor(&self) -> u8 {
        self.minor
    }

    /// The patch part of the version (`MAJOR.MINOR.PATCH[-PRE]`).
    pub const fn patch(&self) -> u8 {
        self.patch
    }

    /// The pre-release part of the version (`MAJOR.MINOR.PATCH[-PRE]`).
    pub const fn pre(&self) -> Option<&'static str> {
        self.pre
    }
}

impl std::fmt::Display for VersionIdentifier {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.string)
    }
}

impl Ord for VersionIdentifier {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.major, self.minor, self.patch, self.pre).cmp(&(
            other.major,
            other.minor,
            other.patch,
            other.pre,
        ))
    }
}

impl PartialOrd for VersionIdentifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for VersionIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl std::borrow::Borrow<str> for VersionIdentifier {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}
impl std::hash::Hash for VersionIdentifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.major, self.minor, self.patch, self.pre).hash(state)
    }
}

/// A migration contained in a released version.
pub trait Migration {
    /// The source code identifier of the migration; usually the `struct`'s Rust
    /// identifier.
    fn id(&self) -> &'static str;
    /// The version this is included in.
    fn version(&self) -> &VersionIdentifier;
    /// A user-oriented description of what the migration does.
    fn description(&self) -> &str;
    /// Question to ask the user when migration is performed interactively.
    fn question(&self) -> &str;
    /// Try to check if migration is applicable. Return `None` on no confidence.
    fn is_applicable(&self, config: &Path) -> Option<bool>;
    /// Perform migration actions for given configuration file, and allow a dry
    /// run and verbose log prints.
    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()>;
    /// Revert migration actions for given configuration file, and allow a dry
    /// run and verbose log prints.
    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()>;
}

/// A released application version.
pub trait Version {
    /// Associated version identifier.
    fn version(&self) -> &VersionIdentifier;
    /// Associated migrations, if any.
    fn migrations(&self) -> Vec<Box<dyn Migration + Send + Sync + 'static>>;
    // /// Associated changelog, if any.
    // fn changelog(&self) -> &str;
    // /// Important notice messagese for users, if any.
    // fn notices(&self) -> &[&str];
}
