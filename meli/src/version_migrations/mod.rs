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

#[cfg(test)]
mod tests;

pub type VersionMap = IndexMap<VersionIdentifier, Box<dyn Version + Send + Sync + 'static>>;

/// Utility macro to define version module imports and a function `versions() ->
/// &'static VersionMap`.
///
/// Version arguments must be given in sorted, ascending order:
///
/// ```rust
/// # use meli::{decl_version_map, version_migrations::*};
/// decl_version_map! {
///    v0_8_8::V0_8_8_ID => v0_8_8::V0_8_8,
///    v0_8_9::V0_8_9_ID => v0_8_9::V0_8_9
/// }
/// ```
///
/// Otherwise compilation will fail:
///
/// ```compile_fail
/// # use meli::{decl_version_map, version_migrations::*};
/// mod v0_0_0 {
///     use meli::version_migrations::*;
///
///     pub const V0_0_0_ID: VersionIdentifier = VersionIdentifier::NULL;
///
///     #[derive(Clone, Copy, Debug)]
///     pub struct V0_0_0;
///
///     impl Version for V0_0_0 {
///         fn version(&self) -> &VersionIdentifier {
///             &V0_0_0_ID
///         }
///
///         fn migrations(&self) -> Vec<Box<dyn Migration + Send + Sync + 'static>> {
///             vec![]
///         }
///     }
/// }
/// decl_version_map! {
///    v0_8_8::V0_8_8_ID => v0_8_8::V0_8_8,
///    v0_0_0::V0_0_0_ID => v0_0_0::V0_0_0,
///    v0_8_9::V0_8_9_ID => v0_8_9::V0_8_9
/// }
/// ```
///
/// ```compile_fail
/// # use meli::{decl_version_map, version_migrations::*};
/// decl_version_map! {
///    v0_8_9::V0_8_9_ID => v0_8_9::V0_8_9,
///    v0_8_8::V0_8_8_ID => v0_8_8::V0_8_8
/// }
/// ```
#[macro_export]
macro_rules! decl_version_map {
    ($($version_id:path => $m:ident::$v:ident),*$(,)?) => {
        fn versions() -> &'static VersionMap {
            use std::sync::OnceLock;
            #[allow(dead_code)]
            const fn const_bytes_cmp(lhs: &[u8], rhs: &[u8]) -> std::cmp::Ordering {
                if lhs.len() < rhs.len() {
                    return std::cmp::Ordering::Less;
                } else if lhs.len() > rhs.len() {
                    return std::cmp::Ordering::Greater;
                };
                let mut i = 0;
                while i < lhs.len() {
                    if lhs[i] < rhs[i] {
                        return std::cmp::Ordering::Less;
                    } else if lhs[i] > rhs[i] {
                        return std::cmp::Ordering::Greater;
                    }
                    i += 1;
                }
                std::cmp::Ordering::Equal
            }

            #[allow(dead_code)]
            const fn const_str_cmp(lhs: &str, rhs: &str) -> std::cmp::Ordering {
                const_bytes_cmp(lhs.as_bytes(), rhs.as_bytes())
            }

            macro_rules! v_ids_cmp {
                ($v2:expr, $v1:expr) => {{

                    $v2.major() >= $v1.major()
                     && ($v2.minor() >= $v1.minor())
                     && ($v2.patch() >= $v1.patch())
                     && ((const_str_cmp($v2.pre(), $v1.pre()) as i8 == std::cmp::Ordering::Greater as i8) || (const_str_cmp($v2.pre(), $v1.pre()) as i8 == std::cmp::Ordering::Equal as i8))
                     && !($v2.major() == $v1.major()
                     && ($v2.minor() == $v1.minor())
                     && ($v2.patch() == $v1.patch()))
                }}
            }
            macro_rules! is_version_ids_sorted {
                () => {
                    true
                };
                ($v0:expr) => {
                    $v0 > VersionIdentifier::NULL && true
                };
                ($v1:expr, $v2:expr) => {{
                    v_ids_cmp!($v2, $v1)
                }};
                ($v1:expr, $v2:expr, $tail_v:tt) => {{
                    v_ids_cmp!($v2, $v1) && is_version_ids_sorted! { $v1, $tail_v }
                }};
            }
            const fn __assert_sorted() -> () {
                assert!(is_version_ids_sorted! { $($version_id),* }, "Version ids in decl_version_mods are not sorted! Please fix it.");
            }
            const _SORT_ASSERTION: () = __assert_sorted();
            const fn __assert_latest() -> () {
                macro_rules! latest_version_id {
                    ($v0:expr) => {
                        $v0
                    };
                    ($v1:expr, $v2:expr) => {{
                        $v2
                    }};
                    ($v1:expr, $v2:expr, $tail_v:tt) => {{
                        latest_version_id! { $tail_v }
                    }};
                }
                if let Some(current_version) = option_env!("CARGO_PKG_VERSION") {
                    let latest_version = latest_version_id!{ $($version_id),* };
                    if const_str_cmp(current_version, latest_version.as_str()) as i8 != std::cmp::Ordering::Equal as i8 {
                        panic!("Current version does not match latest version from version migrations map declaration, please fix it.");
                    }
                }
            }
            const _LATEST_ASSERTION: () = __assert_latest();


            static VERSIONS: OnceLock<VersionMap> = OnceLock::new();
            VERSIONS.get_or_init(|| {
                let val = indexmap::indexmap! {
                    $(
                        $version_id => Box::new($m::$v) as Box<dyn Version + Send + Sync + 'static>
                    ),*
                };
                {
                    let version_ids = val.keys().collect::<Vec<_>>();
                    let mut version_ids_sorted = version_ids.clone();
                    version_ids_sorted.sort();
                    assert_eq!(version_ids, version_ids_sorted, "Version map returned by versions() is not sorted! Check out decl_version_mods! invocation. Version ids were: {:?}", version_ids);
                }
                val
            })
        }
    };
}

macro_rules! decl_version_mods {
    ($($version_id:path => $m:ident::$v:ident),*$(,)?) => {
        $(
            pub mod $m;
            pub use $m::$v;
        )*

        decl_version_map! {
            $($version_id => $m::$v),*
        }
    };
}

decl_version_mods! {
    v0_8_8::V0_8_8_ID => v0_8_8::V0_8_8,
    v0_8_9::V0_8_9_ID => v0_8_9::V0_8_9
}

use std::{cmp::Ordering, path::Path};

use indexmap::{self, IndexMap};
use melib::{error::*, log};

use crate::{conf::FileSettings, terminal::Ask};

pub const LATEST: VersionIdentifier = v0_8_9::V0_8_9_ID;

/// An application version identifier.
#[derive(Clone, Copy, Debug, Eq)]
pub struct VersionIdentifier {
    string: &'static str,
    major: u8,
    minor: u8,
    patch: u8,
    pre: &'static str,
}

impl VersionIdentifier {
    pub const NULL: Self = Self {
        string: "0.0.0",
        major: 0,
        minor: 0,
        patch: 0,
        pre: "",
    };

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
    pub const fn pre(&self) -> &'static str {
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
        self.as_str().hash(state)
    }
}

/// A migration contained in a released version.
pub trait Migration: Send + Sync {
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
pub trait Version: Send + Sync {
    /// Associated version identifier.
    fn version(&self) -> &VersionIdentifier;
    /// Associated migrations, if any.
    fn migrations(&self) -> Vec<Box<dyn Migration + Send + Sync + 'static>>;
    // /// Associated changelog, if any.
    // fn changelog(&self) -> &str;
    // /// Important notice messages for users, if any.
    // fn notices(&self) -> &[&str];
}

impl std::fmt::Debug for dyn Version + Send + Sync {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(dyn Version + Send + Sync))
            .field("version", &self.version())
            .field("migrations", &self.migrations())
            .finish_non_exhaustive()
    }
}

impl std::fmt::Debug for dyn Migration + Send + Sync {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(dyn Migration + Send + Sync))
            .field("id", &self.id())
            .field("version", &self.version())
            .field("description", &self.description())
            .finish_non_exhaustive()
    }
}

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
        let migrations = calculate_migrations(stored_version.as_deref(), version_map);
        if !migrations.is_empty() {
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
            let mut perform_history: Vec<Box<dyn Migration + 'static>> = vec![];
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
        }
        std::fs::write(&version_file, LATEST.as_str()).chain_err_related_path(&version_file)?;
    }

    Ok(())
}

/// Return any migrations between current version and latest version, if any.
pub fn calculate_migrations<'v>(
    current_version: Option<&str>,
    version_map: &'v VersionMap,
) -> Vec<(&'v VersionIdentifier, Vec<Box<dyn Migration + Send + Sync>>)> {
    let mut migrations = vec![];
    if let Some(newer_versions) = current_version
        .and_then(|v| version_map.get_index_of(v))
        .map(|i| i + 1)
        .or(Some(0))
        .and_then(|i| version_map.get_range(i..))
    {
        for (k, v) in newer_versions {
            let vec = v.migrations();
            if !vec.is_empty() {
                migrations.push((k, vec));
            }
        }
    }
    migrations
}
