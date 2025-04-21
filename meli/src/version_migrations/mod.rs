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
//!
//! # How version information is stored in the filesystem and examined
//!
//! On start-up, `meli` checks the contents of `${XDG_DATA_HOME}/meli/.version`
//! (the "version file") to determine if there has been a version upgrade since
//! the last time it was launched, if any. Regardless of the version file
//! existence or its contents, it will write the latest version string as
//! recorded in the module `const` global [`LATEST`] **unless** the version
//! file's content match `LATEST`.
//!
//! [`LATEST`] is verified to contain the actual version at compile-time using
//! Cargo's environment variable `CARGO_PKG_VERSION`.
//!
//! If the version file does not exist, no migrations need to be performed.
//!
//! If the version file is determined to be a previous version,
//! [`calculate_migrations`] is called which examines every migration in the
//! version range starting from the previous version up to the latest. If any
//! migration is applicable, it asks the user interactively whether to perform
//! them. This happens in [`version_setup`].
//!
//! # How `meli` encodes version information statically with types and modules
//!
//! Every release **MUST** have a module associated with it. The module
//! contains:
//!
//! - a public [`VersionIdentifier`] `const` item
//! - a `struct` that represents the version, named by convention `VX_Y_Z[..]`.
//!   The `struct` definition can be empty (e.g. `pub struct V0_0_1;`). The
//!   `struct` **MUST** implement the [`Version`] trait, which can be used to
//!   retrieve the version identifier and the migrations.
//! - Any number of structs representing migrations, which implement the
//!   [`Migration`] trait, and which are returned by the [`Version::migrations`]
//!   method.
//!
//! All versions must be stored in an `IndexMap` (type alias [`VersionMap`])
//! which is retrieved by the function [`versions`].
//!
//! # How migrations work
//!
//! Migrations are **not** guaranteed to be lossless; stored metadata
//! information in the filesystem may be lost.
//!
//! Migrations can optionally claim they are not applicable, which means they
//! will be skipped entirely if migrations are to be applied. The check is done
//! in the [`Migration::is_applicable`] trait method which can opt-in to make
//! checks in the configuration file.
//!
//! Migrations can be performed using the [`Migration::perform`] method, which
//! can optionally attempt to make a "dry run" application, which can check for
//! errors but not make any actual changes in the filesystem.
//!
//! If a migration can be reverted, it can implement the revert logic in
//! [`Migration::revert`] which follows the same logic as [`Migration::perform`]
//! but in reverse. It is not always possible a migration can be reverted, since
//! migrations are not necessarily lossless.

#[cfg(test)]
mod tests;

/// A container for [`Version`]s indexed by their [`VersionIdentifier`].
///
/// Internally it is returned by the [`versions`] function in this
/// module.
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
///    v0_8_9::V0_8_9_ID => v0_8_9::V0_8_9,
///    v0_8_10::V0_8_10_ID => v0_8_10::V0_8_10,
///    v0_8_11::V0_8_11_ID => v0_8_11::V0_8_11,
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
        /// Return all versions in a [`VersionMap`] container.
        ///
        /// The value is lazily initialized on first access.
        pub fn versions() -> &'static VersionMap {
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
            const _VERSION_ARRAY: &[VersionIdentifier] = &[$($version_id),*];
            const fn __assert_sorted() -> () {
                const N: usize = _VERSION_ARRAY.len();
                let mut i = 0;
                while i < N - 1 {
                    assert!(v_ids_cmp!(_VERSION_ARRAY[i+1], _VERSION_ARRAY[i]), "Version ids in decl_version_mods are not sorted! Please fix it.");
                    i += 1;
                }
            }
            const _SORT_ASSERTION: () = __assert_sorted();
            const fn __assert_latest() -> () {
                if let Some(current_version) = option_env!("CARGO_PKG_VERSION") {
                    let latest_version = _VERSION_ARRAY[_VERSION_ARRAY.len() - 1];
                    if const_str_cmp(current_version, latest_version.as_str()) as i8 != std::cmp::Ordering::Equal as i8 {
                        panic!("Current version does not match latest version from version migrations map declaration, please fix it.");
                    }
                    if const_str_cmp(current_version, LATEST.as_str()) as i8 != std::cmp::Ordering::Equal as i8 {
                        panic!("Current version does not match latest version const `LATEST` in meli::version_migrations, please fix it.");
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

/// Wrapper macro over [`decl_version_map`] that also defines the arguments as
/// modules.
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
    v0_8_9::V0_8_9_ID => v0_8_9::V0_8_9,
    v0_8_10::V0_8_10_ID => v0_8_10::V0_8_10,
    v0_8_11::V0_8_11_ID => v0_8_11::V0_8_11
}

use std::{
    cmp::Ordering,
    path::{Path, PathBuf},
};

use indexmap::{self, IndexMap};
use melib::{error::*, log};

use crate::{conf::FileSettings, terminal::Ask};

/// The latest version as defined in the Cargo manifest file of `meli`.
///
/// On compile-time if the `CARGO_PKG_VERSION` environment variable is
/// available, the macro [`decl_version_map`] asserts that it matches the actual
/// latest version string.
pub const LATEST: VersionIdentifier = v0_8_11::V0_8_11_ID;

/// An application version identifier with [Semantic Versioning v2.0.0]
/// semantics.
///
/// There's no support for "Build metadata" of the specification since we're not
/// using those.
///
/// [Semantic Versioning v2.0.0]: https://semver.org/spec/v2.0.0.html
#[derive(Clone, Copy, Debug, Eq)]
pub struct VersionIdentifier {
    string: &'static str,
    major: u8,
    minor: u8,
    patch: u8,
    pre: &'static str,
}

impl VersionIdentifier {
    /// An invalid non-existent release, `v0.0.0`, used for comparison with
    /// other identifiers.
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

/// Compare `(self.major, self.minor, self.patch, self.pre)` fields with another
/// [`VersionIdentifier`].
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

/// Compare `(self.major, self.minor, self.patch, self.pre)` fields with another
/// [`VersionIdentifier`].
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

/// Return the path to the `.version` file, a plain text file that contains the
/// version of meli that "owns" the configuration and data files.
///
/// The actual path examined is `${XDG_DATA_HOME}/meli/.version`.
pub fn version_file() -> Result<PathBuf> {
    let xdg_dirs = xdg::BaseDirectories::with_prefix("meli")?;
    Ok(xdg_dirs.place_data_file(".version")?)
}

/// Inspect current/previous version setup, perform migrations if necessary,
/// etc.
///
/// This function requires an interactive user session, if stdout is not an
/// interactive TTY, the process caller must ensure `stdin` contains the
/// necessary input (`y`, `n`, newline) otherwise this function _blocks_.
pub fn version_setup(
    config: &Path,
    writer: &mut impl std::io::Write,
    reader: &mut impl std::io::BufRead,
) -> Result<()> {
    let version_file = match version_file() {
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
            if prev.as_str() < LATEST.as_str() {
                writeln!(
                    writer,
                    "meli appears updated; file {} contains the value {:?} and the latest version \
                     is {}",
                    version_file.display(),
                    prev,
                    LATEST
                )?;
                writer.flush()?;
            } else {
                writeln!(
                    writer,
                    "This version of meli, {}, appears to be older than the previously used one \
                     stored in the file {}: {}.",
                    LATEST,
                    version_file.display(),
                    prev,
                )?;
                writeln!(
                    writer,
                    "Certain configuration options might not be compatible with this version, \
                     refer to release changelogs if you need to troubleshoot configuration \
                     options problems."
                )?;
                writer.flush()?;
                let ask = Ask::new(
                    "Update .version file to make this warning go away? (CAUTION: current \
                     configuration and stored data might not be compatible with this version!!)",
                )
                .yes_by_default(false);
                if ask.run(writer, reader) {
                    std::fs::write(&version_file, LATEST.as_str())
                        .chain_err_related_path(&version_file)?;
                    return Ok(());
                }
                return Ok(());
            }
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
            writeln!(
                writer,
                "meli appears updated; version file {} was not found and there are potential \
                 migrations to be made.",
                version_file.display()
            )?;
            writer.flush()?;
        }
        writeln!(
            writer,
            "You might need to migrate your configuration data for the new version to work.\nYou \
             can skip any changes you don't want to happen and you can quit at any time."
        )?;
        writeln!(
            writer,
            "{} migration{} {} about to be performed:",
            migrations.len(),
            if migrations.len() == 1 { "" } else { "s" },
            if migrations.len() == 1 { "is" } else { "are" }
        )?;
        for (vers, migrs) in &migrations {
            for m in migrs {
                writeln!(writer, "v{}/{}: {}", vers, m.id(), m.description())?;
            }
        }
        writer.flush()?;
        let ask = Ask::new(format!(
            "Perform {} migration{}?",
            migrations.len(),
            if migrations.len() == 1 { "" } else { "s" }
        ));
        if !ask.run(writer, reader) {
            let ask = Ask::new("Update .version file despite not attempting migrations?")
                .yes_by_default(false);
            if ask.run(writer, reader) {
                std::fs::write(&version_file, LATEST.as_str())
                    .chain_err_related_path(&version_file)?;
                return Ok(());
            }
            return Ok(());
        }
        let mut perform_history: Vec<Box<dyn Migration + 'static>> = vec![];
        for (vers, migrs) in migrations {
            writeln!(writer, "Updating to {}...", vers)?;
            writer.flush()?;
            'migrations: for m in migrs {
                let ask = Ask::new(m.question());
                if ask.run(writer, reader) {
                    if let Err(err) = m.perform(config, false, true) {
                        writeln!(writer, "\nCould not perform migration: {}", err)?;
                        writer.flush()?;
                        let ask = Ask::new("Continue?");
                        if ask.run(writer, reader) {
                            continue 'migrations;
                        }
                        if !perform_history.is_empty() {
                            let ask = Ask::new("Undo already performed migrations before exiting?")
                                .without_default();
                            if ask.run(writer, reader) {
                                while let Some(m) = perform_history.pop() {
                                    write!(writer, "Undoing {}...", m.id())?;
                                    writer.flush()?;
                                    if let Err(err) = m.revert(config, false, true) {
                                        writeln!(
                                            writer,
                                            " [ERROR] could not revert migration: {}",
                                            err
                                        )?;
                                    } else {
                                        writeln!(writer, " [OK]")?;
                                    }
                                    writer.flush()?;
                                }
                            }
                        }
                        return Ok(());
                    }
                    writeln!(writer, "v{}/{} [OK]", vers, m.id())?;
                    writer.flush()?;
                    perform_history.push(m);
                }
            }
        }
    }
    std::fs::write(&version_file, LATEST.as_str()).chain_err_related_path(&version_file)?;

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
