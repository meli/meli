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

use crate::version_migrations::*;

pub const V0_8_8_ID: VersionIdentifier = VersionIdentifier {
    string: "0.8.8",
    major: 0,
    minor: 8,
    patch: 8,
    pre: "",
};

#[derive(Clone, Copy, Debug)]
pub struct V0_8_8;

impl Version for V0_8_8 {
    fn version(&self) -> &VersionIdentifier {
        &V0_8_8_ID
    }

    fn migrations(&self) -> Vec<Box<dyn Migration + Send + Sync + 'static>> {
        vec![Box::new(AddressbookRename) as Box<dyn Migration + Send + Sync + 'static>]
    }
}

#[derive(Clone, Copy, Debug)]
struct AddressbookRename;

impl Migration for AddressbookRename {
    fn id(&self) -> &'static str {
        melib::identify! { AddressbookRename }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_8_8_ID
    }

    fn description(&self) -> &str {
        "The storage file for contacts, stored in the application's data folder, was renamed from \
         `addressbook` to `contacts` to better reflect its purpose."
    }

    fn question(&self) -> &str {
        "Rename ${XDG_DATA_HOME}/meli/*/addressbook files to ${XDG_DATA_HOME}/meli/*/contacts?"
    }

    fn is_applicable(&self, config: &Path) -> Option<bool> {
        if !config.try_exists().unwrap_or(false) {
            return Some(false);
        }
        let Ok(settings) = FileSettings::validate(config.to_path_buf(), false) else {
            return Some(false);
        };
        let mut any = false;
        for account in settings.accounts.keys() {
            let Ok(data_dir) = xdg::BaseDirectories::with_profile("meli", account) else {
                return Some(false);
            };
            if let Ok(addressbook) = data_dir.place_data_file("addressbook") {
                any |= addressbook.try_exists().unwrap_or(false);
            }
        }
        Some(any)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        let settings = FileSettings::validate(config.to_path_buf(), false)?;

        if !dry_run {
            self.perform(config, true, false)
                .chain_err_summary(|| "No files were renamed.")?;
        }
        for account in settings.accounts.keys() {
            let data_dir = xdg::BaseDirectories::with_profile("meli", account)?;
            if let (Ok(addressbook), Ok(contacts)) = (
                data_dir.place_data_file("addressbook"),
                data_dir.place_data_file("contacts"),
            ) {
                match (
                    addressbook.try_exists().unwrap_or(false),
                    contacts.try_exists().unwrap_or(false),
                ) {
                    (true, false) => {
                        if !dry_run {
                            std::fs::rename(&addressbook, &contacts)
                                .chain_err_related_path(&addressbook)?;
                        }
                        if verbose {
                            log::info!(
                                "Migration {}/{}: Renamed {} to {}.",
                                self.version().as_str(),
                                self.id(),
                                addressbook.display(),
                                contacts.display()
                            );
                        }
                    }
                    (true, true) => {
                        return Err(Error::new(format!(
                            "Cannot rename {} to {}: latter already exists.",
                            addressbook.display(),
                            contacts.display()
                        ))
                        .set_related_path(Some(&contacts))
                        .set_kind(ErrorKind::ALREADY_EXISTS));
                    }
                    (false, _) => {}
                }
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        let settings = FileSettings::validate(config.to_path_buf(), false)?;

        if !dry_run {
            self.revert(config, true, false)
                .chain_err_summary(|| "No files were renamed.")?;
        }
        for account in settings.accounts.keys() {
            let data_dir = xdg::BaseDirectories::with_profile("meli", account)?;
            if let (Ok(addressbook), Ok(contacts)) = (
                data_dir.place_data_file("addressbook"),
                data_dir.place_data_file("contacts"),
            ) {
                match (
                    contacts.try_exists().unwrap_or(false),
                    addressbook.try_exists().unwrap_or(false),
                ) {
                    (true, false) => {
                        if !dry_run {
                            std::fs::rename(&contacts, &addressbook)
                                .chain_err_related_path(&contacts)?;
                        }
                        if verbose {
                            log::info!(
                                "Reverted migration {}/{}: Renamed {} to {}.",
                                self.version().as_str(),
                                self.id(),
                                contacts.display(),
                                addressbook.display(),
                            );
                        }
                    }
                    (true, true) => {
                        return Err(Error::new(format!(
                            "Cannot rename {} to {}: latter already exists.",
                            contacts.display(),
                            addressbook.display(),
                        ))
                        .set_related_path(Some(&addressbook))
                        .set_kind(ErrorKind::ALREADY_EXISTS));
                    }
                    (false, _) => {}
                }
            }
        }
        Ok(())
    }
}
