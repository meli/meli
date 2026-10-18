//
// meli
//
// Copyright 2026 Manos Pitsidianakis <manos@pitsidianak.is>
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

//! <https://release.meli-email.org/v0.9.0>

use crate::conf::preprocessing::get_included_configs;
use crate::version_migrations::*;

use melib::{
    contacts::{CardId, JSContactV1},
    UnixTimestamp,
};
use toml_edit::DocumentMut;

/// <https://release.meli-email.org/v0.9.0>
pub const V0_9_0_ID: VersionIdentifier = VersionIdentifier {
    string: "0.9.0",
    major: 0,
    minor: 9,
    patch: 0,
    pre: "",
};

/// <https://release.meli-email.org/v0.9.0>
#[derive(Clone, Copy, Debug)]
pub struct V0_9_0;

impl Version for V0_9_0 {
    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn migrations(&self) -> Vec<Box<dyn Migration + Send + Sync + 'static>> {
        vec![
            Box::new(ServerPasswordCommand) as Box<dyn Migration + Send + Sync + 'static>,
            Box::new(AddressbookRefactor) as Box<dyn Migration + Send + Sync + 'static>,
            Box::new(ContactBackendRefactor) as Box<dyn Migration + Send + Sync + 'static>,
            Box::new(JSContactsConversion) as Box<dyn Migration + Send + Sync + 'static>,
            Box::new(SmtpAuthPasswordCommand) as Box<dyn Migration + Send + Sync + 'static>,
            Box::new(SmtpAuthTokenCommand) as Box<dyn Migration + Send + Sync + 'static>,
        ]
    }
}

#[derive(Deserialize, Serialize)]
struct Card {
    id: String,
    title: String,
    name: String,
    additionalname: String,
    name_prefix: String,
    name_suffix: String,
    birthday: Option<UnixTimestamp>,
    email: String,
    url: String,
    key: String,
    color: u8,
    last_edited: UnixTimestamp,
    extra_properties: IndexMap<String, String>,
    external_resource: bool,
}

impl From<JSContactV1> for Card {
    fn from(val: JSContactV1) -> Self {
        Self {
            id: val.uid.into(),
            name: val.name().unwrap_or_default().into_owned(),
            email: val.email().unwrap_or_default().to_owned(),
            url: val.url().unwrap_or_default().to_owned(),
            key: val
                .crypto_keys
                .first()
                .map(|(_, v)| v.resource.uri.clone())
                .unwrap_or_default(),
            birthday: val.birthday(),
            last_edited: val.updated.0,
            title: String::new(),
            additionalname: String::new(),
            name_prefix: String::new(),
            name_suffix: String::new(),
            color: 0,
            extra_properties: IndexMap::default(),
            external_resource: false,
        }
    }
}

impl From<Card> for JSContactV1 {
    fn from(card: Card) -> Self {
        use json_types::UTCDateTime;
        use melib::contacts::jscontact::*;
        use uuid::Uuid;

        let Card {
            id,
            title,
            name,
            additionalname,
            name_prefix,
            name_suffix,
            birthday,
            email,
            url,
            key,
            last_edited,
            extra_properties,
            external_resource: _,
            color: _,
        } = card;

        let mut ret = Self(
            json_types::Card::default(),
            std::marker::PhantomData::<fn() -> JSContactVersion1>,
        );
        ret.0.updated = UTCDateTime(last_edited);
        ret.0.uid = if let Ok(uid) = Uuid::try_parse(&id) {
            uid
        } else {
            Uuid::new_v5(&Uuid::NAMESPACE_OID, id.as_bytes())
        };
        let need_fullname = title.is_empty()
            && additionalname.is_empty()
            && name_prefix.is_empty()
            && name_suffix.is_empty();
        if need_fullname {
            if !name.is_empty() {
                ret.0.name.full = Some(name);
            }
        } else {
            use std::collections::VecDeque;

            use json_types::{NameComponent, NameComponentKind::*};

            let given;
            let mut given2 = None;
            let mut surname = None;
            let mut surname2 = None;
            let mut rest = None;

            let words_no = name.chars().filter(|c| c.is_whitespace()).count() + 1;

            if words_no == 1 {
                given = Some(NameComponent::new(name, Given));
            } else {
                let mut words = name
                    .split_whitespace()
                    .map(|s| s.to_owned())
                    .collect::<VecDeque<_>>();
                given = words
                    .pop_front()
                    .map(|value| NameComponent::new(value, Given));
                if words.len() > 1 {
                    given2 = words
                        .pop_front()
                        .map(|value| NameComponent::new(value, Given2));
                }
                if words.len() > 1 {
                    surname = words
                        .pop_front()
                        .map(|value| NameComponent::new(value, Surname));
                }
                surname2 = words
                    .pop_front()
                    .map(|value| NameComponent::new(value, Surname2));
                if !words.is_empty() {
                    rest = Some(
                        words
                            .into_iter()
                            .map(|value| NameComponent::new(value, Surname2))
                            .collect::<Vec<_>>(),
                    );
                }
            }
            if !title.is_empty() {
                ret.0.name.components.push(NameComponent::new(title, Title));
            }
            if !name_prefix.is_empty() {
                ret.0
                    .name
                    .components
                    .push(NameComponent::new(name_prefix, Title));
            }
            if let Some(given) = given {
                ret.0.name.components.push(given);
            }
            if !additionalname.is_empty() {
                ret.0
                    .name
                    .components
                    .push(NameComponent::new(additionalname, Given2));
            }
            if let Some(given2) = given2 {
                ret.0.name.components.push(given2);
            }
            if let Some(surname) = surname {
                ret.0.name.components.push(surname);
            }
            if let Some(surname2) = surname2 {
                ret.0.name.components.push(surname2);
            }
            if let Some(rest) = rest {
                ret.0.name.components.extend(rest);
            }
            if !name_suffix.is_empty() {
                // Could also be `Generation` but the best we could do to detect that is heuristics
                ret.0
                    .name
                    .components
                    .push(NameComponent::new(name_suffix, Credential));
            }
        }
        if let Some(birthday) = birthday {
            use json_types::{Anniversary, AnniversaryDate, AnniversaryKind, Timestamp};

            ret.0.anniversaries.insert(
                "k0".into(),
                Anniversary::new(
                    AnniversaryKind::Birth,
                    AnniversaryDate::Timestamp(Timestamp {
                        __type: None,
                        utc: UTCDateTime(birthday),
                        extra: Default::default(),
                    }),
                ),
            );
        }
        if !url.is_empty() {
            use json_types::OnlineService;

            ret.0.online_services.insert(
                "k0".into(),
                OnlineService {
                    uri: url,
                    ..OnlineService::default()
                },
            );
        }
        if !key.is_empty() {
            use json_types::{CryptoKey, Resource};

            ret.0.crypto_keys.insert(
                "k0".into(),
                CryptoKey {
                    __type: None,
                    resource: Resource {
                        uri: key,
                        ..Resource::default()
                    },
                },
            );
        }
        if !extra_properties.is_empty() {
            ret.0.extra.extend(
                extra_properties
                    .into_iter()
                    .map(|(s, v)| (s, serde_json::Value::String(v))),
            );
        }
        ret.0
            .emails
            .insert("main".into(), json_types::EmailAddress::new(email));

        ret
    }
}

/// Transform `server_password_command` to `server_password` secret
#[derive(Clone, Copy, Debug)]
struct ServerPasswordCommand;

impl ServerPasswordCommand {
    fn transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    return Err(Error::new(format!("invalid account value, got: {acc:?}")));
                };
                let mut new_entry = toml_edit::InlineTable::new();
                if let Some(pass) = acc.get_mut("server_password_command") {
                    let pass = pass.as_str().ok_or_else(|| {
                        format!(
                            "Expected string value for server_password_command field, got: {pass}"
                        )
                    })?;
                    new_entry.insert("command", pass.into());
                }
                if let Some(prev) = acc.remove("server_password_command") {
                    let new = toml_edit::Item::Value(toml_edit::Value::from(new_entry));
                    if verbose {
                        log::info!("Account {acc_name}: converting server_password_command = {prev} to server_password = {new}.");
                    }
                    acc.insert("server_password", new);
                }
            }
        }
        Ok(())
    }

    fn revert_transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    return Err(Error::new(format!("invalid accounts value, got: {acc:?}")));
                };
                if let Some(pass) = acc.get("server_password") {
                    let Some(pass_table) = pass.as_table_like() else {
                        continue;
                    };
                    let command = pass_table
                        .get("command")
                        .ok_or_else(|| {
                            format!("Expected server_password command field, got: {pass:?}")
                        })?
                        .as_str()
                        .ok_or_else(|| {
                            format!(
                            "Expected string value for server_password command field, got: {pass:?}"
                        )
                        })?
                        .to_string();
                    if let Some(prev) = acc.remove("server_password") {
                        let new = toml_edit::Item::Value(toml_edit::Value::from(command));
                        if verbose {
                            log::info!("Account {acc_name}: converting server_password = {prev} to server_password_command = {new}.");
                        }
                        acc.insert("server_password_command", new);
                    }
                }
            }
        }
        Ok(())
    }
}

impl Migration for ServerPasswordCommand {
    fn id(&self) -> &'static str {
        melib::identify! { ServerPasswordCommand }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn description(&self) -> &str {
        "Transform `server_password_command` to new syntax: `server_password = { command = \"...\" }`"
    }

    fn question(&self) -> &str {
        "Transform `server_password_command` to new syntax?"
    }

    fn is_applicable(&self, config: &Path) -> Option<bool> {
        for c in get_included_configs(config).ok()? {
            let raw = std::fs::read_to_string(&c).ok()?;
            if raw.contains("server_password_command") {
                return Some(true);
            }
        }

        Some(false)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                continue;
            };

            Self::transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration revert was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                continue;
            };

            Self::revert_transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }
}

/// Change `contacts` serialization format
///
/// "The storage file for contacts, stored in the application's data folder, changed format."
#[derive(Clone, Copy, Debug)]
struct AddressbookRefactor;

impl AddressbookRefactor {
    fn create_backup_file(path: &Path, contents: &str, extension: &str) -> Result<PathBuf> {
        let mut path = path.to_path_buf();
        path.set_extension(extension);
        let mut i: u8 = 0;
        while path.try_exists().unwrap_or(false) {
            if let Ok(c) = std::fs::read_to_string(&path) {
                if c == contents {
                    break;
                }
            }
            if i == u8::MAX {
                path.set_extension("");
                return Err(Error::new(format!("Something has gone horribly wrong, and there are too many (over {}) backup files of path {}. Please cleanup the folder and try again. meli is aborting out of caution.", u8::MAX, path.display())).set_related_path(Some(path)));
            }
            i += 1;
            path.set_extension(format!("{extension}{i}"));
        }
        Ok(path)
    }

    fn convert_contacts(account_name: &str, contents: &str, revert: bool) -> Result<String> {
        #[derive(Deserialize, Serialize)]
        struct PreviousContacts {
            display_name: String,
            cards: IndexMap<String, Card>,
        }
        let retval = if revert {
            let cards: IndexMap<String, Card> = match serde_json::from_str(contents) {
                Ok(v) => v,
                Err(err) => {
                    if serde_json::from_str::<PreviousContacts>(contents).is_ok() {
                        return Ok(contents.to_string());
                    }
                    return Err(err.into());
                }
            };
            let previous = PreviousContacts {
                display_name: account_name.to_string(),
                cards,
            };
            serde_json::to_string(&previous)?
        } else {
            let previous: PreviousContacts = match serde_json::from_str(contents) {
                Ok(v) => v,
                Err(err) => {
                    if serde_json::from_str::<IndexMap<String, Card>>(contents).is_ok() {
                        return Ok(contents.to_string());
                    }
                    return Err(err.into());
                }
            };
            serde_json::to_string(&previous.cards)?
        };

        Ok(retval)
    }
}

impl Migration for AddressbookRefactor {
    fn id(&self) -> &'static str {
        melib::identify! { AddressbookRefactor }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn description(&self) -> &str {
        "The storage file for contacts, stored in the application's data folder, changed format."
    }

    fn question(&self) -> &str {
        "Convert ${XDG_DATA_HOME}/meli/*/contacts files to new format? (you might want to backup manually before you do this; a backup will be created by default in the same folder with a .bkp suffixed filename)"
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
            if let Ok(contacts) = data_dir.place_data_file("contacts") {
                any |= contacts.try_exists().unwrap_or(false);
            }
        }
        Some(any)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        let settings = FileSettings::validate(config.to_path_buf(), false)?;

        if !dry_run {
            self.perform(config, true, false)
                .chain_err_summary(|| "No files were converted.")?;
        }
        for account in settings.accounts.keys() {
            let data_dir = xdg::BaseDirectories::with_profile("meli", account)?;
            if let Ok(contacts) = data_dir.place_data_file("contacts") {
                if !contacts.try_exists().unwrap_or(false) {
                    continue;
                }
                let contents =
                    std::fs::read_to_string(&contacts).chain_err_related_path(&contacts)?;
                let new = Self::convert_contacts(account.as_str(), &contents, false)
                    .chain_err_related_path(&contacts)?;
                if !dry_run {
                    let bkp_filepath = Self::create_backup_file(&contacts, &contents, "bkp")?;
                    std::fs::write(&bkp_filepath, &contents)
                        .chain_err_related_path(&bkp_filepath)?;
                    if verbose {
                        log::info!(
                            "Migration {}/{}: {} is backed up at {}.",
                            self.version().as_str(),
                            self.id(),
                            contacts.display(),
                            bkp_filepath.display()
                        );
                    }
                    std::fs::write(&contacts, &new).chain_err_related_path(&contacts)?;
                }
                if verbose {
                    log::info!(
                        "Migration {}/{}: Converted {}.",
                        self.version().as_str(),
                        self.id(),
                        contacts.display()
                    );
                }
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        let settings = FileSettings::validate(config.to_path_buf(), false)?;

        if !dry_run {
            self.revert(config, true, false)
                .chain_err_summary(|| "No files were converted.")?;
        }
        for account in settings.accounts.keys() {
            let data_dir = xdg::BaseDirectories::with_profile("meli", account)?;
            if let Ok(contacts) = data_dir.place_data_file("contacts") {
                if !contacts.try_exists().unwrap_or(false) {
                    continue;
                }
                let contents =
                    std::fs::read_to_string(&contacts).chain_err_related_path(&contacts)?;
                let new = Self::convert_contacts(account.as_str(), &contents, true)
                    .chain_err_related_path(&contacts)?;
                if !dry_run {
                    std::fs::write(&contacts, &new).chain_err_related_path(&contacts)?;
                }
                if verbose {
                    log::info!(
                            "Reverted migration {}/{}: Converted {}. You might wish to cleanup backed up files in the directory or use them instead.",
                            self.version().as_str(),
                            self.id(),
                            contacts.display()
                        );
                }
            }
        }
        Ok(())
    }
}

/// Add contact backends configuration value
///
/// `notmuch_address_book_query`, `mutt_alias_file`, `vcard_folder` are moved into a single
/// `contacts` configuration value.
#[derive(Clone, Copy, Debug)]
struct ContactBackendRefactor;

impl ContactBackendRefactor {
    fn transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        use melib::conf::ContactBackendConf;

        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    return Err(Error::new(format!("invalid account value, got: {acc:?}")));
                };
                let mut contacts = toml_edit::InlineTable::new();
                for (t, f) in [
                    (
                        ContactBackendConf::NotmuchAddress as fn(String) -> ContactBackendConf,
                        "notmuch_address_book_query",
                    ),
                    (ContactBackendConf::MuttAlias, "mutt_alias_file"),
                    (ContactBackendConf::VCard, "vcard_folder"),
                ] {
                    if let Some(val) = acc.get_mut(f) {
                        let val = val.as_str().ok_or_else(|| {
                            format!("Expected string value for {f} field, got: {val}")
                        })?;
                        let val = t(val.to_string());
                        let val = serde::Serialize::serialize(
                            &val,
                            toml_edit::ser::ValueSerializer::new(),
                        )
                        .expect("serialize ContactBackendConf");
                        contacts.insert(f, val);
                        acc.remove(f);
                    }
                }
                if !contacts.is_empty() {
                    let new = toml_edit::Item::Value(toml_edit::Value::InlineTable(contacts));
                    if verbose {
                        log::info!("Account {acc_name}: adding contacts = {new}.");
                    }
                    acc.insert("contacts", new);
                }
            }
        }
        Ok(())
    }

    fn revert_transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        use melib::conf::ContactBackendConf;

        use serde::de::IntoDeserializer as _;

        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    return Err(Error::new(format!("invalid accounts value, got: {acc:?}")));
                };
                if let Some(contacts) = acc.get_mut("contacts") {
                    let Some(contacts) = contacts.as_table_like_mut() else {
                        continue;
                    };
                    let mut notmuch_address_book_query: Option<String> = None;
                    let mut mutt_alias_file = None;
                    let mut vcard_folder = None;
                    for (opt, variant, f) in [
                        (
                            &mut notmuch_address_book_query,
                            ContactBackendConf::NotmuchAddress(String::new()),
                            "notmuch_address_book_query",
                        ),
                        (
                            &mut mutt_alias_file,
                            ContactBackendConf::MuttAlias(String::new()),
                            "mutt_alias_file",
                        ),
                        (
                            &mut vcard_folder,
                            ContactBackendConf::VCard(String::new()),
                            "vcard_folder",
                        ),
                    ] {
                        let Some(prev) = contacts.remove(f) else {
                            continue;
                        };
                        let toml_edit::Item::Value(prev) = prev else {
                            return Err(Error::new(format!(
                                "{acc_name}: invalid {f} value, got: {prev:?}"
                            )));
                        };
                        let prev = <ContactBackendConf as serde::Deserialize>::deserialize(
                            prev.into_deserializer(),
                        )
                        .unwrap();

                        let val = match (prev, variant) {
                            (
                                ContactBackendConf::MuttAlias(val),
                                ContactBackendConf::MuttAlias(_),
                            )
                            | (ContactBackendConf::VCard(val), ContactBackendConf::VCard(_))
                            | (
                                ContactBackendConf::NotmuchAddress(val),
                                ContactBackendConf::NotmuchAddress(_),
                            ) => val,
                            (prev, _) => {
                                return Err(Error::new(format!(
                                    "{acc_name}: invalid {f} value, got: {prev:?}"
                                )));
                            }
                        };
                        *opt = Some(val);
                    }
                    if !contacts.is_empty() {
                        return Err(Error::new(format!(
                                    "{acc_name}: could not revert migration because we would have to remove these unmigrateable items: {:?}", acc.get("contacts").unwrap().to_string()
                        )));
                    }
                    acc.remove("contacts");
                    if let Some(n) = notmuch_address_book_query {
                        let new = toml_edit::Item::Value(toml_edit::Value::from(n));
                        if verbose {
                            log::info!(
                                "Account {acc_name}: adding notmuch_address_book_query = {new}."
                            );
                        }
                        acc.insert("notmuch_address_book_query", new);
                    }
                    if let Some(n) = mutt_alias_file {
                        let new = toml_edit::Item::Value(toml_edit::Value::from(n));
                        if verbose {
                            log::info!("Account {acc_name}: adding mutt_alias_file = {new}.");
                        }
                        acc.insert("mutt_alias_file", new);
                    }
                    if let Some(n) = vcard_folder {
                        let new = toml_edit::Item::Value(toml_edit::Value::from(n));
                        if verbose {
                            log::info!("Account {acc_name}: adding vcard_folder = {new}.");
                        }
                        acc.insert("vcard_folder", new);
                    }
                }
            }
        }
        Ok(())
    }
}

impl Migration for ContactBackendRefactor {
    fn id(&self) -> &'static str {
        melib::identify! { ContactBackendRefactor }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn description(&self) -> &str {
        "`notmuch_address_book_query`, `mutt_alias_file`, `vcard_folder` are moved into a single `contacts` configuration value"
    }

    fn question(&self) -> &str {
        "Consolidate `notmuch_address_book_query`, `mutt_alias_file`, `vcard_folder` options into the new `contacts` field?"
    }

    fn is_applicable(&self, config: &Path) -> Option<bool> {
        if !config.try_exists().unwrap_or(false) {
            return Some(false);
        }
        for c in get_included_configs(config).ok()? {
            let raw = std::fs::read_to_string(&c).ok()?;
            if raw.contains("notmuch_address_book_query")
                || raw.contains("mutt_alias_file")
                || raw.contains("vcard_folder")
            {
                return Some(true);
            }
        }
        Some(false)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                if verbose {
                    log::warn!("Could not parse {} as valid TOML, skipping.", c.display());
                }
                continue;
            };

            Self::transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration revert was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                if verbose {
                    log::warn!("Could not parse {} as valid TOML, skipping.", c.display());
                }
                continue;
            };

            Self::revert_transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }
}

/// Change `contacts` serialization format
///
/// "The storage file for contacts, stored in the application's data folder, changed format."
#[derive(Clone, Copy, Debug)]
struct JSContactsConversion;

impl JSContactsConversion {
    fn create_backup_file(path: &Path, contents: &str, extension: &str) -> Result<PathBuf> {
        let mut path = path.to_path_buf();
        path.set_extension(extension);
        let mut i: u8 = 0;
        while path.try_exists().unwrap_or(false) {
            if let Ok(c) = std::fs::read_to_string(&path) {
                if c == contents {
                    break;
                }
            }
            if i == u8::MAX {
                path.set_extension("");
                return Err(Error::new(format!("Something has gone horribly wrong, and there are too many (over {}) backup files of path {}. Please cleanup the folder and try again. meli is aborting out of caution.", u8::MAX, path.display())).set_related_path(Some(path)));
            }
            i += 1;
            path.set_extension(format!("{extension}{i}"));
        }
        Ok(path)
    }

    fn convert_contacts(contents: &str, revert: bool) -> Result<String> {
        let retval = if revert {
            let previous: IndexMap<CardId, JSContactV1> = match serde_json::from_str(contents) {
                Ok(v) => v,
                Err(err) => {
                    if serde_json::from_str::<IndexMap<String, Card>>(contents).is_ok() {
                        return Ok(contents.to_string());
                    }
                    return Err(err.into());
                }
            };
            let cards: IndexMap<String, Card> = previous
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect();
            serde_json::to_string(&cards)?
        } else {
            let cards: IndexMap<String, Card> = match serde_json::from_str(contents) {
                Ok(v) => v,
                Err(err) => {
                    if serde_json::from_str::<IndexMap<CardId, JSContactV1>>(contents).is_ok() {
                        return Ok(contents.to_string());
                    }
                    return Err(err.into());
                }
            };
            let new: IndexMap<CardId, JSContactV1> = cards
                .into_iter()
                .map(|(_, v)| {
                    let v: JSContactV1 = v.into();
                    (v.id(), v)
                })
                .collect();
            serde_json::to_string(&new)?
        };

        Ok(retval)
    }
}

impl Migration for JSContactsConversion {
    fn id(&self) -> &'static str {
        melib::identify! { JSContactsConversion }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn description(&self) -> &str {
        "The storage file for contacts, stored in the application's data folder, changed format."
    }

    fn question(&self) -> &str {
        "Convert ${XDG_DATA_HOME}/meli/*/contacts files to new format? (you might want to backup manually before you do this; a backup will be created by default in the same folder with a .bkp suffixed filename)"
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
            if let Ok(contacts) = data_dir.place_data_file("contacts") {
                any |= contacts.try_exists().unwrap_or(false);
            }
        }
        Some(any)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        let settings = FileSettings::validate(config.to_path_buf(), false)?;

        if !dry_run {
            self.perform(config, true, false)
                .chain_err_summary(|| "No files were converted.")?;
        }
        for account in settings.accounts.keys() {
            let data_dir = xdg::BaseDirectories::with_profile("meli", account)?;
            if let Ok(contacts) = data_dir.place_data_file("contacts") {
                if !contacts.try_exists().unwrap_or(false) {
                    continue;
                }
                let contents =
                    std::fs::read_to_string(&contacts).chain_err_related_path(&contacts)?;
                let new =
                    Self::convert_contacts(&contents, false).chain_err_related_path(&contacts)?;
                if !dry_run {
                    let bkp_filepath = Self::create_backup_file(&contacts, &contents, "bkp")?;
                    std::fs::write(&bkp_filepath, &contents)
                        .chain_err_related_path(&bkp_filepath)?;
                    if verbose {
                        log::info!(
                            "Migration {}/{}: {} is backed up at {}.",
                            self.version().as_str(),
                            self.id(),
                            contacts.display(),
                            bkp_filepath.display()
                        );
                    }
                    std::fs::write(&contacts, &new).chain_err_related_path(&contacts)?;
                }
                if verbose {
                    log::info!(
                        "Migration {}/{}: Converted {}.",
                        self.version().as_str(),
                        self.id(),
                        contacts.display()
                    );
                }
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        let settings = FileSettings::validate(config.to_path_buf(), false)?;

        if !dry_run {
            self.revert(config, true, false)
                .chain_err_summary(|| "No files were converted.")?;
        }
        for account in settings.accounts.keys() {
            let data_dir = xdg::BaseDirectories::with_profile("meli", account)?;
            if let Ok(contacts) = data_dir.place_data_file("contacts") {
                if !contacts.try_exists().unwrap_or(false) {
                    continue;
                }
                let contents =
                    std::fs::read_to_string(&contacts).chain_err_related_path(&contacts)?;
                let new =
                    Self::convert_contacts(&contents, true).chain_err_related_path(&contacts)?;
                if !dry_run {
                    std::fs::write(&contacts, &new).chain_err_related_path(&contacts)?;
                }
                if verbose {
                    log::info!(
                            "Reverted migration {}/{}: Converted {}. You might wish to cleanup backed up files in the directory or use them instead.",
                            self.version().as_str(),
                            self.id(),
                            contacts.display()
                        );
                }
            }
        }
        Ok(())
    }
}

/// Transform `send_mail = { auth = { type = "auto", password = { type = "raw", value = "..." } } }` to Secret
#[derive(Clone, Copy, Debug)]
struct SmtpAuthPasswordCommand;

impl SmtpAuthPasswordCommand {
    fn transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    continue;
                };
                let Some(send_mail) = acc.get_mut("send_mail") else {
                    continue;
                };
                let Some(send_mail_table) = send_mail.as_table_like_mut() else {
                    continue;
                };
                let Some(auth) = send_mail_table.get_mut("auth") else {
                    continue;
                };
                let Some(auth_table_like) = auth.as_table_like_mut() else {
                    continue;
                };
                let Some(password) = auth_table_like.get_mut("password") else {
                    continue;
                };
                let Some(type_) = password.get("type") else {
                    continue;
                };
                if matches!(type_.as_str(), Some(raw) if raw.eq_ignore_ascii_case("raw")) {
                    let Some(value) = password
                        .get("value")
                        .and_then(|v| v.as_str())
                        .map(|v| v.to_owned())
                    else {
                        continue;
                    };
                    if let Some(prev) = auth_table_like.remove("password") {
                        let new = toml_edit::Item::Value(toml_edit::Value::from(value));
                        if verbose {
                            log::info!("Account {acc_name}: converting send_mail.auth.password = {prev} to send_mail.auth.password = {new}.");
                        }
                        auth_table_like.insert("password", new);
                    }
                } else if matches!(type_.as_str(), Some(raw) if raw.eq_ignore_ascii_case("command_eval") || raw.eq_ignore_ascii_case("command_evaluation") || raw.eq_ignore_ascii_case("CommandEval"))
                {
                    let Some(value) = password
                        .get("value")
                        .and_then(|v| v.as_str())
                        .map(|v| v.to_owned())
                    else {
                        continue;
                    };
                    if let Some(prev) = auth_table_like.remove("password") {
                        let mut new_entry = toml_edit::InlineTable::new();
                        new_entry.insert("command", toml_edit::Value::from(value));
                        let new = toml_edit::Item::Value(toml_edit::Value::from(new_entry));
                        if verbose {
                            log::info!("Account {acc_name}: converting send_mail.auth.password = {prev} to send_mail.auth.password = {new}.");
                        }
                        auth_table_like.insert("password", new);
                    }
                }
            }
        }
        Ok(())
    }

    fn revert_transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    continue;
                };
                let Some(send_mail) = acc.get_mut("send_mail") else {
                    continue;
                };
                let Some(send_mail_table) = send_mail.as_table_like_mut() else {
                    continue;
                };
                let Some(auth) = send_mail_table.get_mut("auth") else {
                    continue;
                };
                let Some(auth_table_like) = auth.as_table_like_mut() else {
                    continue;
                };
                let Some(password) = auth_table_like.get_mut("password") else {
                    continue;
                };
                if let Some(value) = password.as_str().map(|v| v.to_owned()) {
                    if let Some(prev) = auth_table_like.remove("password") {
                        let mut new = toml_edit::InlineTable::new();
                        new.insert("type", "raw".into());
                        new.insert("value", toml_edit::Value::from(value));
                        let new = toml_edit::Item::Value(toml_edit::Value::InlineTable(new));
                        if verbose {
                            log::info!("Account {acc_name}: reverting send_mail.auth.password = {prev} to send_mail.auth.password = {new}.");
                        }
                        auth_table_like.insert("password", new);
                    }
                } else if let Some(pass_table) = password.as_table_like() {
                    if let Some(command) = pass_table
                        .get("command")
                        .and_then(|v| v.as_str())
                        .map(|v| v.to_owned())
                    {
                        if let Some(prev) = auth_table_like.remove("password") {
                            let mut new = toml_edit::InlineTable::new();
                            new.insert("type", "command_evaluation".into());
                            new.insert("value", toml_edit::Value::from(command));
                            let new = toml_edit::Item::Value(toml_edit::Value::InlineTable(new));
                            if verbose {
                                log::info!("Account {acc_name}: reverting send_mail.auth.password = {prev} to send_mail.auth.password = {new}.");
                            }
                            auth_table_like.insert("password", new);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn is_applicable(doc: &DocumentMut) -> Result<bool> {
        if let Some(accs) = doc.get("accounts") {
            let Some(accs) = accs.as_table() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (_, acc) in accs.iter() {
                let Some(acc) = acc.as_table_like() else {
                    continue;
                };
                let Some(send_mail) = acc.get("send_mail") else {
                    continue;
                };
                let Some(send_mail_table) = send_mail.as_table_like() else {
                    continue;
                };
                let Some(auth) = send_mail_table.get("auth") else {
                    continue;
                };
                let Some(auth_table_like) = auth.as_table_like() else {
                    continue;
                };
                let Some(password) = auth_table_like.get("password") else {
                    continue;
                };
                if password.get("type").is_some() {
                    return Ok(true);
                };
                continue;
            }
        }
        Ok(false)
    }
}

impl Migration for SmtpAuthPasswordCommand {
    fn id(&self) -> &'static str {
        melib::identify! { SmtpAuthPasswordCommand }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn description(&self) -> &str {
        "Transform `send_mail = { password = { type = \"raw\", value = \"...\" } }` to new syntax: `send_mail = { password = \"...\" }`"
    }

    fn question(&self) -> &str {
        "Transform `send_mail = { password = { type = \"raw\", value = \"...\" } }` to new syntax?"
    }

    fn is_applicable(&self, config: &Path) -> Option<bool> {
        for c in get_included_configs(config).ok()? {
            let raw = std::fs::read_to_string(&c).ok()?;
            let Ok(doc) = raw.parse::<DocumentMut>() else {
                continue;
            };
            let Ok(applicable) = Self::is_applicable(&doc) else {
                continue;
            };
            if applicable {
                return Some(true);
            }
        }

        Some(false)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                continue;
            };

            Self::transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration revert was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                continue;
            };

            Self::revert_transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }
}

/// Transform `send_mail = { auth = { type = "xoauth2", token_command = "..." } }` to Secret
#[derive(Clone, Copy, Debug)]
struct SmtpAuthTokenCommand;

impl SmtpAuthTokenCommand {
    fn transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    continue;
                };
                let Some(send_mail) = acc.get_mut("send_mail") else {
                    continue;
                };
                let Some(send_mail_table) = send_mail.as_table_like_mut() else {
                    continue;
                };
                let Some(auth) = send_mail_table.get_mut("auth") else {
                    continue;
                };
                let Some(auth_table_like) = auth.as_table_like_mut() else {
                    continue;
                };
                let Some(value) = auth_table_like
                    .get("token_command")
                    .and_then(|v| v.as_str())
                    .map(|v| v.to_owned())
                else {
                    continue;
                };
                if let Some(prev) = auth_table_like.remove("token_command") {
                    let mut new_entry = toml_edit::InlineTable::new();
                    new_entry.insert("command", toml_edit::Value::from(value));
                    let new = toml_edit::Item::Value(toml_edit::Value::from(new_entry));
                    if verbose {
                        log::info!("Account {acc_name}: converting send_mail.auth.token_command = {prev} to send_mail.auth.token = {new}.");
                    }
                    auth_table_like.insert("token", new);
                }
            }
        }
        Ok(())
    }

    fn revert_transform(doc: &mut DocumentMut, verbose: bool) -> Result<()> {
        if let Some(accs) = doc.get_mut("accounts") {
            let Some(accs) = accs.as_table_mut() else {
                return Err(Error::new(format!("invalid accounts value, got: {accs:?}")));
            };
            for (acc_name, acc) in accs.iter_mut() {
                let Some(acc) = acc.as_table_like_mut() else {
                    continue;
                };
                let Some(send_mail) = acc.get_mut("send_mail") else {
                    continue;
                };
                let Some(send_mail_table) = send_mail.as_table_like_mut() else {
                    continue;
                };
                let Some(auth) = send_mail_table.get_mut("auth") else {
                    continue;
                };
                let Some(auth_table_like) = auth.as_table_like_mut() else {
                    continue;
                };
                let Some(value) = auth_table_like
                    .get("token")
                    .and_then(|v| v.as_table_like())
                    .and_then(|v| v.get("command"))
                    .and_then(|v| v.as_str())
                    .map(|v| v.to_owned())
                else {
                    continue;
                };
                if let Some(prev) = auth_table_like.remove("token") {
                    let new = toml_edit::Item::Value(toml_edit::Value::from(value));
                    if verbose {
                        log::info!("Account {acc_name}: reverting send_mail.auth.token = {prev} to send_mail.auth.token_command = {new}.");
                    }
                    auth_table_like.insert("token_command", new);
                }
            }
        }
        Ok(())
    }
}

impl Migration for SmtpAuthTokenCommand {
    fn id(&self) -> &'static str {
        melib::identify! { SmtpAuthTokenCommand }
    }

    fn version(&self) -> &VersionIdentifier {
        &V0_9_0_ID
    }

    fn description(&self) -> &str {
        "Transform `send_mail = { auth = { type = \"xoauth2\", token_command = \"...\" } }` to Secret"
    }

    fn question(&self) -> &str {
        "Transform `send_mail = { auth = { type = \"xoauth2\", token_command = \"...\" } }` to new syntax?"
    }

    fn is_applicable(&self, config: &Path) -> Option<bool> {
        for c in get_included_configs(config).ok()? {
            let raw = std::fs::read_to_string(&c).ok()?;
            if raw.contains("xoauth2") || raw.contains("XOAuth2") {
                return Some(true);
            }
        }

        Some(false)
    }

    fn perform(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                continue;
            };

            Self::transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }

    fn revert(&self, config: &Path, dry_run: bool, verbose: bool) -> Result<()> {
        for c in get_included_configs(config)? {
            if !dry_run {
                self.perform(config, true, false)
                    .chain_err_summary(|| "No migration revert was performed.")?;
            }
            let raw = std::fs::read_to_string(&c).chain_err_related_path(&c)?;
            let Ok(mut doc) = raw.parse::<DocumentMut>() else {
                continue;
            };

            Self::revert_transform(&mut doc, verbose).chain_err_related_path(&c)?;

            if !dry_run {
                std::fs::write(&c, doc.to_string()).chain_err_related_path(&c)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusty_fork::rusty_fork_test;

    use melib::utils::logging::{LogLevel, Logger};

    rusty_fork_test! {
            #[test]
            fn test_version_migration_server_password_command() {
                run_version_migration_server_password_command()
            }

            #[test]
            fn test_contacts_refactor() {
                run_contacts_refactor()
            }

            #[test]
            fn test_contact_backend_refactor() {
                run_contact_backend_refactor()
            }

        #[test]
        fn test_jscontact_conversion() {
            run_jscontact_conversion()
        }

        #[test]
        fn test_smtp_auth_password_command() {
            run_test_smtp_auth_password_command()
        }

        #[test]
        fn test_smtp_auth_token_command() {
            run_test_smtp_auth_token_command()
        }
    }

    fn run_version_migration_server_password_command() {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let input = r#"
accounts.imap2 = { format = "imap", server_password_command = "echo hunter2" } 

[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password_command = "false"


[accounts.jmap]
root_mailbox = "INBOX"
format = ".map"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password = "hunter2"
"#;
        let mut doc = input.parse::<DocumentMut>().unwrap();

        ServerPasswordCommand::transform(&mut doc, true).unwrap();
        assert_eq!(
            doc.to_string(),
            r#"
accounts.imap2 = { format = "imap", server_password = { command = "echo hunter2" } } 

[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password = { command = "false" }


[accounts.jmap]
root_mailbox = "INBOX"
format = ".map"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password = "hunter2"
"#
        );
        ServerPasswordCommand::revert_transform(&mut doc, true).unwrap();
        assert_eq!(doc.to_string(), input);
    }

    fn run_contacts_refactor() {
        let tempdir = tempfile::tempdir().unwrap();
        const PREVIOUS: &str = r#"{"display_name":"user1@example.com","cards":{"16178106601568626693":{"id":"16178106601568626693","title":"","name":"User 2","additionalname":"","name_prefix":"","name_suffix":"","birthday":null,"email":"user2@example.com","url":"","key":"","color":0,"last_edited":1740521725,"extra_properties":{},"external_resource":false}}}"#;
        #[allow(non_snake_case)]
        let NEW: &str = &PREVIOUS[44..][..280];
        let prev = tempdir.path().join("contacts_prev");
        let new = tempdir.path().join("contacts_new");
        std::fs::write(&prev, PREVIOUS).unwrap();
        std::fs::write(&new, NEW).unwrap();
        // perform
        assert_eq!(
            &AddressbookRefactor::convert_contacts("user1@example.com", PREVIOUS, false).unwrap(),
            NEW
        );
        // revert
        assert_eq!(
            &AddressbookRefactor::convert_contacts("user1@example.com", NEW, true).unwrap(),
            PREVIOUS
        );
        let bkp_filepath = AddressbookRefactor::create_backup_file(&new, NEW, "bkp").unwrap();
        assert_eq!(
            bkp_filepath.file_name().unwrap().to_string_lossy().as_ref(),
            "contacts_new.bkp"
        );
        std::fs::write(&bkp_filepath, b"1").unwrap();
        let bkp_filepath =
            AddressbookRefactor::create_backup_file(&bkp_filepath, NEW, "bkp").unwrap();
        assert_eq!(
            bkp_filepath.file_name().unwrap().to_string_lossy().as_ref(),
            "contacts_new.bkp1"
        );
    }

    fn run_contact_backend_refactor() {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let input = r#"
[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password = "false"
notmuch_address_book_query = "notmuch"
mutt_alias_file = "mutt_alias"
vcard_folder = "vcard_folder"

"#;
        let mut doc = input.parse::<DocumentMut>().unwrap();

        ContactBackendRefactor::transform(&mut doc, true).unwrap();
        assert_eq!(
            doc.to_string(),
            r#"
[accounts.imap]
root_mailbox = "INBOX"
format = "imap"
send_mail = 'false'
identity="username@example.com"
server_username = "null"
server_hostname = "example.com"
server_password = "false"
contacts = { notmuch_address_book_query = { type = "NotmuchAddress", value = "notmuch" }, mutt_alias_file = { type = "MuttAlias", value = "mutt_alias" }, vcard_folder = { type = "VCard", value = "vcard_folder" } }

"#
        );
        ContactBackendRefactor::revert_transform(&mut doc, true).unwrap();
        assert_eq!(doc.to_string(), input);
    }

    fn run_jscontact_conversion() {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let tempdir = tempfile::tempdir().unwrap();
        const PREVIOUS: &str = r#"{"16178106601568626693":{"id":"16178106601568626693","title":"","name":"User 2","additionalname":"","name_prefix":"","name_suffix":"","birthday":null,"email":"user2@example.com","url":"","key":"","color":0,"last_edited":1740521725,"extra_properties":{},"external_resource":false}}"#;
        const REVERT: &str = "{\"3ec00657-bcc5-59ec-aeb1-de6615987560\":{\"id\":\"3ec00657-bcc5-59ec-aeb1-de6615987560\",\"title\":\"\",\"name\":\"User 2\",\"additionalname\":\"\",\"name_prefix\":\"\",\"name_suffix\":\"\",\"birthday\":null,\"email\":\"user2@example.com\",\"url\":\"\",\"key\":\"\",\"color\":0,\"last_edited\":1740521725,\"extra_properties\":{},\"external_resource\":false}}";
        const NEW: &str = "{\"3ec00657-bcc5-59ec-aeb1-de6615987560\":{\"@type\":\"Card\",\"version\":\"1.0\",\"uid\":\"3ec00657-bcc5-59ec-aeb1-de6615987560\",\"updated\":\"2025-02-25T22:15:25Z\",\"name\":{\"full\":\"User 2\"},\"emails\":{\"main\":{\"address\":\"user2@example.com\"}}}}";
        let prev = tempdir.path().join("contacts_prev");
        let new = tempdir.path().join("contacts_new");
        std::fs::write(&prev, PREVIOUS).unwrap();
        std::fs::write(&new, NEW).unwrap();
        // perform
        assert_eq!(
            &JSContactsConversion::convert_contacts(PREVIOUS, false).unwrap(),
            NEW
        );
        // revert
        assert_eq!(
            &JSContactsConversion::convert_contacts(NEW, true).unwrap(),
            REVERT
        );
        // roundtrip
        assert_eq!(
            &JSContactsConversion::convert_contacts(
                &JSContactsConversion::convert_contacts(NEW, true).unwrap(),
                false
            )
            .unwrap(),
            NEW
        );
        let bkp_filepath = JSContactsConversion::create_backup_file(&new, NEW, "bkp").unwrap();
        assert_eq!(
            bkp_filepath.file_name().unwrap().to_string_lossy().as_ref(),
            "contacts_new.bkp"
        );
        std::fs::write(&bkp_filepath, b"1").unwrap();
        let bkp_filepath =
            JSContactsConversion::create_backup_file(&bkp_filepath, NEW, "bkp").unwrap();
        assert_eq!(
            bkp_filepath.file_name().unwrap().to_string_lossy().as_ref(),
            "contacts_new.bkp1"
        );
    }

    fn run_test_smtp_auth_password_command() {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let input = r#"
accounts.imap2 = { send_mail = { auth = { type = "auto", username = "user@example.com", password = { type = "raw", value = "hunter2" } }, security = { type = "STARTTLS" } } }

[accounts.imap]
send_mail = { auth = { type = "auto", username = "user@example.com", password = { type = "raw", value = "hunter2" } }, security = { type = "STARTTLS" } }

[accounts.imap3]
send_mail = { auth = { type = "auto", username = "user@example.com", password = { type = "command_evaluation", value = "echo hunter2" } }, security = { type = "STARTTLS" } }
"#;
        let mut doc = input.parse::<DocumentMut>().unwrap();

        SmtpAuthPasswordCommand::transform(&mut doc, true).unwrap();
        assert_eq!(
            doc.to_string(),
            r#"
accounts.imap2 = { send_mail = { auth = { type = "auto", username = "user@example.com", password = "hunter2" }, security = { type = "STARTTLS" } } }

[accounts.imap]
send_mail = { auth = { type = "auto", username = "user@example.com", password = "hunter2" }, security = { type = "STARTTLS" } }

[accounts.imap3]
send_mail = { auth = { type = "auto", username = "user@example.com", password = { command = "echo hunter2" } }, security = { type = "STARTTLS" } }
"#
        );
        SmtpAuthPasswordCommand::revert_transform(&mut doc, true).unwrap();
        assert_eq!(doc.to_string(), input);
    }

    fn run_test_smtp_auth_token_command() {
        let _logger = Logger::new_with(LogLevel::TRACE, true);
        let input = r#"
accounts.imap2 = { send_mail = { auth = { type = "xoauth2", token_command = "echo hunter2" } } }

[accounts.imap]
send_mail = { auth = { type = "xoauth2", token_command = "echo hunter2" } }
"#;
        let mut doc = input.parse::<DocumentMut>().unwrap();

        SmtpAuthTokenCommand::transform(&mut doc, true).unwrap();
        assert_eq!(
            doc.to_string(),
            r#"
accounts.imap2 = { send_mail = { auth = { type = "xoauth2", token = { command = "echo hunter2" } } } }

[accounts.imap]
send_mail = { auth = { type = "xoauth2", token = { command = "echo hunter2" } } }
"#
        );
        SmtpAuthTokenCommand::revert_transform(&mut doc, true).unwrap();
        assert_eq!(doc.to_string(), input);
    }
}
