/*
 * meli - configuration module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

//! Basic mail account configuration to use with
//! [`backends`](./backends/index.html)

use std::{borrow::Cow, path::Path};

use indexmap::IndexMap;

use crate::{
    backends::SpecialUsageMailbox,
    email::Address,
    error::{Error, ErrorKind, Result},
    ShellExpandTrait,
};

mod field_types;
#[cfg(test)]
mod tests;

pub use field_types::*;

pub trait ExtraSetting: serde::de::DeserializeOwned {
    fn deserialize_extra(value: &serde_json::Value) -> Result<Self> {
        Ok(serde::de::Deserialize::deserialize(value.clone())?)
    }
}

impl<'a> ExtraSetting for Cow<'a, str> {}
impl ExtraSetting for String {}
impl ExtraSetting for field_types::Secret {}

macro_rules! impl_extra_setting_from_str {
    ($($t:ty),*$(,)?) => {
        $(impl ExtraSetting for $t {
            fn deserialize_extra(v: &serde_json::Value) -> Result<Self> {
                Ok(serde::de::Deserialize::deserialize(v.clone()).or_else(|err| {
                    if let Ok(s) = serde::de::Deserialize::deserialize(v.clone()) {
                        let s: Cow<'_, str> = s;
                        if let Ok(v) = <$t as std::str::FromStr>::from_str(s.as_ref()) {
                            return Ok(v);
                        }
                    }
                    Err(err)
                })?)
            }
        })*
    };
}

impl_extra_setting_from_str! { bool, u16, u64 }

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AccountSettings {
    pub name: String,
    /// Name of mailbox that is the root of the mailbox hierarchy.
    ///
    /// Note that this may have special or no meaning depending on the e-mail
    /// backend.
    pub root_mailbox: String,
    pub format: String,
    pub identity: String,
    #[serde(default)]
    pub extra_identities: Vec<String>,
    #[serde(default = "false_val")]
    pub read_only: bool,
    #[serde(default)]
    pub display_name: Option<String>,
    #[serde(default)]
    pub subscribed_mailboxes: Vec<String>,
    #[serde(default)]
    pub mailboxes: IndexMap<String, MailboxConf>,
    #[serde(default)]
    pub manual_refresh: bool,
    #[serde(flatten)]
    pub extra: IndexMap<String, serde_json::Value>,
}

impl AccountSettings {
    /// Create the account's display name from fields
    /// [`AccountSettings::identity`] and [`AccountSettings::display_name`].
    #[deprecated(
        since = "0.8.5",
        note = "Use AccountSettings::main_identity_address instead."
    )]
    pub fn make_display_name(&self) -> Address {
        Address::new(self.display_name.clone(), self.identity.clone())
    }

    /// Return address associated with this account.
    /// It combines the values from [`AccountSettings::identity`] and
    /// [`AccountSettings::display_name`].
    pub fn main_identity_address(&self) -> Address {
        Address::new(self.display_name.clone(), self.identity.clone())
    }

    /// Return addresses of extra identities associated with this account,
    /// if any.
    pub fn extra_identity_addresses(&self) -> Vec<Address> {
        self.extra_identities
            .iter()
            .map(|i| Address::new(None::<&str>, i.clone()))
            .collect()
    }

    pub fn deserialize_extra_field<'de, 'a: 'de, D: ExtraSetting>(
        &'a self,
        extra_field: &str,
    ) -> Result<Option<D>> {
        let Some(v) = self.extra.get(extra_field) else {
            return Ok(None);
        };
        <D>::deserialize_extra(v)
            .map_err(|err| {
                Error::new(format!(
                    "Could not deserialize {extra_field} as {type_name}",
                    type_name = std::any::type_name::<D>()
                ))
                .set_source(Some(crate::src_err_arc_wrap! { err }))
                .set_kind(ErrorKind::Configuration)
            })
            .map(|v| Some(v))
    }

    #[inline]
    fn extra_field_as_str(&'_ self, extra_field: &str) -> Result<Option<Cow<'_, str>>> {
        self.deserialize_extra_field::<Cow<'_, str>>(extra_field)
    }

    pub fn vcard_folder(&'_ self) -> Result<Option<Cow<'_, str>>> {
        self.extra_field_as_str("vcard_folder")
    }

    pub fn notmuch_address_book_query(&self) -> Result<Option<Cow<'_, str>>> {
        self.extra_field_as_str("notmuch_address_book_query")
    }

    pub fn mutt_alias_file(&self) -> Result<Option<Cow<'_, str>>> {
        self.extra_field_as_str("mutt_alias_file")
    }

    pub fn validator<'a, D: ExtraSetting>(
        &'a mut self,
        extra_field: &'static str,
        expected_type: &'static str,
    ) -> FieldValidatorBuilder<'a, D, fn(&D) -> Result<()>> {
        FieldValidatorBuilder {
            inner: self,
            extra_field,
            expected_type,
            validation_fn: None,
            default_value: None,
        }
    }

    pub fn validate_config(&mut self) -> Result<()> {
        {
            if let Some(folder) = self.vcard_folder()? {
                let path = Path::new(folder.as_ref()).expand();
                _ = self.extra.swap_remove("vcard_folder");

                if !matches!(path.try_exists(), Ok(true)) {
                    return Err(Error::new(format!(
                        "`vcard_folder` path {} does not exist",
                        path.display()
                    ))
                    .set_details("`vcard_folder` must be a path of a folder containing .vcf files")
                    .set_kind(ErrorKind::Configuration));
                }
                if !path.is_dir() {
                    return Err(Error::new(format!(
                        "`vcard_folder` path {} is not a directory",
                        path.display()
                    ))
                    .set_details("`vcard_folder` must be a path of a folder containing .vcf files")
                    .set_kind(ErrorKind::Configuration));
                }
            }
            self.notmuch_address_book_query()?;
            _ = self.extra.swap_remove("notmuch_address_book_query");
        }
        {
            if let Some(mutt_alias_file) = self.mutt_alias_file()? {
                let path = Path::new(mutt_alias_file.as_ref()).expand();
                _ = self.extra.swap_remove("mutt_alias_file");

                if !matches!(path.try_exists(), Ok(true)) {
                    return Err(Error::new(format!(
                        "`mutt_alias_file` path {} does not exist",
                        path.display()
                    ))
                    .set_details("`mutt_alias_file` must be an existing path of a mutt alias file")
                    .set_kind(ErrorKind::Configuration));
                }
                if !path.is_file() {
                    return Err(Error::new(format!(
                        "`mutt_alias_file` path {} is not a file",
                        path.display()
                    ))
                    .set_details("`mutt_alias_file` must be a path of a mutt alias file")
                    .set_kind(ErrorKind::Configuration));
                }
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct MailboxConf {
    #[serde(alias = "rename")]
    pub alias: Option<String>,
    #[serde(default = "false_val")]
    pub autoload: bool,
    #[serde(default)]
    pub subscribe: ToggleFlag,
    #[serde(default)]
    pub ignore: ToggleFlag,
    #[serde(default = "none")]
    pub usage: Option<SpecialUsageMailbox>,
    #[serde(default = "none")]
    pub sort_order: Option<usize>,
    #[serde(default = "none")]
    pub encoding: Option<String>,
    #[serde(flatten)]
    pub extra: IndexMap<String, String>,
}

impl Default for MailboxConf {
    fn default() -> Self {
        Self {
            alias: None,
            autoload: false,
            subscribe: ToggleFlag::Unset,
            ignore: ToggleFlag::Unset,
            usage: None,
            sort_order: None,
            encoding: None,
            extra: IndexMap::default(),
        }
    }
}

impl MailboxConf {
    pub fn alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }
}

pub const fn true_val() -> bool {
    true
}

pub const fn false_val() -> bool {
    false
}

pub const fn none<T>() -> Option<T> {
    None
}

#[must_use]
pub struct FieldValidatorBuilder<'a, D: ExtraSetting, ValidationFn: FnOnce(&D) -> Result<()>> {
    inner: &'a mut AccountSettings,
    extra_field: &'static str,
    expected_type: &'static str,
    validation_fn: Option<ValidationFn>,
    default_value: Option<D>,
}

impl<'a, D: ExtraSetting> FieldValidatorBuilder<'a, D, fn(&D) -> Result<()>> {
    #[inline]
    pub fn validation_fn<F: FnOnce(&D) -> Result<()>>(
        self,
        validation_fn: F,
    ) -> FieldValidatorBuilder<'a, D, F> {
        let Self {
            inner,
            extra_field,
            expected_type,
            validation_fn: _,
            default_value,
        } = self;
        FieldValidatorBuilder::<'a, D, F> {
            inner,
            extra_field,
            expected_type,
            validation_fn: Some(validation_fn),
            default_value,
        }
    }
}

impl<'a, D: ExtraSetting, ValidationFn: FnOnce(&D) -> Result<()>>
    FieldValidatorBuilder<'a, D, ValidationFn>
{
    #[inline]
    pub fn default_value(self, default_value: D) -> Self {
        Self {
            default_value: Some(default_value),
            ..self
        }
    }

    #[inline]
    #[must_use = "A validation result must be inspected"]
    pub fn ignore_missing(self) -> Result<Option<D>> {
        self.validate().map(|v| Some(v)).or_else(|err| {
            if matches!(err.kind, ErrorKind::NotFound) {
                return Ok(None);
            }
            Err(err)
        })
    }

    #[inline]
    #[must_use = "A validation result must be inspected"]
    pub fn validate(self) -> Result<D> {
        let Self {
            inner,
            extra_field,
            expected_type,
            validation_fn,
            default_value,
        } = self;
        let Some(raw_value) = inner.extra.swap_remove(extra_field) else {
            if let Some(default_value) = default_value {
                return Ok(default_value);
            }
            return Err(Error::new(format!(
                "{name}: {format} backend requires field `{extra_field}` set",
                name = inner.name,
                format = inner.format
            ))
            .set_kind(ErrorKind::NotFound));
        };
        match <D>::deserialize_extra(&raw_value) {
            Ok(v) => {
                if let Some(validation_fn) = validation_fn {
                    validation_fn(&v).map_err(|err| err.set_summary(inner.name.to_string()))?;
                }
                Ok(v)
            }
            Err(err) => Err(Error::new(format!(
                "{name}: field `{extra_field}` expects value of type {expected_type}",
                name = inner.name,
            ))
            .set_source(Some(crate::src_err_arc_wrap! { err }))
            .set_kind(ErrorKind::Configuration)),
        }
    }
}
