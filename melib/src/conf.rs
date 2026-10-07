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

use std::borrow::Cow;

use indexmap::IndexMap;

use crate::{
    backends::SpecialUsageMailbox,
    email::Address,
    error::{Error, ErrorKind, Result},
};

mod field_types;
#[cfg(test)]
mod tests;

pub use field_types::*;

/// Trait to deserialize a type with [`serde_json`].
pub trait ExtraSetting: serde::de::DeserializeOwned {
    fn deserialize_extra(value: &serde_json::Value) -> Result<Self> {
        Ok(serde::de::Deserialize::deserialize(value.clone())?)
    }
}

impl<'a> ExtraSetting for Cow<'a, str> {}
impl ExtraSetting for String {}
impl ExtraSetting for std::path::PathBuf {}
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

impl_extra_setting_from_str! { bool, u16, u64, i64 }

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ContactBackendConf {
    NotmuchAddress(String),
    MuttAlias(String),
    VCard(String),
    #[cfg(feature = "webdav")]
    CardDAV(crate::utils::webdav::WebDAVServerConf),
}

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
    #[serde(default)]
    pub contacts: IndexMap<String, ContactBackendConf>,
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
                .set_source(Some(Box::new(err)))
                .set_kind(ErrorKind::Configuration)
            })
            .map(|v| Some(v))
    }

    /// Validate an extra field with [`FieldValidator`].
    ///
    /// This removes the value from [`Self::extra`].
    #[allow(clippy::type_complexity, reason = "whadd'ya gonna do?")]
    pub fn validator<'a, D: ExtraSetting>(
        &'a mut self,
        extra_field: &'static str,
        expected_type: &'static str,
    ) -> FieldValidator<'a, &'a mut Self, D, fn(&D) -> Result<()>> {
        FieldValidator {
            inner: self,
            extra_field,
            expected_type,
            validation_fn: None,
            default_value: None,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Validate an extra field of a mailbox with [`FieldValidator`].
    ///
    /// This removes the value from [`MailboxConf::extra`].
    #[allow(clippy::type_complexity, reason = "whadd'ya gonna do?")]
    pub fn mailbox_conf_validator<'a, D: ExtraSetting>(
        &'a mut self,
        mailbox_name: &str,
        extra_field: &'static str,
        expected_type: &'static str,
    ) -> Option<FieldValidator<'a, MailboxConfValidationRef<'a>, D, fn(&D) -> Result<()>>> {
        let Self {
            ref mut mailboxes,
            name: ref account_name,
            ref format,
            ..
        } = self;
        let (mailbox_name, inner) = mailboxes.get_key_value_mut(mailbox_name)?;
        Some(FieldValidator {
            inner: MailboxConfValidationRef {
                mailbox_name,
                account_name,
                format,
                inner,
            },
            extra_field,
            expected_type,
            validation_fn: None,
            default_value: None,
            _phantom: std::marker::PhantomData,
        })
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
    pub extra: IndexMap<String, serde_json::Value>,
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

    /// Deserialize [`Self::extra`] fields as [`[ExtraSetting`] types.
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
                .set_source(Some(Box::new(err)))
                .set_kind(ErrorKind::Configuration)
            })
            .map(|v| Some(v))
    }
}

/// Helper struct to validate [`MailboxConf`].
pub struct MailboxConfValidationRef<'a> {
    inner: &'a mut MailboxConf,
    account_name: &'a str,
    mailbox_name: &'a str,
    format: &'a str,
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

/// Trait for configuration structs that have "extra" deserializable fields like [`AccountSettings`] and [`MailboxConf`].
///
/// ```no_run
/// use indexmap::IndexMap;
/// use melib::{
///     conf::{ExtraSetting, FieldValidationTrait, FieldValidator},
///     error::Result,
/// };
/// use serde::Deserialize;
///
/// #[derive(Default, Deserialize)]
/// #[serde(default)]
/// struct Settings {
///     pub name: String,
///     #[serde(flatten)]
///     pub extra: IndexMap<String, serde_json::Value>,
/// }
///
/// impl Settings {
///     fn validator<'a, D: ExtraSetting>(
///         &'a mut self,
///         extra_field: &'static str,
///         expected_type: &'static str,
///     ) -> FieldValidator<'a, &'a mut Self, D, fn(&D) -> Result<()>> {
///         FieldValidator {
///             inner: self,
///             extra_field,
///             expected_type,
///             validation_fn: None,
///             default_value: None,
///             _phantom: std::marker::PhantomData,
///         }
///     }
/// }
///
/// impl FieldValidationTrait for &mut Settings {
///     fn extra_mut(&mut self) -> &mut IndexMap<String, serde_json::Value> {
///         &mut self.extra
///     }
///
///     fn requires_field_err(&self, extra_field: &'static str) -> String {
///         format!(
///             "{name} requires field `{extra_field}` set",
///             name = self.name,
///         )
///     }
///
///     fn expects_type_err(
///         &self,
///         extra_field: &'static str,
///         expected_type: &'static str,
///     ) -> String {
///         format!(
///             "{name} field `{extra_field}` expects value of type {expected_type}",
///             name = self.name,
///         )
///     }
///
///     fn validation_fn_err(&self, extra_field: &'static str) -> String {
///         format!(
///             "{name} backend field `{extra_field}` is invalid",
///             name = self.name,
///         )
///     }
/// }
///
/// let mut settings = Settings {
///     name: "my_settings".to_string(),
///     extra: indexmap::indexmap! {
///     "one".to_string() => serde_json::json! { 1_i64 },
///     "two".to_string() => serde_json::json! { "2" },
///     },
/// };
///
/// assert_eq!(
///     settings
///         .validator::<i64>("one", "integer")
///         .validate()
///         .unwrap(),
///     1
/// );
/// assert_eq!(
///     settings
///         .validator::<String>("two", "integer")
///         .ignore_missing()
///         .unwrap(),
///     Some("2".to_string())
/// );
/// settings
///     .extra
///     .insert("one".to_string(), "one".to_string().into());
///
/// settings
///     .validator::<i64>("one", "integer")
///     .validate()
///     .unwrap_err();
/// ```
pub trait FieldValidationTrait {
    fn extra_mut(&mut self) -> &mut IndexMap<String, serde_json::Value>;
    /// Create error string for when a missing field is required.
    fn requires_field_err(&self, extra_field: &'static str) -> String;
    /// Create error string for when a field value is of wrong type.
    fn expects_type_err(&self, extra_field: &'static str, expected_type: &'static str) -> String;
    /// Create error string for when a validation function fails.
    fn validation_fn_err(&self, extra_field: &'static str) -> String;
}

impl FieldValidationTrait for &mut AccountSettings {
    fn extra_mut(&mut self) -> &mut IndexMap<String, serde_json::Value> {
        &mut self.extra
    }

    fn requires_field_err(&self, extra_field: &'static str) -> String {
        format!(
            "{name}: {format} backend requires field `{extra_field}` set",
            name = self.name,
            format = self.format
        )
    }

    fn expects_type_err(&self, extra_field: &'static str, expected_type: &'static str) -> String {
        format!(
            "{name}: {format} backend field `{extra_field}` expects value of type {expected_type}",
            name = self.name,
            format = self.format,
        )
    }

    fn validation_fn_err(&self, extra_field: &'static str) -> String {
        format!(
            "{name}: {format} backend field `{extra_field}` is invalid",
            name = self.name,
            format = self.format
        )
    }
}

impl<'a> FieldValidationTrait for MailboxConfValidationRef<'a> {
    fn extra_mut(&mut self) -> &mut IndexMap<String, serde_json::Value> {
        &mut self.inner.extra
    }

    fn requires_field_err(&self, extra_field: &'static str) -> String {
        format!(
            "{account_name} {mailbox_name}: {format} backend requires mailbox field \
             `{extra_field}` set",
            account_name = self.account_name,
            mailbox_name = self.mailbox_name,
            format = self.format
        )
    }

    fn expects_type_err(&self, extra_field: &'static str, expected_type: &'static str) -> String {
        format!(
            "{account_name} {mailbox_name}: {format} backend mailbox field `{extra_field}` \
             expects value of type {expected_type}",
            account_name = self.account_name,
            mailbox_name = self.mailbox_name,
            format = self.format,
        )
    }

    fn validation_fn_err(&self, extra_field: &'static str) -> String {
        format!(
            "{account_name} {mailbox_name}: {format} backend mailbox field `{extra_field}` is \
             invalid",
            account_name = self.account_name,
            mailbox_name = self.mailbox_name,
            format = self.format
        )
    }
}

/// Validate a field.
///
/// See documentation of [`FieldValidationTrait`].
#[must_use]
pub struct FieldValidator<
    'a,
    T: FieldValidationTrait + 'a,
    D: ExtraSetting,
    ValidationFn: FnOnce(&D) -> Result<()>,
> {
    pub inner: T,
    pub extra_field: &'static str,
    pub expected_type: &'static str,
    pub validation_fn: Option<ValidationFn>,
    pub default_value: Option<D>,
    pub _phantom: std::marker::PhantomData<&'a T>,
}

impl<'a, T: FieldValidationTrait, D: ExtraSetting> FieldValidator<'a, T, D, fn(&D) -> Result<()>> {
    #[inline]
    pub fn validation_fn<F: FnOnce(&D) -> Result<()>>(
        self,
        validation_fn: F,
    ) -> FieldValidator<'a, T, D, F> {
        let Self {
            inner,
            extra_field,
            expected_type,
            validation_fn: _,
            _phantom,
            default_value,
        } = self;
        FieldValidator::<'a, T, D, F> {
            inner,
            extra_field,
            expected_type,
            validation_fn: Some(validation_fn),
            default_value,
            _phantom,
        }
    }
}

impl<'a, T: FieldValidationTrait, D: ExtraSetting, ValidationFn: FnOnce(&D) -> Result<()>>
    FieldValidator<'a, T, D, ValidationFn>
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
            if matches!(&err.source, Some(err) if err.kind == ErrorKind::NotFound) {
                return Ok(None);
            }
            Err(err)
        })
    }

    #[inline]
    #[must_use = "A validation result must be inspected"]
    pub fn validate(self) -> Result<D> {
        let Self {
            mut inner,
            extra_field,
            expected_type,
            validation_fn,
            default_value,
            _phantom,
        } = self;
        let Some(raw_value) = inner.extra_mut().swap_remove(extra_field) else {
            if let Some(default_value) = default_value {
                return Ok(default_value);
            }
            let source =
                Error::new(format!("missing field `{extra_field}`")).set_kind(ErrorKind::NotFound);
            return Err(Error::new(inner.requires_field_err(extra_field))
                .set_source(Some(Box::new(source)))
                .set_kind(ErrorKind::Configuration));
        };
        match <D>::deserialize_extra(&raw_value) {
            Ok(v) => {
                if let Some(validation_fn) = validation_fn {
                    validation_fn(&v)
                        .map_err(|err| err.set_summary(inner.validation_fn_err(extra_field)))?;
                }
                Ok(v)
            }
            Err(err) => Err(
                Error::new(inner.expects_type_err(extra_field, expected_type))
                    .set_source(Some(Box::new(err)))
                    .set_kind(ErrorKind::Configuration),
            ),
        }
    }
}
