//
// meli
//
// Copyright 2017-  Manos Pitsidianakis
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

use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, SerializeMap as _, Serializer},
};

use crate::error::{Error, ErrorKind};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum ToggleFlag {
    #[default]
    Unset,
    InternalVal(bool),
    False,
    True,
}

impl From<bool> for ToggleFlag {
    fn from(val: bool) -> Self {
        if val {
            Self::True
        } else {
            Self::False
        }
    }
}

impl ToggleFlag {
    pub fn is_unset(&self) -> bool {
        Self::Unset == *self
    }

    pub fn is_internal(&self) -> bool {
        matches!(self, Self::InternalVal(_))
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Self::False | Self::InternalVal(false))
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Self::True | Self::InternalVal(true))
    }
}

impl Serialize for ToggleFlag {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Unset | Self::InternalVal(_) => serializer.serialize_none(),
            Self::False => serializer.serialize_bool(false),
            Self::True => serializer.serialize_bool(true),
        }
    }
}

impl<'de> Deserialize<'de> for ToggleFlag {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <bool>::deserialize(deserializer)?;
        Ok(s.into())
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum ActionFlag {
    InternalVal(bool),
    False,
    True,
    #[default]
    Ask,
}

impl From<bool> for ActionFlag {
    fn from(val: bool) -> Self {
        if val {
            Self::True
        } else {
            Self::False
        }
    }
}

impl ActionFlag {
    pub fn is_internal(&self) -> bool {
        matches!(self, Self::InternalVal(_))
    }

    pub fn is_ask(&self) -> bool {
        matches!(self, Self::Ask)
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Self::False | Self::InternalVal(false))
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Self::True | Self::InternalVal(true))
    }
}

impl Serialize for ActionFlag {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::InternalVal(_) => serializer.serialize_none(),
            Self::False => serializer.serialize_bool(false),
            Self::True => serializer.serialize_bool(true),
            Self::Ask => serializer.serialize_str("ask"),
        }
    }
}

impl<'de> Deserialize<'de> for ActionFlag {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;
        impl<'de> serde::de::Visitor<'de> for V {
            type Value = ActionFlag;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("either a boolean or the string \"ask\"")
            }

            fn visit_bool<E: serde::de::Error>(self, value: bool) -> Result<Self::Value, E> {
                Ok(if value {
                    ActionFlag::True
                } else {
                    ActionFlag::False
                })
            }

            fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<Self::Value, E> {
                if value.eq_ignore_ascii_case("ask") {
                    Ok(ActionFlag::Ask)
                } else {
                    Err(E::invalid_value(
                        serde::de::Unexpected::Str(value),
                        &"either a boolean or the string \"ask\"",
                    ))
                }
            }
        }
        deserializer.deserialize_any(V)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// String value type for secrets (i.e. passwords/usernames)
pub enum Secret {
    /// Literal string value
    Value(String),
    /// A command that must be invoked to evaluate the secret string value.
    Evaluate {
        command: String,
        store_in_memory: bool,
    },
}

impl Secret {
    /// Prepopulate value in memory if `store_in_memory` is true.
    pub fn prepopulate(&mut self, timeout: Option<std::time::Duration>) -> Result<(), Error> {
        if let Self::Evaluate {
            command: _,
            store_in_memory: true,
        } = self
        {
            if let Some(timeout) = timeout {
                let value = futures::executor::block_on(self.value_with_timeout(timeout))?;
                *self = Self::Value(value);
            } else {
                let value = self.value()?;
                *self = Self::Value(value);
            }
        }
        Ok(())
    }

    pub fn value(&self) -> Result<String, Error> {
        match self {
            Self::Value(val) => Ok(val.clone()),
            Self::Evaluate {
                ref command,
                store_in_memory: _,
            } => std::process::Command::new("sh")
                .args(["-c", command])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .output()
                .map_err(|err| {
                    Error::new(format!("Could not execute command `{command}`"))
                        .set_details(err.to_string())
                        .set_source(Some(crate::src_err_arc_wrap! { err }))
                        .set_kind(ErrorKind::External)
                })
                .and_then(|output| {
                    if output.status.success() {
                        match std::str::from_utf8(&output.stdout) {
                            Ok(v) => Ok(v.trim_end().to_string()),
                            Err(err) => Err(Error::new(format!(
                                "Command `{command}` returned non-UTF-8 bytes"
                            ))
                            .set_details(format!(
                                "stdout was: {stdout:?}",
                                stdout = String::from_utf8_lossy(&output.stdout)
                            ))
                            .set_source(Some(crate::src_err_arc_wrap! { err }))
                            .set_kind(ErrorKind::External)),
                        }
                    } else {
                        Err(Error::new(format!("Could not execute command `{command}`"))
                            .set_details(format!(
                                "Exit status: {status} stdout: {stdout:?} stderr: {stderr:?}",
                                status = output.status,
                                stdout = String::from_utf8_lossy(&output.stdout),
                                stderr = String::from_utf8_lossy(&output.stderr)
                            ))
                            .set_kind(ErrorKind::External))
                    }
                }),
        }
    }

    pub async fn value_with_timeout(&self, timeout: std::time::Duration) -> Result<String, Error> {
        let self_val = self.clone();
        crate::utils::futures::timeout(Some(timeout), smol::unblock(move || self_val.value()))
            .await?
    }

    #[inline]
    pub const fn store_in_memory(&self) -> bool {
        match self {
            Self::Value(_) => true,
            Self::Evaluate {
                command: _,
                store_in_memory,
            } => *store_in_memory,
        }
    }
}

impl Serialize for Secret {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Value(ref s) => serializer.serialize_str(s),
            Self::Evaluate {
                ref command,
                store_in_memory: true,
            } => {
                let mut eval = serializer.serialize_map(Some(1))?;
                eval.serialize_entry("command", &command)?;
                eval.end()
            }
            Self::Evaluate {
                ref command,
                store_in_memory: false,
            } => {
                let mut eval = serializer.serialize_map(Some(2))?;
                eval.serialize_entry("command", &command)?;
                eval.serialize_entry("store_in_memory", &false)?;
                eval.end()
            }
        }
    }
}

// Do a complicated dance to deserialize `Secret` to keep backwards-compatibility with
// `melib::smtp::Password` which used to be:
//
// /// Source of user's password for SMTP authentication
// #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
// #[serde(tag = "type", content = "value")]
// pub enum Password {
//     #[serde(alias = "raw")]
//     Raw(String),
//     #[serde(alias = "command_evaluation", alias = "command_eval")]
//     CommandEval(String),
// }
impl<'de> Deserialize<'de> for Secret {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum SmtpPasswordField {
            Type,
            Value,
        }

        struct SmtpPasswordFieldVisitor;

        impl<'de> serde::de::Visitor<'de> for SmtpPasswordFieldVisitor {
            type Value = SmtpPasswordField;

            fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.write_str("`type` or `value`")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "type" => Ok(SmtpPasswordField::Type),
                    "value" => Ok(SmtpPasswordField::Value),
                    _ => Err(serde::de::Error::unknown_field(value, &["type", "value"])),
                }
            }
        }

        impl<'de> Deserialize<'de> for SmtpPasswordField {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_identifier(SmtpPasswordFieldVisitor)
            }
        }

        enum SecretField {
            Command,
            StoreInMemory,
        }

        struct SecretFieldVisitor;

        impl<'de> serde::de::Visitor<'de> for SecretFieldVisitor {
            type Value = SecretField;

            fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.write_str("`command` or `store_in_memory`")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "command" => Ok(SecretField::Command),
                    "store_in_memory" => Ok(SecretField::StoreInMemory),
                    _ => Err(serde::de::Error::unknown_field(
                        value,
                        &["command", "store_in_memory"],
                    )),
                }
            }
        }

        impl<'de> Deserialize<'de> for SecretField {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_identifier(SecretFieldVisitor)
            }
        }

        enum Field {
            Secret(SecretField),
            SmtpPassword(SmtpPasswordField),
        }

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct FieldVisitor;

                impl<'de> serde::de::Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                        SecretFieldVisitor.expecting(fmt)
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        match value {
                            "command" | "store_in_memory" => {
                                Ok(Field::Secret(SecretFieldVisitor.visit_str(value)?))
                            }
                            "type" | "value" => Ok(Field::SmtpPassword(
                                SmtpPasswordFieldVisitor.visit_str(value)?,
                            )),
                            other => Err(serde::de::Error::unknown_field(
                                other,
                                &["command", "store_in_memory"],
                            )),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }
        struct SecretVisitor;

        impl<'de> serde::de::Visitor<'de> for SecretVisitor {
            type Value = Secret;

            fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.write_str(
                    "either a string literal or map with keys \"command\" (string) and optionally \
                     \"store_in_memory\" (bool)",
                )
            }

            fn visit_string<E: serde::de::Error>(self, value: String) -> Result<Self::Value, E> {
                Ok(Secret::Value(value))
            }

            fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<Self::Value, E> {
                Ok(Secret::Value(value.into()))
            }

            fn visit_borrowed_str<E: serde::de::Error>(
                self,
                value: &str,
            ) -> Result<Self::Value, E> {
                Ok(Secret::Value(value.into()))
            }

            fn visit_map<V>(self, mut access: V) -> Result<Self::Value, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let mut command = None;
                let mut store_in_memory = None;

                let Some(first_key) = access.next_key::<Field>()? else {
                    return Err(serde::de::Error::missing_field("command"));
                };
                match first_key {
                    Field::SmtpPassword(first_key) => match first_key {
                        SmtpPasswordField::Type => {
                            match access.next_value::<String>()?.as_str() {
                                "Raw" | "raw" => {
                                    match access.next_key::<SmtpPasswordField>()? {
                                        Some(SmtpPasswordField::Value) => {}
                                        Some(SmtpPasswordField::Type) => {
                                            return Err(serde::de::Error::duplicate_field("type"))
                                        }

                                        None => {
                                            return Err(serde::de::Error::missing_field("value"))
                                        }
                                    };
                                    let raw_value = access.next_value::<String>()?;
                                    log::warn!(
                                        "SMTP password syntax has been deprecated! Replace with a \
                                         raw string."
                                    );
                                    return Ok(Secret::Value(raw_value));
                                }
                                "CommandEval" | "command_evaluation" | "command_eval" => {
                                    match access.next_key::<SmtpPasswordField>()? {
                                        Some(SmtpPasswordField::Value) => {}
                                        Some(SmtpPasswordField::Type) => {
                                            return Err(serde::de::Error::duplicate_field("type"))
                                        }

                                        None => {
                                            return Err(serde::de::Error::missing_field("value"))
                                        }
                                    };
                                    let command = access.next_value::<String>()?;
                                    log::warn!(
                                        "SMTP password syntax has been deprecated! Replace with \
                                         Secret syntax."
                                    );
                                    return Ok(Secret::Evaluate {
                                        command,
                                        store_in_memory: false,
                                    });
                                }
                                other => {
                                    return Err(serde::de::Error::invalid_value(
                                        serde::de::Unexpected::Str(other),
                                        &"`raw` or `command_evaluation`",
                                    ))
                                }
                            };
                        }
                        SmtpPasswordField::Value => {
                            let raw_value = access.next_value::<String>()?;
                            match access.next_key::<SmtpPasswordField>()? {
                                Some(SmtpPasswordField::Type) => {}
                                Some(SmtpPasswordField::Value) => {
                                    return Err(serde::de::Error::duplicate_field("value"))
                                }
                                None => return Err(serde::de::Error::missing_field("type")),
                            };
                            match access.next_value::<String>()?.as_str() {
                                "Raw" | "raw" => {
                                    log::warn!(
                                        "SMTP password syntax has been deprecated! Replace with a \
                                         raw string."
                                    );
                                    return Ok(Secret::Value(raw_value));
                                }
                                "CommandEval" | "command_evaluation" | "command_eval" => {
                                    log::warn!(
                                        "SMTP password syntax has been deprecated! Replace with \
                                         Secret syntax."
                                    );
                                    return Ok(Secret::Evaluate {
                                        command: raw_value,
                                        store_in_memory: false,
                                    });
                                }
                                other => {
                                    return Err(serde::de::Error::invalid_value(
                                        serde::de::Unexpected::Str(other),
                                        &"`raw` or `command_evaluation`",
                                    ))
                                }
                            }
                        }
                    },
                    Field::Secret(first_key) => {
                        match first_key {
                            SecretField::Command => {
                                command = Some(access.next_value::<String>()?);
                            }
                            SecretField::StoreInMemory => {
                                store_in_memory = Some(access.next_value::<bool>()?);
                            }
                        }
                        while let Some(key) = access.next_key::<SecretField>()? {
                            match key {
                                SecretField::Command => {
                                    if command.is_some() {
                                        return Err(serde::de::Error::duplicate_field("command"));
                                    }
                                }
                                SecretField::StoreInMemory => {
                                    if store_in_memory.is_some() {
                                        return Err(serde::de::Error::duplicate_field(
                                            "store_in_memory",
                                        ));
                                    }
                                    store_in_memory = Some(access.next_value::<bool>()?);
                                }
                            }
                        }
                    }
                }

                let Some(command) = command else {
                    return Err(serde::de::Error::missing_field("command"));
                };
                let store_in_memory = store_in_memory.unwrap_or(true);
                Ok(Secret::Evaluate {
                    command,
                    store_in_memory,
                })
            }
        }

        deserializer.deserialize_any(SecretVisitor)
    }
}
