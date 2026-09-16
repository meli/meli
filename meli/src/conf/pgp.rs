/*
 * meli - configuration module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use melib::{conf::ActionFlag, Error, Result};
use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, Serializer},
};

use crate::conf::{default_values::*, DotAddressable};

/// Settings for digital signing and encryption
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PGPSettings {
    /// auto verify signed e-mail according to RFC3156
    /// Default: true
    #[serde(default = "true_val", alias = "auto-verify-signatures")]
    pub auto_verify_signatures: ActionFlag,

    /// auto decrypt encrypted e-mail
    /// Default: true
    #[serde(default = "true_val", alias = "auto-decrypt")]
    pub auto_decrypt: ActionFlag,

    /// always sign sent e-mail
    /// Default: false
    #[serde(default = "false_val", alias = "auto-sign")]
    pub auto_sign: ActionFlag,

    /// Auto encrypt sent e-mail
    /// Default: false
    #[serde(default = "false_val", alias = "auto-encrypt")]
    pub auto_encrypt: ActionFlag,

    // https://tools.ietf.org/html/rfc4880#section-12.2
    /// Default: None
    #[serde(default = "none", alias = "sign-key")]
    pub sign_key: Option<String>,

    /// Default: None
    #[serde(default = "none", alias = "decrypt-key")]
    pub decrypt_key: Option<String>,

    /// Default: None
    #[serde(default = "none", alias = "encrypt-key")]
    pub encrypt_key: Option<String>,

    /// Default: true
    #[serde(default = "true_val", alias = "encrypt-for-self")]
    pub encrypt_for_self: bool,

    /// Allow remote lookups
    /// Default: False
    #[serde(
        default = "action_internal_value_false",
        alias = "allow-remote-lookups"
    )]
    pub allow_remote_lookup: ActionFlag,

    /// Remote lookup mechanisms.
    /// Default: "local,wkd"
    #[serde(
        default = "default_lookup_mechanism",
        alias = "remote-lookup-mechanisms"
    )]
    pub remote_lookup_mechanisms: melib::email::pgp::LocateKey,
    /// PGP backend to use.
    /// Default: "gpgpme"
    #[serde(default)]
    pub backend: PGPBackendChoice,
}

fn default_lookup_mechanism() -> melib::email::pgp::LocateKey {
    melib::email::pgp::LocateKey::LOCAL | melib::email::pgp::LocateKey::WKD
}

impl Default for PGPSettings {
    fn default() -> Self {
        Self {
            auto_verify_signatures: true.into(),
            auto_decrypt: true.into(),
            auto_sign: false.into(),
            auto_encrypt: false.into(),
            encrypt_for_self: true,
            sign_key: None,
            decrypt_key: None,
            encrypt_key: None,
            allow_remote_lookup: action_internal_value_false::<ActionFlag>(),
            remote_lookup_mechanisms: default_lookup_mechanism(),
            backend: Default::default(),
        }
    }
}

impl DotAddressable for melib::email::pgp::LocateKey {}
impl DotAddressable for PGPBackendChoice {}

impl DotAddressable for PGPSettings {
    fn lookup(&self, parent_field: &str, path: &[&str]) -> Result<String> {
        match path.first() {
            Some(field) => {
                let tail = &path[1..];
                match *field {
                    "auto_verify_signatures" => self.auto_verify_signatures.lookup(field, tail),
                    "auto_decrypt" => self.auto_decrypt.lookup(field, tail),
                    "auto_sign" => self.auto_sign.lookup(field, tail),
                    "auto_encrypt" => self.auto_encrypt.lookup(field, tail),
                    "encrypt_for_self" => self.encrypt_for_self.lookup(field, tail),
                    "sign_key" => self.sign_key.lookup(field, tail),
                    "decrypt_key" => self.decrypt_key.lookup(field, tail),
                    "encrypt_key" => self.encrypt_key.lookup(field, tail),
                    "allow_remote_lookup" => self.allow_remote_lookup.lookup(field, tail),
                    "remote_lookup_mechanisms" => self.remote_lookup_mechanisms.lookup(field, tail),
                    "backend" => self.backend.lookup(field, tail),
                    other => Err(Error::new(format!(
                        "{parent_field} has no field named {other}"
                    ))),
                }
            }
            None => Ok(toml::Value::try_from(self)
                .map_err(|err| err.to_string())?
                .to_string()),
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum PGPBackendChoice {
    #[default]
    GpgME,
    CLI(Box<PGPBackendCLI>),
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PGPBackendCLI {
    pub verify_command: String,
    pub sign_command: String,
    pub encrypt_command: String,
    pub decrypt_command: String,
    pub get_key_command: String,
    pub keylist_command: String,
}

impl Serialize for PGPBackendChoice {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::GpgME => "gpgme".serialize(serializer),
            Self::CLI(ref cli) => cli.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for PGPBackendChoice {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;

        impl<'de> serde::de::Visitor<'de> for V {
            type Value = PGPBackendChoice;

            fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.write_str(
                    r#"either "gpgme" or a map of { verify_command, sign_command, encrypt_command, decrypt_command, get_key_command, keylist_command }"#,
                )
            }

            fn visit_string<E: serde::de::Error>(
                self,
                value: String,
            ) -> std::result::Result<Self::Value, E> {
                if value.eq_ignore_ascii_case("gpgme") {
                    return Ok(PGPBackendChoice::GpgME);
                }
                Err(serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(&value),
                    &"expected `gpgme`",
                ))
            }

            fn visit_str<E: serde::de::Error>(
                self,
                value: &str,
            ) -> std::result::Result<Self::Value, E> {
                if value.eq_ignore_ascii_case("gpgme") {
                    return Ok(PGPBackendChoice::GpgME);
                }
                Err(serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(value),
                    &"expected `gpgme`",
                ))
            }

            fn visit_borrowed_str<E: serde::de::Error>(
                self,
                value: &str,
            ) -> std::result::Result<Self::Value, E> {
                if value.eq_ignore_ascii_case("gpgme") {
                    return Ok(PGPBackendChoice::GpgME);
                }
                Err(serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(value),
                    &"expected `gpgme`",
                ))
            }

            fn visit_map<V>(self, map: V) -> std::result::Result<Self::Value, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let cli: PGPBackendCLI =
                    Deserialize::deserialize(serde::de::value::MapAccessDeserializer::new(map))?;
                Ok(PGPBackendChoice::CLI(Box::new(cli)))
            }
        }

        deserializer.deserialize_any(V)
    }
}
