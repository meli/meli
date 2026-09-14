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
    #[cfg_attr(
        feature = "gpgme",
        serde(
            default = "default_lookup_mechanism",
            alias = "remote-lookup-mechanisms"
        )
    )]
    #[cfg(feature = "gpgme")]
    pub remote_lookup_mechanisms: melib::email::pgp::LocateKey,
    #[cfg(not(feature = "gpgme"))]
    #[cfg_attr(
        not(feature = "gpgme"),
        serde(default, alias = "remote-lookup-mechanisms")
    )]
    pub remote_lookup_mechanisms: String,
}

#[cfg(feature = "gpgme")]
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
            #[cfg(feature = "gpgme")]
            remote_lookup_mechanisms: default_lookup_mechanism(),
            #[cfg(not(feature = "gpgme"))]
            remote_lookup_mechanisms: String::new(),
        }
    }
}

impl DotAddressable for melib::email::pgp::LocateKey {}

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
                    #[cfg(feature = "gpgme")]
                    "remote_lookup_mechanisms" => self.remote_lookup_mechanisms.lookup(field, tail),
                    #[cfg(not(feature = "gpgme"))]
                    "remote_lookup_mechanisms" => self.remote_lookup_mechanisms.lookup(field, tail),
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
