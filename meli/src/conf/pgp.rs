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

use melib::conf::ActionFlag;

use crate::conf::default_values::*;

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
    pub remote_lookup_mechanisms: melib::gpgme::LocateKey,
    #[cfg(not(feature = "gpgme"))]
    #[cfg_attr(
        not(feature = "gpgme"),
        serde(default, alias = "remote-lookup-mechanisms")
    )]
    pub remote_lookup_mechanisms: String,
}

#[cfg(feature = "gpgme")]
fn default_lookup_mechanism() -> melib::gpgme::LocateKey {
    melib::gpgme::LocateKey::LOCAL | melib::gpgme::LocateKey::WKD
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
