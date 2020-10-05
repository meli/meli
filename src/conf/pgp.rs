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

use super::default_vals::*;

/// Settings for digital signing and encryption
#[derive(Debug, Deserialize, Clone, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PGPSettings {
    /// auto verify signed e-mail according to RFC3156
    #[serde(default = "true_val", alias = "auto-verify-signatures")]
    pub auto_verify_signatures: bool,

    /// auto decrypt encrypted e-mail
    #[serde(default = "true_val", alias = "auto-decrypt")]
    pub auto_decrypt: bool,

    /// always sign sent messages
    #[serde(default = "false_val", alias = "auto-sign")]
    pub auto_sign: bool,

    // https://tools.ietf.org/html/rfc4880#section-12.2
    #[serde(default = "none", alias = "sign-key")]
    pub sign_key: Option<String>,

    #[serde(default = "none", alias = "decrypt-key")]
    pub decrypt_key: Option<String>,

    #[serde(default = "none", alias = "encrypt-key")]
    pub encrypt_key: Option<String>,

    /// gpg binary name or file location to use
    #[serde(default, alias = "gpg-binary")]
    pub gpg_binary: Option<String>,
}

impl Default for PGPSettings {
    fn default() -> Self {
        PGPSettings {
            auto_verify_signatures: true,
            auto_decrypt: true,
            auto_sign: false,
            sign_key: None,
            decrypt_key: None,
            encrypt_key: None,
            gpg_binary: None,
        }
    }
}
