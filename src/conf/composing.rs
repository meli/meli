/*
 * meli - conf module
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

//! Configuration for composing email.
use super::default_vals::{ask, false_val, none, true_val};
use melib::ToggleFlag;
use std::collections::HashMap;

/// Settings for writing and sending new e-mail
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub struct ComposingSettings {
    /// A command to pipe new emails to
    /// Required
    pub send_mail: SendMail,
    /// Command to launch editor. Can have arguments. Draft filename is given as the last argument. If it's missing, the environment variable $EDITOR is looked up.
    #[serde(
        default = "none",
        alias = "editor-command",
        alias = "editor-cmd",
        alias = "editor_cmd"
    )]
    pub editor_command: Option<String>,
    /// Embed editor (for terminal interfaces) instead of forking and waiting.
    #[serde(default = "false_val")]
    pub embed: bool,
    /// Set "format=flowed" in plain text attachments.
    /// Default: true
    #[serde(default = "true_val", alias = "format-flowed")]
    pub format_flowed: bool,
    ///Set User-Agent
    ///Default: empty
    #[serde(default = "true_val", alias = "insert_user_agent")]
    pub insert_user_agent: bool,
    /// Set default header values for new drafts
    /// Default: empty
    #[serde(default, alias = "default-header-values")]
    pub default_header_values: HashMap<String, String>,
    /// Store sent mail after successful submission. This setting is meant to be disabled for
    /// non-standard behaviour in gmail, which auto-saves sent mail on its own.
    /// Default: true
    #[serde(default = "true_val")]
    pub store_sent_mail: bool,
    /// The attribution line appears above the quoted reply text.
    /// The format specifiers for the replied address are:
    /// - `%+f` — the sender's name and email address.
    /// - `%+n` — the sender's name (or email address, if no name is included).
    /// - `%+a` — the sender's email address.
    /// The format string is passed to strftime(3) with the replied envelope's date.
    /// Default: "On %a, %0e %b %Y %H:%M, %+f wrote:%n"
    #[serde(default = "none")]
    pub attribution_format_string: Option<String>,
    /// Whether the strftime call for the attribution string uses the POSIX locale instead of
    /// the user's active locale
    /// Default: true
    #[serde(default = "true_val")]
    pub attribution_use_posix_locale: bool,
    /// Forward emails as attachment? (Alternative is inline)
    /// Default: ask
    #[serde(default = "ask", alias = "forward-as-attachment")]
    pub forward_as_attachment: ToggleFlag,
    /// Alternative lists of reply prefixes (etc. ["Re:", "RE:", ...]) to strip
    /// Default: `["Re:", "RE:", "Fwd:", "Fw:", "回复:", "回覆:", "SV:", "Sv:", "VS:", "Antw:", "Doorst:", "VS:", "VL:", "REF:", "TR:", "TR:", "AW:", "WG:", "ΑΠ:", "Απ:", "απ:", "ΠΡΘ:", "Πρθ:", "πρθ:", "ΣΧΕΤ:", "Σχετ:", "σχετ:", "ΠΡΘ:", "Πρθ:", "πρθ:", "Vá:", "Továbbítás:", "R:", "I:", "RIF:", "FS:", "BLS:", "TRS:", "VS:", "VB:", "RV:", "RES:", "Res", "ENC:", "Odp:", "PD:", "YNT:", "İLT:", "ATB:", "YML:"]`
    #[serde(default, alias = "reply-prefix-list-to-strip")]
    pub reply_prefix_list_to_strip: Option<Vec<String>>,
    /// The prefix to use in reply subjects. The de facto prefix is "Re:".
    #[serde(default = "res", alias = "reply-prefix")]
    pub reply_prefix: String,
}

impl Default for ComposingSettings {
    fn default() -> Self {
        ComposingSettings {
            send_mail: SendMail::ShellCommand("/bin/false".into()),
            editor_command: None,
            embed: false,
            format_flowed: true,
            insert_user_agent: true,
            default_header_values: HashMap::default(),
            store_sent_mail: true,
            attribution_format_string: None,
            attribution_use_posix_locale: true,
            forward_as_attachment: ToggleFlag::Ask,
            reply_prefix_list_to_strip: None,
            reply_prefix: res(),
        }
    }
}

fn res() -> String {
    "Re:".to_string()
}

macro_rules! named_unit_variant {
    ($variant:ident) => {
        pub mod $variant {
            pub fn serialize<S>(serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_str(stringify!($variant))
            }

            pub fn deserialize<'de, D>(deserializer: D) -> Result<(), D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct V;
                impl<'de> serde::de::Visitor<'de> for V {
                    type Value = ();
                    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        f.write_str(concat!("\"", stringify!($variant), "\""))
                    }
                    fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<Self::Value, E> {
                        if value == stringify!($variant) {
                            Ok(())
                        } else {
                            Err(E::invalid_value(serde::de::Unexpected::Str(value), &self))
                        }
                    }
                }
                deserializer.deserialize_str(V)
            }
        }
    };
}

mod strings {
    named_unit_variant!(server_submission);
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum SendMail {
    #[cfg(feature = "smtp")]
    Smtp(melib::smtp::SmtpServerConf),
    #[serde(with = "strings::server_submission")]
    ServerSubmission,
    ShellCommand(String),
}
