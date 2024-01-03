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
use std::collections::HashMap;

use melib::{email::HeaderName, ToggleFlag};
use serde::{de, Deserialize, Deserializer};

use super::{
    default_vals::{ask, false_val, none, true_val},
    deserializers::non_empty_string,
};

/// Settings for writing and sending new e-mail
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ComposingSettings {
    /// A command to pipe new emails to
    /// Required
    pub send_mail: SendMail,
    /// Command to launch editor. Can have arguments. Draft filename is given as
    /// the last argument. If it's missing, the environment variable $EDITOR is
    /// looked up.
    #[serde(
        default = "none",
        alias = "editor-command",
        alias = "editor-cmd",
        alias = "editor_cmd"
    )]
    pub editor_command: Option<String>,
    /// Embedded editor (for terminal interfaces) instead of forking and
    /// waiting.
    #[serde(default = "false_val", alias = "embed")]
    pub embedded_pty: bool,
    /// Set "format=flowed" in plain text attachments.
    /// Default: true
    #[serde(default = "true_val", alias = "format-flowed")]
    pub format_flowed: bool,
    /// Set User-Agent
    /// Default: empty
    #[serde(default = "true_val", alias = "insert_user_agent")]
    pub insert_user_agent: bool,
    /// Set default header values for new drafts
    /// Default: empty
    #[serde(default, alias = "default-header-values")]
    pub default_header_values: HashMap<HeaderName, String>,
    /// Wrap header preamble when editing a draft in an editor. This allows you
    /// to write non-plain text email without the preamble creating syntax
    /// errors. They are stripped when you return from the editor. The
    /// values should be a two element array of strings, a prefix and suffix.
    /// Default: None
    #[serde(default, alias = "wrap-header-preamble")]
    pub wrap_header_preamble: Option<(String, String)>,
    /// Store sent mail after successful submission. This setting is meant to be
    /// disabled for non-standard behaviour in gmail, which auto-saves sent
    /// mail on its own. Default: true
    #[serde(default = "true_val")]
    pub store_sent_mail: bool,
    /// The attribution line appears above the quoted reply text.
    /// The format specifiers for the replied address are:
    /// - `%+f` — the sender's name and email address.
    /// - `%+n` — the sender's name (or email address, if no name is included).
    /// - `%+a` — the sender's email address.
    /// The format string is passed to strftime(3) with the replied envelope's
    /// date. Default: "On %a, %0e %b %Y %H:%M, %+f wrote:%n"
    #[serde(default = "none")]
    pub attribution_format_string: Option<String>,
    /// Whether the strftime call for the attribution string uses the POSIX
    /// locale instead of the user's active locale
    /// Default: true
    #[serde(default = "true_val")]
    pub attribution_use_posix_locale: bool,
    /// Forward emails as attachment? (Alternative is inline)
    /// Default: ask
    #[serde(default = "ask", alias = "forward-as-attachment")]
    pub forward_as_attachment: ToggleFlag,
    /// Alternative lists of reply prefixes (etc. ["Re:", "RE:", ...]) to strip
    /// Default: `["Re:", "RE:", "Fwd:", "Fw:", "回复:", "回覆:", "SV:", "Sv:",
    /// "VS:", "Antw:", "Doorst:", "VS:", "VL:", "REF:", "TR:", "TR:", "AW:",
    /// "WG:", "ΑΠ:", "Απ:", "απ:", "ΠΡΘ:", "Πρθ:", "πρθ:", "ΣΧΕΤ:", "Σχετ:",
    /// "σχετ:", "ΠΡΘ:", "Πρθ:", "πρθ:", "Vá:", "Továbbítás:", "R:", "I:",
    /// "RIF:", "FS:", "BLS:", "TRS:", "VS:", "VB:", "RV:", "RES:", "Res",
    /// "ENC:", "Odp:", "PD:", "YNT:", "İLT:", "ATB:", "YML:"]`
    #[serde(default, alias = "reply-prefix-list-to-strip")]
    pub reply_prefix_list_to_strip: Option<Vec<String>>,
    /// The prefix to use in reply subjects. The de facto prefix is "Re:".
    #[serde(default = "res", alias = "reply-prefix")]
    pub reply_prefix: String,
    /// Custom `compose-hooks`.
    #[serde(default, alias = "custom-compose-hooks")]
    pub custom_compose_hooks: Vec<ComposeHook>,
    /// Disabled `compose-hooks`.
    #[serde(default, alias = "disabled-compose-hooks")]
    pub disabled_compose_hooks: Vec<String>,
}

impl Default for ComposingSettings {
    fn default() -> Self {
        Self {
            send_mail: SendMail::ShellCommand("false".into()),
            editor_command: None,
            embedded_pty: false,
            format_flowed: true,
            insert_user_agent: true,
            default_header_values: HashMap::default(),
            store_sent_mail: true,
            wrap_header_preamble: None,
            attribution_format_string: None,
            attribution_use_posix_locale: true,
            forward_as_attachment: ToggleFlag::Ask,
            reply_prefix_list_to_strip: None,
            reply_prefix: res(),
            custom_compose_hooks: vec![],
            disabled_compose_hooks: vec![],
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

pub mod strings {
    named_unit_variant!(server_submission);
}

#[derive(Clone, Debug, Serialize)]
#[serde(untagged)]
pub enum SendMail {
    #[cfg(feature = "smtp")]
    Smtp(melib::smtp::SmtpServerConf),
    #[serde(with = "strings::server_submission")]
    ServerSubmission,
    ShellCommand(String),
}

/// Shell command compose hooks (See
/// [`crate::mail::compose::hooks::Hook`])
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ComposeHook {
    #[serde(deserialize_with = "non_empty_string")]
    name: String,
    #[serde(deserialize_with = "non_empty_string")]
    command: String,
}

impl From<ComposeHook> for crate::mail::hooks::Hook {
    fn from(c: ComposeHook) -> Self {
        Self::new_shell_command(c.name.into(), c.command)
    }
}
const SENDMAIL_ERR_HELP: &str = r#"Invalid `send_mail` value.

Here are some valid examples:

Use server submission in protocols that support it (JMAP, NNTP)
===============================================================

    send_mail = "server_submission"

Using a shell script
====================

    send_mail = "msmtp --read-recipients --read-envelope-from"

Direct SMTP connection
======================

    send_mail = { hostname = "mail.example.com", port = 587, auth = { type = "auto", password = { type = "raw", value = "hunter2" } }, security = { type = "STARTTLS" } }

    [composing.send_mail]
    hostname = "mail.example.com"
    port = 587
    auth = { type = "auto", password = { type = "command_eval", value = "/path/to/password_script.sh" } }
    security = { type = "TLS", danger_accept_invalid_certs = true } }


`send_mail` direct SMTP connection fields:
    - hostname: text
    - port: valid port number
    - envelope_from: text (optional, default is empty),
    - auth: ...
    - security: ... (optional, default is "auto")
    - extensions: ... (optional, default is PIPELINING, CHUNKING, PRDR, 8BITMIME, BINARYMIME, SMTPUTF8, AUTH and DSN_NOTIFY)

Possible values for `send_mail.auth`:

    No authentication:

        auth = { type = "none" }

    Regular authentication:
    Note: `require_auth` and `auth_type` are optional and can be skipped.

        auth = { type = "auto", username = "...", password = "...", require_auth = true, auth_type = ... }

        password can be:
            password = { type = "raw", value = "..." }
            password = { type = "command_eval", value = "/path/to/password_script.sh" }

    XOAuth2 authentication:
    Note: `require_auth` is optional and can be skipped.
        auth = { type = "xoauth2", token_command = "...", require_auth = true }

Possible values for `send_mail.auth.auth_type` when `auth.type` is "auto":

    auth_type = { plain = false, login = true }

Possible values for `send_mail.security`:
Note that in all cases field `danger_accept_invalid_certs` is optional and its default value is false.

    security = "none"
    security = { type = "auto", danger_accept_invalid_certs = false }
    security = { type = "STARTTLS", danger_accept_invalid_certs = false }
    security = { type = "TLS", danger_accept_invalid_certs = false }

Possible values for `send_mail.extensions` (All optional and have default values `true`:
    pipelining
    chunking
    8bitmime
    prdr
    binarymime
    smtputf8
    auth
    dsn_notify: Array of options e.g. ["FAILURE"]
"#;

impl<'de> Deserialize<'de> for SendMail {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum SendMailInner {
            #[cfg(feature = "smtp")]
            Smtp(melib::smtp::SmtpServerConf),
            #[serde(with = "strings::server_submission")]
            ServerSubmission,
            ShellCommand(String),
        }

        match <SendMailInner>::deserialize(deserializer) {
            #[cfg(feature = "smtp")]
            Ok(SendMailInner::Smtp(v)) => Ok(Self::Smtp(v)),
            Ok(SendMailInner::ServerSubmission) => Ok(Self::ServerSubmission),
            Ok(SendMailInner::ShellCommand(v)) => Ok(Self::ShellCommand(v)),
            Err(_err) => Err(de::Error::custom(SENDMAIL_ERR_HELP)),
        }
    }
}
