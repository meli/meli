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

use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub const VARLINK_SERVICE: &str = r#"# The Varlink Service Interface is provided by every varlink service. It
# describes the service and the interfaces it implements.
interface org.varlink.service

# Get a list of all the interfaces a service provides and information
# about the implementation.
method GetInfo() -> (
  vendor: string,
  product: string,
  version: string,
  url: string,
  interfaces: []string
)

# Get the description of an interface that is implemented by this service.
method GetInterfaceDescription(interface: string) -> (description: string)

# The requested interface was not found.
error InterfaceNotFound (interface: string)

# The requested method was not found
error MethodNotFound (method: string)

# The interface defines the requested method, but the service does not
# implement it.
error MethodNotImplemented (method: string)

# One of the passed parameters is invalid.
error InvalidParameter (parameter: string)
"#;

pub const MELI: &str = r#"interface org.meli-email.meli

type VersionIdentifier (
  major: int,
  minor: int,
  patch: int,
  pre: ?string
)

method ServerVersion() -> (server_version: VersionIdentifier)

method Command(client_id: ?string, command: string) -> (success: bool)

type Key (
  Backspace,
  Left,
  Right,
  Up,
  Down,
  Home,
  End,
  PageUp,
  PageDown,
  Delete,
  Insert,
  F1,
  F2,
  F3,
  F4,
  F5,
  F6,
  F7,
  F8,
  F9,
  F10,
  F11,
  F12,
  Null,
  Esc
)

type Modifier (
  Alt,
  Ctrl
)

method InputKey(client_id: ?string, key: Key) -> (success: bool)
method InputCharacter(client_id: ?string, character: string, modifier: ?Modifier) -> (success: bool)
method InputPaste(client_id: ?string, paste: string) -> (success: bool)
"#;

macro_rules! impl_method_type {
    ($t:tt, $s:literal) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
        pub struct $t;

        impl $t {
            pub const NAME: &str = $s;
        }

        impl Serialize for $t {
            fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                serializer.serialize_str($s)
            }
        }

        impl<'de> Deserialize<'de> for $t {
            fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let s = <String>::deserialize(deserializer)?;
                if s != $s {
                    return Err(serde::de::Error::custom(format!(
                        concat!(r#"expected method ""#, $s, ", found `{}`"),
                        s
                    )));
                }
                Ok(Self)
            }
        }
    };
}

impl_method_type! { CommandType, "org.meli-email.meli.Command" }
impl_method_type! { ServerVersionType, "org.meli-email.meli.ServerVersion" }
impl_method_type! { InputKeyType, "org.meli-email.meli.InputKey" }
impl_method_type! { InputCharacterType, "org.meli-email.meli.InputCharacter" }
impl_method_type! { InputPasteType, "org.meli-email.meli.InputPaste" }

impl_method_type! { GetInfoType, "org.varlink.service.GetInfo" }
impl_method_type! { GetInterfaceDescriptionType, "org.varlink.service.GetInterfaceDescription" }

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Copy, Clone, Debug, Eq, PartialEq)]
pub enum Modifier {
    Alt,
    Ctrl,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Copy, Clone, Debug, Eq, PartialEq)]
pub enum Key {
    Backspace,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Delete,
    Insert,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    Null,
    Esc,
}

impl From<Key> for crate::terminal::Key {
    fn from(key: Key) -> Self {
        match key {
            Key::Backspace => Self::Backspace,
            Key::Left => Self::Left,
            Key::Right => Self::Right,
            Key::Up => Self::Up,
            Key::Down => Self::Down,
            Key::Home => Self::Home,
            Key::End => Self::End,
            Key::PageUp => Self::PageUp,
            Key::PageDown => Self::PageDown,
            Key::Delete => Self::Delete,
            Key::Insert => Self::Insert,
            Key::F1 => Self::F(1),
            Key::F2 => Self::F(2),
            Key::F3 => Self::F(3),
            Key::F4 => Self::F(4),
            Key::F5 => Self::F(5),
            Key::F6 => Self::F(6),
            Key::F7 => Self::F(7),
            Key::F8 => Self::F(8),
            Key::F9 => Self::F(9),
            Key::F10 => Self::F(10),
            Key::F11 => Self::F(11),
            Key::F12 => Self::F(12),
            Key::Null => Self::Null,
            Key::Esc => Self::Esc,
        }
    }
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct VarlinkError {
    pub error: String,
    pub parameters: serde_json::Value,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct VersionIdentifier {
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pre: Option<String>,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct CommandRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    pub command: String,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct InputKeyRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    pub key: Key,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct InputCharacterRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    pub character: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub modifier: Option<Modifier>,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct InputPasteRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    pub paste: String,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct GetInfoResponse {
    pub vendor: String,
    pub product: String,
    pub version: String,
    pub url: String,
    pub interfaces: Vec<String>,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct GetInterfaceDescriptionRequest {
    pub interface: String,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(deny_unknown_fields)]
pub struct MethodRequest {
    pub method: String,
    #[serde(default)]
    pub parameters: serde_json::Value,
    #[serde(default)]
    pub oneway: bool,
    #[serde(default)]
    pub more: bool,
    #[serde(default)]
    pub upgrade: bool,
}
