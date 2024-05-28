//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use serde::{de::DeserializeOwned, ser::Serialize};

pub trait Capability:
    Clone
    + Copy
    + std::fmt::Debug
    + PartialEq
    + Eq
    + std::hash::Hash
    + Serialize
    + DeserializeOwned
    + Send
    + Sync
{
    const URI: &'static str;
    const NAME: &'static str;
}

#[macro_export]
macro_rules! _impl_jmap_capability {
    ($(#[$outer:meta])*$ident:ident : $key:literal, name: $name:literal) => {
        $(#[$outer])*
            #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
            pub struct $ident;

        impl $crate::jmap::capabilities::Capability for $ident {
            const URI: &'static str = $key;
            const NAME: &'static str = $name;
        }

        impl $ident {
            pub const fn uri() -> &'static str {
                <Self as $crate::jmap::capabilities::Capability>::URI
            }

            pub const fn name() -> &'static str {
                <Self as $crate::jmap::capabilities::Capability>::NAME
            }
        }

        impl ::serde::ser::Serialize for $ident {
            fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
            where
                S: ::serde::ser::Serializer,
            {
                serializer.serialize_str(<Self as $crate::jmap::capabilities::Capability>::URI)
            }
        }

        impl<'de> ::serde::de::Deserialize<'de> for $ident {
            fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
            where
                D: ::serde::de::Deserializer<'de>,
            {
                if <&'_ str>::deserialize(deserializer)? == <Self as $crate::jmap::capabilities::Capability>::URI {
                    return Ok(Self);
                }

                Err(::serde::de::Error::custom(concat!("Expected string with value \"", $key, '"')))
            }
        }

        impl ::std::fmt::Display for $ident {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(fmt, "JMAP {} Capability", Self::name())
            }
        }
    };
}

_impl_jmap_capability! { JmapCoreCapability: "urn:ietf:params:jmap:core", name: "Core" }
