// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: Copyright 2021, 2022, 2023 chayleaf
// <chayleaf-cratesio@pavluk.org>

use std::borrow::Cow;

use super::{cow::TriCow, parse_urn, Urn, UrnSlice};

impl<'de> serde::Deserialize<'de> for UrnSlice<'de> {
    fn deserialize<D>(de: D) -> Result<Self, <D as serde::Deserializer<'de>>::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = match Cow::<str>::deserialize(de)? {
            Cow::Owned(s) => TriCow::Owned(s),
            Cow::Borrowed(s) => TriCow::Borrowed(s),
        };
        parse_urn(s).map_err(serde::de::Error::custom)
    }
}

impl serde::Serialize for UrnSlice<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> serde::Deserialize<'de> for Urn {
    fn deserialize<D>(de: D) -> Result<Self, <D as serde::Deserializer<'de>>::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[allow(clippy::redundant_clone)]
        Ok(UrnSlice::deserialize(de)?.to_owned())
    }
}

impl serde::Serialize for Urn {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}
