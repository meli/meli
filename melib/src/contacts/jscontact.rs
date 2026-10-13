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

use indexmap::IndexMap;

use crate::{
    contacts::{Card, CardId},
    error::Result,
};

/// Supported `JSContact` versions
pub trait JSContactVersion: std::fmt::Debug {}

#[derive(Debug)]
pub struct JSContactVersionUnknown;
impl JSContactVersion for JSContactVersionUnknown {}

/// Version 1 <https://www.rfc-editor.org/rfc/rfc9553.html>
#[derive(Clone, Debug)]
pub struct JSContactVersion1;
impl JSContactVersion for JSContactVersion1 {}

pub struct CardDeserializer;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JSContact<T: JSContactVersion>(
    pub json_types::Card,
    pub std::marker::PhantomData<fn() -> T>,
);

impl<V: JSContactVersion> JSContact<V> {
    pub fn new_v1() -> JSContact<impl JSContactVersion> {
        JSContact(
            json_types::Card::default(),
            std::marker::PhantomData::<fn() -> JSContactVersion1>,
        )
    }
}

impl CardDeserializer {
    pub fn try_from_str(input: &str) -> Result<JSContact<JSContactVersion1>> {
        let inner: json_types::Card = serde_json::from_str(input)?;

        Ok(JSContact(inner, std::marker::PhantomData))
    }
}

impl<V: JSContactVersion> From<JSContact<V>> for Card {
    fn from(val: JSContact<V>) -> Self {
        let json_types::Card {
            uid, name, emails, ..
        } = val.0;
        let mut card = Self::new();
        card.set_id(CardId::from(uid));
        if let Some(name) = name.full {
            card.set_name(name);
        } else if name.is_ordered {
            card.set_name(
                name.components
                    .iter()
                    .map(|c| c.value.as_str())
                    .collect::<Vec<_>>()
                    .join(name.default_separator.as_deref().unwrap_or(" ")),
            );
        } else {
            use json_types::NameComponentKind::*;

            let mut name_s = String::new();
            let sep = name.default_separator.as_deref().unwrap_or(" ");
            for kind in [
                Title, Given, Given2, Surname, Surname2, Credential, Generation,
            ] {
                if let Some(c) = name.components.iter().find(|c| c.kind == kind) {
                    if !name_s.is_empty() {
                        name_s.push_str(sep);
                    }
                    name_s.push_str(&c.value)
                }
            }
            card.set_name(name_s);
        }
        if let Some(e) = emails.get_index(0) {
            card.set_email(e.1.address.to_string());
        }

        card
    }
}

impl From<Card> for JSContact<JSContactVersion1> {
    fn from(card: Card) -> Self {
        let mut ret = Self(
            json_types::Card::default(),
            std::marker::PhantomData::<fn() -> JSContactVersion1>,
        );
        if let CardId::Uuid(uid) = card.id {
            ret.0.uid = uid.to_string();
        }
        ret.0.name.full = Some(card.name);
        ret.0.emails.insert(
            "main".into(),
            json_types::EmailAddress {
                __type: None,
                address: card.email,
                contexts: IndexMap::new(),
                pref: None,
                label: None,
            },
        );

        ret
    }
}

impl From<json_types::Card> for JSContact<JSContactVersion1> {
    fn from(val: json_types::Card) -> Self {
        Self(val, std::marker::PhantomData::<fn() -> JSContactVersion1>)
    }
}

impl serde::Serialize for JSContact<JSContactVersion1> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for JSContact<JSContactVersion1> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = <json_types::Card>::deserialize(deserializer)?;
        Ok(Self(s, std::marker::PhantomData))
    }
}

pub mod json_types {
    use std::str::FromStr;

    use indexmap::IndexMap;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    use crate::utils::datetime::UnixTimestamp;

    macro_rules! impl_json_type_struct_serde {
        ($t:tt, $s:literal) => {
            #[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
            pub struct $t;

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
                            concat!(r#"expected @type value ""#, $s, ", found `{}`"),
                            s
                        )));
                    }
                    Ok(Self)
                }
            }
        };
    }

    impl_json_type_struct_serde! {CardType, "Card"}
    impl_json_type_struct_serde! {NameType, "Name"}
    impl_json_type_struct_serde! {NameComponentType, "NameComponent"}
    impl_json_type_struct_serde! {EmailAddressType, "EmailAddress"}
    impl_json_type_struct_serde! {OnlineServiceType, "OnlineService"}
    impl_json_type_struct_serde! {CryptoKeyType, "CryptoKey"}
    impl_json_type_struct_serde! {AnniversaryType, "Anniversary"}
    impl_json_type_struct_serde! {PartialDateType, "PartialDate"}
    impl_json_type_struct_serde! {TimestampType, "Timestamp"}
    impl_json_type_struct_serde! {PersonalInfoType, "PersonalInfo"}
    impl_json_type_struct_serde! {AddressType, "Address"}
    impl_json_type_struct_serde! {SpeakToAsType, "SpeakToAs"}
    impl_json_type_struct_serde! {PronounsType, "Pronouns"}
    impl_json_type_struct_serde! {TitleType, "Title"}

    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    #[non_exhaustive]
    pub enum CardVersion {
        #[default]
        _1_0,
    }

    impl Serialize for CardVersion {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str("1.0")
        }
    }

    impl<'de> Deserialize<'de> for CardVersion {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let s = <String>::deserialize(deserializer)?;
            if s != "1.0" {
                return Err(serde::de::Error::custom(format!(
                    r#"expected version value "1.0", found `{s}`"#
                )));
            }
            Ok(Self::_1_0)
        }
    }

    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    pub struct UTCDateTime(pub UnixTimestamp);

    impl UTCDateTime {
        const fn is_none(&self) -> bool {
            matches!(self, Self(0))
        }
    }

    impl std::fmt::Display for UTCDateTime {
        fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
            use crate::utils::datetime::{formats::RFC3339_DATETIME_Z, timestamp_to_string_utc};

            timestamp_to_string_utc(self.0, Some(RFC3339_DATETIME_Z), true).fmt(fmt)
        }
    }

    impl Serialize for UTCDateTime {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            use crate::utils::datetime::{formats::RFC3339_DATETIME_Z, timestamp_to_string_utc};

            serializer.serialize_str(&timestamp_to_string_utc(
                self.0,
                Some(RFC3339_DATETIME_Z),
                true,
            ))
        }
    }

    impl<'de> Deserialize<'de> for UTCDateTime {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            use crate::utils::datetime::{
                formats::RFC3339_DATETIME_Z, parse_timestamp_from_string,
            };

            let s = <String>::deserialize(deserializer)?;
            let Ok((_, val)) = parse_timestamp_from_string(s.as_str(), RFC3339_DATETIME_Z) else {
                return Err(serde::de::Error::custom(format!(
                    r#"expected UTCDateTime value, found `{s}`"#
                )));
            };
            Ok(Self(val))
        }
    }

    #[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum CardKind {
        #[default]
        /// A single person
        Individual,
        /// A group of people or entities
        Group,
        /// an organization
        Org,
        /// A named location
        Location,
        /// A device such as an appliance, a computer, or a network element
        Device,
        /// A software application
        Application,
    }

    impl CardKind {
        const fn is_individual(&self) -> bool {
            matches!(self, Self::Individual)
        }

        pub const fn as_str(&self) -> &'static str {
            match self {
                Self::Individual => "individual",
                Self::Group => "group",
                Self::Org => "org",
                Self::Location => "location",
                Self::Device => "device",
                Self::Application => "application",
            }
        }
    }

    impl FromStr for CardKind {
        type Err = serde_json::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            serde_json::from_str(s)
        }
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Card {
        #[serde(rename = "@type")]
        pub __type: CardType,
        pub version: CardVersion,
        pub uid: String,
        #[serde(default, skip_serializing_if = "UTCDateTime::is_none")]
        pub created: UTCDateTime,
        #[serde(default, skip_serializing_if = "UTCDateTime::is_none")]
        pub updated: UTCDateTime,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        /// The language tag, as defined in `RFC5646`, that best describes the
        /// language used for text in the card, optionally including
        /// additional information such as the script.
        pub language: String,
        #[serde(default, skip_serializing_if = "CardKind::is_individual")]
        pub kind: CardKind,
        #[serde(default, skip_serializing_if = "Name::is_empty")]
        pub name: Name,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub nicknames: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub organizations: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "SpeakToAs::is_empty")]
        pub speak_to_as: SpeakToAs,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub members: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub prod_id: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub related_to: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub emails: IndexMap<String, EmailAddress>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub addresses: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub online_services: IndexMap<String, OnlineService>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub phones: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub preferred_languages: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub calendars: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub scheduling_addresses: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub crypto_keys: IndexMap<String, CryptoKey>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub directories: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub links: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub media: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub localizations: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub anniversaries: IndexMap<String, Anniversary>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub keywords: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub notes: IndexMap<String, serde_json::Value>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub personal_info: IndexMap<String, PersonalInfo>,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    #[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum NameComponentKind {
        Title,
        Given,
        Given2,
        Surname,
        Surname2,
        Credential,
        Generation,
        Separator,
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct NameComponent {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<NameComponentType>,
        pub value: String,
        pub kind: NameComponentKind,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub phonetic: String,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct Name {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<NameType>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub components: Vec<NameComponent>,
        #[serde(default)]
        pub is_ordered: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub default_separator: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub full: Option<String>,
    }

    impl Name {
        const fn is_empty(&self) -> bool {
            self.components.is_empty() && self.full.is_none()
        }
    }

    impl serde::Serialize for Name {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            use serde::ser::SerializeMap as _;

            let map_len = usize::from(self.__type.is_some())
                + usize::from(!self.components.is_empty()) * 2
                + usize::from(self.default_separator.is_some())
                + usize::from(self.full.is_some());
            let mut map = serializer.serialize_map(Some(map_len))?;
            if let Some(__type) = &self.__type {
                map.serialize_entry("@type", __type)?;
            }
            if !self.components.is_empty() {
                map.serialize_entry("components", &self.components)?;
                map.serialize_entry("isOrdered", &self.is_ordered)?;
            }
            if let Some(default_separator) = &self.default_separator {
                map.serialize_entry("defaultSeparator", default_separator)?;
            }
            if let Some(full) = &self.full {
                map.serialize_entry("full", full)?;
            }
            map.end()
        }
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct EmailAddress {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<EmailAddressType>,
        pub address: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pref: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub label: Option<String>,
    }

    #[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum GrammaticalGender {
        #[default]
        None,
        Animate,
        Common,
        Feminine,
        Inanimate,
        Masculine,
        Neuter,
    }

    impl GrammaticalGender {
        const fn is_none(&self) -> bool {
            matches!(self, Self::None)
        }
    }

    #[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct SpeakToAs {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<SpeakToAsType>,
        #[serde(default, skip_serializing_if = "GrammaticalGender::is_none")]
        pub grammatical_gender: GrammaticalGender,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub pronouns: IndexMap<String, Pronouns>,
    }

    impl SpeakToAs {
        fn is_empty(&self) -> bool {
            self.grammatical_gender.is_none() && self.pronouns.is_empty()
        }
    }

    #[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct Pronouns {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<PronounsType>,
        pub pronouns: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pref: Option<u64>,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct OnlineService {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<OnlineServiceType>,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub service: String,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub uri: String,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub user: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pref: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub label: Option<String>,
    }

    /// Abstract type for resource properties
    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Resource {
        pub uri: String,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub media_type: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pref: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub label: Option<String>,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct CryptoKey {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<CryptoKeyType>,
        #[serde(flatten)]
        pub resource: Resource,
    }

    const fn is_zero(n: &u32) -> bool {
        *n == 0
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct PartialDate {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<PartialDateType>,
        #[serde(default, skip_serializing_if = "is_zero")]
        pub year: u32,
        #[serde(default, skip_serializing_if = "is_zero")]
        pub month: u32,
        #[serde(default, skip_serializing_if = "is_zero")]
        pub day: u32,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub calendar_scale: String,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Timestamp {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<TimestampType>,
        pub utc: UTCDateTime,
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(untagged)]
    pub enum AnniversaryDate {
        Partial(PartialDate),
        Timestamp(Timestamp),
    }

    #[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum AnniversaryKind {
        Birth,
        Death,
        Wedding,
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Anniversary {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<AnniversaryType>,
        pub kind: AnniversaryKind,
        pub date: AnniversaryDate,
        #[serde(default, skip_serializing_if = "serde_json::Value::is_null")]
        pub place: serde_json::Value,
    }

    #[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum PersonalInfoKind {
        Expertise,
        Hobby,
        Interest,
    }

    #[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum PersonalInfoLevel {
        #[default]
        None,
        High,
        Medium,
        Low,
    }

    impl PersonalInfoLevel {
        const fn is_none(&self) -> bool {
            matches!(self, Self::None)
        }
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct PersonalInfo {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<PersonalInfoType>,
        pub kind: PersonalInfoKind,
        pub value: String,
        #[serde(default, skip_serializing_if = "PersonalInfoLevel::is_none")]
        pub level: PersonalInfoLevel,
        #[serde(default, skip_serializing_if = "is_zero")]
        pub list_as: u32,
    }

    #[test]
    fn test_addressbook_jscontact() {
        use super::JSContactVersion1;
        use crate::contacts::{jscontact::JSContact, Card as MeliCard, CardId};

        assert_eq!(
            Card {
                __type: CardType,
                version: CardVersion::_1_0,
                uid: "22B2C7DF-9120-4969-8460-05956FE6B065".to_string(),
                kind: CardKind::Individual,
                name: Name {
                    __type: None,
                    components: vec![],
                    full: Some("full_name".to_string()),
                    is_ordered: false,
                    default_separator: None,
                },
                emails: indexmap! {
                    "main".to_string() => EmailAddress {
                        __type: None,
                        address: "user@example.com".to_string(),
                        contexts: IndexMap::new(),
                        pref:None,
                        label: None,
                    }
                },
                ..Card::default()
            },
            serde_json::from_str(
                r#"{
            "@type": "Card",
            "version": "1.0",
            "uid": "22B2C7DF-9120-4969-8460-05956FE6B065",
            "kind": "individual",
            "emails": {
                "main": {
                    "address": "user@example.com"
                }
            },
            "name": {
                "components": [],
                "full": "full_name",
                "isOrdered": false
            }
        }"#
            )
            .unwrap(),
        );
        assert_eq!(
            MeliCard {
                last_edited: 1727155810,
                ..JSContact::<JSContactVersion1>::from(
                    serde_json::from_str::<Card>(
                        r#"{
            "@type": "Card",
            "version": "1.0",
            "uid": "22B2C7DF-9120-4969-8460-05956FE6B065",
            "kind": "individual",
            "emails": {
                "main": {
                    "address": "user@example.com"
                }
            },
            "name": {
                "components": [],
                "full": "full_name",
                "isOrdered": false
            }
        }"#
                    )
                    .unwrap()
                )
                .into()
            },
            MeliCard {
                id: CardId::Uuid(
                    uuid::Uuid::try_parse("22B2C7DF-9120-4969-8460-05956FE6B065").unwrap()
                ),
                title: "".into(),
                name: "full_name".into(),
                additionalname: "".into(),
                name_prefix: "".into(),
                name_suffix: "".into(),
                birthday: None,
                email: "user@example.com".into(),
                url: "".into(),
                key: "".into(),
                color: 0,
                last_edited: 1727155810,
                extra_properties: indexmap::indexmap! {},
                external_resource: false
            },
        );
    }
}
