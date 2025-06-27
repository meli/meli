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
#[derive(Debug)]
pub struct JSContactVersion1;
impl JSContactVersion for JSContactVersion1 {}

pub struct CardDeserializer;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JSContact<T: JSContactVersion>(
    json_types::JsonCardValue,
    std::marker::PhantomData<*const T>,
);

impl<V: JSContactVersion> JSContact<V> {
    pub fn new_v1() -> JSContact<impl JSContactVersion> {
        JSContact(
            json_types::JsonCardValue::default(),
            std::marker::PhantomData::<*const JSContactVersion1>,
        )
    }
}

impl CardDeserializer {
    pub fn try_from_str(input: &str) -> Result<JSContact<JSContactVersion1>> {
        let inner: json_types::JsonCardValue = serde_json::from_str(input)?;

        Ok(JSContact(inner, std::marker::PhantomData))
    }
}

impl<V: JSContactVersion> TryInto<Card> for JSContact<V> {
    type Error = crate::error::Error;

    fn try_into(self) -> Result<Card> {
        let json_types::JsonCardValue {
            uid, name, email, ..
        } = self.0;
        let mut card = Card::new();
        card.set_id(CardId::from(uid));
        if let Some(name) = name.full {
            card.set_name(name);
        }
        if let Some(e) = email.get_index(0) {
            card.set_email(e.1.address.to_string());
        }

        Ok(card)
    }
}

impl From<json_types::JsonCardValue> for JSContact<JSContactVersion1> {
    fn from(val: json_types::JsonCardValue) -> Self {
        Self(val, std::marker::PhantomData::<*const JSContactVersion1>)
    }
}

pub mod json_types {
    use indexmap::IndexMap;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    use crate::utils::datetime::UnixTimestamp;

    macro_rules! impl_json_type_struct_serde {
        ($t:tt, $s:literal) => {
            #[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
            struct $t;

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
                    let s = <&'de str>::deserialize(deserializer)?;
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

    impl_json_type_struct_serde! {JsonCardType, "Card"}
    impl_json_type_struct_serde! {JsonNameType, "Name"}
    impl_json_type_struct_serde! {JsonEmailAddressType, "EmailAddress"}

    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    #[non_exhaustive]
    pub enum JsonCardVersion {
        #[default]
        _1_0,
    }

    impl Serialize for JsonCardVersion {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str("1.0")
        }
    }

    impl<'de> Deserialize<'de> for JsonCardVersion {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let s = <&'de str>::deserialize(deserializer)?;
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

            let s = <&'de str>::deserialize(deserializer)?;
            let Ok((_, val)) = parse_timestamp_from_string(s, RFC3339_DATETIME_Z) else {
                return Err(serde::de::Error::custom(format!(
                    r#"expected UTCDateTime value, found `{s}`"#
                )));
            };
            Ok(Self(val))
        }
    }

    #[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum JsonCardKind {
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

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct JsonCardValue {
        #[serde(rename = "@type")]
        __type: JsonCardType,
        pub version: JsonCardVersion,
        pub uid: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub created: Option<UTCDateTime>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub updated: Option<UTCDateTime>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The language tag, as defined in `RFC5646`, that best describes the
        /// language used for text in the card, optionally including
        /// additional information such as the script.
        pub language: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub kind: Option<JsonCardKind>,
        pub name: JsonCardName,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub email: IndexMap<String, JsonCardEmailAddress>,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct JsonCardName {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        __type: Option<JsonNameType>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub components: Vec<()>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub is_ordered: Option<bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub default_separator: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub full: Option<String>,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct JsonCardEmailAddress {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        __type: Option<JsonEmailAddressType>,
        pub address: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pref: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub label: Option<String>,
    }

    #[test]
    fn test_addressbook_jscontact() {
        use super::JSContactVersion1;
        use crate::contacts::{jscontact::JSContact, Card, CardId};

        assert_eq!(
            JsonCardValue {
                __type: JsonCardType,
                version: JsonCardVersion::_1_0,
                uid: "22B2C7DF-9120-4969-8460-05956FE6B065".to_string(),
                created: None,
                updated: None,
                language: None,
                kind: Some(JsonCardKind::Individual),
                name: JsonCardName {
                    __type: None,
                    components: vec![],
                    full: Some("full_name".to_string()),
                    is_ordered: Some(true),
                    default_separator: None,
                },
                email: indexmap! {
                    "main".to_string() => JsonCardEmailAddress {
                        __type: None,
                        address: "user@example.com".to_string(),
                        contexts: IndexMap::new(),
                        pref:None,
                        label: None,
                    }
                }
            },
            serde_json::from_str(
                r#"{
            "@type": "Card",
            "version": "1.0",
            "uid": "22B2C7DF-9120-4969-8460-05956FE6B065",
            "kind": "individual",
            "email": {
                "main": {
                    "address": "user@example.com"
                }
            },
            "name": {
                "components": [],
                "full": "full_name",
                "isOrdered": true
            }
        }"#
            )
            .unwrap(),
        );
        assert_eq!(
            Card {
                last_edited: 1727155810,
                ..<JSContact<JSContactVersion1> as std::convert::TryInto<Card>>::try_into(
                    JSContact::<JSContactVersion1>::from(
                        serde_json::from_str::<JsonCardValue>(
                            r#"{
            "@type": "Card",
            "version": "1.0",
            "uid": "22B2C7DF-9120-4969-8460-05956FE6B065",
            "kind": "individual",
            "email": {
                "main": {
                    "address": "user@example.com"
                }
            },
            "name": {
                "components": [],
                "full": "full_name",
                "isOrdered": true
            }
        }"#
                        )
                        .unwrap()
                    )
                )
                .unwrap()
            },
            Card {
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
