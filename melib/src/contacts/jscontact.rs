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

use std::borrow::Cow;

use crate::email::Address;

pub type JSContactV1 = JSContact<JSContactVersion1>;

/// Supported `JSContact` versions
pub trait JSContactVersion: std::fmt::Debug + Clone + Eq + PartialEq {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JSContactVersionUnknown;
impl JSContactVersion for JSContactVersionUnknown {}

/// Version 1 <https://www.rfc-editor.org/rfc/rfc9553.html>
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JSContactVersion1;
impl JSContactVersion for JSContactVersion1 {}

/// A generic representation of a `JSContact`.
///
/// Currently, only v1 is supported which can be accessed through the [`JSContactV1`] type alias.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JSContact<T: JSContactVersion>(
    pub json_types::Card,
    pub std::marker::PhantomData<fn() -> T>,
);

impl<V: JSContactVersion> std::ops::DerefMut for JSContact<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<V: JSContactVersion> std::ops::Deref for JSContact<V> {
    type Target = json_types::Card;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V: JSContactVersion> std::fmt::Display for JSContact<V> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.name.is_empty() && self.emails.is_empty() {
            write!(fmt, "empty contact")
        } else {
            write!(fmt, "{}", self.as_address())
        }
    }
}

impl JSContact<JSContactVersion1> {
    /// Create a new v1 `JSContact` with a random `UUIDv4` uid.
    pub fn new() -> Self {
        Self(
            json_types::Card {
                uid: uuid::Uuid::new_v4(),
                ..json_types::Card::default()
            },
            std::marker::PhantomData::<fn() -> JSContactVersion1>,
        )
    }

    /// Generate a [`CardId`] out of the [`json_types::Card::uid`] UUID field.
    #[inline(always)]
    pub fn id(&self) -> crate::contacts::CardId {
        self.0.uid.into()
    }
}

impl Default for JSContact<JSContactVersion1> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V: JSContactVersion> JSContact<V> {
    /// Creates a new version 1 `JSContact`.
    ///
    /// To create a non-generic version, use [`JSContactV1::new`].
    pub fn new_v1() -> JSContact<impl JSContactVersion> {
        JSContactV1::new()
    }

    /// Create an [`Address`] for this contact.
    pub fn as_address(&self) -> Address {
        Address::new(
            self.name().map(|n| n.into_owned()),
            self.email().unwrap_or_default().to_owned(),
        )
    }

    /// Name of this contact.
    pub fn name(&'_ self) -> Option<Cow<'_, str>> {
        if self.name.is_empty() {
            None
        } else if let Some(name) = self.name.full.as_deref() {
            Some(Cow::Borrowed(name))
        } else if self.name.is_ordered {
            Some(Cow::Owned(
                self.name
                    .components
                    .iter()
                    .map(|c| c.value.as_str())
                    .collect::<Vec<_>>()
                    .join(self.name.default_separator.as_deref().unwrap_or(" ")),
            ))
        } else {
            use json_types::NameComponentKind::*;

            let mut name_s = String::new();
            let sep = self.name.default_separator.as_deref().unwrap_or(" ");
            for kind in [
                Title, Given, Given2, Surname, Surname2, Credential, Generation,
            ] {
                if let Some(c) = self.name.components.iter().find(|c| c.kind == kind) {
                    if !name_s.is_empty() {
                        name_s.push_str(sep);
                    }
                    name_s.push_str(&c.value)
                }
            }
            Some(Cow::Owned(name_s))
        }
    }

    /// The first entry in [`Self::emails`].
    pub fn email(&'_ self) -> Option<&str> {
        let (_, email) = self.emails.first()?;
        Some(&email.address)
    }

    /// The first entry in [`Self::online_services`].
    pub fn url(&'_ self) -> Option<&str> {
        let (_, service) = self.online_services.first()?;
        Some(&service.uri)
    }

    /// Locate birthday anniversary of this contact in [`Self::anniversaries`].
    pub fn birthday(&self) -> Option<crate::UnixTimestamp> {
        use json_types::{AnniversaryDate, AnniversaryKind};
        self.anniversaries.values().find_map(|a| {
            if a.kind == AnniversaryKind::Birth {
                if let AnniversaryDate::Timestamp(ts) = &a.date {
                    Some(ts.utc.0)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    /// Serialise `self` to a v4 vCard.
    pub fn to_vcard_string(&self) -> String {
        use crate::utils::vobject::{vcard::VcardBuilder, write_component};

        let mut builder = VcardBuilder::new()
            .with_uid(self.uid.as_urn().to_string())
            .with_version("4.0".to_string());
        if let Some(email) = self.email() {
            builder = builder.with_email(email.to_owned());
        }
        if let Some(name) = self.name() {
            builder = builder.with_fullname(name.into_owned());
        }
        if let Some(url) = self.url() {
            builder = builder.with_url(url.to_owned());
        }
        if let Some((_, nick)) = self.nicknames.first() {
            builder = builder.with_nickname(Default::default(), nick.name.clone());
        }

        write_component(&builder.build().unwrap())
    }
}

impl From<&Address> for JSContact<JSContactVersion1> {
    fn from(addr: &Address) -> Self {
        let mut ret = Self(
            json_types::Card::default(),
            std::marker::PhantomData::<fn() -> JSContactVersion1>,
        );

        if let Some(display_name) = addr.get_display_name() {
            ret.name = display_name.to_string().into();
        }
        ret.0
            .emails
            .insert("main".into(), addr.get_email().to_string().into());
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
    impl_json_type_struct_serde! {NicknameType, "Nickname"}
    impl_json_type_struct_serde! {NoteType, "Note"}
    impl_json_type_struct_serde! {AuthorType, "Author"}
    impl_json_type_struct_serde! {OrganizationType, "Organization"}

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
        pub const fn is_none(&self) -> bool {
            matches!(self, Self(0))
        }

        pub fn now() -> Self {
            Self(crate::utils::datetime::now())
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
        pub uid: uuid::Uuid,
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
        pub nicknames: IndexMap<String, Nickname>,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub organizations: IndexMap<String, Organization>,
        #[serde(default, skip_serializing_if = "SpeakToAs::is_empty")]
        pub speak_to_as: SpeakToAs,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub titles: IndexMap<String, Title>,
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
        pub notes: IndexMap<String, Note>,
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl NameComponent {
        #[inline]
        pub fn new(value: String, kind: NameComponentKind) -> Self {
            Self {
                __type: None,
                value,
                kind,
                phonetic: String::new(),
                extra: IndexMap::new(),
            }
        }
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl Name {
        pub const fn is_empty(&self) -> bool {
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

    impl From<String> for Name {
        fn from(full: String) -> Self {
            Self {
                full: Some(full),
                ..Self::default()
            }
        }
    }

    #[derive(Clone, Debug, Default, Serialize, Deserialize, Eq, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct Nickname {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<NicknameType>,
        pub name: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pref: Option<u64>,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl From<String> for Nickname {
        fn from(name: String) -> Self {
            Self {
                name,
                ..Self::default()
            }
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl From<String> for EmailAddress {
        fn from(address: String) -> Self {
            Self {
                address,
                ..Self::default()
            }
        }
    }

    impl EmailAddress {
        #[inline]
        pub fn new(address: String) -> Self {
            Self {
                __type: None,
                address,
                ..Self::default()
            }
        }
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
        pub const fn is_none(&self) -> bool {
            matches!(self, Self::None)
        }

        pub const fn as_str(&self) -> &'static str {
            match self {
                Self::None => "none",
                Self::Animate => "animate",
                Self::Common => "common",
                Self::Feminine => "feminine",
                Self::Inanimate => "inanimate",
                Self::Masculine => "masculine",
                Self::Neuter => "neuter",
            }
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Timestamp {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<TimestampType>,
        pub utc: UTCDateTime,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl Timestamp {
        #[inline]
        pub fn new(utc: UTCDateTime) -> Self {
            Self {
                utc,
                ..Self::default()
            }
        }
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl Anniversary {
        #[inline]
        pub fn new(kind: AnniversaryKind, date: AnniversaryDate) -> Self {
            Self {
                __type: None,
                kind,
                date,
                place: serde_json::Value::Null,
                extra: IndexMap::new(),
            }
        }
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
        pub const fn is_none(&self) -> bool {
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
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Note {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<NoteType>,
        pub note: String,
        #[serde(default, skip_serializing_if = "UTCDateTime::is_none")]
        pub created: UTCDateTime,
        #[serde(default, skip_serializing_if = "UTCDateTime::is_none")]
        pub updated: UTCDateTime,
        #[serde(default, skip_serializing_if = "Author::is_empty")]
        pub author: Author,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    #[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Author {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<AuthorType>,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub name: String,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub uri: String,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    impl Author {
        pub const fn is_empty(&self) -> bool {
            self.name.is_empty() && self.uri.is_empty()
        }
    }

    #[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "lowercase")]
    pub enum TitleKind {
        #[default]
        Title,
        Role,
    }

    impl TitleKind {
        pub const fn is_title(&self) -> bool {
            matches!(self, Self::Title)
        }
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Title {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<TitleType>,
        pub name: String,
        #[serde(default, skip_serializing_if = "TitleKind::is_title")]
        pub kind: TitleKind,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub organization_id: String,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Organization {
        #[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
        pub __type: Option<OrganizationType>,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub name: String,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub units: Vec<serde_json::Value>,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        pub sort_as: String,
        #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
        pub contexts: IndexMap<String, bool>,
        #[serde(default, flatten, skip_serializing_if = "IndexMap::is_empty")]
        pub extra: IndexMap<String, serde_json::Value>,
    }

    #[test]
    fn test_addressbook_jscontact() {
        assert_eq!(
            Card {
                __type: CardType,
                version: CardVersion::_1_0,
                uid: uuid::Uuid::try_parse("22B2C7DF-9120-4969-8460-05956FE6B065").unwrap(),
                kind: CardKind::Individual,
                name: Name {
                    __type: None,
                    components: vec![],
                    full: Some("full_name".to_string()),
                    is_ordered: false,
                    default_separator: None,
                    ..Name::default()
                },
                emails: indexmap! {
                    "main".to_string() => EmailAddress {
                        __type: None,
                        address: "user@example.com".to_string(),
                        contexts: IndexMap::new(),
                        pref:None,
                        label: None,
                        ..EmailAddress::default()
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
    }
}
