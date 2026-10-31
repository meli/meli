/*
 * meli - contacts module
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

//! # vCard format
//!
//! This module implements the standards:
//!
//! - Version 3 (read-only) [RFC 2426: vCard MIME Directory Profile](https://datatracker.ietf.org/doc/2426)
//! - Version 4 [RFC 6350: vCard Format Specification](https://datatracker.ietf.org/doc/rfc6350/)
//! - Parameter escaping [RFC 6868 Parameter Value Encoding in iCalendar and vCard](https://datatracker.ietf.org/doc/rfc6868/)

use std::{convert::TryInto, path::PathBuf};

use indexmap::IndexMap;

use crate::{
    backends::prelude::ResultFuture,
    contacts::{
        backend::{ContactBackend, ContactBackendCapabilities},
        AddressBook, AddressBookName, Card,
    },
    error::{Error, ErrorKind, Result, ResultIntoError as _},
    utils::{
        parsec::{match_literal_anycase, one_or_more, peek, prefix, take_until, Parser},
        shellexpand::ShellExpandTrait as _,
    },
};

/* Supported vcard versions */
pub trait VCardVersion: Clone + std::fmt::Debug {
    const NAME: &str;
}

#[derive(Debug, Copy, Clone)]
pub struct VCardVersionUnknown;
impl VCardVersion for VCardVersionUnknown {
    const NAME: &str = "Unknown";
}

/// Version 4 <https://tools.ietf.org/html/rfc6350>
#[derive(Debug, Copy, Clone)]
pub struct VCardVersion4;
impl VCardVersion for VCardVersion4 {
    const NAME: &str = "4";
}

/// <https://tools.ietf.org/html/rfc2426>
#[derive(Debug, Copy, Clone)]
pub struct VCardVersion3;
impl VCardVersion for VCardVersion3 {
    const NAME: &str = "3";
}

pub struct CardDeserializer;

const HEADER_CRLF: &str = "BEGIN:VCARD\r\n"; //VERSION:4.0\r\n";
const FOOTER_CRLF: &str = "END:VCARD\r\n";
const HEADER_LF: &str = "BEGIN:VCARD\n"; //VERSION:4.0\n";
const FOOTER_LF: &str = "END:VCARD\n";
const HEADER: &str = "BEGIN:VCARD"; //VERSION:4.0";
const FOOTER: &str = "END:VCARD";

#[derive(Debug, Clone)]
pub struct VCard<V: VCardVersion>(
    IndexMap<String, ContentLine>,
    std::marker::PhantomData<fn() -> V>,
);

impl<V: VCardVersion> VCard<V> {
    pub fn new_v4() -> VCard<impl VCardVersion> {
        VCard(
            IndexMap::default(),
            std::marker::PhantomData::<fn() -> VCardVersion4>,
        )
    }

    pub const fn version(&self) -> &str {
        <V as VCardVersion>::NAME
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ContentLine {
    group: Option<String>,
    params: Vec<String>,
    value: String,
    others: Vec<Self>,
}

impl CardDeserializer {
    pub fn try_from_str(mut input: &str) -> Result<VCard<impl VCardVersion>> {
        let (sep, fold) = if input.starts_with(HEADER_CRLF) {
            ("\r\n", "\r\n ")
        } else {
            ("\n", "\n ")
        };

        input = if (!input.starts_with(HEADER_CRLF)
            || (!input.ends_with(FOOTER_CRLF) && !input.ends_with(FOOTER)))
            && (!input.starts_with(HEADER_LF)
                || (!input.ends_with(FOOTER_LF) && !input.ends_with(FOOTER)))
        {
            return Err(Error::new(format!(
                "Error while parsing vcard: input does not start or end with correct header and \
                 footer: {input:?}"
            ))
            .set_kind(ErrorKind::ValueError)
            .set_details(
                "vcard file entries are expected to start with a `BEGIN:VCARD` line and end with \
                 a `END:VCARD` line.",
            ));
        } else if input.starts_with(HEADER_CRLF) {
            &input[HEADER_CRLF.len()..input.len() - FOOTER_CRLF.len()]
        } else {
            &input[HEADER_LF.len()..input.len() - FOOTER_LF.len()]
        };

        let mut ret: IndexMap<String, ContentLine> = IndexMap::default();

        enum Stage {
            Group,
            Name,
            Param,
            Value,
        }
        let mut stage: Stage;

        let mut cursor = 0;
        while cursor < input.len() {
            let mut line_end = 0;
            while let Some(next_ln) = input[cursor..][line_end..].find(sep) {
                if input[cursor..][line_end..][next_ln + sep.len()..].starts_with(' ') {
                    line_end += next_ln + sep.len();
                    if !input[cursor..][line_end..].contains(sep) {
                        line_end = input[cursor..].len();
                        break;
                    }
                    continue;
                }
                line_end += next_ln + sep.len();
                break;
            }
            if line_end == 0 {
                line_end = input[cursor..].len();
            }
            let l = input[cursor..][..line_end].trim();
            cursor += line_end;

            let mut el = ContentLine::default();
            let mut value_start = 0;
            let mut has_colon = false;
            stage = Stage::Group;
            let mut name = String::new();
            for i in 0..l.len() {
                let byte = l.as_bytes()[i];
                match (byte, &stage) {
                    (b'.', Stage::Group) if l.as_bytes()[i] != b'\\' => {
                        el.group = Some(l[value_start..i].to_string());
                        value_start = i + 1;
                        stage = Stage::Name;
                    }
                    (b';', Stage::Group) => {
                        name = l[value_start..i].to_string();
                        value_start = i + 1;
                        stage = Stage::Param;
                    }
                    (b';', Stage::Param) => {
                        el.params.push(l[value_start..i].to_string());
                        value_start = i + 1;
                    }
                    (b';', Stage::Name) => {
                        name = l[value_start..i].to_string();
                        value_start = i + 1;
                        stage = Stage::Param;
                    }
                    (b':', Stage::Group) | (b':', Stage::Name) => {
                        name = l[value_start..i].to_string();
                        has_colon = true;
                        value_start = i + 1;
                        stage = Stage::Value;
                    }
                    (b':', Stage::Param) if l.as_bytes()[i.saturating_sub(1)] != b'\\' => {
                        el.params.push(l[value_start..i].to_string());
                        has_colon = true;
                        value_start = i + 1;
                        stage = Stage::Value;
                    }
                    _ => {}
                }
            }
            if !has_colon {
                return Err(Error::new(format!(
                    "Error while parsing vcard: error at line {l}, no colon. {el:?}"
                ))
                .set_kind(ErrorKind::ValueError));
            }
            if name.is_empty() {
                return Err(Error::new(format!(
                    "Error while parsing vcard: error at line {l}, no name for content line. \
                     {el:?}"
                ))
                .set_kind(ErrorKind::ValueError));
            }
            let mut value = l[value_start..].replace("\\:", ":");
            if value.contains(fold) {
                value = value.replace(fold, "");
            }
            el.value = value;
            if ret.contains_key(&name) {
                ret.entry(name).or_default().others.push(el);
            } else {
                ret.insert(name, el);
            }
        }
        Ok(VCard(
            ret,
            std::marker::PhantomData::<fn() -> VCardVersion4>,
        ))
    }
}

impl<V: VCardVersion> TryInto<Card> for VCard<V> {
    type Error = crate::error::Error;

    /// Lossily convert according to `RFC9555`.
    ///
    /// `Loffredo, M. and R. Stepanek, "JSContact: Converting from and to vCard", RFC 9555, DOI
    /// 10.17487/RFC9555, May 2024, <https://www.rfc-editor.org/info/rfc9555/>.`
    fn try_into(mut self) -> Result<Card> {
        let mut card = Card::new();
        card.uid = if let Some(val) = self.0.swap_remove("UID") {
            val.value.into()
        } else {
            let mut value = String::new();
            if let Some(val) = self.0.get("FN") {
                value.push_str(&val.value);
            }
            if let Some(val) = self.0.get("N") {
                value.push_str(&val.value);
            }
            if let Some(val) = self.0.get("EMAIL") {
                value.push_str(&val.value);
            }
            value.into()
        };
        if let Some(val) = self.0.swap_remove("FN") {
            card.name = val.value.into();
        }
        if let Some(val) = self.0.swap_remove("NICKNAME") {
            card.nicknames.insert("k0".into(), val.value.into());
        }
        if let Some(val) = self.0.swap_remove("BDAY") {
            // 4.3.4.  DATE-AND-OR-TIME
            // Either a DATE-TIME, a DATE, or a TIME value.  To allow unambiguous
            // interpretation, a stand-alone TIME value is always preceded by a "T".
            // Examples for "date-and-or-time":
            //           19961022T140000
            //           --1022T1400
            //           ---22T14
            //           19850412
            //           1985-04
            //           1985
            //           --0412
            //           ---12
            //           T102200
            //           T1022
            //           T10
            //           T-2200
            //           T--00
            //           T102200Z
            //           T102200-0800
            use crate::contacts::jscontact::json_types::{
                Anniversary, AnniversaryDate, AnniversaryKind, Timestamp, UTCDateTime,
            };
            if let Some(birthday) =
                crate::utils::datetime::timestamp_from_string(val.value.as_str(), "%Y%m%d\0")
                    .unwrap_or_default()
            {
                card.0.anniversaries.insert(
                    "k0".into(),
                    Anniversary::new(
                        AnniversaryKind::Birth,
                        AnniversaryDate::Timestamp(Timestamp::new(UTCDateTime(birthday))),
                    ),
                );
            }
        }
        if let Some(mut val) = self.0.swap_remove("EMAIL") {
            use crate::contacts::jscontact::json_types::EmailAddress;

            let add_email =
                |card: &'_ mut Card, val: ContentLine, ctr: &mut u32| -> (String, EmailAddress) {
                    let ContentLine {
                        group,
                        value,
                        others,
                        params,
                    } = val;
                    debug_assert!(others.is_empty());

                    let mut group = group.unwrap_or_else(|| format!("EMAIL-{ctr}"));
                    let pref = params
                        .iter()
                        .find_map(|p| p.strip_prefix("PREF=")?.parse::<u64>().ok());
                    let contexts = params
                        .iter()
                        .find_map(|p| Some((p.strip_prefix("TYPE=")?.to_string(), true)))
                        .into_iter()
                        .collect();
                    while card.emails.contains_key(&group) {
                        *ctr += 1;
                        group = format!("EMAIL-{ctr}");
                    }
                    *ctr += 1;
                    (
                        group,
                        EmailAddress {
                            address: value,
                            pref,
                            contexts,
                            ..EmailAddress::default()
                        },
                    )
                };

            let mut ctr = 1;
            let others = std::mem::take(&mut val.others);
            let (group, entry) = add_email(&mut card, val, &mut ctr);
            card.0.emails.insert(group, entry);
            for val in others {
                let (group, entry) = add_email(&mut card, val, &mut ctr);
                card.0.emails.insert(group, entry);
            }
        }
        if let Some(val) = self.0.swap_remove("CATEGORIES") {
            let ContentLine {
                group: _,
                value,
                others,
                ..
            } = val;
            if !value.is_empty() {
                card.keywords.insert(value, true);
            }
            for val in others {
                let ContentLine { value, others, .. } = val;
                debug_assert!(others.is_empty());
                card.keywords.insert(value, true);
            }
        }
        if let Some(mut val) = self.0.swap_remove("URL") {
            use crate::contacts::jscontact::json_types::{Link, Resource};

            let add_link =
                |card: &'_ mut Card, val: ContentLine, ctr: &mut u32| -> (String, Link) {
                    let ContentLine {
                        group,
                        value,
                        others,
                        params,
                    } = val;
                    debug_assert!(others.is_empty());

                    let mut group = group.unwrap_or_else(|| format!("LINK-{ctr}"));
                    let pref = params
                        .iter()
                        .find_map(|p| p.strip_prefix("PREF=")?.parse::<u64>().ok());
                    let contexts = params
                        .iter()
                        .find_map(|p| Some((p.strip_prefix("TYPE=")?.to_string(), true)))
                        .into_iter()
                        .collect();
                    while card.links.contains_key(&group) {
                        *ctr += 1;
                        group = format!("LINK-{ctr}");
                    }
                    *ctr += 1;
                    (
                        group,
                        Link {
                            resource: Resource {
                                uri: value,
                                pref,
                                contexts,
                                ..Resource::default()
                            },
                            ..Link::default()
                        },
                    )
                };

            let mut ctr = 1;
            let others = std::mem::take(&mut val.others);
            let (group, entry) = add_link(&mut card, val, &mut ctr);
            card.0.links.insert(group, entry);
            for val in others {
                let (group, entry) = add_link(&mut card, val, &mut ctr);
                card.0.links.insert(group, entry);
            }
        }

        {
            use crate::contacts::jscontact::json_types::OnlineService;

            let add_os =
                |card: &'_ mut Card, val: ContentLine, ctr: &mut u32| -> (String, OnlineService) {
                    let ContentLine {
                        group,
                        value,
                        others,
                        params,
                    } = val;
                    debug_assert!(others.is_empty());

                    let mut group = group.unwrap_or_else(|| format!("OS-{ctr}"));
                    let service = params
                        .iter()
                        .find_map(|p| Some(p.strip_prefix("SERVICE-TYPE=")?.to_string()))
                        .unwrap_or_default();
                    let user = params
                        .iter()
                        .find_map(|p| Some(p.strip_prefix("USERNAME=")?.to_string()))
                        .unwrap_or_default();
                    let pref = params
                        .iter()
                        .find_map(|p| p.strip_prefix("PREF=")?.parse::<u64>().ok());
                    let contexts = params
                        .iter()
                        .find_map(|p| Some((p.strip_prefix("TYPE=")?.to_string(), true)))
                        .into_iter()
                        .collect();
                    while card.online_services.contains_key(&group) {
                        *ctr += 1;
                        group = format!("OS-{ctr}");
                    }
                    *ctr += 1;
                    (
                        group,
                        OnlineService {
                            service,
                            uri: value,
                            user,
                            pref,
                            contexts,
                            ..OnlineService::default()
                        },
                    )
                };
            if let Some(mut val) = self.0.swap_remove("IMPP") {
                let others = std::mem::take(&mut val.others);
                let mut ctr = 1;
                let (group, mut entry) = add_os(&mut card, val, &mut ctr);
                entry.extra.insert("vCardName".into(), "impp".into());
                card.0.online_services.insert(group, entry);
                for val in others {
                    let (group, mut entry) = add_os(&mut card, val, &mut ctr);
                    entry.extra.insert("vCardName".into(), "impp".into());
                    card.0.online_services.insert(group, entry);
                }
            }

            if let Some(mut val) = self.0.swap_remove("SOCIALPROFILE") {
                let others = std::mem::take(&mut val.others);
                let mut ctr = 1;
                let (group, mut entry) = add_os(&mut card, val, &mut ctr);
                if url::Url::parse(&entry.uri).is_err() {
                    std::mem::swap(&mut entry.user, &mut entry.uri);
                }
                entry
                    .extra
                    .insert("vCardName".into(), "socialprofile".into());
                card.0.online_services.insert(group, entry);
                for val in others {
                    let (group, mut entry) = add_os(&mut card, val, &mut ctr);
                    if url::Url::parse(&entry.uri).is_err() {
                        std::mem::swap(&mut entry.user, &mut entry.uri);
                    }
                    entry
                        .extra
                        .insert("vCardName".into(), "socialprofile".into());
                    card.0.online_services.insert(group, entry);
                }
            }
        }

        if let Some(mut val) = self.0.swap_remove("KEY") {
            use crate::contacts::jscontact::json_types::{CryptoKey, Resource};

            let add_key =
                |card: &'_ mut Card, val: ContentLine, ctr: &mut u32| -> (String, CryptoKey) {
                    let ContentLine {
                        group,
                        value,
                        others,
                        params,
                    } = val;
                    debug_assert!(others.is_empty());

                    let mut group = group.unwrap_or_else(|| format!("KEY-{ctr}"));
                    let pref = params
                        .iter()
                        .find_map(|p| p.strip_prefix("PREF=")?.parse::<u64>().ok());
                    let contexts = params
                        .iter()
                        .find_map(|p| Some((p.strip_prefix("TYPE=")?.to_string(), true)))
                        .into_iter()
                        .collect();
                    while card.crypto_keys.contains_key(&group) {
                        *ctr += 1;
                        group = format!("KEY-{ctr}");
                    }
                    *ctr += 1;
                    (
                        group,
                        CryptoKey {
                            __type: None,
                            resource: Resource {
                                uri: value,
                                contexts,
                                pref,
                                ..Resource::default()
                            },
                        },
                    )
                };

            let mut ctr = 1;
            let others = std::mem::take(&mut val.others);
            let (group, entry) = add_key(&mut card, val, &mut ctr);
            card.0.crypto_keys.insert(group, entry);
            for val in others {
                let (group, entry) = add_key(&mut card, val, &mut ctr);
                card.0.crypto_keys.insert(group, entry);
            }
        }
        if let Some(mut val) = self.0.swap_remove("NICKNAME") {
            use crate::contacts::jscontact::json_types::Nickname;

            let add_nick =
                |card: &'_ mut Card, val: ContentLine, ctr: &mut u32| -> (String, Nickname) {
                    let ContentLine {
                        group,
                        value,
                        others,
                        params,
                    } = val;
                    debug_assert!(others.is_empty());

                    let mut group = group.unwrap_or_else(|| format!("NICK-{ctr}"));
                    let pref = params
                        .iter()
                        .find_map(|p| p.strip_prefix("PREF=")?.parse::<u64>().ok());
                    let contexts = params
                        .iter()
                        .find_map(|p| Some((p.strip_prefix("TYPE=")?.to_string(), true)))
                        .into_iter()
                        .collect();
                    while card.nicknames.contains_key(&group) {
                        *ctr += 1;
                        group = format!("NICK-{ctr}");
                    }
                    *ctr += 1;
                    (
                        group,
                        Nickname {
                            name: value,
                            contexts,
                            pref,
                            ..Nickname::default()
                        },
                    )
                };

            let mut ctr = 1;
            let others = std::mem::take(&mut val.others);
            let (group, entry) = add_nick(&mut card, val, &mut ctr);
            card.0.nicknames.insert(group, entry);
            for val in others {
                let (group, entry) = add_nick(&mut card, val, &mut ctr);
                card.0.nicknames.insert(group, entry);
            }
        }
        if let Some(val) = self.0.swap_remove("KIND") {
            // RFC 9555 2.4.2. KIND
            use crate::contacts::jscontact::json_types::CardKind;
            card.0.kind = match val.value.as_str() {
                v if v.eq_ignore_ascii_case(CardKind::Individual.as_str()) => CardKind::Individual,
                v if v.eq_ignore_ascii_case(CardKind::Group.as_str()) => CardKind::Group,
                v if v.eq_ignore_ascii_case(CardKind::Org.as_str()) => CardKind::Org,
                v if v.eq_ignore_ascii_case(CardKind::Location.as_str()) => CardKind::Location,
                v if v.eq_ignore_ascii_case(CardKind::Device.as_str()) => CardKind::Device,
                v if v.eq_ignore_ascii_case(CardKind::Application.as_str()) => {
                    CardKind::Application
                }
                _ => CardKind::default(),
            };
        }
        if let Some(val) = self.0.swap_remove("PRODID") {
            card.0.prod_id = val.value;
        }
        // [ref:TODO]: updated value
        // if let Some(val) = self.0.swap_remove("REV") {
        //     card.0.updated = ...
        // }

        Ok(card)
    }
}

fn parse_card<'a>() -> impl Parser<'a, Vec<&'a str>> {
    move |input| {
        one_or_more(prefix(
            peek(match_literal_anycase(HEADER)),
            take_until(match_literal_anycase(FOOTER)),
        ))
        .parse(input)
    }
}

pub fn load_cards(p: &std::path::Path) -> Result<Vec<Card>> {
    fn read_file(
        f: &std::path::Path,
        ret: &mut Vec<Result<Card>>,
        contents: &mut String,
    ) -> Result<()> {
        use std::io::Read as _;
        contents.clear();
        std::fs::File::open(f)
            .chain_err_related_path(f)?
            .read_to_string(contents)
            .chain_err_related_path(f)?;
        match parse_card().parse(contents.as_str()) {
            Ok((_, c)) => {
                for s in c {
                    ret.push(
                        CardDeserializer::try_from_str(s)
                            .and_then(TryInto::try_into)
                            .map_err(|err| err.set_related_path(Some(&f))),
                    );
                }
            }
            Err(err) => {
                log::warn!("Could not parse vcard from {}: {}", f.display(), err);
            }
        }
        Ok(())
    }

    let mut ret: Vec<Result<_>> = Vec::new();
    let mut contents = String::with_capacity(256);
    let mut is_any_valid = false;
    if !p.is_dir() {
        read_file(p, &mut ret, &mut contents)?;
    } else {
        let vcf_dir = std::fs::read_dir(p).chain_err_related_path(p)?;
        for f in vcf_dir {
            if f.is_err() {
                continue;
            }
            let f = f.chain_err_related_path(p)?.path();
            if f.is_file() {
                read_file(&f, &mut ret, &mut contents)?;
            }
        }
    }
    for c in &ret {
        if c.is_err() {
            log::debug!("Error parsing contact from {}: {c:?}", p.display());
        } else {
            is_any_valid = true;
        }
    }
    if is_any_valid {
        ret.retain(Result::is_ok);
    }
    ret.into_iter().collect::<Result<Vec<Card>>>()
}

#[derive(Debug)]
pub struct VCardContacts {
    pub path: PathBuf,
}

impl ContactBackend for VCardContacts {
    fn capabilities(&mut self) -> ContactBackendCapabilities {
        ContactBackendCapabilities::default()
    }

    fn address_books(&mut self) -> ResultFuture<Vec<AddressBookName>> {
        Ok(Box::pin(async {
            Ok(vec![AddressBookName("vcard_folder".into())])
        }))
    }

    fn fetch_book(&mut self, address_book: &AddressBookName) -> ResultFuture<AddressBook> {
        if address_book.0.as_ref() != "vcard_folder" {
            return Err(Error::new("").set_kind(ErrorKind::ValueError));
        }
        let name = address_book.clone();
        let vcard_path = self.path.clone();
        Ok(Box::pin(async move {
            let expanded_path = vcard_path.expand();
            let cards = load_cards(&expanded_path)
                .map_err(|err| {
                    let mut err = err.set_summary("Could not load vcards");
                    if expanded_path != *vcard_path {
                        err = err.set_details(format!(
                            "Note: vcard_folder was expanded from {} to {}",
                            vcard_path.display(),
                            expanded_path.display()
                        ));
                    }
                    err
                })?
                .into_iter()
                .map(|c| (c.id(), c))
                .collect();

            Ok(AddressBook {
                name,
                cards,
                read_only: true,
            })
        }))
    }

    fn search(
        &self,
        _term: &str,
        _address_book: Option<&AddressBookName>,
    ) -> ResultFuture<Vec<Card>> {
        Err(Error::new("").set_kind(ErrorKind::NotSupported))
    }

    fn add(&mut self, _: &AddressBookName, _: Card) -> ResultFuture<Card> {
        Err(Error::new("").set_kind(ErrorKind::NotSupported))
    }

    fn edit(&mut self, _: &AddressBookName, _: Card) -> ResultFuture<Card> {
        Err(Error::new("").set_kind(ErrorKind::NotSupported))
    }

    fn delete(&mut self, _: &AddressBookName, _: Card) -> ResultFuture<()> {
        Err(Error::new("").set_kind(ErrorKind::NotSupported))
    }
}

#[test]
fn test_vcard_parse() {
    // Test with CRLF endings
    let j = "BEGIN:VCARD\r\nVERSION:4.0\r\nN:Gump;Forrest;;Mr.;\r\nFN:Forrest Gump\r\nORG:Bubba Gump Shrimp Co.\r\nTITLE:Shrimp Man\r\nPHOTO;MEDIATYPE=image/gif:http://www.example.com/dir_photos/my_photo.gif\r\nTEL;TYPE=work,voice;VALUE=uri:tel:+1-111-555-1212\r\nTEL;TYPE=home,voice;VALUE=uri:tel:+1-404-555-1212\r\nADR;TYPE=WORK;PREF=1;LABEL=\"100 Waters Edge\\nBaytown\\, LA 30314\\nUnited States of America\":;;100 Waters Edge;Baytown;LA;30314;United States of America\r\nADR;TYPE=HOME;LABEL=\"42 Plantation St.\\nBaytown\\, LA 30314\\nUnited States of America\":;;42 Plantation St.;Baytown;LA;30314;United States of America\r\nEMAIL:forrestgump@example.com\r\nREV:20080424T195243Z\r\nx-qq:21588891\r\nEND:VCARD\r\n";

    let parsed = CardDeserializer::try_from_str(j).unwrap();
    let contents = indexmap::indexmap! {
        "VERSION".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "4.0".to_string(),
            others: vec![],
        },
        "N".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "Gump;Forrest;;Mr.;".to_string(),
            others: vec![],
        },
        "FN".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "Forrest Gump".to_string(),
            others: vec![],
        },
        "ORG".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "Bubba Gump Shrimp Co.".to_string(),
            others: vec![],
        },
        "TITLE".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "Shrimp Man".to_string(),
            others: vec![],
        },
        "PHOTO".to_string() => ContentLine {
            group: None,
            params: vec![
                "MEDIATYPE=image/gif".to_string(),
            ],
            value: "http://www.example.com/dir_photos/my_photo.gif".to_string(),
            others: vec![],
        },
        "TEL".to_string() => ContentLine {
             group: None,
             params: vec!["TYPE=work,voice".into(), "VALUE=uri".into()],
             value: "tel:+1-111-555-1212".into(),
             others: vec![
                 ContentLine {
                     group: None,
                     params: vec![
                         "TYPE=home,voice".to_string(),
                         "VALUE=uri".to_string(),
                     ],
                     value: "tel:+1-404-555-1212".to_string(),
                     others: vec![],
                 }
             ],
        },
        "ADR".to_string() => ContentLine {
            group: None,
            params: vec![
                "TYPE=WORK".to_string(),
                "PREF=1".to_string(),
                "LABEL=\"100 Waters Edge\\nBaytown\\, LA 30314\\nUnited States of America\"".to_string()
            ],
            value: ";;100 Waters Edge;Baytown;LA;30314;United States of America".to_string(),
            others: vec![ContentLine {
            group: None,
            params: vec![
                "TYPE=HOME".to_string(),
                "LABEL=\"42 Plantation St.\\nBaytown\\, LA 30314\\nUnited States of America\"".to_string(),
            ],
            value: ";;42 Plantation St.;Baytown;LA;30314;United States of America".to_string(),
            others: vec![],
        }],
        },
        "EMAIL".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "forrestgump@example.com".to_string(),
            others: vec![],
        },
        "REV".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "20080424T195243Z".to_string(),
            others: vec![],
        },
        "x-qq".to_string() => ContentLine {
            group: None,
            params: vec![],
            value: "21588891".to_string(),
            others: vec![],
        },
    };
    assert_eq!(parsed.version(), VCardVersion4::NAME);
    assert_eq!(parsed.0, contents);

    // Test with LF endings
    let j = "BEGIN:VCARD\nVERSION:4.0\nN:Gump;Forrest;;Mr.;\nFN:Forrest Gump\nORG:Bubba Gump Shrimp Co.\nTITLE:Shrimp Man\nPHOTO;MEDIATYPE=image/gif:http://www.example.com/dir_photos/my_photo.gif\nTEL;TYPE=work,voice;VALUE=uri:tel:+1-111-555-1212\nTEL;TYPE=home,voice;VALUE=uri:tel:+1-404-555-1212\nADR;TYPE=WORK;PREF=1;LABEL=\"100 Waters Edge\\nBaytown\\, LA 30314\\nUnited States of America\":;;100 Waters Edge;Baytown;LA;30314;United States of America\nADR;TYPE=HOME;LABEL=\"42 Plantation St.\\nBaytown\\, LA 30314\\nUnited States of America\":;;42 Plantation St.;Baytown;LA;30314;United States of America\nEMAIL:forrestgump@example.com\nREV:20080424T195243Z\nx-qq:21588891\nEND:VCARD\n";
    let parsed2 = CardDeserializer::try_from_str(j).unwrap();
    assert_eq!(parsed2.version(), VCardVersion4::NAME);
    assert_eq!(parsed2.0, contents);

    // Test with value folding
    let j = "BEGIN:VCARD\nVERSION:4.0\nN:Gump;Forrest;;Mr.;\nFN:Forrest \n Gump\nORG:Bubba Gump Shrimp Co.\nTITLE:Shrimp Man\nPHOTO;MEDIATYPE=image/gif:http://www.example.com/dir_photos/my_photo.gif\nTEL;TYPE=work,voice;VALUE=uri:tel:+1-111-555-1212\nTEL;TYPE=home,voice;VALUE=uri:tel:+1-404-555-1212\nADR;TYPE=WORK;PREF=1;LABEL=\"100 Waters Edge\\nBaytown\\, LA 30314\\nUnited States of America\":;;100 Waters Edge;Baytown;LA;30314;United States of America\nADR;TYPE=HOME;LABEL=\"42 Plantation St.\\nBaytown\\, LA 30314\\nUnited States of America\":;;42 Plantation St.;Baytown;LA;30314;United States of America\nEMAIL:forrestgump@example.com\nREV:20080424T195243Z\nx-qq:21588891\nEND:VCARD\n";
    let parsed3 = CardDeserializer::try_from_str(j).unwrap();
    assert_eq!(parsed3.version(), VCardVersion4::NAME);
    assert_eq!(parsed3.0, contents);
}
